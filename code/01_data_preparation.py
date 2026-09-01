#!/usr/bin/env python3
"""Build the firm-year analytical panel used in the R analysis.

This script prepares proprietary Compustat firm data, annual WTI oil-price
changes, and industry energy-intensity measures from the U.S. Census Annual
Survey of Manufactures. It exports a documented, regression-ready panel for
the companion R workflow.

The raw data are not distributed because the Compustat extract is proprietary.
All paths are relative to the project root so the workflow can be reproduced on
another computer once the source files are placed in ``data/raw``.
"""

from __future__ import annotations

from pathlib import Path
from typing import Iterable

import numpy as np
import pandas as pd


# -----------------------------------------------------------------------------
# 1. Project paths and source-file names
# -----------------------------------------------------------------------------

PROJECT_ROOT = Path(__file__).resolve().parent
RAW_DIR = PROJECT_ROOT / "data" / "raw"
PROCESSED_DIR = PROJECT_ROOT / "data" / "processed"
PROCESSED_DIR.mkdir(parents=True, exist_ok=True)

COMPUSTAT_FILE = RAW_DIR / "compustat_1999.csv"
WTI_FILE = RAW_DIR / "POILWTIUSDM.csv"
ASM_FILE = RAW_DIR / "ASM_2008_31GS101_with_ann.csv"

PANEL_OUTPUT = PROCESSED_DIR / "thesis_model_df.csv"
CONTROLS_OUTPUT = PROCESSED_DIR / "panel_patch_controls.csv"

COMPUSTAT_COLUMNS = [
    "gvkey", "fyear", "datadate", "naics", "sic", "at", "ceq", "che",
    "dlc", "dltt", "ppent", "cogs", "ni", "oiadp", "sale", "xsga",
    "capx", "emp",
]

ASM_COLUMNS = [
    "Geographic area name",
    "2007 NAICS codes and NAICS-based rollup code",
    "Year",
    "Cost of purchased fuels ($1,000)",
    "Purchased electricity ($1,000)",
    "Total value of shipments ($1,000)",
]


# -----------------------------------------------------------------------------
# 2. Validation and transformation helpers
# -----------------------------------------------------------------------------

def require_columns(data: pd.DataFrame, columns: Iterable[str], name: str) -> None:
    """Raise a clear error when an input file is missing required variables."""
    missing = sorted(set(columns) - set(data.columns))
    if missing:
        raise ValueError(f"{name} is missing required columns: {missing}")


def assert_unique_key(data: pd.DataFrame, keys: list[str], name: str) -> None:
    """Ensure that each analytical unit appears only once."""
    duplicated = data.duplicated(keys, keep=False)
    if duplicated.any():
        examples = data.loc[duplicated, keys].head(10).to_dict("records")
        raise ValueError(
            f"{name} does not have a unique {keys} key. Examples: {examples}"
        )


def winsorize_series(
    series: pd.Series,
    lower: float = 0.01,
    upper: float = 0.99,
) -> pd.Series:
    """Cap non-missing observations at the selected empirical quantiles."""
    if not 0 <= lower < upper <= 1:
        raise ValueError("Winsorization bounds must satisfy 0 <= lower < upper <= 1.")

    lower_bound, upper_bound = series.quantile([lower, upper])
    return series.clip(lower=lower_bound, upper=upper_bound)


def consecutive_lag(
    data: pd.DataFrame,
    value_column: str,
    group_column: str = "gvkey",
    year_column: str = "fyear",
) -> pd.Series:
    """Create a one-year lag without carrying values across gaps in the panel.

    Compustat panels are unbalanced. A simple groupwise shift can therefore
    treat a value from several years earlier as a one-year lag. This helper
    retains the shifted value only when the prior observation is exactly one
    fiscal year earlier.
    """
    lagged_value = data.groupby(group_column)[value_column].shift(1)
    lagged_year = data.groupby(group_column)[year_column].shift(1)
    return lagged_value.where(data[year_column].eq(lagged_year + 1))


def clean_numeric_text(series: pd.Series) -> pd.Series:
    """Convert comma-formatted text fields to numeric values."""
    return pd.to_numeric(
        series.astype(str).str.replace(",", "", regex=False).str.strip(),
        errors="coerce",
    )


# -----------------------------------------------------------------------------
# 3. Prepare Compustat firm-year data
# -----------------------------------------------------------------------------

def prepare_compustat_panel(compustat: pd.DataFrame) -> pd.DataFrame:
    """Construct investment outcomes and firm-level controls from Compustat."""
    require_columns(compustat, COMPUSTAT_COLUMNS, "Compustat data")
    panel = compustat.loc[:, COMPUSTAT_COLUMNS].copy()

    # Harmonize identifiers and dates before filtering or merging.
    panel["gvkey"] = panel["gvkey"].astype(str).str.strip()
    panel["naics"] = panel["naics"].astype(str).str.strip()
    panel["datadate"] = pd.to_datetime(panel["datadate"], errors="coerce")
    panel["fyear"] = pd.to_numeric(panel["fyear"], errors="coerce")

    # Retain earlier years so the first analysis year can use valid lagged data.
    panel = panel.loc[panel["fyear"].between(1999, 2023)].copy()

    # NAICS sectors 31–33 define manufacturing.
    panel["naics2"] = panel["naics"].str.extract(r"^(\d{2})")
    panel = panel.loc[panel["naics2"].isin(["31", "32", "33"])].copy()

    # Core variables are required before constructing investment ratios.
    panel = panel.dropna(
        subset=["gvkey", "fyear", "datadate", "at", "capx", "sale", "ppent"]
    ).copy()
    assert_unique_key(panel, ["gvkey", "fyear"], "Filtered Compustat panel")

    panel = panel.sort_values(["gvkey", "fyear", "datadate"]).reset_index(drop=True)

    # Construct valid beginning-of-period denominators for investment measures.
    panel["at_lag"] = consecutive_lag(panel, "at")
    panel["ppent_lag"] = consecutive_lag(panel, "ppent")
    panel["sale_lag"] = consecutive_lag(panel, "sale")

    panel["inv_at"] = panel["capx"] / panel["at_lag"]
    panel["inv_ppent"] = panel["capx"] / panel["ppent_lag"]
    panel.loc[panel["at_lag"] <= 0, "inv_at"] = np.nan
    panel.loc[panel["ppent_lag"] <= 0, "inv_ppent"] = np.nan

    # Standard firm controls used in the empirical investment literature.
    panel["ln_at"] = np.where(panel["at"] > 0, np.log(panel["at"]), np.nan)
    panel["lev"] = (panel["dlc"].fillna(0) + panel["dltt"].fillna(0)) / panel["at"]
    panel["prof"] = panel["oiadp"] / panel["at"]
    panel["sales_growth"] = (panel["sale"] - panel["sale_lag"]) / panel["sale_lag"]
    panel.loc[panel["sale_lag"] <= 0, "sales_growth"] = np.nan
    panel.loc[panel["at"] <= 0, ["ln_at", "lev", "prof"]] = np.nan

    # Additional balance-sheet controls are exported for the preferred R models.
    panel["cash_at"] = panel["che"] / panel["at"]
    panel["tangibility"] = panel["ppent"] / panel["at"]
    panel["cogs_share"] = panel["cogs"] / panel["sale"]
    panel = panel.replace([np.inf, -np.inf], np.nan)

    # Winsorized variants limit the influence of extreme accounting ratios.
    for variable in ["inv_at", "inv_ppent", "lev", "prof", "sales_growth"]:
        panel[f"{variable}_w"] = winsorize_series(panel[variable])

    panel["naics4"] = panel["naics"].str.extract(r"^(\d{4})")
    return panel


# -----------------------------------------------------------------------------
# 4. Construct annual WTI oil-price shocks
# -----------------------------------------------------------------------------

def prepare_wti_shocks(wti_raw: pd.DataFrame) -> pd.DataFrame:
    """Aggregate monthly WTI prices and compute annual log price changes."""
    require_columns(wti_raw, ["observation_date", "POILWTIUSDM"], "WTI data")
    oil = wti_raw.copy()
    oil["observation_date"] = pd.to_datetime(oil["observation_date"], errors="coerce")
    oil["wti"] = pd.to_numeric(oil["POILWTIUSDM"], errors="coerce")
    oil = oil.dropna(subset=["observation_date", "wti"]).copy()
    oil = oil.loc[oil["wti"] > 0].copy()
    oil["year"] = oil["observation_date"].dt.year

    annual = (
        oil.groupby("year", as_index=False)["wti"]
        .mean()
        .sort_values("year")
        .reset_index(drop=True)
    )
    annual["log_wti"] = np.log(annual["wti"])
    annual["oil_shock"] = annual["log_wti"].diff()
    return annual


# -----------------------------------------------------------------------------
# 5. Construct industry energy-intensity measures
# -----------------------------------------------------------------------------

def prepare_energy_intensity(asm_raw: pd.DataFrame) -> pd.DataFrame:
    """Build a time-invariant four-digit NAICS energy-intensity measure."""
    require_columns(asm_raw, ASM_COLUMNS, "ASM data")
    asm = asm_raw.loc[:, ASM_COLUMNS].copy()
    asm = asm.loc[asm["Geographic area name"].eq("United States")].copy()

    asm = asm.rename(
        columns={
            "2007 NAICS codes and NAICS-based rollup code": "asm_naics",
            "Year": "year",
            "Cost of purchased fuels ($1,000)": "fuel_cost",
            "Purchased electricity ($1,000)": "electricity_cost",
            "Total value of shipments ($1,000)": "shipments",
        }
    )
    asm["naics4"] = asm["asm_naics"].astype(str).str.extract(r"^(\d{4})")
    for variable in ["fuel_cost", "electricity_cost", "shipments"]:
        asm[variable] = clean_numeric_text(asm[variable])
    asm["year"] = pd.to_numeric(asm["year"], errors="coerce")

    asm = asm.dropna(
        subset=["naics4", "year", "fuel_cost", "electricity_cost", "shipments"]
    ).copy()
    asm = asm.loc[asm["shipments"] > 0].copy()

    asm["energy_intensity_jt"] = (
        asm["fuel_cost"] + asm["electricity_cost"]
    ) / asm["shipments"]

    # Averaging across years produces the predetermined industry exposure used
    # to compare firms without allowing contemporaneous firm outcomes to define it.
    energy = (
        asm.groupby("naics4", as_index=False)["energy_intensity_jt"]
        .mean()
        .rename(columns={"energy_intensity_jt": "energy_intensity"})
    )
    assert_unique_key(energy, ["naics4"], "Energy-intensity table")
    return energy


# -----------------------------------------------------------------------------
# 6. Merge sources and export regression-ready files
# -----------------------------------------------------------------------------

def build_analysis_files(
    panel: pd.DataFrame,
    annual_oil: pd.DataFrame,
    energy: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Merge external exposures and create the R analysis inputs."""
    merged = panel.merge(
        annual_oil[["year", "wti", "log_wti", "oil_shock"]],
        left_on="fyear",
        right_on="year",
        how="left",
        validate="many_to_one",
    ).drop(columns="year")

    merged["naics4"] = merged["naics4"].astype("string").str.strip()
    energy = energy.copy()
    energy["naics4"] = energy["naics4"].astype("string").str.strip()
    merged = merged.merge(energy, on="naics4", how="left", validate="many_to_one")

    model_columns = [
        "gvkey", "fyear", "datadate", "naics", "naics4",
        "inv_at_w", "inv_ppent_w", "ln_at", "lev_w", "prof_w",
        "sales_growth_w", "wti", "log_wti", "oil_shock",
        "energy_intensity", "inv_at", "inv_ppent", "lev", "prof",
        "sales_growth",
    ]
    model_panel = merged.loc[:, model_columns].copy()
    model_panel["fyear"] = model_panel["fyear"].astype("Int64")

    controls_columns = [
        "gvkey", "fyear", "datadate", "at", "capx", "ppent", "sale", "che",
        "dlc", "dltt", "ceq", "ni", "oiadp", "xsga", "cogs", "emp",
        "cash_at", "tangibility", "cogs_share",
    ]
    controls = merged.loc[:, controls_columns].copy()
    controls["fyear"] = controls["fyear"].astype("Int64")

    assert_unique_key(model_panel, ["gvkey", "fyear"], "Model panel")
    assert_unique_key(controls, ["gvkey", "fyear"], "Controls panel")
    return model_panel, controls


def print_export_summary(model_panel: pd.DataFrame) -> None:
    """Report concise validation statistics for the generated panel."""
    usable = model_panel.dropna(
        subset=[
            "inv_at_w", "ln_at", "lev_w", "prof_w", "sales_growth_w",
            "oil_shock", "energy_intensity",
        ]
    )
    print(f"Saved panel rows: {len(model_panel):,}")
    print(f"Usable baseline rows: {len(usable):,}")
    print(f"Unique firms: {usable['gvkey'].nunique():,}")
    print(f"Fiscal-year range: {usable['fyear'].min()}–{usable['fyear'].max()}")
    print(f"Missing energy intensity: {model_panel['energy_intensity'].isna().mean():.1%}")


def main() -> None:
    """Run the complete Python data-preparation pipeline."""
    compustat = pd.read_csv(COMPUSTAT_FILE)
    wti_raw = pd.read_csv(WTI_FILE)
    asm_raw = pd.read_csv(ASM_FILE)

    panel = prepare_compustat_panel(compustat)
    annual_oil = prepare_wti_shocks(wti_raw)
    energy = prepare_energy_intensity(asm_raw)
    model_panel, controls = build_analysis_files(panel, annual_oil, energy)

    model_panel.to_csv(PANEL_OUTPUT, index=False)
    controls.to_csv(CONTROLS_OUTPUT, index=False)
    print_export_summary(model_panel)
    print(f"Model panel written to: {PANEL_OUTPUT}")
    print(f"Controls panel written to: {CONTROLS_OUTPUT}")


if __name__ == "__main__":
    main()
