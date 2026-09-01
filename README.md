# Firm-Level Investment Responses to Oil Price Shocks

**An end-to-end empirical research workflow in Python and R**

This portfolio project examines whether oil-price shocks reduce capital investment among U.S. manufacturing firms and whether responses are larger in energy-intensive industries. It demonstrates a complete empirical research workflow, from raw financial and industry data preparation to panel-data estimation, dynamic inference, structural oil-shock decomposition, robustness checks, and publication-ready outputs.

## Research Question

How do oil price shocks affect firm-level capital investment among U.S. manufacturing firms, and do firms in more energy-intensive industries respond differently from other firms?

## Project Overview

The project combines Python-based data preparation with R-based econometric analysis.

Python is used to construct the analytical firm-year panel from raw financial, oil-price, and industry data. R is used for firm-specific fiscal-year alignment, panel variable construction, fixed-effects estimation, structural shock decomposition, cumulative-effect inference, selected robustness checks, and output generation.

## Workflow

```text
Compustat financial data
WTI oil prices              --> Python data preparation --> Firm-year panel
ASM energy-intensity data

Firm-year panel
Monthly structural oil shocks --> R fiscal-year alignment
                                  |
                                  v
                     +------------+-------------+
                     |            |             |
                     v            v             v
              Reduced-form   Dynamic      Shock decomposition
                 models      effects
                     |            |             |
                     +------------+-------------+
                                  |
                                  v
                       Publication-ready outputs
```

## Key Technical Features

### Python: Analytical Data Engineering

- Filters and validates a proprietary Compustat firm panel.
- Constructs investment outcomes and firm-level financial controls.
- Creates valid lags without carrying values across gaps in an unbalanced panel.
- Aggregates monthly WTI prices and constructs annual log price changes.
- Builds four-digit NAICS energy intensity from Census Annual Survey of Manufactures data.
- Uses explicit merge validation and portable project paths.

### R: Statistical Analysis

- Validates input files and firm-year panel keys before estimation.
- Constructs treatment variables, interaction terms, and consecutive-year lags.
- Aligns monthly structural oil shocks with firm-specific fiscal-year windows using a `data.table` non-equijoin.
- Estimates firm and year fixed-effects models using `fixest`.
- Uses two-way clustered standard errors by firm and fiscal year.
- Evaluates current, lagged, and cumulative dynamic effects.
- Decomposes oil-price movements into supply and economic-activity shocks.
- Conducts focused robustness checks using an alternative investment outcome and continuous/nonlinear energy-intensity specifications.
- Produces labeled regression tables and records package/session information.

## Repository Structure

```text
firm-investment-oil-shocks/
├── README.md
├── .gitignore
├── code/
│   ├── 01_data_preparation.py
│   └── 02_panel_analysis.R
├── notebooks/
│   └── 02_panel_analysis.Rmd
├── docs/
│   ├── portfolio_overview.pdf
│   └── r_code_sample_full_source.pdf
├── data/
│   ├── README.md
│   ├── raw/
│   │   └── .gitkeep
│   └── processed/
│       └── .gitkeep
└── output/
    ├── README.md
    └── tables/
        └── .gitkeep
```

## Files

- `code/01_data_preparation.py`: prepares the analytical firm-year panel from raw financial, oil-price, and industry data.
- `code/02_panel_analysis.R`: contains the complete documented R workflow for panel construction, fiscal-year shock alignment, fixed-effects estimation, structural oil-shock decomposition, cumulative-effect inference, robustness checks, and output generation.
- `notebooks/02_panel_analysis.Rmd`: provides a narrative version of the analysis with selected code excerpts.
- `docs/portfolio_overview.pdf`: provides a concise portfolio-style overview of the empirical workflow.
- `docs/r_code_sample_full_source.pdf`: provides the full R code sample as a PDF.
- `data/README.md`: documents expected input files and explains why raw and processed data are not distributed.
- `output/README.md`: documents the outputs generated when the scripts are run with authorized data access.

## Data Sources

- Compustat firm-level financial statements
- FRED West Texas Intermediate oil prices
- U.S. Census Annual Survey of Manufactures
- Baumeister-Hamilton structural oil shocks

## Data Availability

The raw and processed data files are not included in this repository. The firm-level financial data come from Compustat and are proprietary. The code is provided to document the full analytical workflow and can be run by users with authorized access to the required datasets.

Expected source-file locations are documented in `data/README.md`.

## Reproducibility

To reproduce the full workflow with authorized data access:

1. Place raw source files in `data/raw/`.
2. Run `code/01_data_preparation.py` to construct the processed firm-year panel.
3. Run `code/02_panel_analysis.R` for the complete statistical workflow.
4. Knit `notebooks/02_panel_analysis.Rmd` to generate the narrative portfolio notebook.

The R script writes generated tables to `output/tables/` and records the computing environment in `output/session_info.txt`.

## Code Sample Materials

For a concise narrative overview, see `docs/portfolio_overview.pdf`.

For the complete R code sample, see `docs/r_code_sample_full_source.pdf` or `code/02_panel_analysis.R`.
