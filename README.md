# Analysis of South Carolina's Unemployment Insurance Claims

This repository contains an applied economic research project completed as part of the **Economic Scholars Program** at the University of South Carolina, in collaboration with the **South Carolina Department of Employment and Workforce (SCDEW)**. The project analyzes South Carolina’s unemployment insurance (UI) system using administrative, labor market, and macroeconomic data.

The research consists of two primary components:
1. **Forecasting unemployment insurance claims in South Carolina** using monthly economic indicators.
2. **Modeling state-level UI dynamics** using Markov chain transition matrices to study claim volatility across U.S. states.

---

## Project Objectives

- Construct a comprehensive, reproducible dataset combining administrative UI records with labor market and macroeconomic data.
- Develop and validate statistical models to forecast one-month-ahead UI claims in South Carolina.
- Analyze persistence and transitions in UI claim volumes across states using stochastic process methods.

---

## Data Sources

The project integrates multiple data sources spanning **2000–2024**, including:
- **SCDEW administrative data** (ETA 5159 unemployment insurance claims)
- **LAUS** labor market data (employment, unemployment, labor force)
- **FRED** macroeconomic indicators (hires, layoffs, separations, quits, labor force participation)
- **S&P 500 indices**, including sector-level measures
- State and national population data used to construct per-capita measures

All data are transformed to a **monthly frequency**, with additional lagged and growth-rate variables engineered for modeling.

---

## Methodology

### 1. Forecasting UI Claims (South Carolina)
- Built multiple linear regression models to forecast **one-month-ahead UI claims per capita**.
- Conducted extensive model selection and diagnostic testing, including:
  - Residual analysis
  - Leverage and influence diagnostics
  - Multicollinearity checks (VIF)
- Final model achieved strong predictive performance (Adjusted R² ≈ 0.79).

### 2. Markov Chain Analysis (Multi-State)
- Modeled quarterly UI claim volumes using **discrete-state Markov chains**.
- Estimated transition probability matrices for multiple U.S. states.
- Analyzed persistence and transition behavior across claim volume levels.
- This component was completed as a formal paper for **STAT 521: Applied Stochastic Processes**.

---

## Repository Structure


### Data
The `Datasets/` directory contains all raw and processed data used in the analysis, organized by source:
- **ClaimsData/**: South Carolina unemployment insurance claims data (ETA 203 and ETA 5159).
- **FRED-Jobs/**: Labor market flow variables from FRED (hires, layoffs, openings, quits, separations, LFPR).
- **LAUS/**: Local Area Unemployment Statistics for South Carolina (employment, unemployment, labor force).
- **SP500/**: S&P 500 aggregate and sector-level indices.
- **FinalData.csv**: Final merged dataset used for modeling.
- **scPopulation.csv**: Population data used to construct per-capita measures.

### Analysis Code
All analysis is written in **R**, with scripts organized by purpose:
- **R-Files/**: Core R scripts for data cleaning, modeling, visualization, and Markov chain analysis.
- **R-Markdown-Files/**: Reproducible R Markdown files corresponding to each analysis step.

### Outputs
- **PDF-Files/**: Rendered reports and final write-ups, including model results and the Markov chain paper.

### Documentation
- **README.md**: Project overview and documentation.

---

## Tools and Technologies

- **R**
- Statistical modeling and forecasting
- Time series and panel-style data construction
- Stochastic process modeling (Markov chains)
- Data visualization and reproducible research workflows

---

## Notes

This project emphasizes **model interpretability, robustness, and economic reasoning** over black-box prediction, reflecting real-world policy analysis constraints.
