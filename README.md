# Poverty Analysis in Eastern Indonesia Using Multiple Regression

A group project for the **Regression Analysis** course at **BINUS University**.

The project examines relationships between poverty rates and socioeconomic indicators in Eastern Indonesia using 2021 data. It compares Ordinary Least Squares (OLS) with alternative regression methods, selecting Generalized Least Squares (GLS) to address residual autocorrelation.

## Dataset

`2021socio_economic_indonesia.csv` contains **514 district/city records**. Removing rows with missing values leaves **241 complete observations** for analysis.

The report identifies the source as the *Socio-Economic of Indonesia in 2021* dataset on Kaggle.

The response variable is `poorpeople_percentage`. The initial predictors are:

- Regional GDP
- Life expectancy
- Average years of schooling
- Per capita expenditure
- Open unemployment rate
- Gini ratio

## Methods

- Pearson correlation analysis.
- Multiple linear regression with backward elimination.
- Residual normality, heteroskedasticity, autocorrelation, and multicollinearity checks.
- Outlier, leverage, and Cook's distance diagnostics.
- Comparison with robust regression, median regression, Cochrane–Orcutt, Weighted Least Squares, and GLS.

The selected GLS model uses an **AR(1) correlation structure** and retains four predictors.

## Reported Results

### Final GLS Model

| Predictor | Coefficient | Association with poverty rate |
| --- | --- | --- |
| Life expectancy | −0.47038 | Negative |
| Average years of schooling | −0.69090 | Negative |
| Per capita expenditure | −0.00119 | Negative |
| Gini ratio | 114.38673 | Positive |

All four retained predictors were reported as statistically significant at the **0.05 level**.

The open unemployment rate was removed during the initial OLS selection, while regional GDP was excluded from the final GLS model.

These results describe associations within the analyzed data, rather than proving causal effects.

### Model Fit

- Reduced OLS model R²: **0.7112**
- Final GLS squared correlation between observed and fitted values: **0.7078**
- Reported GLS AIC: **1475.542**
- Reported GLS BIC: **1503.218**

The GLS value is calculated as `cor(actual, predicted)^2` in the script; it is not the same measure as conventional OLS R².

### Final Model Diagnostics

| Diagnostic | Reported p-value | Interpretation |
| --- | --- | --- |
| Lilliefors normality test | 0.000074 | Evidence against residual normality |
| Heteroskedasticity check | 0.000001067 | Unequal residual variance remains |
| Durbin–Watson test | 0.8509 | No significant evidence of residual autocorrelation under this test |

The reported diagnostics suggest that GLS addressed autocorrelation, while normality and unequal variance remained unresolved.

The AR(1) structure follows the existing observation order. The script does not define geographic neighbors or a spatial-distance model.

Results are taken from the supplied report and code comments and have not been independently reproduced.

## Tools

**R and RStudio**, with packages including `Hmisc`, `dplyr`, `purrr`, `broom`, `nortest`, `lmtest`, `car`, `MASS`, `quantreg`, `orcutt`, and `nlme`.

## Project Files

| File | Description |
| --- | --- |
| `aol.R` | Correlation analysis, regression models, and diagnostics |
| `2021socio_economic_indonesia.csv` | Socioeconomic dataset |

## How to Run

1. Open `aol.R` in R or RStudio and install the required packages.
2. Correct `vif(model_gls)ss` to `vif(model_gls)`.
3. Run the main analysis and select the supplied CSV when prompted.

## Authors

**Group 8 — BINUS University**

- Marcell Prasetyo
- Kelvin Fardiman
- Nusantara Kusuma
