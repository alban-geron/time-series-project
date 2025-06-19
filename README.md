# Time Series Project

This repository contains the results of a time series modeling project conducted by Théo Lartigau and Alban Géron. Our objective was to forecast the monthly, seasonally and working-day adjusted **Industrial Production Index (IPI)** for the manufacture of beverages in France. The original data is publicly available from [INSEE](https://www.insee.fr/fr/statistiques/serie/010767669), and it reflects the physical output of France’s beverage industry (including breweries, wineries, distillers, soft drink producers, and bottled water facilities), adjusted for calendar effects and seasonality.

## Methodology

We performed first-order differencing to make the series stationary and fitted an $\textsf{ARMA}(p, q)$ model to the transformed data. Using this model, we generated forecasts for future values of the IPI and constructed prediction intervals. We also derived and visualized confidence regions for future values of the series, discussed the underlying statistical assumptions, and explored how the availability of related time series data could enhance forecasting accuracy.

## Repository contents

- `script.R` — R script used for data processing, model fitting, and forecasting.
- `data.csv` — CSV file containing the original time series.
- `report.tex` — LaTeX source of the report summarizing our methods and findings.
