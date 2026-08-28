# Data

## Datasets

Three series supplied for the course project and stored next to the analysis scripts:

| File | Series | Sample | Observations |
|------|--------|--------|--------------|
| `elec.csv` | U.S. residential electricity retail sales (million kWh) | 1973M01–2011M12 | 468 |
| `wti_oil_price.csv` | Seasonally adjusted WTI crude-oil price (USD/barrel) | 1986-01–2024-10 | 466 |
| `disney_stock_price.csv` | Disney daily closing price | 2007-01-03–2024-11-13 | 4,498 |

The combined calendar span is 1973–2024; no single series covers that entire window.

## Official / original sources

The CSVs were provided on the course Canvas site. They correspond to standard public series:

- Electricity: U.S. Energy Information Administration retail sales of electricity to residential customers
- Oil: West Texas Intermediate crude-oil price, seasonally adjusted
- Equities: Walt Disney Company daily closing price

This repository does not re-download the series. Place the three CSVs in `data/` using the filenames above.

## Training and holdout samples used in the code

| Series | Training | Holdout |
|--------|----------|---------|
| Electricity | through 2010M12 (456 months) | 2011 (12 months) |
| WTI | through 2022-12 | 2023–2024 (22 months) |
| Disney | through 2024-09-30 | from 2024-10-01 |

## Cleaning steps

`R/01_data_prep.R` only validates dimensions and missing values. `R/02_analysis.R` then:

- Electricity: parse `YYYYMmm` dates, take logs, construct a time index and month factors
- WTI: parse dates, form first differences
- Disney: parse dates, form log prices, daily log returns, and squared returns

No observations are imputed.

## License and citation

Confirm the license of any refreshed download from EIA, the oil-price vendor, and the equity data vendor before redistributing a new extract. The copies here are the course files used to produce the reported estimates.
