# Monetary Policy Surprises for Russia (BRW)

Monthly series of Bank of Russia monetary policy surprises, identified from the Russian government bond yield curve with the method of [Bu, Rogers and Wu (2021), "A Unified Measure of Fed Monetary Policy Shocks", *Journal of Monetary Economics* 118: 331–349](https://doi.org/10.1016/j.jmoneco.2020.11.002).

**Download the series:** [CSV](output/MonetaryShocksBuRogersWu.csv) · [Excel](output/MonetaryShocksBuRogersWu.xlsx)

## The series

| column | description |
|---|---|
| `date` | last day of the month |
| `year_month` | the same month, e.g. `2014 Feb` |
| `estimate_brw` | the surprise, in percentage points of the 2-year yield; summed if a month had two decisions |

Every month from February 2014 is listed. Months without a key rate decision are 0. March 2022 is empty (`NA`): its only decision fell while the Moscow Exchange bond market was closed. A positive value is a tightening surprise.

## Method

The BRW approach uses the whole yield curve rather than a single short rate. It runs in two stages:

1. **Sensitivities.** For each maturity from two weeks to four years, the daily yield change is regressed on the change in the 2-year yield. The regression is instrumented by the difference between decision days and the same weekday one week earlier: yields are more volatile on decision days, and that extra variance identifies the response to policy news.
2. **Surprises.** On each decision day, the one-day yield changes across maturities, net of the first-stage constants, are regressed on the sensitivities. The coefficient is the surprise.

A surprise of 1 moves the yield curve as much as a 1 percentage point change in the 2-year yield.

## Data sources

- **Yield curve:** the Moscow Exchange zero-coupon yield curve for government bonds (G-curve), daily since January 2014. The scripts download the curve parameters from the [MOEX ISS API](https://iss.moex.com) and compute yields from them.
- **Decision dates:** Bank of Russia Board of Directors meetings on the key rate, from the [Bank of Russia calendar](https://www.cbr.ru/dkp/cal_mp/).

## Repository layout

```
BRWRussia/
├── run_all.R                 full update: dates, yield curve, estimation, charts
├── R/
│   ├── 01_decision_dates.R   updates data/BoRMPSchedule.csv from cbr.ru
│   ├── 02_yield_curve.R      updates data/yc_parameters.csv from MOEX, builds data/yield_curves.csv
│   ├── 03_estimate_shocks.R  estimates the surprises, writes output/MonetaryShocksBuRogersWu.*
│   ├── 04_plots.R            charts in output/figures/ (not tracked)
│   └── extra/                variance diagnostics
├── data/                     inputs: decision dates, curve parameters, yields
└── output/                   the series in CSV and Excel
```

## Running

Requires R 4.1 or later; packages are installed on first run through `pacman`.

```r
# from anywhere inside the project folder (or open BRWRussia.Rproj)
source("run_all.R")
```

All files are read from and written to the project folder. To re-estimate from the saved data without downloading anything, run `R/03_estimate_shocks.R` alone.

## Citation

If you use the series, please cite Bu, Rogers and Wu (2021) for the method and this repository for the Russian series:

> Vlasov, A. (2026). *Monetary Policy Surprises for Russia (BRW)*. https://github.com/avlsv/BRWRussia

## License

Code is released under the MIT License (see `LICENSE`).
