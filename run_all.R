# Full update: decision dates, yield curve, shock estimation, charts.
# All files are read from and written to this project folder (data/ and output/).

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

steps <- c(
  "R/01_decision_dates.R", # Bank of Russia key rate decision dates (cbr.ru)
  "R/02_yield_curve.R",    # MOEX zero-coupon yield curve (G-curve)
  "R/03_estimate_shocks.R",# Bu-Rogers-Wu monetary policy shocks
  "R/04_plots.R"           # charts and the table of recent shocks
)

for (step in steps) {
  message("\n=== ", step)
  source(here::here(step), local = new.env())
}
