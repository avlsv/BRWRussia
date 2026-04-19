# Packages ------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, openxlsx,
  lubridate, readxl,
  estimatr, tsibble, broom
)

# renv::snapshot()
# i_am("BRWRussia/MonetaryPolicyShocksEstimation.R")
# setwd(here("BRWRussia"))
# setwd("~/Documents/BRWRussia")

# source("YieldCurveConstruction.R")
# source("BoRSiteParser.R")


rates <- read_csv("yield_curves_1.csv") |>
  mutate(index = as.Date(index)) |>
  filter(!if_all(!index, is.na)) |>
  suppressMessages() |>
  select(-2)


returns <- rates |>
  mutate(across(-index, \(x) c(NA, diff(x, lag = 1))))


FOMC_dates <- read_csv("BoRMPSchedule.csv") |>
  select(date, meeting_link_ru) |>
  rename(fomc_date = date) |>
  na.omit() |>
  select(fomc_date)


FOMC_months <-
  FOMC_dates |>
  mutate(
    fomc_date = as.Date(fomc_date),
    fomc_yearmonth = yearmonth(fomc_date)
  )


returns_restricted <-
  returns %>%
  filter(index >= "2014-01-01") |>
  na.omit()

common_region <-
  dplyr::left_join(FOMC_dates, returns, by = join_by(fomc_date == index))


lost <- sum(is.na(common_region$CBRFYC3))
notlost <- nrow(common_region) - lost

message(paste0(
  "We have lost ", lost,
  " observations!\nThe total number of observations left is ",
  notlost, "!"
))


common_region_clean <- common_region |> na.omit()


# Monetary and NonMonetary Regions Creaction ------

monetary_region <- as_date(common_region_clean$fomc_date)
nonmonetary_region <- monetary_region - 7


monetary_region_diff_rate <-
  returns_restricted |>
  filter(index %in% monetary_region) |>
  select(-index)

nonmonetary_region_diff_rate <-
  returns_restricted |>
  filter(index %in% nonmonetary_region) |>
  select(-index)


# Monetary Shock Identification (BRW, 2021) ------


## Assumption check ------

variance_inequality <-
  diag(var(monetary_region_diff_rate) > var(nonmonetary_region_diff_rate))


if (prod(variance_inequality) == 0) {
  stop("Variance inequality (the main assumption of estimation) is not satisfied. ")
}

# write.xlsx(bind_rows(
#   monetary_region_diff_rate |> mutate(region = "monetary"),
#   nonmonetary_region_diff_rate |> mutate(region = "nonmonetary")
# ), "regions_diff.xlsx")


## First Stage ----

### Dataset Preparations ------

first_stage_data_no_iv <-
  bind_rows(
    monetary_region_diff_rate,
    nonmonetary_region_diff_rate
  ) |>
  rename(regressor = CBRFYC2)

instruments <-
  bind_rows(
    monetary_region_diff_rate,
    -nonmonetary_region_diff_rate
  ) |>
  rename(instrument = CBRFYC2) |>
  select(instrument)


first_stage_data <- bind_cols(first_stage_data_no_iv, instruments) |>
  relocate(regressor, .after = last_col()) |>
  relocate(instrument, .after = last_col())


### First Stage Start -----


{
  betas <- tibble()

  for (i in 1:(dim(first_stage_data)[2] - 2)) {
    formula_string <- paste0(names(first_stage_data)[i], " ~ regressor| instrument")
    current_formula <- as.formula(formula_string)

    reg <- estimatr::iv_robust(current_formula, data = first_stage_data) |>
      tidy() |>
      filter(term == "regressor")

    reg[1, 1] <- names(first_stage_data)[i]

    betas <- bind_rows(betas, reg)
  }
}

betas <- betas |> mutate(horizon = as.numeric(str_remove(term, "CBRFYC")))
## Second Stage -----

### Preparation ------
# Use ONLY the 1-day changes on the monetary policy announcement days.
# Drop the benchmark rate (CBRFYC2) as instructed by the paper.

second_stage_data <- monetary_region_diff_rate |> select(-CBRFYC2)
beta_vector <- betas$estimate

### Second stage start ------
monetary_shocks <- tibble()

for (i in 1:nrow(second_stage_data)) {
  # Extract the 1-day yield changes for the i-th policy meeting
  yields <- second_stage_data[i, ] |> unlist()

  # Cross-sectional regression of yields on the sensitivity index (betas)
  reg <- lm(yields ~ beta_vector) |> tidy()

  # The exact date of the policy meeting
  meeting_date <- monetary_region[i]

  # Extract the coefficient for beta_vector (which is row 2)
  shock <-
    add_column(reg, date = meeting_date)[2, ] |>
    select(-term)

  monetary_shocks <- bind_rows(monetary_shocks, shock)
}


# diff_returns_monthly |>
#   pivot_longer(cols = -year_month) |>
#   mutate(horizon = as.numeric(str_remove(name, "CBRFYC"))) |>
#   mutate(year = year(year_month), month = month(year_month)) |>
#   filter(year_month %in% FOMC_months$fomc_yearmonth) |>
#   ggplot(aes(y = value, x = horizon, color = year_month, group = year_month)) +
#   geom_line() +
#   geom_hline(aes(yintercept = 0), color = "darkred") +
#   facet_grid(year ~ month, scales = "free", axes = "all")

#
# ggsave(
#   filename = "return_profiles.pdf",
#   width = 420,
#   height = 297,
#   units = "mm",
#   dpi = 300
# )
#

ggplot(betas, aes(x = horizon, y = estimate)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(breaks = breaks_pretty(6))


monetary_shocks_cleaned <-
  monetary_shocks |>
  mutate(year_month = yearmonth(date)) |>
  group_by(year_month) |>
  mutate(estimate = sum(estimate)) |>
  ungroup() |>
  distinct() |>
  mutate(
    estimate_brw =
      ifelse(year_month %in% FOMC_months$fomc_yearmonth, estimate, 0),
    estimate_ref = ifelse(p.value <= 0.01, estimate_brw, 0),
    date = ym(year_month + 1) - 1
  ) |>
  select(date, year_month, estimate_brw, estimate_ref) |>  distinct() 


# pacf(monetary_shocks_cleaned$estimate_ref)

# Results Export -----

write_csv(monetary_shocks_cleaned, "MonetaryShocksBuRogersWu.csv")

message("Monetary shocks estimation completed successfully!
        Results saved to 'MonetaryShocksBuRogersWu.csv'")
