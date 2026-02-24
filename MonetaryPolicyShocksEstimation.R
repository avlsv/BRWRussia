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
#setwd("~/Documents/BRWRussia")




rates <- read_csv("YieldCurve.csv", ) |>
  mutate(index = as.Date(index)) |>
  filter(!if_all(!index, is.na)) |> suppressMessages()



returns <- rates |>
  mutate(across(-index, \(x) c(NA, diff(x,lag=1))))



FOMC_dates <- read_csv("BoRMPSchedule.csv") |>
  select(date, meeting_link_ru)|> 
  rename(fomc_date=date) |>
  na.omit() |>
  select(fomc_date)



FOMC_months <-
  FOMC_dates |>
  mutate(
    fomc_date = as.Date(fomc_date),
    fomc_yearmonth = yearmonth(fomc_date)
  )


returns_restricted <- returns %>%
  filter(index >= "2014-01-01") |>
  na.omit()

common_region <-
  dplyr::left_join(FOMC_dates, returns, by = join_by(fomc_date == index))


lost <- sum(is.na(common_region$CBRFYC3))
notlost <- nrow(common_region) - lost

message(paste0("We have lost ", lost,
               " observations!\nThe total number of observations left is ", 
               notlost, "!"))


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

betas

## Second Stage -----

### Preparation ------

diff_returns_monthly <-
  returns_restricted |>
  mutate(year_month = yearmonth(index)) |>
  group_by(year_month) |>
  summarise(across(-index, sum)) |>
  select(-CBRFYC2)

beta_vector <- betas$estimate


### Second stage start ------


{
  monetary_shocks <- tibble()
  for (i in 1:dim(diff_returns_monthly)[1]) {
    yields <- diff_returns_monthly[i, ] |>
      select(-year_month) |>
      unlist()

    reg <- lm(yields ~ beta_vector) |> tidy()

    year_month_i <- diff_returns_monthly$year_month[i]

    shock <-
      add_column(reg, year_month = year_month_i)[2, ] |>
      select(-term)

    monetary_shocks <- bind_rows(monetary_shocks, shock)
  }
}


monetary_shocks_cleaned <-
  monetary_shocks |>
  mutate(
    estimate_brw =
      ifelse(year_month %in% FOMC_months$fomc_yearmonth, estimate, 0),
    date = ym(year_month + 1) - 1
  ) |>
  select(date,year_month, estimate_brw)


# Results Export -----

write_csv(monetary_shocks_cleaned, "MonetaryShocksBuRogersWu.csv")

message("Monetary shocks estimation completed successfully! 
        Results saved to 'MonetaryShocksBuRogersWu.csv'")
