# Packages ------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}





pacman::p_load(
  here, tidyverse, openxlsx,
  lubridate, readxl,
  estimatr, tsibble, broom
)




rates <- read_csv(here::here("data/yield_curves.csv")) |>
  mutate(index = as.Date(index)) |>
  filter(!if_all(!index, is.na)) |>
  suppressMessages() |>
  select(-2)


returns <- rates |>
  mutate(across(-index, \(x) c(NA, diff(x, lag = 1))))


FOMC_dates <- read_csv(here::here("data/BoRMPSchedule.csv")) |>
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

    fit <- estimatr::iv_robust(current_formula, data = first_stage_data) |>
      tidy()

    reg <- fit |> filter(term == "regressor")

    reg[1, 1] <- names(first_stage_data)[i]

    # The constant alpha_i of eq. (4) is needed in the second stage, eq. (5)
    reg$alpha <- fit$estimate[fit$term == "(Intercept)"]

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
alpha_vector <- betas$alpha

stopifnot(identical(betas$term, names(second_stage_data)))


### Second stage start ------
monetary_shocks <- tibble()

for (i in 1:nrow(second_stage_data)) {
  # Extract the 1-day yield changes for the i-th policy meeting
  yields <- second_stage_data[i, ] |> unlist()

  # Cross-sectional regression of yields on the sensitivity index (betas), eq. (5) of
  # Bu, Rogers, Wu (FEDS 2019-043): dR_i,t = alpha_i + e_t * beta_i + v_i,t.
  # The constants are the first-stage alpha_i, not a new constant for each meeting:
  # with betas between 0.9 and 1.4 such a constant absorbs most of the move
  reg <- lm(I(yields - alpha_vector) ~ 0 + beta_vector) |> tidy()

  # The exact date of the policy meeting
  meeting_date <- monetary_region[i]

  # Extract the coefficient for beta_vector
  shock <-
    add_column(reg, date = meeting_date) |>
    filter(term == "beta_vector") |>
    select(-term)

  monetary_shocks <- bind_rows(monetary_shocks, shock)
}

# 
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
  select(date, year_month, estimate_brw) |>  distinct() 


# Every month from the first decision to the last day of yield data:
# 0 where there was no key rate decision, NA where every decision of the month fell on a day
# without yield data (the MOEX bond market was closed in late February and March 2022)

lost_months <-
  common_region |>
  filter(is.na(CBRFYC3)) |>
  mutate(year_month = yearmonth(fomc_date)) |>
  filter(!year_month %in% monetary_shocks_cleaned$year_month) |>
  pull(year_month)

all_months <- tibble(
  year_month = seq(min(monetary_shocks_cleaned$year_month), yearmonth(max(rates$index)), by = 1)
)

monetary_shocks_cleaned <-
  all_months |>
  left_join(select(monetary_shocks_cleaned, year_month, estimate_brw), by = "year_month") |>
  mutate(
    estimate_brw = case_when(
      !is.na(estimate_brw) ~ estimate_brw,
      year_month %in% lost_months ~ NA_real_,
      TRUE ~ 0
    ),
    date = ym(year_month + 1) - 1
  ) |>
  select(date, year_month, estimate_brw)


# pacf(monetary_shocks_cleaned$estimate_ref)

# Results Export -----

write_csv(monetary_shocks_cleaned, here::here("output/MonetaryShocksBuRogersWu.csv"))

write.xlsx(monetary_shocks_cleaned, here::here("output/MonetaryShocksBuRogersWu.xlsx"))

message("Monetary shocks estimation completed successfully!
        Results saved to output/MonetaryShocksBuRogersWu.csv")
