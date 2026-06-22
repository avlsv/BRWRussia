if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# pak::pak("x1o/moexer")
pacman::p_load(here, lubridate, moexer, padr, devtools, data.table, tidytable, tidyverse)



# --- 1. UPDATE PARAMS DATA -----

file_path <- "yc_parameters.csv"
d_till <- query_iss(rest_path = "engines/stock/zcyc")$yearyields.dates$till |> as.Date()

# Load existing data and set d_from, or start from scratch if file is missing
if (file.exists(file_path)) {
  parameters_saved <- read_csv(file_path, show_col_types = FALSE)
  # Get the latest date we already have in the file
  last_date <- max(as.Date(parameters_saved$tradedate), na.rm = TRUE)
  d_from <- last_date + 1
} else {
  parameters_saved <- tibble()
  d_from <- as.Date("2014-01-06")
}

# Only fetch if there are new dates to download
if (d_from <= d_till) {
  date_seq <- seq.Date(d_from, d_till, by = "1 day")
  params_new <- tibble()

  for (i in date_seq) {
    res <- query_iss(
      rest_path = "engines/stock/zcyc",
      params = list(
        date = as.Date(i),
        intraday = as.integer(0)
      )
    )

    # Check if data exists (MOEX implies no params on weekends/trading holidays)
    if (!is.null(res$params) && nrow(res$params) > 0) {
      params_new <- bind_rows(params_new, res$params)
    }

    message(as.Date(i), " finished")
  }

  # Bind with old data and save using write_csv (avoids creating rowname columns)
  if (nrow(params_new) > 0) {
    params_tbl <- bind_rows(parameters_saved, params_new)
    write_csv(params_tbl, file_path)
    message("Data updated and saved to ", file_path)
  } else {
    params_tbl <- parameters_saved
    message("No new trading days found to add.")
  }
} else {
  params_tbl <- parameters_saved
  message("Data is already up to date.")
}


# --- 2. PREPARE THE MODEL CONSTANTS ------

# MOEX constants for the G-Curve (Nelson-Siegel-Svensson splines)
a <- c(0, 0.6)
for (i in seq(2, 8)) {
  a[i + 1] <- a[i] + a[2] * 1.6^(i - 1)
}

b <- c(a[2])
for (i in seq(1, 8)) {
  b[i + 1] <- b[i] * 1.6
}


# --- 3. Calculating yield curve for multiple horizons -----

# Vectorized function - calculates Y(t) for ALL dates in df simultaneously

calc_yield_for_horizon <- function(df, t, a, b) {
  # Dynamically get columns G1 to G9 (MOEX occasionally changes case)
  g_cols <- paste0("G", 1:9)
  if (!all(g_cols %in% names(df))) g_cols <- paste0("g", 1:9)

  G_mat <- as.matrix(df[, g_cols])

  # Part 1 uses the whole dataframe vectors
  part1 <- df$B1 +
    (df$B2 + df$B3) * (df$T1 / t) * (1 - exp(-t / df$T1)) -
    df$B3 * exp(-t / df$T1)

  # Part 2 matrix multiplication: (N x 9 matrix) %*% (9 x 1 matrix) = (N x 1)
  exp_vec <- as.matrix(exp(-(t - a)^2 / (b)^2))
  part2 <- G_mat %*% exp_vec

  rate <- as.numeric(part1 + part2)
  # Return vector of yields
  return(rate)
}


horizons <- c(seq(1/24, 1, by = 1 /12),1,2, seq(1+1/12,4, by=1/6))|> unique()
# c(1/12, 0.25, 0.5, 0.75, 1, 2, 3, 5, 7, 10, 15, 20, 30)

# Calculate the yields for each horizon and merge them into a finished tibble
yield_curves_tbl <- tidytable(tradedate = as.Date(params_tbl$tradedate)) |>
  bind_cols(
    map_dfc(horizons, function(h) {
      # Compute yields for this specific horizon 'h'
      yields <- calc_yield_for_horizon(params_tbl, h, a, b)

      # Assign a dynamic column name, e.g., 'CBRFYC0.25', 'CBRFYC10'
      tibble(!!paste0("CBRFYC", h) := yields)
    })
  )


annualization <- \(x){
  10000 * (exp(x / 10000) - 1)
}


# Saving the results
yield_curves_tbl_an <- yield_curves_tbl |>
  mutate_at(vars(-tradedate), annualization) |>
  mutate_at(vars(-tradedate), \(x)x / 100) |>
  rename(index = tradedate)

write_csv(yield_curves_tbl_an, "yield_curves_1.csv")


## Old code that extracts rates from moex directly (not parameters) -----

#
# if (d_till + 30 > d_from) {
#   message("new data available")
#
#   date_seq <- seq.Date(d_from + 1, d_till, by = "1 day")
#
#   {
#     rates_new_api <- tibble()
#     for (i in date_seq) {
#       rates_api_i <- try(
#         {
#           query_iss(
#             rest_path = "engines/stock/zcyc",
#             params = list(
#               date = as.Date(i),
#               intraday = as.integer(0)
#             )
#           )$yearyields |>
#             mutate(security = paste0("CBRFYC", period)) |>
#             select(-period, -tradetime)
#         },
#         silent = TRUE
#       )
#
#       if (inherits(rates_api_i, "try-error")) {
#         message(paste("Skipping iteration", i, "due to error."))
#         next # Moves to the next i
#       }
#
#       rates_new_api <- bind_rows(rates_new_api, rates_api_i)
#     }
#
#     rates_api_wide <- rates_new_api |>
#       tidyr::pivot_wider(names_from = security, values_from = value) |>
#       rename(index = tradedate) |>
#       arrange(index)
#
#     rates_to_save <-
#       bind_rows(rates_saved, rates_api_wide) |>
#       distinct() |>
#       suppressMessages()
#
#     write_csv(pad(rates_to_save), "YieldCurve.csv")
#   }
# } else {
#   message("no new data available")
# }
