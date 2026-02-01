if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, here, lubridate, moexer, padr)

#setwd("~/Documents/BRWRussia")

rates_saved <- read_csv("YieldCurve.csv") |>
  mutate(index = as.Date(index)) |>
  suppressMessages()

d_from <- max(rates_saved$index)
d_till <- query_iss(rest_path = "engines/stock/zcyc")$yearyields.dates$till

if (d_till > d_from) {
  message("new data available")

  date_seq <- seq.Date(d_from + 1, d_till, by = "1 day")

  {
    rates_new_api <- tibble()
    for (i in date_seq) {
      rates_api_i <- try(
        {
          query_iss(
            rest_path = "engines/stock/zcyc",
            params = list(
              date = as.Date(i),
              intraday = as.integer(0)
            )
          )$yearyields |>
            mutate(security = paste0("CBRFYC", period)) |>
            select(-period, -tradetime)
        },
        silent = TRUE
      )

      if (inherits(rates_api_i, "try-error")) {
        message(paste("Skipping iteration", i, "due to error."))
        next # Moves to the next i
      }

      rates_new_api <- bind_rows(rates_new_api, rates_api_i)
    }

    rates_api_wide <- rates_new_api |>
      tidyr::pivot_wider(names_from = security, values_from = value) |>
      rename(index = tradedate) |>
      arrange(index)

    rates_to_save <-
      bind_rows(rates_saved, rates_api_wide) |>
      distinct() |>
      suppressMessages()

    write_csv(pad(rates_to_save), "YieldCurve.csv")
  }
} else {
  message("no new data available")
}
