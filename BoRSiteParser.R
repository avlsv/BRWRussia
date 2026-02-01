if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# Load rvest (for scraping), stringr (for pattern matching/extraction), and dplyr (for piping)
pacman::p_load(rvest, stringr, dplyr, readr, tibble) # tibble added for structured output

# i_am("BRWRussia/MonetaryPolicyShocksEstimation.R")
# setwd(here("BRWRussia"))

# ----------------------------------------------------------------------
# 2. Define target URL and link pattern
# ----------------------------------------------------------------------

target_url <- "https://www.cbr.ru/dkp/cal_mp/"

# Define the link pattern. We use parentheses () around the date part
# (the digits) to CAPTURE them for later extraction.
# Pattern: (digits, length 8) followed by _133000key.htm (or similar structure)
date_capture_pattern <- "(/press/pr/\\?file=)(\\d{8})_(\\d{6}key\\.htm$)"

# The date format is DDMMYYYY (e.g., 24102025)


# ----------------------------------------------------------------------
# 3. Scrape, filter, and extract dates
# ----------------------------------------------------------------------


tryCatch(
  {
    cat("Scraping links from:", target_url, "\n")

    cbr_html <- read_html(target_url)

    all_links <- cbr_html |>
      html_elements("a") |>
      html_attr("href") |>
      unique() # Keep only unique links


    link_matches <- str_match(all_links, date_capture_pattern)
    colnames(link_matches) <- c("full_link", "link_prefix", "date_string_raw", "link_suffix")
    matched_data <- as_tibble(link_matches) |>
      filter(!is.na(full_link)) |>
      select(full_link, date_string_raw)


    if (nrow(matched_data) > 0) {
      final_data <-
        matched_data |>
        mutate(
          day = str_sub(date_string_raw, 1, 2),
          month = str_sub(date_string_raw, 3, 4),
          year = str_sub(date_string_raw, 5, 8),
          extracted_date = as.Date(
            paste(year, month, day, sep = "-"),
            format = "%Y-%m-%d"
          )
        ) |>
        select(extracted_date, full_link) |>
        arrange(extracted_date) |>
        mutate(full_link_cbr = paste0("https://www.cbr.ru", full_link)) |>
        select(extracted_date, full_link_cbr) |>
        rename(
          date = extracted_date,
          meeting_link_ru = full_link_cbr
        )

      message(paste("\nSuccessfully extracted", nrow(final_data), "meetings since 2020-09-18."))

      saved_meetings <- read_csv("BoRMPSchedule.csv", show_col_types = FALSE) |> 
        mutate(date = as.Date(date))

      nrow(saved_meetings)

      
      updated_schedule <-
        bind_rows(saved_meetings, final_data) |>
        unique() |>
        arrange(date)
      
      if (nrow(updated_schedule) > nrow(saved_meetings)) {
        message("New meetings were founded, the BoRMPSchedule.csv is being updated")
        write_csv(updated_schedule, "BoRMPSchedule.csv")
      } else {
        message("No new meeting was founded")
      }
      
    } else {
      message("\nNo links matching the monetary policy meeting pattern were found on the page.\n")
    }
  },
  error = function(e) {
    warning("\nAn error occurred during scraping:\n")
    warning(conditionMessage(e), "\n")
    warning("Please check your internet connection.\n")
  }
)
