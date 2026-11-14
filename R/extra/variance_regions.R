pacman::p_load(
  here, openxlsx, readr, readxl, tsibble, broom,
  systemfonts, ragg, scales, tidyverse, DescTools
)

regions_diff <- read_excel(here::here("data/regions_diff.xlsx"))
regions_diff_long <- regions_diff |>
  pivot_longer(-region) |>
  mutate(horizon = sub("CBRFYC", "", name) |> as.numeric())


ggplot(regions_diff_long, aes(x = value, color = region)) +
  geom_density(trim = T, kernel = "epanechnikov", adjust = 2) +
  facet_wrap(~horizon)
