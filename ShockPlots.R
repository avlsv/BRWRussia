library(pacman)

p_load(
  openxlsx, readr, readxl, tsibble, broom, ragg, scales, tidyverse, DescTools, knitr
)


MonetaryShocksBuRogersWu <- read_csv("MonetaryShocksBuRogersWu.csv")


shocks_quarterly <-
  MonetaryShocksBuRogersWu |>
  mutate(
    date_ym = yearmonth(date),
    date_yq = yearquarter(date)
  ) |>
  group_by(date_yq) |>
  summarise(estimate_brw = sum(estimate_brw))



shocks_variance_yearly <-
  MonetaryShocksBuRogersWu |>
  mutate(date_y = year(date)) |>
  group_by(date_y) |>
  filter(date_y < 2026) |>
  summarise(
    var_shock = sqrt(VarCI(estimate_brw, conf.level = 0.95, method = "bonett"))[1],
    lwr.ci = sqrt(VarCI(estimate_brw, conf.level = 0.95, method = "bonett"))[2],
    upr.ci = sqrt(VarCI(estimate_brw, conf.level = 0.95, method = "bonett"))[3]
  )


shocks_variance_plot <-
  ggplot(shocks_variance_yearly) +
  geom_col(aes(x = date_y, y = var_shock), fill = "#6E8C2F", color = "#2F2926") +
  geom_errorbar(aes(x = date_y, ymin = lwr.ci, ymax = upr.ci), width = 0.2, color = "#2F2926") +
  scale_y_continuous(NULL,
    position = "right",
    expand = expansion(mult = c(0.005, 0.05))
  ) +
  scale_x_continuous(NULL, n.breaks = 7) +
  labs(
    title = "стандартные отклонения шоков монетарной политики\nпо годам",
    caption = "шоки посчитаны ЦМИ по методу Bu-Rogers-Wu (2021), 95% доверительный интервал Bonett (2006)"
  ) +
  theme_light()+
  theme(
    # --- 1. OVERALL PLOT & PANEL BACKGROUND ---
    plot.background  = element_rect(fill = "#FDF5E6", color = NA),
    panel.background = element_rect(fill = "#FDF5E6", color = NA),
    panel.border     = element_blank(),
    plot.margin      = unit(c(0.5, 0.5, 0.5, 0.5), "in"),
    
    # --- 2. GLOBAL TEXT & TITLES ---
    text          = element_text(size = 10, color = "#2F2926", family = "SB Sans Interface"),
    plot.title    = element_text(size = 11, color = "#2F2926", hjust = 0, margin = margin(b = 2)), 
    plot.subtitle = element_text(size = 10, color = "#2F2926"),
    plot.caption  = element_text(size = 6, color = "#737373"),
    
    # --- 3. AXES (LINES, TICKS, & TEXT) ---
    axis.title        = element_text(color = "#2c291d", size = 10),
    axis.text         = element_text(color = "#2F2926", size = 10),
    axis.text.x       = element_text(margin = margin(t = 8)),
    axis.text.y.left  = element_text(margin = margin(r = 5)),
    axis.text.y.right = element_text(margin = margin(l = 5)),
    axis.line.x       = element_line(color = "#2F2926"),
    axis.line.y       = element_line(color = "#2F2926"),
    axis.ticks.x      = element_line(color = "#2E2A25", linewidth = 0.4),
    axis.ticks.y      = element_line(color = "#2E2A25", linewidth = 0.4),
    axis.ticks.length = unit(-4, "pt"), # Negative value makes ticks point inward
    
    # --- 4. GRID LINES ---
    panel.grid.major.y = element_line(color = alpha("#2F2926", 0)),
    panel.grid.minor.y = element_line(color = alpha("#2F2926", 0)),
    panel.grid.major.x = element_line(color = alpha("#2F2926", 0)),
    panel.grid.minor.x = element_line(color = alpha("#2F2926", 0)),
    
    # --- 5. LEGEND ---
    legend.position      = "top",
    legend.justification = c(0, 1), 
    legend.direction     = "horizontal",
    legend.box           = "horizontal",
    legend.background    = element_blank(),
    legend.key           = element_blank(),
    legend.key.width     = unit(1.2, "line"), 
    legend.spacing.x     = unit(0.1, "cm"), 
    legend.margin        = margin(t = 0, b = 0, l = 0, r = 0),
    legend.box.margin    = margin(t = 0, b = 5, l = 0, r = 0), 
    legend.text          = element_text(size = 10, color = "#2F2926"), 
    
    # --- 6. FACETING (STRIPS) ---
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", color = "#2F2926")
  )


shocks_variance_plot

ggsave("ru_shocks_variance_plot.png",
  device = ragg::agg_png,
  shocks_variance_plot,
  width = 5.25,
  height = 5.25,
  dpi = 400
)

ggsave("~/Library/CloudStorage/OneDrive-Personal/MACRO/export/ru_shocks_variance_plot.png",
  device = ragg::agg_png,
  shocks_variance_plot,
  width = 5.25,
  height = 5.25,
  dpi = 400
)


shocks_quarterly_plot <-
  ggplot(shocks_quarterly, aes(x = date_yq, y = estimate_brw)) +
  geom_col(fill = "#6E8C2F", color = "#404040") +
  scale_y_continuous("п.п.", position = "right") +
  scale_x_yearquarter(NULL, date_breaks = "1 year", date_labels = "'%y") +
  labs(
    title = "монетарные шоки, сумма по кварталам",
    caption = "шоки посчитаны ЦМИ по методу Bu-Rogers-Wu (2021)\nразмер шока эквивалентен изменению кривой доходности на горизонте 2х лет"
  ) +
  theme_light()+
  theme(
    # --- 1. OVERALL PLOT & PANEL BACKGROUND ---
    plot.background  = element_rect(fill = "#FDF5E6", color = NA),
    panel.background = element_rect(fill = "#FDF5E6", color = NA),
    panel.border     = element_blank(),
    plot.margin      = unit(c(0.5, 0.5, 0.5, 0.5), "in"),
    
    # --- 2. GLOBAL TEXT & TITLES ---
    text          = element_text(size = 10, color = "#2F2926", family = "SB Sans Interface"),
    plot.title    = element_text(size = 11, color = "#2F2926", hjust = 0, margin = margin(b = 2)), 
    plot.subtitle = element_text(size = 10, color = "#2F2926"),
    plot.caption  = element_text(size = 6, color = "#737373"),
    
    # --- 3. AXES (LINES, TICKS, & TEXT) ---
    axis.title        = element_text(color = "#2c291d", size = 10),
    axis.text         = element_text(color = "#2F2926", size = 10),
    axis.text.x       = element_text(margin = margin(t = 8)), 
    axis.text.y.left  = element_text(margin = margin(r = 5)),
    axis.text.y.right = element_text(margin = margin(l = 5)),
    axis.line.x       = element_line(color = "#2F2926"),
    axis.line.y       = element_line(color = "#2F2926"),
    axis.ticks.x      = element_line(color = "#2E2A25", linewidth = 0.4),
    axis.ticks.y      = element_line(color = "#2E2A25", linewidth = 0.4),
    axis.ticks.length = unit(-4, "pt"), # Negative value makes ticks point inward
    
    # --- 4. GRID LINES ---
    panel.grid.major.y = element_line(color = alpha("#2F2926", 0)),
    panel.grid.minor.y = element_line(color = alpha("#2F2926", 0)),
    panel.grid.major.x = element_line(color = alpha("#2F2926", 0)),
    panel.grid.minor.x = element_line(color = alpha("#2F2926", 0)),
    
    # --- 5. LEGEND ---
    legend.position      = "top",
    legend.justification = c(0, 1), 
    legend.direction     = "horizontal",
    legend.box           = "horizontal",
    legend.background    = element_blank(),
    legend.key           = element_blank(),
    legend.key.width     = unit(1.2, "line"), 
    legend.spacing.x     = unit(0.1, "cm"), 
    legend.margin        = margin(t = 0, b = 0, l = 0, r = 0),
    legend.box.margin    = margin(t = 0, b = 5, l = 0, r = 0), 
    legend.text          = element_text(size = 10, color = "#2F2926"), 
    
    # --- 6. FACETING (STRIPS) ---
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", color = "#2F2926")
  )




ggsave("ru_monetary_shocks_quarterly.png",
  shocks_quarterly_plot,
  device = ragg::agg_png,
  width = 5.25,
  height = 5.25,
  dpi = 400
)

ggsave("~/Library/CloudStorage/OneDrive-Personal/MACRO/export/ru_monetary_shocks_quarterly.png",
  shocks_quarterly_plot,
  device = ragg::agg_png,
  width = 5.25,
  height = 5.25,
  dpi = 400
)






shocks_monthly_plot <-
  ggplot(MonetaryShocksBuRogersWu, aes(x = yearmonth(year_month), y = estimate_brw)) +
  geom_col(fill = "#6E8C2F", color = "#404040", linewidth = 0.2) +
  scale_y_continuous("п.п.", position = "right") +
  scale_x_yearmonth(NULL, date_breaks = "1 year", date_labels = "'%y") +
  labs(
    title = "монетарные шоки по месяцам",
    caption = "шоки посчитаны ЦМИ по методу Bu-Rogers-Wu (2021)\nразмер шока эквивалентен изменению кривой доходности на горизонте 2х лет"
  ) +
  theme_light()+
  theme(
    # --- 1. OVERALL PLOT & PANEL BACKGROUND ---
    plot.background  = element_rect(fill = "#FDF5E6", color = NA),
    panel.background = element_rect(fill = "#FDF5E6", color = NA),
    panel.border     = element_blank(),
    plot.margin      = unit(c(0.5, 0.5, 0.5, 0.5), "in"),
    
    # --- 2. GLOBAL TEXT & TITLES ---
    text          = element_text(size = 10, color = "#2F2926", family = "SB Sans Interface"),
    plot.title    = element_text(size = 11, color = "#2F2926", hjust = 0, margin = margin(b = 2)), 
    plot.subtitle = element_text(size = 10, color = "#2F2926"),
    plot.caption  = element_text(size = 6, color = "#737373"),
    
    # --- 3. AXES (LINES, TICKS, & TEXT) ---
    axis.title        = element_text(color = "#2c291d", size = 10),
    axis.text         = element_text(color = "#2F2926", size = 10),
    axis.text.x       = element_text(margin = margin(t = 8)), 
    axis.text.y.left  = element_text(margin = margin(r = 5)),
    axis.text.y.right = element_text(margin = margin(l = 5)),
    axis.line.x       = element_line(color = "#2F2926"),
    axis.line.y       = element_line(color = "#2F2926"),
    axis.ticks.x      = element_line(color = "#2E2A25", linewidth = 0.4),
    axis.ticks.y      = element_line(color = "#2E2A25", linewidth = 0.4),
    axis.ticks.length = unit(-4, "pt"), # Negative value makes ticks point inward
    
    # --- 4. GRID LINES ---
    panel.grid.major.y = element_line(color = alpha("#2F2926", 0)),
    panel.grid.minor.y = element_line(color = alpha("#2F2926", 0)),
    panel.grid.major.x = element_line(color = alpha("#2F2926", 0)),
    panel.grid.minor.x = element_line(color = alpha("#2F2926", 0)),
    
    # --- 5. LEGEND ---
    legend.position      = "top",
    legend.justification = "left",
    legend.direction     = "horizontal",
    legend.box           = "horizontal",
    legend.background    = element_blank(),
    legend.key           = element_blank(),
    legend.key.width     = unit(1.5, "line"), # Adjust to match shorter Python legend lines
    legend.box.margin    = margin(b = -5, l = -4, t = -5),
    legend.text          = element_text(size = 10, color = "#2F2926"), 
    
    # --- 6. FACETING (STRIPS) ---
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", color = "#2F2926")
  )




ggsave("ru_monetary_shocks_monthly.png",
       shocks_monthly_plot,
       device = ragg::agg_png,
       width = 5.25,
       height = 5.25,
       dpi = 400
)

ggsave("~/Library/CloudStorage/OneDrive-Personal/MACRO/export/ru_monetary_shocks_monthly.png",
       shocks_monthly_plot,
       device = ragg::agg_png,
       width = 5.25,
       height = 5.25,
       dpi = 400
)


table_for_print <-
  MonetaryShocksBuRogersWu |>
  mutate(year_month = yearmonth(year_month)) |>
  filter(year_month > yearmonth("2025-03-01"), estimate_brw != 0) |>
  select(year_month, estimate_brw) |>
  mutate(estimate_brw = round(estimate_brw, 2)) |>
  mutate(shock = case_when(
    estimate_brw > 0 ~ 
      paste0("<span style='color:#8A0D2F;'>+", estimate_brw, "%</span>"),
    estimate_brw < 0 ~ 
      paste0("<span style='color:#355E3B;'>–", -estimate_brw, "%</span>"),
    TRUE ~ as.character(estimate_brw)
  )) |>
  mutate(date = paste(
    year(year_month),
    lubridate::month(year_month, label = T, abbr = T, locale = "ru_RU"),
    sep = " "
  )) |>
  select(-year_month, -estimate_brw) |>
  relocate(date) |>
  rename(`монетарный шок` = shock, `месяц заседания` = date)

md_table <- kable(table_for_print, format = "pipe")

# 3. Save it to your working directory
writeLines(md_table, "last_monetary_shocks.md")
