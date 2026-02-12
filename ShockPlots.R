library(pacman)

p_load(
  openxlsx, readr, readxl, tsibble, broom,
  systemfonts, ragg, scales, tidyverse, DescTools
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
  summarise(
    var_shock = sqrt(VarCI(estimate_brw, conf.level = 0.90, method = "bonett"))[1],
    lwr.ci = sqrt(VarCI(estimate_brw, conf.level = 0.90, method = "bonett"))[2],
    upr.ci = sqrt(VarCI(estimate_brw, conf.level = 0.90, method = "bonett"))[3]
  )

sqrt(VarCI(MonetaryShocksBuRogersWu$estimate_brw, conf.level = 0.90, method = "bonett"))[1]

shocks_variance_plot <-
  ggplot(shocks_variance_yearly) +
  geom_col(aes(x = date_y, y = var_shock), fill = "#6E8C2F", color = "#2F2926") +
  geom_errorbar(aes(x = date_y, ymin = lwr.ci, ymax = upr.ci), width = 0.2, color = "#2F2926") +
  scale_y_continuous(NULL,
    position = "right",
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_x_continuous(NULL, n.breaks = 7) +
  theme_light() +
  labs(
    title = "стандартные отклонения шоков монетарной политики\nпо годам",
    caption = "шоки посчитатны ЦМИ по методу Bu-Rogers-Wu (2021), 90% доверительный интервал"
  ) +
  theme(
    plot.title = element_text(size = 13),
    # plot.title = element_markdown(size = 12, color = "#404040"),
    text = element_text(size = 13, family = "SB Sans Interface"),
    plot.background = element_rect(fill = "#FDF5E6", color = NA),
    panel.background = element_rect(fill = "#FDF5E6", color = NA),
    legend.position = "top",
    legend.direction = "horizontal",
    # legend.direction = "vertical",
    legend.box.margin = margin(b = -5, l = -4, t = -5),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box = "horizontal",
    legend.justification = "left",
    panel.border = element_blank(),
    axis.line = element_line(colour = "#2F2926"),
    axis.text.x = element_text(colour = "#2F2926"),
    axis.text.y = element_text(colour = "#2F2926"),
    axis.ticks = element_line(colour = "#2F2926"),
    plot.caption = element_text(size = 6, color = "#737373"),
    # panel.grid.major = element_line(color = NA),
    # panel.grid.minor = element_line(color = alpha("#404040", 0.2)),
    panel.grid.major.y = element_line(color = NA),
    panel.grid.minor.y = element_line(color = NA),
    panel.grid.major.x = element_line(color = NA),
    panel.grid.minor.x = element_line(color = NA),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", color = "#2F2926")
  )


shocks_variance_plot

ggsave("ru_shocks_variance_plot.png",
  device = ragg::agg_png,
  shocks_variance_plot,
  width = 9 / 1.6,
  height = 9 / 1.6,
  dpi = 400
)

ggsave("~/Library/CloudStorage/OneDrive-Personal/MACRO/export/ru_shocks_variance_plot.png",
  device = ragg::agg_png,
  shocks_variance_plot,
  width = 9 / 1.6,
  height = 9 / 1.6,
  dpi = 400
)

shocks_quarterly_plot <-
  ggplot(shocks_quarterly, aes(x = date_yq, y = estimate_brw)) +
  geom_col(fill = "#6E8C2F", color = "#404040") +
  scale_y_continuous(NULL, position = "right") +
  scale_x_yearquarter(NULL, date_breaks = "1 year", date_labels = "'%y") +
  theme_light() +
  labs(
    title = "монетарные шоки по кварталам",
    caption = "шоки посчитатны ЦМИ по методу Bu-Rogers-Wu (2021)"
  ) +
  theme(
    plot.title = element_text(size = 13),
    # plot.title = element_markdown(size = 13),
    text = element_text(size = 13, family = "SB Sans Interface"),
    plot.background = element_rect(fill = "#FDF5E6", color = NA),
    panel.background = element_rect(fill = "#FDF5E6", color = NA),
    legend.position = "top",
    legend.direction = "horizontal",
    # legend.direction = "vertical",
    legend.box.margin = margin(b = -5, l = -4, t = -5),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box = "horizontal",
    legend.justification = "left",
    panel.border = element_blank(),
    axis.line = element_line(colour = "#404040"),
    axis.text.x = element_text(colour = "#404040"),
    axis.text.y = element_text(colour = "#404040"),
    axis.ticks = element_line(colour = "#404040"),
    plot.caption = element_text(size = 6, color = "#737373"),
    # panel.grid.major = element_line(color = NA),
    # panel.grid.minor = element_line(color = "gray100"),
    panel.grid.major.y = element_line(color = alpha("#404040", 0.2)),
    panel.grid.minor.y = element_line(color = NA),
    panel.grid.major.x = element_line(color = alpha("#404040", 0.1)),
    panel.grid.minor.x = element_line(color = NA),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", color = "#404040")
  )


ggsave("ru_monetary_shocks_quarterly.png",
  shocks_quarterly_plot,
  device = ragg::agg_png,
  width = 9 / 1.6,
  height = 9 / 1.6,
  dpi = 400
)

ggsave("~/Library/CloudStorage/OneDrive-Personal/MACRO/export/ru_monetary_shocks_quarterly.png",
  shocks_quarterly_plot,
  device = ragg::agg_png,
  width = 9 / 1.6,
  height = 9 / 1.6,
  dpi = 400
)
