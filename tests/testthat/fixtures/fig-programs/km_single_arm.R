# ---- data ------------------------------------------------------------------
df <- adtte |>
  filter(PARAMCD == "OS", FASFL == "Y") |>
  # days -> months
  mutate(AVAL = AVAL / 30.4375)

fit <- survfit2(Surv(AVAL, CNSR == 0) ~ 1, data = df)

x_breaks <- pretty(c(0, max(fit$time)))

# the number at risk at the x axis's breaks, from the fit
sr <- summary(fit, times = x_breaks, extend = TRUE)
risk <- data.frame(
  time   = sr$time,
  strata = "All",
  n_risk = sr$n.risk
) |>
  mutate(strata = factor(strata))

# ---- plot ------------------------------------------------------------------
pal <- c(All = tfl_colours("treatment")[[1]])

plot <- ggsurvfit(fit, linewidth = 0.3, colour = pal[[1]]) +
  add_censor_mark(shape = 4, size = 3, stroke = 0.6, colour = pal[[1]]) +
  geom_hline(yintercept = 0.5, linetype = "twodash", colour = "grey50", linewidth = 0.3) +
  scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  coord_cartesian(xlim = range(x_breaks), ylim = c(0, 1)) +
  labs(x = "Time (Months)", y = "Survival Probability") +
  theme_minimal(base_size = 10) +
  theme(
    axis.title        = element_text(size = 10),
    axis.text         = element_text(size = 8),
    legend.text       = element_text(size = 9),
    legend.key.size   = unit(0.7, "lines"),
    panel.border      = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid        = element_blank(),
    axis.ticks        = element_line(linewidth = 0.4),
    axis.ticks.length = unit(2, "mm")
  ) +
  theme(legend.position = "none")

p_risk <- ggplot(risk, aes(x = time, y = strata, label = n_risk, colour = strata)) +
  geom_text(size = 3) +
  scale_colour_manual(values = pal, guide = "none") +
  scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0.02, 0.02))) +
  coord_cartesian(xlim = range(x_breaks), clip = "off") +
  labs(title = "Number of Patients at Risk", x = NULL, y = NULL) +
  theme_void(base_size = 10) +
  theme(
    plot.title          = element_text(hjust = 0, size = rel(0.9)),
    plot.title.position = "plot",
    axis.text.y         = element_text(hjust = 1, margin = margin(r = 5))
  )

plot <- plot / p_risk + plot_layout(heights = c(0.833, 0.167))
