# ---- data ------------------------------------------------------------------
adtte_os <- adtte |>
  filter(PARAMCD == "OS", FASFL == "Y") |>
  # days -> months
  mutate(AVAL = AVAL / days_per_month)

fit <- survfit2(Surv(AVAL, CNSR == 0) ~ TRT01P, data = adtte_os)

x_breaks <- pretty(c(0, max(fit$time)))

# the number at risk at the x axis's breaks, from the fit
sr <- summary(fit, times = x_breaks, extend = TRUE)
risk <- data.frame(
  time   = sr$time,
  strata = sub("^[^=]*=", "", as.character(sr$strata)),
  n_risk = sr$n.risk
) |>
  mutate(strata = factor(strata, levels = rev(levels(factor(adtte_os$TRT01P)))))

# ---- plot ------------------------------------------------------------------
# the treatment palette, a colour for each TRT01P
pal <- tfl_colours("treatment", levels(factor(adtte_os$TRT01P)))

plot <- ggsurvfit(fit, linewidth = 0.3) +
  add_censor_mark(shape = 4, size = 3, stroke = 0.6) +
  geom_hline(yintercept = 0.5, linetype = "twodash", colour = "grey50",
             linewidth = 0.3) +
  scale_colour_manual(values = pal, breaks = names(pal)) +
  scale_x_continuous(
    breaks = x_breaks,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  coord_cartesian(xlim = range(x_breaks), ylim = c(0, 1)) +
  labs(x = "Time (Months)", y = "Survival Probability") +
  theme_tfl(theme = "boxed") +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.98, 0.98),
    legend.justification   = c(1, 1),
    legend.background      = element_rect(colour = "black", fill = "white", linewidth = 0.3),
    legend.title           = element_blank()
  )

p_risk <- ggplot(risk, aes(x = time, y = strata, label = n_risk, colour = strata)) +
  geom_text(size = 3) +
  scale_colour_manual(values = pal, guide = "none") +
  scale_x_continuous(
    breaks = x_breaks,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_cartesian(xlim = range(x_breaks), clip = "off") +
  labs(title = "Number of Patients at Risk", x = NULL, y = NULL) +
  theme_void(base_size = 10) +
  theme(
    plot.title          = element_text(hjust = 0, size = rel(0.9)),
    plot.title.position = "plot",
    axis.text.y         = element_text(hjust = 1, margin = margin(r = 5))
  )

plot <- plot / p_risk + plot_layout(heights = c(0.833, 0.167))
