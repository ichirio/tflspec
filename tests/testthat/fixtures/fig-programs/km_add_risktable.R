# ---- data ------------------------------------------------------------------
df <- adtte |>
  filter(PARAMCD == "OS", FASFL == "Y") |>
  # days -> months
  mutate(AVAL = AVAL / 30.4375)

fit <- survfit2(Surv(AVAL, CNSR == 0) ~ TRT01P, data = df)

x_breaks <- pretty(c(0, max(fit$time)))

# ---- plot ------------------------------------------------------------------
# the treatment palette, a colour for each TRT01P
pal <- tfl_colours("treatment", levels(droplevels(factor(df$TRT01P))))

plot <- ggsurvfit(fit, linewidth = 0.3) +
  add_censor_mark(shape = 4, size = 3, stroke = 0.6) +
  geom_hline(yintercept = 0.5, linetype = "twodash", colour = "grey50", linewidth = 0.3) +
  add_risktable(times = x_breaks, risktable_stats = "n.risk", size = 3) +
  scale_colour_manual(values = pal, breaks = names(pal)) +
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
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.98, 0.98),
    legend.justification   = c(1, 1),
    legend.background      = element_rect(colour = "black", fill = "white", linewidth = 0.3)
  ) +
  theme(legend.title = element_blank())
