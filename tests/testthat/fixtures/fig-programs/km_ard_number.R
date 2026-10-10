# ---- data ------------------------------------------------------------------
df <- adtte |>
  filter(PARAMCD == "TTDE", SAFFL == "Y")

fit <- survfit2(Surv(AVAL, CNSR == 0) ~ TRT01A, data = df)

# ---- plot ------------------------------------------------------------------
# the treatment palette, a colour for each TRT01A
pal <- tfl_colours("treatment", levels(droplevels(factor(df$TRT01A))))

plot <- ggsurvfit(fit, linewidth = 0.3) +
  add_censor_mark(shape = 4, size = 3, stroke = 0.6) +
  annotate(
    "text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 3,
    label = paste0("Median (Placebo): ", ard_value(ard, "KM", "prob", "estimate", TRT01A = "Placebo", level = 0.5, digits = 0), " days")
  ) +
  annotate(
    "text", x = Inf, y = Inf, hjust = 1.1, vjust = 3, size = 3,
    label = paste0("Median (Xanomeline Low Dose): ", ard_value(ard, "KM", "prob", "estimate", TRT01A = "Xanomeline Low Dose", level = 0.5, digits = 0), " days")
  ) +
  scale_colour_manual(values = pal, breaks = names(pal)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Time (Days)", y = "Survival Probability") +
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
