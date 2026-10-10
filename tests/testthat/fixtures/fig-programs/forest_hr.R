# ---- data ------------------------------------------------------------------
df <- adtte |>
  filter(PARAMCD == "TTDE", SAFFL == "Y")

est <- ard_stats(ard, c("HR", "HR_SEX", "HR_AGEGR1"), variable = "TRT01A", stats = c("n_obs", "estimate", "conf.low", "conf.high"), by = c("SEX", "AGEGR1"))

# your code
est <- est |>
  # the reference arm has no interval; a subgroup whose model did not
  # converge (no events in an arm) has no point: NE
  filter(!is.na(conf.low)) |>
  filter(TRT01A == "Xanomeline High Dose") |>
  mutate(label = case_when(!is.na(SEX) ~ paste("SEX:", SEX),
                    !is.na(AGEGR1) ~ paste("AGEGR1:", AGEGR1),
                    TRUE ~ "All subjects"),
         ok = is.finite(estimate) & is.finite(conf.low) & is.finite(conf.high) & conf.high < 1000,
         txt = ifelse(ok, sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high), "NE"),
         across(c(estimate, conf.low, conf.high), ~ ifelse(ok, .x, NA)),
         y = rev(row_number()))

# ---- plot ------------------------------------------------------------------
pal <- c(All = tfl_colours("treatment")[[1]])

plot <- ggplot() +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
  geom_errorbar(data = est, aes(y = y, xmin = conf.low, xmax = conf.high), width = 0.25, na.rm = TRUE) +
  geom_point(data = est, aes(x = estimate, y = y), shape = 15, size = 2.5) +
  scale_x_log10() +
  labs(x = "Hazard Ratio (95% CI)") +
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
  theme(legend.position = "none") +
  scale_y_continuous(breaks = est$y, labels = est$label, expand = expansion(add = 0.6)) +
  labs(y = NULL)

p_txt4 <- ggplot(est, aes(y = y)) +
  geom_text(aes(x = 0, label = n_obs), size = 3) +
  geom_text(aes(x = 1, label = txt), size = 3) +
  scale_x_continuous(limits = c(-0.4, 1.6), breaks = c(0, 1), labels = c("N", "Hazard ratio (95% CI)"), position = "top") +
  scale_y_continuous(breaks = est$y, expand = expansion(add = 0.6)) +
  theme_void(base_size = 10) +
  theme(axis.text.x.top = element_text(face = "bold"))

plot <- plot + p_txt4 + plot_layout(widths = c(0.6, 0.4))
