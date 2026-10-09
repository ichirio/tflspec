# ---- data ------------------------------------------------------------------
# ---- code lists: each value in its order, as it is in the data ----
# (a value a list does not have stops the program: add it to the list)
cl_trta <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

df <- advs |>
  filter(PARAMCD == "SYSBP") |>
  left_join(
    adsl |> select(USUBJID, TRTA, SAFFL),
    by = "USUBJID"
  ) |>
  filter(SAFFL == "Y", !is.na(AVAL) & !is.na(AVISITN)) |>
  mutate(AVISIT = reorder(factor(AVISIT), AVISITN)) |>
  set_levels(TRTA = cl_trta)

sm <- df |>
  filter(!is.na(AVAL)) |>
  group_by(TRTA, AVISITN, AVISIT) |>
  summarise(n = n(), mean = mean(AVAL), sd = sd(AVAL), .groups = "drop") |>
  mutate(se = sd / sqrt(n), lo = mean - se, hi = mean + se)

# ---- plot ------------------------------------------------------------------
# the treatment palette, a colour for each TRTA
pal <- tfl_colours("treatment", levels(droplevels(factor(df$TRTA))))
pd <- position_dodge(width = 0.3)

plot <- ggplot() +
  geom_line(data = sm, aes(x = AVISIT, y = mean, colour = TRTA, group = TRTA), linewidth = 0.5, position = pd) +
  geom_point(data = sm, aes(x = AVISIT, y = mean, colour = TRTA, group = TRTA), shape = 16, size = 2, position = pd) +
  geom_errorbar(data = sm, aes(x = AVISIT, ymin = lo, ymax = hi, colour = TRTA), width = 0.2, position = pd) +
  scale_colour_manual(values = pal, breaks = names(pal)) +
  labs(x = "Visit", y = "Mean (+/- SE) SYSBP") +
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
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())
