# ---- data ------------------------------------------------------------------
df <- adtr |>
  filter(PARAMCD == "BPCHG", FASFL == "Y") |>
  left_join(
    adrs |> filter(PARAMCD == "BOR") |> select(USUBJID, BOR = AVALC),
    by = "USUBJID"
  ) |>
  filter(!is.na(AVAL)) |>
  arrange(desc(AVAL)) |>
  mutate(INDEX = row_number()) |>
  mutate(BOR = factor(BOR, levels = c("CR", "PR", "SD", "PD", "NE")))

# ---- plot ------------------------------------------------------------------
# the response palette: a colour for each value
pal <- tfl_colours("response")

plot <- ggplot() +
  geom_col(data = df, aes(x = INDEX, y = AVAL, fill = BOR), width = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", colour = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(20, -30), linetype = "dashed", colour = "grey", linewidth = 0.5) +
  annotate("text", x = Inf, y = c(20, -30), label = paste0("", c(20, -30), "%"), hjust = -0.3, size = 3.5) +
  scale_fill_manual(values = pal, breaks = names(pal), na.value = "grey80") +
  scale_y_continuous(breaks = seq(-100, 100, by = 20)) +
  coord_cartesian(ylim = c(-100, 100), clip = "off") +
  labs(x = "Patients", y = "Best % Change in Sum of Target Lesion Diameters") +
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
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.98, 0.98),
    legend.justification   = c(1, 1),
    legend.background      = element_rect(colour = "black", fill = "white", linewidth = 0.3)
  ) +
  theme(legend.title = element_blank()) +
  # room for the labels
  theme(plot.margin = margin(5.5, 50, 5.5, 5.5))
