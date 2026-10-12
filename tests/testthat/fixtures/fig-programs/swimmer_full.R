# ---- data ------------------------------------------------------------------
adsl_fas <- adsl |>
  filter(FASFL == "Y") |>
  left_join(
    adrs |> filter(PARAMCD == "BOR") |> select(USUBJID, BOR = AVALC),
    by = "USUBJID"
  ) |>
  filter(!is.na(TRTDURD)) |>
  # days -> months
  mutate(TRTDURD = TRTDURD / days_per_month) |>
  mutate(X0 = 0) |>
  mutate(Y_ID = reorder(USUBJID, TRTDURD)) |>
  mutate(X_ARROW = TRTDURD + max(TRTDURD) * 0.03) |>
  mutate(BOR = factor(BOR, levels = c("CR", "PR", "SD", "PD", "NE")))

ongoing <- adsl_fas |>
  filter(EOSSTT == "ONGOING")

assess <- adrs |>
  filter(PARAMCD == "OVR" & !is.na(ADY)) |>
  inner_join(adsl_fas |> select(USUBJID, Y_ID), by = "USUBJID") |>
  mutate(ADY = ADY / days_per_month)

# ---- plot ------------------------------------------------------------------
# the response_light palette: a colour for each value
pal <- tfl_colours("response_light")

plot <- ggplot() +
  geom_segment(
    data = adsl_fas,
    aes(x = X0, y = Y_ID, xend = TRTDURD, yend = Y_ID, colour = BOR),
    linewidth = 1.2
  ) +
  geom_segment(
    data = ongoing,
    aes(x = TRTDURD, y = Y_ID, xend = X_ARROW, yend = Y_ID),
    linewidth = 0.6,
    arrow = arrow(length = unit(2, "mm"), type = "closed")
  ) +
  geom_point(
    data = assess,
    aes(x = ADY, y = Y_ID, colour = AVALC, group = AVALC),
    shape = 15,
    size = 1.5
  ) +
  geom_point(
    data = adsl_fas,
    aes(x = DTHADY, y = Y_ID),
    shape = 17,
    size = 2.5,
    na.rm = TRUE
  ) +
  scale_colour_manual(values = pal, breaks = names(pal)) +
  labs(x = "Time (Months)", y = "Subject") +
  theme_tfl(theme = "L_axis") +
  theme(legend.position = "right", legend.title = element_blank())
