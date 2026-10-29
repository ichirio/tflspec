# ---- data ------------------------------------------------------------------
adtte_ttde <- adtte |>
  filter(PARAMCD == "TTDE", SAFFL == "Y")

# AVAL in days, from each record's unit, AVALU (days when the data has no AVALU)
adtte_ttde <- adtte_ttde |>
  mutate(AVALU = if ("AVALU" %in% names(adtte_ttde)) toupper(AVALU) else "DAYS")
units_unknown <- setdiff(adtte_ttde$AVALU, c("DAYS", "WEEKS", "MONTHS", "YEARS"))
if (length(units_unknown)) {
  stop("AVALU: a unit not DAYS, WEEKS, MONTHS or YEARS: ", toString(units_unknown), call. = FALSE)
}
adtte_ttde <- adtte_ttde |>
  mutate(
    AVAL = AVAL * case_when(
      AVALU == "DAYS"   ~ 1,
      AVALU == "WEEKS"  ~ days_per_week,
      AVALU == "MONTHS" ~ days_per_month,
      AVALU == "YEARS"  ~ days_per_year
    ),
    AVALU = "DAYS"
  )

fit <- survfit2(Surv(AVAL, CNSR == 0) ~ TRT01A, data = adtte_ttde)

# the numbers it prints, from the ARD
ard_label_1 <- paste0(
  "Median (Placebo): ",
  ard_value(ard, "KM", "prob", "estimate", TRT01A = "Placebo", level = 0.5,
            digits = 0),
  " days"
)
ard_label_2 <- paste0(
  "Median (Xanomeline Low Dose): ",
  ard_value(ard, "KM", "prob", "estimate", TRT01A = "Xanomeline Low Dose",
            level = 0.5, digits = 0),
  " days"
)

# ---- plot ------------------------------------------------------------------
# the treatment palette, a colour for each TRT01A
pal <- tfl_colours("treatment", levels(factor(adtte_ttde$TRT01A)))

plot <- ggsurvfit(fit, linewidth = 0.3) +
  add_censor_mark(shape = 3, size = 3, stroke = 0.6) +
  annotate(
    "text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 3,
    label = ard_label_1
  ) +
  annotate(
    "text", x = Inf, y = Inf, hjust = 1.1, vjust = 3, size = 3,
    label = ard_label_2
  ) +
  scale_colour_manual(values = pal, breaks = names(pal)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Time (Days)", y = "Survival Probability") +
  theme_tfl(theme = "boxed") +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.98, 0.98),
    legend.justification   = c(1, 1),
    legend.background      = element_rect(colour = "black", fill = "white", linewidth = 0.3),
    legend.title           = element_blank()
  )
