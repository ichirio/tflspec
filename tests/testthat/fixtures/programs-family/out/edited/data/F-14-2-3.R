# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- data ------------------------------------------------------------------
# ---- code lists: each value in its order, as it is in the data ----
# (a value a list does not have stops the program: add it to the list)
cl_trt01a <- c("Drug X", "Control")

# the ARD of T-14-2-2 (its rows of the study ARD)
ard <- readRDS("output/ard/ard.rds") |>
  filter(output_id == "T-14-2-2")
if (!nrow(ard)) stop("The study ARD has no rows for T-14-2-2: run programs/ard/T-14-2-2.R first.")
# (the definition it was made from: the report records it, #293)
ard_built <- ard_fingerprint("T-14-2-2")

# the data: ADTTE (data catalog)
adtte <- readRDS("data/adam/adtte.rds")
adtte <- blank_as_na(adtte)

adtte_sub <- adtte |>
  order_levels() |>
  filter(PARAMCD == "TTSKIN", SAFFL == "Y") |>
  tte_time("days") |>
  set_levels(TRT01A = cl_trt01a)

fit <- survfit2(Surv(AVAL, CNSR == 0) ~ TRT01A, data = adtte_sub)

# the numbers it prints, from the ARD
ard_label_1 <- paste0(
  "Median (days), Drug X: ",
  ard_value(ard, "KM", "prob", "estimate", TRT01A = "Drug X", level = 0.5,
            digits = 0)
)
ard_label_2 <- paste0(
  "Median (days), Control: ",
  ard_value(ard, "KM", "prob", "estimate", TRT01A = "Control", level = 0.5,
            digits = 0)
)
ard_label_3 <- paste0(
  "HR, Drug X vs Control: ",
  ard_value(ard, "HR", "TRT01A", "estimate", level = "Drug X", digits = 2),
  " (95% CI ",
  ard_value(ard, "HR", "TRT01A", "conf.low", level = "Drug X", digits = 2),
  ", ",
  ard_value(ard, "HR", "TRT01A", "conf.high", level = "Drug X", digits = 2),
  ")"
)

# ---- plot ------------------------------------------------------------------
# the treatment palette, a colour for each TRT01A
pal <- tfl_colours("treatment", levels(factor(adtte_sub$TRT01A)))

plot <- ggsurvfit(fit, linewidth = 0.3) +
  add_censor_mark(shape = 4, size = 3, stroke = 0.6) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey50",
             linewidth = 0.3) +
  annotate(
    "text", x = 0, y = -Inf, hjust = 0, vjust = -2.1, size = 3,
    label = ard_label_1
  ) +
  annotate(
    "text", x = 0, y = -Inf, hjust = 0, vjust = -0.6, size = 3,
    label = ard_label_2
  ) +
  annotate(
    "text", x = Inf, y = 0.15, hjust = 1.02, vjust = -0.6, size = 3,
    label = ard_label_3
  ) +
  scale_colour_manual(values = pal, breaks = names(pal)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Time (Days)", y = "Probability of No Skin Disorder") +
  theme_tfl(theme = "boxed") +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.98, 0.98),
    legend.justification   = c(1, 1),
    legend.background      = element_rect(colour = "black", fill = "white", linewidth = 0.3),
    legend.title           = element_blank()
  )

# the figure checks: dropped rows, colours against the standard
tfl_check(plot)

# the figure drawn at its size on the page: 8.33 x 4.79 in (its own size), 300 dpi
plot <- fig_sized(plot, width_in = 8.33, height_in = 4.79, dpi = 300)
