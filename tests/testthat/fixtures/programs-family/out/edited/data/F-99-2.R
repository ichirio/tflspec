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
# the data: ADVS, ADSL (data catalog)
advs <- readRDS("data/adam/advs.rds")
advs <- blank_as_na(advs)
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)

advs_sub <- advs |>
  order_levels(from = adsl) |>
  filter(PARAMCD == "SYSBP") |>
  left_join(
    adsl |> order_levels() |> select(USUBJID, TRT01A, SAFFL),
    by = "USUBJID"
  ) |>
  filter(SAFFL == "Y", !is.na(CHG) & !is.na(AVISITN)) |>
  mutate(AVISIT = reorder(factor(AVISIT), AVISITN))

sm <- advs_sub |>
  filter(!is.na(CHG)) |>
  group_by(TRT01A, AVISITN, AVISIT) |>
  summarise(n = n(), mean = mean(CHG), sd = sd(CHG), .groups = "drop") |>
  mutate(se = sd / sqrt(n), lo = ifelse(n > 1, mean - qt(0.975, pmax(n - 1, 1)) * se, NA_real_), hi = ifelse(n > 1, mean + qt(0.975, pmax(n - 1, 1)) * se, NA_real_))

# ---- plot ------------------------------------------------------------------
# the treatment palette, a colour for each TRT01A
pal <- tfl_colours("treatment", levels(factor(advs_sub$TRT01A)))
pd <- position_dodge(width = 0.3)

plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = "solid", colour = "grey60",
             linewidth = 0.3) +
  geom_line(
    data = sm,
    aes(x = AVISIT, y = mean, colour = TRT01A, group = TRT01A),
    linewidth = 0.5,
    position = pd
  ) +
  geom_point(
    data = sm,
    aes(x = AVISIT, y = mean, colour = TRT01A, group = TRT01A),
    shape = 16,
    size = 2,
    position = pd
  ) +
  geom_errorbar(
    data = sm,
    aes(x = AVISIT, ymin = lo, ymax = hi, colour = TRT01A),
    width = 0.2,
    position = pd
  ) +
  scale_colour_manual(values = pal, breaks = names(pal)) +
  labs(x = "Visit", y = "Mean (95% CI) CHG of SYSBP") +
  theme_tfl(theme = "boxed") +
  theme(legend.position = "bottom", legend.title = element_blank())

# the figure checks: dropped rows, colours against the standard
tfl_check(plot)

# the figure drawn at its size on the page: 7.3 x 4.2 in (its own size), 300 dpi
plot <- fig_sized(plot, width_in = 7.3, height_in = 4.2, dpi = 300)
