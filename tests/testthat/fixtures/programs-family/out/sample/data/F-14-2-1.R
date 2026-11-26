# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- data: the figure's data -----------------------------------------------
# the report's data: ADSL, ADVS (data catalog)
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)
advs <- readRDS("data/adam/advs.rds")
advs <- blank_as_na(advs)

# the report's subjects: its analysis set (its data's population)
adsl <- readRDS("data/adam/adsl.rds") |>
  blank_as_na() |>
  order_levels()

# adsl_sub: the subjects. population SAF (Safety Analysis Set)
adsl_sub <- filter(adsl, SAFFL == "Y")

# ---- content: your code ----------------------------------------------------
# your code: leaves `content`
# adsl_sub: the subjects of the report's analysis set, made before this code
saf <- adsl_sub$USUBJID
d <- advs[advs$PARAMCD == "SYSBP" & advs$USUBJID %in% saf &
            advs$AVISITN > 0 & advs$AVISITN <= 8 & !is.na(advs$CHG), ]
m <- aggregate(CHG ~ TRTA + AVISITN, data = d, FUN = function(x)
  c(mean = mean(x), se = stats::sd(x) / sqrt(length(x))))
m <- data.frame(m[c("TRTA", "AVISITN")], m$CHG)
m$CYCLE <- m$AVISITN + 1  # AVISITN 1 is cycle 2 day 1
arms <- c("Drug X", "Control")
m$TRTA <- factor(m$TRTA, levels = arms)
plot <- ggplot(m, aes(CYCLE, mean, colour = TRTA)) +
  geom_hline(yintercept = 0, colour = tfl_setting("zero_line_colour"),
             linewidth = tfl_num("line_width")) +
  geom_line(position = position_dodge(width = 0.6), linewidth = tfl_num("line_width")) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.4,
                position = position_dodge(width = 0.6)) +
  scale_colour_tfl("treatment", arms) +
  scale_x_continuous(breaks = sort(unique(m$CYCLE))) +
  labs(x = "Cycle (day 1)", y = "Mean change from baseline (mmHg)",
       colour = NULL) +
  theme_tfl() + theme(legend.position = "bottom")
tfl_check(plot)
content <- plot

# the figure drawn at its size on the page: 6.5 x 4.5 in (its own size), 300 dpi
content <- fig_sized(content, width_in = 6.5, height_in = 4.5, dpi = 300)
content <- report_content(content)
