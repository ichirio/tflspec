# ============================================================================
#  Program    : programs/tfl/F-14-2-1.R
#  Output     : F-14-2-1 (figure) -> output/tfl/F-14-2-1.rtf
#  Description: Mean change from baseline in systolic blood pressure
#  Generated  : <masked>
#  The data and content parts are your code where its steps are; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "F-14-2-1"

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

# the report's subjects: its analysis set (its data's population)
adsl <- order_levels(adsl)

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

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/F-14-2-1.yml defines it
doc <- rtf_document(
  page = list(orientation = "landscape"),
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(
    OUTPUT_LABEL = "Figure 14.2.1",
    OUTPUT_TITLE = "Mean (SE) Change from Baseline in Systolic Blood Pressure over Time",
    OUTPUT_POPULATION = "Safety Analysis Set"
  ),
  file = "output/tfl/F-14-2-1.rtf"
) |>
  rtf_section(
    header = header_standard,
    footer = rtf_footer(
      list(
        c(l = "Vertical bars: mean +/- SE.  Systolic blood pressure on day 1 of each 21-day cycle."),
        c(l = "Source: tflspec's oncology sample study XYZ-ONC-01 (simulated ADaM data; not real patients)."),
        c(l = "{PROGRAM}       Generated on: {DATETIME}")
      )
    )
  ) |>
  rtf_figures(content)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
