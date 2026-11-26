# ============================================================================
#  Program    : programs/tfl/F-14-2-2.R
#  Output     : F-14-2-2 (figure) -> output/tfl/F-14-2-2.rtf
#  Description: KM plot of the time to first skin disorder
#  Generated  : <masked>
#  The data and content parts are your code where its steps are; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "F-14-2-2"

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
# the report's data: ADTTE (data catalog)

# the ARD of T-14-2-2 (its rows of the study ARD)
ard <- readRDS(file.path(path_ard, "ard.rds")) |>
  filter(output_id == "T-14-2-2")
if (!nrow(ard)) stop("The study ARD has no rows for T-14-2-2: run programs/ard/T-14-2-2.R first.")
# (the definition it was made from: the report records it, #293)
ard_built <- ard_fingerprint("T-14-2-2")

# ---- content: your code ----------------------------------------------------
# your code: leaves `content`
arms <- c("Drug X", "Control")
# adsl_sub: the subjects of the report's analysis set, made before this code
km_df <- adtte[adtte$PARAMCD == "TTSKIN" & adtte$USUBJID %in% adsl_sub$USUBJID, ]
km_df$TRT01A <- factor(km_df$TRT01A, levels = arms)
km_fit <- survfit2(Surv(AVAL, 1 - CNSR) ~ TRT01A, data = km_df)

# the number at risk: the KM table's (T-14-2-2's ARD, `ard`: the
# figure's ARD source) -- checked against the curve's own count
risk_df <- tfl_km_risk(ard, km_fit)
x_breaks <- sort(unique(risk_df$time))

pal <- tfl_colours("treatment", arms)
p_km <- ggsurvfit(km_fit, linewidth = tfl_num("line_width", "km")) +
  add_censor_mark(shape = tfl_marker("censor")$shape,
                  size = tfl_num("censor_size", "km"),
                  stroke = tfl_num("censor_stroke", "km")) +
  geom_hline(yintercept = 0.5, linetype = tfl_setting("median_linetype", "km"),
             colour = tfl_setting("median_colour", "km"),
             linewidth = tfl_num("median_line_width", "km")) +
  scale_colour_manual(values = pal, breaks = names(pal)) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(breaks = seq(0, 1, by = tfl_num("y_by", "km"))) +
  coord_cartesian(xlim = range(x_breaks), ylim = c(0, 1)) +
  labs(x = "Days since first dose",
       y = "Probability without a skin disorder", colour = NULL) +
  theme_tfl("km") + theme(legend.position = "bottom")
p_risk <- ggplot(risk_df, aes(time, factor(strata, levels = rev(arms)),
                              label = n_risk, colour = strata)) +
  geom_text(size = tfl_num("text_size")) +
  scale_colour_manual(values = pal, guide = "none") +
  scale_x_continuous(breaks = x_breaks) +
  coord_cartesian(xlim = range(x_breaks)) +
  labs(title = tfl_setting("risk_title", "km"), x = NULL, y = NULL) +
  theme_void(base_size = tfl_num("base_size")) +
  theme(plot.title = element_text(hjust = 0, size = rel(0.9)),
        axis.text.y = element_text(hjust = 1, margin = margin(r = 5)))
plot <- p_km / p_risk +
  plot_layout(heights = c(1, tfl_num("risk_height", "km")))
tfl_check(plot)
content <- plot

# the figure drawn at its size on the page: 6.5 x 4.5 in (its own size), 300 dpi
content <- fig_sized(content, width_in = 6.5, height_in = 4.5, dpi = 300)
content <- report_content(content)

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/F-14-2-2.yml defines it
doc <- rtf_document(
  page = list(orientation = "landscape"),
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(
    OUTPUT_LABEL = "Figure 14.2.2",
    OUTPUT_TITLE = "Kaplan-Meier Plot of Time to First Treatment-Emergent Skin Disorder",
    OUTPUT_POPULATION = "Safety Analysis Set"
  ),
  file = "output/tfl/F-14-2-2.rtf"
) |>
  rtf_section(
    header = header_standard,
    footer = rtf_footer(
      list(
        c(l = "x = censored.  The number at risk is that of Table 14.2.2."),
        c(l = "Source: tflspec's oncology sample study XYZ-ONC-01 (simulated ADaM data; not real patients)."),
        c(l = "{PROGRAM}       Generated on: {DATETIME}")
      )
    )
  ) |>
  rtf_figures(content)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id, ard = ard_built)

# the end of this run
options(tflplanner.run = NULL)
