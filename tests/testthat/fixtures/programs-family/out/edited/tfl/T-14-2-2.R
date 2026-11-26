# ============================================================================
#  Program    : programs/tfl/T-14-2-2.R
#  Output     : T-14-2-2 (table) -> output/tfl/T-14-2-2.rtf
#  Description: Time to first skin disorder: KM estimates
#  Generated  : <masked>
#  The data part makes `data`; the report part is written from spec/.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")

report_id <- "T-14-2-2"

# ---- data: this report's normalized ARD ------------------------------------
# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# this report's rows of the study ARD (made by programs/ard/T-14-2-2.R)
ard <- readRDS(file.path(path_ard, "ard.rds")) |>
  filter(output_id == report_id) |>
  select(-c(output_id, analysis_id, population_id))
if (!nrow(ard)) {
  stop("The study ARD has no rows for ", report_id, ": run programs/ard/T-14-2-2.R first.")
}

# normalize and rework
data <- ard |> normalize_ard()
# row labels: the median, and the event-free probability by day (day 0 left out)
is_time <- data$variable == "time"
data$.label[data$variable == "prob"] <- "Median (95% CI)"
data$.label[is_time] <- paste("Day", as.character(data$variable_level[is_time]))
data <- data[!(is_time & as.character(data$variable_level) == "0"), ]

# ---- report ----------------------------------------------------------------
TRUE
# the table, as spec/outputs/T-14-2-2.yml defines it
# BIGN: the column headings' N, apart from the table's rows
bign <- tflspec::tfl_bign_values(data)
data <- tflspec::tfl_bign_drop(data)

# the code lists: each code in its order (its label, when it has one)
cl_trt01a <- c("Drug X", "Control")

plan <- table_plan(data, cols = "TRT01A", rows = c(group = "variable")) |>
  plan_cells(
    prob = list(
      is.na(estimate) ~ "NE",
      is.na(conf.high) ~ "{estimate:.0f} ({conf.low:.0f}, NE)",
      "{estimate:.0f} ({conf.low:.0f}, {conf.high:.0f})"
    ),
    time = "{estimate:.1f%} ({conf.low:.1f%}, {conf.high:.1f%})",
    default = list(n == 0 ~ "0", "{n:.0f} ({p:.1f%})"),
    na = "-"
  ) |>
  plan_levels(variable = c("prob", "time"), TRT01A = cl_trt01a) |>
  plan_labels(
    variable = c(
      prob = "Time to first event (days)",
      time = "Event-free probability (95% CI)"
    )
  ) |>
  plan_col_header(
    tribble(
      ~line, ~cols,       ~span,  ~text,
      "1",   "row_label", NA,     NA,
      "1",   ".values",   "each", "{col}",
      "2",   "row_label", NA,     NA,
      "2",   ".values",   "each", "(N={n})"
    ),
    values = bign
  ) |>
  plan_stub(name = "row_label", before = TRUE, heading = "") |>
  plan_blanks(
    where = "between_groups",
    first = TRUE,
    last = TRUE,
    counted = TRUE
  ) |>
  plan_columns(widths = c(row_label = 40, .values = 18)) |>
  plan_style(align_count_pct = TRUE)

# the report, as spec/outputs/T-14-2-2.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(
    OUTPUT_LABEL = "Table 14.2.2",
    OUTPUT_TITLE = "Time to First Treatment-Emergent Skin Disorder: Kaplan-Meier Estimates",
    OUTPUT_POPULATION = "Safety Analysis Set"
  ),
  file = "output/tfl/T-14-2-2.rtf"
) |>
  rtf_section(
    header = header_standard,
    footer = rtf_footer(
      list(
        c(l = "Skin disorder: first treatment-emergent adverse event of the SOC Skin and subcutaneous tissue disorders; censored at the end of the treatment-emergent period (30 days after the last dose, death or the data cut-off)."),
        c(l = "Kaplan-Meier estimates; 95% CI of the median by the Brookmeyer-Crowley method (log transformation). NE = not estimable."),
        c(l = "Source: tflspec's oncology sample study XYZ-ONC-01 (simulated ADaM data; not real patients)."),
        c(l = "{PROGRAM}       Generated on: {DATETIME}")
      )
    )
  ) |>
  rtf_tables(plan)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
