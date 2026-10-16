# Study XYZ-ONC-01: the specs -------------------------------------------------
#
# Writes the workbooks in spec/ once; from then on the workbooks are what
# the study edits (in Excel or in tflplanner), and run_all.R reads them.
#
#   spec/ard_spec.xlsx   the analyses: one cards / cardx call a row
#   spec/tables.xlsx     how each ARD becomes a table
#   spec/report.xlsx     the page, header, footer, titles and footnotes
#   spec/plot_list.xlsx  the figures, one row each

library(tflspec)

df <- function(...) data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
dir.create("spec", showWarnings = FALSE)

# ---- ARD spec ------------------------------------------------------------------
ard_spec <- tfl_ard_spec(list(
  study = df(key = c("id", "output"),
             value = c("USUBJID", "output/ard/ard.rds")),
  datasets = df(dataset = c("ADSL", "ADAE", "ADRS"), level = "ADaM",
                path = c("data/adam/adsl.rds", "data/adam/adae.rds",
                         "data/adam/adrs.rds")),
  populations = df(population_id = c("ITT", "SAF"), dataset = "ADSL",
                   where = c('ITTFL == "Y"', 'SAFFL == "Y"'),
                   label = c("Intent-to-Treat Set", "Safety Analysis Set")),
  analysis_data = df(
    output_id = c("T-14-2-1", "T-14-3-1"),
    data_id = c("adrs_bor", "adae_teae"),
    label = c("Best confirmed overall response, ITT", "TEAEs of the safety set"),
    from = c("ADRS", "ADAE"),
    population_id = c("ITT", "SAF"),
    where = c('PARAMCD == "BOR"', 'TRTEMFL == "Y"'),
    derive = c('RESP = AVALC %in% c("CR", "PR") | DCR = AVALC %in% c("CR", "PR", "SD")', NA)),
  analyses = df(
    output_id = c(rep("T-14-1-1", 3), rep("T-14-2-1", 4), rep("T-14-3-1", 2)),
    analysis_id = c("ARMN", "CONT", "CAT",
                    "ARMN", "BOR", "ORR", "DCR",
                    "ARMN", "TEAE"),
    label = c("Subjects", "Age, baseline sum of diameters", "Demographic categories",
              "Subjects", "Best overall response", "Objective response rate", "Disease control rate",
              "Subjects", "TEAEs by SOC and PT"),
    method = c("categorical", "continuous", "categorical",
               "categorical", "categorical", "proportion_ci", "proportion_ci",
               "categorical", "hierarchical"),
    dataset = c(rep(NA, 9)),
    data = c(NA, NA, NA, NA, "adrs_bor", "adrs_bor", "adrs_bor", NA, "adae_teae"),
    population_id = c("SAF", "SAF", "SAF", "ITT", NA, NA, NA, "SAF", NA),
    by = c(NA, "TRT01A", "TRT01A", NA, "TRT01P", "TRT01P", "TRT01P", NA, "TRT01A"),
    variables = c("TRT01A", "AGE | BSLD", "AGEGR1 | SEX | RACE | ECOGBL",
                  "TRT01P", "AVALC", "RESP", "DCR",
                  "TRT01A", "AEBODSYS | AEDECOD"),
    denominator = c(rep(NA, 8), "SAF"),
    args = c(rep(NA, 5), 'method = "clopper-pearson"', 'method = "clopper-pearson"',
             NA, "over_variables = TRUE"))))
tfl_write_ard_spec(ard_spec, "spec/ard_spec.xlsx")

# ---- table spec ----------------------------------------------------------------
# One row a table on `tables`; the other sheets keyed by output_id.  The
# arms are the data's factor levels (Drug X first), so no sheet says them.
ids <- c("T-14-1-1", "T-14-2-1", "T-14-3-1")
two_lines <- function(id, stub1, stub2) df(
  output_id = id, line = c("1", "1", "2", "2"),
  cols = c("row_label", ".values", "row_label", ".values"),
  span = c("", "each", "", "each"),
  text = c(stub1, "{col}", stub2, "(N={n})"))
tables <- tfl_table_spec(list(
  study = df(key = "rounding", value = "sas"),
  tables = df(output_id = ids,
              cols = c("TRT01A", "TRT01P", "TRT01A"),
              rows = c("group = variable", "group = variable", "group1 = AEBODSYS"),
              label = c("", "", "label = AEDECOD"),
              sort = c("", "", ".overall | group1 | .depth | -n | label"),
              overall_label = c("", "", "Subjects with at least one TEAE")),
  variables = df(
    output_id = c(rep("T-14-1-1", 6), rep("T-14-2-1", 3)),
    variable = c("AGE", "BSLD", "AGEGR1", "SEX", "RACE", "ECOGBL",
                 "AVALC", "RESP", "DCR"),
    label = c("Age (years)", "Baseline sum of diameters (mm)", "Age group, n (%)",
              "Sex, n (%)", "Race, n (%)", "Baseline ECOG performance status, n (%)",
              "Best overall response, n (%)", "Objective response rate (CR + PR)",
              "Disease control rate (CR + PR + SD)"),
    order = c(1:6, 1:3),
    levels = c("", "", "<65 | >=65", "", "", "0 | 1", "", "", "")),
  codelists = df(
    output_id = c("T-14-1-1", "T-14-1-1", rep("T-14-2-1", 5)),
    variable = c("SEX", "SEX", rep("AVALC", 5)),
    value = c("M", "F", "CR", "PR", "SD", "PD", "NE"),
    label = c("Male", "Female", "Complete response (CR)", "Partial response (PR)",
              "Stable disease (SD)", "Progressive disease (PD)", "Not evaluable (NE)"),
    order = c("1", "2", "1", "2", "3", "4", "5")),
  cells = df(
    output_id = c(rep("T-14-1-1", 4), "", "T-14-2-1", "T-14-2-1"),
    variable = c(rep("continuous", 4), "categorical", "proportion_ci", "proportion_ci"),
    row = c("n", "Mean (SD)", "Median", "Min, Max", "", "n (%)", "95% CI"),
    template = c("{N}", "{mean} ({sd})", "{median}", "{min}, {max}", "{n} ({p:.1f%})",
                 "{n} ({estimate:.1f%})", "[{conf.low:.1f%}, {conf.high:.1f%}]"),
    digits = c("0", "1,2", "1", "0", "", "", "")),
  layout = df(output_id = ids, stub_name = "row_label", stub_before = "TRUE",
              blank_where = "between_groups", pages_max_rows = c("40", "40", "30"),
              pages_split = c("group_safe", "group_safe", "group_force")),
  columns = df(output_id = rep(ids, each = 2), column = rep(c("row_label", ".values"), 3),
               rel_width = c("3", "1", "3", "1", "4", "1")),
  col_header = rbind(two_lines("T-14-1-1", "", "Characteristic"),
                     two_lines("T-14-2-1", "", "Response"),
                     two_lines("T-14-3-1", "System organ class", "  Preferred term"))))
tfl_write_table_spec(tables, "spec/tables.xlsx")

# ---- report spec ---------------------------------------------------------------
# The study's running header and footer once (a blank output_id); each
# report's titles and footnotes.
titles <- list(
  "T-14-1-1" = c("Table 14.1.1", "Demographic and Baseline Characteristics", "Safety Analysis Set"),
  "T-14-2-1" = c("Table 14.2.1", "Best Overall Response, Objective Response Rate and Disease Control Rate",
                 "Intent-to-Treat Set"),
  "T-14-3-1" = c("Table 14.3.1", "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
                 "Safety Analysis Set"))
notes <- list(
  "T-14-1-1" = "SD: standard deviation; ECOG: Eastern Cooperative Oncology Group.",
  "T-14-2-1" = c("Best overall response by RECIST 1.1, investigator assessment; CR and PR confirmed at least 4 weeks later.",
                 "CI: exact (Clopper-Pearson) 95% confidence interval."),
  "T-14-3-1" = c("A subject is counted once in a system organ class and once in a preferred term.",
                 "Treatment-emergent: onset from the first dose to 30 days after the last dose."))
report <- tfl_table_spec(list(
  study = df(key = "output_path", value = "output"),
  report = df(output_id = c("", ids), type = c("table", "", "", ""),
              file = c("{output_id}.rtf", "", "", ""),
              program = c("{output_id}", "", "", "")),
  header = df(output_id = "", line = c("1", "2"),
              left = c("Example Pharma", "Study XYZ-ONC-01"),
              right = c("Clinical Study Report", "Page {PAGE} of {TOTAL_PAGES}")),
  footer = df(output_id = "", line = "1", left = "{PROGRAM}", right = "Data cut-off: 30JUN2026"),
  titles = df(output_id = rep(ids, each = 3), line = rep(c("1", "2", "3"), 3),
              center = unlist(titles, use.names = FALSE)),
  footnotes = df(output_id = rep(ids, lengths(notes)),
                 line = unlist(lapply(lengths(notes), function(k) as.character(seq_len(k)))),
                 left = unlist(notes, use.names = FALSE))))
tfl_write_report_spec(report, "spec/report.xlsx")

# ---- plot list -------------------------------------------------------------------
# One row a figure (the older sheet-based figure spec): its type and style,
# the parameter, the group, the population flag and its own arguments.
# tfl_fig_list_template() adds the sheets of styles and arguments, and
# reads the groups' values and colours from the data.  The KM plots draw
# AVAL as it is (time_unit "days" means no conversion): ADTTE is in months.
source("prepare_data.R")     # the data, as the programs will read them
q <- function(x) paste0("'", x, "'")
plots <- df(
  plot_id = c("F-14-2-1", "F-14-2-2", "F-14-2-3", "F-14-2-4", "F-14-2-5", "F-14-2-6"),
  type    = c("km", "km", "forest", "waterfall", "swimmer", "individual"),
  style   = c("risk_table", "risk_table", "hr", "response", "full", "spider"),
  title   = c("Kaplan-Meier Plot of Progression-Free Survival",
              "Kaplan-Meier Plot of Overall Survival",
              "Progression-Free Survival: Hazard Ratio by Subgroup",
              "Best Percent Change from Baseline in Sum of Diameters",
              "Time on Treatment and Response, Confirmed Responders",
              "Percent Change from Baseline in Sum of Diameters over Time"),
  param   = c("PFS", "OS", "PFS", "BPCHG", NA, "SUMDIAM"),
  group   = c("TRT01P", "TRT01P", "ARMCMP", NA, NA, NA),
  pop     = c("ITTFL", "ITTFL", "ITTFL", "EFFFL", "RSPFL", "EFFFL"),
  legend  = NA,
  args    = c('time_unit = "days", x_label = "Time (Months)", x_by = 3',
              'time_unit = "days", x_label = "Time (Months)", x_by = 3',
              'subgroups = c("AGEGR1", "SEX", "ECOGBL", "REGION1")',
              'y_label = "Best change from baseline in sum of diameters (%)"',
              'events = c(Death = "DTHADY"), ongoing = c(EOTSTT = "ONGOING"), x_by = 3',
              'value = "PCHG"'))
names(adam) <- toupper(names(adam))
tfl_fig_list_template(adam, "spec/plot_list.xlsx", rows = plots)
