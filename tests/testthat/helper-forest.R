# The forest plot's own ARD (tflplanner #293 phase 6), built from the
# analyses its design brings (attr(design, "analyses")) on a set of ADaM
# data frames (ADSL, ADTTE), as a study's ARD program would.
forest_example_ard <- function(adam, design, output_id = "F1") {
  an <- attr(design, "analyses")
  dir <- exact_dir(adam)
  S <- .ard_spec_sheets
  sh <- function(rows, cols) exact_sheet(lapply(seq_len(nrow(rows)), function(i)
    c(list(output_id = output_id), as.list(rows[i, ]))), cols)
  pop <- an$analysis_data$population_id
  sp <- tfl_ard_spec(list(
    study = exact_sheet(list(list(key = "id", value = "USUBJID")), S$study),
    datasets = exact_sheet(list(list(dataset = "ADSL", path = "adam/ADSL.rds"),
                                list(dataset = "ADTTE", path = "adam/ADTTE.rds")), S$datasets),
    populations = exact_sheet(list(list(population_id = pop, dataset = "ADSL",
                                        where = sprintf("%s == \"Y\"", pop))), S$populations),
    analysis_data = sh(an$analysis_data, S$analysis_data),
    analyses = sh(an$analyses, S$analyses)))
  suppressMessages(suppressWarnings(tfl_build_ard(sp, dir = dir, save = FALSE)))
}
