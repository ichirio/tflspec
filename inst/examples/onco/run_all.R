# Study XYZ-ONC-01: specs -> programs -> outputs ------------------------------
#
#   1. the analysis data                        prepare_data.R -> data/adam/
#   2. one program per spec, written by tflspec  programs/
#        ard.R          the study ARD (cards / cardx)    spec/ard_spec.xlsx
#        T-14-x-x.R     a table: ARD -> plan -> RTF      spec/tables.xlsx + report.xlsx
#        F-14-x-x.R     a figure (ggplot2)               spec/plot_list.xlsx
#   3. run them                                 output/
#
# The programs need cards, cardx, rtfreporter, dplyr, ggplot2 and ggsurvfit;
# not tflspec.  Run from this folder (a copy of it):
#
#   file.copy(system.file("examples", "onco", package = "tflspec"), ".", recursive = TRUE)
#   setwd("onco"); source("run_all.R")

library(tflspec)

# 1. the analysis data
source("prepare_data.R")
dir.create("programs", showWarnings = FALSE)
dir.create(file.path("output", "ard"), recursive = TRUE, showWarnings = FALSE)

# 2a. the ARD program: every table's analyses; the tables' code lists put
#     the values' order (and labels) on the data first
ard_spec <- tfl_read_ard_spec(file.path("spec", "ard_spec.xlsx"))
tables <- tfl_read_table_spec(file.path("spec", "tables.xlsx"))
writeLines(tfl_ard_code(ard_spec, codelists = tables$codelists),
           file.path("programs", "ard.R"))

# 2b. one program per table: the table's ARD, the plan, the report
books <- file.path("spec", c("report.xlsx", "tables.xlsx"))
for (id in tables$tables$output_id) {
  spec <- tfl_read_report_spec(books, output_id = id)
  writeLines(c(
    sprintf("# %s: %s", id, spec$titles$center[spec$titles$line == "2"]),
    "library(rtfreporter)",
    "",
    "# ---- data: this table's rows of the study ARD",
    'ard <- readRDS("output/ard/ard.rds")',
    sprintf('ard <- ard[ard$output_id == "%s", ]', id),
    tfl_normalize_code(spec, ard_spec, id),
    "",
    "# ---- table",
    tfl_table_code(spec, pipe = "|>"),
    "",
    "# ---- report",
    tfl_report_code(spec, content = "plan"),
    sprintf('generate_rtfreport(doc, "%s", overwrite = TRUE)', tfl_report_path(spec))),
    file.path("programs", paste0(id, ".R")))
}

# 2c. one program per figure: they read the data as the other programs do,
#     and save the figure in output/
options(tflspec.data_expr = 'readRDS("data/adam/{ds}.rds")',
        tflspec.fig_path  = 'file.path("output", "{plot_id}.png")')
names(adam) <- toupper(names(adam))     # the groups' values and colours
tfl_fig_list_code(file.path("spec", "plot_list.xlsx"), adam = adam, dir = "programs")

# 3. run every program, the ARD first
for (f in c("ard.R", setdiff(list.files("programs", pattern = "[.]R$"), "ard.R"))) {
  message("running programs/", f)
  source(file.path("programs", f), local = new.env())
}
