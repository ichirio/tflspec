# ============================================================================
#  The AI kit: what tflspec_ai_manual(dir) writes for an AI chat
# ============================================================================
#
#  A chat model cannot read the user's disk: the user attaches files, and
#  many chats take ten at most and no folders.  So the kit is one flat
#  folder of eight files -- the request, the manual, the TOC / shell guide,
#  every spec sheet blank, a blank figure design, and a made-up study end to
#  end (its TOC and shells, the specs written from them, its figure).
#  Everything is written when asked: the blank sheets come from the column
#  catalog (inst/spec/columns.csv) and the example from the code below, so
#  neither can drift from what this version reads.

.ai_kit_files <- c(
  start    = "00_START_HERE.md",
  manual   = "01_tflspec-ai-user-manual.md",
  guide    = "02_tflspec-ai-toc-shell-guide.md",
  blank    = "03_blank_specs.xlsx",
  blank_fig = "04_blank_figure.yml",
  toc      = "05_example_toc_shell.xlsx",
  specs    = "06_example_specs.xlsx",
  figure   = "07_example_figure.yml",
  metadata = "08_adam_metadata.csv")

# ---- the column catalog as a sheet ------------------------------------------

# What each column of the kit's sheets means, one row a column: the spec it
# belongs to first, so a reader of one workbook holding every spec can tell
# them apart.  `kinds`: the specs the workbook holds.
.ai_columns_sheet <- function(kinds = c("ard", "table", "report", "listing")) {
  ks <- .spec_kind_sheets()
  cols <- tfl_spec_columns()
  out <- list()
  for (k in kinds) for (sh in ks[[k]]) {
    d <- tfl_spec_columns(sh)
    d <- d[!is.na(d$column) & nzchar(d$column), , drop = FALSE]
    if (!nrow(d)) next
    out[[length(out) + 1L]] <- data.frame(
      spec = k, sheet = sh, column = d$column, description = d$description,
      example = ifelse(is.na(d$example), "", d$example), stringsAsFactors = FALSE)
  }
  wb <- cols[cols$sheet %in% c("(workbook)", "study", "about") &
               !is.na(cols$column) & nzchar(cols$column), , drop = FALSE]
  common <- data.frame(spec = "every", sheet = wb$sheet, column = wb$column,
                       description = wb$description,
                       example = ifelse(is.na(wb$example), "", wb$example),
                       stringsAsFactors = FALSE)
  rbind(common, do.call(rbind, out))
}

# Adds sheets after the spec sheets of a workbook tflspec wrote (`_columns`,
# `_assumptions` ...): their names start with _, so no reader takes them
# as part of the definition.
.ai_add_sheets <- function(path, sheets) {
  wb <- openxlsx::loadWorkbook(path)
  for (nm in names(sheets)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, sheets[[nm]], keepNA = FALSE)
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
    openxlsx::setColWidths(wb, nm, seq_along(sheets[[nm]]), "auto")
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

# ---- the blank specs ----------------------------------------------------------

.ai_blank_specs <- function() {
  list(ard = tfl_ard_spec_template(), table = tfl_table_spec(),
       report = tfl_table_spec(), listing = tfl_listing_spec())
}

# Writes the blank specs: one workbook (`03_blank_specs.xlsx`), or with
# `split` one a spec (`03a_blank_ard_spec.xlsx` ...).  Returns the paths.
.ai_write_blank <- function(dir, split = FALSE) {
  b <- .ai_blank_specs()
  if (!split) {
    p <- file.path(dir, .ai_kit_files[["blank"]])
    tfl_write_specs(p, ard = b$ard, table = b$table, report = b$report,
                    listing = b$listing)
    .ai_add_sheets(p, list(`_columns` = .ai_columns_sheet()))
    return(p)
  }
  out <- character()
  for (i in seq_along(b)) {
    k <- names(b)[i]
    p <- file.path(dir, sprintf("03%s_blank_%s_spec.xlsx", letters[i], k))
    args <- list(p); args[[k]] <- b[[k]]
    do.call(tfl_write_specs, args)
    .ai_add_sheets(p, list(`_columns` = .ai_columns_sheet(k)))
    out <- c(out, p)
  }
  out
}

# The blank figure design: every piece and field of tfl_fig_parts(), as
# comments under the four keys a design has.
.ai_blank_figure <- function() {
  p <- tfl_fig_parts()
  one <- function(x) gsub("[\r\n]+", " ", x)
  block <- function(sec, key) {
    d <- p[p$section == sec, , drop = FALSE]
    out <- character()
    for (pc in unique(d$piece)) {
      f <- d[d$piece == pc, , drop = FALSE]
      out <- c(out, sprintf("# - %s: %s -- %s", key, pc, one(f$piece_label[1L])),
               sprintf("#     %s: %s%s%s", f$field, one(f$label),
                       ifelse(is.na(f$default) | !nzchar(f$default), "",
                              paste0(" (default ", f$default, ")")),
                       ifelse(is.na(f$choices) | !nzchar(f$choices), "",
                              paste0(" [", f$choices, "]"))))
    }
    out
  }
  c("# A figure design, blank: one .yml a figure (tfl_read_fig_design()).",
    "# Start from a template (tfl_fig_templates()) and write only the fields",
    "# you set; every field is listed below as a comment.",
    "# Your guesses go under `assumptions:` (a list of strings) at the end.",
    "template:",
    "data:            # the steps, in order; `read` first",
    block("data", "step"),
    "plot:            # the figure's settings",
    sprintf("#   %s: %s", p$field[p$section == "plot"], one(p$label[p$section == "plot"])),
    "layers:          # what is drawn, in order",
    block("layers", "layer"),
    "assumptions: []")
}

# ---- the example: a made-up study, XYZ-001 ------------------------------------
#
# Four outputs on the CDISC pilot data (pharmaverseadam): T-1 demographics,
# T-2 treatment-emergent AEs by SOC and PT, L-1 serious AEs, F-1 mean
# systolic blood pressure by visit.  The TOC and shells are what a company
# would hand over; the specs are what the guide says to write from them.

.ai_df <- function(...) data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)

.ai_example_ard <- function() {
  list(
    study = .ai_df(key = c("id", "output"),
                   value = c("USUBJID", "output/ard/ard.rds")),
    datasets = .ai_df(dataset = c("ADSL", "ADAE", "ADVS"), level = "ADaM",
                      path = c("data/adam/adsl.rds", "data/adam/adae.rds",
                               "data/adam/advs.rds")),
    populations = .ai_df(population_id = "SAF", dataset = "ADSL",
                         where = "SAFFL == \"Y\"", label = "Safety Analysis Set"),
    # T-1: the Total column's N is the analysis set's (total_n)
    analyses = .ai_df(
      output_id = c("T-1", "T-1", "T-1", "T-2"),
      analysis_id = c("N", "AGE", "CAT", "AE"),
      label = c("Subjects", "Age (years)", "Sex and age group", "TEAEs by SOC and PT"),
      method = c("total_n", "continuous", "categorical", "hierarchical"),
      dataset = c("ADSL", "ADSL", "ADSL", "ADAE"),
      population_id = "SAF",
      where = c("", "", "", "TRTEMFL == \"Y\""),
      by = c("", "TRT01A", "TRT01A", "TRT01A"),
      overall = c("", "TRUE", "TRUE", ""),
      variables = c("", "AGE", "SEX | AGEGR1", "AEBODSYS | AEDECOD"),
      # the subjects with at least one TEAE: the row above the SOCs
      args = c("", "", "", "over_variables = TRUE")))
}

.ai_example_table <- function() {
  arms <- "Placebo | Xanomeline Low Dose | Xanomeline High Dose"
  hdr <- function(id, stub1, stub2) .ai_df(
    output_id = id, line = c("1", "1", "2", "2"),
    cols = c("row_label", ".values", "row_label", ".values"),
    span = c("", "each", "", "each"),
    text = c(stub1, "{col}", stub2, "(N={n})"))
  list(
    study = .ai_df(key = "rounding", value = "sas"),
    tables = .ai_df(output_id = c("T-1", "T-2"), cols = "TRT01A",
                    rows = c("group = variable", "group1 = AEBODSYS"),
                    label = "",
                    # the any-TEAE row first, then the SOCs alphabetically,
                    # PTs by the total of the arms within a SOC (the
                    # carrier .sort_stat hidden on the columns sheet)
                    sort = c("", ".overall | group1 | .depth | -.sort_stat | label"),
                    sort_stat = c("", "n"),
                    overall_label = c("", "Subjects with at least one TEAE"),
                    total = c("Total", "")),
    # the arms in the shells' order (a column key's levels), then T-1's
    # variables
    variables = .ai_df(output_id = c("T-1", "T-2", "T-1", "T-1", "T-1"),
                       variable = c("TRT01A", "TRT01A", "AGE", "SEX", "AGEGR1"),
                       label = c("", "", "Age (years)", "Sex, n (%)", "Age group, n (%)"),
                       order = c("", "", "1", "2", "3"),
                       levels = c(rep(arms, 2), "", "", "18-64 | >64")),
    codelists = .ai_df(output_id = "T-1", variable = "SEX", value = c("M", "F"),
                       label = c("Male", "Female"), order = c("1", "2")),
    cells = .ai_df(
      output_id = c(rep("T-1", 5), "T-2"),
      variable = c(rep("continuous", 4), "categorical", ""),
      row = c("n", "Mean (SD)", "Median", "Min, Max", "", ""),
      template = c("{N}", "{mean} ({sd})", "{median}", "{min}, {max}",
                   "{n} ({p:.1f%})", "{n} ({p:.1f%})"),
      digits = c("0", "1,2", "1", "0", "", "")),
    layout = .ai_df(output_id = c("T-1", "T-2"), blank_where = "between_groups",
                    stub_name = "row_label", stub_before = "TRUE"),
    columns = .ai_df(output_id = "T-2", column = c("row_label", ".values", ".sort_stat"),
                     rel_width = c("3", "1", ""), hide = c("", "", "TRUE")),
    col_header = rbind(hdr("T-1", "", "Parameter"),
                       hdr("T-2", "System Organ Class", "  Preferred Term")))
}

.ai_example_report <- function() {
  ids <- c("T-1", "T-2", "L-1", "F-1")
  t1 <- c("Table 14.1.1", "Table 14.3.1", "Listing 16.2.7", "Figure 14.2.1")
  t2 <- c("Demographic Characteristics",
          "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
          "Serious Adverse Events",
          "Mean (SE) Systolic Blood Pressure by Visit")
  list(
    report = .ai_df(output_id = ids, type = c("table", "table", "listing", "figure")),
    header = .ai_df(output_id = "", line = "1", left = "XYZ-001",
                    right = "Page {PAGE} of {TOTAL_PAGES}"),
    footer = .ai_df(output_id = "", line = "1", left = "{PROGRAM}",
                    right = "Generated on: {DATETIME}"),
    titles = .ai_df(output_id = rep(ids, each = 3), line = rep(c("1", "2", "3"), 4),
                    center = as.vector(rbind(t1, t2, "Safety Analysis Set"))),
    footnotes = .ai_df(output_id = "T-2", line = c("1", "2"),
                       left = c("A subject is counted once in a system organ class and once in a preferred term.",
                                "Coded with MedDRA.")))
}

.ai_example_listing <- function() {
  list(
    listings = .ai_df(output_id = "L-1", type = "multiline", dataset = "ADAE",
                      where = "SAFFL == \"Y\" & AESER == \"Y\"",
                      sort = "USUBJID | ASTDY"),
    listing_cols = .ai_df(
      output_id = "L-1",
      vars = c("USUBJID", "TRT01A", "AEBODSYS | AEDECOD", "ASTDY", "AESEV", "AEOUT"),
      label = c("Subject", "Treatment", "System Organ Class /\\nPreferred Term",
                "Start\\nDay", "Severity", "Outcome"),
      sep = c("", "", "\" / \"", "", "", ""),
      collapse_repeats = c("TRUE", "", "", "", "", "")))
}

.ai_example_figure <- function() {
  d <- tfl_fig_template("mean_se", data = "ADVS", param = "SYSBP", pop = "SAFFL",
                        group = "TRTA")
  # ADVS has the arm and the flag itself: no join
  d$data <- Filter(function(s) !identical(s$step, "join"), d$data)
  d$plot$x_label <- "Visit"
  d$plot$y_label <- "Mean (SE) Systolic Blood Pressure (mmHg)"
  d
}

# The guesses written in the example, one row each: the model of the
# `_assumptions` sheet the guide asks for.
.ai_example_assumptions <- function() {
  .ai_df(
    output_id = c("", "", "", "T-1", "T-1", "T-2", "T-2", "T-2", "L-1", "L-1", "F-1", "F-1"),
    sheet = c("populations", "datasets", "variables", "codelists", "tables", "analyses",
              "analyses", "tables", "listing_cols", "listings", "data", "data"),
    item = c("SAF", "paths", "TRT01A levels", "SEX", "cols", "AE denominator", "AE where",
             "cols", "columns", "where", "param", "summary"),
    assumption = c("SAFFL == \"Y\" on ADSL",
                   "data/adam/<dataset>.rds",
                   "the arms as the data name them, in the shells' order",
                   "the data code sex as M / F",
                   "the arm is TRT01A (actual treatment)",
                   "the analysis set per column (SAF)",
                   "treatment-emergent is TRTEMFL == \"Y\"",
                   "the arm on ADAE is TRT01A",
                   "USUBJID, TRT01A, AEBODSYS / AEDECOD, ASTDY, AESEV, AEOUT",
                   "serious is AESER == \"Y\"; SAFFL is on ADAE",
                   "systolic blood pressure is PARAMCD SYSBP",
                   "the mean and its standard error of AVAL at each AVISIT"),
    question = c("Is the safety set flagged by SAFFL?",
                 "Where are the datasets, and in which format?",
                 "Are these the arms' values in the data?",
                 "Please give the code list of SEX.",
                 "Actual or planned treatment for the demographics?",
                 "Percentages of the analysis set, or of subjects with any AE?",
                 "Is TRTEMFL the treatment-emergent flag?",
                 "Actual or planned treatment for the AE table?",
                 "Are these the variables behind the shell's columns?",
                 "Is SAFFL on ADAE, or should the listing keep ADSL's safety subjects?",
                 "Is the parameter code SYSBP?",
                 "Observed values (AVAL) or change from baseline (CHG)?"))
}

# ---- the example's TOC and shells ---------------------------------------------

.ai_example_toc <- function() {
  rep_ <- .ai_example_report()
  t <- rep_$titles
  title <- function(id, k) t$center[t$output_id == id & t$line == k]
  ids <- rep_$report$output_id
  fn <- function(id, k) {
    f <- rep_$footnotes$left[rep_$footnotes$output_id == id]
    if (length(f) >= k) f[[k]] else ""
  }
  rows <- list(c("", "", "14.1 Demographic and baseline characteristics", "", "", "", ""),
               c("T-1", "", title("T-1", "1"), title("T-1", "2"), "Safety Analysis Set", "", ""),
               c("", "", "14.2 Vital signs", "", "", "", ""),
               c("F-1", "", title("F-1", "1"), title("F-1", "2"), "Safety Analysis Set", "", ""),
               c("", "", "14.3 Adverse events", "", "", "", ""),
               c("T-2", "", title("T-2", "1"), title("T-2", "2"), "Safety Analysis Set",
                 fn("T-2", 1), fn("T-2", 2)),
               c("", "", "16.2 Subject data listings", "", "", "", ""),
               c("L-1", "", title("L-1", "1"), title("L-1", "2"), "Safety Analysis Set", "", ""))
  m <- do.call(rbind, rows)
  colnames(m) <- c("Shell Number", "Phase", "Title1", "Title2", "Population",
                   "Footnote1", "Footnote2")
  stopifnot(setequal(m[nzchar(m[, 1]), 1], ids))
  as.data.frame(m, stringsAsFactors = FALSE, check.names = FALSE)
}

.ai_example_shells <- function() {
  arms <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
  g <- function(...) {
    r <- list(...)
    w <- max(lengths(r))
    do.call(rbind, lapply(r, function(x) c(x, rep("", w - length(x)))))
  }
  pct <- "xx (xx.x%)"
  t1 <- g(c("T-1", "Table 14.1.1  Demographic Characteristics  (Safety Analysis Set)"),
          c("", arms, "Total"),
          c("Parameter", rep("(N=xx)", 4)),
          "Age (years)",
          c("  n", rep("xx", 4)),
          c("  Mean (SD)", rep("xx.x (xx.xx)", 4)),
          c("  Median", rep("xx.x", 4)),
          c("  Min, Max", rep("xx, xx", 4)),
          "Sex, n (%)",
          c("  Male", rep(pct, 4)),
          c("  Female", rep(pct, 4)),
          "Age group, n (%)",
          c("  18-64", rep(pct, 4)),
          c("  >64", rep(pct, 4)))
  t2 <- g(c("T-2", "Table 14.3.1  Treatment-Emergent Adverse Events by System Organ Class and Preferred Term  (Safety Analysis Set)"),
          c("System Organ Class", arms),
          c("  Preferred Term", rep("(N=xx)", 3)),
          c("Subjects with at least one TEAE", rep(pct, 3)),
          c("SYSTEM ORGAN CLASS 1", rep(pct, 3)),
          c("  Preferred term 1", rep(pct, 3)),
          c("  Preferred term 2", rep(pct, 3)),
          "",
          c("SYSTEM ORGAN CLASS 2", rep(pct, 3)),
          c("  Preferred term 1", rep(pct, 3)),
          "",
          "SOCs alphabetically; PTs by decreasing frequency in the total of the arms within a SOC.",
          "A subject is counted once in a system organ class and once in a preferred term.",
          "Coded with MedDRA.")
  l1 <- g(c("L-1", "Listing 16.2.7  Serious Adverse Events  (Safety Analysis Set)"),
          c("Subject", "Treatment", "System Organ Class /\nPreferred Term", "Start\nDay",
            "Severity", "Outcome"),
          c("xxxx-xx-xxxx", "xxxxxxxx", "XXXXXXXX /\nxxxxxxxx", "xx", "xxxxxx", "xxxxxxxx"),
          c("", "", "XXXXXXXX /\nxxxxxxxx", "xx", "xxxxxx", "xxxxxxxx"),
          "",
          "Sorted by subject and start day; the subject printed once.")
  f1 <- g(c("F-1", "Figure 14.2.1  Mean (SE) Systolic Blood Pressure by Visit  (Safety Analysis Set)"),
          "A line per arm through the mean at each visit, an error bar of one standard error, the arms side by side at each visit.",
          "x axis: Visit (Baseline, Week 2, Week 4 ... End of Treatment), in visit order.",
          "y axis: Mean (SE) Systolic Blood Pressure (mmHg).",
          "Legend below the plot: the three arms, in the order Placebo, Low Dose, High Dose.")
  lapply(list(`T-1` = t1, `T-2` = t2, `L-1` = l1, `F-1` = f1), function(m) {
    d <- as.data.frame(m, stringsAsFactors = FALSE)
    names(d) <- paste0("c", seq_len(ncol(d)))
    d
  })
}

.ai_write_example_toc <- function(path) {
  wb <- openxlsx::createWorkbook()
  bold <- openxlsx::createStyle(textDecoration = "bold")
  wrap <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", "XYZ-001 -- Table of contents, v0.3 (made up for the tflspec AI kit)",
                      startRow = 1, startCol = 1)
  toc <- .ai_example_toc()
  openxlsx::writeData(wb, "TOC", toc, startRow = 2, headerStyle = bold)
  openxlsx::setColWidths(wb, "TOC", seq_along(toc), c(14, 8, 16, 60, 20, 60, 20))
  for (nm in names(sh <- .ai_example_shells())) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, sh[[nm]], colNames = FALSE)
    openxlsx::addStyle(wb, nm, wrap, rows = seq_len(nrow(sh[[nm]])),
                       cols = seq_len(ncol(sh[[nm]])), gridExpand = TRUE)
    openxlsx::setColWidths(wb, nm, seq_len(ncol(sh[[nm]])), c(34, rep(22, ncol(sh[[nm]]) - 1L)))
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

.ai_write_example_specs <- function(path) {
  tfl_write_specs(path, ard = .ai_example_ard(),
                  table = .ai_example_table(), report = .ai_example_report(),
                  listing = .ai_example_listing())
  .ai_add_sheets(path, list(`_assumptions` = .ai_example_assumptions()))
}

.ai_write_example_figure <- function(path) {
  tfl_write_fig_design(.ai_example_figure(), path)
  a <- .ai_example_assumptions()
  a <- a[a$output_id == "F-1", , drop = FALSE]
  cat(c("assumptions:",
        sprintf("- %s", encodeString(paste0(a$assumption, ". ", a$question), quote = "'"))),
      file = path, sep = "\n", append = TRUE)
  invisible(path)
}

# ---- what to give the AI --------------------------------------------------------

# What to attach, what to give through the function, and what never: the
# same table in 00_START_HERE.md (for the person) and, as text, in the guide
# (for the assistant).
.ai_what_to_give <- function(meta = FALSE) {
  c("## What to give the AI",
    "",
    "| | Input | How | What it improves |",
    "|---|---|---|---|",
    "| **Required** | The TOC (or the study's list of outputs) | attach it as it is (Excel, CSV, Word, PDF) | the output ids, titles, populations and footnotes, copied exactly |",
    "| **Required** | The shells of the outputs you want | attach them | the rows, columns, statistics and layout of each table, listing and figure |",
    sprintf("| **Strongly recommended** | The ADaM metadata, **one** source | through the function, not as an attachment: `tflspec_ai_manual(dir, define = )`, or `adam_spec = `, or `datasets = ` | real dataset and variable names, labels and code list values instead of guesses%s |",
            if (meta) " (here: `08_adam_metadata.csv`)" else "; not given for this kit"),
    "| Nice to have | The SAP sections on the analysis sets and the methods | attach those pages, not the whole SAP | the ARD spec's methods (test, CI, model), denominators and analysis sets |",
    "| Nice to have | Company standards: code lists, display conventions (decimals, `n (%)`, sort order) | attach them | the house style without being told each time |",
    "| Nice to have | A previous study's specs | attach one workbook | the same choices as last time |",
    "",
    "**The TOC and the shells: one file or several, both are fine.** Many",
    "companies keep them in one workbook, a TOC sheet and a sheet a shell; others",
    "have a TOC file and a shells file. Attach them as they are. One file is",
    "simplest when the chat limits how many files you can upload; add the",
    "nice-to-haves when there is room.",
    "",
    "**ADaM metadata: one source, in this order.** `define.xml`; else the ADaM",
    "spec (also when the define is older than the data); else the datasets",
    "themselves, of which only the column information goes into the kit (names,",
    "labels, kinds, short code lists; never a subject's values). Give one: two",
    "that disagree leave the assistant to choose, without saying it did. With",
    "several, the function uses the first of this order and says which.",
    "",
    "**Do not give** patient-level data (the ADaM or SDTM datasets, listings with",
    "real values), results from the study unless your company allows it, files",
    "that are not about these outputs, or several versions of one document.",
    "Follow your company's rules on what may be sent to an AI service; this kit",
    "sends nothing by itself.",
    "",
    "**When something is missing**, the assistant works with what is there and",
    "lists every choice it made in `_assumptions`. Without ADaM metadata it names",
    "datasets and variables after the ADaM IG, and each of those is a row there.")
}

# ---- 00_START_HERE.md --------------------------------------------------------------

.ai_start_here <- function(split = FALSE, meta = FALSE) {
  blank <- if (split) "`03a_blank_ard_spec.xlsx` ... `03d_blank_listing_spec.xlsx`" else
    paste0("`", .ai_kit_files[["blank"]], "`")
  c(sprintf("# Start here -- tflspec %s AI kit", utils::packageVersion("tflspec")),
    "",
    "Paste the request below into the chat, attach every file of this folder, and",
    "attach your own TOC and shells (Excel, Word or PDF).",
    "",
    .ai_what_to_give(meta),
    "",
    "## The request",
    "",
    "> You are writing specification files for the R package tflspec, which",
    "> turns them into the programs that make clinical tables, listings and",
    "> figures.  Read `01_tflspec-ai-user-manual.md` (what each file and column",
    "> is) and `02_tflspec-ai-toc-shell-guide.md` (how to read a TOC and shells)",
    "> first, and look at the example: `05_example_toc_shell.xlsx` is a TOC and",
    "> shells, `06_example_specs.xlsx` and `07_example_figure.yml` the files",
    "> written from them.",
    ">",
    "> Then write the same files for the TOC and shells I attached:",
    ">",
    sprintf("> 1. one workbook with exactly the sheets and columns of %s", blank),
    ">    (the ARD, table, report and listing specs; fill what the TOC and",
    ">    shells say, leave the rest empty; add nothing but `note` columns);",
    "> 2. one `.yml` a figure, in the form of `04_blank_figure.yml`;",
    "> 3. a sheet `_assumptions` in the workbook (`output_id`, `sheet`, `item`,",
    ">    `assumption`, `question`), one row for everything you decided without",
    ">    being told -- datasets, flags, variables, denominators, methods, the",
    ">    order of levels -- and an `assumptions:` list in each `.yml`.",
    ">",
    if (meta) c(
    "> `08_adam_metadata.csv` is the study's ADaM metadata: take every dataset,",
    "> variable and level you write from it, and only from it.  A shell row with",
    "> no variable there is a row of `_assumptions`, not a guess.",
    ">"),
    "> Copy titles, footnotes and labels exactly.  Return the files for",
    "> download; if you cannot write `.xlsx`, return one CSV block a sheet,",
    "> titled with the sheet's name.  After the files, list the assumptions",
    "> and questions as text.  Nothing else.",
    "",
    "## The files",
    "",
    "| File | What it is |",
    "|---|---|",
    "| `00_START_HERE.md` | this request |",
    "| `01_tflspec-ai-user-manual.md` | the manual: sheets, columns, values, functions |",
    "| `02_tflspec-ai-toc-shell-guide.md` | how to read a TOC and shells into specs |",
    sprintf("| %s | every spec sheet, empty; `_columns` says what each column means |", blank),
    "| `04_blank_figure.yml` | a figure design, every field as a comment |",
    "| `05_example_toc_shell.xlsx` | a made-up TOC (sheet `TOC`) and four shells |",
    "| `06_example_specs.xlsx` | the specs written from them, with `_assumptions` |",
    "| `07_example_figure.yml` | the figure design written from shell F-1 |",
    if (meta) "| `08_adam_metadata.csv` | the study's ADaM metadata: datasets, variables, labels, kinds, levels |",
    "",
    "## Back in R",
    "",
    "```r",
    "library(tflspec)",
    "toc <- tfl_read_toc(\"TOC.xlsx\", sheet = \"TOC\", skip = 1,          # your TOC's layout",
    "                    map = list(output_id = \"Shell Number\", title = c(\"Title1\", \"Title2\"),",
    "                               population = \"Population\"))",
    "tfl_ai_review(\"XYZ-001_specs.xlsx\", toc = toc)   # each file the AI returned",
    if (meta) "# with the metadata: tfl_ai_review(..., metadata = read.csv(\"08_adam_metadata.csv\"))",
    "```",
    "",
    "It writes, next to the file, `<file>_repair.md` (what the AI can correct:",
    "paste it into the same chat) and `<file>_questions.md` (what you decide:",
    "answer each item and paste it back).  Then read the files with",
    "`tfl_read_ard_spec()`, `tfl_read_table_spec()`, `tfl_read_report_spec()`,",
    "`tfl_read_listing_spec()` and `tfl_read_fig_design()`.")
}

# ---- the kit -----------------------------------------------------------------------

.ai_write_kit <- function(dir, split = FALSE, overwrite = FALSE, meta = NULL) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  names_out <- .ai_kit_files
  if (split) names_out <- names_out[names(names_out) != "blank"]
  have <- file.path(dir, names_out)
  have <- have[file.exists(have)]
  if (length(have) && !isTRUE(overwrite)) {
    stop(sprintf(paste0(
      "`tflspec_ai_manual()`: %s already in '%s'; pass `overwrite = TRUE` ",
      "to write the kit again."),
      paste(basename(have), collapse = ", "), dir), call. = FALSE)
  }
  # a kit written again without metadata keeps no metadata of the last one
  if (is.null(meta)) unlink(file.path(dir, .ai_kit_files[["metadata"]]))
  src <- function(name) system.file("ai", name, package = "tflspec")
  f <- function(k) file.path(dir, .ai_kit_files[[k]])
  writeLines(.ai_start_here(split, meta = !is.null(meta)), f("start"), useBytes = TRUE)
  file.copy(src("tflspec-ai-user-manual.md"), f("manual"), overwrite = TRUE)
  file.copy(src("tflspec-ai-toc-shell-guide.md"), f("guide"), overwrite = TRUE)
  blank <- .ai_write_blank(dir, split)
  writeLines(.ai_blank_figure(), f("blank_fig"), useBytes = TRUE)
  .ai_write_example_toc(f("toc"))
  .ai_write_example_specs(f("specs"))
  .ai_write_example_figure(f("figure"))
  out <- c(f("start"), f("manual"), f("guide"), blank, f("blank_fig"), f("toc"),
           f("specs"), f("figure"))
  if (!is.null(meta)) {
    utils::write.csv(meta, f("metadata"), row.names = FALSE, na = "",
                     fileEncoding = "UTF-8")
    out <- c(out, f("metadata"))
  }
  out <- normalizePath(out, winslash = "/")
  size <- sum(file.size(out))
  message(sprintf("tflspec AI kit: %d files, %s KB, in %s", length(out),
                  format(round(size / 1024)), normalizePath(dir, winslash = "/")))
  message(paste0("  ", basename(out), "  (", format(round(file.size(out) / 1024)), " KB)",
                 collapse = "\n"))
  message(sprintf("Attach all files in %s, plus your TOC and shells, and paste the request in 00_START_HERE.md.",
                  normalizePath(dir, winslash = "/")))
  invisible(out)
}
