# ============================================================================
#  The AI kit: what tflspec_ai_manual(dir) writes for an AI chat
# ============================================================================
#
#  A chat model cannot read the user's disk: the user attaches files, and
#  many chats take ten at most and no folders.  So the kit is one flat
#  folder of seven files -- the request, the manual, the TOC / shell guide,
#  the spec's schema, a blank study in the answer's form, and a made-up study
#  end to end (its TOC and shells, the spec files written from them).  The
#  answer is one Markdown text holding every file of the study's YAML spec
#  (R/ai_answer.R: tfl_ai_unpack() writes it into a study folder).
#  Everything is written when asked: the schema is the installed one, and
#  the example comes from the code below through the one-time conversion,
#  so neither can drift from what this version reads.
.ai_kit_files <- c(
  start    = "00_START_HERE.md",
  manual   = "01_tflspec-ai-user-manual.md",
  guide    = "02_tflspec-ai-toc-shell-guide.md",
  schema   = "03_spec_schema.yml",
  blank    = "04_blank_spec.md",
  toc      = "05_example_toc_shell.xlsx",
  example  = "06_example_spec.md",
  metadata = "07_adam_metadata.csv")

# ---- the blank specs (what the workbook review reads) -------------------

.ai_blank_specs <- function() {
  list(ard = tfl_ard_spec_template(), table = tfl_table_spec(),
       report = tfl_table_spec(), listing = tfl_listing_spec())
}


# ---- the example: a made-up study, XYZ-001 ------------------------------------
#
# Four outputs on tflspec's oncology sample data (adsl, adae, advs; arms
# Drug X and Control): T-1 demographics, T-2 treatment-emergent AEs by SOC
# and PT, L-1 serious AEs, F-1 mean systolic blood pressure by visit.  The TOC and shells are what a company
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
  arms <- "Drug X | Control"
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
                       levels = c(rep(arms, 2), "", "", "<65 | >=65")),
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
                        group = "TRT01A")
  # ADVS has the arm and the flag itself: no join; the shell's visits are
  # baseline to cycle 9 (AVISITN 0 to 8)
  d$data <- Filter(function(s) !identical(s$step, "join"), d$data)
  for (i in seq_along(d$data)) {
    if (identical(d$data[[i]]$step, "filter")) d$data[[i]]$expr <- "!is.na(AVAL) & AVISITN <= 8"
  }
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
  arms <- c("Drug X", "Control")
  g <- function(...) {
    r <- list(...)
    w <- max(lengths(r))
    do.call(rbind, lapply(r, function(x) c(x, rep("", w - length(x)))))
  }
  pct <- "xx (xx.x%)"
  k <- length(arms)
  t1 <- g(c("T-1", "Table 14.1.1  Demographic Characteristics  (Safety Analysis Set)"),
          c("", arms, "Total"),
          c("Parameter", rep("(N=xx)", k + 1)),
          "Age (years)",
          c("  n", rep("xx", k + 1)),
          c("  Mean (SD)", rep("xx.x (xx.xx)", k + 1)),
          c("  Median", rep("xx.x", k + 1)),
          c("  Min, Max", rep("xx, xx", k + 1)),
          "Sex, n (%)",
          c("  Male", rep(pct, k + 1)),
          c("  Female", rep(pct, k + 1)),
          "Age group, n (%)",
          c("  <65", rep(pct, k + 1)),
          c("  >=65", rep(pct, k + 1)))
  t2 <- g(c("T-2", "Table 14.3.1  Treatment-Emergent Adverse Events by System Organ Class and Preferred Term  (Safety Analysis Set)"),
          c("System Organ Class", arms),
          c("  Preferred Term", rep("(N=xx)", k)),
          c("Subjects with at least one TEAE", rep(pct, k)),
          c("SYSTEM ORGAN CLASS 1", rep(pct, k)),
          c("  Preferred term 1", rep(pct, k)),
          c("  Preferred term 2", rep(pct, k)),
          "",
          c("SYSTEM ORGAN CLASS 2", rep(pct, k)),
          c("  Preferred term 1", rep(pct, k)),
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
          "x axis: Visit (Baseline, Cycle 2 Day 1 ... Cycle 9 Day 1), in visit order.",
          "y axis: Mean (SE) Systolic Blood Pressure (mmHg).",
          "Legend below the plot: the two arms, in the order Drug X, Control.")
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

# ---- the example as the YAML spec ------------------------------------------------

# "" is how the example's sheets say "blank"; a workbook read back says NA
.ai_blank_na <- function(x) {
  lapply(x, function(d) {
    if (is.data.frame(d)) d[] <- lapply(d, function(v) { v[!is.na(v) & v == ""] <- NA; v })
    d
  })
}

# The made-up study as the files of its YAML spec, in a folder (made here):
# the example's sheets through the one-time conversion, so the example is
# always what this version reads and writes; and the assumptions the guide
# asks for, as spec/_assumptions.yml.
.ai_example_study <- function(dir) {
  na <- .ai_blank_na
  rep_ <- .ai_example_report()
  titles <- rep_$titles
  title_of <- function(id, k) titles$center[titles$output_id == id & titles$line == k]
  ids <- rep_$report$output_id
  planner <- .ai_df(
    output_id = ids,
    description = vapply(ids, function(id) title_of(id, "2"), ""),
    setup_code = NA_character_, process_code = NA_character_,
    section = c("14.1", "14.3", "16.2", "14.2"),
    population = "SAF", datasets = NA_character_, batches = NA_character_)
  sheets <- list(
    table = tfl_table_spec(c(na(.ai_example_table()), na(rep_))),
    planner = planner,
    ard = tfl_ard_spec(na(.ai_example_ard())),
    listing = unclass(tfl_listing_spec(na(.ai_example_listing()))),
    figures = list(`F-1` = unclass(.ai_example_figure())),
    study = list(study_id = "XYZ-001",
                 title = "A made-up study (tflspec's oncology sample data)"))
  suppressMessages(tfl_yaml_from_sheets(sheets, dir, overwrite = TRUE, figures = "items"))
  unlink(file.path(dir, "spec", "_not_converted.yml"))
  a <- .ai_example_assumptions()
  where <- c(populations = "spec/data.yml populations", datasets = "spec/data.yml datasets",
             variables = "content", codelists = "codelists", tables = "content",
             analyses = "ard.analyses", listing_cols = "content", listings = "data",
             data = "data")
  rows <- lapply(seq_len(nrow(a)), function(i) {
    r <- list(output_id = if (nzchar(a$output_id[i])) a$output_id[i],
              where = unname(where[a$sheet[i]]), item = a$item[i],
              assumption = a$assumption[i], question = a$question[i])
    Filter(Negate(is.null), r)
  })
  .ai_write_text(yaml::as.yaml(rows, indent.mapping.sequence = TRUE),
                 file.path(dir, "spec", "_assumptions.yml"))
  invisible(dir)
}

# 06_example_spec.md: the example study's files in the answer's form
.ai_example_spec_md <- function() {
  d <- file.path(tempfile("ai-example-"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  .ai_example_study(d)
  c("# The example's answer -- study XYZ-001",
    "",
    "The specs written from `05_example_toc_shell.xlsx`, in the form of the answer:",
    "each file of the study a heading with its path and a `yaml` block.  The",
    "study's files first (`study.yml`, `spec/data.yml`, `spec/defaults.yml`,",
    "`spec/toc.yml`), then one file a report (`spec/outputs/<id>.yml`), then the",
    "assumptions (`spec/_assumptions.yml`).  `tfl_ai_unpack()` writes an answer",
    "of this form into a study folder.",
    "",
    .ai_answer_md(d))
}

# 04_blank_spec.md: every file of a study, its keys said in comments, in
# the answer's form
.ai_blank_spec_md <- function() {
  blk <- function(path, ...) c(paste("###", path), "", "```yaml", c(...), "```", "")
  c("# The files to write -- a blank study",
    "",
    "Every file of a study's YAML spec, in the form of the answer: a heading",
    "with the file's path, then a `yaml` block.  Write each file the study",
    "needs, replace the values, keep the keys; a key you have nothing for is",
    "left out.  `!r` marks R code (a condition, an expression) -- write it only",
    "where the value is R; a list, a map or a column name is written as YAML.",
    "What each function's arguments take is `03_spec_schema.yml`.",
    "",
    blk("study.yml",
        "study_id: XYZ-001              # the study's id, as the TOC writes it",
        "title: A study title",
        "settings:",
        "  rounding: sas                # sas: half away from zero; r: half to even",
        "  program_dir: programs/tfl    # where the report programs are",
        "  program: \"{output_id}.R\"     # each report's program",
        "  file: \"{output_id}.rtf\"      # each report's RTF file",
        "keys:",
        "  id: USUBJID                  # the subject key of the data"),
    blk("spec/data.yml",
        "datasets:                      # every dataset a report reads",
        "  - dataset: ADSL",
        "    level: ADaM",
        "    path: data/adam/adsl.rds",
        "populations:                   # the analysis sets",
        "  - id: SAF",
        "    dataset: ADSL",
        "    where: !r SAFFL == \"Y\"     # R: which subjects are in it",
        "    label: Safety Analysis Set"),
    blk("spec/toc.yml",
        "outputs:                       # every report, in the TOC's order",
        "  - id: T-1",
        "    kind: table                # table | listing | figure",
        "    label: Demographic Characteristics",
        "    population: SAF"),
    blk("spec/defaults.yml",
        "content:                       # what every table starts from",
        "  - normalize_ard: {}",
        "page:",
        "  header:                      # the running header, by line",
        "    '1': {left: XYZ-001, right: 'Page {PAGE} of {TOTAL_PAGES}'}",
        "  footer:",
        "    '1': {left: '{PROGRAM}', right: 'Generated on: {DATETIME}'}"),
    blk("spec/outputs/T-1.yml",
        "id: T-1",
        "kind: table",
        "extends: [defaults]            # the study's defaults under this report",
        "ard:                           # the report's part of the study ARD",
        "  analyses:",
        "    - id: AGE",
        "      method: continuous       # continuous | categorical | hierarchical | ...",
        "      dataset: ADSL",
        "      population: SAF",
        "      by: [TRT01A]",
        "      variables: [AGE]",
        "data:                          # what the table reads: the study ARD",
        "  source: ard",
        "  object: ard",
        "codelists:                     # a variable's values, in the order printed",
        "  SEX:",
        "    - {value: M, label: Male}",
        "    - {value: F, label: Female}",
        "content:                       # the table: one item a plan verb",
        "  - table_plan: {cols: TRT01A, rows: {group: variable}}",
        "  - plan_cells:",
        "      continuous: {Mean (SD): '{mean:.1f} ({sd:.2f})'}",
        "      categorical: '{n} ({p:.1f%})'",
        "  - plan_labels: {AGE: Age (years)}",
        "  - plan_col_header:",
        "      header:",
        "        - {line: '1', cols: .values, span: each, text: '{col}'}",
        "        - {line: '2', cols: .values, span: each, text: '(N={n})'}",
        "page:",
        "  titles:",
        "    '1': {center: Table 14.1.1}",
        "    '2': {center: Demographic Characteristics}",
        "  footnotes:",
        "    '1': {left: A footnote of this table.}"),
    blk("spec/outputs/L-1.yml",
        "id: L-1",
        "kind: listing",
        "extends: [defaults]",
        "data:",
        "  source: dataset",
        "  dataset: ADAE",
        "  where: !r AESER == \"Y\"",
        "  sort: [USUBJID, ASTDY]",
        "content:",
        "  - plan_listing:",
        "      type: multiline",
        "      cols:                    # one listing_col() a printed column",
        "        - {fn: listing_col, args: {vars: [USUBJID], label: Subject}}",
        "        - {fn: listing_col, args: {vars: [AEBODSYS, AEDECOD], label: 'SOC /\\nPT'}}",
        "page:",
        "  titles:",
        "    '1': {center: Listing 16.2.7}"),
    blk("spec/outputs/F-1.yml",
        "id: F-1",
        "kind: figure",
        "extends: [defaults]",
        "data:",
        "  source: code",
        "  steps:                       # the figure's data, a step each",
        "    - {step: read, dataset: ADVS}",
        "    - {step: param, value: SYSBP}",
        "    - {step: summary, name: sm, value: AVAL, by: 'TRTA, AVISIT', interval: se}",
        "content:                       # the function that makes the plot, its layers, its settings",
        "  - ggplot: {data: sm, mapping: {x: AVISIT, 'y': mean, colour: TRTA}}",
        "  - geom_line: {position: {fn: position_dodge, args: {width: 0.3}}}",
        "  - geom_errorbar: {mapping: {ymin: lo, ymax: hi}, width: 0.2,",
        "                   position: {fn: position_dodge, args: {width: 0.3}}}",
        "  - scale_colour_tfl: {palette: treatment}   # the company standard's colours",
        "  - labs: {x: Visit, 'y': Mean (SE)}",
        "  - theme_tfl: {}                            # the company standard's theme",
        "  - tfl_image: {dpi: 300}                    # the image: always the last item",
        "page:",
        "  titles:",
        "    '1': {center: Figure 14.2.1}"),
    blk("spec/_assumptions.yml",
        "# one item for everything decided without being told",
        "- output_id: T-1",
        "  where: ard.analyses",
        "  item: AGE",
        "  assumption: the arm is TRT01A (actual treatment)",
        "  question: Actual or planned treatment?"))
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
            if (meta) " (here: `07_adam_metadata.csv`)" else "; not given for this kit"),
    "| Nice to have | The SAP sections on the analysis sets and the methods | attach those pages, not the whole SAP | the ARD spec's methods (test, CI, model), denominators and analysis sets |",
    "| Nice to have | Company standards: code lists, display conventions (decimals, `n (%)`, sort order) | attach them | the house style without being told each time |",
    "| Nice to have | A previous study's specs | attach its spec files (or its workbook) | the same choices as last time |",
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
    "lists every choice it made in `spec/_assumptions.yml`. Without ADaM metadata it names",
    "datasets and variables after the ADaM IG, and each of those is an item there.")
}

# ---- 00_START_HERE.md --------------------------------------------------------------

.ai_start_here <- function(meta = FALSE) {
  f <- function(k) paste0("`", .ai_kit_files[[k]], "`")
  c(sprintf("# Start here -- tflspec %s AI kit", utils::packageVersion("tflspec")),
    "",
    "Paste the request below into the chat, attach every file of this folder, and",
    "attach your own TOC and shells (Excel, Word or PDF).",
    "",
    .ai_what_to_give(meta),
    "",
    "## The request",
    "",
    "> You are writing the specification files of a study for the R package",
    "> tflspec, which turns them into the programs that make clinical tables,",
    sprintf("> listings and figures.  Read %s (what each file and key is) and",
            f("manual")),
    sprintf("> %s (how to read a TOC and shells) first; %s says",
            f("guide"), f("schema")),
    "> what every function's arguments take.  Look at the example:",
    sprintf("> %s is a TOC and shells, %s the files written from them.",
            f("toc"), f("example")),
    ">",
    "> Then write the same files for the TOC and shells I attached, as ONE",
    sprintf("> answer in the form of %s and %s: each file a heading", f("blank"), f("example")),
    "> with its path (`### spec/outputs/T-1.yml`) and its content in a `yaml`",
    "> block --",
    ">",
    "> 1. `study.yml`, `spec/data.yml` (the datasets and the analysis sets) and",
    ">    `spec/toc.yml` (the reports, in the TOC's order);",
    "> 2. `spec/defaults.yml` when the reports share a page header, footer or",
    ">    table layout;",
    "> 3. one `spec/outputs/<id>.yml` a report the TOC lists, with what its",
    ">    shell says;",
    "> 4. `spec/_assumptions.yml`: one item for everything you decided without",
    ">    being told -- datasets, flags, variables, denominators, methods, the",
    ">    order of levels -- each its `output_id`, `where`, `item`,",
    ">    `assumption` and `question`.",
    ">",
    if (meta) c(
    sprintf("> %s is the study's ADaM metadata: take every dataset,", f("metadata")),
    "> variable and level you write from it, and only from it.  A shell row with",
    "> no variable there is an item of `_assumptions.yml`, not a guess.",
    ">"),
    "> Copy titles, footnotes and labels exactly.  Write `!r` only where the",
    "> value is R code; a list, a map or a column name is written as YAML.",
    "> After the files, list the assumptions and questions as text.  Nothing",
    "> else.",
    "",
    "## The files",
    "",
    "| File | What it is |",
    "|---|---|",
    "| `00_START_HERE.md` | this request |",
    "| `01_tflspec-ai-user-manual.md` | the manual: the files, their keys and values, the functions |",
    "| `02_tflspec-ai-toc-shell-guide.md` | how to read a TOC and shells into the spec files |",
    "| `03_spec_schema.yml` | every function a spec item calls, its arguments and their types; the spec sheets' columns |",
    "| `04_blank_spec.md` | every file of a study, blank, in the answer's form, its keys said in comments |",
    "| `05_example_toc_shell.xlsx` | a made-up TOC (sheet `TOC`) and four shells |",
    "| `06_example_spec.md` | the files written from them, in the answer's form, with `_assumptions.yml` |",
    if (meta) "| `07_adam_metadata.csv` | the study's ADaM metadata: datasets, variables, labels, kinds, levels |",
    "",
    "## Back in R",
    "",
    "```r",
    "library(tflspec)",
    "tfl_ai_unpack(\"answer.md\", \"XYZ-001\")      # the answer, saved as a file (or its text)",
    "toc <- tfl_read_toc(\"TOC.xlsx\", sheet = \"TOC\", skip = 1,          # your TOC's layout",
    "                    map = list(output_id = \"Shell Number\", title = c(\"Title1\", \"Title2\"),",
    "                               population = \"Population\"))",
    "tfl_ai_review(\"XYZ-001\", toc = toc)            # the study folder",
    if (meta) "# with the metadata: tfl_ai_review(..., metadata = read.csv(\"07_adam_metadata.csv\"))",
    "```",
    "",
    "It writes, in the folder, `XYZ-001_repair.md` (what the AI can correct:",
    "paste it into the same chat) and `XYZ-001_questions.md` (what you decide:",
    "answer each item and paste it back).  Then the study is read with",
    "`tfl_read_study_spec()` and `tfl_read_outputs()`, and its programs are",
    "written from it.")
}

# ---- the kit -----------------------------------------------------------------------

.ai_write_kit <- function(dir, overwrite = FALSE, meta = NULL) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  have <- file.path(dir, .ai_kit_files)
  have <- have[file.exists(have)]
  if (length(have) && !isTRUE(overwrite)) {
    stop(sprintf(paste0(
      "`tflspec_ai_manual()`: %s already in '%s'; pass `overwrite = TRUE` ",
      "to write the kit again."),
      paste(basename(have), collapse = ", "), dir), call. = FALSE)
  }
  # a kit written again without metadata keeps no metadata of the last one
  if (is.null(meta)) unlink(file.path(dir, .ai_kit_files[["metadata"]]))
  src <- function(name) system.file(name, package = "tflspec")
  f <- function(k) file.path(dir, .ai_kit_files[[k]])
  .ai_write_text(.ai_start_here(meta = !is.null(meta)), f("start"))
  file.copy(src(file.path("ai", "tflspec-ai-user-manual.md")), f("manual"), overwrite = TRUE)
  file.copy(src(file.path("ai", "tflspec-ai-toc-shell-guide.md")), f("guide"), overwrite = TRUE)
  file.copy(src(file.path("spec", "schema.yml")), f("schema"), overwrite = TRUE)
  .ai_write_text(.ai_blank_spec_md(), f("blank"))
  .ai_write_example_toc(f("toc"))
  .ai_write_text(.ai_example_spec_md(), f("example"))
  out <- c(f("start"), f("manual"), f("guide"), f("schema"), f("blank"), f("toc"), f("example"))
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
