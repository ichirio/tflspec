# tfl_ai_prepare(unit = "toc", templates = TRUE / FALSE): the requests ask for a template
# where one fits (listed from the registry), or the code SPEC only (as
# before); an answer's template checked, expanded and built; the
# comparison of the two

aitpl_study <- function(root) {
  ad <- file.path(root, "adam")
  dir.create(ad, recursive = TRUE, showWarnings = FALSE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(ad, paste0(x, ".xpt")))
  }
  normalizePath(ad, winslash = "/")
}
aitpl_book <- function() system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
aitpl_answer <- function(...) paste(c("```yaml", ..., "```"), collapse = "\n")

test_that("templates = TRUE / FALSE: the setting at each request's head; FALSE as before", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  ad <- aitpl_study(root)
  d1 <- file.path(root, "with")
  d0 <- file.path(root, "without")
  r1 <- tfl_ai_prepare(unit = "toc", d1, aitpl_book(), adam = ad, quiet = TRUE, size = 3L)
  r0 <- tfl_ai_prepare(unit = "toc", d0, aitpl_book(), adam = ad, quiet = TRUE, size = 3L, templates = FALSE)
  expect_true(r1$templates)
  expect_false(r0$templates)
  expect_identical(r1$requests$request, r0$requests$request)
  expect_error(tfl_ai_prepare(unit = "toc", d0, aitpl_book(), adam = ad, quiet = TRUE, templates = "prefer"),
               "TRUE .* or FALSE")
  for (i in seq_len(nrow(r1$requests))) {
    with <- readLines(file.path(d1, r1$requests$file[i]), encoding = "UTF-8")
    without <- readLines(file.path(d0, r0$requests$file[i]), encoding = "UTF-8")
    at <- match("<!-- templates: TRUE -->", with)
    expect_false(is.na(at))
    expect_false(any(grepl("templates:", without, fixed = TRUE)))
    expect_false(any(grepl("## Templates first", without, fixed = TRUE)))
    # the same request but the setting's line and the templates' part
    ts <- grep("^Templates: ", with)[1L]
    sep <- ts + match(TRUE, grepl("^## ", with[-seq_len(ts)]))
    keep <- with[-c(at, seq(ts, sep - 1L))]
    expect_identical(keep[keep != ""], without[without != ""], info = r1$requests$request[i])
  }
  # a table's request names the likely table templates, a figure's the figures'
  t1 <- paste(readLines(file.path(d1, "ai", "requests", "ae-1.md"), encoding = "UTF-8"), collapse = "
")
  expect_match(t1, "- T-14-3-1: [^
]*`ae_soc_pt`")
  expect_false(grepl("`km_survival`", t1, fixed = TRUE))
  f1 <- paste(readLines(file.path(d1, "ai", "requests", "figure-1.md"), encoding = "UTF-8"), collapse = "
")
  expect_match(f1, "- F-14-2-2: [^
]*`km_survival`")
  expect_false(grepl("`demog`", f1, fixed = TRUE))
  # all of them, with their arguments, in the file every request names
  tp <- paste(readLines(file.path(d1, "ai", "templates.md"), encoding = "UTF-8"), collapse = "
")
  expect_match(tp, "- `ae_soc_pt`: Adverse events by SOC and PT (FDA STF Table 14) (table, layout hierarchical; variant of hierarchical)", fixed = TRUE)
  expect_match(tp, "  - `population` [target]*", fixed = TRUE)
  expect_match(tp, "  - `where` [target] (text) = \"TRTEMFL == \\\"Y\\\"\"", fixed = TRUE)
  expect_match(tp, "- `km_survival`:", fixed = TRUE)
  # a variant registered now is listed, its from said
  tfl_template_variant("demog_x", from = "demog", defaults = list(continuous = "AGE"))
  on.exit(rm(list = "demog_x", envir = tflspec:::.tpl_registry), add = TRUE)
  expect_true(any(grepl("`demog_x`: .*variant of demog", tflspec:::.ai_templates_list("table"))))
  # the agent's instructions say it too
  da <- file.path(root, "agent")
  ra <- tfl_ai_prepare(unit = "toc", da, aitpl_book(), adam = ad, quiet = TRUE, size = 3L)
  ag <- readLines(file.path(da, "ai", "AGENTS.md"), encoding = "UTF-8")
  expect_true(any(grepl("## Templates first", ag, fixed = TRUE)))
  expect_match(ra$agent_prompt, "template where one fits", fixed = TRUE)
  db <- file.path(root, "agent0")
  rb <- tfl_ai_prepare(unit = "toc", db, aitpl_book(), adam = ad, quiet = TRUE, size = 3L, templates = FALSE)
  expect_false(any(grepl("## Templates first", readLines(file.path(db, "ai", "AGENTS.md")), fixed = TRUE)))
  expect_false(grepl("template", rb$agent_prompt, fixed = TRUE))
})

test_that("answers with templates: checked, written, built (RTF, PNG); a bad argument a repair", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("ggsurvfit")
  root <- withr::local_tempdir()
  ad <- aitpl_study(root)
  d <- file.path(root, "study")
  r <- tfl_ai_prepare(unit = "toc", d, aitpl_book(), adam = ad, quiet = TRUE, size = 3L)
  ai_drafts_from_answer(d, aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-1-1]}",
    "outputs:",
    "  - id: T-14-1-1",
    "    kind: table",
    "    label: Demographic and Baseline Characteristics",
    "    common: {population: SAF}",
    "    extends: [defaults]",
    "    template: {name: demog, args: {by: TRT01A, continuous: AGE, categorical: [AGEGR1, SEX, RACE]}}",
    "    page: {titles: {\"1\": {center: Table 14.1.1}}}",
    "assumptions: []"), "table")
  ai_drafts_from_answer(d, aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-3-1]}",
    "outputs:",
    "  - id: T-14-3-1",
    "    kind: table",
    "    label: Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
    "    common: {population: SAF}",
    "    extends: [defaults]",
    "    template: {name: ae_soc_pt}",
    "    content:",
    "      - plan_paginate_rows: {max_rows: 18}",
    "    page: {titles: {\"1\": {center: Table 14.3.1}}}",
    "assumptions: []"), "table")
  ai_drafts_from_answer(d, aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-3-5]}",
    "outputs:",
    "  - id: T-14-3-5",
    "    kind: table",
    "    label: Laboratory values and change from baseline",
    "    common: {population: SAF}",
    "    extends: [defaults]",
    "    template:",
    "      name: param_change",
    "      args: {data: ADLB, by: TRT01A, params: [ALT, AST], visits: [CYCLE 2 DAY 1], param_order: ~, colour: red}",
    "    page: {titles: {\"1\": {center: Table 14.3.5}}}",
    "assumptions: []"), "table")
  ai_drafts_from_answer(d, aitpl_answer(
    "tflspec_ai: {task: figure, version: 1, output_id: F-14-2-2}",
    "data: [{step: read, dataset: ADTTE}, {step: param, value: OS}]",
    "template: {name: km_survival, args: {by: TRT01A, time_unit: months}}",
    "plot: {x_label: Months since randomisation, legend: bottom}",
    "layers:",
    "  - {layer: geom_hline, yintercept: 0.5, linetype: dashed}",
    "assumptions: []"), "figure")
  res <- suppressMessages(tfl_ai_run(d))
  rp <- res$reports
  st <- stats::setNames(rp$status, rp$output_id)
  expect_identical(unname(st[c("T-14-1-1", "T-14-3-1", "F-14-2-2")]), rep("ok", 3L))
  for (id in c("T-14-1-1", "T-14-3-1", "F-14-2-2")) {
    expect_true(file.exists(file.path(d, rp$rtf[rp$output_id == id])), info = id)
  }
  expect_true(file.exists(file.path(d, rp$png[rp$output_id == "F-14-2-2"])))
  # the report file keeps the template, as answered
  y <- yaml::read_yaml(file.path(d, "ai", "spec", "outputs", "T-14-3-1.yml"))
  expect_identical(y$template$name, "ae_soc_pt")
  # a template figure keeps its own settings, over the template's
  y <- yaml::read_yaml(file.path(d, "ai", "spec", "outputs", "F-14-2-2.yml"))
  expect_identical(y$plot$x_label, "Months since randomisation")
  ro <- tfl_resolve_output(tfl_read_output(file.path(d, "ai", "spec", "outputs", "F-14-2-2.yml")),
                           tfl_read_study_spec(d))
  expect_identical(ro$plot$x_label, "Months since randomisation")
  # a bad argument: an error that names the template's parameters, a repair
  expect_identical(unname(st["T-14-3-5"]), "error")
  expect_match(rp$first[rp$output_id == "T-14-3-5"],
               "colour is not an argument of template param_change; its parameters are population",
               fixed = TRUE)
  expect_true(file.exists(file.path(d, "ai", "repairs", "labs-1.md")))
  # the repaired answer: built
  f <- file.path(d, "ai", "spec", "outputs", "T-14-3-5.yml")
  a <- readLines(f, encoding = "UTF-8")
  a <- sub(", colour: red", "", a, fixed = TRUE)
  writeLines(a[!grepl("^[[:space:]]*colour: red[[:space:]]*$", a)], f)
  res <- suppressMessages(tfl_ai_run(d))
  rp <- res$reports
  expect_identical(rp$status[rp$output_id == "T-14-3-5"], "ok")
  expect_true(file.exists(file.path(d, rp$rtf[rp$output_id == "T-14-3-5"])))
  status <- readLines(file.path(d, "ai", "status.md"), encoding = "UTF-8")
  expect_true("templates: TRUE" %in% status)
  # the comparison: one row a report, what it was answered with, its values,
  # its check, its repairs
  ai_drafts_from_answer(d, aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-2-1]}",
    "outputs:",
    "  - id: T-14-2-1",
    "    kind: table",
    "    label: Sex by arm",
    "    common: {population: ITT}",
    "    extends: [defaults]",
    "    ard:",
    "      analyses:",
    "        - {id: SEX, method: categorical, dataset: ADSL, population: ITT, by: [TRT01A], variables: [SEX]}",
    "    content:",
    "      - table_plan: {cols: TRT01A, rows: {group: variable}}",
    "    page: {titles: {\"1\": {center: Table 14.2.1}}}",
    "assumptions: []"), "table")
  cmp <- tfl_ai_compare(d)
  expect_true(file.exists(file.path(d, "ai", "compare.csv")))
  expect_identical(names(cmp), c("output_id", "request", "templates", "answered_with", "values",
                                 "check", "repairs"))
  expect_setequal(cmp$output_id, r$toc$toc_id)
  by_id <- function(col) stats::setNames(cmp[[col]], cmp$output_id)
  expect_identical(unname(by_id("answered_with")[c("T-14-1-1", "T-14-3-1", "T-14-3-5", "F-14-2-2",
                                                   "T-14-2-1")]),
                   c("template:demog", "template:ae_soc_pt", "template:param_change",
                     "template:km_survival", "code"))
  expect_true(all(cmp$templates))
  expect_identical(unname(by_id("repairs")[c("T-14-3-5", "T-14-1-1")]), c(1L, 0L))
  expect_true(all(cmp$check %in% c("ok", "warning")))
  expect_true(by_id("values")[["T-14-2-1"]] > by_id("values")[["T-14-1-1"]])
  expect_identical(utils::read.csv(file.path(d, "ai", "compare.csv"), stringsAsFactors = FALSE)$output_id,
                   cmp$output_id)
  # an unknown template
  bad <- tfl_ai_parse(aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [T-1]}",
    "outputs:", "  - {id: T-1, kind: table, template: {name: demo}}", "assumptions: []"), "table")
  p <- tfl_ai_check(bad, tfl_ai_context("table", outputs = "T-1"))
  expect_true(any(grepl("'demo' is not a template; the templates are", p$problem, fixed = TRUE)))
})

test_that("what every request repeats is a file, written once", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  ad <- aitpl_study(root)
  d1 <- file.path(root, "with")
  d0 <- file.path(root, "without")
  r1 <- tfl_ai_prepare(unit = "toc", d1, aitpl_book(), adam = ad, quiet = TRUE, size = 3L)
  r0 <- tfl_ai_prepare(unit = "toc", d0, aitpl_book(), adam = ad, quiet = TRUE, size = 3L, templates = FALSE)
  for (f in c("ai/instructions-table.md", "ai/instructions-figure.md", "ai/templates.md")) {
    expect_true(f %in% r1$files, info = f)
  }
  expect_false(file.exists(file.path(d0, "ai", "templates.md")))
  # the instructions once, with the header's place holders; no template list
  it <- readLines(file.path(d1, "ai", "instructions-table.md"), encoding = "UTF-8")
  expect_true(any(grepl("outputs: [\"<id>\", \"<id>\"]", it, fixed = TRUE)))
  expect_false(any(grepl("### The templates", it, fixed = TRUE)))
  expect_identical(it, readLines(file.path(d0, "ai", "instructions-table.md"), encoding = "UTF-8"))
  tp <- paste(readLines(file.path(d1, "ai", "templates.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(tp, "- `ae_soc_pt`: Adverse events by SOC and PT", fixed = TRUE)
  expect_match(tp, "- `km_survival`:", fixed = TRUE)
  expect_match(tp, "as the schema of `instructions-table.md` says", fixed = TRUE)
  # a request: the files named, its header, the likely templates; no copies
  ae <- paste(readLines(file.path(d1, "ai", "requests", "ae-1.md"), encoding = "UTF-8"), collapse = "\n")
  expect_lt(nchar(ae), 8000)
  expect_match(ae, "Follow `instructions-table.md`", fixed = TRUE)
  expect_match(ae, "- T-14-3-1: [^\n]*`ae_soc_pt`")
  expect_match(ae, "`adam_summary.md`: name only", fixed = TRUE)
  expect_false(grepl("```yaml", ae, fixed = TRUE))
  expect_false(grepl("### ADSL", ae, fixed = TRUE))
  fg <- paste(readLines(file.path(d1, "ai", "requests", "figure-1.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(fg, "Follow `instructions-figure.md`", fixed = TRUE)
  expect_match(fg, "- F-14-2-2: `km", fixed = TRUE)
  ae0 <- paste(readLines(file.path(d0, "ai", "requests", "ae-1.md"), encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("templates", ae0, fixed = TRUE))
  # AGENTS.md says to read them
  ag <- paste(readLines(file.path(d1, "ai", "AGENTS.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(ag, "`instructions-table.md`", fixed = TRUE)
  expect_match(ag, "Read `templates.md` once", fixed = TRUE)
  expect_identical(readLines(file.path(d1, ".github", "copilot-instructions.md")),
                   readLines(file.path(d1, "AGENTS.md")))
  # the likely templates, from a title
  expect_identical(tflspec:::.ai_templates_likely("Demographic and Baseline Characteristics", "table"),
                   c("demog", "stf_t02_demog"))
  expect_identical(tflspec:::.ai_templates_likely("Listing of Deaths", "table"), "listing")
  expect_identical(tflspec:::.ai_templates_likely("", "table"), character())
})

test_that("tfl_ai_compare(): a study without templates; no answer; a repair counted once", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  ad <- aitpl_study(root)
  d <- file.path(root, "study")
  r <- tfl_ai_prepare(unit = "toc", d, aitpl_book(), adam = ad, quiet = TRUE, size = 3L, templates = FALSE)
  cmp <- tfl_ai_compare(d, file = NULL)
  expect_false(file.exists(file.path(d, "ai", "compare.csv")))
  expect_setequal(cmp$output_id, r$toc$toc_id)
  expect_false(any(cmp$templates))
  expect_true(all(is.na(cmp$answered_with)))
  expect_true(all(cmp$check == "no answer"))
  expect_true(all(cmp$repairs == 0L))
  # a draft with an error: a repair, logged once however often it is read
  ai_drafts_from_answer(d, aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-1-1]}",
    "outputs:",
    "  - id: T-14-1-1",
    "    kind: table",
    "    common: {population: SAF}",
    "    template: {name: demo}",
    "assumptions: []"), "table")
  for (i in 1:2) suppressMessages(tfl_ai_collect_answers(d))
  cmp <- tfl_ai_compare(d)
  expect_true(file.exists(file.path(d, "ai", "compare.csv")))
  at <- cmp$output_id == "T-14-1-1"
  expect_identical(cmp$check[at], "error")
  expect_identical(cmp$repairs[at], 1L)
  expect_true(all(cmp$repairs[!at] == 0L))
})

test_that("templates.md carries the facts a trial found only by building", {
  txt <- tflspec:::.ai_templates_list(c("table", "listing", "figure"))
  note <- function(tpl) {
    i <- grep(sprintf("^- `%s`:", tpl), txt)
    expect_length(i, 1L)
    nxt <- grep("^- ", txt)
    nxt <- nxt[nxt > i]
    j <- if (length(nxt)) nxt[1L] - 1L else length(txt)
    paste(txt[i:j][grepl("^  - note: ", txt[i:j])], collapse = " ")
  }
  expect_match(note("param_change"), "ONTRTFL == \"Y\" | ABLFL == \"Y\"", fixed = TRUE)
  expect_match(note("shift"), "need not be in the code list", fixed = TRUE)
  expect_match(note("shift"), "`levels` are the data's values", fixed = TRUE)
  expect_match(note("shift"), "give the report's `codelists` for both the post and the baseline", fixed = TRUE)
  expect_no_match(note("shift"), "codes or its labels", fixed = TRUE)
  expect_match(note("shift"), "`Missing` and `Total` are the template's rows: not in `levels`", fixed = TRUE)
  expect_match(note("lab_shift"), "`Missing` and `Total` are the template's rows: not in `levels`", fixed = TRUE)
  expect_match(note("lab_shift"), "`levels` are the data's values", fixed = TRUE)
  expect_no_match(note("lab_shift"), "codes or its labels", fixed = TRUE)
  expect_match(note("km_survival"), "`pvalue: false`", fixed = TRUE)
  # (the short list, names only, has none)
  expect_false(any(grepl("note:", tflspec:::.ai_templates_list("table", short = TRUE), fixed = TRUE)))
})

test_that("a listing drafted from the listing template is checked as a template, a code SPEC as before", {
  tpl <- tfl_ai_parse(aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [L-1]}",
    "outputs:",
    "  - id: L-1",
    "    kind: listing",
    "    label: Listing of adverse events",
    "    template:",
    "      name: listing",
    "      args:",
    "        data: ADAE",
    "        columns:",
    "          - {vars: [USUBJID], label: Subject}",
    "          - {vars: [AEDECOD], label: Term}",
    "    page: {titles: {\"1\": {center: Listing 16.2.1}}}",
    "assumptions: []"), "table")
  ctx <- tfl_ai_context("table", outputs = "L-1")
  p <- tfl_ai_check(tpl, ctx)
  expect_false(any(p$severity == "error"), info = paste(p$problem, collapse = "; "))
  expect_false(any(grepl("no analysis set", p$problem, fixed = TRUE)))
  # a code SPEC listing without an analysis set: still the error
  code <- tfl_ai_parse(aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [L-1]}",
    "outputs:",
    "  - id: L-1",
    "    kind: listing",
    "    label: Listing of adverse events",
    "    data: {source: dataset, dataset: ADAE}",
    "    content:",
    "      - plan_listing: {cols: [{listing_col: {vars: [USUBJID]}}]}",
    "    page: {titles: {\"1\": {center: Listing 16.2.1}}}",
    "assumptions: []"), "table")
  pc <- tfl_ai_check(code, ctx)
  expect_true(any(grepl("the listing names no analysis set", pc$problem, fixed = TRUE)))
})

test_that("a listing drafted from the listing template is built (RTF)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  root <- withr::local_tempdir()
  ad <- aitpl_study(root)
  d <- file.path(root, "study")
  tfl_ai_prepare(unit = "toc", d, aitpl_book(), adam = ad, quiet = TRUE, size = 3L)
  # (the study's TOC has no listing: the row T-14-2-1 is answered as one)
  ai_drafts_from_answer(d, aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-2-1]}",
    "outputs:",
    "  - id: T-14-2-1",
    "    kind: listing",
    "    label: Listing of adverse events",
    "    extends: [defaults]",
    "    template:",
    "      name: listing",
    "      args:",
    "        data: ADAE",
    "        columns:",
    "          - {vars: [USUBJID], label: Subject}",
    "          - {vars: [AEDECOD], label: Preferred term}",
    "    page: {titles: {\"1\": {center: Listing 16.2.1}}}",
    "assumptions: []"), "table")
  res <- suppressMessages(tfl_ai_run(d))
  rp <- res$reports
  expect_identical(rp$status[rp$output_id == "T-14-2-1"], "ok")
  expect_true(file.exists(file.path(d, rp$rtf[rp$output_id == "T-14-2-1"])))
})

# shift's `levels` and `worst` against the data's values (#636)
aitpl_shift_answer <- function(template = "shift", args = "levels: [Low, Normal, High]", codelists = FALSE,
                                cl_vars = c("ANRIND", "BNRIND"), cl_missing = FALSE, extends = "none") {
  cl <- c("    codelists:",
          paste0("      ", cl_vars,
                 ": [{value: LOW, label: Low}, {value: NORMAL, label: Normal}, {value: HIGH, label: High}",
                 if (cl_missing) ", {value: Missing, label: Missing}", "]"))
  tfl_ai_parse(aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-3-4]}",
    "outputs:",
    "  - id: T-14-3-4",
    "    kind: table",
    "    label: Shift table",
    "    common: {population: SAF}",
    if (codelists) cl,
    paste0("    extends: ", extends),
    "    template:",
    sprintf("      name: %s", template),
    "      args:",
    paste0("        ", args),
    "assumptions: []"), "table")
}
aitpl_shift_check <- function(..., datasets = TRUE, study_dir = NULL) {
  ds <- if (datasets) tfl_ai_adam_summary(list(ADLB = tflspec::adlb, ADSL = tflspec::adsl))
  p <- tfl_ai_check(aitpl_shift_answer(...),
                    tfl_ai_context("table", outputs = "T-14-3-4", datasets = ds, study_dir = study_dir))
  p[grepl("template args (levels|worst)", p$where), , drop = FALSE]
}

test_that("shift: `levels` that match no value of the data are an error, unless a code list gives the text (#636)", {
  # the data's ANRIND values: HIGH, LOW, NORMAL (tflspec::adlb)
  expect_setequal(unique(tflspec::adlb$ANRIND), c("HIGH", "LOW", "NORMAL"))
  p <- aitpl_shift_check()
  expect_identical(p$severity, "error")
  expect_match(p$problem, "Low, Normal, High match no value of ADLB.ANRIND (HIGH, LOW, NORMAL)", fixed = TRUE)
  expect_match(p$problem, "`levels` are the data's values; for other printed text give `codelists` with value and label",
               fixed = TRUE)
  # a code list for both columns: the labels are the printed text
  expect_identical(nrow(aitpl_shift_check(codelists = TRUE)), 0L)
  # a list for one column only leaves the other unmatched: the same silent failure
  for (v in c("ANRIND", "BNRIND")) {
    p <- aitpl_shift_check(codelists = TRUE, cl_vars = v)
    expect_identical(p$severity, "error", info = v)
    expect_match(p$problem, "both ANRIND and BNRIND", fixed = TRUE)
  }
  # the data's values
  expect_identical(nrow(aitpl_shift_check(args = "levels: [LOW, NORMAL, HIGH]")), 0L)
  # a value the data lacks, besides: said, but a shell may have one
  p <- aitpl_shift_check(args = "levels: [LOW, NORMAL, HIGH, VERY HIGH]")
  expect_identical(p$severity, "warning")
  expect_match(p$problem, "VERY HIGH match no value", fixed = TRUE)
  expect_match(p$problem, "will not print", fixed = TRUE)
  # the data not known: nothing to say
  expect_identical(nrow(aitpl_shift_check(datasets = FALSE)), 0L)
  # lab_shift: its post / baseline are ANRIND / BNRIND
  p <- aitpl_shift_check("lab_shift", c("visit: Week 4", "params: [ALT]", "levels: [Low, Normal, High]"))
  expect_identical(p$severity, "error")
  expect_match(p$problem, "of template lab_shift", fixed = TRUE)
})

test_that("shift: `worst` is matched with the data's values, a code list's label does not count (#636)", {
  p <- aitpl_shift_check(args = "worst: [Low, Normal, High]", codelists = TRUE)
  expect_identical(p$severity, "error")
  expect_match(p$problem, "worst of template shift: Low, Normal, High match no value", fixed = TRUE)
  expect_identical(nrow(aitpl_shift_check(args = "worst: [HIGH, LOW, NORMAL]", codelists = TRUE)), 0L)
  p <- aitpl_shift_check(args = "worst: [HIGH, LOW, Normal]")
  expect_identical(p$severity, "warning")
  # (an unmatched `worst` entry ranks nothing: not "will not print")
  expect_match(p$problem, "Normal match no value", fixed = TRUE)
  expect_match(p$problem, "they rank nothing, so check the order", fixed = TRUE)
  expect_no_match(p$problem, "will not print", fixed = TRUE)
})

test_that("shift: `Missing` and `Total` in `levels` are the template's rows: an error, whatever the code lists or `worst` (#636)", {
  for (cl in c(FALSE, TRUE)) {
    # (`Missing` is the template's row when `missing_label` is given)
    p <- aitpl_shift_check(args = c("levels: [LOW, NORMAL, HIGH, Missing]", "missing_label: Missing"),
                           codelists = cl, cl_missing = cl)
    expect_identical(p$severity, "error", info = cl)
    expect_match(p$problem, "`Missing` is a row the template adds: leave it out of `levels`", fixed = TRUE)
  }
  # (worst unset; Total; a label of the report's own)
  p <- aitpl_shift_check(args = c("levels: [LOW, NORMAL, HIGH, Total]", "worst: ~"))
  expect_match(p$problem, "`Total` is a row the template adds", fixed = TRUE)
  p <- aitpl_shift_check(args = c("levels: [LOW, NORMAL, HIGH, Absent]", "missing_label: Absent"))
  expect_match(p$problem, "`Absent` is a row the template adds", fixed = TRUE)
  # (the others of the entries are still judged)
  p <- aitpl_shift_check(args = c("levels: [Low, Missing]", "missing_label: Missing"))
  expect_length(p$problem, 2L)
  # nothing more to say of the data's values with them left out
  expect_identical(nrow(aitpl_shift_check(args = "levels: [LOW, NORMAL, HIGH]")), 0L)
})

test_that("shift, lab_shift: a draft with `post_header` passes the kit check; the note names it (#642)", {
  ds <- tfl_ai_adam_summary(list(ADLB = tflspec::adlb, ADSL = tflspec::adsl))
  ctx <- tfl_ai_context("table", outputs = "T-14-3-4", datasets = ds)
  ok <- c("levels: [LOW, NORMAL, HIGH]", "post_header: 'Worst post-baseline [a]: {col}'")
  p <- tfl_ai_check(aitpl_shift_answer(args = ok), ctx)
  expect_identical(nrow(p[grepl("template args", p$where), , drop = FALSE]), 0L)
  p <- tfl_ai_check(aitpl_shift_answer("lab_shift", c("visit: Week 4", "params: [ALT]", ok)), ctx)
  expect_identical(nrow(p[grepl("template args", p$where), , drop = FALSE]), 0L)
  # an argument the template lacks is said (the check is looking)
  bad <- tfl_ai_check(aitpl_shift_answer(args = c(ok[1L], "post_heading: x")), ctx)
  expect_true(any(grepl("post_heading", bad$problem, fixed = TRUE)))
  expect_match(.ai_tpl_notes$shift, "`post_header`", fixed = TRUE)
  expect_match(.ai_tpl_notes$lab_shift, "`post_header`", fixed = TRUE)
})

# a draft's levels covered by the code lists of the layers it extends (#647)
aitpl_layer_study <- function(defaults = NULL, pattern = NULL, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines("study_id: CL-1", file.path(d, "study.yml"))
  writeLines(c("content: []", if (length(defaults)) c("codelists:", paste0("  ", defaults))),
             file.path(d, "spec", "defaults.yml"))
  if (length(pattern)) {
    dir.create(file.path(d, "spec", "patterns"))
    writeLines(c("codelists:", paste0("  ", pattern)), file.path(d, "spec", "patterns", "pp.yml"))
  }
  d
}
aitpl_layer_lists <- paste0(c("ANRIND", "BNRIND"),
  ": [{value: LOW, label: Low}, {value: NORMAL, label: Normal}, {value: HIGH, label: High}]")

test_that("shift: `levels` covered by the code lists of the layers the draft extends pass (#647)", {
  skip_on_cran()
  d <- aitpl_layer_study(defaults = aitpl_layer_lists)
  expect_identical(nrow(aitpl_shift_check(extends = "defaults", study_dir = d)), 0L)
  # a pattern's, the draft extending it
  dp <- aitpl_layer_study(pattern = aitpl_layer_lists)
  expect_identical(nrow(aitpl_shift_check(extends = "[patterns/pp, defaults]", study_dir = dp)), 0L)
  # not a layer the draft extends: not its lists
  p <- aitpl_shift_check(extends = "none", study_dir = d)
  expect_identical(p$severity, "error")
  p <- aitpl_shift_check(extends = "defaults", study_dir = dp)
  expect_identical(p$severity, "error")
  # lab_shift: the same lists
  expect_identical(nrow(aitpl_shift_check("lab_shift", c("visit: Week 4", "params: [ALT]", "levels: [Low, Normal, High]"),
                                          extends = "defaults", study_dir = d)), 0L)
})

test_that("shift: `levels` the layers' lists do not cover are the same error as with the report's own list (#647)", {
  skip_on_cran()
  d <- aitpl_layer_study(defaults = aitpl_layer_lists)
  own <- aitpl_shift_check(args = "levels: [Low, Normal, Extreme]", codelists = TRUE)
  layer <- aitpl_shift_check(args = "levels: [Low, Normal, Extreme]", extends = "defaults", study_dir = d)
  expect_identical(layer$severity, own$severity)
  expect_identical(layer$problem, own$problem)
  # one column's list only (the other column's has none): the error of the own lists
  one <- aitpl_layer_study(defaults = aitpl_layer_lists[1])
  p <- aitpl_shift_check(extends = "defaults", study_dir = one)
  expect_identical(p$severity, "error")
  expect_match(p$problem, "both ANRIND and BNRIND", fixed = TRUE)
  # the draft's own list of one column and the layer's of the other: both are the report's
  mix <- aitpl_shift_check(codelists = TRUE, cl_vars = "ANRIND", extends = "defaults",
                           study_dir = aitpl_layer_study(defaults = aitpl_layer_lists[2]))
  expect_identical(nrow(mix), 0L)
})

test_that("shift: a draft that extends and a study that cannot be read leave the check out (#647)", {
  skip_on_cran()
  d <- aitpl_layer_study(defaults = aitpl_layer_lists)
  ctx <- tfl_ai_context("table", outputs = "T-14-3-4", study_dir = d,
                        datasets = tfl_ai_adam_summary(list(ADLB = tflspec::adlb, ADSL = tflspec::adsl)))
  # (read when the context was made; not readable when the answer is checked)
  writeLines("codelists: [", file.path(d, "spec", "defaults.yml"))
  expect_error(tfl_read_study_spec(d))
  chk <- function(extends) {
    p <- tfl_ai_check(aitpl_shift_answer(extends = extends), ctx)
    p[grepl("template args (levels|worst)", p$where), , drop = FALSE]
  }
  expect_identical(nrow(chk("defaults")), 0L)
  # a draft that extends nothing needs no study: its own lists are all
  expect_identical(chk("none")$severity, "error")
  # no study in the context: the same
  ctx$study_dir <- NULL
  expect_identical(nrow(chk("defaults")), 0L)
})

test_that("templates.md never cuts a default, and names the km settings that are not parameters", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("ggsurvfit")
  txt <- tflspec:::.ai_templates_list(c("table", "listing", "figure"))
  # (every default is there whole: a cut one ends in "...")
  for (n in tfl_templates()$name) {
    for (x in tflspec:::.tpl_shown(tfl_template_params(n))) {
      v <- tflspec:::.ai_tpl_value(x$default)
      expect_true(any(grepl(v, txt, fixed = TRUE)), info = paste(n, x$name))
    }
  }
  expect_false(any(grepl("= [^:]*[.][.][.]$", txt)))
  # a long default is whole, on the line under its parameter
  i <- grep("^  - `cells`.*whole, on the next line", txt)
  expect_true(length(i) >= 1L)
  expect_match(txt[i[1L] + 1L], "^    ")
  expect_true(any(grepl("cells", txt, fixed = TRUE)))
  # the km types: risk_title and the look, from the registry
  for (nm in c("km_survival", "km_cumulative")) {
    at <- grep(sprintf("^- `%s`:", nm), txt)
    expect_length(at, 1L)
    nxt <- grep("^- ", txt)
    blk <- txt[at:(c(nxt[nxt > at], length(txt) + 1L)[1L] - 1L)]
    blk <- paste(blk, collapse = "
")
    expect_match(blk, "`risk_title`", fixed = TRUE, info = nm)
    expect_match(blk, "plot: {risk_title:", fixed = TRUE, info = nm)
    expect_match(blk, "`censor_shape`", fixed = TRUE, info = nm)
  }
  # a report written as the block says passes the draft check
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adtte"), "AIT-1")
  o <- tfl_output("F-9-ait", "figure", common = list(population = "SAF"), label = "KM", extends = "defaults",
                  data = list(steps = list(list(step = "read", dataset = "ADTTE"),
                                           list(step = "param", value = "PFS"))),
                  template = list(name = "km_survival", args = list(by = "TRT01A")))
  o$plot <- list(risk_title = "Number at risk")
  tfl_write_output(o, file.path(d, "spec", "outputs", "F-9-ait.yml"))
  p <- suppressMessages(suppressWarnings(tfl_check_spec(d)))
  expect_identical(nrow(p[p$level == "error", , drop = FALSE]), 0L)
  # and a look key under plot: is the error the block says
  o$plot <- list(risk_title = "Number at risk", censor_shape = 2)
  tfl_write_output(o, file.path(d, "spec", "outputs", "F-9-ait.yml"))
  p <- suppressMessages(suppressWarnings(tfl_check_spec(d)))
  expect_identical(p$item[p$level == "error"], "plot$censor_shape")
  expect_identical(tfl_check_fig_design(tflspec:::.fig_design_of_output(o))$field, "censor_shape")
})
