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
  r1 <- tfl_ai_prepare(unit = "toc", d1, aitpl_book(), adam = ad, quiet = TRUE, instructions = "each")
  r0 <- tfl_ai_prepare(unit = "toc", d0, aitpl_book(), adam = ad, quiet = TRUE, instructions = "each", templates = FALSE)
  expect_true(r1$templates)
  expect_false(r0$templates)
  expect_identical(r1$requests$request, r0$requests$request)
  expect_error(tfl_ai_prepare(unit = "toc", d0, aitpl_book(), adam = ad, quiet = TRUE, instructions = "each", templates = "prefer"),
               "TRUE .* or FALSE")
  for (i in seq_len(nrow(r1$requests))) {
    with <- readLines(file.path(d1, r1$requests$file[i]), encoding = "UTF-8")
    without <- readLines(file.path(d0, r0$requests$file[i]), encoding = "UTF-8")
    expect_identical(with[1L], "<!-- templates: TRUE -->")
    expect_false(any(grepl("templates:", without, fixed = TRUE)))
    expect_false(any(grepl("## Templates first", without, fixed = TRUE)))
    # the same request but the setting's line and the templates' part
    at <- match("## Templates first", with)
    sep <- at + match("---", with[-seq_len(at)])
    keep <- with[-c(1L, seq(at, sep - 1L))]
    expect_identical(keep[keep != ""], without[without != ""], info = r1$requests$request[i])
  }
  # a table's request lists the table templates, a figure's the figures'
  t1 <- paste(readLines(file.path(d1, "ai", "requests", "ae-1.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(t1, "- `ae_soc_pt`: Adverse events by SOC and PT (FDA STF Table 14) (table, layout hierarchical; variant of hierarchical)", fixed = TRUE)
  expect_match(t1, "  - `population` [target]*", fixed = TRUE)
  expect_match(t1, "  - `where` [target] (text) = \"TRTEMFL == \\\"Y\\\"\"", fixed = TRUE)
  expect_false(grepl("`km_survival`", t1, fixed = TRUE))
  f1 <- paste(readLines(file.path(d1, "ai", "requests", "figure-1.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(f1, "- `km_survival`:", fixed = TRUE)
  expect_false(grepl("`demog`", f1, fixed = TRUE))
  # a variant registered now is listed, its from said
  tfl_template_variant("demog_x", from = "demog", defaults = list(continuous = "AGE"))
  on.exit(rm(list = "demog_x", envir = tflspec:::.tpl_registry), add = TRUE)
  expect_true(any(grepl("`demog_x`: .*variant of demog", tflspec:::.ai_templates_list("table"))))
  # the agent's instructions say it too
  da <- file.path(root, "agent")
  ra <- tfl_ai_prepare(unit = "toc", da, aitpl_book(), adam = ad, quiet = TRUE, mode = "agent", size = 3L)
  ag <- readLines(file.path(da, "AGENTS.md"), encoding = "UTF-8")
  expect_true(any(grepl("## Templates first", ag, fixed = TRUE)))
  expect_match(ra$agent_prompt, "template where one fits", fixed = TRUE)
  db <- file.path(root, "agent0")
  rb <- tfl_ai_prepare(unit = "toc", db, aitpl_book(), adam = ad, quiet = TRUE, mode = "agent", size = 3L, templates = FALSE)
  expect_false(any(grepl("Templates first", readLines(file.path(db, "AGENTS.md")), fixed = TRUE)))
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
  r <- tfl_ai_prepare(unit = "toc", d, aitpl_book(), adam = ad, quiet = TRUE, instructions = "each")
  tfl_ai_save_answer(d, "demographics-1", aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-1-1]}",
    "outputs:",
    "  - id: T-14-1-1",
    "    kind: table",
    "    label: Demographic and Baseline Characteristics",
    "    population: SAF",
    "    extends: [defaults]",
    "    template: {name: demog, args: {categorical: [AGEGR1, SEX, RACE]}}",
    "    page: {titles: {\"1\": {center: Table 14.1.1}}}",
    "assumptions: []"))
  tfl_ai_save_answer(d, "ae-1", aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-3-1]}",
    "outputs:",
    "  - id: T-14-3-1",
    "    kind: table",
    "    label: Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
    "    population: SAF",
    "    extends: [defaults]",
    "    template: {name: ae_soc_pt}",
    "    content:",
    "      - plan_paginate_rows: {max_rows: 18}",
    "    page: {titles: {\"1\": {center: Table 14.3.1}}}",
    "assumptions: []"))
  tfl_ai_save_answer(d, "labs-1", aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-3-5]}",
    "outputs:",
    "  - id: T-14-3-5",
    "    kind: table",
    "    label: Laboratory values and change from baseline",
    "    population: SAF",
    "    extends: [defaults]",
    "    template:",
    "      name: param_change",
    "      args: {data: ADLB, params: [ALT, AST], visits: [CYCLE 2 DAY 1], param_order: ~, colour: red}",
    "    page: {titles: {\"1\": {center: Table 14.3.5}}}",
    "assumptions: []"))
  tfl_ai_save_answer(d, "figure-1", aitpl_answer(
    "tflspec_ai: {task: figure, version: 1, output_id: F-14-2-2}",
    "data: [{step: read, dataset: ADTTE}, {step: param, value: OS}]",
    "template: {name: km_survival, args: {by: TRT01A, time_unit: months}}",
    "layers:",
    "  - {layer: geom_hline, yintercept: 0.5, linetype: dashed}",
    "assumptions: []"))
  res <- suppressMessages(tfl_ai_run(d))
  rp <- res$reports
  st <- stats::setNames(rp$status, rp$output_id)
  expect_identical(unname(st[c("T-14-1-1", "T-14-3-1", "F-14-2-2")]), rep("ok", 3L))
  for (id in c("T-14-1-1", "T-14-3-1", "F-14-2-2")) {
    expect_true(file.exists(file.path(d, rp$rtf[rp$output_id == id])), info = id)
  }
  expect_true(file.exists(file.path(d, rp$png[rp$output_id == "F-14-2-2"])))
  # the report file keeps the template, as answered
  y <- yaml::read_yaml(file.path(d, "spec", "outputs", "T-14-3-1.yml"))
  expect_identical(y$template$name, "ae_soc_pt")
  # a bad argument: an error that names the template's parameters, a repair
  expect_identical(unname(st["T-14-3-5"]), "error")
  expect_match(rp$first[rp$output_id == "T-14-3-5"],
               "colour is not an argument of template param_change; its parameters are population",
               fixed = TRUE)
  expect_true(file.exists(file.path(d, "ai", "repairs", "labs-1.md")))
  # the repaired answer: built
  a <- readLines(file.path(d, "ai", "answers", "labs-1.md"), encoding = "UTF-8")
  tfl_ai_save_answer(d, "labs-1", sub(", colour: red", "", a, fixed = TRUE))
  res <- suppressMessages(tfl_ai_run(d))
  rp <- res$reports
  expect_identical(rp$status[rp$output_id == "T-14-3-5"], "ok")
  expect_true(file.exists(file.path(d, rp$rtf[rp$output_id == "T-14-3-5"])))
  status <- readLines(file.path(d, "ai", "status.md"), encoding = "UTF-8")
  expect_true("templates: TRUE" %in% status)
  # the comparison: one row a report, what it was answered with, its values,
  # its check, its repairs
  tfl_ai_save_answer(d, "efficacy-1", aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-2-1]}",
    "outputs:",
    "  - id: T-14-2-1",
    "    kind: table",
    "    label: Sex by arm",
    "    population: ITT",
    "    extends: [defaults]",
    "    ard:",
    "      analyses:",
    "        - {id: SEX, method: categorical, dataset: ADSL, population: ITT, by: [TRT01A], variables: [SEX]}",
    "    data: {source: ard}",
    "    content:",
    "      - table_plan: {cols: TRT01A, rows: {group: variable}}",
    "    page: {titles: {\"1\": {center: Table 14.2.1}}}",
    "assumptions: []"))
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

test_that("mode = \"agent\": what every request repeats is a file, written once", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  ad <- aitpl_study(root)
  d1 <- file.path(root, "with")
  d0 <- file.path(root, "without")
  r1 <- tfl_ai_prepare(unit = "toc", d1, aitpl_book(), adam = ad, quiet = TRUE, mode = "agent", size = 3L)
  r0 <- tfl_ai_prepare(unit = "toc", d0, aitpl_book(), adam = ad, quiet = TRUE, mode = "agent", size = 3L, templates = FALSE)
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
  expect_match(tp, "as the schema of `ai/instructions-table.md` says", fixed = TRUE)
  # a request: the files named, its header, the likely templates; no copies
  ae <- paste(readLines(file.path(d1, "ai", "requests", "ae-1.md"), encoding = "UTF-8"), collapse = "\n")
  expect_lt(nchar(ae), 8000)
  expect_match(ae, "Follow `ai/instructions-table.md`", fixed = TRUE)
  expect_match(ae, "Your answer's header: `tflspec_ai: {task: table, version: 1, outputs: [T-14-3-1]}`",
               fixed = TRUE)
  expect_match(ae, "- T-14-3-1: [^\n]*`ae_soc_pt`")
  expect_match(ae, "`ai/adam_summary.md`: name only", fixed = TRUE)
  expect_false(grepl("```yaml", ae, fixed = TRUE))
  expect_false(grepl("### ADSL", ae, fixed = TRUE))
  fg <- paste(readLines(file.path(d1, "ai", "requests", "figure-1.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(fg, "Follow `ai/instructions-figure.md`", fixed = TRUE)
  expect_match(fg, "- F-14-2-2: `km", fixed = TRUE)
  ae0 <- paste(readLines(file.path(d0, "ai", "requests", "ae-1.md"), encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("templates", ae0, fixed = TRUE))
  # AGENTS.md says to read them
  ag <- paste(readLines(file.path(d1, "AGENTS.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(ag, "`ai/instructions-table.md`", fixed = TRUE)
  expect_match(ag, "Read `ai/templates.md` once", fixed = TRUE)
  expect_identical(readLines(file.path(d1, ".github", "copilot-instructions.md")),
                   readLines(file.path(d1, "AGENTS.md")))
  # the likely templates, from a title
  expect_identical(tflspec:::.ai_templates_likely("Demographic and Baseline Characteristics", "table"),
                   c("demog", "stf_t02_demog"))
  expect_identical(tflspec:::.ai_templates_likely("Listing of Deaths", "table"), "listing")
  expect_identical(tflspec:::.ai_templates_likely("", "table"), character())
})

test_that("a chat's request over max_chars: the templates' arguments by name only", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  ad <- aitpl_study(root)
  d1 <- file.path(root, "full")
  d2 <- file.path(root, "short")
  r1 <- tfl_ai_prepare(unit = "toc", d1, aitpl_book(), adam = ad, quiet = TRUE, instructions = "each")
  r2 <- suppressWarnings(tfl_ai_prepare(unit = "toc", d2, aitpl_book(), adam = ad, quiet = TRUE, instructions = "each",
                                        max_chars = 45000))
  ae1 <- paste(readLines(file.path(d1, "ai", "requests", "ae-1.md"), encoding = "UTF-8"), collapse = "\n")
  ae2 <- paste(readLines(file.path(d2, "ai", "requests", "ae-1.md"), encoding = "UTF-8"), collapse = "\n")
  expect_lt(nchar(ae2), nchar(ae1))
  expect_lte(nchar(ae2), 45000)
  expect_match(ae2, "- `ae_soc_pt`: Adverse events by SOC and PT", fixed = TRUE)
  expect_match(ae2, "  - `population` [target]*, ", fixed = TRUE)
  expect_false(grepl("(text) = ", ae2, fixed = TRUE))
  expect_match(ae1, "(text) = ", fixed = TRUE)
})

test_that("tfl_ai_compare(): a study without templates; no answer; a repair counted once", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  ad <- aitpl_study(root)
  d <- file.path(root, "study")
  r <- tfl_ai_prepare(unit = "toc", d, aitpl_book(), adam = ad, quiet = TRUE, instructions = "each", templates = FALSE)
  cmp <- tfl_ai_compare(d, file = NULL)
  expect_false(file.exists(file.path(d, "ai", "compare.csv")))
  expect_setequal(cmp$output_id, r$toc$toc_id)
  expect_false(any(cmp$templates))
  expect_true(all(is.na(cmp$answered_with)))
  expect_true(all(cmp$check == "no answer"))
  expect_true(all(cmp$repairs == 0L))
  # an answer with an error: a repair, logged once however often it is read
  tfl_ai_save_answer(d, "demographics-1", aitpl_answer(
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-1-1]}",
    "outputs:",
    "  - id: T-14-1-1",
    "    kind: table",
    "    population: SAF",
    "    template: {name: demo}",
    "assumptions: []"))
  for (i in 1:2) suppressMessages(tfl_ai_collect_answers(d))
  cmp <- tfl_ai_compare(d)
  expect_true(file.exists(file.path(d, "ai", "compare.csv")))
  at <- cmp$output_id == "T-14-1-1"
  expect_identical(cmp$check[at], "error")
  expect_identical(cmp$repairs[at], 1L)
  expect_true(all(cmp$repairs[!at] == 0L))
})
