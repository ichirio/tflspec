# A report's own R content (`content: !r`, canon 11-28, tflspec#1018): one
# content contract for every kind (a ggplot, an rtfplot, a table_plan, a
# data frame or rtftable pages, or a list of them) and one way its code sees the report's data (the report's data
# rows, the study's ADaM it names, `ard` when it has analyses; never
# tflspec's sample data), the same in a session (the preview,
# tfl_run_output(), tfl_check_build()) and in the report's program.  A
# fictional study whose ADSL differs from the sample's (a third SEX, "U").

oc_study <- function() {
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- tempfile("own-content-")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  adsl <- tflspec:::.ai_chr(tflspec::adsl)[seq_len(30L), ]
  adsl$SEX <- rep(c("F", "M", "U"), length.out = 30L)
  haven::write_xpt(adsl, file.path(d, "adam", "adsl.xpt"))
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "OWN"))
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE, showWarnings = FALSE)
  d
}

oc_write <- function(d, o) tfl_write_output(o, file.path(d, "spec", "outputs", paste0(o$id, ".yml")))

oc_resolved <- function(d, id) {
  st <- tfl_read_study_spec(d)
  list(st = st, ro = tfl_resolve_output(tfl_read_output(file.path(d, "spec", "outputs",
                                                                   paste0(id, ".yml"))), st))
}

# the table's code: its ARD made from the study's ADSL, its plan
oc_plan_code <- paste(
  "a <- cards::ard_stack(adsl, .by = TRT01A, cards::ard_tabulate(variables = SEX))",
  "content <- rtfreporter::table_plan(rtfreporter::normalize_ard(a), cols = \"TRT01A\")",
  sep = "\n")

# the report's program (tfl_report_program()) for a report of the study
oc_program <- function(d, id, type = "table") {
  x <- oc_resolved(d, id)
  o <- tfl_read_output(file.path(d, "spec", "outputs", paste0(id, ".yml")))
  r <- list(id = id, type = type, program = paste0(id, ".R"), file = paste0(id, ".rtf"),
            steps = list(data = NA_character_, content = as.character(o$content)),
            output = o, spec_output = o)
  tfl_report_program(r, study = x$st, shared = list(ard = tflspec:::.ard_spec_of_study(x$st)$spec),
                     date = as.Date("2026-01-01"), generator = "tflspec 0.0.0")
}

# the program's data and content parts run as the program runs them: after
# the study's setup (which reads the study's ADaM: `adsl`), with
# fig_setup.R's report_content()
oc_run_parts <- function(code, objects) {
  from <- grep("^# ---- data", code)[1L]
  to <- grep("^# ---- report", code)[1L] - 1L
  env <- new.env(parent = globalenv())
  eval(parse(text = tflspec:::.setup_report_content), env)
  for (n in names(objects)) assign(n, objects[[n]], envir = env)
  eval(parse(text = code[from:to]), env)
  get("content", envir = env)
}

oc_has_u <- function(plan) any(unlist(plan, use.names = FALSE) %in% "U")

test_that("a table of its own content code, no data or analysis: the study's ADSL in every path", {
  skip_on_cran()
  d <- oc_study()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  oc_write(d, tfl_output("T-1", "table", content = tfl_r(oc_plan_code)))
  x <- oc_resolved(d, "T-1")
  # the preview: what the code reads, then its run
  data <- tflspec:::.own_content_data(x$ro, x$st, d)
  expect_identical(names(data), "adsl")
  expect_setequal(unique(data$adsl$SEX), c("F", "M", "U"))
  plan <- suppressMessages(tfl_content_run(x$ro, data))
  expect_s3_class(plan, "table_plan")
  expect_true(oc_has_u(plan))
  # tfl_run_output() and tfl_check_build()
  rtf <- tfl_run_output("T-1", d)
  expect_true(file.exists(rtf))
  expect_match(paste(readLines(rtf, warn = FALSE), collapse = " "), "\\bU\\b")
  expect_identical(nrow(tfl_check_build(d, "T-1")), 0L)
  # the program: no ARD read (the report has no analyses), the contract's
  # check, the same plan from the study's ADSL
  code <- oc_program(d, "T-1")
  expect_false(any(grepl("readRDS(file.path(path_ard", code, fixed = TRUE)))
  expect_false(any(grepl("The study ARD has no rows", code, fixed = TRUE)))
  expect_true("content <- report_content(content)" %in% code)
  p2 <- suppressMessages(oc_run_parts(code, list(adsl = data$adsl)))
  expect_s3_class(p2, "table_plan")
  expect_true(oc_has_u(p2))
})

test_that("a table's own content code: a data frame and rtftable pages still work; a number gives the classed stop", {
  skip_on_cran()
  d <- oc_study()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # a data frame and rtftable pages, as report_content() took them before:
  # in the session, tfl_run_output(), tfl_check_build() and the program
  df_code <- "content <- adsl[adsl$SEX == \"U\", c(\"USUBJID\", \"SEX\")]"
  for (code in c(df_code, paste0(df_code, "\ncontent <- rtfreporter::as_rtftables(content)"))) {
    oc_write(d, tfl_output("T-2", "table", content = tfl_r(code)))
    x <- oc_resolved(d, "T-2")
    data <- tflspec:::.own_content_data(x$ro, x$st, d)
    got <- suppressMessages(tfl_content_run(x$ro, data))
    expect_true(is.data.frame(got) || inherits(got[[1L]], "rtftable"), info = code)
    rtf <- tfl_run_output("T-2", d)
    expect_match(paste(readLines(rtf, warn = FALSE), collapse = " "), "\\bU\\b", info = code)
    expect_identical(nrow(tfl_check_build(d, "T-2")), 0L, info = code)
    p2 <- suppressMessages(oc_run_parts(oc_program(d, "T-2"), list(adsl = data$adsl)))
    expect_true(is.data.frame(p2) || inherits(p2[[1L]], "rtftable"), info = code)
  }
  # a number: the same classed stop in the session, tfl_run_output(),
  # tfl_check_build() and the program (which says the same words)
  oc_write(d, tfl_output("T-2", "table", content = tfl_r("content <- 1")))
  x <- oc_resolved(d, "T-2")
  data <- tflspec:::.own_content_data(x$ro, x$st, d)
  e1 <- expect_error(tfl_content_run(x$ro, data), class = "tfl_content_contract")
  expect_identical(conditionMessage(e1), paste0(
    "T-2: `content` is numeric: make it a ggplot, an rtfplot, a table_plan, a data frame ",
    "or rtftable pages (or a list of them)."))
  e2 <- expect_error(tfl_run_output("T-2", d), class = "tfl_content_contract")
  expect_match(conditionMessage(e2), conditionMessage(e1), fixed = TRUE)
  cb <- tfl_check_build(d, "T-2")
  expect_identical(cb$level, "error")
  expect_match(cb$message, conditionMessage(e1), fixed = TRUE)
  e3 <- expect_error(oc_run_parts(oc_program(d, "T-2"), list(adsl = data$adsl)),
                     class = "tfl_content_contract")
  expect_match(conditionMessage(e1), conditionMessage(e3), fixed = TRUE)
  # an item of a list: said which
  expect_error(tflspec:::.own_content_contract(list(data$adsl, 1), "T-9"),
               "T-9: `content` (item 2) is numeric", fixed = TRUE, class = "tfl_content_contract")
})

test_that("own content code never reads tflspec's sample data: a classed stop names the dataset", {
  skip_on_cran()
  d <- oc_study()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  oc_write(d, tfl_output("T-3", "table", content = tfl_r(
    "content <- rtfreporter::table_plan(adae, cols = \"TRT01A\")")))
  x <- oc_resolved(d, "T-3")
  # (the study has no ADAE; tflspec's sample adae is on the search path)
  data <- tflspec:::.own_content_data(x$ro, x$st, d)
  expect_false("adae" %in% names(data))
  expect_error(tfl_content_run(x$ro, data), "reads ADAE, which is not among the report's data",
               class = "tfl_content_data")
  expect_error(tfl_run_output("T-3", d), class = "tfl_content_data")
  # the program: a stop() line before the code
  code <- oc_program(d, "T-3")
  expect_true(any(grepl("^stop\\(\"T-3: the report's own code reads ADAE", code)))
  # code that makes the dataset itself reads no sample data
  o <- x$ro
  o$content <- tfl_r("adae <- adsl\ncontent <- rtfreporter::table_plan(adae, cols = \"TRT01A\")")
  expect_error(tflspec:::.own_reads_given(o, "adsl"), NA)
})

test_that("own content code reads the report's data rows by id (ard.analysis_data)", {
  skip_on_cran()
  d <- oc_study()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  o <- tfl_output("T-4", "table", content = tfl_r(paste(
    "a <- cards::ard_stack(adsl_u, .by = TRT01A, cards::ard_tabulate(variables = SEX))",
    "content <- rtfreporter::table_plan(rtfreporter::normalize_ard(a), cols = \"TRT01A\")",
    sep = "\n")))
  o$ard <- list(analysis_data = list(list(id = "adsl_u", from = "ADSL", where = tfl_r("SEX == \"U\""))))
  oc_write(d, o)
  x <- oc_resolved(d, "T-4")
  data <- tflspec:::.own_content_data(x$ro, x$st, d)
  expect_true("adsl_u" %in% names(data))
  expect_identical(unique(data$adsl_u$SEX), "U")
  plan <- suppressMessages(tfl_content_run(x$ro, data))
  expect_s3_class(plan, "table_plan")
  expect_true(file.exists(tfl_run_output("T-4", d)))
  # the program makes it by its ARD definition's data part
  code <- oc_program(d, "T-4")
  expect_true(any(grepl("^adsl_u <- ", code)))
  expect_false(any(grepl("readRDS(file.path(path_ard", code, fixed = TRUE)))
})

test_that("a figure of its own content code: unchanged, its contract the same in both paths", {
  skip_on_cran()
  d <- oc_study()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  oc_write(d, tfl_output("F-1", "figure", content = tfl_r(
    "content <- ggplot2::ggplot(adsl, ggplot2::aes(AGE)) + ggplot2::geom_histogram(bins = 5)")))
  x <- oc_resolved(d, "F-1")
  data <- tflspec:::.own_content_data(x$ro, x$st, d)
  p <- suppressMessages(tfl_content_run(x$ro, data))
  expect_s3_class(p, "ggplot")
  rtf <- suppressWarnings(tfl_run_output("F-1", d))
  expect_match(paste(readLines(rtf, warn = FALSE), collapse = " "), "\\pict", fixed = TRUE)
  code <- oc_program(d, "F-1", type = "figure")
  # (its program's check is report_content(), as before)
  expect_true("content <- report_content(content)" %in% code)
  # ggplots, alone or in a list, a data frame and a table_plan hold (one
  # contract for every kind); a number does not
  plan <- rtfreporter::table_plan(data$adsl[1:2, c("USUBJID", "SEX")])
  expect_error(tflspec:::.own_content_contract(list(p, p), "F-1"), NA)
  expect_error(tflspec:::.own_content_contract(list(p, data$adsl), "F-1"), NA)
  expect_error(tflspec:::.own_content_contract(list(p, plan), "F-1"), NA)
  expect_error(tflspec:::.own_content_contract(list(), "F-1"), NA)
  expect_error(tflspec:::.own_content_contract(list(p, 1), "F-1"),
               "F-1: `content` (item 2) is numeric", fixed = TRUE, class = "tfl_content_contract")
  o <- x$ro
  o$content <- tfl_r("content <- 1")
  expect_error(tfl_content_run(o, data), "`content` is numeric", fixed = TRUE,
               class = "tfl_content_contract")
})

test_that("an analysis of method custom and content code reading the ARD: still works", {
  skip_on_cran()
  d <- oc_study()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  o <- tfl_output("T-5", "table", content = tfl_r(
    "content <- rtfreporter::table_plan(rtfreporter::normalize_ard(ard), cols = \"TRT01A\")"))
  o$data <- list(population = "SAF")
  o$ard <- list(analyses = list(list(
    id = "AGE", method = "custom", dataset = "ADSL", by = "TRT01A",
    code = tfl_r("cards::ard_summary(data, by = TRT01A, variables = AGE)"))))
  oc_write(d, o)
  x <- oc_resolved(d, "T-5")
  data <- tflspec:::.own_content_data(x$ro, x$st, d)
  expect_true("ard" %in% names(data))
  expect_true(nrow(data$ard) > 0L)
  plan <- suppressMessages(tfl_content_run(x$ro, data))
  expect_s3_class(plan, "table_plan")
  expect_true(file.exists(tfl_run_output("T-5", d)))
  expect_identical(nrow(tfl_check_build(d, "T-5")), 0L)
  # the program reads its rows of the study ARD
  code <- oc_program(d, "T-5")
  expect_true(any(grepl("ard <- readRDS(file.path(path_ard", code, fixed = TRUE)))
})
