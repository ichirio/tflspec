# tfl_check_spec(): a table's column header N with no BIGN to take it from
# (#969: the AI baseline drafts printed N=NA under Total and the arms)

hn_report <- function(analyses = list(), header = "{col}\n(N={n})", values = NULL, total = TRUE,
                      cols = "TRT01A") {
  ph <- list(header = list(list(line = "1", cols = ".values", span = "each", text = header)))
  if (!is.null(values)) ph$values <- values
  list(kind = "table", ard = list(analyses = analyses), content = c(
    list(tfl_item("table_plan", list(cols = cols, rows = "variable"))),
    if (total) list(tfl_item("plan_total", list(label = "Total"))),
    list(tfl_item("plan_col_header", ph))))
}
hn_pop <- list(id = "POP", method = "dichotomous", data = "adsl_sub", by = "TRT01A",
               variables = c("SAFFL", "ITTFL"))
hn_bign <- function(v = "TRT01A") Filter(Negate(is.null), list(
  id = "BIGN", method = "categorical", data = "adsl_sub", variables = v))

test_that("a header's {n} and no BIGN: a warning with the analysis to add", {
  p <- .check_header_n_rows(hn_report(list(hn_pop)), "T-POP")
  expect_identical(nrow(p), 1L)
  expect_identical(p$level, "warning")
  expect_identical(p$part, "content")
  expect_identical(p$item, "plan_col_header(header)")
  expect_match(p$message, "has none", fixed = TRUE)
  expect_match(p$message, "variables: [TRT01A]", fixed = TRUE)
  expect_match(p$message, "the Total column's N is their sum", fixed = TRUE)
  # no Total column: not named
  p2 <- .check_header_n_rows(hn_report(list(hn_pop), total = FALSE), "T-POP")
  expect_false(grepl("Total", p2$message, fixed = TRUE))
  # the other tokens ask for it too: {n1}, {n:Placebo}
  expect_identical(nrow(.check_header_n_rows(hn_report(list(hn_pop), header = "{col} {n:sum}"), "T")), 1L)
  expect_identical(nrow(.check_header_n_rows(hn_report(list(hn_pop), header = "{col} [{n1}]"), "T")), 1L)
})

test_that("a BIGN by the column variable: no row; by another variable: a warning naming both", {
  expect_identical(NROW(.check_header_n_rows(hn_report(list(hn_bign(), hn_pop)), "T-POP")), 0L)
  # columns of two depths: BIGN's variable at either is a column's
  expect_identical(NROW(.check_header_n_rows(
    hn_report(list(hn_bign("SEX"), hn_pop), cols = "TRT01A | SEX"), "T")), 0L)
  p <- .check_header_n_rows(hn_report(list(hn_bign("TRT01P"), hn_pop)), "T-POP")
  expect_identical(nrow(p), 1L)
  expect_identical(p$level, "warning")
  expect_match(p$message, "counts by TRT01P", fixed = TRUE)
  expect_match(p$message, "the table's columns are TRT01A", fixed = TRUE)
  expect_match(p$message, "`variables: [TRT01A]`", fixed = TRUE)
  # (a `by` counts as BIGN's grouping too)
  b <- list(id = "BIGN", method = "categorical", data = "adsl_sub", by = "TRT01A", variables = "SAFFL")
  expect_identical(NROW(.check_header_n_rows(hn_report(list(b)), "T")), 0L)
})

test_that("no row: a header with its own values or no {n}, a BIGN of one N, a table of records", {
  expect_identical(NROW(.check_header_n_rows(hn_report(list(hn_pop), values = list(n = c(A = 3))), "T")), 0L)
  expect_identical(NROW(.check_header_n_rows(hn_report(list(hn_pop), header = "{col}"), "T")), 0L)
  # BIGN with no variable: the analysis set's one N, every column's
  expect_identical(NROW(.check_header_n_rows(hn_report(list(hn_bign(NULL), hn_pop)), "T")), 0L)
  r <- hn_report(list(hn_pop))
  r$data <- list(source = "dataset")
  expect_identical(NROW(.check_header_n_rows(r, "T")), 0L)
  expect_identical(NROW(.check_header_n_rows(list(kind = "figure", content = list()), "F")), 0L)
  # a header given as lines asks as the cells do
  r <- hn_report(list(hn_pop))
  r$content[[3L]] <- tfl_item("plan_col_header", list(lines = c("{col}", "(N={n})")))
  expect_identical(nrow(.check_header_n_rows(r, "T")), 1L)
})

test_that("in tfl_check_spec(): the report's row, gone with a BIGN by the columns", {
  skip_if_not_installed("haven")
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  # fictional subjects
  adsl <- data.frame(STUDYID = "EX-1", USUBJID = sprintf("EX-1-%03d", 1:6),
                     TRT01A = rep(c("Drug X", "Placebo"), 3), SAFFL = "Y",
                     ITTFL = c("Y", "Y", "Y", "Y", "N", "Y"), stringsAsFactors = FALSE)
  haven::write_xpt(adsl, file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "EX-1")
  r <- hn_report(list(hn_pop))
  o <- tfl_output("T-POP", "table", common = list(population = "SAF"), label = "Populations",
    ard = list(analysis_data = list(list(id = "adsl_sub", from = "ADSL", population = "SAF")),
               analyses = r$ard$analyses),
    content = r$content)
  f <- file.path(d, "spec", "outputs", "T-POP.yml")
  tfl_write_output(o, f)
  p <- tfl_check_spec(d, "T-POP", adam = FALSE)
  h <- p[p$item %in% "plan_col_header(header)", ]
  expect_identical(nrow(h), 1L)
  expect_identical(h$level, "warning")
  expect_identical(h$output_id, "T-POP")
  o$ard$analyses <- list(hn_bign(), hn_pop)
  tfl_write_output(o, f)
  p <- tfl_check_spec(d, "T-POP", adam = FALSE)
  expect_false(any(p$item %in% "plan_col_header(header)"))
})
