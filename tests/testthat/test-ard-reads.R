# tfl_ard_reads(): whose ARD each report reads (G4 B), from the SPEC alone

reads_study <- function() tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))

reads_an <- function(id) list(list(id = id, method = "cards::ard_tabulate", data = "adsl_sub"))

reads_fig <- function(id, source = NULL, ...) {
  page <- if (!is.null(source)) list(report = list(ard_source = source)) else list()
  tfl_output(id, "figure", page = page, ...)
}

test_that("a table with analyses reads its own ARD; a listing none", {
  outs <- list(T1 = tfl_output("T1", "table", ard = list(analyses = reads_an("N"))),
               L1 = tfl_output("L1", "listing"))
  r <- tfl_ard_reads(reads_study(), outs)
  expect_identical(r$report, "T1")
  expect_identical(r$reads, "T1")
  expect_identical(r$kind, "own")
  expect_true(is.na(r$reason))
})

test_that("a figure's ARD source: a table's, own, taken in, or not found", {
  outs <- list(
    T1 = tfl_output("T1", "table", ard = list(analyses = reads_an("N"))),
    L1 = tfl_output("L1", "listing"),
    F1 = reads_fig("F1", "table:T1"),
    F2 = reads_fig("F2", "table:T9"),
    F3 = reads_fig("F3", "table:L1"),
    F4 = reads_fig("F4", "import:cro.json"),
    F5 = reads_fig("F5", "own", ard = list(analyses = reads_an("HR"))),
    F6 = reads_fig("F6", "own"))
  r <- tfl_ard_reads(reads_study(), outs)
  row <- function(id) r[r$report == id, , drop = FALSE]
  expect_identical(row("F1")$reads, "T1")
  expect_identical(row("F1")$kind, "source")
  expect_identical(row("F2")$reason, "source report not found")
  expect_identical(row("F3")$reason, "source not in analyses")
  expect_identical(nrow(row("F4")), 0L)
  # its own ARD once, though its source says own too
  expect_identical(row("F5")$reads, "F5")
  expect_identical(row("F5")$kind, "own")
  expect_identical(row("F6")$reason, "source not in analyses")
  expect_true(all(is.na(r$reads[!is.na(r$reason)])))
})

test_that("a figure with no source: its own analyses (a template's) or a design that reads an ARD", {
  outs <- list(
    F1 = reads_fig("F1", template = list(name = "forest_hr", args = list(param = "PFS")),
                   ard = list(analyses = reads_an("HR"))),
    F2 = reads_fig("F2", data = list(steps = list(list(step = "ard_stats")))),
    F3 = reads_fig("F3"))
  r <- tfl_ard_reads(reads_study(), outs)
  expect_identical(r$report, c("F1", "F2"))
  expect_identical(r$reads, c("F1", NA))
  expect_identical(r$reason, c(NA, "no source"))
})

test_that("from the study's files: a file that cannot be read is a row with its reason", {
  d <- tempfile("reads")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"), full.names = TRUE),
            d, recursive = TRUE)
  writeLines(c("id: [", "kind: table"), file.path(d, "outputs", "T-BAD.yml"))
  r <- tfl_ard_reads(d)
  expect_identical(r$reads[r$report == "T-14-1-1"], "T-14-1-1")
  bad <- r[r$report == "T-BAD", , drop = FALSE]
  expect_identical(nrow(bad), 1L)
  expect_true(is.na(bad$reads))
  expect_match(bad$reason, "^file not read: ")
})

test_that("a figure whose table source is itself; no reports", {
  outs <- list(F1 = reads_fig("F1", "table:F1", ard = list(analyses = reads_an("HR"))))
  r <- tfl_ard_reads(reads_study(), outs)
  expect_identical(r$reads, "F1")
  expect_identical(nrow(tfl_ard_reads(reads_study(), list())), 0L)
  expect_named(tfl_ard_reads(reads_study(), list()), c("report", "reads", "kind", "reason"))
})

test_that("outputs that are not reports, or not named by their ids, stop", {
  expect_error(tfl_ard_reads(reads_study(), list(T1 = list(id = "T1"))), "named list of tfl_output")
  t1 <- tfl_output("T1", "table")
  expect_error(tfl_ard_reads(reads_study(), list(T1 = t1, T1 = t1)), "each once")
  expect_error(tfl_ard_reads(reads_study(), list(T2 = t1)), "each once")
})
