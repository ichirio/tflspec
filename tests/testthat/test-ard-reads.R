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

# ---- the review's findings (#917) --------------------------------------------

# every row is worked out, or says why not
reads_invariant <- function(r) {
  expect_true(all(!is.na(r$reads) | (!is.na(r$reason) & nzchar(r$reason))))
}

reads_stats <- list(list(step = "ard_stats"))

test_that("M1: an ard_stats step under data: stats:, or in a plot of a composed figure, is seen", {
  f1 <- reads_fig("F1", data = list(stats = reads_stats))
  f2 <- reads_fig("F2")
  f2$plots <- list(a = list(layers = list(list(layer = "point"))),
                   b = list(stats = reads_stats))
  f3 <- reads_fig("F3")
  f3$plots <- list(a = list(layers = list(list(layer = "point"))))
  r <- tfl_ard_reads(reads_study(), list(F1 = f1, F2 = f2, F3 = f3))
  expect_identical(r$report, c("F1", "F2"))
  expect_identical(r$reason, c("no source", "no source"))
  expect_identical(r$kind, c("source", "source"))
  reads_invariant(r)
})

test_that("M2: a report with figure: and no analyses reads an ARD; with analyses its own", {
  f1 <- reads_fig("F1")
  f1$figure <- list(name = "forest_hr", args = list())
  f2 <- reads_fig("F2", ard = list(analyses = reads_an("HR")))
  f2$figure <- list(name = "forest_hr", args = list())
  r <- tfl_ard_reads(reads_study(), list(F1 = f1, F2 = f2))
  expect_identical(r$report, c("F1", "F2"))
  expect_identical(r$reads, c(NA, "F2"))
  expect_identical(r$kind, c("source", "own"))
  expect_identical(r$reason, c("no source", NA))
  reads_invariant(r)
})

test_that("M3: tfl_ard_number as an item, at the top level or inside a named plot, is seen", {
  num <- tfl_item("tfl_ard_number", list(analysis_id = "KM", stat = "estimate"))
  gg <- tfl_item("ggplot", list(data = "adtte"))
  f1 <- reads_fig("F1", content = list(gg, num))
  f2 <- reads_fig("F2", content = list(tfl_plot("km", list(gg, num))))
  f3 <- reads_fig("F3", content = list(tfl_plot("km", list(gg))))
  r <- tfl_ard_reads(reads_study(), list(F1 = f1, F2 = f2, F3 = f3))
  expect_identical(r$report, c("F1", "F2"))
  expect_identical(r$reason, c("no source", "no source"))
  reads_invariant(r)
})

test_that("M4: a content of R code is not looked into; its data steps and own analyses still count", {
  code <- tfl_r("ggplot(adsl) + geom_bar(aes(SEX))")
  f1 <- reads_fig("F1", content = code)
  f2 <- reads_fig("F2", content = code, ard = list(analyses = reads_an("HR")))
  f3 <- reads_fig("F3", content = code, data = list(steps = reads_stats))
  r <- tfl_ard_reads(reads_study(), list(F1 = f1, F2 = f2, F3 = f3))
  expect_identical(r$report, c("F2", "F3"))
  expect_identical(r$reads, c("F2", NA))
  expect_identical(r$reason, c(NA, "no source"))
  reads_invariant(r)
})

test_that("S1: a ref analysis is not worked out; the report's other analyses are its own", {
  outs <- list(
    T2 = tfl_output("T2", "table", ard = list(analyses = list(list(id = "N", ref = "T1")))),
    T3 = tfl_output("T3", "table", ard = list(analyses = c(reads_an("N"),
                                                           list(list(id = "M", ref = "T1"))))))
  r <- tfl_ard_reads(reads_study(), outs)
  expect_identical(r$report, c("T2", "T3", "T3"))
  expect_identical(r$reads, c(NA, "T3", NA))
  expect_identical(r$kind, c("own", "own", "own"))
  expect_identical(r$reason, c("ref analysis: T1", NA, "ref analysis: T1"))
  reads_invariant(r)
})

test_that("S2: a source that is not understood is a row", {
  bad <- c("T-14-2-2", "Table:T1", "import:", "table:")
  outs <- stats::setNames(lapply(seq_along(bad), function(i) {
    reads_fig(paste0("F", i), bad[i])
  }), paste0("F", seq_along(bad)))
  r <- tfl_ard_reads(reads_study(), outs)
  expect_identical(r$report, names(outs))
  expect_true(all(is.na(r$reads)))
  expect_identical(r$kind, rep("source", length(bad)))
  expect_identical(r$reason, paste0("source not understood: ", bad))
  reads_invariant(r)
})

test_that("S3: a template written in map form ({name: }) with analyses of its own is one own row", {
  f1 <- reads_fig("F1", template = list(name = "forest_hr"),
                  ard = list(analyses = reads_an("HR")),
                  data = list(steps = reads_stats))
  r <- tfl_ard_reads(reads_study(), list(F1 = f1))
  expect_identical(nrow(r), 1L)
  expect_identical(r$reads, "F1")
  expect_identical(r$kind, "own")
  reads_invariant(r)
})

test_that("a template that does not resolve is a row, and so is a source that names it", {
  t1 <- tfl_output("T1", "table", template = list(name = "no_such_template", args = list()))
  f1 <- reads_fig("F1", "table:T1")
  f2 <- reads_fig("F2", template = list(name = "no_such_template", args = list()))
  r <- tfl_ard_reads(reads_study(), list(T1 = t1, F1 = f1, F2 = f2))
  row <- function(id) r[r$report == id, , drop = FALSE]
  expect_identical(nrow(row("T1")), 1L)
  expect_match(row("T1")$reason, "^lookup failed")
  expect_true(is.na(row("T1")$reads))
  expect_identical(row("F1")$reason, "source lookup failed")
  expect_match(row("F2")$reason, "^lookup failed")
  reads_invariant(r)
})

test_that("a figure's own row stays when its source cannot be worked out", {
  f1 <- reads_fig("F1", c("own", "table:T1"), ard = list(analyses = reads_an("HR")))
  r <- tfl_ard_reads(reads_study(), list(F1 = f1))
  expect_identical(r$report, c("F1", "F1"))
  expect_identical(r$reads, c("F1", NA))
  expect_identical(r$kind, c("own", "source"))
  expect_match(r$reason[2L], "^lookup failed")
  reads_invariant(r)
})

test_that("outputs that is not a list stops", {
  expect_error(tfl_ard_reads(reads_study(), "T1"), "named list of tfl_output")
  expect_error(tfl_ard_reads(reads_study(), character(0)), "named list of tfl_output")
})

test_that("a template figure whose analyses come only from the template is one own row", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  tplfig_study(d, c("adsl", "adtte"), "RD-1")
  o <- tfl_output("F-9-1", "figure", common = list(population = "SAF"), label = "Forest",
                  extends = "defaults",
                  template = list(name = "forest_hr",
                                  args = list(param = "PFS", population = "SAF",
                                              subgroups = c("SEX", "AGEGR1"))))
  r <- tfl_ard_reads(tfl_read_study_spec(d), list(`F-9-1` = o))
  expect_identical(nrow(r), 1L)
  expect_identical(r$reads, "F-9-1")
  expect_identical(r$kind, "own")
  expect_true(is.na(r$reason))
  reads_invariant(r)
})
