# tfl_ard_reads() (R/ard_reads.R): every reason its help lists, each with
# the invariant that a row has a read or a reason, never both, never
# neither; and the input shapes no other test reaches.  Fictional data only.

cov_reads_study <- function() {
  tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
}

cov_an <- function(id) list(list(id = id, method = "cards::ard_tabulate", data = "adsl_sub"))

cov_fig <- function(id, source = NULL, ...) {
  page <- if (!is.null(source)) list(report = list(ard_source = source)) else list()
  tfl_output(id, "figure", page = page, ...)
}

# a row has a read or a reason (NA when worked out), never both, never neither
cov_invariant <- function(r) {
  has_reason <- !is.na(r$reason) & nzchar(r$reason)
  expect_true(all(is.na(r$reads) == has_reason))
}

cov_row <- function(r, id) r[r$report == id, , drop = FALSE]

# ---- each reason ----------------------------------------------------------

test_that("reason \"no source\": a figure whose design reads an ARD and no source is chosen", {
  r <- tfl_ard_reads(cov_reads_study(),
                     list(F1 = cov_fig("F1", data = list(steps = list(list(step = "ard_stats"))))))
  expect_identical(r$reason, "no source")
  expect_identical(r$kind, "source")
  cov_invariant(r)
})

test_that("reason \"source not understood\": a value that is none of own, table:, import:", {
  r <- tfl_ard_reads(cov_reads_study(), list(F1 = cov_fig("F1", "owner"),
                                             F2 = cov_fig("F2", "import:  ")))
  expect_identical(r$reason, c("source not understood: owner", "source not understood: import:"))
  cov_invariant(r)
})

test_that("reason \"source not in analyses\": the source report has no analyses of its own", {
  outs <- list(T1 = tfl_output("T1", "table", ard = list(analyses = list())),
               F1 = cov_fig("F1", "table:T1"))
  r <- tfl_ard_reads(cov_reads_study(), outs)
  expect_identical(r$report, "F1")
  expect_identical(r$reason, "source not in analyses")
  cov_invariant(r)
})

test_that("reason \"source report not found\": the source is not among the outputs", {
  r <- tfl_ard_reads(cov_reads_study(), list(F1 = cov_fig("F1", "table:T-14-9-9")))
  expect_identical(r$reason, "source report not found")
  cov_invariant(r)
})

test_that("reason \"source lookup failed\": the source report's analyses could not be worked out", {
  outs <- list(T1 = tfl_output("T1", "table", template = list(name = "no_such_v1/demog")),
               F1 = cov_fig("F1", "table:T1"))
  r <- tfl_ard_reads(cov_reads_study(), outs)
  expect_identical(cov_row(r, "F1")$reason, "source lookup failed")
  expect_match(cov_row(r, "T1")$reason, "^lookup failed: ")
  cov_invariant(r)
})

test_that("reason \"ref analysis\": every ref named once, in one row", {
  an <- list(list(id = "A", ref = "T2"), list(id = "B", ref = "T3"), list(id = "C", ref = "T2"))
  r <- tfl_ard_reads(cov_reads_study(),
                     list(T1 = tfl_output("T1", "table", ard = list(analyses = an))))
  expect_identical(r$reason, "ref analysis: T2, T3")
  expect_identical(r$kind, "own")
  cov_invariant(r)
})

test_that("reason \"file not read\": two files that cannot be read are two rows", {
  d <- tempfile("cov-reads")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"), full.names = TRUE),
            d, recursive = TRUE)
  writeLines(c("id: [", "kind: table"), file.path(d, "outputs", "T-BAD-1.yml"))
  writeLines("kind: {", file.path(d, "outputs", "T-BAD-2.yml"))
  r <- tfl_ard_reads(d)
  bad <- r[startsWith(r$report, "T-BAD"), , drop = FALSE]
  expect_identical(sort(bad$report), c("T-BAD-1", "T-BAD-2"))
  expect_true(all(startsWith(bad$reason, "file not read: ")))
  expect_identical(bad$kind, c("own", "own"))
  cov_invariant(r)
})

test_that("reason \"lookup failed\": a source of two values; the own row stays", {
  f1 <- cov_fig("F1", c("table:T1", "table:T2"), ard = list(analyses = cov_an("HR")))
  r <- tfl_ard_reads(cov_reads_study(), list(F1 = f1))
  expect_identical(r$reads, c("F1", NA))
  expect_identical(r$reason[[2L]], "lookup failed: `page: report: ard_source:` is one value.")
  cov_invariant(r)
})

test_that("reason \"lookup failed\": a report whose rows cannot be made is one row", {
  # (a ref that cannot be written as text: the refs' names cannot be made)
  an <- list(list(id = "A", ref = new.env()))
  r <- tfl_ard_reads(cov_reads_study(),
                     list(T1 = tfl_output("T1", "table", ard = list(analyses = an))))
  expect_identical(nrow(r), 1L)
  expect_match(r$reason, "^lookup failed: ")
  cov_invariant(r)
})

test_that("reason \"lookup failed\": a template name that is not one text", {
  t1 <- tfl_output("T1", "table", template = list(name = c("a_v1/x", "b_v1/y")))
  r <- tfl_ard_reads(cov_reads_study(), list(T1 = t1))
  expect_match(r$reason, "^lookup failed: .*template: name:")
  cov_invariant(r)
})

# ---- the source's shapes ---------------------------------------------------

test_that("a source of NA or an empty vector is no source", {
  f1 <- cov_fig("F1", NA_character_, data = list(steps = list(list(step = "ard_stats"))))
  f2 <- cov_fig("F2", character(), data = list(steps = list(list(step = "ard_stats"))))
  f3 <- cov_fig("F3", NA_character_)
  r <- tfl_ard_reads(cov_reads_study(), list(F1 = f1, F2 = f2, F3 = f3))
  expect_identical(r$report, c("F1", "F2"))
  expect_identical(r$reason, c("no source", "no source"))
  cov_invariant(r)
})

test_that("a source with spaces around it, or given as a list, is read", {
  outs <- list(T1 = tfl_output("T1", "table", ard = list(analyses = cov_an("N"))),
               F1 = cov_fig("F1", "  table: T1 "),
               F2 = cov_fig("F2", list("own"), ard = list(analyses = cov_an("HR"))),
               F3 = cov_fig("F3", " import: cro.json "))
  r <- tfl_ard_reads(cov_reads_study(), outs)
  expect_identical(cov_row(r, "F1")$reads, "T1")
  expect_identical(cov_row(r, "F2")$reads, "F2")
  expect_identical(nrow(cov_row(r, "F3")), 0L)
  cov_invariant(r)
})

test_that("a source that is case-different (Own, TABLE:T1) is not understood", {
  outs <- list(T1 = tfl_output("T1", "table", ard = list(analyses = cov_an("N"))),
               F1 = cov_fig("F1", "Own"), F2 = cov_fig("F2", "TABLE:T1"))
  r <- tfl_ard_reads(cov_reads_study(), outs)
  expect_identical(cov_row(r, "F1")$reason, "source not understood: Own")
  expect_identical(cov_row(r, "F2")$reason, "source not understood: TABLE:T1")
  cov_invariant(r)
})

test_that("a table's or a listing's ard_source is not looked at", {
  outs <- list(T1 = tfl_output("T1", "table", page = list(report = list(ard_source = "nonsense"))),
               L1 = tfl_output("L1", "listing", page = list(report = list(ard_source = "table:T9"))))
  r <- tfl_ard_reads(cov_reads_study(), outs)
  expect_identical(nrow(r), 0L)
})

test_that("a figure with a design that reads no ARD, and no source, has no row", {
  gg <- tfl_item("ggplot", list(data = "adsl"))
  f1 <- cov_fig("F1", content = list(gg))
  f2 <- cov_fig("F2", content = list(tfl_plot("p", list(gg))))
  r <- tfl_ard_reads(cov_reads_study(), list(F1 = f1, F2 = f2))
  expect_identical(nrow(r), 0L)
})

# ---- the inputs -------------------------------------------------------------

test_that("a study read without its folder and no outputs stops", {
  st <- cov_reads_study()
  st$dir <- NULL
  expect_error(tfl_ard_reads(st), "give `outputs`, or a study read from its folder")
  expect_identical(nrow(tfl_ard_reads(st, list())), 0L)
})

test_that("outputs without names, or a data frame, stop", {
  t1 <- tfl_output("T1", "table")
  expect_error(tfl_ard_reads(cov_reads_study(), list(t1)), "named list of tfl_output")
  expect_error(tfl_ard_reads(cov_reads_study(), data.frame(id = "T1")), "named list of tfl_output")
  expect_error(tfl_ard_reads(cov_reads_study(), list(T1 = t1, T2 = NULL)), "named list of tfl_output")
})

test_that("a study given as its folder with a trailing slash is read", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  r <- tfl_ard_reads(paste0(d, "/"))
  expect_identical(r, tfl_ard_reads(d))
  cov_invariant(r)
})

test_that("a study folder with no outputs/ folder has no rows", {
  d <- tempfile("cov-reads")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  file.copy(file.path(system.file("extdata", "spec-yaml", package = "tflspec"),
                      c("study.yml", "defaults.yml")), d)
  r <- tfl_ard_reads(d)
  expect_identical(nrow(r), 0L)
  expect_named(r, c("report", "reads", "kind", "reason"))
})

# ---- the items -------------------------------------------------------------

test_that(".reads_items_use_ard(): a content that is not a list, an item without fn, a fn of two", {
  f <- tflspec:::.reads_items_use_ard
  expect_false(f(tfl_r("1")))
  expect_false(f("text"))
  expect_false(f(list("a", 1)))
  expect_false(f(list(list(args = list()))))
  expect_false(f(list(list(fn = c("tfl_ard_number", "x")))))
  expect_true(f(list(list(fn = "tflspec::tfl_ard_number"))))
  expect_false(f(list(structure(list(items = list(list(fn = "ggplot"))), class = "tfl_plot"))))
})
