# ai/defaults.md: every default the drafts took, the most important first

test_that(".ai_impact_class(): the numbers, the rows or statistics, the text or the look", {
  f <- tflspec:::.ai_impact_class
  expect_identical(f("data", "population"), 1L)
  expect_identical(f("ard", "anything"), 1L)
  expect_identical(f("page", "Denominator"), 1L)
  expect_identical(f("content", "sort"), 2L)
  expect_identical(f("template", "x"), 2L)
  expect_identical(f("codelists", "x"), 2L)
  expect_identical(f("page", "total column"), 2L)
  expect_identical(f("page", "footnotes"), 3L)
  expect_identical(f("content", "widths"), 3L)
  expect_identical(f(NA, NA), 3L)
  expect_identical(f(c("data", "content", "page"), c("population", "sort", "footnotes")), c(1L, 2L, 3L))
})

test_that("tfl_ai_run() writes ai/defaults.md: groups 1, 2, 3; widths in one line a report", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- outs_study(root)
  # (assumptions the drafts take: a population, a sort, a footnote, two widths)
  f <- file.path(d, "ai", "spec", "outputs", "T-14-3-1.yml")
  o <- yaml::read_yaml(f)
  o$ai$assumptions <- list(
    list(where = "data", item = "population", assumption = "the Safety Analysis Set", value = "SAFFL = Y",
         from = "D1"),
    list(where = "content", item = "sort order", assumption = "by descending frequency"),
    list(where = "page", item = "footnotes", assumption = "the shell's footnote 1"),
    list(where = "page", item = "widths", assumption = "the shell's proportions"),
    list(where = "page", item = "widths of the stub", assumption = "the stub wider"))
  yaml::write_yaml(o, f)
  suppressMessages(tfl_ai_run(d, rtf = FALSE))
  p <- file.path(d, "ai", "defaults.md")
  expect_true(file.exists(p))
  x <- readLines(p, encoding = "UTF-8")
  # the full list: after "## Every default, by report"
  e <- grep("^## Every default, by report", x)
  expect_length(e, 1L)
  expect_true(any(grepl("most important first", x, fixed = TRUE)))
  x <- x[e:length(x)]
  g <- grep("^### [123][.] ", x)
  expect_length(g, 3L)
  expect_match(x[g[1L]], "Changes the numbers", fixed = TRUE)
  expect_match(x[g[2L]], "Changes the rows or statistics", fixed = TRUE)
  expect_match(x[g[3L]], "Changes the text or the look", fixed = TRUE)
  grp <- function(k) x[seq(g[k], if (k < 3L) g[k + 1L] - 1L else length(x))]
  expect_true(any(grepl("^- T-14-3-1 data/population: the Safety Analysis Set [(]from: D1[)]", grp(1L))))
  expect_true(any(grepl("content/sort order", grp(2L), fixed = TRUE)))
  expect_true(any(grepl("page/footnotes", grp(3L), fixed = TRUE)))
  w <- grep("T-14-3-1 .*widths", x)
  expect_length(w, 1L)
  expect_true(w > g[3L])
  # status.md says where it is
  st <- readLines(file.path(d, "ai", "status.md"), encoding = "UTF-8")
  expect_true(grepl("ai/defaults.md", st[1L], fixed = TRUE))
})

test_that(".ai_from_is_document(): the shell, the SAP, the TOC, the define, a dataset, a D<n> are documents", {
  f <- tflspec:::.ai_from_is_document
  expect_identical(f(c("shell", "SAP 9.4", "TOC", "define", "adam_summary", "ADSL", "D4", "the Shell")),
                   rep(TRUE, 8L))
  expect_identical(f(c("programmer convention", "company convention", "company hints",
                       "(default, Q3)", "", NA)), rep(FALSE, 6L))
})

test_that("ai/defaults.md: the agent's own defaults first, merged by topic across the reports", {
  a <- data.frame(
    class = c(1L, 1L, 1L, 2L, 2L, 3L),
    id = c("14.1.10", "14.1.2", "14.1.1", "14.1.1", "14.1.2", "14.1.1"),
    where = c("data", "data", "data", "content", "content", "page"),
    item = c("population", "population", "population", "sort", "sort", "footnotes"),
    value = c("SAF", "SAF", "FAS", "freq", "alpha", NA),
    text = c("a", "b", "c", "d", "e", "f"),
    src = c("programmer convention", "programmer convention", "company hints",
            "not stated", "not stated", "(default, Q2)"), stringsAsFactors = FALSE)
  z <- tflspec:::.ai_defaults_topics(a, 1L)
  expect_identical(z, c(
    "- population: SAF (2 reports: 14.1.2, 14.1.10) (from: programmer convention)",
    "- population: FAS (1 report: 14.1.1) (from: company hints)"))
  expect_length(tflspec:::.ai_defaults_topics(a, 2L), 2L)
  b <- a[rep(1L, 8L), ]
  b$id <- sprintf("14.1.%d", 1:8)
  expect_match(tflspec:::.ai_defaults_topics(b, 1L),
               "(8 reports: 14.1.1, 14.1.2, 14.1.3, 14.1.4, 14.1.5, ... +3)", fixed = TRUE)
  expect_identical(tflspec:::.ai_defaults_topics(a, 4L), "- (none)")
})
