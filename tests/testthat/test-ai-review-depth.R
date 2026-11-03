# How deep the agent checks what it built (study.yml ai: review, #598):
# the levels, and the program and the facts tfl_ai_run() writes for it

test_that("the review levels: a kind each, the default for a kind not said, none", {
  lv <- tflspec:::.ai_review_levels
  expect_identical(lv(list()), c(figure = 3L, table = 2L, listing = 1L))
  expect_identical(lv(list(table = 3)), c(figure = 3L, table = 3L, listing = 1L))
  expect_identical(lv(1), c(figure = 1L, table = 1L, listing = 1L))
  expect_identical(lv(list(listing = "none", figure = FALSE)), c(figure = NA, table = 2L, listing = NA))
  expect_error(lv(list(tables = 2)), "the kinds are figure, table, listing; not tables")
  expect_error(lv(list(figure = "high")), "figure is 0 to 3, or none; not high")
})

test_that("a table's facts: its header, rows, empty cells; its pages, one over full", {
  skip_if_not_installed("rtfreporter")
  df <- data.frame(Item = c("Sex, n (%)", sprintf("  Row %02d", 1:59)),
                   A = c("", sprintf("%d (%.1f)", 1:59, 1:59 / 2)),
                   B = c("", "NA (NA)", "", sprintf("%d", 3:59)), stringsAsFactors = FALSE)
  doc <- rtfreporter::rtf_document() |>
    rtfreporter::rtf_tables(list(T1 = df), titles = c("Table 1", "Things"), footnotes = "A note.")
  x <- tflspec:::.ai_table_facts(doc, "T-1")
  expect_identical(x[1:2], c("# T-1: what R made, in words (read this, not the RTF)",
                             "More than one page (1 laid out, one over full): read *The pages* too."))
  expect_true("3 columns, 60 rows (and 0 blank rows between them)." %in% x)
  expect_true("  1: Item | A | B" %in% x)
  expect_true("Rows (the first 20 and the last 20):" %in% x)
  expect_true("  ... 20 rows ..." %in% x)
  # the heading row's empty cells are not counted; a row's are
  expect_true("Empty cells (in a row with others): 1, in the rows \"Row 02\"." %in% x)
  expect_true("Missing values printed (NA, NaN, Inf): 1, in the rows \"Row 01\"." %in% x)
  pg <- x[(which(x == "## The pages") + 2L):length(x)]
  expect_match(pg[1L], "^1 page\\(s\\) \\(letter landscape, 9 pt; about 46 lines a page")
  expect_match(pg[2L], "^Page 1: 2 title lines, 1 header lines, 60 rows .* lines more than a page holds")
  expect_true("  Title: Table 1 / Things" %in% pg)
  expect_true("  Footnotes: A note." %in% pg)
  # a short one fits
  doc2 <- rtfreporter::rtf_document() |> rtfreporter::rtf_tables(list(T1 = df[1:5, ]), titles = "Table 2")
  y <- tflspec:::.ai_table_facts(doc2, "T-2")
  expect_identical(y[2L], "One page.")
  expect_true(any(grepl("lines; fits.", y, fixed = TRUE)))
})

test_that("tfl_ai_run(): each report's program and, for a table, its facts beside its RTF", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  root <- withr::local_tempdir()
  d <- outs_study(root)
  res <- suppressMessages(tfl_ai_run(d, requests = c("ae-1", "figure-1")))
  r <- res$reports
  rtf <- r$rtf[r$output_id == "T-14-3-1"]
  expect_false(is.na(rtf))
  base <- file.path(d, sub("[.]rtf$", "", rtf))
  # the program: the analysis set, the records, the analysis and the content
  p <- readLines(paste0(base, ".R"), encoding = "UTF-8")
  expect_identical(p[1L], "# T-14-3-1: the program that makes it, as tflspec writes it from the draft.")
  expect_true(any(grepl("filter(adsl, SAFFL == \"Y\")", p, fixed = TRUE)))
  expect_true(any(grepl("filter(TRTEMFL == \"Y\")", p, fixed = TRUE)))
  expect_true("# ---- content ----" %in% p)
  expect_false(any(grepl("^set_levels <- function", p)))
  # the facts
  f <- readLines(paste0(base, "-facts.txt"), encoding = "UTF-8")
  expect_true(all(c("## The table", "## The pages") %in% f))
  expect_true(any(grepl("(N=80)", f, fixed = TRUE)))
  # a figure: its program too (its facts are beside its PNG)
  fig <- file.path(d, sub("[.]rtf$", ".R", r$rtf[r$output_id == "F-14-2-2"]))
  expect_true(file.exists(fig))
})
