# tfl_check_spec(): every cell template of a report, as warnings with the fix

cc <- function(...) .check_cell_rows(list(kind = "table", content = list(...)), "T1")
pc <- function(...) cc(tfl_item("plan_cells", list(...)))

test_that("a statistic the cell cannot have: a warning naming the statistics", {
  p <- pc(categorical = "{n} ({pct:.1f%})")
  expect_identical(p$level, "warning")
  expect_match(p$message, "\"pct\"", fixed = TRUE)
  expect_match(p$message, "the statistics here are n, N, p", fixed = TRUE)
  expect_match(p$message, "did you mean \"p\"", fixed = TRUE)
  expect_identical(p$item, "plan_cells(categorical)")
  # a continuous row has N, not n
  p <- pc(continuous = list(n = "{n}", `Mean (SD)` = "{Mean} ({sd:.2f})"))
  expect_identical(nrow(p), 2L)
  expect_match(p$message[2L], "did you mean \"mean\"", fixed = TRUE)
  # the template's own argument, and a plan_compare column
  o <- list(kind = "table", template = list(name = "demog", args = list(count_cell = "{count} ({p:.1f%})")))
  expect_match(.check_cell_rows(o, "T1")$message, "\"count\"", fixed = TRUE)
  expect_identical(.check_cell_rows(o, "T1")$item, "template$args$count_cell")
  p <- cc(tfl_item("plan_compare", list(`p-value` = "{pvalue:.3f}")))
  expect_identical(p$level, "warning")
})

test_that("a format rtfreporter cannot format: a warning with the form", {
  p <- pc(continuous = list(Mean = "{mean:.1}", Min = "{min:0f}"),
          categorical = "{n} ({p:.1f %})")
  expect_identical(p$level, rep("warning", 3L))
  expect_match(p$message[1L], "write {mean:.1f}", fixed = TRUE)
  expect_match(p$message[2L], "write {min:.0f}", fixed = TRUE)
  expect_match(p$message[3L], "write {p:.1f%}", fixed = TRUE)
  # (Python's `.1%` is the static check's error: no second row)
  expect_identical(nrow(pc(categorical = "{n} ({p:.1%})")), 0L)
  # the forms rtfreporter has
  ok <- pc(continuous = list(a = "{mean:.1f}", b = "{median:.3s}", c = "{N:d}", d = "{sd:.2f}"),
           categorical = "{n:stat} ({p:.1f%}) {N:stat_fmt} {p:%}")
  expect_identical(nrow(ok), 0L)
})

test_that("a percentage shown with no % in its format: a warning with the fix", {
  p <- pc(categorical = "{n} ({p:.1f})")
  expect_identical(p$level, "warning")
  expect_match(p$message, "write {p:.1f%}", fixed = TRUE)
  expect_match(p$message, "prints no % sign", fixed = TRUE)
  # a proportion with a literal % after it is wrong the same way
  expect_identical(nrow(pc(categorical = "{n} ({p:.1f}%)")), 1L)
  # `.1f%` prints no sign: the % after the brace is right, not a double sign
  expect_identical(nrow(pc(categorical = "{n} ({p:.1f%}%)")), 0L)
})

test_that("no warning where the cell is right or the statistics cannot be known", {
  # a bare token and stat_fmt read the ARD's own format
  expect_identical(nrow(pc(continuous = list(`Mean (SD)` = "{mean} ({sd})", Median = "{median:stat_fmt}"))), 0L)
  expect_identical(nrow(pc(categorical = "{n} ({p})")), 0L)
  expect_identical(nrow(pc(categorical = "{n} ({p:.1f%})")), 0L)
  # a guard list, a guard as R, cell_rows() as a call and as R
  expect_identical(nrow(pc(categorical = list(tfl_r("n < 3 ~ '<3'"), "{n} ({p:.1f%})"))), 0L)
  expect_identical(nrow(pc(categorical = list(tfl_r("n == 0 ~ \"0\""), "{n} ({p:.1f%})"))), 0L)
  expect_identical(nrow(pc(AGE = tfl_r("cell_rows(\"1\" = \"{N}\", \"2\" = \"{mean:.1f}\")"))), 0L)
  expect_identical(nrow(pc(AGE = tfl_call("cell_rows", list(`1` = "{N}", `2` = "{mean:.1f}")))), 0L)
  # an `!r` content: R code is not read, its braces are no cell
  expect_identical(nrow(pc(AGE = tfl_r("function(x) { x }"))), 0L)
  # a statistic of a report's own analysis; a report whose ARD is its own code
  o <- list(kind = "table", ard = list(analyses = list(list(method = "continuous", statistics = "mean | geo_mean"))),
            content = list(tfl_item("plan_cells", list(continuous = list(G = "{geo_mean:.2f}")))))
  expect_identical(nrow(.check_cell_rows(o, "T1")), 0L)
  o$ard <- list(analyses = list(list(method = "continuous", code = "cards::ard_continuous(adsl, variables = AGE)")))
  o$content[[1L]]$args$continuous <- list(X = "{whatever}")
  expect_identical(nrow(.check_cell_rows(o, "T1")), 0L)
  o$ard <- tfl_r("cards::ard_continuous(adsl, variables = AGE)")
  expect_identical(nrow(.check_cell_rows(o, "T1")), 0L)
  # a template's own statistic (the PK cell's {value})
  o <- list(kind = "table", template = list(name = "pk_conc", args = list(cell = "{value:.3s}")))
  expect_identical(nrow(.check_cell_rows(o, "T1")), 0L)
})

test_that("in tfl_check_spec(): the warnings are rows of the report, not errors", {
  p <- .check_static_rows(list(kind = "table", content = list(
    tfl_item("plan_cells", list(categorical = "{n} ({pct:.1f%})")))), "T1")
  expect_identical(p$level, "warning")
  expect_identical(p$output_id, "T1")
  expect_identical(p$part, "content")
})
