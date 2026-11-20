# BIGN's rows are the column headings' N, not the table's: tflspec takes
# them out of the table's ARD by analysis_id and hands their numbers to
# plan_col_header(values = list(n = )); rtfreporter reads no analysis_id
# (the user's rule 2026-11-21, rtfreporter#1103).  Fictional numbers.

bs_ard <- function() {
  data.frame(
    analysis_id = c("BIGN", "BIGN", "BIGN", "S1", "S1"),
    variable = c("ARM", "ARM", "ARM", "SEX", "SEX"),
    variable_level = I(list("Drug A", "Drug A", "Drug B", "F", "F")),
    stat_name = c("n", "N", "n", "n", "p"),
    stat = I(list(12, 30, 18, 5, 0.4)),
    stringsAsFactors = FALSE)
}

bs_out <- function(header, values = NULL, bign = TRUE) {
  o <- tfl_output("T-1", "table", content = list(
    tfl_item("table_plan", list(cols = "ARM")),
    tfl_item("plan_col_header", c(list(header = header), if (!is.null(values)) list(values = values)))))
  o$ard <- list(analyses = list(list(id = if (bign) "BIGN" else "A0"), list(id = "S1")))
  o
}

test_that("BIGN's n by group, one number with no by, NULL with none", {
  expect_identical(tflspec:::.bign_n(bs_ard()), c(`Drug A` = 12, `Drug B` = 18))
  one <- bs_ard()[1L, ]
  one$variable_level <- I(list(NULL))
  expect_identical(tflspec:::.bign_n(one), 12)
  expect_null(tflspec:::.bign_n(bs_ard()[4:5, ]))
  expect_identical(unique(tflspec:::.bign_drop(bs_ard())$analysis_id), "S1")
})

test_that("a heading that says {n} and gives no values takes BIGN's", {
  has <- function(o) tflspec:::.bign_has(o)
  expect_true(has(bs_out("{col} (N={n})")))
  expect_false(has(bs_out("{col} (N={n})", bign = FALSE)))
  it <- tflspec:::.bign_header_items(bs_out("{col} (N={n})")$content, list(n = 1))
  expect_true(attr(it, "used"))
  expect_identical(it[[2L]]$args$values, list(n = 1))
  # no {n}: untouched; values given: kept
  it <- tflspec:::.bign_header_items(bs_out("{col}")$content, list(n = 1))
  expect_false(attr(it, "used"))
  it <- tflspec:::.bign_header_items(bs_out("{col} {n:Drug A}", values = list(n = 5))$content, list(n = 1))
  expect_identical(it[[2L]]$args$values, list(n = 5))
})

test_that("the program takes BIGN's rows out of the ARD and names their n", {
  code <- tflspec:::.bign_code("ard")
  env <- new.env()
  assign("ard", bs_ard(), envir = env)
  eval(parse(text = code), env)
  expect_identical(get("bign_n", env), c(`Drug A` = 12, `Drug B` = 18))
  expect_false(any(get("ard", env)$analysis_id %in% "BIGN"))
  # the run and the program agree
  expect_identical(get("bign_n", env), tflspec:::.bign_n(bs_ard()))
  expect_false(any(grepl("bign_n", tflspec:::.bign_code("ard", n = FALSE))))
})
