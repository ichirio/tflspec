# BIGN's rows are the column headings' N, not the table's: tflspec takes
# them out of the table's ARD by analysis_id and hands their numbers to
# plan_col_header(values = ); rtfreporter reads no analysis_id (the user's
# rule 2026-11-21, rtfreporter#1103).  Every table made from an ARD, the
# same lines whichever way its spec was read.  Fictional numbers.

bs_ard <- function() {
  data.frame(
    analysis_id = c("BIGN", "BIGN", "BIGN", "S1", "S1"),
    variable = c("ARM", "ARM", "ARM", "SEX", "SEX"),
    variable_level = I(list("Drug A", "Drug A", "Drug B", "F", "F")),
    stat_name = c("n", "N", "n", "n", "p"),
    stat = I(list(12, 30, 18, 5, 0.4)),
    stringsAsFactors = FALSE)
}

bs_out <- function(header, values = NULL, labels = NULL, source = NULL) {
  o <- tfl_output("T-1", "table", content = c(
    list(tfl_item("table_plan", list(cols = "ARM"))),
    if (!is.null(labels)) list(tfl_item("plan_labels", list(ARM = labels))),
    list(tfl_item("plan_col_header", c(list(header = header), if (!is.null(values)) list(values = values))))))
  if (!is.null(source)) o$data <- list(source = source)
  o
}

test_that("tfl_bign_values(): BIGN's n by group, one number with no group, NULL with none", {
  expect_identical(tfl_bign_values(bs_ard()), list(n = c(`Drug A` = 12, `Drug B` = 18)))
  one <- bs_ard()[1L, ]
  one$variable_level <- I(list(NULL))
  expect_identical(tfl_bign_values(one), list(n = 12))
  expect_null(tfl_bign_values(bs_ard()[4:5, ]))
  # an ARD with no analysis_id (a planner's own subset): nothing found, nothing changed
  bare <- bs_ard()[, -1L]
  expect_null(tfl_bign_values(bare))
  expect_identical(tfl_bign_drop(bare), bare)
  expect_identical(unique(tfl_bign_drop(bs_ard())$analysis_id), "S1")
})

test_that("the n is named as the columns are: the labels of the table's column variable", {
  lab <- c(`Drug A` = "Drug A 10 mg")
  expect_identical(tfl_bign_values(bs_ard(), labels = lab), list(n = c(`Drug A 10 mg` = 12, `Drug B` = 18)))
  o <- bs_out("{col} (N={n})", labels = c(ARM = "Arm", lab))
  expect_identical(tflspec:::.bign_var(o$content), "ARM")
  expect_identical(tflspec:::.bign_labels(o$content, "ARM"), lab)
  expect_null(tflspec:::.bign_labels(o$content, "SEX"))
  # cols written with a bar: the first
  p <- list(tfl_item("table_plan", list(cols = "TRT01A | SEX")))
  expect_identical(tflspec:::.bign_var(p), "TRT01A")
})

test_that("a heading that says {n} and gives no values takes BIGN's", {
  expect_true(tflspec:::.bign_applies(bs_out("{col} (N={n})")))
  expect_false(tflspec:::.bign_applies(bs_out("{col}", source = "dataset")))
  it <- tflspec:::.bign_header_items(bs_out("{col} (N={n})")$content, list(n = 1))
  expect_true(attr(it, "used"))
  expect_identical(it[[2L]]$args$values, list(n = 1))
  # no {n}: untouched; values given: kept
  it <- tflspec:::.bign_header_items(bs_out("{col}")$content, list(n = 1))
  expect_false(attr(it, "used"))
  it <- tflspec:::.bign_header_items(bs_out("{col} {n:Drug A}", values = list(n = 5))$content, list(n = 1))
  expect_identical(it[[2L]]$args$values, list(n = 5))
})

test_that("the program's lines do what the run does", {
  lab <- c(`Drug A` = "Drug A 10 mg")
  code <- tflspec:::.bign_code("ard", labels = lab)
  expect_true(any(grepl("tfl_bign_values(ard, labels = ", code, fixed = TRUE)))
  expect_true(any(grepl("ard <- tflspec::tfl_bign_drop(ard)", code, fixed = TRUE)) ||
                any(grepl("ard <- tfl_bign_drop(ard)", code, fixed = TRUE)))
  env <- new.env()
  assign("ard", bs_ard(), envir = env)
  eval(parse(text = code), env)
  expect_identical(get("bign", env), tfl_bign_values(bs_ard(), labels = lab))
  expect_false(any(get("ard", env)$analysis_id %in% "BIGN"))
  # no heading reads it: only the drop
  expect_false(any(grepl("bign <-", tflspec:::.bign_code("ard", n = FALSE), fixed = TRUE)))
})

test_that("the content's code and the run take BIGN's n into the header", {
  o <- bs_out("{col} (N={n})", labels = c(`Drug A` = "Drug A 10 mg"))
  code <- tfl_content_code(o)
  expect_match(code[1L], "^# BIGN")
  expect_true(any(grepl("values = bign", code, fixed = TRUE)))
})
