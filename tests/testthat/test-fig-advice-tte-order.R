# (A4) a time_unit step on AVAL writes tte_time(), which stops when the rows
# hold two or more units (AVALU): the parameter is kept before it

tte_design <- function(steps) {
  tfl_fig_design(
    data = steps,
    stats = list(list(step = "survfit", name = "fit", time = "AVAL", censor = "CNSR", by = "TRT01P")),
    plot = list(colour_by = "TRT01P"),
    layers = list(list(layer = "km_curve")))
}
read_tte <- list(step = "read", dataset = "ADTTE")
to_months <- list(step = "time_unit", variable = "AVAL", unit = "months")

test_that("a time unit before the parameter step is advised against", {
  d <- tte_design(list(read_tte, to_months, list(step = "param", value = "OS")))
  a <- tfl_fig_advice(d)
  expect_true("tte_order" %in% a$rule)
  r <- a[a$rule == "tte_order", ]
  expect_equal(r$level, "warning")
  expect_equal(r$part, "data")
  expect_equal(r$args[[1]], list(2L, 3L, 2L, 3L))
  expect_match(r$message, "Step 2 changes AVAL's unit with tte_time() before step 3", fixed = TRUE)
  expect_match(r$message, "move step 2 after step 3", fixed = TRUE)
  # no one-step fix: a fix adds or changes a piece, none moves one
  expect_null(r$fix[[1]])
  # with the example data too
  expect_true("tte_order" %in% tfl_fig_advice(d, tfl_example_adam())$rule)
})

test_that("the parameter kept first gets no such advice", {
  d <- tte_design(list(read_tte, list(step = "param", value = "OS"), to_months))
  expect_false("tte_order" %in% tfl_fig_advice(d)$rule)
  # the template's own order
  km <- tfl_fig_template("km_risk_table", data = "ADTTE", param = "OS", group = "TRT01P")
  expect_false("tte_order" %in% tfl_fig_advice(km)$rule)
  # no parameter step at all: nothing to put first
  d <- tte_design(list(read_tte, to_months))
  expect_false("tte_order" %in% tfl_fig_advice(d)$rule)
})

test_that("a filter on PARAMCD counts as keeping the parameter", {
  d <- tte_design(list(read_tte, to_months, list(step = "filter", expr = "PARAMCD == \"OS\"")))
  a <- tfl_fig_advice(d)
  expect_true("tte_order" %in% a$rule)
  expect_equal(a$args[[which(a$rule == "tte_order")]], list(2L, 3L, 2L, 3L))
  # a filter on something else does not
  d <- tte_design(list(read_tte, to_months, list(step = "filter", expr = "!is.na(AVAL)")))
  expect_false("tte_order" %in% tfl_fig_advice(d)$rule)
  # nor does a filter on PARAMCD that comes before the time unit
  d <- tte_design(list(read_tte, list(step = "filter", expr = "PARAMCD %in% c(\"OS\")"), to_months))
  expect_false("tte_order" %in% tfl_fig_advice(d)$rule)
})

test_that("a time unit on another variable (days, no tte_time()) is not advised", {
  d <- tte_design(list(read_tte,
                       list(step = "time_unit", variable = "ADY", unit = "months"),
                       list(step = "param", value = "OS")))
  expect_false("tte_order" %in% tfl_fig_advice(d)$rule)
  # the variable left to its default (AVAL) counts
  d <- tte_design(list(read_tte, list(step = "time_unit", unit = "months"),
                       list(step = "param", value = "OS")))
  expect_true("tte_order" %in% tfl_fig_advice(d)$rule)
})

test_that("the template's order, moved, is advised; moved back it is not", {
  km <- tfl_fig_template("km_risk_table", data = "ADTTE", param = "OS", group = "TRT01P")
  k <- vapply(km$data, function(s) s$step %||% "", "")
  ip <- which(k == "param"); it <- which(k == "time_unit")
  expect_true(ip < it)
  bad <- km
  bad$data[c(ip, it)] <- km$data[c(it, ip)]
  a <- tfl_fig_advice(bad)
  expect_true("tte_order" %in% a$rule)
  expect_equal(a$args[[which(a$rule == "tte_order")]], list(ip, it, ip, it))
})
