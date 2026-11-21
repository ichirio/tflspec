# What rtfreporter cannot do yet stops at the content step, from one table
# (inst/spec/limits.yml, R/limits.R): the message says the issue and how to
# write it today; a fixed version stops nothing.

lim_content <- function(cols, total = TRUE) {
  c(list(tfl_item("table_plan", list(cols = cols, rows = list(group = "variable")))),
    if (total) list(tfl_item("plan_total", list())))
}
lim_row <- function() Filter(function(r) identical(r$id, "rtfreporter-1103"), tflspec:::.limits_table())

test_that("the 1103 row stops two column keys with a Total, before the version that does it", {
  rows <- lim_row()
  expect_length(rows, 1L)
  old <- package_version("0.8.2.9000")
  expect_error(tflspec:::.content_limits(lim_content(c("TRTA", "SEX")), "T-1", rows = rows, version = old),
               "T-1: rtfreporter cannot yet place the Total column", fixed = TRUE)
  expect_error(tflspec:::.content_limits(lim_content("TRTA | SEX"), rows = rows, version = old),
               "(ichirio/rtfreporter#1103). Instead: Use one column key", fixed = TRUE)
  # not met: one key, or no Total
  expect_silent(tflspec:::.content_limits(lim_content("TRTA"), rows = rows, version = old))
  expect_silent(tflspec:::.content_limits(lim_content(c("TRTA", "SEX"), total = FALSE), rows = rows, version = old))
  # fixed_in and later: nothing stops
  expect_silent(tflspec:::.content_limits(lim_content(c("TRTA", "SEX")), rows = rows,
                                          version = package_version(rows[[1L]]$fixed_in)))
})

test_that("every row has its columns, and a word outside the closed list stops", {
  for (r in tflspec:::.limits_table()) {
    expect_true(all(c("id", "issue", "when", "message", "instead", "added") %in% names(r)), info = r$id)
    expect_true(all(c("en", "ja") %in% names(r$message)), info = r$id)
    expect_true(all(c("en", "ja") %in% names(r$instead)), info = r$id)
  }
  bad <- list(list(id = "x", issue = "i", when = list(item = "plan_total", name = "T-1"),
                   message = list(en = "m"), instead = list(en = "w")))
  expect_error(tflspec:::.content_limits(lim_content("TRTA"), rows = bad), "unknown word name")
})

test_that("the run and the program both reach the table", {
  rows <- list(list(id = "t", issue = "ichirio/rtfreporter#0", when = list(item = "plan_total"),
                    message = list(en = "no Total yet."), instead = list(en = "leave it out.")))
  local_mocked_bindings(.limits_table = function() rows, .package = "tflspec")
  o <- tfl_output("T-1", "table", content = lim_content("TRTA"))
  expect_error(tfl_content_code(o), "no Total yet. (ichirio/rtfreporter#0)", fixed = TRUE)
  ard <- data.frame(variable = "SEX", variable_level = I(list("F")), stat_name = "n",
                    stat = I(list(1)), TRTA = "A")
  expect_error(tfl_content_run(o, ard), "no Total yet.", fixed = TRUE)
})
