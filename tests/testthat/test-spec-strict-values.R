# A field that takes true or false and an enum are read as written:
# yes / no (the yaml package's own TRUE / FALSE) are read as logicals, other
# text and "SAS" stop with a message

strict_read <- function(...) {
  f <- tempfile(fileext = ".yml")
  writeLines(c("id: T-1", "kind: table", "content:",
               "  - table_plan: {cols: TRT01A}", ...), f)
  withr::defer(unlink(f), envir = parent.frame())
  list(file = f, out = function() tfl_read_output(f))
}

test_that("yes / no in a field that takes true or false are TRUE / FALSE; other text stops, naming file, field and value", {
  s <- strict_read("  - plan_filter: {where: !r 'p > 0.5', any: yes}")
  o <- s$out()
  expect_identical(o$content[[length(o$content)]]$args$any, TRUE)
  s <- strict_read("  - plan_filter: {where: !r 'p > 0.5', any: 'no'}")
  o <- s$out()
  expect_identical(o$content[[length(o$content)]]$args$any, FALSE)
  s <- strict_read("  - plan_filter: {where: !r 'p > 0.5', any: maybe}")
  e <- tryCatch(s$out(), error = function(e) conditionMessage(e))
  expect_match(e, s$file, fixed = TRUE)
  expect_match(e, 'any takes true or false, not "maybe"', fixed = TRUE)
  s <- strict_read("  - plan_filter: {where: !r 'p > 0.5', any: 'Yes'}")
  expect_error(s$out(), 'not "Yes"', fixed = TRUE)
})

test_that("true / false / !unset in a logical field pass", {
  for (v in c("true", "false", "!unset")) {
    s <- strict_read(paste0("  - plan_filter: {where: !r 'p > 0.5', any: ", v, "}"))
    o <- s$out()
    expect_s3_class(o, "tfl_output")
  }
  s <- strict_read("  - plan_filter: {where: !r 'p > 0.5', any: false}")
  expect_identical(s$out()$content[[2L]]$args$any, FALSE)
})

test_that("a rounding spelled \"SAS\" stops, naming the values", {
  s <- strict_read("  - plan_digits: {rounding: SAS}")
  expect_error(s$out(), 'plan_digits rounding takes one of "sas", "r"', fixed = TRUE)
  expect_error(s$out(), 'not "SAS"', fixed = TRUE)
  study <- list(study = list(settings = list(rounding = "SAS")))
  expect_error(tflspec:::.study_rounding(study), "settings: rounding takes one of", fixed = TRUE)
})

test_that("rounding sas and r pass", {
  for (v in c("sas", "r")) {
    s <- strict_read(paste0("  - plan_digits: {rounding: ", v, "}"))
    expect_identical(s$out()$content[[2L]]$args$rounding, v)
    study <- list(study = list(settings = list(rounding = v)))
    expect_identical(tflspec:::.study_rounding(study), v)
  }
})
