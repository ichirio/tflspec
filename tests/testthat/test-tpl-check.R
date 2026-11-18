# A template's check: list (tflspec#851) and the derive operation given:

chk_run <- function(check, env = list(), params = names(env), origins = character()) {
  env <- tflspec:::.tr_derive(NULL, env, params, "t/x", origins)
  tflspec:::.tr_check(check, env, params, origins, "t/x", "who()")
}

test_that("check: a fail that is something stops with the filled message", {
  chk <- list(list(fail = list("if" = "{x}", then = "{y}"), message = "x with {y} is wrong."))
  expect_error(chk_run(chk, list(x = TRUE, y = "Q")), "^who\\(\\): t/x: x with Q is wrong\\.$")
  expect_null(chk_run(chk, list(x = FALSE, y = "Q")))
  expect_null(chk_run(chk, list(x = TRUE, y = NULL)))
  expect_null(chk_run(NULL))
})

test_that("check: items are worked out in order; the first that fails stops", {
  chk <- list(list(fail = list(value = "{a}"), message = "first"),
              list(fail = list(value = "{b}"), message = "second"))
  expect_error(chk_run(chk, list(a = TRUE, b = TRUE)), "first")
  expect_error(chk_run(chk, list(a = FALSE, b = TRUE)), "second")
})

test_that("check: an unknown key, a missing fail or message, a check that is not a list stop", {
  env <- list(a = TRUE)
  expect_error(chk_run(list(list(fail = list(value = "{a}"), message = "m", why = "x")), env),
               "template t/x: check: item 1: why is not a key")
  expect_error(chk_run(list(list(fail = list(value = "{a}"))), env), "item 1 needs message")
  expect_error(chk_run(list(list(message = "m")), env), "item 1 needs fail")
  expect_error(chk_run("a", env), "template t/x: check: is a list")
  expect_error(chk_run(list(a = list(fail = list(value = "{a}"), message = "m")), env),
               "is a list of items")
  expect_error(chk_run(list("a"), env), "item 1 is a map")
  expect_error(chk_run(list(list(fail = list(first = "{a}"), message = "m")), env),
               "item 1 fail: write one operation")
})

test_that("given: true only for an argument the report set; unknown names stop", {
  env <- list(p = "page", q = NULL, r = 1)
  org <- c(p = "template", q = "report", r = "series")
  e <- tflspec:::.tr_derive(list(
    one = list(given = "q"), two = list(given = list("p", "q")),
    none = list(given = list("p", "r"))), env, names(env), "t/x", org)
  expect_true(e$one)
  expect_true(e$two)
  expect_null(e$none)
  expect_error(tflspec:::.tr_derive(list(a = list(given = "zz")), env, names(env), "t/x", org),
               "given: zz is not a param of the template")
  expect_error(tflspec:::.tr_derive(list(a = list(given = "p", sep = ",")), env, names(env), "t/x", org),
               "given takes no sep")
})

lck_resolve <- function(...) {
  tfl_template_resolve(list(id = "X", kind = "listing", template = c(
    list(name = "tflspec_v1/tflspec_listing", from = "ADAE",
         columns = list(list(vars = "USUBJID"))), list(...))))
}

test_that("tflspec_listing: by_break block needs exactly one by variable", {
  expect_error(lck_resolve(by = c("TRT01A", "AESEV"), by_break = "block"),
               "tflspec_v1/tflspec_listing: by_break: block needs exactly one `by` variable; got TRT01A, AESEV\\.")
  expect_no_error(lck_resolve(by = "TRT01A", by_break = "block"))
  expect_no_error(lck_resolve(by = c("TRT01A", "AESEV"), by_break = "page"))
})

test_that("tflspec_listing: by_heading is not used with by_break block", {
  expect_error(lck_resolve(by = "TRT01A", by_break = "block", by_heading = "Arm: {value}"),
               "by_heading is not used with by_break: block")
  expect_no_error(lck_resolve(by = "TRT01A", by_break = "page", by_heading = "Arm: {value}"))
  expect_no_error(lck_resolve(by = "TRT01A", by_heading = "Arm: {value}"))
})

test_that("tflspec_listing: by_break and by_heading need by", {
  expect_error(lck_resolve(by_break = "block"), "by_break and by_heading need `by`\\.")
  expect_error(lck_resolve(by_break = "page"), "by_break and by_heading need `by`\\.")
  expect_error(lck_resolve(by_heading = "Arm: {value}"), "by_break and by_heading need `by`\\.")
  # nothing set (the default by_break page) and no by passes
  expect_no_error(lck_resolve())
})
