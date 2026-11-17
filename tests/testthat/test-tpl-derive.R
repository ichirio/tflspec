# A template's derive: names (tflspec#847): one operation each on the
# arguments' values, worked out in order; not arguments

derive_run <- function(derive, env = list(), params = names(env)) {
  tflspec:::.tr_derive(derive, env, params, "t/x")
}

test_that("derive: the operations on a list", {
  env <- list(h = c("AEBODSYS", "AEHLT", "AEDECOD"), one = "AEDECOD", none = NULL)
  e <- derive_run(list(
    outer = list(but_last = "{h}"),
    rows = list(keys = "{outer}", prefix = "group"),
    lab = list(last = "{h}"),
    text = list(join = "{h}", sep = " / "),
    comma = list(join = "{h}"),
    plain = list(value = list(".overall", "{outer}", "label")),
    one_outer = list(but_last = "{one}"),
    one_rows = list(keys = "{one_outer}", prefix = "group"),
    no_text = list(join = "{none}"),
    no_last = list(last = "{none}")), env)
  expect_identical(e$outer, c("AEBODSYS", "AEHLT"))
  expect_identical(e$rows, list(group1 = "AEBODSYS", group2 = "AEHLT"))
  expect_identical(e$lab, "AEDECOD")
  expect_identical(e$text, "AEBODSYS / AEHLT / AEDECOD")
  expect_identical(e$comma, "AEBODSYS, AEHLT, AEDECOD")
  expect_identical(e$plain, list(".overall", "AEBODSYS", "AEHLT", "label"))
  # nothing: the names are there, their values NULL
  for (k in c("one_outer", "one_rows", "no_text", "no_last")) {
    expect_true(k %in% names(e), info = k)
    expect_null(e[[k]], info = k)
  }
  # not arguments: the arguments' own values are as given
  expect_identical(e[names(env)], env)
})

test_that("derive: if gives then when the value is something, else else or nothing", {
  env <- list(on = TRUE, off = FALSE, txt = "No events.", blank = "", nul = NULL,
              col = "Control")
  e <- derive_run(list(
    a = list("if" = "{on}", then = list(position = "last")),
    b = list("if" = "{off}", then = list(position = "last")),
    c = list("if" = "{txt}", then = list(text = "{txt}")),
    d = list("if" = "{blank}", then = list(text = "{blank}")),
    f = list("if" = "{nul}", then = "x"),
    g = list("if" = "{col}", then = "[{col}]", "else" = ""),
    h = list("if" = "{nul}", then = "[{nul}]", "else" = "")), env)
  expect_identical(e$a, list(position = "last"))
  expect_null(e$b)
  expect_identical(e$c, list(text = "No events."))
  expect_null(e$d)
  expect_null(e$f)
  expect_identical(e$g, "[Control]")
  expect_identical(e$h, "")
})

test_that("derive: an unknown operation, an option an operation has not, an argument's name stop", {
  expect_error(derive_run(list(a = list(first = "{h}")), list(h = "A")),
               "derive: a: write one operation.*value, if, last, but_last, join, keys")
  expect_error(derive_run(list(a = "{h}"), list(h = "A")), "write one operation")
  expect_error(derive_run(list(a = list(last = "{h}", join = "{h}")), list(h = "A")),
               "write one operation")
  expect_error(derive_run(list(a = list(last = "{h}", sep = ", ")), list(h = "A")),
               "last takes no sep")
  expect_error(derive_run(list(a = list(join = "{h}", prefix = "g")), list(h = "A")),
               "join takes no prefix \\(its options: sep\\)")
  expect_error(derive_run(list(h = list(last = "{h}")), list(h = "A")),
               "derive: h is an argument's name")
  expect_error(derive_run(list(list(last = "{h}")), list(h = "A")), "is a map")
})

test_that("derived names are not arguments: no card, not in args, a report cannot give one", {
  rep <- function(...) list(id = "X", kind = "table", template = c(list(
    name = "tflspec_v1/tflspec_hierarchy", from = "ADAE", by = "TRT01A",
    hierarchy = c("AEBODSYS", "AEDECOD")), list(...)))
  r <- tfl_template_resolve(rep())
  expect_false(any(grepl("^hierarchy_", names(attr(r, "args")))))
  expect_false(any(grepl("^hierarchy_", tfl_arg_defs(template = "tflspec_v1/tflspec_hierarchy")$name)))
  expect_error(tfl_template_resolve(rep(hierarchy_sort = "label")),
               "has no argument hierarchy_sort")
  # a step whose derived arguments are nothing is left out
  fns <- function(r) vapply(r$content, function(i) i$fn, "")
  expect_true("plan_total" %in% fns(tfl_template_resolve(rep(total = TRUE))))
  expect_false("plan_total" %in% fns(tfl_template_resolve(rep(total = FALSE))))
})
