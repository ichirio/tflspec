# A language-neutral condition is written untagged; `!r` keeps full R
# (R/neutral.R).

test_that(".neutral_expr() accepts the whitelist", {
  ok <- c('AGE >= 18', 'p > 0.5', 'x == 1e3', 'SEX == "F"', "SEX == 'F'",
          'SAFFL != "Y"', 'x < -1', 'x <= 2 & y >= 3', 'a | b', '!a', '(a)',
          'x == TRUE', 'x == FALSE', 'is.na(ADY)', '!is.na(ADY)',
          'PARAMCD == "OVR" & !is.na(ADY)', 'AVISIT %in% c("A", "B")',
          '!x %in% c(1, 2)', 'AEREL %in% c("3", "4")',
          '(a == 1 | b == 2) & !is.na(c)', 'is.na(x) | is.na(y)')
  for (s in ok) expect_true(.neutral_expr(s), info = s)
})

test_that(".neutral_expr() refuses what a Python reader could not read", {
  no <- c('between(AGE, 18, 65)', 'a && b', 'a || b', 'x == 18L', 'x == NA',
          'xor(a, b)', 'x + 1 > 2', 'x = 1', '~ x == 1', 'f(x)', 'x[1] == 2',
          'd$x == 1', 'pkg::f(x) == 1', 'x %in% y', 'x %in% c(a, b)',
          'x %in% 1', 'x == c(1, 2)',
          '"AGE >= 18"', '18', 'AGE', 'TRUE', '-1', 'x ==', 'a b', '')
  for (s in no) {
    r <- .neutral_expr(s)
    expect_false(isTRUE(r), info = s)
    expect_true(nzchar(attr(r, "why")), info = s)
  }
  expect_match(attr(.neutral_expr("between(AGE, 18, 65)"), "why"), "between", fixed = TRUE)
  expect_false(isTRUE(.neutral_expr(c("a > 1", "b > 2"))))
})

test_that("an untagged neutral condition is read as tfl_r and written untagged", {
  d <- tempfile("ne")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  f <- file.path(d, "T-1.yml")
  writeLines(c("id: T-1", "kind: table", "content:",
               "  - plan_filter:",
               "      where: AVISIT == \"CYCLE 2 DAY 1\" & PARAMCD %in% c(\"ALT\", \"AST\")",
               "      any: true",
               "  - plan_cell_style: {where: p > 0.5, bold: true}"), f)
  o <- tfl_read_output(f)
  expect_s3_class(o$content[[1]]$args$where, "tfl_r")
  expect_s3_class(o$content[[2]]$args$where, "tfl_r")
  expect_equal(nrow(tfl_schema_check(o$content[[1]])), 0L)
  g <- file.path(d, "out.yml")
  tfl_write_output(o, g)
  txt <- readLines(g)
  expect_false(any(grepl("!r", txt, fixed = TRUE)))
  o2 <- tfl_read_output(g)
  expect_identical(o2$content, o$content)
})

test_that("`!r` of a neutral condition is written untagged, of other R it stays", {
  mk <- function(w) tfl_output("T-1", "table", content = list(
    tfl_item("plan_filter", list(where = w))))
  g <- tempfile(fileext = ".yml")
  on.exit(unlink(g))
  tfl_write_output(mk(tfl_r("TRTEMFL == \"Y\"")), g)
  expect_false(any(grepl("!r", readLines(g), fixed = TRUE)))
  tfl_write_output(mk(tfl_r("between(AGE, 18, 65)")), g)
  expect_true(any(grepl("!r", readLines(g), fixed = TRUE)))
  expect_identical(tfl_read_output(g)$content[[1]]$args$where, tfl_r("between(AGE, 18, 65)"))
  # a text that starts with `!` is quoted by the emitter and comes back
  tfl_write_output(mk(tfl_r("!is.na(X) & X > 1")), g)
  expect_identical(tfl_read_output(g)$content[[1]]$args$where, tfl_r("!is.na(X) & X > 1"))
  tfl_write_output(mk(tfl_r("!is.na(X)")), g)
  expect_identical(tfl_read_output(g)$content[[1]]$args$where, tfl_r("!is.na(X)"))
  # a list of mixed forms
  w <- list(tfl_r("p > 1"), tfl_r("between(n, 1, 2)"), tfl_r("!is.na(N)"))
  tfl_write_output(mk(w), g)
  expect_identical(tfl_read_output(g)$content[[1]]$args$where, w)
})

test_that("tfl_schema_check() says what a string in a slot of only R is", {
  chk <- function(fn, v, a = "where") tfl_schema_check(tfl_item(fn, setNames(list(v), a)))
  expect_equal(nrow(chk("plan_filter", "AGE >= 18")), 0L)
  expect_equal(nrow(chk("plan_filter", list("p > 1", tfl_r("n > 0")))), 0L)
  m <- chk("plan_filter", "between(AGE, 18, 65)")
  expect_equal(m$level, "error")
  expect_match(m$message, "not a neutral condition", fixed = TRUE)
  expect_match(m$message, "between", fixed = TRUE)
  expect_match(m$message, "!r <R>", fixed = TRUE)
  expect_match(chk("plan_filter", list("p > 1", "f(x)"))$message, "refused: `f`", fixed = TRUE)
  m <- chk("plan_filter", tfl_r("\"AGE >= 18\""))
  expect_match(m$message, "string literal", fixed = TRUE)
  expect_match(m$message, "without the quotes", fixed = TRUE)
  expect_equal(nrow(chk("plan_cell_style", "p > 0.5")), 0L)
  # a slot that also takes a string says nothing of `!r`
  expect_equal(nrow(chk("plan_cell_style", "red", "color")), 0L)
  m <- chk("plan_blanks", "AGE >= 18")
  expect_false(any(grepl("neutral", m$message)))
})

test_that("tfl_content_code() writes a neutral string as an expression", {
  mk <- function(w) tfl_output("T-1", "table", data = list(source = "ard"), content = list(
    tfl_item("table_plan", list(cols = "ARM", rows = list(param = "PARAMCD"))),
    tfl_item("plan_filter", list(where = w))))
  code <- function(w) paste(tfl_content_code(mk(w)), collapse = "\n")
  expect_match(code("AGE >= 18"), "plan_filter(AGE >= 18)", fixed = TRUE)
  expect_false(grepl("\"AGE", code("AGE >= 18"), fixed = TRUE))
  # a mixed list: bare expressions, no structure()
  m <- code(list("p > 1", tfl_r("n > 0")))
  expect_match(m, "plan_filter(p > 1, n > 0)", fixed = TRUE)
  expect_false(grepl("structure", m, fixed = TRUE))
  expect_match(code(tfl_r("n > 0")), "plan_filter(n > 0)", fixed = TRUE)
  # a string that is not neutral stops
  expect_error(code("between(AGE, 18, 65)"), "not a neutral condition")
  expect_error(code(list("p > 1", "f(x)")), "not a neutral condition")
  # a tfl_r is its text, never a deparsed structure()
  expect_identical(.spec_code_line(tfl_r("n > 0")), "n > 0")
})

test_that("tfl_check_spec() on the example study finds nothing new", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  p <- tfl_check_spec(d)
  expect_false(any(grepl("neutral|string literal", p$message)))
})

test_that("only a slot of only R is written untagged; a mixed slot keeps `!r`", {
  g <- tempfile(fileext = ".yml")
  on.exit(unlink(g))
  rt <- function(items, ...) {
    tfl_write_output(tfl_output("T-1", "table", content = items, ...), g)
    tfl_read_output(g)
  }
  # enum|list<integer>|r
  it <- tfl_item("plan_blanks", list(where = tfl_r("n == 0")))
  expect_identical(rt(list(it))$content[[1]]$args$where, tfl_r("n == 0"))
  expect_true(any(grepl("!r", readLines(g), fixed = TRUE)))
  # string|r
  it <- tfl_item("plan_col_header", list(span = tfl_r("x == 1")))
  expect_identical(rt(list(it))$content[[1]]$args$span, tfl_r("x == 1"))
  expect_true(any(grepl("!r", readLines(g), fixed = TRUE)))
  # map<string, column|r>
  it <- tfl_item("geom_line", list(mapping = list(colour = tfl_r("x == 1"))))
  back <- rt(list(it))$content[[1]]$args$mapping$colour
  expect_identical(back, tfl_r("x == 1"))
  # the data steps and a report's own R keep what they had
  o <- tfl_output("T-1", "table", data = list(source = "code", steps = list(tfl_r("x > 1"))),
                  content = list(tfl_item("plan_filter", list(where = tfl_r("n > 0")))))
  tfl_write_output(o, g)
  txt <- readLines(g)
  expect_true(any(grepl("!r x > 1", txt, fixed = TRUE)))
  expect_true(any(grepl("where: n > 0", txt, fixed = TRUE)))
  # a slot of only R: untagged
  tfl_write_output(tfl_output("T-1", "table", content = list(
    tfl_item("plan_cell_style", list(where = tfl_r("p > 0.5"))))), g)
  expect_false(any(grepl("!r", readLines(g), fixed = TRUE)))
})

test_that("a lone name in a slot of only R but not a formula is written as before", {
  expect_identical(.schema_to_r("facet_wrap", list(facets = "PARAM"))$facets, "PARAM")
  expect_error(.schema_to_r("plan_filter", list(where = "PARAM")), "neutral")
  expect_error(.schema_to_r("plan_filter", list(where = "between(AGE, 1, 2)")), "`where`", fixed = TRUE)
})
