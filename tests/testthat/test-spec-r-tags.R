# A guard whose `!r` tag was lost stops in the reader (#893,
# R/spec_r_tags.R): the schema's type decides, never the text.

# a report file of these content lines, read
rt_read <- function(lines) {
  d <- tempfile("rtag")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "T-9-1.yml")
  writeLines(c("id: T-9-1", "kind: table", "content:", lines), f)
  tfl_read_output(f)
}

test_that("a guard chain read with `!r` is a tfl_r", {
  o <- rt_read(c("  - plan_cells:",
                 "      categorical: !r 'c(n == 0 ~ \"0\", \"{n}\")'",
                 "      continuous:",
                 "        \"Mean (SD)\": [!r 'N < 3 ~ \"{mean:.1f} (-)\"', \"{mean:.1f} ({sd:.2f})\"]"))
  a <- o$content[[1]]$args
  expect_s3_class(a$categorical, "tfl_r")
  expect_identical(a$categorical, tfl_r("c(n == 0 ~ \"0\", \"{n}\")"))
  ms <- a$continuous[["Mean (SD)"]]
  expect_s3_class(ms[[1]], "tfl_r")
  expect_identical(ms[[2]], "{mean:.1f} ({sd:.2f})")
})

test_that("a guard read as a quoted string into a guard-only argument stops", {
  # (the schema has no argument of guards alone: one is added here)
  real <- .schema_all
  local_mocked_bindings(.schema_all = function(figure = TRUE) {
    c(real(figure), list(plan_zero = list(
      package = "rtfreporter",
      args = list(zero = list(type = "guard"), cells = list(type = "list<guard>"),
                  text = list(type = "template|guard")))))
  })
  # with the tag: read
  o <- rt_read(c("  - plan_zero:", "      zero: !r 'n == 0 ~ \"0\"'"))
  expect_identical(o$content[[1]]$args$zero, tfl_r("n == 0 ~ \"0\""))
  # quoted: stops, naming the argument, the report and how to write it
  e <- expect_error(rt_read(c("  - plan_zero:", "      zero: 'n == 0 ~ \"0\"'")),
                    "plan_zero(zero = ) takes R here (a guard)", fixed = TRUE)
  expect_match(conditionMessage(e), "T-9-1", fixed = TRUE)
  expect_match(conditionMessage(e), "`!r 'n == 0 ~ \"0\"'`", fixed = TRUE)
  # written without the tag (a plain scalar): the same
  expect_error(rt_read(c("  - plan_zero:", "      zero: n == 0 ~ \"0\"")), "tag was lost")
  # a list of guards, one of them untagged
  expect_error(rt_read(c("  - plan_zero:",
                         "      cells: [!r 'n == 0 ~ \"0\"', 'p < 0.001 ~ \"<0.1%\"']")),
               "plan_zero(cells = ) takes R here", fixed = TRUE)
  # `{fn:, args:}` form
  expect_error(rt_read(c("  - fn: plan_zero", "    args: {zero: 'n == 0 ~ \"0\"'}")),
               "plan_zero(zero = )", fixed = TRUE)
  # an item whose map is itself a call (`- call: {fn:, args:}`): checked as that call
  expect_error(rt_read(c("  - call: {fn: plan_zero, args: {zero: 'n == 0 ~ \"0\"'}}")),
               "plan_zero(zero = )", fixed = TRUE)
  o <- rt_read(c("  - call: {fn: survfit2, package: ggsurvfit, args: {formula: !r 'y ~ x'}}"))
  expect_s3_class(o$content[[1]]$args, "tfl_call")
  # a quote inside is doubled, as YAML's single quotes write it
  expect_error(rt_read(c("  - plan_zero:", "      zero: \"A == 'x' ~ 'y'\"")),
               "`!r 'A == ''x'' ~ ''y'''`", fixed = TRUE)
  # template|guard: a string is a template, read as it is
  o <- rt_read(c("  - plan_zero:", "      text: 'n == 0 ~ \"0\"'"))
  expect_identical(o$content[[1]]$args$text, "n == 0 ~ \"0\"")
})

test_that("a string in a slot of only R opens as written; the check says what it is", {
  # plan_filter's where (list<r>|r): a neutral string is read as R
  o <- rt_read(c("  - plan_filter: {where: p > 0.05}"))
  expect_s3_class(o$content[[1]]$args$where, "tfl_r")
  # any other string as written (tflplanner opens it; the check refuses it)
  o <- rt_read(c("  - plan_filter: {where: 'between(AGE, 18, 65)'}"))
  expect_identical(o$content[[1]]$args$where, "between(AGE, 18, 65)")
  expect_match(tfl_schema_check(o$content[[1]])$message, "not a neutral condition", fixed = TRUE)
  o <- rt_read(c("  - plan_filter: {where: !r 'between(AGE, 18, 65)'}"))
  expect_identical(o$content[[1]]$args$where, tfl_r("between(AGE, 18, 65)"))
  # a formula's slot (plan_cell_style's where)
  o <- rt_read(c("  - plan_cell_style: {where: 'between(p, 0, 1)', bold: true}"))
  expect_identical(o$content[[1]]$args$where, "between(p, 0, 1)")
})

test_that("an `r` slot that writes a string keeps it, as the code writes it", {
  o <- rt_read(c("  - plan_columns: {cell_format: right_align}",
                 "  - plan_paginate_cols: {col_header: Continued}"))
  expect_identical(o$content[[1]]$args$cell_format, "right_align")
  expect_identical(o$content[[2]]$args$col_header, "Continued")
})

test_that("a type with a member of text keeps a plain string", {
  # template|map<...>|call<cell_rows>|list<guard|template>
  o <- rt_read(c("  - plan_cells:",
                 "      categorical: \"{n} ({p:.1f%})\"",
                 "      continuous: {\"Mean (SD)\": [\"{mean:.1f} ({sd:.2f})\"]}"))
  expect_identical(o$content[[1]]$args$categorical, "{n} ({p:.1f%})")
  # string|r
  o <- rt_read(c("  - plan_col_header: {span: Treatment}"))
  expect_identical(o$content[[1]]$args$span, "Treatment")
  # map<string, column|guard>
  o <- rt_read(c("  - table_plan: {cols: TRT01A, rows: {soc: AEBODSYS, pt: AEDECOD}}"))
  expect_identical(o$content[[1]]$args$rows$soc, "AEBODSYS")
})

test_that("the rule reads the type only", {
  ty <- function(t) .schema_type(t)
  lost <- .r_tag_lost
  g <- "n == 0 ~ \"0\""
  expect_true(lost(g, ty("guard")))
  expect_true(lost(list(tfl_r(g), g), ty("list<guard>")))
  expect_true(lost(c(a = g), ty("map<string, guard>")))
  expect_false(lost(tfl_r(g), ty("guard")))
  expect_false(lost(g, ty("template|guard")))
  expect_false(lost(g, ty("template|list<guard|template>")))
  expect_false(lost(list(tfl_r(g), "{n}"), ty("list<guard|template>")))
  # a type with no guard: the reader keeps its strings (r, formula)
  expect_false(lost("right_align", ty("r")))
  expect_false(lost("f(x)", ty("list<r>|r")))
  expect_false(lost("f(x)", ty("formula")))
  expect_false(lost("abc", ty("list<number>|r")))
  # a guard beside an r: the string is taken by the r
  expect_false(lost(g, ty("guard|r")))
  # a value no member fits, as R or not, is the check's (tfl_schema_check())
  expect_false(lost(list(a = 1), ty("guard")))
  expect_false(lost(NULL, ty("guard")))
  expect_false(lost(tfl_unset(), ty("guard")))
})

test_that("the example study and its reports still read", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  for (f in list.files(file.path(d, "outputs"), pattern = "[.]yml$", full.names = TRUE)) {
    expect_no_error(tfl_read_output(f))
  }
  expect_no_error(tfl_read_study_spec(d))
})
