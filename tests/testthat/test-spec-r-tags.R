# R whose `!r` tag was lost stops in the reader (#893, R/spec_r_tags.R):
# the schema's type decides, never the text.

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
  real <- tfl_schema
  local_mocked_bindings(tfl_schema = function(fn = NULL) {
    if (identical(fn, "plan_zero")) {
      return(list(package = "rtfreporter",
                  args = list(zero = list(type = "guard"), cells = list(type = "list<guard>"),
                              text = list(type = "template|guard"))))
    }
    real(fn)
  })
  # with the tag: read
  o <- rt_read(c("  - plan_zero:", "      zero: !r 'n == 0 ~ \"0\"'"))
  expect_identical(o$content[[1]]$args$zero, tfl_r("n == 0 ~ \"0\""))
  # quoted: stops, naming the argument, the report and how to write it
  e <- expect_error(rt_read(c("  - plan_zero:", "      zero: 'n == 0 ~ \"0\"'")),
                    "plan_zero(zero = ) takes R", fixed = TRUE)
  expect_match(conditionMessage(e), "T-9-1", fixed = TRUE)
  expect_match(conditionMessage(e), "`!r 'n == 0 ~ \"0\"'`", fixed = TRUE)
  # written without the tag (a plain scalar): the same
  expect_error(rt_read(c("  - plan_zero:", "      zero: n == 0 ~ \"0\"")), "tag was lost")
  # a list of guards, one of them untagged
  expect_error(rt_read(c("  - plan_zero:",
                         "      cells: [!r 'n == 0 ~ \"0\"', 'p < 0.001 ~ \"<0.1%\"']")),
               "plan_zero(cells = ) takes R", fixed = TRUE)
  # `{fn:, args:}` form, and a value under a call
  expect_error(rt_read(c("  - fn: plan_zero", "    args: {zero: 'n == 0 ~ \"0\"'}")),
               "plan_zero(zero = )", fixed = TRUE)
  # template|guard: a string is a template, read as it is
  o <- rt_read(c("  - plan_zero:", "      text: 'n == 0 ~ \"0\"'"))
  expect_identical(o$content[[1]]$args$text, "n == 0 ~ \"0\"")
})

test_that("a string in a slot of only R stops when it is no neutral condition", {
  # plan_filter's where (list<r>|r): a neutral string is read as R
  o <- rt_read(c("  - plan_filter: {where: p > 0.05}"))
  expect_s3_class(o$content[[1]]$args$where, "tfl_r")
  e <- expect_error(rt_read(c("  - plan_filter: {where: 'between(AGE, 18, 65)'}")),
                    "plan_filter(where = ) takes R", fixed = TRUE)
  expect_match(conditionMessage(e), "`!r 'between(AGE, 18, 65)'`", fixed = TRUE)
  o <- rt_read(c("  - plan_filter: {where: !r 'between(AGE, 18, 65)'}"))
  expect_identical(o$content[[1]]$args$where, tfl_r("between(AGE, 18, 65)"))
  # a quote inside is doubled, as YAML's single quotes write it
  expect_error(rt_read(c("  - plan_filter: {where: \"grepl('x', A)\"}")),
               "`!r 'grepl(''x'', A)'`", fixed = TRUE)
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
  ty <- function(t) tflspec:::.schema_type(t)
  lost <- tflspec:::.r_tag_lost
  g <- "n == 0 ~ \"0\""
  expect_true(lost(g, ty("guard")))
  expect_true(lost(list(tfl_r(g), g), ty("list<guard>")))
  expect_true(lost(c(a = g), ty("map<string, guard>")))
  expect_false(lost(tfl_r(g), ty("guard")))
  expect_false(lost(g, ty("template|guard")))
  expect_false(lost(g, ty("template|list<guard|template>")))
  expect_false(lost(g, ty("string|r")))
  expect_false(lost(list(tfl_r(g), "{n}"), ty("list<guard|template>")))
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
