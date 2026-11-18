# A blank-row rule as plan_blanks(where = ): rtfreporter's
# blank_rows_by_change() and blank_rows_by_rule(), written by a SPEC
# ({fn: blank_rows_by_change, args: {cols: ...}}), checked by the schema.

test_that("the schema takes a blank-row rule, and a list of them, in plan_blanks(where = )", {
  chg <- tfl_call("blank_rows_by_change", list(cols = "AEBODSYS"))
  rul <- tfl_call("blank_rows_by_rule", list(col = "row_label", pattern = "^Any",
                                             where = "after"))
  ok <- function(where) nrow(tfl_schema_check(tfl_item("plan_blanks", list(where = where))))
  expect_identical(ok(chg), 0L)
  expect_identical(ok(rul), 0L)
  expect_identical(ok(list(chg, rul)), 0L)
  expect_identical(ok(tfl_call("blank_rows_by_change", list(
    cols = list("PARAM", "AVISIT"), group_by = "indent", include_before_first = FALSE,
    include_after_last = FALSE))), 0L)
  # the named rules and positions as before
  expect_identical(ok("between_groups"), 0L)
  expect_identical(ok(list(0L, 5L)), 0L)
  # a wrong value inside the rule is said, with the rule's name
  p <- tfl_schema_check(tfl_item("plan_blanks", list(where = tfl_call(
    "blank_rows_by_change", list(cols = "AEBODSYS", group_by = "level")))))
  expect_gt(nrow(p), 0L)
  expect_match(paste(p$message, collapse = " "), "blank_rows_by_change")
  expect_gt(nrow(tfl_schema_check(tfl_item("plan_blanks", list(where = tfl_call(
    "blank_rows_by_rule", list(col = "row_label", pattern = "^Any", where = "inside")))))), 0L)
  # another function is not a blank-row rule
  expect_gt(ok(tfl_call("rtf_border", list(bottom = TRUE))), 0L)
  # the schema's arguments are the installed rtfreporter's
  skip_if_not_installed("rtfreporter")
  for (fn in c("blank_rows_by_change", "blank_rows_by_rule")) {
    f <- names(formals(getExportedValue("rtfreporter", fn)))
    expect_setequal(names(tfl_schema(fn)$args), f)
  }
})

test_that("a blank-row rule is written as rtfreporter's call", {
  out <- tfl_output("T-1", "table", content = list(
    tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable"))),
    tfl_item("plan_blanks", list(where = tfl_call("blank_rows_by_rule", list(
      col = "Characteristic", pattern = "^[^ ]", where = "before"))))))
  flat <- function(o) gsub("\\s+", " ", gsub("\\(\\s+", "(", gsub("\\s+\\)", ")",
    paste(tfl_content_code(o, pipe = "|>"), collapse = "\n"))))
  code <- flat(out)
  expect_match(code, paste0("plan_blanks\\(where = blank_rows_by_rule\\(col = \"Characteristic\", ",
                            "pattern = \"\\^\\[\\^ \\]\", where = \"before\"\\)\\)"))
  two <- tfl_output("T-1", "table", content = list(
    tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable"))),
    tfl_item("plan_blanks", list(where = list(
      tfl_call("blank_rows_by_change", list(cols = "AEBODSYS")),
      tfl_call("blank_rows_by_rule", list(col = "AEBODSYS", pattern = "^Any")))))))
  code <- flat(two)
  expect_match(code, "where = list\\(blank_rows_by_change\\(cols = \"AEBODSYS\"\\), blank_rows_by_rule\\(")
})

test_that("the sheets leave a rule to content_more, whole", {
  items <- list(
    tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable"))),
    tfl_item("plan_blanks", list(where = tfl_call("blank_rows_by_change", list(cols = "AEBODSYS")),
                                 first = TRUE)))
  sp <- tflspec:::.plan_sheets_from_content(items, "T")
  expect_true(any(grepl("^plan_blanks: where: a blank-row rule", attr(sp, "not_converted"))))
  # no column is written: the item, its first = TRUE too, goes whole
  for (k in c("blank_where", "blank_first")) {
    expect_true(is.null(sp$layout[[k]]) || all(is.na(sp$layout[[k]])), info = k)
  }
  # positions and the named rules still go to the column
  sp <- tflspec:::.plan_sheets_from_content(list(
    items[[1L]], tfl_item("plan_blanks", list(where = list(0L, 5L)))), "T")
  expect_length(attr(sp, "not_converted"), 0L)
  sp <- tflspec:::.plan_sheets_from_content(list(
    items[[1L]], tfl_item("plan_blanks", list(where = "between_groups"))), "T")
  expect_length(attr(sp, "not_converted"), 0L)
})

test_that("a rule puts the blank rows rtfreporter puts for the same call (ADSL)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
  ard <- cards::ard_stack(cards::ADSL, .by = ARM,
                          cards::ard_summary(variables = AGE),
                          cards::ard_tabulate(variables = c(SEX, RACE)))
  data <- rtfreporter::normalize_ard(ard)
  # the variables' heading rows (the stub's indent is a format, not text)
  rule <- tfl_call("blank_rows_by_rule", list(col = "Characteristic",
                                              pattern = "^(AGE|SEX|RACE)$", where = "before"))
  out <- tfl_output("T-1", "table", content = list(
    tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable"))),
    tfl_item("plan_cells", list(continuous = list(`Mean (SD)` = "{mean:.1f} ({sd:.2f})"),
                                categorical = "{n}")),
    tfl_item("plan_stub", list(name = "Characteristic")),
    tfl_item("plan_blanks", list(where = rule))))
  p <- suppressMessages(tfl_content_run(out, data))
  got <- suppressMessages(rtfreporter::plan_apply(p))
  # the same plan written with rtfreporter's verbs by hand
  want <- rtfreporter::table_plan(data, cols = "ARM", rows = c(group = "variable")) |>
    rtfreporter::plan_cells(continuous = c(`Mean (SD)` = "{mean:.1f} ({sd:.2f})"),
                            categorical = "{n}") |>
    rtfreporter::plan_stub(name = "Characteristic") |>
    rtfreporter::plan_blanks(where = rtfreporter::blank_rows_by_rule(
      "Characteristic", "^(AGE|SEX|RACE)$", where = "before"))
  want <- suppressMessages(rtfreporter::plan_apply(want))
  expect_identical(got[[1L]]$blank_rows, want[[1L]]$blank_rows)
  expect_identical(got[[1L]]$data, want[[1L]]$data)
  # a blank row before each variable's heading after the first (one above
  # the first row is plan_blanks(first = TRUE)'s)
  expect_identical(got[[1L]]$data$Characteristic[got[[1L]]$blank_rows + 1L],
                   c("SEX", "RACE"))
})
