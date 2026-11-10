# plan_style(border_group = ): the rule under the last row of each row group,
# written in a SPEC as sides (`border_group: bottom`) like the other border_*
# items: checked by the schema, read from YAML, written as code only when set.

bg_item <- function(b) tfl_item("plan_style", list(border_group = b))

bg_yaml <- function(style_line = NULL) {
  f <- tempfile(fileext = ".yml")
  writeLines(c(
    "id: T-1", "kind: table", "content:",
    "  - table_plan: {cols: ARM, rows: {group: variable}}",
    "  - plan_cells: {categorical: '{n} ({p:.1f%})'}",
    "  - plan_row_group: {mode: value}",
    style_line), f)
  f
}

test_that("the schema accepts border_group as an rtf_border call", {
  ok <- function(b) nrow(tfl_schema_check(bg_item(b))) == 0L
  expect_true(ok(tfl_call("rtf_border", list(bottom = TRUE))))
  expect_true(ok(tfl_call("rtf_border", list(
    bottom = tfl_call("rtf_border_line", list(style = "dot", width = 10L))))))
  p <- tfl_schema_check(bg_item("middle"))
  expect_identical(p$arg, "border_group")
  expect_match(p$message, "call<rtf_border>", fixed = TRUE)
})

test_that("the style sheet's border_group column is plan_style()'s rule of a group", {
  tb <- function(...) {
    list(tables = data.frame(output_id = "T-1", cols = "ARM", rows = "group = variable",
                             stringsAsFactors = FALSE),
         cells = data.frame(output_id = "T-1", variable = NA, row = NA,
                            template = "{n} ({p:.1f%})", stringsAsFactors = FALSE),
         style = data.frame(output_id = "T-1", ..., stringsAsFactors = FALSE))
  }
  sp <- tfl_table_spec(tb(border_group = "bottom"))
  expect_identical(sp$style$border_group, "bottom")
  code <- paste(tfl_table_code(sp, "T-1", data = "data", normalize = FALSE), collapse = "\n")
  expect_match(code, "border_group = rtf_border(bottom = TRUE)", fixed = TRUE)
  # blank / absent: nothing is written
  sp0 <- tfl_table_spec(tb(align_count_pct = "TRUE"))
  expect_false("border_group" %in% names(sp0$style) &&
                 !all(is.na(sp0$style$border_group)))
  code0 <- paste(tfl_table_code(sp0, "T-1", data = "data", normalize = FALSE), collapse = "\n")
  expect_false(grepl("border_group", code0, fixed = TRUE))
  expect_error(tfl_table_spec(tb(border_group = "middle")),
               "top [|] bottom [|] left [|] right, or none")
})

test_that("codegen writes border_group only when it is set", {
  skip_if_not_installed("cards")
  on <- bg_yaml("  - plan_style: {border_group: {fn: rtf_border, args: {bottom: true}}}")
  off <- bg_yaml()
  on.exit(unlink(c(on, off)))
  out <- tfl_read_output(on)
  for (it in out$content) expect_identical(nrow(tfl_schema_check(it)), 0L, info = it$fn)
  code_on <- paste(tfl_content_code(out, pipe = "|>"), collapse = "\n")
  expect_match(code_on, "border_group = rtf_border(bottom = TRUE)", fixed = TRUE)
  code_off <- paste(tfl_content_code(tfl_read_output(off), pipe = "|>"), collapse = "\n")
  expect_false(grepl("border_group", code_off, fixed = TRUE))
  # absent: the code is what it was (no plan_style item at all)
  expect_false(grepl("plan_style", code_off, fixed = TRUE))
})

test_that("a SPEC with border_group draws a rule under each group's last row", {
  skip_if_not_installed("cards")
  render <- function(style_line) {
    f <- bg_yaml(style_line)
    on.exit(unlink(f))
    code <- tfl_content_code(tfl_read_output(f), pipe = "|>")
    ard <- cards::ard_stack(cards::ADSL, .by = ARM,
                            cards::ard_tabulate(variables = c(SEX, RACE)))
    env <- new.env()
    env$data <- rtfreporter::normalize_ard(ard)
    eval(parse(text = c("library(rtfreporter)", code)), env)
    rtf <- tempfile(fileext = ".rtf")
    on.exit(unlink(rtf), add = TRUE)
    suppressMessages(rtfreporter::generate_rtfreport(
      rtfreporter::rtf_tables(rtfreporter::rtf_document(), env$plan), rtf,
      overwrite = TRUE, program = "p.R"))
    rows <- strsplit(paste(readLines(rtf, warn = FALSE), collapse = "\n"),
                     "\\trowd", fixed = TRUE)[[1L]][-1L]
    # which rows (\trowd blocks) carry a bottom cell border
    grepl("\\clbrdrb", rows, fixed = TRUE)
  }
  with <- render("  - plan_style: {border_group: {fn: rtf_border, args: {bottom: true}}}")
  without <- render(NULL)
  # blocks: the header, SEX's two rows, RACE's three; the rule is under each
  # group's last row (SEX's, RACE's), the header keeping its own default rule
  # (block 1, as without it)
  expect_identical(which(with), c(1L, 3L, 6L))
  # absent: no rule under any group (only the default one of the header)
  expect_identical(which(without), 1L)
})
