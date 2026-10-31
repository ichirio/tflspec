# A border's sides with a look of their own (rtfreporter::rtf_border_line()
# in rtf_border()), the rules between rows and cells (inside_h, inside_v),
# and a whole table's border as a call: checked by the schema, read from
# YAML, written as code rtfreporter runs.

border_item <- function(b, arg = "border") {
  tfl_item("plan_style", stats::setNames(list(b), arg))
}

test_that("a side's look is checked inside the border", {
  ok <- function(b, arg = "border") nrow(tfl_schema_check(border_item(b, arg))) == 0L
  expect_true(ok(tfl_call("rtf_border", list(all = TRUE, inside_h = TRUE,
                                             inside_v = "none"))))
  expect_true(ok(tfl_call("rtf_border", list(
    bottom = tfl_call("rtf_border_line", list(style = "double", width = 30L,
                                              color = "#003366")))),
    "border_last_row"))
  expect_true(ok("tfl"))
  p <- tfl_schema_check(border_item(tfl_call("rtf_border", list(
    top = tfl_call("rtf_border_line", list(style = "wavy"))))))
  expect_identical(p$arg, "border")
  expect_match(p$message, "rtf_border(top = ): rtf_border_line(style = ) is character wavy",
               fixed = TRUE)
  p <- tfl_schema_check(border_item(tfl_call("rtf_border", list(top = "wavy"))))
  expect_match(p$message, "rtf_border(top = ) is character wavy", fixed = TRUE)
  p <- tfl_schema_check(border_item(tfl_call("rtf_border", list(
    bottom = tfl_call("rtf_border_line", list(width = "thick"))))))
  expect_match(p$message, "rtf_border_line(width = ) is character thick", fixed = TRUE)
  # a side is not a border
  p <- tfl_schema_check(border_item(tfl_call("rtf_border_line", list(style = "dot"))))
  expect_match(p$message, "call<rtf_border>", fixed = TRUE)
})

test_that("a border with sides of their own look reads from YAML and runs", {
  skip_if_not_installed("cards")
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(c(
    "id: T-1", "kind: table", "content:",
    "  - table_plan: {cols: ARM, rows: {group: variable}}",
    "  - plan_cells: {categorical: '{n} ({p:.1f%})'}",
    paste0("  - plan_style: {border_body: {fn: rtf_border, args: {inside_h: ",
           "{fn: rtf_border_line, args: {style: dot, width: 10}}}}, ",
           "border_last_row: {fn: rtf_border, args: {bottom: {fn: rtf_border_line, ",
           "args: {style: double, width: 30, color: '#003366'}}}}}")), f)
  out <- tfl_read_output(f)
  for (it in out$content) expect_identical(nrow(tfl_schema_check(it)), 0L, info = it$fn)
  code <- tfl_content_code(out, pipe = "|>")
  expect_true(any(grepl(
    "border_last_row = rtf_border(\n      bottom = rtf_border_line(style = \"double\", width = 30, color = \"#003366\")",
    paste(code, collapse = "\n"), fixed = TRUE)))
  ard <- cards::ard_stack(cards::ADSL, .by = ARM, cards::ard_tabulate(variables = SEX))
  env <- new.env()
  env$data <- rtfreporter::normalize_ard(ard)
  eval(parse(text = c("library(rtfreporter)", code)), env)
  expect_s3_class(env$plan, "table_plan")
  rtf <- tempfile(fileext = ".rtf")
  on.exit(unlink(rtf), add = TRUE)
  suppressMessages(rtfreporter::generate_rtfreport(
    rtfreporter::rtf_tables(rtfreporter::rtf_document(), env$plan), rtf,
    overwrite = TRUE, program = "p.R"))
  txt <- paste(readLines(rtf, warn = FALSE), collapse = "\n")
  # a dotted rule between the body's rows, a double one 30 twips wide under the last
  expect_match(txt, "\\brdrdot\\brdrw10", fixed = TRUE)
  expect_match(txt, "\\brdrdb\\brdrw30", fixed = TRUE)
})
