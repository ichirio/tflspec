# A plan, through its table spec and a workbook, back: the RTF a reader
# gets is the same byte for byte.  For each case of fixtures/table-cases.R:
#   (a) the plan's own RTF;
#   (b) tfl_as_table_spec() -> write the workbook -> read it ->
#       tfl_table_plan() -> RTF;
#   (c) the code tfl_table_code() writes from the read spec, run -> RTF.
# A plan's titles and footnotes are the report's: those cases go through
# the report spec (tfl_report()) instead.  A case whose spec says what it
# could not carry (not_converted) is checked to say so.

rt_rtf_doc <- function(doc) {
  old <- options(rtfreporter.render_time =
                   as.POSIXct("2000-01-01 00:00:00", tz = "UTC"))
  on.exit(options(old), add = TRUE)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  suppressMessages(generate_rtfreport(doc, f, overwrite = TRUE,
                                      program = "program.R"))
  readBin(f, "raw", file.info(f)$size)
}
rt_pages <- function(p) suppressMessages(plan_apply(p, "pages"))
rt_rtf <- function(p) rt_rtf_doc(rtf_tables(rtf_document(), rt_pages(p)))

test_that("a plan through its spec and a workbook gives the same RTF", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  env <- new.env(parent = globalenv())
  cases <- source(test_path("fixtures", "table-cases.R"), local = env)$value
  said <- c("cell_style", "style_border_obj", "titles_pages",
            "columns_twips", "columns_cell_format", "header_sep")
  report <- c("titles", "footnotes")
  for (nm in names(cases)) {
    p <- eval(cases[[nm]], env)
    sp <- suppressMessages(tfl_as_table_spec(p, output_id = "T"))
    if (nm %in% said) {
      expect_true(length(attr(sp, "not_converted")) > 0L, label = nm)
      next
    }
    # plan_after() steps are said (they stay in code) yet change nothing
    # here: the identity, and the decimal split the columns sheet carries
    if (nm %in% c("after_decimal", "after_fun")) {
      expect_match(attr(sp, "not_converted"), "plan_after", all = FALSE)
    } else {
      expect_identical(attr(sp, "not_converted"), character(), label = nm)
    }
    f <- tempfile(fileext = ".xlsx")
    if (nm %in% report) tfl_write_specs(f, table = sp, report = sp) else
      tfl_write_table_spec(sp, f)
    back <- tfl_read_report_spec(f, output_id = "T")
    unlink(f)
    data <- plan_layers(p)$data
    plan_b <- plan_cells(tfl_table_plan(data, back), notes = FALSE)
    code_env <- new.env(parent = globalenv())
    code_env$data <- data
    eval(parse(text = tfl_table_code(back, data = "data", plan = "plan_c", normalize = FALSE)),
         code_env)
    plan_c <- plan_cells(code_env$plan_c, notes = FALSE)
    if (nm %in% report) {
      a <- rt_rtf_doc(rtf_tables(rtf_document(), p))
      b <- rt_rtf_doc(tfl_report(back, content = plan_b))
      c <- rt_rtf_doc(tfl_report(back, content = plan_c))
    } else {
      a <- rt_rtf(p)
      b <- rt_rtf(plan_b)
      c <- rt_rtf(plan_c)
    }
    expect_identical(b, a, label = paste(nm, "(workbook)"))
    expect_identical(c, a, label = paste(nm, "(code)"))
  }
})

test_that("tfl_as_table_spec() compares the RTF, not the page objects", {
  skip_if_not_installed("cards")
  env <- new.env(parent = globalenv())
  cases <- source(test_path("fixtures", "table-cases.R"), local = env)$value
  # the decimal split: page objects that differ, the same RTF
  sp <- suppressMessages(tfl_as_table_spec(eval(cases$columns_decimal, env),
                                           output_id = "T"))
  expect_true(attr(sp, "same_pages"))
  sp <- suppressMessages(tfl_as_table_spec(eval(cases$base, env),
                                           compare = FALSE))
  expect_true(is.na(attr(sp, "same_pages")))
})
