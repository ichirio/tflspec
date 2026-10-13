# The code a table / report definition stands for: what tfl_table_code() and
# tfl_report_code() write must make what tfl_table_plan() / tfl_report() make.

sc_ard <- function() {
  adsl <- cards::ADSL
  adsl$SEX <- as.character(adsl$SEX)
  adsl$TRT <- as.character(adsl$ARM)
  suppressMessages(normalize_ard(cards::ard_stack(
    adsl, .by = TRT,
    cards::ard_continuous(
      variables = c(AGE, BMIBL),
      statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd"))),
    cards::ard_categorical(variables = SEX, statistic = ~ c("n", "p")),
    .total_n = TRUE)))
}

sc_plan <- function(d) {
  table_plan(d, cols = "TRT", rows = c(group = "variable")) |>
    plan_levels(SEX = c("M", "F")) |>
    plan_labels(AGE = "Age (years)", SEX = "Sex") |>
    plan_cells(continuous  = c("n" = "{N:d}", "Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n:d} ({p:.1f%})", notes = FALSE) |>
    plan_digits(1, rounding = "sas") |>
    plan_stub(name = "row_label", before = TRUE) |>
    plan_blanks(where = "between_groups", first = TRUE) |>
    plan_paginate_rows(max_rows = 6, split = "group_safe") |>
    plan_columns(widths = c(4, 2)) |>
    plan_style(align_count_pct = TRUE) |>
    plan_col_header(values = list(n = TRUE), rtf_col_header(c("", "{col}"),
                                                 c("Characteristic", "(N={n})")))
}

# run the written code with `data` bound, give the plan
sc_run <- function(code, d, name = "plan") {
  env <- new.env(parent = asNamespace("tflspec"))
  env$data <- d
  suppressMessages(eval(parse(text = code), env))
  get(name, envir = env)
}

sc_pages <- function(p) plan_apply(p, "pages")

test_that("tfl_table_code() writes the plan the definition makes", {
  skip_if_not_installed("cards")
  d <- sc_ard()
  sp <- suppressMessages(tfl_as_table_spec(sc_plan(d), output_id = "T1"))
  code <- tfl_table_code(sp, pipe = "|>")
  expect_match(code[grep("<- table_plan", code)[1L]], "^plan <- table_plan\\(data, cols = \"TRT\"")
  expect_true(any(grepl("plan_digits(rounding = \"sas\")", code, fixed = TRUE)))
  expect_true(any(grepl("plan_col_header(", code, fixed = TRUE)))
  expect_true(any(grepl("data.frame(", code, fixed = TRUE)))
  expect_false(any(grepl("structure(", code, fixed = TRUE)))
  expect_false(any(grepl("tfl_read_table_spec", code, fixed = TRUE)))

  by_code <- sc_run(code, d)
  by_spec <- tfl_table_plan(d, sp) |>
               plan_cells(notes = FALSE)
  expect_equal(sc_pages(by_code), sc_pages(by_spec))
  expect_equal(sc_pages(by_code), sc_pages(sc_plan(d)))

  # the names and the pipe are the caller's
  code2 <- tfl_table_code(sp, data = "dm", plan = "tbl", pipe = "%>%")
  expect_match(code2[grep("<- table_plan", code2)[1L]], "^tbl <- table_plan\\(dm,")
  expect_true(any(grepl("%>%$", code2)))
  expect_false(any(grepl("|>", code2, fixed = TRUE)))
  expect_error(tfl_table_code(tfl_table_spec(tables = data.frame(
    output_id = c("A", "B"), cols = "TRT"))), "defines 2 reports")
})

test_that("a spanning header, widths by name and a hidden column go over too", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$GRP <- ifelse(adsl$AGE < 70, "Young", "Old")
  d <- suppressMessages(normalize_ard(cards::ard_stack(
    adsl, .by = c(TRT, GRP),
    cards::ard_categorical(variables = SEX, statistic = ~ c("n", "p")))))
  sp <- tfl_table_spec(
    tables = data.frame(cols = "TRT | GRP", rows = "group = variable",
                        sort = "TRUE"),
    variables = data.frame(variable = "GRP", levels = "Young | Old"),
    layout = data.frame(stub_name = "row_label", stub_before = "TRUE"),
    columns = data.frame(column = c("row_label", ".values"),
                         rel_width = c("4", "2")),
    col_header = data.frame(
      line = c(1, 1, 2, 2, 2),
      cols = c("row_label", ".values", "row_label", "GRP = Young", "GRP = Old"),
      span = c(NA, "TRT", NA, "each", "each"),
      text = c(NA, "{col1}", "Sex", "<70", ">=70"),
      border_bottom = c(NA, "single", NA, NA, NA)))
  code <- tfl_table_code(sp, pipe = "|>")
  expect_true(any(grepl("plan_columns(", code, fixed = TRUE)))
  expect_true(any(grepl("plan_sort(TRUE)", code, fixed = TRUE)))
  expect_equal(sc_pages(sc_run(code, d)),
               sc_pages(tfl_table_plan(d, sp) |>
                          plan_cells(notes = FALSE)))
})

test_that("the example workbooks write code that runs to the same plan", {
  skip_if_not_installed("readxl")
  dir <- system.file("extdata", "ard-spec", package = "tflspec")
  skip_if(!nzchar(dir), "examples not installed")
  for (id in c("DM", "AE", "ORR", "LB", "PK")) {
    code <- tfl_table_code(file.path(dir, paste0(id, ".xlsx")), pipe = "|>")
    expect_true(is.character(code) && length(code) > 1L, label = id)
    expect_silent(parse(text = code))
  }
})

# ------------------------------------------------------------ reports

sc_rep_spec <- function() tfl_table_spec(
  study = c(output_path = "out", program_dir = "C:\\tfl"),
  report = data.frame(output_id = c(NA, "T2"), page_footer = c(NA, "FALSE"),
                      title_font_size_half_points = c(NA, "20")),
  page = data.frame(output_id = "T2", orientation = "portrait",
                    margin_left_in = "0.5", font_size_half_points = "18"),
  header = data.frame(output_id = c(NA, NA, "T1", "T1"), line = c(1, 2, 3, 4),
                      left = c("SPONSOR", "PROTOCOL", NA, NA),
                      center = c(NA, NA, NA, "Table 1"),
                      right = c(NA, "Page {PAGE} of {TOTAL_PAGES}", NA, NA)),
  footer = data.frame(output_id = c(NA, "T1"), line = c(99, 1),
                      left = c("{PROGRAM}  {DATETIME}", "A footnote.")),
  titles = data.frame(output_id = "T2", line = 1, center = "Table 2"),
  footnotes = data.frame(output_id = "T2", line = 1, left = "Below the table."))

sc_render <- function(doc) {
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

test_that("tfl_report_code() writes the document tfl_report() makes", {
  old <- options(rtfreporter.render_time = as.POSIXct("2026-01-01 09:00"))
  on.exit(options(old), add = TRUE)
  pages <- as_rtftables(data.frame(A = c("a", "b"), B = c("c", "d")))
  for (id in c("T1", "T2")) {
    sp <- suppressMessages(tflspec:::.ard_spec_scope(sc_rep_spec(), id))
    code <- tfl_report_code(sp, content = "pages")
    expect_match(code[1L], "^doc <- rtf_document\\(", label = id)
    env <- new.env(parent = asNamespace("tflspec"))
    env$pages <- pages
    eval(parse(text = code), env)
    expect_identical(sc_render(env$doc), sc_render(tfl_report(sp, content = pages)),
                     label = id)
  }
  sp <- suppressMessages(tflspec:::.ard_spec_scope(sc_rep_spec(), "T2"))
  code <- tfl_report_code(sp, content = "pages", doc = "rtf")
  expect_false(any(grepl("rtf_footer", code)))             # no running footer
  expect_true(any(grepl("rtf_header", code)))
  # one pipe from rtf_document(): the steps take the document from it
  expect_true(any(grepl("  rtf_titles(", code, fixed = TRUE)))
  expect_true(any(grepl("font_size_half_points = 20", code, fixed = TRUE)))
  expect_true(any(grepl("rtf_default_format(font_size_half_points = 18", code,
                        fixed = TRUE)))
})

test_that("a figure report writes rtf_figures(), at its size on the page", {
  # blank: the figure's own size, as rtfreporter places it
  sp <- tfl_table_spec(report = data.frame(output_id = "F1", type = "figure"))
  code <- tfl_report_code(sp, content = "plots")
  expect_true(any(grepl("rtf_figures(plots)", code, fixed = TRUE)))
  # fit: letter landscape's body, 9.5 x 6.7 in (two spacing lines)
  sp <- tfl_table_spec(report = data.frame(output_id = "F1", type = "figure",
                                           figure_width_in = "fit", figure_height_in = "fit"))
  code <- tfl_report_code(sp, content = "plots")
  expect_true(any(grepl("rtf_figures(plots, width_twips = 13680, height_twips = 9648)",
                        code, fixed = TRUE)))
})

test_that("a table and its report, written out, make the report the objects make", {
  skip_if_not_installed("cards")
  old <- options(rtfreporter.render_time = as.POSIXct("2026-01-01 09:00"))
  on.exit(options(old), add = TRUE)
  d <- sc_ard()
  tsp <- suppressMessages(tfl_as_table_spec(sc_plan(d), output_id = "T1"))
  rsp <- suppressMessages(tflspec:::.ard_spec_scope(sc_rep_spec(), "T1"))
  code <- c(tfl_table_code(tsp, pipe = "|>"),
            tfl_report_code(rsp, content = "plan"))
  env <- new.env(parent = asNamespace("tflspec"))
  env$data <- d
  suppressMessages(eval(parse(text = code), env))
  by_objects <- tfl_report(rsp, content = tfl_table_plan(d, tsp) |>
                                  plan_cells(notes = FALSE))
  expect_identical(sc_render(env$doc), sc_render(by_objects))
})

test_that("a report's watermark and a figure's size are report columns", {
  sp <- tfl_table_spec(report = data.frame(output_id = "F1", type = "figure",
                                           watermark = "DRAFT",
                                           figure_width_in = "9",
                                           figure_height_in = "5.5"))
  code <- paste(tfl_report_code(sp, content = "plots"), collapse = "\n")
  expect_match(code, "watermark = \"DRAFT\"", fixed = TRUE)
  expect_match(code, "rtf_figures(plots, width_twips = 12960, height_twips = 7920)",
               fixed = TRUE)
  # the document tfl_report() makes carries them too
  skip_if_not_installed("ggplot2")
  png <- tempfile(fileext = ".png")
  grDevices::png(png, width = 400, height = 300)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  grDevices::dev.off()
  on.exit(unlink(png), add = TRUE)
  doc <- tfl_report(sp, content = list(rtfplot(png)))
  expect_false(is.null(doc$document$watermark))
  expect_error(tfl_table_spec(report = data.frame(output_id = "F1",
                                                  figure_width_in = "wide")),
               "a number")
})
