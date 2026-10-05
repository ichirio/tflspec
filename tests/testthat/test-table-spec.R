# The table definition workbook: sheets, scope, and what it makes.
# (The ARD engine's own tests moved to rtfreporter with it.)
# Tests for the EXPERIMENTAL cards/cardx ARD helpers (issue #474).
# This whole file belongs to R/ard-experimental.R and is deleted with it.

skip_if_no_cards <- function() {
  testthat::skip_if_not_installed("cards")
}

make_ard <- function() {
  adsl <- cards::ADSL
  adsl$AGEGR <- as.character(cut(adsl$AGE, c(0, 64, 74, 200),
                                 labels = c("<65", "65-74", ">=75")))
  adsl$SEX <- as.character(adsl$SEX)
  adsl$TRT <- as.character(adsl$ARM)
  cards::ard_stack(
    adsl, .by = TRT,
    cards::ard_continuous(
      variables = AGE,
      statistic = ~ cards::continuous_summary_fns(
        c("N", "mean", "sd", "median", "min", "max"))),
    cards::ard_categorical(variables = c(AGEGR, SEX),
                           statistic = ~ c("n", "p")),
    .total_n = TRUE)
}

# ard_table() was withdrawn (#474): the one entry point is
# normalize_ard() |> widen_ard().  These tests were written against the
# collapsed call, so this helper does the split, routing each argument to the
# step that owns it -- which also keeps them honest about where each belongs.
ard_pipe <- function(ard, ...) {
  args <- list(...)
  keep <- intersect(names(args), setdiff(names(formals(normalize_ard)), "ard"))
  x <- do.call(normalize_ard, c(list(ard = ard), args[keep]))
  do.call(widen_ard, c(list(x = x), args[setdiff(names(args), keep)]))
}

# ------------------------------------------------------------ normalize_ard



# ------------------------------------- normalize_ard() |> widen_ard()



# --------------------------------------------- positional vs named key ---



# ------------------------------------------------------------- pull_ard ---



# ------------------------------------ statistics the table never prints ---



# ------------------------------------------------- the `rows` default ----



# ------------------------------------------------- the declarative sort ---



# ------------------------------------------------------ factor variables ---

make_factor_ard <- function() {
  adsl <- cards::ADSL
  adsl$TRT    <- as.character(adsl$ARM)
  adsl$AGEGR  <- cut(adsl$AGE, c(0, 64, 74, Inf),
                     labels = c("<65", "65-74", ">=75"))
  adsl$SEX    <- factor(adsl$SEX, levels = c("F", "M"),
                        labels = c("Female", "Male"))
  cards::ard_stack(
    adsl, .by = TRT,
    cards::ard_continuous(variables = AGE,
                          statistic = ~ cards::continuous_summary_fns("mean")),
    cards::ard_categorical(variables = c(AGEGR, SEX),
                           statistic = ~ c("n", "p")))
}



# ------------------------------------------------- stat versus stat_fmt ---



# ------------------------------------------------------- overall_row(from) ---

make_bound_ae <- function() {
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adae <- merge(cards::ADAE[, c("USUBJID", "AESOC")],
                adsl[, c("USUBJID", "TRT")], by = "USUBJID")
  adae <- adae[adae$AESOC %in% c("CARDIAC DISORDERS",
                                 "GASTROINTESTINAL DISORDERS"), ]
  # the treatment IS the analysed variable in the overall block
  overall <- cards::ard_tabulate(adsl[adsl$USUBJID %in% adae$USUBJID, ],
                                 variables = TRT, denominator = adsl,
                                 statistic = ~ c("n", "p"))
  soc <- cards::ard_tabulate(unique(adae[, c("USUBJID", "TRT", "AESOC")]),
                             by = TRT, variables = AESOC,
                             denominator = adsl,
                             statistic = ~ c("n", "N", "p"))
  cards::bind_ard(overall, soc)
}



# ------------------------------------------------------------ the notes ----



# ----------------------------------------------------------------- the spec

# A definition is read by tfl_table_plan(), which lives in the plan spike;
# these tests skip once that file is deleted.
spec_table <- function(d, spec, ...) {
  testthat::skip_if_not(exists("table_plan", mode = "function"),
                        "the plan spike is not here")
  plan_apply(tfl_table_plan(d, spec, ...) |>
               plan_cells(notes = FALSE), "table")
}

dm_spec <- function(output_id = NA) {
  tfl_table_spec(
    study = c(rounding = "sas"),
    tables = data.frame(output_id = output_id, cols = "TRT",
                        rows = "group = variable",
                        stringsAsFactors = FALSE),
    variables = data.frame(
      output_id = output_id,
      variable = c("AGE", "AGEGR", "SEX"),
      label    = c("Age (years)", "Age group", "Sex"),
      order    = 1:3,
      levels   = c(NA, "<65 | 65-74 | >=75", NA),
      stringsAsFactors = FALSE),
    cells = data.frame(
      output_id = output_id,
      variable = c("AGE", "AGE", "categorical"),
      row      = c("n", "Mean (SD)", NA),
      template = c("{N}", "{mean} ({sd})", "{n} ({p})"),
      digits   = c("0", "1,2", NA),
      stringsAsFactors = FALSE))
}


test_that("a three-sheet spec supplies the roles as well as the cells", {
  skip_if_no_cards()
  sp <- dm_spec()
  expect_s3_class(sp, "tfl_table_spec")
  expect_identical(names(sp), c("study", "tables", "variables", "codelists", "cells",
                                "layout", "columns", "style", "cell_styles",
                                "col_header", "report", "page", "header", "footer",
                                "titles", "footnotes", "tokens"))

  # no cols / rows in the call: the `tables` sheet says them
  tbl <- spec_table(normalize_ard(make_ard()), sp)
  expect_identical(as.character(unique(tbl$group)),
                   c("Age (years)", "Age group", "Sex"))
  age <- tbl[tbl$group == "Age (years)", ]
  expect_identical(as.character(age$label), c("n", "Mean (SD)"))
  # digits = "1,2" per token, with no inline spec in the template
  expect_match(age$Placebo[2], "^[0-9]+[.][0-9] [(][0-9]+[.][0-9]{2}[)]$")
  gr <- tbl[tbl$group == "Age group", ]
  expect_identical(as.character(gr$label), c("<65", "65-74", ">=75"))

  # the same table as the arguments written out
  ref <- widen_ard(normalize_ard(make_ard()), cols = "TRT",
                    rows = c(group = "variable"), rounding = "sas",
                    labels = c(AGE = "Age (years)", AGEGR = "Age group",
                               SEX = "Sex"),
                    levels = list(AGEGR = c("<65", "65-74", ">=75")),
                    cells = list(AGE = c("n" = "{N:.0f}",
                                         "Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
                                 categorical = "{n} ({p})"),
                    notes = FALSE)
  expect_equal(as.data.frame(tbl), as.data.frame(ref))
})

test_that("an argument given in the call wins over the spec", {
  skip_if_no_cards()
  d <- normalize_ard(make_ard())
  tbl <- spec_table(d, dm_spec(), rows = c(block = "variable"))
  expect_true("block" %in% names(tbl))
  expect_false("group" %in% names(tbl))
})

test_that("the workbook round-trips, and nothing but a workbook is one", {
  skip_if_no_cards()
  sp <- dm_spec("DM")
  expect_error(tfl_write_table_spec(sp, tempfile(fileext = ".csv")), ".xlsx workbook")
  expect_error(tfl_write_table_spec(sp, tempfile()), ".xlsx workbook")
  expect_error(tfl_read_table_spec("spec.csv"), ".xlsx workbook")

  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  tfl_write_table_spec(sp, f)
  expect_true(all(c("study", "about") %in% readxl::excel_sheets(f)))
  back <- tfl_read_table_spec(f, output_id = "DM")
  expect_identical(attr(back, "output_id"), "DM")
  expect_identical(tflspec:::.ard_spec_study_value(back, "rounding"), "sas")
  expect_equal(back$cells$template, sp$cells$template)
  expect_equal(back$variables$levels, sp$variables$levels)
  a <- spec_table(normalize_ard(make_ard()), f)
  b <- spec_table(normalize_ard(make_ard()), sp)
  expect_equal(a, b)
})

test_that("rows with the same key are one chain, and `when` guards one", {
  skip_if_no_cards()
  d <- normalize_ard(make_ard())
  d$stat[d$stat_name == "n" & d$variable == "SEX" & d$.label == "F"] <- 0
  sp <- tfl_table_spec(
    tables = data.frame(cols = "TRT", rows = "group = variable"),
    cells = data.frame(variable = c("SEX", "SEX"),
                       when     = c("n == 0", NA),
                       template = c("none", "{n} ({p})")))
  tbl <- spec_table(d, sp)
  sex <- tbl[tbl$group == "SEX", ]
  expect_identical(unname(unlist(sex[sex$label == "F", "Placebo"])), "none")
  expect_false(any(sex$Placebo[sex$label == "M"] == "none"))
  expect_error(tflspec:::.ard_spec_cells(tfl_table_spec(cells = data.frame(
    variable = "SEX", when = "n ==", template = "x"))), "not valid R")
})

test_that("quoted values in `rows` are constant headings; NA drops the label", {
  a <- tflspec:::.ard_spec_table_args(tfl_table_spec(tables = data.frame(
    cols = "BASEGR", rows = 'LBTOX_LBL | group1 = "Worst Post-Baseline"',
    label = "NA", sort = ".overall | group1 | -n")))
  expect_identical(a$cols, "BASEGR")
  expect_true(is.list(a$rows))
  expect_identical(names(a$rows), c("LBTOX_LBL", "group1"))  # its own name
  expect_identical(a$rows[[1L]], "LBTOX_LBL")
  expect_s3_class(a$rows[[2L]], "formula")
  expect_true(is.na(a$label))
  expect_identical(a$sort, c(".overall", "group1", "-n"))
  b <- tflspec:::.ard_spec_table_args(tfl_table_spec(tables = data.frame(
    cols = "TR01AG1 | SEROSTAT", label = "label = AEDECOD", sort = "false")))
  expect_identical(b$cols, c("TR01AG1", "SEROSTAT"))
  expect_identical(b$label, c(label = "AEDECOD"))
  expect_false(b$sort)
})

test_that("tfl_table_spec() refuses what it would otherwise quietly ignore", {
  expect_error(tfl_table_spec(tables = data.frame(cols = "TRT", colz = "x")),
               "does not read")
  # a column that belongs on another sheet says which
  expect_error(tfl_table_spec(tables = data.frame(cols = "TRT", levels = "a | b")),
               "a `variables` column")
  expect_error(tfl_table_spec(study = c(rounding = "banker")), "must be")
  # rounding is one per study: on `tables` it is refused, pointing at `study`
  expect_error(tfl_table_spec(tables = data.frame(cols = "TRT", rounding = "sas")),
               "`study` sheet")
  expect_error(tfl_table_spec(study = c(font = "Arial")), "does not read")
  expect_error(tfl_table_spec(study = data.frame(key = c("rounding", "rounding"),
                                           value = c("r", "sas"))), "twice")
  expect_error(tfl_table_spec(cells = data.frame(variable = "AGE", row = "n")),
               "no `template`")
  expect_error(tfl_table_spec(tables = data.frame(output_id = c("T1", "T1"),
                                            cols = "TRT")), "two rows")
  expect_error(tfl_table_spec(variables = data.frame(variable = c("AGE", "AGE"))),
               "two rows")
  # `note` is for people and always allowed
  expect_s3_class(tfl_table_spec(tables = data.frame(cols = "TRT", note = "hi")),
                  "tfl_table_spec")
  # the one-sheet layout names where its columns went
  expect_error(tfl_table_spec(data.frame(variable = "AGE", template = "{mean}")),
               "one-sheet layout")
})

test_that("`cols` has to come from somewhere", {
  skip_if_no_cards()
  expect_error(spec_table(normalize_ard(make_ard()),
                          tfl_table_spec(cells = data.frame(
                            variable = "AGE", template = "{mean}"))),
               "`cols` is required")
})

test_that("tfl_table_spec_template() scaffolds the three sheets", {
  skip_if_no_cards()
  sp <- tfl_table_spec_template(make_ard(), cols = "TRT", output_id = "DM")
  expect_s3_class(sp, "tfl_table_spec")
  expect_identical(sp$tables$cols, "TRT")
  expect_identical(sp$tables$output_id, "DM")
  expect_true(all(c("AGE", "AGEGR", "SEX") %in% sp$variables$variable))
  expect_false("TRT" %in% sp$variables$variable)   # a key, not a variable
  age <- sp$cells[sp$cells$variable == "AGE", ]
  expect_true(all(c("n", "Mean (SD)", "Min, Max") %in% age$row))
  lv <- sp$variables$levels[sp$variables$variable == "AGEGR"]
  expect_setequal(strsplit(lv, " | ", fixed = TRUE)[[1]], c("<65", "65-74", ">=75"))
  # and it runs as written
  tbl <- spec_table(normalize_ard(make_ard()), sp)
  expect_true(all(c("Placebo") %in% names(tbl)))
})


# --------------------------------------------- one shared spec, many reports


test_that("output_id: a report's own row replaces the default, per sheet", {
  sp <- tfl_table_spec(
    tables = data.frame(output_id = c(NA, "T14-3-1"),
                        cols = c("TRT", NA), na = c("-", "NE")),
    variables = data.frame(output_id = c(NA, "T14-3-1"),
                           variable = "AGE", label = c("Age", "Age PK")),
    cells = data.frame(output_id = c(NA, "T14-3-1", "T14-3-1"),
                       variable = "AGE", row = "Mean (SD)",
                       template = c("{mean} ({sd})", "{mean}", "{median}"),
                       digits = c("1,2", "3", "3")))
  a <- tflspec:::.ard_spec_scope(sp, "T14-1-1")
  b <- tflspec:::.ard_spec_scope(sp, "T14-3-1")
  expect_identical(a$tables$na, "-")
  expect_identical(b$tables$na, "NE")
  expect_identical(b$tables$cols, "TRT")         # blank: the default stays
  expect_identical(a$variables$label, "Age")
  expect_identical(b$variables$label, "Age PK")
  expect_identical(a$cells$digits, "1,2")
  # the report's own chain replaces the default one whole
  expect_identical(b$cells$template, c("{mean}", "{median}"))
})

test_that("several reports and no output_id is refused, naming them", {
  sp <- tfl_table_spec(tables = data.frame(output_id = c("DM", "AE"), cols = "TRT"))
  expect_error(tflspec:::.ard_spec_scope(sp), "defines 2 reports")
  expect_identical(attr(tflspec:::.ard_spec_scope(sp, "AE"), "output_id"),
                   "AE")
  one <- tfl_table_spec(tables = data.frame(output_id = "DM", cols = "TRT"))
  expect_identical(attr(tflspec:::.ard_spec_scope(one), "output_id"), "DM")
})

test_that("output_id against a spec that cannot honour it is an error", {
  only <- tfl_table_spec(tables = data.frame(output_id = "T1", cols = "TRT"))
  expect_error(tflspec:::.ard_spec_scope(only, "T9"), "nothing would apply")
  # a file of defaults serves any report, quietly
  defaults <- tfl_table_spec(tables = data.frame(cols = "TRT"))
  expect_silent(tflspec:::.ard_spec_scope(defaults, "T9"))
})

test_that("an unnamed report falls back to the defaults, and says so", {
  sp <- tfl_table_spec(cells = data.frame(output_id = c(NA, "T1"),
                                    variable = c("AGE", "SEX"),
                                    template = c("{mean}", "{n}")))
  expect_message(tflspec:::.ard_spec_scope(sp, "T9"),
                 "default rows are used")
  expect_identical(nrow(suppressMessages(
    tflspec:::.ard_spec_scope(sp, "T9"))$cells), 1L)
})

test_that("the workbook's sheets: reserved ones are reported, unknown refused", {
  mk <- function(...) tflspec:::.ard_spec_from_sheets(list(...), "x.xlsx")
  t <- data.frame(cols = "TRT")
  expect_message(mk(tables = t, figures = data.frame(fig = "F1")),
                 "reserved for a later version")
  expect_error(mk(tables = t, Sheet2 = data.frame(a = 1)), "nobody reads")
  expect_silent(mk(tables = t, `_notes` = data.frame(a = 1),
                   Sheet2 = data.frame()))
  # the study sheet is read, never "nobody reads"
  sp <- mk(tables = t, study = data.frame(key = "rounding", value = "sas"))
  expect_identical(tflspec:::.ard_spec_study_value(sp, "rounding"), "sas")
  expect_error(mk(tables = t, about = data.frame(key = "spec_version",
                                                 value = "99")),
               "spec_version 99")
  expect_error(mk(Sheet1 = data.frame(variable = "AGE", template = "{n}")),
               "one-sheet layout")
})


# -------------------------------------------- which template made each cell


test_that("the rounding family: argument > spec > option > R's own", {
  skip_if_no_cards()
  d <- normalize_ard(make_ard())
  d <- d[d$variable == "AGE" & d$stat_name == "mean", ]
  d$stat <- 0.25
  cell <- function(...) {
    widen_ard(d, cols = "TRT", rows = c(group = "variable"),
               cells = "{mean:.1f}", notes = FALSE, ...)$Placebo[1]
  }
  sp <- tfl_table_spec(study = c(rounding = "sas"),
                 cells = data.frame(variable = "AGE", template = "{mean:.1f}"))

  expect_identical(cell(), "0.2")                         # R's own, the default
  expect_identical(cell(rounding = "sas"), "0.3")         # the argument
  from_spec <- function(p) {
    plan_apply(p, "table")$Placebo[1]
  }
  if (exists("table_plan", mode = "function")) {
    p <- tfl_table_plan(d, sp, cols = "TRT", rows = c(group = "variable")) |>
           plan_cells(notes = FALSE)
    expect_identical(from_spec(p), "0.3")                        # the spec
    expect_identical(from_spec(p |> plan_digits(rounding = "r")), "0.2")
  }

  old <- options(rtfreporter.rounding = "sas")            # the package's one
  on.exit(options(old), add = TRUE)
  expect_identical(cell(), "0.3")                         # the option
  expect_identical(cell(rounding = "r"), "0.2")           # argument beats option
  options(old)
})

# ------------------------------------------- key order, key rows, .kind ----

make_fct_ard <- function() {
  adsl <- cards::ADSL
  adsl$TRT <- factor(as.character(adsl$ARM),
                     c("Xanomeline Low Dose", "Placebo",
                       "Xanomeline High Dose"))
  adsl$SEX <- as.character(adsl$SEX)
  cards::ard_stack(adsl, .by = TRT,
                   cards::ard_continuous(variables = AGE),
                   cards::ard_categorical(variables = SEX))
}
fct_cells <- list(continuous = "{mean:.1f}", categorical = "{n}")
fct_order <- c("Xanomeline Low Dose", "Placebo", "Xanomeline High Dose")


test_that("the example workbooks shipped with the package still read and run", {
  skip_if_not_installed("readxl")
  dir <- system.file("extdata", "ard-spec", package = "tflspec")
  skip_if(!nzchar(dir), "examples not installed")
  for (id in c("DM", "AE", "ORR", "LB", "PK")) {
    sp <- tfl_read_table_spec(file.path(dir, paste0(id, ".xlsx")))
    expect_identical(attr(tflspec:::.ard_spec_scope(sp), "output_id"), id)
    expect_s3_class(tfl_read_table_spec(file.path(dir, "study.xlsx"),
                                  output_id = id), "tfl_table_spec")
  }
  whole <- tfl_read_table_spec(file.path(dir, "study.xlsx"))     # the study, whole
  expect_setequal(unique(whole$tables$output_id), c("DM", "AE", "ORR", "LB", "PK"))
  expect_error(tflspec:::.ard_spec_scope(whole), "defines 5 reports")
})


# ------------------------------------------------ the report half

rep_spec <- function() tfl_table_spec(
  study = c(output_path = "out", program_dir = "C:\\tfl"),
  report = data.frame(output_id = c(NA, "T2"), page_footer = c(NA, "FALSE")),
  page = data.frame(output_id = "T2", orientation = "portrait",
                    margin_left_in = "0.5"),
  header = data.frame(output_id = c(NA, NA, "T1", "T1"), line = c(1, 2, 3, 4),
                      left = c("SPONSOR", "PROTOCOL", NA, NA),
                      center = c(NA, NA, NA, "Table 1"),
                      right = c(NA, "Page {PAGE} of {TOTAL_PAGES}", NA, NA)),
  footer = data.frame(output_id = c(NA, "T1"), line = c(99, 1),
                      left = c("{PROGRAM}  {DATETIME}", "A footnote.")),
  footnotes = data.frame(output_id = "T2", line = 1, left = "Below the table."))

render_lines <- function(doc) {
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}


test_that("tfl_report() is the document the same code would build", {
  old <- options(rtfreporter.render_time = as.POSIXct("2026-01-01 09:00"))
  on.exit(options(old), add = TRUE)
  pages <- as_rtftables(data.frame(A = "a", B = "b"))
  sp <- tflspec:::.ard_spec_scope(rep_spec(), "T1")
  by_spec <- tfl_report(sp, content = pages)
  by_code <- rtf_document(program = "C:\\tfl\\T1") |>
    rtf_section(secinfo = list(
      header = rtf_header(list(c("SPONSOR"), c("PROTOCOL", "Page {PAGE} of {TOTAL_PAGES}"),
                               c(""), c("Table 1"))),
      footer = rtf_footer(list(c(l = "A footnote."), c(l = "{PROGRAM}  {DATETIME}"))))) |>
    rtf_tables(pages)
  # the unnamed c("SPONSOR") is centred; the workbook said left
  by_code$sections[[1L]]$header$rows[[1L]] <- c(l = "SPONSOR")
  expect_identical(render_lines(by_spec), render_lines(by_code))
  # the default program `{output_id}` has no extension: rtfreporter adds .R
  expect_true(any(grepl("C:\\\\tfl\\\\T1.R  01Jan2026  09:00", render_lines(by_spec),
                        fixed = TRUE)))
  expect_identical(tfl_report_path(sp), file.path("out", "T1.rtf"))
})

test_that("a report can drop the running footer and use the page sheet", {
  pages <- as_rtftables(data.frame(A = "a"))
  sp <- suppressMessages(tflspec:::.ard_spec_scope(rep_spec(), "T2"))
  doc <- tfl_report(sp, content = pages)
  expect_null(doc$sections[[1L]]$footer)
  expect_identical(doc$document$page$orientation, "portrait")
  expect_identical(doc$document$page$margin_left_in, 0.5)
  expect_length(doc$footnotes, 1L)
})

test_that("a definition can be split over workbooks, and a sheet said once", {
  skip_if_not_installed("writexl"); skip_if_not_installed("readxl")
  a <- tempfile(fileext = ".xlsx"); b <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(a, b)), add = TRUE)
  tfl_write_table_spec(rep_spec(), a)
  tfl_write_table_spec(tfl_table_spec(study = c(rounding = "sas"),
                              tables = data.frame(output_id = "T1",
                                                  cols = "TRT")), b)
  sp <- tfl_read_report_spec(c(a, b), output_id = "T1")
  expect_identical(tflspec:::.ard_spec_study_value(sp, "rounding"), "sas")
  expect_identical(tflspec:::.ard_spec_study_value(sp, "output_path"), "out")
  expect_identical(sp$tables$cols, "TRT")
  expect_identical(nrow(sp$header), 4L)
  # the same sheet with rows in both is refused
  tfl_write_table_spec(tfl_table_spec(header = data.frame(line = 1, left = "x")), b)
  expect_error(tfl_read_report_spec(c(a, b)), "has rows in both")
})

test_that("the line is the key: a report's line replaces the default one", {
  sp <- tfl_table_spec(footer = data.frame(output_id = c(NA, NA, "T1"),
                                       line = c(1, 99, 99),
                                       left = c("house", "run", "mine")))
  t1 <- tflspec:::.ard_spec_scope(sp, "T1")
  expect_identical(t1$footer$left[order(as.numeric(t1$footer$line))],
                   c("house", "mine"))
  expect_error(tfl_table_spec(header = data.frame(line = c(1, 1), left = "x")),
               "two rows")
})

test_that("tfl_report() takes the output before the content", {
  skip_if_not_installed("rtfreporter")
  expect_error(tfl_report(tfl_table_spec(), list(1)),
               "tfl_report(spec, content = plan)", fixed = TRUE)
})

test_that("a report's line that says (none) takes the study's line of that number out", {
  ftr <- data.frame(output_id = c("", "", "T2"), line = c("1", "99", "99"),
                    left = c("Note", "{PROGRAM}", "(none)"),
                    center = "", right = "", stringsAsFactors = FALSE)
  band <- function(f, id) {
    sp <- tfl_table_spec(report = data.frame(output_id = c("T1", "T2"),
                                             type = "table"),
                         footer = f)
    .ard_spec_band(.ard_spec_scope(sp, id), "footer")
  }
  expect_identical(band(ftr, "T1"), list(c(l = "Note"), c(l = "{PROGRAM}")))
  expect_identical(band(ftr, "T2"), list(c(l = "Note")))
  # in any cell; a band left with no line is no band
  f2 <- ftr[2:3, ]
  f2$left[2] <- ""; f2$center[2] <- "(none)"
  expect_null(band(f2, "T2"))
  expect_identical(band(f2, "T1"), list(c(l = "{PROGRAM}")))
})
