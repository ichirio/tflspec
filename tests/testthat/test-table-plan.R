# The table definition as a plan: tfl_table_plan() / tfl_as_table_spec().
# (The plan engine's own tests moved to rtfreporter with it.)
# Tests for the DEFERRED, LAST-WINS plan spike (#474).
# This whole file belongs to R/ard-plan-spike.R and is deleted with it.

skip_if_no_cards2 <- function() testthat::skip_if_not_installed("cards")

# Flattening is RUN before the plan now, so that table_plan()'s roles
# name columns that exist.  This keeps the tests to one line.
nz <- function(x) {
  if (identical(rtfreporter:::.plan_source_kind(x), "ard"))
    suppressMessages(normalize_ard(x)) else x
}

plan_ard <- function() {
  adsl <- cards::ADSL
  adsl$SEX <- as.character(adsl$SEX)
  adsl$TRT <- as.character(adsl$ARM)
  cards::ard_stack(
    adsl, .by = TRT,
    cards::ard_continuous(
      variables = c(AGE, BMIBL),
      statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd"))),
    cards::ard_categorical(variables = SEX, statistic = ~ c("n", "p")),
    .total_n = TRUE)
}

base_plan <- function(ard = plan_ard()) {
  table_plan(nz(ard), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous  = c("n"         = "{N:d}",
                               "Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n:d} ({p:.1f%})")
}

# --------------------------------------------------------------- the rule



# ------------------------------------------------------------ the digits



# ----------------------------------------------------- nothing runs early



# ------------------------------------------- the same answer as the verbs



# ----------------------------------------------------------- the seam



# --------------------------------------------------------------- refusals



# ------------------------------------------------- what a plan starts from

# A long summary somebody built with dplyr: keys, a statistic name and a
# value, and nothing cards ever touched.
hand_long <- function(var_col = "PARAM") {
  d <- data.frame(
    TRT       = rep(c("A", "B"), each = 6),
    PARAM     = rep(rep(c("ALT", "AST"), each = 3), 2),
    stat_name = rep(c("n", "mean", "sd"), 4),
    stat      = c(20, 31.245, 4.1, 20, 28.7, 3.92,
                  18, 33.108, 5.3, 18, 30.2, 4.44),
    stringsAsFactors = FALSE)
  names(d)[names(d) == "PARAM"] <- var_col
  d
}


# --------------------------------------------------------- the display half

disp_plan <- function(ard = plan_ard()) {
  table_plan(nz(ard), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous  = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
               categorical = "{n:.0f} ({p:.1f%})", notes = FALSE)
}


test_that("plan_cell_style() by place is style_header() / style_cols(), declared", {
  skip_if_no_cards2()
  p <- base_plan() |>
    plan_col_header(rtf_col_header(c("", "", "{col}")))
  arms <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")
  by_hand <- as_rtftables(p) |>
    style_header(bold = TRUE, align = "center") |>
    style_cols(cols = arms, align = "center")
  # the value columns by name (`.values`), and by their own names
  for (cols in list(".values", arms)) {
    by_decl <- as_rtftables(p |>
      plan_cell_style(header = TRUE, bold = TRUE, align = "center") |>
      plan_cell_style(cols = cols, align = "center"))
    expect_identical(by_decl, by_hand)
  }
  # the workbook's cell_styles sheet says it
  sp <- suppressMessages(tfl_as_table_spec(
    plan_cell_style(p, header = TRUE, bold = TRUE)))
  expect_identical(sp$cell_styles$header, "TRUE")
  expect_identical(sp$cell_styles$bold, "TRUE")
  expect_true(attr(sp, "same_pages"))
})

test_that("the style sheet's border_* columns are plan_style()'s rules of a row", {
  skip_if_no_cards2()
  p <- base_plan()
  by_code <- plan_apply(plan_style(p, border_header = rtf_border(top = TRUE,
                                                                 bottom = TRUE),
                                   border_last_row = rtf_border(bottom = TRUE)),
                        "pages")
  sp <- suppressMessages(tfl_as_table_spec(
    plan_style(p, border_header = rtf_border(top = TRUE, bottom = TRUE),
               border_last_row = rtf_border(bottom = TRUE))))
  expect_identical(sp$style$border_header, "top | bottom")
  expect_identical(sp$style$border_last_row, "bottom")
  expect_true(isTRUE(attr(sp, "same_pages")))
  expect_error(tfl_table_spec(style = data.frame(border_body = "middle")),
               "top \\| bottom \\| left \\| right, or none")
})

# ------------------------------------------------- conditional cell styles

styled <- function(...) {
  suppressMessages(plan_apply(
    disp_plan() |>
      plan_stub(vars = c("group", "label"), name = "row_label",
                before = TRUE) |>
      plan_cell_style(...) |>
      plan_style(border = "tfl"),
    "pages"))
}


# --------------------------------------------------------- plan_template()



# ------------------------------------------------- the seams, as expressions



# ------------------------------------------------ titles, footnotes, listings

# a listing splits on max_rows alone, which keeps these tests about the
# blocks rather than about pagination strategies
.pages_src <- function(n = 12L) {
  data.frame(USUBJID = sprintf("S-%03d", seq_len(n)),
             ARM = rep(c("A", "B"), length.out = n),
             stringsAsFactors = FALSE)
}
.pages_plan <- function(max_rows = 4L) {
  table_plan(.pages_src()) |>
    plan_listing(listing_col("USUBJID", width = 12)) |>
    plan_paginate_rows(max_rows = max_rows)
}



# ------------------------------------------- the three ways in, one system



# ------------------------------- a frame that never went near cards

# Columns named the way a study names them: no `variable`, no
# `stat_name`, no `stat`, and the row label in two different places
# depending on what kind of row it is.
own_frame <- function() {
  data.frame(
    TRT   = rep(c("A", "B"), each = 5L),
    PARAM = rep(c("AGE", "AGE", "AGE", "SEX", "SEX"), 2L),
    CAT   = c(NA, NA, NA, "F", "M", NA, NA, NA, "F", "M"),
    STAT  = c("N", "mean", "sd", "n", "n", "N", "mean", "sd", "n", "n"),
    VALUE = c(10, 55.5, 4.25, 6, 4, 12, 57.25, 3.5, 7, 5),
    stringsAsFactors = FALSE)
}



# -------------------------------------------------------- the row order


# ------------------------------------ needed and not wanted, said once


# ------------------------------------ the header without a function


# ------------------------------------------ digits, per statistic

open_plan <- function() {
  table_plan(nz(plan_ard()), cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous  = c("n"         = "{N:.0f}",
                               "Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n:.0f} ({p:%})", notes = FALSE)
}


# --------------------------------- the denominator the ARD states

# the header numbers n = TRUE reads, without the bookkeeping attributes
nv <- function(p) {
  v <- rtfreporter:::.plan_n_values(p, TRUE)
  if (!length(v)) return(numeric(0))
  stats::setNames(as.numeric(v), names(v))
}


# ------------------------------------------ the header n, key rows, .kind --



# ------------------------------------------------ roles from a definition file


test_that("tfl_table_plan() takes the roles from the spec's tables sheet", {
  skip_if_no_cards2()
  sp <- tfl_table_spec(
    tables = data.frame(cols = "TRT", rows = "group = variable"),
    cells  = data.frame(variable = c("continuous", "categorical"),
                        row      = c("Mean (SD)", NA),
                        template = c("{mean} ({sd})", "{n} ({p})"),
                        digits   = c("1,2", "0")))
  d <- nz(plan_ard())
  p <- tfl_table_plan(d, sp) |>
         plan_cells(notes = FALSE)
  expect_identical(p$roles$cols, "TRT")
  expect_identical(p$roles$rows, c(group = "variable"))
  expect_equal(as.data.frame(plan_apply(p, "table")),
               as.data.frame(widen_ard(d, cols = "TRT",
                 rows = c(group = "variable"),
                 cells = list(continuous  = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
                              categorical = "{n:.0f} ({p:.0f})"),
                 notes = FALSE)))
  # a role in the call still wins, and is checked against the data
  p2 <- tfl_table_plan(d, sp, rows = c(block = "variable")) |>
          plan_cells(notes = FALSE)
  expect_identical(p2$roles$rows, c(block = "variable"))
  bad <- tfl_table_spec(tables = data.frame(cols = "NOPE"))
  expect_error(tfl_table_plan(d, bad), "no column 'NOPE'|NOPE")
})


# ------------------------------------------- the table half of a workbook

spec_pages_ard <- function() nz(plan_ard())


test_that("layout / columns / style give the pages the verbs give", {
  skip_if_no_cards2()
  d <- spec_pages_ard()
  sp <- tfl_table_spec(
    tables  = data.frame(cols = "TRT", rows = "group = variable"),
    cells   = data.frame(variable = c("continuous", "continuous", "categorical"),
                         row = c("n", "Mean (SD)", NA),
                         template = c("{N:d}", "{mean} ({sd})", "{n:d} ({p:.1f%})"),
                         digits = c(NA, "1,2", NA)),
    layout  = data.frame(stub_name = "row_label", stub_before = "TRUE",
                         blank_where = "between_groups", blank_first = "TRUE",
                         pages_max_rows = "6", pages_split = "group_safe"),
    columns = data.frame(column = c("row_label", ".values"),
                         rel_width = c("4", "2")),
    style   = data.frame(align_count_pct = "TRUE", row_height_twips = "220"))
  by_spec <- plan_apply(tfl_table_plan(d, sp) |>
                          plan_cells(notes = FALSE), "pages")
  by_code <- table_plan(d, cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(continuous  = c("n" = "{N:d}",
                               "Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
               categorical = "{n:d} ({p:.1f%})", notes = FALSE) |>
    plan_stub(name = "row_label", before = TRUE) |>
    plan_blanks(where = "between_groups", first = TRUE) |>
    plan_paginate_rows(max_rows = 6, split = "group_safe") |>
    plan_columns(widths = c(4, 2)) |>
    plan_style(align_count_pct = TRUE,
               row_height_twips = 220L) |>
    plan_apply("pages")
  expect_true(length(by_spec) > 1L)
  expect_equal(by_spec, by_code)
  # and the comparison is not vacuous: one changed setting shows
  sp2 <- sp
  sp2$style$row_height_twips <- "240"
  expect_false(isTRUE(all.equal(
    plan_apply(tfl_table_plan(d, sp2) |>
                 plan_cells(notes = FALSE), "pages"), by_code)))
})

test_that("blank rows at positions: whole numbers to plan_blanks(), and back", {
  d <- spec_pages_ard()
  lay <- function(where) tfl_table_spec(
    tables = data.frame(cols = "TRT", rows = "group = variable"),
    layout = data.frame(blank_where = where))
  sp <- lay("0 | -1")
  expect_identical(sp$layout$blank_where[[1L]], "0 | -1")
  by_spec <- plan_apply(tfl_table_plan(d, sp), "pages")
  by_code <- table_plan(d, cols = "TRT", rows = c(group = "variable")) |>
    plan_blanks(where = c(0L, -1L)) |>
    plan_apply("pages")
  expect_equal(by_spec, by_code)
  # written as a person writes them
  expect_true(any(grepl("plan_blanks(where = c(0, -1))",
                        tfl_table_code(sp, "T1"), fixed = TRUE)))
  # a plan's positions back to the sheet
  back <- suppressMessages(tfl_as_table_spec(
    table_plan(d, cols = "TRT", rows = c(group = "variable")) |>
      plan_blanks(where = c(0L, -1L)), "T1"))
  expect_identical(back$layout$blank_where[[1L]], "0 | -1")
  # neither a rule nor positions
  expect_error(lay("between_groups | 2"), "row positions")
})

test_that("a verb written after tfl_table_plan() still wins", {
  skip_if_no_cards2()
  d <- spec_pages_ard()
  sp <- tfl_table_spec(tables = data.frame(cols = "TRT", rows = "group = variable"),
                 layout = data.frame(pages_max_rows = "6",
                                     pages_split = "group_safe"))
  p <- tfl_table_plan(d, sp) |>
    plan_cells(notes = FALSE) |>
    plan_paginate_rows(max_rows = 40)
  expect_s3_class(plan_apply(p, "pages")[[1L]], "rtftable")
  expect_length(plan_apply(p, "pages"), 1L)
})

test_that("`.values` widths follow the data; a column left out is named", {
  skip_if_no_cards2()
  d <- spec_pages_ard()
  base <- function(columns) tfl_table_spec(
    tables = data.frame(cols = "TRT", rows = "group = variable"),
    layout = data.frame(stub_name = "row_label", stub_before = "TRUE"),
    columns = columns)
  pg <- plan_apply(tfl_table_plan(d, base(data.frame(
    column = c("row_label", ".values"), rel_width = c("5", "2")))) |>
      plan_cells(notes = FALSE),
    "pages")
  first <- if (inherits(pg, "rtftable")) pg else pg[[1L]]
  expect_identical(first$col_rel_width,
                   c(5, rep(2, ncol(first$data) - 1L)))
  expect_error(plan_apply(tfl_table_plan(d, base(data.frame(
    column = "row_label", rel_width = "5"))) |>
      plan_cells(notes = FALSE), "pages"),
    "not for")
})

test_that("group_collapse alone does not become the grouping column", {
  # `lay$group_col` would partially match `group_collapse`
  skip_if_no_cards2()
  sp <- tfl_table_spec(tables = data.frame(cols = "TRT", rows = "group = variable"),
                 layout = data.frame(group_collapse = "1"))
  p <- tfl_table_plan(spec_pages_ard(), sp) |>
         plan_cells(notes = FALSE)
  g <- rtfreporter:::.plan_merge(rtfreporter:::.plan_of(p, "group"))
  expect_null(g$group_col)
  expect_identical(g$collapse_repeats, 1L)
})

test_that("stats = rows formats come from `cells` rows with no template", {
  skip_if_no_cards2()
  d <- spec_pages_ard()
  d <- d[d$variable == "AGE", , drop = FALSE]
  sp <- tfl_table_spec(
    tables = data.frame(cols = "TRT", rows = "Analyte = variable",
                        label = "Statistics = stat_label", stats = "rows"),
    cells = data.frame(row = c("N", "Mean", "SD"), digits = c("0", NA, NA),
                       signif = c(NA, "4", "5")))
  by_spec <- plan_apply(tfl_table_plan(d, sp) |>
                          plan_cells(notes = FALSE), "pages")
  by_code <- table_plan(d, cols = "TRT", rows = c(Analyte = "variable"),
                      label = c(Statistics = "stat_label")) |>
                        plan_cells(stats = "rows", notes = FALSE) |>
    plan_digits(.rows = c(N = "0", Mean = "4s", SD = "5s")) |>
    plan_apply("pages")
  expect_equal(by_spec, by_code)
  # the same rows on a stats = cells table are a mistake, and said to be
  bad <- sp
  bad$tables$stats <- NA
  expect_error(tfl_table_plan(nz(plan_ard()), bad),
               "not `stats = rows`")
})

test_that("display values are checked where they are written", {
  expect_error(tfl_table_spec(layout = data.frame(pages_max_rows = "twenty")),
               "`layout\\$pages_max_rows` must be a whole number")
  expect_error(tfl_table_spec(style = data.frame(align_count_pct = "maybe")),
               "TRUE or FALSE")
  expect_error(tfl_table_spec(columns = data.frame(rel_width = "2")),
               "needs a `column`")
  expect_error(tfl_table_spec(columns = data.frame(column = c("a", "a"))),
               "two rows")
  expect_error(tfl_table_spec(layout = data.frame(output_id = c("T1", "T1"),
                                            pages_max_rows = "5")),
               "two rows")
  lay <- tflspec:::.ard_spec_typed(
    tfl_table_spec(layout = data.frame(pages_cont_label = '" (Cont.)"',
                                 colpages_keep = "1 | 2",
                                 stub_vars = "a | b"))$layout, "layout")
  expect_identical(lay$pages_cont_label, " (Cont.)")   # quotes keep spaces
  expect_identical(lay$colpages_keep, 1:2)
  expect_identical(lay$stub_vars, c("a", "b"))
})


# ------------------------------------------------ the col_header sheet

hdr_spec <- function(col_header, ...) tfl_table_spec(
  tables = data.frame(cols = "TRT", rows = "group = variable"),
  layout = data.frame(stub_name = "row_label", stub_before = "TRUE"),
  col_header = col_header, ...)
first_page <- function(pg) if (inherits(pg, "rtftable")) pg else pg[[1L]]


test_that("a col_header sheet gives the header rtf_col_header() gives", {
  skip_if_no_cards2()
  d <- spec_pages_ard()
  sp <- hdr_spec(data.frame(
    line = c(1, 1, 2, 2),
    cols = c("row_label", ".values", "row_label", ".values"),
    span = c(NA, "each", NA, "each"),
    text = c(NA, "{col}", "Characteristic", "(N={n})")))
  by_spec <- plan_apply(tfl_table_plan(d, sp) |>
                          plan_cells(notes = FALSE), "pages")
  by_code <- table_plan(d, cols = "TRT", rows = c(group = "variable")) |>
               plan_cells(notes = FALSE) |>
    plan_stub(name = "row_label", before = TRUE) |>
    plan_col_header(values = list(n = TRUE), rtf_col_header(
      c("", "{col}"), c("Characteristic", "(N={n})"))) |>
    plan_apply("pages")
  expect_equal(by_spec, by_code)
  # {n} was read from the ARD without being asked for
  expect_match(first_page(by_spec)$col_header[[2L]][2L], "^\\(N=[0-9]+\\)$")
})

test_that("span = a key makes one spanner per value; KEY = value selects", {
  skip_if_no_cards2()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$GRP <- ifelse(adsl$AGE < 70, "Young", "Old")
  d <- normalize_ard(cards::ard_stack(
    adsl, .by = c(TRT, GRP),
    cards::ard_categorical(variables = SEX, statistic = ~ c("n", "p"))))
  sp <- tfl_table_spec(
    tables = data.frame(cols = "TRT | GRP", rows = "group = variable"),
    layout = data.frame(stub_name = "row_label", stub_before = "TRUE"),
    col_header = data.frame(
      line = c(1, 1, 2, 2, 2),
      cols = c("row_label", ".values", "row_label", "GRP = Young", "GRP = Old"),
      span = c(NA, "TRT", NA, "each", "each"),
      text = c(NA, "{col1}", "Sex", "<70", ">=70"),
      border_bottom = c(NA, "single", NA, NA, NA)))
  h <- first_page(plan_apply(tfl_table_plan(d, sp) |>
                               plan_cells(notes = FALSE),
                             "pages"))$col_header
  top <- h[[1L]]
  spanners <- Filter(function(cc) cc$to > cc$from, top)
  expect_length(spanners, 3L)                          # one per arm
  expect_setequal(vapply(spanners, `[[`, "", "label"),
                  unique(adsl$TRT))
  expect_true(all(vapply(spanners, function(cc) !is.null(cc$border), NA)))
  lab <- h[[2L]]
  if (!is.character(lab)) lab <- vapply(lab, `[[`, "", "label")
  expect_identical(lab[1L], "Sex")
  expect_setequal(unique(lab[-1L]), c("<70", ">=70"))
})

test_that("a report's own header replaces the default header whole", {
  sp <- tfl_table_spec(col_header = data.frame(
    output_id = c(NA, NA, "T1"), line = c(1, 2, 1),
    cols = ".values", text = c("a", "b", "mine")))
  t1 <- tflspec:::.ard_spec_scope(sp, "T1")
  expect_identical(t1$col_header$text, "mine")
  t2 <- suppressMessages(tflspec:::.ard_spec_scope(sp, "T2"))
  expect_identical(t2$col_header$text, c("a", "b"))
})

test_that("col_header refuses what it cannot place", {
  skip_if_no_cards2()
  d <- spec_pages_ard()
  expect_error(tfl_table_spec(col_header = data.frame(line = 1, text = "x")),
               "needs a `line` and `cols`")
  bad <- function(...) plan_apply(tfl_table_plan(d, hdr_spec(
    data.frame(line = 1, ...))) |>
      plan_cells(notes = FALSE), "pages")
  expect_error(bad(cols = "NOPE", text = "x"), "no column 'NOPE'")
  expect_error(bad(cols = ".values", span = "ARMX", text = "x"),
               "not a column key")
  expect_error(bad(cols = "1:99", text = "x"), "outside")
  # a typed \n is a line break; quotes keep leading spaces
  v <- tflspec:::.ard_spec_typed(tfl_table_spec(col_header = data.frame(
    line = 1, cols = "a", text = '"  a\\nb"'))$col_header, "col_header")
  expect_identical(v$text, "  a\nb")
})


# ------------------------------------------------ plan -> workbook

code_plan <- function(d = spec_pages_ard()) {
  table_plan(d, cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(notes = FALSE) |>
    plan_labels(AGE = "Age (years)", SEX = "Sex") |>
    plan_cells(continuous  = c("n" = "{N:d}", "Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n:d} ({p:.1f%})") |>
    plan_digits(1, rounding = "sas") |>
    plan_stub(name = "row_label", before = TRUE) |>
    plan_blanks(where = "between_groups", first = TRUE) |>
    plan_paginate_rows(max_rows = 6, split = "group_safe") |>
    plan_columns(widths = c(4, 2)) |>
    plan_style(align_count_pct = TRUE) |>
    plan_col_header(values = list(n = TRUE), rtf_col_header(c("", "{col}"),
                                             c("Characteristic", "(N={n})")))
}


test_that("tfl_as_table_spec() writes a plan as a workbook that gives its pages", {
  skip_if_no_cards2()
  p <- code_plan()
  sp <- tfl_as_table_spec(p, output_id = "T1")
  expect_s3_class(sp, "tfl_table_spec")
  expect_true(attr(sp, "same_pages"))
  expect_length(attr(sp, "not_converted"), 0L)
  expect_identical(sp$tables$cols, "TRT")
  expect_identical(tflspec:::.ard_spec_study_value(sp, "rounding"), "sas")
  expect_identical(sp$layout$pages_max_rows, "6")
  # widths by name, the value columns as one `.values`
  expect_identical(sp$columns$column, c("row_label", ".values"))
  # the header came back as tokens and spans, not as this study's numbers
  expect_true(any(sp$col_header$text %in% "(N={n})"))
  expect_true(all(sp$col_header$span[sp$col_header$cols == ".values"] == "each"))

  skip_if_not_installed("writexl"); skip_if_not_installed("readxl")
  f <- tempfile(fileext = ".xlsx"); on.exit(unlink(f), add = TRUE)
  tfl_write_table_spec(sp, f)
  back <- plan_apply(
    tfl_table_plan(p$data, tfl_read_table_spec(f, output_id = "T1")) |>
      plan_cells(notes = FALSE), "pages")
  expect_equal(back, plan_apply(p, "pages"))
})

test_that("what a workbook cannot say is listed, and the check says so", {
  skip_if_no_cards2()
  p <- code_plan() |> plan_after(function(x) x)
  expect_message(sp <- tfl_as_table_spec(p), "plan_after\\(\\) step stays in code")
  expect_true(any(grepl("plan_after", attr(sp, "not_converted"))))
  expect_true(attr(sp, "same_pages"))      # an identity step changed nothing
  p2 <- code_plan() |> plan_after(function(x) { x[[1L]]$data[1, 1] <- "X"; x })
  sp2 <- suppressMessages(tfl_as_table_spec(p2))
  expect_false(attr(sp2, "same_pages"))
})

test_that("a named list of plans is one study workbook", {
  skip_if_no_cards2()
  sp <- tfl_as_table_spec(list(DM = code_plan(), DM2 = code_plan()))
  expect_setequal(unique(sp$tables$output_id), c("DM", "DM2"))
  expect_identical(unname(attr(sp, "same_pages")), c(TRUE, TRUE))
  expect_error(tfl_as_table_spec(list(code_plan(), code_plan())), "unique names")
})

test_that("a one-arm spanner comes back as one spanner per arm", {
  skip_if_no_cards2()
  adsl <- cards::ADSL
  adsl$TRT <- "ONLY"
  adsl$GRP <- ifelse(adsl$AGE < 70, "Young", "Old")
  d <- normalize_ard(cards::ard_stack(
    adsl, .by = c(TRT, GRP),
    cards::ard_categorical(variables = SEX, statistic = ~ c("n", "p"))))
  p <- table_plan(d, cols = c("TRT", "GRP"), rows = c(group = "variable")) |>
         plan_cells(notes = FALSE) |>
    plan_stub(name = "row_label", before = TRUE) |>
    plan_col_header(rtf_col_header(
      list(col_cell(1, ""), col_cell(c(2, 3), "ONLY")),
      c("Sex", "{col2}")))
  sp <- tfl_as_table_spec(p)
  expect_true(attr(sp, "same_pages"))
  top <- sp$col_header[sp$col_header$line == "1" & sp$col_header$cols == ".values", ]
  expect_identical(top$text, "{col1}")
  expect_identical(top$span, "TRT")
})

# ------------------------------- the header N, level by level (#482) --

hdr_rows <- function(out) {
  first <- if (inherits(out, "rtftable")) out else out[[1L]]
  lapply(first$col_header, function(r) if (is.character(r)) r else
    vapply(r, function(z) as.character(z$label), ""))
}

adsl_trt <- function() {
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$SEX <- as.character(adsl$SEX)
  adsl$AGEGRP <- ifelse(adsl$AGE < 70, "<70", ">=70")
  adsl
}


test_that("numbers given by the caller may be keyed at any depth", {
  skip_if_not_installed("cards")
  adsl <- adsl_trt()
  adsl$AGE[1:20] <- NA
  d <- normalize_ard(cards::ard_continuous(adsl, by = c(TRT, SEX),
                                           variables = AGE))
  p <- table_plan(d, cols = c("TRT", "SEX"), rows = c(group = "variable")) |>
    plan_cells(continuous = "{mean:.1f}", notes = FALSE)
  arm <- c(table(adsl$TRT))
  cell <- table(adsl$TRT, adsl$SEX)
  cell <- stats::setNames(as.vector(cell), paste(
    rep(rownames(cell), 2L), rep(colnames(cell), each = 3L), sep = "____"))
  hdr <- rtf_col_header(c("", "", "{col1} N={n1}"),
                        c("", "", "{col2} N={n}"))
  # from the ARD: NA and a warning, not AGE's non-missing count
  expect_warning(out <- suppressMessages(plan_apply(
    plan_col_header(p, values = list(n = TRUE), hdr), "pages")), "only AGE")
  expect_identical(hdr_rows(out)[[2L]][3L], "F N=NA")
  for (n in list(c(arm, cell), function(data) c(arm, cell))) {
    out <- suppressMessages(plan_apply(
      plan_col_header(p, hdr, values = list(n = n)), "pages"))
    expect_identical(hdr_rows(out)[[1L]][3L], "Placebo N=86")
    expect_identical(hdr_rows(out)[[2L]][3:4], c("F N=53", "M N=33"))
  }
  # a workbook's header, its numbers from code
  sp <- tfl_table_spec(
    tables = data.frame(cols = "TRT | SEX", rows = "group = variable"),
    cells = data.frame(variable = "AGE", template = "{mean:.1f}"),
    col_header = data.frame(line = c(1, 2), cols = ".values",
                            span = c("TRT", "each"),
                            text = c("{col1} (N={n})", "{col2} (N={n})")))
  expect_warning(suppressMessages(plan_apply(
    tfl_table_plan(d, sp) |>
      plan_cells(notes = FALSE), "pages")), "printed as NA")
  out <- suppressMessages(plan_apply(
    tfl_table_plan(d, sp) |>
      plan_cells(notes = FALSE) |>
      plan_col_header(values = list(n = c(arm, cell))), "pages"))
  h <- hdr_rows(out)
  expect_true("Placebo (N=86)" %in% h[[1L]])
  expect_identical(h[[2L]][3:4], c("F (N=53)", "M (N=33)"))
})


# the analysis set and the subjects with the test: both in the ARD
two_pop <- function() {
  set.seed(1)
  lb <- expand.grid(USUBJID = cards::ADSL$USUBJID,
                    PARAM = c("ALT", "HGB"), stringsAsFactors = FALSE)
  lb$BASEGR <- sample(c("G0", "G1"), nrow(lb), TRUE)
  lb$WORSTGR <- sample(c("G0", "G1", "G2"), nrow(lb), TRUE)
  lb <- lb[!(lb$PARAM == "HGB" & seq_len(nrow(lb)) %% 10 == 0), ]
  d <- normalize_ard(cards::bind_ard(
    cards::ard_categorical(lb, by = c(PARAM, BASEGR), variables = WORSTGR),
    cards::ard_categorical(lb, by = PARAM, variables = BASEGR),
    cards::ard_total_n(cards::ADSL)), drop_contexts = "attributes")
  list(d = d, tested = table(lb$PARAM), set = nrow(cards::ADSL))
}
two_pop_plan <- function(d, n, text = "T (N={n})") {
  table_plan(d, cols = "BASEGR", rows = c(PARAM = "PARAM"),
           label = c(label = ".label")) |>
    plan_cells("{n}", notes = FALSE) |>
    plan_paginate_group(keep = FALSE) |>
    plan_col_header(values = if (is.list(n)) n else list(n = n),
                    rtf_col_header(
      list(col_cell(1, ""), col_cell(c(2, 3), text)),
      c("", "{col} (n={n})")))
}
spanner <- function(pg) vapply(pg, function(p)
  p$col_header[[1L]][[2L]]$label, "")


test_that("both populations in one header, from code and from a workbook", {
  skip_if_not_installed("cards")
  x <- two_pop()
  want <- sprintf("T (N=%d), tested n=%d", x$set,
                  as.integer(x$tested[c("ALT", "HGB")]))
  pg <- expect_silent(suppressMessages(plan_apply(two_pop_plan(
    x$d, list(n = "page", N = "table"), "T (N={N}), tested n={n}"),
    "pages")))
  expect_identical(unname(spanner(pg)), want)
  sp <- tfl_table_spec(
    tables = data.frame(cols = "BASEGR", rows = "PARAM = PARAM",
                        label = "label = .label",
                        header_n = "n = page | N = table"),
    cells = data.frame(template = "{n}"),
    layout = data.frame(group_page = "TRUE", group_keep = "FALSE"),
    col_header = data.frame(
      line = c(1, 2, 2), cols = c(".values", "1", ".values"),
      span = c(NA, NA, "each"),
      text = c("T (N={N}), tested n={n}", NA, "{col} (n={n})")))
  pg <- expect_silent(suppressMessages(plan_apply(
    tfl_table_plan(x$d, sp) |>
      plan_cells(notes = FALSE), "pages")))
  lab <- vapply(pg, function(p) {
    l <- vapply(p$col_header[[1L]], `[[`, "", "label")
    l[nzchar(l)][1L]
  }, "")
  expect_identical(unname(lab), want)
  expect_error(tfl_table_spec(tables = data.frame(
    cols = "BASEGR", header_n = "tested")) |>
      tflspec:::.ard_spec_table_args(), "not a population")
  expect_error(rtfreporter:::.plan_n_values(two_pop_plan(x$d, "x"), "x"),
               "a population is")
})

test_that("tfl_as_table_spec() writes the population to tables$header_n", {
  expect_identical(rtfreporter:::.plan_scope_text("table"), "table")
  expect_identical(rtfreporter:::.plan_scope_text(
    list(n = "page", N = "table")), "n = page | N = table")
  expect_null(rtfreporter:::.plan_scope_text(TRUE))
  expect_null(rtfreporter:::.plan_scope_text(c(A = 86)))
  expect_identical(tflspec:::.ard_spec_header_n("n = page | N = table"),
                   list(n = "page", N = "table"))
})

test_that("a plan's titles and footnotes go to the titles / footnotes sheets", {
  skip_if_no_cards2()
  p <- code_plan() |>
    plan_titles("Table 14.1.1", "Demographics") |>
    plan_footnotes("N: subjects in the population.")
  sp <- tfl_as_table_spec(p, output_id = "T1")
  expect_true(attr(sp, "same_pages"))
  expect_length(attr(sp, "not_converted"), 0L)
  expect_identical(sp$titles$center, c("Table 14.1.1", "Demographics"))
  expect_equal(as.integer(sp$titles$line), c(1L, 2L))
  expect_identical(sp$footnotes$left, "N: subjects in the population.")
  # titles that differ by page cannot be one sheet: said, not dropped
  p2 <- code_plan() |> plan_paginate_rows(max_rows = 100) |>
    plan_titles(pages = list(c("Table 1", "Part A")))
  sp2 <- suppressMessages(tfl_as_table_spec(p2, output_id = "T1"))
  expect_true(any(grepl("differ by page", attr(sp2, "not_converted"))))
})

test_that("cell styles are a sheet, both ways, and give the same pages", {
  skip_if_no_cards2()
  p <- code_plan() |>
    plan_cell_style(cols = "row_label", bold = TRUE) |>
    plan_cell_style(header = TRUE, italic = TRUE) |>
    plan_cell_style(where = ~ row_label == "Sex", background = "#EEEEEE",
                    bold = TRUE)
  # `plan_cell_style(header = TRUE)` on a header of several label rows makes
  # rtfreporter say that the look is shared by all of them; that is its
  # message, not what this test is about
  sp <- suppressWarnings(tfl_as_table_spec(p, output_id = "T1"))
  expect_true(attr(sp, "same_pages"))
  expect_length(attr(sp, "not_converted"), 0L)
  cs <- sp$cell_styles
  expect_identical(nrow(cs), 4L)
  expect_identical(cs$where[!is.na(cs$where)], rep("row_label == \"Sex\"", 2L))
  expect_identical(cs$header[!is.na(cs$header)], "TRUE")
  # the code writes one plan_cell_style() a row, `where` as a formula
  code <- paste(tfl_table_code(sp), collapse = "\n")
  expect_match(code, "plan_cell_style(where = ~ row_label == \"Sex\", bold = TRUE)",
               fixed = TRUE)
  # through the workbook
  skip_if_not_installed("writexl"); skip_if_not_installed("readxl")
  f <- tempfile(fileext = ".xlsx"); on.exit(unlink(f), add = TRUE)
  tfl_write_table_spec(sp, f)
  back <- suppressWarnings(plan_apply(
    tfl_table_plan(p$data, tfl_read_table_spec(f, output_id = "T1")) |>
      plan_cells(notes = FALSE), "pages"))
  expect_equal(back, suppressWarnings(plan_apply(p, "pages")))
})

test_that("a look computed row by row is said, not dropped", {
  skip_if_no_cards2()
  p <- code_plan() |> plan_cell_style(bold = ~ row_label == "Sex")
  sp <- suppressMessages(tfl_as_table_spec(p, output_id = "T1"))
  expect_true(any(grepl("computed row by row", attr(sp, "not_converted"))))
})

test_that("the cell_styles sheet is checked", {
  row <- function(...) data.frame(output_id = NA, ..., stringsAsFactors = FALSE)
  expect_error(tfl_table_spec(cell_styles = row(cols = "a")), "styles nothing")
  expect_error(tfl_table_spec(cell_styles = row(where = "a ==", bold = "TRUE")),
               "not an R condition")
  expect_error(tfl_table_spec(cell_styles = row(header = "TRUE",
                                                where = "a == 1",
                                                bold = "TRUE")),
               "no rows for `where`")
  expect_error(tfl_table_spec(cell_styles = rbind(
    row(where = "a == 1", bold = "TRUE"), row(where = "b == 1", bold = "TRUE"))),
    "one conditional rule per look")
})

test_that("break_before, cut_by, fit and allow_span_break are layout keys", {
  skip_if_no_cards2()
  p <- table_plan(spec_pages_ard(), cols = "TRT",
                  rows = c(group = "variable")) |>
    plan_cells(notes = FALSE) |>
    plan_cells(continuous  = c("n" = "{N:d}", "Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n:d} ({p:.1f%})") |>
    plan_paginate_rows(split = "rows", break_before = 4L) |>
    plan_paginate_cols(every = 2, fit = FALSE, allow_span_break = FALSE)
  sp <- tfl_as_table_spec(p, output_id = "T1")
  expect_true(attr(sp, "same_pages"))
  expect_length(attr(sp, "not_converted"), 0L)
  expect_identical(sp$layout$pages_break_before, "4")
  expect_identical(sp$layout$colpages_fit, "FALSE")
  expect_identical(sp$layout$colpages_allow_span_break, "FALSE")
  code <- paste(tfl_table_code(sp), collapse = "\n")
  expect_match(code, "break_before = 4L", fixed = TRUE)
  # a separator in the column names
  p2 <- code_plan() |> plan_paginate_cols(cut_by = "____")
  sp2 <- suppressMessages(tfl_as_table_spec(p2, output_id = "T1",
                                            compare = FALSE))
  expect_identical(sp2$layout$colpages_cut_by, "____")
})

test_that("a style that is not one value is said, not written twice", {
  skip_if_no_cards2()
  p <- code_plan() |> plan_style(border = rtfreporter::rtf_border(top = TRUE))
  sp <- suppressMessages(tfl_as_table_spec(p, output_id = "T1"))
  expect_lte(nrow(sp$style), 1L)
  expect_true(any(grepl("plan_style(border", attr(sp, "not_converted"),
                        fixed = TRUE)))
})

test_that("cell styles convert in a plan with no plan_style()", {
  skip_if_no_cards2()
  p <- base_plan() |> plan_stub(name = "row_label", before = TRUE) |>
    plan_cell_style(where = ~ row_label == "Sex", bold = TRUE)
  sp <- tfl_as_table_spec(p, output_id = "T1")
  expect_length(attr(sp, "not_converted"), 0L)
  expect_true(attr(sp, "same_pages"))
  expect_identical(nrow(sp$style), 0L)
})

test_that("the codelists sheet: the values' place, as the ARD has them (plan_levels)", {
  skip_if_not_installed("cards")
  skip_if(utils::packageVersion("rtfreporter") < "0.8.2.9004")
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  # the ARD program puts the code lists' labels on the data (set_levels())
  adsl$SEX <- c(F = "Female", M = "Male")[as.character(adsl$SEX)]
  adsl$AGEGR1 <- ifelse(adsl$AGEGR1 == "<65", "Under 65", as.character(adsl$AGEGR1))
  ard <- cards::ard_stack(adsl, .by = TRT,
    cards::ard_categorical(variables = c(SEX, AGEGR1), statistic = ~ c("n", "p")))
  d <- suppressMessages(rtfreporter::normalize_ard(ard))
  sp <- tfl_table_spec(list(
    tables = data.frame(output_id = "T1", cols = "TRT", rows = "group = variable"),
    variables = data.frame(output_id = NA, variable = c("SEX", "AGEGR1"),
                           label = c("Sex", "Age group"), order = c("1", "2"),
                           levels = c(NA, "Under 65 | 65-80 | >80")),
    codelists = data.frame(output_id = "T1", variable = c("SEX", "SEX", "AGEGR1"),
                           value = c("F", "M", "<65"),
                           label = c("Female", "Male", "Under 65"),
                           order = c("2", "1", NA)),
    cells = data.frame(output_id = NA, template = "{n:d} ({p:.1f%})")))
  page <- function(p) {
    x <- suppressMessages(rtfreporter::plan_apply(p))
    if (is.data.frame(x)) x else if (inherits(x, "rtftable")) x$data else x[[1L]]$data
  }
  x <- page(suppressMessages(tfl_table_plan(d, sp, output_id = "T1")))
  # the code list's order, of the ARD's values (its labels); the variables
  # sheet's levels win
  expect_identical(as.character(x$label),
                   c("Male", "Female", "Under 65", "65-80", ">80"))
  expect_identical(unique(as.character(x$group)), c("Sex", "Age group"))
  code <- tfl_table_code(sp, output_id = "T1")
  expect_true(any(grepl('SEX = c("Male", "Female")', code, fixed = TRUE)))
  # plan_labels(): the variables' headings only -- the values' text is the ARD's
  expect_true(any(grepl('plan_labels(SEX = "Sex", AGEGR1 = "Age group")', code, fixed = TRUE)))
  e <- new.env()
  e$data <- d
  suppressMessages(eval(parse(text = code), envir = e))
  expect_identical(page(e$plan), x)
  # one value said twice for a report is refused
  bad <- sp
  bad$codelists <- rbind(bad$codelists, bad$codelists[1L, ])
  expect_error(tfl_table_spec(unclass(bad)), "two rows for 'SEX / F'")
  # a code list is a report's: a row without one is refused
  bad <- sp
  bad$codelists$output_id[3L] <- NA
  expect_error(tfl_table_spec(unclass(bad)), "Row 3 of the code lists (AGEGR1 / <65)",
               fixed = TRUE)
})

test_that("the code list of `variable` (an earlier form) moves to the variables' labels", {
  old <- list(
    tables = data.frame(output_id = "T1", cols = "TRT", rows = "group = variable"),
    variables = data.frame(output_id = "T1", variable = "AGEGR1",
                           label = "Age group", order = "2"),
    codelists = data.frame(output_id = "T1",
                           variable = c("variable", "variable", "SEX", "SEX"),
                           value = c("SEX", "AGEGR1", "F", "M"),
                           label = c("Sex (code list)", "not this", "Female", "Male"),
                           order = c("2", "1", "1", "2")),
    cells = data.frame(output_id = NA, template = "{n:d} ({p:.1f%})"))
  expect_message(tfl_table_spec(old), "variables sheet's labels now")
  sp <- suppressMessages(tfl_table_spec(old))
  # SEX's heading moved to the variables sheet (a row of its own); AGEGR1's
  # own label wins; the code lists keep the values only
  v <- sp$variables
  expect_identical(v$label[v$variable == "SEX"], "Sex (code list)")
  expect_identical(v$label[v$variable == "AGEGR1"], "Age group")
  expect_false("variable" %in% sp$codelists$variable)
  lab <- .ard_spec_labels(.ard_spec_scope(sp, "T1"))
  expect_identical(unname(lab[c("SEX", "AGEGR1")]), c("Sex (code list)", "Age group"))
})
