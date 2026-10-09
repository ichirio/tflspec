# ============================================================================
#  Definition workbooks for five reports -- DM, AE, ORR, LB shift, PK (#474)
# ============================================================================
#
#  Run from the repository root:
#
#      Rscript data-raw/ard-spec-examples/make-examples.R
#
#  For each report this script
#    1. builds an example ARD from the cards example data (and made-up
#       values where cards has none),
#    2. writes its definition workbook to inst/extdata/ard-spec/<id>.xlsx,
#    3. reads the workbook back and runs tfl_table_plan() on the ARD,
#    4. checks the table data frame is IDENTICAL to widen_ard() written
#       out, and the finished rtftable pages to the report's plan code --
#       and that tfl_as_table_spec(<plan code>) gives those pages back.
#
#  It also writes study.xlsx: all five in one workbook, keyed by output_id,
#  with the study's rounding on its `study` sheet, and the shared n (%)
#  template written once, on a `cells` row whose output_id is blank.
# ============================================================================


suppressMessages({
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists("R/table_spec.R")) {
    library(rtfreporter)
    pkgload::load_all(".", quiet = TRUE)
  } else {
    library(rtfreporter)
    library(tflspec)
  }
  library(cards)
  library(dplyr)
})

out_dir <- file.path("inst", "extdata", "ard-spec")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# One data.frame per sheet, NA where a cell is blank.  `tbl()` keeps the
# sheets readable here: one line per spreadsheet row.
tbl <- function(...) {
  d <- rbind.data.frame(..., stringsAsFactors = FALSE,
                        make.row.names = FALSE)
  d[] <- lapply(d, function(v) { v[v %in% ""] <- NA; v })
  d
}

# Two checks per report: the table data frame against widen_ard() written
# out, and the finished rtftable pages against the report's plan code --
# column header included: the workbook's `col_header` sheet has to give
# what the code's hand-written rtf_col_header() gives.
check <- function(id, ard_n, spec_path, code_tbl, code_plan, header,
                  pages_n = ard_n) {
  sp <- tfl_read_table_spec(spec_path, output_id = id)
  from_spec <- plan_apply(plan_cells(tfl_table_plan(ard_n, sp), notes = FALSE), "table")
  ok <- isTRUE(all.equal(as.data.frame(from_spec), as.data.frame(code_tbl)))
  pg_spec <- plan_apply(plan_cells(tfl_table_plan(pages_n, sp), notes = FALSE), "pages")
  pg_code <- plan_apply(code_plan, "pages")
  ok_pg <- isTRUE(all.equal(pg_spec, pg_code))
  # and the other way: the plan code written back as a workbook
  back <- suppressMessages(tfl_as_table_spec(code_plan, output_id = id))
  ok_pg <- ok_pg && isTRUE(attr(back, "same_pages"))
  np <- if (inherits(pg_spec, "rtftable")) 1L else length(pg_spec)
  cat(sprintf(
    "  %-4s %-11s table %3d x %2d == code: %-5s  pages %2d == plan code: %s\n",
    id, basename(spec_path), nrow(from_spec), ncol(from_spec),
    if (ok) "TRUE" else "FALSE", np,
    if (ok_pg) "TRUE" else "FALSE  <-- MISMATCH"))
  if (!ok) print(all.equal(as.data.frame(from_spec), as.data.frame(code_tbl)))
  if (!ok_pg) print(utils::head(all.equal(pg_spec, pg_code), 10))
  invisible(pg_spec)
}

# add the table-half sheets to a spec built above
with_pages <- function(sp, layout = NULL, columns = NULL, style = NULL,
                       col_header = NULL) {
  x <- unclass(sp)
  x$layout <- layout; x$columns <- columns; x$style <- style
  x$col_header <- col_header
  tfl_table_spec(x)
}
# one header cell per row: tbl() of these
hc <- function(id, line, cols, text = "", span = "", border_top = "",
               border_bottom = "") {
  list(output_id = id, line = as.character(line), cols = cols, span = span,
       text = text, border_top = border_top, border_bottom = border_bottom)
}

specs <- list()


# ============================================================================
#  The five reports.  Each one: its ARD, made from the cards example data
#  (and made-up values where cards has none); its definition (tables /
#  variables / cells, then layout / columns / style / col_header); the
#  table written as code twice -- widen_ard() at once, and the plan -- for
#  check() to compare with what the workbook gives.
# ============================================================================

arms <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

# ---------------------------------------------------------------- 1. DM
# Demographics by actual arm: age and baseline weight summarised; age group,
# sex and race counted.
ard_dm <- ard_stack(
  cards::ADSL, .by = TRT01A,
  ard_continuous(variables = c(AGE, WEIGHTBL),
                 statistic = ~ continuous_summary_fns(
                   c("N", "mean", "sd", "median", "min", "max"))),
  ard_categorical(variables = c(AGEGR1, SEX, RACE)),
  .total_n = TRUE)

dm_labels <- c(AGE      = "Age (years)",
               AGEGR1   = "Age group, n (%)",
               SEX      = "Sex, n (%)",
               RACE     = "Race, n (%)",
               WEIGHTBL = "Weight at baseline (kg)")
dm_cont <- c("n"         = "{N:.0f}",
             "Mean (SD)" = "{mean:.1f} ({sd:.2f})",
             "Median"    = "{median:.1f}",
             "Min, Max"  = "{min:.0f}, {max:.0f}")

specs$DM <- tfl_table_spec(
  study = c(rounding = "sas"),
  tables = tbl(
    list(output_id = "DM", cols = "TRT01A", rows = "group = variable",
         label = "", sort = "", note = "Demographics, one column per arm")),
  variables = tbl(
    list(output_id = "DM", variable = "AGE",      label = dm_labels[["AGE"]],      order = 1, levels = ""),
    list(output_id = "DM", variable = "AGEGR1",   label = dm_labels[["AGEGR1"]],   order = 2, levels = "<65 | 65-80 | >80"),
    list(output_id = "DM", variable = "SEX",      label = dm_labels[["SEX"]],      order = 3, levels = "F | M"),
    list(output_id = "DM", variable = "RACE",     label = dm_labels[["RACE"]],     order = 4, levels = ""),
    list(output_id = "DM", variable = "WEIGHTBL", label = dm_labels[["WEIGHTBL"]], order = 5, levels = "")),
  cells = tbl(
    list(output_id = "DM", variable = "continuous",  context = "", row = "n",         when = "", template = "{N}",                digits = "0",   signif = ""),
    list(output_id = "DM", variable = "continuous",  context = "", row = "Mean (SD)", when = "", template = "{mean} ({sd})",      digits = "1,2", signif = ""),
    list(output_id = "DM", variable = "continuous",  context = "", row = "Median",    when = "", template = "{median}",           digits = "1",   signif = ""),
    list(output_id = "DM", variable = "continuous",  context = "", row = "Min, Max",  when = "", template = "{min}, {max}",       digits = "0",   signif = ""),
    list(output_id = "DM", variable = "categorical", context = "", row = "",          when = "", template = "{n:.0f} ({p:.1f%})", digits = "",    signif = "")))
specs$DM <- with_pages(specs$DM,
  layout = tbl(list(output_id = "DM", stub_name = "row_label",
                    stub_before = "TRUE", blank_where = "between_groups",
                    blank_first = "TRUE", blank_last = "FALSE",
                    pages_max_rows = "30", pages_split = "group_safe")),
  columns = tbl(list(output_id = "DM", column = "row_label", rel_width = "4"),
                list(output_id = "DM", column = ".values",   rel_width = "2")),
  style = tbl(list(output_id = "DM", align_count_pct = "TRUE")),
  col_header = tbl(
    hc("DM", 1, "row_label"),
    hc("DM", 1, ".values", "{col}", span = "each"),
    hc("DM", 2, "row_label", "Parameter"),
    hc("DM", 2, ".values", "(N={n})", span = "each")))

dm_n <- normalize_ard(ard_dm)
dm_levels <- list(AGEGR1 = c("<65", "65-80", ">80"), SEX = c("F", "M"))
dm_code <- widen_ard(
  dm_n, cols = "TRT01A", rows = c(group = "variable"),
  labels = dm_labels, levels = dm_levels,
  cells = list(continuous = dm_cont, categorical = "{n:.0f} ({p:.1f%})"),
  rounding = "sas", notes = FALSE)
dm_header <- function(p) plan_col_header(p, values = list(n = TRUE), rtf_col_header(
  c("",          "{col}"),
  c("Parameter", "(N={n})")))
dm_plan <- table_plan(dm_n, cols = "TRT01A", rows = c(group = "variable")) |>
  plan_cells(notes = FALSE) |>
  plan_labels(dm_labels) |>
  plan_levels(AGEGR1 = dm_levels$AGEGR1, SEX = dm_levels$SEX) |>
  plan_cells(continuous = dm_cont, categorical = "{n:.0f} ({p:.1f%})") |>
  plan_digits(rounding = "sas") |>
  plan_stub(name = "row_label", before = TRUE) |>
  plan_blanks(where = "between_groups", first = TRUE, last = FALSE) |>
  plan_paginate_rows(max_rows = 30, split = "group_safe") |>
  plan_columns(widths = c(4, 2)) |>
  plan_style(align_count_pct = TRUE) |>
  dm_header()

# ---------------------------------------------------------------- 2. AE
# Treatment-emergent adverse events by system organ class and preferred
# term, in a column for each arm and sex; the subjects of ADSL are the
# denominators.  The ARD's own tabulation of the column variables (its
# `tabulate` rows) is not a row of the table.
adae <- cards::ADAE |> filter(TRTEMFL == "Y")
ard_ae <- ard_stack_hierarchical(
  adae, variables = c(AEBODSYS, AEDECOD), by = c(TRTA, SEX),
  denominator = cards::ADSL, id = USUBJID, over_variables = TRUE)
ard_ae <- ard_ae[ard_ae$context != "tabulate", ]

ae_levels <- list(TRTA = arms, SEX = c("F", "M"))
ae_sort <- c(".overall", "group1", ".depth", "-n", "label")
specs$AE <- tfl_table_spec(
  study = c(rounding = "sas"),
  tables = tbl(
    list(output_id = "AE", cols = "TRTA | SEX",
         rows = "group1 = AEBODSYS", label = "label = AEDECOD",
         sort = paste(ae_sort, collapse = " | "),
         note = "TEAEs by SOC and PT; the most frequent first within a SOC")),
  variables = tbl(
    list(output_id = "AE", variable = "TRTA", label = "", order = NA,
         levels = paste(arms, collapse = " | ")),
    list(output_id = "AE", variable = "SEX",  label = "", order = NA,
         levels = "F | M")),
  cells = tbl(
    list(output_id = "AE", variable = "", context = "", row = "", when = "",
         template = "{n:.0f} ({p:.1f%})", digits = "", signif = "")))
specs$AE <- with_pages(specs$AE,
  layout = tbl(list(output_id = "AE", stub_name = "row_label",
                    stub_before = "TRUE", blank_where = "between_groups",
                    blank_first = "TRUE", blank_last = "TRUE",
                    blank_counted = "TRUE", pages_max_rows = "28",
                    pages_split = "group_force")),
  columns = tbl(list(output_id = "AE", column = "row_label", rel_width = "36"),
                list(output_id = "AE", column = ".values",   rel_width = "8")),
  style = tbl(list(output_id = "AE", align_count_pct = "TRUE",
                   row_height_twips = "220")),
  col_header = tbl(
    hc("AE", 1, "row_label"),
    hc("AE", 1, ".values", "{col1} (N={n:sum})", span = "TRTA",
       border_bottom = "single"),
    hc("AE", 2, "row_label", "System organ class
  Preferred term"),
    hc("AE", 2, ".values", "{col2}
(N={n})
n (%)", span = "each")))

ae_n <- normalize_ard(ard_ae, hierarchy = c("AEBODSYS", "AEDECOD"),
                      overall = "Subjects with any TEAE")
ae_code <- widen_ard(
  ae_n, cols = c("TRTA", "SEX"), rows = c(group1 = "AEBODSYS"),
  label = c(label = "AEDECOD"), levels = ae_levels,
  cells = "{n:.0f} ({p:.1f%})", sort = ae_sort,
  rounding = "sas", notes = FALSE)
ae_header <- function(p) plan_col_header(p, values = list(n = TRUE), rtf_col_header(
  c(list(col_cell(1, "")),
    lapply(seq_along(arms), function(i)
      col_cell(c(2 * i, 2 * i + 1), "{col1} (N={n:sum})",
               border = rtf_border(bottom = "single")))),
  c("System organ class\n  Preferred term", "{col2}\n(N={n})\nn (%)")))
ae_plan <- table_plan(ae_n, cols = c("TRTA", "SEX"),
                      rows = c(group1 = "AEBODSYS"),
                      label = c(label = "AEDECOD")) |>
  plan_cells(notes = FALSE) |>
  plan_levels(TRTA = ae_levels$TRTA, SEX = ae_levels$SEX) |>
  plan_sort(".overall", "group1", ".depth", "-n", "label") |>
  plan_cells("{n:.0f} ({p:.1f%})") |>
  plan_digits(rounding = "sas") |>
  plan_stub(name = "row_label", before = TRUE) |>
  plan_blanks(where = "between_groups", first = TRUE, last = TRUE,
              counted = TRUE) |>
  plan_paginate_rows(max_rows = 28, split = "group_force") |>
  plan_columns(widths = c(36, 8)) |>
  plan_style(align_count_pct = TRUE, row_height_twips = 220L) |>
  ae_header()

# ---------------------------------------------------------------- 3. ORR
# The response rate (made up) with its exact 95% CI, by arm, in two
# subgroup families -- age group and sex.  Each arm has two columns: the
# subjects (N) and the responders with the CI under them; a subgroup
# without responders prints 0, one where all responded (100).
set.seed(2026)
resp <- cards::ADSL |>
  select(USUBJID, TRT01A, AGEGR1, SEX) |>
  mutate(RESP = runif(n()) < c(Placebo = 0.2, "Xanomeline Low Dose" = 0.4,
                               "Xanomeline High Dose" = 0.55)[TRT01A])
resp$RESP[resp$TRT01A == "Placebo" & resp$AGEGR1 == ">80"] <- FALSE
ard_resp <- bind_rows(
  resp |> group_by(TRT01A, AGEGR1) |>
    cardx::ard_categorical_ci(variables = RESP, method = "clopper-pearson"),
  resp |> group_by(TRT01A, SEX) |>
    cardx::ard_categorical_ci(variables = RESP, method = "clopper-pearson"))
ard_resp <- ard_resp[ard_resp$stat_name %in%
                       c("N", "n", "estimate", "conf.low", "conf.high"), ]

resp_cells <- list(
  n    = c("1" = "{N:.0f}"),
  resp = cell_rows(
    "1" = c(n == 0        ~ "0",
            estimate == 1 ~ "{n:.0f} (100)",
                            "{n:.0f} ({estimate:.1f%})"),
    "2" = "[{conf.low:.1f%}, {conf.high:.1f%}]"))
specs$ORR <- tfl_table_spec(
  study = c(rounding = "sas"),
  tables = tbl(
    list(output_id = "ORR", cols = "TRT01A | variable",
         rows = "family = group2 | subgroup = group2_level", label = "NA",
         sort = "FALSE", sep = "_",
         note = "`variable` (n / resp) is made with mutate() before the table")),
  cells = tbl(
    list(output_id = "ORR", variable = "n",    context = "", row = "1", when = "",              template = "{N:.0f}",                              digits = "", signif = ""),
    list(output_id = "ORR", variable = "resp", context = "", row = "1", when = "n == 0",        template = "0",                                    digits = "", signif = ""),
    list(output_id = "ORR", variable = "resp", context = "", row = "1", when = "estimate == 1", template = "{n:.0f} (100)",                        digits = "", signif = ""),
    list(output_id = "ORR", variable = "resp", context = "", row = "1", when = "",              template = "{n:.0f} ({estimate:.1f%})",            digits = "", signif = ""),
    list(output_id = "ORR", variable = "resp", context = "", row = "2", when = "",              template = "[{conf.low:.1f%}, {conf.high:.1f%}]", digits = "", signif = "")))
resp_widths <- c(14, 14, rep(c(7, 17), length(arms)))
resp_cols <- list(
  list(output_id = "ORR", column = "family",   rel_width = "14", row_title = "TRUE"),
  list(output_id = "ORR", column = "subgroup", rel_width = "14", row_title = "TRUE"))
for (a in sort(arms)) {
  resp_cols <- c(resp_cols, list(
    list(output_id = "ORR", column = paste0(a, "_n"),    rel_width = "7",  row_title = ""),
    list(output_id = "ORR", column = paste0(a, "_resp"), rel_width = "17", row_title = "")))
}
specs$ORR <- with_pages(specs$ORR,
  layout = tbl(list(output_id = "ORR", group_mode = "value",
                    group_collapse = "1 | 2",
                    blank_where = "between_groups", blank_first = "TRUE",
                    blank_last = "TRUE", pages_max_rows = "16",
                    pages_split = "group_safe")),
  columns = do.call(tbl, resp_cols),
  style = tbl(list(output_id = "ORR", align_count_pct = "FALSE")),
  col_header = tbl(
    hc("ORR", 1, "family | subgroup"),
    hc("ORR", 1, ".values", "{col1}", span = "TRT01A"),
    hc("ORR", 2, "family | subgroup", "Subgroup"),
    hc("ORR", 2, "variable = n", "N", span = "each"),
    hc("ORR", 2, "variable = resp", "Responders, n (%)
[95% CI]", span = "each")))

resp_n <- normalize_ard(ard_resp) |>
  mutate(variable = if_else(stat_name == "N", "n", "resp"),
         group2 = c(AGEGR1 = "Age group", SEX = "Sex")[group2])
resp_code <- widen_ard(
  resp_n, cols = c("TRT01A", "variable"), sep = "_",
  rows = c(family = "group2", subgroup = "group2_level"), label = NA,
  cells = resp_cells, sort = FALSE, rounding = "sas", notes = FALSE)
resp_header <- function(p) plan_col_header(p, function(n, tbl) {
  a <- unique(sub("_(n|resp)$", "", names(tbl)[-(1:2)]))
  rtf_col_header(
    c(list(col_cell(c(1, 2), "")),
      lapply(seq_along(a), function(i) col_cell(c(2 * i + 1, 2 * i + 2), a[i]))),
    c(list(col_cell(c(1, 2), "Subgroup")),
      unlist(lapply(seq_along(a), function(i)
        list(col_cell(2 * i + 1, "N"),
             col_cell(2 * i + 2, "Responders, n (%)\n[95% CI]"))),
        recursive = FALSE)))
})
resp_plan <- table_plan(resp_n, cols = c("TRT01A", "variable"),
                        rows = c(family = "group2", subgroup = "group2_level"),
                        label = NA) |>
  plan_cells(notes = FALSE) |>
  plan_sort(FALSE) |>
  plan_cells(n = resp_cells$n, resp = resp_cells$resp) |>
  plan_digits(rounding = "sas") |>
  plan_row_group(mode = "value", collapse = c(1L, 2L)) |>
  plan_blanks(where = "between_groups", first = TRUE, last = TRUE) |>
  plan_paginate_rows(max_rows = 16, split = "group_safe") |>
  plan_columns(widths = resp_widths, row_title = c(1, 2), sep = "_") |>
  plan_style(align_count_pct = FALSE) |>
  resp_header()

# ---------------------------------------------------------------- 4. LB
# A shift table (made-up grades): baseline grade across, worst grade after
# baseline down, one page per parameter with its own subjects.  The "All"
# row and column are tabulations of their own; the subjects per baseline
# grade -- the header's N -- are the column variable's tabulation (#482).
set.seed(41)
lab <- cards::ADSL |>
  select(USUBJID) |>
  tidyr::crossing(PARAM = c("Alanine aminotransferase", "Platelet count")) |>
  mutate(BGRADE = sample(c("Grade 0", "Grade 1", "Grade 2"), n(), TRUE,
                         c(.7, .2, .1)),
         WGRADE = sample(c("Grade 0", "Grade 1", "Grade 2", "Grade 3"),
                         n(), TRUE, c(.5, .3, .15, .05))) |>
  # fewer subjects have a platelet count: the two pages differ in N
  filter(!(PARAM == "Platelet count" & row_number() %% 15 == 0))
ard_lb <- bind_rows(
  ard_categorical(lab, by = c(PARAM, BGRADE), variables = WGRADE),
  ard_categorical(mutate(lab, WGRADE = "All"), by = c(PARAM, BGRADE), variables = WGRADE),
  ard_categorical(mutate(lab, BGRADE = "All"), by = c(PARAM, BGRADE), variables = WGRADE),
  ard_categorical(mutate(lab, BGRADE = "All", WGRADE = "All"),
                  by = c(PARAM, BGRADE), variables = WGRADE),
  ard_categorical(lab, by = PARAM, variables = BGRADE))

lb_levels <- list(BGRADE = c("Grade 0", "Grade 1", "Grade 2", "All"),
                  WGRADE = c("Grade 0", "Grade 1", "Grade 2", "Grade 3", "All"))
specs$LB <- tfl_table_spec(
  study = c(rounding = "sas"),
  tables = tbl(
    list(output_id = "LB", cols = "BGRADE",
         rows = 'PARAM = PARAM | group1 = "Worst grade after baseline"',
         label = "label = .label", sort = "",
         note = "Shift from the baseline grade to the worst grade after it")),
  variables = tbl(
    list(output_id = "LB", variable = "BGRADE", label = "", order = NA,
         levels = paste(lb_levels$BGRADE, collapse = " | ")),
    list(output_id = "LB", variable = "WGRADE", label = "", order = NA,
         levels = paste(lb_levels$WGRADE, collapse = " | "))),
  cells = tbl(
    list(output_id = "LB", variable = "", context = "", row = "", when = "",
         template = "{n:.0f} ({p:.1f%})", digits = "", signif = "")))
specs$LB <- with_pages(specs$LB,
  layout = tbl(list(output_id = "LB", stub_name = "row_label",
                    group_page = "TRUE", group_keep = "FALSE",
                    blank_first = "TRUE", blank_last = "TRUE")),
  columns = tbl(list(output_id = "LB", column = "row_label", rel_width = "4"),
                list(output_id = "LB", column = ".values",   rel_width = "1")),
  style = tbl(list(output_id = "LB", align_count_pct = "TRUE")),
  col_header = tbl(
    hc("LB", 1, "row_label"),
    hc("LB", 1, ".values", "Baseline grade
(N={n})"),
    hc("LB", 2, "row_label", "Worst grade"),
    hc("LB", 2, ".values", "{col}", span = "each")))

lb_code <- widen_ard(
  normalize_ard(ard_lb), cols = "BGRADE",
  rows = c(PARAM = "PARAM", group1 = ~ "Worst grade after baseline"),
  label = c(label = ".label"), cells = "{n:.0f} ({p:.1f%})",
  levels = lb_levels, rounding = "sas", notes = FALSE)
lb_header <- function(p) plan_col_header(p, values = list(n = TRUE), rtf_col_header(
  list(col_cell(1L, ""), col_cell(c(2L, 5L), "Baseline grade\n(N={n})")),
  c("Worst grade", lb_levels$BGRADE)))
lb_pages_n <- normalize_ard(ard_lb, drop_contexts = "attributes")
lb_plan <- table_plan(lb_pages_n, cols = "BGRADE",
                      rows = c(PARAM = "PARAM",
                               group1 = ~ "Worst grade after baseline"),
                      label = c(label = ".label")) |>
  plan_cells(notes = FALSE) |>
  plan_levels(BGRADE = lb_levels$BGRADE, WGRADE = lb_levels$WGRADE) |>
  plan_cells("{n:.0f} ({p:.1f%})") |>
  plan_digits(rounding = "sas") |>
  plan_stub(name = "row_label") |>
  plan_paginate_group(keep = FALSE) |>
  plan_blanks(first = TRUE, last = TRUE) |>
  plan_columns(widths = c(4, rep(1, 4))) |>
  plan_style(align_count_pct = TRUE) |>
  lb_header()

# ---------------------------------------------------------------- 5. PK
# Concentrations (made up) of a parent drug and its metabolite at 24
# nominal times: one statistic a row, one time a column, too many columns
# for a page -- they continue on the next pages, the first two columns
# repeated.  Each statistic has its own digits: decimals, or "<k>s" for k
# significant digits.
set.seed(5)
pk_times <- c("Pre-dose", paste(c(0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 16,
                                  24, 36, 48, 72, 96, 120, 144, 168, 192, 216,
                                  240), "h"))
pk <- cards::ADSL |>
  select(USUBJID) |>
  tidyr::crossing(ANALYTE = c("Parent drug", "Metabolite M1"), NOMTIME = pk_times) |>
  mutate(CONC = rlnorm(n(), log(ifelse(ANALYTE == "Parent drug", 80, 15)), 0.6))
ard_pk <- ard_stack(
  pk, .by = c(ANALYTE, NOMTIME),
  ard_continuous(variables = CONC,
                 statistic = ~ continuous_summary_fns(
                   c("N", "mean", "sd", "median", "min", "max"))))
pk_stats <- c("N", "Mean", "SD", "Median", "Min", "Max")
pk_digits <- c(N = "0", Mean = "3s", SD = "3s", Median = "3s", Min = "1",
               Max = "1")

specs$PK <- tfl_table_spec(
  study = c(rounding = "sas"),
  tables = tbl(
    list(output_id = "PK", cols = "NOMTIME", rows = "Analyte = ANALYTE",
         label = "Statistic = stat_label", stats = "rows", sort = "",
         note = "One statistic a row, its digits on the cells sheet")),
  variables = tbl(
    list(output_id = "PK", variable = "Statistic", label = "", order = NA,
         levels = paste(pk_stats, collapse = " | ")),
    list(output_id = "PK", variable = "NOMTIME", label = "", order = NA,
         levels = paste(pk_times, collapse = " | "))),
  # each statistic's digits, as rows of `cells` with no template
  cells = do.call(tbl, lapply(names(pk_digits), function(st) list(
    output_id = "PK", variable = "", context = "", row = st, when = "",
    template = "",
    digits = if (endsWith(pk_digits[[st]], "s")) "" else pk_digits[[st]],
    signif = if (endsWith(pk_digits[[st]], "s")) sub("s$", "", pk_digits[[st]]) else ""))))
specs$PK <- with_pages(specs$PK,
  layout = tbl(list(output_id = "PK", group_collapse = "TRUE",
                    blank_where = "between_groups", blank_first = "TRUE",
                    blank_last = "TRUE", pages_max_rows = "20",
                    colpages_every = "9", colpages_keep = "1 | 2")),
  columns = tbl(
    list(output_id = "PK", column = "Analyte",   rel_width = "3",
         row_title = "TRUE", decimal_split = ""),
    list(output_id = "PK", column = "Statistic", rel_width = "2",
         row_title = "TRUE", decimal_split = ""),
    list(output_id = "PK", column = ".values",   rel_width = "2",
         row_title = "",     decimal_split = "TRUE")),
  col_header = tbl(
    hc("PK", 1, "Analyte"),
    hc("PK", 1, "Statistic"),
    hc("PK", 1, ".values", "Nominal time after dose"),
    hc("PK", 2, "Analyte", "Analyte"),
    hc("PK", 2, "Statistic", "Statistic"),
    hc("PK", 2, ".values", "{col}", span = "each")))

pk_n <- normalize_ard(ard_pk)
pk_code <- widen_ard(
  pk_n, cols = "NOMTIME", rows = c(Analyte = "ANALYTE"),
  label = c(Statistic = "stat_label"), stats = "rows",
  levels = list(Statistic = pk_stats, NOMTIME = pk_times),
  rounding = "sas", notes = FALSE)
pk_header <- function(p) plan_col_header(p, function(n, tbl) list(
  list(col_cell(1, ""), col_cell(2, ""),
       col_cell(c(3, length(tbl)), "Nominal time after dose")),
  names(tbl)))
pk_plan <- table_plan(pk_n, cols = "NOMTIME", rows = c(Analyte = "ANALYTE"),
                      label = c(Statistic = "stat_label")) |>
  plan_cells(stats = "rows", notes = FALSE) |>
  plan_levels(Statistic = pk_stats, NOMTIME = pk_times) |>
  plan_digits(rounding = "sas") |>
  plan_digits(.rows = pk_digits) |>
  plan_row_group(collapse = TRUE) |>
  plan_blanks(where = "between_groups", first = TRUE, last = TRUE) |>
  plan_paginate_rows(max_rows = 20) |>
  pk_header() |>
  # widths by column name, the row titles and the decimal alignment of the
  # values, declared
  plan_columns(widths = c(Analyte = 3, Statistic = 2, .values = 2),
               row_title = c("Analyte", "Statistic"), decimal = ".values") |>
  plan_paginate_cols(every = 9, keep = 1:2)

# ------------------------------------------------------ write and check
# Each workbook holds the sheets its spec needs; what a column means is a
# comment on its header cell (tfl_spec_columns()).
write_book <- function(spec, path) tfl_write_table_spec(spec, path)

run_checks <- function(path_of) {
  check("DM",  normalize_ard(ard_dm), path_of("DM"),  dm_code,  dm_plan,
        dm_header)
  check("AE",  ae_n,                  path_of("AE"),  ae_code,  ae_plan,
        ae_header)
  check("ORR", resp_n,                 path_of("ORR"), resp_code, resp_plan,
        resp_header)
  check("LB",  normalize_ard(ard_lb), path_of("LB"),  lb_code,  lb_plan,
        lb_header, pages_n = lb_pages_n)
  check("PK",  normalize_ard(ard_pk), path_of("PK"),  pk_code,  pk_plan,
        pk_header)
}

cat("\nOne workbook per report:\n")
for (id in names(specs)) {
  write_book(specs[[id]], file.path(out_dir, paste0(id, ".xlsx")))
}
run_checks(function(id) file.path(out_dir, paste0(id, ".xlsx")))

# ------------------------------------------ one study workbook, all five
# The rounding family is one per study, on the `study` sheet.  What the
# reports share is written ONCE, on a row with a blank output_id -- here the
# n (%) template on `cells`.  A lookup that finds nothing more specific
# falls through to it, so a report's row that only restates it -- AE's and
# LB's catch-all, DM's `categorical` -- is dropped rather than kept twice.
all_sheet <- function(s) do.call(rbind, lapply(specs, function(x) x[[s]]))
study_cells <- all_sheet("cells")
default_tpl <- "{n:.0f} ({p:.1f%})"
restates <- !is.na(study_cells$template) &
  study_cells$template == default_tpl &
  is.na(study_cells$row) & is.na(study_cells$when) &
  is.na(study_cells$digits) & is.na(study_cells$signif) &
  is.na(study_cells$context) &
  (is.na(study_cells$variable) | study_cells$variable == "categorical")
study_cells <- rbind(
  tbl(list(output_id = "", variable = "", context = "", row = "", when = "",
           template = default_tpl, digits = "", signif = "")),
  study_cells[!restates, , drop = FALSE])
study <- tfl_table_spec(all_sheet("tables"), all_sheet("variables"), study_cells,
                  study = c(rounding = "sas"), layout = all_sheet("layout"),
                  columns = all_sheet("columns"), style = all_sheet("style"),
                  col_header = all_sheet("col_header"))
study_path <- file.path(out_dir, "study.xlsx")
write_book(study, study_path)

cat("\nThe same five from one study workbook (study.xlsx):\n")
run_checks(function(id) study_path)
cat("\nwritten to", normalizePath(out_dir), "\n")


# ============================================================================
#  The report half: report.xlsx beside study.xlsx
# ============================================================================
#
#  A separate workbook for what surrounds the table -- the list of outputs,
#  the page, the running header and footer, titles and footnotes -- read
#  together with the table workbook: tfl_read_report_spec(c(report, study)).
#  Each report is checked against the same document written as code, as
#  RTF text, with the run time fixed.

options(rtfreporter.render_time = as.POSIXct("2026-01-01 09:00:00"))
prog_dir <- "C:\\study\\tfl"
page_header <- list(c("Example Pharma", "Clinical Study Report"),
                    c("Study EX-001", "Page {PAGE} of {TOTAL_PAGES}"))
run_line <- "{PROGRAM}       Generated on: {DATETIME}"
rpt_titles <- list(
  DM  = c("", "Table 14.1.2", "Demographics and Baseline Characteristics", "Safety Population"),
  AE  = c("", "Table 14.3.1.2", "Treatment-Emergent Adverse Events by System Organ Class, Preferred Term and Sex", "Safety Population"),
  ORR = c("", "Table 14.2.3", "Response Rate by Subgroup", "Efficacy Population"),
  LB  = c("", "Table 14.3.4", "Laboratory Toxicity Grade: Shift from Baseline", "Safety Population", ""),
  PK  = c("", "Table 14.5.1", "Summary of Concentrations by Nominal Time", "Pharmacokinetic Population"))
rpt_notes <- list(
  DM  = c("SD: standard deviation; Min: minimum; Max: maximum."),
  AE  = c("A subject is counted once in a system organ class and once in a preferred term.", "Coded with MedDRA."),
  ORR = c("Response: complete or partial response.", "CI: exact (Clopper-Pearson) confidence interval."),
  LB  = c("Each column is a baseline grade; each row the worst grade after baseline."),
  PK  = c("SD: standard deviation", "Concentrations below the lower limit of quantification are set to zero."))
# ---- the same five documents written as code
code_doc <- function(id, plan) {
  prog <- paste(prog_dir, id, sep = "\\")
  hdr <- rtf_header(c(page_header, as.list(rpt_titles[[id]])))
  if (id == "PK") {
    return(rtf_document(
      page = list(orientation = "landscape", paper_size = "A4",
                  margin_top_in = 0.5, margin_left_in = 0.5,
                  margin_right_in = 0.5, margin_bottom_in = 0.5),
      default_format = rtf_default_format(footnote_width = "page"),
      program = prog) |>
      rtf_section(secinfo = list(header = hdr)) |>
      rtf_tables(plan, font_size_half_points = 14) |>
      rtf_footnotes(list(c(rpt_notes$PK, run_line)),
                    font_size_half_points = 14))
  }
  rtf_document(program = prog) |>
    rtf_section(secinfo = list(
      header = hdr,
      footer = rtf_footer(c(lapply(rpt_notes[[id]], function(t) c(l = t)),
                            list(c(l = run_line)))))) |>
    rtf_tables(plan, auto_section = id == "LB")
}

# ---- report.xlsx
lnrow <- function(id, line, left = "", center = "", right = "")
  list(output_id = id, line = as.character(line), left = left,
       center = center, right = right)
hdr_rows <- lapply(seq_along(page_header), function(i)
  lnrow("", i, page_header[[i]][1L], right = page_header[[i]][2L]))
ftr_rows <- list(lnrow("", 99, run_line))
fn_rows <- list()
for (id in names(rpt_titles)) {
  tl <- rpt_titles[[id]]
  for (i in seq_along(tl)) hdr_rows[[length(hdr_rows) + 1L]] <- lnrow(id, i + 2, center = tl[i])
  nt <- rpt_notes[[id]]
  if (id == "PK") {
    for (i in seq_along(nt)) fn_rows[[length(fn_rows) + 1L]] <- lnrow(id, i, nt[i])
    fn_rows[[length(fn_rows) + 1L]] <- lnrow(id, length(nt) + 1, run_line)
  } else {
    for (i in seq_along(nt)) ftr_rows[[length(ftr_rows) + 1L]] <- lnrow(id, i, nt[i])
  }
}
report_spec <- tfl_table_spec(
  study = c(output_path = "output", program_dir = prog_dir),
  report = tbl(
    list(output_id = "",   type = "table", file = "{output_id}.rtf",
         program = "{output_id}", auto_section = "", table_font_size_half_points = "",
         footnote_font_size_half_points = "", page_footer = "",
         note = "the study's defaults"),
    list(output_id = "LB", type = "", file = "", program = "",
         auto_section = "TRUE", table_font_size_half_points = "", footnote_font_size_half_points = "",
         page_footer = "", note = "one section per parameter"),
    list(output_id = "PK", type = "", file = "", program = "",
         auto_section = "", table_font_size_half_points = "14", footnote_font_size_half_points = "14",
         page_footer = "FALSE", note = "the run line goes under the table")),
  page = tbl(list(output_id = "PK", orientation = "landscape",
                  paper_size = "A4", margin_top_in = "0.5",
                  margin_bottom_in = "0.5", margin_left_in = "0.5",
                  margin_right_in = "0.5", footnote_width = "page")),
  header = do.call(tbl, hdr_rows),
  footer = do.call(tbl, ftr_rows),
  footnotes = do.call(tbl, fn_rows))
report_book <- file.path(out_dir, "report.xlsx")
tfl_write_report_spec(report_spec, report_book)

cat("\nThe report half: report.xlsx + study.xlsx -> tfl_report():\n")
code_plans <- list(DM = dm_plan, AE = ae_plan, ORR = resp_plan, LB = lb_plan,
                   PK = pk_plan)
pages_n <- list(DM = normalize_ard(ard_dm), AE = ae_n, ORR = resp_n,
                LB = lb_pages_n, PK = normalize_ard(ard_pk))
tmp <- tempfile("rtf"); dir.create(tmp)
for (id in names(code_plans)) {
  sp <- tfl_read_report_spec(c(report_book, study_path), output_id = id)
  p <- plan_cells(tfl_table_plan(pages_n[[id]], sp), notes = FALSE)
  a <- file.path(tmp, "code.rtf"); b <- file.path(tmp, "spec.rtf")
  generate_rtfreport(code_doc(id, code_plans[[id]]), a, overwrite = TRUE)
  generate_rtfreport(tfl_report(sp, content = p), b, overwrite = TRUE)
  same <- identical(readLines(a, warn = FALSE), readLines(b, warn = FALSE))
  # and the program the definition writes: tfl_table_code() +
  # tfl_report_code(), run, gives the same RTF again
  prog <- c(tfl_table_code(sp, pipe = "|>"),
            tfl_report_code(sp, content = "plan"))
  env <- new.env(parent = asNamespace("tflspec"))
  env$data <- pages_n[[id]]
  suppressMessages(eval(parse(text = prog), env))
  g <- file.path(tmp, "gen.rtf")
  generate_rtfreport(env$doc, g, overwrite = TRUE)
  gen <- identical(readLines(g, warn = FALSE), readLines(b, warn = FALSE))
  cat(sprintf("  %-4s -> %-14s identical RTF to the code: %s; written program: %s\n",
              id, tfl_report_path(sp), if (same) "TRUE" else "FALSE  <-- MISMATCH",
              if (gen) "TRUE" else "FALSE  <-- MISMATCH"))
}
options(rtfreporter.render_time = NULL)
