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
  code <- tfl_table_code(sp, data = "data", pipe = "|>", normalize = FALSE)
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
  code2 <- tfl_table_code(sp, data = "dm", plan = "tbl", pipe = "%>%", normalize = FALSE)
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
  code <- tfl_table_code(sp, data = "data", pipe = "|>", normalize = FALSE)
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
  code <- c(tfl_table_code(tsp, data = "data", pipe = "|>", normalize = FALSE),
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

test_that("tfl_normalize_code(): the hierarchy and the any-event row the definitions say (#236)", {
  sp <- tfl_table_spec(tables = data.frame(
    output_id = c("T-1", "T-2"), cols = "TRTA", rows = c(NA, "group1 = AEBODSYS"),
    overall_label = c(NA, "Subjects with at least one TEAE")))
  ard <- tfl_ard_spec(list(
    datasets = data.frame(dataset = c("ADSL", "ADAE"), path = c("adsl.rds", "adae.rds")),
    analyses = data.frame(output_id = c("T-1", "T-2"), analysis_id = c("AGE", "AE"),
                          method = c("continuous", "hierarchical"),
                          dataset = c("ADSL", "ADAE"), by = "TRTA",
                          variables = c("AGE", "AEBODSYS | AEDECOD"),
                          args = c(NA, "over_variables = TRUE"))))
  expect_identical(tfl_normalize_code(sp, ard, "T-1"), "data <- normalize_ard(ard)")
  expect_identical(tfl_normalize_code(sp, ard, "T-2"),
                   paste0("data <- normalize_ard(ard, hierarchy = c(\"AEBODSYS\", \"AEDECOD\"), ",
                          "overall = \"Subjects with at least one TEAE\")"))
  expect_identical(tfl_normalize_code(sp, ard, "T-2", ard = "x", data = "d"),
                   paste0("d <- normalize_ard(x, hierarchy = c(\"AEBODSYS\", \"AEDECOD\"), ",
                          "overall = \"Subjects with at least one TEAE\")"))
  # the row is a hierarchy's: without one, say so
  expect_error(tfl_normalize_code(sp, NULL, "T-2"), "give `ard_spec`")
  # the column reads and writes back
  f <- tempfile(fileext = ".xlsx"); on.exit(unlink(f), add = TRUE)
  tfl_write_table_spec(sp, f)
  expect_identical(tfl_read_table_spec(f)$tables$overall_label[2L], "Subjects with at least one TEAE")
})

test_that("tables$stat_groups: one record's statistics in two columns, as plan_stat_groups() (rtfreporter #619)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom", minimum_version = "1.0.8")
  skip_if_not(utils::packageVersion("rtfreporter") >= "0.8.2.9046")
  adsl <- cards::ADSL
  adsl$RESP <- factor(ifelse(adsl$AGE > 75, "Y", "N"), levels = c("Y", "N"))
  ard <- cards::ard_strata(adsl, .by = ARM, .strata = SEX, .f = ~ cardx::ard_categorical_ci(
    .x, variables = RESP, value = RESP ~ "Y", method = "clopper-pearson", conf.level = 0.90))
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "ORR", cols = "ARM | .stat_group",
                        rows = "grp1 = group2 | grp2 = group2_level", label = "NA", sort = "FALSE",
                        stat_groups = "n = N | orr_ci = n, estimate, conf.low, conf.high"),
    cells = data.frame(output_id = "ORR", variable = c("n", "orr_ci", "orr_ci"),
                       row = c("1", "1", "2"),
                       template = c("{N:.0f}", "{n:.0f} ({estimate:.1f%})",
                                    "{conf.low:.1f%}, {conf.high:.1f%}")))
  code <- paste(tfl_table_code(sp, "ORR"), collapse = "
")
  expect_match(code, "plan_stat_groups(", fixed = TRUE)
  expect_match(code, "orr_ci = c(\"n\", \"estimate\", \"conf.low\", \"conf.high\")", fixed = TRUE)
  tab <- suppressMessages(rtfreporter::plan_apply(
    tfl_table_plan(rtfreporter::normalize_ard(ard), sp, "ORR"), "table"))
  tab <- as.data.frame(if (is.data.frame(tab)) tab else tab[[1L]])
  expect_true(all(c("Placebo____n", "Placebo____orr_ci") %in% names(tab)))
  # a plan says it back
  back <- tfl_as_table_spec(tfl_table_plan(rtfreporter::normalize_ard(ard), sp, "ORR"),
                            output_id = "ORR", compare = FALSE)
  expect_identical(back$tables$stat_groups, "n = N | orr_ci = n, estimate, conf.low, conf.high")
  expect_error(tfl_table_spec(tables = data.frame(output_id = "X", cols = "ARM", stat_groups = "n N")) |>
                 tfl_table_code("X"), "not `name = statistic")
})

test_that("layout$group_heading: a page per arm named with the arm and its N, as plan_paginate_group(heading = ) (rtfreporter #619)", {
  skip_if_not_installed("cards")
  skip_if_not(utils::packageVersion("rtfreporter") >= "0.8.2.9046")
  skip_if_not("heading" %in% names(formals(rtfreporter::plan_paginate_group)))
  adsl <- cards::ADSL
  ard <- cards::bind_ard(cards::ard_tabulate(adsl, variables = ARM),
                         cards::ard_tabulate(adsl, by = ARM, variables = SEX))
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T", cols = ".stat_group",
                        rows = "arm = ARM | label = variable_level", label = "NA",
                        stat_groups = "n = n | pct = p"),
    cells = data.frame(output_id = "T", variable = c("n", "pct"),
                       template = c("{n}", "{p:.1f%}")),
    layout = data.frame(output_id = "T", group_page = "TRUE", group_col = "arm",
                        group_keep = "FALSE", group_heading = "Treatment: {value} (N={n})"))
  code <- paste(tfl_table_code(sp, "T"), collapse = "\n")
  expect_match(code, "heading = \"Treatment: {value} (N={n})\"", fixed = TRUE)
  p <- tfl_table_plan(rtfreporter::normalize_ard(ard), sp, "T")
  pages <- suppressMessages(rtfreporter::plan_apply(p))
  n_arm <- table(adsl$ARM)
  expect_identical(names(pages), sprintf("Treatment: %s (N=%d)", names(n_arm), as.integer(n_arm)))
  back <- tfl_as_table_spec(p, output_id = "T", compare = FALSE)
  expect_identical(back$layout$group_heading, "Treatment: {value} (N={n})")
})

test_that("layout$group_heading_at: where the page names print, said in the plan; a report's auto_section given wins (rtfreporter #672)", {
  skip_if_not_installed("cards")
  skip_if_not("heading_at" %in% names(formals(rtfreporter::plan_paginate_group)))
  adsl <- cards::ADSL
  ard <- rtfreporter::normalize_ard(cards::bind_ard(cards::ard_tabulate(adsl, variables = ARM),
                                                    cards::ard_tabulate(adsl, by = ARM, variables = SEX)))
  sp <- function(at = NA, auto = NA) tfl_table_spec(
    tables = data.frame(output_id = "T", cols = ".stat_group",
                        rows = "arm = ARM | label = variable_level", label = "NA",
                        stat_groups = "n = n | pct = p"),
    cells = data.frame(output_id = "T", variable = c("n", "pct"),
                       template = c("{n}", "{p:.1f%}")),
    layout = data.frame(output_id = "T", group_page = "TRUE", group_col = "arm",
                        group_keep = "FALSE", group_heading = "Arm: {value}",
                        group_heading_at = at),
    report = data.frame(output_id = "T", auto_section = auto),
    header = data.frame(output_id = "T", line = 1, left = "Table 1"))
  code <- paste(tfl_table_code(sp("section"), "T"), collapse = "
")
  expect_match(code, 'heading_at = "section"', fixed = TRUE)
  rtf <- function(s) {
    s <- suppressMessages(tflspec:::.ard_spec_scope(s, "T"))
    p <- tfl_table_plan(ard, s, "T")
    f <- tempfile(fileext = ".rtf"); on.exit(unlink(f))
    suppressMessages(rtfreporter::generate_rtfreport(tfl_report(s, content = p), f, overwrite = TRUE))
    unname(tools::md5sum(f))
  }
  withr::local_options(rtfreporter.render_time = as.POSIXct("2000-01-01", tz = "UTC"))
  # the plan's word gives the report's auto_section = TRUE, and a blank
  # report setting does not undo it
  expect_identical(rtf(sp("section")), rtf(sp(auto = "TRUE")))
  expect_false(identical(rtf(sp("section")), rtf(sp())))
  # the report's own setting wins
  expect_identical(rtf(sp("section", auto = "FALSE")), rtf(sp()))
  p <- tfl_table_plan(ard, sp("title"), "T")
  back <- tfl_as_table_spec(p, output_id = "T", compare = FALSE)
  expect_identical(back$layout$group_heading_at, "title")
  expect_identical(back$layout$group_heading, "Arm: {value}")
})

test_that("tables$stack and layout$pages_heading: subgroups as one pair of keys, BY pages named (rtfreporter #619)", {
  skip_if_not_installed("cards")
  skip_if_not("stack" %in% names(formals(rtfreporter::normalize_ard)))
  adsl <- cards::ADSL
  adsl$ALL <- "All"
  ard <- cards::bind_ard(
    cards::ard_tabulate(adsl, variables = ARM),
    cards::ard_tabulate(adsl, by = c(ARM, ALL), variables = DISCONFL),
    cards::ard_tabulate(adsl, by = c(SEX, ARM), variables = DISCONFL))
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T", cols = ".stat_group",
                        rows = "arm = ARM | grp = subgroup | lvl = subgroup_level",
                        label = "NA", stat_groups = "n = n | pct = p",
                        stack = "subgroup = ALL, SEX"),
    cells = data.frame(output_id = "T", variable = c("n", "pct"),
                       template = c("{n}", "{p:.1f%}")),
    layout = data.frame(output_id = "T", pages_page_by = "arm", pages_max_rows = "20",
                        pages_heading = "Treatment: {value} (N={n})"))
  line <- tfl_normalize_code(sp, output_id = "T")
  expect_identical(line, "data <- normalize_ard(ard, stack = list(subgroup = c(\"ALL\", \"SEX\")))")
  data <- eval(parse(text = sub("normalize_ard", "rtfreporter::normalize_ard", line, fixed = TRUE)))
  code <- paste(tfl_table_code(sp, "T"), collapse = "\n")
  expect_match(code, "heading = \"Treatment: {value} (N={n})\"", fixed = TRUE)
  p <- tfl_table_plan(data, sp, "T")
  pages <- suppressMessages(rtfreporter::plan_apply(p))
  n_arm <- table(adsl$ARM)
  expect_identical(names(pages), sprintf("Treatment: %s (N=%d)", names(n_arm), as.integer(n_arm)))
  back <- tfl_as_table_spec(p, output_id = "T", compare = FALSE)
  expect_identical(back$layout$pages_heading, "Treatment: {value} (N={n})")
  expect_error(tfl_normalize_code(tfl_table_spec(tables = data.frame(output_id = "X", cols = "ARM",
                                                                     stack = "SEX")), output_id = "X"),
               "tables$stack", fixed = TRUE)
})

test_that("tables$stack: crossed layers (AGEGR1 * SEX), the slots, stack_sep (rtfreporter #619)", {
  skip_if_not("stack_sep" %in% names(formals(rtfreporter::normalize_ard)))
  line <- function(stack, sep = NA) tfl_normalize_code(tfl_table_spec(tables = data.frame(
    output_id = "X", cols = "ARM", stack = stack, stack_sep = sep)), output_id = "X")
  expect_identical(line("subgroup = ALL, SEX, AGEGR1 * SEX"),
                   "data <- normalize_ard(ard, stack = list(subgroup = list(\"ALL\", \"SEX\", c(\"AGEGR1\", \"SEX\"))))")
  expect_identical(line("subgroup = group2, group3"),
                   "data <- normalize_ard(ard, stack = list(subgroup = c(\"group2\", \"group3\")))")
  expect_identical(line("subgroup = SEX, AGEGR1 * SEX", "\" x \""),
                   "data <- normalize_ard(ard, stack = list(subgroup = list(\"SEX\", c(\"AGEGR1\", \"SEX\"))), stack_sep = \" x \")")
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  ard <- cards::bind_ard(cards::ard_tabulate(adsl, by = c(ARM, SEX), variables = DISCONFL),
                         cards::ard_tabulate(adsl, by = c(ARM, AGEGR1, SEX), variables = DISCONFL))
  data <- eval(parse(text = sub("normalize_ard", "rtfreporter::normalize_ard",
                                line("subgroup = SEX, AGEGR1 * SEX"), fixed = TRUE)))
  expect_identical(levels(data$subgroup), c("SEX", "AGEGR1 / SEX"))
})

test_that("tables$compare: comparison columns from the sheets, as plan_compare() (rtfreporter #625)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not("plan_compare" %in% getNamespaceExports("rtfreporter"))
  adsl <- cards::ADSL
  ard <- cards::bind_ard(
    cards::ard_stack(adsl, .by = ARM, cards::ard_summary(variables = AGE),
                     cards::ard_tabulate(variables = SEX)),
    cardx::ard_stats_kruskal_test(adsl, by = ARM, variables = AGE),
    cardx::ard_stats_chisq_test(adsl, by = ARM, variables = SEX))
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "DM", cols = "ARM", rows = "grp = variable",
                        label = "variable_level", compare = "p"),
    cells = data.frame(output_id = "DM", variable = c("continuous", "categorical", "p", "p"),
                       when = c(NA, NA, "p.value < 0.001", NA),
                       template = c("{mean:.1f}", "{n}", "<0.001", "{p.value:.3f}")))
  code <- paste(tfl_table_code(sp, "DM"), collapse = "\n")
  expect_match(code, "plan_compare(", fixed = TRUE)
  p <- tfl_table_plan(rtfreporter::normalize_ard(ard), sp, "DM")
  tab <- suppressMessages(rtfreporter::plan_apply(rtfreporter::plan_cells(p, notes = FALSE), "table"))
  tab <- as.data.frame(if (is.data.frame(tab)) tab else tab[[1L]])
  expect_true("p" %in% names(tab))
  expect_false(anyNA(tab$p[match(c("AGE", "SEX"), tab$grp)]))
  back <- tfl_as_table_spec(p, output_id = "DM", compare = FALSE)
  expect_identical(back$tables$compare, "p")
  expect_true(all(c("<0.001", "{p.value:.3f}") %in% back$cells$template[back$cells$variable %in% "p"]))
  expect_error(tfl_table_code(tfl_table_spec(tables = data.frame(output_id = "X", cols = "ARM", compare = "p")), "X"),
               "no `cells` row")
})

test_that("tables$compare: a column's lines from the cells rows' `row` (rtfreporter #625)", {
  skip_if_not("plan_compare" %in% getNamespaceExports("rtfreporter"))
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "ORR", cols = "ARM", compare = "diff | p"),
    cells = data.frame(output_id = "ORR", variable = c("diff", "diff", "p"), row = c("1", "2", "1"),
                       template = c("{estimate:.1f%}", "({conf.low:.1f%}, {conf.high:.1f%})", "{p.value:.3f}")))
  code <- paste(tfl_table_code(sp, "ORR"), collapse = "\n")
  expect_match(code, "diff = cell_rows(", fixed = TRUE)
  expect_match(code, "p = cell_rows(", fixed = TRUE)
})

test_that("tables$stack_nest: a crossed layer's outer level as a key of its own (rtfreporter #623)", {
  skip_if_not("stack_nest" %in% names(formals(rtfreporter::normalize_ard)))
  sp <- tfl_table_spec(tables = data.frame(output_id = "X", cols = "ARM",
                                           stack = "subgroup = SEX, AGEGR1 * SEX", stack_nest = "TRUE"))
  expect_identical(tfl_normalize_code(sp, output_id = "X"),
                   "data <- normalize_ard(ard, stack = list(subgroup = list(\"SEX\", c(\"AGEGR1\", \"SEX\"))), stack_nest = TRUE)")
  sp$tables$stack_nest <- NA
  expect_false(grepl("stack_nest", tfl_normalize_code(sp, output_id = "X")))
})

test_that("tables$show_keys and stat_groups' .variables / .others (rtfreporter #621)", {
  skip_if_not("show_keys" %in% names(formals(rtfreporter::table_plan)))
  sp <- tfl_table_spec(tables = data.frame(
    output_id = "T", cols = "TRT | .stat_group", rows = "block = variable | grp = subgroup | lvl = subgroup_level",
    label = "NA", show_keys = "SEX | AGEGR1",
    stat_groups = "n = N | orr_ci = n, estimate, conf.low, conf.high | .variables = RESP | .others = n"))
  code <- paste(tfl_table_code(sp, "T"), collapse = "\n")
  expect_match(code, "show_keys = c(\"SEX\", \"AGEGR1\")", fixed = TRUE)
  expect_match(code, ".variables = \"RESP\"", fixed = TRUE)
  expect_match(code, ".others = \"n\"", fixed = TRUE)
})

test_that("tables$keep_where / keep_any / keep_parents: plan_filter() from the sheet (rtfreporter #620)", {
  skip_if_not_installed("cards")
  skip_if_not("plan_filter" %in% getNamespaceExports("rtfreporter"))
  ard <- cards::ard_stack_hierarchical(cards::ADAE, variables = c(AESOC, AEDECOD), by = TRTA,
                                       denominator = cards::ADSL, id = USUBJID)
  d <- rtfreporter::normalize_ard(ard, hierarchy = c("AESOC", "AEDECOD"))
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "AE", cols = "TRTA", rows = "soc = AESOC | pt = AEDECOD",
                        label = "NA", keep_where = "p >= 0.05", keep_any = "FALSE"),
    cells = data.frame(output_id = "AE", variable = NA, template = "{n}"))
  code <- paste(tfl_table_code(sp, "AE"), collapse = "\n")
  expect_match(code, "plan_filter(p >= 0.05, any = FALSE)", fixed = TRUE)
  p <- tfl_table_plan(d, sp, "AE")
  by_spec <- suppressMessages(rtfreporter::plan_apply(p, "table"))
  by_code <- suppressMessages(rtfreporter::plan_apply(
    rtfreporter::table_plan(d, cols = "TRTA", rows = c(soc = "AESOC", pt = "AEDECOD"), label = NA) |>
      rtfreporter::plan_cells("{n}") |> rtfreporter::plan_filter(p >= 0.05, any = FALSE), "table"))
  expect_identical(by_spec, by_code)
  back <- tfl_as_table_spec(p, output_id = "AE", compare = FALSE)
  expect_identical(back$tables$keep_where, "p >= 0.05")
  expect_identical(back$tables$keep_any, "FALSE")
  expect_error(tfl_table_code(tfl_table_spec(tables = data.frame(output_id = "X", cols = "TRTA",
                                                                 keep_where = "p >= ")), "X"), "is not R")
})

test_that("tfl_table_code(): one pipe from the ARD, normalize_ard() its first step (#302)", {
  sp <- tfl_table_spec(tables = data.frame(
    output_id = c("T-1", "T-2"), cols = "TRTA", rows = c("group = variable", "group1 = AEBODSYS"),
    label = c(NA, "label = AEDECOD"),
    overall_label = c(NA, "Subjects with at least one TEAE")))
  ard <- tfl_ard_spec(list(
    datasets = data.frame(dataset = c("ADSL", "ADAE"), path = c("adsl.rds", "adae.rds")),
    analyses = data.frame(output_id = c("T-1", "T-2"), analysis_id = c("AGE", "AE"),
                          method = c("continuous", "hierarchical"),
                          dataset = c("ADSL", "ADAE"), by = "TRTA",
                          variables = c("AGE", "AEBODSYS | AEDECOD"),
                          args = c(NA, "over_variables = TRUE"))))
  c1 <- tfl_table_code(sp, "T-1", pipe = "|>")
  expect_identical(c1[1:3], c("plan <- ard |>", "  normalize_ard() |>",
                              "  table_plan(cols = \"TRTA\", rows = c(group = \"variable\"))"))
  flat <- function(x) gsub("[[:space:]]+", " ", paste(x, collapse = " "))
  c2 <- tfl_table_code(sp, "T-2", pipe = "|>", ard_spec = ard)
  expect_match(flat(c2), paste0("plan <- ard |> normalize_ard( hierarchy = c(\"AEBODSYS\", \"AEDECOD\"), ",
                                "overall = \"Subjects with at least one TEAE\" ) |> table_plan("),
               fixed = TRUE)
  # no ARD definition: the hierarchy its rows and label say, as the spec files take it
  expect_identical(tfl_table_code(sp, "T-2", pipe = "|>"), c2)
  # the program normalizes the ARD itself
  c3 <- tfl_table_code(sp, "T-1", data = "data", pipe = "|>", normalize = FALSE)
  expect_match(c3[1L], "^plan <- table_plan[(]data, cols")
})

test_that("plan_levels()'s names: variable, the column keys, the row keys, then the variables (#302)", {
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T", cols = "TRT01A", rows = "group = variable"),
    variables = data.frame(variable = c("SEX", "TRT01A", "AGEGR1"), order = c(1, NA, 2),
                           levels = c("F | M", "Drug X | Placebo", "<65 | >=65")))
  code <- paste(tfl_table_code(sp, "T", pipe = "|>"), collapse = "\n")
  expect_match(code, paste0("plan_levels(\n    variable = c(\"SEX\", \"AGEGR1\"),\n",
                            "    TRT01A = cl_trt01a,\n    SEX = cl_sex,\n    AGEGR1 = cl_agegr1"),
               fixed = TRUE)
})

test_that("plan_levels() the ARD already says is left out of the program and the run (the owner, 10/18)", {
  cl <- list(TRT01A = list(list(value = "A"), list(value = "B")),
             SEX = list(list(value = "F", label = "Female"), list(value = "M", label = "Male")))
  ard <- list(analyses = list(
    list(id = "N", method = "categorical", data = "adsl_saf", variables = "TRT01A"),
    list(id = "CONT", method = "continuous", data = "adsl_saf", by = "TRT01A", variables = "AGE"),
    list(id = "CAT", method = "categorical", data = "adsl_saf", by = "TRT01A", variables = "SEX")))
  lv <- function(args) tfl_output("T-1", "table", codelists = cl, ard = ard, content = list(
    tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable"))),
    tfl_item("plan_levels", args)))
  code <- function(o) paste(tfl_content_code(o, pipe = "|>"), collapse = "\n")
  # the code lists and the variables in the ARD's order: nothing to say
  c1 <- code(lv(list(variable = c("AGE", "SEX"), TRT01A = c("A", "B"), SEX = c("Female", "Male"))))
  expect_false(grepl("plan_levels", c1, fixed = TRUE))
  expect_false(grepl("cl_", c1, fixed = TRUE))
  # a report's own order is kept: another order of the values, of the variables
  c2 <- code(lv(list(variable = c("SEX", "AGE"), SEX = c("Male", "Female"))))
  expect_match(c2, 'variable = c("SEX", "AGE")', fixed = TRUE)
  expect_match(c2, "SEX = cl_sex", fixed = TRUE)
  # the first variables in the ARD's order (the rest after them): nothing to say
  expect_false(grepl("plan_levels", code(lv(list(variable = "AGE"))), fixed = TRUE))
  # an analysis that is not cards' (cardx's survfit names the arms from the strata): kept
  ard$analyses[[2L]]$method <- "cardx::ard_survival_survfit"
  o3 <- tfl_output("T-1", "table", codelists = cl, ard = ard, content = lv(list(TRT01A = c("A", "B")))$content)
  expect_match(code(o3), "TRT01A = cl_trt01a", fixed = TRUE)
  # .drop_empty alone stays
  expect_match(code(lv(list(TRT01A = c("A", "B"), .drop_empty = "SEX"))), ".drop_empty", fixed = TRUE)
})

test_that("a named order (plan_levels(SEX = c(F = \"Female\"))): the schema, the code, the sheets both ways; never pruned", {
  # the schema takes a map (value: text) as well as a sequence
  expect_identical(nrow(tfl_schema_check(tfl_item("plan_levels", list(SEX = list(F = "Female", M = "Male"))))), 0L)
  o <- tfl_output("T-1", "table", content = list(
    tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable"))),
    tfl_item("plan_levels", list(SEX = list(M = "Male", F = "Female"), variable = list(AGE = "Age (years)", SEX = "SEX")))))
  code <- paste(tfl_content_code(o, pipe = "|>"), collapse = "\n")
  expect_match(code, 'cl_sex <- c(M = "Male", F = "Female")', fixed = TRUE)
  expect_match(code, 'variable = c(AGE = "Age (years)", SEX = "SEX")', fixed = TRUE)
  # the sheets: `M = Male | F = Female` in the levels cell, a heading from the order's name
  sh <- .plan_sheets_from_content(o$content, "T-1")
  expect_identical(sh$variables$levels[sh$variables$variable == "SEX"], "M = Male | F = Female")
  expect_identical(sh$variables$label[sh$variables$variable == "AGE"], "Age (years)")
  lv <- tflspec:::.ard_spec_levels(sh)
  expect_identical(lv$SEX, c(M = "Male", F = "Female"))
  # a code list's order given with text is a change the ARD does not say: kept
  cl <- list(SEX = list(list(value = "M", label = "Male"), list(value = "F", label = "Female")))
  ard <- list(analyses = list(list(id = "CAT", method = "categorical", data = "adsl", by = "TRT01A", variables = "SEX")))
  p <- tfl_output("T-1", "table", codelists = cl, ard = ard, content = list(
    tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable"))),
    tfl_item("plan_levels", list(SEX = list(Male = "Men", Female = "Women")))))
  expect_match(paste(tfl_content_code(p), collapse = "\n"), 'Male = "Men"', fixed = TRUE)
})
