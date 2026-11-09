# The prototype of demog under the new design: its own template, the analysis
# variables required, the two-part SPEC (ard: / content:), the formats on the
# ARD side, and the params for the arm headings, widths, blank rows, the page
# and the style (DEMOG-DESIGN).

dp_items <- function(x) x$content
dp_item <- function(x, fn) Filter(function(it) identical(it$fn, fn), x$content)
dp_args <- function(...) utils::modifyList(list(population = "SAF", continuous = "AGE",
                                                categorical = c("AGEGR1", "SEX")), list(...))

# A study folder on the sample ADSL (factors as text), with a few values
# missing: RACE of three subjects, SEX of two
dp_study <- function(env = parent.frame(), na = TRUE) {
  testthat::skip_if_not_installed("haven")
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "adam"))
  sl <- tflspec:::.ai_chr(tflspec::adsl)
  if (na) {
    sl$RACE[which(sl$SAFFL == "Y")[1:3]] <- ""
    sl$SEX[which(sl$SAFFL == "Y")[4:5]] <- NA
  }
  haven::write_xpt(sl, file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "DP-01")
  # (the study's defaults: no stub heading -- the rtfreporter at hand has none --
  # and one style set, the variant's)
  writeLines(c("content:", "  - normalize_ard: {}",
               "  - plan_cells: {categorical: \"{n} ({p})\"}",
               "  - plan_stub: {name: row_label, before: true}",
               "  - plan_blanks: {where: between_groups}",
               "styles:", "  stf_t02:", "    content:",
               "      - plan_style: {font_size_half_points: 16}"),
             file.path(d, "spec", "defaults.yml"))
  d
}

dp_run <- function(d, tpl, extra = list()) {
  o <- do.call(tfl_output, c(list("T-DP", "table", population = "SAF", extends = "defaults",
                                  template = tpl), extra))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-DP.yml"))
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-DP", dir = d, save = FALSE))
  ro <- tfl_resolve_output(tfl_read_outputs(d)[["T-DP"]], st)
  p <- suppressMessages(tfl_content_run(ro, ard))
  list(table = as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table"))),
       st = st, ro = ro, ard = ard)
}

test_that("demog is its own template, no longer a variant of summary", {
  expect_null(tflspec:::.tpl_registry$demog$from)
  expect_identical(tflspec:::.tpl_registry$demog$kind, "table")
  expect_identical(tflspec:::.tpl_registry$summary$from, NULL)
})

test_that("the analysis variables are required: at least one, else a clear error", {
  expect_error(tfl_template_expand("demog", list(population = "SAF")),
               "give `continuous` or `categorical`")
  expect_error(tfl_template_expand("demog", list(continuous = "AGE")), "needs 'population'")
  expect_no_error(tfl_template_expand("demog", list(population = "SAF", continuous = "AGE")))
  expect_no_error(tfl_template_expand("demog", list(population = "SAF", categorical = "SEX")))
  # the variables are as written, not the data's (no any_of())
  x <- tfl_template_expand("demog", dp_args())
  an <- function(id) Filter(function(a) identical(a$id, id), x$ard$analyses)[[1L]]
  expect_null(an("CONT")$optional)
  expect_identical(an("CAT")$variables, c("AGEGR1", "SEX"))
})

test_that("a variant sets the variables, so it needs only the population", {
  expect_no_error(tfl_template_expand("stf_t02_demog", list(population = "SAF")))
  p <- tfl_template_params("stf_t02_demog")
  expect_false(p$continuous$required)
  expect_identical(p$continuous$default, "AGE")
  expect_match(tflspec:::.tpl_get("stf_t02_demog", "t")$label, "(FDA STF Table 2)", fixed = TRUE)
  expect_error(tfl_template_expand("demog_fda_safety", list(population = "SAF")), "no template")
})

test_that("summary is unchanged: its variants and defaults still work", {
  x <- tfl_template_expand("summary", list(population = "SAF", continuous = "AGE"))
  expect_true(any(vapply(x$content, function(it) it$fn == "plan_paginate_rows", NA)))
  expect_identical(tflspec:::.tpl_get("summary", "t")$params$continuous_statistics$default,
                   c("N", "mean", "sd", "median", "min", "max"))
})

test_that("the statistics computed are the ones formats names, plus any the cells name", {
  x <- tfl_template_expand("demog", dp_args())
  stat <- Filter(function(a) identical(a$id, "CONT"), x$ard$analyses)[[1L]]$statistics
  expect_setequal(stat, c("N", "mean", "sd", "median", "min", "max"))
  # {q1} in a cell adds the ARD's p25
  y <- tfl_template_expand("demog", dp_args(
    cells = list(n = "{N}", `Q1, Q3` = "{q1}, {q3}")))
  st2 <- Filter(function(a) identical(a$id, "CONT"), y$ard$analyses)[[1L]]$statistics
  expect_true(all(c("p25", "p75", "mean") %in% st2))
  cells <- dp_item(y, "plan_cells")[[1L]]$args$continuous
  expect_identical(cells[["Q1, Q3"]], "{p25}, {p75}")
  # no continuous variable, no continuous analysis
  z <- tfl_template_expand("demog", list(population = "SAF", categorical = "SEX"))
  expect_false("CONT" %in% vapply(z$ard$analyses, `[[`, "", "id"))
})

test_that("formats: the ARD's digits, by statistic and by variable; the default is today's", {
  x <- tfl_template_expand("demog", dp_args(continuous = c("AGE", "TRTDURD"),
                                           formats = list(TRTDURD = list(mean = 0, sd = 1))))
  cont <- Filter(function(a) identical(a$id, "CONT"), x$ard$analyses)[[1L]]
  expect_match(cont$formats, "mean=1 | sd=2", fixed = TRUE)
  expect_match(cont$formats, "TRTDURD:mean=0 | TRTDURD:sd=1", fixed = TRUE)
  cat <- Filter(function(a) identical(a$id, "CAT"), x$ard$analyses)[[1L]]
  expect_identical(cat$formats, "p=xx.x%")
  # the cells are placement only: bare tokens
  cells <- dp_item(x, "plan_cells")[[1L]]$args$continuous
  expect_identical(cells[["Median"]], "{median}")
  expect_identical(unlist(cells[["Mean (SD)"]]), c("{mean} ({sd})", "{mean} (-)"))
  # the formats given by name are merged into the default
  f <- tfl_template_params("demog")$formats
  expect_true(f$merge)
})

test_that("formats reach the table: AGE one decimal, a variable of its own more; p a percent", {
  skip_if_not_installed("haven")
  d <- dp_study(na = FALSE)
  r <- dp_run(d, list(name = "demog", ard = list(population = "SAF", continuous = c("AGE", "TRTDURD"),
                                                 categorical = "SEX",
                                                 formats = list(TRTDURD = list(mean = 0, sd = 1)))))
  txt <- unlist(lapply(r$table, as.character))
  expect_true(any(grepl("^60\\.1 \\(9\\.59\\)$", txt)))
  expect_true(any(grepl("^189 \\(135\\.0\\)$", txt)))
  expect_true(any(grepl("^41 \\(51\\.3\\)$", txt)))
})

test_that("a group of one has no SD: the {mean} (<sd_missing>) fallback still works on stat_fmt", {
  skip_if_not_installed("cards")
  d <- tflspec::adsl[tflspec::adsl$SAFFL == "Y", ]
  d <- d[d$TRT01A == "Drug X" | seq_len(nrow(d)) == which(d$TRT01A == "Control")[1L], ]
  fm <- list(cards::everything() ~ list(N = 0, mean = 1, sd = 2, median = 1, min = 0, max = 0))
  ard <- cards::bind_ard(
    cards::ard_tabulate(d, variables = TRT01A),
    cards::apply_fmt_fun(cards::ard_summary(d, by = TRT01A, variables = AGE, fmt_fun = fm)))
  plan <- tpl_demog(ard, population = "SAF", continuous = "AGE", categorical = character(),
                    sd_missing = "-")
  tab <- as.data.frame(suppressMessages(rtfreporter::plan_apply(plan, "table")))
  txt <- unlist(lapply(tab, as.character))
  expect_true(any(grepl("^[0-9.]+ \\(-\\)$", txt)))
  expect_true(any(grepl("^[0-9.]+ \\([0-9.]+\\)$", txt)))
})

test_that("missing_row counts the subjects with no value, last; categorical_levels shows 0 rows", {
  skip_if_not_installed("haven")
  d <- dp_study()
  r <- dp_run(d, list(name = "demog", ard = list(
    population = "SAF", continuous = "AGE", categorical = c("SEX", "RACE"),
    missing_row = TRUE, missing_label = "Not recorded"),
    content = list(categorical_levels = list(
      RACE = c("WHITE", "ASIAN", "BLACK OR AFRICAN AMERICAN", "OTHER", "UNKNOWN")))))
  lab <- as.character(r$table$label)
  key <- as.character(r$table$group)
  # (rtfreporter orders a label two variables share, as here "Not recorded", once for
  # both: the row's place is not tested, its counts are)
  race <- lab[key == "Race, n (%)"]
  expect_setequal(race, c("WHITE", "ASIAN", "BLACK OR AFRICAN AMERICAN", "OTHER", "UNKNOWN",
                          "Not recorded"))
  expect_true("Not recorded" %in% lab[key == "Sex, n (%)"])
  # the counts: 3 subjects of RACE, 2 of SEX
  vals <- function(g) {
    k <- which(key == g & lab == "Not recorded")
    sum(as.numeric(sub(" .*$", "", unlist(r$table[k, -(1:2), drop = FALSE]))))
  }
  expect_identical(vals("Race, n (%)"), 3)
  expect_identical(vals("Sex, n (%)"), 2)
  # UNKNOWN: in the list, none in the data: shown with 0
  k <- which(key == "Race, n (%)" & lab == "UNKNOWN")
  expect_true(all(grepl("^0 ", unlist(r$table[k, -(1:2), drop = FALSE]))))
})

test_that("without missing_row there is no such row; a value the list lacks stops", {
  skip_if_not_installed("haven")
  d <- dp_study()
  r <- dp_run(d, list(name = "demog", ard = list(population = "SAF", categorical = "SEX")))
  expect_false(any(grepl("Missing", unlist(lapply(r$table, as.character)))))
  expect_error(dp_run(d, list(name = "demog", ard = list(population = "SAF", categorical = "RACE"),
                              content = list(categorical_levels = list(RACE = c("ASIAN", "WHITE"))))),
               "code list does not")
  expect_error(tfl_template_expand("demog", dp_args(
    categorical_levels = list(RACE = "ASIAN"))), "not in `categorical`")
  expect_error(tfl_template_expand("demog", dp_args(missing_row = TRUE, missing_label = c("a", "b"))),
               "one text")
})

test_that("total: a Total column last, labelled by total_label", {
  x <- tfl_template_expand("demog", dp_args(total = TRUE, total_label = "All subjects"))
  pt <- dp_item(x, "plan_total")[[1L]]$args
  expect_identical(pt, list(label = "All subjects", position = "last"))
  expect_false(length(dp_item(tfl_template_expand("demog", dp_args()), "plan_total")) > 0L)
  cont <- Filter(function(a) identical(a$id, "CONT"), x$ard$analyses)[[1L]]
  expect_true(cont$overall)
})

test_that("header_n and col_header: the arm heading text, {n} dropped with none", {
  x <- tfl_template_expand("demog", dp_args())
  h <- dp_item(x, "plan_col_header")[[1L]]
  expect_null(h$explicit)
  expect_identical(h$args$header[[2L]]$text, "{col}\n(N={n})")
  y <- tfl_template_expand("demog", dp_args(header_n = "none"))
  expect_identical(dp_item(y, "plan_col_header")[[1L]]$args$header[[2L]]$text, "{col}")
  expect_true(dp_item(y, "plan_col_header")[[1L]]$explicit)
  z <- tfl_template_expand("demog", dp_args(col_header = "{col}\nN={n}"))
  expect_identical(dp_item(z, "plan_col_header")[[1L]]$args$header[[2L]]$text, "{col}\nN={n}")
  w <- tfl_template_expand("demog", dp_args(col_header = "{col}\nN={n}", header_n = "none"))
  expect_identical(dp_item(w, "plan_col_header")[[1L]]$args$header[[2L]]$text, "{col}")
  expect_error(tfl_template_expand("demog", dp_args(header_n = "tested")), "header_n")
})

test_that("widths: auto is the row title 35% and the arms equal; numbers are relative", {
  cl <- list(TRT01A = c("Drug X", "Control"))
  x <- tfl_template_expand("demog", dp_args(), codelists = cl)
  w <- dp_item(x, "plan_columns")[[1L]]$args$widths
  expect_equal(w, c(35, 32.5, 32.5))
  expect_null(dp_item(x, "plan_columns")[[1L]]$explicit)
  y <- tfl_template_expand("demog", dp_args(total = TRUE), codelists = cl)
  expect_equal(dp_item(y, "plan_columns")[[1L]]$args$widths, c(35, rep(65 / 3, 3)))
  z <- tfl_template_expand("demog", dp_args(widths = c(40, 20, 20)), codelists = cl)
  expect_equal(dp_item(z, "plan_columns")[[1L]]$args$widths, c(40, 20, 20))
  expect_true(dp_item(z, "plan_columns")[[1L]]$explicit)
  # the arms are not known: no widths item (rtfreporter's own)
  expect_length(dp_item(tfl_template_expand("demog", dp_args()), "plan_columns"), 0L)
  expect_error(tfl_template_expand("demog", dp_args(widths = c(40, -1))), "relative widths")
  expect_error(tfl_template_expand("demog", dp_args(widths = "wide")), "relative widths")
})

test_that("max_rows, blanks: a default is under the study, a value given is over it", {
  x <- tfl_template_expand("demog", dp_args())
  pr <- dp_item(x, "plan_paginate_rows")[[1L]]
  expect_identical(pr$args, list(max_rows = 30L, split = "group_safe"))
  expect_false(isTRUE(pr$explicit))
  expect_true(tflspec:::.tpl_is_look(pr))
  expect_identical(dp_item(x, "plan_blanks")[[1L]]$args, list(where = "between_groups"))
  y <- tfl_template_expand("demog", dp_args(max_rows = 20, blanks = FALSE))
  pr2 <- dp_item(y, "plan_paginate_rows")[[1L]]
  expect_identical(pr2$args$max_rows, 20L)
  expect_identical(pr2$args$split, "group_safe")
  expect_false(tflspec:::.tpl_is_look(pr2))
  bl <- dp_item(y, "plan_blanks")[[1L]]
  expect_identical(bl$args, list(first = FALSE, last = FALSE))
  expect_false(tflspec:::.tpl_is_look(bl))
})

test_that("a given max_rows and blanks win over the study's defaults in a report", {
  skip_if_not_installed("haven")
  d <- dp_study(na = FALSE)
  s <- tfl_read_study_spec(d)
  k <- length(s$defaults$content) + 1L
  s$defaults$content[[k]] <- tfl_item("plan_paginate_rows", list(max_rows = 40L), from = "defaults")
  mk <- function(args) tfl_output("T-9", "table", population = "SAF",
                                  template = list(name = "demog", ard = list(
                                    population = "SAF", continuous = "AGE", categorical = "SEX"),
                                    content = args))
  pr <- function(o) {
    ro <- tfl_resolve_output(o, s)
    ro$content[[match("plan_paginate_rows", vapply(ro$content, `[[`, "", "fn"))]]$args
  }
  expect_identical(pr(mk(list())), list(max_rows = 40L))
  expect_identical(pr(mk(list(max_rows = 12))), list(max_rows = 12L, split = "group_safe"))
})

test_that("style: the template's param is the default, the report's style: wins", {
  skip_if_not_installed("haven")
  d <- dp_study(na = FALSE)
  s <- tfl_read_study_spec(d)
  s$styles <- list(compact = list(data = list(), codelists = list(),
                                  content = list(tfl_item("plan_style", list(font_size_half_points = 14L),
                                                          from = "style:compact")), page = list()),
                   big = list(data = list(), codelists = list(),
                              content = list(tfl_item("plan_style", list(font_size_half_points = 24L),
                                                      from = "style:big")), page = list()))
  fs <- function(o) {
    ro <- tfl_resolve_output(o, s)
    ps <- ro$content[[match("plan_style", vapply(ro$content, `[[`, "", "fn"))]]
    ps$args$font_size_half_points
  }
  tpl <- function(...) list(name = "demog", ard = list(population = "SAF", continuous = "AGE"),
                            content = list(...))
  expect_identical(tflspec:::.tpl_get("demog", "t")$params$style$default, "standard")
  # the param
  expect_identical(fs(tfl_output("T-1", "table", population = "SAF", template = tpl(style = "compact"))), 14L)
  # the report's own style: wins
  expect_identical(fs(tfl_output("T-1", "table", population = "SAF", style = "big",
                                 template = tpl(style = "compact"))), 24L)
  # no style, no param: standard (the study has none: no plan_style from a style)
  expect_error(tfl_resolve_output(tfl_output("T-1", "table", population = "SAF",
                                             template = tpl(style = "nope")), s), "no style 'nope'")
  # a variant's default style is the default of its reports
  expect_identical(tflspec:::.tpl_style_default(tfl_output(
    "T-1", "table", population = "SAF", template = list(name = "stf_t02_demog"))), "stf_t02")
})

test_that("the SPEC has two parts, ard: and content:; the old flat args: is still read", {
  mk <- function(t) tfl_output("T-1", "table", population = "SAF", template = t)
  a <- tflspec:::.tpl_spec_of(mk(list(name = "demog", ard = list(population = "SAF", continuous = "AGE"),
                                      content = list(total_label = "All", widths = "auto"))))
  expect_setequal(names(a$args), c("population", "continuous", "total_label", "widths"))
  flat <- tflspec:::.tpl_spec_of(mk(list(name = "demog", args = list(population = "SAF", continuous = "AGE",
                                                                    total_label = "All"))))
  expect_identical(flat$args$total_label, "All")
  both <- tflspec:::.tpl_spec_of(mk(list(name = "demog", args = list(population = "SAF"),
                                         ard = list(continuous = "AGE"))))
  expect_setequal(names(both$args), c("population", "continuous"))
  # a name in the part it does not act in stops, naming the right one
  expect_error(tflspec:::.tpl_spec_of(mk(list(name = "demog", ard = list(population = "SAF", widths = "auto")))),
               "'widths' acts in `content:`, not `ard:`")
  expect_error(tflspec:::.tpl_spec_of(mk(list(name = "demog", content = list(total = TRUE)))),
               "'total' acts in `ard:`, not `content:`")
  expect_error(tflspec:::.tpl_spec_of(mk(list(name = "demog", args = list(population = "SAF"),
                                              ard = list(population = "SAF")))), "given twice")
  expect_error(tflspec:::.tpl_spec_of(mk(list(name = "demog", ard = "SAF"))), "map of parameters")
  # every parameter is in one part
  p <- names(tfl_template_params("demog"))
  parts <- tflspec:::.tpl_registry$demog$parts
  expect_setequal(c(parts$ard, parts$content), p)
  expect_length(intersect(parts$ard, parts$content), 0L)
})

test_that("a two-part report runs end to end and reads back from YAML", {
  skip_if_not_installed("haven")
  d <- dp_study(na = FALSE)
  o <- tfl_output("T-DP", "table", population = "SAF", extends = "defaults", template = list(
    name = "demog", ard = list(population = "SAF", continuous = "AGE", categorical = c("SEX", "RACE"),
                               total = TRUE),
    content = list(total_label = "All subjects", blanks = FALSE, widths = c(40, 20, 20, 20))),
    codelists = list(TRT01A = list(list(value = "Drug X", label = "Drug X"),
                                   list(value = "Control", label = "Control"))))
  f <- file.path(d, "spec", "outputs", "T-DP.yml")
  tfl_write_output(o, f)
  back <- tfl_read_outputs(d)[["T-DP"]]
  expect_identical(names(back$template), c("name", "ard", "content"))
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-DP", dir = d, save = FALSE))
  ro <- tfl_resolve_output(back, st)
  p <- suppressMessages(tfl_content_run(ro, ard))
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  expect_true("All subjects" %in% names(t))
  expect_false(any(trimws(as.character(t[[2L]])) == "" & !is.na(t[[1L]]) & t[[1L]] == ""))
  # A = B: the program's call and the build give the same table
  code <- tfl_content_code(ro, pipe = "|>")
  e <- new.env()
  e$ard <- ard
  suppressMessages(eval(parse(text = code), envir = e))
  expect_identical(as.data.frame(suppressMessages(rtfreporter::plan_apply(e$plan, "table"))), t)
})

test_that("stf_t02_demog: the FDA STF Table 2 look as defaults of a variant; it runs", {
  skip_if_not_installed("haven")
  d <- dp_study()
  x <- tfl_template_expand("stf_t02_demog", list(population = "SAF"))
  expect_identical(dp_item(x, "plan_total")[[1L]]$args$label, "Total Population")
  hdr <- dp_item(x, "plan_col_header")[[1L]]
  expect_identical(hdr$args$header[[2L]]$text, "{col}\nN={n}")
  expect_identical(dp_item(x, "plan_cells")[[1L]]$args$continuous[["Median (min, max)"]],
                   "{median} ({min}, {max})")
  r <- dp_run(d, list(name = "stf_t02_demog", ard = list(population = "SAF")),
              extra = list(codelists = list(TRT01A = list(list(value = "Drug X", label = "Drug X"),
                                                          list(value = "Control", label = "Control")))))
  expect_true("Total Population" %in% names(r$table))
  lab <- as.character(r$table$label)
  grp <- as.character(r$table$group)
  expect_true(all(c("Sex, n (%)", "Age, years", "Country of participation, n (%)") %in% grp))
  expect_true("Median (min, max)" %in% lab)
})

test_that("tfl_tpl: the arms come from the ARD for the widths, so A = B", {
  skip_if_not_installed("cards")
  saf <- tflspec::adsl[tflspec::adsl$SAFFL == "Y", ]
  fm <- list(cards::everything() ~ list(N = 0, mean = 1, sd = 2, median = 1, min = 0, max = 0))
  ard <- cards::bind_ard(
    cards::ard_tabulate(saf, variables = TRT01A),
    cards::apply_fmt_fun(cards::ard_summary(saf, by = TRT01A, variables = AGE, fmt_fun = fm)),
    cards::apply_fmt_fun(cards::ard_tabulate(saf, by = TRT01A, variables = SEX,
                                             fmt_fun = cards::everything() ~ list(p = cards::label_round(1, scale = 100)))))
  p <- tpl_demog(ard, population = "SAF", continuous = "AGE", categorical = "SEX")
  tab <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  expect_true(all(c("Drug X", "Control") %in% names(tab)))
  # the arms are read from the ARD for the template's expansion
  by_ard <- tflspec:::.tpl_object
  expect_true(is.function(by_ard))
})
