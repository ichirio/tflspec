# B-5, part 1: the writers of the From form (OBJECTS.md B-5): an analysis
# item's `name:` and `from:` in the ARD program and on the analyses sheet,
# and a figure plot's `from:` in the figure code.  Today's keys write
# today's code (the existing tests and goldens are that check).

# ---- the analyses, from the sheet -------------------------------------------

b5_base <- function() list(analysis_id = "A", method = "categorical", population_id = "SAF",
                           by = "TRTA", variables = "SEX")
b5_age <- function(...) c(list(analysis_id = "AGE", method = "continuous", by = "TRTA",
                               variables = "AGE", from = "adsl_sub"), list(...))

# the spec of rows (the first has the analysis set, so `adsl_sub` is made)
b5_spec <- function(...) {
  rows <- list(...)
  sp <- exact_spec(rows[[1L]])
  sp$analyses <- do.call(rbind, lapply(rows, function(r) exact_spec(r)$analyses))
  sp
}
b5_fit <- function(...) c(list(analysis_id = "FIT", type = "model", method = "stats::lm",
                               args = "formula = AGE ~ SEX", from = "adsl_sub"), list(...))
b5_code <- function(sp) paste(tfl_ard_code(sp, output_id = "T", save = FALSE), collapse = "\n")

test_that("the analyses sheet has the columns name and from, blank for an item without them", {
  expect_true(all(c("name", "from") %in% .ard_spec_sheets$analyses))
  expect_identical(utils::tail(.ard_spec_sheets$analyses, 2L), c("name", "from"))
  sp <- b5_spec(b5_base())
  expect_true(all(is.na(sp$analyses$name)) && all(is.na(sp$analyses$from)))
  # a workbook written before the columns existed reads (they are blank)
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  tfl_write_ard_spec(sp, f)
  expect_true(all(c("name", "from") %in% names(readxl::read_excel(f, "analyses"))))
  old <- unclass(sp)
  old$analyses <- old$analyses[setdiff(names(old$analyses), c("name", "from"))]
  openxlsx_book <- openxlsx::createWorkbook()
  for (s in names(old)) {
    openxlsx::addWorksheet(openxlsx_book, s)
    openxlsx::writeData(openxlsx_book, s, old[[s]], keepNA = FALSE)
  }
  openxlsx::saveWorkbook(openxlsx_book, f, overwrite = TRUE)
  expect_false("name" %in% names(readxl::read_excel(f, "analyses")))
  back <- tfl_read_ard_spec(f)
  expect_true(all(c("name", "from") %in% names(back$analyses)))
  expect_true(all(is.na(back$analyses$name)) && all(is.na(back$analyses$from)))
})

test_that("an item without name and from is written as before, with the columns or without them", {
  without <- b5_spec(b5_base(), list(analysis_id = "AGE", method = "continuous", by = "TRTA",
                                     variables = "AGE", population_id = "SAF"))
  txt <- b5_code(without)
  old <- without
  old$analyses <- old$analyses[setdiff(names(old$analyses), c("name", "from"))]
  expect_identical(b5_code(old), txt)
  expect_match(txt, "bind_items(.ard_a, .ard_age)", fixed = TRUE)
})

test_that("`from:` writes `name <- from |> method(...)`", {
  txt <- b5_code(b5_spec(b5_base(), b5_age()))
  expect_match(txt, "\n.ard_age <- adsl_sub |>\n  ard_summary(", fixed = TRUE)
  expect_match(txt, "tag_ard(report_id, \"AGE\")", fixed = TRUE)
  expect_match(txt, "bind_items(.ard_a, .ard_age)", fixed = TRUE)
  # the first argument is the pipe's: no `data = ` and no copy of the data
  expect_false(grepl("ard_summary(adsl_sub", txt, fixed = TRUE))
  expect_false(grepl("data = adsl_sub", txt, fixed = TRUE))
})

test_that("`name:` gives the item's R name; the others keep the default", {
  txt <- b5_code(b5_spec(utils::modifyList(b5_base(), list(name = "sex_ard")),
                         list(analysis_id = "AGE", method = "continuous", by = "TRTA",
                              variables = "AGE", population_id = "SAF")))
  expect_match(txt, "\nsex_ard <- adsl_sub |>\n", fixed = TRUE)
  expect_match(txt, "\n.ard_age <- adsl_sub |>\n", fixed = TRUE)
  expect_match(txt, "bind_items(sex_ard, .ard_age)", fixed = TRUE)
  expect_false(grepl(".ard_a <-", txt, fixed = TRUE))
})

test_that("`name:` and `from:` together", {
  txt <- b5_code(b5_spec(b5_base(), b5_age(name = "age_ard")))
  expect_match(txt, "\nage_ard <- adsl_sub |>\n  ard_summary(", fixed = TRUE)
  expect_match(txt, "bind_items(.ard_a, age_ard)", fixed = TRUE)
  # a default name is not given twice: the item's own name is taken
  twice <- b5_code(b5_spec(utils::modifyList(b5_base(), list(name = ".ard_age")), b5_age()))
  expect_match(twice, "\n.ard_age <- adsl_sub |>\n  mutate(", fixed = TRUE)
  expect_match(twice, "\n.ard_age_1 <- adsl_sub |>\n  ard_summary(", fixed = TRUE)
})

test_that("a model item with `from:` writes `name <- from |> method(args)`", {
  fit <- b5_fit()
  txt <- b5_code(b5_spec(b5_base(), fit))
  expect_match(txt, "# FIT: a model, kept as fit (not in the ARD)\nfit <- adsl_sub |>\n  (stats::)?lm\\(formula = AGE ~ SEX\\)")
  expect_match(txt, "ard <- .ard_a", fixed = TRUE)
  named <- b5_code(b5_spec(b5_base(), utils::modifyList(fit, list(name = "fit_age"))))
  expect_match(named, "\nfit_age <- adsl_sub |>\n  (stats::)?lm\\(")
  # without `from:`, `name:` gives the name only: today's `data = ` form
  plain <- b5_code(b5_spec(b5_base(), utils::modifyList(fit, list(from = NULL, name = "fit_age", args = "formula = AGE ~ SEX",
                                                                  population_id = "SAF"))))
  expect_match(plain, "\nfit_age <- (stats::)?lm\\(formula = AGE ~ SEX, data = ")
})

test_that("an item of several calls (ards:) pipes each call from `from:` and is named by `name:`", {
  calls <- list(list(by = "TRTA", variables = "SEX"), list(by = "TRTA", variables = "RACE"))
  sp <- b5_spec(utils::modifyList(b5_base(), list(from = "adsl_sub")))
  sp$analyses$ards <- list(calls)
  txt <- b5_code(sp)
  expect_match(txt, "\n.ard_a_1 <- adsl_sub |>\n", fixed = TRUE)
  expect_match(txt, "\n.ard_a_2 <- adsl_sub |>\n", fixed = TRUE)
  expect_match(txt, ".ard_a <- bind_items(.ard_a_1, .ard_a_2)", fixed = TRUE)
  sp$analyses$name <- "cats"
  txt2 <- b5_code(sp)
  expect_match(txt2, "\ncats_1 <- adsl_sub |>\n", fixed = TRUE)
  expect_match(txt2, "\ncats <- bind_items(cats_1, cats_2)", fixed = TRUE)
  expect_match(txt2, "ard <- cats", fixed = TRUE)
})

test_that("the program runs: an item with `from:` and `name:` makes the ARD", {
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  sp <- b5_spec(b5_base(), b5_age(name = "age_ard"), b5_fit(name = "fit_age"))
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, output_id = "T", save = FALSE))
  saf <- adam$ADSL[adam$ADSL$SAFFL == "Y", ]
  expect_setequal(unique(ard$analysis_id), c("A", "AGE"))
  n <- ard[ard$analysis_id == "AGE" & ard$stat_name == "N" & ard$variable == "AGE", ]
  expect_identical(as.integer(sum(unlist(n$stat))), nrow(saf))
  # the same numbers as the item that makes its own data
  own <- b5_spec(b5_base(), list(analysis_id = "AGE", method = "continuous", by = "TRTA",
                                 variables = "AGE", population_id = "SAF"))
  ard0 <- suppressMessages(tfl_build_ard(own, dir = dir, output_id = "T", save = FALSE))
  key <- function(x) exact_numbers(x[x$analysis_id == "AGE", ])
  expect_identical(key(ard), key(ard0))
  expect_true(grepl("\nfit_age <- adsl_sub |>", b5_code(sp), fixed = TRUE))
})

test_that("`from:` with dataset or where stops; so does a nested item with name or from", {
  for (k in list(list(dataset = "ADSL"), list(where = "AGE > 1"))) {
    sp <- b5_spec(b5_base(), do.call(b5_age, k))
    expect_error(b5_code(sp), sprintf("AGE has `from:` and `%s`", names(k)))
  }
  rows <- list(
    list(analysis_id = "DEMO", method = "stack", population_id = "SAF", by = "TRTA"),
    list(analysis_id = "CONT", parent = "DEMO", method = "continuous", variables = "AGE"))
  sp <- exact_spec(rows[[1L]])
  sp$analyses <- rbind(sp$analyses, exact_sheet(list(c(rows[[2L]], list(output_id = "T"))), .ard_spec_sheets$analyses))
  expect_no_error(b5_code(sp))
  for (k in c("name", "from")) {
    sp2 <- sp
    sp2$analyses[[k]][2L] <- "x"
    expect_error(b5_code(sp2), "`name:` / `from:` on a nested item \\(CONT, in DEMO\\)")
  }
})

# ---- the analyses, from YAML: the program, the list, the sheets --------------

b5_study_with <- function(items) {
  root <- tempfile("tflspec-b5")
  dir.create(root)
  d <- as_study(root)
  o <- tfl_read_output(file.path(d, "spec", "outputs", "T-14-1-1.yml"))
  o$id <- "T-B5"
  o$ard$analyses <- c(o$ard$analyses, items)
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-B5.yml"))
  list(dir = d, root = root, output = tfl_read_output(file.path(d, "spec", "outputs", "T-B5.yml")))
}

b5_items <- function() list(
  list(id = "AGE", method = "cards::ard_summary", from = "adsl_sub", by = "TRT01A", variables = "AGE"),
  list(id = "SEXC", method = "cards::ard_tabulate", name = "sex_ard", from = "adsl_sub",
       by = "TRT01A", variables = "SEX"),
  list(id = "AEC", method = "cards::ard_tabulate", name = "ae_ard", data = "adae_sub",
       by = "TRT01A", variables = "AESEV"))

test_that("from YAML: the program and tfl_output_objects() give the same names", {
  skip_on_cran()
  skip_if_not_installed("haven")
  s <- b5_study_with(b5_items())
  on.exit(unlink(s$root, recursive = TRUE), add = TRUE)
  st <- tfl_read_study_spec(s$dir)
  txt <- paste(suppressWarnings(suppressMessages(
    tfl_ard_code(st, output_id = "T-B5", save = FALSE))), collapse = "\n")
  expect_match(txt, "\n.ard_age <- adsl_sub |>\n  ard_summary(", fixed = TRUE)
  expect_match(txt, "\nsex_ard <- adsl_sub |>\n", fixed = TRUE)
  expect_match(txt, "\nae_ard <- adae_sub |>\n", fixed = TRUE)
  ob <- tfl_output_objects(s$output)
  nm <- ob$name[ob$stage == "analysis" & ob$id %in% c("AGE", "SEXC", "AEC")]
  expect_setequal(nm, c(".ard_age", "sex_ard", "ae_ard"))
  # every name the list gives the items is assigned in the program
  lines <- strsplit(txt, "\n")[[1L]]
  made <- sub(" <- .*$", "", grep("^[.A-Za-z][A-Za-z0-9._]* <- (adsl_sub|adae_sub) \\|>", lines, value = TRUE))
  expect_true(all(nm %in% made))
})

test_that("the sheets carry name and from, and the YAML comes back from them", {
  skip_on_cran()
  skip_if_not_installed("haven")
  s <- b5_study_with(b5_items())
  on.exit(unlink(s$root, recursive = TRUE), add = TRUE)
  sh <- suppressMessages(tfl_sheets_from_yaml(s$dir))
  an <- sh$ard$analyses[sh$ard$analyses$output_id %in% "T-B5", ]
  expect_identical(an$name[an$analysis_id == "SEXC"], "sex_ard")
  expect_identical(an$from[an$analysis_id == "AGE"], "adsl_sub")
  expect_true(is.na(an$name[an$analysis_id == "AGE"]))
  expect_true(is.na(an$from[an$analysis_id == "AEC"]))
  expect_identical(an$data[an$analysis_id == "AEC"], "adae_sub")
  # a report without the keys: the columns are blank, the others as before
  plain <- sh$ard$analyses[sh$ard$analyses$output_id %in% "T-14-1-1", ]
  expect_true(all(is.na(plain$name)) && all(is.na(plain$from)))
  old_cols <- setdiff(.ard_spec_sheets$analyses, c("name", "from"))
  expect_identical(names(sh$ard$analyses)[seq_along(old_cols)], old_cols)
  # the round trip: YAML -> sheets -> YAML
  to <- tempfile("b5-back")
  dir.create(to)
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  suppressMessages(tfl_yaml_from_sheets(sh, to))
  back <- tfl_read_outputs(to)[["T-B5"]]$ard$analyses
  b <- stats::setNames(back, vapply(back, function(e) e$id, ""))
  expect_identical(b$AGE$from, "adsl_sub")
  expect_null(b$AGE$name)
  expect_identical(b$SEXC$name, "sex_ard")
  expect_identical(b$SEXC$from, "adsl_sub")
  expect_null(b$AEC$from)
  expect_identical(b$AEC$name, "ae_ard")
})

# ---- the figure plots -------------------------------------------------------

b5_fig <- function(..., input = "df") {
  tfl_output("F-1", "figure", content = list(tfl_plot("km", list(...), input = input, form = "map")))
}
b5_fig_lines <- function(o) {
  x <- .fig_content_code(o)
  x[!startsWith(x, "# ----") & nzchar(x)]
}

test_that("a plot with `from:` writes `plot <- input |> first(...) + ...`", {
  o <- b5_fig(tfl_item("ggplot", list()), tfl_item("geom_point", list()),
              tfl_item("labs", list(title = "A")))
  expect_identical(b5_fig_lines(o), c("content <- df |>", "  ggplot() +", "  geom_point() +",
                                      "  labs(title = \"A\")"))
  # a plot without it is as before
  o0 <- b5_fig(tfl_item("ggplot", list()), tfl_item("geom_point", list()), input = NULL)
  expect_identical(b5_fig_lines(o0), c("content <- ggplot() +", "  geom_point()"))
  # one call
  expect_identical(b5_fig_lines(b5_fig(tfl_item("ggplot", list()))), c("content <- df |>", "  ggplot()"))
  # two plots: the named one pipes, the other is as before
  two <- tfl_output("F-1", "figure", content = list(
    tfl_plot("a", list(tfl_item("ggplot", list()), tfl_item("geom_point", list())), input = "df", form = "map"),
    tfl_plot("b", list(tfl_item("ggplot", list()), tfl_item("geom_line", list()))),
    tfl_item("wrap_plots", list(plots = c("a", "b")))))
  l <- b5_fig_lines(two)
  expect_true(all(c("p_a <- df |>", "  ggplot() +", "p_b <- ggplot() +") %in% l))
})

test_that("mixed joins: a `|>` call after a `+` chain is in parentheses", {
  # (ggsurvfit() is joined with `|>`, geom_point() with `+`: tfl_fn_defs())
  d <- tfl_fn_defs()
  expect_identical(d$join[d$fn == "ggsurvfit"], "|>")
  expect_identical(d$join[d$fn == "geom_point"], "+")
  o <- b5_fig(tfl_item("ggplot", list()), tfl_item("geom_point", list()), tfl_item("ggsurvfit", list()),
              tfl_item("labs", list(title = "A")))
  expect_identical(b5_fig_lines(o), c("content <- (df |>", "  ggplot() +", "  geom_point()) |>",
                                      "  ggsurvfit() +", "  labs(title = \"A\")"))
  # the code parses as the grouping says: ((df |> ggplot() + geom_point()) |> ggsurvfit()) + labs()
  e <- parse(text = paste(b5_fig_lines(o), collapse = "\n"))[[1L]][[3L]]
  expect_identical(as.character(e[[1L]]), "+")
  # the chain itself: grouped as often as a `|>` follows a `+`
  l <- .fig_pipe_chain("p", "df", c("a()", "b()", "c()", "d()", "e()", "f()"),
                       c("|>", "+", "|>", "+", "|>", "|>"))
  expect_identical(l, c("p <- ((df |>", "  a() +", "  b()) |>", "  c() +", "  d()) |>", "  e() |>", "  f()"))
  expect_identical(.fig_pipe_chain("p", "df", c("a()", "b()"), c("|>", "|>")),
                   c("p <- df |>", "  a() |>", "  b()"))
})

test_that("a plot that makes its own data takes no `from:`", {
  o <- b5_fig(tfl_item("tfl_risk_panel", list(fit = "fit")))
  expect_error(.fig_content_code(o), "starts with tfl_risk_panel, which makes its own data")
})

test_that("the code of a plot with `from:` runs", {
  skip_if_not_installed("ggplot2")
  o <- b5_fig(tfl_item("ggplot", list(mapping = tfl_r("ggplot2::aes(x, y)"))),
              tfl_item("geom_point", list()), tfl_item("labs", list(title = "A")))
  e <- new.env()
  e$df <- data.frame(x = 1:3, y = c(2, 3, 5))
  code <- paste(c("library(ggplot2)", b5_fig_lines(o)), collapse = "\n")
  eval(parse(text = code), e)
  expect_s3_class(e$content, "ggplot")
  expect_identical(e$content$labels$title, "A")
  expect_identical(nrow(e$content$data), 3L)
})
