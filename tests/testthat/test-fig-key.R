# The code SPEC's figure content key, `figure: {name, args}`: a series
# figure template written out (tfl_template_to_spec()), through YAML and the
# sheets, and its program (tfl_content_code()).  Fictional data only.

fk_lines <- function() {
  list(
    km = c("id: F-21", "kind: figure", "label: Kaplan-Meier", "data:", "  - id: adsl_sub", "    population: ITT",
           "template:", "  name: tflspec_v1/km_simple", "  data:", "    time_unit: months",
           "  analysis:", "    by: TRT01A"),
    forest = c("id: F-22", "kind: figure", "label: Forest", "data:", "  - id: adsl_sub", "    population: ITT",
               "template:", "  name: tflspec_v1/forest_hr", "  data:", "    subgroup_vars: [SEX]",
               "  analysis:", "    by: TRT01A", "    comparison: Drug X",
               "    conditions:", "      - {subgroup: SEX, subgroup_lvl: F, where: !r SEX == \"F\"}"))
}

fk_adtte <- function() {
  set.seed(20261117)
  n <- 40L
  data.frame(
    TRT01A = rep(c("Control", "Drug X"), each = n / 2L),
    SEX = rep(c("F", "M"), times = n / 2L),
    AVAL = round(c(stats::rexp(n / 2L, 1 / 200), stats::rexp(n / 2L, 1 / 320)) + 1),
    CNSR = stats::rbinom(n, 1L, 0.3),
    stringsAsFactors = FALSE)
}

fk_spec <- function(lines, d) {
  f <- file.path(d, "spec", "outputs", paste0(sub("^id: ", "", lines[1L]), ".yml"))
  writeLines(lines, f)
  on.exit(unlink(f))
  invisible(tfl_copy_templates(d))
  tfl_template_to_spec(tfl_read_output(f), tfl_read_study_spec(d))
}

# the lines after the first (a comment saying where the call came from)
fk_body <- function(code) as.character(code)[-1L]

test_that("tfl_template_to_spec() writes figure: {name, args} for a series figure template", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- file.path(tempdir(), "fig-key-1")
  unlink(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adtte"), id = "FK-1")
  km <- fk_spec(fk_lines()$km, d)
  expect_null(km$template)
  expect_identical(km$figure$name, "km_curves")
  # the model's arguments, then the function's params
  expect_setequal(names(km$figure$args)[1:3], c("by", "time", "censor"))
  expect_identical(km$figure$args$by, "TRT01A")
  expect_identical(km$figure$args$time_unit, "months")
  expect_identical(km$figure$args$legend, "bottom")
  fo <- fk_spec(fk_lines()$forest, d)
  expect_identical(fo$figure, list(name = "forest_hr", args = list(comparison = "Drug X", size_by_n = FALSE,
                                                                    bold_headings = FALSE)))
})

test_that("the key goes through YAML and the sheets, and writes the same call as the template's path", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- file.path(tempdir(), "fig-key-2")
  unlink(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adtte"), id = "FK-2")
  st <- tfl_read_study_spec(d)
  for (k in c("km", "forest")) {
    lines <- fk_lines()[[k]]
    spec <- fk_spec(lines, d)
    # YAML
    f <- file.path(d, "spec", "outputs", paste0(spec$id, ".yml"))
    tfl_write_output(spec, f)
    txt <- paste(readLines(f), collapse = "\n")
    expect_match(txt, "figure:\n  name: ", fixed = TRUE, info = k)
    back <- tfl_read_output(f)
    expect_equal(back$figure, spec$figure, info = k)
    # the program: the same call as the template's own path
    ref <- tfl_content_code(tfl_template_resolve(tflspec:::.series_read_yaml(
      { g <- tempfile(fileext = ".yml"); writeLines(lines, g); g }), study = st$study))
    expect_identical(fk_body(tfl_content_code(back)), fk_body(ref), info = k)
    expect_identical(fk_body(tfl_content_code(spec)), fk_body(ref), info = k)
    # the sheets: the design list has it, and YAML from the sheets has it again
    sh <- suppressWarnings(tfl_sheets_from_yaml(d))
    expect_equal(sh$figures[[spec$id]]$figure, spec$figure, info = k)
    out <- file.path(tempdir(), paste0("fig-key-out-", k))
    unlink(out, recursive = TRUE)
    suppressMessages(suppressWarnings(tfl_yaml_from_sheets(sh, out)))
    g <- list.files(out, paste0("^", spec$id, "[.]yml$"), recursive = TRUE, full.names = TRUE)
    expect_length(g, 1L)
    expect_equal(tfl_read_output(g[[1L]])$figure, spec$figure, info = k)
    unlink(out, recursive = TRUE)
    unlink(f)
  }
})

test_that("a code SPEC's figure program runs to a ggplot", {
  skip_on_cran()
  skip_if_not_installed("ggplot2", "3.5.2")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("survival")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("haven")
  d <- file.path(tempdir(), "fig-key-3")
  unlink(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adtte"), id = "FK-3")
  tt <- fk_adtte()
  km <- fk_spec(fk_lines()$km, d)
  bign <- cards::ard_tabulate(tt, variables = "TRT01A")
  bign$analysis_id <- "BIGN"
  p <- tfl_content_run(km, list(adtte_sub = tt, ard = bign))
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("an unknown figure name, a missing model argument, and figure: with content: stop", {
  o <- tfl_output("F-9", "figure")
  o$figure <- list(name = "waterfall", args = list())
  expect_error(tfl_content_code(o), "km_curves, forest_hr")
  o$figure <- list(name = "km_curves", args = list(by = "TRT01A"))
  expect_error(tfl_content_code(o), "needs time, censor")
  o$figure <- list(name = "forest_hr", args = list())
  o$content <- list(tfl_item("ggplot", list()))
  expect_error(tfl_content_code(o), "no `content:`")
})
