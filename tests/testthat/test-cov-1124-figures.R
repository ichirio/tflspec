# Branches no other test reaches in the figure code that entered `next`
# from 2026-11-24 (A2-2: a figure's analysis set is its report's data
# population; forest_hr's subgroup headings; the AI's checks): the
# subjects part, the template designs' join and parameter steps, a template
# report's analysis set, and the AI's listing and figure checks.  Fictional
# data only.

cov_fig_spec <- function(populations, adata = NULL) {
  tfl_ard_spec(c(list(
    datasets = data.frame(dataset = c("ADSL", "ADTTE"), path = c("adsl.rds", "adtte.rds")),
    populations = populations,
    analyses = data.frame(output_id = "T-1", analysis_id = "A1", method = "categorical",
                          dataset = "ADSL", population_id = "SAF", by = "TRT01A",
                          variables = "SEX")),
    if (!is.null(adata)) list(analysis_data = adata)))
}

# ---- the subjects part (R/fig_model.R) --------------------------------------------

test_that("a set made under the report's own subjects row is read from that row", {
  x <- cov_fig_spec(data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
                    adata = data.frame(output_id = "F-1", data_id = "adsl_saf", from = "ADSL",
                                       population_id = "SAF"))
  sj <- .fig_subjects_part(x, "F-1", "SAF", data = "adsl_saf")
  expect_identical(sj$subjects$dataset, "ADSL")
  expect_true(any(grepl("^adsl_saf <- ", unlist(sj$code))))
})

test_that("a set with a condition but no object of its subjects stops, never every subject", {
  x <- cov_fig_spec(data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
                    adata = data.frame(output_id = "F-1", data_id = "adsl_saf", from = "ADSL",
                                       population_id = "SAF", where = "AGE > 1"))
  e <- tryCatch(.fig_subjects_part(x, "F-1", "SAF", data = "adsl_saf"), error = identity)
  expect_s3_class(e, "tfl_fig_subjects")
  expect_match(conditionMessage(e), "F-1: the subjects of analysis set SAF are not made by its ARD definition")
})

test_that("the subjects part's lines start with no blank line and never two in a row", {
  expect_identical(.fig_subjects_lines(c("", "# ---- data ----", "", "a <- 1", "", "", "b <- 2")),
                   c("a <- 1", "", "b <- 2"))
})

test_that("a set whose ARD definition cannot be made says why", {
  expect_match(.fig_subjects_no_ard("F-1", "SAF", "no data.yml"),
               "F-1: the report's analysis set SAF: .* which cannot be made \\(no data.yml\\)")
  expect_match(.fig_subjects_no_ard("F-1", "SAF"), "and the study has none")
})

cov_study <- function(env = parent.frame()) {
  root <- tempfile("cov1124-fig")
  dir.create(root)
  do.call(on.exit, list(bquote(unlink(.(root), recursive = TRUE)), add = TRUE), envir = env)
  as_study(root)
}

test_that("a figure's subjects are made from a study given as its folder", {
  skip_if_not_installed("haven")
  d <- cov_study()
  f <- tfl_output("F-9", "figure", content = list(tfl_item("point", list(x = "AGE", y = "AGE"))))
  f$data <- list(source = "code", population = "SAF", steps = list(list(step = "read", dataset = "ADSL")))
  sj <- .fig_output_subjects(f, d, "the test")
  expect_identical(sj$subjects$dataset, "ADSL")
})

test_that("a study with no ARD definition stops the figure of a set", {
  f <- tfl_output("F-9", "figure", content = list(tfl_item("point", list(x = "AGE", y = "AGE"))))
  f$data <- list(source = "code", population = "SAF")
  st <- structure(list(), class = "tfl_study_spec")
  e <- tryCatch(local({
    local_mocked_bindings(.ard_spec_of_study = function(...) list(spec = NULL))
    .fig_output_subjects(f, st, "the test")
  }), error = identity)
  expect_s3_class(e, "tfl_fig_subjects")
  expect_match(conditionMessage(e), "and the study has none")
})

test_that("a figure run with a study given as its folder reads the study", {
  skip_if_not_installed("haven")
  d <- cov_study()
  f <- tfl_output("F-9", "figure", content = list(tfl_item("point", list(x = "AGE", y = "AGE"))))
  p <- suppressMessages(tfl_content_run(f, data.frame(AGE = c(30, 40)), study = d))
  expect_s3_class(p, "ggplot")
})

test_that("a figure style standard with no size or theme gives the defaults", {
  local_mocked_bindings(pp_default_options = function(type = NA) list(dpi = "", base_size = NULL))
  expect_identical(.fig_kind_size("km"), list(dpi = 300, base_size = 10, theme = "boxed"))
})

test_that("km with join_adsl joins the subject-level data before the parameter", {
  d <- tfl_fig_template("km_simple", group = "TRT01A", join_adsl = TRUE)
  expect_identical(vapply(d$data, function(s) s$step, "")[1:3], c("read", "join", "param"))
})

test_that("forest_hr with join_adsl joins the subject-level data before the parameter", {
  d <- tfl_fig_template("forest_hr", group = "TRT01A", join_adsl = TRUE)
  expect_identical(vapply(d$data, function(s) s$step, "")[1:3], c("read", "join", "param"))
})

test_that("a PK figure given a parameter keeps it before the join", {
  d <- tfl_fig_template("pk_mean", group = "TRT01A", param = "DRUGX")
  expect_identical(vapply(d$data, function(s) s$step, "")[1:3], c("read", "param", "join"))
})

# ---- forest_hr's body (R/tpl_fig.R, #988) ------------------------------------------

test_that("forest_hr's subgroup headings are the code lists' variable labels", {
  a <- list(data = "ADTTE", population = "SAF", param = "PFS", group = "TRT01A", subgroups = "SEX",
            time = "AVAL", censor = "CNSR")
  ctx <- list(codelists = list(variable = list(list(value = "SEX", label = "Sex at birth"))))
  b <- .tpl_forest_body(a, ctx)
  expect_identical(b$ard$analysis_data[[1L]]$id, "adtte_saf_pfs")
  expect_true(any(grepl("Sex at birth", deparse(b), fixed = TRUE)))
  # the report's own heading wins over the code list's
  a$subgroup_labels <- list(SEX = "Sex")
  b2 <- .tpl_forest_body(a, ctx)
  expect_false(any(grepl("Sex at birth", deparse(b2), fixed = TRUE)))
})

# ---- a template report's analysis set (R/tpl_resolve.R) -------------------------

test_that("a template with no population argument takes no set on the data map", {
  rep <- list(id = "T-1", data = list(population = "SAF"))
  expect_error(.tr_report_set(rep, list(), "group", "tpl_x", "who"),
               "who: tpl_x takes no analysis set \\(it has no argument population\\); the report's data says SAF")
})

test_that("a population argument of !unset against the data's set stops, saying none", {
  rep <- list(id = "T-1", data = list(population = "SAF"))
  expect_error(.tr_report_set(rep, list(population = tfl_unset()), "population", "tpl_x", "who"),
               "its template argument population none: write it once")
})

test_that("a population argument the same as the data's set is kept", {
  rep <- list(id = "T-1", data = list(population = "SAF"))
  r <- .tr_report_set(rep, list(population = "SAF"), "population", "tpl_x", "who")
  expect_null(r$data)
  expect_identical(r$given, list(population = "SAF"))
})

# ---- the AI's checks (R/ai_listing.R, R/ai_figure.R) ------------------------------

test_that("the AI's listing check: an unknown common set and data with dataset and set are errors", {
  o <- list(id = "L-1", kind = "listing", label = "L", common = list(population = "NOPE"),
            data = list(source = "dataset", data = "adae_x", dataset = "ADAE", population = "SAF"),
            ard = list(analysis_data = list(list(id = "adae_x", from = "ADAE"))),
            content = list(tfl_item("plan_listing", list(cols = list()))))
  p <- .ai_check_listing_output(o, "L-1", list(populations = "SAF"), NULL)
  expect_true(any(p$where == "L-1 common population" & grepl("'NOPE' is not an analysis set", p$problem)))
  expect_true(any(p$where == "L-1 data" & grepl("not both", p$problem)))
})

test_that("the AI's figure draw of a report the folder has not got has no subjects", {
  d <- tempfile("cov1124-fig")
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  wd <- setwd(d)
  on.exit(setwd(wd), add = TRUE)
  expect_null(.ai_fig_subjects(NULL, "F-404", NULL))
})
