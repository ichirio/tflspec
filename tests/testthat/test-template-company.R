# The company layer: a template registered from outside (tfl_template()),
# run (tfl_template_run()), tested for look parameters
# (tfl_test_templates()), and a company's package to start from
# (tfl_use_company_templates()).

tc_forget <- function(names) {
  reg <- get(".tpl_registry", envir = asNamespace("tflspec"))
  rm(list = intersect(names, ls(reg)), envir = reg)
}

test_that("tflspec's own templates take no look as a parameter, and each example expands", {
  look <- tfl_test_templates()
  expect_identical(look$param[look$status == "look"], character(0))
  expect_identical(sum(grepl("^not checked", look$status)), 0L, info = paste(look$status, collapse = "; "))
  for (t in tfl_templates()$name) {
    ex <- tflspec:::.tpl_examples[[t]]
    if (is.null(ex)) next
    x <- suppressMessages(tfl_template_expand(t, ex))
    expect_gt(length(x$content), 0L, label = t)
  }
  # every template of tflspec has its example
  expect_setequal(tfl_templates()$name, names(tflspec:::.tpl_examples))
})

test_that("tfl_template(): registered from outside, listed, replaced with a message, a look parameter warned", {
  withr::defer(tc_forget(c("tc_ages", "tc_wide")))
  tfl_template("tc_ages", kind = "table", layout = "summary",
    params = list(tfl_param("population", "SAF")),
    body = function(a, ctx) tfl_template_expand("summary",
      list(population = a$population, variables = "AGE"),
      codelists = ctx$codelists, population = ctx$population),
    call = "tc_ages")
  t <- tfl_templates()
  expect_identical(t$call[t$name == "tc_ages"], "tc_ages")
  expect_identical(t$package[t$name == "demog"], "tflspec")
  x <- tfl_template_expand("tc_ages")
  expect_true(all(vapply(x$content, `[[`, "", "from") == "template:tc_ages"))
  expect_message(tfl_template("tc_ages", kind = "table", layout = "summary",
    params = list(tfl_param("population", "SAF")),
    body = function(a, ctx) tfl_template_expand("summary", list(population = a$population,
                                                                variables = "AGE"))),
    "replaces")
  # a parameter that ends in the look
  expect_warning(tfl_template("tc_wide", kind = "table", layout = "summary",
    params = list(tfl_param("population", "SAF"), tfl_param("widths", c(row_label = 4))),
    body = function(a, ctx) {
      x <- tfl_template_expand("summary", list(population = a$population, variables = "AGE"))
      x$content <- c(x$content, list(tfl_item("plan_columns", list(widths = a$widths))))
      x
    }), "`widths` \\(plan_columns\\(widths\\)\\)")
  look <- tfl_test_templates("tc_wide")
  expect_identical(look$param[look$status == "look"], "widths")
  expect_error(tfl_template("tc bad", "table", "summary", list(), function(a, ctx) NULL), "one name")
  expect_error(tfl_template("tc_x", "table", "summary", list("population"), function(a, ctx) NULL),
               "tfl_param")
})

test_that("tfl_template_run() and tfl_template_params(); a company's template in a report's program", {
  skip_if_not_installed("cards")
  withr::defer(tc_forget("tc_demog"))
  p <- tfl_template_params("demog")
  expect_s3_class(p$cells, "tfl_param")
  p$cells$default <- list(n = "{N:.0f}", Median = "{median:.1f}")
  tfl_template("tc_demog", kind = "table", layout = "summary", params = p,
    body = function(a, ctx) tfl_template_expand("demog", a, codelists = ctx$codelists,
                                                population = ctx$population),
    call = "tc_demog")
  saf <- adsl[adsl$SAFFL == "Y", ]
  ard <- cards::bind_ard(cards::ard_tabulate(saf, variables = TRT01A),
                         cards::ard_summary(saf, by = TRT01A, variables = AGE),
                         cards::ard_tabulate(saf, by = TRT01A, variables = c(AGEGR1, SEX, RACE, ETHNIC)))
  plan <- suppressMessages(tfl_template_run("tc_demog", ard, population = "SAF"))
  tab <- suppressMessages(rtfreporter::plan_apply(plan, "table"))
  expect_identical(sum(tab[[2L]] %in% "Median"), 1L)
  # the program calls the company's function
  s <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  o <- tfl_output("T-9", "table", population = "SAF",
                  template = list(name = "tc_demog", args = list(population = "SAF")))
  expect_match(tfl_content_code(tfl_resolve_output(o, s)), "plan <- tc_demog(", fixed = TRUE, all = FALSE)
})

test_that("tfl_use_company_templates(): a package to start from, whose templates register, run and pass their test", {
  d <- file.path(withr::local_tempdir(), "acmetpl")
  f <- tfl_use_company_templates(d, templates = c("demog", "ae_soc_pt"))
  expect_true(all(file.exists(file.path(d, c("DESCRIPTION", "NAMESPACE", "LICENSE", "README.md",
    "R/templates.R", "R/zzz.R", "tests/testthat.R", "tests/testthat/test-templates.R",
    "inst/standards/defaults.yml")))))
  expect_identical(read.dcf(file.path(d, "DESCRIPTION"))[, "Package"], c(Package = "acmetpl"))
  expect_false(any(grepl("^export", readLines(file.path(d, "NAMESPACE")))))
  for (r in list.files(d, "[.]R$", recursive = TRUE, full.names = TRUE)) expect_silent(parse(r))
  expect_true(is.list(yaml::read_yaml(file.path(d, "inst/standards/defaults.yml"))))
  # its templates, registered as its .onLoad() does
  withr::defer(tc_forget(c("acmetpl_demog", "acmetpl_ae_soc_pt")))
  e <- new.env()
  sys.source(file.path(d, "R", "templates.R"), envir = e)
  e$register_templates()
  tt <- tfl_templates()
  expect_identical(tt$from[match(c("acmetpl_demog", "acmetpl_ae_soc_pt"), tt$name)],
                   c("demog", "ae_soc_pt"))
  # its tests' checks
  look <- tfl_test_templates(c("acmetpl_demog", "acmetpl_ae_soc_pt"))
  expect_identical(look$param[look$status == "look"], character(0))
  x <- tfl_template_expand("acmetpl_ae_soc_pt", list(population = "SAF"))
  expect_true(all(vapply(x$content, `[[`, "", "from") == "template:acmetpl_ae_soc_pt"))
  expect_error(tfl_use_company_templates(d), "not empty")
  expect_error(tfl_use_company_templates(file.path(tempdir(), "x"), package = "1bad"), "not a package name")
})
