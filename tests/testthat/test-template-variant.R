# tfl_template_variant(): a template by its defaults alone

tv_study <- function() tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
tv_drop <- function(...) for (n in c(...)) {
  if (exists(n, envir = .tpl_registry, inherits = FALSE)) rm(list = n, envir = .tpl_registry)
}

# the report through the YAML file (template:), A (its program) and B (its
# items written out): the same RTF
tv_rtfs <- function(o, ard, s) {
  op <- options(rtfreporter.render_time = as.POSIXct("2000-01-01 00:00:00", tz = "UTC"))
  on.exit(options(op))
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  tfl_write_output(o, f)
  ro <- tfl_resolve_output(tfl_read_output(f), s)
  rtf <- function(plan) {
    g <- tempfile(fileext = ".rtf")
    on.exit(unlink(g))
    suppressMessages(rtfreporter::generate_rtfreport(
      tfl_report(ro, content = plan, study = s), g, overwrite = TRUE, program = "p.R"))
    unname(tools::md5sum(g))
  }
  e <- new.env()
  e$ard <- ard
  code <- tfl_content_code(ro, pipe = "|>")
  suppressMessages(eval(parse(text = code), envir = e))
  w <- tfl_template_to_spec(o, s)
  list(code = code,
       yaml = rtf(suppressMessages(tfl_content_run(ro, ard))),
       a = rtf(e$plan),
       b = rtf(suppressMessages(tfl_content_run(tfl_resolve_output(w, s), ard))))
}

test_that("a variant: from's parameters and types, its own defaults; listed with its from", {
  on.exit(tv_drop("demog_as"))
  tfl_template_variant("demog_as", from = "demog",
                       defaults = list(continuous = "AGE", categorical = c("AGEGR1", "SEX")))
  p <- tfl_template_params("demog_as")
  expect_identical(names(p), names(tfl_template_params("demog")))
  expect_identical(p$categorical$default, c("AGEGR1", "SEX"))
  expect_identical(p$categorical$type, "vars")
  tt <- tfl_templates()
  expect_identical(tt$from[tt$name == "demog_as"], "demog")
  expect_true(is.na(tt$from[tt$name == "demog"]))
  expect_identical(tt$call[tt$name == "demog_as"], "tpl_demog")
  expect_identical(nrow(tfl_test_templates("demog_as")), 0L)
  expect_error(tfl_template_variant("x", from = "demog", defaults = list(colour = 1)),
               "no parameter 'colour'")
  expect_error(tfl_template_variant("x", from = "nope"), "no template")
  # a default given makes a required parameter optional
  tfl_template_variant("demog_as", from = "demog", defaults = list(population = "SAF")) |>
    expect_message("replaces")
  expect_false(tfl_template_params("demog_as")$population$required)
})

test_that("from as it is now: a fix to it reaches the variant", {
  on.exit({
    tv_drop("demog_as", "tv_base")
  })
  d <- .tpl_get("demog", "test")
  suppressMessages(tfl_template("tv_base", "table", "summary", params = d$params, body = d$body))
  tfl_template_variant("demog_as", from = "tv_base", defaults = list(continuous = "AGE"))
  p <- d$params
  p$heading <- tfl_param("heading", "X", type = "text")
  suppressMessages(tfl_template("tv_base", "table", "summary", params = p, body = d$body))
  expect_true("heading" %in% names(tfl_template_params("demog_as")))
})

test_that("the same name: the variant replaces from, and reads the one it replaced", {
  on.exit({
    tv_drop("summary")
    assign("summary", get(".base.summary", envir = .tpl_registry), envir = .tpl_registry)
    tv_drop(".base.summary")
  })
  expect_message(tfl_template_variant("summary", from = "summary",
                                      defaults = list(variables = "AGE")), "replaces")
  expect_identical(tfl_template_params("summary")$variables$default, "AGE")
  expect_identical(tfl_templates()$from[tfl_templates()$name == "summary"], "summary")
  x <- tfl_template_expand("summary", list(population = "SAF"))
  expect_identical(x$ard$analyses[[2L]]$variables, "AGE")
})

test_that("first to last: the report's argument, the study's default, the variant's, from's", {
  on.exit(tv_drop("demog_as"))
  # the sample study, with default continuous cells
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dir.create(d)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"), full.names = TRUE),
            d, recursive = TRUE)
  f <- file.path(d, "defaults.yml")
  y <- readLines(f)
  y <- sub("- plan_cells: {categorical: \"{n:.0f} ({p:.1f%})\"}",
           "- plan_cells: {categorical: \"{n:.0f} ({p:.1f%})\", continuous: {n: \"{N:.0f}\"}}",
           y, fixed = TRUE)
  writeLines(y, f)
  s <- tfl_read_study_spec(d)
  vc <- list(n = "{N:.0f}", Mean = "{mean:.2f}")
  tfl_template_variant("demog_as", from = "demog",
                       defaults = list(categorical = "SEX", cells = vc))
  cells_of <- function(args) {
    o <- tfl_output("T-9", "table", population = "SAF",
                    template = list(name = "demog_as", args = args))
    ro <- tfl_resolve_output(o, s)
    pc <- Filter(function(it) identical(it$fn, "plan_cells"), ro$content)
    list(cells = pc[[1L]]$args$continuous,
         cat = Filter(function(a) identical(a$id, "CAT"), ro$ard$analyses)[[1L]]$variables)
  }
  # the study's defaults have plan_cells(continuous = ): over the variant's
  r <- cells_of(list(population = "SAF"))
  expect_identical(r$cells, list(n = "{N:.0f}"))
  # the variant's over from's (no study default for categorical)
  expect_identical(r$cat, "SEX")
  # the report's over all of them
  own <- list(n = "{N:.0f}")
  r2 <- cells_of(list(population = "SAF", cells = own, categorical = "RACE"))
  expect_identical(r2$cells, own)
  expect_identical(r2$cat, "RACE")
})

test_that("demog: the YAML's template:, A and B make the same RTF; A calls tpl_demog()", {
  skip_if_not_installed("cards")
  on.exit(tv_drop("demog_as"))
  tfl_template_variant("demog_as", from = "demog",
                       defaults = list(continuous = "AGE", categorical = c("AGEGR1", "SEX")))
  s <- tv_study()
  saf <- adsl[adsl$SAFFL == "Y", ]
  ard <- cards::bind_ard(cards::ard_tabulate(saf, variables = TRT01A),
                         cards::ard_summary(saf, by = TRT01A, variables = AGE),
                         cards::ard_tabulate(saf, by = TRT01A, variables = c(AGEGR1, SEX)))
  o <- tfl_output("T-9", "table", population = "SAF", label = "Demographics",
                  template = list(name = "demog_as", args = list(population = "SAF")))
  r <- tv_rtfs(o, ard, s)
  expect_match(r$code, "# template demog_as: a variant of demog", fixed = TRUE, all = FALSE)
  expect_match(r$code, "plan <- tpl_demog(", fixed = TRUE, all = FALSE)
  expect_match(paste(r$code, collapse = "\n"), "categorical = c(\"AGEGR1\", \"SEX\")",
               fixed = TRUE)
  expect_identical(r$a, r$yaml)
  expect_identical(r$b, r$yaml)
})

test_that("ae_soc_pt: the YAML's template:, A and B make the same RTF", {
  skip_if_not_installed("cards")
  on.exit(tv_drop("ae_soc_pt_all"))
  tfl_template_variant("ae_soc_pt_all", from = "ae_soc_pt",
                       defaults = list(where = NULL, overall = "Any AE"))
  s <- tv_study()
  saf <- adsl[adsl$SAFFL == "Y", ]
  ae <- adae[adae$USUBJID %in% saf$USUBJID, ]
  ard <- cards::ard_stack_hierarchical(ae, variables = c(AEBODSYS, AEDECOD), by = TRT01A,
                                       denominator = saf, id = USUBJID, over_variables = TRUE)
  o <- tfl_output("T-9", "table", population = "SAF", label = "AEs",
                  template = list(name = "ae_soc_pt_all", args = list(population = "SAF")))
  r <- tv_rtfs(o, ard, s)
  expect_match(r$code, "plan <- tpl_ae_soc_pt(", fixed = TRUE, all = FALSE)
  expect_match(paste(r$code, collapse = "\n"), "overall = \"Any AE\"", fixed = TRUE)
  expect_identical(r$a, r$yaml)
  expect_identical(r$b, r$yaml)
})

test_that("a study's spec/templates/*.R: registered when the study is read", {
  on.exit(tv_drop("demog_study"))
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "spec", "templates"), recursive = TRUE)
  writeLines(c("# the study's demographics",
               "tflspec::tfl_template_variant(\"demog_study\", from = \"demog\",",
               "  defaults = list(categorical = \"SEX\"))"),
             file.path(d, "spec", "templates", "demog.R"))
  tfl_read_study_spec(d)
  tt <- tfl_templates()
  expect_identical(tt$package[tt$name == "demog_study"],
                   sprintf("study %s: spec/templates/demog.R", basename(d)))
  expect_identical(tfl_template_params("demog_study")$categorical$default, "SEX")
  writeLines("stop('no')", file.path(d, "spec", "templates", "bad.R"))
  expect_error(tfl_read_study_spec(d), "spec/templates/bad.R: no")
})
