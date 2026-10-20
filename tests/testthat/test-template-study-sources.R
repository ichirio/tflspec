# A template parameter's value from the study standard: the report's
# population, the study's treatment (tfl_param(study = ))

# the sample study, its study.yml saying the treatment
ss_study <- function(treatment = "TRT01P") {
  d <- tempfile()
  dir.create(d)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"),
                       full.names = TRUE), d, recursive = TRUE)
  if (!is.null(treatment)) {
    writeLines(c("study_id: SS-1", "settings:", paste0("  treatment: ", treatment)),
               file.path(d, "study.yml"))
  }
  s <- tfl_read_study_spec(d)
  unlink(d, recursive = TRUE)
  s
}
ss_resolve <- function(name, args, s, population = "SAF") {
  o <- tfl_output("T-9", "table", population = population,
                  template = list(name = name, args = args))
  tfl_resolve_output(o, s)
}
ss_an <- function(ro, id) Filter(function(a) identical(a$id, id), ro$ard$analyses)[[1L]]

test_that("the report's population: the template's population, written in its program", {
  s <- ss_study(NULL)
  ro <- ss_resolve("demog", list(), s)
  expect_identical(attr(ro, "template")$args$population, "SAF")
  expect_identical(attr(ro, "template")$study, "population")
  code <- tfl_content_code(ro, pipe = "|>")
  expect_match(paste(code, collapse = "\n"), "tpl_demog(ard, population = \"SAF\"",
               fixed = TRUE)
  # the report's own argument wins, and says so
  expect_message(ro2 <- ss_resolve("demog", list(population = "SCRF"), s),
                 "overrides the study standard \\(output.population\\)")
  expect_identical(attr(ro2, "template")$args$population, "SCRF")
})

test_that("study.yml's settings: treatment: the templates' by", {
  s <- ss_study("TRT01P")
  ro <- ss_resolve("ae_soc_pt", list(), s)
  expect_identical(ss_an(ro, "TEAE")$by, "TRT01P")
  ro <- ss_resolve("demog", list(), s)
  expect_identical(ss_an(ro, "CONT")$by, "TRT01P")
  # the report's own by wins
  ro <- suppressMessages(ss_resolve("demog", list(by = "SEX"), s))
  expect_identical(ss_an(ro, "CONT")$by, "SEX")
  # no setting: the template's own default
  ro <- ss_resolve("demog", list(), ss_study(NULL))
  expect_identical(ss_an(ro, "CONT")$by, "TRT01A")
})

test_that("first to last: the report's argument, the study standard, the variant's, from's", {
  on.exit(rm("demog_arm", envir = .tpl_registry))
  tfl_template_variant("demog_arm", from = "demog", defaults = list(by = "ARM"))
  # the study's treatment over the variant's default
  ro <- ss_resolve("demog_arm", list(), ss_study("TRT01P"))
  expect_identical(ss_an(ro, "CONT")$by, "TRT01P")
  # no study setting: the variant's
  ro <- ss_resolve("demog_arm", list(), ss_study(NULL))
  expect_identical(ss_an(ro, "CONT")$by, "ARM")
})

test_that("tfl_test_templates(): the analysis set or the treatment with no study source", {
  expect_false(any(grepl("no study source", tfl_test_templates()$status)))
  d <- .tpl_get("demog", "test")
  p <- c(d$params, list(tfl_param("arm", "ARM", type = "vars", study = NULL)))
  suppressMessages(tfl_template("ss_arm", "table", "summary", params = p, body = d$body,
                                example = list(population = "SAF")))
  on.exit(rm("ss_arm", envir = .tpl_registry))
  r <- tfl_test_templates("ss_arm")
  expect_identical(r$status[r$param %in% "arm"], "no study source (population / treatment)")
  expect_identical(tfl_template_params("ss_arm")$by$study, "settings.treatment")
})
