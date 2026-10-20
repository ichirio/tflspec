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

test_that("km_simple: the analysis set's subjects, by data.yml's definition (a semi join)", {
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(d)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"),
                       full.names = TRUE), d, recursive = TRUE)
  writeLines(c("populations:", "  - id: SAF", "    dataset: ADSL", "    where: !r SAFFL == \"Y\""),
             file.path(d, "data.yml"))
  s <- tfl_read_study_spec(d)
  o <- tfl_output("F-9", "figure", population = "SAF",
                  template = list(name = "km_simple", args = list(param = "PFS", group = "TRT01A")))
  ro <- tfl_resolve_output(o, s)
  st <- ro$data$steps
  j <- Filter(function(x) identical(x$how, "semi"), st)
  expect_length(j, 1L)
  expect_identical(j[[1L]][c("step", "dataset", "where", "by")],
                   list(step = "join", dataset = "ADSL", where = "SAFFL == \"Y\"", by = "USUBJID"))
  expect_false(any(vapply(st, function(x) identical(x$step, "flag"), NA)))
  code <- paste(tfl_fig_design_code(tfl_fig_design(data = st)), collapse = "\n")
  expect_match(code, "semi_join(", fixed = TRUE)
  expect_match(code, "filter(SAFFL == \"Y\") |> select(USUBJID)", fixed = TRUE)
  # a semi join takes no variables; a left join still needs them
  chk <- tfl_check_fig_design(tfl_fig_design(data = st))
  expect_false(any(chk$field %in% "vars"))
  left <- list(list(step = "read", dataset = "ADTTE"), list(step = "join", dataset = "ADSL"))
  expect_true(any(tfl_check_fig_design(tfl_fig_design(data = left))$field %in% "vars"))
  # an id the study does not define
  o$population <- "ITT"
  expect_error(tfl_resolve_output(o, s), "no analysis set 'ITT'")
  # the semi join keeps the rows of the set's subjects: the same as the flag
  df <- adtte[adtte$PARAMCD == adtte$PARAMCD[1L], ]
  saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
  kept <- dplyr::semi_join(df, dplyr::select(adsl[adsl$SAFFL == "Y", ], USUBJID), by = "USUBJID")
  expect_identical(sort(unique(kept$USUBJID)), sort(intersect(unique(df$USUBJID), saf)))
})
