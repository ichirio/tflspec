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
  # (with the study standard's look: its plan_stub, plan_blanks ... too)
  expect_identical(attr(ro, "template")$study[1L], "population")
  code <- tfl_content_code(ro, pipe = "|>")
  expect_match(paste(code, collapse = "\n"), "  \"demog\",\n  population = \"SAF\",",
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
  # the design form: the designer's layers, the settings in `plot:`; the
  # report's own items after the layers, its own settings over the template's
  o2 <- o
  o2$content <- list(tfl_item("geom_hline", list(yintercept = 0.5)),
                     tfl_item("labs", list(y = "P")))
  o2$plot <- list(y_by = 0.25)
  r2 <- tfl_resolve_output(o2, s)
  expect_identical(vapply(r2$content, `[[`, "", "fn"),
                   c("km_curve", "censor_mark", "geom_hline", "labs"))
  expect_identical(r2$plot[c("x_label", "y_by")], list(x_label = "Time (Months)", y_by = 0.25))
  fd <- .fig_design_of_output(r2)
  expect_identical(fd$layers[[3L]][c("layer", "fn")], list(layer = "call", fn = "geom_hline"))
  expect_identical(fd$plot$add[[1L]]$fn, "labs")
  expect_identical(tfl_template_to_spec(o2, s)$plot$x_label, "Time (Months)")
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
  expect_error(tfl_resolve_output(o, s), "population ITT is not in `populations`")
  # a study that defines none stops too, as a table's analysis data does
  s$data$populations <- NULL
  o$population <- "SAF"
  expect_error(tfl_resolve_output(o, s), "population SAF is not in `populations`")
  # the semi join keeps the rows of the set's subjects: the same as the flag
  df <- adtte[adtte$PARAMCD == adtte$PARAMCD[1L], ]
  saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
  kept <- dplyr::semi_join(df, dplyr::select(adsl[adsl$SAFFL == "Y", ], USUBJID), by = "USUBJID")
  expect_identical(sort(unique(kept$USUBJID)), sort(intersect(unique(df$USUBJID), saf)))
})
