# A template parameter's value from the study standard: the report's
# population, the study's treatment (tfl_param(study = ))

# tflspec's adtte: ggsurvfit's, attached by an earlier test, has no TRT01A
# (under devtools::test() the helpers' copy is behind it)
adtte <- tflspec::adtte

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
  o <- tfl_output("T-9", "table", common = list(population = population),
                  template = list(name = name, args = args))
  tfl_resolve_output(o, s)
}
ss_an <- function(ro, id) Filter(function(a) identical(a$id, id), ro$ard$analyses)[[1L]]

test_that("the report's population: the template's population, written in its program", {
  s <- ss_study(NULL)
  ro <- ss_resolve("ae_soc_pt", list(), s)
  expect_identical(attr(ro, "template")$args$population, "SAF")
  # (with the study standard's look: its plan_stub, plan_blanks ... too)
  expect_identical(attr(ro, "template")$study[1L], "population")
  code <- tfl_content_code(ro, pipe = "|>")
  expect_match(paste(code, collapse = "\n"), "\"ae_soc_pt\",\\s+population = \"SAF\"")
  # the report's own argument wins, and says so
  expect_message(ro2 <- ss_resolve("ae_soc_pt", list(population = "SCRF"), s),
                 "overrides the study standard \\(output.common.population\\)")
  expect_identical(attr(ro2, "template")$args$population, "SCRF")
  # demog: the analysis set is the report's `population:` only, never an argument
  ro3 <- ss_resolve("demog", list(by = "TRT01A", continuous = "AGE"), s)
  expect_identical(attr(ro3, "template")$args$population, "SAF")
  expect_error(ss_resolve("demog", list(by = "TRT01A", continuous = "AGE", population = "SCRF"), s),
               "takes the analysis set from the report's `common: population`")
})

test_that("study.yml's settings: treatment is no template's value: by / group are the report's (canon 1.15)", {
  s <- ss_study("TRT01P")
  # (its default, not the study's treatment; the report writes its own)
  ro <- ss_resolve("ae_soc_pt", list(), s)
  expect_identical(ss_an(ro, "AE")$by, "TRT01A")
  expect_false("by" %in% attr(ro, "template")$study)
  ro <- ss_resolve("ae_soc_pt", list(by = "TRT01P"), s)
  expect_identical(ss_an(ro, "AE")$by, "TRT01P")
  # no template's by or group reads it
  for (nm in tfl_templates()$name) {
    ps <- tryCatch(tfl_template_params(nm), error = function(e) NULL)
    for (k in c("by", "group")) expect_null(ps[[k]]$study, info = paste(nm, k))
  }
  # a required one not written stops, the study's treatment or not
  km <- function(args) tfl_resolve_output(tfl_output("F-9", "figure", common = list(population = "SAF"),
                                                     template = list(name = "km", args = args)), s)
  expect_error(km(list(param = "PFS")), "needs 'by'")
  fo <- function(args) tfl_resolve_output(tfl_output("F-9", "figure", common = list(population = "SAF"),
                                                     template = list(name = "forest_hr", args = args)), s)
  expect_error(fo(list(param = "PFS", population = "SAF")), "needs 'group'")
  # demog, summary, disposition, ae_overview, param_change: `by` is required, never the
  # study's treatment (canon 1.14)
  for (nm in c("summary", "disposition", "ae_overview", "param_change")) {
    expect_null(tfl_template_params(nm)$by$study, info = nm)
  }
  expect_error(ss_resolve("summary", list(continuous = "AGE"), s), "needs 'by'")
  expect_error(ss_resolve("demog", list(continuous = "AGE"), s), "needs 'by'")
  ro <- ss_resolve("demog", list(continuous = "AGE", by = "TRT01A"), s)
  expect_identical(ss_an(ro, "CONT")$by, "TRT01A")
  expect_null(tfl_template_params("demog")$by$study)
})

test_that("study.yml's settings: sd_missing: what prints for a missing SD; the report's argument wins (#673)", {
  mark <- function(ro) {
    cl <- Filter(function(i) identical(i$fn, "plan_cells"), ro$content)[[1L]]$args$continuous
    cl[["Mean (SD)"]]
  }
  dm <- list(by = "TRT01A", continuous = "AGE")
  s <- ss_study("TRT01P")
  s$study$settings$sd_missing <- "NA"
  ro <- ss_resolve("demog", dm, s)
  expect_identical(attr(ro, "template")$args$sd_missing, "NA")
  expect_true("sd_missing" %in% attr(ro, "template")$study)
  expect_identical(mark(ro), c("{mean} ({sd})", "{mean} (NA)"))
  # the report's template: args: wins, and says so
  expect_message(ro2 <- ss_resolve("demog", c(dm, list(sd_missing = "")), s),
                 "overrides the study standard \\(settings.sd_missing\\)")
  expect_identical(mark(ro2), c("{mean} ({sd})", "{mean}"))
  # param_change too
  pc <- list(data = "ADLB", by = "TRT01A", params = "ALT")
  expect_identical(mark(ss_resolve("param_change", pc, s)), c("{mean} ({sd})", "{mean} (NA)"))
  expect_identical(mark(suppressMessages(ss_resolve("param_change", c(pc, list(sd_missing = "~")), s))),
                   c("{mean} ({sd})", "{mean} (~)"))
  # no setting: the template's `-`
  expect_identical(mark(ss_resolve("demog", dm, ss_study(NULL))), c("{mean} ({sd})", "{mean} (-)"))
  # a setting with braces stops
  s$study$settings$sd_missing <- "{sd}"
  expect_error(ss_resolve("demog", dm, s), "sd_missing")
})

test_that("first to last: the report's argument, the study standard, the variant's, from's", {
  on.exit(rm("demog_arm", envir = .tpl_registry))
  tfl_template_variant("demog_arm", from = "ae_soc_pt", defaults = list(by = "ARM"))
  # the variant's default over from's; the study's treatment is no layer
  # of `by` (canon 1.15)
  ro <- ss_resolve("demog_arm", list(), ss_study("TRT01P"))
  expect_identical(ss_an(ro, "AE")$by, "ARM")
  ro <- ss_resolve("demog_arm", list(), ss_study(NULL))
  expect_identical(ss_an(ro, "AE")$by, "ARM")
  # the report's argument over the variant's
  ro <- suppressMessages(ss_resolve("demog_arm", list(by = "SEX"), ss_study("TRT01P")))
  expect_identical(ss_an(ro, "AE")$by, "SEX")
  # the study standard's layer, by a parameter that reads one (settings.sd_missing)
  s <- ss_study(NULL)
  s$study$settings$sd_missing <- "NA"
  expect_identical(attr(ss_resolve("demog", list(by = "TRT01A", continuous = "AGE"), s), "template")$args$sd_missing,
                   "NA")
})

test_that("tfl_test_templates(): the analysis set with no study source; the treatment is none", {
  expect_false(any(grepl("no study source", tfl_test_templates()$status)))
  d <- .tpl_get("summary", "test")
  p <- c(d$params, list(tfl_param("pop", "SAF", type = "text", study = NULL),
                        tfl_param("arm", "ARM", type = "vars", study = NULL)))
  suppressMessages(tfl_template("ss_arm", "table", "summary", params = p, body = d$body,
                                example = list(population = "SAF")))
  on.exit(rm("ss_arm", envir = .tpl_registry))
  r <- tfl_test_templates("ss_arm")
  expect_identical(r$status[r$param %in% "pop"], "no study source (population)")
  # (the treatment: the report's to write, no study source wanted, canon 1.15)
  expect_false("arm" %in% r$param)
  # (summary's `by` reads no study value: canon 1.14)
  expect_null(tfl_template_params("ss_arm")$by$study)
})

test_that("km_survival: the analysis set is the data's, no step keeps it; one the study does not define stops", {
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(d)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"),
                       full.names = TRUE), d, recursive = TRUE)
  writeLines(c("populations:", "  - id: SAF", "    dataset: ADSL", "    where: !r SAFFL == \"Y\""),
             file.path(d, "data.yml"))
  s <- tfl_read_study_spec(d)
  o <- tfl_output("F-9", "figure", common = list(population = "SAF"),
                  template = list(name = "km", args = list(param = "PFS", by = "TRT01A",
                                                                risk_table = FALSE, median_line = FALSE,
                                                                pvalue = FALSE)))
  ro <- tfl_resolve_output(o, s)
  st <- ro$data$steps
  # (A2: one rule -- the report program makes the set's subjects and joins
  # the records to them; the template's data says the set, no step of it)
  expect_identical(ro$data$population, "SAF")
  expect_length(Filter(function(x) identical(x$how, "semi"), st), 0L)
  expect_false(any(vapply(st, function(x) identical(x$step, "flag"), NA)))
  expect_identical(tfl_template_to_spec(o, s)$data$population, "SAF")
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
  # a left join still needs its variables
  left <- list(list(step = "read", dataset = "ADTTE"), list(step = "join", dataset = "ADSL"))
  expect_true(any(tfl_check_fig_design(tfl_fig_design(data = left))$field %in% "vars"))
  # an id the study does not define
  o$common$population <- "ITT"
  expect_error(tfl_resolve_output(o, s), "population ITT is not in `populations`")
  # a study that defines none stops too, as a table's analysis data does
  s$data$populations <- NULL
  o$common$population <- "SAF"
  expect_error(tfl_resolve_output(o, s), "population SAF is not in `populations`")
})
