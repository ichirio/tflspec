# The ARD programs of a study, run end to end by a child R that has no
# tflspec (the programs run on base R, cards and dplyr): a template's
# derive step on a data that lacks its order variable, a text comparison on
# a list column, and the denominator's join of a column the subjects' data
# does not have.

ep_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("haven")
}
ep_study <- function(root, datasets) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in datasets) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "EP-1")
  d
}
# the output's program, run by a child R from the study folder (the child
# looks for packages where this R does; system2(env = ) is Unix-only); the
# ARD it made, and what the child printed
ep_run_child <- function(d, id) {
  rscript <- file.path(R.home("bin"), "Rscript")
  withr::local_envvar(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  st <- tfl_read_study_spec(d)
  code <- tfl_ard_code(st, output_id = id, save = FALSE, dir = d)
  withr::local_dir(d)
  writeLines(c(code, "saveRDS(ard, 'ard.rds')"), "prog.R")
  out <- suppressWarnings(system2(rscript, "prog.R", stdout = TRUE, stderr = TRUE))
  list(ard = if (file.exists("ard.rds")) readRDS("ard.rds"), out = out, code = code)
}

test_that("lab_abnormal on a data without PARAMN: the parameters in their own order, a message", {
  ep_skip()
  expect_false("PARAMN" %in% names(adlb))
  d <- ep_study(withr::local_tempdir(), c("adsl", "adlb"))
  f <- file.path(d, "spec", "outputs", "T-LA.yml")
  tfl_write_output(tfl_output("T-LA", "table", population = "SAF", extends = "defaults",
                              template = list(name = "lab_abnormal",
                                              args = list(levels = c("LOW", "HIGH")))), f)
  r <- ep_run_child(d, "T-LA")
  expect_true(any(grepl("PARAM = reorder_by(PARAM, PARAMN)", r$code, fixed = TRUE)))
  expect_false(is.null(r$ard), info = paste(r$out, collapse = "\n"))
  expect_true(any(grepl("reorder_by(): the data has no PARAMN", r$out, fixed = TRUE)))
  expect_true(any(r$ard$analysis_id == "ABNORMAL"))
  # the parameters as factor levels, in their own order
  pm <- unique(unlist(r$ard$group2_level[r$ard$group2 %in% "PARAM"]))
  expect_true(length(pm) >= 2L)
  # the same in this R: tfl_build_ard() with the message
  st <- tfl_read_study_spec(d)
  expect_message(a <- tfl_build_ard(st, output_id = "T-LA", dir = d, save = FALSE),
                 "reorder_by\\(\\): the data has no PARAMN")
  expect_identical(nrow(a), nrow(r$ard))
})

test_that("param_change writes the same step for PARAM and AVISIT", {
  x <- tfl_template_expand("param_change", list(population = "SAF", data = "ADVS"))
  dv <- as.character(x$ard$analysis_data[[2L]]$derive)
  expect_match(dv, "PARAM = reorder_by(PARAM, PARAMN)", fixed = TRUE)
  expect_match(dv, "AVISIT = reorder_by(AVISIT, AVISITN)", fixed = TRUE)
})

test_that("reorder_by(): the order variable's order, or the values' own with a message", {
  e <- new.env()
  eval(parse(text = tfl_helpers_code(), encoding = "UTF-8"), envir = e)
  d <- data.frame(PARAM = c("ALT", "AST", "ALT"), PARAMN = c(2, 1, 2))
  expect_identical(levels(e$reorder_by(d$PARAM, d$PARAMN)), c("AST", "ALT"))
  expect_message(f <- dplyr::mutate(d, PARAM = e$reorder_by(PARAM, NOPE))$PARAM,
                 "the data has no NOPE")
  expect_identical(levels(f), c("ALT", "AST"))
})

test_that("ancova: the differences' filter compares text (a list column of names)", {
  ep_skip()
  skip_if_not_installed("cardx")
  skip_if_not_installed("emmeans")
  d <- ep_study(withr::local_tempdir(), c("adsl", "advs"))
  f <- file.path(d, "spec", "outputs", "T-AN.yml")
  tfl_write_output(tfl_output("T-AN", "table", population = "SAF", extends = "defaults",
                              template = list(name = "ancova", args = list(
                                data = "ADVS", param = "SYSBP", visit = "CYCLE 2 DAY 1"))), f)
  r <- ep_run_child(d, "T-AN")
  expect_true(any(grepl("endsWith(as.character(unlist(variable_level))", r$code, fixed = TRUE)))
  expect_false(is.null(r$ard), info = paste(r$out, collapse = "\n"))
  expect_false(any(grepl("non-character", r$out)))
  expect_true(any(r$ard$analysis_id == "LSDIFF"))
  expect_true(any(r$ard$variable == "DIFF"))
})

test_that("a denominator's by column the subjects' data lacks (AESEV): the records' own", {
  ep_skip()
  d <- ep_study(withr::local_tempdir(), c("adsl", "adae"))
  f <- file.path(d, "spec", "outputs", "T-AE.yml")
  tfl_write_output(tfl_output(
    "T-AE", "table", population = "SAF", extends = "defaults", label = "TEAE by severity",
    ard = list(
      analysis_data = list(
        list(id = "adsl_saf", from = "ADSL", population = "SAF"),
        list(id = "adae_saf", from = "ADAE", subjects = "adsl_saf",
             where = tfl_r("TRTEMFL == \"Y\""))),
      analyses = list(
        list(id = "GROUPN", method = "categorical", data = "adsl_saf", variables = "TRT01A"),
        list(id = "TEAE", method = "hierarchical", data = "adae_saf",
             by = c("TRT01A", "AESEV"), variables = c("AEBODSYS", "AEDECOD"),
             denominator = "adsl_saf", args = "over_variables = TRUE"))),
    data = list(source = "ard", object = "ard"),
    content = list(tfl_item("table_plan", list(cols = c("TRT01A", "AESEV"),
                                               rows = list(group1 = "AEBODSYS"),
                                               label = "AEDECOD")))), f)
  r <- ep_run_child(d, "T-AE")
  # the join takes from ADSL only what it has; AESEV is dropped from nothing
  expect_true(any(grepl("select(-any_of(intersect(c(\"TRT01A\", \"AESEV\"), names(adsl_saf))))",
                        r$code, fixed = TRUE)))
  expect_true(any(grepl("select(adsl_saf, USUBJID, any_of(c(\"TRT01A\", \"AESEV\")))",
                        r$code, fixed = TRUE)))
  expect_false(is.null(r$ard), info = paste(r$out, collapse = "\n"))
  expect_false(any(grepl("doesn't exist", r$out)))
  a <- r$ard[r$ard$analysis_id %in% "TEAE", ]
  expect_true(any(a$group2 %in% "AESEV"))
  expect_setequal(unique(unlist(a$group2_level[a$group2 %in% "AESEV"])),
                  unique(adae$AESEV[adae$TRTEMFL == "Y"]))
})
