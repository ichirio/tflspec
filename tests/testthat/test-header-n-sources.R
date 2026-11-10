# No built-in template makes an ARD with two different counts for one column
# at the same level (rtfreporter#944: the column header stops with "the ARD
# states two different counts for a column's N").  Each table template is
# built on the example study with the variations that change its ARD (total,
# stack, header_n, denominator, subset), through the ARD and the table step.

hn_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
}

# the example study, written once for the file (the datasets the templates read)
hn_env <- new.env(parent = emptyenv())
hn_study <- function() {
  if (is.null(hn_env$dir)) {
    d <- tempfile("tflspec-hn")
    dir.create(file.path(d, "adam"), recursive = TRUE)
    for (x in c("adsl", "adae", "adex", "adlb", "advs", "adrs", "adtte")) {
      haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                       file.path(d, "adam", paste0(x, ".xpt")))
    }
    tfl_ai_study_init(d, adam = "adam", study_id = "HN-1")
    hn_env$dir <- d
  }
  hn_env$dir
}
withr::defer(if (!is.null(hn_env$dir)) unlink(hn_env$dir, recursive = TRUE),
             testthat::teardown_env())

# a template's own arguments on the example study; a call changes them
hn_base <- list(
  hierarchical = list(hierarchy = c("AEBODSYS", "AEDECOD")),
  ae_max_grade = list(grade_levels = c("MILD", "MODERATE", "SEVERE")),
  ae_pt_freq = list(min_pct = 5),
  lab_abnormal = list(levels = c("LOW", "HIGH"), where = "ONTRTFL == \"Y\""),
  lab_shift = list(visit = "CYCLE 2 DAY 1", params = c("ALT", "AST")),
  shift = list(data_id = "adlb_shift", by_levels = c("Drug X", "Control")),
  param_change = list(data = "ADLB", params = c("ALT", "AST"), where = "ONTRTFL == \"Y\""),
  ancova = list(data = "ADVS", param = "SYSBP", visit = "CYCLE 2 DAY 1"),
  summary = list(continuous = "AGE"))

# the report: its template built to the table step; NULL, or what stopped it
hn_try <- function(tpl, ..., pop = "SAF") {
  args <- utils::modifyList(as.list(hn_base[[tpl]]), list(...))
  d <- hn_study()
  o <- tfl_output("T-1", "table", population = pop, label = "T-1", extends = "defaults",
                  template = list(name = tpl, args = args))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-1.yml"))
  owd <- setwd(d)
  on.exit(setwd(owd), add = TRUE)
  st <- tfl_read_study_spec(".")
  tryCatch({
    p <- suppressWarnings(suppressMessages(tflspec:::.output_build_content(o, st)))
    suppressWarnings(suppressMessages(rtfreporter::plan_apply(p)))
    NULL
  }, error = function(e) conditionMessage(e))
}

hn_expect_ok <- function(tpl, ..., label = tpl) {
  msg <- hn_try(tpl, ...)
  expect_null(msg, info = label)
  invisible(msg)
}

test_that("every table template, as its example: no two counts for one column", {
  hn_skip()
  for (tpl in c("ae_disc_soc_pt", "ae_eair", "ae_max_grade", "ae_overview", "ae_pt_freq",
                "ae_ser_soc_pt", "ae_soc_pt", "ancova", "demog", "disposition",
                "hierarchical", "lab_abnormal", "lab_shift", "orr", "param_change", "shift",
                "stf_t02_demog", "summary")) {
    hn_expect_ok(tpl)
  }
})

test_that("total = TRUE and stack = TRUE: the Total column and the stacked layer add no count", {
  hn_skip()
  for (tpl in c("demog", "summary", "hierarchical", "ae_soc_pt")) {
    hn_expect_ok(tpl, total = TRUE, label = paste(tpl, "total"))
  }
  hn_expect_ok("stf_t02_demog", total = FALSE, label = "stf_t02_demog total = FALSE")
  hn_expect_ok("demog", stack = TRUE, total = TRUE, label = "demog stack total")
})

test_that("subset: the same subjects in every analysis", {
  hn_skip()
  for (tpl in c("demog", "shift")) {
    hn_expect_ok(tpl, subset = "SEX == \"F\"", label = paste(tpl, "subset"))
  }
})

test_that("header_n: each source the template offers", {
  hn_skip()
  for (h in c("set", "none")) {
    hn_expect_ok("ae_soc_pt", header_n = h, label = paste("ae_soc_pt header_n", h))
  }
  hn_expect_ok("hierarchical", header_n = "none", label = "hierarchical header_n none")
  for (h in c("tested", "none")) {
    hn_expect_ok("lab_shift", header_n = h, label = paste("lab_shift header_n", h))
  }
  hn_expect_ok("ae_overview", header_n = "denominator", label = "ae_overview header_n denominator")
  hn_expect_ok("orr", header_n = "denominator", label = "orr header_n denominator")
})

test_that("denominator: a number, named numbers, \"data\", another analysis set", {
  hn_skip()
  named <- c("Drug X" = 60, Control = 55)
  hn_expect_ok("ae_soc_pt", denominator = 60, label = "ae_soc_pt denominator number")
  hn_expect_ok("ae_soc_pt", denominator = named, label = "ae_soc_pt denominator named")
  hn_expect_ok("ae_soc_pt", denominator = "data", label = "ae_soc_pt denominator data")
  hn_expect_ok("ae_soc_pt", denominator = "adsl_itt", label = "ae_soc_pt denominator adsl_itt")
  hn_expect_ok("ae_soc_pt", denominator = "data", header_n = "tested",
               label = "ae_soc_pt denominator data, header_n tested")
  hn_expect_ok("ae_soc_pt", denominator = "data", total = TRUE,
               label = "ae_soc_pt denominator data, total")
  hn_expect_ok("ae_pt_freq", denominator = named, label = "ae_pt_freq denominator named")
  hn_expect_ok("ae_max_grade", denominator = "data", label = "ae_max_grade denominator data")
  # one number with total = TRUE stops (D2b); named numbers sum to the Total
  hn_expect_ok("ae_ser_soc_pt", denominator = named, total = TRUE,
               label = "ae_ser_soc_pt denominator named, total")
  hn_expect_ok("hierarchical", denominator = "data", label = "hierarchical denominator data")
})

test_that("stat_block, on the report's own ARD: the ORR and its population", {
  hn_skip()
  adsl <- tflspec::adsl[tflspec::adsl$ITTFL == "Y", ]
  bor <- tflspec::adrs[tflspec::adrs$PARAMCD == "BOR" & tflspec::adrs$USUBJID %in% adsl$USUBJID, ]
  bor$RESP <- factor(ifelse(bor$AVALC %in% c("CR", "PR"), "Y", "N"), levels = c("Y", "N"))
  ard <- cards::bind_ard(
    cards::ard_tabulate(adsl, variables = "TRT01P"),
    cards::ard_strata(bor, .by = TRT01P, .f = function(.x) {
      cardx::ard_categorical_ci(.x, variables = RESP, value = RESP ~ "Y")
    }))
  p <- tfl_tpl(ard, "stat_block", variable = "RESP", by = "TRT01P",
               cells = list(N = "{N}", "ORR (%)" = "{n} ({estimate:.1f%})"))
  expect_no_error(suppressWarnings(suppressMessages(rtfreporter::plan_apply(p))))
})

test_that("the check itself: two tabulations of one arm over different subjects do stop", {
  hn_skip()
  adsl <- tflspec::adsl
  everyone <- cards::ard_tabulate(adsl, variables = TRT01A)
  everyone$analysis_id <- "GROUPN_ALL"
  women <- cards::ard_tabulate(adsl[adsl$SEX == "F", ], variables = TRT01A)
  women$analysis_id <- "GROUPN_F"
  body <- cards::ard_summary(adsl, by = TRT01A, variables = AGE,
                             statistic = ~ cards::continuous_summary_fns("mean"))
  ard <- dplyr::bind_rows(everyone, women, body)
  p <- rtfreporter::table_plan(rtfreporter::normalize_ard(ard), cols = "TRT01A")
  p <- rtfreporter::plan_col_header(p, "{col} (N={n})", values = list(n = TRUE))
  expect_error(suppressWarnings(suppressMessages(rtfreporter::plan_apply(p))),
               "two different counts")
})
