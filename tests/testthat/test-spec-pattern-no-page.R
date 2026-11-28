# Patterns are gone (canon 11-12 :380, :433): a spec/patterns/ file and a
# report's `extends: patterns/<name>` stop as old forms.  What the sheets
# carry of a study without them: the study's code lists (study_codelists)
# and a report's own (#662).  Package fictional data (adsl, adlb); a study
# per case.

pnp_study <- function(pattern = NULL, page = NULL, defaults = NULL, own = NULL, extends = NULL,
                      env = parent.frame()) {
  skip_if_not_installed("haven")
  d <- tempfile("pnp-")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  withr::defer(unlink(d, recursive = TRUE), envir = env)
  for (x in c("adsl", "adlb")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)), file.path(d, "adam", paste0(x, ".xpt")))
  }
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "DMY-001"))
  spec <- file.path(d, "spec")
  if (length(defaults)) cat(c("codelists:", paste0("  ", defaults), ""), sep = "\n",
                            file = file.path(spec, "defaults.yml"), append = TRUE)
  if (!is.null(pattern) || !is.null(page)) {
    dir.create(file.path(spec, "patterns"), showWarnings = FALSE)
    writeLines(c(if (length(pattern)) c("codelists:", paste0("  ", pattern)), page),
               file.path(spec, "patterns", "pp.yml"))
  }
  if (is.null(extends)) extends <- if (!is.null(pattern) || !is.null(page)) "[patterns/pp, defaults]" else "[defaults]"
  writeLines(c("id: T-A", "kind: table", "common: {population: SAF}", paste0("extends: ", extends), own,
               "analysis:", "  analyses:", "    - id: A1", "      method: categorical",
               "      population: SAF", "      by: [ARM]", "      variables: [SEX]",
               "content:", "  - table_plan: {cols: ARM}"),
             file.path(spec, "outputs", "T-A.yml"))
  d
}

# the study written from its sheets into a copy of its folder
pnp_round <- function(d, env = parent.frame()) {
  sh <- tfl_sheets_from_yaml(d)
  d2 <- tempfile("pnp-rt-")
  dir.create(d2)
  withr::defer(unlink(d2, recursive = TRUE), envir = env)
  file.copy(file.path(d, "adam"), d2, recursive = TRUE)
  suppressMessages(tfl_yaml_from_sheets(sh, d2, overwrite = TRUE))
  list(sheets = sh, dir = d2)
}

pnp_code <- function(d) {
  st <- tfl_read_study_spec(d)
  out <- tfl_resolve_output(tfl_read_outputs(d)[["T-A"]], st)
  list(ard = paste(tfl_ard_code(st, output_id = "T-A", save = FALSE), collapse = "\n"),
       report = paste(tfl_report_code(out, study = st), collapse = "\n"))
}

# the round trip of a study; `same`: the code of the study and of its
# copy is identical
pnp_check <- function(d, same = TRUE, env = parent.frame()) {
  rt <- pnp_round(d, env)
  expect_no_error(tfl_table_spec(rt$sheets$table))
  expect_false(dir.exists(file.path(rt$dir, "spec", "patterns")))
  if (same) {
    a <- pnp_code(d)
    b <- pnp_code(rt$dir)
    expect_identical(b$ard, a$ard)
    expect_identical(b$report, a$report)
  }
  rt
}

pnp_pat <- "SEX: [{value: M, label: PatM}, {value: F, label: PatF}]"
pnp_def <- "SEX: [{value: M, label: DefM}, {value: F, label: DefF}]"
pnp_notes <- function(sh) sh$not_converted[sh$not_converted$part == "codelists", ]

test_that("a pattern file stops the read: an old form, whatever it holds", {
  skip_on_cran()
  for (page in list("content: []", c("page:", "  rtf_page: {orientation: landscape}"))) {
    d <- pnp_study(page = page)
    e <- tryCatch(tfl_read_study_spec(d), error = function(e) e)
    expect_s3_class(e, "tflspec_old_form")
    expect_identical(e$form, "patterns_dir")
    expect_match(conditionMessage(e), "spec/patterns/: pp.yml (a pattern file) is no longer read", fixed = TRUE)
  }
  d <- pnp_study(pattern = pnp_pat)
  expect_error(tfl_read_study_spec(d), class = "tflspec_old_form")
})

test_that("a report's extends: patterns/<name> stops: an old form", {
  skip_on_cran()
  d <- pnp_study(extends = "[patterns/pp, defaults]")
  e <- tryCatch(tfl_read_outputs(d), error = function(e) e)
  expect_s3_class(e, "tflspec_old_form")
  expect_identical(e$form, "extends_pattern")
  expect_match(conditionMessage(e), "`extends: patterns/pp` is no longer read", fixed = TRUE)
})

test_that("the defaults' code lists cross the sheets as study_codelists (#662, A7)", {
  skip_on_cran()
  d <- pnp_study(defaults = pnp_def)
  sh <- tfl_sheets_from_yaml(d)
  expect_identical(nrow(pnp_notes(sh)), 0L)
  expect_identical(sh$study_codelists$label, c("DefM", "DefF"))
  d2 <- tempfile("pnp-rt-")
  dir.create(d2)
  withr::defer(unlink(d2, recursive = TRUE))
  file.copy(file.path(d, "adam"), d2, recursive = TRUE)
  suppressMessages(tfl_yaml_from_sheets(sh, d2, overwrite = TRUE))
  # (the study's lists, not a layer's)
  expect_length(tfl_read_study_spec(d2)$defaults$codelists, 0L)
  expect_identical(tfl_read_study_spec(d2)$codelists, tfl_read_study_spec(d)$codelists)
})

test_that("a report's own list is a row, and says nothing (#662)", {
  skip_on_cran()
  own <- c("codelists:", "  SEX: [{value: M, label: OwnM}, {value: F, label: OwnF}]")
  d <- pnp_study(own = own)
  rt <- pnp_check(d)
  expect_identical(rt$sheets$table$codelists$label, c("OwnM", "OwnF"))
  expect_identical(nrow(pnp_notes(rt$sheets)), 0L)
  expect_identical(names(tfl_read_outputs(rt$dir)[["T-A"]]$codelists), "SEX")
})
