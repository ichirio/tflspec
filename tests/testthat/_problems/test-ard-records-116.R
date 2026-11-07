# Extracted from test-ard-records.R:116

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "tflspec", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
rc_df <- function(...) as.data.frame(list(...), stringsAsFactors = FALSE)
rc_spec <- function(records = rc_rows(), an = rc_an(), check = TRUE) {
  s <- list(
    study = rc_df(key = "id", value = "USUBJID"),
    datasets = rc_df(dataset = c("ADSL", "ADAE"), path = c("adsl.rds", "adae.rds")),
    populations = rc_df(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    records = records,
    analyses = an)
  if (check) tfl_ard_spec(s) else s
}
rc_rows <- function() rc_df(
  record_id = c("TEAE", "TESAE"), dataset = "ADAE",
  where = c("TRTEMFL == \"Y\"", "TRTEMFL == \"Y\" & AESER == \"Y\""),
  label = c("Treatment-emergent adverse events", "Treatment-emergent SAEs"),
  any_label = c(NA, "Any SAE"))
rc_an <- function() rc_df(
  output_id = c("T1", "T1"), analysis_id = c("SAE", "SEV"),
  method = "cards::ard_stack_hierarchical", records = c("TESAE", "TEAE"),
  population_id = "SAF", where = c(NA, "AESEV == \"SEVERE\" | AESER == \"Y\""),
  by = "TRT01A", variables = "AEBODSYS | AEDECOD", denominator = "population",
  args = "over_variables = TRUE")

# test -------------------------------------------------------------------------
skip_if_not_installed("haven")
d <- withr_tempdir()
tplfig_study(d, c("adsl", "adae"), "REC-1")
cat(paste0("records:\n",
             "  - {id: TESAE, dataset: ADAE, where: !r TRTEMFL == \"Y\" & AESER == \"Y\", ",
             "label: Treatment-emergent SAEs, any_label: Any SAE}\n"),
      file = file.path(d, "spec", "data.yml"), append = TRUE)
o <- tfl_output("T-SAE", "table",
    ard = list(analyses = list(list(id = "AE", method = "cards::ard_stack_hierarchical",
      records = "TESAE", population = "SAF", by = "TRT01A",
      variables = c("AEBODSYS", "AEDECOD"), denominator = "population",
      args = tfl_r("over_variables = TRUE")))),
    content = list(tfl_item("normalize_ard", list(hierarchy = c("AEBODSYS", "AEDECOD"))),
                   tfl_item("table_plan", list(cols = "TRT01A"))))
tfl_write_output(o, file.path(d, "spec", "outputs", "T-SAE.yml"))
sh <- tfl_sheets_from_yaml(d)
expect_identical(sh$ard$records$record_id, "TESAE")
expect_identical(sh$ard$records$any_label, "Any SAE")
expect_identical(sh$ard$analyses$records[sh$ard$analyses$output_id == "T-SAE"], "TESAE")
st <- tfl_read_study_spec(d)
code <- tfl_ard_code(st, output_id = "T-SAE", save = FALSE, part = "body")
expect_true(any(startsWith(code, "# adae_sub: TESAE")))
res <- tfl_resolve_output(tfl_read_outputs(d)[["T-SAE"]], st)
nz <- Filter(function(it) identical(it$fn, "normalize_ard"), res$content)[[1L]]
expect_identical(nz$args$overall, "Any SAE")
out <- withr_tempdir()
suppressMessages(tfl_yaml_from_sheets(sh, out))
