# Named sets of records (sheet `records`; data.yml `records:`): an
# analysis's `records` for its dataset and condition; `any_label` the "any"
# row of its hierarchy

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

test_that("an analysis reads its records' dataset under their condition, then its own", {
  code <- tfl_ard_code(rc_spec(), output_id = "T1", save = FALSE, part = "body")
  txt <- paste(code, collapse = "\n")
  expect_true("# adae_sub: TESAE — Treatment-emergent SAEs (TRTEMFL == \"Y\" & AESER == \"Y\"); of the adsl_sub subjects"
              %in% code)
  expect_match(txt, "filter(TRTEMFL == \"Y\", AESER == \"Y\")", fixed = TRUE)
  # (the analysis's condition with an | at its top in parentheses)
  expect_match(txt, "# adae_sub2: TEAE — Treatment-emergent adverse events", fixed = TRUE)
  expect_match(txt, "TRTEMFL == \"Y\" & (AESEV == \"SEVERE\" | AESER == \"Y\")", fixed = TRUE)
  expect_identical(tfl_ard_data_names(rc_spec())$data, c("adae_sub", "adae_sub2"))
})

test_that("the ARD: the records' subjects and the analysis set's denominator", {
  skip_if_not_installed("cards")
  dir <- withr_tempdir()
  adsl <- cards::ADSL
  adsl$TRT01A <- adsl$ARM
  adae <- cards::ADAE
  adae$TRTEMFL <- "Y"
  saveRDS(adsl, file.path(dir, "adsl.rds"))
  saveRDS(adae, file.path(dir, "adae.rds"))
  a <- tfl_build_ard(rc_spec(), dir = dir, save = FALSE)
  ov <- a[a$analysis_id == "SAE" & a$variable == "..ard_hierarchical_overall.." &
            a$stat_name == "n", ]
  saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
  ser <- adae[adae$AESER == "Y" & adae$USUBJID %in% saf, ]
  expect_equal(sum(as.numeric(unlist(ov$stat))), length(unique(ser$USUBJID)))
})

test_that("the sheet's problems and an analysis's", {
  probs <- function(...) tflspec:::.ard_spec_problems(tflspec:::.ard_spec_shaped(rc_spec(..., check = FALSE)))$message
  r <- rc_rows(); r$dataset[2L] <- "ADXX"
  expect_true(any(grepl("dataset ADXX is not one of the datasets", probs(records = r), fixed = TRUE)))
  r <- rc_rows(); r$record_id[2L] <- "TEAE"
  expect_true(any(grepl("records TEAE: named twice", probs(records = r), fixed = TRUE)))
  r <- rc_rows(); r$where[1L] <- "TRTEMFL =="
  expect_true(any(grepl("the condition is not R", probs(records = r), fixed = TRUE)))
  an <- rc_an(); an$records[1L] <- "TEXX"
  expect_true(any(grepl("records TEXX is not one of the records sheet's (TEAE, TESAE)",
                        probs(an = an), fixed = TRUE)))
  an <- rc_an(); an$dataset <- c("ADAE", NA)
  expect_true(any(grepl("leave `dataset` and `data` blank", probs(an = an), fixed = TRUE)))
})

test_that("the \"any\" row: the records' any_label, else Any <id>; the table's own wins", {
  sp <- tfl_table_spec(tables = data.frame(output_id = "T1", cols = "TRT01A",
                                           rows = "group1 = AEBODSYS"))
  x <- rc_spec()
  expect_match(tfl_normalize_code(sp, x, "T1"), "overall = \"Any SAE\"", fixed = TRUE)
  an <- rc_an()[2:1, ]
  expect_match(tfl_normalize_code(sp, rc_spec(an = an), "T1"), "overall = \"Any TEAE\"",
               fixed = TRUE)
  sp2 <- tfl_table_spec(tables = data.frame(output_id = "T1", cols = "TRT01A",
                                            rows = "group1 = AEBODSYS",
                                            overall_label = "Subjects with an SAE"))
  expect_match(tfl_normalize_code(sp2, x, "T1"), "overall = \"Subjects with an SAE\"",
               fixed = TRUE)
})

test_that("YAML: data.yml records: and a report's records: id; the any row in its normalize_ard()", {
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
  back <- tfl_sheets_from_yaml(out)
  expect_identical(back$ard$records, sh$ard$records)
})
