# A denominator of numbers: one number (every column's N) or one a value
# of the by (c("Drug X" = 86, ...); YAML {Drug X: 86, ...})

dn_df <- function(...) as.data.frame(list(...), stringsAsFactors = FALSE)

dn_spec <- function(den_ae, den_sex, check = TRUE) {
  s <- list(
    study = dn_df(key = "id", value = "USUBJID"),
    datasets = dn_df(dataset = c("ADSL", "ADAE"), path = c("adsl.rds", "adae.rds")),
    populations = dn_df(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analyses = dn_df(
      output_id = "T1",
      analysis_id = c("AE", "SEX"),
      method = c("cards::ard_stack_hierarchical", "cards::ard_tabulate"),
      dataset = c("ADAE", "ADSL"),
      population_id = "SAF",
      by = "TRT01A",
      variables = c("AEBODSYS | AEDECOD", "SEX"),
      denominator = c(den_ae, den_sex)))
  if (check) tfl_ard_spec(s) else s
}

dn_arms <- c("Placebo" = 100L, "Xanomeline High Dose" = 90L, "Xanomeline Low Dose" = 80L)
dn_text <- sprintf("c(%s)", paste(sprintf("\"%s\" = %d", names(dn_arms), dn_arms), collapse = ", "))

test_that("numbers are read: one, or one a value; else NULL or NA", {
  expect_identical(tflspec:::.den_numbers("86"), 86L)
  expect_identical(tflspec:::.den_numbers(dn_text), dn_arms)
  expect_null(tflspec:::.den_numbers("population"))
  expect_null(tflspec:::.den_numbers("adsl_saf"))
  expect_identical(tflspec:::.den_numbers("c(86, 85)"), NA)
  expect_identical(tflspec:::.den_numbers("c(A = 1.5)"), NA)
  expect_identical(tflspec:::.den_numbers("c(A = 1, A = 2)"), NA)
})

test_that("the code: an integer, or that many subjects of each value of the by", {
  x <- dn_spec(dn_text, "200")
  code <- paste(tfl_ard_code(x, output_id = "T1", save = FALSE, part = "body"), collapse = "\n")
  expect_match(code, "denominator = 200L", fixed = TRUE)
  expect_match(code, paste0("data.frame(TRT01A = rep(c(\"Placebo\", \"Xanomeline High Dose\", ",
                            "\"Xanomeline Low Dose\"), c(100L, 90L, 80L)), USUBJID = seq_len(270L))"),
               fixed = TRUE)
  expect_true(is.language(str2lang(paste0("{", code, "\n}"))))
})

test_that("the ARD's N: the numbers given, for a nested count and a tabulation", {
  skip_if_not_installed("cards")
  dir <- withr_tempdir()
  adsl <- cards::ADSL
  adsl$TRT01A <- adsl$ARM
  adae <- cards::ADAE
  saveRDS(adsl, file.path(dir, "adsl.rds"))
  saveRDS(adae, file.path(dir, "adae.rds"))
  a <- tfl_build_ard(dn_spec(dn_text, dn_text), dir = dir, save = FALSE)
  for (id in c("AE", "SEX")) {
    n <- a[a$analysis_id == id & a$stat_name == "N" & !is.na(a$group1), ]
    got <- tapply(as.numeric(unlist(n$stat)), unlist(n$group1_level), unique)
    expect_equal(as.numeric(got[names(dn_arms)]), as.numeric(dn_arms), info = id)
  }
  b <- tfl_build_ard(dn_spec("300", "300"), dir = dir, save = FALSE)
  n <- b[b$analysis_id == "SEX" & b$stat_name == "N", ]
  expect_identical(unique(as.numeric(unlist(n$stat))), 300)
})

test_that("a denominator of numbers that cannot be is a problem", {
  p <- tflspec:::.ard_spec_problems(dn_spec("c(86, 85)", NA, check = FALSE))
  expect_true(any(grepl("each named by a value of the by", p$message, fixed = TRUE)))
  s <- dn_spec(dn_text, NA, check = FALSE)
  s$analyses$by[1L] <- "TRT01A | SEX"
  p <- tflspec:::.ard_spec_problems(s)
  expect_true(any(grepl("needs one by variable", p$message, fixed = TRUE)))
  p <- tflspec:::.ard_spec_problems(dn_spec("lots", NA, check = FALSE))
  expect_true(any(grepl("or numbers (86", p$message, fixed = TRUE)))
  expect_s3_class(dn_spec("86", dn_text), "tfl_ard_spec")
})

test_that("YAML: a map of numbers is the sheet's c(...) and back", {
  m <- list(`Drug X` = 86L, Placebo = 85L)
  cell <- tflspec:::.den_cell(m)
  expect_identical(cell, "c(\"Drug X\" = 86, \"Placebo\" = 85)")
  expect_identical(tflspec:::.den_yaml(cell), list(`Drug X` = 86L, Placebo = 85L))
  expect_identical(tflspec:::.den_yaml("86"), 86L)
  expect_identical(tflspec:::.den_cell(86L), 86L)
  expect_identical(tflspec:::.den_yaml("adsl_saf"), "adsl_saf")
})

test_that("a YAML study: the report's map of numbers is the program's, and comes back a map", {
  skip_if_not_installed("haven")
  d <- withr_tempdir()
  tplfig_study(d, c("adsl", "adae"), "DEN-1")
  arms <- sort(unique(as.character(tflspec::adsl$TRT01A)))
  den <- stats::setNames(as.list(seq_along(arms) * 10L), arms)
  o <- tfl_output("T-DEN", "table", ard = list(analyses = list(
    list(id = "AE", method = "cards::ard_stack_hierarchical", dataset = "ADAE",
         population = "SAF", by = "TRT01A", variables = c("AEBODSYS", "AEDECOD"),
         denominator = den))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-DEN.yml"))
  sh <- tfl_sheets_from_yaml(d)
  an <- sh$ard$analyses
  expect_identical(an$denominator[an$output_id == "T-DEN"], tflspec:::.den_cell(den))
  st <- tfl_read_study_spec(d)
  a <- suppressMessages(tfl_build_ard(st, dir = d, output_id = "T-DEN", save = FALSE))
  n <- a[a$stat_name == "N" & !is.na(a$group1), ]
  got <- tapply(as.numeric(unlist(n$stat)), unlist(n$group1_level), unique)
  expect_equal(as.numeric(got[arms]), as.numeric(unlist(den)))
  out <- withr_tempdir()
  suppressMessages(tfl_yaml_from_sheets(sh, out))
  f <- list.files(out, "^T-DEN[.]yml$", recursive = TRUE, full.names = TRUE)
  back <- tfl_read_output(f[1L])
  expect_identical(back$ard$analyses[[1L]]$denominator, den)
})
