# tfl_code_refs(): the names a SPEC's R code uses, for a rename to warn of

test_that("the names code reads: symbols, not strings or comments; code that does not parse by words", {
  code <- c(where = "AGE >= 65 & USUBJID %in% adsl_sub$USUBJID",
            derive = "x <- n_jp / 2  # adae_sub not used",
            label = "\"adsl_sub\"",
            broken = "adae_sub |> filter(")
  r <- tfl_code_refs(code, c("adsl_sub", "n_jp", "adae_sub"))
  expect_identical(r$code, c("where", "derive", "broken"))
  expect_identical(r$name, c("adsl_sub", "n_jp", "adae_sub"))
  # a list of !r values, unnamed: positions
  r2 <- tfl_code_refs(list(tfl_r("filter(adsl_sub, SAFFL == 'Y')"), NA_character_), "adsl_sub")
  expect_identical(r2$code, "1")
  expect_identical(nrow(tfl_code_refs("AGE > 1", "adsl_sub")), 0L)
  expect_error(tfl_code_refs(1, "a"), "character vectors")
})
