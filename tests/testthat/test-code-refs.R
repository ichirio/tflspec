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

test_that("tfl_output_data_names(): the ARD's data by name, what reads them; a figure's named steps", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "XYZ"))
  ans <- readLines(system.file("ai", "example", "onco-answer.md", package = "tflspec"),
                   encoding = "UTF-8")
  invisible(tfl_ai_unpack(ans, d))
  st <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  n <- tfl_output_data_names(outs[["T-14-2-1"]], st)
  expect_named(n, c("output_id", "object", "name", "kind", "from", "used_by"))
  expect_true("adrs_bor" %in% n$name)
  expect_identical(n$from[n$name == "adrs_bor"], "ADRS")
  expect_true(any(!is.na(n$used_by[n$name == "adrs_bor"])))
  expect_true("ard" %in% n$name[n$kind == "ard"])
  # a figure's steps: a named subset, a fit
  f <- tfl_output("F-1", "figure", data = list(steps = list(
    list(step = "read", dataset = "ADTTE"),
    list(step = "filter", expr = "PARAMCD == 'OS'", out = "os"),
    list(step = "survfit", name = "fit", time = "AVAL", censor = "CNSR"))))
  nf <- tfl_output_data_names(f, st)
  expect_identical(nf$name, c("os", "fit"))
  expect_identical(nf$kind, c("subset", "fit"))
  expect_identical(nf$from, c("ADTTE", "os"))
})
