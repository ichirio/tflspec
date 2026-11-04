test_that("the sample study writes the ADaM and the documents, and no answer", {
  d <- tempfile("aisample")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  r <- tfl_ai_sample_study(d)
  expect_identical(as.character(r), d)
  ext <- if (requireNamespace("haven", quietly = TRUE)) ".xpt" else ".rds"
  expect_setequal(list.files(file.path(d, "adam")),
                  paste0(c("adsl", "adae", "adlb", "advs", "adtte"), ext))
  expect_setequal(list.files(file.path(d, "inputs")),
                  c("toc.xlsx", "shells.xlsx", "adam_spec.xlsx", "define.xml", "sap.docx"))
  expect_true(file.exists(file.path(d, "GUIDE-ja.md")))
  expect_false(dir.exists(file.path(d, "spec")))
  expect_false(dir.exists(file.path(d, "ai")))
  expect_true(all(file.exists(attr(r, "files"))))
  expect_error(tfl_ai_sample_study(d), "Already there")
  expect_no_error(tfl_ai_sample_study(d, overwrite = TRUE))
})

test_that("the sample ADaM: three arms, FAS two more than SAF, one study", {
  ds <- tflspec:::.ai_sample_adam()
  adsl <- ds$adsl
  expect_identical(unique(adsl$STUDYID), "DMY-001")
  expect_identical(levels(adsl$TRT01P), c("Placebo", "Drug A 10 mg", "Drug A 20 mg"))
  expect_true(all(startsWith(as.character(adsl$USUBJID), "DMY-001-")))
  expect_identical(sum(adsl$FASFL == "Y", na.rm = TRUE), sum(adsl$SAFFL == "Y", na.rm = TRUE) + 2L)
  expect_identical(names(adsl)[match("ITTFL", names(adsl)) + 1L], "FASFL")
  expect_true(all(ds$adae$USUBJID %in% adsl$USUBJID))
})

test_that("the sample ADSL has the baseline BMI the T-DM shell lists, and the spec says so", {
  adsl <- tflspec:::.ai_sample_adam()$adsl
  expect_true(all(c("HEIGHTBL", "WEIGHTBL", "BMIBL") %in% names(adsl)))
  expect_identical(attr(adsl$BMIBL, "label"), "Baseline BMI (kg/m2)")
  expect_identical(attr(adsl$HEIGHTBL, "label"), "Baseline Height (cm)")
  expect_identical(attr(adsl$WEIGHTBL, "label"), "Baseline Weight (kg)")
  expect_false(anyNA(adsl$BMIBL))
  expect_true(all(adsl$BMIBL > 15 & adsl$BMIBL < 45))
  expect_equal(adsl$BMIBL, round(adsl$WEIGHTBL / (adsl$HEIGHTBL / 100)^2, 1), ignore_attr = TRUE)
  # the other variables are as they were; the random seed is put back
  set.seed(1)
  s1 <- .Random.seed
  tflspec:::.ai_sample_adam()
  expect_identical(.Random.seed, s1)
  # the documents the agent reads list them
  dx <- paste(readLines(system.file("ai-sample", "define.xml", package = "tflspec"), warn = FALSE),
              collapse = "
")
  expect_match(dx, 'OID="IT.ADSL.BMIBL"', fixed = TRUE)
  skip_if_not_installed("readxl")
  v <- readxl::read_excel(system.file("ai-sample", "adam_spec.xlsx", package = "tflspec"),
                          sheet = "Variables")
  expect_true(all(c("HEIGHTBL", "WEIGHTBL", "BMIBL") %in% v$Variable[v$Dataset == "ADSL"]))
  expect_identical(v$Label[v$Variable == "BMIBL"], "Baseline BMI (kg/m2)")
})
