# Tests of {name}(), next to its file.  Run them with
#   testthat::test_file("<this file>")
# from the study (or standards) folder.

source("{file}")

test_that("{name}() gives a cards ARD with the statistics it declares", {
  data <- cards::ADSL[cards::ADSL$ARM %in% c("Placebo", "Xanomeline High Dose"), ]
  p <- tflspec::tfl_check_ard_function({name}, data, by = ARM, variables = AGE)
  expect_identical(p$message[p$level %in% c("error", "warning")], character())
})
