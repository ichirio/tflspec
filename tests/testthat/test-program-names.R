# A generated program's own variables are never a data's or an ARD's
# column: ADaM columns upper case, ARD and spec columns lower case with
# underscores, the program's names report_id, cl_<variable>, path_<folder>,
# ard_<analysis id>, pop_<set>, fmt_default ...

# the names R code assigns at its top level
assigned_names <- function(code) {
  ex <- parse(text = code, keep.source = FALSE)
  out <- character()
  for (e in ex) {
    if (is.call(e) && as.character(e[[1L]]) %in% c("<-", "=") && is.name(e[[2L]])) {
      out <- c(out, as.character(e[[2L]]))
    }
  }
  unique(out)
}

test_that("a program's own names are no data's and no ARD's columns", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  sp <- exact_spec(list(method = "categorical", dataset = "ADSL",
                        population_id = "SAF", by = "ARM",
                        variables = "AGEGR1 | SEX"))
  cl <- data.frame(output_id = "T", variable = c("SEX", "SEX", "ARM"),
                   value = c("F", "M", "Placebo"),
                   label = c("Female", "Male", NA), order = c("1", "2", "1"))
  cl <- rbind(cl, data.frame(output_id = "T", variable = "ARM",
                             value = c("Xanomeline High Dose", "Xanomeline Low Dose"),
                             label = NA, order = c("2", "3")))
  withr::local_options(tflspec.paths = c(path_adam = "adam"))
  body <- tfl_ard_code(sp, part = "body", codelists = cl)
  mine <- c(assigned_names(body), "report_id", "path_adam",
            assigned_names(tfl_ard_code(sp, part = "setup")),
            assigned_names(tfl_helpers_code()))
  expect_true(all(c("report_id", "cl_sex", "cl_arm") %in% mine))
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE, codelists = cl))
  columns <- unique(c(unlist(lapply(adam, names)), names(ard),
                      "output_id", "analysis_id", "population_id"))
  expect_identical(intersect(mine, columns), character(0))
  # the ARD has no method column (ARS's methodId comes from the spec)
  expect_false(any(grepl("method", names(ard))))
})
