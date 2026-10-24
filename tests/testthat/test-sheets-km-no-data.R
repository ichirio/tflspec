# One KM type report with no `data:` steps (#469): the study still converts
# to sheets, that report named in a warning and kept as it is; resolving it
# still stops

test_that("a KM type report with no data: steps: the sheets with a warning; its run stops", {
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(d)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"),
                       full.names = TRUE), d, recursive = TRUE)
  s <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  o <- tfl_output("F-9", "figure", population = "SAF",
                  template = list(name = "km_survival", args = list(by = "TRT01A")))
  expect_error(tfl_resolve_output(o, s), "says its data in its own `data:` steps")
  sh <- NULL
  expect_warning(sh <- tfl_sheets_from_spec(s, c(outs, list(`F-9` = o))),
                 "F-9: kept as it is", fixed = TRUE)
  # the other reports are there, and F-9 with its template
  expect_true(all(names(outs) %in% unlist(lapply(sh, function(b) lapply(b, function(x)
    if (is.data.frame(x) && "output_id" %in% names(x)) x$output_id)))))
  expect_true("F-9" %in% unlist(lapply(sh, function(b) lapply(b, function(x)
    if (is.data.frame(x) && "output_id" %in% names(x)) x$output_id))))
  # back to YAML: its template and args as they were
  to <- tempfile()
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  suppressMessages(tfl_yaml_from_sheets(sh, to))
  f <- list.files(to, "^F-9[.]yml$", recursive = TRUE, full.names = TRUE)
  expect_length(f, 1L)
  back <- tfl_read_output(f[1L])
  expect_identical(back$template$name, "km_survival")
  expect_identical(back$template$args$by, "TRT01A")
})
