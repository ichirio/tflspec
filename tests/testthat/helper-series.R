# The test-only series tflspec_test (fixtures/series/tflspec_test): the
# purpose templates that left tflspec_v1 (demog, ae_soc_pt, listing,
# orr_subgroup, lab_tox_shift; canon 2026-11-24), kept for the engine
# coverage only they have.  local_test_series() builds it in a temporary
# folder -- the package's tflspec_v1/series.yml with its name: rewritten
# (no copy of it to drift) and the fixture templates -- and points the
# option tflspec.series_dir at it for the calling test.  A new name, never
# tflspec_v1: a folder of that name under the option would shadow the
# shipped series.  Not a setup file: a test that lists the series would
# see it.
local_test_series <- function(env = parent.frame()) {
  root <- withr::local_tempdir("series", .local_envir = env)
  d <- file.path(root, "tflspec_test")
  dir.create(file.path(d, "templates"), recursive = TRUE)
  sy <- readLines(system.file("series", "tflspec_v1", "series.yml", package = "tflspec"),
                  encoding = "UTF-8", warn = FALSE)
  sy <- sub("^name: tflspec_v1$", "name: tflspec_test", sy)
  writeLines(sy, file.path(d, "series.yml"), useBytes = TRUE)
  src <- test_path("fixtures", "series", "tflspec_test", "templates")
  ok <- file.copy(list.files(src, "[.]yml$", full.names = TRUE), file.path(d, "templates"))
  stopifnot(length(ok) == 5L, all(ok))
  withr::local_options(tflspec.series_dir = normalizePath(root, winslash = "/"), .local_envir = env)
  invisible(root)
}
