# A study.yml of the current form in `dir`: `x` its lines (YAML text) or a
# list, `spec_version:` (this tflspec's) put first when it says none -- a
# study.yml with no version stops (canon 11-10 :377).  Returns the file.
write_study_yml <- function(dir, x = character()) {
  f <- file.path(dir, "study.yml")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  v <- tflspec:::.spec_version
  if (is.list(x)) {
    if (is.null(x[["spec_version"]])) x <- c(list(spec_version = v), x)
    yaml::write_yaml(x, f)
  } else {
    if (!any(grepl("^spec_version:", x))) x <- c(paste0("spec_version: ", v), x)
    writeLines(x, f, useBytes = TRUE)
  }
  invisible(f)
}
