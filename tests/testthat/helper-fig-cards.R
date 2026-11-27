# A figure report's cards through the study's layers (C1): a study of the
# package's sample data (tfl_example_adam(): ADSL and ADTTE, its analysis
# set FAS among the study's sets) whose pattern `cards` has a figure card
# of its own, and a report on it resolved as its program is
# (tfl_resolve_output()): the pattern's content under the report's cards,
# a merge that is not the empty one.  Fictional data only.

fcards_study <- function(env = parent.frame()) {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- file.path(withr::local_tempdir(.local_envir = env), "study")
  adam <- tfl_example_adam()
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("ADSL", "ADTTE")) {
    haven::write_xpt(tflspec:::.ai_chr(adam[[x]]), file.path(d, "adam", paste0(tolower(x), ".xpt")))
  }
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "FC-1"))
  dir.create(file.path(d, "spec", "patterns"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("content:", "  - labs: {caption: \"Source: ADTTE\"}"),
             file.path(d, "spec", "patterns", "cards.yml"))
  d
}

# The report (its YAML lines), on the pattern `cards` and the defaults,
# resolved in the study `d`
fcards_resolve <- function(d, lines) {
  f <- file.path(d, "spec", "outputs", paste0(sub("^id: ", "", lines[1L]), ".yml"))
  writeLines(c(lines, "extends: [patterns/cards, defaults]"), f)
  on.exit(unlink(f))
  invisible(suppressMessages(tfl_copy_templates(d)))
  tfl_resolve_output(tfl_read_output(f), tfl_read_study_spec(d))
}
