# Every template, as a template report and as the code SPEC
# tfl_template_to_spec() writes from it, built to its RTF (a figure's PNG
# is in it) by a child R that has tflspec loaded but not attached: the two
# RTFs are the same.  The templates are the catalog's (.tpl_examples: a
# template added without an example fails test-template-company.R), so a
# new template is held to this too.  (The study, the report and the child
# Rs: helper-template-run.R.)

test_that("every template: its RTF as a template report = as the code SPEC tfl_template_to_spec() writes", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("cards")
  tsr_skip_without_child()
  d <- tsr_study(withr::local_tempdir())
  names_ <- setdiff(names(tflspec:::.tpl_examples), tsr_without)
  ids <- vapply(names_, function(nm) tsr_output(nm)$id, "")
  for (nm in names_) {
    tfl_write_output(tsr_output(nm), file.path(d, "spec", "outputs", paste0(ids[[nm]], ".yml")))
  }
  # the child: each report built as it is (A), then written as its code SPEC
  # and built again (B); a build's error is recorded, never stops the loop
  w <- withr::local_tempdir()
  keep <- Sys.getenv("TSR_KEEP")
  script <- c(
    "# (a study's setup attaches rtfreporter, never tflspec)",
    "library(rtfreporter)",
    "d <- commandArgs(TRUE)[1]; part <- commandArgs(TRUE)[2]; sdir <- commandArgs(TRUE)[3]",
    "ids <- commandArgs(TRUE)[-(1:3)]",
    "res <- lapply(ids, function(id) {",
    "  f <- file.path(sdir, 'spec', 'outputs', paste0(id, '.yml'))",
    "  run <- function() tryCatch(tflspec::tfl_run_output(id, dir = sdir), error = function(e) e)",
    "  a <- run()",
    "  if (!inherits(a, 'error')) file.copy(a, file.path(d, paste0(id, '-A.rtf')), overwrite = TRUE)",
    "  b <- tryCatch({",
    "    st <- tflspec::tfl_read_study_spec(sdir)",
    "    tflspec::tfl_write_output(tflspec::tfl_template_to_spec(tflspec::tfl_read_output(f), st), f)",
    "    run()", "  }, error = function(e) e)",
    "  if (!inherits(b, 'error')) file.copy(b, file.path(d, paste0(id, '-B.rtf')), overwrite = TRUE)",
    "  msg <- function(x) if (inherits(x, 'error')) gsub('[\\r\\n]+', ' ', conditionMessage(x)) else ''",
    "  data.frame(id = id, a = msg(a), b = msg(b))",
    "})",
    "csv <- file.path(d, paste0('tsr-', part, '.csv'))",
    "write.csv(do.call(rbind, res), paste0(csv, '.tmp'), row.names = FALSE)",
    "file.rename(paste0(csv, '.tmp'), csv)")
  # (the reports are independent: a few children build them side by side,
  # each its own share, and leave the RTFs and its csv in `d`)
  got <- tsr_children(d, ids, script, w)
  expect_true(got$ok, info = paste(utils::tail(got$out, 20L), collapse = "\n"))
  r <- got$r
  # (the run's date and time, wherever a footer prints them, are not the report)
  rtf <- function(p) {
    x <- readLines(p, warn = FALSE)
    x <- gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}([T ][0-9]{2}:[0-9]{2}(:[0-9]{2})?)?", "<date>", x)
    gsub("[0-9]{2}[A-Z][a-z]{2}[0-9]{4} +[0-9]{2}:[0-9]{2}", "<date>", x)
  }
  for (i in seq_len(nrow(r))) {
    nm <- names_[match(r$id[i], ids)]
    expect_identical(r$a[i], "", info = paste(nm, "as a template:", r$a[i]))
    expect_identical(r$b[i], "", info = paste(nm, "as its code SPEC:", r$b[i]))
    if (nzchar(r$a[i]) || nzchar(r$b[i])) next
    if (nzchar(keep)) file.copy(file.path(d, paste0(r$id[i], c("-A.rtf", "-B.rtf"))), keep, overwrite = TRUE)
    expect_identical(rtf(file.path(d, paste0(r$id[i], "-B.rtf"))),
                     rtf(file.path(d, paste0(r$id[i], "-A.rtf"))), info = nm)
  }
  expect_setequal(r$id, unname(ids))
})
