# Every template, as a template report and as the code SPEC
# tfl_template_to_spec() writes from it, built to its RTF (a figure's PNG
# is in it) by a child R that has tflspec loaded but not attached: the two
# RTFs are the same.  The templates are the catalog's (.tpl_examples: a
# template added without an example fails test-template-company.R), so a
# new template is held to this too.

tsr_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adlb", "advs", "adtte", "adrs", "adtr", "adex")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "TSR-1")
  d
}

# The catalog's examples this study cannot build as they are -- the same in
# both forms, so no conversion is at stake: km_table and stat_block take
# the report's own ARD analyses (`ard:`), which an example has not;
# param_change's example keeps ANL01FL == "Y", which the bundled ADLB has
# not (its records are taken as they are here).
tsr_args <- list(param_change = list(where = NULL))
tsr_without <- c("km_table", "stat_block")

# a template report of the catalog's example (a template whose data stage
# is the report's own: its data: steps, as test-template-spec.R gives them)
tsr_output <- function(nm) {
  tp <- tfl_templates()
  own <- if (any(vapply(tflspec:::.tpl_get(nm, "test")$params, function(p) isTRUE(p$hidden) &&
                          identical(p$stage, "data"), NA))) {
    list(steps = list(list(step = "read", dataset = "ADTTE"), list(step = "param", value = "PFS")))
  }
  tfl_output(paste0("X-", gsub("_", "-", nm)), tp$kind[tp$name == nm], population = "SAF",
             label = nm, template = list(name = nm, args = utils::modifyList(
               tflspec:::.tpl_examples[[nm]], tsr_args[[nm]] %||% list(), keep.null = FALSE)),
             data = own)
}

test_that("every template: its RTF as a template report = as the code SPEC tfl_template_to_spec() writes", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("cards")
  rscript <- file.path(R.home("bin"), "Rscript")
  withr::local_envvar(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  has <- suppressWarnings(system2(rscript, c("-e", shQuote("cat(requireNamespace('tflspec', quietly = TRUE))")),
                                  stdout = TRUE, stderr = FALSE))
  skip_if_not(identical(utils::tail(has, 1L), "TRUE"), "tflspec is not installed where a child R looks")
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
  writeLines(c(
    "# (a study's setup attaches rtfreporter, never tflspec)",
    "library(rtfreporter)",
    "d <- commandArgs(TRUE)[1]; ids <- commandArgs(TRUE)[-1]",
    "res <- lapply(ids, function(id) {",
    "  f <- file.path(d, 'spec', 'outputs', paste0(id, '.yml'))",
    "  run <- function() tryCatch(tflspec::tfl_run_output(id, dir = d), error = function(e) e)",
    "  a <- run()",
    "  if (!inherits(a, 'error')) file.copy(a, file.path(d, paste0(id, '-A.rtf')), overwrite = TRUE)",
    "  b <- tryCatch({",
    "    st <- tflspec::tfl_read_study_spec(d)",
    "    tflspec::tfl_write_output(tflspec::tfl_template_to_spec(tflspec::tfl_read_output(f), st), f)",
    "    run()", "  }, error = function(e) e)",
    "  if (!inherits(b, 'error')) file.copy(b, file.path(d, paste0(id, '-B.rtf')), overwrite = TRUE)",
    "  msg <- function(x) if (inherits(x, 'error')) gsub('[\\r\\n]+', ' ', conditionMessage(x)) else ''",
    "  data.frame(id = id, a = msg(a), b = msg(b))",
    "})",
    "write.csv(do.call(rbind, res), file.path(d, 'tsr.csv'), row.names = FALSE)"),
    file.path(w, "run.R"))
  out <- suppressWarnings(system2(rscript, c(file.path(w, "run.R"), d, unname(ids)),
                                  stdout = TRUE, stderr = TRUE))
  csv <- file.path(d, "tsr.csv")
  expect_true(file.exists(csv), info = paste(utils::tail(out, 20L), collapse = "\n"))
  r <- utils::read.csv(csv, colClasses = "character", na.strings = character())
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
