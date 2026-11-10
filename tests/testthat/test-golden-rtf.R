# Golden RTFs: every template of the catalog (its example, as
# test-template-to-spec-run.R builds it), built to its RTF in three studies
# of the bundled data, and the md5 of each RTF kept in
# _snaps/golden-rtf.md, one line a (template, study).  A change to a template
# or to the layers under it shows in a PR as the changed lines: those are the
# reports whose printed output moved.  After a change meant to move them,
# review the diff and accept it: testthat::snapshot_accept("golden-rtf").
#
# The studies:
#   shipped  the study as tfl_ai_study_init() writes it (its defaults.yml)
#   plain    no defaults at all: no digits, header, stub or page settings
#   company  the company standard that tfl_use_company_templates() writes
#            (inst/standards/defaults.yml) as the study's defaults.yml, with
#            the shipped page header and footer
# A template that cannot build in a study is a line saying why, never left
# out.  What is hashed is the RTF's text, line endings as "\n", with the
# run's dates and times and any file path replaced by a mark.
#
# Also: a company's copy of a template (tfl_use_company_templates()) gives
# the same RTF as the template, in the company study.
# (The study, the report and the child Rs: helper-template-run.R.)

golden_rtf_hash <- function(p) {
  x <- rawToChar(readBin(p, "raw", file.size(p)))
  Encoding(x) <- "bytes"
  x <- gsub("\r\n?", "\n", x, useBytes = TRUE)
  # (a footer's date and time, however it prints them)
  x <- gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}([T ][0-9]{2}:[0-9]{2}(:[0-9]{2})?)?", "<date>", x, useBytes = TRUE)
  x <- gsub("[0-9]{2}[A-Z][a-z]{2}[0-9]{4} +[0-9]{2}:[0-9]{2}(:[0-9]{2})?", "<date>", x, useBytes = TRUE)
  # (a path to a file or folder of the run)
  x <- gsub("[A-Za-z]:[/\\\\][^ \"'}{;]*", "<path>", x, useBytes = TRUE)
  x <- gsub("/(tmp|var|home|Users)/[^ \"'}{;]*", "<path>", x, useBytes = TRUE)
  f <- tempfile("golden-rtf")
  on.exit(unlink(f), add = TRUE)
  writeBin(charToRaw(x), f)
  unname(tools::md5sum(f))
}

# the reports of `ids` built in the copy `d` of a study: the md5 of each
# RTF, or the error a build ended in
golden_build <- function(d, ids) {
  w <- withr::local_tempdir()
  unlink(list.files(d, "^(tsr-.*[.]csv|.*[.]rtf)$", full.names = TRUE))
  script <- c(
    "library(rtfreporter)",
    "d <- commandArgs(TRUE)[1]; part <- commandArgs(TRUE)[2]; sdir <- commandArgs(TRUE)[3]",
    "ids <- commandArgs(TRUE)[-(1:3)]",
    "reg <- Sys.getenv('TSR_REG')",
    "if (nzchar(reg)) { e <- new.env(); sys.source(reg, e); e$register_templates() }",
    "res <- lapply(ids, function(id) {",
    "  a <- tryCatch(tflspec::tfl_run_output(id, dir = sdir), error = function(e) e)",
    "  if (!inherits(a, 'error')) file.copy(a, file.path(d, paste0(id, '.rtf')), overwrite = TRUE)",
    "  data.frame(id = id, err = if (inherits(a, 'error')) gsub('[\\r\\n]+', ' ', conditionMessage(a)) else '')",
    "})",
    "csv <- file.path(d, paste0('tsr-', part, '.csv'))",
    "write.csv(do.call(rbind, res), paste0(csv, '.tmp'), row.names = FALSE)",
    "file.rename(paste0(csv, '.tmp'), csv)")
  got <- tsr_children(d, ids, script, w)
  expect_true(got$ok, info = paste(utils::tail(got$out, 20L), collapse = "\n"))
  r <- got$r
  r$md5 <- vapply(seq_len(nrow(r)), function(i) {
    p <- file.path(d, paste0(r$id[i], ".rtf"))
    if (nzchar(r$err[i]) || !file.exists(p)) "" else golden_rtf_hash(p)
  }, "")
  r$err <- gsub("[A-Za-z]:[/\\\\][^ ']*", "<path>", r$err)
  r$err <- substr(trimws(gsub("[ ]+", " ", r$err)), 1L, 100L)
  r[match(ids, r$id), c("id", "md5", "err"), drop = FALSE]
}

# a copy of the study `d` whose spec/defaults.yml is `lines`
golden_study <- function(d, lines, root) {
  dir.create(root)
  file.copy(d, root, recursive = TRUE)
  e <- file.path(root, basename(d))
  writeLines(lines, file.path(e, "spec", "defaults.yml"))
  e
}

test_that("golden RTFs: every template in three studies, a company's copy the same RTF", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("cards")
  tsr_skip_without_child()
  root <- withr::local_tempdir()
  d <- tsr_study(root)
  all_ <- names(tflspec:::.tpl_examples)
  built <- setdiff(all_, tsr_without)
  ids <- vapply(all_, function(nm) tsr_output(nm)$id, "")
  for (nm in built) {
    tfl_write_output(tsr_output(nm), file.path(d, "spec", "outputs", paste0(ids[[nm]], ".yml")))
  }
  # the company's standard, as the package that tfl_use_company_templates()
  # writes has it
  co <- file.path(root, "acmetpl")
  tfl_use_company_templates(co, templates = c("demog", "ae_soc_pt"))
  std <- readLines(file.path(co, "inst", "standards", "defaults.yml"))
  shipped <- readLines(file.path(d, "spec", "defaults.yml"))
  company <- c(std, shipped[seq(which(shipped == "page:"), length(shipped))])
  studies <- c(shipped = d,
               plain = golden_study(d, "content: []", file.path(root, "plain")),
               company = golden_study(d, company, file.path(root, "company")))
  expect_true(all(dir.exists(studies)))

  # the table: a line for every (template, study)
  rows <- lapply(names(studies), function(s) {
    r <- golden_build(studies[[s]], unname(ids[built]))
    r$template <- built[match(r$id, ids[built])]
    r$study <- s
    r
  })
  tab <- do.call(rbind, rows)
  tab$note <- ifelse(nzchar(tab$err), paste("not built:", tab$err), "")
  tab$md5 <- ifelse(nzchar(tab$md5), tab$md5, strrep("-", 32L))
  skipped <- expand.grid(template = tsr_without, study = names(studies), stringsAsFactors = FALSE)
  skipped$md5 <- strrep("-", 32L)
  skipped$note <- paste("not built: the example has no ARD analyses of its own (ard:) for the",
                        "study to run")
  tab <- rbind(tab[c("template", "study", "md5", "note")], skipped[c("template", "study", "md5", "note")])
  tab <- tab[order(match(tab$template, all_), match(tab$study, names(studies))), ]
  expect_identical(nrow(tab), length(all_) * length(studies))
  # (the shipped study builds all it can: a template that cannot is a finding)
  expect_identical(tab$note[tab$study == "shipped" & !tab$template %in% tsr_without],
                   rep("", length(built)))
  line <- trimws(sprintf("%-26s %-8s %s %s", tab$template, tab$study, tab$md5, tab$note), "right")
  expect_snapshot(cat(line, sep = "\n"))

  # a company's copy of a template: the same RTF as the template, in the company study
  withr::defer({
    reg <- get(".tpl_registry", envir = asNamespace("tflspec"))
    rm(list = intersect(c("acmetpl_demog", "acmetpl_ae_soc_pt"), ls(reg)), envir = reg)
  })
  e <- new.env()
  sys.source(file.path(co, "R", "templates.R"), envir = e)
  e$register_templates()
  copies <- c("demog", "ae_soc_pt")
  for (nm in copies) {
    tfl_write_output(tsr_output(nm, use = paste0("acmetpl_", nm)),
                     file.path(studies[["company"]], "spec", "outputs", paste0(ids[[nm]], ".yml")))
  }
  withr::local_envvar(TSR_REG = file.path(co, "R", "templates.R"))
  cp <- golden_build(studies[["company"]], unname(ids[copies]))
  expect_identical(cp$err, c("", ""))
  want <- vapply(copies, function(nm) tab$md5[tab$template == nm & tab$study == "company"], "")
  expect_identical(cp$md5, unname(want))
})
