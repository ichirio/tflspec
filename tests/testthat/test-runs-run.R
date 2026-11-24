# Resolving a run definition, its command, the programs' check, the list and
# the compare of the runs (G4 slice B1).  The real Rscript runs are in
# test-runner-run.R.

# a study as test-runs-file.R makes it (T-2, T-1, F-1 reading T-1, L-1), and a
# figure that reads an ARD with no source
runs_study_f <- function() {
  d <- runs_study()
  tfl_write_output(tfl_output("F-2", "figure", data = list(steps = list(list(step = "ard_stats")))),
                   file.path(d, "spec", "outputs", "F-2.yml"))
  d
}

test_that("tfl_resolve_run(): all is expanded, ids in report-list order, the options filled", {
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  write_runs_text(d, c(
    "runs:",
    "  - name: Everything", "    reports: all",
    "  - name: Some", "    reports: [T-1, T-2]"))
  r <- tfl_resolve_run(d, "Everything")
  expect_identical(r$reports, c("T-2", "T-1", "F-1", "L-1"))
  expect_identical(r$mode, "all")
  expect_identical(r$copy_data, FALSE)
  expect_identical(nrow(r$problems), 0L)
  # the ARD each report reads: the tables' own, F-1's from T-1; the listing none
  expect_identical(r$ard, c("T-2", "T-1"))
  expect_setequal(r$reads$report, c("T-1", "T-2", "F-1"))
  s <- tfl_resolve_run(d, "Some")
  expect_identical(s$reports, c("T-2", "T-1"))
  expect_identical(nrow(s$problems), 0L)
  expect_error(tfl_resolve_run(d, "Nope"), "no run definition named 'Nope'")
  expect_error(tfl_resolve_run(d, "some"), "it has: Everything, Some")
})

test_that("tfl_resolve_run(): a read outside the definition is a problem naming both ids", {
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  write_runs_text(d, c("runs:", "  - name: Fig", "    reports: [F-1]",
                       "  - name: Both", "    reports: [F-1, T-1]"))
  r <- tfl_resolve_run(d, "Fig")
  expect_identical(nrow(r$problems), 1L)
  expect_match(r$problems$problem, "F-1 reads the ARD of T-1, which the definition does not select",
               fixed = TRUE)
  expect_identical(nrow(tfl_resolve_run(d, "Both")$problems), 0L)
})

test_that("tfl_resolve_run() fails closed on every row whose read is NA, whatever the reason", {
  d <- runs_study_f()
  on.exit(unlink(d, recursive = TRUE))
  # (a source that is not understood, a report whose file cannot be read)
  tfl_write_output(tfl_output("F-3", "figure", page = list(report = list(ard_source = "wat:1"))),
                   file.path(d, "spec", "outputs", "F-3.yml"))
  writeLines(c("id: [", "kind: table"), file.path(d, "spec", "outputs", "T-BAD.yml"))
  write_runs_text(d, c("runs:", "  - name: Risky", "    reports: [T-1, F-2, F-3, T-BAD]"))
  r <- tfl_resolve_run(d, "Risky")
  p <- r$problems$problem
  expect_true(any(grepl("^F-2: no source", p)))
  expect_true(any(grepl("^F-3: source not understood: wat:1", p)))
  expect_true(any(grepl("^T-BAD: file not read: ", p)))
  # the ones that read fine are no problem
  expect_false(any(grepl("^T-1", p)))
  expect_true(all(r$problems$definition == "Risky"))
})

test_that("tfl_resolve_run() carries the definition's own check rows", {
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  write_runs_text(d, c("runs:", "  - name: Bad name", "    reports: [T-1, T-99]",
                       "  - name: Fine", "    reports: [T-1]"))
  r <- tfl_resolve_run(d, "Bad name")
  expect_true(any(r$problems$field == "name"))
  expect_true(any(grepl("no report T-99", r$problems$problem)))
  expect_identical(nrow(tfl_resolve_run(d, "Fine")$problems), 0L)
})

test_that("tfl_resolve_run() on an old study (sets not written yet) gives the migrated definitions", {
  d <- runs_study(toc_sets = list(`T-1` = "Topline", `T-2` = "Topline"))
  on.exit(unlink(d, recursive = TRUE))
  r <- tfl_resolve_run(d, "Topline")
  expect_identical(r$reports, c("T-2", "T-1"))
  expect_false(file.exists(tfl_runs_file(d)))
})

test_that("tfl_run_command(): the command from the study folder, with --run=<name>", {
  d <- tempfile()
  cmd <- tfl_run_command(d, "Topline")
  expect_identical(cmd[-1L], c("programs/autoexec_all.R", "--run=Topline"))
  expect_identical(basename(cmd[[1L]]), basename(file.path(R.home("bin"), "Rscript")))
  expect_identical(attr(cmd, "wd"), normalizePath(d, winslash = "/", mustWork = FALSE))
  expect_error(tfl_run_command(d, ""), "name")
  expect_error(tfl_run_command(d, c("a", "b")), "name")
  # no copy_data or data_root argument: a different choice is another definition
  expect_identical(names(formals(tfl_run_command)), c("study_dir", "name"))
  expect_identical(names(formals(tfl_run_study)), c("study_dir", "name"))
})

test_that("tfl_programs_problem(): programs made before the run definitions say to regenerate", {
  d <- tempfile("prog")
  dir.create(file.path(d, "programs"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  expect_match(tfl_programs_problem(d), "autoexec_all.R is missing")
  writeLines("run_batch(c('ard', 'tfl'))", file.path(d, "programs", "autoexec_all.R"))
  expect_match(tfl_programs_problem(d), "batch.R is missing")
  # an old batch.R: `sets`, no stamp
  writeLines(c(".batch_root <- \"runs\"", ".batch_sets <- list()"), file.path(d, "programs", "batch.R"))
  expect_match(tfl_programs_problem(d), "regenerate the programs")
  expect_error(tfl_run_study(d, "Main"), "regenerate the programs")
  # one with a stamp and runs
  m <- rr_manifest()
  writeLines(tfl_runner_code(m), file.path(d, "programs", "batch.R"))
  expect_identical(tfl_programs_problem(d), "")
  # a stamp and no runs is old too
  code <- tfl_runner_code(m)
  writeLines(code[!startsWith(code, ".batch_runs <- ")], file.path(d, "programs", "batch.R"))
  expect_match(tfl_programs_problem(d), "regenerate the programs")
})

# a run folder as the runner leaves it
make_run <- function(root, name, lines, csv = NULL, files = character()) {
  p <- file.path(root, name)
  dir.create(p, recursive = TRUE)
  if (!is.null(lines)) writeLines(lines, file.path(p, "batch.txt"))
  if (!is.null(csv)) utils::write.csv(csv, file.path(p, "run.csv"), row.names = FALSE)
  for (f in names(files)) {
    dir.create(dirname(file.path(p, f)), recursive = TRUE, showWarnings = FALSE)
    writeLines(files[[f]], file.path(p, f))
  }
  p
}

test_that("tfl_list_runs(): new folders, old folders, unfinished ones, together, by start", {
  d <- tempfile("list")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  root <- file.path(d, "runs")
  ok <- data.frame(part = "tfl", program = "p.R", output_id = "T-1", status = "OK", errors = 0L,
                   warnings = 2L, stringsAsFactors = FALSE)
  bad <- ok
  bad$status <- "ERROR"
  # a new, finished run of the definition Topline_2 (not the second run of Topline)
  make_run(root, "batch_20261124-101010_Topline_2", c(
    "Run      : Topline_2", "Mode     : all", "Kind     : run", "Started  : 2026-11-24 10:10:10",
    "Logs     : R CMD BATCH", "Study    : S-1", "Finished : 2026-11-24 10:11:00",
    "Excluded : none", "Verified : yes"), ok)
  # a new run that is still going: only the start record
  make_run(root, "batch_20261124-101010_Topline", c(
    "Run      : Topline", "Mode     : ard", "Kind     : run", "Started  : 2026-11-24 10:10:10"))
  # a failed one, a preview, a re-run
  make_run(root, "batch_20261124-120000_Fail", c(
    "Run      : Fail", "Mode     : all", "Kind     : run", "Started  : 2026-11-24 12:00:00",
    "Finished : 2026-11-24 12:01:00", "Verified : yes"), bad)
  make_run(root, "preview_20261124-130000", c(
    "Run      : none", "Mode     : all", "Kind     : preview", "Started  : 2026-11-24 13:00:00",
    "Finished : 2026-11-24 13:01:00"), ok)
  make_run(root, "batch_20261124-140000_Again", c(
    "Run      : Again", "Mode     : all", "Kind     : run", "Started  : 2026-11-24 14:00:00",
    "Finished : 2026-11-24 14:01:00", "Re-run   : 2026-11-24 14:01:00"), ok)
  # older runner folders: the definition in `Batch set :`, the mode in the name's word
  make_run(root, "20261101_090000_report_Topline", c(
    "Batch    : 20261101_090000_report_Topline", "Kind     : run", "Study    : S-1",
    "Batch set : Topline", "Started  : 2026-11-01 09:00:00", "Finished : 2026-11-01 09:05:00",
    "Excluded : programs/tfl/x.R", "Verified : yes"), ok,
    files = c(`data/a.csv` = "x"))
  # a folder that crashed before batch.txt: named by its stamp only
  make_run(root, "20261102_080000_all", NULL)
  l <- tfl_list_runs(d)
  expect_named(l, c("run", "definition", "mode", "kind", "what", "started", "finished", "verified",
                    "rerun", "programs", "errors", "warnings", "state", "data", "excluded", "path"))
  # newest start first; a tie by name, descending; old and new names together
  expect_identical(l$run, c("batch_20261124-140000_Again", "preview_20261124-130000",
                            "batch_20261124-120000_Fail", "batch_20261124-101010_Topline_2",
                            "batch_20261124-101010_Topline", "20261102_080000_all",
                            "20261101_090000_report_Topline"))
  row <- function(r) l[l$run == r, , drop = FALSE]
  # Topline_2 is a definition, not the second run of Topline
  expect_identical(row("batch_20261124-101010_Topline_2")$definition, "Topline_2")
  expect_identical(row("batch_20261124-101010_Topline")$definition, "Topline")
  expect_identical(row("batch_20261124-101010_Topline")$state, "running or stopped")
  expect_identical(row("batch_20261124-101010_Topline")$mode, "ard")
  expect_identical(row("batch_20261124-101010_Topline_2")$state, "done")
  expect_identical(row("batch_20261124-101010_Topline_2")$warnings, 2L)
  expect_identical(row("batch_20261124-120000_Fail")$state, "failed")
  expect_identical(row("batch_20261124-120000_Fail")$errors, 1L)
  expect_identical(row("preview_20261124-130000")$kind, "preview")
  expect_false(row("preview_20261124-130000")$verified)
  expect_true(row("batch_20261124-140000_Again")$rerun)
  # the older folders
  o <- row("20261101_090000_report_Topline")
  expect_identical(o$definition, "Topline")
  expect_identical(o$mode, "report")
  expect_identical(o$what, "report")
  expect_identical(o$started, "2026-11-01 09:00:00")
  expect_identical(o$excluded, "programs/tfl/x.R")
  expect_true(o$data)
  expect_true(o$verified)
  c0 <- row("20261102_080000_all")
  expect_identical(c0$state, "running or stopped")
  expect_identical(c0$mode, "all")
  expect_identical(c0$definition, "")
  expect_identical(c0$started, "2026-11-02 08:00:00")
  # no runs folder: no row, the same columns
  e <- tfl_list_runs(file.path(d, "none"))
  expect_identical(nrow(e), 0L)
  expect_named(e, names(l))
})

test_that("tfl_list_runs() and tfl_run_index() read one folder the same way", {
  d <- tempfile("same")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  writeLines("study_id: S-1", file.path(d, "study.yml"))
  ok <- data.frame(part = "tfl", program = "p.R", output_id = "T-1", status = "OK", md5 = "abc",
                   errors = 0L, warnings = 0L, stringsAsFactors = FALSE)
  make_run(file.path(d, "runs"), "batch_20261124-101010_Topline", c(
    "Run      : Topline", "Mode     : all", "Kind     : run", "Started  : 2026-11-24 10:10:10",
    "Logs     : R CMD BATCH", "Batch    : batch_20261124-101010_Topline", "Study    : S-1",
    "Finished : 2026-11-24 10:11:00", "Verified : yes"), ok)
  # (a run that is going or stopped is in the list, not in the index)
  make_run(file.path(d, "runs"), "batch_20261124-111111_Topline", c(
    "Run      : Topline", "Mode     : all", "Kind     : run", "Started  : 2026-11-24 11:11:11",
    "Study    : S-1"))
  idx <- tfl_run_index(d)
  expect_identical(unique(idx$run), "batch_20261124-101010_Topline")
  l <- tfl_list_runs(d)
  expect_identical(nrow(l), 2L)
  expect_identical(l$path[l$run == "batch_20261124-101010_Topline"], idx$path[[1L]])
  expect_identical(tfl_latest_run(d, "T-1")$run, "batch_20261124-101010_Topline")
})

test_that("tfl_compare_runs(): inputs by md5, results by bytes; the programs, not code/", {
  d <- tempfile("cmp")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  a <- make_run(d, "a", "x", files = list(`programs/p.R` = "1", `output/tfl/T.rtf` = "same",
                                       `output/tfl/U.rtf` = "old", `output/tfl/G.rtf` = "gone",
                                       `data_manifest.csv` = c("path,md5", "d.csv,aaa")))
  b <- make_run(d, "b", "x", files = list(`programs/p.R` = "2", `programs/q.R` = "new",
                                       `output/tfl/T.rtf` = "same", `output/tfl/U.rtf` = "new",
                                       `output/tfl/N.rtf` = "n",
                                       `data_manifest.csv` = c("path,md5", "d.csv,aaa")))
  r <- tfl_compare_runs(a, b)
  res <- function(k, f) r$result[r$kind == k & r$file == f]
  expect_identical(res("code", "p.R"), "changed")
  expect_identical(res("code", "q.R"), "added")
  expect_identical(res("output", "output/tfl/T.rtf"), "identical")
  expect_identical(res("output", "output/tfl/U.rtf"), "different")
  expect_identical(res("output", "output/tfl/G.rtf"), "not made")
  expect_identical(res("output", "output/tfl/N.rtf"), "new")
  # the same data: not listed
  expect_false("data" %in% r$kind)
  expect_identical(attr(r, "batches"), c(against = "a", batch = "b"))
  expect_setequal(attr(r, "changed")$outputs,
                  c("output/tfl/U.rtf", "output/tfl/G.rtf", "output/tfl/N.rtf"))
  expect_setequal(attr(r, "changed")$inputs, c("p.R", "q.R"))
  # no data manifest on one side: not kept
  unlink(file.path(a, "data_manifest.csv"))
  r2 <- tfl_compare_runs(a, b)
  expect_identical(r2$result[r2$kind == "data"], "not kept")
  # the same time-stamped RTF
  writeLines("made 2026-11-24 10:00:00", file.path(a, "output", "tfl", "T.rtf"))
  writeLines("made 2026-11-24 11:00:00", file.path(b, "output", "tfl", "T.rtf"))
  expect_identical(tfl_compare_runs(a, b)$result[tfl_compare_runs(a, b)$file == "output/tfl/T.rtf"],
                   "same but the time")
  expect_error(tfl_compare_runs(a, file.path(d, "none")), "no run folder")
})
