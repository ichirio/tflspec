# The run definitions file, spec/runs.yml (G4 slice B1): read, migrate, check,
# text, write.  The failing inputs of the two reviews of SLICE-B are cases here.

test_that("tfl_runs_file() is the layout's spec folder and runs.yml", {
  expect_identical(tfl_runs_file("S"), file.path("S", "spec", "runs.yml"))
})

test_that("names and ids stay text: No, Y, 1.0, 001, 14.10, yes", {
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  write_runs_text(d, c(
    "runs:",
    "  - name: No", "    reports: [14.10, 001, yes, NO]",
    "  - name: Y", "    reports: [T-1]",
    "  - name: 1.0", "    reports: T-1",
    "  - name: \"001\"", "    reports: [T-1]"))
  r <- unclass(tfl_read_runs(d))
  attributes(r) <- NULL
  expect_identical(vapply(r, function(x) x$name, ""), c("No", "Y", "1.0", "001"))
  expect_identical(r[[1L]]$reports, c("14.10", "001", "yes", "NO"))
  # a scalar id is one id
  expect_identical(r[[3L]]$reports, "T-1")
})

test_that("the word all is a lower-case scalar only; [all] and All are check rows", {
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  write_runs_text(d, c(
    "runs:",
    "  - name: A", "    reports: all",
    "  - name: B", "    reports: [all]",
    "  - name: C", "    reports: All",
    "  - name: D", "    reports: [all, T-1]",
    "  - name: E", "    reports: [T-1, T-1]"))
  r <- tfl_read_runs(d)
  expect_identical(unclass(r)[[1L]]$reports, "all")
  p <- tfl_check_runs(r, d)
  expect_false("A" %in% p$definition)
  expect_true(any(p$definition == "B" & grepl("write `reports: all`", p$problem, fixed = TRUE)))
  expect_true(any(p$definition == "C" & grepl("lower case", p$problem)))
  expect_true(any(p$definition == "D" & grepl("no other id", p$problem)))
  expect_true(any(p$definition == "E" & grepl("T-1 is listed twice", p$problem)))
})

test_that("an empty list, an unknown id and a duplicated id", {
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  write_runs_text(d, c(
    "runs:",
    "  - name: Empty", "    reports: []",
    "  - name: NoKey",
    "  - name: Unknown", "    reports: [T-1, T-99]"))
  p <- tfl_check_runs(tfl_read_runs(d), d)
  expect_true(any(p$definition == "Empty" & p$field == "reports" & grepl("empty", p$problem)))
  expect_true(any(p$definition == "NoKey" & p$field == "reports" & grepl("empty", p$problem)))
  expect_true(any(p$definition == "Unknown" & grepl("no report T-99", p$problem)))
  expect_false(any(p$definition == "Unknown" & grepl("T-1", p$problem)))
})

test_that("the name rule: letters, digits, _ and -, a letter first, at most 40", {
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  bad <- c("1A", "a b", "a.b", "ä", "-a", "_a", "", strrep("a", 41L))
  good <- c("a", "A1", "a-b_c", strrep("a", 40L), "all", "All", "selected", "CON")
  one <- function(nm) {
    list(list(name = nm, reports = "T-1"))
  }
  for (nm in bad) {
    p <- tfl_check_runs(one(nm), d)
    expect_true(any(p$field == "name"), info = nm)
  }
  for (nm in good) {
    p <- tfl_check_runs(one(nm), d)
    expect_false(any(p$field == "name"), info = nm)
  }
  # a trailing newline is not a letter or a digit
  expect_true(any(tfl_check_runs(one("a\n"), d)$field == "name"))
  expect_true(any(tfl_check_runs(list(list(reports = "T-1")), d)$field == "name"))
})

test_that("two names the same ignoring case are a row for each", {
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  runs <- list(list(name = "Topline", reports = "T-1"), list(name = "topline", reports = "T-2"),
               list(name = "Other", reports = "T-1"))
  p <- tfl_check_runs(runs, d)
  expect_setequal(p$definition[grepl("used twice", p$problem)], c("Topline", "topline"))
  expect_false("Other" %in% p$definition)
})

test_that("options: missing is fine; an unknown key and a wrong value are rows", {
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  write_runs_text(d, c(
    "runs:",
    "  - name: Plain", "    reports: [T-1]",
    "  - name: Full", "    reports: [T-1]", "    mode: ard", "    copy_data: true",
    "  - name: Odd", "    reports: [T-1]", "    mode: ARD", "    copy_data: yes",
    "    tags: [a, b]", "    zip: {level: 5}"))
  r <- tfl_read_runs(d)
  p <- tfl_check_runs(r, d)
  expect_false(any(p$definition %in% c("Plain", "Full")))
  expect_true(any(p$definition == "Odd" & p$field == "mode"))
  expect_true(any(p$definition == "Odd" & p$field == "copy_data"))
  expect_true(any(p$definition == "Odd" & p$field == "tags" & grepl("update tflspec", p$problem)))
  expect_true(any(p$definition == "Odd" & p$field == "zip"))
  # typed options
  full <- unclass(r)[[2L]]
  expect_identical(full$mode, "ard")
  expect_identical(full$copy_data, TRUE)
})

test_that("a file that does not parse is a problem, not an error", {
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  write_runs_text(d, c("runs:", "  - name: [", "    reports"))
  r <- expect_no_error(tfl_read_runs(d))
  expect_length(unclass(r), 0L)
  p <- tfl_check_runs(r, d)
  expect_true(any(p$field == "file" & grepl("cannot be read as YAML", p$problem)))
  # not a map, no runs: key, an entry that is not a map
  write_runs_text(d, c("- a", "- b"))
  expect_true(any(tfl_check_runs(tfl_read_runs(d), d)$field == "file"))
  write_runs_text(d, c("other: 1"))
  expect_true(any(grepl("no `runs:`", tfl_check_runs(tfl_read_runs(d), d)$problem)))
  write_runs_text(d, c("runs:", "  - just text"))
  expect_true(any(grepl("is a map", tfl_check_runs(tfl_read_runs(d), d)$problem)))
  # an empty file and `runs: []`: no definition, no problem
  write_runs_text(d, character())
  expect_length(unclass(tfl_read_runs(d)), 0L)
  expect_identical(nrow(tfl_check_runs(tfl_read_runs(d), d)), 0L)
  write_runs_text(d, "runs: []")
  expect_identical(nrow(tfl_check_runs(tfl_read_runs(d), d)), 0L)
})

test_that("the text is stable: quoted, in report-list order, unknown keys kept", {
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  write_runs_text(d, c(
    "runs:",
    "  - name: Topline", "    reports: [F-1, L-1, T-1]", "    mode: ard",
    "    copy_data: false", "    note: {a: 1, b: [x, y]}",
    "  - name: All", "    reports: all"))
  r <- tfl_read_runs(d)
  expect_identical(attr(r, "order"), c("T-2", "T-1", "F-1", "L-1"))
  txt <- tfl_runs_text(r)
  expect_identical(txt[[1L]], "# spec/runs.yml: the study's run definitions")
  expect_true("  - name: \"Topline\"" %in% txt)
  # the ids in the report list's order, quoted
  expect_true("    reports: [\"T-1\", \"F-1\", \"L-1\"]" %in% txt)
  expect_true("    reports: all" %in% txt)
  expect_true("    mode: ard" %in% txt)
  expect_true("    copy_data: false" %in% txt)
  # the same definitions, the same bytes; and the text reads back the same
  expect_identical(tfl_runs_text(r), txt)
  write_runs_text(d, txt)
  r2 <- tfl_read_runs(d)
  expect_identical(tfl_runs_text(r2), txt)
  expect_identical(unclass(r2)[[1L]]$note, list(a = "1", b = list("x", "y")))
  expect_true(any(grepl("^    note:", txt)))
  # no definition
  expect_identical(tfl_runs_text(list()), c("# spec/runs.yml: the study's run definitions", "runs: []"))
  # a name with a quote or a backslash is written whole
  q <- tfl_runs_text(list(list(name = "a\"b\\c", reports = "T-1")))
  expect_true("  - name: \"a\\\"b\\\\c\"" %in% q)
})

test_that("tfl_write_runs() writes bytes, refuses a bad new name, and removes the file with no definition", {
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  runs <- list(list(name = "Topline", reports = c("T-1", "F-1")))
  f <- tfl_write_runs(runs, d)
  expect_identical(f, tfl_runs_file(d))
  raw <- readBin(f, "raw", file.size(f))
  expect_false(as.raw(13L) %in% raw)
  expect_identical(readLines(f), tfl_runs_text(runs))
  # a new name that breaks the rule, or repeats one: refused, nothing written
  before <- readBin(f, "raw", file.size(f))
  expect_error(tfl_write_runs(c(runs, list(list(name = "a b", reports = "T-1"))), d), "breaks the rule")
  expect_error(tfl_write_runs(c(runs, list(list(name = "topline", reports = "T-1"))), d), "used twice")
  expect_identical(readBin(f, "raw", file.size(f)), before)
  # an old name that already breaks the rule can still be saved
  write_runs_text(d, c("runs:", "  - name: Interim Analysis", "    reports: [T-1]"))
  old <- tfl_read_runs(d)
  expect_no_error(tfl_write_runs(old, d))
  # the last definition removed: the file is removed, not `runs: []`
  tfl_write_runs(list(), d)
  expect_false(file.exists(f))
})

test_that("no runs.yml: the file is read from toc.yml sets, in report-list order, and nothing is written", {
  d <- runs_study(toc_sets = list(`T-1` = c("Topline", "Final"), `T-2` = "Final",
                                   `F-1` = "Topline", `L-1` = "topline"))
  on.exit(unlink(d, recursive = TRUE))
  r <- tfl_read_runs(d)
  expect_identical(attr(r, "migrated"), 3L)
  defs <- unclass(r)
  attributes(defs) <- NULL
  # the names in the order they first appear in the report list (T-2, T-1, F-1, L-1)
  expect_identical(vapply(defs, function(x) x$name, ""), c("Final", "Topline", "topline"))
  expect_identical(defs[[1L]]$reports, c("T-2", "T-1"))
  expect_identical(defs[[2L]]$reports, c("T-1", "F-1"))
  expect_false(file.exists(tfl_runs_file(d)))
  # two names that differ only in case both migrate, and are a problem until renamed
  p <- tfl_check_runs(r, d)
  expect_setequal(p$definition[grepl("used twice", p$problem)], c("Topline", "topline"))
  # a name kept as it was, an old name that breaks the rule is a row
  d2 <- runs_study(toc_sets = list(`T-1` = "Interim Analysis"))
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  p2 <- tfl_check_runs(tfl_read_runs(d2), d2)
  expect_true(any(p2$definition == "Interim Analysis" & p2$field == "name"))
  # no sets, no definition
  d3 <- runs_study()
  on.exit(unlink(d3, recursive = TRUE), add = TRUE)
  expect_identical(attr(tfl_read_runs(d3), "migrated"), 0L)
})

test_that("an old state: tfl_runs_migrate() takes the data frame", {
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  o <- data.frame(output_id = c("T-1", "T-2", "F-1"),
                  batches = c("Topline | Final", "Final", NA), stringsAsFactors = FALSE)
  r <- tfl_runs_migrate(d, outputs = o)
  defs <- unclass(r)
  attributes(defs) <- NULL
  expect_identical(vapply(defs, function(x) x$name, ""), c("Topline", "Final"))
  expect_identical(defs[[2L]]$reports, c("T-1", "T-2"))
  expect_identical(attr(r, "migrated"), 2L)
  expect_error(tfl_runs_migrate(d, outputs = data.frame(x = 1)), "batches")
})

test_that("the workbook planner sheet's batches column migrates too", {
  skip_if_not_installed("openxlsx")
  d <- runs_study()
  on.exit(unlink(d, recursive = TRUE))
  openxlsx::write.xlsx(
    list(`_tflplanner` = data.frame(output_id = c("T-1", "T-2"), batches = c("Topline", "Topline | Final"),
                                    stringsAsFactors = FALSE)),
    file.path(d, "spec", "report_spec.xlsx"))
  r <- tfl_runs_migrate(d)
  defs <- unclass(r)
  attributes(defs) <- NULL
  expect_identical(vapply(defs, function(x) x$name, ""), c("Topline", "Final"))
  expect_identical(defs[[1L]]$reports, c("T-1", "T-2"))
})

test_that("runs.yml and toc.yml sets both present: runs.yml wins, a check row says it", {
  d <- runs_study(toc_sets = list(`T-1` = "Old"))
  on.exit(unlink(d, recursive = TRUE))
  write_runs_text(d, c("runs:", "  - name: New", "    reports: [T-1]"))
  r <- tfl_read_runs(d)
  expect_null(attr(r, "migrated"))
  expect_identical(unclass(r)[[1L]]$name, "New")
  p <- tfl_check_runs(r, d)
  expect_true(any(p$field == "sets" & p$problem == "toc.yml sets: is ignored (runs.yml exists)"))
  expect_identical(p$level[p$field == "sets"], "warning")
})

test_that("tfl_check_spec() includes the run rows and forbids the output id all", {
  d <- runs_study(toc_sets = list(`T-1` = "Interim Analysis"))
  on.exit(unlink(d, recursive = TRUE))
  p <- tfl_check_spec(d)
  r <- p[p$part == "runs", , drop = FALSE]
  expect_true(nrow(r) >= 1L)
  expect_true(all(is.na(r$output_id)))
  expect_true(any(grepl("Interim Analysis", r$item)))
  # a report called all
  tfl_write_output(tfl_output("all", "listing"), file.path(d, "spec", "outputs", "all.yml"))
  p <- tfl_check_spec(d)
  expect_true(any(p$output_id %in% "all" & p$level == "error" & grepl("not allowed", p$message)))
  # one report's check carries no run rows
  expect_false(any(tfl_check_spec(d, output_id = "T-1")$part == "runs"))
})

test_that("the options table has two rows, in the form of tfl_arg_defs()", {
  o <- tflspec:::.runs_options()
  expect_identical(o$name, c("mode", "copy_data"))
  expect_identical(o$default, list("all", FALSE))
  expect_true(all(c("name", "label_en", "input", "type", "required", "default", "tier") %in% names(o)))
  expect_false("tfl_run_options" %in% getNamespaceExports("tflspec"))
})

test_that("the runner holds the same name rule as tfl_check_runs()", {
  e <- new.env()
  eval(parse(text = readLines(tfl_runner_file(), warn = FALSE, encoding = "UTF-8")), e)
  expect_identical(e$.batch_name_rx, tflspec:::.runs_name_rx)
})
