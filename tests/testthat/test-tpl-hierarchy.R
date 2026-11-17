# tflspec_v1/tflspec_hierarchy: the series base template for the records of
# any dataset counted in a hierarchy.  The package's sample data
# (fictional) only.

hier_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
}

# A study of the sample data: ADSL and ADAE, the sample-01 defaults
hier_study <- function(d) {
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "HIER-1")
  file.copy(test_path("fixtures", "twin", "defaults-sample01.yml"),
            file.path(d, "spec", "defaults.yml"), overwrite = TRUE)
  d
}

# A report of the template: `tpl` the lines under `template:` (after its
# name), `data` the lines of the report's own data rows
hier_report <- function(id, tpl, data = character()) {
  c(paste0("id: ", id), "kind: table", "label: Records by hierarchy",
    "common:", "  population: SAF",
    "codelists:", "  TRT01A:", "    - value: Drug X", "    - value: Control",
    "template:", "  name: tflspec_v1/tflspec_hierarchy", tpl,
    if (length(data)) c("data:", data),
    "style: compact")
}

# The report written to the study, as its code SPEC; its ARD and RTF
hier_run <- function(d, id, lines, rtf = TRUE) {
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  writeLines(lines, f)
  st <- tfl_read_study_spec(d)
  tfl_write_output(tfl_template_to_spec(tfl_read_output(f), st), f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, dir = d, output_id = id, save = FALSE))
  list(ard = ard, rtf = if (rtf) tfl_run_output(id, dir = d))
}

# An ARD's rows as text, the analysis ids renamed (AE_ANY -> ANY ...)
hier_ard <- function(a) {
  a <- as.data.frame(a)
  a$analysis_id <- sub("^AE_", "", a$analysis_id)
  cols <- c("analysis_id", grep("^group[0-9]+(_level)?$", names(a), value = TRUE),
            "variable", "variable_level", "stat_name", "stat", "stat_fmt")
  cols <- intersect(cols, names(a))
  cell <- function(v) if (is.list(v)) vapply(v, function(x)
    if (is.null(x) || !length(x)) "" else paste(format(unlist(x), digits = 15), collapse = "|"), "")
    else ifelse(is.na(v), "", as.character(v))
  do.call(paste, c(lapply(a[cols], cell), sep = " / "))
}

hier_tpl <- c("  from: ADAE",
              "  analysis:",
              "    by: [TRT01A]",
              "    hierarchy: [AEBODSYS, AEDECOD]")
hier_teae <- c("  - id: rec_sub", "    where: !r TRTEMFL == \"Y\"")

test_that("tflspec_hierarchy on ADAE gives ae_soc_pt's ARD and an RTF", {
  hier_skip()
  d <- hier_study(file.path(tempdir(), "hier-ae"))
  on.exit(unlink(d, recursive = TRUE))
  h <- hier_run(d, "H-1", hier_report("H-1", hier_tpl, hier_teae))
  x <- readLines(test_path("fixtures", "twin", "T-14-3-1T.yml"))
  x <- sub("^id: T-14-3-1T$", "id: A-1", x)
  a <- hier_run(d, "A-1", x, rtf = FALSE)
  expect_identical(unique(as.character(h$ard$analysis_id)), c("BIGN", "ANY", "L1", "L2"))
  expect_identical(hier_ard(h$ard), hier_ard(a$ard))
  expect_true(file.exists(h$rtf))
  expect_true(any(grepl("Subjects with at least one event", readLines(h$rtf, warn = FALSE))))
})

test_that("tflspec_hierarchy: analysis_ids rename the depths", {
  hier_skip()
  d <- hier_study(file.path(tempdir(), "hier-ids"))
  on.exit(unlink(d, recursive = TRUE))
  h <- hier_run(d, "H-2", hier_report("H-2", c(hier_tpl, "    analysis_ids: {ANY: AEANY, L2: AEPT}"),
                                      hier_teae), rtf = FALSE)
  expect_identical(unique(as.character(h$ard$analysis_id)), c("BIGN", "AEANY", "L1", "AEPT"))
})

test_that("tflspec_hierarchy: from and hierarchy are required", {
  rep <- function(tpl) list(id = "X", kind = "table",
                            template = c(list(name = "tflspec_v1/tflspec_hierarchy"), tpl))
  expect_error(tfl_template_resolve(rep(list(by = "TRT01A", hierarchy = c("AEBODSYS", "AEDECOD")))),
               "needs from")
  expect_error(tfl_template_resolve(rep(list(by = "TRT01A", from = "ADAE"))),
               "needs hierarchy")
})

test_that("tflspec_hierarchy: sort_column orders the rows by one arm's count", {
  hier_skip()
  d <- hier_study(file.path(tempdir(), "hier-sort"))
  on.exit(unlink(d, recursive = TRUE))
  # the keys: the arm's count, the SOC by its own
  r <- tfl_template_resolve(
    list(id = "X", kind = "table", template = list(
      name = "tflspec_v1/tflspec_hierarchy", from = "ADAE", by = "TRT01A",
      hierarchy = c("AEBODSYS", "AEDECOD"), sort_column = "Control")))
  keys <- Filter(function(i) identical(i$fn, "plan_sort"), r$content)
  expect_identical(as.character(unlist(keys[[1L]]$args$keys)),
                   c(".overall", "-n@group1[Control]", "group1", ".depth", "-n[Control]", "label"))
  h <- hier_run(d, "H-3", hier_report("H-3", c(hier_tpl, "  content:", "    sort_column: Control"),
                                      hier_teae))
  ae <- tflspec::adae[tflspec::adae$TRTEMFL %in% "Y" & tflspec::adae$TRT01A %in% "Control", ]
  n <- tapply(ae$USUBJID, as.character(ae$AEBODSYS), function(u) length(unique(u)))
  want <- names(n)[order(-n, names(n))]
  txt <- paste(readLines(h$rtf, warn = FALSE), collapse = "\n")
  pos <- vapply(want, function(s) regexpr(s, txt, fixed = TRUE)[[1L]], 0)
  expect_true(all(pos > 0))
  expect_identical(order(pos), seq_along(want))
})

test_that("tflspec_hierarchy: empty_text prints when no record is left; none when unset", {
  hier_skip()
  d <- hier_study(file.path(tempdir(), "hier-empty"))
  on.exit(unlink(d, recursive = TRUE))
  none <- c("  - id: rec_sub", "    where: !r TRTEMFL == \"Z\"")
  h <- hier_run(d, "H-4", hier_report("H-4", hier_tpl, none))
  expect_true(any(grepl("No records were reported.", readLines(h$rtf, warn = FALSE), fixed = TRUE)))
  # a report's own sentence
  h <- hier_run(d, "H-5", hier_report("H-5", c(hier_tpl, "  content:", "    empty_text: No events."), none))
  expect_true(any(grepl("No events.", readLines(h$rtf, warn = FALSE), fixed = TRUE)))
  # unset: no plan_empty card
  r <- tfl_template_resolve(
    list(id = "X", kind = "table", template = list(
      name = "tflspec_v1/tflspec_hierarchy", from = "ADAE", by = "TRT01A",
      hierarchy = "AEDECOD", empty_text = NULL)))
  expect_false("plan_empty" %in% vapply(r$content, function(i) i$fn, ""))
  y <- tfl_template_resolve(
    list(id = "X", kind = "table", template = list(
      name = "tflspec_v1/tflspec_hierarchy", from = "ADAE", by = "TRT01A",
      hierarchy = "AEDECOD", empty_text = "")))
  expect_false("plan_empty" %in% vapply(y$content, function(i) i$fn, ""))
})

test_that("tflspec_hierarchy: min_pct filters the rows; total needs the Total column", {
  hier_skip()
  d <- hier_study(file.path(tempdir(), "hier-pct"))
  on.exit(unlink(d, recursive = TRUE))
  all <- hier_run(d, "H-6", hier_report("H-6", hier_tpl, hier_teae), rtf = FALSE)
  few <- hier_run(d, "H-7", hier_report("H-7", c(hier_tpl, "    min_pct: 20"), hier_teae), rtf = FALSE)
  rows <- function(a) length(unique(as.character(unlist(as.data.frame(a)$variable_level))))
  expect_lt(rows(few$ard), rows(all$ard))
  p <- as.data.frame(few$ard)
  p <- p[p$analysis_id == "L2" & p$stat_name == "p", ]
  top <- tapply(vapply(p$stat, function(s) as.numeric(unlist(s))[1L], 0),
                vapply(p$variable_level, function(s) as.character(unlist(s))[1L], ""), max)
  expect_true(all(top >= 0.2))
  tot <- hier_run(d, "H-9", hier_report("H-9", c(hier_tpl, "    total: true", "    min_pct: 20",
                                                   "    min_pct_in: total"), hier_teae))
  expect_true(any(grepl("Total", readLines(tot$rtf, warn = FALSE), fixed = TRUE)))
  expect_error(hier_run(d, "H-8", hier_report("H-8", c(hier_tpl, "    min_pct: 20",
                                                         "    min_pct_in: total"), hier_teae),
                        rtf = FALSE), "Total")
})
