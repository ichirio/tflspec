# tflspec_v1/tflspec_event: the series base template for the subjects with
# at least one record of each kind of event, one row a kind.  The
# package's sample data (fictional) only.

evt_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
}

# A study of the sample data: ADSL and ADAE, the sample-01 defaults
evt_study <- function(d) {
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "EVT-1")
  file.copy(test_path("fixtures", "twin", "defaults-sample01.yml"),
            file.path(d, "spec", "defaults.yml"), overwrite = TRUE)
  d
}

# A report of the template: `tpl` the lines under `template:` (after its
# name), `more` the report's own lines after it; the records kept are the
# treatment-emergent ones
evt_report <- function(id, tpl, more = character()) {
  c(paste0("id: ", id), "kind: table", "label: Overview of TEAEs",
    "common:", "  population: SAF",
    "codelists:", "  TRT01A:", "    - value: Drug X", "    - value: Control",
    "template:", "  name: tflspec_v1/tflspec_event", tpl,
    "data:", "  - id: rec_sub", "    where: !r TRTEMFL == \"Y\"",
    more, "style: compact")
}

# The report written to the study, as its code SPEC; its ARD and RTF
evt_run <- function(d, id, lines, rtf = TRUE) {
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  writeLines(lines, f)
  st <- tfl_read_study_spec(d)
  tfl_write_output(tfl_template_to_spec(tfl_read_output(f), st), f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, dir = d, output_id = id, save = FALSE))
  list(ard = as.data.frame(ard), rtf = if (rtf) tfl_run_output(id, dir = d))
}

# one ARD cell: the statistic of the analysis, group (NA: the Total
# column) and stat_name
evt_stat <- function(a, id, arm, stat) {
  lv <- vapply(a$group1_level, function(x) if (length(x)) as.character(unlist(x))[1L] else NA_character_, "")
  hit <- a$analysis_id == id & a$stat_name == stat & (if (is.na(arm)) is.na(lv) else lv %in% arm)
  vapply(a$stat[hit], function(x) as.numeric(unlist(x))[1L], 0)
}

evt_tpl <- c("  from: ADAE",
             "  analysis:",
             "    by: [TRT01A]",
             "    total: true",
             "    events:",
             "      - {id: ANY, label: Any TEAE}",
             "      - {id: SER, label: Any serious TEAE, where: !r AESER == \"Y\"}",
             "      - {id: NONE, label: Any TEAE of no such outcome, where: !r AEOUT == \"NO SUCH\"}",
             "  content:",
             "    heading: Subjects with")

test_that("tflspec_event resolves: the rows are the EVT item's condition rows, ids their own", {
  r <- tfl_template_resolve(
    list(id = "X", kind = "table", template = list(
      name = "tflspec_v1/tflspec_event", from = "ADAE", by = "TRT01A", total = TRUE,
      events = list(list(id = "ANY", label = "Any TEAE"),
                    list(id = "SER", label = "Any serious TEAE", where = tfl_r("AESER == \"Y\""))))))
  evt <- Filter(function(a) identical(a$id, "EVT"), r$ard$analyses)[[1L]]
  expect_identical(evt$params, c("label", "where"))
  expect_identical(evt$conditions$analysis_id, c("ANY", "SER"))
  expect_identical(evt$conditions$label, c("Any TEAE", "Any serious TEAE"))
  expect_identical(evt$conditions$where, c(NA, "AESER == \"Y\""))
  expect_identical(evt$method, "hierarchical")
  fns <- vapply(r$content, function(i) i$fn, "")
  # (no plan_cells: count_cell empty, the study's own)
  expect_identical(fns, c("table_plan", "plan_total", "plan_col_header", "plan_paginate_rows"))
  # total off (the series' default): no plan_total card
  r0 <- tfl_template_resolve(
    list(id = "X", kind = "table", template = list(
      name = "tflspec_v1/tflspec_event", from = "ADAE", by = "TRT01A",
      events = list(list(id = "ANY", label = "Any TEAE")))))
  expect_false("plan_total" %in% vapply(r0$content, function(i) i$fn, ""))
})

test_that("tflspec_event: from and events are required", {
  rep <- function(tpl) list(id = "X", kind = "table",
                            template = c(list(name = "tflspec_v1/tflspec_event"), tpl))
  expect_error(tfl_template_resolve(rep(list(by = "TRT01A", events = list(list(id = "ANY", label = "Any"))))),
               "needs from")
  expect_error(tfl_template_resolve(rep(list(by = "TRT01A", from = "ADAE"))),
               "needs events")
})

test_that("tflspec_event: an ARD row a kind, the subjects counted; a kind no record meets is 0", {
  evt_skip()
  d <- evt_study(file.path(tempdir(), "evt-ard"))
  on.exit(unlink(d, recursive = TRUE))
  e <- evt_run(d, "E-1", evt_report("E-1", evt_tpl))
  a <- e$ard
  expect_identical(unique(as.character(a$analysis_id)), c("BIGN", "ANY", "SER", "NONE"))
  ae <- tflspec::adae[tflspec::adae$TRTEMFL %in% "Y", ]
  for (arm in c("Drug X", "Control")) {
    n_any <- length(unique(ae$USUBJID[ae$TRT01A %in% arm]))
    n_ser <- length(unique(ae$USUBJID[ae$TRT01A %in% arm & ae$AESER %in% "Y"]))
    expect_equal(evt_stat(a, "ANY", arm, "n"), n_any, info = arm)
    expect_equal(evt_stat(a, "SER", arm, "n"), n_ser, info = arm)
    # the row no record meets: there, 0 in every group
    expect_equal(evt_stat(a, "NONE", arm, "n"), 0, info = arm)
    expect_equal(evt_stat(a, "NONE", arm, "p"), 0, info = arm)
  }
  # the Total column
  expect_equal(evt_stat(a, "ANY", NA, "n"), length(unique(ae$USUBJID)))
  expect_equal(evt_stat(a, "NONE", NA, "n"), 0)
  txt <- readLines(e$rtf, warn = FALSE)
  pos <- vapply(c("Any TEAE", "Any serious TEAE", "Any TEAE of no such outcome"),
                function(s) regexpr(s, paste(txt, collapse = "\n"), fixed = TRUE)[[1L]], 0)
  expect_true(all(pos > 0))
  # the rows in the events' order
  expect_identical(order(pos), 1:3)
  expect_true(any(grepl("Subjects with", txt, fixed = TRUE)))
  expect_true(any(grepl("Total", txt, fixed = TRUE)))
})

test_that("tflspec_event: total off (the series' default); an appended card indents rows", {
  evt_skip()
  d <- evt_study(file.path(tempdir(), "evt-nototal"))
  on.exit(unlink(d, recursive = TRUE))
  # (the rows are flat; a report's plan_cell_style indents the ones after the first)
  card <- c("content:", paste0("  - plan_cell_style: {cols: row_label, where: !r 'row_label != ",
                               "\"Any TEAE\"', indent_twips: 240}"))
  e <- evt_run(d, "E-2", evt_report("E-2", evt_tpl[-4], card))
  lv <- vapply(e$ard$group1_level, function(x) length(x) > 0L && !is.na(unlist(x)[1L]), NA)
  expect_true(all(lv[e$ard$analysis_id %in% c("ANY", "SER", "NONE")]))
  txt <- readLines(e$rtf, warn = FALSE)
  expect_false(any(grepl("Total", txt, fixed = TRUE)))
  expect_true(any(grepl("\\li240\\ri0 Any serious TEAE", txt, fixed = TRUE)))
  expect_true(any(grepl("\\li0\\ri0 Any TEAE\\cell", txt, fixed = TRUE)))
})
