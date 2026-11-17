# tfl_yaml_from_sheets(base = ): the sheets' changes onto the YAML spec in
# `to`, against the spec they were written from; what the sheets cannot
# hold stays; a file they do not change stays byte for byte

mg_sample <- function() system.file("extdata", "spec-yaml", package = "tflspec")

mg_demog <- function() {
  tfl_output("T-11", "table", label = "Demographics",
             template = list(name = "tflspec_v1/demog", args = list(
               by = "TRT01A", continuous = "AGE", categorical = c("SEX", "RACE"),
               analysis_ids = list(AGE = "AGE_STATS"),
               formats = list(mean = 1L, sd = 2L))),
             content = list(tfl_item("plan_paginate_rows", list(max_rows = 24L))))
}

# A study folder with a YAML spec (the sample's, written from its sheets),
# T-11 (a template report) added, and the files then given what the sheets
# cannot hold: T-11's maps and own content, T-14-1-1's CONT formats (a map)
# and a page key no sheet has.  The folder; removed by the caller.
mg_folder <- function() {
  to <- tempfile("mg-yaml")
  dir.create(to)
  sh <- suppressWarnings(suppressMessages(tfl_sheets_from_spec(
    tfl_read_study_spec(mg_sample()), c(tfl_read_outputs(mg_sample()), list(`T-11` = mg_demog())))))
  suppressWarnings(suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE)))
  od <- file.path(to, "spec", "outputs")
  tfl_write_output(mg_demog(), file.path(od, "T-11.yml"))
  o <- tfl_read_output(file.path(od, "T-14-1-1.yml"))
  i <- which(vapply(o$ard$analyses, function(a) identical(a$id, "CONT"), NA))
  o$ard$analyses[[i]]$formats <- list(mean = 1L, sd = 2L)
  o$page$memo <- "kept"
  tfl_write_output(o, file.path(od, "T-14-1-1.yml"))
  to
}

# the spec in `to` as the sheets were written from it: a copy (removed by the caller)
mg_base <- function(to) {
  b <- tempfile("mg-base")
  dir.create(b)
  file.copy(list.files(to, full.names = TRUE), b, recursive = TRUE)
  b
}

mg_file <- function(to, rel) readBin(file.path(to, rel), "raw", 1e6)
mg_out <- function(id) file.path("spec", "outputs", paste0(id, ".yml"))

# the sheets of `base`, `edit`ed, back onto `to`
mg_back <- function(to, base, edit = identity) {
  sh <- edit(suppressWarnings(suppressMessages(tfl_sheets_from_yaml(base))))
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE, base = base))
  tfl_read_outputs(to)
}

mg_cont <- function(o) o$ard$analyses[[which(vapply(o$ard$analyses, function(a) identical(a$id, "CONT"), NA))]]

mg_label <- function(id, v) function(sh) {
  sh$planner$description[sh$planner$output_id %in% id] <- v
  sh
}

test_that("nothing edited: every file stays byte for byte, and nothing is said", {
  to <- mg_folder()
  base <- mg_base(to)
  on.exit(unlink(c(to, base), recursive = TRUE), add = TRUE)
  rels <- c(mg_out("T-11"), mg_out("T-14-1-1"), "study.yml", "spec/defaults.yml", "spec/toc.yml")
  before <- lapply(rels, mg_file, to = to)
  expect_no_warning(mg_back(to, base))
  expect_identical(lapply(rels, mg_file, to = to), before)
})

test_that("a cell edited reaches the file; what the sheets cannot hold stays; another report is untouched", {
  to <- mg_folder()
  base <- mg_base(to)
  on.exit(unlink(c(to, base), recursive = TRUE), add = TRUE)
  was <- tfl_read_outputs(to)
  t11 <- mg_file(to, mg_out("T-11"))
  expect_no_warning(b <- mg_back(to, base, mg_label("T-14-1-1", "Demographics (edited)")))
  o <- b[["T-14-1-1"]]
  expect_identical(o$label, "Demographics (edited)")
  # (a nested analysis key, a page key, the content: as the file had them)
  expect_identical(mg_cont(o)$formats, list(mean = 1L, sd = 2L))
  expect_identical(o$page$memo, "kept")
  expect_identical(o$content, was[["T-14-1-1"]]$content)
  expect_identical(mg_file(to, mg_out("T-11")), t11)
})

test_that("a deleted cell takes the key away", {
  to <- mg_folder()
  base <- mg_base(to)
  on.exit(unlink(c(to, base), recursive = TRUE), add = TRUE)
  b <- mg_back(to, base, function(sh) {
    a <- sh$ard$analyses
    a$statistics[a$output_id %in% "T-14-1-1" & a$analysis_id %in% "CONT"] <- NA
    sh$ard$analyses <- a
    sh
  })
  cont <- mg_cont(b[["T-14-1-1"]])
  expect_null(cont$statistics)
  expect_identical(cont$variables, "AGE")
  expect_identical(cont$formats, list(mean = 1L, sd = 2L))
})

test_that("a template report edited on the sheets keeps its maps and its own content", {
  to <- mg_folder()
  base <- mg_base(to)
  on.exit(unlink(c(to, base), recursive = TRUE), add = TRUE)
  expect_no_warning(b <- mg_back(to, base, function(sh) {
    tr <- sh$template_rows
    tr$value[tr$output_id %in% "T-11" & tr$param %in% "categorical" & tr$value %in% "RACE"] <- "AGEGR1"
    sh$template_rows <- tr
    mg_label("T-11", "Demographics (edited)")(sh)
  }))
  o <- b[["T-11"]]
  a <- o$template$args
  expect_identical(o$label, "Demographics (edited)")
  expect_identical(a$categorical, c("SEX", "AGEGR1"))
  expect_identical(a$analysis_ids, list(AGE = "AGE_STATS"))
  expect_identical(a$formats, list(mean = 1L, sd = 2L))
  expect_identical(names(a), names(mg_demog()$template$args))
  expect_identical(vapply(o$content, function(it) it$fn, ""), "plan_paginate_rows")
})

test_that("a hand edit of the YAML since base stays where the sheets did not change it", {
  to <- mg_folder()
  base <- mg_base(to)
  on.exit(unlink(c(to, base), recursive = TRUE), add = TRUE)
  f <- file.path(to, mg_out("T-14-1-1"))
  o <- tfl_read_output(f)
  o$label <- "Demographics (by hand)"
  tfl_write_output(o, f)
  b <- mg_back(to, base, function(sh) {
    a <- sh$ard$analyses
    a$statistics[a$output_id %in% "T-14-1-1" & a$analysis_id %in% "CONT"] <- "N | mean"
    sh$ard$analyses <- a
    sh
  })
  expect_identical(b[["T-14-1-1"]]$label, "Demographics (by hand)")
  expect_identical(mg_cont(b[["T-14-1-1"]])$statistics, c("N", "mean"))
})

test_that("the same key changed in the YAML and on the sheets: the sheets' value, said", {
  to <- mg_folder()
  base <- mg_base(to)
  on.exit(unlink(c(to, base), recursive = TRUE), add = TRUE)
  f <- file.path(to, mg_out("T-14-1-1"))
  o <- tfl_read_output(f)
  o$label <- "Demographics (by hand)"
  tfl_write_output(o, f)
  expect_warning(b <- mg_back(to, base, mg_label("T-14-1-1", "Demographics (sheet)")),
                 "spec/outputs/T-14-1-1.yml: label", fixed = TRUE)
  expect_identical(b[["T-14-1-1"]]$label, "Demographics (sheet)")
})

test_that("a base that is no YAML spec stops", {
  to <- mg_folder()
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  sh <- suppressWarnings(suppressMessages(tfl_sheets_from_yaml(to)))
  nowhere <- tempfile("mg-none")
  expect_error(suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE, base = nowhere)), "base")
})

test_that("no base: what the sheets cannot hold is said", {
  to <- mg_folder()
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  sh <- suppressWarnings(suppressMessages(tfl_sheets_from_yaml(to)))
  expect_true(any(sh$not_converted$part == "analyses.CONT.formats"))
  out <- tempfile("mg-out")
  dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  w <- character()
  withCallingHandlers(suppressMessages(tfl_yaml_from_sheets(sh, out, overwrite = TRUE)),
                      warning = function(cnd) {
                        w <<- c(w, conditionMessage(cnd))
                        invokeRestart("muffleWarning")
                      })
  expect_true(any(grepl("T-14-1-1: keys the sheets cannot hold are not kept: analyses.CONT.formats", w, fixed = TRUE)))
  expect_true(any(grepl("T-11: template arguments the templates sheet cannot hold are not kept: analysis_ids, formats",
                        w, fixed = TRUE)))
})

test_that("tfl_spec_merge3(): maps by key, lists of maps by id, conflicts named", {
  # a key the sheets cannot hold (in neither) stays; one deleted goes
  expect_identical(c(tfl_spec_merge3(list(a = 1, b = 2, x = 9), list(a = 1, b = 2), list(a = 3))),
                   list(a = 3, x = 9))
  # rows by id: the edited one merged, an added one taken, a deleted one gone
  s <- list(list(id = "A", v = 1, k = "keep"), list(id = "B", v = 2))
  w <- list(list(id = "A", v = 1), list(id = "B", v = 2))
  n <- list(list(id = "A", v = 5), list(id = "C", v = 3))
  expect_identical(c(tfl_spec_merge3(s, w, n)), list(list(id = "A", v = 5, k = "keep"), list(id = "C", v = 3)))
  # the YAML changed v since base, the sheets too: theirs, named
  m <- tfl_spec_merge3(list(v = 7), list(v = 1), list(v = 5), orig = list(v = 1.5))
  expect_identical(m[["v"]], 5)
  expect_identical(attr(m, "conflicts"), "v")
  # no orig: no conflict told
  expect_identical(attr(tfl_spec_merge3(list(v = 7), list(v = 1), list(v = 5)), "conflicts"), character())
})
