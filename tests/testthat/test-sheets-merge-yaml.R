# The sheets back onto a folder that has a YAML spec: a report file takes
# only what the sheets change; what they cannot hold stays as the file has
# it; a report they do not change keeps its file byte for byte

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

mg_bytes <- function(to, id) readBin(file.path(to, "spec", "outputs", paste0(id, ".yml")), "raw", 1e6)

# the folder's sheets, `edit`ed, back onto the folder
mg_back <- function(to, edit = identity) {
  sh <- edit(suppressWarnings(suppressMessages(tfl_sheets_from_yaml(to))))
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
  tfl_read_outputs(to)
}

mg_cont <- function(o) o$ard$analyses[[which(vapply(o$ard$analyses, function(a) identical(a$id, "CONT"), NA))]]

test_that("nothing edited: every report file stays byte for byte, and nothing is said", {
  to <- mg_folder()
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  before <- lapply(c("T-11", "T-14-1-1"), function(id) mg_bytes(to, id))
  expect_no_warning(mg_back(to))
  expect_identical(lapply(c("T-11", "T-14-1-1"), function(id) mg_bytes(to, id)), before)
})

test_that("a cell edited reaches the file; what the sheets cannot hold stays; another report is untouched", {
  to <- mg_folder()
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  was <- tfl_read_outputs(to)
  t11 <- mg_bytes(to, "T-11")
  b <- mg_back(to, function(sh) {
    sh$planner$description[sh$planner$output_id %in% "T-14-1-1"] <- "Demographics (edited)"
    sh
  })
  o <- b[["T-14-1-1"]]
  expect_identical(o$label, "Demographics (edited)")
  # (a nested analysis key, a page key, the content: as the file had them)
  expect_identical(mg_cont(o)$formats, list(mean = 1L, sd = 2L))
  expect_identical(o$page$memo, "kept")
  expect_identical(o$content, was[["T-14-1-1"]]$content)
  expect_identical(mg_bytes(to, "T-11"), t11)
})

test_that("a deleted cell takes the key away", {
  to <- mg_folder()
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  b <- mg_back(to, function(sh) {
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
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  expect_no_warning(b <- mg_back(to, function(sh) {
    sh$planner$description[sh$planner$output_id %in% "T-11"] <- "Demographics (edited)"
    sh
  }))
  o <- b[["T-11"]]
  d <- tfl_read_output(file.path(to, "spec", "outputs", "T-11.yml"))
  expect_identical(o$label, "Demographics (edited)")
  w <- mg_demog()
  expect_identical(o$template$args$analysis_ids, list(AGE = "AGE_STATS"))
  expect_identical(o$template$args$formats, list(mean = 1L, sd = 2L))
  expect_identical(names(o$template$args), names(w$template$args))
  expect_identical(vapply(o$content, function(it) it$fn, ""), "plan_paginate_rows")
  expect_identical(d$template, o$template)
})

test_that("a template argument edited on the sheets reaches the file, its maps kept", {
  to <- mg_folder()
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  b <- mg_back(to, function(sh) {
    tr <- sh$template_rows
    tr$value[tr$output_id %in% "T-11" & tr$param %in% "categorical" & tr$value %in% "RACE"] <- "AGEGR1"
    sh$template_rows <- tr
    sh
  })
  a <- b[["T-11"]]$template$args
  expect_identical(a$categorical, c("SEX", "AGEGR1"))
  expect_identical(a$analysis_ids, list(AGE = "AGE_STATS"))
})

test_that("no YAML in the folder: the maps the sheets cannot hold are said, as before", {
  sh <- suppressWarnings(suppressMessages(tfl_sheets_from_spec(
    tfl_read_study_spec(mg_sample()), list(`T-11` = mg_demog()))))
  to <- tempfile("mg-yaml")
  dir.create(to)
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  expect_warning(suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE)),
                 "analysis_ids, formats", fixed = TRUE)
})

test_that(".merge3(): maps by key, lists of maps by id, items by verb", {
  m3 <- tflspec:::.merge3
  # a key the sheets cannot hold (in neither) stays; one deleted goes
  expect_identical(m3(list(a = 1, b = 2, x = 9), list(a = 1, b = 2), list(a = 3)), list(a = 3, x = 9))
  # rows by id: the edited one merged, an added one taken, the spec's order
  s <- list(list(id = "A", v = 1, k = "keep"), list(id = "B", v = 2))
  w <- list(list(id = "A", v = 1), list(id = "B", v = 2))
  n <- list(list(id = "A", v = 5), list(id = "B", v = 2), list(id = "C", v = 3))
  expect_identical(m3(s, w, n), list(list(id = "A", v = 5, k = "keep"), list(id = "B", v = 2),
                                     list(id = "C", v = 3)))
  # a row deleted on the sheets goes
  expect_identical(m3(s, w, n[1L]), list(list(id = "A", v = 5, k = "keep")))
})
