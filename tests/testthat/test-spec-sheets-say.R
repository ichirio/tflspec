# The sheet round trip says what it does not keep (#689, part 2): the notes
# reach the person, what stops a conversion is dropped with a note, and the
# flat workbook leaves what it has no column for

# a small study from the text of its files (name under spec/ -> lines)
rts_study <- function(files = list(), study = NULL, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("datasets:", "  - dataset: ADSL", "    level: ADaM", "    path: adam/adsl.xpt",
               "  - dataset: ADTTE", "    level: ADaM", "    path: adam/adtte.xpt",
               "populations:", "  - id: SAF", "    dataset: ADSL", "    where: !r SAFFL == \"Y\"",
               "    label: Safety Population"), file.path(d, "spec", "data.yml"))
  writeLines(study %||% c("study_id: MIN-1", "title: minimal", "settings:", "  rounding: sas",
                          "  output_path: output/tfl", "  program_dir: programs/tfl",
                          "  file: '{output_id}.rtf'", "keys:", "  id: USUBJID"),
             file.path(d, "study.yml"))
  if (is.null(files[["defaults.yml"]])) files[["defaults.yml"]] <- "content: []"
  for (nm in names(files)) {
    f <- file.path(d, "spec", nm)
    dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
    writeLines(files[[nm]], f)
  }
  d
}

rts_table <- function(id = "T-1", extends = "defaults", ...) {
  c(paste0("id: ", id), "kind: table", "label: One", "population: SAF",
    paste0("extends: [", paste(extends, collapse = ", "), "]"), ...,
    "content:", "  - table_plan: {cols: TRT01A, rows: {group: variable}}")
}

# YAML -> sheets -> YAML, the messages kept
rts_trip <- function(d, env = parent.frame(), ...) {
  to <- withr::local_tempdir(.local_envir = env)
  msgs <- character()
  keep <- function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }
  sh <- withCallingHandlers(tfl_sheets_from_yaml(d), message = keep)
  r <- withCallingHandlers(tfl_yaml_from_sheets(sh, to, overwrite = TRUE, ...), message = keep)
  list(sh = sh, r = r, to = to, msgs = msgs,
       nc = yaml::read_yaml(file.path(to, "spec", "_not_converted.yml"))$not_converted)
}

test_that("the sheets' notes are written to _not_converted.yml, and counted in one message", {
  d <- rts_study(list("defaults.yml" = c("codelists:", "  SEX: [F, M]", "content: []"),
                      "outputs/T-1.yml" = rts_table()))
  x <- rts_trip(d)
  expect_gt(nrow(x$sh$not_converted), 0L)
  parts <- vapply(x$nc, function(e) e$sheet, "")
  expect_true("codelists" %in% parts)
  expect_identical(sum(grepl("could not hold", x$msgs)), 1L)
  expect_match(x$msgs[grepl("could not hold", x$msgs)],
               paste0("tfl_yaml_from_sheets(): ", nrow(x$sh$not_converted), " part"), fixed = TRUE)
  # the converter's own notes are still there, and the return lists all of them
  expect_true(all(x$sh$not_converted$reason %in% x$r$not_converted$reason))
})

test_that("a study that lost nothing says nothing, and its list is the converter's own", {
  d <- rts_study(list("outputs/T-1.yml" = rts_table()))
  x <- rts_trip(d)
  expect_identical(nrow(x$sh$not_converted), 0L)
  expect_identical(length(x$msgs), 0L)
  expect_identical(length(x$nc), nrow(x$r$not_converted))
})

# -- item 2: a figure written as items -------------------------------------------

rts_fig <- function(id = "F-1", top = NULL, data_extra = NULL) {
  c(paste0("id: ", id), "kind: figure", "label: Fig one", "population: SAF", "extends: none", top,
    "data:", data_extra, "  steps:", "    - {step: read, dataset: ADTTE}", "    - {step: param, value: PFS}",
    "content:", "  - ggplot: {data: data, mapping: {x: AVAL, y: AGE}}", "  - geom_point: {size: 2}",
    "  - scale_x_continuous: {limits: !r 'c(0, 10)'}", "  - labs: {x: Time}")
}

test_that("a figure written as items comes back with its items and data, either figures =", {
  d <- rts_study(list("outputs/F-1.yml" = rts_fig()))
  a <- tfl_read_outputs(d)[["F-1"]]
  for (fg in c("design", "items")) {
    x <- rts_trip(d, figures = fg)
    b <- tfl_read_outputs(x$to)[["F-1"]]
    expect_length(b$content, 4L)
    expect_identical(b$content, a$content, info = fg)
    expect_identical(b$data, a$data, info = fg)
    expect_false(any(x$sh$not_converted$output %in% "F-1"), info = fg)
  }
  # the design list as a file (spec/figures/<id>.yml) keeps the items too
  f <- withr::local_tempfile(fileext = ".yml")
  tfl_write_fig_design(x$sh$figures[["F-1"]], f)
  expect_match(paste(readLines(f), collapse = "\n"), "limits: !r c(0, 10)", fixed = TRUE)
  y <- .conv_read_fig(f)
  expect_identical(lapply(y$content, .item_from_list), a$content)
})

test_that("what a figure of items has no place for is named in the notes, by report id", {
  d <- rts_study(list("outputs/F-1.yml" = rts_fig(top = "ggplot2_version: 4.0.0")))
  x <- rts_trip(d)
  expect_false(any(x$sh$not_converted$output %in% "F-1"))
  expect_identical(tfl_read_outputs(x$to)[["F-1"]]$ggplot2_version, "4.0.0")
  # the designer's plot settings beside the items are the design list's too
  d1 <- rts_study(list("outputs/F-1.yml" = rts_fig(top = c("plot:", "  title: T", "  theme: minimal"))))
  x1 <- rts_trip(d1)
  expect_false(any(x1$sh$not_converted$output %in% "F-1"))
  expect_identical(tfl_read_outputs(x1$to)[["F-1"]]$plot, tfl_read_outputs(d1)[["F-1"]]$plot)
  d2 <- rts_study(list("outputs/F-1.yml" = sub("steps:", "object: ard\n  steps:", rts_fig(), fixed = TRUE)))
  x2 <- rts_trip(d2)
  nc <- x2$sh$not_converted
  expect_match(nc$reason[nc$output %in% "F-1"], "object is not kept", fixed = TRUE)
  expect_true(any(vapply(x2$nc, function(e) identical(e$output, "F-1"), NA)))
})
