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
