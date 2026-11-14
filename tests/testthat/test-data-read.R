# A report's `common:` part and its `data:` list (DP-2): the reader, the old
# places that stop, the round trip through the sheets, A = B

dr_write <- function(lines) {
  f <- tempfile(fileext = ".yml")
  writeLines(lines, f)
  f
}

dr_table <- c(
  "id: T-1", "kind: table",
  "common:", "  population: SAF", "  treatment: TRT01A", "  population_label: Safety set",
  "data:",
  "  - id: adsl_sub", "    from: ADSL", "    where: !r SEX == \"F\"",
  "  - id: adae_sub", "    from: ADAE", "    subjects: adsl_sub", "    where: !r TRTEMFL == \"Y\"",
  "ard:", "  analyses:",
  "    - {id: AE, method: hierarchical, data: adae_sub, by: [TRT01A], variables: [AEBODSYS, AEDECOD]}",
  "content:", "  - table_plan: {cols: TRT01A, rows: {group1: AEBODSYS}}")

test_that("common: is read as the report's common part, a report may have none", {
  f <- dr_write(dr_table)
  on.exit(unlink(f))
  o <- tfl_read_output(f)
  expect_identical(o$common, list(population = "SAF", treatment = "TRT01A", population_label = "Safety set"))
  expect_identical(tflspec:::.out_population(o), "SAF")
  g <- dr_write(c("id: T-2", "kind: table", "content: []"))
  on.exit(unlink(g), add = TRUE)
  o2 <- tfl_read_output(g)
  expect_identical(o2$common, list())
  expect_true(is.na(tflspec:::.out_population(o2)))
  expect_identical(tfl_output("T-3", "table", common = list(population = NA))$common, list())
})

test_that("the data: list is the report's definitions, written back as data:, not under ard:", {
  f <- dr_write(dr_table)
  on.exit(unlink(f))
  o <- tfl_read_output(f)
  expect_identical(vapply(o$ard$analysis_data, function(e) e$id, ""), c("adsl_sub", "adae_sub"))
  expect_null(o$data)
  out <- tempfile(fileext = ".yml")
  on.exit(unlink(out), add = TRUE)
  tfl_write_output(o, out)
  txt <- readLines(out)
  expect_true("data:" %in% txt)
  expect_false(any(grepl("analysis_data", txt)))
  expect_false(any(grepl("source: ard", txt)))
  expect_identical(tfl_read_output(out)$ard$analysis_data, o$ard$analysis_data)
  expect_identical(tfl_read_output(out)$common, o$common)
  # the common part is written in the file, before the report's steps
  expect_lt(which(txt == "common:"), which(txt == "data:"))
})

test_that("each old place stops with a line saying where it went", {
  old_pop <- dr_write(c("id: T-1", "kind: table", "population: SAF", "content: []"))
  expect_error(tfl_read_output(old_pop), "`population:` -> `common: population`", fixed = TRUE)
  old_ard <- dr_write(c("id: T-1", "kind: table", "ard:", "  analysis_data:", "    - {id: adsl_sub, from: ADSL}",
                        "  analyses: []", "content: []"))
  expect_error(tfl_read_output(old_ard), "`ard: analysis_data:` moved to `data:`", fixed = TRUE)
  old_map <- dr_write(c("id: T-1", "kind: table", "data: {source: ard}", "content: []"))
  expect_error(tfl_read_output(old_map), "a table reads its ARD", fixed = TRUE)
  old_lst <- dr_write(c("id: L-1", "kind: listing", "ard:", "  analysis_data:", "    - {id: a, from: ADAE}", "content: []"))
  expect_error(tfl_read_output(old_lst), "moved to `data:`", fixed = TRUE)
  expect_error(tfl_output("T-1", "table", population = "SAF"), "`population` -> `common: population`", fixed = TRUE)
  unlink(c(old_pop, old_ard, old_map, old_lst))
})

test_that("an unknown common: key stops, naming the keys; a value is one text", {
  f <- dr_write(c("id: T-1", "kind: table", "common:", "  population: SAF", "  arm: TRT01A", "content: []"))
  expect_error(tfl_read_output(f), "`common` takes `population`, `treatment` and `population_label`", fixed = TRUE)
  expect_error(tfl_output("T-1", "table", common = list(set = "SAF")), "`common` takes", fixed = TRUE)
  expect_error(tfl_output("T-1", "table", common = list(population = c("SAF", "ITT"))), "one text", fixed = TRUE)
  unlink(f)
})

test_that("a listing with a data: list prints its last definition; written back it is the list", {
  f <- dr_write(c("id: L-1", "kind: listing", "common: {population: SAF}",
                  "data:", "  - {id: adsl_sub, from: ADSL}",
                  "  - {id: adae_sub, from: ADAE, subjects: adsl_sub, where: !r AESER == \"Y\"}",
                  "content: []"))
  on.exit(unlink(f))
  o <- tfl_read_output(f)
  expect_identical(o$data, list(source = "dataset", data = "adae_sub"))
  out <- tempfile(fileext = ".yml")
  on.exit(unlink(out), add = TRUE)
  tfl_write_output(o, out)
  txt <- readLines(out)
  expect_false(any(grepl("source: dataset", txt)))
  expect_identical(tfl_read_output(out)$data, o$data)
  # a map that says more than that has no place beside the list: `ard: data:`
  o$data$where <- "AESER == 'Y'"
  tfl_write_output(o, out)
  txt2 <- readLines(out)
  expect_true(any(grepl("^data:", txt2)))
  expect_true(any(grepl("^  data:", txt2)))
  expect_identical(tfl_read_output(out)$data, o$data)
  expect_identical(tfl_read_output(out)$ard$analysis_data, o$ard$analysis_data)
})

test_that("the subjects definition takes the report's analysis set; a definition of its own set keeps it", {
  o <- tfl_read_output(dr_write(c(
    "id: T-1", "kind: table", "common: {population: SAF}",
    "data:", "  - {id: adsl_sub, from: ADSL}",
    "  - {id: adsl_den, from: ADSL, population: ITT}",
    "  - {id: adae_sub, from: ADAE, subjects: adsl_sub}", "content: []")))
  a <- o$ard$analysis_data
  expect_null(tflspec:::.entry_pop_inject(a[[3L]], "SAF")$population)
  expect_identical(tflspec:::.entry_pop_inject(a[[1L]], "SAF")$population, "SAF")
  expect_identical(tflspec:::.entry_pop_inject(a[[2L]], "SAF")$population, "ITT")
  expect_null(tflspec:::.entry_pop_strip(list(id = "x", from = "ADSL", population = "SAF"), "SAF")$population)
  expect_identical(tflspec:::.entry_pop_strip(list(id = "x", from = "ADSL", population = "ITT"), "SAF")$population, "ITT")
})

test_that("YAML -> sheets -> YAML keeps common: and data: of the sample report", {
  skip_if_not_installed("openxlsx")
  d0 <- system.file("extdata", "spec-yaml", package = "tflspec")
  d <- tempfile("dr-rt-")
  to <- tempfile("dr-rt-back-")
  on.exit(unlink(c(d, to), recursive = TRUE))
  dir.create(d)
  file.copy(d0, d, recursive = TRUE)
  d <- file.path(d, "spec-yaml")
  f <- file.path(d, "outputs", "T-14-1-1.yml")
  o0 <- tfl_read_output(f)
  expect_identical(o0$common$population, "SAF")
  expect_identical(o0$common$treatment, "TRT01A")
  expect_identical(vapply(o0$ard$analysis_data, function(e) e$id, ""), "adsl_sub")
  sh <- tfl_sheets_from_yaml(d)
  # the subjects definition carries the report's set in the ARD's sheet ...
  ad <- sh$ard$analysis_data
  expect_identical(ad$population_id[ad$output_id == "T-14-1-1"], "SAF")
  # ... and the report row the other two keys
  rep_row <- sh$table$report[sh$table$report$output_id %in% "T-14-1-1", ]
  expect_identical(rep_row$treatment, "TRT01A")
  tfl_yaml_from_sheets(sh, to, overwrite = TRUE)
  back <- tfl_read_output(file.path(to, "spec", "outputs", "T-14-1-1.yml"))
  expect_identical(back$common, o0$common)
  expect_identical(back$ard$analysis_data, o0$ard$analysis_data)
  txt <- readLines(file.path(to, "spec", "outputs", "T-14-1-1.yml"))
  expect_true(any(grepl("^common:", txt)))
  expect_true(any(grepl("^data:", txt)))
  expect_false(any(grepl("analysis_data|^population:", txt)))
})

test_that("a template report written as a code SPEC: common: kept, the subjects definition says no set", {
  d <- system.file("extdata", "spec-yaml", package = "tflspec")
  s <- tfl_read_study_spec(d)
  o <- tfl_output("T-9", "table", common = list(population = "SAF", treatment = "TRT01A"),
                  template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE")))
  b <- tfl_template_to_spec(o, s)
  expect_identical(b$common$population, "SAF")
  expect_identical(b$common$treatment, "TRT01A")
  ad <- b$ard$analysis_data
  subj <- Filter(function(e) identical(toupper(e$from), "ADSL") && is.null(e$subjects), ad)
  expect_true(length(subj) >= 1L)
  expect_true(all(vapply(subj, function(e) is.null(e$population), NA)))
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  tfl_write_output(b, f)
  txt <- readLines(f)
  expect_true("data:" %in% txt)
  expect_false(any(grepl("analysis_data", txt)))
  expect_identical(tfl_read_output(f)$common, b$common)
})

test_that("common: population_label is the text {OUTPUT_POPULATION} prints", {
  o <- tfl_output("T-1", "table", common = list(population_label = "All subjects"))
  expect_identical(tflspec:::.page_own_tokens(o)$OUTPUT_POPULATION, "All subjects")
  o2 <- tfl_output("T-1", "table", common = list(population = "SAF", population_label = "The safety set"))
  expect_identical(tflspec:::.page_own_tokens(o2)$OUTPUT_POPULATION, "The safety set")
  expect_null(tflspec:::.page_own_tokens(tfl_output("T-1", "table"))$OUTPUT_POPULATION)
})

# (DP-2: place pending, see S1)
test_that("a table's rework steps are the content's rework: item; the old places stop", {
  f <- dr_write(c("id: T-1", "kind: table", "common: {population: SAF}",
                  "data:", "  - {id: adsl_sub, from: ADSL}",
                  "content:", "  - rework: {source: ard, object: ard, steps: [{step: code, code: !r 'data <- data'}]}",
                  "  - table_plan: {cols: TRT01A}"))
  o <- tfl_read_output(f)
  expect_identical(o$data$source, "ard")
  expect_length(o$data$steps, 1L)
  expect_identical(vapply(o$content, function(it) it$fn, ""), "table_plan")
  out <- tempfile(fileext = ".yml")
  on.exit(unlink(c(f, out)), add = TRUE)
  tfl_write_output(o, out)
  txt <- readLines(out)
  expect_true(any(grepl("^  - rework:", txt)))
  expect_false(any(grepl("^  data:", txt)))
  expect_identical(tfl_read_output(out)$data, o$data)
  expect_identical(tfl_read_output(out)$content, o$content)
  # under `ard: data:` or `ard: analysis_data:` a table's rework map stops
  for (ard in list(c("ard:", "  data: {source: ard, object: ard, steps: []}"),
                   c("ard:", "  analysis_data: {source: ard, object: x}"),
                   "data: {source: ard, object: x}")) {
    g <- dr_write(c("id: T-1", "kind: table", ard, "content: []"))
    expect_error(tfl_read_output(g), "write them under `content: rework:`", fixed = TRUE)
    unlink(g)
  }
})
