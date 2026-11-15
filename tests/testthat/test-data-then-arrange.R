# A report's data rows: `arrange:` (the sort, after derive and before keep)
# and `then:` (R added at the end of the row's pipe)

ta_df <- function(...) as.data.frame(list(...), stringsAsFactors = FALSE)

ta_spec <- function(rows) {
  tfl_ard_spec(list(
    study = ta_df(key = "id", value = "USUBJID"),
    datasets = ta_df(dataset = c("ADSL", "ADAE"), path = c("adsl.rds", "adae.rds")),
    populations = ta_df(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analysis_data = rows,
    analyses = ta_df(output_id = "T1", analysis_id = "AE", method = "cards::ard_tabulate",
                     data = "adae_x", variables = "AEDECOD")))
}

ta_rows <- function(...) {
  base <- ta_df(output_id = "T1", data_id = "adae_x", from = "ADAE")
  extra <- list(...)
  for (k in names(extra)) base[[k]] <- extra[[k]]
  base
}

# the code of the data adae_x, as one text, from a spec's program
ta_code_of <- function(x, id = "adae_x") {
  code <- tfl_ard_code(x, output_id = "T1", save = FALSE, part = "body")
  i <- grep(paste0("^", id, " <-"), code)
  expect_length(i, 1L)
  code[i]
}

test_that("the code of a data row: filter, derive, arrange, keep, distinct, then", {
  x <- ta_spec(ta_rows(
    where = "TRTEMFL == \"Y\"", derive = "SERF = AESER == \"Y\"",
    arrange = "USUBJID | desc(ASTDT)", keep = "AEDECOD | SERF",
    distinct = "USUBJID", then = "dplyr::mutate(N = 1) |> dplyr::filter(N > 0)"))
  txt <- strsplit(ta_code_of(x), "\n", fixed = TRUE)[[1L]]
  at <- function(p) grep(p, txt, fixed = TRUE)
  expect_length(at("arrange(USUBJID, desc(ASTDT))"), 1L)
  expect_true(at("filter(TRTEMFL") < at("mutate(SERF"))
  expect_true(at("mutate(SERF") < at("arrange("))
  expect_true(at("arrange(") < at("select("))
  expect_true(at("select(") < at("distinct("))
  expect_true(at("distinct(") < at("mutate(N = 1)"))
  expect_true(at("mutate(N = 1)") < at("filter(N > 0)"))
  # the last two lines: the then's steps, one a line, the pipe's own
  n <- length(txt)
  expect_match(txt[n - 1L], "mutate\\(N = 1\\) \\|>$")
  expect_match(txt[n], "^  (dplyr::)?filter\\(N > 0\\)$")
})

test_that("a data row's then: alone, one step and a pipe of steps", {
  x <- ta_spec(ta_rows(then = "dplyr::mutate(AVAL = round(AVALN, 1)) |> dplyr::filter(AVAL > 0)"))
  txt <- ta_code_of(x)
  expect_match(txt, "AVAL = round\\(AVALN, 1\\)\\) \\|>\n  (dplyr::)?filter\\(AVAL > 0\\)$")
  # a pipe's bar inside a string or a bracket is not a pipe
  x <- ta_spec(ta_rows(then = "dplyr::filter(grepl(\"a |> b\", AEDECOD))"))
  expect_match(ta_code_of(x), "a |> b", fixed = TRUE)
  expect_identical(.then_steps("f(x) |> g(\"|>\") |> h()"), c("f(x)", "g(\"|>\")", "h()"))
  expect_identical(.then_steps(NA), character())
})

test_that("then: with code: is an error naming the row; so is arrange: with code:", {
  rows <- ta_rows(code = "dplyr::filter(adae, TRTEMFL == \"Y\")",
                  then = "dplyr::filter(AGE > 1)")
  expect_error(ta_spec(rows), "adae_x.*`code` makes the data itself.*`then`")
  rows <- ta_rows(code = "dplyr::filter(adae, TRTEMFL == \"Y\")", arrange = "USUBJID")
  expect_error(ta_spec(rows), "adae_x.*`code` makes the data itself.*`arrange`")
})

test_that("a bad arrange element is an error naming the row", {
  expect_error(ta_spec(ta_rows(arrange = "USUBJID | desc(ASTDT, 2)")),
               "adae_x.*`arrange` element .*desc\\(ASTDT, 2\\).* not a variable name or desc\\(name\\)")
  expect_error(ta_spec(ta_rows(arrange = "AGE + 1")), "adae_x.*`arrange` element")
  # names, desc(name) and a name with a dot are fine
  expect_no_error(ta_spec(ta_rows(arrange = "USUBJID | desc( ASTDT ) | A.B")))
  expect_error(ta_spec(ta_rows(then = "dplyr::filter(")), "adae_x.*`then` does not read as R")
})

test_that("arrange: sorts a built data, before keep and distinct; then: is applied", {
  skip_if_not_installed("dplyr")
  x <- ta_spec(ta_rows(arrange = "USUBJID | desc(AESEQ)", keep = "AEDECOD",
                       then = "dplyr::mutate(N = dplyr::row_number()) |> dplyr::filter(N <= 20)"))
  env <- new.env(parent = globalenv())
  assign("adae", tflspec::adae, envir = env)
  eval(parse(text = c("library(dplyr)", ta_code_of(x))), envir = env)
  got <- env$adae_x
  ae <- tflspec::adae
  want <- ae[order(ae$USUBJID, -ae$AESEQ), ]
  # the sort variable AESEQ is not kept, USUBJID is
  expect_identical(names(got), c("USUBJID", "AEDECOD", "N"))
  expect_identical(nrow(got), 20L)
  expect_identical(as.character(got$USUBJID), as.character(want$USUBJID[1:20]))
  expect_identical(as.character(got$AEDECOD), as.character(want$AEDECOD[1:20]))
  # distinct keeps the first row in the sort: the last event of each subject
  x <- ta_spec(ta_rows(arrange = "USUBJID | desc(AESEQ)", distinct = "USUBJID"))
  env2 <- new.env(parent = globalenv())
  assign("adae", tflspec::adae, envir = env2)
  eval(parse(text = c("library(dplyr)", ta_code_of(x))), envir = env2)
  got <- env2$adae_x
  top <- tapply(ae$AESEQ, ae$USUBJID, max)
  expect_identical(nrow(got), length(top))
  expect_identical(as.numeric(got$AESEQ), as.numeric(top[as.character(got$USUBJID)]))
})

ta_yaml <- c(
  "id: T-9", "kind: table", "data:",
  "  - id: adsl_sub", "    from: ADSL", "    population: SAF",
  "  - id: adae_x", "    from: ADAE", "    subjects: adsl_sub",
  "    where: !r TRTEMFL == \"Y\"", "    derive: !r SERF = AESER == \"Y\"",
  "    arrange: [USUBJID, desc(ASTDT)]", "    keep: [AEDECOD, SERF]",
  "    distinct: [USUBJID]",
  "    then: !r dplyr::mutate(N = 1) |> dplyr::filter(N > 0)",
  "ard:", "  analyses:", "    - id: AE", "      method: cards::ard_tabulate",
  "      data: adae_x", "      variables: [AEDECOD]",
  "content:", "  - table_plan: {cols: TRT01A}")

test_that("the YAML round trip of arrange: and then:", {
  f <- tempfile(fileext = ".yml")
  f2 <- tempfile(fileext = ".yml")
  on.exit(unlink(c(f, f2)), add = TRUE)
  writeLines(ta_yaml, f)
  o <- tfl_read_output(f)
  row <- o$ard$analysis_data[[2L]]
  expect_identical(as.character(row$arrange), c("USUBJID", "desc(ASTDT)"))
  expect_s3_class(row$then, "tfl_r")
  tfl_write_output(o, f2)
  o2 <- tfl_read_output(f2)
  expect_identical(o2$ard$analysis_data, o$ard$analysis_data)
  txt <- readLines(f2)
  at <- function(p) grep(p, txt, fixed = TRUE)[1L]
  expect_true(at("derive:") < at("arrange:"))
  expect_true(at("arrange:") < at("keep:"))
  expect_true(at("distinct:") < at("then:"))
  # a one-name arrange is still a sequence
  o$ard$analysis_data[[2L]]$arrange <- "USUBJID"
  tfl_write_output(o, f2)
  expect_true(any(grepl("^ *arrange:\\s*$", readLines(f2))) ||
                any(grepl("arrange: \\[USUBJID\\]", readLines(f2))))
  expect_identical(as.character(tfl_read_output(f2)$ard$analysis_data[[2L]]$arrange), "USUBJID")
})

test_that("the sheets round trip of arrange: and then:", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  d <- tempfile("ta-study-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  file.copy(list.files(src, full.names = TRUE), d, recursive = TRUE)
  writeLines(ta_yaml, file.path(d, "outputs", "T-9.yml"))
  f <- file.path(d, "view.xlsx")
  tfl_write_spec_xlsx(d, f)
  ad <- as.data.frame(readxl::read_excel(f, "analysis_data", col_types = "text"))
  expect_true(all(c("arrange", "then") %in% names(ad)))
  expect_identical(ad$arrange[ad$id %in% "adae_x"], "USUBJID | desc(ASTDT)")
  expect_identical(ad$then[ad$id %in% "adae_x"], "dplyr::mutate(N = 1) |> dplyr::filter(N > 0)")
  # the same workbook taken back: nothing changes
  r <- tfl_import_spec_xlsx(d, f)
  expect_identical(nrow(r$changes), 0L, info = paste(utils::capture.output(print(r$changes)), collapse = "
"))
  # an edited cell comes back as the row's key
  wb <- openxlsx::loadWorkbook(f)
  x <- openxlsx::read.xlsx(wb, "analysis_data")
  x[] <- lapply(x, as.character)
  x$arrange[x$id %in% "adae_x"] <- "AEDECOD | USUBJID"
  openxlsx::removeWorksheet(wb, "analysis_data")
  openxlsx::addWorksheet(wb, "analysis_data")
  openxlsx::writeData(wb, "analysis_data", x, keepNA = FALSE)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  tfl_import_spec_xlsx(d, f)
  o <- tfl_read_outputs(d)[["T-9"]]
  row <- o$ard$analysis_data[[2L]]
  expect_identical(as.character(row$arrange), c("AEDECOD", "USUBJID"))
  expect_s3_class(row$then, "tfl_r")
})

test_that("the schema and the AI kit list the two keys", {
  expect_true(all(c("arrange", "then") %in% .ard_spec_sheets$analysis_data))
  sh <- .ard_spec_sheets$analysis_data
  expect_identical(sh[match("derive", sh) + 1L], "arrange")
  expect_identical(sh[length(sh)], "then")
  sch <- paste(readLines(system.file("spec", "schema.yml", package = "tflspec")), collapse = "\n")
  expect_match(sch, "- column: arrange", fixed = TRUE)
  expect_match(sch, "- column: then", fixed = TRUE)
  g <- paste(readLines(system.file("ai", "agent", "instructions-spec.md", package = "tflspec")), collapse = "\n")
  expect_match(g, "`derive`, `arrange`, `keep`, `distinct`, `code`, `then`", fixed = TRUE)
})
