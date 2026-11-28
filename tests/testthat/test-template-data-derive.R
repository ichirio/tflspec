# A data row's `derive` (a new column, `name = expression`) in a template
# report: the same mutate() as in a report that writes its own `analysis:`, in
# the same place, and the ARD builds with the column.  The map form
# (`derive: {AGEGR2: <R>}`) says the same pieces.

dd_study <- function(derive_lines, template) {
  d <- tempfile()
  dir.create(d)
  dir.create(file.path(d, "data"))
  saveRDS(tflspec::adsl, file.path(d, "data", "adsl.rds"))
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"), full.names = TRUE),
            d, recursive = TRUE)
  writeLines(c("datasets:", "  - dataset: ADSL", "    path: data/adsl.rds", "populations:",
               "  - {id: SAF, dataset: ADSL, where: 'SAFFL == \"Y\"'}"), file.path(d, "data.yml"))
  y <- c("id: T-9", "kind: table", "label: Demographics",
         "common: {population: SAF, treatment: TRT01A}",
         if (template) c("template:", "  name: demog",
                         "  args: {by: TRT01A, continuous: AGE, categorical: [AGEGR2, SEX]}"),
         "data:", "  - id: adsl_sub", "    from: ADSL", "    population: SAF", derive_lines,
         if (!template) c("analysis:", "  analyses:",
                          "    - {id: CAT, method: cards::ard_tabulate, data: adsl_sub, by: [TRT01A], variables: [AGEGR2, SEX]}"))
  writeLines(y, file.path(d, "outputs", "T-9.yml"))
  d
}
dd_mutate <- function(d) {
  s <- tfl_read_study_spec(d)
  code <- tfl_ard_code(s, output_id = "T-9", save = FALSE)
  at <- grep("^adsl_sub <-", code)
  blk <- code[at:min(length(code), at + 6L)]
  list(line = grep("mutate\\(AGEGR2", blk, value = TRUE), blk = blk)
}

test_that("a template report's data-row derive is the own report's mutate(), before keep's select()", {
  expr <- "AGEGR2 = dplyr::if_else(AGE < 65, \"<65\", \">=65\")"
  forms <- list(
    string = paste0("    derive: '", expr, "'"),
    map = "    derive: {AGEGR2: 'dplyr::if_else(AGE < 65, \"<65\", \">=65\")'}")
  for (nm in names(forms)) {
    d_own <- dd_study(forms[[nm]], template = FALSE)
    d_tpl <- dd_study(forms[[nm]], template = TRUE)
    on.exit(unlink(c(d_own, d_tpl), recursive = TRUE), add = TRUE)
    own <- dd_mutate(d_own)
    tpl <- dd_mutate(d_tpl)
    expect_length(own$line, 1L)
    expect_identical(tpl$line, own$line)
    expect_match(own$line, "mutate(AGEGR2 = if_else(AGE < 65", fixed = TRUE)
  }
  # after the where (the analysis set's filter), before keep's select()
  d <- dd_study(c(forms$string, "    keep: [AGEGR2, AGE, TRT01A]"), template = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  blk <- dd_mutate(d)$blk
  at <- vapply(c("filter\\(SAFFL", "mutate\\(AGEGR2", "select\\("),
               function(p) grep(p, blk)[1L], 0L)
  expect_false(anyNA(at))
  expect_identical(order(at), 1:3)
})

test_that("a template report's ARD builds with the derived column", {
  skip_if_not_installed("cards")
  d <- dd_study("    derive: {AGEGR2: 'dplyr::if_else(AGE < 65, \"<65\", \">=65\")'}", template = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  old <- setwd(d)
  on.exit(setwd(old), add = TRUE)
  ard <- suppressMessages(tfl_build_ard(tfl_read_study_spec("."), ".", output_id = "T-9", save = FALSE))
  expect_true("AGEGR2" %in% unlist(ard$variable))
})

test_that("a report's data row merges into the template's row of its id, key by key", {
  # (1) the template's subset (its row's where) and the report's derive: both
  tp <- c("template:", "  name: demog",
          "  args: {by: TRT01A, continuous: AGE, categorical: [AGEGR2], subset: 'SEX == \"F\"'}")
  mk <- function(rows) {
    d <- dd_study("", template = TRUE)
    y <- readLines(file.path(d, "outputs", "T-9.yml"))
    y <- y[seq_len(match("template:", y) - 1L)]
    writeLines(c(y, tp, "data:", rows), file.path(d, "outputs", "T-9.yml"))
    d
  }
  blk <- function(d) dd_mutate(d)$blk
  d1 <- mk(c("  - id: adsl_sub", "    derive: 'AGEGR2 = AGE'"))
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)
  b1 <- blk(d1)
  expect_true(any(grepl("filter(SEX == \"F\")", b1, fixed = TRUE)))
  expect_true(any(grepl("mutate(AGEGR2 = AGE)", b1, fixed = TRUE)))
  # (3) a key the template has, written by the report: the report's wins
  d3 <- mk(c("  - id: adsl_sub", "    where: !r 'AGE > 30'", "    derive: 'AGEGR2 = AGE'"))
  on.exit(unlink(d3, recursive = TRUE), add = TRUE)
  b3 <- blk(d3)
  expect_true(any(grepl("filter(AGE > 30)", b3, fixed = TRUE)))
  expect_false(any(grepl("SEX == \"F\"", b3)))
  expect_true(any(grepl("mutate(AGEGR2 = AGE)", b3, fixed = TRUE)))
  # (2) the template's derive and the report's where: both; a row the
  # template lacks is added; analyses of an id are replaced whole
  tpl <- list(analysis_data = list(list(id = "a", from = "ADSL", population = "SAF", derive = "X = 1"),
                                   list(id = "b", from = "a")),
              analyses = list(list(id = "n", method = "m1")))
  own <- list(analysis_data = list(list(id = "a", where = "AGE > 30"), list(id = "c", from = "a")),
              analyses = list(list(id = "n", variables = "v")))
  m <- tflspec:::.merge_ard(tpl, own)
  expect_identical(m$analysis_data[[1L]],
                   list(id = "a", from = "ADSL", population = "SAF", derive = "X = 1", where = "AGE > 30"))
  expect_identical(vapply(m$analysis_data, `[[`, "", "id"), c("a", "b", "c"))
  expect_identical(m$analyses[[1L]], list(id = "n", variables = "v"))
})
