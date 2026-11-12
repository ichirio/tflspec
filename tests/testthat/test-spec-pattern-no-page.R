# pattern (a marker row) and the pattern's code lists (on the report), so
# tfl_table_spec() and the save work and nothing in the output changes (#662).
# Package fictional data (adsl, adlb); a study per case.

pnp_study <- function(pattern = NULL, page = NULL, defaults = NULL, own = NULL, extends = NULL,
                      env = parent.frame()) {
  skip_if_not_installed("haven")
  d <- tempfile("pnp-")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  withr::defer(unlink(d, recursive = TRUE), envir = env)
  for (x in c("adsl", "adlb")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)), file.path(d, "adam", paste0(x, ".xpt")))
  }
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "DMY-001"))
  spec <- file.path(d, "spec")
  if (length(defaults)) cat(c("codelists:", paste0("  ", defaults), ""), sep = "\n",
                            file = file.path(spec, "defaults.yml"), append = TRUE)
  if (!is.null(pattern) || !is.null(page)) {
    dir.create(file.path(spec, "patterns"), showWarnings = FALSE)
    writeLines(c(if (length(pattern)) c("codelists:", paste0("  ", pattern)), page),
               file.path(spec, "patterns", "pp.yml"))
  }
  if (is.null(extends)) extends <- if (!is.null(pattern) || !is.null(page)) "[patterns/pp, defaults]" else "[defaults]"
  writeLines(c("id: T-A", "kind: table", "common: {population: SAF}", paste0("extends: ", extends), own,
               "ard:", "  analyses:", "    - id: A1", "      method: categorical",
               "      population: SAF", "      by: [ARM]", "      variables: [SEX]",
               "content:", "  - table_plan: {cols: ARM}"),
             file.path(spec, "outputs", "T-A.yml"))
  d
}

# the study written from its sheets into a copy of its folder
pnp_round <- function(d, env = parent.frame()) {
  sh <- tfl_sheets_from_yaml(d)
  d2 <- tempfile("pnp-rt-")
  dir.create(d2)
  withr::defer(unlink(d2, recursive = TRUE), envir = env)
  file.copy(file.path(d, "adam"), d2, recursive = TRUE)
  suppressMessages(tfl_yaml_from_sheets(sh, d2, overwrite = TRUE))
  list(sheets = sh, dir = d2)
}

pnp_code <- function(d) {
  st <- tfl_read_study_spec(d)
  out <- tfl_resolve_output(tfl_read_outputs(d)[["T-A"]], st)
  list(ard = paste(tfl_ard_code(st, output_id = "T-A", save = FALSE), collapse = "\n"),
       report = paste(tfl_report_code(out, study = st), collapse = "\n"))
}

# the round trip of a study; `same`: the code of the study and of its
# copy is identical
pnp_check <- function(d, same = TRUE, env = parent.frame()) {
  rt <- pnp_round(d, env)
  expect_no_error(tfl_table_spec(rt$sheets$table))
  expect_true("pp" %in% names(tfl_read_study_spec(rt$dir)$patterns))
  expect_true("patterns/pp" %in% tfl_read_outputs(rt$dir)[["T-A"]]$extends)
  if (same) {
    a <- pnp_code(d)
    b <- pnp_code(rt$dir)
    expect_identical(b$ard, a$ard)
    expect_identical(b$report, a$report)
  }
  rt
}

pnp_pat <- "SEX: [{value: M, label: PatM}, {value: F, label: PatF}]"
pnp_def <- "SEX: [{value: M, label: DefM}, {value: F, label: DefF}]"
pnp_notes <- function(sh) sh$not_converted[sh$not_converted$part == "codelists", ]

test_that("`page: {}`: the pattern keeps its place and the output does not change (#662)", {
  skip_on_cran()
  d <- pnp_study(page = "page: {}")
  rt <- pnp_check(d)
  r <- rt$sheets$table$report
  expect_identical(r$output_id[!is.na(r$output_id)], c("T-A", "@pp"))
  expect_true(all(is.na(r[r$output_id %in% "@pp", setdiff(names(r), "output_id")])))
  expect_identical(nrow(pnp_notes(rt$sheets)), 0L)
})

test_that("an orientation only keeps the pattern, with no marker (#662)", {
  skip_on_cran()
  d <- pnp_study(page = c("page:", "  rtf_page: {orientation: landscape}"))
  rt <- pnp_check(d)
  r <- rt$sheets$table$report
  expect_identical(r$output_id[!is.na(r$output_id)], "T-A")
  expect_true("@pp" %in% rt$sheets$table$page$output_id)
  expect_identical(tfl_read_study_spec(rt$dir)$patterns$pp$page$rtf_page$orientation, "landscape")
})

test_that("a pattern with code lists only: the sheets work, and its lists stay in YAML, said (#662)", {
  skip_on_cran()
  d <- pnp_study(pattern = pnp_pat)
  # (the stated limit: the lists have no sheet, so a study written from the
  # sheets into a new folder has not got them; the note says so, and the
  # output is not asserted identical there)
  rt <- pnp_check(d, same = FALSE)
  expect_identical(rt$sheets$table$report$output_id[!is.na(rt$sheets$table$report$output_id)], c("T-A", "@pp"))
  n <- pnp_notes(rt$sheets)
  expect_identical(n$output, "@pp")
  expect_match(n$reason, "stay in YAML")
  expect_identical(nrow(rt$sheets$table$codelists), 0L)
})

test_that("the defaults' code lists are said to stay in YAML (#662)", {
  skip_on_cran()
  d <- pnp_study(defaults = pnp_def)
  sh <- tfl_sheets_from_yaml(d)
  n <- pnp_notes(sh)
  expect_identical(nrow(n), 1L)
  expect_true(is.na(n$output))
  expect_match(n$reason, "defaults.yml.*stay in YAML")
  # (the stated limit, as above)
  d2 <- tempfile("pnp-rt-")
  dir.create(d2)
  withr::defer(unlink(d2, recursive = TRUE))
  file.copy(file.path(d, "adam"), d2, recursive = TRUE)
  suppressMessages(tfl_yaml_from_sheets(sh, d2, overwrite = TRUE))
  expect_length(tfl_read_study_spec(d2)$defaults$codelists, 0L)
})

test_that("a report's own list is a row, and says nothing (#662)", {
  skip_on_cran()
  own <- c("codelists:", "  SEX: [{value: M, label: OwnM}, {value: F, label: OwnF}]")
  d <- pnp_study(page = "page: {}", own = own)
  rt <- pnp_check(d)
  expect_identical(rt$sheets$table$codelists$label, c("OwnM", "OwnF"))
  expect_identical(nrow(pnp_notes(rt$sheets)), 0L)
  expect_identical(names(tfl_read_outputs(rt$dir)[["T-A"]]$codelists), "SEX")
})

test_that("the marker row changes no default: the report's rows come out the same (#662)", {
  skip_on_cran()
  d <- pnp_study(page = "page: {}")
  sp <- tfl_table_spec(tfl_sheets_from_yaml(d)$table)
  r <- sp$report
  own <- r[!is.na(r$output_id) & r$output_id == "T-A", , drop = FALSE]
  marker <- r[!is.na(r$output_id) & r$output_id == "@pp", , drop = FALSE]
  expect_identical(nrow(marker), 1L)
  over <- tflspec:::.ard_spec_over_pattern(r[is.na(r$output_id), , drop = FALSE]
                                           , marker, "report", "T-A")
  # (the default row, then the report's row: its cells as the marker's blanks
  # leave them)
  expect_identical(over[nrow(over), setdiff(names(over), "output_id")],
                   marker[1L, setdiff(names(marker), "output_id")], ignore_attr = TRUE)
  one <- tflspec:::.ard_spec_scope(sp, "T-A")$report
  expect_identical(one$output_id, "T-A")
  expect_identical(one$type[1L], "table")
})

test_that("a report naming a pattern that does not exist still stops (#662)", {
  skip_on_cran()
  d <- pnp_study(page = "page: {}")
  f <- file.path(d, "spec", "outputs", "T-A.yml")
  writeLines(sub("patterns/pp", "patterns/pq", readLines(f)), f)
  sh <- suppressWarnings(tfl_sheets_from_yaml(d))
  expect_error(tfl_table_spec(sh$table), "no page pattern of that name")
})

test_that("a pattern with titles gets no marker row; a study with no layer lists no note (#662)", {
  skip_on_cran()
  d <- pnp_study(page = c("page:", "  titles:", "    '1': {center: A title}"))
  sh <- tfl_sheets_from_yaml(d)
  r <- sh$table$report
  expect_identical(r$output_id[!is.na(r$output_id)], "T-A")
  expect_true("@pp" %in% sh$table$titles$output_id)
  expect_no_error(tfl_table_spec(sh$table))
  expect_identical(nrow(pnp_notes(sh)), 0L)
  rt <- pnp_check(d)
  expect_identical(rt$sheets$table$titles$output_id, sh$table$titles$output_id)
})
