# tfl_fill_widths(): a study's reports say their column widths

test_that("a table with no widths gets them written, with an assumption; one with widths is left", {
  skip_if_not_installed("rtfreporter")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "adam"))
  saveRDS(tflspec::adsl, file.path(d, "adam", "adsl.rds"))
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "ONCO-01", overwrite = TRUE))
  mk <- function(id, extra = list()) {
    o <- tfl_output(id, "table", population = "SAF")
    o$extends <- list("defaults")
    o$ard <- list(analyses = list(list(id = "AGE", method = "continuous", dataset = "ADSL",
                                       population = "SAF", by = list("TRT01A"),
                                       variables = list("AGE"))))
    o$data <- list(source = "ard")
    o$content <- c(list(tfl_item("table_plan", list(cols = "TRT01A",
                                                    rows = list(group = "variable"))),
                        tfl_item("plan_cells", list(continuous = list(
                          n = "{N:.0f}", "Mean (SD)" = "{mean:.1f} ({sd:.2f})")))), extra)
    tfl_write_output(o, file.path(d, "spec", "outputs", paste0(id, ".yml")))
  }
  mk("T-1")
  mk("T-2", list(tfl_item("plan_columns", list(widths = list(row_label = 2, .values = 1)))))
  before <- readLines(file.path(d, "spec", "outputs", "T-2.yml"))
  set <- suppressMessages(tfl_fill_widths(d))
  expect_identical(set, "T-1")
  y <- yaml::read_yaml(file.path(d, "spec", "outputs", "T-1.yml"))
  pc <- Filter(function(it) identical(names(it), "plan_columns"), y$content)
  expect_length(pc, 1L)
  w <- pc[[1L]]$plan_columns$widths
  expect_true(length(w) >= 2L && all(unlist(w) > 0))
  # the one with widths: as it was
  expect_identical(readLines(file.path(d, "spec", "outputs", "T-2.yml")), before)
  # its assumption, once; no AI file made
  ass <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  wa <- Filter(function(x) identical(x$item, "widths"), ass)
  expect_identical(vapply(wa, `[[`, "", "output_id"), "T-1")
  expect_false(file.exists(file.path(d, "ai", "widths.yml")))
  # again: nothing to do
  expect_length(suppressMessages(tfl_fill_widths(d)), 0L)
  expect_error(tfl_fill_widths(d, output_id = "T-9"), "no report 'T-9'")
})

test_that("a table whose widths come from the study's defaults is left alone, quietly", {
  skip_if_not_installed("rtfreporter")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "adam"))
  saveRDS(tflspec::adsl, file.path(d, "adam", "adsl.rds"))
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "ONCO-01", overwrite = TRUE))
  # the defaults give every table its widths
  f <- file.path(d, "spec", "defaults.yml")
  y <- readLines(f, encoding = "UTF-8")
  at <- grep("^  - plan_blanks:", y)
  writeLines(append(y, "  - plan_columns: {widths: {row_label: 2, .values: 1}}", after = at), f)
  o <- tfl_output("T-3", "table", population = "SAF")
  o$extends <- list("defaults")
  o$ard <- list(analyses = list(list(id = "AGE", method = "continuous", dataset = "ADSL",
                                     population = "SAF", by = list("TRT01A"),
                                     variables = list("AGE"))))
  o$data <- list(source = "ard")
  o$content <- list(tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable"))),
                    tfl_item("plan_cells", list(continuous = list(n = "{N:.0f}"))))
  p <- file.path(d, "spec", "outputs", "T-3.yml")
  tfl_write_output(o, p)
  before <- readLines(p)
  # its file says no widths, the resolved report has them: nothing to do, no message
  expect_no_message(set <- tfl_fill_widths(d, output_id = "T-3"))
  expect_length(set, 0L)
  expect_identical(readLines(p), before)
  expect_false(file.exists(file.path(d, "spec", "_assumptions.yml")) &&
                 any(vapply(yaml::read_yaml(file.path(d, "spec", "_assumptions.yml")),
                            function(x) identical(x$item, "widths"), NA)))
})

# ---- a listing's columns fitted to the study's data (rtfreporter's fit_listing_widths()) ----

# a study with ADSL and ADAE (xpt) when `adam`, none when not
fw_study <- function(adam = TRUE, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "adam"))
  # (no ADaM yet: ADSL only, the study needs one file)
  for (x in if (adam) c("adsl", "adae") else "adsl") {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "ONCO-01", overwrite = TRUE))
  d
}
fw_col <- function(v, label, ...) {
  tfl_call("listing_col", c(list(vars = as.list(v), label = label), list(...)))
}
fw_cols <- function(...) {
  list(fw_col("USUBJID", "Subject", ...), fw_col("TRT01A", "Treatment"),
       fw_col("AEDECOD", "Preferred term"), fw_col("ASTDT", "Start date"),
       fw_col("AENDT", "End date"), fw_col("AESEV", "Severity"),
       fw_col("AEREL", "Relationship to study drug"))
}
fw_code <- function(id, cols, var = "USUBJID") {
  tfl_output(id, "listing", label = id, extends = "defaults",
             data = list(source = "dataset", dataset = "ADAE", population = "SAF",
                         where = tfl_r("AESER == \"Y\""), sort = list("TRT01A", var)),
             content = list(tfl_item("plan_listing", list(type = "multiline", cols = cols))))
}
fw_tpl <- function(id, columns) {
  tfl_output(id, "listing", label = id, extends = "defaults", population = "SAF",
             template = list(name = "listing", args = list(
               data = "ADAE", columns = columns, where = "AESER == \"Y\"",
               sort = c("TRT01A", "USUBJID"))))
}
fw_tpl_cols <- function(width = NULL) {
  cl <- function(v, label, ...) list(vars = v, label = label, ...)
  list(cl("USUBJID", "Subject", width = width), cl("TRT01A", "Treatment"),
       cl("AEDECOD", "Preferred term"), cl("ASTDT", "Start date"), cl("AENDT", "End date"),
       cl("AESEV", "Severity"), cl("AEREL", "Relationship to study drug"))
}
fw_put <- function(d, o) tfl_write_output(o, file.path(d, "spec", "outputs", paste0(o$id, ".yml")))
fw_file <- function(d, id) file.path(d, "spec", "outputs", paste0(id, ".yml"))
# the widths a code listing's file says / a template listing's
fw_code_w <- function(d, id) {
  o <- tfl_read_output(fw_file(d, id))
  vapply(o$content[[1L]]$args$cols, function(cl) cl$args$width %||% NA_integer_, 1L)
}
fw_tpl_w <- function(d, id) {
  o <- tfl_read_output(fw_file(d, id))
  vapply(o$template$args$columns, function(cl) as.integer(cl$width %||% NA_integer_), 1L)
}
fw_skip <- function() {
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("haven")
  skip_if_not(is.function(tryCatch(getExportedValue("rtfreporter", "fit_listing_widths"),
                                   error = function(e) NULL)))
}

test_that("a code listing's and the listing template's widths are fitted to the data, as integers in the SPEC", {
  fw_skip()
  d <- fw_study()
  fw_put(d, fw_code("L-1", fw_cols()))
  fw_put(d, fw_tpl("L-2", fw_tpl_cols()))
  set <- suppressMessages(tfl_fill_widths(d))
  expect_setequal(set, c("L-1", "L-2"))
  wc <- fw_code_w(d, "L-1")
  wt <- fw_tpl_w(d, "L-2")
  expect_false(anyNA(wc))
  expect_false(anyNA(wt))
  # the same listing, the same data and page: the same widths, by either route
  expect_identical(wc, wt)
  # the fitted number is not the first guess, and the id fits on one line
  guess <- vapply(.ai_listing_widths(fw_code("L-9", fw_cols()))$content[[1L]]$args$cols,
                  function(cl) cl$args$width, 1L)
  expect_false(identical(wc, guess))
  expect_gte(wc[1L], max(nchar(tflspec::adae$USUBJID)))
  # whole numbers in the file, and they read back the same
  txt <- readLines(fw_file(d, "L-1"))
  expect_true(any(grepl("width: [0-9]+$", txt)))
  expect_false(any(grepl("width: [0-9]+\\.", txt)))
  expect_true(all(vapply(tfl_read_output(fw_file(d, "L-1"))$content[[1L]]$args$cols,
                         function(cl) is.integer(cl$args$width), NA)))
  o <- tfl_read_output(fw_file(d, "L-1"))
  tfl_write_output(o, fw_file(d, "L-1"))
  expect_identical(readLines(fw_file(d, "L-1")), txt)
  expect_identical(fw_code_w(d, "L-1"), wc)
  # the template's report is still the template: its columns carry the widths
  y <- yaml::read_yaml(fw_file(d, "L-2"))
  expect_identical(y$template$name, "listing")
  expect_length(y$content, 0L)
  expect_type(y$template$args$columns[[1L]]$width, "integer")
  # the assumption says which: fitted
  ass <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  wa <- Filter(function(x) identical(x$item, "widths"), ass)
  expect_setequal(vapply(wa, `[[`, "", "value"), "fitted widths")
  # again: nothing to do
  expect_length(suppressMessages(tfl_fill_widths(d)), 0L)
})

test_that("a width a person set stays; the fit gives the other columns theirs", {
  fw_skip()
  d <- fw_study()
  cols <- fw_cols()
  cols[[3L]]$args$width <- 31L
  fw_put(d, fw_code("L-1", cols))
  tc <- fw_tpl_cols()
  tc[[3L]]$width <- 31L
  fw_put(d, fw_tpl("L-2", tc))
  # a listing with every width set is not touched, byte for byte
  full <- lapply(fw_cols(), function(cl) { cl$args$width <- 14L; cl })
  fw_put(d, fw_code("L-3", full))
  before <- readLines(fw_file(d, "L-3"))
  set <- suppressMessages(tfl_fill_widths(d))
  expect_setequal(set, c("L-1", "L-2"))
  expect_identical(readLines(fw_file(d, "L-3")), before)
  wc <- fw_code_w(d, "L-1")
  expect_identical(wc[3L], 31L)
  expect_false(anyNA(wc))
  expect_identical(fw_tpl_w(d, "L-2"), wc)
  # the others were fitted around the 31: the listing still fits the page
  expect_lte(sum(wc), 126L)
  expect_gte(wc[1L], max(nchar(tflspec::adae$USUBJID)))
})

test_that("with no data the code listing gets the first guess as before and the template is left alone", {
  fw_skip()
  d <- fw_study(adam = FALSE)
  fw_put(d, fw_code("L-1", fw_cols()))
  fw_put(d, fw_tpl("L-2", fw_tpl_cols()))
  before <- readLines(fw_file(d, "L-2"))
  set <- suppressMessages(tfl_fill_widths(d))
  expect_identical(set, "L-1")
  guess <- .ai_listing_widths(fw_code("L-1", fw_cols()))
  expect_identical(fw_code_w(d, "L-1"),
                   vapply(guess$content[[1L]]$args$cols, function(cl) cl$args$width, 1L))
  expect_identical(fw_code_w(d, "L-1")[1L], 12L)
  expect_identical(readLines(fw_file(d, "L-2")), before)
  ass <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  wa <- Filter(function(x) identical(x$item, "widths"), ass)
  expect_identical(vapply(wa, `[[`, "", "output_id"), "L-1")
  expect_identical(vapply(wa, `[[`, "", "value"), "default widths")
})

test_that("a column the data lacks falls back to the guess, without an error", {
  fw_skip()
  d <- fw_study()
  cols <- c(fw_cols(), list(fw_col("NOSUCHVAR", "Not there")))
  fw_put(d, fw_code("L-1", cols))
  tc <- c(fw_tpl_cols(), list(list(vars = "NOSUCHVAR", label = "Not there")))
  fw_put(d, fw_tpl("L-2", tc))
  before <- readLines(fw_file(d, "L-2"))
  expect_no_error(set <- suppressMessages(tfl_fill_widths(d)))
  expect_identical(set, "L-1")
  expect_identical(fw_code_w(d, "L-1")[c(1L, 8L)], c(12L, 20L))
  expect_identical(readLines(fw_file(d, "L-2")), before)
  ass <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  wa <- Filter(function(x) identical(x$item, "widths"), ass)
  expect_identical(vapply(wa, `[[`, "", "value"), "default widths")
})

test_that("a dataset that cannot be read falls back to the guess too", {
  fw_skip()
  d <- fw_study()
  writeLines("not an xpt file", file.path(d, "adam", "adae.xpt"))
  fw_put(d, fw_code("L-1", fw_cols()))
  expect_no_error(set <- suppressMessages(tfl_fill_widths(d)))
  expect_identical(set, "L-1")
  expect_identical(fw_code_w(d, "L-1")[1L], 12L)
})

test_that("above 5,000 records the fit is made on an even-spaced sample, and gives the full fit's widths", {
  fw_skip()
  set.seed(5)
  adae <- tflspec:::.ai_chr(tflspec::adae)
  big <- adae[sample(nrow(adae), 20000L, replace = TRUE), ]
  rownames(big) <- NULL
  # a short data set is not sampled
  expect_identical(tflspec:::.ai_fit_sample(adae), adae)
  s <- tflspec:::.ai_fit_sample(big)
  expect_identical(nrow(s), 5000L)
  # no random numbers: the same rows every time
  expect_identical(tflspec:::.ai_fit_sample(big), s)
  vs <- c("USUBJID", "TRT01A", "AEDECOD", "ASTDT", "AENDT", "AESEV", "AEREL")
  spec <- rtfreporter::listing_spec(lapply(vs, function(v) rtfreporter::listing_col(v, label = v)))
  w <- function(d) vapply(rtfreporter::fit_listing_widths(d, spec)$cols, function(cl) cl$width, 1L)
  expect_identical(w(s), w(big))
  expect_identical(w(tflspec:::.ai_fit_sample(big)), w(s))
})
