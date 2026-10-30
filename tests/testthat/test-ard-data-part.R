# A figure or a listing reads the data a report defines (ard.analysis_data)
# as a table's analyses do: tfl_ard_code(part = "data") writes the report's
# data part; a figure's read step names one (`data:`), a listing's row too
# (`data`, or `population_id` for the subjects of an analysis set)

dp_spec <- function() {
  tfl_ard_spec(list(
    datasets = data.frame(dataset = c("ADSL", "ADAE"), path = c("adsl.rds", "adae.rds")),
    populations = data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analysis_data = data.frame(output_id = "L-1", data_id = c("adae_saf", "adae_ser"),
                               from = c("ADAE", "adae_saf"), population_id = c("SAF", NA),
                               where = c(NA, "AESER == \"Y\"")),
    analyses = data.frame(output_id = "T-1", analysis_id = "A1", method = "categorical",
                          dataset = "ADSL", population_id = "SAF", by = "TRT01A",
                          variables = "SEX")))
}

test_that("tfl_ard_code(part = \"data\"): a report's datasets, analysis sets and analysis data", {
  x <- dp_spec()
  code <- tfl_ard_code(x, "L-1", part = "data")
  expect_true(any(grepl("^adae <- readRDS", code)))
  expect_true(any(grepl("^pop_saf <- ", code)))
  expect_true(any(grepl("^adae_saf <- ", code)))
  expect_true(any(grepl("^adae_ser <- ", code)))
  expect_false(any(grepl("ard_|save_ard|report_id", code)))
  # the data named, with those it is made from
  one <- tfl_ard_code(x, "L-1", part = "data", data = "adae_saf")
  expect_false(any(grepl("^adae_ser <- ", one)))
  expect_error(tfl_ard_code(x, "L-1", part = "data", data = "nope"), "does not define")
  expect_error(tfl_ard_code(x, part = "data"), "one report's data")
})

test_that("a listing reads an analysis data, or the subjects of an analysis set", {
  x <- dp_spec()
  sp <- tfl_listing_spec(
    data.frame(output_id = c("L-1", "L-2", "L-3"), dataset = c(NA, "ADAE", "ADSL"),
               data = c("adae_ser", NA, NA), population_id = c(NA, "SAF", "SAF"),
               sort = "USUBJID"),
    data.frame(output_id = c("L-1", "L-2", "L-3"), vars = "USUBJID"))
  l1 <- tfl_listing_code(sp, "L-1", ard = x)
  expect_true(any(grepl("^adae_ser <- ", l1)))
  expect_true(any(grepl("^data <- adae_ser", l1)))
  l2 <- tfl_listing_code(sp, "L-2", ard = x)
  expect_true(any(grepl("^data <- adae", l2)))
  expect_true(any(grepl("semi_join(dplyr::select(pop_saf, USUBJID)", l2, fixed = TRUE)))
  # the analysis set's own dataset: the analysis set itself
  expect_true(any(grepl("^data <- pop_saf", tfl_listing_code(sp, "L-3", ard = x))))
  expect_error(tfl_listing_code(sp, "L-1", x$datasets), "give the ARD definition")
  # a listing of a dataset alone: as before, no ARD needed
  plain <- tfl_listing_spec(data.frame(output_id = "L-4", dataset = "ADAE"),
                            data.frame(output_id = "L-4", vars = "USUBJID"))
  expect_true(any(grepl("^# the data: ADAE", tfl_listing_code(plain, "L-4", x$datasets))))
  expect_error(tfl_listing_spec(data.frame(output_id = "L-9", dataset = "ADAE", data = "adae_ser"),
                                data.frame(output_id = "L-9", vars = "USUBJID")),
               "its `data` or a `dataset`")
})

test_that("a figure's read step names an analysis data: read by name, not from a file", {
  d <- list(data = list(list(step = "read", data = "adae_ser"),
                        list(step = "filter", expr = "AESEV == 'SEVERE'")),
            layers = list(list(layer = "point", x = "AESTDY", y = "AESTDY")))
  code <- tfl_fig_design_code(d)
  expect_identical(attr(code, "data"), "adae_ser")
  expect_length(attr(code, "reads"), 0L)
  expect_true(any(grepl("^df <- adae_ser", code)))
})

test_that("a YAML study: the listing's data and analysis set through the sheets; a figure built from its analysis data", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("ggsurvfit")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adtte")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "XYZ"))
  out <- file.path(d, "spec", "outputs")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  # a listing of an analysis data
  l <- tfl_output("L-1", "listing", content = list(tfl_item("plan_listing", list(
    cols = list(tfl_call("listing_col", list(vars = "USUBJID")))))))
  l$data <- list(source = "dataset", data = "adae_ser")
  l$ard <- list(analysis_data = list(list(id = "adae_ser", from = "ADAE",
                                          where = tfl_r("AESER == \"Y\""))))
  tfl_write_output(l, file.path(out, "L-1.yml"))
  l2 <- l
  l2$id <- "L-2"
  l2$data <- list(source = "dataset", dataset = "ADAE", population = "SAF")
  l2$ard <- NULL
  tfl_write_output(l2, file.path(out, "L-2.yml"))
  b <- tfl_sheets_from_yaml(d)
  ls <- b$listing$listings
  expect_identical(ls$data[ls$output_id == "L-1"], "adae_ser")
  expect_identical(ls$population_id[ls$output_id == "L-2"], "SAF")
  to <- file.path(d, "back")
  invisible(suppressWarnings(tfl_yaml_from_sheets(b, to)))
  back <- tfl_read_outputs(to)
  expect_identical(back[["L-1"]]$data$data, "adae_ser")
  expect_identical(back[["L-2"]]$data$population, "SAF")
  # a KM figure from its analysis data (the OS records)
  f <- tfl_output("F-1", "figure", content = list())
  f$data <- list(steps = list(list(step = "read", data = "adtte_os")))
  f$template <- list(name = "km_survival", args = list(by = "TRT01A"))
  f$ard <- list(analysis_data = list(list(id = "adtte_os", from = "ADTTE",
                                          where = tfl_r("PARAMCD == \"OS\""))))
  tfl_write_output(f, file.path(out, "F-1.yml"))
  st <- tfl_read_study_spec(d)
  code <- tfl_ard_code(st, "F-1", part = "data")
  expect_true(any(grepl("^adtte_os <- ", code)))
  rtf <- tfl_run_output("F-1", d)
  expect_true(file.exists(rtf))
  expect_match(paste(readLines(rtf, warn = FALSE), collapse = " "), "\\pict", fixed = TRUE)
})

test_that("the data part names each analysis set as a report's own code reads it: adsl_sub beside pop_saf (#520)", {
  x <- dp_spec()
  code <- tfl_ard_code(x, "L-1", part = "data")
  expect_true(any(grepl("^adsl_sub <- pop_saf$", code)))
  sp <- tfl_listing_spec(data.frame(output_id = "L-2", dataset = "ADAE", population_id = "SAF", sort = "USUBJID"),
                         data.frame(output_id = "L-2", vars = "USUBJID"))
  expect_true(any(grepl("^adsl_sub <- pop_saf$", tfl_listing_code(sp, "L-2", ard = x, part = "data"))))
  # two sets of one dataset: the second is adsl_sub2
  x2 <- tfl_ard_spec(list(
    datasets = data.frame(dataset = c("ADSL", "ADAE"), path = c("adsl.rds", "adae.rds")),
    populations = data.frame(population_id = c("SAF", "ITT"), dataset = "ADSL",
                             where = c("SAFFL == \"Y\"", "ITTFL == \"Y\"")),
    analysis_data = data.frame(output_id = "L-9", data_id = c("a_saf", "a_itt"), from = "ADAE",
                               population_id = c("SAF", "ITT")),
    analyses = data.frame(output_id = "T-9", analysis_id = "A1", method = "categorical",
                          dataset = "ADSL", population_id = "SAF", by = "TRT01A", variables = "SEX")))
  code2 <- tfl_ard_code(x2, "L-9", part = "data")
  expect_true(any(grepl("^adsl_sub <- pop_saf$", code2)))
  expect_true(any(grepl("^adsl_sub2 <- pop_itt$", code2)))
})
