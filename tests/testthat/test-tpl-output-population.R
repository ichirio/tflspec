# A report built from a series template has no `common: population`; its
# analysis set is on its subjects data entry (tflspec_summary's adsl_sub), and the
# header's {OUTPUT_POPULATION} prints that set's label (data.yml's)

test_that("a template report's {OUTPUT_POPULATION} is its subjects entry's set label", {
  skip_if_not_installed("haven")
  d <- tempfile("tpl-outpop")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(file.path(d, "s"), "adsl", id = "OP-1")
  sd <- file.path(d, "s")
  o <- tfl_output("T-DM", "table", label = "Demographics",
                  template = list(name = "tflspec_v1/tflspec_summary",
                                  args = list(by = "TRT01A", continuous = "AGE")))
  tfl_write_output(o, file.path(sd, "spec", "outputs", "T-DM.yml"))
  invisible(tfl_copy_templates(sd))
  st <- tfl_read_study_spec(sd)
  ro <- tfl_resolve_output(tfl_read_output(file.path(sd, "spec", "outputs", "T-DM.yml")), st)
  expect_true(is.na(tflspec:::.out_population(ro)))
  expect_identical(tflspec:::.out_population_or_data(ro), "SAF")
  lab <- Filter(function(p) identical(p$id, "SAF"), st$data$populations)[[1L]]$label
  expect_true(nzchar(lab))
  expect_identical(unname(tflspec:::.output_tokens(ro, st)["OUTPUT_POPULATION"]), lab)
  # `common: population_label` wins
  ro$common$population_label <- "Safety set, as the report says"
  expect_identical(unname(tflspec:::.output_tokens(ro, st)["OUTPUT_POPULATION"]),
                   "Safety set, as the report says")
})

test_that("the set of the subjects entries: one distinct value only", {
  ent <- function(...) list(id = "adsl_sub", from = "ADSL", ...)
  expect_identical(tflspec:::.out_population_or_data(list(data = list(ent(population = "SAF")))), "SAF")
  expect_identical(tflspec:::.out_population_or_data(
    list(data = list(ent(population = "SAF"), ent(population = "SAF")))), "SAF")
  expect_true(is.na(tflspec:::.out_population_or_data(
    list(data = list(ent(population = "SAF"), ent(population = "ITT"))))))
  expect_true(is.na(tflspec:::.out_population_or_data(list(data = list(ent())))))
  expect_true(is.na(tflspec:::.out_population_or_data(
    list(data = list(list(id = "adae_sub", from = "ADAE", population = "SAF"))))))
  # the subjects entries before the old `common: population` (A2: the set lives on the data)
  expect_identical(tflspec:::.out_population_or_data(
    list(common = list(population = "ITT"), data = list(ent(population = "SAF")))), "SAF")
})
