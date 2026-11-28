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
  expect_identical(tflspec:::.report_set(ro), "SAF")
  lab <- Filter(function(p) identical(p$id, "SAF"), st$data$populations)[[1L]]$label
  expect_true(nzchar(lab))
  expect_identical(unname(tflspec:::.output_tokens(ro, st)["OUTPUT_POPULATION"]), lab)
  # `common: population_label` wins
  ro$common$population_label <- "Safety set, as the report says"
  expect_identical(unname(tflspec:::.output_tokens(ro, st)["OUTPUT_POPULATION"]),
                   "Safety set, as the report says")
})

test_that("the set of the rows that make subjects: one distinct value, else BIGN's (A2-T Q3)", {
  ent <- function(...) list(id = "adsl_sub", from = "ADSL", ...)
  rs <- function(...) tflspec:::.report_set(list(...))
  expect_identical(rs(data = list(ent(population = "SAF"))), "SAF")
  expect_identical(rs(data = list(ent(population = "SAF"), ent(population = "SAF"))), "SAF")
  expect_true(is.na(rs(data = list(ent(population = "SAF"), ent(population = "ITT")))))
  expect_true(is.na(rs(data = list(ent()))))
  # a row of records of a set carries it too; a row of another's subjects names none
  expect_identical(rs(data = list(list(id = "adae_sub", from = "ADAE", population = "SAF"))), "SAF")
  expect_true(is.na(rs(data = list(list(id = "adae_sub", from = "ADAE", subjects_in = "adsl_sub")))))
  # the rows' set before `common: population` (read last, until it goes)
  expect_identical(rs(common = list(population = "ITT"), data = list(ent(population = "SAF"))), "SAF")
  expect_identical(rs(common = list(population = "ITT"), data = list(ent())), "ITT")
})
