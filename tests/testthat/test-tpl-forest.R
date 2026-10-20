fr_skip <- function() {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("survival")
  skip_if_not_installed("cardx")
}

test_that("forest_hr: the report's ARD (its Cox analyses) and the design that reads it", {
  x <- tfl_template_expand("forest_hr", list(param = "PFS", population = "SAF"))
  ad <- x$ard$analysis_data[[1L]]
  expect_identical(ad[c("id", "from", "population")],
                   list(id = "adtte_saf_pfs", from = "ADTTE", population = "SAF"))
  expect_identical(as.character(ad$where), "PARAMCD == \"PFS\"")
  expect_identical(ad$add, list("SEX", "AGEGR1"))
  expect_identical(vapply(x$ard$analyses, `[[`, "", "id"), c("HR", "HR_SEX", "HR_AGEGR1"))
  expect_identical(vapply(x$data$steps, `[[`, "", "step"), c("ard_stats", "code"))
  expect_identical(x$call_steps, 2L)
  expect_identical(vapply(x$content, `[[`, "", "fn"),
                   c("vline", "errorbar_h", "point", "text_column"))
  expect_identical(tfl_template_params("forest_hr")$subgroups$type, "vars")
})

test_that("in a study: the ARD built; A (the call, the layout) = B (the design) = the template SPEC", {
  skip_on_cran()
  fr_skip()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adtte"), "FR-1")
  o <- tfl_output("F-9-1", "figure", population = "SAF", label = "Forest",
                  extends = "defaults",
                  template = list(name = "forest_hr",
                                  args = list(param = "PFS", population = "SAF",
                                              subgroups = c("SEX", "AGEGR1"))),
                  content = list(tfl_item("geom_vline", list(xintercept = 0.5, linetype = "dotted")),
                                 tfl_item("labs", list(x = "Hazard ratio (95% CI)"))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "F-9-1.yml"))
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "F-9-1", dir = d, save = FALSE))
  expect_true(all(c("HR", "HR_SEX", "HR_AGEGR1") %in% ard$analysis_id))
  x <- tplfig_check(o, d, c(data, list(ard = ard)))
  txt <- paste(x$a, collapse = "\n")
  expect_match(txt, "fig <- tpl_forest_hr(ard, subgroups = c(SEX, AGEGR1)) +", fixed = TRUE)
  expect_match(txt, "content <- tpl_forest_layout(fig)", fixed = TRUE)
  # the forest alone, then its text column at its right
  p <- tpl_forest_hr(ard, group = TRT01A, subgroups = c(SEX, AGEGR1))
  expect_s3_class(p, "ggplot")
  expect_s3_class(tpl_forest_layout(p), "patchwork")
  expect_error(tpl_forest_layout(ggplot2::ggplot()), "a forest of tpl_forest_hr")
})
