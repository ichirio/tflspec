est_df <- data.frame(label = c("Overall", "Male", "Female"), est = c(0.8, 0.7, 0.9),
                     lcl = c(0.6, 0.5, 0.6), ucl = c(1.1, 1.0, 1.3), n = c(38, 20, 18))

test_that("the catalogue classifies every type and marks the defaults", {
  cat <- tfl_fig_catalog()
  expect_true(all(c("category", "type", "style", "default", "status", "fun", "subtypes") %in% names(cat)))
  expect_false(anyDuplicated(paste(cat$type, cat$style)) > 0)
  impl <- tfl_fig_catalog("implemented")
  expect_gte(length(unique(impl$type)), 15)
  # exactly one default style per type
  expect_true(all(tapply(cat$default, cat$type, sum) == 1))
  for (f in unique(impl$fun)) expect_true(exists(f, mode = "function"), info = f)
  expect_true(all(is.na(tfl_fig_catalog("planned")$fun)))
  expect_identical(tfl_fig_types()$type, impl$type)
})

test_that("every style of the template types generates runnable code", {
  skip_if_not_installed("patchwork")
  adam <- tfl_example_adam()
  impl <- tfl_fig_catalog("implemented")
  impl <- impl[!impl$type %in% c("km", "waterfall", "swimmer", "sankey", "sunburst"), ]
  for (i in seq_len(nrow(impl))) {
    fun <- get(impl$fun[i])
    for (use_adam in list(adam, NULL)) {
      code <- fun(use_adam, style = impl$style[i])
      expect_s3_class(code, "tfl_code")
      expect_false(grepl("tflspec::", code, fixed = TRUE))
      expect_no_warning(expect_no_error(run_code(code, list(est_df = est_df)),
                                        message = paste(impl$type[i], impl$style[i])))
    }
  }
})

test_that("ADSL variables are joined from ADSL", {
  code <- tfl_fig_mean(param = "AST")
  expect_match(code, 'select(-any_of(c("TRT01A", "SAFFL")))', fixed = TRUE)
  expect_match(code, 'left_join(adsl |> select(USUBJID, all_of(c("TRT01A", "SAFFL"))), by = "USUBJID")', fixed = TRUE)
  expect_match(code, 'filter(PARAMCD == "AST")', fixed = TRUE)
})

test_that("codelists are literal with ADaM data", {
  adam <- tfl_example_adam()
  expect_match(tfl_fig_box(adam), 'pal_grp <- c("Drug A" = "blue", "Drug B" = "#D55E00")', fixed = TRUE)
  expect_match(tfl_fig_forest(adam), 'levels = c("Drug A", "Drug B")', fixed = TRUE)
})

test_that("forest marks non-estimable subgroups as NE", {
  adam <- tfl_example_adam()
  env <- run_code(tfl_fig_forest(adam, style = "or"))
  expect_true("NE" %in% env$est_df$txt)
  expect_true(all(is.na(env$est_df$est[env$est_df$txt == "NE"])))
})

test_that("swimmer subtype: bars from a start variable", {
  adam <- tfl_example_adam()
  code <- tfl_fig_swimmer(adam, start = "TRTSDY", end = "TRTEDY", x_max = 24)
  expect_match(code, "geom_segment(aes(x = TRTSDY, xend = TRTEDY", fixed = TRUE)
  expect_match(code, "mutate(across(c(TRTSDY, TRTEDY", fixed = TRUE)
  expect_no_error(run_code(code))
  expect_error(tfl_fig_swimmer(start = "TRTSDY"), "needs `end`")
})

test_that("unknown columns are reported when ADaM data is given", {
  adam <- tfl_example_adam()
  expect_error(tfl_fig_mean(adam, value = "NOPE"), "no column")
  expect_error(tfl_fig_forest(adam, subgroups = "NOPE"), "no column")
})

test_that("new types work through the Excel plot list", {
  adam <- tfl_example_adam()
  rows <- data.frame(plot_id = c("F1", "F2", "F3"), type = c("forest", "ae_dot", "mean"),
                     style = c("hr", "incidence", "se_n"), param = c("PFS", NA, "AST"),
                     group = NA, args = c(NA, "top = 5", NA))
  code <- tfl_fig_list_code(rows, adam)
  expect_match(code[["F1"]], 'PARAMCD == "PFS"', fixed = TRUE)
  expect_match(code[["F2"]], "n = 5", fixed = TRUE)
  expect_match(code[["F3"]], 'PARAMCD == "AST"', fixed = TRUE)
})

test_that("where adds a record condition and groups follow the paired numeric code", {
  adam <- tfl_example_adam()
  adam$ADSL$TRT01AN <- ifelse(adam$ADSL$TRT01A == "Drug B", 1, 2)   # Drug B first
  code <- tfl_fig_mean(adam, where = 'AVISITN <= 8')
  expect_match(code, "filter(AVISITN <= 8)", fixed = TRUE)
  expect_match(code, 'pal_grp <- c("Drug B" = "blue", "Drug A" = "#D55E00")', fixed = TRUE)
  expect_match(code, "mutate(TRT01A = factor(TRT01A, levels = names(pal_grp)))", fixed = TRUE)
  env <- run_code(code)
  expect_equal(max(env$sum_df$AVISITN), 8)
  expect_equal(levels(env$sum_df$TRT01A), c("Drug B", "Drug A"))
})

test_that("pharmaverseadam example files are shipped", {
  d <- system.file("examples", "pharmaverseadam", package = "tflspec")
  expect_true(all(file.exists(file.path(d, c("prepare_adam.R", "plot_list.xlsx", "run_all.R")))))
  rows <- openxlsx::read.xlsx(file.path(d, "plot_list.xlsx"), sheet = "plots")
  expect_true(all(rows$type %in% tfl_fig_catalog("implemented")$type))
  expect_false(any(rows$type %in% c("sankey", "sunburst")))
})
