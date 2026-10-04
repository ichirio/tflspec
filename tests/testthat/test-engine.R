with_variant <- function(spec, type, pos) {
  spec$plots$legend_type <- type
  spec$plots$legend_pos <- pos
  spec
}

test_that("example spec generates runnable code for every legend variant", {
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("patchwork")
  adam <- tfl_example_adam()
  variants <- list(c("mapped", "right"), c("mapped", "inside_tr"), c("mapped", "bottom"),
                   c("manual", "below"), c("manual", "inside_br"), c("manual", "right"),
                   c("none", "right"))
  for (v in variants) {
    spec <- with_variant(tfl_example_fig_spec(), v[1], v[2])
    for (use_adam in list(adam, NULL)) {
      code <- tfl_fig_code(spec, adam = use_adam)
      for (id in names(code)) {
        expect_no_error(run_code(code[[id]]), message = paste(id, v, collapse = " "))
      }
    }
  }
})

test_that("a manual legend is drawn for every figure type, without warnings", {
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("patchwork")
  for (pos in c("below", "inside_br", "right")) {
    code <- tfl_fig_code(with_variant(tfl_example_fig_spec(), "manual", pos))
    for (id in names(code)) {
      # the panel has its columns (not NA): no item is dropped
      expect_false(grepl("ncol = NA", code[[id]], fixed = TRUE), label = paste(id, pos))
      expect_no_warning(run_code(code[[id]]), message = paste(id, pos))
    }
  }
})

test_that("generated code does not depend on tflspec", {
  code <- tfl_fig_code(tfl_example_fig_spec(), adam = tfl_example_adam())
  expect_false(any(grepl("tflspec::", code)))
  expect_true(all(grepl("ggsave(", code, fixed = TRUE)))
})

test_that("codelist values from ADaM become literal palettes", {
  code <- tfl_fig_code(tfl_example_fig_spec(), "F-KM-1", adam = tfl_example_adam())
  expect_match(code, 'pal_strata <- c("Drug A" = "blue", "Drug B" = "#D55E00")', fixed = TRUE)
  expect_match(code, 'PARAMCD == "OS"', fixed = TRUE)
  # without data, an unnamed palette is resolved at run time
  code2 <- tfl_fig_code(tfl_example_fig_spec(), "F-KM-1")
  expect_match(code2, "unique(na.omit(as.character(km_df$TRT01P)))", fixed = TRUE)
})

test_that("levels sheet sets order, labels and colours", {
  spec <- tfl_example_fig_spec()
  spec$levels <- data.frame(plot_id = "F-WF-1", variable = "BOR", value = c("PD", "PR"),
                            label = c("Progressive", NA), order = c(1, 2), colour = c("red", NA))
  spec <- tfl_fig_spec(spec$plots, spec$roles, spec$filters, spec$levels, spec$legend, spec$options)
  code <- tfl_fig_code(spec, "F-WF-1")
  expect_match(code, 'pal_fill <- c("PD" = "red", "PR" = "#0000FF")', fixed = TRUE)
  expect_match(code, 'pal_fill_lab <- c("PD" = "Progressive", "PR" = "PR")', fixed = TRUE)
})

test_that("legend sheet gives a data-independent legend", {
  spec <- tfl_example_fig_spec()
  lg <- data.frame(plot_id = "F-SW-1", order = 1:3,
                   label = c("Complete response", "Ongoing", "Death"),
                   glyph = c("rect", "line", "point"),
                   shape = c(NA, NA, "triangle_down"),
                   colour = c(NA, "black", "black"), fill = c("#99CC99", NA, NA), linetype = NA)
  spec <- tfl_fig_spec(spec$plots, spec$roles, spec$filters, spec$levels, lg, spec$options)
  code <- tfl_fig_code(spec, "F-SW-1")
  expect_match(code, "manual (legend sheet)", fixed = TRUE)
  expect_match(code, '"Complete response", "rect", NA, NA, "#99CC99", NA', fixed = TRUE)
  expect_match(code, '"Death", "point", 25, "black", "black", NA', fixed = TRUE)
  skip_if_not_installed("patchwork")
  expect_no_error(run_code(code))
})

test_that("tfl_check_fig_spec reports problems", {
  adam <- tfl_example_adam()
  bad <- tfl_example_fig_spec()
  bad$filters$value[1] <- "OSX"
  bad$roles <- bad$roles[!(bad$roles$plot_id == "F-WF-1" & bad$roles$role == "value"), ]
  bad$plots$legend_pos[1] <- "somewhere"
  out <- suppressMessages(tfl_check_fig_spec(bad, adam))
  expect_true(any(grepl("OSX", out$message)))
  expect_true(any(grepl("required role 'value'", out$message)))
  expect_true(any(grepl("legend_pos", out$message)))
  expect_equal(nrow(suppressMessages(tfl_check_fig_spec(tfl_example_fig_spec(), adam))), 0)
})

test_that("spec template round-trips", {
  adam <- tfl_example_adam()
  path <- tempfile(fileext = ".xlsx")
  tfl_fig_spec_template(adam, path, spec = tfl_example_fig_spec())
  expect_true(all(c("plots", "roles", "filters", "levels", "legend", "options",
                    "adam_vars", "adam_values") %in% openxlsx::getSheetNames(path)))
  spec2 <- tfl_read_fig_spec(path)
  expect_identical(tfl_fig_code(spec2, adam = adam), tfl_fig_code(tfl_example_fig_spec(), adam = adam))
})

test_that("unknown layer is an error", {
  spec <- tfl_example_fig_spec()
  spec$plots$layers[1] <- "censor_mark, sparkles"
  expect_error(tfl_fig_code(spec, "F-KM-1"), "unknown layer")
})
