wf_pop <- list(list(id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""))

test_that("waterfall: the design form; the bars' order the call's step", {
  x <- tfl_template_expand("waterfall", list(param = "SUMDIAM", population = "SAF",
                                             where = "AVISIT == \"WEEK 12\""),
                           population = wf_pop)
  # (the call's: one bar a subject said, the bars beyond the axis said, the order)
  expect_identical(vapply(x$data$steps, `[[`, "", "step"),
                   c("read", "join", "param", "filter", "filter", "code", "code", "rank"))
  expect_identical(x$data$steps[[4L]]$expr, "AVISIT == \"WEEK 12\"")
  expect_identical(x$call_steps, 3L)
  r <- tfl_template_expand("waterfall", list(param = "SUMDIAM", by_response = TRUE))
  expect_identical(vapply(r$data$steps, `[[`, "", "step"),
                   c("read", "param", "join", "filter", "code", "code", "rank", "derive", "levels"))
  expect_identical(r$call_steps, 5L)
  expect_identical(r$plot$colour_by, "BOR")
})

test_that("tpl_waterfall(): one bar a subject, the largest first", {
  skip_if_not_installed("dplyr")
  df <- adtr[adtr$AVISIT == "WEEK 12", ]
  p <- tpl_waterfall(df, value = PCHG)
  expect_s3_class(p, "ggplot")
  b <- ggplot2::layer_data(p, 1L)
  expect_identical(nrow(b), sum(!is.na(df$PCHG)))
  expect_identical(tplfig_png(p), tplfig_png(tpl_waterfall(df, value = "PCHG")))
  bor <- adrs[adrs$PARAMCD == "BOR", c("USUBJID", "AVALC")]
  names(bor)[2L] <- "BOR"
  q <- tpl_waterfall(merge(df, bor, by = "USUBJID", all.x = TRUE), by_response = TRUE)
  expect_true(nrow(b) > 50L)
  expect_true(length(unique(ggplot2::layer_data(q, 1L)$fill)) > 1L)
})

test_that("in a study: A (the data steps, the call) = B (the design) = the template SPEC", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adtr", "adrs"), "WF-1")
  for (resp in c(FALSE, TRUE)) {
    o <- tfl_output(paste0("F-9-", resp), "figure", population = "SAF", label = "Waterfall",
                    extends = "defaults",
                    template = list(name = "waterfall",
                                    args = list(param = "SUMDIAM", by_response = resp,
                                                where = "AVISIT == \"WEEK 12\"")),
                    content = list(tfl_item("labs", list(y = "Change in the sum of diameters (%)"))))
    x <- tplfig_check(o, d, data)
    txt <- paste(x$a, collapse = "\n")
    expect_match(txt, "semi_join(", fixed = TRUE)
    expect_match(txt, "filter(PARAMCD == \"SUMDIAM\", AVISIT == \"WEEK 12\"", fixed = TRUE)
    expect_match(txt, sprintf("content <- tflspec::tfl_tpl(adtr_sumdiam, \"waterfall\", by_response = %s) +", resp),
                 fixed = TRUE)
    expect_identical(grepl("PARAMCD == \"BOR\"", txt, fixed = TRUE), resp)
  }
})

test_that("tpl_waterfall(): a panel per arm, its settings as a named vector (#573)", {
  skip_if_not_installed("dplyr")
  df <- adtr[adtr$AVISIT == "WEEK 12" & !is.na(adtr$PCHG), ]
  bor <- adrs[adrs$PARAMCD == "BOR", c("USUBJID", "AVALC")]
  names(bor)[2L] <- "BOR"
  df <- merge(df, bor, by = "USUBJID", all.x = TRUE)
  for (resp in c(FALSE, TRUE)) {
    # the reference lines' labels drawn in each panel
    p <- tpl_waterfall(df, by_response = resp, plot = list(facet_by = "TRT01A"))
    expect_type(tplfig_png(p), "character")
    lab <- ggplot2::layer_data(p, 4L)
    expect_identical(sort(unique(lab$label)), c("-30%", "20%"))
    expect_identical(length(unique(lab$PANEL)), length(unique(df$TRT01A)))
  }
  # (the program's call writes the settings as list(); a named vector reads too)
  q <- tpl_waterfall(df, plot = c(x_label = "Subjects", y_min = -100, y_max = 100))
  expect_type(tplfig_png(q), "character")
})
