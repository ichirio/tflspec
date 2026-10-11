test_that("templates get little advice; a bare design gets the usual", {
  adam <- tfl_example_adam()
  km <- tfl_fig_template("km_risk_table", param = unique(adam$ADTTE$PARAMCD)[1], group = "TRT01P")
  a <- tfl_fig_advice(km, adam)
  expect_setequal(a$rule, "km_axis")
  bare <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADTTE"), list(step = "param", value = "OS")),
    stats = list(list(step = "survfit", name = "fit", time = "AVAL", censor = "CNSR", by = "TRT01P")),
    plot = list(colour_by = "TRT01P", legend = "none"),
    layers = list(list(layer = "km_curve")))
  a <- tfl_fig_advice(bare, adam)
  expect_true(all(c("km_risk", "km_censor", "km_unit", "legend_none", "pop") %in% a$rule))
  expect_true(all(a$level %in% c("info", "warning")))
})

test_that("fixes make the change they name", {
  adam <- tfl_example_adam()
  bare <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADTTE"), list(step = "param", value = "OS")),
    stats = list(list(step = "survfit", name = "fit", time = "AVAL", censor = "CNSR", by = "TRT01P")),
    plot = list(colour_by = "TRT01P", legend = "none"),
    layers = list(list(layer = "km_curve")))
  a <- tfl_fig_advice(bare, adam)
  d <- bare
  for (f in a$fix) d <- tfl_fig_apply_fix(d, f)
  kinds <- vapply(d$layers, `[[`, "", "layer")
  expect_equal(kinds, c("km_curve", "censor_mark", "risk_table"))
  # the data steps before the fit (the named step), which stays last
  expect_equal(vapply(d$data, `[[`, "", "step"), c("read", "param", "time_unit", "flag", "survfit"))
  expect_equal(d$plot$legend, "bottom")
  # the fixed design draws, and the advice is now only the axis
  expect_equal(nrow(tfl_check_fig_design(d, adam)), 0L)
  expect_setequal(tfl_fig_advice(d, adam)$rule, "km_axis")
})

test_that("groups are counted against the palette and the legend", {
  adam <- tfl_example_adam()
  adam$ADSL$MANY <- paste0("G", seq_len(nrow(adam$ADSL)) %% 8)
  d <- tfl_fig_template("mean_se", param = unique(adam$ADLB$PARAMCD)[1], group = "MANY")
  d$plot$legend <- "inside"
  a <- tfl_fig_advice(d, adam)
  expect_true(all(c("palette_short", "legend_inside") %in% a$rule))
  d2 <- tfl_fig_apply_fix(d, a$fix[[which(a$rule == "legend_inside")]])
  expect_equal(d2$plot$legend, "bottom")
  # a named palette missing a value
  w <- tfl_fig_template("waterfall_response")
  adam$ADRS$AVALC[adam$ADRS$PARAMCD == "BOR"][1] <- "CRX"
  w$plot$colour_by <- "BOR"
  # BOR is joined, so the levels come from nowhere: no palette advice, and no error
  expect_false("palette_names" %in% tfl_fig_advice(w, adam)$rule)
})

test_that("text visits with no order, and a waterfall's usual marks", {
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADLB")),
    stats = list(list(step = "summary", name = "sm", value = "AVAL", by = "TRT01A, AVISIT")),
    plot = list(colour_by = "TRT01A"),
    layers = list(list(layer = "line", data = "sm", x = "AVISIT", y = "mean", colour = "TRT01A")))
  a <- tfl_fig_advice(d)
  expect_true("visit_order" %in% a$rule)
  d2 <- tfl_fig_apply_fix(d, a$fix[[which(a$rule == "visit_order")]])
  # the order is a step on df: before the summary that is made from it
  k <- vapply(d2$data, `[[`, "", "step")
  expect_equal(k, c("read", "levels", "summary"))
  lv <- d2$data[[2L]]
  expect_equal(lv$step, "levels"); expect_equal(lv$order_by, "AVISITN")
  w <- tfl_fig_template("waterfall_plain")
  w$layers <- w$layers[1]
  w$plot$x_text <- TRUE
  a <- tfl_fig_advice(w)
  expect_true(all(c("waterfall_ref", "waterfall_x") %in% a$rule))
})
