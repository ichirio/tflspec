run_design <- function(design, adam = tfl_example_adam()) {
  code <- tfl_fig_design_code(design, "t")
  e <- new.env()
  for (n in names(adam)) assign(tolower(n), adam[[n]], envir = e)
  dir <- tempfile("fig")
  dir.create(dir)
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  suppressMessages(eval(parse(text = code), envir = e))
  list(code = code, fig = e$fig, png = file.exists(e$fig_path))
}

test_that("the pieces are described, the catalog's layers with them", {
  p <- tfl_fig_parts()
  expect_setequal(unique(p$section), c("data", "stats", "plot", "layers"))
  expect_true(all(c("read", "join", "survfit", "summary", "km_curve", "risk_table",
                    "line", "errorbar", "ribbon", "boxplot", "text_repel") %in% p$piece))
  expect_equal(p$kind[p$piece == "hline" & p$field == "yintercept"], "values")
  expect_true("data" %in% p$field[p$piece == "line"])
  expect_false("data" %in% p$field[p$piece == "hline"])
})

test_that("the templates make figures that draw, and check clean", {
  skip_if_not_installed("ggsurvfit")
  adam <- tfl_example_adam()
  prm <- unique(adam$ADTTE$PARAMCD)[1]
  for (d in list(
    tfl_fig_template("km_risk_table", param = prm, group = "TRT01P"),
    tfl_fig_template("km_ci", param = prm, group = "TRT01P"),
    tfl_fig_template("km_single_arm", param = prm),
    tfl_fig_template("mean_se_n", param = unique(adam$ADLB$PARAMCD)[1]),
    tfl_fig_template("waterfall_response"))) {
    expect_equal(nrow(tfl_check_fig_design(d, adam)), 0L, info = d$template)
    r <- run_design(d, adam)
    expect_true(r$png, info = d$template)
  }
})

test_that("a design goes to YAML and back", {
  d <- tfl_fig_template("mean_ci", param = "ALT")
  f <- tempfile(fileext = ".yml")
  tfl_write_fig_design(d, f)
  expect_equal(unclass(tfl_read_fig_design(f)), unclass(d))
  # a type / style / args design (0.0.12) reads as a whole-figure layer
  writeLines(c("type: km", "style: simple", "args:", "  param: OS"), f)
  d1 <- tfl_read_fig_design(f)
  expect_equal(d1$layers[[1]]$layer, "figure")
  expect_match(paste(tfl_fig_design_code(d1), collapse = "\n"), "PARAMCD == \"OS\"")
})

test_that("your own code, and any function, fit in", {
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADLB"),
                list(step = "data_code", code = "df <- df[df$AVAL > 0, ]"),
                list(step = "derive", variable = "LOGV", expr = "log(AVAL)")),
    plot = list(x_label = "Visit"),
    layers = list(list(layer = "geom", geom = "geom_boxplot",
                       aes = "x = AVISIT | y = LOGV", params = "outlier.shape = NA"),
                  list(layer = "layer_code", code = "p <- p + ggplot2::ggtitle('mine')")))
  r <- run_design(d)
  expect_true(r$png)
  expect_true(any(grepl("df <- df[df$AVAL > 0, ]", r$code, fixed = TRUE)))
  expect_true(any(grepl("geom_boxplot(data = df, aes(x = AVISIT, y = LOGV), outlier.shape = NA)",
                        r$code, fixed = TRUE)))
})

test_that("a layer is added to the catalog, and used like the others", {
  tfl_fig_add_layer("my_rug", "geom_rug", "ggplot2", "Rug",
                    aes = c(x = "X"), params = data.frame(field = "alpha", kind = "number",
                                                          label = "Transparency", default = "0.4"))
  expect_true("my_rug" %in% tfl_fig_parts()$piece)
  d <- tfl_fig_design(data = list(list(step = "read", dataset = "ADSL")),
                      layers = list(list(layer = "my_rug", x = "TRTDURD")))
  expect_match(paste(tfl_fig_design_code(d), collapse = "\n"),
               "geom_rug(data = df, aes(x = TRTDURD), alpha = 0.4)", fixed = TRUE)
  expect_true(run_design(d)$png)
})

test_that("the checks find what does not fit the data", {
  adam <- tfl_example_adam()
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADTTE"),
                list(step = "param", value = "NOPE"),
                list(step = "join", dataset = "ADSL", vars = "TRT01A, NOVAR"),
                list(step = "flag", variable = "XFL"),
                list(step = "zap")),
    stats = list(list(step = "survfit", time = "AVAL", censor = "CNSR", by = "TRT01A")),
    plot = list(legend = "left", colour_by = "TRT01A"),
    layers = list(list(layer = "censor_mark"),
                  list(layer = "line", data = "sm", x = "A", y = "B")))
  p <- tfl_check_fig_design(d, adam)
  has <- function(part, pat) any(startsWith(p$part, part) & grepl(pat, p$problem))
  expect_true(has("data[2]", "no PARAMCD NOPE"))
  expect_true(has("data[3]", "NOVAR"))
  expect_true(has("data[4]", "XFL"))
  expect_true(has("data[5]", "unknown"))
  expect_true(has("plot", "not one of"))
  expect_true(has("layers[1]", "needs the KM curves"))
  expect_true(has("layers[2]", "no data or statistics named sm"))
  d2 <- tfl_fig_design(layers = list(list(layer = "hline", yintercept = 0),
                                      list(layer = "km_curve")))
  expect_true(any(grepl("first layer", tfl_check_fig_design(d2)$problem)))
})

test_that("every template has its clinical category and the data it reads", {
  tp <- tfl_fig_templates()
  expect_true(all(c("category", "data") %in% names(tp)))
  expect_false(anyNA(tp$category))
  expect_false(anyNA(tp$data))
  # the catalog's categories, not new ones
  expect_true(all(tp$category %in% tfl_fig_catalog()$category))
  expect_identical(tp$data[tp$template == "km_risk_table"], "ADTTE")
  expect_identical(tp$category[tp$template == "ae_dot_incidence"], "Safety")
  expect_identical(tp$data[tp$template == "mean_se"], "ADLB / ADVS + ADSL")
})

test_that("every template draws on the example data and checks clean", {
  skip_if_not_installed("ggsurvfit")
  adam <- tfl_example_adam()
  tp <- tfl_fig_templates()
  prm <- unique(adam$ADTTE$PARAMCD)[1]
  for (i in seq_len(nrow(tp))) {
    t <- tp$template[i]
    args <- switch(tp$kind[i], km = list(param = prm, group = if (t != "km_single_arm") "TRT01P"),
                   forest = list(param = prm), list())
    d <- do.call(tfl_fig_template, c(list(t), args))
    expect_equal(nrow(tfl_check_fig_design(d, adam)), 0L, info = t)
    expect_true(run_design(d, adam)$png, info = t)
  }
})

test_that("the new statistics and settings write what they say", {
  d <- tfl_fig_template("bar_rate_ci")
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  expect_match(code, "binom.test", fixed = TRUE)
  expect_match(code, 'label = sprintf("%.1f%%', fixed = TRUE)
  d <- tfl_fig_template("pk_individual")
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  expect_match(code, "scale_y_log10()", fixed = TRUE)
  expect_match(code, "facet_wrap(vars(TRT01A))", fixed = TRUE)
  d <- tfl_fig_template("swimmer_assessment")
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  expect_match(code, "assess <- adrs |>", fixed = TRUE)
  expect_match(code, "inner_join(df |> select(USUBJID, Y_ID)", fixed = TRUE)
  expect_match(code, "arrow = arrow(", fixed = TRUE)
  # a whole-script template is one figure layer
  d <- tfl_fig_template("edish_alt")
  expect_equal(d$layers[[1]]$layer, "figure")
  expect_equal(nrow(tfl_fig_advice(d)), 0L)
})

test_that("a time can stay in days: no conversion, no check error", {
  adam <- tfl_example_adam()
  prm <- unique(adam$ADTTE$PARAMCD)[1]
  d <- tfl_fig_template("km_simple", param = prm, group = "TRT01P",
                        time_unit = "days")
  steps <- vapply(d$data, `[[`, "", "step")
  expect_true("time_unit" %in% steps)
  expect_identical(d$data[[which(steps == "time_unit")]]$unit, "days")
  chk <- tfl_check_fig_design(d, adam)
  expect_false(any(chk$level == "error" & grepl("time_unit", chk$part)))
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  expect_false(grepl("# days -> days", code, fixed = TRUE))
  expect_false(grepl("AVAL = AVAL / 1", code, fixed = TRUE))
  expect_true("days" %in% strsplit(tfl_fig_parts()$choices[
    tfl_fig_parts()$piece == "time_unit" & tfl_fig_parts()$field == "unit"], " | ", fixed = TRUE)[[1L]])
})

test_that("a report program's figure: no save, its own name, the study's palette", {
  d <- tfl_fig_template("km_simple", data = "ADTTE", param = "TTDE",
                        pop = "SAFFL", group = "TRT01A", time_unit = "days")
  whole <- tfl_fig_design_code(d)
  expect_true(any(grepl("^# ---- saving the figure", whole)))
  expect_true(any(grepl("^ggsave\\(", whole)))
  expect_true(any(grepl("pal <- setNames(", whole, fixed = TRUE)))
  expect_false(any(grepl("#####", whole, fixed = TRUE)))
  expect_false(any(grepl("%>%", whole, fixed = TRUE)))
  part <- tfl_fig_design_code(d, setup = TRUE, save = FALSE, name = "plot")
  expect_false(any(grepl("ggsave|saving the figure", part)))
  expect_true("plot <- p" %in% part)
  expect_false(any(grepl("^fig", part)))
  expect_true(any(grepl('pal <- tfl_colours("treatment", levels(droplevels(factor(df$TRT01A))))',
                        part, fixed = TRUE)))
  # it runs, after the study's figure setup
  skip_if_not_installed("ggsurvfit")
  e <- new.env()
  eval(parse(text = tfl_fig_setup_code()), e)
  e$adtte <- data.frame(USUBJID = 1:6, PARAMCD = "TTDE", SAFFL = "Y",
                        TRT01A = rep(c("A", "B"), 3), AVAL = c(5, 8, 12, 3, 9, 15),
                        CNSR = c(0, 1, 0, 0, 1, 0))
  suppressPackageStartupMessages(eval(parse(text = part), e))
  expect_s3_class(e$plot, "ggplot")
  expect_identical(unname(e$pal), unname(e$tfl_colours("treatment", c("A", "B"))))
})
