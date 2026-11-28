run_design <- function(design, adam = tfl_example_adam(), ard = NULL) {
  code <- tfl_fig_design_code(design, "t")
  e <- new.env()
  for (n in names(adam)) assign(tolower(n), adam[[n]], envir = e)
  # a figure reading an ARD: the ARD, and the study helpers that read it
  if (!is.null(ard)) {
    assign("ard", ard, envir = e)
    eval(parse(text = tfl_helpers_code()), envir = e)
  }
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
  expect_setequal(unique(p$section), c("data", "plot", "layers"))
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
    tfl_fig_template("mean_se_n", group = "TRT01A", param = unique(adam$ADLB$PARAMCD)[1]),
    tfl_fig_template("waterfall_response"))) {
    expect_equal(nrow(tfl_check_fig_design(d, adam)), 0L, info = d$template)
    r <- run_design(d, adam)
    expect_true(r$png, info = d$template)
  }
})

test_that("a design goes to YAML and back", {
  d <- tfl_fig_template("mean_ci", group = "TRT01A", param = "ALT")
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
  expect_true(any(grepl("adlb_sub <- adlb_sub[adlb_sub$AVAL > 0, ]", r$code, fixed = TRUE)))
  expect_true(any(grepl("geom_boxplot(data = adlb_sub, aes(x = AVISIT, y = LOGV), outlier.shape = NA)",
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
               "geom_rug(data = adsl_sub, aes(x = TRTDURD), alpha = 0.4)", fixed = TRUE)
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
  # every figure type drawn with ggplot2: about 20 s on CRAN's machines; CI
  # runs it on every configuration
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  adam <- tfl_example_adam()
  tp <- tfl_fig_templates()
  prm <- unique(adam$ADTTE$PARAMCD)[1]
  for (i in seq_len(nrow(tp))) {
    t <- tp$template[i]
    args <- switch(tp$kind[i], km = list(param = prm), forest = list(param = prm), list())
    d <- do.call(tfl_fig_template, c(list(t), args, list(group = fig_group_of(t))))
    expect_equal(nrow(tfl_check_fig_design(d, adam)), 0L, info = t)
    # the forest plot reads its own ARD: made first, from its analyses
    ard <- if (!is.null(attr(d, "analyses"))) forest_example_ard(adam, d)
    expect_true(run_design(d, adam, ard)$png, info = t)
  }
})

test_that("the new statistics and settings write what they say", {
  d <- tfl_fig_template("bar_rate_ci", group = "TRT01P")
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  expect_match(code, "binom.test", fixed = TRUE)
  expect_match(code, 'label = paste0(fmt_num(rate, 1), "% (", x, "/", n, ")")', fixed = TRUE)
  d <- tfl_fig_template("pk_individual", group = "TRT01A")
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  expect_match(code, "scale_y_log10()", fixed = TRUE)
  expect_match(code, "facet_wrap(vars(TRT01A))", fixed = TRUE)
  d <- tfl_fig_template("swimmer_assessment")
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  expect_match(code, "assess <- adrs |>", fixed = TRUE)
  expect_match(code, "inner_join(adsl_sub |> select(USUBJID, Y_ID)", fixed = TRUE)
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
  # the chain goes into `plot` at once; no library(), no header
  expect_true(any(startsWith(part, "plot <- ggsurvfit(fit")))
  expect_false(any(grepl("^library\\(|^# Generated", part)))
  expect_identical(part[[1L]], "# ---- data ------------------------------------------------------------------")
  expect_true(any(grepl("^# ---- plot ", part)))
  expect_false(any(grepl("^fig", part)))
  # the data under a name that says what it is (adtte_sub, not df)
  expect_identical(tfl_fig_data_name(d), "adtte_sub")
  expect_identical(tfl_fig_data_name(tfl_fig_design(data = list(list(step = "read", dataset = "ADSL")))), "adsl_sub")
  expect_null(tfl_fig_data_name(tfl_fig_design(data = list(list(step = "flag", variable = "SAFFL")))))
  expect_true(any(grepl('pal <- tfl_colours("treatment", levels(factor(adtte_sub$TRT01A)))',
                        part, fixed = TRUE)))
  # it runs, after the study's figure setup
  skip_if_not_installed("ggsurvfit")
  e <- new.env()
  eval(parse(text = tfl_fig_setup_code()), e)
  eval(parse(text = tfl_helpers_code()), e)
  e$adtte <- data.frame(USUBJID = 1:6, PARAMCD = "TTDE", SAFFL = "Y",
                        TRT01A = rep(c("A", "B"), 3), AVAL = c(5, 8, 12, 3, 9, 15),
                        CNSR = c(0, 1, 0, 0, 1, 0))
  suppressPackageStartupMessages(eval(parse(text = part), e))
  expect_s3_class(e$plot, "ggplot")
  expect_identical(unname(e$pal), unname(e$tfl_colours("treatment", c("A", "B"))))
})

test_that("a number a figure prints is fmt_num(): the study's rounding, as a table's", {
  # (A3) the rate, count and forest steps; a study's program has fmt_num()
  # from its setup, a script of its own defines it (unless the setup has)
  for (t in c("bar_rate_ci", "bar_stacked")) {
    prog <- paste(tfl_fig_design_code(tfl_fig_template(t, group = "TRT01P"), "F1", setup = TRUE), collapse = "\n")
    expect_false(grepl("fmt_num <- function", prog, fixed = TRUE), info = t)
    own <- strsplit(paste(tfl_fig_design_code(tfl_fig_template(t, group = "TRT01P"), "F1"), collapse = "\n"), "\n")[[1]]
    expect_identical(sum(grepl('^if [(]!exists[(]"fmt_num"[)][)] fmt_num <- function', own)), 1L, info = t)
  }
  rate <- paste(tfl_fig_design_code(tfl_fig_template("bar_rate_ci", group = "TRT01P"), "F1", setup = TRUE), collapse = "\n")
  expect_match(rate, 'label = paste0(fmt_num(rate, 1), "% (", x, "/", n, ")")', fixed = TRUE)
  count <- paste(tfl_fig_design_code(tfl_fig_template("bar_stacked", group = "TRT01P"), "F1", setup = TRUE), collapse = "\n")
  expect_match(count, 'label = paste0(fmt_num(pct, 0), "%")', fixed = TRUE)
  fr <- tfl_fig_template("forest_hr", data = "ADTTE", param = "TTDE", pop = "SAFFL", group = "TRT01A",
                         subgroups = "SEX, AGEGR1", comparison = "Xanomeline High Dose")
  forest <- paste(tfl_fig_design_code(fr, "F1", setup = TRUE, save = FALSE), collapse = "\n")
  expect_match(forest, 'txt = ifelse(ok, paste0(fmt_num(estimate, 2), " (", fmt_num(conf.low, 2),', fixed = TRUE)
  expect_false(grepl("%.2f", forest, fixed = TRUE))
  # the programs are R that parses
  for (prog in list(rate, count, forest, paste(own, collapse = "\n"))) {
    expect_no_error(parse(text = prog))
  }
  # the script's own fmt_num() is the study helper's: 1/8 is 12.5%, "13" under "sas"
  a <- grep('^if [(]!exists[(]"fmt_num"[)][)]', own)
  b <- a - 1L + which(own[a:length(own)] == "}")[1L]
  e <- new.env(parent = globalenv())
  eval(parse(text = own[a:b]), e)
  old <- options(rtfreporter.rounding = "sas")
  on.exit(options(old), add = TRUE)
  expect_identical(e$fmt_num(100 * 1 / 8, 0), "13")
  expect_identical(e$fmt_num(NA, 1), "NE")
  # a time to event's helper and fmt_num() together: one comment, both defined
  both <- tflspec:::.fig_helper_used(c("df |> tte_time(\"months\")", "lab <- fmt_num(x, 1)"))
  expect_identical(both[2L], "# the study helpers tte_time(), fmt_num() (tfl_helpers_code()), unless the study's setup has them")
  expect_identical(sum(grepl("^if [(]!exists", both)), 2L)
  # a helper a used helper calls comes with it: fmt_pvalue() needs fmt_num()
  pv <- tflspec:::.fig_helper_used("lab <- fmt_pvalue(p)")
  expect_identical(sum(grepl('^if [(]!exists[(]"(fmt_num|fmt_pvalue)"', pv)), 2L)
  e2 <- new.env(parent = globalenv())
  eval(parse(text = pv), e2)
  expect_identical(e2$fmt_pvalue(c(0.0004, 0.0125)), c("<0.001", "0.013"))
  # a name that only ends with a helper's is not a call of it
  expect_length(tflspec:::.fig_helper_used("my_fmt_num(x, 1)"), 0L)
  # nor is a comment or a string that names one (the ARD figure's "# Input
  # ARD" line; bind_items()'s message naming tag_ard())
  expect_length(tflspec:::.fig_helper_used(c("# read with ard_value() / ard_stats()",
                                             "msg <- \"fmt_num(x, 1)\"")), 0L)
  bi <- tflspec:::.fig_helper_used("ard <- bind_items(a, b)")
  expect_identical(sum(grepl("^if [(]!exists", bi)), 1L)
  # every helper of the study's file, not a fixed few: the ones it calls
  # with it, in the file's order (order_arms(), one line, calls
  # order_levels(), which calls set_levels(), which calls codelist_codes())
  arms <- tflspec:::.fig_helper_used("adsl <- adsl |> order_arms()")
  expect_identical(arms[2L], paste("# the study helpers codelist_codes(), set_levels(), order_levels(), order_arms()",
                                   "(tfl_helpers_code()), unless the study's setup has them"))
  expect_identical(sum(grepl("^if [(]!exists", arms)), 4L)
  # each one's whole definition, as the file has it
  h <- new.env(parent = globalenv())
  eval(parse(text = tfl_helpers_code()), h)
  fns <- tflspec:::.study_helper_fns()
  expect_true(all(c("set_levels", "order_levels", "order_arms", "tte_time", "fmt_num",
                    "fmt_pvalue", "ard_value", "ard_stats", "read_ard") %in% fns))
  expect_setequal(fns, ls(h, all.names = TRUE))
  for (f in fns) {
    one <- tflspec:::.fig_helper_used(sprintf("y <- %s(x)", f))
    e3 <- new.env(parent = globalenv())
    # (the guard off: a function of that name elsewhere would skip it)
    eval(parse(text = sub("^if [(]!exists[(]\"[^\"]+\"[)][)] ", "", one)), e3)
    expect_true(f %in% ls(e3, all.names = TRUE), info = f)
    expect_true(all(ls(e3, all.names = TRUE) %in% fns), info = f)
    for (g in ls(e3, all.names = TRUE)) expect_identical(deparse(e3[[g]]), deparse(h[[g]]), info = g)
  }
  expect_true(tflspec:::.calls_helper("lab <- fmt_num(x, 1)"))
  expect_false(tflspec:::.calls_helper("lab <- sprintf(\"%.1f\", x)"))
})

test_that("a script of its own carries every study helper it calls: the arms' order_levels() (A3)", {
  lines <- function(x) unlist(strsplit(paste(as.character(x), collapse = "\n"), "\n", fixed = TRUE))
  d <- tfl_fig_template("mean_se", group = "TRT01P")
  own <- lines(tfl_fig_design_code(d, "F1", save = FALSE, name = "fig"))
  def <- function(f) sum(grepl(sprintf('^if [(]!exists[(]"%s"[)][)] %s <- function', f, f), own))
  expect_true(any(grepl("order_levels(", own, fixed = TRUE)))
  # order_levels() once, and set_levels(), which it calls
  expect_identical(def("order_levels"), 1L)
  expect_identical(def("set_levels"), 1L)
  expect_identical(sum(startsWith(own, "# the study helper")), 1L)
  expect_no_error(parse(text = own))
  # a study's program: its setup has them, none defined
  prog <- lines(tfl_fig_design_code(d, "F1", setup = TRUE, save = FALSE, name = "fig"))
  expect_false(any(grepl("^if [(]!exists", prog)))
  # it runs alone: no study helpers but its own
  skip_if_not_installed("ggplot2")
  e <- new.env(parent = globalenv())
  for (n in names(adam <- tfl_example_adam())) assign(tolower(n), adam[[n]], envir = e)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  suppressMessages(eval(parse(text = own), envir = e))
  expect_true(is.function(e$order_levels))
  # (ADSL's arm by its number, TRT01PN)
  expect_identical(levels(e$adlb_sub$TRT01P), c("Drug A", "Drug B"))
  expect_s3_class(e$fig, "ggplot")
})

test_that("a time's unit is a named constant of the setup, never a column", {
  expect_identical(tfl_time_unit_days(),
                   c(days_per_week = 7, days_per_month = 30.4375, days_per_year = 365.25))
  d <- tfl_fig_template("km_simple", data = "ADTTE", param = "TTDE", pop = "SAFFL", group = "TRT01A")
  # a study's program: the figure setup defines it (unless the study's setup has)
  prog <- strsplit(paste(tfl_fig_design_code(d, "F1", setup = TRUE), collapse = "\n"), "\n")[[1]]
  # (AVAL from its unit, AVALU: the study helper, one pipe step)
  expect_true(any(grepl("  tte_time(\"months\")", prog, fixed = TRUE)))
  expect_false(any(grepl("days_per_month", prog, fixed = TRUE)))
  expect_false(any(grepl("30.4375", prog, fixed = TRUE)))
  expect_true(any(grepl('if (!exists("days_per_month")) days_per_month <- 30.4375',
                        tfl_fig_setup_code(), fixed = TRUE)))
  # a script of its own: defines what it uses (AVAL from its unit: the
  # helper tte_time(), no constant; another time, in days: the constant;
  # the ordering helpers besides, as every script of its own)
  own <- strsplit(paste(tfl_fig_design_code(d, "F1"), collapse = "\n"), "\n")[[1]]
  own <- own[!grepl('^if [(]!exists[(]"(codelist_codes|set_levels|order_levels)"', own)]
  expect_identical(sum(grepl("^if [(]!exists", own)), 1L)
  expect_true(any(grepl("^if [(]!exists[(]\"tte_time\"[)][)] tte_time <- function", own)))
  d$data[[which(vapply(d$data, `[[`, "", "step") == "time_unit")]]$variable <- "ADY"
  own <- strsplit(paste(tfl_fig_design_code(d, "F1"), collapse = "\n"), "\n")[[1]]
  own <- own[!grepl('^if [(]!exists[(]"(codelist_codes|set_levels|order_levels)"', own)]
  expect_identical(sum(grepl("^if [(]!exists", own)), 1L)
  expect_true(any(grepl("^if [(]!exists[(]\"days_per_month\"[)][)]", own)))
  expect_false(any(grepl("tte_time", own, fixed = TRUE)))
  expect_true(any(grepl("mutate(ADY = ADY / days_per_month)", own, fixed = TRUE)))
  # the study's value wins
  e <- new.env()
  e$days_per_month <- 30
  eval(parse(text = .fig_unit_guard()), envir = e)
  expect_identical(e$days_per_month, 30)
  expect_identical(e$days_per_year, 365.25)
})

test_that("a figure's own colours over the palette, and its legend title (#577)", {
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADSL")),
    plot = list(colour_by = "TRT01A", colours = list(`Drug B` = "grey40"), legend_title = "Treatment"),
    layers = list(list(layer = "point", x = "TRT01A", y = "TRTDURD", colour = "TRT01A")))
  code <- tfl_fig_design_code(d, save = FALSE)
  txt <- paste(code, collapse = "
")
  expect_match(txt, "pal <- replace(setNames(rep_len(", fixed = TRUE)
  expect_match(txt, "lv), c(\"Drug B\"), c(\"grey40\"))", fixed = TRUE)
  expect_match(txt, "labs(colour = \"Treatment\", fill = \"Treatment\"", fixed = TRUE)
  expect_false(grepl("legend.title = element_blank()", txt, fixed = TRUE))
  r <- run_design(d)
  expect_true(r$png)
  # with the setup, and as text
  d$plot$colours <- "Drug B = grey40, Drug A = #D62728"
  code <- paste(tfl_fig_design_code(d, setup = TRUE, save = FALSE), collapse = "
")
  expect_match(code, "pal <- replace(tfl_colours(\"treatment\", levels(factor(adsl_sub$TRT01A))), c(\"Drug B\", \"Drug A\"), c(\"grey40\", \"#D62728\"))",
               fixed = TRUE)
  x <- tfl_fig_content_from_design(d)
  fns <- vapply(x$content, function(it) it$fn, "")
  expect_true("scale_colour_manual" %in% fns)
  expect_false("scale_colour_tfl" %in% fns)
  # without: as before
  d$plot$colours <- NULL
  d$plot$legend_title <- NULL
  expect_match(paste(tfl_fig_design_code(d, save = FALSE), collapse = "
"),
               "legend.title = element_blank()", fixed = TRUE)
})

test_that("ref_label with facet_by is drawn inside each panel, and asks for no margin", {
  mk <- function(facet) {
    plot <- list(x_label = "Visit", y_label = "Value")
    if (facet) plot$facet_by <- "TRT"
    tfl_fig_design(
      data = list(list(step = "code", code = paste0(
        'df <- data.frame(TRT = rep(c("A", "B"), each = 3), x = rep(1:3, 2), ',
        'y = c(1, 2, 3, 2, 3, 4))'))),
      plot = plot,
      layers = list(list(layer = "geom", geom = "geom_line", data = "df", aes = "x = x | y = y"),
                    list(layer = "ref_label", y = "2.5", label = "Limit")))
  }
  lines_of <- function(facet) tfl_fig_design_code(mk(facet), save = FALSE, name = "fig")
  # (the code is wrapped over lines: its words, one space apart)
  flat <- function(facet) gsub("[[:space:]]+", " ", paste(lines_of(facet), collapse = " "))
  code_f <- flat(TRUE)
  expect_match(code_f, "aes(x = -Inf, y = y, label = label), hjust = -0.1, vjust = -0.5,",
               fixed = TRUE)
  expect_false(grepl("margin(5.5, 50", code_f, fixed = TRUE))
  expect_false(grepl("clip", code_f, fixed = TRUE))

  # without panels: the label right of the panel, the margin and clip off, as before
  code_n <- flat(FALSE)
  expect_match(code_n, "annotate(\"text\", x = Inf, y = 2.5, label = \"Limit\", hjust = -0.3",
               fixed = TRUE)
  expect_match(code_n, "margin(5.5, 50, 5.5, 5.5)", fixed = TRUE)
  expect_match(code_n, "coord_cartesian(clip = \"off\")", fixed = TRUE)

  # drawn: the label row sits at the left edge of every panel
  env <- new.env()
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  suppressMessages(eval(parse(text = lines_of(TRUE)), envir = env))
  b <- ggplot2::ggplot_build(env$fig)
  rows <- Filter(function(d) "label" %in% names(d), b$data)
  expect_length(rows, 1L)
  rows <- rows[[1L]]
  expect_equal(nrow(rows), 2L)
  expect_setequal(as.character(rows$PANEL), c("1", "2"))
  expect_true(all(rows$x == -Inf))
  expect_true(all(rows$y == 2.5))
})

test_that("plot.colours as text: split at the commas between pairs, and a call is said not to be a colour", {
  own <- function(x) .fig_own_colours(list(colours = x))
  expect_identical(own("CR = darkgreen, PD = #D62728"), c(CR = "darkgreen", PD = "#D62728"))
  expect_identical(own("CR=darkgreen,PD=#D62728"), c(CR = "darkgreen", PD = "#D62728"))
  expect_error(own("X = rgb(1,0,0), Y = blue"), "`rgb(1,0,0)` is a call, not a colour R reads", fixed = TRUE)
  expect_error(own("X = blue, Y = rgb(1,0,0)"), "hex code")
  expect_error(own("X = blue, Y"), "value = colour pairs")
  expect_identical(own(c(X = "blue")), c(X = "blue"))
})

test_that("a report's content code carries no study helper: the program's setup has them (A3)", {
  code <- c("adlb_sub <- order_levels(adlb_sub)", "lab <- fmt_num(x, 2)")
  expect_true(length(tflspec:::.fig_script_head(code)) > 0L)
  expect_length(tflspec:::.fig_script_head(code, setup = TRUE), 0L)
  old <- options(tflspec.fig_in_program = TRUE)
  on.exit(options(old), add = TRUE)
  expect_length(tflspec:::.fig_script_head(code), 0L)
})

test_that("a script of its own names the packages its carried helpers call", {
  needs_line <- function(code) grep("^# also needs:", code, value = TRUE)
  # fmt_num() calls rtfreporter::round_num(): the script needs rtfreporter
  own <- tfl_fig_design_code(tfl_fig_template("bar_rate_ci", group = "TRT01P"), "F1")
  expect_true(any(grepl("rtfreporter", needs_line(own))))
  expect_true("rtfreporter" %in% attr(own, "needs"))
  # the helpers' own base packages and the attached ones are not named
  nl <- needs_line(own)
  expect_length(nl, 1L)
  expect_false(grepl("\\b(stats|utils|tools|base|ggplot2)\\b", nl))
  # what carries fmt_num() carries its need (fmt_pvalue(), ard_value())
  for (f in c("fmt_pvalue(p)", "ard_value(a, 1)")) {
    expect_true("rtfreporter" %in% tflspec:::.fig_script_needs(paste0("x <- ", f)), info = f)
  }
  # a pkg:: in the program's own comment adds nothing: no helper is carried
  expect_length(tflspec:::.fig_script_needs("x <- 1 # rtfreporter::round_num"), 0L)
  expect_length(tflspec:::.fig_script_needs("order_arms(d) # rtfreporter::round_num"), 0L)
  # a carried helper names exactly the packages its code calls with pkg::
  # (fmt_num() calls rtfreporter::round_num() and nothing else)
  expect_true("rtfreporter" %in% tflspec:::.fig_script_needs("fmt_num(x, 1)"))
  # tag_ard() calls dplyr::mutate(); the cards:: of the comment above it in
  # the helper file (cards::ard_stack()) and the ard_summary() of its error
  # text are not calls
  expect_true("dplyr" %in% tflspec:::.fig_script_needs("tag_ard(a, 'R', 'A')"))
  # a helper that calls no other package needs none (order_arms() -> order_levels())
  expect_false(any(c(tflspec:::.r_base_pkgs) %in% tflspec:::.fig_script_needs("order_arms(d)")))
  expect_identical(tflspec:::.fig_script_needs("x <- 1", needs = "ggtext"), "ggtext")
  # a package the script attaches is not named twice
  expect_false("rtfreporter" %in% tflspec:::.fig_script_needs("fmt_num(x, 1)", libs = "rtfreporter"))
  # with the study's setup, nothing changes
  st <- tfl_fig_design_code(tfl_fig_template("bar_rate_ci", group = "TRT01P"), "F1", setup = TRUE)
  expect_length(needs_line(st), 0L)
  expect_false("rtfreporter" %in% attr(st, "needs"))
  expect_identical(tflspec:::.fig_script_needs("fmt_num(x, 1)", setup = TRUE), character())
})

test_that("a script of its own names every package its own code calls as pkg::fn", {
  # forest_or's own code calls rtfreporter::round_num(): the script needs it
  expect_identical(tflspec:::.fig_script_needs("lab <- rtfreporter::round_num(x, 1)"),
                   "rtfreporter")
  # pkg:::fn is a call of the package too
  expect_identical(tflspec:::.fig_script_needs("y <- scales:::percent(x)"), "scales")
  # R's base packages are installed with R: not named
  expect_identical(tflspec:::.fig_script_needs("m <- stats::median(x)"), character())
  expect_identical(tflspec:::.fig_script_needs(c("m <- stats::median(x)",
                                                 "g <- grid::unit(1, 'mm')",
                                                 "v <- utils::head(x)")), character())
  # a package the script attaches with library() is not named
  expect_identical(tflspec:::.fig_script_needs("y <- scales::percent(x)", libs = "scales"),
                   character())
  # a pkg:: in a comment or a string is not a call
  expect_length(tflspec:::.fig_script_needs("x <- 1 # scales::percent"), 0L)
  expect_length(tflspec:::.fig_script_needs("x <- 'scales::percent(y)'"), 0L)
  # the own code's packages, the writer's and the carried helpers' together, once
  expect_setequal(tflspec:::.fig_script_needs(c("a <- rtfreporter::round_num(x, 1)",
                                                "b <- fmt_num(x, 1)"), needs = "ggtext"),
                  c("ggtext", "rtfreporter"))
  # code in pieces that do not parse together is read piece by piece
  expect_identical(tflspec:::.fig_script_needs(c("p <- f(", "y <- scales::percent(x)")),
                   "scales")
  # with the study's setup, or in a program: what the writer recorded, as is
  expect_identical(tflspec:::.fig_script_needs("a <- rtfreporter::round_num(x, 1)", setup = TRUE),
                   character())
  old <- options(tflspec.fig_in_program = TRUE)
  on.exit(options(old), add = TRUE)
  expect_identical(tflspec:::.fig_script_needs("a <- rtfreporter::round_num(x, 1)",
                                               needs = "ggtext"), "ggtext")
})

test_that("a template whose own code calls rtfreporter:: names it in its script", {
  needs_line <- function(code) grep("^# also needs:", unlist(strsplit(code, "\n", fixed = TRUE)), value = TRUE)
  for (tpl in c("forest_or", "butterfly_soc", "butterfly_pt")) {
    own <- tfl_fig_design_code(tfl_fig_template(tpl), "F1")
    expect_true(any(grepl("rtfreporter", needs_line(own))), info = tpl)
    st <- tfl_fig_design_code(tfl_fig_template(tpl), "F1", setup = TRUE)
    # the code is there, only the line is gone
    expect_true(any(grepl("rtfreporter::round_num", st, fixed = TRUE)), info = tpl)
    expect_length(needs_line(st), 0L)
  }
})

test_that("the in-program option works end to end: a report's content code has no helper definitions (A3)", {
  old0 <- options(tflspec.fig_in_program = NULL)
  on.exit(options(old0), add = TRUE)
  expect_null(getOption("tflspec.fig_in_program"))
  design <- tfl_fig_template("bar_rate_ci", group = "TRT01P")
  # outside a program the design's own script carries the helper it calls
  own <- tfl_fig_design_code(design, "F1")
  expect_true(any(grepl("if (!exists(\"fmt_num\")) ", own, fixed = TRUE)))
  out <- list(id = "F1", template = "bar_rate_ci")
  code <- tflspec:::.content_fig_code_out(out)
  # the program's setup has the helpers: no definition lines, and the option is back
  expect_false(any(grepl("if (!exists(", code, fixed = TRUE)))
  expect_null(getOption("tflspec.fig_in_program"))
  # with the option set, the needs line adds no helper package
  old <- options(tflspec.fig_in_program = TRUE)
  on.exit(options(old), add = TRUE)
  expect_identical(tflspec:::.fig_script_needs("fmt_num(x, 1)"), character())
  expect_identical(tflspec:::.fig_script_needs("fmt_num(x, 1)", needs = "ggtext"), "ggtext")
})

test_that("a figure's group has no default: a template that splits the arms stops, naming itself (PRINCIPLES 1.14)", {
  # every preset that draws the arms: its name, the argument, what it is
  tp <- tfl_fig_templates()
  for (t in tp$template[tp$parts]) {
    if (is.null(fig_group_of(t))) next
    expect_error(tfl_fig_template(t), sprintf("%s: `group` is required (the variable that splits the arms).", t),
                 fixed = TRUE, info = t)
    expect_error(tfl_fig_template(t, group = ""), "`group` is required", fixed = TRUE, info = t)
    expect_s3_class(tfl_fig_template(t, group = fig_group_of(t)), "tfl_fig_design")
  }
  # the ones that draw no arms need none
  for (t in c("km_single_arm", "individual_spider", "waterfall_response", "swimmer_full")) {
    expect_s3_class(tfl_fig_template(t), "tfl_fig_design")
  }
  # the template SPECs: `group` required, no default (like km's `by`)
  for (nm in c("mean_se", "mean_ci", "forest_hr")) {
    g <- tfl_template_params(nm)$group
    expect_true(isTRUE(g$required), info = nm)
    expect_null(g$default, info = nm)
    expect_error(tfl_template_expand(nm, list(param = "SYSBP", population = "SAF")), "group", info = nm)
  }
  # the functions: the same stop, before the data
  expect_error(tpl_mean_se(advs, param = "SYSBP"), "mean_se: `group` is required", fixed = TRUE)
  expect_error(tpl_mean_ci(advs, param = "SYSBP"), "mean_ci: `group` is required", fixed = TRUE)
  expect_error(tpl_forest_hr(data.frame()), "forest_hr: `group` is required", fixed = TRUE)
})

# ---- axis_title_width (tflspec#603-4, canon 11-28) ---------------------------------

fd_t90 <- "Mean Change from Baseline in Diastolic Blood Pressure (mmHg) with 95% Confidence Intervals"
fd_t90_50 <- "Mean Change from Baseline in Diastolic Blood\nPressure (mmHg) with 95% Confidence Intervals"

fd_axis <- function(...) {
  tfl_fig_design(
    data = list(list(step = "read", dataset = "ADSL")),
    plot = list(...),
    layers = list(list(layer = "point", x = "TRTDURD", y = "TRTDURD")))
}
# the code as one text; a title as the program writes it (its "\n" escaped)
fd_code <- function(d) paste(tfl_fig_design_code(d, save = FALSE), collapse = "\n")
fd_lit <- function(x) encodeString(x, quote = "\"")

test_that("a designed figure writes an axis title longer than axis_title_width wrapped", {
  # the figure style's width (50): the 90-character title on two lines, in the code
  code <- fd_code(fd_axis(x_label = "Age (years)", y_label = fd_t90))
  expect_true(grepl(fd_lit(fd_t90_50), code, fixed = TRUE))
  expect_false(grepl(fd_lit(fd_t90), code, fixed = TRUE))
  expect_true(grepl(fd_lit("Age (years)"), code, fixed = TRUE))
  # a 30-character title: as written
  t30 <- "Mean Change from Baseline (mm)"
  code <- fd_code(fd_axis(y_label = t30))
  expect_true(grepl(fd_lit(t30), code, fixed = TRUE))
  # the figure's own width; `!unset`: one line
  code <- fd_code(fd_axis(y_label = fd_t90, axis_title_width = 30))
  expect_true(grepl(fd_lit(tflspec:::.fig_wrap_title(fd_t90, 30)), code, fixed = TRUE))
  d_unset <- fd_axis(y_label = fd_t90, axis_title_width = tfl_unset())
  code <- fd_code(d_unset)
  expect_true(grepl(fd_lit(fd_t90), code, fixed = TRUE))
  # (the title, not the figure's title, is wrapped)
  code <- fd_code(fd_axis(title = fd_t90))
  expect_true(grepl(fd_lit(fd_t90), code, fixed = TRUE))
  # the check takes the setting, `!unset` too
  adam <- tfl_example_adam()
  n0 <- nrow(tfl_check_fig_design(fd_axis(y_label = fd_t90), adam))
  expect_identical(nrow(tfl_check_fig_design(d_unset, adam)), n0)
  expect_identical(nrow(tfl_check_fig_design(fd_axis(y_label = fd_t90, axis_title_width = 30), adam)), n0)
  expect_gt(nrow(tfl_check_fig_design(fd_axis(axis_title_width = "wide"), adam)), n0)
  # (a number that is no width -- 0, 2.5 -- is caught by the check, as the
  # program would stop)
  expect_gt(nrow(tfl_check_fig_design(fd_axis(axis_title_width = 0), adam)), n0)
  expect_gt(nrow(tfl_check_fig_design(fd_axis(axis_title_width = 2.5), adam)), n0)
  # a design file keeps the `!unset`: written as the tag, read back as it
  f <- tempfile(fileext = ".yml")
  tfl_write_fig_design(d_unset, f)
  expect_true(any(grepl("axis_title_width: !unset", readLines(f), fixed = TRUE)))
  back <- tfl_read_fig_design(f)
  expect_s3_class(back$plot$axis_title_width, "tfl_unset")
  expect_true(grepl(fd_lit(fd_t90), fd_code(back), fixed = TRUE))
  # a company's figure style: its width
  s <- tfl_fig_style()
  s$settings$value[s$settings$key == "axis_title_width"] <- "40"
  old <- options(tflspec.fig_style = s)
  on.exit(options(old), add = TRUE)
  code <- fd_code(fd_axis(y_label = fd_t90))
  expect_true(grepl(fd_lit(tflspec:::.fig_wrap_title(fd_t90, 40)), code, fixed = TRUE))
})

test_that("a designed figure's wrapped axis title is the drawn one", {
  skip_if_not_installed("ggplot2", "3.5.2")
  r <- run_design(fd_axis(y_label = fd_t90))
  expect_identical(ggplot2::get_labs(r$fig)$y, fd_t90_50)
  r <- run_design(fd_axis(y_label = fd_t90, axis_title_width = tfl_unset()))
  expect_identical(ggplot2::get_labs(r$fig)$y, fd_t90)
})

test_that("the km template's axis_title_width goes to its design; NULL / !unset: no wrap", {
  x <- function(...) tfl_template_expand("km", list(param = "PFS", by = "TRT01A", ...))$plot
  expect_identical(x()$axis_title_width, 50)
  expect_identical(x(axis_title_width = 30)$axis_title_width, 30)
  expect_s3_class(x(axis_title_width = tfl_unset())$axis_title_width, "tfl_unset")
  # the study standard's key, its fallback the template's 50
  p <- tfl_template_params("km")$axis_title_width
  expect_identical(p$study, "figure_style.axis_title_width")
  expect_identical(p$default, 50)
  # (the template's settings with none over them: `!unset` kept)
  base <- list(y_label = "Y", axis_title_width = tfl_unset())
  expect_identical(tflspec:::.tpl_plot_over(base, NULL), base)
  expect_identical(tflspec:::.tpl_plot_over(base, list(axis_title_width = 40))$axis_title_width, 40)
  # a report's `plot:` saying `!unset` over the template's width: no wrap, kept
  base50 <- list(y_label = "Y", axis_title_width = 50)
  expect_s3_class(tflspec:::.tpl_plot_over(base50, list(axis_title_width = tfl_unset()))$axis_title_width,
                  "tfl_unset")
  expect_s3_class(x(plot = list(axis_title_width = tfl_unset()))$axis_title_width, "tfl_unset")
  # (another setting's `!unset` removes it, as before)
  expect_null(tflspec:::.tpl_plot_over(base50, list(y_label = tfl_unset()))$y_label)
  # drawn: tfl_tpl()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("ggplot2", "3.5.2")
  adtte <- tflspec::adtte
  df <- adtte[adtte$PARAMCD == "PFS", ]
  y_of <- function(...) ggplot2::get_labs(tfl_tpl(df, "km", by = TRT01A, risk_table = FALSE,
                                                  y_label = fd_t90, ...))$y
  expect_identical(y_of(), fd_t90_50)
  expect_identical(y_of(axis_title_width = tfl_unset()), fd_t90)
})

test_that("a rate / count step's label is its label_cell, one line by default (#603 6a)", {
  d <- tfl_fig_template("bar_rate_ci", group = "TRT01P")
  i <- which(vapply(d$data, function(s) identical(s$step, "rate"), NA))
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  expect_match(code, 'label = paste0(fmt_num(rate, 1), "% (", x, "/", n, ")")', fixed = TRUE)
  expect_false(grepl("\\n(", code, fixed = TRUE))
  # its own: other columns, a line break (written as \n in the string)
  d$data[[i]]$label_cell <- "{rate:.1f}%\n({lcl:.1f}, {ucl:.1f}) n={n}"
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  expect_match(code, paste0('label = paste0(fmt_num(rate, 1), "%\\n(", fmt_num(lcl, 1), ", ", ',
                            'fmt_num(ucl, 1), ") n=", n)'), fixed = TRUE)
  expect_false("label_cell" %in% tfl_check_fig_design(d)$field)
  # a token that is no column of the step stops, naming the step and its columns
  d$data[[i]]$label_cell <- "{rate:.1f}% ({N})"
  msg <- tryCatch(tfl_fig_design_code(d), error = conditionMessage)
  expect_match(msg, "label_cell of the rate step (rt): {N} is not one of its columns", fixed = TRUE)
  expect_match(msg, "{rate}, {x}, {n}, {lcl}, {ucl}", fixed = TRUE)
  chk <- tfl_check_fig_design(d)
  expect_identical(chk$problem[chk$field == "label_cell"], msg)
  # a format other than .<decimals>f
  d$data[[i]]$label_cell <- "{rate:d}"
  expect_error(tfl_fig_design_code(d), "a format is .<decimals>f", fixed = TRUE)
  # count: its percent, no decimal
  d <- tfl_fig_template("bar_stacked", group = "TRT01P")
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  expect_match(code, 'label = paste0(fmt_num(pct, 0), "%")', fixed = TRUE)
  j <- which(vapply(d$data, function(s) identical(s$step, "count"), NA))
  d$data[[j]]$label_cell <- "{n} ({pct:.1f}%)"
  code <- paste(tfl_fig_design_code(d), collapse = "\n")
  expect_match(code, 'label = paste0(n, " (", fmt_num(pct, 1), "%)")', fixed = TRUE)
  d$data[[j]]$label_cell <- "{x}"
  expect_error(tfl_fig_design_code(d), "label_cell of the count step (ct): {x}", fixed = TRUE)
  # the default made from numbers: one line, the study's rounding
  e <- new.env()
  eval(parse(text = tfl_helpers_code()), e)
  e$rate <- 100 * 5 / 15
  e$x <- 5L
  e$n <- 15L
  expect_identical(eval(parse(text = .cell_template_code(.fig_label_cells$rate, .fig_label_cols$rate,
                                                         "the rate step")), e),
                   "33.3% (5/15)")
  # the steps' catalog: the field and its default
  p <- tfl_fig_parts()
  expect_identical(p$default[p$piece == "rate" & p$field == "label_cell"], "{rate:.1f}% ({x}/{n})")
  expect_identical(p$default[p$piece == "count" & p$field == "label_cell"], "{pct:.0f}%")
  # a typed \n (a one-line field) is a line break as a real one is
  expect_identical(.cell_template_code("{pct:.0f}%\\n{n}", c("pct", "n"), "the count step"),
                   .cell_template_code("{pct:.0f}%\n{n}", c("pct", "n"), "the count step"))
})
