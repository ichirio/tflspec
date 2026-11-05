# A layer's aesthetics (inst/spec/schema.yml's `aes:`, R/spec_figure.R):
# the names a mapping takes, those a layer needs (ggplot2's
# required_aes, "x|y" one or the other), and a plot's layers checked for
# them.

aes_problems <- function(fn, args) tfl_schema_check(tfl_item(fn, args))
aes_plot <- function(...) tfl_plot("p", list(...))

test_that("no key of a card is a YAML boolean (a bare y: is TRUE)", {
  ent <- tfl_schema()
  for (nm in names(ent)) {
    for (k in c("aes", "args")) {
      expect_false(any(names(ent[[nm]][[k]]) %in% c("TRUE", "FALSE")), info = paste(nm, k))
    }
  }
  expect_identical(tfl_schema("geom_line")$aes_required, c("x", "y"))
  expect_true("y" %in% tfl_schema("geom_point")$aes)
  expect_false("TRUE" %in% tfl_schema("geom_point")$aes)
})

test_that("a card marks the aesthetics ggplot2 needs, and their alternatives", {
  skip_if_not(utils::packageVersion("ggplot2") >= "4.0.0")
  ent <- tfl_schema()
  for (nm in names(ent)) {
    e <- ent[[nm]]
    if (!identical(e$figure, "layer") || !identical(e$package %||% "ggplot2", "ggplot2") ||
        !grepl("^(geom|stat)_", nm)) next
    req <- .schema_layer_required(nm, list())
    s <- tfl_schema(nm)
    for (g in req) {
      alts <- strsplit(g, "|", fixed = TRUE)[[1L]]
      m <- intersect(s$aes_required, alts)
      expect_true(length(m) > 0L, info = paste(nm, g))
      if (length(m) == 1L && length(alts) > 1L) {
        expect_setequal(c(m, s$aes_or[[m]]), alts)
      }
    }
    # nothing marked that ggplot2 does not need
    expect_true(all(s$aes_required %in% unlist(strsplit(req, "|", fixed = TRUE))), info = nm)
  }
  # an error bar across (a forest plot's): y, xmin, xmax
  s <- tfl_schema("geom_errorbar")
  expect_identical(s$aes_or, list(x = "y", ymin = "xmin", ymax = "xmax"))
})

test_that("a layer's mapping takes its aesthetics; an unknown one warns", {
  expect_identical(nrow(aes_problems("geom_line", list(mapping = list(x = "AVISITN", y = "AVAL",
                                                                      colour = "TRTA")))), 0L)
  # ggplot2's other spellings
  expect_identical(nrow(aes_problems("geom_point", list(mapping = list(color = "TRTA", pch = "TRTA")))), 0L)
  p <- aes_problems("geom_line", list(mapping = list(x = "AVISITN", colr = "TRTA")))
  expect_identical(p$level, "warning")
  expect_identical(p$arg, "mapping")
  expect_match(p$message, "geom_line has no aesthetic `colr`")
  # a stat's own aesthetics (stat_summary_bin's weight) and a geom's drawn
  # by another stat
  expect_identical(nrow(aes_problems("geom_pointrange", list(stat = "summary", fun.data = "mean_se",
                                                             mapping = list(x = "AVISITN", y = "CHG")))), 0L)
  # an extension's layer: its own aesthetics
  skip_if_not_installed("ggrepel")
  expect_identical(nrow(aes_problems("geom_text_repel", list(mapping = list(label = "USUBJID")))), 0L)
  expect_match(aes_problems("geom_text_repel", list(mapping = list(lable = "USUBJID")))$message,
               "no aesthetic `lable`")
})

test_that("a plot's layer missing an aesthetic it needs warns", {
  ok <- aes_plot(tfl_item("ggplot", list(mapping = list(x = "AVISITN", y = "MEAN", colour = "TRTA"))),
                 tfl_item("geom_errorbar", list(mapping = list(ymin = "LO", ymax = "HI"), width = 0.2)),
                 tfl_item("geom_hline", list(yintercept = 0)),
                 tfl_item("geom_step", list(stat = "ecdf")),
                 tfl_item("geom_pointrange", list(stat = "summary", fun.data = "mean_se")))
  expect_identical(nrow(tfl_schema_check(ok)), 0L)
  # a forest plot's bars across: y, xmin, xmax
  forest <- aes_plot(tfl_item("ggplot", list(mapping = list(x = "EST", y = "SUBGRP"))),
                     tfl_item("geom_errorbar", list(mapping = list(xmin = "LCL", xmax = "UCL"))),
                     tfl_item("geom_vline", list(xintercept = 1)))
  expect_identical(nrow(tfl_schema_check(forest)), 0L)
  bad <- aes_plot(tfl_item("ggplot", list(mapping = list(x = "AVISITN", y = "MEAN"))),
                  tfl_item("geom_errorbar", list(mapping = list(ymin = "LO"))),
                  tfl_item("geom_vline", list()),
                  tfl_item("geom_text", list(inherit.aes = FALSE, mapping = list(x = "X", y = "Y"))))
  p <- tfl_schema_check(bad)
  expect_identical(p$level, rep("warning", 3L))
  expect_identical(p$fn, c("geom_errorbar", "geom_vline", "geom_text"))
  expect_match(p$message[1L], "needs the aesthetic `ymax` (or `xmax`)", fixed = TRUE)
  expect_match(p$message[2L], "needs the aesthetic `xintercept`", fixed = TRUE)
  expect_match(p$message[3L], "`label`.*inherit.aes: false")
  # the stat's (stat_ecdf needs x or y, mapped nowhere)
  p <- tfl_schema_check(aes_plot(tfl_item("ggplot", list()), tfl_item("stat_ecdf", list())))
  expect_match(p$message, "stat_ecdf needs the aesthetic `x` (or `y`)", fixed = TRUE)
  # a figure's own geom layer: its aes and params
  p <- tfl_schema_check(aes_plot(tfl_item("ggplot", list()),
                                 tfl_item("geom", list(geom = "geom_segment",
                                                       aes = list(x = "A", y = "B", xend = "C")))))
  expect_identical(nrow(p), 0L)
})

test_that("a plot whose mapping is not known here is not judged", {
  # a mapping written as R
  p <- aes_plot(tfl_item("ggplot", list(mapping = tfl_r("aes(x = AVISITN, y = AVAL)"))),
                tfl_item("geom_line", list()))
  expect_false(any(grepl("needs the aesthetic", tfl_schema_check(p)$message)))
  # a plot not begun by ggplot() (ggsurvfit's own mapping)
  p <- aes_plot(tfl_item("ggsurvfit", list(x = "fit")), tfl_item("geom_step", list()))
  expect_false(any(grepl("needs the aesthetic", tfl_schema_check(p)$message)))
})

# ---- a mapping's value: a column, or an expression of the form (R/fig_aes.R) ----

test_that("a mapping's expression of the form is written as it is, and read back as its text", {
  it <- tfl_item("geom_bar", list(mapping = list(x = "factor(AVISITN)", y = "after_stat(count / sum(count))",
                                                 fill = "TRTA")))
  e <- .fig_item_lang(it)
  expect_identical(.fig_deparse(e),
                   "geom_bar(mapping = aes(x = factor(AVISITN), y = after_stat(count/sum(count)), fill = TRTA))")
  back <- .fig_lang_item(e)
  expect_identical(back$args$mapping$x, "factor(AVISITN)")
  expect_identical(back$args$mapping$y, "after_stat(count/sum(count))")
  expect_identical(back$args$mapping$fill, "TRTA")
  # a waterfall's bars, a stage(), a colour after its scale
  for (txt in c("reorder(USUBJID, -PCHG)", "stage(AVAL, after_stat = x)",
                "after_scale(alpha(colour, 0.4))", "factor(SEX, levels = c(\"F\", \"M\"))",
                "log10(AVAL)", "interaction(TRTA, AVISIT)", "mean - se", "-PCHG",
                "paste0(n, \" (\", pct, \"%)\")", "sprintf(\"%.1f\", AVAL)", "\"Overall\"")) {
    w <- .fig_value_lang(list(x = txt), "mapping")
    expect_identical(.fig_deparse(w[[2L]]), .fig_deparse(str2lang(txt)), info = txt)
    expect_identical(.fig_lang_value(w, "mapping")$x, .fig_deparse(str2lang(txt)), info = txt)
  }
  # any other R stays R, both ways; a column is its name
  r <- .fig_lang_value(quote(aes(y = dplyr::if_else(AVAL > 0, 1, 2), x = AVISITN)), "mapping")
  expect_s3_class(r$y, "tfl_r")
  expect_identical(r$x, "AVISITN")
  expect_identical(.fig_deparse(.fig_value_lang(list(y = tfl_r("dplyr::if_else(a, 1, 2)")), "mapping")),
                   "aes(y = dplyr::if_else(a, 1, 2))")
  # a number is a constant (group = 1), both ways
  expect_identical(.fig_deparse(.fig_value_lang(list(group = 1L), "mapping")), "aes(group = 1)")
  expect_identical(.fig_lang_value(quote(aes(group = 1)), "mapping")$group, 1)
  # a text constant (a legend's label) in its quotes, both ways
  expect_identical(.fig_lang_value(quote(aes(colour = "Overall")), "mapping")$colour, "\"Overall\"")
  expect_identical(.fig_deparse(.fig_value_lang(list(colour = "\"Overall\""), "mapping")),
                   "aes(colour = \"Overall\")")
  # a column's name in backticks is the column
  expect_identical(.fig_deparse(.fig_value_lang(list(x = "`a b`"), "mapping")), "aes(x = `a b`)")
  # a text of neither: a column of that name, as before
  expect_identical(.fig_deparse(.fig_value_lang(list(x = "my col"), "mapping")), "aes(x = `my col`)")
  expect_identical(.fig_deparse(.fig_value_lang(list(x = "system(\"ls\")"), "mapping")),
                   "aes(x = `system(\"ls\")`)")
})

test_that("the form: calls of the mapping's functions, of columns, constants and arithmetic", {
  ok <- c("factor(A)", "as.numeric(A)", "after_stat(density)", "after_stat(count/max(count))",
          "reorder(A, -B)", "stage(A, after_scale = alpha(fill, 0.5))", "rev(factor(A))")
  ok <- c(ok, "A + B", "mean - 1.96 * se", "(A + B)/2", "ifelse(A, 1, 2)", "factor(toupper(A))")
  no <- c("factor(A)[1]", "after_stat(get(\"x\"))", "function(x) x", "A$B",
          "dplyr::if_else(A, 1, 2)", "Sys.Date() - ADT", "ifelse(A, system(\"ls\"), 2)")
  for (t in ok) expect_true(.fig_aes_form(str2lang(t)), info = t)
  for (t in no) expect_false(.fig_aes_form(str2lang(t)), info = t)
  expect_identical(.fig_aes_stat_vars(quote(after_stat(count / sum(count)))), "count")
  expect_identical(.fig_aes_stat_vars(quote(stage(AVAL, after_stat = density))), "density")
  expect_identical(.fig_aes_stat_vars(quote(factor(A))), character())
})

test_that("the check: a text of neither form warns; an after_stat()'s variable is the stat's", {
  p <- aes_problems("geom_histogram", list(mapping = list(x = "AVAL", y = "after_stat(density)")))
  expect_identical(nrow(p), 0L)
  p <- aes_problems("geom_histogram", list(mapping = list(x = "AVAL", y = "after_stat(densty)")))
  expect_true(any(grepl("computes no variable `densty`", p$message)))
  expect_identical(unique(p$level), "warning")
  # the stat it is given: an ECDF's
  expect_identical(nrow(aes_problems("geom_step", list(stat = "ecdf",
                                                        mapping = list(x = "CHG", y = "after_stat(ecdf)")))), 0L)
  expect_true(any(grepl("`count`", aes_problems("geom_step", list(
    stat = "ecdf", mapping = list(x = "CHG", y = "after_stat(count)")))$message)))
  # stat_*() itself; an aesthetic after the stat is known
  expect_identical(nrow(aes_problems("stat_count", list(mapping = list(x = "AESOC", y = "after_stat(prop)",
                                                                       group = 1)))), 0L)
  expect_identical(nrow(aes_problems("geom_bar", list(mapping = list(x = "A", label = "after_stat(x)")))), 1L)
  p <- aes_problems("geom_point", list(mapping = list(x = "factor(AVISITN", y = "AVAL")))
  expect_true(any(grepl("neither a column's name nor an expression", p$message)))
  # a stat whose variables are not listed is not judged
  expect_identical(nrow(aes_problems("geom_point", list(mapping = list(x = "A", y = "after_stat(zz)")))), 0L)
})

test_that("the variables listed for a stat are those its layer's data has", {
  skip_if_not(utils::packageVersion("ggplot2") >= "4.0.0")
  d <- data.frame(x = c(1, 2, 2, 3, 3, 3, 4, 5), y = c(2, 3, 1, 4, 2, 5, 3, 4), g = rep(c("a", "b"), 4))
  cases <- list(StatCount = ggplot2::geom_bar(), StatBin = ggplot2::geom_histogram(bins = 3),
                StatDensity = ggplot2::geom_density(), StatEcdf = ggplot2::stat_ecdf(),
                StatSummary = ggplot2::stat_summary(fun.data = ggplot2::mean_se),
                StatBoxplot = ggplot2::geom_boxplot(ggplot2::aes(group = g)),
                StatYdensity = ggplot2::geom_violin(ggplot2::aes(group = g)),
                StatSmooth = ggplot2::geom_smooth(method = "lm", formula = y ~ x),
                StatSum = ggplot2::geom_count(), StatBin2d = ggplot2::geom_bin_2d(bins = 3))
  for (cls in names(cases)) {
    l <- cases[[cls]]
    expect_identical(class(l$stat)[1L], cls)
    m <- if (cls %in% c("StatCount", "StatBin", "StatDensity", "StatEcdf")) ggplot2::aes(x) else ggplot2::aes(x, y)
    ld <- suppressWarnings(suppressMessages(ggplot2::layer_data(ggplot2::ggplot(d, m) + l)))
    own <- setdiff(.fig_stat_vars[[cls]], c("notchupper", "notchlower", "relvarwidth", "outliers",
                                            "quantile", "wdensity"))
    expect_true(all(own %in% names(ld)), info = paste(cls, paste(setdiff(own, names(ld)), collapse = ",")))
  }
})

test_that("a figure's code maps the expressions as written, and draws", {
  skip_if_not_installed("ggplot2")
  p <- tfl_plot("p", list(
    tfl_item("ggplot", list(data = "d", mapping = list(x = "factor(v)"))),
    tfl_item("geom_bar", list(mapping = list(y = "after_stat(count / sum(count))")))))
  code <- vapply(p$items, function(it) .fig_deparse(.fig_item_lang(it)), "")
  expect_identical(code[1L], "ggplot(data = d, mapping = aes(x = factor(v)))")
  d <- data.frame(v = c(1, 1, 2), g = c("a", "b", "a"))
  g <- eval(str2lang(paste(code, collapse = " + ")), list(d = d), asNamespace("ggplot2"))
  ld <- ggplot2::layer_data(g)
  expect_equal(sum(ld$y), 1)
  expect_identical(nrow(.schema_check_plot_aes(p)), 0L)
})

test_that("a condition is of the form: a comparison, ifelse(), cut(), is.na(), %in%", {
  # a waterfall's response, a value above the normal range, a missing date,
  # categories of change
  for (txt in c("PCHG <= -30", "ifelse(PCHG <= -30, \"Response\", \"No response\")",
                "AVAL > ANRHI", "!is.na(ADT)", "AVISIT %in% c(\"Week 4\", \"Week 8\")",
                "cut(PCHG, c(-Inf, -30, 20, Inf))", "AVAL > ANRHI | AVAL < ANRLO",
                "PARAMCD == \"SYSBP\" & AVAL >= 140",
                "after_stat(count/sum(n[!duplicated(group)]))", "toupper(substr(SEX, 1, 1))")) {
    e <- str2lang(txt)
    expect_true(.fig_aes_form(e), info = txt)
    w <- .fig_value_lang(list(fill = .fig_deparse(e)), "mapping")
    expect_identical(w[[2L]], e, info = txt)
    expect_identical(.fig_lang_value(w, "mapping")$fill, .fig_deparse(e), info = txt)
  }
  # an index is inside the form only
  expect_false(.fig_aes_form(quote(n[!duplicated(group)])))
  # read from code and back, the check quiet
  for (code in c("geom_col(mapping = aes(x = reorder(USUBJID, -PCHG), y = PCHG, fill = PCHG <= -30))",
                 "geom_point(mapping = aes(x = ADY, y = AVAL, shape = !is.na(ADT)))")) {
    it <- .fig_lang_item(str2lang(code))
    expect_false(any(vapply(it$args$mapping, inherits, NA, "tfl_r")), info = code)
    expect_identical(nrow(tfl_schema_check(it)), 0L, info = code)
    expect_identical(.fig_deparse(.fig_item_lang(it)), code)
  }
  # a waterfall coloured by response draws
  skip_if_not_installed("ggplot2")
  d <- data.frame(USUBJID = c("01", "02", "03"), PCHG = c(-45, 10, -31))
  it <- tfl_item("geom_col", list(mapping = list(x = "reorder(USUBJID, -PCHG)", y = "PCHG",
                                                 fill = "ifelse(PCHG <= -30, \"PR\", \"SD/PD\")")))
  g <- ggplot2::ggplot(d) + eval(.fig_item_lang(it), asNamespace("ggplot2"))
  ld <- ggplot2::layer_data(g)
  expect_identical(length(unique(ld$fill)), 2L)
})
