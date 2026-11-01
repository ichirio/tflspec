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
