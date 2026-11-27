# A figure template's script makes the figure written by hand
# (fixtures/fig-hand.R): every layer's data, the labels, and each axis's
# range, breaks and break labels.  Data, not pixels: the same under any
# graphics device, and a change of ggplot2's drawing does not count.

# (`pop`: the report's analysis set, by its flag on ADSL -- its subjects
# made as a report program's data part makes them, adsl_sub, and the
# records the design reads joined to them: a template keeps no set itself)
fx_run <- function(design, adam, pop = NULL) {
  if (!is.null(pop)) {
    attr(design, "subjects") <- list(object = "adsl_sub", dataset = "ADSL", id = "USUBJID")
  }
  code <- tfl_fig_design_code(design, "t")
  e <- new.env()
  for (n in names(adam)) assign(tolower(n), adam[[n]], envir = e)
  if (!is.null(pop)) e$adsl_sub <- adam$ADSL[adam$ADSL[[pop]] %in% "Y", ]
  dir <- tempfile("fig")
  dir.create(dir)
  owd <- setwd(dir)
  on.exit({ setwd(owd); unlink(dir, recursive = TRUE) }, add = TRUE)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  suppressMessages(suppressWarnings(eval(parse(text = code), envir = e)))
  e
}

# what a plot says: its layers' data, its labels, its axes
fx_said <- function(g) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  b <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(g)))
  pp <- b$layout$panel_params[[1L]]
  axis <- function(a) {
    s <- pp[[a]]
    if (is.null(s)) return(NULL)
    list(range = s$continuous_range %||% s$limits,
         breaks = s$breaks, labels = s$get_labels())
  }
  labs <- b$plot$labels
  list(layers = lapply(b$data, function(d) {
         d <- as.data.frame(d)
         d[order(names(d))]
       }),
       labels = labs[intersect(c("x", "y", "title", "colour", "fill"),
                               names(labs))],
       x = axis("x"), y = axis("y"))
}

# (The forest plot is not here: its template reads the figure's own ARD
# since #293 phase 6, and test-fig-forest.R checks its hazard ratios
# against coxph()'s, overall and within each subgroup.)
test_that("four templates' scripts make the figures written by hand", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("survival")
  adam <- tfl_example_adam()
  hand <- source(test_path("fixtures", "fig-hand.R"), local = new.env())$value
  os <- unique(adam$ADTTE$PARAMCD)[1L]
  lb <- unique(adam$ADLB$PARAMCD)[1L]
  cases <- list(
    km_risk_table = list(tfl_fig_template("km_risk_table", param = os,
                                          group = "TRT01P"),
                         hand$km_risk_table(adam, os), "FASFL"),
    waterfall_response = list(tfl_fig_template("waterfall_response"),
                              hand$waterfall_response(adam), "FASFL"),
    mean_se = list(tfl_fig_template("mean_se", group = "TRT01A", param = lb),
                   hand$mean_se(adam, lb), "SAFFL"),
    box_by_visit = list(tfl_fig_template("box_by_visit", group = "TRT01A"),
                        hand$box_by_visit(adam), "SAFFL"))
  for (nm in names(cases)) {
    e <- fx_run(cases[[nm]][[1L]], adam, pop = cases[[nm]][[3L]])
    want <- cases[[nm]][[2L]]
    for (g in names(want)) {
      # the figure's chain goes into `fig` (a whole-script figure keeps
      # `p`); with a panel under it, `fig` is a patchwork: its first plot
      main <- if (!is.null(e$p)) e$p else if (inherits(e$fig, "patchwork")) e$fig[[1L]] else e$fig
      got <- fx_said(if (identical(g, "p")) main else e[[g]])
      exp <- fx_said(want[[g]])
      expect_identical(length(got$layers), length(exp$layers),
                       label = paste(nm, g, "layers"))
      for (i in seq_along(exp$layers)) {
        expect_equal(got$layers[[i]], exp$layers[[i]],
                     label = paste(nm, g, "layer", i), ignore_attr = TRUE)
      }
      expect_equal(got$labels, exp$labels, label = paste(nm, g, "labels"),
                   ignore_attr = TRUE)
      expect_equal(got$x, exp$x, label = paste(nm, g, "x axis"))
      expect_equal(got$y, exp$y, label = paste(nm, g, "y axis"))
    }
  }
})
