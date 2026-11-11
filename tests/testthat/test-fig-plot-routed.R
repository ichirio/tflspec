# A figure draft that tflplanner would refuse is an error at the draft check
# (tfl_check_spec()) too: the design tflplanner opens is the sheets' one (the
# template expanded, no template left), and a `plot:` key a template reads
# itself (the risk table's title) must pass its check, an unknown key not.

fig_routed_report <- function(plot = NULL, nm = "km_survival", id = "F-9-routed") {
  o <- tfl_output(id, "figure", population = "SAF", label = "KM",
                  extends = "defaults",
                  data = list(steps = list(list(step = "read", dataset = "ADTTE"),
                                           list(step = "param", value = "PFS"))),
                  template = list(name = nm, args = list(by = "TRT01A")))
  if (length(plot)) o$plot <- plot
  o
}

# the design as tflplanner is given it: the sheets' one
fig_routed_planner_design <- function(o, d) {
  st <- tfl_read_study_spec(d)
  tfl_sheets_from_spec(st, stats::setNames(list(o), o$id), templates = "expand")$figures[[o$id]]
}

test_that("a km report with plot.risk_title passes the design check, expanded or not", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adtte"), "FRT-1")
  for (nm in c("km_survival", "km_cumulative")) {
    o <- fig_routed_report(list(risk_title = "Subjects at risk", x_max = 24), nm)
    # as written (the template named)
    expect_identical(nrow(tfl_check_fig_design(tflspec:::.fig_design_of_output(o))), 0L, info = nm)
    # as tflplanner opens it (no template, the plot settings joined)
    pd <- fig_routed_planner_design(o, d)
    expect_null(pd$template)
    expect_identical(pd$plot$risk_title, "Subjects at risk")
    expect_identical(nrow(tfl_check_fig_design(structure(pd, class = "tfl_fig_design"))), 0L, info = nm)
  }
})

test_that("tfl_check_spec() passes a km report with plot.risk_title, and its title reaches the code", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("ggsurvfit")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adtte"), "FRT-2")
  o <- fig_routed_report(list(risk_title = "Subjects at risk"))
  tfl_write_output(o, file.path(d, "spec", "outputs", paste0(o$id, ".yml")))
  p <- suppressMessages(suppressWarnings(tfl_check_spec(d)))
  expect_identical(nrow(p[p$level == "error", , drop = FALSE]), 0L)
  st <- tfl_read_study_spec(d)
  ro <- tfl_resolve_output(tfl_read_output(file.path(d, "spec", "outputs", paste0(o$id, ".yml"))), st)
  code <- paste(suppressMessages(tfl_content_code(ro)), collapse = "\n")
  expect_match(code, "risk_title = \"Subjects at risk\"", fixed = TRUE)
})

test_that("an unknown plot key is the same error in both checks, with a template or not", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  tplfig_study(d, c("adsl", "adtte"), "FRT-3")
  o <- fig_routed_report(list(risk_title = "Subjects at risk", bogus = 1))
  f <- file.path(d, "spec", "outputs", paste0(o$id, ".yml"))
  tfl_write_output(o, f)
  direct <- tfl_check_fig_design(tflspec:::.fig_design_of_output(o))
  expect_identical(direct$field, "bogus")
  expect_identical(direct$problem, "is not a figure setting")
  planner <- tfl_check_fig_design(structure(fig_routed_planner_design(o, d), class = "tfl_fig_design"))
  expect_identical(planner$field, "bogus")
  expect_identical(planner$problem, direct$problem)
  p <- suppressMessages(suppressWarnings(tfl_check_spec(d)))
  e <- p[p$part == "figure" & p$level == "error", , drop = FALSE]
  expect_identical(nrow(e), 1L)
  expect_identical(e$item, "plot$bogus")
  expect_identical(e$message, direct$problem)

  # a figure of the designer's own (no template): the same
  g <- tfl_output("F-9-own", "figure", population = "SAF", label = "own",
                  extends = "defaults", content = list())
  g$plot <- list(bogus = 1)
  g$data <- list(steps = list(list(step = "read", dataset = "ADTTE")))
  expect_identical(tfl_check_fig_design(tflspec:::.fig_design_of_output(g))$problem,
                   "is not a figure setting")
  # risk_title with no template and no risk table to read it is no setting either
  g$plot <- list(risk_title = "x")
  expect_identical(tfl_check_fig_design(tflspec:::.fig_design_of_output(g))$field, "risk_title")
})
