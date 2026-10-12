# A designed figure's program, as a report program has it (the study's
# figure setup sourced before it, #293): two sections, `# ---- data ----`
# and `# ---- plot ----`, one pipe an object, one `+` chain into `plot`, a
# panel a chain of its own, no library().  Byte for byte against the
# fixtures (tests/testthat/fixtures/fig-programs/); a change of the code
# shows here first.  To write them again after a wanted change:
# fig_programs_write() below, then look at the diff.

fig_program_cases <- function() {
  cl <- data.frame(output_id = "F-MEAN", variable = "TRTA",
                   value = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
                   label = NA, order = 1:3)
  km_add <- tfl_fig_template("km_risk_table", data = "ADTTE", param = "OS",
                             pop = "FASFL", group = "TRT01P")
  km_add$layers[[length(km_add$layers)]]$method <- "add_risktable"
  # the medians printed from a table's ARD (ard_number), as the sample's F-14-2-3
  km_ard <- tfl_fig_template("km_simple", data = "ADTTE", param = "TTDE",
                             pop = "SAFFL", group = "TRT01A", time_unit = "days")
  arms <- c("Placebo", "Xanomeline Low Dose")
  km_ard$layers <- c(km_ard$layers, lapply(seq_along(arms), function(i)
    list(layer = "ard_number", analysis_id = "KM", variable = "prob", level = 0.5,
         stat = "estimate", group = paste("TRT01A =", arms[i]),
         label = paste0("Median (", arms[i], "): {value} days"), digits = 0, vjust = 1.5 * i)))
  list(
    km_risk_table = list(id = "F-KM",
      design = tfl_fig_template("km_risk_table", data = "ADTTE", param = "OS",
                                pop = "FASFL", group = "TRT01P")),
    km_add_risktable = list(id = "F-KM", design = km_add),
    km_ard_number = list(id = "F-KM", design = km_ard),
    km_single_arm = list(id = "F-KM1", design = tfl_fig_template("km_single_arm")),
    mean_se_codelists = list(id = "F-MEAN", codelists = cl,
      design = tfl_fig_template("mean_se", data = "ADVS", param = "SYSBP", group = "TRTA")),
    waterfall_response = list(id = "F-WF", design = tfl_fig_template("waterfall_response")),
    swimmer_full = list(id = "F-SW", design = tfl_fig_template("swimmer_full")),
    # the forest plot from the figure's own ARD (#293 phase 6)
    forest_hr = list(id = "F-FOREST",
      design = tfl_fig_template("forest_hr", data = "ADTTE", param = "TTDE", pop = "SAFFL",
                                group = "TRT01A", subgroups = "SEX, AGEGR1",
                                comparison = "Xanomeline High Dose")))
}

fig_program_code <- function(case) {
  as.character(tfl_fig_design_code(case$design, case$id, setup = TRUE, save = FALSE,
                                   name = "plot", codelists = case$codelists))
}

fig_programs_write <- function(dir = test_path("fixtures", "fig-programs")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  cs <- fig_program_cases()
  for (nm in names(cs)) writeLines(fig_program_code(cs[[nm]]), file.path(dir, paste0(nm, ".R")))
}

test_that("a designed figure's program is the one fixed in the fixtures", {
  withr::local_options(tflspec.ggplot2_version = NULL)
  cs <- fig_program_cases()
  for (nm in names(cs)) {
    want <- readLines(test_path("fixtures", "fig-programs", paste0(nm, ".R")), encoding = "UTF-8")
    expect_identical(fig_program_code(cs[[nm]]), want, label = nm)
  }
})

test_that("a program has its two sections, one chain, no library()", {
  for (case in fig_program_cases()) {
    code <- fig_program_code(case)
    expect_identical(code[[1L]], section("data"))
    expect_identical(sum(code == section("plot")), 1L)
    expect_false(any(grepl("^library\\(|^p <- p \\+|^# Generated", code)))
    expect_true(any(startsWith(code, "plot <- ")))
    # the palette first in the plot section
    expect_match(code[which(code == section("plot")) + 1L], "^# the .* palette|^pal <- ")
    expect_false(inherits(tryCatch(parse(text = code), error = function(e) e), "error"))
  }
})

test_that("what a program needs is in its attributes", {
  cs <- fig_program_cases()
  code <- tfl_fig_design_code(cs$km_risk_table$design, "F-KM", setup = TRUE)
  expect_identical(attr(code, "reads"), "ADTTE")
  expect_setequal(attr(code, "libs"), c("dplyr", "ggplot2", "ggsurvfit", "patchwork"))
  sw <- tfl_fig_design_code(cs$swimmer_full$design, "F-SW", setup = TRUE)
  expect_setequal(attr(sw, "reads"), c("ADSL", "ADRS"))
})

test_that("the number at risk: a panel from the fit by default, or add_risktable", {
  cs <- fig_program_cases()
  panel <- fig_program_code(cs$km_risk_table)
  expect_true(any(grepl("^sr <- summary\\(fit, times = x_breaks", panel)))
  expect_true(any(startsWith(panel, "p_risk <- ggplot(risk, ")))
  expect_true("plot <- plot / p_risk + plot_layout(heights = c(0.833, 0.167))" %in% panel)
  # the panel's data is in the data section, before the plot section
  expect_lt(grep("^risk <- data.frame", panel), which(panel == section("plot")))
  add <- fig_program_code(cs$km_add_risktable)
  expect_false(any(grepl("p_risk|^sr <- ", add)))
  expect_true(any(grepl("add_risktable(times = x_breaks, risktable_stats = \"n.risk\", size = 3)",
                        add, fixed = TRUE)))
  expect_identical(nrow(tfl_check_fig_design(cs$km_add_risktable$design)), 0L)
})

test_that("a design of before (data + stats, data_code / stats_code) reads as one list", {
  old <- list(
    template = "x",
    data = list(list(step = "read", dataset = "ADSL"),
                list(step = "data_code", code = "df <- df[df$AGE > 0, ]")),
    stats = list(list(step = "summary", name = "sm", value = "AGE", by = "TRT01A"),
                 list(step = "stats_code", code = "sm$x <- 1")),
    plot = list(colour_by = "TRT01A"),
    layers = list(list(layer = "point", data = "sm", x = "TRT01A", y = "mean")))
  f <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(old, f)
  d <- tfl_read_fig_design(f)
  expect_identical(vapply(d$data, `[[`, "", "step"), c("read", "code", "summary", "code"))
  expect_null(d$stats)
  new <- tfl_fig_design(data = d$data, plot = d$plot, layers = d$layers, template = "x")
  expect_identical(tfl_fig_design_code(d, setup = TRUE), tfl_fig_design_code(new, setup = TRUE))
  # written back: one list, no stats
  g <- withr::local_tempfile(fileext = ".yml")
  tfl_write_fig_design(d, g)
  y <- yaml::read_yaml(g)
  expect_null(y$stats)
  expect_identical(vapply(y$data, `[[`, "", "step"), c("read", "code", "summary", "code"))
})

test_that("a step after a named object changes it, in its pipe; code lists go on df first", {
  cl <- data.frame(output_id = "F", variable = "TRT01A", value = c("B", "A"),
                   label = NA, order = 1:2)
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADSL"),
                list(step = "summary", name = "sm", value = "AGE", by = "TRT01A"),
                list(step = "derive", variable = "lo2", expr = "lo * 2")),
    plot = list(colour_by = "TRT01A"),
    layers = list(list(layer = "point", data = "sm", x = "TRT01A", y = "mean")))
  code <- paste(tfl_fig_design_code(d, "F", setup = TRUE, codelists = cl), collapse = "\n")
  expect_match(code, "df <- adsl |>\n  set_levels(TRT01A = cl_trt01a)", fixed = TRUE)
  expect_match(code, "mutate(se = sd / sqrt(n), lo = mean - se, hi = mean + se) |>\n  mutate(lo2 = lo * 2)",
               fixed = TRUE)
})

test_that("the user's layer code is a statement on p, the chain around it", {
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADSL")),
    layers = list(list(layer = "hline", yintercept = 0),
                  list(layer = "layer_code", code = "p <- p + ggplot2::ggtitle('mine')"),
                  list(layer = "hline", yintercept = 1)))
  code <- tfl_fig_design_code(d, setup = TRUE, save = FALSE, name = "plot")
  i <- which(code == "p <- p + ggplot2::ggtitle('mine')")
  expect_length(i, 1L)
  expect_true(any(startsWith(code[seq_len(i)], "p <- ggplot() +")))
  expect_true("p <- p +" %in% code[-seq_len(i)])
  expect_identical(code[[length(code)]], "plot <- p")
  expect_false(inherits(tryCatch(parse(text = code), error = function(e) e), "error"))
})
