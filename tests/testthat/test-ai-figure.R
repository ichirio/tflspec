# The AI steps with a figure: its own request, its answer (a figure design)
# collected as the figure's report file in the design's form, drawn by
# tfl_ai_run() as tflplanner draws it

fig_study <- function(root) {
  ad <- file.path(root, "adam")
  dir.create(ad, recursive = TRUE, showWarnings = FALSE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(ad, paste0(x, ".xpt")))
  }
  normalizePath(ad, winslash = "/")
}
fig_book <- function() system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")

test_that("a figure is a request of its own: its shell, its analysis set, the design's answer", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  r <- tfl_ai_prepare(unit = "toc", d, fig_book(), adam = fig_study(root), quiet = TRUE, size = 3L)
  fr <- r$requests[r$requests$type == "figure", ]
  expect_identical(fr$request, "figure-1")
  expect_identical(fr$family, "figure")
  expect_identical(fr$outputs, "F-14-2-2")
  expect_identical(r$toc$request[r$toc$toc_id == "F-14-2-2"], "figure-1")
  expect_identical(tfl_ai_requests(d)$type, r$requests$type)
  txt <- paste(readLines(file.path(d, fr$file), encoding = "UTF-8"), collapse = "\n")
  expect_match(txt, "## The shell", fixed = TRUE)
  expect_match(txt, "Y axis: Survival Probability", fixed = TRUE)
  expect_match(txt, "Analysis set: ITT", fixed = TRUE)
  expect_match(txt, "Read the figure's shell and its TOC row above", fixed = TRUE)
  expect_false(grepl("Attached:", txt, fixed = TRUE))
  # the dry run says the same requests
  dry <- tfl_ai_prepare(unit = "toc", d, fig_book(), adam = fig_study(root), quiet = TRUE, size = 3L, dry_run = TRUE)
  expect_identical(dry$requests[c("request", "type", "family", "outputs")],
                   r$requests[c("request", "type", "family", "outputs")])
  # tables only, when asked
  r2 <- suppressMessages(tfl_ai_write_requests(unit = "toc", d, tfl_ai_read_shell(fig_book()),
                                               tfl_ai_adam_summary(fig_study(root)),
                                               types = "table", size = 3L, overwrite = TRUE))
  expect_false("figure" %in% r2$type)
})

test_that("the figure's answer: its report file in the design's form, the same design back", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  tfl_ai_prepare(unit = "toc", d, fig_book(), adam = fig_study(root), quiet = TRUE, size = 3L)
  ex <- test_path("fixtures", "answers-toc", "figure-1.md")
  ai_drafts_from_answer(d, ex, "figure")
  res <- suppressMessages(tfl_ai_collect_answers(d))
  fr <- res$reports[res$reports$type == "figure", ]
  expect_identical(fr$status, "ok")
  expect_identical(fr$file, "ai/spec/outputs/F-14-2-2.yml")
  o <- tfl_read_output(file.path(d, fr$file))
  expect_identical(o$kind, "figure")
  expect_identical(o$label, "Kaplan-Meier Plot of Overall Survival")
  expect_identical(o$population, "ITT")
  expect_identical(o$data$source, "code")
  # the design as the figure designer gets it back: the answer's design
  a <- tfl_ai_parse(readLines(ex, encoding = "UTF-8"), "figure")
  # (the AI's drafts: read as the study's report files)
  fd <- withr::with_options(list(tflspec.outputs_dir = "ai/spec/outputs"),
                            tfl_sheets_from_yaml(d))$figures[["F-14-2-2"]]
  back <- tfl_fig_design(data = fd$data, stats = fd$stats %||% list(), plot = fd$plot,
                         layers = fd$layers, template = fd$template)
  expect_identical(as.character(tfl_fig_design_code(back, "F", save = FALSE)),
                   as.character(tfl_fig_design_code(a$design, "F", save = FALSE)))
  # its assumptions, with their questions, in the study's file
  expect_true(all(c("Planned (TRT01P) or actual (TRT01A) treatment for the curves?",
                    "Is OS's AVAL in months?") %in% res$assumptions$question))
  # a draft with an error: a repair prompt
  bad <- sub("shape: plus", "shape: \"+\"", readLines(ex, encoding = "UTF-8"), fixed = TRUE)
  unlink(file.path(d, fr$file))
  ai_drafts_from_answer(d, bad, "figure")
  res2 <- suppressMessages(tfl_ai_collect_answers(d))
  fr2 <- res2$reports[res2$reports$type == "figure", ]
  expect_identical(fr2$status, "error")
  expect_identical(fr2$repair, "ai/repairs/figure-1.md")
  expect_match(fr2$problems_text, "censor_mark", fixed = TRUE)
  expect_true(file.exists(file.path(d, "ai", "spec", "outputs", "F-14-2-2.yml")))   # (a draft that fails the check stays)
})

test_that("tfl_ai_run(): the figure drawn, its PNG and RTF; agent_prompt", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("ggsurvfit")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  r <- tfl_ai_prepare(unit = "toc", d, fig_book(), adam = fig_study(root), quiet = TRUE, size = 3L)
  # (from the ai folder, the agent's workspace: a copy of it says the same)
  expect_match(r$agent_prompt, "Read AGENTS.md and follow it", fixed = TRUE)
  expect_match(r$agent_prompt, "Process each request on its own", fixed = TRUE)
  expect_match(r$agent_prompt, "run only `Rscript run.R <request>`", fixed = TRUE)
  expect_false(grepl(normalizePath(root, winslash = "/"), r$agent_prompt, fixed = TRUE))
  expect_true("ai/agent_prompt.md" %in% r$files)
  expect_identical(paste(readLines(file.path(d, "ai", "agent_prompt.md"), encoding = "UTF-8"),
                         collapse = "\n"), r$agent_prompt)
  ai_drafts_from_fixtures(d)
  res <- suppressMessages(tfl_ai_run(d))
  fr <- res$reports[res$reports$type == "figure", ]
  expect_identical(fr$png, "ai/output/tfl/F-14-2-2.png")
  expect_identical(fr$rtf, "ai/output/tfl/F-14-2-2.rtf")
  expect_true(all(file.exists(file.path(d, c(fr$png, fr$rtf)))))
  expect_gt(file.size(file.path(d, fr$png)), 10000)
  expect_true(all(!is.na(res$reports$rtf)))
})

test_that("an agent's figure draft with its layers as `layers:` (the instructions' form) is drawn", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  tfl_ai_prepare(unit = "toc", d, fig_book(), adam = fig_study(root), quiet = TRUE, size = 3L)
  ai_drafts_from_fixtures(d)
  # the agent's file: no template, the layers beside an empty content
  f <- file.path(d, "ai", "spec", "outputs", "F-14-2-2.yml")
  writeLines(c(
    "id: F-14-2-2", "kind: figure", "label: Best overall response by arm", "population: ITT",
    "extends: [defaults]",
    "data:", "  source: code", "  steps:",
    "    - {step: read, dataset: ADRS}", "    - {step: flag, variable: ITTFL}",
    "    - {step: param, value: OVR}",
    "content: []",
    "plot: {x_label: Arm, y_label: Subjects}",
    "layers:", "  - {layer: point, x: AVISITN, y: AVAL}",
    "page:", "  titles:", "    '1': {center: Figure 14.2.2}"), f)
  o <- tfl_read_output(f)
  fd <- tflspec:::.fig_design_of_output(o)
  expect_identical(vapply(fd$layers, `[[`, "", "layer"), "point")
  res <- suppressMessages(tfl_ai_run(d, requests = "figure-1"))
  fr <- res$reports[res$reports$output_id == "F-14-2-2", ]
  expect_false(is.na(fr$png), info = paste(fr$first, fr$rtf_problem))
  expect_true(file.exists(file.path(d, fr$png)))
})

test_that("a design's preset written as a template is the design's preset, not a template SPEC", {
  expect_true(tflspec:::.fig_preset_named(list(name = "box_by_visit")))
  expect_false(tflspec:::.fig_preset_named(list(name = "km_survival")))
  expect_false(tflspec:::.fig_preset_named(list(name = "no_such")))
  expect_false(tflspec:::.fig_preset_named("box_by_visit"))
  # a name of both (forest_hr): without the template SPEC's required
  # arguments the preset, with them the template SPEC
  expect_true(tflspec:::.fig_preset_named(list(name = "forest_hr")))
  expect_false(tflspec:::.fig_preset_named(list(name = "forest_hr", args = list(param = "OS"))))
  expect_null(tflspec:::.tpl_spec_of(tfl_output("F-3", "figure", template = list(name = "forest_hr"))))
  o <- tfl_output("F-1", "figure", template = list(name = "box_by_visit"))
  expect_null(tflspec:::.tpl_spec_of(o))
  expect_identical(tflspec:::.fig_design_of_output(o)$template, "box_by_visit")
  expect_identical(tflspec:::.tpl_spec_of(tfl_output("F-2", "figure",
                                                      template = list(name = "km_survival")))$name,
                   "km_survival")
})

test_that("a built figure's facts beside its PNG; a figure of the agent's own code is drawn (#512)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  tfl_ai_prepare(unit = "toc", d, fig_book(), adam = fig_study(root), quiet = TRUE, size = 3L)
  ai_drafts_from_fixtures(d)
  # the fallback after two fixes: the data steps, and one layer_code that draws the whole plot
  f <- file.path(d, "ai", "spec", "outputs", "F-14-2-2.yml")
  writeLines(c(
    "id: F-14-2-2", "kind: figure", "label: Best overall response by arm", "population: ITT",
    "extends: [defaults]",
    "data:", "  source: code", "  steps:",
    "    - {step: read, dataset: ADRS}", "    - {step: flag, variable: ITTFL}",
    "    - {step: param, value: OVR}",
    "content: []",
    "layers:",
    "  - {layer: layer_code, code: 'p <- ggplot(df, aes(AVISITN, AVAL, colour = TRT01P)) + geom_point() + labs(x = \"Visit\", y = \"Value\")'}",
    "page:", "  titles:", "    '1': {center: Figure 14.2.2}"), f)
  res <- suppressMessages(tfl_ai_run(d, requests = "figure-1"))
  fr <- res$reports[res$reports$output_id == "F-14-2-2", ]
  expect_false(is.na(fr$png), info = paste(fr$first, fr$rtf_problem))
  facts <- sub("[.]png$", "-facts.txt", file.path(d, fr$png))
  expect_true(file.exists(facts))
  x <- readLines(facts, encoding = "UTF-8")
  expect_true("- x: Visit" %in% x)
  expect_true(any(grepl("^- 1[.] geom_point: [1-9][0-9]* row", x)))
  expect_true(any(grepl("^- x axis: from", x)))
  expect_true(any(grepl("^- colour: ", x)))
})

test_that("a km_survival draft with `codelists` for its arms is drawn (set_levels is around it)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("ggsurvfit")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  tfl_ai_prepare(unit = "toc", d, fig_book(), adam = fig_study(root), quiet = TRUE, size = 3L)
  ai_drafts_from_fixtures(d)
  f <- file.path(d, "ai", "spec", "outputs", "F-14-2-2.yml")
  writeLines(c(
    "id: F-14-2-2", "kind: figure", "label: Kaplan-Meier Plot of Overall Survival", "population: ITT",
    "extends: [defaults]",
    "data:", "  source: code", "  steps:",
    "    - {step: read, dataset: ADTTE}", "    - {step: flag, variable: ITTFL}",
    "    - {step: param, value: OS}",
    "content: []",
    "codelists:", "  TRT01P:",
    "    - {value: Control, label: Control}", "    - {value: Drug X, label: Drug X}",
    "template:", "  name: km_survival", "  args: {by: TRT01P, time_unit: months}",
    "page:", "  titles:", "    '1': {center: Figure 14.2.2}"), f)
  res <- suppressMessages(tfl_ai_run(d, requests = "figure-1"))
  fr <- res$reports[res$reports$output_id == "F-14-2-2", ]
  expect_false(grepl("set_levels", fr$rtf_problem %||% "", fixed = TRUE), info = fr$rtf_problem)
  expect_false(is.na(fr$png), info = paste(fr$first, fr$rtf_problem))
  expect_true(file.exists(file.path(d, fr$png)))
})

test_that("codelists written as plain values are read as {value, label} (E9)", {
  f <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("id: F-1", "kind: figure", "label: KM", "codelists:",
               "  TRT01P: [Placebo, Drug A]",
               "  SEX:", "    - {value: F, label: Female}"), f)
  o <- tfl_read_output(f)
  expect_identical(o$codelists$TRT01P[[2L]], list(value = "Drug A", label = "Drug A"))
  expect_identical(o$codelists$SEX[[1L]], list(value = "F", label = "Female"))
  rows <- tflspec:::.codelists_rows(o$codelists, "F-1")
  expect_identical(rows$label, c("Placebo", "Drug A", "Female"))
})
