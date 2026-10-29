# The AI steps with a figure: its own request, its answer (a figure design)
# collected as the figure's report file in the design's form, drawn by
# tfl_ai_run() as tflplanner draws it; the questions read and answered
# (tfl_ai_questions(), tfl_ai_answer_questions())

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
  # (from the study folder: a copy of it says the same)
  expect_match(r$agent_prompt, "Follow AGENTS.md", fixed = TRUE)
  expect_match(r$agent_prompt, "Do only ai/requests/", fixed = TRUE)
  expect_false(grepl("run.R", r$agent_prompt, fixed = TRUE))
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
