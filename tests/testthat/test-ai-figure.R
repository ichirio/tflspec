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
  r <- tfl_ai_prepare(d, fig_book(), adam = fig_study(root), quiet = TRUE)
  fr <- r$requests[r$requests$type == "figure", ]
  expect_identical(fr$request, "figure-1")
  expect_identical(fr$family, "figure")
  expect_identical(fr$outputs, "F-14-2-2")
  expect_identical(r$toc$request[r$toc$output_id == "F-14-2-2"], "figure-1")
  expect_identical(tfl_ai_requests(d)$type, r$requests$type)
  txt <- paste(readLines(file.path(d, fr$file), encoding = "UTF-8"), collapse = "\n")
  expect_match(txt, "tflspec_ai: {task: figure, version: 1, output_id: F-14-2-2}", fixed = TRUE)
  expect_match(txt, "## The shell", fixed = TRUE)
  expect_match(txt, "Y axis: Survival Probability", fixed = TRUE)
  expect_match(txt, "Analysis set: ITT", fixed = TRUE)
  expect_match(txt, "Read the figure's shell and its TOC row above", fixed = TRUE)
  expect_false(grepl("Attached:", txt, fixed = TRUE))
  # the dry run says the same requests
  dry <- tfl_ai_prepare(d, fig_book(), adam = fig_study(root), quiet = TRUE, dry_run = TRUE)
  expect_identical(dry$requests[c("request", "type", "family", "outputs")],
                   r$requests[c("request", "type", "family", "outputs")])
  # tables only, when asked
  r2 <- suppressMessages(tfl_ai_write_requests(d, tfl_ai_read_shell(fig_book()),
                                               tfl_ai_adam_summary(fig_study(root)),
                                               types = "table", overwrite = TRUE))
  expect_false("figure" %in% r2$type)
})

test_that("the figure's answer: its report file in the design's form, the same design back", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  tfl_ai_prepare(d, fig_book(), adam = fig_study(root), quiet = TRUE)
  ex <- system.file("ai", "example", "answers", "figure-1.md", package = "tflspec")
  tfl_ai_save_answer(d, "figure-1", readLines(ex, encoding = "UTF-8"))
  res <- suppressMessages(tfl_ai_collect_answers(d))
  fr <- res$reports[res$reports$type == "figure", ]
  expect_identical(fr$status, "ok")
  expect_identical(fr$file, "spec/outputs/F-14-2-2.yml")
  o <- tfl_read_output(file.path(d, fr$file))
  expect_identical(o$kind, "figure")
  expect_identical(o$label, "Kaplan-Meier Plot of Overall Survival")
  expect_identical(o$population, "ITT")
  expect_identical(o$data$source, "code")
  # the design as the figure designer gets it back: the answer's design
  a <- tfl_ai_parse(readLines(ex, encoding = "UTF-8"), "figure")
  fd <- tfl_sheets_from_yaml(d)$figures[["F-14-2-2"]]
  back <- tfl_fig_design(data = fd$data, stats = fd$stats %||% list(), plot = fd$plot,
                         layers = fd$layers, template = fd$template)
  expect_identical(as.character(tfl_fig_design_code(back, "F", save = FALSE)),
                   as.character(tfl_fig_design_code(a$design, "F", save = FALSE)))
  # its assumptions, with their questions, in the study's file
  expect_true(all(c("Planned (TRT01P) or actual (TRT01A) treatment for the curves?",
                    "Is OS's AVAL in months?") %in% res$assumptions$question))
  # an answer with an error: a repair prompt, no file
  bad <- sub("shape: plus", "shape: \"+\"", readLines(ex, encoding = "UTF-8"), fixed = TRUE)
  unlink(file.path(d, fr$file))
  tfl_ai_save_answer(d, "figure-1", bad)
  res2 <- suppressMessages(tfl_ai_collect_answers(d))
  fr2 <- res2$reports[res2$reports$type == "figure", ]
  expect_identical(fr2$status, "error")
  expect_identical(fr2$repair, "ai/repairs/figure-1.md")
  expect_match(fr2$problems_text, "censor_mark", fixed = TRUE)
  expect_false(file.exists(file.path(d, "spec", "outputs", "F-14-2-2.yml")))
})

test_that("tfl_ai_run(): the figure drawn, its PNG and RTF; agent_prompt", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("ggsurvfit")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  r <- tfl_ai_prepare(d, fig_book(), adam = fig_study(root), quiet = TRUE, mode = "agent")
  expect_match(r$agent_prompt, "/AGENTS.md", fixed = TRUE)
  expect_match(r$agent_prompt, "/ai/requests/index.md", fixed = TRUE)
  expect_match(r$agent_prompt, "/run.R", fixed = TRUE)
  ex <- system.file("ai", "example", "answers", package = "tflspec")
  file.copy(list.files(ex, full.names = TRUE), file.path(d, "ai", "answers"))
  res <- suppressMessages(tfl_ai_run(d))
  fr <- res$reports[res$reports$type == "figure", ]
  expect_identical(fr$png, "output/tfl/F-14-2-2.png")
  expect_identical(fr$rtf, "output/tfl/F-14-2-2.rtf")
  expect_true(all(file.exists(file.path(d, c(fr$png, fr$rtf)))))
  expect_gt(file.size(file.path(d, fr$png)), 10000)
  expect_true(all(!is.na(res$reports$rtf)))
  expect_true(any(grepl("F-14-2-2", res$questions$reports, fixed = TRUE)))
})

test_that("tfl_ai_questions() and tfl_ai_answer_questions(): read, answer, the rest as it is", {
  d <- withr::local_tempdir()
  a <- data.frame(output_id = c("T-1", "T-2", "F-1"), where = c("ard.analyses", NA, "data"),
                  item = c("arm", "arm", "time"),
                  assumption = c("TRT01A", "TRT01A", "months"),
                  question = c("Actual or planned treatment?", "Actual or planned treatment?",
                               "Is AVAL in months?"), stringsAsFactors = FALSE)
  .ai_questions_update(d, a)
  q <- tfl_ai_questions(d)
  expect_named(q, c("id", "output_id", "where", "question", "provisional", "answer", "status"))
  expect_identical(q$id, c("Q1", "Q2"))
  expect_identical(q$output_id, c("T-1, T-2", "F-1"))
  expect_identical(q$where, c("ard.analyses, arm", "data, time"))
  expect_identical(q$provisional, c("TRT01A", "months"))
  expect_identical(q$answer, c(NA_character_, NA_character_))
  expect_identical(q$status, c("open", "open"))
  f <- file.path(d, "ai", "questions.md")
  before <- readLines(f, encoding = "UTF-8")
  tfl_ai_answer_questions(d, c(Q2 = "Yes, months.\nAVALU says so."))
  q2 <- tfl_ai_questions(d)
  expect_identical(q2$answer, c(NA_character_, "Yes, months.\nAVALU says so."))
  after <- readLines(f, encoding = "UTF-8")
  # only Q2's answer changed
  expect_identical(after[seq_len(grep("^## Q2", after))], before[seq_len(grep("^## Q2", before))])
  tfl_ai_answer_questions(d, data.frame(id = c("Q1", "Q2"), answer = c("Planned.", NA)))
  q3 <- tfl_ai_questions(d)
  expect_identical(q3$answer, c("Planned.", NA_character_))
  # an answer again: replaced, not added to
  tfl_ai_answer_questions(d, c(Q1 = "Planned, TRT01P."))
  expect_identical(tfl_ai_questions(d)$answer[1L], "Planned, TRT01P.")
  expect_identical(sum(grepl("^## Q", readLines(f, encoding = "UTF-8"))), 2L)
  expect_error(tfl_ai_answer_questions(d, c(Q9 = "x")), "no question Q9")
  expect_error(tfl_ai_answer_questions(d, "x"), "named by the ids")
  expect_identical(nrow(tfl_ai_questions(file.path(d, "none"))), 0L)
})
