# tfl_ai_prepare(mode = "agent") and tfl_ai_run(): the study folder as a
# coding agent's work -- every request without a stop, the questions
# gathered into ai/questions.md, a second round from the answers (#374)

agent_study <- function(root, adam_inside = FALSE) {
  ad <- if (adam_inside) file.path(root, "study", "adam") else file.path(root, "adam")
  dir.create(ad, recursive = TRUE, showWarnings = FALSE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(ad, paste0(x, ".xpt")))
  }
  normalizePath(ad, winslash = "/")
}
ex_book <- function() system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")

test_that("mode = \"agent\": the instructions, run.R, questions, decisions; toc.yml; the paths", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  ad <- agent_study(root)
  r <- tfl_ai_prepare(d, ex_book(), adam = ad, quiet = TRUE, mode = "agent")
  expect_identical(r$mode, "agent")
  have <- c("AGENTS.md", ".github/copilot-instructions.md", "run.R", "ai/questions.md",
            "ai/decisions.md", "spec/toc.yml")
  expect_true(all(have %in% r$files))
  expect_true(all(file.exists(file.path(d, have))))
  ag <- readLines(file.path(d, "AGENTS.md"), encoding = "UTF-8")
  expect_identical(readLines(file.path(d, ".github", "copilot-instructions.md"), encoding = "UTF-8"), ag)
  for (k in c("do not stop", "Round 2", "read the ADaM itself", "never by opening",
              "ai/questions.md", "ai/decisions.md", "Rscript run.R")) {
    expect_true(any(grepl(k, ag, fixed = TRUE)), info = k)
  }
  # each request starts with the agent's steps; its length counts them
  req1 <- readLines(file.path(d, r$requests$file[1L]), encoding = "UTF-8")
  expect_identical(req1[1L], "<!-- tflspec: agent mode -->")
  expect_match(req1[2L], sprintf("ai/answers/%s.md", r$requests$request[1L]), fixed = TRUE)
  expect_identical(tfl_ai_requests(d)$chars, r$requests$chars)
  # the shells' text, and where each is in the workbook file (its sheet)
  txt <- paste(req1, collapse = "\n")
  expect_match(txt, "The TOC / SHELL workbook is the file `", fixed = TRUE)
  expect_match(txt, sprintf("- %s: sheet '", strsplit(r$requests$outputs[1L], ", ")[[1L]][1L]),
               fixed = TRUE)
  expect_match(txt, "# Workbook onco_toc_shell.xlsx", fixed = TRUE)
  expect_match(txt, "Never read another sheet on a guess", fixed = TRUE)
  expect_false(any(grepl("Attach", readLines(file.path(d, "ai", "requests", "index.md")))))
  for (k in c("read **that place alone** with R", "Never read another sheet on a guess")) {
    expect_true(any(grepl(k, ag, fixed = TRUE)), info = k)
  }
  # tflplanner's places, and its TOC
  st <- yaml::read_yaml(file.path(d, "study.yml"))
  expect_identical(st$settings$output_path, "output/tfl")
  expect_identical(st$settings$program_dir, "programs/tfl")
  toc <- yaml::read_yaml(file.path(d, "spec", "toc.yml"))$outputs
  expect_identical(vapply(toc, `[[`, "", "id"), r$toc$output_id)
  expect_identical(toc[[1L]]$section, "14.1")
  expect_true(all(vapply(toc, function(e) !is.null(e$population), NA)))
  expect_true(any(grepl("VS Code", r$next_steps, fixed = TRUE)))
  expect_length(r$hint, 0L)
})

test_that("mode = \"agent\": the ADaM inside the study folder is fine; chat mode writes no agent file", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  agent_study(root, adam_inside = TRUE)
  r <- tfl_ai_prepare(d, ex_book(), adam = "adam", quiet = TRUE, mode = "agent")
  expect_length(r$hint, 0L)
  expect_true(file.exists(file.path(d, "AGENTS.md")))
  d2 <- file.path(root, "chat")
  agent_study(file.path(root, "chat-root"))
  r2 <- tfl_ai_prepare(d2, ex_book(), adam = normalizePath(file.path(root, "chat-root", "adam"),
                                                           winslash = "/"), quiet = TRUE)
  expect_false(file.exists(file.path(d2, "AGENTS.md")))
  expect_false(file.exists(file.path(d2, "run.R")))
  expect_true(file.exists(file.path(d2, "spec", "toc.yml")))
  expect_false(any(grepl("agent mode", readLines(file.path(d2, r2$requests$file[1L])), fixed = TRUE)))
})

test_that("tfl_ai_run(): the RTFs, ai/status.md, the questions; a second round keeps the answers", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  ad <- agent_study(root)
  tfl_ai_prepare(d, ex_book(), adam = ad, quiet = TRUE, mode = "agent")
  ex <- system.file("ai", "example", "answers", package = "tflspec")
  file.copy(list.files(ex, full.names = TRUE), file.path(d, "ai", "answers"))
  res <- suppressMessages(tfl_ai_run(d))
  rp <- res$reports
  expect_true(all(file.exists(file.path(d, rp$rtf))))
  expect_true(all(startsWith(rp$rtf, "output/tfl/")))
  status <- readLines(file.path(d, "ai", "status.md"), encoding = "UTF-8")
  expect_true(any(grepl("5 of 5 reports have an RTF", status, fixed = TRUE)))
  q <- res$questions
  expect_gt(nrow(q), 0L)
  expect_true(all(q$status == "open"))
  # the person answers Q1; the agent marks it decided; a run keeps both
  f <- file.path(d, "ai", "questions.md")
  x <- readLines(f, encoding = "UTF-8")
  at <- grep("^## Q1[.]", x)
  ans <- at + grep("^- Answer:", x[at:length(x)])[1L] - 1L
  x[ans] <- "- Answer: planned treatment, TRT01P"
  st <- at + grep("^- Status:", x[at:length(x)])[1L] - 1L
  x[st] <- "- Status: decided"
  writeLines(x, f, useBytes = TRUE)
  res2 <- suppressMessages(tfl_ai_run(d))
  expect_identical(res2$questions$answer[1L], "planned treatment, TRT01P")
  expect_identical(res2$questions$status[1L], "decided")
  expect_identical(nrow(res2$questions), nrow(q))
  expect_identical(readLines(f, encoding = "UTF-8"), x)
  # a second prepare keeps the questions and the decisions
  writeLines(c(readLines(file.path(d, "ai", "decisions.md")), "- Q1 (T-14-1-1): TRT01P"),
             file.path(d, "ai", "decisions.md"))
  dec <- readLines(file.path(d, "ai", "decisions.md"))
  tfl_ai_prepare(d, ex_book(), adam = ad, quiet = TRUE, mode = "agent", overwrite = TRUE)
  expect_identical(readLines(f, encoding = "UTF-8"), x)
  expect_identical(readLines(file.path(d, "ai", "decisions.md")), dec)
})

test_that("the questions: one asked by several reports once, the reports listed; added to later", {
  d <- withr::local_tempdir()
  a <- data.frame(output_id = c("T-1", "T-2", "T-3"), where = NA_character_, item = NA_character_,
                  assumption = c("TRT01A", "TRT01A", "SAFFL"),
                  question = c("Actual (TRT01A) or planned treatment?",
                               "actual (TRT01A) or planned treatment", "Which analysis set?"),
                  stringsAsFactors = FALSE)
  q <- .ai_questions_update(d, a)
  expect_identical(q$question, c("Actual (TRT01A) or planned treatment?", "Which analysis set?"))
  expect_identical(q$reports, c("T-1, T-2", "T-3"))
  a2 <- rbind(a, data.frame(output_id = "T-4", where = NA, item = NA, assumption = "TRT01A",
                            question = "Actual (TRT01A) or planned treatment?"))
  q2 <- .ai_questions_update(d, a2)
  expect_identical(q2$reports[1L], "T-1, T-2, T-4")
  expect_identical(nrow(q2), 2L)
  # no question: nothing new
  expect_identical(nrow(.ai_questions_update(d, a2[0L, ])), 2L)
})

test_that("mode = \"agent\": a study with its ADaM and workbook inside it names no path of this PC", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  ad <- agent_study(root, adam_inside = TRUE)            # given whole
  dir.create(file.path(d, "shells"), recursive = TRUE, showWarnings = FALSE)
  book <- file.path(d, "shells", "toc.xlsx")
  file.copy(ex_book(), book)
  r <- tfl_ai_prepare(d, book, adam = ad, quiet = TRUE, mode = "agent")
  here <- normalizePath(root, winslash = "/")
  files <- list.files(d, recursive = TRUE, all.files = TRUE, pattern = "[.](md|yml|R)$")
  for (f in files) {
    txt <- paste(readLines(file.path(d, f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_false(grepl(tolower(here), tolower(txt), fixed = TRUE), info = f)
  }
  expect_false(grepl(tolower(here), tolower(r$agent_prompt), fixed = TRUE))
  # the workbook and the ADaM, from the study folder
  ae <- paste(readLines(file.path(d, "ai", "requests", "ae-1.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(ae, "shells/toc.xlsx", fixed = TRUE)
  dy <- yaml::read_yaml(file.path(d, "spec", "data.yml"))
  expect_true(all(startsWith(vapply(dy$datasets, `[[`, "", "path"), "adam/")))
  # one outside the study: whole, as it must be
  d2 <- file.path(root, "study2")
  r2 <- tfl_ai_prepare(d2, ex_book(), adam = ad, quiet = TRUE, mode = "agent")
  dy2 <- yaml::read_yaml(file.path(d2, "spec", "data.yml"))
  expect_true(all(startsWith(tolower(vapply(dy2$datasets, `[[`, "", "path")), tolower(ad))))
})
