# The AI's assumptions: their ids and kinds (ai/assumption_ids.csv), the
# person's answers (ai/assumption_answers.csv), what they make of
# ai/corrections.md and ai/questions.md, and a fix the agent made kept

asm_rows <- function(d, rows) {
  dir.create(file.path(d, "spec"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(d, "ai"), recursive = TRUE, showWarnings = FALSE)
  writeLines(yaml::as.yaml(rows, indent.mapping.sequence = TRUE),
             file.path(d, "spec", "_assumptions.yml"))
  d
}

asm_five <- function() {
  list(
    list(output_id = "T-1", where = "ard.analyses", item = "arm", assumption = "TRT01A",
         question = "Actual or planned?"),
    list(output_id = "T-2", where = "ard.analyses", item = "arm", assumption = "TRT01A",
         question = "actual  or planned ?"),
    list(output_id = "T-1", where = "codelists", item = "SEX", assumption = "M / F"),
    list(output_id = "T-3", where = "codelists", item = "sex", assumption = "M/F"),
    list(output_id = "T-3", where = "page", item = "titles", assumption = "two lines"))
}

test_that("each assumption an id kept across collections, and a kind", {
  d <- asm_rows(withr::local_tempdir(), asm_five())
  a <- tfl_ai_assumptions(d)
  expect_named(a, c("id", "kind", "n_reports", "output_id", "where", "item", "assumption",
                    "value", "question", "verdict", "correct", "status"))
  expect_identical(a$id, paste0("A", 1:5))
  expect_identical(a$kind, c("K1", "K1", "K2", "K2", "K3"))
  expect_identical(a$n_reports, c(2L, 2L, 2L, 2L, 1L))
  expect_true(all(a$status == "open"))
  reg <- utils::read.csv(file.path(d, "ai", "assumption_ids.csv"), stringsAsFactors = FALSE)
  expect_named(reg, c("type", "key", "id"))
  expect_identical(sum(reg$type == "A"), 5L)
  # the ids written into spec/_assumptions.yml
  y <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  expect_identical(vapply(y, `[[`, "", "id"), paste0("A", 1:5))
  # collected again, in another order, an item written differently, a new
  # one: the same key its id, the new one the next
  r <- asm_five()[c(5, 3, 1)]
  r[[3]]$item <- "Arm."
  r[[4]] <- list(output_id = "T-4", where = "page", item = "footnotes", assumption = "none")
  asm_rows(d, r)
  b <- tfl_ai_assumptions(d)
  expect_identical(b$id, c("A5", "A3", "A1", "A6"))
  expect_identical(b$kind, c("K3", "K2", "K1", "K4"))
  expect_identical(b$n_reports, c(1L, 1L, 1L, 1L))
})

test_that("the answers file: written, read, an answer removed; its errors", {
  d <- asm_rows(withr::local_tempdir(), asm_five())
  expect_null(tfl_ai_assumption_answers_read(d))
  tfl_ai_assumption_answers_write(d, data.frame(
    id = c("A1", "K2"), verdict = c("ok", "wrong"), correct = c(NA, "1 / 2 coded")))
  x <- tfl_ai_assumption_answers_read(d)
  expect_named(x, c("id", "scope", "output_id", "verdict", "correct"))
  expect_identical(x$scope, c("report", "all"))
  expect_identical(x$output_id, c("T-1", NA))
  # a row with neither verdict nor correct removes the id's answer
  tfl_ai_assumption_answers_write(d, data.frame(id = "A1", verdict = "", correct = ""))
  expect_identical(tfl_ai_assumption_answers_read(d)$id, "K2")
  # (a correct with no verdict: wrong)
  tfl_ai_assumption_answers_write(d, data.frame(id = "a5", correct = "one line"))
  expect_identical(tfl_ai_assumption_answers_read(d)$verdict, c("wrong", "wrong"))
  expect_error(tfl_ai_assumption_answers_write(d, data.frame(id = "A9", verdict = "ok")),
               "no assumption A9")
  expect_error(tfl_ai_assumption_answers_write(d, data.frame(id = "A1", verdict = "yes")),
               "`ok` or `wrong`")
  expect_error(tfl_ai_assumption_answers_write(d, data.frame(id = "A1", verdict = "wrong")),
               "no `correct`")
  expect_error(tfl_ai_assumption_answers_write(d, data.frame(id = "A1", scope = "all", verdict = "ok")),
               "scope `report`")
  expect_error(tfl_ai_assumption_answers_write(d, data.frame(id = "A1", output_id = "T-2",
                                                             verdict = "ok")),
               "an assumption of T-1")
  expect_error(tfl_ai_assumption_answers_write(d, data.frame(id = "A5", verdict = "ok"),
                                               overwrite = FALSE), "answered already")
  expect_error(tfl_ai_assumption_answers_write(d, data.frame(id = "X1")), "A<n>")
})

test_that("the status: a kind's answer for each of its reports, a report's own wins", {
  d <- asm_rows(withr::local_tempdir(), asm_five())
  tfl_ai_assumption_answers_write(d, data.frame(
    id = c("K2", "A4", "A1"), verdict = c("wrong", "ok", "ok"), correct = c("1 / 2 coded", NA, NA)))
  a <- tfl_ai_assumptions(d)
  expect_identical(a$status, c("confirmed", "open", "answered", "confirmed", "open"))
  expect_identical(a$verdict, c("ok", NA, "wrong", "ok", NA))
  expect_identical(a$correct[3L], "1 / 2 coded")
  # the agent did the section: done; a row it fixed: done
  tfl_ai_corrections(d)
  f <- file.path(d, "ai", "corrections.md")
  x <- readLines(f, encoding = "UTF-8")
  x[x == "- Status: open"] <- "- Status: done"
  writeLines(x, f)
  expect_identical(tfl_ai_assumptions(d)$status[3L], "done")
  r <- asm_five()
  r[[5]]$status <- "fixed"
  asm_rows(d, r)
  expect_identical(tfl_ai_assumptions(d)$status[5L], "done")
})

test_that("ai/corrections.md: a report's and a kind's sections; kept, changed, withdrawn", {
  d <- asm_rows(withr::local_tempdir(), asm_five())
  tfl_ai_assumption_answers_write(d, data.frame(
    id = c("A5", "K2", "A4", "A2"), verdict = c("wrong", "wrong", "ok", "ok"),
    correct = c("three lines", "1 / 2 coded", NA, NA)))
  new <- tfl_ai_corrections(d)
  expect_identical(new, c("## A5. assumption of T-3: titles",
                          "## K2. assumption of every report: SEX"))
  f <- file.path(d, "ai", "corrections.md")
  x <- readLines(f, encoding = "UTF-8")
  expect_true(all(c("- Was: two lines", "- Correct: three lines", "- Reports: T-1",
                    "- Was: M / F", "- Correct: 1 / 2 coded") %in% x))
  expect_identical(sum(x == "- Status: open"), 2L)
  # nothing new: nothing added; a correct changed while open: changed
  expect_identical(tfl_ai_corrections(d), character())
  tfl_ai_assumption_answers_write(d, data.frame(id = "A5", verdict = "wrong", correct = "3 lines"))
  expect_identical(tfl_ai_corrections(d), character())
  x <- readLines(f, encoding = "UTF-8")
  expect_true("- Correct: 3 lines" %in% x)
  expect_false("- Correct: three lines" %in% x)
  expect_identical(length(grep("^## A5[.]", x)), 1L)
  # the agent did K2; A5's answer removed: withdrawn, K2 kept done
  k <- grep("^## K2", x)
  x[k + match("- Status: open", x[k:length(x)]) - 1L] <- "- Status: done"
  writeLines(x, f)
  tfl_ai_assumption_answers_write(d, data.frame(id = "A5"))
  tfl_ai_corrections(d)
  y <- readLines(f, encoding = "UTF-8")
  expect_identical(sum(y == "- Status: withdrawn"), 1L)
  expect_identical(sum(y == "- Status: done"), 1L)
  expect_identical(length(grep("^## [AK][0-9]+[.]", y)), 2L)
})

test_that("ai/questions.md: a question answered through its assumptions, the person's answer kept", {
  r <- asm_five()
  r[[6]] <- list(output_id = "T-4", where = "data", item = "visit", assumption = "C2D1",
                 question = "Which visit?")
  d <- asm_rows(withr::local_tempdir(), r)
  a <- tfl_ai_assumptions(d)
  q <- .ai_questions_update(d, a)
  qf <- file.path(d, "ai", "questions.md")
  x <- readLines(qf, encoding = "UTF-8")
  expect_true(all(c("- Assumptions: A1, A2", "- Assumptions: A6") %in% x))
  expect_identical(tfl_ai_questions(d)$answer, c(NA_character_, NA_character_))
  # the same assumptions again: the file as it was; a new report asking it: added
  .ai_questions_update(d, a)
  expect_identical(readLines(qf, encoding = "UTF-8"), x)
  # the person answered Q2 there
  tfl_ai_answer_questions(d, c(Q2 = "the worst value"))
  # A1 alone: Q1 not yet answered
  tfl_ai_assumption_answers_write(d, data.frame(id = "A1", verdict = "ok"))
  expect_true(is.na(tfl_ai_questions(d)$answer[1L]))
  tfl_ai_assumption_answers_write(d, data.frame(id = c("A2", "A6"), verdict = "wrong",
                                                correct = c("TRT01P", "C3D1")))
  q <- tfl_ai_questions(d)
  expect_identical(q$answer[1L], "A1: confirmed\nA2: TRT01P")
  expect_identical(q$answer[2L], "the worst value")
  # the answers removed: the answer written from them emptied
  tfl_ai_assumption_answers_write(d, data.frame(id = "A2"))
  expect_true(is.na(tfl_ai_questions(d)$answer[1L]))
  # and through the kind
  tfl_ai_assumption_answers_write(d, data.frame(id = "K1", verdict = "wrong", correct = "TRT01P"))
  tfl_ai_assumption_answers_write(d, data.frame(id = "A1"))
  expect_identical(tfl_ai_questions(d)$answer[1L], "A1, A2: TRT01P")
  # tfl_ai_corrections(): the answered question's section, as before
  new <- tfl_ai_corrections(d)
  expect_true(any(startsWith(new, "## Q1 (answered).")))
  expect_true(any(startsWith(new, "## Q2 (answered).")))
})

test_that("collected answers: ids kept; an assumption the agent fixed survives a collection", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  ad <- file.path(root, "adam")
  dir.create(ad, recursive = TRUE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(ad, paste0(x, ".xpt")))
  }
  d <- file.path(root, "study")
  book <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
  tfl_ai_prepare(unit = "toc", d, book, adam = normalizePath(ad, winslash = "/"),
                 quiet = TRUE, size = 3L)
  ai_drafts_from_fixtures(d)
  res <- suppressMessages(tfl_ai_collect_answers(d))
  expect_named(res$assumptions, c("output_id", "where", "item", "assumption", "value",
                                  "question", "id", "kind"))
  expect_true(all(grepl("^A[0-9]+$", res$assumptions$id)))
  expect_true(all(grepl("^K[0-9]+$", res$assumptions$kind)))
  expect_false(anyDuplicated(res$assumptions$id) > 0L)
  res2 <- suppressMessages(tfl_ai_collect_answers(d))
  # (the usual widths, written into the draft by the first collection, stay an
  # assumption of the draft's: the rows and their ids are the same)
  expect_identical(res2$assumptions, res$assumptions)
  # the demographics' arm is wrong; the agent applies it and marks the row fixed
  a <- tfl_ai_assumptions(d)
  id <- a$id[a$output_id %in% "T-14-1-1" & a$item %in% "arm"]
  expect_length(id, 1L)
  f <- file.path(d, "spec", "_assumptions.yml")
  y <- yaml::read_yaml(f)
  at <- which(vapply(y, function(e) identical(e$id, id), NA))
  y[[at]]$assumption <- "the columns are the planned treatment, TRT01P"
  y[[at]]$status <- "fixed"
  writeLines(yaml::as.yaml(y, indent.mapping.sequence = TRUE), f)
  # collected again: the answer still says TRT01A -- the fixed row kept, a note
  res3 <- suppressMessages(tfl_ai_collect_answers(d))
  expect_identical(res3$assumptions$id, res2$assumptions$id)
  y3 <- yaml::read_yaml(f)
  r <- Filter(function(e) identical(e$id, id), y3)[[1L]]
  expect_identical(r$status, "fixed")
  expect_identical(r$assumption, "the columns are the planned treatment, TRT01P")
  expect_match(r$note, "the answer still says", fixed = TRUE)
})

test_that("an assumption's value -- the value the AI used -- is kept and shown", {
  rows <- asm_five()
  rows[[1L]]$assumption <- "Treatment variable for the columns: TRT01A (actual), because the set is SAF"
  rows[[1L]]$value <- "TRT01A"
  rows[[5L]]$value <- list("Table 14.1.1", "Demographics")   # a list: joined
  d <- asm_rows(withr::local_tempdir(), rows)
  a <- tfl_ai_assumptions(d)
  expect_named(a, c("id", "kind", "n_reports", "output_id", "where", "item", "assumption",
                    "value", "question", "verdict", "correct", "status"))
  expect_identical(a$value, c("TRT01A", NA, NA, NA, "Table 14.1.1, Demographics"))
  # written back with the ids, the value stays, after the assumption
  y <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  expect_identical(y[[1L]]$value, "TRT01A")
  expect_identical(names(y[[1L]]), c("id", "kind", "output_id", "where", "item", "assumption",
                                     "value", "question"))
  expect_null(y[[2L]]$value)
})

test_that("a field tflspec does not know survives the file being written back", {
  rows <- asm_five()
  rows[[1L]]$value <- "TRT01A"
  rows[[1L]]$source <- "shell"              # a person's own field
  rows[[3L]]$checked_by <- list("A", "B")
  d <- asm_rows(withr::local_tempdir(), rows)
  a <- tfl_ai_assumptions(d)                # stamps the ids: the file is written back
  expect_identical(a$value[1L], "TRT01A")
  y <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  expect_identical(y[[1L]]$source, "shell")
  expect_identical(y[[1L]]$value, "TRT01A")
  expect_identical(y[[3L]]$checked_by, "A, B")
  expect_null(y[[2L]]$source)
  expect_false("source" %in% names(a))      # the table shows the known columns
})
