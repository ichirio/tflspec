# The agent's spec route: it asks in the chat, writes the drafts
# (ai/spec/outputs/<id>.yml) with their `ai:` block, keeps the answers in
# ai/decisions.md; the person's check reads the drafts as it reads the
# answers, and what is marked to confirm is listed.

rs_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  d
}
rs_book <- function() system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")

test_that("decisions: written as structured lines, read back; the older lines and a person's own too", {
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "ai"), recursive = TRUE)
  f <- tfl_ai_decisions_write(d, data.frame(scope = c("all", "ADAE"), topic = c("treatment_var.SAF", "teae.where"),
                                            decision = c("TRT01A", "TRTEMFL == \"Y\""),
                                            source = c("person", "chat, T-14-3-1")))
  expect_true(file.exists(f))
  cat("- Q2 (T-14-1-1, T-14-1-2): the arms in the code list's order\n",
      "- a footnote on every table says the data cut\n", file = f, append = TRUE, sep = "")
  r <- tfl_ai_decisions_read(d)
  expect_identical(r$id[1:2], c("D1", "D2"))
  expect_identical(r$scope, c("all", "ADAE", "T-14-1-1, T-14-1-2", "all"))
  expect_identical(r$topic[1:2], c("treatment_var.SAF", "teae.where"))
  expect_identical(r$decision[2], "TRTEMFL == \"Y\"")
  expect_identical(r$source[2], "chat, T-14-3-1")
  expect_identical(r$source[4], "person")
  # ids go on from the last
  tfl_ai_decisions_write(d, data.frame(scope = "T-14-3-1", topic = "denominator", decision = "the arm's subjects"))
  r2 <- tfl_ai_decisions_read(d)
  expect_identical(utils::tail(r2$id, 1L), "D3")
  # what applies to a report, under the carry settings
  a <- tflspec:::.ai_decisions_for(d, "T-14-3-5", datasets = "ADAE", section = NA_character_,
                                   settings = tflspec:::.ai_spec_defaults)
  expect_identical(a$how[a$id %in% c("D1", "D2")], c("apply", "apply"))
  expect_identical(a$how[a$id %in% "D3"], "ask")
  expect_identical(a$how[a$scope == "T-14-1-1, T-14-1-2"], "ask")
  narrow <- tflspec:::.ai_spec_defaults; narrow$carry <- "all"
  b <- tflspec:::.ai_decisions_for(d, "T-14-3-5", datasets = "ADAE", settings = narrow)
  expect_identical(b$how[b$id %in% "D2"], "ask")
  blk <- tflspec:::.ai_decisions_block(d, "T-14-3-5", datasets = "ADAE")
  expect_true(any(grepl("^## Decisions that apply", blk)))
  expect_true(any(grepl("D1 \\[all\\]", blk)))
})

test_that("the settings: the package's, under study.yml's ai: keys; the language's name", {
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(d)
  s <- tflspec:::.ai_spec_settings(d)
  expect_identical(s$max_rounds, 3L)
  expect_identical(s$ask_when, "request")
  expect_identical(s$footnotes_at, "footer")
  writeLines(c("study_id: X", "ai:", "  language: ja", "  max_rounds: 2", "  carry: [all]"),
             file.path(d, "study.yml"))
  s2 <- tflspec:::.ai_spec_settings(d)
  expect_identical(s2$language, "ja")
  expect_identical(s2$max_rounds, 2L)
  expect_identical(s2$carry, "all")
  # the study standard says where the footnotes go; study.yml wins over it
  dir.create(file.path(d, "spec"))
  writeLines(c("ai:", "  footnotes_at: table"), file.path(d, "spec", "defaults.yml"))
  expect_identical(tflspec:::.ai_spec_settings(d)$footnotes_at, "table")
  expect_match(tflspec:::.ai_footnotes_text("table"), "page: footnotes", fixed = TRUE)
  expect_match(tflspec:::.ai_footnotes_text("footer"), "page: footer", fixed = TRUE)
  writeLines(c("study_id: X", "ai:", "  footnotes_at: footer"), file.path(d, "study.yml"))
  expect_identical(tflspec:::.ai_spec_settings(d)$footnotes_at, "footer")
  expect_identical(tflspec:::.ai_language_name("ja"), "Japanese")
  expect_identical(tflspec:::.ai_language_name("Deutsch"), "Deutsch")
})

test_that("prepare(): AGENTS.md asks and writes the drafts; the requests carry the decisions; the check reads the drafts", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  root <- withr::local_tempdir()
  d <- rs_study(root)
  tfl_ai_prepare(unit = "toc", d, rs_book(), adam = "adam", quiet = TRUE, size = 3L)
  ag <- readLines(file.path(d, "ai", "AGENTS.md"), warn = FALSE, encoding = "UTF-8")
  expect_true(any(grepl("<!-- tflspec route: spec -->", ag, fixed = TRUE)))
  expect_true(any(grepl("ai/spec/outputs/<id>.yml", ag, fixed = TRUE)))
  expect_true(any(grepl("three times at most", ag, fixed = TRUE)))
  expect_true(any(grepl("page: footer", ag, fixed = TRUE)))
  expect_true(any(grepl("in English", ag, fixed = TRUE)))
  expect_false(any(grepl("{{", ag, fixed = TRUE)))
  expect_false(dir.exists(file.path(d, "ai", "answers")))
  dec <- readLines(file.path(d, "ai", "decisions.md"), warn = FALSE, encoding = "UTF-8")
  expect_true(any(grepl("D<n> \\[<scope>\\]", dec)))
  # the shared instructions say what a draft holds, once
  ins <- readLines(file.path(d, "ai", "instructions-table.md"), warn = FALSE, encoding = "UTF-8")
  expect_identical(sum(startsWith(ins, "## The skeleton (what a draft holds)")), 1L)
  expect_true(any(grepl("page: footer", ins, fixed = TRUE)))
  pr <- readLines(file.path(d, "ai", "agent_prompt.md"), warn = FALSE, encoding = "UTF-8")
  expect_true(any(grepl("ask me here, in English", pr, fixed = TRUE)))
  expect_true(any(grepl("ai/spec/outputs/<id>.yml", pr, fixed = TRUE)))
  # a request's head says the route; with a decision, the request lists it
  idx <- tfl_ai_requests(d)
  req <- idx$request[idx$outputs == "T-14-3-1" | grepl("T-14-3-1", idx$outputs)][1L]
  expect_false(is.na(req))
  rq <- readLines(file.path(d, "ai", "requests", paste0(req, ".md")), warn = FALSE, encoding = "UTF-8")
  expect_true(any(grepl("<!-- tflspec: agent -->", rq, fixed = TRUE)))
  expect_false(any(grepl("ai/answers", rq, fixed = TRUE)))
  tfl_ai_decisions_write(d, data.frame(scope = "all", topic = "treatment_var.SAF", decision = "TRT01A", source = "person"))
  tfl_ai_prepare(unit = "toc", d, rs_book(), adam = "adam", quiet = TRUE, size = 3L)
  rq <- readLines(file.path(d, "ai", "requests", paste0(req, ".md")), warn = FALSE, encoding = "UTF-8")
  expect_true(any(grepl("^## Decisions that apply", rq)))
  expect_true(any(grepl("D1 [all] treatment_var.SAF: TRT01A", rq, fixed = TRUE)))
  # no draft yet: the next step says to ask the agent
  r0 <- suppressMessages(tfl_ai_collect_answers(d))
  expect_true(all(r0$reports$status == "no answer"))
  expect_identical(r0$stray, character(0))
  expect_true(any(grepl("ask the agent", r0$next_steps)))
  # the agent's draft: the fixture answer's report, with an ai: block
  a <- tfl_ai_parse(readLines(test_path("fixtures", "answers-toc", "ae-1.md"), warn = FALSE), "table")
  o <- a$outputs[["T-14-3-1"]]
  o$ai <- list(
    assumptions = list(list(where = "ard.analyses", item = "where", assumption = "treatment-emergent is TRTEMFL == \"Y\"",
                            value = "TRTEMFL == \"Y\"", from = "D1")),
    confirm = list(list(where = "content", item = "plan_cells", why = "the shell shows n (%) but not its denominator")))
  area <- file.path(d, "ai", "spec", "outputs")
  dir.create(area, recursive = TRUE, showWarnings = FALSE)
  tfl_write_output(o, file.path(area, "T-14-3-1.yml"))
  r <- suppressMessages(tfl_ai_collect_answers(d))
  st <- r$reports[r$reports$output_id == "T-14-3-1", ]
  expect_true(nrow(st) == 1L && st$status %in% c("ok", "warning"), info = st$problems_text)
  expect_identical(r$next_kind[[req]], "done")
  # no answers folder: the drafts are read in a folder of the check's own;
  # the draft written again with its ai: block
  expect_false(dir.exists(file.path(d, "ai", "answers")))
  back <- tfl_read_output(file.path(area, "T-14-3-1.yml"))
  expect_identical(back$ai$confirm[[1L]]$item, "plan_cells")
  expect_identical(back$ai$assumptions[[1L]]$from, "D1")
  # the assumptions numbered, their `from` kept
  asm <- tfl_ai_assumptions(d)
  expect_true(any(asm$output_id == "T-14-3-1" & grepl("treatment-emergent", asm$assumption)))
  af <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  expect_true(any(vapply(af, function(e) identical(e$from, "D1"), NA)))
  # what is to confirm: listed, and a `check` row of tfl_check_spec()
  cf <- tfl_ai_confirm(d)
  expect_identical(cf$item, "plan_cells")
  expect_match(cf$file, "ai/spec/outputs/T-14-3-1.yml", fixed = TRUE)
  suppressMessages(tfl_ai_adopt(d, "T-14-3-1"))
  p <- tfl_check_spec(d, "T-14-3-1")
  expect_true(any(p$level == "check" & grepl("to confirm", p$message)))
  expect_identical(nrow(tfl_ai_confirm(d, "adopted")), 1L)
  # a draft YAML cannot read: refused with why, its request's step says the file
  writeLines(c("id: T-14-3-5", "kind: table", "label: [unclosed"), file.path(area, "T-14-3-5.yml"))
  r2 <- suppressMessages(tfl_ai_collect_answers(d))
  req2 <- idx$request[grepl("T-14-3-5", idx$outputs)][1L]
  st2 <- r2$reports[r2$reports$output_id == "T-14-3-5", ]
  expect_identical(st2$status, "error")
  expect_match(st2$first, "T-14-3-5.yml: the YAML cannot be read", fixed = TRUE)
  expect_identical(r2$next_kind[[req2]], "fix_answer")
  expect_match(r2$next_args[[req2]]$drafts, "ai/spec/outputs/T-14-3-5.yml", fixed = TRUE)
  # a draft no request asks for: stray
  tfl_write_output(tfl_output("T-99-9-9", kind = "table", label = "x"), file.path(area, "T-99-9-9.yml"))
  r3 <- suppressMessages(tfl_ai_collect_answers(d))
  expect_identical(r3$stray, "ai/spec/outputs/T-99-9-9.yml")
})
