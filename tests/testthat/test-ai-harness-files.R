# tfl_ai_prepare(): the files for the agent's environments (Claude Code,
# VS Code with Copilot, the Copilot app): ai/CLAUDE.md, ai/.claude/settings.json,
# the workspace's settings, and the first message that starts with AGENTS.md

harness_study <- function() {
  d <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_prepare(unit = "toc", d, system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec"),
                 adam = "adam", quiet = TRUE, size = 3L)
  d
}
harness_lines <- function(d, ...) readLines(file.path(d, "ai", ...), warn = FALSE, encoding = "UTF-8")

test_that("ai/CLAUDE.md imports AGENTS.md; ai/.claude/settings.json allows run.R and denies the rest", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- harness_study()
  expect_identical(harness_lines(d, "CLAUDE.md"),
                   c("<!-- tflspec: this file makes Claude Code read AGENTS.md -->", "@AGENTS.md"))
  s <- jsonlite::fromJSON(paste(harness_lines(d, ".claude", "settings.json"), collapse = "\n"))
  p <- s$permissions
  expect_true("Bash(Rscript ../run.R *)" %in% p$allow)
  for (k in c("spec/outputs/**", "decisions.md", "progress.md", "questions-end.md")) {
    expect_true(all(paste0(c("Edit(", "Write("), k, ")") %in% p$allow), info = k)
  }
  expect_setequal(p$deny, c("Bash(pip *)", "Bash(curl *)", "Bash(wget *)", "Bash(Rscript -e *)",
                            "Bash(R *)", "WebFetch", "WebSearch", "Read(../**)", "Edit(../**)"))
  expect_true(p$blockReadsOutsideWorkingDirectories)
  expect_true(p$blockWritesOutsideWorkingDirectories)
  # nothing is allowed that is also denied
  expect_length(intersect(p$allow, p$deny), 0L)
})

test_that("the workspace holds the settings: AGENTS.md, instruction files, run.R auto-approved alone", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- harness_study()
  ws <- jsonlite::fromJSON(paste(harness_lines(d, "study.code-workspace"), collapse = "\n"),
                           simplifyVector = FALSE)
  expect_identical(ws$folders[[1L]]$path, ".")
  st <- ws$settings
  expect_true(st$chat.useAgentsMdFile)
  expect_true(st$github.copilot.chat.codeGeneration.useInstructionFiles)
  expect_true(st$chat.tools.terminal.enableAutoApprove)
  rules <- st$chat.tools.terminal.autoApprove
  expect_length(rules, 1L)
  expect_true(rules[[1L]])
  re <- substr(names(rules), 2L, nchar(names(rules)) - 1L)
  expect_true(grepl(re, "Rscript ../run.R T-14-1-1"))
  expect_false(grepl(re, "Rscript ../run.R T-14-1-1; rm -r x"))
  expect_false(grepl(re, "Rscript -e 1"))
  expect_false(grepl(re, "pip install x"))
})

test_that("the first message starts with AGENTS.md; so does the new-chat text; AGENTS-root names the one command", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- harness_study()
  p <- paste(harness_lines(d, "agent_prompt.md"), collapse = "\n")
  expect_true(startsWith(p, "First read AGENTS.md in this folder and follow it. "))
  expect_false(grepl("ai/AGENTS.md", p, fixed = TRUE))
  expect_true(startsWith(tflspec:::.ai_agent_prompt("s"), "First read ai/AGENTS.md in this folder and follow it. "))
  x <- tflspec:::.ai_spec_agents("{{per_chat}}", list(requests_per_chat = 5L, language = "en", carry = "x",
                                                      review = c(table = 1L), footnotes_at = "bottom",
                                                      titles_in_header = character()),
                                 list(one = "", batch = ""))
  expect_match(x, "Open a new chat and give it ai/agent_prompt.md (it starts: First read AGENTS.md in this folder and follow it)",
               fixed = TRUE)
  for (f in c(file.path(d, "AGENTS.md"), file.path(d, ".github", "copilot-instructions.md"),
              file.path(d, "ai", ".github", "copilot-instructions.md"))) {
    expect_true("The only command you may run: `Rscript ../run.R <request>`." %in%
                  readLines(f, warn = FALSE, encoding = "UTF-8"), info = f)
  }
})

test_that("a CLAUDE.md or settings.json of the person's is not written over; the kit's own is on a rerun", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- harness_study()
  cm <- file.path(d, "ai", "CLAUDE.md")
  st <- file.path(d, "ai", ".claude", "settings.json")
  writeLines(c("# my notes", "@AGENTS.md", "Be brief."), cm)
  writeLines("{\"permissions\": {\"allow\": [\"Bash(ls)\"]}}", st)
  expect_message(tflspec:::.ai_claude_files(d, overwrite = TRUE), "ai/CLAUDE.md is not the kit's own")
  expect_identical(readLines(cm), c("# my notes", "@AGENTS.md", "Be brief."))
  expect_identical(readLines(st), "{\"permissions\": {\"allow\": [\"Bash(ls)\"]}}")
  # the kit's own: not written without overwrite, written again with it
  unlink(c(cm, st))
  expect_length(tflspec:::.ai_claude_files(d, overwrite = FALSE), 2L)
  writeLines(c(readLines(cm), "# edited"), cm)
  expect_length(tflspec:::.ai_claude_files(d, overwrite = FALSE), 0L)
  expect_true("# edited" %in% readLines(cm))
  expect_length(tflspec:::.ai_claude_files(d, overwrite = TRUE), 2L)
  expect_false("# edited" %in% readLines(cm))
  # tfl_ai_refresh_agents() writes them again as well
  unlink(cm)
  tfl_ai_refresh_agents(d)
  expect_true(file.exists(cm))
})
