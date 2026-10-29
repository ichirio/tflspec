# tfl_ai_prepare(unit = "toc", size = 3L) and tfl_ai_run(): the study folder as a
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

test_that("the agent's files: AGENTS.md, run.R, decisions; toc.yml; the paths; the first message", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  ad <- agent_study(root)
  r <- tfl_ai_prepare(unit = "toc", d, ex_book(), adam = ad, quiet = TRUE)
  expect_false("mode" %in% names(r))
  # the default: one report a request, named after the report
  expect_identical(r$requests$request, c("T-14-1-1", "T-14-3-1", "T-14-3-5", "T-14-2-1", "F-14-2-2"))
  have <- c("AGENTS.md", ".github/copilot-instructions.md", "run.R",
            "ai/decisions.md", "spec/toc.yml")
  expect_true(all(have %in% r$files))
  expect_true(all(file.exists(file.path(d, have))))
  expect_true(dir.exists(file.path(d, "ai", "spec", "outputs")))
  expect_false(dir.exists(file.path(d, "ai", "answers")))
  expect_false(file.exists(file.path(d, "ai", "requests", "00-instructions.md")))
  ag <- readLines(file.path(d, "AGENTS.md"), encoding = "UTF-8")
  expect_identical(readLines(file.path(d, ".github", "copilot-instructions.md"), encoding = "UTF-8"), ag)
  for (k in c("<!-- tflspec route: spec -->", "ai/decisions.md", "three times at most",
              "ai/spec/outputs/<id>.yml")) {
    expect_true(any(grepl(k, ag, fixed = TRUE)), info = k)
  }
  expect_false(any(grepl("Rscript run.R", ag, fixed = TRUE)))
  expect_false(any(grepl("{{", ag, fixed = TRUE)))
  # the first message: the first two requests, then a stop
  expect_true(is.character(r$agent_prompt) && length(r$agent_prompt) == 1L)
  expect_match(r$agent_prompt, sprintf("Do only ai/requests/%s.md and ai/requests/%s.md",
                                       r$requests$request[1L], r$requests$request[2L]), fixed = TRUE)
  expect_match(r$agent_prompt, "ask me here, in English,", fixed = TRUE)
  expect_match(r$agent_prompt, "ai/spec/outputs/<id>.yml", fixed = TRUE)
  # each request starts with the agent's head
  req1 <- readLines(file.path(d, r$requests$file[1L]), encoding = "UTF-8")
  expect_identical(req1[1L], "<!-- tflspec: agent -->")
  expect_match(req1[2L], "> **For the agent**", fixed = TRUE)
  expect_match(req1[2L], sprintf("ai/spec/outputs/%s.yml", r$requests$request[1L]), fixed = TRUE)
  expect_identical(r$agent_files, character())
  for (f in c("ai/instructions-table.md", "ai/instructions-figure.md", "ai/templates.md")) {
    expect_true(file.exists(file.path(d, f)), info = f)
  }
  # the next requests: those whose reports have no draft yet, two at a time
  nx1 <- tfl_ai_agent_prompt(d, n = 2L)
  expect_identical(as.character(nx1), r$agent_prompt)
  expect_identical(attr(nx1, "requests"), r$requests$request[1:2])
  mark <- function(q, on = TRUE) {
    ids <- strsplit(r$requests$outputs[r$requests$request == q], ", ")[[1L]]
    f <- file.path(d, "ai", "spec", "outputs", paste0(ids, ".yml"))
    if (on) {
      for (i in seq_along(ids)) {
        tfl_write_output(tfl_output(ids[i], kind = "table", label = "x"), f[i])
      }
    } else {
      unlink(f)
    }
  }
  for (q in r$requests$request[1:2]) mark(q)
  nx2 <- tfl_ai_agent_prompt(d, n = 1L)
  expect_identical(attr(nx2, "requests"), r$requests$request[3L])
  expect_match(nx2, sprintf("Do only ai/requests/%s.md, ", r$requests$request[3L]), fixed = TRUE)
  expect_identical(attr(tfl_ai_agent_prompt(d, requests = r$requests$request[5L]), "requests"),
                   r$requests$request[5L])
  expect_error(tfl_ai_agent_prompt(d, requests = "nope"), "No request 'nope'")
  for (q in r$requests$request) mark(q)
  expect_true(is.na(tfl_ai_agent_prompt(d)))
  for (q in r$requests$request) mark(q, FALSE)
  expect_identical(tfl_ai_requests(d)$chars, r$requests$chars)
  # tflplanner's places, and its TOC
  st <- yaml::read_yaml(file.path(d, "study.yml"))
  expect_identical(st$settings$output_path, "output/tfl")
  expect_identical(st$settings$program_dir, "programs/tfl")
  toc <- yaml::read_yaml(file.path(d, "spec", "toc.yml"))$outputs
  expect_identical(vapply(toc, `[[`, "", "id"), r$toc$toc_id)
  expect_identical(toc[[1L]]$section, "14.1")
  expect_true(all(vapply(toc, function(e) !is.null(e$population), NA)))
  expect_true(any(grepl("VS Code", r$next_steps, fixed = TRUE)))
  expect_length(r$hint, 0L)
})

test_that("the ADaM inside the study folder is fine", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  agent_study(root, adam_inside = TRUE)
  r <- tfl_ai_prepare(unit = "toc", d, ex_book(), adam = "adam", quiet = TRUE, size = 3L)
  expect_length(r$hint, 0L)
  expect_true(file.exists(file.path(d, "AGENTS.md")))
})

test_that("tfl_ai_run(): the RTFs, ai/status.md; a second prepare keeps the decisions", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  ad <- agent_study(root)
  tfl_ai_prepare(unit = "toc", d, ex_book(), adam = ad, quiet = TRUE, size = 3L)
  ai_drafts_from_fixtures(d)
  res <- suppressMessages(tfl_ai_run(d))
  rp <- res$reports
  expect_true(all(file.exists(file.path(d, rp$rtf))))
  # (a new study: the AI's drafts and their RTF apart, ai/)
  expect_true(all(startsWith(rp$rtf, "ai/output/tfl/")))
  status <- readLines(file.path(d, "ai", "status.md"), encoding = "UTF-8")
  expect_true(any(grepl("5 of 5 reports have an RTF", status, fixed = TRUE)))
  # a second prepare keeps the decisions
  writeLines(c(readLines(file.path(d, "ai", "decisions.md")), "- Q1 (T-14-1-1): TRT01P"),
             file.path(d, "ai", "decisions.md"))
  dec <- readLines(file.path(d, "ai", "decisions.md"))
  tfl_ai_prepare(unit = "toc", d, ex_book(), adam = ad, quiet = TRUE, size = 3L, overwrite = TRUE)
  expect_identical(readLines(file.path(d, "ai", "decisions.md")), dec)
})

test_that("tfl_ai_run(): a report built again only when it changed; one request's; a build's repair", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("ggsurvfit")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  ad <- agent_study(root, adam_inside = TRUE)
  tfl_ai_prepare(unit = "toc", d, ex_book(), adam = "adam", quiet = TRUE, size = 3L)
  ai_drafts_from_fixtures(d)
  built <- function(res) stats::setNames(res$reports$built, res$reports$output_id)
  r1 <- suppressMessages(tfl_ai_run(d))
  expect_true(all(built(r1) == "built"))
  expect_true(file.exists(file.path(d, "ai", "built.csv")))
  rtf <- file.path(d, r1$reports$rtf)
  t1 <- file.mtime(rtf)
  # nothing changed: every report kept, its RTF as it was
  r2 <- suppressMessages(tfl_ai_run(d))
  expect_true(all(built(r2) == "kept"))
  expect_identical(r2$reports$rtf, r1$reports$rtf)
  expect_identical(file.mtime(rtf), t1)
  # one draft changed: its report alone
  a <- file.path(d, "ai", "spec", "outputs", "T-14-1-1.yml")
  writeLines(sub("label: Demographic and Baseline Characteristics",
                 "label: Demographic and Baseline Characteristics (ITT)", readLines(a, encoding = "UTF-8"),
                 fixed = TRUE), a, useBytes = TRUE)
  r3 <- suppressMessages(tfl_ai_run(d))
  dm <- r3$reports$request == "demographics-1"
  expect_true(all(r3$reports$built[dm] == "built"))
  expect_true(all(r3$reports$built[!dm] == "kept"))
  # one request asked for: its reports, the others as the last run left them
  r4 <- suppressMessages(tfl_ai_run(d, requests = "ae-1", rebuild = TRUE))
  ae <- r4$reports$request == "ae-1"
  expect_true(all(r4$reports$built[ae] == "built"))
  expect_true(all(r4$reports$built[!ae] == "kept"))
  expect_true(all(!is.na(r4$reports$rtf)))
  status <- readLines(file.path(d, "ai", "status.md"), encoding = "UTF-8")
  expect_true(any(grepl("5 of 5 reports have an RTF", status, fixed = TRUE)))
  expect_error(tfl_ai_run(d, requests = "ae-9"), "no request 'ae-9'")
  # a report whose build fails (its data gone): a repair says why
  file.remove(file.path(d, "adam", "adae.xpt"))
  r5 <- suppressMessages(tfl_ai_run(d))
  bad <- r5$reports$request == "ae-1"
  expect_true(all(!is.na(r5$reports$rtf_problem[bad])))
  bf <- file.path(d, "ai", "repairs", "ae-1-build.md")
  rp <- readLines(bf, encoding = "UTF-8")
  expect_true("## Could not be built" %in% rp)
  expect_true(any(grepl("Rscript run.R ae-1", rp, fixed = TRUE)))
  expect_false(file.exists(file.path(d, "ai", "repairs", "ae-1.md")))   # (the checks passed)
  # a collect in between (a screen's reload) leaves it for the agent to read
  suppressMessages(tfl_ai_collect_answers(d))
  expect_identical(readLines(bf, encoding = "UTF-8"), rp)
  # (the ADaM changed: every other report built again, and fine)
  expect_true(all(r5$reports$built[!bad] == "built"))
  # its RTF of the last run that built it: gone, not listed
  expect_false(any(file.exists(rtf[r1$reports$request == "ae-1"])))
  expect_true(all(is.na(r5$reports$rtf[bad])))
  # a run of another request: ae-1 still as its last build left it
  r5b <- suppressMessages(tfl_ai_run(d, requests = "demographics-1"))
  expect_identical(r5b$reports$rtf_problem[bad], r5$reports$rtf_problem[bad])
  status <- readLines(file.path(d, "ai", "status.md"), encoding = "UTF-8")
  nx <- grep("^- ae-1: ", status, value = TRUE)
  expect_match(nx, "could not be built", fixed = TRUE)
  expect_match(nx, "ai/repairs/ae-1-build.md", fixed = TRUE)
  expect_true(file.exists(bf))
  # the data back: the request's reports built, its build repair gone
  ad2 <- agent_study(file.path(root, "again"))
  file.copy(file.path(ad2, "adae.xpt"), file.path(d, "adam", "adae.xpt"))
  r6 <- suppressMessages(tfl_ai_run(d, requests = "ae-1"))
  expect_true(all(is.na(r6$reports$rtf_problem)))
  expect_false(file.exists(bf))
})

test_that("a study copied elsewhere: its answers checked and built against its own files; an agent's next steps", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("ggsurvfit")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  agent_study(root, adam_inside = TRUE)
  tfl_ai_prepare(unit = "toc", d, ex_book(), adam = "adam", quiet = TRUE, size = 3L)
  # the requests keep no path of this PC
  ctx <- readRDS(file.path(d, "ai", "requests", "_contexts.rds"))
  sd <- unlist(lapply(ctx, function(x) x$context$study_dir))
  expect_true(all(sd == "."))
  # copied to another place, the first one gone
  d2 <- file.path(root, "elsewhere", "study")
  dir.create(dirname(d2), recursive = TRUE)
  file.copy(d, dirname(d2), recursive = TRUE)
  unlink(d, recursive = TRUE)
  # the agent's wording, not a chat's
  ix <- paste(readLines(file.path(d2, "ai", "requests", "index.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(ix, "`ai/spec/outputs/<id>.yml`", fixed = TRUE)
  expect_false(grepl("new chat", ix, fixed = TRUE))
  r0 <- suppressMessages(tfl_ai_collect_answers(d2))
  expect_match(r0$next_steps[["ae-1"]], "ask the agent for ai/requests/ae-1.md", fixed = TRUE)
  expect_false(any(grepl("chat", r0$next_steps, fixed = TRUE)))
  # answered there: checked against its own study files, built
  ai_drafts_from_fixtures(d2)
  res <- suppressMessages(tfl_ai_run(d2))
  expect_true(all(res$reports$status %in% c("ok", "warning")))
  expect_true(all(file.exists(file.path(d2, res$reports$rtf))))
  expect_false(dir.exists(d))
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
  r <- tfl_ai_prepare(unit = "toc", d, book, adam = ad, quiet = TRUE, size = 3L,
                      agent_reads = c("workbook", "adam"))
  expect_identical(r$agent_files, c("shells/toc.xlsx", "adam"))
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
  r2 <- tfl_ai_prepare(unit = "toc", d2, ex_book(), adam = ad, quiet = TRUE, size = 3L)
  dy2 <- yaml::read_yaml(file.path(d2, "spec", "data.yml"))
  expect_true(all(startsWith(tolower(vapply(dy2$datasets, `[[`, "", "path")), tolower(ad))))
})

test_that("tfl_ai_run(): a request not built yet says so, after a run of another", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  agent_study(root, adam_inside = TRUE)
  tfl_ai_prepare(unit = "toc", d, ex_book(), adam = "adam", quiet = TRUE, size = 3L)
  ai_drafts_from_fixtures(d, only = c("demographics-1", "ae-1"))
  r <- suppressMessages(tfl_ai_run(d, requests = "demographics-1"))
  ae <- r$reports$request == "ae-1"
  expect_true(all(is.na(r$reports$built[ae])))
  status <- readLines(file.path(d, "ai", "status.md"), encoding = "UTF-8")
  nx <- grep("^- ae-1: ", status, value = TRUE)
  expect_match(nx, "not built yet: run `Rscript run.R ae-1`", fixed = TRUE)
  expect_false(grepl("the report files are written", nx, fixed = TRUE))
  # (an ai/built.csv of an older tflspec, no problem column: read as built)
  f <- file.path(d, "ai", "built.csv")
  b <- utils::read.csv(f, colClasses = "character")
  utils::write.csv(b[setdiff(names(b), "problem")], f, row.names = FALSE)
  expect_true(all(is.na(tflspec:::.ai_built_read(d)$problem)))
})

test_that("a report not built for a missing package: no repair of the answer, the step says to install it", {
  expect_identical(.ai_missing_package(c("there is no package called 'cards'",
                                         "The figure's code needs the dplyr package: install it.",
                                         "object 'x' not found", NA)),
                   c("cards", "dplyr", NA, NA))
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "ai"), recursive = TRUE)
  r <- data.frame(request = c("a", "b"), output_id = c("T-1", "T-2"), status = "ok",
                  built = NA_character_, rtf = NA_character_,
                  rtf_problem = c("there is no package called 'cards'", "object 'x' not found"),
                  stringsAsFactors = FALSE)
  .ai_build_repairs(d, r)
  expect_false(file.exists(.ai_build_repair_file(d, "a")))
  expect_true(file.exists(.ai_build_repair_file(d, "b")))
})

test_that("the AI's drafts apart: run.R builds them into ai/output; tfl_ai_adopt() copies them; the adopted are not written over", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  ad <- agent_study(root)
  tfl_ai_prepare(unit = "toc", d, ex_book(), adam = ad, quiet = TRUE, size = 3L)
  expect_identical(tfl_ai_migrate(d), character())
  expect_true(dir.exists(file.path(d, "ai", "spec", "outputs")))
  ai_drafts_from_fixtures(d)
  rp <- suppressMessages(tfl_ai_run(d))$reports
  expect_true(all(startsWith(rp$rtf, "ai/output/tfl/")))
  expect_true(all(file.exists(file.path(d, rp$rtf))))
  expect_true(all(startsWith(rp$file, "ai/spec/outputs/")))
  expect_length(list.files(file.path(d, "spec", "outputs")), 0L)
  id <- rp$output_id[1L]
  expect_identical(tfl_ai_adopt(d, id)$status, "adopted")
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  expect_true(file.exists(f))
  expect_identical(tfl_ai_adopt(d, id)$status, "same")
  # a person changes the adopted file: a new round leaves it; adopting again asks
  writeLines(c(readLines(f), "# changed by hand"), f)
  suppressMessages(tfl_ai_run(d, rebuild = TRUE))
  expect_true(any(grepl("changed by hand", readLines(f), fixed = TRUE)))
  expect_message(a2 <- tfl_ai_adopt(d, id), "left as the study has them")
  expect_identical(a2$status, "kept")
  expect_identical(tfl_ai_adopt(d, id, overwrite = TRUE)$status, "adopted")
  expect_false(any(grepl("changed by hand", readLines(f), fixed = TRUE)))
  expect_identical(tfl_ai_adopt(d, "NOPE")$status, "no draft")
  log <- utils::read.csv(file.path(d, "ai", "adopted.csv"), stringsAsFactors = FALSE)
  expect_identical(log$output_id, id)
})

test_that("tfl_ai_migrate(): a study's AI reports copied apart and recorded as adopted", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  ad <- agent_study(root)
  # (a study of the form before: its drafts in spec/outputs)
  tfl_ai_prepare(unit = "toc", d, ex_book(), adam = ad, quiet = TRUE, size = 3L,
                 area = FALSE)
  made <- ai_drafts_from_fixtures(d)
  suppressMessages(tfl_ai_collect_answers(d))
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(d, "ai", "spec", "outputs", paste0(made, ".yml")),
            file.path(d, "spec", "outputs"))
  unlink(file.path(d, "ai", "spec"), recursive = TRUE)
  ids <- tfl_ai_migrate(d)
  expect_setequal(ids, made)
  expect_true(all(file.exists(file.path(d, "ai", "spec", "outputs", paste0(made, ".yml")))))
  log <- utils::read.csv(file.path(d, "ai", "adopted.csv"), stringsAsFactors = FALSE)
  expect_setequal(log$output_id, made)
  expect_error(tfl_ai_adopt(file.path(root, "nowhere")), "keeps the AI's drafts in spec/outputs")
})

test_that("mode = \"agent\": what the agent may open -- agent_reads, references", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- file.path(root, "study")
  ad <- agent_study(root)
  sap <- file.path(root, "sap.txt")
  writeLines("The primary analysis set is the FAS.", sap)
  r <- tfl_ai_prepare(unit = "toc", d, ex_book(), adam = ad, quiet = TRUE,
                      agent_reads = "adam", docs = sap)
  # the reference copied into the study folder; the ADaM allowed, the workbook not
  expect_true(file.exists(file.path(d, "ai", "docs", "sap.txt")))
  expect_identical(r$agent_files[2L], "ai/docs/sap.txt")
  expect_length(r$agent_files, 2L)
  ag <- paste(readLines(file.path(d, "AGENTS.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(ag, "`ai/docs/sap.txt`", fixed = TRUE)
  expect_match(ag, "- the ADaM in `", fixed = TRUE)
  expect_false(grepl("- the TOC / SHELL workbook", ag, fixed = TRUE))
  expect_match(ag, "Run a command only to read the files listed", fixed = TRUE)
  # any tool the PC has, not R alone; at most 3 reads a request
  expect_match(ag, "PowerShell, Python, R", fixed = TRUE)
  expect_match(ag, "At most 3 times for a request", fixed = TRUE)
  expect_false(grepl("{{", ag, fixed = TRUE))
  expect_match(r$agent_prompt, "install nothing", fixed = TRUE)
  expect_identical(as.character(tfl_ai_agent_prompt(d, n = 2L)), r$agent_prompt)
  req1 <- readLines(file.path(d, r$requests$file[1L]), encoding = "UTF-8")
  # what it does not take
  expect_error(tfl_ai_prepare(unit = "toc", file.path(root, "s2"), ex_book(), adam = ad,
                              quiet = TRUE, agent_reads = "sas"),
               "agent_reads = ): \"sas\"", fixed = TRUE)
  expect_error(tfl_ai_prepare(unit = "toc", file.path(root, "s3"), ex_book(), adam = ad,
                              quiet = TRUE, docs = file.path(root, "none.pdf")),
               "docs = ): no file", fixed = TRUE)
  expect_error(tfl_ai_prepare(unit = "toc", file.path(root, "s4"), ex_book(), adam = ad,
                              quiet = TRUE, agent_max_reads = "a"),
               "agent_max_reads = ): one number", fixed = TRUE)
})

test_that("what every report's build depends on names the packages' versions", {
  withr::local_dir(withr::local_tempdir())
  k <- tflspec:::.ai_built_common(list(data = list(datasets = list())))
  v <- function(p) if (requireNamespace(p, quietly = TRUE)) format(utils::packageVersion(p)) else "-"
  for (p in c("tflspec", "rtfreporter", "cards", "cardx", "ggplot2")) {
    expect_true(grepl(paste0("|", p, "|", v(p), "|"), paste0(k, "|"), fixed = TRUE), label = p)
  }
})
