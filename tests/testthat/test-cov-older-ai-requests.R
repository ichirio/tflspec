# The branches no other test reaches in R/ai_requests.R: the requests'
# arguments checked, the sheets' requests for an agent that reads files,
# a request added over one there, and each next step of
# tfl_ai_collect_answers().  Error and edge paths first.  Fictional data only.

cov_req_ns <- function() asNamespace("tflspec")

cov_req_shell <- function() {
  ex <- system.file("ai", "example", package = "tflspec")
  tfl_ai_read_shell(file.path(ex, "onco_toc_shell.xlsx"))
}

cov_req_study <- function(env = parent.frame()) {
  skip_if_not_installed("readxl")
  d <- withr_tempdir(env)
  dir.create(file.path(d, "adam"))
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    saveRDS(df, file.path(d, "adam", paste0(x, ".rds")))
  }
  suppressMessages(tfl_ai_study_init(d, study_id = "XYZ-01"))
  list(dir = d, shell = cov_req_shell(), adam = tfl_ai_adam_summary(file.path(d, "adam")))
}

# ---- tfl_ai_write_requests(): the arguments ------------------------------------

test_that("tfl_ai_write_requests(): `progress` not a function stops", {
  s <- cov_req_study()
  expect_error(tfl_ai_write_requests(s$dir, s$shell, s$adam, progress = "yes"),
               "`progress` is a function(step, i, n), or NULL.", fixed = TRUE)
})

test_that("tfl_ai_write_requests(): a shell that is no tfl_ai_read_shell() result (and has no path) stops", {
  s <- cov_req_study()
  sh <- unclass(s$shell)
  sh$path <- NULL
  expect_error(tfl_ai_write_requests(s$dir, sh, s$adam),
               "`shell` is a tfl_ai_read_shell() result.", fixed = TRUE)
})

test_that("tfl_ai_write_requests(): `max_chars` under 1000 stops", {
  s <- cov_req_study()
  expect_error(tfl_ai_write_requests(s$dir, s$shell, s$adam, max_chars = 10),
               "`max_chars` is one number, 1000 or more, or NULL.", fixed = TRUE)
  expect_error(tfl_ai_write_requests(s$dir, s$shell, s$adam, max_chars = NA_real_),
               "`max_chars` is one number", fixed = TRUE)
})

test_that("tfl_ai_write_requests(unit = \"toc\"): types none of the TOC has stops, naming them", {
  s <- cov_req_study()
  expect_error(tfl_ai_write_requests(s$dir, s$shell, s$adam, unit = "toc", types = "listing"),
               "none of the TOC's 5 reports is a listing", fixed = TRUE)
  expect_false(dir.exists(file.path(s$dir, "ai", "requests")))
})

test_that("tfl_ai_write_requests(unit = \"toc\", types = \"figure\"): the figures alone, no table", {
  s <- cov_req_study()
  r <- suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam, unit = "toc", types = "figure"))
  expect_identical(r$request, "figure-1")
  expect_identical(r$type, "figure")
  expect_identical(r$outputs, "F-14-2-2")
})

test_that("tfl_ai_write_requests(max_chars = ): one report over the limit is a warning naming it", {
  s <- cov_req_study()
  w <- character()
  r <- withCallingHandlers(
    suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam, unit = "toc", types = "table",
                                           max_chars = 1000)),
    warning = function(e) {
      w <<- c(w, conditionMessage(e))
      invokeRestart("muffleWarning")
    })
  expect_identical(nrow(r), 4L)
  expect_length(w, 4L)
  expect_match(w[1L], "characters with one report (T-14-1-1), over `max_chars` (1000)", fixed = TRUE)
})

# ---- tfl_ai_write_requests(): the shell sheets -----------------------------------

test_that("tfl_ai_write_requests(workbook = \"file\"): a sheet's request names the workbook and its sheet", {
  s <- cov_req_study()
  r <- suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam, workbook = "file"))
  x <- readLines(file.path(s$dir, r$file[r$request == "Safety"]), encoding = "UTF-8")
  expect_true(any(grepl("The TOC / SHELL workbook is the file `.*onco_toc_shell.xlsx`; this request's sheet\\(s\\): 'Safety'.", x)))
})

test_that("tfl_ai_write_requests(examples = ): a sheet's request takes an example of the study's by its path", {
  s <- cov_req_study()
  dir.create(file.path(s$dir, "spec", "outputs"), recursive = TRUE, showWarnings = FALSE)
  tfl_write_output(tfl_output("T-99-1", "table", label = "An example report"),
                   file.path(s$dir, "spec", "outputs", "T-99-1.yml"))
  r0 <- suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam))
  r1 <- suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam, overwrite = TRUE,
                                               examples = file.path("spec", "outputs", "T-99-1.yml")))
  tab <- r1$type == "table"
  expect_true(all(r1$chars[tab] > r0$chars[match(r1$request[tab], r0$request)]))
})

test_that("tfl_ai_write_requests(examples = ): named for another family, a sheet's request has none", {
  s <- cov_req_study()
  r0 <- suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam))
  r1 <- suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam, overwrite = TRUE,
                                               examples = c(no_such_family = "x.yml")))
  expect_identical(r1$chars, r0$chars)
})

# ---- tfl_ai_write_requests(add = TRUE) -------------------------------------------

test_that("tfl_ai_write_requests(add, overwrite): a report its old request had and no longer asked for is said", {
  s <- cov_req_study()
  suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam, unit = "toc", types = "table"))
  cf <- file.path(s$dir, "ai", "requests", "_contexts.rds")
  ctx <- readRDS(cf)
  ctx[[1L]]$ids <- c(ctx[[1L]]$ids, "T-99-9-9")
  saveRDS(ctx, cf)
  m <- character()
  withCallingHandlers(
    tfl_ai_write_requests(s$dir, s$shell, s$adam, unit = "toc", types = "table",
                          add = TRUE, overwrite = TRUE),
    message = function(e) {
      m <<- c(m, conditionMessage(e))
      invokeRestart("muffleMessage")
    })
  expect_true(any(grepl("T-99-9-9 no longer asked for (their request T-14-1-1", m, fixed = TRUE)))
})

test_that("tfl_ai_write_requests(add = TRUE): the sheets' requests asked again stop, a number a sheet makes counted", {
  s <- cov_req_study()
  suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam))
  sh <- s$shell
  sh$sheet_numbers <- data.frame(sheet = "Safety", number = "Table 14.3.9", toc_id = NA_character_,
                                 make = TRUE, source = "person", stringsAsFactors = FALSE)
  expect_error(suppressMessages(tfl_ai_write_requests(s$dir, sh, s$adam, add = TRUE)),
               "T-14-1-1, T-14-2-1, F-14-2-2, T-14-3-1, T-14-3-5 already asked for (T14.1.1, T14.2.1, Safety, F14.2.2)",
               fixed = TRUE)
})

# ---- tfl_ai_requests() ---------------------------------------------------------

test_that("tfl_ai_requests(): a request whose file was removed has NA characters", {
  s <- cov_req_study()
  r <- suppressMessages(tfl_ai_write_requests(s$dir, s$shell, s$adam, unit = "toc", types = "table"))
  file.remove(file.path(s$dir, r$file[1L]))
  x <- tfl_ai_requests(s$dir)
  expect_identical(x$chars[1L], NA_integer_)
  expect_false(anyNA(x$chars[-1L]))
})

# ---- tfl_ai_collect_answers(): each next step ------------------------------------

# One request of the four tables, the sample answer's drafts
cov_collect_study <- function(env = parent.frame()) {
  s <- cov_req_study(env)
  shell <- s$shell
  shell$toc$family <- "demographics"
  suppressMessages(tfl_ai_write_requests(s$dir, shell, s$adam, unit = "toc", types = "table", size = 8))
  ex <- system.file("ai", "example", "onco-answer.md", package = "tflspec")
  suppressMessages(ai_drafts_from_answer(s$dir, ex, "table"))
  s$dir
}
cov_no_problem <- function(...) {
  data.frame(severity = character(), where = character(), problem = character(),
             stringsAsFactors = FALSE)
}

test_that("tfl_ai_collect_answers(): an answer that cannot be parsed: fix_answer, the error its first problem", {
  d <- cov_collect_study()
  res <- with_mocked_bindings(suppressMessages(tfl_ai_collect_answers(d)),
                              tfl_ai_parse = function(...) stop("the answer is cut"),
                              .package = "tflspec")
  expect_identical(unname(res$next_kind), "fix_answer")
  expect_true(all(res$reports$status == "error"))
  expect_true(all(res$reports$first == "the answer is cut"))
})

test_that("tfl_ai_collect_answers(): a check that stops: answer_again, the error its first problem", {
  d <- cov_collect_study()
  res <- with_mocked_bindings(suppressMessages(tfl_ai_collect_answers(d)),
                              tfl_ai_check = function(...) stop("the check stopped"),
                              .package = "tflspec")
  expect_identical(unname(res$next_kind), "answer_again")
  expect_true(all(res$reports$first == "the check stopped"))
})

test_that("tfl_ai_collect_answers(): report files that cannot be written: see_problem, the reports errors", {
  d <- cov_collect_study()
  res <- with_mocked_bindings(suppressMessages(tfl_ai_collect_answers(d)),
                              tfl_ai_check = cov_no_problem,
                              tfl_ai_unpack = function(...) stop("the disk is full"),
                              .package = "tflspec")
  expect_identical(unname(res$next_kind), "see_problem")
  expect_true(all(res$reports$status == "error"))
  expect_true(all(res$reports$first == "the disk is full"))
})

test_that("tfl_ai_collect_answers(): a report of the request with no draft: add_missing, its draft named", {
  d <- cov_collect_study()
  unlink(file.path(d, "ai", "spec", "outputs", "T-14-3-5.yml"))
  res <- with_mocked_bindings(suppressMessages(tfl_ai_collect_answers(d)), tfl_ai_check = cov_no_problem,
                              .package = "tflspec")
  expect_identical(unname(res$next_kind), "add_missing")
  a <- res$next_args[[1L]]
  expect_identical(a$missing, "T-14-3-5")
  expect_identical(a$drafts, "ai/spec/outputs/T-14-3-5.yml")
})

test_that("tfl_ai_collect_answers(): an answer with no report: none_asked, 'no report'", {
  d <- cov_collect_study()
  res <- with_mocked_bindings(suppressMessages(tfl_ai_collect_answers(d)),
                              tfl_ai_parse = function(...) list(outputs = list()),
                              tfl_ai_check = cov_no_problem,
                              .package = "tflspec")
  expect_identical(unname(res$next_kind), "none_asked")
  expect_match(res$next_steps[[1L]], "(they are no report)", fixed = TRUE)
  expect_true(all(res$reports$first == "not in the answer (it has no report)"))
})

test_that("tfl_ai_collect_answers(): an answer of other reports only: none_asked, naming them", {
  d <- cov_collect_study()
  real <- tfl_ai_parse
  res <- with_mocked_bindings(suppressMessages(tfl_ai_collect_answers(d)),
                              tfl_ai_parse = function(...) {
                                a <- real(...)
                                names(a$outputs) <- paste0("X-", seq_along(a$outputs))
                                a
                              },
                              tfl_ai_check = cov_no_problem,
                              .package = "tflspec")
  expect_identical(unname(res$next_kind), "none_asked")
  expect_match(res$next_steps[[1L]], "(they are X-1, X-2, X-3, X-4)", fixed = TRUE)
  expect_identical(res$next_args[[1L]]$has, c("X-1", "X-2", "X-3", "X-4"))
})

test_that("tfl_ai_collect_answers(): a report the answer gives in parts: its parts are the reports", {
  d <- cov_collect_study()
  real <- tfl_ai_parse
  res <- with_mocked_bindings(suppressMessages(tfl_ai_collect_answers(d)),
                              tfl_ai_parse = function(...) {
                                a <- real(...)
                                i <- match("T-14-1-1", names(a$outputs))
                                a$outputs <- c(a$outputs[-i],
                                               list(`T-14-1-1a` = a$outputs[[i]],
                                                    `T-14-1-1b` = a$outputs[[i]]))
                                a$outputs$`T-14-1-1a`$id <- "T-14-1-1a"
                                a$outputs$`T-14-1-1b`$id <- "T-14-1-1b"
                                a
                              },
                              tfl_ai_check = cov_no_problem,
                              .package = "tflspec")
  expect_true(all(c("T-14-1-1a", "T-14-1-1b") %in% res$reports$output_id))
  expect_false("T-14-1-1" %in% res$reports$output_id)
})

# ---- the answer's internals ------------------------------------------------------

test_that(".ai_sheet_answer_ids(): an answer with no report keeps the ids asked for", {
  expect_identical(cov_req_ns()$.ai_sheet_answer_ids("T-1", list(outputs = list()), "r1", list()), "T-1")
})

test_that(".ai_manifest_rows(): a report listed under another name is the answer's; one not listed is added", {
  a <- list(outputs = list(`T-14-1-1` = list(label = "Demographics"), `T-14-9-9` = list(label = "More")),
            report_list = data.frame(output_id = c("Table 14.1.1", "Table 16.1.1"),
                                     title = c("Demographics", "Listed only"),
                                     split = NA_character_, toc = "14.1.1", stringsAsFactors = FALSE))
  m <- cov_req_ns()$.ai_manifest_rows("q1", a)
  # (a listed report with no report file keeps its listed id)
  expect_identical(m$output_id, c("T-14-1-1", "Table 16.1.1", "T-14-9-9"))
  expect_identical(m$listed, c(TRUE, TRUE, FALSE))
  expect_identical(m$title, c("Demographics", "Listed only", "More"))
})

test_that(".ai_answer_as_asked(): a report named by its number is renamed, its assumptions follow; one with no number stays", {
  a <- list(outputs = list(foo = list(id = "foo"), `Table 14.1.1` = list(id = "Table 14.1.1")),
            assumption_rows = data.frame(output_id = "Table 14.1.1", where = "w",
                                         stringsAsFactors = FALSE))
  x <- cov_req_ns()$.ai_answer_as_asked(a, "T-14-1-1")
  expect_identical(names(x$outputs), c("foo", "T-14-1-1"))
  expect_identical(x$outputs[[2L]]$id, "T-14-1-1")
  expect_identical(x$assumption_rows$output_id, "T-14-1-1")
})

test_that(".ai_answer_overall(): an overall row with no hierarchical analysis is left alone", {
  o <- list(content = list(list(fn = "normalize_ard", args = list(overall = "Any AE"))),
            ard = list(analyses = list(list(id = "a1", method = "cards::ard_tabulate"))))
  a <- list(outputs = list(T1 = o))
  expect_identical(cov_req_ns()$.ai_answer_overall(a), a)
})

test_that(".ai_answer_overall(): a hierarchical analysis with arguments gets over_variables added; the assumption in Japanese", {
  o <- list(content = list(list(fn = "normalize_ard", args = list(overall = "Any AE"))),
            ard = list(analyses = list(list(id = "h1", method = "hierarchical",
                                            args = list("by = TRT01A")))))
  x <- cov_req_ns()$.ai_answer_overall(list(outputs = list(T1 = o)), "ja")
  expect_identical(unclass(x$outputs$T1$ard$analyses[[1L]]$args)[1L], "by = TRT01A, over_variables = TRUE")
  expect_identical(x$assumption_rows$item, "h1")
  expect_match(x$assumption_rows$assumption, "over_variables = TRUE \u3092\u8ffd\u52a0", fixed = TRUE)
})

# ---- print.tfl_ai_collected() ----------------------------------------------------

test_that("print.tfl_ai_collected(): the manifest's TOC rows, reports with no draft and reports not listed", {
  x <- structure(list(
    reports = data.frame(request = "q1", output_id = c("T-1", "T-2"), status = c("ok", "warning"),
                         first = c(NA, "a warning"), stringsAsFactors = FALSE),
    next_steps = c(q1 = "read the warnings"),
    manifest = data.frame(request = "q1", toc = c("14.1.1", "14.1.1", NA),
                          output_id = c("T-1", "T-2", "T-3"), title = NA_character_,
                          split = NA_character_, listed = c(TRUE, FALSE, TRUE),
                          status = c("ok", "warning", "not answered"), reason = NA_character_,
                          stringsAsFactors = FALSE),
    manifest_file = "ai/manifest.csv"), class = "tfl_ai_collected")
  out <- capture.output(print(x))
  line <- grep("^Manifest", out, value = TRUE)
  expect_identical(line, paste0("Manifest (ai/manifest.csv): 3 report(s) from 1 TOC row(s) -- ",
                                "1 in a request's `reports` list but with no draft, ",
                                "1 in a draft but not in its request's `reports` list"))
})

test_that("print.tfl_ai_collected(): a manifest with no TOC row and nothing odd: the count alone", {
  x <- structure(list(
    reports = data.frame(request = "q1", output_id = "T-1", status = "ok", first = NA_character_,
                         stringsAsFactors = FALSE),
    next_steps = NULL,
    manifest = data.frame(request = "q1", toc = NA_character_, output_id = "T-1",
                          title = NA_character_, split = NA_character_, listed = NA,
                          status = "ok", reason = NA_character_, stringsAsFactors = FALSE)),
    class = "tfl_ai_collected")
  out <- capture.output(print(x))
  expect_true("Manifest (ai/manifest.csv): 1 report(s)" %in% out)
  expect_false(any(grepl("next:", out, fixed = TRUE)))
})
