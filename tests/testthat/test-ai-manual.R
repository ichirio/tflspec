# The AI user manual (inst/ai/tflspec-ai-user-manual.md) is the briefing a
# user attaches to a chat session.  Its whole value is that it is TRUE: an
# assistant cannot tell a stale manual from a fresh one, so what it claims
# is checked here rather than by eye.

.ai_file <- function() {
  system.file("ai", "tflspec-ai-user-manual.md", package = "tflspec")
}

.ai_lines <- function() {
  p <- .ai_file()
  skip_if_not(nzchar(p) && file.exists(p), "manual not installed")
  readLines(p, encoding = "UTF-8", warn = FALSE)
}

# the code the manual shows: inside ``` fences
.ai_code <- function(lines) {
  fence <- grepl("^```", lines)
  lines[cumsum(fence) %% 2L == 1L & !fence]
}

test_that("the manual is installed and tflspec_ai_manual() finds it", {
  expect_true(nzchar(.ai_file()) && file.exists(.ai_file()))
  expect_identical(tflspec_ai_manual(), .ai_file())
})

test_that("the manual states the version it documents", {
  v <- as.character(utils::packageVersion("tflspec"))
  head5 <- .ai_lines()[1:5]
  expect_true(any(grepl(v, head5, fixed = TRUE)),
              info = paste("the manual does not state", v,
                           "in its first five lines -- bump the stamp"))
})

test_that("its API list is exactly the package's exports", {
  lines <- .ai_lines()
  from <- grep("^## 13\\. Complete public API", lines)
  to   <- grep("^## 14\\.", lines)
  expect_length(from, 1L)
  expect_length(to, 1L)
  block <- lines[seq(from + 1L, to - 1L)]
  listed <- gsub("`", "", unique(unlist(regmatches(
    block, gregexpr("`[A-Za-z_][A-Za-z0-9_.]*`", block)))), fixed = TRUE)
  exported <- getNamespaceExports("tflspec")
  expect_identical(setdiff(exported, listed), character(0),
                   info = "exported but not in the manual's API list")
  expect_identical(setdiff(listed, exported), character(0),
                   info = "in the manual's API list but not exported")
})

test_that("every tflspec function the manual names exists", {
  lines <- .ai_lines()
  # but for the table of names that do not
  wrong <- seq(grep("^## 12\\.", lines), grep("^## 13\\.", lines) - 1L)
  lines <- lines[-wrong]
  named <- unique(unlist(regmatches(lines, gregexpr("\\btfl(spec)?_[a-z_]+\\b", lines))))
  named <- setdiff(named, c("tfl_ard_normalize", "tfl_plan", "tfl_apply_plan",
                            "tfl_plan_"))  # the former names of section 11
  named <- named[!startsWith(named, "tfl_plan_")]
  expect_identical(setdiff(named, getNamespaceExports("tflspec")), character(0))
})

test_that("its code never calls a former name", {
  code <- .ai_code(.ai_lines())
  former <- c("tfl_ard_normalize", "tfl_plan", "tfl_apply_plan", "spread_ard",
              "plan_fmt", "plan_header_style", "plan_col_style",
              "plan_zone_style")
  hits <- former[vapply(former, function(f)
    any(grepl(paste0("\\b", f, "\\("), code)), NA)]
  expect_identical(hits, character(0))
  expect_false(any(grepl("\\b(stub_into|group_show|colpages_carry|pages_by)\\b", code)))
})

test_that("tflspec_ai_manual(file = ) copies it out", {
  dest <- tempfile(fileext = ".md")
  on.exit(unlink(dest), add = TRUE)
  expect_identical(tflspec_ai_manual(file = dest), dest)
  expect_identical(readLines(dest, warn = FALSE),
                   readLines(.ai_file(), warn = FALSE))
  expect_error(tflspec_ai_manual(file = dest), "overwrite")
  expect_no_error(tflspec_ai_manual(file = dest, overwrite = TRUE))
  d <- file.path(tempdir(), "ai-copy")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_identical(basename(tflspec_ai_manual(file = d)),
                   "tflspec-ai-user-manual.md")
})

# the R code blocks only (```r), not the text / yaml ones
.ai_r_code <- function(lines) {
  open <- grepl("^```r[[:space:]]*$", lines)
  fence <- grepl("^```", lines)
  inside <- logical(length(lines)); on <- FALSE
  for (i in seq_along(lines)) {
    if (fence[i]) { on <- !on && open[i]; next }
    inside[i] <- on
  }
  lines[inside]
}

# the "Names that do NOT exist" table: its left column
.ai_wrong_names <- function(lines) {
  from <- grep("^## 12\\. Names that do NOT exist", lines)
  to   <- grep("^## 13\\.", lines)
  expect_length(from, 1L)
  block <- lines[seq(from + 1L, to - 1L)]
  rows <- grep("^\\|", block, value = TRUE)[-(1:2)]
  left <- vapply(strsplit(rows, "|", fixed = TRUE), `[`, "", 2L)
  nm <- unlist(regmatches(left, gregexpr("`[A-Za-z_][A-Za-z0-9_.:]*", left)))
  unique(gsub("`", "", nm, fixed = TRUE))
}

test_that("the names the manual calls wrong are not exports, nor in its code", {
  lines <- .ai_lines()
  wrong <- .ai_wrong_names(lines)
  expect_gt(length(wrong), 5L)
  # layout column names are not functions
  fns <- wrong[!wrong %in% c("stub_into", "group_show", "colpages_carry", "pages_by")]
  code <- .ai_code(lines)
  # `pkg::fn`: that spelling is the mistake (the function is another
  # package's), so the spelling must not be in the code
  qual <- fns[grepl("::", fns, fixed = TRUE)]
  expect_false(any(vapply(qual, function(q)
    any(grepl(paste0(q, "("), code, fixed = TRUE)), NA)))
  # a bare name: a row about an argument names a real function; a row about
  # a name names one that exists nowhere
  bare <- setdiff(fns[!grepl("::", fns, fixed = TRUE)],
                  getNamespaceExports("rtfreporter"))
  expect_identical(intersect(bare, getNamespaceExports("tflspec")), character(0))
  used <- bare[vapply(bare, function(f)
    any(grepl(paste0("\\b", f, "\\("), code)), NA)]
  expect_identical(used, character(0))
})

test_that("the named arguments of tflspec calls in its code are real", {
  code <- paste(.ai_r_code(.ai_lines()), collapse = "\n")
  exprs <- tryCatch(parse(text = code, keep.source = FALSE),
                    error = function(e) NULL)
  skip_if(is.null(exprs), "the manual's code blocks are not all R")
  ex <- getNamespaceExports("tflspec")
  bad <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    f <- e[[1L]]
    fn <- if (is.name(f)) as.character(f) else if (is.call(f) &&
      identical(f[[1L]], as.name("::"))) as.character(f[[3L]]) else ""
    if (fn %in% ex) {
      fm <- names(formals(getExportedValue("tflspec", fn)))
      nm <- setdiff(names(as.list(e)[-1L]) %||% character(), "")
      if (!"..." %in% fm) bad <<- c(bad, paste0(fn, "(", setdiff(nm, fm), " = )")[length(setdiff(nm, fm)) > 0L])
    }
    for (a in as.list(e)[-1L]) walk(a)
  }
  for (e in exprs) walk(e)
  expect_identical(bad, character(0))
})

# a fenced block of the manual, found by its first line
.ai_example <- function(marker) {
  lines <- .ai_lines()
  start <- grep(marker, lines, fixed = TRUE)
  expect_length(start, 1L)
  end <- start + which(startsWith(lines[-seq_len(start)], "```"))[1L] - 1L
  lines[start:end]
}

test_that("the manual's survival and model rows run as written, to the hand-written ARD", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  skip_if_not_installed("broom.helpers")
  e <- new.env()
  eval(parse(text = .ai_example("# manual example: analyses rows for survival")), e)
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # (as the program makes it: dplyr, a column keeps its label)
  data <- dplyr::filter(adam$ADTTE,
                        USUBJID %in% dplyr::filter(adam$ADSL, SAFFL == "Y")$USUBJID)
  hand <- list(
    KM = cardx::ard_survival_survfit(data, y = "survival::Surv(AVAL, 1 - CNSR)",
                                     variables = TRTA, times = c(30, 90)),
    HR = cardx::ard_regression(data, formula = survival::Surv(AVAL, 1 - CNSR) ~ TRTA,
                               method = "coxph", package = "survival"))
  for (r in list(e$km, e$cox)) {
    r$output_id <- "T"
    got <- suppressMessages(suppressWarnings(
      tfl_build_ard(exact_spec(r), dir = dir, save = FALSE)))
    expect_identical(exact_numbers(got), exact_numbers(hand[[r$analysis_id]]),
                     label = r$analysis_id)
  }
})

test_that("the manual's table rows run as written, to the table written in code", {
  skip_if_not_installed("cards")
  e <- new.env()
  eval(parse(text = .ai_example("# manual example: a table's tables and cells rows")), e)
  adsl <- cards::ADSL
  adae <- cards::ADAE
  ard <- cards::ard_stack_hierarchical(adae, by = TRTA, variables = c(AESOC, AEDECOD),
                                       denominator = adsl, id = USUBJID)
  d <- suppressMessages(normalize_ard(ard, hierarchy = c("AESOC", "AEDECOD")))
  sp <- tfl_table_spec(tables = e$tables, cells = e$cells)
  by_spec <- suppressMessages(plan_apply(
    plan_cells(tfl_table_plan(d, sp), notes = FALSE), "pages"))
  by_code <- suppressMessages(plan_apply(
    table_plan(d, cols = "TRTA", rows = c(group1 = "AESOC"),
               label = c(label = "AEDECOD")) |>
      plan_cells(notes = FALSE) |>
      plan_cells("{n} ({p:.1f%})"), "pages"))
  expect_identical(tflspec:::.spec_rtf(by_spec), tflspec:::.spec_rtf(by_code))
})
