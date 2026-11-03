# The company's reference pack (#547, step 3): learned conventions adopted
# into the standard's, written for an agent, copied into each study

learned_file <- function(d) {
  f <- file.path(d, "learned.yml")
  writeLines(c("tfl_company_hints: {version: 1, files: 3, tables: 9}",
               "kinds:",
               "  all:",
               "    font: {value: \"Courier New 9 pt\", seen: 3, of: 3}",
               "  demog:",
               "    n_form: {value: \"(N=xx)\", seen: 3, of: 3}",
               "    zero: {value: \"0\", seen: 1, of: 3}",
               "    continuous: {value: \"n | Mean (SD) | Median | Min, Max\", seen: 2, of: 3}"), f)
  f
}

test_that("tfl_ai_style_adopt(): the proposed rows by default, or those chosen", {
  d <- withr::local_tempdir()
  std <- data.frame(kind = c("all", "all"), key = c("font", "n_form"),
                    value = c("Courier New 9 pt", "N = xx"))
  x <- tfl_ai_style_diff(learned_file(d), std)
  a <- tfl_ai_style_adopt(x, standard = std)
  # the same font stays the standard's; demog's own N form and statistics are added;
  # zero (1 of 3) is by hand, so not adopted
  expect_identical(a$kind, c("all", "all", "demog", "demog"))
  expect_identical(a$key, c("font", "n_form", "n_form", "continuous"))
  expect_identical(a$value, c("Courier New 9 pt", "N = xx", "(N=xx)",
                              "n | Mean (SD) | Median | Min, Max"))
  expect_identical(a$from, c("standard", "standard", "learned", "learned"))
  # chosen by hand: the zero form only
  b <- tfl_ai_style_adopt(x, rows = which(x$key == "zero"), standard = std)
  expect_identical(b$key, c("font", "n_form", "zero"))
  expect_identical(b$from, c("standard", "standard", "learned"))
  # a standard's own value replaced
  std2 <- rbind(std, data.frame(kind = "demog", key = "zero", value = "-"))
  c2 <- tfl_ai_style_adopt(tfl_ai_style_diff(learned_file(d), std2), rows = 3L, standard = std2)
  expect_identical(c2$value[c2$kind == "demog" & c2$key == "zero"], "0")
  expect_error(tfl_ai_style_adopt(data.frame(a = 1)), "tfl_ai_style_diff")
  expect_error(tfl_ai_style_adopt(x, rows = 99L, standard = std), "rows of `diff`")
})

test_that("tfl_ai_reference_build(): the conventions by kind, for an agent", {
  d <- withr::local_tempdir()
  conv <- data.frame(kind = c("demog", "all", "demog"), key = c("n_form", "font", "zero"),
                     value = c("(N=xx)", "Courier New 9 pt", ""))
  f <- tfl_ai_reference_build(conv, dir = file.path(d, "ref"), notes = "Example Pharma's own words.")
  expect_true(all(file.exists(f)))
  x <- readLines(f[["conventions"]], encoding = "UTF-8")
  expect_true("Example Pharma's own words." %in% x)
  # every kind first; an empty value left out
  expect_identical(grep("^## ", x, value = TRUE), c("## Every kind", "## `demog`"))
  expect_true(any(startsWith(x, "- `font`: Courier New 9 pt")))
  expect_true(any(startsWith(x, "- `n_form`: (N=xx)")))
  expect_false(any(grepl("`zero`", x, fixed = TRUE)))
  expect_true(any(grepl("from: \"company convention\"", x, fixed = TRUE)))
  expect_error(tfl_ai_reference_build(list()), "`kind`, `key` and `value`")
})

test_that("a study's agent files: the company's files copied, AGENTS.md's second step reads them first", {
  d <- withr::local_tempdir()
  study <- file.path(d, "study")
  dir.create(study)
  home <- file.path(d, "ai")
  withr::local_options(tflspec.ai_home = home)
  agents <- function() paste(readLines(file.path(study, "ai", "AGENTS.md"), encoding = "UTF-8"),
                             collapse = "
")
  ref <- function(f) file.path(study, "ai", "reference", f)
  hints <- function(v) writeLines(c("tfl_company_hints: {version: 1, files: 3, tables: 9}",
                                    "kinds:", "  all:",
                                    sprintf("    font: {value: \"%s\", seen: 3, of: 3}", v)),
                                  file.path(home, "company_hints.yml"))
  # neither: the usual second step
  tflspec:::.ai_agent_files(study)
  expect_match(agents(), tflspec:::.ai_step2_usual, fixed = TRUE)
  expect_false(dir.exists(file.path(study, "ai", "reference")))
  # hints only
  dir.create(home)
  hints("Courier New 9 pt")
  w <- tflspec:::.ai_agent_files(study, overwrite = TRUE)
  expect_true(file.exists(ref("company_hints.yml")))
  expect_true(ref("company_hints.yml") %in% w)
  expect_false(file.exists(ref("conventions.md")))
  a <- agents()
  expect_false(grepl(tflspec:::.ai_step2_usual, a, fixed = TRUE))
  expect_match(a, "**Else the company's hints, then the usual convention, settle it.**", fixed = TRUE)
  expect_match(a, "from: \"company hints\"", fixed = TRUE)
  expect_false(grepl("company convention", a, fixed = TRUE))
  # (its paths from ai/, the agent's folder)
  expect_match(a, "`reference/company_hints.yml` is how this company's past reports were made", fixed = TRUE)
  expect_match(a, "A point under `conflicts:` is the standard's", fixed = TRUE)
  # unchanged: not copied again; changed: copied
  w2 <- tflspec:::.ai_agent_files(study, overwrite = TRUE)
  expect_false(any(grepl("reference", w2, fixed = TRUE)))
  hints("Arial 10 pt")
  w3 <- tflspec:::.ai_agent_files(study, overwrite = TRUE)
  expect_true(ref("company_hints.yml") %in% w3)
  expect_true(any(grepl("Arial 10 pt", readLines(ref("company_hints.yml")), fixed = TRUE)))
  # both
  tfl_ai_reference_build(data.frame(kind = "demog", key = "n_form", value = "(N=xx)"))
  w4 <- tflspec:::.ai_agent_files(study, overwrite = TRUE)
  expect_true(ref("conventions.md") %in% w4)
  a <- agents()
  expect_match(a, "**Else the company's conventions, then its hints, then the usual convention, settle it.**",
               fixed = TRUE)
  expect_match(a, "`reference/conventions.md` is the company standard's, by kind of report", fixed = TRUE)
  expect_match(a, "from: \"company convention\"", fixed = TRUE)
  expect_match(a, "from: \"company hints\"", fixed = TRUE)
  # conventions only
  unlink(file.path(home, "company_hints.yml"))
  unlink(file.path(study, "ai", "reference"), recursive = TRUE)
  tflspec:::.ai_agent_files(study, overwrite = TRUE)
  expect_false(file.exists(ref("company_hints.yml")))
  a <- agents()
  expect_match(a, "**Else the company's conventions, then the usual convention, settle it.**", fixed = TRUE)
  expect_match(a, "from: \"company convention\"", fixed = TRUE)
  expect_false(grepl("company hints", a, fixed = TRUE))
  expect_false(grepl(tflspec:::.ai_step2_usual, a, fixed = TRUE))
  # changed conventions: copied
  tfl_ai_reference_build(data.frame(kind = "demog", key = "n_form", value = "N = xx"))
  w5 <- tflspec:::.ai_agent_files(study, overwrite = TRUE)
  expect_true(ref("conventions.md") %in% w5)
  expect_true(any(grepl("N = xx", readLines(ref("conventions.md")), fixed = TRUE)))
  # the usual step stays once in AGENTS.md's own text
  src <- readLines(system.file("ai", "agent", "AGENTS.md", package = "tflspec"), encoding = "UTF-8")
  expect_identical(lengths(regmatches(paste(src, collapse = "
"),
                                      gregexpr(tflspec:::.ai_step2_usual, paste(src, collapse = "
"),
                                               fixed = TRUE))), 1L)
})
