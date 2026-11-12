# A page configuration is a style set: complete for the page step (the
# settings and the fixed lines of the four bands in two blocks, `first` and
# `last`); the page step reads it, the template, the report and what is
# forced over it, and nothing from the study's files.

pc_dir <- function(defaults = NULL, study = NULL, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  file.copy(list.files(src, full.names = TRUE), d, recursive = TRUE)
  if (!is.null(defaults)) writeLines(defaults, file.path(d, "defaults.yml"))
  if (!is.null(study)) writeLines(study, file.path(d, "study.yml"))
  d
}
pc_defaults <- c("content:", "  - plan_cells: {categorical: \"{n} ({p:.1f%})\"}")
pc_acme <- c(
  "styles:",
  "  acme:",
  "    page:",
  "      rtf_page: {orientation: portrait, margin_left_in: 0.75}",
  "      rtf_style: {font_size_half_points: 16}",
  "      header:",
  "        first:",
  "          - {left: \"ACME\", right: \"Page {PAGE} of {TOTAL_PAGES}\"}",
  "        last:",
  "          - {center: \"{OUTPUT_LABEL}\"}",
  "      footer:",
  "        last:",
  "          - {left: \"{PROGRAM}\", right: \"{DATETIME}\"}",
  "          - {left: \"Confidential\"}",
  "    content:",
  "      - plan_paginate_rows: {max_rows: 20}")
pc_code <- function(o, s) paste(tfl_report_code(tfl_resolve_output(o, s), content = "plan", study = s),
                                collapse = "\n")
pc_report <- function(d) tfl_read_outputs(d)[["T-14-1-1"]]

test_that("a defaults.yml with page: stops, naming the page configuration", {
  d <- pc_dir(c(pc_defaults, "page:", "  header:", "    1: {left: X}"))
  expect_error(tfl_read_study_spec(d), "`page:` moved to a page configuration \\(styles:\\)")
})

test_that("extends: {page: ...} stops: the page comes from the page configuration", {
  expect_error(tfl_output("T-1", "table", extends = list(page = "defaults")),
               "the page comes from the page configuration \\(styles:\\)")
  d <- pc_dir()
  f <- file.path(d, "outputs", "T-14-1-1.yml")
  writeLines(sub("extends: [defaults]", "extends: {page: none}", readLines(f), fixed = TRUE), f)
  expect_error(tfl_read_outputs(d), "the page comes from the page configuration")
})

test_that("a study set named as a built-in one stops", {
  for (nm in c("tfl_standard", "tfl_small")) {
    d <- pc_dir(c(pc_defaults, "styles:", sprintf("  %s:", nm), "    page: {rtf_page: {orientation: portrait}}"))
    expect_error(tfl_read_study_spec(d), sprintf("%s is a built-in: copy it and save it under your own name", nm))
  }
})

test_that("a configuration's keys are a closed list", {
  env <- environment()
  bad <- function(lines) pc_dir(c(pc_defaults, "styles:", "  acme:", lines), env = env)
  expect_error(tfl_read_study_spec(bad(c("    page: {rtf_bogus: {a: 1}}"))), "no key 'rtf_bogus'")
  expect_error(tfl_read_study_spec(bad(c("    data: {}"))), "unknown key 'data'")
  expect_error(tfl_read_study_spec(bad(c("    page: {tokens: {A: b}}"))), "tokens moved to study.yml")
  expect_error(tfl_read_study_spec(bad(c("    page: {header: {first: [{left: a}], 1: {left: b}}}"))),
               "mixes numbered lines and the blocks")
  # titles are a band of a configuration too
  s <- tfl_read_study_spec(bad(c("    page: {titles: {first: [{center: T}]}}")))
  expect_identical(names(s$styles$acme$page$titles), "first")
})

test_that("a band prints the configuration's first lines, the body, then the last lines", {
  d <- pc_dir(c(pc_defaults, pc_acme))
  s <- tfl_read_study_spec(d)
  o <- pc_report(d)
  o$style <- "acme"
  o$page$header <- list(`1` = list(left = "BODY"))
  ro <- tfl_resolve_output(o, s)
  hl <- vapply(ro$page$header, function(l) paste(unlist(l), collapse = "|"), "")
  expect_identical(unname(hl), c("ACME|Page {PAGE} of {TOTAL_PAGES}", "BODY", "{OUTPUT_LABEL}"))
  # the configuration's settings; the report's own change one
  expect_identical(ro$page$rtf_page$margin_left_in, 0.75)
  o$page$rtf_page <- list(orientation = "landscape")
  ro2 <- tfl_resolve_output(o, s)
  expect_identical(ro2$page$rtf_page$orientation, "landscape")
  expect_identical(ro2$page$rtf_page$margin_left_in, 0.75)
  # the configuration's content half is a layer of the content step
  fns <- vapply(ro$content, `[[`, "", "fn")
  expect_identical(ro$content[[match("plan_paginate_rows", fns)]]$args$max_rows, 30L)
  o$content <- Filter(function(it) it$fn != "plan_paginate_rows", o$content)
  fns <- vapply(tfl_resolve_output(o, s)$content, `[[`, "", "fn")
  expect_identical(tfl_resolve_output(o, s)$content[[match("plan_paginate_rows", fns)]]$args$max_rows, 20L)
})

test_that("footer: {last: {1: !unset}} drops the configuration's program line, the others stay", {
  d <- pc_dir(c(pc_defaults, pc_acme))
  s <- tfl_read_study_spec(d)
  o <- pc_report(d)
  o$style <- "acme"
  expect_match(pc_code(o, s), "{PROGRAM}", fixed = TRUE)
  o$page$footer <- list(last = list(`1` = tfl_unset()))
  code <- pc_code(o, s)
  expect_false(grepl("{PROGRAM}", code, fixed = TRUE))
  expect_match(code, "Confidential", fixed = TRUE)
  # a whole band off
  o$page$footer <- tfl_unset()
  expect_false(grepl("Confidential", pc_code(o, s), fixed = TRUE))
})

test_that("the default configuration: study.yml settings.style, else tfl_standard; study tokens from study.yml", {
  d <- pc_dir(c(pc_defaults, pc_acme), "settings: {style: acme}\ntokens: {COMPANY: Acme Pharma}")
  s <- tfl_read_study_spec(d)
  o <- pc_report(d)
  expect_identical(tfl_resolve_output(o, s)$page$rtf_page$orientation, "portrait")
  expect_identical(tfl_resolve_output(o, s)$page$tokens$COMPANY, "Acme Pharma")
  o$style <- "tfl_standard"
  ro <- tfl_resolve_output(o, s)
  expect_null(ro$page$rtf_page$orientation)
  expect_identical(ro$page$tokens$COMPANY, "Acme Pharma")
  # a report's own token wins
  o$page$tokens <- list(COMPANY = "Own")
  expect_identical(tfl_resolve_output(o, s)$page$tokens$COMPANY, "Own")
  # no settings.style: tfl_standard
  s2 <- tfl_read_study_spec(pc_dir())
  expect_identical(tfl_styles(s2)$name[tfl_styles(s2)$default], "tfl_standard")
  expect_error(tfl_resolve_output({o2 <- pc_report(d); o2$style <- "nope"; o2}, s), "no style 'nope'")
})

test_that("the setup file writes each configuration's header and footer as header_<name> / footer_<name>", {
  d <- pc_dir(c(pc_defaults, pc_acme), "settings: {style: acme}\ntokens: {COMPANY: Acme Pharma}")
  s <- tfl_read_study_spec(d)
  code <- tfl_report_setup_code(s)
  # (the study's own sets; a built-in one when it is the default)
  for (x in c("header_acme", "footer_acme")) expect_true(any(startsWith(code, paste(x, "<-"))), info = x)
  expect_false(any(grepl("tfl_standard|tfl_small", code)))
  # the study's tokens and the default configuration's font size
  expect_match(paste(code, collapse = "\n"), "rtfreporter.tokens = list(COMPANY = \"Acme Pharma\")", fixed = TRUE)
  expect_match(paste(code, collapse = "\n"), "rtfreporter.font_size_half_points = 16", fixed = TRUE)
  # acme's header: its first line, then its last line
  i <- grep("^header_acme <-", code)
  hdr <- paste(code[i:(grep("^footer_acme <-", code) - 1L)], collapse = "\n")
  expect_lt(regexpr("ACME", hdr, fixed = TRUE), regexpr("OUTPUT_LABEL", hdr, fixed = TRUE))
  # a report of the setup file names its configuration's band, and leaves the
  # study's font size and tokens to the options
  o <- pc_report(d)
  rc <- paste(tfl_report_code(o, setup = TRUE, study = s), collapse = "\n")
  expect_match(rc, "header = header_acme", fixed = TRUE)
  expect_match(rc, "footer = footer_acme", fixed = TRUE)
  expect_false(grepl("COMPANY", rc, fixed = TRUE))
  o$style <- "tfl_small"
  rc <- paste(tfl_report_code(o, setup = TRUE, study = s), collapse = "\n")
  # (a built-in set that is not the default has no object: its band is written out)
  expect_false(grepl("header_tfl_small", rc, fixed = TRUE))
  expect_match(rc, "Protocol: {STUDY_ID}", fixed = TRUE)
  expect_match(rc, "font_size_half_points = 14", fixed = TRUE)
  # a report with a body line writes its band out
  o <- pc_report(d)
  o$page$header <- list(`1` = list(left = "BODY"))
  rc <- paste(tfl_report_code(o, setup = TRUE, study = s), collapse = "\n")
  expect_false(grepl("header = header_acme", rc, fixed = TRUE))
  expect_match(rc, "BODY", fixed = TRUE)
})
