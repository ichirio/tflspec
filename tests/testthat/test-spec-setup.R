# The study's setup of its report programs and a report's tokens, from the
# YAML spec: what the sheets saying the same write.

su_sheets <- function() {
  tfl_table_spec(
    report = data.frame(output_id = "T-1", type = "table"),
    page = data.frame(output_id = NA, font = "Arial", font_size_half_points = "16"),
    header = data.frame(output_id = NA, line = c("1", "2", "3"),
                        left = c("{COMPANY}", "Protocol: {STUDY_ID}", NA),
                        center = c(NA, NA, "{OUTPUT_LABEL}"),
                        right = c("DRAFT", "Page {PAGE} of {TOTAL_PAGES}", NA)),
    footer = data.frame(output_id = NA, line = "1", left = "{PROGRAM}"),
    titles = data.frame(output_id = "T-1", line = "1", center = "Demographics"),
    tokens = data.frame(output_id = NA, name = c("COMPANY", "STUDY_ID", "GONE"),
                        value = c("Sample Pharma", "ABC-123", "(none)")))
}

# the study: its default page configuration (a set "std"), its tokens study.yml's
su_study <- function(page = su_page(), tokens = su_tokens) {
  structure(list(study = list(settings = list(program_dir = "programs/tfl", style = "std"),
                              tokens = tokens),
                 styles = list(std = list(page = page)), defaults = list()),
            class = "tfl_study_spec")
}
su_tokens <- list(COMPANY = "Sample Pharma", STUDY_ID = "ABC-123", GONE = tfl_unset())

su_page <- function() {
  list(
    rtf_document = list(font_table = list(list(name = "Arial"))),
    rtf_default_format = list(font_size_half_points = 16L),
    header = list(`1` = list(left = "{COMPANY}", right = "DRAFT"),
                  `2` = list(left = "Protocol: {STUDY_ID}", right = "Page {PAGE} of {TOTAL_PAGES}"),
                  `3` = list(center = "{OUTPUT_LABEL}")),
    footer = list(`1` = list(left = "{PROGRAM}")))
}

su_output <- function() {
  tfl_output("T-1", "table", extends = "defaults",
             page = list(titles = list(`1` = list(center = "Demographics"))))
}

test_that("a study's setup code from its configuration is what its sheets write: the options; the bands per configuration", {
  yaml <- tfl_report_setup_code(su_study())
  sheets <- tfl_report_setup_code(su_sheets())
  # (the options and tokens: the same lines; the sheets' header and footer
  # are study_header / study_footer, the YAML route's one pair a configuration)
  n <- grep("^header_std <-", yaml)[1L] - 1L
  expect_identical(yaml[seq_len(n)], sheets[seq_len(n)])
  txt <- paste(yaml, collapse = "\n")
  expect_match(txt, "rtfreporter.font = \"Arial\"", fixed = TRUE)
  expect_match(txt, "rtfreporter.font_size_half_points = 16", fixed = TRUE)
  expect_match(txt, "COMPANY = \"Sample Pharma\"", fixed = TRUE)
  expect_false(grepl("GONE", txt, fixed = TRUE))
  expect_match(txt, "header_std <- rtf_header(", fixed = TRUE)
  expect_match(txt, "drop_empty_rows = TRUE", fixed = TRUE)
  expect_match(txt, "footer_std <- rtf_footer(", fixed = TRUE)
})

test_that("a configuration that says nothing of the page has no setup code", {
  expect_identical(tfl_report_setup_code(su_study(list(), list())), character())
  pg <- su_page()
  pg$header <- tfl_unset()
  pg$footer <- NULL
  pg$rtf_document <- NULL
  code <- tfl_report_setup_code(su_study(pg, list()))
  expect_identical(code, "options(rtfreporter.font_size_half_points = 16)")
})

test_that("a report's program after the setup refers to the setup's names", {
  skip_if_not_installed("rtfreporter")
  st <- su_study()
  setup <- tfl_report_setup_code(st)
  code <- tfl_report_code(su_output(), content = "plan", setup = TRUE, study = st)
  txt <- paste(code, collapse = "\n")
  expect_match(txt, "header = header_std", fixed = TRUE)
  expect_match(txt, "footer = footer_std", fixed = TRUE)
  expect_false(grepl("COMPANY", txt, fixed = TRUE))
  # the setup defines every name the report's code uses
  env <- new.env(parent = asNamespace("rtfreporter"))
  old <- options(rtfreporter.tokens = NULL, rtfreporter.font = NULL,
                 rtfreporter.font_size_half_points = NULL)
  on.exit(options(old), add = TRUE)
  eval(parse(text = setup), env)
  expect_true(all(c("header_std", "footer_std") %in% ls(env)))
  expect_identical(getOption("rtfreporter.tokens")$COMPANY, "Sample Pharma")
})

test_that("a YAML report's tokens are its document's, as its sheets give them", {
  st <- su_study()
  tk <- tfl_report_tokens(su_output(), study = st)
  expect_identical(tk, tfl_report_tokens(su_sheets(), "T-1"))
  expect_identical(tk[["OUTPUT_LABEL"]], "Table 1")
  expect_identical(tk[["COMPANY"]], "Sample Pharma")
  # resolved first, the same
  expect_identical(tfl_report_tokens(tfl_resolve_output(su_output(), st)), tk)
  expect_length(tfl_report_tokens(tfl_output("T-2", "table")), 0L)
})

test_that("a report's program: in the study's program_dir, not taken for it", {
  st <- su_study()
  code <- tfl_report_code(su_output(), content = "plan", study = st)
  expect_true(any(grepl("program_fallback = \"programs/tfl/T-1\"", code, fixed = TRUE)))
  st$study$settings$program <- "{output_id}.R"
  code <- tfl_report_code(su_output(), content = "plan", study = st)
  expect_true(any(grepl("program = \"programs/tfl/T-1.R\"", code, fixed = TRUE)))
})
