# The tokens sheet: tokens of one's own for a report's header, footer,
# titles and footnotes (rtfreporter::rtf_document(tokens = )).

tk_spec <- function(tokens) tfl_table_spec(
  study = c(output_path = "out"),
  report = data.frame(output_id = c(NA, "T1", "T2")),
  footer = data.frame(output_id = NA, line = 1, left = "Study {STUDY} {CUTOFF}"),
  titles = data.frame(output_id = NA, line = 1, center = "Table for {STUDY}"),
  tokens = tokens)

tk_tokens <- data.frame(
  output_id = c(NA, NA, "T1", "T2"),
  name = c("STUDY", "CUTOFF", "CUTOFF", "STUDY"),
  value = c("ABC-123", "01JUN2026", "01JAN2027", "(none)"))

test_that("a report's tokens are the defaults, its own rows replacing them", {
  sp <- tk_spec(tk_tokens)
  t1 <- tflspec:::.ard_spec_tokens(tflspec:::.ard_spec_scope(sp, "T1"))
  expect_identical(t1, list(STUDY = "ABC-123", CUTOFF = "01JAN2027"))
  # (none) takes a default out
  t2 <- tflspec:::.ard_spec_tokens(tflspec:::.ard_spec_scope(sp, "T2"))
  expect_identical(t2, list(CUTOFF = "01JUN2026"))
  # no sheet, no tokens
  expect_null(tflspec:::.ard_spec_tokens(tflspec:::.ard_spec_scope(tk_spec(NULL), "T1")))
})

test_that("tfl_report_code() hands them to rtf_document(tokens = )", {
  sp <- tflspec:::.ard_spec_scope(tk_spec(tk_tokens), "T1")
  code <- paste(tfl_report_code(sp, content = "pages"), collapse = "\n")
  expect_match(code, "tokens = list(STUDY = \"ABC-123\", CUTOFF = \"01JAN2027\")",
               fixed = TRUE)
  # none: no tokens argument
  code0 <- paste(tfl_report_code(tflspec:::.ard_spec_scope(tk_spec(NULL), "T1"),
                                 content = "pages"), collapse = "\n")
  expect_false(grepl("tokens =", code0, fixed = TRUE))
})

test_that("the report fills them when the file is written", {
  skip_if_not_installed("rtfreporter", "0.8.2.9023")
  sp <- tflspec:::.ard_spec_scope(tk_spec(tk_tokens), "T1")
  doc <- tfl_report(sp, content = rtfreporter::as_rtftables(data.frame(A = "a")))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  rtfreporter::generate_rtfreport(doc, f, overwrite = TRUE)
  out <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(out, "Study ABC-123 01JAN2027", fixed = TRUE)
  expect_match(out, "Table for ABC-123", fixed = TRUE)
})

test_that("a token's name follows rtfreporter's rule", {
  bad <- function(nm) tk_spec(data.frame(output_id = NA, name = nm, value = "x"))
  expect_error(bad("study"), "upper case")
  expect_error(bad("1ST"), "upper case")
  expect_error(bad("PAGE"), "rtfreporter's own")
  expect_error(bad("PROGRAM_FULL"), "rtfreporter's own")
  expect_error(bad(NA), "needs a `name`")
  # one row a name in a scope
  expect_error(tk_spec(data.frame(output_id = c("T1", "T1"), name = "STUDY",
                                  value = c("a", "b"))), "two rows")
  # the reserved list is rtfreporter's own
  skip_if_not_installed("rtfreporter", "0.8.2.9023")
  own <- rtfreporter::rtf_text_tokens()$token
  expect_true(all(gsub("[{}]", "", own) %in% tflspec:::.ard_spec_rtf_tokens))
})

test_that("the tokens sheet goes through a workbook and back", {
  skip_if_not_installed("openxlsx")
  sp <- tk_spec(tk_tokens)
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  tfl_write_report_spec(sp, f)
  back <- tfl_read_report_spec(f)
  t <- back$tokens
  expect_identical(t$name, tk_tokens$name)
  expect_identical(t$value, tk_tokens$value)
  expect_identical(is.na(t$output_id), is.na(tk_tokens$output_id))
  # the column help is there
  expect_true(all(c("name", "value") %in% tfl_spec_columns("tokens")$column))
})

# the report's own tokens: OUTPUT_ID, OUTPUT_LABEL, and the TOC's
own_spec <- function(tokens = NULL, header = NULL) tfl_table_spec(
  study = c(output_path = "out"),
  report = data.frame(output_id = c(NA, "T-14-1-1", "L-16-2-7"),
                      type = c("table", NA, "listing")),
  header = header %||% data.frame(
    output_id = NA, line = c("4", "5", "6"),
    center = c("{OUTPUT_LABEL}", "{OUTPUT_TITLE}", "<{OUTPUT_POPULATION}>")),
  tokens = tokens)

test_that("a report's own tokens are given when its bands say them", {
  t1 <- tflspec:::.ard_spec_tokens(tflspec:::.ard_spec_scope(own_spec(), "T-14-1-1"),
                                   list(type = "table"))
  expect_identical(t1, list(OUTPUT_LABEL = "Table 14.1.1", OUTPUT_TITLE = "",
                            OUTPUT_POPULATION = ""))
  l1 <- tflspec:::.ard_spec_tokens(tflspec:::.ard_spec_scope(own_spec(), "L-16-2-7"),
                                   list(type = "listing"))
  expect_identical(l1$OUTPUT_LABEL, "Listing 16.2.7")
  # the tokens sheet wins: its own label, a TOC's title, other words for the kinds
  tk <- data.frame(output_id = c("T-14-1-1", "T-14-1-1", NA),
                   name = c("OUTPUT_TITLE", "OUTPUT_POPULATION", "OUTPUT_KIND_TABLE"),
                   value = c("Demographics", "Safety Analysis Set", "Tab."))
  t2 <- tflspec:::.ard_spec_tokens(tflspec:::.ard_spec_scope(own_spec(tk), "T-14-1-1"),
                                   list(type = "table"))
  expect_identical(t2$OUTPUT_LABEL, "Tab. 14.1.1")
  expect_identical(t2$OUTPUT_TITLE, "Demographics")
  # a report whose bands say none: no own tokens (its program as before)
  none <- own_spec(header = data.frame(output_id = NA, line = "1", left = "Company"))
  expect_null(tflspec:::.ard_spec_tokens(tflspec:::.ard_spec_scope(none, "T-14-1-1")))
})

test_that("a line left with nothing but empty tokens is not printed", {
  sp <- tflspec:::.ard_spec_scope(own_spec(), "T-14-1-1")
  code <- paste(tfl_report_code(sp, content = "pages"), collapse = "\n")
  # the label's line stays; the title's and the analysis set's go
  expect_match(code, "{OUTPUT_LABEL}", fixed = TRUE)
  expect_false(grepl("{OUTPUT_TITLE}", code, fixed = TRUE))
  expect_false(grepl("<{OUTPUT_POPULATION}>", code, fixed = TRUE))
  expect_match(code, "OUTPUT_LABEL = \"Table 14.1.1\"", fixed = TRUE)
  tk <- data.frame(output_id = "T-14-1-1", name = "OUTPUT_POPULATION",
                   value = "Safety Analysis Set")
  code2 <- paste(tfl_report_code(tflspec:::.ard_spec_scope(own_spec(tk), "T-14-1-1"),
                                 content = "pages"), collapse = "\n")
  expect_match(code2, "<{OUTPUT_POPULATION}>", fixed = TRUE)
})

test_that("the report fills its own tokens when the file is written", {
  skip_if_not_installed("rtfreporter", "0.8.2.9023")
  tk <- data.frame(output_id = "T-14-1-1", name = "OUTPUT_TITLE", value = "Demographics")
  sp <- tflspec:::.ard_spec_scope(own_spec(tk), "T-14-1-1")
  doc <- tfl_report(sp, content = rtfreporter::as_rtftables(data.frame(A = "a")))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  rtfreporter::generate_rtfreport(doc, f, overwrite = TRUE)
  out <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(out, "Table 14.1.1", fixed = TRUE)
  expect_match(out, "Demographics", fixed = TRUE)
  expect_false(grepl("OUTPUT_", out, fixed = TRUE))
})
