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

setup_spec <- function(tokens = NULL, header = NULL) own_spec(
  tokens = rbind(data.frame(output_id = NA, name = c("COMPANY", "STUDY_ID"),
                            value = c("Sample \"Pharma\"", "ABC-123")), tokens),
  header = header %||% data.frame(
    output_id = NA, line = c("1", "2", "4", "5"),
    left = c("{COMPANY}", "PROTOCOL: {STUDY_ID}", NA, NA),
    center = c(NA, NA, "{OUTPUT_LABEL}", "{OUTPUT_TITLE}"),
    right = c(NA, "Page {PAGE} of {TOTAL_PAGES}", NA, NA)))

test_that("what the study's reports share is written once, in a setup", {
  tk <- data.frame(output_id = "T-14-1-1", name = "OUTPUT_TITLE", value = "Demographics")
  sp <- setup_spec(tk)
  set <- paste(tfl_report_setup_code(sp), collapse = "\n")
  expect_match(set, "options(", fixed = TRUE)
  expect_match(set, "COMPANY = \"Sample \\\"Pharma\\\"\"", fixed = TRUE)
  expect_match(set, "STUDY_ID = \"ABC-123\"", fixed = TRUE)
  expect_false(grepl("OUTPUT_TITLE =", set, fixed = TRUE))
  expect_match(set, "study_header <- rtf_header(", fixed = TRUE)
  expect_match(set, "{OUTPUT_LABEL}", fixed = TRUE)
  # the report then says the header by name, and only its own tokens
  opt <- paste(tfl_report_code(sp, "T-14-1-1", content = "pages", setup = TRUE),
               collapse = "\n")
  expect_false(grepl("COMPANY", opt, fixed = TRUE))
  expect_false(grepl("rtf_header", opt, fixed = TRUE))
  expect_match(opt, "header = study_header", fixed = TRUE)
  expect_match(opt, "OUTPUT_TITLE = \"Demographics\"", fixed = TRUE)
  # standing alone (the default): as before
  doc <- paste(tfl_report_code(sp, "T-14-1-1", content = "pages"), collapse = "\n")
  expect_match(doc, "COMPANY = ", fixed = TRUE)
  expect_match(doc, "rtf_header(", fixed = TRUE)
  # nothing shared: no setup
  expect_identical(tfl_report_setup_code(tfl_table_spec()), character())
})

test_that("only a report with its own header writes it", {
  # no title for L-16-2-7: the study's header still (its line left out
  # when the file is written)
  sp <- setup_spec()
  l <- paste(tfl_report_code(sp, "L-16-2-7", content = "pages", setup = TRUE),
             collapse = "\n")
  expect_match(l, "header = study_header", fixed = TRUE)
  expect_match(l, "OUTPUT_TITLE = \"\"", fixed = TRUE)
  hd <- rbind(setup_spec()$header,
              data.frame(output_id = "T-14-1-1", line = "1", left = "Own",
                         center = NA, right = NA))
  t <- paste(tfl_report_code(setup_spec(header = hd), "T-14-1-1",
                             content = "pages", setup = TRUE), collapse = "\n")
  expect_match(t, "rtf_header(", fixed = TRUE)
  expect_match(t, "\"Own\"", fixed = TRUE)
})

test_that("the setup and a report's program make the same file as it alone", {
  skip_if_not_installed("rtfreporter", "0.8.2.9025")
  tk <- data.frame(output_id = "T-14-1-1", name = "OUTPUT_TITLE", value = "Demographics")
  sp <- setup_spec(tk)
  old <- getOption("rtfreporter.tokens")
  on.exit(options(rtfreporter.tokens = old), add = TRUE)
  run <- function(code) {
    env <- new.env(parent = asNamespace("rtfreporter"))
    env$pages <- rtfreporter::as_rtftables(data.frame(A = "a"))
    eval(parse(text = code), env)
    f <- tempfile(fileext = ".rtf")
    rtfreporter::generate_rtfreport(env$doc, f, overwrite = TRUE)
    out <- readLines(f, warn = FALSE)
    unlink(f)
    out[!grepl("creatim|revtim", out)]
  }
  # T-14-1-1 has a title; L-16-2-7 none (its title line left out)
  for (id in c("T-14-1-1", "L-16-2-7")) {
    options(rtfreporter.tokens = old)
    alone <- run(tfl_report_code(sp, id, content = "pages"))
    shared <- run(c(tfl_report_setup_code(sp),
                    tfl_report_code(sp, id, content = "pages", setup = TRUE)))
    expect_identical(shared, alone)
    expect_true(any(grepl("ABC-123", shared, fixed = TRUE)))
  }
})

test_that("tfl_report_tokens() gives a report's tokens as its program does", {
  tk <- data.frame(output_id = "T-14-1-1", name = "OUTPUT_TITLE", value = "Demographics")
  v <- tfl_report_tokens(setup_spec(tk), "T-14-1-1")
  expect_identical(v[["COMPANY"]], "Sample \"Pharma\"")
  expect_identical(v[["OUTPUT_LABEL"]], "Table 14.1.1")
  expect_identical(v[["OUTPUT_TITLE"]], "Demographics")
  expect_length(tfl_report_tokens(tfl_table_spec()), 0L)
})

test_that("a spec of defaults only is the report's it is asked for", {
  sp <- tfl_table_spec(
    report = data.frame(output_id = NA, type = "table", file = "{output_id}.rtf"),
    header = data.frame(output_id = NA, line = "1", center = "{OUTPUT_LABEL}"))
  expect_identical(tfl_report_tokens(sp, "T-14-1-1")[["OUTPUT_LABEL"]], "Table 14.1.1")
  expect_identical(tfl_report_path(sp, "T-14-1-1"), "T-14-1-1.rtf")
})

font_spec <- function() tfl_table_spec(
  study = c(output_path = "out"),
  report = data.frame(output_id = c(NA, "T-14-1-1", "L-16-2-7"),
                      type = c("table", NA, "listing")),
  page = data.frame(output_id = c(NA, "T-14-1-1", "L-16-2-7"),
                    font = c("Courier New", NA, NA),
                    font_size_half_points = c("20", NA, "16")))

test_that("the company's font and size are the setup's options(), once", {
  sp <- font_spec()
  set <- tfl_report_setup_code(sp)
  expect_identical(set[1:4], c("options(", '  rtfreporter.font = "Courier New",',
                               "  rtfreporter.font_size_half_points = 20L", ")"))
  # a report says none of the study's; its own size it says
  t <- paste(tfl_report_code(sp, "T-14-1-1", content = "pages", setup = TRUE),
             collapse = "\n")
  expect_false(grepl("font", t, fixed = TRUE))
  l <- paste(tfl_report_code(sp, "L-16-2-7", content = "pages", setup = TRUE),
             collapse = "\n")
  expect_false(grepl("font_table", l, fixed = TRUE))
  expect_match(l, "font_size_half_points = 16L", fixed = TRUE)
  # standing alone: in its program
  a <- paste(tfl_report_code(sp, "T-14-1-1", content = "pages"), collapse = "\n")
  expect_match(a, 'font_table = list(list(name = "Courier New"))', fixed = TRUE)
  expect_match(a, "font_size_half_points = 20L", fixed = TRUE)
  # none said: no line
  expect_false(any(grepl("rtfreporter.font", tfl_report_setup_code(own_spec()),
                         fixed = TRUE)))
})

test_that("with the company's font, the setup and a program make the file it alone does", {
  skip_if_not_installed("rtfreporter", "0.8.2.9025")
  sp <- font_spec()
  old <- options(rtfreporter.font = NULL, rtfreporter.font_size_half_points = NULL)
  on.exit(options(old), add = TRUE)
  run <- function(code) {
    env <- new.env(parent = asNamespace("rtfreporter"))
    env$pages <- rtfreporter::as_rtftables(data.frame(A = "a"))
    eval(parse(text = code), env)
    f <- tempfile(fileext = ".rtf")
    rtfreporter::generate_rtfreport(env$doc, f, overwrite = TRUE)
    out <- readLines(f, warn = FALSE)
    unlink(f)
    out[!grepl("creatim|revtim", out)]
  }
  for (id in c("T-14-1-1", "L-16-2-7")) {
    options(old)
    alone <- run(tfl_report_code(sp, id, content = "pages"))
    shared <- run(c(tfl_report_setup_code(sp),
                    tfl_report_code(sp, id, content = "pages", setup = TRUE)))
    expect_identical(shared, alone)
    expect_true(any(grepl("Courier New", shared, fixed = TRUE)))
  }
})
