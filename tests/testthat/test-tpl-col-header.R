# The two ways a column header is made (canon 11-28, CG-5): (1) the whole
# header, `col_header`, the tables sheet's cells (line, cols, span, text),
# in tflspec_summary, tflspec_hierarchy and tflspec_shift; (2) additions to
# the header the template makes: `col_header_values` (plan_col_header(values
# = )) and the short texts inside it (hierarchy: heading, value_header;
# shift: heading, post_header, group_header).  Not both.

ch_hdr <- function(r) {
  it <- Filter(function(i) identical(i$fn, "plan_col_header"), r$content)
  if (length(it)) it[[1L]]$args
}
ch_hier <- function(...) list(id = "H", kind = "table", template = c(list(
  name = "tflspec_v1/tflspec_hierarchy", from = "ADAE", by = "TRT01A",
  hierarchy = c("AEBODSYS", "AEDECOD")), list(...)))
ch_shift <- function(...) list(id = "S", kind = "table", template = c(list(
  name = "tflspec_v1/tflspec_shift", by = "TRT01A"), list(...)))
ch_summary <- function(...) list(id = "M", kind = "table", template = c(list(
  name = "tflspec_v1/tflspec_summary", by = "TRT01A", continuous = "AGE"), list(...)))
ch_cell <- function(line, cols, span = NULL, text = NULL) {
  x <- list(line = line, cols = cols, span = span, text = text)
  x[!vapply(x, is.null, NA)]
}
ch_default <- function(template) {
  d <- tfl_arg_defs(template = paste0("tflspec_v1/", template))
  d$default[[match("col_header", d$name)]]
}

test_that("the three templates have col_header as the header's cells, and col_header_values", {
  for (t in c("tflspec_summary", "tflspec_hierarchy", "tflspec_shift")) {
    d <- tfl_arg_defs(template = paste0("tflspec_v1/", t))
    i <- match("col_header", d$name)
    expect_identical(d$type[[i]], "rows<line, cols, span, text>", info = t)
    expect_identical(names(d$columns[[i]]), c("line", "cols", "span", "text"), info = t)
    expect_true("col_header_values" %in% d$name, info = t)
  }
  # summary: the study's header by default, as before
  expect_null(ch_default("tflspec_summary"))
})

test_that("tflspec_hierarchy: the default header is the one written out in col_header's default", {
  made <- ch_hdr(tfl_template_resolve(ch_hier()))
  expect_identical(names(made), "header")
  expect_identical(made$header, list(
    ch_cell("1", "row_label"),
    ch_cell("1", ".values", "each", "{col}\n(N={n})\nn (%)"),
    ch_cell("2", "row_label"),
    ch_cell("2", ".values")))
  expect_identical(ch_default("tflspec_hierarchy"), made$header)
})

test_that("tflspec_hierarchy: value_header and heading each change only their cell", {
  base <- ch_hdr(tfl_template_resolve(ch_hier()))$header
  vh <- ch_hdr(tfl_template_resolve(ch_hier(value_header = "{col}\n(N={n})")))$header
  expect_identical(vh[-2L], base[-2L])
  expect_identical(vh[[2L]]$text, "{col}\n(N={n})")
  hd <- ch_hdr(tfl_template_resolve(ch_hier(heading = c("SOC", "  PT"))))$header
  expect_identical(hd[-3L], base[-3L])
  expect_identical(hd[[3L]]$text, "SOC\n  PT")
})

test_that("tflspec_shift: the default header is col_header's default; group_header changes its cell", {
  made <- ch_hdr(tfl_template_resolve(ch_shift()))$header
  expect_identical(made, list(
    ch_cell("1", "row_label"),
    ch_cell("1", ".values", "TRT01A", "{col1} (N={n})"),
    ch_cell("2", "row_label", text = "Baseline"),
    ch_cell("2", ".values", "each", "{col}")))
  # (the default written out names the group key {by}: filled when given)
  given <- ch_hdr(tfl_template_resolve(ch_shift(col_header = ch_default("tflspec_shift"))))
  expect_identical(given$header, made)
  gh <- ch_hdr(tfl_template_resolve(ch_shift(group_header = "{col1}")))$header
  expect_identical(gh[-2L], made[-2L])
  expect_identical(gh[[2L]]$text, "{col1}")
  ph <- ch_hdr(tfl_template_resolve(ch_shift(post_header = "Post: {col}")))$header
  expect_identical(ph[-4L], made[-4L])
  expect_identical(ph[[4L]]$text, "Post: {col}")
  hd <- ch_hdr(tfl_template_resolve(ch_shift(heading = "At baseline")))$header
  expect_identical(hd[-3L], made[-3L])
  expect_identical(hd[[3L]]$text, "At baseline")
})

test_that("col_header replaces the made header whole, in each template", {
  cells <- list(ch_cell("1", "row_label", text = "Term"), ch_cell("1", ".values", text = "{col}"))
  expect_identical(ch_hdr(tfl_template_resolve(ch_hier(col_header = cells))), list(header = cells))
  expect_identical(ch_hdr(tfl_template_resolve(ch_shift(col_header = cells))), list(header = cells))
  expect_identical(ch_hdr(tfl_template_resolve(ch_summary(col_header = cells))), list(header = cells))
  # summary with none: no plan_col_header of the template (the study's)
  expect_null(ch_hdr(tfl_template_resolve(ch_summary())))
})

test_that("col_header_values is plan_col_header(values = ) only", {
  v <- list(N = "table")
  for (r in list(ch_hier(), ch_shift())) {
    a <- ch_hdr(tfl_template_resolve(r))
    r$template$col_header_values <- v
    b <- ch_hdr(tfl_template_resolve(r))
    expect_identical(b$values, v)
    expect_identical(b[names(b) != "values"], a)
  }
  expect_identical(ch_hdr(tfl_template_resolve(ch_summary(col_header_values = v))), list(values = v))
})

test_that("col_header with a text param of the made header stops, naming both", {
  cells <- list(ch_cell("1", ".values", text = "{col}"))
  expect_error(tfl_template_resolve(ch_hier(col_header = cells, heading = "SOC")),
               "col_header .*heading.*Give col_header or heading, not both")
  expect_error(tfl_template_resolve(ch_hier(col_header = cells, value_header = "{col}")),
               "Give col_header or value_header, not both")
  for (p in c("heading", "post_header", "group_header")) {
    r <- ch_shift(col_header = cells)
    r$template[[p]] <- "x {col}"
    expect_error(tfl_template_resolve(r), paste0("Give col_header or ", p, ", not both"), info = p)
  }
  # col_header_values goes with either way
  expect_no_error(tfl_template_resolve(ch_hier(col_header = cells, col_header_values = list(N = "table"))))
})

test_that("summary's old row form of col_header stops with the cell form", {
  old <- list(list(row_label = "", .values = "{col}"), list(row_label = "", .values = "(N={n})"))
  expect_error(tfl_template_resolve(ch_summary(col_header = old)),
               "col_header is the header's cells.*row_label, .values.*old row form.*cols: row_label")
  expect_error(tfl_template_resolve(ch_hier(col_header = list(list(text = "x")))),
               "a line and cols in each")
})

# bug (a): a template's header and a report card's lines left in one call
test_that("a later plan_col_header(lines) put together with a header keeps lines only", {
  items <- list(tfl_item("plan_col_header", list(header = list(ch_cell("1", ".values", text = "{col}")))),
                tfl_item("plan_col_header", list(lines = list(c(row_label = "", .values = "{col}")))))
  m <- suppressMessages(tflspec:::.plan_merge_repeats(items, "T"))
  expect_length(m, 1L)
  expect_identical(names(m[[1L]]$args), "lines")
  # and the other way round; an argument of neither stays
  items <- list(tfl_item("plan_col_header", list(lines = list(c(.values = "{col}")), values = list(n = TRUE))),
                tfl_item("plan_col_header", list(header = list(ch_cell("1", ".values")))))
  m <- suppressMessages(tflspec:::.plan_merge_repeats(items, "T"))
  expect_setequal(names(m[[1L]]$args), c("values", "header"))
  # a study's items by verb (tfl_tpl(study = ))
  s <- tflspec:::.tpl_study_items(list(plan_col_header = list(header = list(ch_cell("1", ".values"))),
                                       plan_col_header = list(lines = list(c(.values = "{col}")))))
  expect_identical(names(s[[1L]]$args), "lines")
})

# bug (b): values with other tokens than n stopped BIGN's n
test_that("BIGN's n goes into values that give other tokens only", {
  hdr <- tfl_item("plan_col_header", list(header = list(ch_cell("1", ".values", text = "{col} (N={n})")),
                                          values = list(N = "table")))
  out <- tflspec:::.bign_header_items(list(hdr), list(n = c(A = 3, B = 4)))
  expect_true(attr(out, "used"))
  expect_identical(out[[1L]]$args$values, list(N = "table", n = c(A = 3, B = 4)))
  # the program's: bign$n beside them
  out <- tflspec:::.bign_header_items(list(hdr), tfl_r("bign"))
  expect_identical(out[[1L]]$args$values$n, tfl_r("bign$n"))
  expect_identical(out[[1L]]$args$values$N, "table")
  # an n of the report's own stays its own
  hdr$args$values <- list(n = "page")
  out <- tflspec:::.bign_header_items(list(hdr), list(n = c(A = 3)))
  expect_false(attr(out, "used"))
  expect_identical(out[[1L]]$args$values, list(n = "page"))
})
