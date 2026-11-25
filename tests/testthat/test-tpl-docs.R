# Each table and listing template's Rd page, tfl_tpl_<name> (R/tpl_docs.R,
# made from the registry by roxygen @eval): that none is missing and that
# the pages are as the registry is now (devtools::document() run)

tpl_rd_db <- function() {
  db <- tryCatch(tools::Rd_db("tflspec"), error = function(e) list())
  if (!length(db)) {
    # (a source tree, devtools::load_all(): its man/ files)
    man <- system.file("man", package = "tflspec")
    f <- if (nzchar(man)) list.files(man, "[.]Rd$", full.names = TRUE) else character()
    db <- stats::setNames(lapply(f, tools::parse_Rd), basename(f))
  }
  db
}
tpl_rd_tag <- function(x) attr(x, "Rd_tag") %||% ""
tpl_rd_aliases <- function(rd) {
  tags <- vapply(rd, tpl_rd_tag, "")
  unlist(lapply(rd[tags == "\\alias"], function(x) paste(unlist(x), collapse = "")))
}
# the rows of the Parameters section's table (the header's not counted)
tpl_rd_param_rows <- function(rd) {
  tags <- vapply(rd, tpl_rd_tag, "")
  for (s in rd[tags == "\\section"]) {
    if (!identical(trimws(paste(unlist(s[[1L]]), collapse = "")), "Parameters")) next
    n <- 0L
    walk <- function(x) {
      if (identical(tpl_rd_tag(x), "\\cr")) n <<- n + 1L
      if (is.list(x)) for (y in x) walk(y)
    }
    walk(s[[2L]])
    return(n - 1L)
  }
  NA_integer_
}

test_that("every table and listing template has its Rd page, a row a parameter", {
  db <- tpl_rd_db()
  skip_if(!length(db), "no Rd pages")
  by_alias <- list()
  for (rd in db) for (a in tpl_rd_aliases(rd)) by_alias[[a]] <- rd
  tt <- tfl_templates()
  tt <- tt[tt$kind %in% c("table", "listing") & tt$package == "tflspec", , drop = FALSE]
  topic <- paste0("tfl_tpl_", tt$name)
  missing <- setdiff(topic, names(by_alias))
  # (a new template: one `#' @eval .tpl_rd("<name>")` block in R/tpl_docs.R)
  expect_identical(missing, character())
  for (nm in tt$name) {
    rd <- by_alias[[paste0("tfl_tpl_", nm)]]
    if (is.null(rd)) next
    expect_identical(tpl_rd_param_rows(rd), nrow(tfl_tpl_args(nm)), label = nm)
  }
})

test_that(".tpl_rd(): the roxygen tags of a page", {
  r <- .tpl_rd("demog")
  expect_identical(r[2L], "@name tfl_tpl_demog")
  expect_match(r[1L], "^@title Demographics: the template demog$")
  # (the example call names `by` and the variables: they are required)
  expect_true(any(grepl("tfl_tpl(ard, \"demog\", ", r, fixed = TRUE) & grepl("by = \"TRT01A\", continuous = ", r, fixed = TRUE)))
  tbl <- r[startsWith(r, "@section Parameters:")]
  # (the tag's line, the table's head and its rule, a row a parameter)
  expect_length(strsplit(tbl, "\n", fixed = TRUE)[[1L]], 3L + nrow(tfl_tpl_args("demog")))
  # a listing's call is on its data, named after its dataset
  expect_true(any(grepl("tfl_tpl(adae, \"listing\"", .tpl_rd("listing"), fixed = TRUE)))
})

# A page's text as Rd2txt() prints it, its quotes and spaces dropped (a code
# span prints quoted, a table cell padded) and its dashes as one (Rd prints
# -- as an en dash, or - in ASCII), to find a definition's text in
tpl_rd_plain <- function(x) {
  x <- gsub("[`'\u2018\u2019]", "", paste(x, collapse = " "))
  x <- gsub("[\u2013\u2014]|-+", "-", x)
  gsub("\\s+", " ", x)
}

test_that("every template page parses clean and prints each parameter's text as written", {
  db <- tpl_rd_db()
  skip_if(!length(db), "no Rd pages")
  withr::local_options(useFancyQuotes = FALSE)
  by_alias <- list()
  for (rd in db) for (a in tpl_rd_aliases(rd)) by_alias[[a]] <- rd
  tt <- tfl_templates()
  tt <- tt[tt$kind %in% c("table", "listing") & tt$package == "tflspec", , drop = FALSE]
  seen <- character()
  for (nm in tt$name) {
    rd <- by_alias[[paste0("tfl_tpl_", nm)]]
    if (is.null(rd)) next
    # the Rd parses again without a warning, and checkRd() finds nothing
    # (a lost brace is R CMD check's NOTE)
    src <- tempfile(fileext = ".Rd")
    writeLines(paste(as.character(rd, deparse = TRUE), collapse = ""), src)
    expect_no_warning(tools::parse_Rd(src))
    expect_identical(as.character(tools::checkRd(rd)), character(), label = nm)
    out <- tempfile(fileext = ".txt")
    tools::Rd2txt(rd, out = out, options = list(underline_titles = FALSE, width = 10000L))
    txt <- tpl_rd_plain(readLines(out, encoding = "UTF-8"))
    a <- tfl_tpl_args(nm)
    for (i in seq_len(nrow(a))) {
      doc <- a$doc[i]
      if (is.na(doc) || !nzchar(doc)) next
      expect_true(grepl(tpl_rd_plain(doc), txt, fixed = TRUE),
                  label = sprintf("%s: %s's text on its page", nm, a$param[i]))
      seen <- c(seen, strsplit(doc, "")[[1L]])
    }
  }
  # (the definitions hold these specials today: each was checked above)
  expect_true(all(c("{", "}", "<", ">", "%", "`", "_") %in% seen))
})

test_that(".tpl_rd_cell(): markdown and Rd specials come through roxygen's markdown", {
  expect_identical(.tpl_rd_cell(c(NA, "")), c("", ""))
  # Rd's and the table's specials escaped; & < * _ ~ and a lone backtick as entities
  expect_identical(.tpl_rd_cell("a {x} [y] | \\ % > < & *m* _n_ ~ ` b"),
                   "a \\{x\\} \\[y\\] \\| \\\\ % > &lt; &amp; &#42;m&#42; &#95;n&#95; &#126; &#96; b")
  # a code span stays one; one with a | is quoted text
  expect_identical(.tpl_rd_cell("`{value}` and `a | b`"), "`{value}` and 'a \\| b'")
  expect_identical(.tpl_rd_cell("plan_cells <data>_sub"), "plan_cells &lt;data>&#95;sub")
  expect_identical(.tpl_rd_cell("a\nb"), "a b")
})
