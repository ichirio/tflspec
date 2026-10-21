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
  expect_match(r[1L], "^@title Demographics [(]FDA STF Table 2[)]: the template demog$")
  expect_true(any(grepl("tfl_tpl(ard, \"demog\", population = \"SAF\")", r, fixed = TRUE)))
  tbl <- r[startsWith(r, "@section Parameters:")]
  # (the tag's line, the table's head and its rule, a row a parameter)
  expect_length(strsplit(tbl, "\n", fixed = TRUE)[[1L]], 3L + nrow(tfl_tpl_args("demog")))
  # a listing's call is on its data
  expect_true(any(grepl("tfl_tpl(data, \"listing\"", .tpl_rd("listing"), fixed = TRUE)))
})
