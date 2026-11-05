# A listing that gives no page size pages at the `listing` template's 22 rows
# (#638): tfl_resolve_output() puts plan_paginate_rows(max_rows = 22) last
# when the resolved items have plan_listing and no pagination item.

# A study (ADAE and ADSL as xpt) with one listing report, `content` its items
# (YAML text lines); `defaults` / `pattern` the study's layers' content lines.
.lp_study <- function(content, defaults = NULL, pattern = NULL, extra = NULL,
                      extends = "none") {
  d <- tempfile("lp")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "XYZ"))
  sp <- file.path(d, "spec")
  if (length(defaults)) {
    f <- file.path(sp, "defaults.yml")
    y <- readLines(f, warn = FALSE)
    at <- grep("^content:", y)
    # (the study's defaults may have a content: already)
    y <- if (length(at)) append(y, defaults, after = at[1L]) else c(y, "content:", defaults)
    writeLines(y, f)
  }
  ext <- paste0("extends: ", extends)
  if (length(pattern)) {
    dir.create(file.path(sp, "patterns"), showWarnings = FALSE)
    writeLines(c("content:", pattern), file.path(sp, "patterns", "paged.yml"))
    ext <- "extends: [patterns/paged]"
  }
  writeLines(c("id: L-1", "kind: listing", "label: AEs", "population: SAF", ext,
               "data: {source: dataset, dataset: ADAE, population: SAF, where: !r 'AESEV == \"MILD\"'}",
               extra, "content:", content,
               "page:", "  titles: {'1': {center: Listing 1}}"),
             file.path(sp, "outputs", "L-1.yml"))
  d
}
.lp_cols <- c("  - plan_listing:",
              "      cols:",
              "        - {fn: listing_col, args: {vars: [USUBJID], label: Subject, width: 20}}",
              "        - {fn: listing_col, args: {vars: [AEDECOD], label: Preferred term, width: 30}}")
.lp_resolved <- function(d, provenance = FALSE) {
  st <- tfl_read_study_spec(d)
  tfl_resolve_output(tfl_read_outputs(d)[["L-1"]], st, provenance = provenance)
}
.lp_rows <- function(d) {
  vapply(.lp_resolved(d)$content, function(it)
    if (identical(it$fn, "plan_paginate_rows")) as.character(it$args$max_rows) else NA_character_, "")
}
.lp_fns <- function(ro) vapply(ro$content, `[[`, "", "fn")
# the built report's pages and the page breaks of its RTF
.lp_build <- function(d) {
  st <- tfl_read_study_spec(d)
  content <- withr::with_dir(d, tflspec:::.output_build_content(tfl_read_outputs(d)[["L-1"]], st))
  f <- withr::with_dir(d, tfl_run_output("L-1", d))
  x <- paste(readLines(f, warn = FALSE), collapse = "\n")
  list(pages = length(rtfreporter::plan_apply(content)),
       breaks = lengths(regmatches(x, gregexpr("\\page", x, fixed = TRUE))))
}

skip_lp <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("withr")
}

test_that("a listing with no page size pages at 22 rows, a page break between pages", {
  skip_lp()
  d <- .lp_study(.lp_cols)
  on.exit(unlink(d, recursive = TRUE))
  ro <- .lp_resolved(d)
  expect_identical(.lp_fns(ro), c("plan_listing", "plan_paginate_rows"))
  expect_identical(.lp_rows(d)[2L], "22")
  # (the records counted from the package's ADAE.  The listing's layout has a
  # blank row after each record, so 22 rows hold 10 or 11 records, not 22:
  # the pages are between ceiling(n / 11) and ceiling(n / 10))
  n <- sum(tflspec::adae$AESEV == "MILD")
  expect_identical(n, 455L)
  b <- .lp_build(d)
  expect_gte(b$pages, ceiling(n / 11))
  expect_lte(b$pages, ceiling(n / 10))
  # (the RTF: a break between pages)
  expect_identical(b$breaks, b$pages - 1L)
  # (the same as the page size written in the report)
  d2 <- .lp_study(c(.lp_cols, "  - plan_paginate_rows: {max_rows: 22}"))
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  expect_identical(.lp_build(d2)$pages, b$pages)
})

test_that("a report's own page size wins: one item, its rows", {
  skip_lp()
  d <- .lp_study(c(.lp_cols, "  - plan_paginate_rows: {max_rows: 30}"))
  on.exit(unlink(d, recursive = TRUE))
  expect_identical(.lp_fns(.lp_resolved(d)), c("plan_listing", "plan_paginate_rows"))
  expect_identical(.lp_rows(d)[2L], "30")
  # (a blank row after each record: 15 records a page at most, 14 at least)
  b <- .lp_build(d)
  expect_gte(b$pages, ceiling(455 / 15))
  expect_lte(b$pages, ceiling(455 / 14))
})

test_that("a page size of a pattern wins: one item; the study's defaults do not reach a listing", {
  skip_lp()
  d <- .lp_study(.lp_cols, pattern = "  - plan_paginate_rows: {max_rows: 35}")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_identical(.lp_fns(.lp_resolved(d)), c("plan_listing", "plan_paginate_rows"))
  expect_identical(.lp_rows(d)[2L], "35")
  # (the defaults' content is a table's, a listing takes its page only: a page
  # size there is not the listing's, so the listing's own default is)
  d2 <- .lp_study(.lp_cols, defaults = "  - plan_paginate_rows: {max_rows: 40}",
                  extends = "[defaults]")
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  expect_identical(.lp_fns(.lp_resolved(d2)), c("plan_listing", "plan_paginate_rows"))
  expect_identical(.lp_rows(d2)[2L], "22")
})

test_that("a page a group alone is a page size: nothing is added", {
  skip_lp()
  d <- .lp_study(c(.lp_cols, "  - plan_paginate_group: {col: USUBJID}"))
  on.exit(unlink(d, recursive = TRUE))
  expect_identical(.lp_fns(.lp_resolved(d)), c("plan_listing", "plan_paginate_group"))
})

test_that("the listing template has one page size item, 22 rows", {
  skip_lp()
  d <- .lp_study(.lp_cols)
  on.exit(unlink(d, recursive = TRUE))
  writeLines(c("id: L-1", "kind: listing", "label: AEs", "population: SAF",
               "template:",
               "  name: listing",
               "  args: {data: ADAE, columns: [{vars: [USUBJID], label: Subject}]}",
               "page:", "  titles: {'1': {center: Listing 1}}"),
             file.path(d, "spec", "outputs", "L-1.yml"))
  ro <- .lp_resolved(d)
  expect_identical(sum(.lp_fns(ro) == "plan_paginate_rows"), 1L)
  expect_identical(ro$content[[which(.lp_fns(ro) == "plan_paginate_rows")]]$args$max_rows, 22L)
  # (as a spec: the template's items, one page size, none added a second time)
  sp <- tfl_template_to_spec(tfl_read_outputs(d)[["L-1"]], tfl_read_study_spec(d))
  expect_identical(sum(vapply(sp$content, function(it) identical(it$fn, "plan_paginate_rows"), NA)), 1L)
})

test_that("a table with no plan_listing, and a report's own code, get nothing", {
  skip_lp()
  t <- tfl_output("T-1", "table", content = list(tfl_item("table_plan", list(cols = "ARM"))))
  st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  r <- tfl_resolve_output(t, st)
  expect_false("plan_paginate_rows" %in% vapply(r$content, `[[`, "", "fn"))
  code <- tfl_r("content <- rtfreporter::table_plan(adae) |> rtfreporter::plan_listing()")
  o <- tfl_output("L-9", "listing", extends = "none", content = code)
  expect_identical(tfl_resolve_output(o, st)$content, code)
})

test_that("the program carries the page size, and the file is not given it", {
  skip_lp()
  d <- .lp_study(.lp_cols)
  on.exit(unlink(d, recursive = TRUE))
  ro <- .lp_resolved(d)
  code <- paste(tfl_content_code(ro), collapse = "\n")
  expect_match(code, "plan_paginate_rows(max_rows = 22", fixed = TRUE)
  # (the file the report is written to has its own items alone)
  st <- tfl_read_study_spec(d)
  out <- tfl_read_outputs(d)[["L-1"]]
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  invisible(tfl_resolve_output(out, st))
  tfl_write_output(out, f)
  expect_false(any(grepl("plan_paginate", readLines(f, warn = FALSE))))
  # (with the provenance the planner reads: from the defaults, not the report's own)
  pr <- .lp_resolved(d, provenance = TRUE)
  it <- pr$content[[which(.lp_fns(pr) == "plan_paginate_rows")]]
  expect_identical(it$from, "defaults")
  expect_identical(it$arg_from, c(max_rows = "defaults"))
})
