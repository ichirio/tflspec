# What adopting would do (tfl_ai_adopt(dry_run = TRUE)), and what the AI
# changed in a report since it was adopted (tfl_ai_adopted_diff())

adiff_study <- function() {
  d <- tempfile("adiff-")
  dir.create(file.path(d, "ai", "spec", "outputs"), recursive = TRUE)
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  mk <- function(id, label, where) {
    o <- tfl_output(id, "table", label = label, population = "SAF")
    o$ard <- list(analyses = list(list(id = "AE", method = "hierarchical", dataset = "ADAE",
                                       where = tfl_r(where), variables = list("AEDECOD"))))
    o$page <- list(titles = list(`1` = list(center = id), `2` = list(center = label)))
    o
  }
  tfl_write_output(mk("T-1", "Adverse events", "TRTEMFL == \"Y\""),
                   file.path(d, "ai", "spec", "outputs", "T-1.yml"))
  tfl_write_output(mk("T-2", "Deaths", "DTHFL == \"Y\""),
                   file.path(d, "ai", "spec", "outputs", "T-2.yml"))
  list(dir = d, mk = mk)
}

test_that("tfl_ai_adopt(dry_run = TRUE): new, same, replace -- nothing copied", {
  s <- adiff_study()
  d <- s$dir
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  r <- tfl_ai_adopt(d, dry_run = TRUE)
  expect_identical(r$status, c("new", "new"))
  expect_false(file.exists(file.path(d, "spec", "outputs", "T-1.yml")))
  expect_false(file.exists(file.path(d, "ai", "adopted.csv")))
  invisible(tfl_ai_adopt(d, "T-1"))
  tfl_write_output(s$mk("T-1", "Adverse events (TEAE)", "TRTEMFL == \"Y\" & AESER == \"Y\""),
                   file.path(d, "ai", "spec", "outputs", "T-1.yml"))
  r <- tfl_ai_adopt(d, dry_run = TRUE)
  expect_identical(stats::setNames(r$status, r$output_id), c(`T-1` = "replace", `T-2` = "new"))
  invisible(tfl_ai_adopt(d, "T-2"))
  expect_identical(tfl_ai_adopt(d, "T-2", dry_run = TRUE)$status, "same")
})

test_that("tfl_ai_adopted_diff(): each value that differs, by its path; every newer draft at once", {
  s <- adiff_study()
  d <- s$dir
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  invisible(tfl_ai_adopt(d))
  expect_identical(nrow(tfl_ai_adopted_diff(d)), 0L)
  expect_identical(attr(tfl_ai_adopted_diff(d), "text"), "no differences")
  # the AI redrafts T-1: its title, its condition, a footnote added
  o <- s$mk("T-1", "Adverse events (TEAE)", "TRTEMFL == \"Y\" & AESER == \"Y\"")
  o$page$footnotes <- list(`1` = list(left = "Serious only."))
  tfl_write_output(o, file.path(d, "ai", "spec", "outputs", "T-1.yml"))
  x <- tfl_ai_adopted_diff(d)
  expect_named(x, c("output_id", "where", "change", "adopted", "draft"))
  expect_true(all(x$output_id == "T-1"))
  w <- stats::setNames(x$change, x$where)
  expect_identical(w[["label"]], "changed")
  expect_identical(w[["ard.analyses[AE].where"]], "changed")
  expect_identical(w[["page.footnotes.1.left"]], "added")
  expect_identical(x$draft[x$where == "ard.analyses[AE].where"],
                   "!r TRTEMFL == \"Y\" & AESER == \"Y\"")
  expect_match(attr(x, "text"), "differences in 1 report", fixed = TRUE)
  # one report asked for
  expect_identical(unique(tfl_ai_adopted_diff(d, "T-1")$output_id), "T-1")
  expect_identical(nrow(tfl_ai_adopted_diff(d, "T-2")), 0L)
})
