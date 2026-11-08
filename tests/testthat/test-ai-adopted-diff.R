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

test_that("a report adopted that spec/toc.yml does not list gets its entry, after its number's", {
  s <- adiff_study()
  d <- s$dir
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  yaml::write_yaml(list(outputs = list(
    list(id = "T-1", kind = "table", label = "Adverse events"),
    list(id = "T-14-1-1", kind = "table", label = "Demographics"),
    list(id = "T-14-9-9", kind = "table", label = "Other"))), file.path(d, "spec", "toc.yml"))
  tfl_write_output(s$mk("T-14-1-1.a", "Demographics, part a", "TRTEMFL == \"Y\""),
                   file.path(d, "ai", "spec", "outputs", "T-14-1-1.a.yml"))
  dry <- tfl_ai_adopt(d, dry_run = TRUE)
  expect_identical(stats::setNames(dry$toc, dry$output_id),
                   c(`T-1` = NA, `T-14-1-1.a` = "add", `T-2` = "add"))
  r <- tfl_ai_adopt(d)
  expect_identical(sort(r$output_id[r$toc %in% "added"]), c("T-14-1-1.a", "T-2"))
  toc <- yaml::read_yaml(file.path(d, "spec", "toc.yml"))$outputs
  # the part after its number; a report of no number at the end
  expect_identical(vapply(toc, `[[`, "", "id"), c("T-1", "T-14-1-1", "T-14-1-1.a", "T-14-9-9", "T-2"))
  expect_identical(toc[[3L]]$label, "Demographics, part a")
  expect_identical(toc[[3L]]$population, "SAF")
  # adopted again: no second entry
  invisible(tfl_ai_adopt(d, overwrite = TRUE))
  expect_length(yaml::read_yaml(file.path(d, "spec", "toc.yml"))$outputs, 5L)
})

# ---- adopting a draft that has no widths: tflspec's are written (#690) ----

aw_study <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "adam"))
  saveRDS(tflspec::adsl, file.path(d, "adam", "adsl.rds"))
  saveRDS(tflspec::adae, file.path(d, "adam", "adae.rds"))
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "ONCO-01", overwrite = TRUE))
  dir.create(file.path(d, "ai", "spec", "outputs"), recursive = TRUE)
  d
}
aw_table <- function(d, id, widths = FALSE) {
  o <- tfl_output(id, "table", population = "SAF")
  o$extends <- list("defaults")
  o$ard <- list(analyses = list(list(id = "AGE", method = "continuous", dataset = "ADSL",
                                     population = "SAF", by = list("TRT01A"),
                                     variables = list("AGE"))))
  o$data <- list(source = "ard")
  o$content <- list(tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable"))),
                    tfl_item("plan_cells", list(continuous = list(n = "{N:.0f}"))))
  if (widths) {
    o$content <- c(o$content, list(tfl_item("plan_columns",
                                            list(widths = list(row_label = 2, .values = 1)))))
  }
  tfl_write_output(o, file.path(d, "ai", "spec", "outputs", paste0(id, ".yml")))
}
aw_listing <- function(d, id) {
  cl <- function(v, label, width) tfl_call("listing_col", list(vars = list(v), label = label, width = width))
  o <- tfl_output(id, "listing", label = id, extends = "defaults",
                  data = list(source = "dataset", dataset = "ADAE", population = "SAF"),
                  content = list(tfl_item("plan_listing", list(type = "multiline", cols = list(
                    cl("USUBJID", "Subject", 20L), cl("AEDECOD", "Preferred term", 40L))))))
  tfl_write_output(o, file.path(d, "ai", "spec", "outputs", paste0(id, ".yml")))
}
aw_md5 <- function(...) unname(tools::md5sum(file.path(...)))
aw_widths <- function(f) {
  pc <- Filter(function(it) identical(it$fn, "plan_columns"), tfl_read_output(f)$content)
  if (length(pc)) pc[[1L]]$args$widths else NULL
}

test_that("tfl_ai_adopt(): a draft with no widths is adopted with tflspec's, the draft too; adopted again, nothing new", {
  skip_if_not_installed("rtfreporter")
  d <- aw_study()
  aw_table(d, "T-1")
  dr <- file.path(d, "ai", "spec", "outputs", "T-1.yml")
  st <- file.path(d, "spec", "outputs", "T-1.yml")
  expect_null(aw_widths(dr))
  expect_message(r <- tfl_ai_adopt(d, "T-1"), "T-1 had no column widths")
  expect_identical(r$status, "adopted")
  # one width a printed column (row headings and two arms: 60:20:20)
  w <- unlist(aw_widths(st))
  expect_length(w, 3L)
  expect_equal(unname(w / sum(w)), c(0.6, 0.2, 0.2))
  expect_identical(aw_md5(d, "ai", "spec", "outputs", "T-1.yml"), aw_md5(d, "spec", "outputs", "T-1.yml"))
  # its assumption, once
  asm <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  expect_identical(sum(vapply(asm, function(x) identical(x$item, "widths") && identical(x$output_id, "T-1"), NA)), 1L)
  # the RTF says the widths
  f <- suppressMessages(tfl_run_output("T-1", d))
  l <- readLines(f, warn = FALSE)
  row <- tail(l[startsWith(l, "\\trowd") & grepl("cellx", l)], 1L)
  cx <- as.integer(regmatches(row, gregexpr("(?<=cellx)[0-9]+", row, perl = TRUE))[[1L]])
  expect_equal(round(diff(c(0L, cx)) / max(cx), 2), c(0.6, 0.2, 0.2))
  # adopted again: the same, nothing written
  before <- c(aw_md5(d, "spec", "outputs", "T-1.yml"), aw_md5(d, "spec", "_assumptions.yml"),
              aw_md5(d, "ai", "adopted.csv"))
  expect_no_message(r2 <- tfl_ai_adopt(d, "T-1"))
  expect_identical(r2$status, "same")
  expect_identical(before, c(aw_md5(d, "spec", "outputs", "T-1.yml"), aw_md5(d, "spec", "_assumptions.yml"),
                             aw_md5(d, "ai", "adopted.csv")))
  # the draft is not newer than its adoption
  expect_identical(tfl_ai_outputs(d)$adopted[tfl_ai_outputs(d)$output_id == "T-1"], "yes")
  # a dry run writes nothing
  aw_table(d, "T-2")
  m <- aw_md5(d, "ai", "spec", "outputs", "T-2.yml")
  expect_identical(tfl_ai_adopt(d, "T-2", dry_run = TRUE)$status, "new")
  expect_identical(aw_md5(d, "ai", "spec", "outputs", "T-2.yml"), m)
})

test_that("tfl_ai_adopt(): a table with widths and a listing with its widths set are adopted byte for byte", {
  skip_if_not_installed("rtfreporter")
  d <- aw_study()
  aw_table(d, "T-1", widths = TRUE)
  aw_listing(d, "L-1")
  ids <- c("T-1", "L-1")
  m <- vapply(ids, function(i) aw_md5(d, "ai", "spec", "outputs", paste0(i, ".yml")), "")
  expect_no_message(r <- tfl_ai_adopt(d))
  expect_identical(sort(r$status), c("adopted", "adopted"))
  for (i in ids) {
    expect_identical(aw_md5(d, "spec", "outputs", paste0(i, ".yml")), m[[i]])
    expect_identical(aw_md5(d, "ai", "spec", "outputs", paste0(i, ".yml")), m[[i]])
  }
  expect_false(file.exists(file.path(d, "spec", "_assumptions.yml")))
})

test_that("tfl_ai_adopt(): widths that cannot be filled never stop the adoption", {
  skip_if_not_installed("rtfreporter")
  d <- aw_study()
  aw_table(d, "T-1")
  # a draft that cannot be read stops tfl_fill_widths()
  writeLines(c("id: [", "  broken"), file.path(d, "ai", "spec", "outputs", "T-9.yml"))
  m <- aw_md5(d, "ai", "spec", "outputs", "T-1.yml")
  expect_message(r <- tfl_ai_adopt(d, "T-1"), "column widths were not filled")
  expect_identical(r$status, "adopted")
  expect_identical(aw_md5(d, "spec", "outputs", "T-1.yml"), m)
  expect_identical(aw_md5(d, "ai", "spec", "outputs", "T-1.yml"), m)
})
