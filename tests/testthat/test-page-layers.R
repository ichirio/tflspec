# The page step layers as the other steps (canon PRINCIPLES 2, 11-28,
# CG-6): the page settings (style) < the template's page: < the report's
# page: < force.  A report's page: changes only the lines, cells and call
# arguments it writes.  Fictional values only.

pl_resolve <- function(tpl, rep, cfg = list()) {
  tflspec:::.page_resolve(cfg, NULL, tflspec:::.page_over(tpl, rep))
}

test_that("a report's rtf_page argument keeps the template's other arguments", {
  pg <- pl_resolve(list(rtf_page = list(orientation = "landscape")),
                   list(rtf_page = list(margin_left_in = 0.5)))
  expect_identical(pg$rtf_page$orientation, "landscape")
  expect_identical(pg$rtf_page$margin_left_in, 0.5)
  # the page settings' arguments under both
  pg <- pl_resolve(list(rtf_page = list(orientation = "landscape")),
                   list(rtf_page = list(margin_left_in = 0.5)),
                   cfg = list(rtf_page = list(orientation = "portrait", margin_top_in = 1)))
  expect_identical(pg$rtf_page, list(orientation = "landscape", margin_top_in = 1,
                                     margin_left_in = 0.5))
})

test_that("a report's line changes only that line's cells", {
  tpl <- list(footer = list(`1` = list(left = "Fictional footer A"),
                            `2` = list(left = "Fictional footer B")))
  rep <- list(footer = list(`2` = list(right = "Page {PAGE}")))
  pg <- pl_resolve(tpl, rep)
  expect_identical(pg$footer, list(`1` = list(left = "Fictional footer A"),
                                   `2` = list(left = "Fictional footer B", right = "Page {PAGE}")))
  # a template written as a sequence: numbered from 1
  pg <- pl_resolve(list(footnotes = list(list(left = "N1"), list(left = "N2"))),
                   list(footnotes = list(`2` = list(left = "N2 changed"))))
  expect_identical(pg$footnotes, list(`1` = list(left = "N1"), `2` = list(left = "N2 changed")))
  # a line the template has not: added after
  pg <- pl_resolve(tpl, list(footer = list(`3` = list(center = "C"))))
  expect_identical(names(pg$footer), c("1", "2", "3"))
  expect_identical(pg$footer$`3`, list(center = "C"))
})

test_that("!unset drops a cell, a line or a band; a sequence replaces the body", {
  tpl <- list(footnotes = list(`1` = list(left = "N1"), `2` = list(left = "N2", right = "R2")))
  u <- tflspec::tfl_unset()
  # a cell
  pg <- pl_resolve(tpl, list(footnotes = list(`2` = list(right = u))))
  expect_identical(pg$footnotes, list(`1` = list(left = "N1"), `2` = list(left = "N2")))
  # a line
  pg <- pl_resolve(tpl, list(footnotes = list(`1` = u)))
  expect_identical(pg$footnotes, list(`1` = list(left = "N2", right = "R2")))
  # a band: the page settings' lines of the band too
  cfg <- list(footnotes = list(last = list(`1` = list(left = "Program: {PROGRAM}"))))
  pg <- pl_resolve(tpl, list(footnotes = u), cfg = cfg)
  expect_null(pg$footnotes)
  # an unnamed sequence: the report's lines, none of the template's
  pg <- pl_resolve(tpl, list(footnotes = list(list(left = "Only")), titles = NULL), cfg = cfg)
  expect_identical(pg$footnotes, list(`1` = list(left = "Only"), `2` = list(left = "Program: {PROGRAM}")))
  # a block: the page settings' last lines gone, the body kept
  pg <- pl_resolve(tpl, list(footnotes = list(last = u)), cfg = cfg)
  expect_identical(pg$footnotes, list(`1` = list(left = "N1"), `2` = list(left = "N2", right = "R2")))
  # the template's !unset stays a marker until the page settings: their
  # argument is dropped, a later layer may still write it
  cfg2 <- list(rtf_page = list(orientation = "portrait", margin_top_in = 1))
  pg <- pl_resolve(list(rtf_page = list(margin_top_in = u)), NULL, cfg = cfg2)
  expect_identical(pg$rtf_page, list(orientation = "portrait"))
  pg <- pl_resolve(list(rtf_page = list(margin_top_in = u)), list(rtf_page = list(margin_top_in = 2)),
                   cfg = cfg2)
  expect_identical(pg$rtf_page, list(orientation = "portrait", margin_top_in = 2))
  # `~` says nothing
  pg <- pl_resolve(tpl, list(footnotes = NULL, rtf_page = NULL))
  expect_identical(pg$footnotes, tpl$footnotes)
})

test_that("a band prints style first, the merged body, style last", {
  cfg <- list(titles = list(first = list(`1` = list(left = "Sponsor")),
                            last = list(`1` = list(left = "Last"))))
  pg <- pl_resolve(list(titles = list(`1` = list(center = "T1"), `2` = list(center = "T2"))),
                   list(titles = list(`2` = list(center = "T2 own"))), cfg = cfg)
  expect_identical(unname(vapply(pg$titles, function(l) unlist(l)[[1L]], "")),
                   c("Sponsor", "T1", "T2 own", "Last"))
})

# a fictional study with its own copy of tflspec_v1/tflspec_summary, the
# copy's page: written as a custom template would
pl_study <- function(root, page) {
  st <- file.path(root, "STUDY-PL")
  dir.create(st)
  write_study_yml(st, c("study_id: STUDY-PL", "settings:", "  rounding: sas"))
  tfl_copy_templates(st, "tflspec_v1/tflspec_summary")
  f <- list.files(st, "^tflspec_summary[.]yml$", recursive = TRUE, full.names = TRUE)
  stopifnot(length(f) == 1L)
  x <- readLines(f, encoding = "UTF-8", warn = FALSE)
  at <- grep("^page:", x)
  stopifnot(length(at) == 1L)
  writeLines(c(x[seq_len(at - 1L)], page), f, useBytes = TRUE)
  st
}

pl_report <- function(page = NULL) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  writeLines(c("id: T-PL-1", "kind: table", "label: Fictional demographics",
               "template:", "  name: tflspec_v1/tflspec_summary", "  analysis:", "    by: TRT01A",
               "    continuous: [AGE]", "    categorical: [SEX]", page), f)
  tflspec:::.series_read_yaml(f)
}

test_that("a template's page: and the report's page: merge in tfl_template_resolve()", {
  root <- withr::local_tempdir("pl")
  st <- pl_study(root, c("page:", "  rtf_page: {orientation: landscape}",
                         "  footnotes:", "    1: {left: Fictional note 1}",
                         "    2: {left: Fictional note 2}"))
  out <- tfl_template_resolve(pl_report(c("page:", "  rtf_page: {margin_left_in: 0.5}",
                                          "  footnotes:", "    2: {right: Fictional right}")),
                              study = st)
  expect_identical(out$page$rtf_page, list(orientation = "landscape", margin_left_in = 0.5))
  expect_identical(out$page$footnotes,
                   list(`1` = list(left = "Fictional note 1"),
                        `2` = list(left = "Fictional note 2", right = "Fictional right")))
  # no report page: the template's
  d <- tfl_template_resolve(pl_report(), study = st)
  expect_identical(d$page$rtf_page, list(orientation = "landscape"))
  expect_identical(attr(d, "page_template")$rtf_page, list(orientation = "landscape"))
})

test_that("tfl_page_origins() says which layer gave each resolved field", {
  root <- withr::local_tempdir("pl")
  st <- pl_study(root, c("page:", "  rtf_page: {orientation: landscape}",
                         "  footnotes:", "    1: {left: Fictional note 1}",
                         "    2: {left: Fictional note 2}"))
  s <- tfl_read_study_spec(st)
  rf <- file.path(root, "T-PL-1.yml")
  writeLines(c("id: T-PL-1", "kind: table", "label: Fictional demographics",
               "template:", "  name: tflspec_v1/tflspec_summary", "  analysis:", "    by: TRT01A",
               "    continuous: [AGE]", "    categorical: [SEX]",
               "page:", "  rtf_page: {margin_left_in: 0.5}",
               "  footnotes:", "    2: {right: Fictional right}"), rf)
  o <- tfl_read_output(rf)
  po <- tfl_page_origins(o, s)
  expect_named(po, c("key", "block", "line", "name", "value", "origin"))
  get <- function(key, name, line = NA) {
    r <- po[po$key == key & po$name == name & (is.na(line) | po$line %in% line), , drop = FALSE]
    r$origin
  }
  expect_identical(get("rtf_page", "orientation"), "template")
  expect_identical(get("rtf_page", "margin_left_in"), "report")
  # the footnotes: the resolved numbering (the style's first lines before)
  fn <- po[po$key == "footnotes" & po$block == "body", , drop = FALSE]
  expect_identical(fn$value, c("Fictional note 1", "Fictional note 2", "Fictional right"))
  expect_identical(fn$origin, c("template", "template", "report"))
  # the same values as the resolved page
  res <- tfl_resolve_output(o, s)
  expect_identical(res$page$rtf_page$orientation, "landscape")
  expect_identical(res$page$rtf_page$margin_left_in, 0.5)
  lines <- res$page$footnotes
  expect_identical(lines[[fn$line[3L]]]$right, "Fictional right")
  expect_identical(lines[[fn$line[1L]]]$left, "Fictional note 1")
  # every other field: the style's
  rest <- po[!(po$key %in% c("rtf_page", "footnotes") & po$origin %in% c("template", "report")), ]
  expect_true(all(rest$origin %in% c("style", "study")))
  # a resolved report is not taken
  expect_error(tfl_page_origins(res, s), "as read")
})

test_that("a shipped template (page: {titles: ~, footnotes: ~}) gives the report's page as it is", {
  rp <- list(titles = list(`1` = list(center = "Fictional title")),
             footnotes = list(`1` = list(left = "Fictional note")),
             rtf_page = list(orientation = "landscape"))
  out <- tfl_template_resolve(pl_report(c("page:", "  rtf_page: {orientation: landscape}",
                                          "  titles:", "    1: {center: Fictional title}",
                                          "  footnotes:", "    1: {left: Fictional note}")))
  expect_identical(out$page[names(rp)], rp)
  expect_null(attr(out, "page_template"))
})
