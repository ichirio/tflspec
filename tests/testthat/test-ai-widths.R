# The AI's reports always say their column widths: what an answer leaves
# out, R writes -- a table's once it is built (tflspec's rule,
# .tfl_default_widths()), a listing's when the answer is read -- with the
# assumption that they are the default ones

test_that("a table with no widths gets the usual layout's in its report file, when read and when built", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  root <- withr::local_tempdir()
  ad <- file.path(root, "adam")
  dir.create(ad)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(ad, paste0(x, ".xpt")))
  }
  d <- file.path(root, "study")
  book <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
  tfl_ai_prepare(unit = "toc", d, book, adam = ad, quiet = TRUE, size = 3L)
  ai_drafts_from_fixtures(d)
  # read: the tables' widths written already (their data can be read)
  col <- suppressMessages(tfl_ai_collect_answers(d))
  f1 <- col$reports$file[col$reports$output_id == "T-14-1-1"]
  y1 <- yaml::read_yaml(file.path(d, f1))
  expect_true(any(vapply(y1$content, function(it) identical(names(it), "plan_columns"), NA)))
  expect_true(file.exists(file.path(d, "ai", "widths.yml")))
  res <- suppressMessages(tfl_ai_run(d, requests = c("demographics-1", "ae-1")))
  files <- res$reports$file[res$reports$request %in% c("demographics-1", "ae-1")]
  # (tflspec's rule: one width a printed column, by name, in characters;
  # the stub at least 12)
  for (f in files) {
    y <- yaml::read_yaml(file.path(d, f))
    pc <- Filter(function(it) identical(names(it), "plan_columns"), y$content)
    expect_length(pc, 1L)
    w <- pc[[1L]]$plan_columns$widths
    expect_null(w$.values, info = f)
    expect_true(length(w) >= 2L && all(unlist(w) > 0), info = f)
    expect_true(w[[1L]] >= 12L, info = f)
  }
  # each report whose widths R set says so: an assumption
  ass <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  wa <- Filter(function(x) identical(x$item, "widths"), ass)
  expect_true(all(c("T-14-1-1", "T-14-3-1") %in% vapply(wa, `[[`, "", "output_id")))
  expect_match(wa[[1L]]$assumption, "default proportions, set for now", fixed = TRUE)
  # run again: kept (the file as it is now is what was built)
  res2 <- suppressMessages(tfl_ai_run(d, requests = c("demographics-1", "ae-1")))
  b <- res2$reports$built[res2$reports$request %in% c("demographics-1", "ae-1")]
  expect_true(all(b == "kept"))
})

test_that("the default rule: the user's three tiers, the row headings keeping their content", {
  w <- function(body, stub = 1L, ...) tflspec:::.tfl_default_widths(body, stub, ...)
  vals <- function(k) stats::setNames(as.list(rep("40 (51.3%)", k)), paste0("C", seq_len(k)))
  tab <- function(stub, k) as.data.frame(c(list(row_label = stub), vals(k)))
  age <- c("Age group, n (%)", "    65-80")
  # 1: each value 60 / (n + 1) % of the 120, the stub the rest -- 70:30,
  # 60:20:20, 55:15:15:15
  expect_identical(unname(as.vector(w(tab(age, 1)))), c(84L, 36L))
  expect_identical(unname(as.vector(w(tab(age, 2)))), c(72L, 24L, 24L))
  x <- w(tab(age, 3))
  expect_identical(unname(as.vector(x)), c(66L, 18L, 18L, 18L))
  expect_identical(attr(x, "tier"), 1L)
  expect_identical(names(x), c("row_label", "C1", "C2", "C3"))
  # 2: ten value columns of 12 leave nothing; of 8 (two lines) the stub its
  # content (18)
  y <- w(tab(age, 10))
  expect_identical(attr(y, "tier"), 2L)
  expect_true(all(y[-1L] == 8L))
  expect_identical(unname(y[1L]), 40L)
  # a long stub (28, kept up to 30) with twelve columns stays at 2: 24 is
  # less than it keeps, but two value columns or more
  long <- "a long row heading of text"
  t2 <- w(tab(long, 12))
  expect_identical(attr(t2, "tier"), 2L)
  expect_identical(unname(t2[1L]), 24L)
  # 3: with fourteen, 8 is left: all equal, the stub two
  z <- w(tab(long, 14))
  expect_identical(attr(z, "tier"), 3L)
  expect_identical(unname(z[1L]), 2L * unname(z[2L]))
  expect_true(all(z[-1L] == z[2L]))
})

test_that("a table that says its widths is left as it is", {
  o <- tfl_output("T-1", "table", content = list(
    tfl_item("table_plan", list(cols = "TRT01A")),
    tfl_item("plan_columns", list(widths = list(row_label = 5, .values = 2)))))
  expect_true(.ai_has_widths(o))
  expect_null(.ai_table_widths(o, o, NULL))
})

test_that("a listing's columns get widths from their variables and headers", {
  vars <- data.frame(dataset = "ADAE", variable = c("USUBJID", "ASTDT", "ASTDY", "AEDECOD"),
                     kind = c("char", "date", "num", "char"), stringsAsFactors = FALSE)
  col <- function(v, label, width = NULL) {
    tfl_call("listing_col", c(list(vars = as.list(v), label = label), if (!is.null(width)) list(width = width)))
  }
  o <- tfl_output("L-1", "listing", content = list(tfl_item("plan_listing", list(cols = list(
    col("USUBJID", "Subject"), col("ASTDT", "Start date"), col("ASTDY", "Study day of onset"),
    col(c("AEBODSYS", "AEDECOD"), "SOC / PT"), col("ASTDY", "Day", width = 6))))))
  o$data <- list(source = "dataset", dataset = "ADAE")
  w <- vapply(.ai_listing_widths(o, vars)$content[[1L]]$args$cols, function(cl) cl$args$width, 1)
  expect_identical(unname(w), c(12, 10, 18, 20, 6))
  # a table is not touched
  t1 <- tfl_output("T-1", "table", content = list(tfl_item("table_plan", list(cols = "TRT01A"))))
  expect_identical(.ai_listing_widths(t1, vars), t1)
})

# ---- remembered widths are tied to the draft they were made for (#639) ----
# ai/widths.yml holds, per report, the widths and a fingerprint of the draft
# (resolved over its layers, widths left out): a rewritten draft that changes
# the columns gets widths made again, not the old names.

aw639_study <- function(root) {
  ad <- file.path(root, "adam")
  dir.create(ad, recursive = TRUE, showWarnings = FALSE)
  for (x in c("adsl", "adlb")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(ad, paste0(x, ".xpt")))
  }
  d <- file.path(root, "study")
  tfl_ai_prepare(unit = "toc", d, system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec"),
                 adam = normalizePath(ad, winslash = "/"), quiet = TRUE, size = 3L)
  d
}

# the Issue's draft: the shift template, its codelists optional, its widths optional
aw639_draft <- function(d, codelists = FALSE, title = "Table 14.3.5", widths = NULL) {
  cl <- c("    codelists:",
          paste0("      ", c("ANRIND", "BNRIND"),
                 ": [{value: LOW, label: Low}, {value: NORMAL, label: Normal}, {value: HIGH, label: High}]"))
  ai_drafts_from_answer(d, paste(c("```yaml",
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-3-5]}",
    "outputs:",
    "  - id: T-14-3-5",
    "    kind: table",
    "    label: Shift table",
    "    population: SAF",
    "    extends: [defaults]",
    if (codelists) cl,
    "    template:",
    "      name: shift",
    "      args: {data: ADLB, where: \"ONTRTFL == 'Y'\", percent_of: total}",
    if (!is.null(widths)) c("    content:", paste0("      - plan_columns: {widths: ", widths, "}")),
    sprintf("    page: {titles: {\"1\": {center: %s}}}", title),
    "assumptions: []", "```"), collapse = "\n"), "table")
}

aw639_run <- function(d) {
  res <- suppressMessages(tfl_ai_run(d, requests = "labs-1"))
  rp <- res$reports[res$reports$output_id == "T-14-3-5", , drop = FALSE]
  y <- yaml::read_yaml(file.path(d, rp$file))
  pc <- Filter(function(it) identical(names(it), "plan_columns"), y$content)
  list(rp = rp, widths = if (length(pc)) pc[[1L]]$plan_columns$widths,
       entry = yaml::read_yaml(file.path(d, "ai", "widths.yml"))[["T-14-3-5"]],
       key = tflspec:::.ai_built_read(d)$key[tflspec:::.ai_built_read(d)$output_id == "T-14-3-5"])
}

aw639_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
}

test_that("a rewritten draft that adds codelists gets widths for its new columns (#639)", {
  aw639_skip()
  d <- aw639_study(withr::local_tempdir())
  aw639_draft(d)
  r1 <- aw639_run(d)
  expect_identical(r1$rp$status, "ok")
  expect_true(any(grepl("____LOW$", names(r1$widths))))
  expect_false(any(grepl("____Low$", names(r1$widths))))
  # the entry: the widths and a fingerprint of the draft
  expect_named(r1$entry, c("draft", "widths"), ignore.order = TRUE)
  expect_true(is.character(r1$entry$draft) && length(r1$entry$draft) == 1L)
  # rewritten with codelists, still no widths
  aw639_draft(d, codelists = TRUE)
  r2 <- aw639_run(d)
  expect_identical(r2$rp$status, "ok")
  expect_false(is.na(r2$rp$rtf))
  expect_true(file.exists(file.path(d, r2$rp$rtf)))
  expect_true(any(grepl("____Low$", names(r2$widths))))
  expect_false(any(grepl("____LOW$", names(r2$widths))))
  expect_false(identical(r2$entry$draft, r1$entry$draft))
  expect_identical(names(r2$entry$widths), names(r2$widths))
})

test_that("a draft run again is kept, and a read-time build that cannot be made does not lose that (#639)", {
  aw639_skip()
  d <- aw639_study(withr::local_tempdir())
  aw639_draft(d)
  # (the widths are not made when the answer is read: run.R's build gives them)
  testthat::local_mocked_bindings(.ai_widths_on_read = function(...) invisible(character()),
                                  .package = "tflspec")
  r1 <- aw639_run(d)
  expect_identical(r1$rp$built, "built")
  expect_true(length(r1$widths) > 1L)
  r2 <- aw639_run(d)
  expect_identical(r2$rp$built, "kept")
  expect_identical(r2$key, r1$key)
  expect_identical(r2$widths, r1$widths)
  expect_identical(r2$entry, r1$entry)
})

test_that("a rewrite that changes only a title gets the same widths (#639)", {
  aw639_skip()
  d <- aw639_study(withr::local_tempdir())
  aw639_draft(d)
  r1 <- aw639_run(d)
  aw639_draft(d, title = "Table 14.3.5 shift")
  r2 <- aw639_run(d)
  expect_identical(r2$rp$status, "ok")
  expect_identical(r2$rp$built, "built")
  expect_identical(r2$widths, r1$widths)
  expect_false(identical(r2$entry$draft, r1$entry$draft))
})

test_that("a layer's change makes the widths of a rewritten draft again (#639)", {
  aw639_skip()
  d <- aw639_study(withr::local_tempdir())
  aw639_draft(d)
  r1 <- aw639_run(d)
  expect_true(any(grepl("____LOW$", names(r1$widths))))
  f <- file.path(d, "spec", "defaults.yml")
  y <- yaml::read_yaml(f)
  y$codelists <- list(ANRIND = list(list(value = "LOW", label = "Low"), list(value = "NORMAL", label = "Normal"),
                                    list(value = "HIGH", label = "High")),
                      BNRIND = list(list(value = "LOW", label = "Low"), list(value = "NORMAL", label = "Normal"),
                                    list(value = "HIGH", label = "High")))
  yaml::write_yaml(y, f)
  # the same draft written again (its text unchanged): the layer it extends
  # is what changed.  Since #655 a layer's code list reaches the ARD, so the
  # list renames the columns (____LOW -> ____Low): the widths are made again
  # for the draft as the layers now make it, and name the new columns.
  aw639_draft(d)
  r2 <- aw639_run(d)
  expect_identical(r2$rp$status, "ok")
  expect_false(is.na(r2$rp$rtf))
  expect_false(identical(r2$entry$draft, r1$entry$draft))
  expect_identical(names(r2$entry$widths), names(r2$widths))
  expect_true(any(grepl("____Low$", names(r2$widths))))
  expect_false(any(grepl("____LOW$", names(r2$widths))))
})

test_that("an old-form ai/widths.yml naming columns that are gone does not stop a build (#639)", {
  aw639_skip()
  d <- aw639_study(withr::local_tempdir())
  aw639_draft(d)
  r1 <- aw639_run(d)
  # (before: the widths alone, by the old column names)
  yaml::write_yaml(list(`T-14-3-5` = r1$entry$widths), file.path(d, "ai", "widths.yml"))
  aw639_draft(d, codelists = TRUE)
  r2 <- aw639_run(d)
  expect_identical(r2$rp$status, "ok")
  expect_false(is.na(r2$rp$rtf))
  expect_true(any(grepl("____Low$", names(r2$widths))))
  # and it is remembered in the new form
  expect_named(r2$entry, c("draft", "widths"), ignore.order = TRUE)
})

test_that("a draft with its own widths is left alone (#639)", {
  aw639_skip()
  d <- aw639_study(withr::local_tempdir())
  aw639_draft(d)
  r1 <- aw639_run(d)
  own <- r1$widths
  own[[1L]] <- own[[1L]] + 2L
  txt <- paste0("{", paste(sprintf("\"%s\": %s", names(own), unlist(own)), collapse = ", "), "}")
  aw639_draft(d, widths = txt)
  r2 <- aw639_run(d)
  expect_identical(r2$rp$status, "ok")
  expect_identical(r2$widths, own)
})
