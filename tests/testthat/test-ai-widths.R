# The AI's reports always say their column widths: what an answer leaves
# out, R writes -- a table's once it is built (rtfreporter's
# default_col_widths()), a listing's when the answer is read -- with the
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
  # (rtfreporter's default_col_widths(): one width a printed column, by
  # name; before it, the stub 3 -- a hierarchy's 4 -- to the values' 1)
  dcw <- exists("default_col_widths", envir = asNamespace("rtfreporter"), inherits = FALSE)
  for (f in files) {
    y <- yaml::read_yaml(file.path(d, f))
    pc <- Filter(function(it) identical(names(it), "plan_columns"), y$content)
    expect_length(pc, 1L)
    w <- pc[[1L]]$plan_columns$widths
    if (dcw) {
      expect_null(w$.values, info = f)
      expect_true(length(w) >= 2L && all(unlist(w) > 0), info = f)
    } else {
      expect_identical(w$.values, 1L, info = f)
      stub <- unlist(w[names(w) != ".values"])
      expect_true(length(stub) >= 1L && all(stub >= 2L), info = f)
    }
  }
  if (!dcw) {
    # the AE table (a hierarchy): its stub 4
    ae <- yaml::read_yaml(file.path(d, files[grepl("T-14-3-1", files)]))
    w <- Filter(function(it) identical(names(it), "plan_columns"), ae$content)[[1L]]$plan_columns$widths
    expect_true(any(unlist(w[names(w) != ".values"]) == 4L))
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
