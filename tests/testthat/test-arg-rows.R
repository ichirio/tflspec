test_that("sort keys read as rows: by, direction, level, column", {
  r <- tfl_rows_read(c(".overall", "-n@group1", "group1", ".depth", "-n[Total]", "label"), "sort_keys")
  expect_identical(names(r), c("by", "direction", "level", "column"))
  expect_identical(r$by, c(".overall", "n", "group1", ".depth", "n", "label"))
  expect_identical(r$direction, c("asc", "desc", "asc", "asc", "desc", "asc"))
  expect_identical(r$level, c(NA, "group1", NA, NA, NA, NA))
  expect_identical(r$column, c(NA, NA, NA, NA, "Total", NA))
  # both scopes, an aggregate, levels(), a column with spaces
  r2 <- tfl_rows_read(c("-max(n)@group1[Drug X]", "levels(group1)", "-p[Xanomeline High Dose]"), "sort_keys")
  expect_identical(r2$by, c("max(n)", "levels(group1)", "p"))
  expect_identical(r2$level, c("group1", NA, NA))
  expect_identical(r2$column, c("Drug X", NA, "Xanomeline High Dose"))
})

test_that("the rows write the same keys back (a round trip)", {
  keys <- list(
    c(".overall", "-n@group1", "group1", ".depth", "-n", "label"),
    c(".overall", "-n@group1[Placebo]", "group1", ".depth", "-n[Placebo]", "label"),
    c("levels(group1)", "-max(n)", "min(n)@group2[Total]", "-.overall"),
    c("USUBJID", "-AESTDTC"),
    # keys of no form the grammar knows are kept whole in `by`
    c("a@", "x[y", "a@b@c"),
    "-n")
  for (k in keys) {
    expect_identical(tfl_rows_write(tfl_rows_read(k, "sort_keys"), "sort_keys"), k, label = paste(k, collapse = " "))
  }
})

test_that("false is no rows and the switch; NULL is NULL; no rows is NULL", {
  r <- tfl_rows_read(FALSE, "sort_keys")
  expect_identical(nrow(r), 0L)
  expect_true(attr(r, "false"))
  expect_identical(tfl_rows_write(r, "sort_keys", false = isTRUE(attr(r, "false"))), FALSE)
  expect_null(tfl_rows_read(NULL, "sort_keys"))
  expect_null(tfl_rows_write(r, "sort_keys"))
  expect_null(tfl_rows_write(list(), "sort_keys"))
  expect_identical(tfl_rows_write(list(), "sort_keys", false = TRUE), FALSE)
  expect_identical(nrow(tfl_rows_read(character(), "sort_keys")), 0L)
})

test_that("a form's records write as keys: blank cells left out, a row with no by too", {
  recs <- list(list(by = ".overall"),
               list(by = "n", direction = "desc", level = "group1"),
               list(direction = "asc"),
               list(by = "n", direction = "desc", level = "", column = "Drug X"),
               list(by = "label", direction = "asc"))
  expect_identical(tfl_rows_write(recs, "sort_keys"), c(".overall", "-n@group1", "-n[Drug X]", "label"))
  # a data frame with NA cells, as tfl_rows_read() gives
  df <- data.frame(by = c("n", "label"), direction = c("desc", NA), level = NA, column = c("Total", NA),
                   stringsAsFactors = FALSE)
  expect_identical(tfl_rows_write(df, "sort_keys"), c("-n[Total]", "label"))
})

test_that("rows the text cannot say stop, and so do a bad value and serializer", {
  expect_error(tfl_rows_write(list(list(by = "n", direction = "down")), "sort_keys"), "asc or desc")
  expect_error(tfl_rows_write(list(list(by = "n@group1", level = "group2")), "sort_keys"), "has @ or \\[")
  expect_error(tfl_rows_write(list(list(by = "n", level = "a@b")), "sort_keys"), "one row key")
  expect_error(tfl_rows_write(list(list(by = "n", column = "a]b")), "sort_keys"), "has no \\]")
  expect_error(tfl_rows_read(TRUE, "sort_keys"), "keys are text")
  expect_error(tfl_rows_read(c("-n", ""), "sort_keys"), "keys are text")
  expect_error(tfl_rows_read("-n", "nope"), "serializer is one of sort_keys")
  expect_error(tfl_rows_write(list(), "nope"), "serializer is one of sort_keys")
})

test_that("plan_sort's keys are Rows of four columns, written as text by sort_keys", {
  d <- tfl_arg_defs(fn = "plan_sort")
  k <- d[d$name == "keys", ]
  expect_identical(k$input, "table")
  expect_identical(k$serializer, "sort_keys")
  expect_identical(k$type, "rows<by,direction,level,column>|list<string>|logical")
  expect_identical(names(k$columns[[1L]]), c("by", "direction", "level", "column"))
  expect_identical(k$columns[[1L]]$direction$values, c("asc", "desc"))
  expect_identical(k$columns[[1L]]$by$value_labels$value, c(".overall", ".depth"))
  expect_false(is.na(k$columns[[1L]]$by$value_labels$label_ja[1L]))
  expect_false(is.na(k$false_label_ja))
  expect_identical(k$new_row[[1L]], list(direction = "asc"))
  # (the dots' type is the SPEC's text: unchanged)
  expect_identical(d$type[d$name == "..."], "list<string>|logical")
  # every other row: no serializer
  expect_true(all(is.na(d$serializer[d$name != "keys"])))
  # the SPEC's value checks as before: text keys and false pass
  expect_identical(nrow(tfl_schema_check(tfl_item("plan_sort", list(keys = c(".overall", "-n@group1"))))), 0L)
  expect_identical(nrow(tfl_schema_check(tfl_item("plan_sort", list(keys = FALSE)))), 0L)
})

test_that("a serializer outside the closed list stops", {
  t1 <- function(d) tflspec:::.arg_table(list(a = d), "content", "f")
  expect_error(t1(list(type = "rows<a,b>", serializer = "csv")), "serializer \"csv\" is not one of sort_keys")
  expect_identical(t1(list(type = "rows<a,b>"))$serializer, NA_character_)
})
