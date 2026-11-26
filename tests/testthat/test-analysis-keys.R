# An analysis item's `name:` and `from:` (OBJECTS.md B-4, item 2): reading,
# the object list, the rename, the checks, and the ARD program (the writers
# are B-5's: test-b5-writers.R).

ak_read <- function(lines) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(lines, f)
  tfl_read_output(f)
}

ak_table <- function(extra_a = character(), extra_b = character()) ak_read(c(
  "id: T-1", "kind: table", "label: AE",
  "data:",
  "  - id: adsl_sub", "    from: ADSL",
  "ard:", "  analyses:",
  "    - id: BIGN", "      method: categorical", "      data: adsl_sub", "      variables: [TRT01A]",
  extra_a,
  "    - id: AGE", "      method: continuous",
  if (!any(grepl("from:", extra_b))) "      data: adsl_sub", "      variables: [AGE]",
  extra_b,
  "content:", "  - table_plan: {rows: [AGE]}"))

test_that("`name:` and `from:` are read without a warning", {
  expect_no_warning(o <- ak_table("      name: pop", "      from: pop"))
  an <- o$ard$analyses
  expect_identical(an[[1L]]$name, "pop")
  expect_identical(an[[2L]]$from, "pop")
  # an unknown key is still warned
  expect_warning(ak_table("      nmae: pop"), "is not a key")
})

test_that("the object list: a named item under its name, the others as today", {
  x <- tfl_output_objects(ak_table("      name: pop"))
  i <- which(x$stage == "analysis" & x$id == "BIGN")
  expect_identical(x$name[i], "pop")
  expect_identical(x$name_source[i], "spec")
  expect_true(x$used[i])
  j <- which(x$stage == "analysis" & x$id == "AGE")
  expect_identical(x$name[j], tflspec:::.ard_item_names("AGE"))
  expect_identical(x$name_source[j], "default")
  # without the key: the default, as before
  y <- tfl_output_objects(ak_table())
  expect_identical(y$name[y$id %in% "BIGN" & y$stage == "analysis"], tflspec:::.ard_item_names("BIGN", "AGE")[1L])
  expect_true(all(y$name_source[y$stage == "analysis"] == "default"))
})

test_that("`from:` is a use; an undefined name is reported", {
  o <- ak_table("      name: pop", "      from: pop")
  u <- attr(tfl_output_objects(o), "uses")
  expect_true(any(u$name == "pop" & u$arg == "from" & u$item == "AGE"))
  x <- tfl_output_objects(o)
  expect_identical(x$from[x$id %in% "AGE" & x$stage == "analysis"], "pop")
  expect_equal(nrow(tfl_output_name_check(o)), 0L)
  r <- tfl_output_name_check(ak_table(extra_b = "      from: nothing"))
  expect_true(any(r$level == "error" & grepl("`nothing`, which nothing above", r$message)))
  # made below it
  below <- ak_table("      from: late", "      name: late")
  r <- tfl_output_name_check(below)
  expect_true(any(r$level == "error" & grepl("made below it", r$message)))
  # a study dataset is fine
  ds <- ak_table(extra_b = "      from: ADSL")
  expect_true(any(tfl_output_name_check(ds)$level == "error"))
  expect_false(any(.check_object_rows(ds, "T-1", datasets = "ADSL")$level == "error"))
})

test_that("a rename writes the item's `name:`, not its id, and follows `from:`", {
  o <- ak_table("      name: pop", "      from: pop")
  n <- tfl_output_rename(o, "pop", "pop2")
  expect_identical(n$ard$analyses[[1L]]$name, "pop2")
  expect_identical(n$ard$analyses[[1L]]$id, "BIGN")
  expect_identical(n$ard$analyses[[2L]]$from, "pop2")
  # an item without `name:` is renamed by its id, as before
  d <- tfl_output_rename(ak_table(), "AGE", "AGE2")
  expect_identical(d$ard$analyses[[2L]]$id, "AGE2")
  expect_null(d$ard$analyses[[2L]]$name)
  # a data a `from:` names
  f <- tfl_output_rename(ak_table(extra_b = "      from: adsl_sub"), "adsl_sub", "pop_sub")
  expect_identical(f$ard$analyses[[2L]]$from, "pop_sub")
  # a new name another object has
  expect_error(tfl_output_rename(ak_table("      name: pop", "      name: pop_b"), "pop", "pop_b"),
               "already names an object")
})

test_that("the checks: `from:` with `data:`, a name that is not an R name, a name twice", {
  both <- ak_table(extra_b = c("      data: adsl_sub", "      from: adsl_sub"))
  r <- tfl_output_name_check(both)
  expect_true(any(r$level == "error" & grepl("from:` \\(the pipe\\) or `data:` \\(the first argument\\), not both",
                                              r$message)))
  # `from:` alone: no error of this kind
  one <- ak_read(c("id: T-1", "kind: table", "data:", "  - id: adsl_sub", "    from: ADSL",
                   "ard:", "  analyses:", "    - id: AGE", "      method: continuous",
                   "      from: adsl_sub", "      variables: [AGE]",
                   "content:", "  - table_plan: {rows: [AGE]}"))
  expect_false(any(grepl("not both", tfl_output_name_check(one)$message)))
  bad <- tfl_output_name_check(ak_table("      name: 2bad name"))
  expect_true(any(bad$level == "error" & grepl("not an R name", bad$message)))
  dup <- tfl_output_name_check(ak_table("      name: same", "      name: same"))
  expect_true(any(dup$level == "error" & grepl("names 2 objects", dup$message)))
  # through tfl_check_spec()-style rows too: the same function
  expect_true(any(tfl_output_name_check(both)$part == "analysis"))
})

ak_code <- function(d, a) {
  f <- file.path(d, "spec", "outputs", "T-14-1-1.yml")
  o0 <- tfl_read_output(f)
  on.exit(tfl_write_output(o0, f), add = TRUE)
  o <- o0
  if (!is.null(a)) o$ard$analyses[[length(o$ard$analyses) + 1L]] <- a
  tfl_write_output(o, f)
  st <- tfl_read_study_spec(d)
  tryCatch(paste(suppressWarnings(suppressMessages(
    tfl_ard_code(st, output_id = "T-14-1-1", save = FALSE))), collapse = "\n"),
    error = function(e) paste("ERROR:", conditionMessage(e)))
}

test_that("the ARD program writes `name:` / `from:` (B-5); an item without them is unchanged", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- tempfile("tflspec-ak")
  dir.create(root)
  d <- as_study(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  base <- list(id = "WALK", method = "cards::ard_summary", data = "adsl_sub", by = "TRT01A",
               variables = "AGE")
  plain <- ak_code(d, base)
  expect_false(startsWith(plain, "ERROR:"))
  # the same text twice: nothing changed by the reader
  expect_identical(ak_code(d, base), plain)
  nm <- ak_code(d, c(base, list(name = "walk_ard")))
  expect_match(nm, "\nwalk_ard <- adsl_sub |>\n  ard_summary(", fixed = TRUE)
  fr <- ak_code(d, c(base[setdiff(names(base), "data")], list(from = "adsl_sub")))
  expect_match(fr, "\n.ard_walk <- adsl_sub |>\n  ard_summary(", fixed = TRUE)
})
