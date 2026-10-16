# The YAML spec, stage 0: the objects, and a report file read and written

yml_fixture <- function(...) system.file("extdata", "spec-yaml", ..., package = "tflspec")

test_that("the objects check their parts", {
  expect_s3_class(tfl_r("p > 0.5"), "tfl_r")
  expect_identical(attr(tfl_r("x"), "tag"), "!r")
  expect_identical(as.character(tfl_r(c("a", "b"))), "a
b")
  expect_s3_class(tfl_unset(), "tfl_unset")
  cl <- tfl_call("rtf_border", list(top = "single"))
  expect_identical(cl$fn, "rtf_border")
  expect_error(tfl_call("f", list("a")), "named list")
  it <- tfl_item("plan_cells", list(continuous = list(n = "{N}")))
  expect_identical(it$from, "own")
  expect_error(tfl_item("plan_cells", from = "elsewhere"), "pattern")
  out <- tfl_output("T-1", "table", content = list(it))
  expect_s3_class(out, "tfl_output")
  expect_identical(out$extends, "defaults")
  expect_true(is.na(out$label))
  expect_error(tfl_output("T 1", "table"), "letters")
  expect_error(tfl_output("T-1", "table", content = list(1)), "tfl_item")
  expect_output(print(out), "plan_cells\\(continuous\\)")
})

test_that("a report file reads: tags, nested calls, content items", {
  out <- tfl_read_output(yml_fixture("outputs", "T-14-1-1.yml"))
  expect_s3_class(out, "tfl_output")
  expect_identical(out$id, "T-14-1-1")
  expect_identical(out$kind, "table")
  expect_identical(out$population, "SAF")
  expect_identical(out$extends, "defaults")
  fns <- vapply(out$content, `[[`, "", "fn")
  expect_identical(fns, c("table_plan", "plan_labels", "plan_cells", "plan_digits",
                          "plan_paginate_rows", "plan_cell_style"))
  expect_true(all(vapply(out$content, inherits, NA, "tfl_item")))
  expect_true(all(vapply(out$content, `[[`, "", "from") == "own"))
  # the !r tag
  w <- out$content[[6L]]$args$where
  expect_s3_class(w, "tfl_r")
  expect_identical(as.character(w), "p > 0.5")
  # nested maps kept as they are
  expect_identical(out$content[[3L]]$args$continuous[["Mean (SD)"]], "{mean} ({sd})")
  expect_identical(out$content[[4L]]$args$continuous$sd, 2L)    # plan_digits: by statistic
  # data and page
  expect_identical(out$data$source, "ard")
  expect_identical(out$ard$analyses[[2L]]$statistic$AGE[[2L]], "mean")
  expect_identical(out$page$titles[["1"]]$center, "Table 14.1.1")
  expect_identical(out$codelists$TRT01A[[2L]]$label, "Control 100 mg")
})

test_that("the defaults file reads with the same reader's pieces: a nested call", {
  y <- yaml::read_yaml(yml_fixture("defaults.yml"), handlers = tflspec:::.yaml_handlers())
  items <- lapply(y$content, tflspec:::.item_from_list)
  st <- items[[which(vapply(items, `[[`, "", "fn") == "plan_style")]]
  expect_identical(st$fn, "plan_style")
  expect_s3_class(st$args$border_header, "tfl_call")
  expect_identical(st$args$border_header$fn, "rtf_border")
  expect_identical(st$args$border_header$args$top, TRUE)
})

test_that("a report written and read again is the same", {
  out <- tfl_read_output(yml_fixture("outputs", "T-14-1-1.yml"))
  out$content[[6L]]$args$bold <- tfl_unset()
  out$page$rtf_tables <- list(border = tfl_call("rtf_border", list(top = "single")))
  f <- withr::local_tempfile(fileext = ".yml")
  tfl_write_output(out, f)
  txt <- readLines(f)
  expect_true(any(grepl("!r", txt, fixed = TRUE)))
  expect_true(any(grepl("!unset", txt, fixed = TRUE)))
  back <- tfl_read_output(f)
  expect_s3_class(back$content[[6L]]$args$bold, "tfl_unset")
  expect_s3_class(back$page$rtf_tables$border, "tfl_call")
  # the parts compared without the unset (its value is nothing)
  back$content[[6L]]$args$bold <- NULL
  out$content[[6L]]$args$bold <- NULL
  expect_equal(back, out, ignore_attr = FALSE)
})

test_that("a file of several reports gives a list, each over the file's base", {
  f <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(
    "base:",
    "  kind: table",
    "  population: SAF",
    "  content:",
    "    - table_plan: {cols: TRT01A}",
    "outputs:",
    "  - id: T-A",
    "    label: A",
    "  - id: T-B",
    "    label: B",
    "    population: ITT"), f)
  outs <- tfl_read_output(f)
  expect_length(outs, 2L)
  expect_identical(vapply(outs, `[[`, "", "id"), c("T-A", "T-B"))
  expect_identical(outs[[1L]]$population, "SAF")
  expect_identical(outs[[2L]]$population, "ITT")
  expect_identical(outs[[2L]]$content[[1L]]$fn, "table_plan")
})

test_that("n, N, y, yes, no, off stay text as keys and values; only true / false are logical", {
  f <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("id: T-1", "kind: table", "content:",
               "  - plan_cells: {continuous: {n: '{N}', N: '{N}'}}",
               "  - plan_digits: {AGE: {n: 0, N: 0, mean: 1}}",
               "  - plan_sort: {keys: [n, N], stat: n}",
               "  - plan_total: {label: Y, on: yes, off: no, shown: true, hidden: false}"), f)
  o <- tfl_read_output(f)
  expect_identical(names(o$content[[1L]]$args$continuous), c("n", "N"))
  expect_identical(names(o$content[[2L]]$args$AGE), c("n", "N", "mean"))
  expect_identical(o$content[[3L]]$args$keys, c("n", "N"))
  expect_identical(o$content[[3L]]$args$stat, "n")
  a <- o$content[[4L]]$args
  expect_identical(a$label, "Y")
  expect_identical(a[["on"]], "yes")
  expect_identical(a[["off"]], "no")
  expect_identical(a$shown, TRUE)
  expect_identical(a$hidden, FALSE)
  # and back
  g <- withr::local_tempfile(fileext = ".yml")
  tfl_write_output(o, g)
  expect_equal(tfl_read_output(g)$content, o$content)
})

test_that("a sequence holding an !r or a call stays a list; one of scalars is a vector", {
  f <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("id: T-1", "kind: table", "content:",
               "  - plan_cells: {prob: [!r 'is.na(estimate) ~ \"NE\"', '{estimate}'], vars: [AGE, SEX], n: [1, 2]}",
               "  - plan_style: {borders: [{fn: rtf_border, args: {top: true}}, {fn: rtf_border, args: {bottom: true}}]}"), f)
  o <- tfl_read_output(f)
  a <- o$content[[1L]]$args
  expect_type(a$prob, "list")
  expect_s3_class(a$prob[[1L]], "tfl_r")
  expect_identical(a$prob[[2L]], "{estimate}")
  expect_identical(a$vars, c("AGE", "SEX"))
  expect_identical(a$n, c(1L, 2L))
  b <- o$content[[2L]]$args$borders
  expect_type(b, "list")
  expect_s3_class(b[[2L]], "tfl_call")
  # and back
  g <- withr::local_tempfile(fileext = ".yml")
  tfl_write_output(o, g)
  expect_equal(tfl_read_output(g)$content, o$content)
})

test_that("analyses under data: (the old place) stop the read, naming ard:", {
  f <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("id: T-1", "kind: table", "data:", "  source: ard", "  analyses:",
               "    - id: A", "      method: categorical"), f)
  expect_error(tfl_read_output(f), "ard: section")
})

test_that("an NA argument (table_plan(label = NA)) is written .na and read back NA", {
  o <- tfl_output("T-1", "table", content = list(tfl_item("table_plan",
    list(cols = "TRT", rows = list(group = "variable"), label = NA))))
  f <- withr::local_tempfile(fileext = ".yml")
  tfl_write_output(o, f)
  expect_true(any(grepl("label: .na", readLines(f), fixed = TRUE)))
  r <- tfl_read_output(f)
  expect_identical(r$content[[1L]]$args$label, NA)
  expect_true(any(grepl("label = NA", tfl_content_code(r), fixed = TRUE)))
})
