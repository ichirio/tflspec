labelled <- function(cd) {
  is.list(cd) && is.character(cd$label_en) && !is.na(cd$label_en) &&
    is.character(cd$label_ja) && !is.na(cd$label_ja)
}

test_that("every table argument has labelled columns, a map its key and value", {
  check <- function(d, where) {
    for (i in which(d$input == "table")) {
      cols <- d$columns[[i]]
      info <- paste0(where, ": ", d$name[i])
      expect_false(is.null(cols), info = info)
      if (identical(cols, "params")) next
      if (startsWith(d$type[i], "map<")) expect_setequal(names(cols), c("key", "value"))
      for (k in names(cols)[nzchar(names(cols))]) {
        if (is.list(cols[[k]])) expect_true(labelled(cols[[k]]), info = paste(info, k))
      }
    }
  }
  # (every schema function: a table the definition writes no columns for
  # gets them from its type, the field contract section 4; step<content>
  # alone has none, its items are cards)
  for (fn in names(tflspec:::.schema_all(figure = TRUE))) {
    d <- tfl_arg_defs(fn = fn)
    check(d[d$type != "step<content>", , drop = FALSE], fn)
  }
  dir <- system.file("series", package = "tflspec")
  for (s in list.dirs(dir, recursive = FALSE, full.names = FALSE)) {
    for (f in list.files(file.path(dir, s, "templates"), pattern = "[.]yml$")) {
      tpl <- paste0(s, "/", sub("[.]yml$", "", f))
      check(tfl_arg_defs(template = tpl), tpl)
    }
  }
})

test_that("stack and combine give their headers and stack its new row", {
  d <- tfl_arg_defs(fn = "normalize_ard")
  st <- d[d$name == "stack", ]
  expect_identical(st$columns[[1]]$key$label_en, "Name")
  expect_identical(st$columns[[1]]$value$label_en, "Layers")
  expect_true(labelled(st$columns[[1]]$value))
  expect_identical(st$new_row[[1]], list(key = "subgroup"))
  cb <- d[d$name == "combine", ]
  expect_identical(cb$columns[[1]]$value$label_en, "Variables")
  expect_null(cb$new_row[[1]])
})

test_that("a map whose value is a map has its own columns", {
  d <- tfl_arg_defs(template = "tflspec_v1/tflspec_shift")
  vl <- d$columns[[which(d$name == "value_labels")]]
  expect_identical(vl$value$columns$key$label_en, "Code")
  expect_identical(vl$value$columns$value$label_en, "Label")
  expect_true(labelled(vl$value$columns$key))
  d <- tfl_arg_defs(template = "tflspec_v1/tflspec_summary")
  nest <- d$columns[[which(d$name == "nest")]]
  expect_identical(nest$key$label_en, "Nested variable")
  expect_identical(nest$value$columns$key$label_en, "Parent variable")
  expect_identical(nest$value$columns$value$label_en, "Level")
})

test_that("new_row names a column of the table, else it stops", {
  cols <- list(key = list(label = "Name"), value = list(label = "Layers"))
  expect_identical(tflspec:::.arg_new_row(list(key = "a"), cols, "x"), list(key = "a"))
  expect_null(tflspec:::.arg_new_row(NULL, cols, "x"))
  expect_error(tflspec:::.arg_new_row(list(nope = "a"), cols, "x"),
               "new_row column `nope` is not one of the table's columns [(]key, value[)]")
  expect_error(tflspec:::.arg_new_row("a", cols, "x"), "new_row is a map")
})

test_that("a table without columns: gets them from its type (contract section 4)", {
  w <- tflspec:::.arg_columns(tflspec:::.arg_type_columns("vec<column, number>|list<number>"),
                              NULL, "x")
  expect_identical(names(w), c("key", "value"))
  expect_identical(c(w$key$label_en, w$key$label_ja), c("Column", "\u5217"))
  expect_identical(c(w$key$type, w$key$input), c("column", "pick"))
  expect_identical(c(w$value$type, w$value$input), c("number", "text"))
  expect_identical(w$value$label_en, "Value")
  # rows<..>: a column a field, in order
  h <- tfl_arg_defs(fn = "plan_col_header")
  hc <- h$columns[[which(h$name == "header")]]
  expect_identical(names(hc)[1:3], c("line", "cols", "span"))
  # a map whose value is a map: its own key and value
  cols <- tflspec:::.arg_type_columns("map<string, map<stat, string>>")
  expect_identical(cols$value$columns$key$label$en, "Statistic")
  # a list of maps: the map's columns; no table member: none
  expect_identical(names(tflspec:::.arg_type_columns("list<map<string, string>>")), c("key", "value"))
  expect_null(tflspec:::.arg_type_columns("step<content>"))
  expect_null(tflspec:::.arg_type_columns("list<column>"))
  # written columns: win
  d <- tfl_arg_defs(fn = "plan_columns")
  expect_identical(names(d$columns[[which(d$name == "widths")]]), c("column", "width"))
  n <- tfl_arg_defs(fn = "normalize_ard")
  expect_identical(n$columns[[which(n$name == "stack")]]$value$label_en, "Layers")
})
