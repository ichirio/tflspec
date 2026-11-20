labelled <- function(cd) {
  is.list(cd) && is.character(cd$label_en) && !is.na(cd$label_en) &&
    is.character(cd$label_ja) && !is.na(cd$label_ja)
}

test_that("every table argument has columns, a map its labelled key and value", {
  check <- function(d, where) {
    for (i in which(d$input == "table")) {
      cols <- d$columns[[i]]
      info <- paste0(where, ": ", d$name[i])
      expect_false(is.null(cols), info = info)
      if (identical(cols, "params") || !startsWith(d$type[i], "map<")) next
      expect_setequal(names(cols), c("key", "value"))
      for (k in names(cols)) expect_true(labelled(cols[[k]]), info = paste(info, k))
    }
  }
  for (fn in c("normalize_ard", "plan_columns")) check(tfl_arg_defs(fn = fn), fn)
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
})

test_that("new_row names a column of the table, else it stops", {
  cols <- list(key = list(label = "Name"), value = list(label = "Layers"))
  expect_identical(tflspec:::.arg_new_row(list(key = "a"), cols, "x"), list(key = "a"))
  expect_null(tflspec:::.arg_new_row(NULL, cols, "x"))
  expect_error(tflspec:::.arg_new_row(list(nope = "a"), cols, "x"),
               "new_row column `nope` is not one of the table's columns [(]key, value[)]")
  expect_error(tflspec:::.arg_new_row("a", cols, "x"), "new_row is a map")
})
