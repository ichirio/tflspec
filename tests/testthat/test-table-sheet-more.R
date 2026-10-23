# A table YAML -> sheets -> YAML: an item any of whose arguments the sheets
# cannot say goes whole, as YAML, into the tables row's content_more (not
# into the other sheets as well), and comes back -- nothing lost (#448)

tsm_study <- function(extra = list(), defaults_extra = list(), env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  file.copy(list.files(src, full.names = TRUE), d, recursive = TRUE)
  o <- tfl_read_outputs(d)[["T-14-1-1"]]
  # (the items the sheets say whole, and stay as they are)
  o$content <- c(list(
    tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable"))),
    tfl_item("plan_cells", list(continuous = list(n = "{N}", `Mean (SD)` = "{mean} ({sd})"))),
    tfl_item("plan_paginate_rows", list(max_rows = 30L, split = "group_safe"))), extra)
  tfl_write_output(o, file.path(d, "outputs", "T-14-1-1.yml"))
  if (length(defaults_extra)) {
    f <- file.path(d, "defaults.yml")
    y <- yaml::read_yaml(f, handlers = tflspec:::.yaml_handlers())
    y$content <- c(y$content, tflspec:::.items_to_list(defaults_extra))
    writeLines(yaml::as.yaml(y, indent.mapping.sequence = TRUE,
                             handlers = tflspec:::.yaml_out_handlers()), f)
  }
  d
}

tsm_key <- function(items) {
  vapply(items, function(it) paste(it$fn, paste(deparse(it$args[order(names(it$args))]), collapse = "")), "")
}

tsm_back <- function(sh) {
  to <- withr::local_tempdir(.local_envir = parent.frame())
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
  to
}

tsm_unsaid <- list(
  tfl_item("plan_titles", list(titles = list("Safety population"))),
  tfl_item("plan_after", list(steps = tfl_r("function(x) x"))),
  tfl_item("plan_digits", list(continuous = list(N = 0L, mean = 1L))),
  tfl_item("plan_style", list(border_body = tfl_call("rtf_border", list(top = "double")))),
  tfl_item("plan_columns", list(widths = list(3, 1, 1))),
  tfl_item("plan_hide", list(cols = list("A"))),
  tfl_item("plan_hide", list(cols = list("B"))))

test_that("the items the sheets cannot say: whole in content_more, and back", {
  d <- tsm_study(tsm_unsaid)
  sh <- tfl_sheets_from_yaml(d)
  tb <- sh$table$tables
  cm <- tb$content_more[tb$output_id %in% "T-14-1-1"]
  for (fn in c("plan_titles", "plan_after", "plan_digits", "plan_style", "plan_columns")) {
    expect_match(cm, paste0("- ", fn, ":"), fixed = TRUE)
  }
  # the first plan_hide said by the columns sheet, the second kept whole
  expect_match(cm, "cols: B", fixed = TRUE)
  cl <- sh$table$columns
  expect_identical(cl$hide[cl$output_id %in% "T-14-1-1" & cl$column %in% "A"], "TRUE")
  expect_false(any(cl$column %in% "B"))
  # never both: plan_style's border and the widths are not on the other sheets
  st <- sh$table$style
  expect_true(!any(st$output_id %in% "T-14-1-1") ||
                all(is.na(st$border_body[st$output_id %in% "T-14-1-1"])))
  expect_false(any(nzchar(cl$rel_width[cl$output_id %in% "T-14-1-1"] %||% character()) &
                     !is.na(cl$rel_width[cl$output_id %in% "T-14-1-1"])))
  # nothing lost: no note for them
  expect_false(any(sh$not_converted$output %in% "T-14-1-1"))
  back <- tfl_read_outputs(tsm_back(sh))[["T-14-1-1"]]
  expect_setequal(tsm_key(back$content), tsm_key(tfl_read_outputs(d)[["T-14-1-1"]]$content))
  # the content_more items by their verbs' rank: table_plan first
  expect_identical(back$content[[1L]]$fn, "table_plan")
})

test_that("a table_plan() the sheets cannot say goes whole, and is the only one back", {
  d <- tsm_study()
  o <- tfl_read_outputs(d)[["T-14-1-1"]]
  o$content[[1L]] <- tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable"),
                                                 stat = "stat"))
  tfl_write_output(o, file.path(d, "outputs", "T-14-1-1.yml"))
  sh <- tfl_sheets_from_yaml(d)
  tb <- sh$table$tables
  i <- tb$output_id %in% "T-14-1-1"
  expect_match(tb$content_more[i], "- table_plan:", fixed = TRUE)
  expect_true(is.na(tb$cols[i]))
  back <- tfl_read_outputs(tsm_back(sh))[["T-14-1-1"]]
  expect_identical(sum(vapply(back$content, function(it) identical(it$fn, "table_plan"), NA)), 1L)
  expect_setequal(tsm_key(back$content), tsm_key(tfl_read_outputs(d)[["T-14-1-1"]]$content))
})

test_that("the defaults' items the sheets cannot say: on the blank row, and back", {
  d <- tsm_study(defaults_extra = list(tfl_item("plan_footnotes", list(footnotes = list("Source: ADSL")))))
  sh <- tfl_sheets_from_yaml(d)
  tb <- sh$table$tables
  expect_match(tb$content_more[is.na(tb$output_id)], "- plan_footnotes:", fixed = TRUE)
  to <- tsm_back(sh)
  back <- tfl_read_study_spec(to)$defaults$content
  fn <- tfl_read_study_spec(d)$defaults$content
  fn <- Filter(function(it) identical(it$fn, "plan_footnotes"), fn)
  expect_true(all(tsm_key(fn) %in% tsm_key(back)))
  expect_length(fn, 1L)
})

test_that("a table the sheets say whole: no content_more, as before", {
  d <- tsm_study()
  sh <- tfl_sheets_from_yaml(d)
  tb <- sh$table$tables
  expect_true(is.null(tb$content_more) || all(is.na(tb$content_more)))
  back <- tfl_read_outputs(tsm_back(sh))[["T-14-1-1"]]
  expect_identical(tsm_key(back$content), tsm_key(tfl_read_outputs(d)[["T-14-1-1"]]$content))
})
