# tfl_check_spec(): every `!r` value of a report (its `ard:` aside, which
# the ARD check reads by its own fields) is read as R once; one that does
# not read is an error row with its place in the SPEC and the parser's
# message

test_that("an `!r` value that does not read as R: an error row, its place and the parser's message", {
  skip_if_not_installed("haven")
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "RP-1")
  one <- function(id, item) {
    tfl_write_output(tfl_output(id, "table", common = list(population = "SAF"),
      template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")),
      content = list(item)), file.path(d, "spec", "outputs", paste0(id, ".yml")))
  }
  # a slot of R (plan_filter's condition), and a guard before a cell template
  one("T-F", tfl_item("plan_filter", list(where = tfl_r("p >="))))
  one("T-G", tfl_item("plan_cells", list(categorical = list(tfl_r("n == 0 ~"), "{n} ({p:.1f%})"))))
  p <- tfl_check_spec(d)
  shown <- paste(utils::capture.output(print(p[, c("output_id", "part", "item", "level", "message")])),
                 collapse = "\n")
  r <- p[grepl("does not read as R", p$message, fixed = TRUE), , drop = FALSE]
  expect_identical(sort(r$output_id), c("T-F", "T-G"), info = shown)
  expect_true(all(r$level == "error"), info = shown)
  expect_identical(r$item[r$output_id == "T-F"], "content$plan_filter$args$where", info = shown)
  expect_identical(r$item[r$output_id == "T-G"], "content$plan_cells$args$categorical$1", info = shown)
  expect_true(all(grepl("unexpected end of input", r$message, fixed = TRUE)), info = shown)
  expect_match(r$message[r$output_id == "T-F"], "p >=", fixed = TRUE)

  # R that reads: no such row
  one("T-F", tfl_item("plan_filter", list(where = tfl_r("p >= 0.05"))))
  one("T-G", tfl_item("plan_cells", list(categorical = list(tfl_r("n == 0 ~ '0'"), "{n} ({p:.1f%})"))))
  p <- tfl_check_spec(d)
  expect_false(any(grepl("does not read as R", p$message, fixed = TRUE)))
})

# The walker itself (.r_walk(), tflspec#986): in-memory reports, the parser
# by the kind of slot the schema says, each `!r` once

test_that(".r_walk() reads each `!r` by its slot: expression, guard, formula, call", {
  walk <- function(...) {
    outs <- list(`T-1` = tfl_output("T-1", "table", content = list(...)))
    .r_walk(NULL, outs, "T-1")
  }
  # a plain expression (plan_filter's condition)
  r <- walk(tfl_item("plan_filter", list(where = tfl_r("p >="))))
  expect_identical(r$item, "content$plan_filter$args$where")
  expect_identical(r$part, "content")
  expect_identical(r$level, "error")
  expect_identical(r$output_id, "T-1")
  expect_match(r$message, "unexpected end of input", fixed = TRUE)
  # a guard before a cell template: its place is the list's index
  r <- walk(tfl_item("plan_cells", list(categorical = list(tfl_r("n == 0 ~"), "{n}"))))
  expect_identical(r$item, "content$plan_cells$args$categorical$1")
  # (a one-sided `~ text` is no guard; the same text is a formula in a formula's slot)
  r <- walk(tfl_item("plan_cells", list(categorical = list(tfl_r("~ 'x'"), "{n}"))))
  expect_identical(r$item, "content$plan_cells$args$categorical$1")
  expect_match(r$message, "both sides", fixed = TRUE)
  expect_identical(nrow(walk(tfl_item("plan_cell_style", list(where = tfl_r("~ p > 0.5"))))), 0L)
  # a formula (plan_cell_style's rows)
  r <- walk(tfl_item("plan_cell_style", list(where = tfl_r("p >"))))
  expect_identical(r$item, "content$plan_cell_style$args$where")
  # a call (plan_style's border)
  r <- walk(tfl_item("plan_style", list(border_body = tfl_r("rtf_border(inside_h = TRUE"))))
  expect_identical(r$item, "content$plan_style$args$border_body")
})

test_that(".r_walk(): valid R, blank text and plain strings give no row; a template's defaults are not walked", {
  walk <- function(o) .r_walk(NULL, list(`T-1` = o), "T-1")
  ok <- tfl_output("T-1", "table", content = list(
    tfl_item("plan_filter", list(where = tfl_r("p >= 0.05"))),
    tfl_item("plan_cells", list(categorical = list(tfl_r("n == 0 ~ '0'"), "{n} ({p:.1f%})"))),
    tfl_item("plan_cell_style", list(where = tfl_r("~ p > 0.5"))),
    tfl_item("plan_style", list(border_body = tfl_r("rtf_border(inside_h = TRUE)"))),
    tfl_item("plan_filter", list(where = tfl_r("")))))
  expect_identical(nrow(walk(ok)), 0L)
  # an `!r` that is not in the report as written (a template's own default,
  # which the report does not hold) is not read: walk `o`, not the resolved one
  o <- tfl_output("T-1", "table", template = list(name = "demog", args = list(by = "TRT01A")))
  expect_identical(nrow(walk(o)), 0L)
  # in a template's arguments as written it is read, with its place
  o <- tfl_output("T-1", "table", template = list(name = "demog", args = list(by = tfl_r("TRT01A +"))))
  expect_identical(walk(o)$item, "template$args$by")
})

test_that("a broken `!r` is one row, and the `program` warning that said the same is gone", {
  skip_if_not_installed("haven")
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "RP-2")
  broken <- tfl_item("plan_cells", list(categorical = list(tfl_r("n == 0 ~"), "{n} ({p:.1f%})")))
  # (without the walker's row the writer's stop is what said it)
  expect_error(suppressMessages(tfl_content_code(tfl_output("T-G", "table", content = list(broken)))))
  tfl_write_output(tfl_output("T-G", "table", common = list(population = "SAF"),
    template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")),
    content = list(broken)), file.path(d, "spec", "outputs", "T-G.yml"))
  p <- tfl_check_spec(d)
  shown <- paste(utils::capture.output(print(p[, c("output_id", "part", "item", "level", "message")])),
                 collapse = "\n")
  g <- p[p$output_id %in% "T-G", , drop = FALSE]
  expect_identical(sum(g$item == "content$plan_cells$args$categorical$1"), 1L, info = shown)
  expect_identical(sum(grepl("does not read as R", g$message, fixed = TRUE)), 1L, info = shown)
  expect_identical(sum(g$part == "program"), 0L, info = shown)
  # no row twice: the same (output, part, item, message)
  expect_false(anyDuplicated(g[, c("part", "item", "message")]) > 0L, info = shown)
})
