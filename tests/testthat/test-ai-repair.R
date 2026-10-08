# tfl_ai_repair_prompt(): the problems sent back, with the schema of the
# offending parts only.

ai_repair_answer <- function() {
  tfl_ai_parse(c(
    "```yaml",
    "tflspec_ai: {task: figure, version: 1, output_id: F-14-2-9}",
    "template: km_simple",
    "data:",
    "- {step: read, dataset: ADTTE}",
    "stats:",
    "- {step: survfit, name: fit, time: AVAL, censor: CNSR}",
    "layers:",
    "- {layer: km_curve}",
    "- {layer: censor_mark, colour: red}",
    "assumptions: []",
    "```"
  ), "figure")
}

test_that("the repair prompt names each problem and only the offending schema", {
  a <- ai_repair_answer()
  ctx <- tfl_ai_context("figure", output_id = "F-14-2-1")
  p <- tfl_ai_check(a, ctx)
  r <- tfl_ai_repair_prompt(a, context = ctx)
  expect_s3_class(r, "tfl_ai_text")
  for (i in seq_len(nrow(p))) {
    expect_match(r, paste0("| ", p$where[i], " | ", p$problem[i], " |"), fixed = TRUE)
  }
  expect_match(r, "The schema of censor_mark:", fixed = TRUE)
  expect_match(r, "- `censor_mark`:", fixed = TRUE)
  expect_false(grepl("`km_curve`", r, fixed = TRUE))
  expect_false(grepl("### data", r, fixed = TRUE))
  expect_match(r, "Return the whole block again, corrected", fixed = TRUE)
  # the format's rules only when the block could not be read
  expect_false(grepl("Write the block in YAML", r, fixed = TRUE))
  expect_snapshot(cat(r))
})

test_that("an unreadable block gets the format's rules and no schema", {
  a <- tfl_ai_parse(c(
    "```yaml",
    "tflspec_ai: {task: toc, version: 1}",
    "toc:",
    "- {output_id: T-1, title: Table 1: Demographics}",
    "```"
  ), "toc")
  r <- tfl_ai_repair_prompt(a)
  expect_match(r, "Write the block in YAML", fixed = TRUE)
  expect_false(grepl("The schema of", r, fixed = TRUE))
  expect_snapshot(cat(r))
})

test_that("a toc's problems bring the toc's schema; errors before warnings", {
  a <- tfl_ai_parse(c(
    "```yaml",
    "tflspec_ai: {task: toc, version: 1}",
    "toc:",
    "- {output_id: T-1, type: chart, title: A}",
    "- {output_id: T-2, type: table, title: B, colour: red}",
    "```"
  ), "toc")
  r <- tfl_ai_repair_prompt(a, context = tfl_ai_context("toc"))
  expect_match(r, "The schema of toc:", fixed = TRUE)
  rows <- grep("^\\| (error|warning) ", strsplit(r, "\n")[[1L]], value = TRUE)
  expect_identical(sub("^\\| ([a-z]+) .*$", "\\1", rows), c("error", "warning"))
})

test_that("given problems are sent as they are; none is an error", {
  a <- ai_repair_answer()
  p <- data.frame(where = "plot", problem = "the legend is missing")
  r <- tfl_ai_repair_prompt(a, p)
  expect_match(r, "| error | plot | the legend is missing |", fixed = TRUE)
  expect_match(r, "### plot", fixed = TRUE)
  expect_error(tfl_ai_repair_prompt(a, p[0, ]), "no problems")
  expect_error(tfl_ai_repair_prompt(a, data.frame(x = 1)), "`where` and `problem`")
  expect_error(tfl_ai_repair_prompt(list()), "tfl_ai_answer")
})
