# tfl_ai_parse() on answers as models write them: the fixtures in
# fixtures/ai/<task>-<case>.md, good and bad, each with its expected
# problems in fixtures/ai/expected.csv (no problem row: none expected).

ai_fixture <- function(name) {
  readLines(test_path("fixtures", "ai", paste0(name, ".md")),
            encoding = "UTF-8", warn = FALSE)
}

ai_parse_fixture <- function(name) {
  tfl_ai_parse(ai_fixture(name), sub("-.*$", "", name))
}

test_that("every fixture gives exactly its expected problems", {
  exp <- utils::read.csv(test_path("fixtures", "ai", "expected.csv"),
                         colClasses = "character", na.strings = "")
  files <- sub("\\.md$", "", list.files(test_path("fixtures", "ai"), "\\.md$"))
  expect_setequal(unique(exp$fixture), files)
  for (f in files) {
    a <- ai_parse_fixture(f)
    want <- exp[exp$fixture == f & !is.na(exp$where), , drop = FALSE]
    got <- a$problems
    expect_identical(nrow(got), nrow(want), label = f)
    if (!nrow(want) || nrow(got) != nrow(want)) next
    expect_identical(got$where, want$where, label = f)
    expect_identical(got$severity, want$severity, label = f)
    for (i in seq_len(nrow(want))) {
      expect_match(got$problem[i], want$problem[i], fixed = TRUE, label = f)
    }
  }
})

test_that("a toc answer becomes rows with tfl_read_toc()'s fields, all text", {
  a <- ai_parse_fixture("toc-yaml-flow")
  expect_s3_class(a, "tfl_ai_answer")
  toc <- a$sheets$toc
  expect_identical(names(toc), tflspec:::.toc_fields)
  expect_true(all(vapply(toc, is.character, NA)))
  expect_identical(toc$output_id, c("T-14-1-1", "T-14-3-1", "F-14-2-1"))
  expect_identical(toc$title[2], "Overview of Adverse Events | Treatment-Emergent")
  expect_identical(toc$footnote[c(1, 3)], c(NA_character_, NA_character_))
  expect_identical(a$assumptions, "The SAP names no footnotes for Figure 14.2.1.")
  expect_identical(a$format, "yaml")
  expect_match(a$raw, "^tflspec_ai:")
  expect_null(a$output_id)
  expect_output(print(a), "toc: 3 row(s)", fixed = TRUE)
})

test_that("block style, JSON and lists read as the flow style does", {
  b <- ai_parse_fixture("toc-yaml-block")$sheets$toc
  expect_identical(b$title[1],
                   "Summary of Demographic and Baseline Characteristics | Overall")
  j <- ai_parse_fixture("toc-json")
  expect_identical(j$format, "json")
  expect_identical(j$sheets$toc$datasets[1], "ADSL")
  expect_identical(j$sheets$toc$title,
                   c("Summary of Demographic and Baseline Characteristics",
                     "Primary Efficacy Analysis"))
  expect_identical(j$assumptions, "The SAP gives no section numbers.")
})

test_that("Japanese text is kept as written", {
  a <- ai_parse_fixture("toc-japanese")
  toc <- a$sheets$toc
  expect_identical(toc$population[1], "安全性解析対象集団")
  expect_identical(strsplit(toc$footnote[1], " | ", fixed = TRUE)[[1L]][2],
                   "年齢は同意取得時。")
  expect_length(a$assumptions, 1L)
})

test_that("the answer is the block with the header, not the first block", {
  a <- ai_parse_fixture("toc-two-blocks")
  expect_identical(a$sheets$toc$output_id, c("T-14-1-1", "F-14-2-1"))
  # no fence at all: the whole text, when it is the answer
  bare <- c("tflspec_ai: {task: toc, version: 1}",
            "toc:", "- {output_id: T-1, title: A}")
  expect_identical(tfl_ai_parse(bare, "toc")$sheets$toc$output_id, "T-1")
})

test_that("nothing is repaired: an unquoted brace is a YAML problem with its line", {
  a <- ai_parse_fixture("figure-unquoted-brace")
  expect_null(a$design)
  expect_match(a$problems$problem, "line 4", fixed = TRUE)
  expect_output(print(a), "1 problem(s)", fixed = TRUE)
})

test_that("a figure answer becomes a design", {
  a <- ai_parse_fixture("figure-yaml")
  expect_s3_class(a$design, "tfl_fig_design")
  expect_identical(a$output_id, "F-14-2-1")
  expect_identical(a$design$template, "km_risk_table")
  expect_identical(vapply(a$design$layers, `[[`, "", "layer"),
                   c("km_curve", "censor_mark", "risk_table"))
  expect_identical(a$design$plot$x_label, "Time (Months)")
  j <- ai_parse_fixture("figure-json")
  expect_identical(j$design$plot$y_max, 1L)
  expect_identical(j$design$stats[[1L]]$by, "TRT01P")
  # JSON's "!r ..." is raw R, as YAML's !r tag
  json <- c(
    "```json",
    "{\"tflspec_ai\": {\"task\": \"figure\", \"version\": 1,",
    "                \"output_id\": \"F\"},",
    " \"plot\": {\"add\": [{\"fn\": \"scale_y_continuous\",",
    "                     \"args\": {\"labels\": \"!r scales::percent\"}}]}}",
    "```"
  )
  r <- tfl_ai_parse(json, "figure")
  expect_true(tflspec:::.is_fig_r(r$design$plot$add[[1L]]$args$labels))
})

test_that("the 0.0.12 shape reads as one figure layer; a note is kept", {
  a <- ai_parse_fixture("figure-old-shape")
  expect_identical(a$design$layers[[1L]]$layer, "figure")
  expect_identical(a$design$layers[[1L]]$type, "forest")
  b <- ai_parse_fixture("figure-not-pieces")
  expect_identical(b$note, "drafted from SAP v2")
  expect_length(b$design$data, 0L)
})

test_that("tfl_ai_parse() checks its arguments", {
  expect_error(tfl_ai_parse(1, "toc"), "as text")
  expect_error(tfl_ai_parse("x", "listing"), "no task")
})
