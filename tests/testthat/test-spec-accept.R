# Stage 1's acceptance: the user's example pipeline, written as a spec file's
# content, comes back as its code; and the code, run, makes what
# tfl_content_run() makes from the same items.

accept_yaml <- '
id: T-1
kind: table
label: Demographics
extends: none
content:
  - normalize_ard: {}
  - table_plan: {cols: ARM, rows: {group: variable}}
  - plan_cells:
      continuous: {"Mean (SD)": "{mean:.1f} ({sd:.2f})"}
      categorical: "{n:.0f} ({p:.1f%})"
      notes: false
  - plan_labels: {AGE: "Age (years)", SEX: "Sex, n (%)"}
  - plan_stub: {}
  - plan_col_header:
      values: {n: true}
      header: {fn: rtf_col_header, args: {rows: [["", "{col}\\n(N={n})"]]}}
'

test_that("the user's pipeline is a spec file's content, and comes back as its code", {
  f <- withr::local_tempfile(fileext = ".yml")
  writeLines(accept_yaml, f)
  out <- tfl_read_output(f)
  for (it in out$content) {
    p <- tfl_schema_check(it)
    expect_identical(nrow(p[p$level == "error", ]), 0L, info = it$fn)
  }
  code <- tfl_content_code(out, pipe = "|>")
  expect_identical(code, c(
    "plan <- ard |>",
    "  normalize_ard() |>",
    "  table_plan(cols = \"ARM\", rows = c(group = \"variable\")) |>",
    "  plan_cells(",
    "    continuous = list(\"Mean (SD)\" = \"{mean:.1f} ({sd:.2f})\"),",
    "    categorical = \"{n:.0f} ({p:.1f%})\",",
    "    notes = FALSE",
    "  ) |>",
    "  plan_labels(AGE = \"Age (years)\", SEX = \"Sex, n (%)\") |>",
    "  plan_stub() |>",
    "  plan_col_header(",
    "    values = list(n = TRUE),",
    "    rtf_col_header(c(\"\", \"{col}\\n(N={n})\"))",
    "  )"))
})

test_that("the code, run, is what tfl_content_run() makes", {
  skip_if_not_installed("cards")
  f <- withr::local_tempfile(fileext = ".yml")
  writeLines(accept_yaml, f)
  out <- tfl_read_output(f)
  adsl <- cards::ADSL
  adsl$ARM <- factor(adsl$ARM, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
  ard <- cards::ard_stack(adsl, .by = ARM, cards::ard_summary(variables = AGE),
                          cards::ard_tabulate(variables = SEX))
  env <- new.env()
  env$ard <- ard
  eval(parse(text = c("library(rtfreporter)", tfl_content_code(out, pipe = "|>"))), env)
  direct <- tfl_content_run(out, ard)
  pages <- function(p) suppressMessages(rtfreporter::plan_apply(p, "table"))
  expect_identical(pages(env$plan), pages(direct))
})

test_that("every table case: its code, run, is what its content makes directly", {
  skip_on_cran()
  skip_if_not_installed("cards")
  env <- new.env(parent = globalenv())
  cases <- source(test_path("fixtures", "table-cases.R"), local = env)$value
  pages <- function(p) suppressMessages(rtfreporter::plan_apply(p, "table"))
  n <- 0L
  for (nm in names(cases)) {
    p <- eval(cases[[nm]], env)
    sp <- suppressMessages(tfl_as_table_spec(p, output_id = "T", compare = FALSE))
    out <- tfl_output("T", "table", content = tflspec:::.plan_content_from_sheets(sp, "T"))
    run_env <- new.env()
    run_env$data <- p$data
    eval(parse(text = c("library(rtfreporter)", tfl_content_code(out, data = "data", pipe = "|>"))),
         run_env)
    expect_identical(pages(run_env$plan), pages(tfl_content_run(out, p$data)), label = nm)
    n <- n + 1L
  }
  expect_gt(n, 40L)
})
