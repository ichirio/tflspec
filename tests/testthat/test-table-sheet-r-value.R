# A report's item with an R value (a call or an expression: plan_style(border
# = rtf_border(...)), written in the YAML as `!r` or {fn:, args:}) on its way
# to the sheets the app's preview reads (tfl_sheets_from_spec()): no cell
# holds it as text -- it went back as the string "rtf_border(...)", and
# rtfreporter stopped ("`border` must be ...").  The item goes whole to the
# tables row's content_more, and tfl_table_plan() runs it as the program does.

tsr_head <- tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable")))
tsr_border <- "rtf_border(top = \"single\", bottom = \"single\", inside_v = \"single\")"

tsr_data <- function() {
  adsl <- cards::ADSL[cards::ADSL$ARM %in% c("Placebo", "Xanomeline High Dose"), ]
  suppressMessages(rtfreporter::normalize_ard(cards::ard_stack(
    adsl, .by = "ARM",
    cards::ard_continuous(variables = "AGE",
                          statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd"))),
    cards::ard_categorical(variables = "SEX"),
    .overall = FALSE, .missing = FALSE, .attributes = TRUE, .total_n = FALSE)))
}

tsr_study <- function(content, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  write_study_yml(d, c("study_id: T-RV", "title: trv", "settings:", "  rounding: sas",
               "  output_path: output/tfl", "  program_dir: programs/tfl",
               "  file: '{output_id}.rtf'", "keys:", "  id: USUBJID"))
  writeLines(c("datasets:", "  - dataset: ADSL", "    level: ADaM", "    path: adam/adsl.xpt",
               "populations:", "  - id: SAF", "    dataset: ADSL",
               "    where: !r SAFFL == \"Y\"", "    label: Safety Population"),
             file.path(d, "spec", "data.yml"))
  y <- list(content = tflspec:::.items_to_list(list(tfl_item("normalize_ard", list()))))
  writeLines(yaml::as.yaml(y, indent.mapping.sequence = TRUE, handlers = tflspec:::.yaml_out_handlers()),
             file.path(d, "spec", "defaults.yml"))
  o <- tfl_output("T-1", "table", label = "One", common = list(population = "SAF"), extends = "defaults",
                  data = list(source = "ard", object = "ard"), content = content)
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-1.yml"))
  d
}

# the table as the app's preview makes it (tflplanner's preview_pages()):
# the YAML as sheets, the table spec of them, its plan
tsr_spec <- function(d) {
  sh <- tfl_sheets_from_yaml(d)
  keep <- intersect(names(sh$table), c("study", names(tflspec:::.ard_spec_schema())))
  list(sh = sh, sp = tfl_table_spec(sh$table[keep]))
}
tsr_pages <- function(plan) {
  suppressMessages(rtfreporter::plan_apply(rtfreporter::plan_cells(plan, notes = FALSE)))
}

test_that("the bridge puts no R value on a sheet: the item is not converted, the sheets as before it", {
  for (b in list(tfl_r(tsr_border),
                 tfl_call("rtf_border", list(top = "single", inside_v = "single")))) {
    sp <- tflspec:::.plan_sheets_from_content(
      list(tsr_head, tfl_item("plan_style", list(border = b, align_count_pct = TRUE))), "T-1")
    expect_identical(attr(sp, "not_converted_at"), 2L)
    expect_match(attr(sp, "not_converted"), "plan_style: an R value", fixed = TRUE, all = FALSE)
    expect_true(all(is.na(sp$style$border)))
  }
  # (a plain word stays on the style sheet, as before)
  sp <- tflspec:::.plan_sheets_from_content(
    list(tsr_head, tfl_item("plan_style", list(border = "none"))), "T-1")
  expect_identical(attr(sp, "not_converted_at"), integer())
  expect_identical(sp$style$border, "none")
})

test_that("plan_style(border = !r rtf_border(...)): whole in content_more, and the preview draws it", {
  skip_if_not_installed("cards")
  st <- tfl_item("plan_style", list(border = tfl_r(tsr_border), align_count_pct = TRUE))
  d <- tsr_study(list(tsr_head, st))
  x <- tsr_spec(d)
  tb <- x$sh$table$tables
  expect_match(tb$content_more[tb$output_id %in% "T-1"], "- plan_style:", fixed = TRUE)
  s <- x$sh$table$style
  expect_true(!any(s$output_id %in% "T-1") || all(is.na(s$border[s$output_id %in% "T-1"])))
  expect_false(any(x$sh$not_converted$output %in% "T-1"))
  # the preview's table: no error, the border the call makes -- the same
  # pages as the plan with the call piped in by hand
  data <- tsr_data()
  got <- tsr_pages(tfl_table_plan(data, x$sp, "T-1"))
  ref <- tsr_spec(tsr_study(list(tsr_head)))$sp
  want <- tsr_pages(rtfreporter::plan_style(
    tfl_table_plan(data, ref, "T-1"),
    border = rtfreporter::rtf_border(top = "single", bottom = "single", inside_v = "single"),
    align_count_pct = TRUE))
  expect_equal(got, want)
  # (and the program writes the call, not a string)
  code <- tfl_table_code(x$sp, "T-1")
  expect_true(any(grepl("border = rtf_border(top = \"single\"", code, fixed = TRUE)))
  expect_false(any(grepl("border = \"rtf_border", code, fixed = TRUE)))
})

test_that("any verb's R value runs in the preview as in the program (plan_paginate_rows(max_rows = !r))", {
  skip_if_not_installed("cards")
  pr <- tfl_item("plan_paginate_rows", list(max_rows = tfl_r("2L + 2L")))
  x <- tsr_spec(tsr_study(list(tsr_head, pr)))
  tb <- x$sh$table$tables
  expect_match(tb$content_more[tb$output_id %in% "T-1"], "- plan_paginate_rows:", fixed = TRUE)
  data <- tsr_data()
  got <- tsr_pages(tfl_table_plan(data, x$sp, "T-1"))
  ref <- tsr_spec(tsr_study(list(tsr_head)))$sp
  want <- tsr_pages(rtfreporter::plan_paginate_rows(tfl_table_plan(data, ref, "T-1"), max_rows = 4L))
  expect_equal(got, want)
  # (and it says something: the table unpaginated is another)
  expect_false(isTRUE(all.equal(got, tsr_pages(tfl_table_plan(data, ref, "T-1")))))
})

test_that("a step's R is evaluated with rtfreporter's functions in reach, as the program attaches it", {
  e <- tflspec:::.spec_run_env()
  expect_true(exists("rtf_border", envir = e))
  expect_true(exists("tfl_r", envir = e))
  expect_identical(tflspec:::.spec_run_env(), e)
  b <- tflspec:::.spec_apply(rtfreporter::rtf_border, list(top = tflspec:::.spec_expr("\"single\"")))
  expect_s3_class(b, "rtf_border")
})
