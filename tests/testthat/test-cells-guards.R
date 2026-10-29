# plan_cells: guards under one row's label (digits by a key's value), and a
# row's label as an entry of its own refused

test_that("guards under a row's label are written and run as formulas", {
  it <- tfl_item("plan_cells", list(continuous = list(
    n = "{N:.0f}",
    `Mean (SD)` = list(tfl_r("PARAMCD == \"BILI\" ~ \"{mean:.2f} ({sd:.3f})\""), "{mean:.1f} ({sd:.2f})"))))
  expect_identical(nrow(tfl_schema_check(it)), 0L)
  o <- tfl_output("T", "table", data = list(source = "ard"),
                  content = list(tfl_item("table_plan", list(cols = "ARM", rows = list(param = "PARAMCD"))), it))
  code <- paste(tfl_content_code(o), collapse = "\n")
  expect_match(code, "PARAMCD == \"BILI\" ~ \"{mean:.2f} ({sd:.3f})\"", fixed = TRUE)
  expect_false(grepl("structure(", code, fixed = TRUE))
  skip_if_not_installed("rtfreporter")
  ard <- expand.grid(ARM = c("A", "B"), PARAMCD = c("ALT", "BILI"), stringsAsFactors = FALSE)
  ard <- do.call(rbind, lapply(seq_len(nrow(ard)), function(i) data.frame(
    ard[i, ], variable = "AVAL", stat_name = c("N", "mean", "sd"), stat = c(10, 1.23456, 2.34567),
    .kind = "continuous", .label = "AVAL", stringsAsFactors = FALSE, row.names = NULL)))
  tb <- as.data.frame(rtfreporter::plan_apply(suppressMessages(tfl_content_run(o, ard)), "table"))
  ms <- tb[tb$label == "Mean (SD)", ]
  expect_identical(ms$A[ms$param == "ALT"], "1.2 (2.35)")
  expect_identical(ms$A[ms$param == "BILI"], "1.23 (2.346)")
})

test_that("a row's label as a plan_cells entry of its own is refused, with the form to use", {
  it <- tfl_item("plan_cells", list(`Mean (SD)` = list(tfl_r("PARAMCD == \"BILI\" ~ \"{mean:.2f}\""), "{mean:.1f}")))
  p <- tfl_schema_check(it)
  expect_identical(p$level, "error")
  expect_match(p$message, "continuous: {\"Mean (SD)\": ...}", fixed = TRUE)
  # a variable's name, a kind: as before
  expect_identical(nrow(tfl_schema_check(tfl_item("plan_cells", list(AGE = "{mean:.1f}", categorical = "{n}")))), 0L)
})
