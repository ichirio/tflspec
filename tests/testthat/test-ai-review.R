
test_that("a YAML report: its items against the schema, its R code where the spec has a form", {
  f <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("id: T-1", "kind: table", "extends: none", "content:",
               "  - table_plan: {cols: ARM, rows: {group: variable}}",
               "  - plan_stub: {nmae: Characteristic}",
               "  - plan_labels: {AGE: !r 'paste(\"Age\")'}",
               "  - plan_filter: {where: !r 'n > 0'}",
               "  - plan_levels: {ARM: !r 'c(\"A\", \"B\")'}"), f)
  r <- tfl_ai_review(f, write = FALSE)
  expect_true(any(r$rule == "K17" & grepl("nmae", r$message, fixed = TRUE)))
  k18 <- r[r$rule == "K18", , drop = FALSE]
  expect_true(nrow(k18) >= 1L)
  expect_false(any(grepl("where", k18$field, fixed = TRUE)))
  expect_true(all(r$output_id[r$rule %in% c("K16", "K17", "K18")] == "T-1"))
})
