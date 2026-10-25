# The person's corrections for the agent's next round, in one file
# (ai/corrections.md): from their list of reports and the questions answered

test_that("tfl_ai_corrections(): add, drop, change, note and the answered questions; written again, kept", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "ai"))
  e <- data.frame(
    sheet = c("t.15.1.5.1", "t.14.1.1", NA, NA, NA),
    toc_id = c("Table 15.1.5.1", NA, NA, NA, NA),
    output_id = c("t.15.1.5.1.b", "t.14.1.3", "t.14.2.1", "t.14.3.1", "t.14.4.1"),
    title = c("Demographics - Phase 2 Part", NA, "Response", NA, NA),
    population = c("SAF", NA, "SAF", NA, NA), split = NA,
    action = c("add", "drop", "change", NA, NA),
    was = c(NA, NA, "population: FAS", NA, NA),
    reason = c(NA, "a close-number guess", NA, NA, NA),
    note = c("same layout as .a", NA, NA, "Sort the SOCs by the Total column", NA))
  tfl_ai_expected_write(d, e)
  writeLines(c("# Questions for the person", "",
               "## Q1. What is the baseline?", "- Reports: t.14.2.1, t.14.2.2",
               "- Status: open", "- Answer: the last value on or before day 1", "",
               "## Q2. Which visits?", "- Reports: t.14.3.1", "- Status: open", "- Answer:", ""),
             file.path(d, "ai", "questions.md"))
  new <- tfl_ai_corrections(d)
  expect_identical(new, c(
    "## C1. add t.15.1.5.1.b (sheet t.15.1.5.1, TOC Table 15.1.5.1)",
    "## C2. drop t.14.1.3 (sheet t.14.1.1)",
    "## C3. change t.14.2.1: population FAS -> SAF",
    "## C4. note t.14.3.1",
    "## Q1 (answered). Reports: t.14.2.1, t.14.2.2"))
  f <- file.path(d, "ai", "corrections.md")
  x <- readLines(f, encoding = "UTF-8")
  expect_true("- Note: same layout as .a" %in% x)
  expect_true("- Reason: a close-number guess" %in% x)
  expect_true("- Answer: the last value on or before day 1" %in% x)
  expect_identical(sum(x == "- Status: open"), 5L)
  # the agent did one; the person adds a correction: the rest kept
  x[match("- Status: open", x)] <- "- Status: done"
  writeLines(x, f)
  e$action[5L] <- "drop"
  e$reason[5L] <- "not in this study"
  tfl_ai_expected_write(d, e, overwrite = TRUE)
  expect_identical(tfl_ai_corrections(d), "## C5. drop t.14.4.1")
  y <- readLines(f, encoding = "UTF-8")
  expect_identical(sum(y == "- Status: done"), 1L)
  expect_identical(length(grep("^## C1[.]", y)), 1L)
  # nothing new: nothing written
  expect_identical(tfl_ai_corrections(d), character())
})
