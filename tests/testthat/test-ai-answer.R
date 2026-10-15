# The AI's answer: every spec file of a study in one Markdown text

ans <- function() c(
  "Here are the files.", "",
  "### study.yml", "", "```yaml", "study_id: XYZ-001", "```", "",
  "### spec/outputs/T-1.yml", "", "```yaml", "id: T-1", "kind: table", "extends: none",
  "content:", "  - table_plan: {cols: TRT01A}", "  - plan_cells: {categorical: \"{n} ({p:.1f%})\"}", "```", "",
  "Some text the assistant added.", "",
  "### `spec/_assumptions.yml`", "", "```yml", "- {output_id: T-1, item: SEX, assumption: M / F, question: The code list?}", "```")

test_that("tfl_ai_unpack() writes the answer's files, which read as the study's", {
  d <- withr::local_tempdir()
  out <- tfl_ai_unpack(ans(), d)
  expect_setequal(sub(paste0("^", normalizePath(d, "/"), "/"), "", out),
                  c("study.yml", "spec/outputs/T-1.yml", "spec/_assumptions.yml"))
  o <- tfl_read_output(file.path(d, "spec", "outputs", "T-1.yml"))
  expect_identical(o$id, "T-1")
  expect_identical(vapply(o$content, function(it) it$fn, ""), c("table_plan", "plan_cells"))
  expect_error(tfl_ai_unpack(ans(), d), "already there")
  expect_no_error(tfl_ai_unpack(ans(), d, overwrite = TRUE))
  # from a file too
  f <- withr::local_tempfile(fileext = ".md")
  writeLines(ans(), f)
  d2 <- withr::local_tempdir()
  expect_length(tfl_ai_unpack(f, d2), 3L)
})

test_that("tfl_ai_unpack() writes spec files only, valid YAML, once each", {
  d <- withr::local_tempdir()
  one <- function(path, body = "id: x") c(paste("###", path), "```yaml", body, "```")
  expect_error(tfl_ai_unpack(one("../evil.yml"), d), "not a spec file")
  expect_error(tfl_ai_unpack(one("R/code.yml"), d), "not a spec file")
  expect_error(tfl_ai_unpack(one("C:/x/spec/a.yml"), d), "not a spec file")
  expect_error(tfl_ai_unpack(c(one("spec/a.yml"), one("spec/a.yml")), d), "twice")
  expect_error(tfl_ai_unpack(one("spec/a.yml", "key: [unclosed"), d), "not valid YAML")
  expect_error(tfl_ai_unpack("no files here", d), "holds no file")
  expect_length(list.files(d, recursive = TRUE), 0L)
})

test_that("a folder is written as an answer, and the answer writes the folder back", {
  d <- withr::local_tempdir()
  tfl_ai_unpack(ans(), d)
  md <- tflspec:::.ai_answer_md(d)
  expect_identical(md[1L], "### study.yml")
  expect_true(which(md == "### spec/outputs/T-1.yml") < which(md == "### spec/_assumptions.yml"))
  d2 <- withr::local_tempdir()
  tfl_ai_unpack(md, d2)
  for (f in c("study.yml", "spec/outputs/T-1.yml", "spec/_assumptions.yml")) {
    expect_identical(readLines(file.path(d2, f)), readLines(file.path(d, f)), info = f)
  }
})
