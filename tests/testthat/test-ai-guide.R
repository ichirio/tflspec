# The table guide's code, run as the guide writes it: its "Try it on the
# sample" block, and its main blocks (steps 3 and 6) on the bundled example.

guide_blocks <- function(file) {
  x <- readLines(system.file("ai", file, package = "tflspec"), encoding = "UTF-8")
  open <- which(x == "```r")
  lapply(open, function(i) {
    end <- i + which(x[(i + 1L):length(x)] == "```")[1L]
    list(heading = x[max(grep("^## ", x[seq_len(i)]))], code = x[(i + 1L):(end - 1L)])
  })
}

guide_block <- function(file, heading) {
  b <- guide_blocks(file)
  b[vapply(b, function(z) grepl(heading, z$heading, fixed = TRUE), NA)]
}

for (g in c("tflspec-ai-table-guide.md", "tflspec-ai-table-guide-ja.md")) {
  test_that(sprintf("%s: the sample block runs on a clean folder, every report ok", g), {
    skip_on_cran()
    skip_if_not_installed("haven")
    skip_if_not_installed("cards")
    td <- withr::local_tempdir()
    sample <- guide_block(g, if (grepl("-ja", g)) "見本で試す" else
                            "Try it on the sample")
    expect_length(sample, 1L)
    env <- new.env(parent = globalenv())
    # its tempdir() is this test's own
    env$tempdir <- function(...) td
    res <- NULL
    out <- capture.output(res <- suppressMessages(withVisible(
      eval(parse(text = sample[[1L]]$code), env)))$value)
    expect_s3_class(res, "tfl_ai_collected")
    expect_setequal(res$reports$output_id, c("T-14-1-1", "T-14-2-1", "T-14-3-1", "T-14-3-5"))
    expect_identical(unique(res$reports$status), "ok")
    expect_true(all(file.exists(file.path(td, "sample-study", "spec", "outputs",
                                          paste0(res$reports$output_id, ".yml")))))
  })
}

test_that("the guide's steps 3 and 6 run on the bundled example, RTF and all", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "adam"))
  for (x in c("adsl", "adae", "adrs", "adlb")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
  }
  ex <- system.file("ai", "example", package = "tflspec")
  file.copy(file.path(ex, "onco_toc_shell.xlsx"), file.path(d, "TOC_SHELL.xlsx"))
  withr::local_dir(d)
  env <- new.env(parent = globalenv())
  run <- function(code) {
    code <- code[!grepl("^\\s*(setwd|library)\\(", code)]
    capture.output(suppressMessages(eval(parse(text = code), env)))
  }
  b <- guide_blocks("tflspec-ai-table-guide.md")
  step3 <- b[[which(vapply(b, function(z) grepl("^## 3", z$heading), NA))[1L]]]$code
  run(step3)
  expect_true(all(file.exists(c("ai/adam_summary.md", "ai/toc_shell.md", "ai/requests/index.md"))))
  expect_true(all(file.exists(file.path("ai", "requests", paste0(env$r$request, ".md")))))
  # the chat's answers: the stored ones, one a request (size = 3 here: the
  # sample's requests are one report each, as with size = 8)
  file.copy(list.files(file.path(ex, "answers"), full.names = TRUE), "ai/answers")
  step6 <- b[vapply(b, function(z) grepl("^## 6", z$heading), NA)]
  run(step6[[1L]]$code)
  expect_identical(unique(env$res$reports$status), "ok")
  run(step6[[2L]]$code)
  expect_true(all(file.exists(file.path("output", "tfl", paste0(env$res$reports$output_id, ".rtf")))))
})
