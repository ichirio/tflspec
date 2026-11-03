# The AI's reports listed for the person (tfl_ai_outputs())

test_that("tfl_ai_outputs(): a row a report, its request's sheets, status, adoption", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- outs_study(root)
  res <- suppressMessages(tfl_ai_run(d, rtf = FALSE))
  o <- tfl_ai_outputs(d)
  expect_named(o, c("sheet", "shell_code", "toc_id", "output_id", "title", "population",
                    "split", "request", "status", "rtf", "problem",
                    "adopted", "file"))
  made <- res$reports$output_id[!is.na(res$reports$file)]
  expect_true(all(made %in% o$output_id))
  expect_true(all(startsWith(stats::na.omit(o$file), "ai/spec/outputs/")))
  expect_true(all(o$adopted == "no"))
  expect_true(all(o$status %in% c("ok", "warning", "error", "no answer", "not answered", "draft")))
  expect_true(any(!is.na(o$population)))
  # adopted: so; the AI redrafting it later: "draft newer"
  id <- made[1L]
  invisible(tfl_ai_adopt(d, id))
  expect_identical(tfl_ai_outputs(d)$adopted[match(id, o$output_id)], "yes")
  f <- file.path(d, o$file[match(id, o$output_id)])
  writeLines(c(readLines(f, warn = FALSE), "# redrafted"), f)
  expect_identical(tfl_ai_outputs(d)$adopted[match(id, o$output_id)], "yes, draft newer")
})

test_that("tfl_ai_collect_answers(): an assumption's `from` beside ones without it is kept, not an error (#598)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  d <- outs_study(root)
  suppressMessages(tfl_ai_collect_answers(d))
  f <- file.path(d, "ai", "spec", "outputs", "T-14-1-1.yml")
  y <- yaml::read_yaml(f)
  expect_gt(length(y$ai$assumptions), 1L)
  y$ai$assumptions[[1L]]$from <- "programmer convention"
  yaml::write_yaml(y, f)
  res <- suppressMessages(tfl_ai_collect_answers(d))
  o <- tfl_ai_outputs(d)
  expect_true(o$status[o$output_id == "T-14-1-1"] %in% c("ok", "warning"))
  a <- tfl_ai_assumptions(d)
  expect_true("from" %in% names(a))
  expect_true("programmer convention" %in% a$from[a$output_id == "T-14-1-1"])
})
