# tfl_ai_adopt() copies no draft that has an error; tfl_ai_outputs() lists
# every report asked for

# A small study of three drafts (two good, one with a broken YAML) made from
# the example study's drafts; returns the study folder and the three ids
ae_study <- function(root) {
  d <- outs_study(root)
  suppressMessages(tfl_ai_collect_answers(d))
  area <- file.path(d, "ai", "spec", "outputs")
  ids <- sub("[.]yml$", "", list.files(area, "[.]yml$"))
  list(dir = d, area = area, ids = ids)
}

ae_break <- function(f) {
  x <- readLines(f, warn = FALSE)
  writeLines(c(x, "note: \"never closed"), f)
}

test_that("tfl_ai_adopt(): a draft with a broken YAML is skipped, the others are adopted", {
  skip_on_cran()
  skip_if_not_installed("haven")
  s <- ae_study(withr::local_tempdir())
  expect_gte(length(s$ids), 3L)
  ok <- s$ids[1:2]
  bad <- s$ids[3L]
  ae_break(file.path(s$area, paste0(bad, ".yml")))
  msgs <- character()
  res <- withCallingHandlers(
    tfl_ai_adopt(s$dir, c(ok, bad)),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  expect_identical(res$status[match(ok, res$output_id)], rep("adopted", 2L))
  expect_identical(res$status[res$output_id == bad], "skipped (error)")
  expect_true(is.na(res$file[res$output_id == bad]))
  expect_match(res$problem[res$output_id == bad], "cannot be read")
  expect_true(all(is.na(res$problem[res$output_id %in% ok])))
  expect_true(all(file.exists(file.path(s$dir, "spec", "outputs", paste0(ok, ".yml")))))
  expect_false(file.exists(file.path(s$dir, "spec", "outputs", paste0(bad, ".yml"))))
  one <- grep("not copied|nothing was copied", msgs, value = TRUE)
  expect_length(one, 1L)
  expect_match(one, bad, fixed = TRUE)
  expect_match(one, "agent's chat")
  # the log records the adopted only
  expect_false(bad %in% tflspec:::.ai_adopted_read(s$dir)$output_id)
  # a dry run says the same
  dr <- tfl_ai_adopt(s$dir, bad, dry_run = TRUE)
  expect_identical(dr$status, "skipped (error)")
})

test_that("tfl_ai_adopt(): a draft the last check marked error is skipped, a warning is adopted", {
  skip_on_cran()
  skip_if_not_installed("haven")
  s <- ae_study(withr::local_tempdir())
  id <- s$ids[1L]
  m <- utils::read.csv(file.path(s$dir, "ai", "manifest.csv"), stringsAsFactors = FALSE)
  m$status[m$output_id == id] <- "error"
  m$reason[m$output_id == id] <- "the analysis names a variable the data lacks"
  utils::write.csv(m, file.path(s$dir, "ai", "manifest.csv"), row.names = FALSE)
  # (the manifest is newer than the draft)
  Sys.setFileTime(file.path(s$dir, "ai", "manifest.csv"), Sys.time() + 5)
  w <- s$ids[2L]
  m$status[m$output_id == w] <- "warning"
  utils::write.csv(m, file.path(s$dir, "ai", "manifest.csv"), row.names = FALSE)
  Sys.setFileTime(file.path(s$dir, "ai", "manifest.csv"), Sys.time() + 5)
  res <- suppressMessages(tfl_ai_adopt(s$dir, c(id, w)))
  expect_identical(res$status[res$output_id == id], "skipped (error)")
  expect_match(res$problem[res$output_id == id], "variable the data lacks")
  expect_identical(res$status[res$output_id == w], "adopted")
})

test_that("tfl_ai_adopt(): a good study file is not overwritten by a draft with an error", {
  skip_on_cran()
  skip_if_not_installed("haven")
  s <- ae_study(withr::local_tempdir())
  id <- s$ids[1L]
  suppressMessages(tfl_ai_adopt(s$dir, id))
  dest <- file.path(s$dir, "spec", "outputs", paste0(id, ".yml"))
  before <- unname(tools::md5sum(dest))
  ae_break(file.path(s$area, paste0(id, ".yml")))
  expect_message(res <- tfl_ai_adopt(s$dir, id, overwrite = TRUE), id, fixed = TRUE)
  expect_identical(res$status, "skipped (error)")
  expect_identical(unname(tools::md5sum(dest)), before)
  expect_no_error(tfl_read_output(dest))
})

test_that("tfl_ai_adopt(): every draft with an error: nothing is copied, no R error", {
  skip_on_cran()
  skip_if_not_installed("haven")
  s <- ae_study(withr::local_tempdir())
  for (id in s$ids) ae_break(file.path(s$area, paste0(id, ".yml")))
  expect_message(res <- tfl_ai_adopt(s$dir), "nothing was copied")
  expect_true(all(res$status == "skipped (error)"))
  expect_identical(nrow(res), length(s$ids))
  expect_length(list.files(file.path(s$dir, "spec", "outputs"), "[.]yml$"), 0L)
})

test_that("tfl_ai_outputs(): a report asked for but never written is `not answered`, and not adopted", {
  skip_on_cran()
  skip_if_not_installed("haven")
  s <- ae_study(withr::local_tempdir())
  asked <- tflspec:::.ai_asked_ids(s$dir)
  gone <- s$ids[1L]
  expect_true(gone %in% asked)
  unlink(file.path(s$area, paste0(gone, ".yml")))
  # (no manifest: the collection has not seen it)
  unlink(file.path(s$dir, "ai", "manifest.csv"))
  o <- tfl_ai_outputs(s$dir)
  expect_true(all(asked %in% o$output_id))
  expect_identical(o$status[o$output_id == gone], "not answered")
  expect_true(is.na(o$file[o$output_id == gone]))
  res <- suppressMessages(tfl_ai_adopt(s$dir, gone))
  expect_identical(res$status, "no draft")
  expect_false(file.exists(file.path(s$dir, "spec", "outputs", paste0(gone, ".yml"))))
})
