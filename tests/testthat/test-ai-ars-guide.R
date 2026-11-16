# The ARS guide for the agent (#511): its example passes tfl_check_ars(),
# the guide's JSON is the example's, and tfl_ai_prepare() writes ai/ars/
# only when asked

ars_kit <- function(f) system.file("ai", "ars", f, package = "tflspec")

test_that("the ARS guide's example reporting event passes tfl_check_ars()", {
  f <- ars_kit("example-reporting-event.json")
  expect_true(file.exists(f))
  expect_identical(nrow(tfl_check_ars(f, schema = FALSE)), 0L)
  skip_if_not_installed("jsonvalidate")
  expect_identical(nrow(tfl_check_ars(f)), 0L)
})

test_that("the guide's JSON blocks are the example's subsets and analysis", {
  re <- jsonlite::fromJSON(ars_kit("example-reporting-event.json"), simplifyVector = FALSE)
  md <- readLines(ars_kit("README.md"), encoding = "UTF-8")
  expect_true(any(grepl("tfl_check_ars(", md, fixed = TRUE)))
  open <- which(md == "```json")
  expect_length(open, 3L)
  blocks <- lapply(open, function(i) {
    end <- i + which(md[(i + 1L):length(md)] == "```")[1L]
    jsonlite::fromJSON(paste(md[(i + 1L):(end - 1L)], collapse = "\n"), simplifyVector = FALSE)
  })
  expect_true(list(blocks[[1L]]) %in% re$dataSubsets)
  expect_true(list(blocks[[2L]]) %in% re$dataSubsets)
  expect_true(list(blocks[[3L]]) %in% re$analyses)
  # the compound subset: each sub-clause its subClauseId, level and order
  wc <- blocks[[2L]]$compoundExpression$whereClauses
  expect_length(wc, 2L)
  for (w in wc) expect_true(all(c("subClauseId", "level", "order") %in% names(w)))
})

test_that("tfl_check_ars() names a sub-clause without its level, order or subClauseId", {
  re <- jsonlite::fromJSON(ars_kit("example-reporting-event.json"), simplifyVector = FALSE)
  ce <- re$dataSubsets[[3L]]$compoundExpression
  ce$whereClauses[[1L]] <- re$dataSubsets[[1L]]$condition
  ce$whereClauses[[2L]] <- list(subClauseId = "Dss_NONE")
  re$dataSubsets[[3L]]$compoundExpression <- ce
  f <- withr::local_tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(re, auto_unbox = TRUE, pretty = TRUE), f)
  ck <- tfl_check_ars(f, schema = FALSE)
  at <- "DataSubset Dss_TEAE_SER whereClauses"
  expect_true(all(c("level", "order") %in% ck$field[ck$part == paste0(at, "[1]")]))
  expect_true("subClauseId" %in% ck$field[ck$part == paste0(at, "[1]")])
  expect_true(all(c("level", "order", "subClauseId") %in% ck$field[ck$part == paste0(at, "[2]")]))
  expect_true(any(grepl("no data subset Dss_NONE", ck$problem)))
})

test_that("tfl_ai_prepare() writes ai/ars/ only when asked, and AGENTS.md names it", {
  skip_on_cran()
  skip_if_not_installed("haven")
  adam_in <- function(d) {
    dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
    for (x in c("adsl", "adae")) {
      df <- getExportedValue("tflspec", x)
      for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
      haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
    }
    d
  }
  d <- adam_in(withr::local_tempdir())
  book <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
  r <- suppressMessages(tfl_ai_prepare(d, book, study_id = "XYZ-DEMO-01"))
  expect_false(dir.exists(file.path(d, "ai", "ars")))
  expect_false(any(grepl("^ai/ars/", r$files)))
  expect_false(any(grepl("ars/README.md", readLines(file.path(d, "ai", "AGENTS.md")),
                         fixed = TRUE)))
  # a study already there: the guide written, AGENTS.md kept (and said so)
  expect_message(tfl_ai_prepare(d, book, study_id = "XYZ-DEMO-01", ars = TRUE),
                 "tfl_ai_refresh_agents")
  expect_true(file.exists(file.path(d, "ai", "ars", "README.md")))
  # a new study asking for it
  d <- adam_in(withr::local_tempdir())
  r <- suppressMessages(tfl_ai_prepare(d, book, study_id = "XYZ-DEMO-01", ars = TRUE))
  got <- file.path(d, "ai", "ars", c("README.md", "example-reporting-event.json"))
  expect_true(all(file.exists(got)))
  expect_true(all(c("ai/ars/README.md", "ai/ars/example-reporting-event.json") %in% r$files))
  expect_identical(nrow(tfl_check_ars(got[2L], schema = FALSE)), 0L)
  ag <- paste(readLines(file.path(d, "ai", "AGENTS.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(ag, "ars/README.md", fixed = TRUE)
  expect_match(ag, "tfl_check_ars()", fixed = TRUE)
  # the guide's paths are from ai/, the agent's folder
  expect_false(any(grepl("ai/ars/", readLines(got[1L]), fixed = TRUE)))
  # the setting is kept: a repeat writes the guide again
  expect_true(isTRUE(tfl_ai_prepare_settings(d)$ars))
  unlink(file.path(d, "ai", "ars"), recursive = TRUE)
  suppressMessages(tfl_ai_prepare(d))
  expect_true(all(file.exists(got)))
  expect_error(tfl_ai_prepare(d, book, ars = NA), "ars = ")
})
