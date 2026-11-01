# The AI's assumptions: their ids and kinds (ai/assumption_ids.csv), the
# fix the agent made kept

asm_rows <- function(d, rows) {
  dir.create(file.path(d, "spec"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(d, "ai"), recursive = TRUE, showWarnings = FALSE)
  writeLines(yaml::as.yaml(rows, indent.mapping.sequence = TRUE),
             file.path(d, "spec", "_assumptions.yml"))
  d
}

asm_five <- function() {
  list(
    list(output_id = "T-1", where = "ard.analyses", item = "arm", assumption = "TRT01A",
         question = "Actual or planned?"),
    list(output_id = "T-2", where = "ard.analyses", item = "arm", assumption = "TRT01A",
         question = "actual  or planned ?"),
    list(output_id = "T-1", where = "codelists", item = "SEX", assumption = "M / F"),
    list(output_id = "T-3", where = "codelists", item = "sex", assumption = "M/F"),
    list(output_id = "T-3", where = "page", item = "titles", assumption = "two lines"))
}

test_that("each assumption an id kept across collections, and a kind", {
  d <- asm_rows(withr::local_tempdir(), asm_five())
  a <- tfl_ai_assumptions(d)
  expect_named(a, c("id", "kind", "n_reports", "output_id", "where", "item", "assumption",
                    "value", "question", "from"))
  expect_identical(a$id, paste0("A", 1:5))
  expect_identical(a$kind, c("K1", "K1", "K2", "K2", "K3"))
  expect_identical(a$n_reports, c(2L, 2L, 2L, 2L, 1L))
  expect_true(all(a$status == "open"))
  reg <- utils::read.csv(file.path(d, "ai", "assumption_ids.csv"), stringsAsFactors = FALSE)
  expect_named(reg, c("type", "key", "id"))
  expect_identical(sum(reg$type == "A"), 5L)
  # the ids written into spec/_assumptions.yml
  y <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  expect_identical(vapply(y, `[[`, "", "id"), paste0("A", 1:5))
  # collected again, in another order, an item written differently, a new
  # one: the same key its id, the new one the next
  r <- asm_five()[c(5, 3, 1)]
  r[[3]]$item <- "Arm."
  r[[4]] <- list(output_id = "T-4", where = "page", item = "footnotes", assumption = "none")
  asm_rows(d, r)
  b <- tfl_ai_assumptions(d)
  expect_identical(b$id, c("A5", "A3", "A1", "A6"))
  expect_identical(b$kind, c("K3", "K2", "K1", "K4"))
  expect_identical(b$n_reports, c(1L, 1L, 1L, 1L))
})

test_that("collected answers: ids kept; an assumption the agent fixed survives a collection", {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr::local_tempdir()
  ad <- file.path(root, "adam")
  dir.create(ad, recursive = TRUE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(ad, paste0(x, ".xpt")))
  }
  d <- file.path(root, "study")
  book <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
  tfl_ai_prepare(unit = "toc", d, book, adam = normalizePath(ad, winslash = "/"),
                 quiet = TRUE, size = 3L)
  ai_drafts_from_fixtures(d)
  res <- suppressMessages(tfl_ai_collect_answers(d))
  expect_named(res$assumptions, c("output_id", "where", "item", "assumption", "value",
                                  "question", "id", "kind"))
  expect_true(all(grepl("^A[0-9]+$", res$assumptions$id)))
  expect_true(all(grepl("^K[0-9]+$", res$assumptions$kind)))
  expect_false(anyDuplicated(res$assumptions$id) > 0L)
  res2 <- suppressMessages(tfl_ai_collect_answers(d))
  # (the usual widths, written into the draft by the first collection, stay an
  # assumption of the draft's: the rows and their ids are the same)
  expect_identical(res2$assumptions, res$assumptions)
  # the demographics' arm is wrong; the agent applies it and marks the row fixed
  a <- tfl_ai_assumptions(d)
  id <- a$id[a$output_id %in% "T-14-1-1" & a$item %in% "arm"]
  expect_length(id, 1L)
  f <- file.path(d, "spec", "_assumptions.yml")
  y <- yaml::read_yaml(f)
  at <- which(vapply(y, function(e) identical(e$id, id), NA))
  y[[at]]$assumption <- "the columns are the planned treatment, TRT01P"
  y[[at]]$status <- "fixed"
  writeLines(yaml::as.yaml(y, indent.mapping.sequence = TRUE), f)
  # collected again: the answer still says TRT01A -- the fixed row kept, a note
  res3 <- suppressMessages(tfl_ai_collect_answers(d))
  expect_identical(res3$assumptions$id, res2$assumptions$id)
  y3 <- yaml::read_yaml(f)
  r <- Filter(function(e) identical(e$id, id), y3)[[1L]]
  expect_identical(r$status, "fixed")
  expect_identical(r$assumption, "the columns are the planned treatment, TRT01P")
  expect_match(r$note, "the answer still says", fixed = TRUE)
})

test_that("an assumption's value -- the value the AI used -- is kept and shown", {
  rows <- asm_five()
  rows[[1L]]$assumption <- "Treatment variable for the columns: TRT01A (actual), because the set is SAF"
  rows[[1L]]$value <- "TRT01A"
  rows[[5L]]$value <- list("Table 14.1.1", "Demographics")   # a list: joined
  d <- asm_rows(withr::local_tempdir(), rows)
  a <- tfl_ai_assumptions(d)
  expect_named(a, c("id", "kind", "n_reports", "output_id", "where", "item", "assumption",
                    "value", "question", "from"))
  expect_identical(a$value, c("TRT01A", NA, NA, NA, "Table 14.1.1, Demographics"))
  # written back with the ids, the value stays, after the assumption
  y <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  expect_identical(y[[1L]]$value, "TRT01A")
  expect_identical(names(y[[1L]]), c("id", "kind", "output_id", "where", "item", "assumption",
                                     "value", "question"))
  expect_null(y[[2L]]$value)
})

test_that("a field tflspec does not know survives the file being written back", {
  rows <- asm_five()
  rows[[1L]]$value <- "TRT01A"
  rows[[1L]]$source <- "shell"              # a person's own field
  rows[[3L]]$checked_by <- list("A", "B")
  d <- asm_rows(withr::local_tempdir(), rows)
  a <- tfl_ai_assumptions(d)                # stamps the ids: the file is written back
  expect_identical(a$value[1L], "TRT01A")
  y <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  expect_identical(y[[1L]]$source, "shell")
  expect_identical(y[[1L]]$value, "TRT01A")
  expect_identical(y[[3L]]$checked_by, "A, B")
  expect_null(y[[2L]]$source)
  expect_false("source" %in% names(a))      # the table shows the known columns
})
