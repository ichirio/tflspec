# The study's ADaM metadata for the AI kit (tfl_adam_metadata()), and its
# use by the kit, tfl_ai_context() and tfl_ai_review().

md_define <- function() system.file("extdata", "define", "define.xml", package = "tflspec")

test_that("tfl_adam_metadata(): one row a variable from the define, with its code list's values", {
  skip_if_not_installed("xml2")
  md <- tfl_adam_metadata(define = md_define())
  expect_identical(names(md), c("dataset", "dataset_label", "variable", "label",
                                "kind", "levels", "codelist", "source"))
  expect_identical(attr(md, "source"), "define.xml")
  sex <- md[md$dataset == "ADSL" & md$variable == "SEX", ]
  expect_identical(sex$levels, "M | F")
  expect_identical(sex$kind, "character")
  expect_identical(md$kind[md$variable == "AGE"][1L], "numeric")
  expect_true(is.na(md$levels[md$variable == "USUBJID"][1L]))
  expect_identical(md$dataset_label[md$dataset == "ADSL"][1L], "Subject-Level Analysis Dataset")
})

test_that("tfl_adam_metadata(): from the data, the column information and never a subject's values", {
  adsl <- data.frame(USUBJID = c("01-001", "01-002", "01-003"),
                     SITEID = c("701", "702", "701"),
                     SEX = c("F", "M", "F"),
                     ARM = factor(c("Placebo", "Active", "Placebo"),
                                  levels = c("Placebo", "Active")),
                     AGE = c(63, 71, 58),
                     RANDDT = as.Date(c("2024-01-02", "2024-01-05", "2024-01-09")),
                     COMMENT = c(strrep("a long free-text comment ", 3), "x", "y"))
  attr(adsl$SEX, "label") <- "Sex"
  md <- tfl_adam_metadata(datasets = list(adsl = adsl))
  expect_identical(attr(md, "source"), "datasets")
  expect_identical(unique(md$dataset), "ADSL")
  lv <- stats::setNames(md$levels, md$variable)
  expect_identical(unname(lv["SEX"]), "F | M")
  expect_identical(unname(lv["ARM"]), "Placebo | Active")          # a factor's own order
  expect_true(all(is.na(lv[c("USUBJID", "SITEID", "AGE", "RANDDT", "COMMENT")])))
  expect_identical(md$label[md$variable == "SEX"], "Sex")
  expect_identical(md$kind[md$variable == "RANDDT"], "date")
  # a column with more values than max_levels lists none
  expect_true(is.na(tfl_adam_metadata(datasets = list(ADSL = adsl), max_levels = 1L)$levels[3L]))
})

test_that("tfl_adam_metadata(): one source, the first of define, adam_spec, datasets", {
  skip_if_not_installed("xml2")
  adsl <- data.frame(SEX = c("F", "M"))
  expect_message(md <- tfl_adam_metadata(define = md_define(), datasets = list(ADSL = adsl)),
                 "using `define`; `datasets` not used")
  expect_identical(attr(md, "source"), "define.xml")
  expect_error(tfl_adam_metadata(), "give one of")
  expect_error(tfl_adam_metadata(define = "no-such.xml"), "a file that exists")
  expect_error(tfl_adam_metadata(datasets = 1:3), "named list of data frames")
})

test_that("tflspec_ai_manual(define = ): the kit holds the metadata and the request says to use it", {
  skip_if_not_installed("xml2")
  skip_if_not_installed("readxl")
  d <- file.path(tempdir(), "ai-kit-metadata")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  kit <- suppressMessages(tflspec_ai_manual(d, define = md_define()))
  expect_identical(basename(kit)[length(kit)], "07_adam_metadata.csv")
  expect_length(kit, 8L)
  csv <- utils::read.csv(file.path(d, "07_adam_metadata.csv"), colClasses = "character")
  expect_identical(names(csv)[1:3], c("dataset", "dataset_label", "variable"))
  start <- readLines(file.path(d, "00_START_HERE.md"))
  expect_true(any(grepl("## What to give the AI", start, fixed = TRUE)))
  expect_true(any(grepl("take every dataset,", start, fixed = TRUE)))
  expect_true(any(grepl("07_adam_metadata.csv", start, fixed = TRUE)))
  # written again without metadata, the kit keeps none of the last one
  kit2 <- suppressMessages(tflspec_ai_manual(d, overwrite = TRUE))
  expect_false(file.exists(file.path(d, "07_adam_metadata.csv")))
  expect_length(kit2, 7L)
  expect_false(any(grepl("take every dataset,", readLines(file.path(d, "00_START_HERE.md")),
                         fixed = TRUE)))
})

test_that("tfl_ai_context('toc', datasets = ) takes the metadata frame", {
  skip_if_not_installed("xml2")
  md <- tfl_adam_metadata(define = md_define())
  ctx <- tfl_ai_context("toc", datasets = md)
  expect_identical(names(ctx$datasets), c("dataset", "dataset_label", "variable",
                                          "label", "kind", "levels"))
  p <- paste(format(tfl_ai_prompt("toc", ctx)), collapse = "\n")
  expect_match(p, "## The datasets", fixed = TRUE)
  expect_match(p, "ADSL", fixed = TRUE)
})

test_that("tfl_ai_review(metadata = ): unknown names and levels are the AI's to fix", {
  d <- file.path(tempdir(), "ai-review-metadata")
  unlink(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  kit <- suppressMessages(tflspec_ai_manual(file.path(d, "kit"), overwrite = TRUE))
  st <- file.path(d, "XYZ-001")
  tfl_ai_unpack(kit[basename(kit) == "06_example_spec.md"], st)
  md <- tfl_adam_metadata(datasets = list(ADSL = tflspec::adsl,
                                          ADAE = tflspec::adae,
                                          ADVS = tflspec::advs))
  # the example, against the sample data's own metadata: nothing unknown
  r0 <- tfl_ai_review(st, metadata = md, write = FALSE)
  expect_false(any(r0$rule %in% c("K14", "K15")))
  # a renamed variable and an invented level
  t1 <- file.path(st, "spec", "outputs", "T-1.yml")
  x <- readLines(t1)
  x[which(x == "        - AGE")[1L]] <- "        - AGEYRS"
  x <- sub("    - value: M", "    - value: Female", x, fixed = TRUE)
  writeLines(x, t1)
  r <- tfl_ai_review(st, metadata = md, write = FALSE)
  k14 <- r[r$rule == "K14", ]
  expect_true(any(grepl("AGEYRS", k14$message, fixed = TRUE)))
  expect_true(all(k14$who == "ai"))
  expect_match(k14$hint[grepl("AGEYRS", k14$message)][1L], "AGE", fixed = TRUE)
  expect_true(any(r$rule == "K15" & grepl("Female", r$message, fixed = TRUE)))
  # the kit's CSV read back works the same
  csv <- file.path(d, "md.csv")
  utils::write.csv(md, csv, row.names = FALSE, na = "")
  r2 <- tfl_ai_review(st, metadata = csv, write = FALSE)
  expect_identical(sum(r2$rule == "K14"), sum(r$rule == "K14"))
  expect_error(tfl_ai_review(st, metadata = 1:3, write = FALSE), "tfl_adam_metadata")
})
