# A listing written in code, through its listing spec and a workbook, back:
# the RTF is the same byte for byte -- from the pages tfl_listing() makes
# and from the program tfl_listing_code() writes.

lr_rtf <- function(pages) {
  old <- options(rtfreporter.render_time =
                   as.POSIXct("2000-01-01 00:00:00", tz = "UTC"))
  on.exit(options(old), add = TRUE)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  suppressMessages(generate_rtfreport(rtf_tables(rtf_document(), pages), f,
                                      overwrite = TRUE, program = "p.R"))
  readBin(f, "raw", file.info(f)$size)
}

lr_data <- function() {
  d <- as.data.frame(cards::ADSL)[1:30, c("USUBJID", "AGE", "SEX", "TRT01A")]
  d$SEX <- as.character(d$SEX)
  d[order(d$TRT01A, d$USUBJID), ]
}

test_that("a plan's listing through its spec and a workbook gives the same RTF", {
  skip_if_not_installed("cards")
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  d <- lr_data()
  rownames(d) <- NULL
  p <- table_plan(d) |>
    plan_listing(listing_col("TRT01A", label = "Treatment", collapse_repeats = TRUE),
                 listing_col("USUBJID", label = "Subject", width = 12),
                 listing_col(c("AGE", "SEX"), sep = " / ", align = "center",
                             label = "Age / Sex")) |>
    plan_paginate_rows(max_rows = 10)
  sp <- tfl_as_listing_spec(p, "L-1", dataset = "ADSL")
  expect_identical(attr(sp, "not_converted"), character())
  expect_true(attr(sp, "same_pages"))
  expect_identical(sp$listing_cols$align, c(NA, NA, "center"))
  expect_identical(sp$listing_cols$sep, c(NA, NA, "\" / \""))
  want <- lr_rtf(suppressMessages(plan_apply(p, "pages")))

  # through the workbook
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  tfl_write_listing_spec(sp, f)
  back <- tfl_read_listing_spec(f)
  expect_identical(lr_rtf(tfl_listing(d, back, "L-1")), want)

  # through the program it writes
  dir <- tempfile("lr")
  dir.create(file.path(dir, "data"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  saveRDS(d, file.path(dir, "data", "adsl.rds"))
  cat_ <- data.frame(dataset = "ADSL", path = "data/adsl.rds",
                     derive = NA_character_)
  code <- tfl_listing_code(back, "L-1", cat_)
  e <- new.env(parent = globalenv())
  eval(parse(text = tfl_helpers_code()), e)
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)
  eval(parse(text = code), e)
  setwd(owd)
  expect_identical(lr_rtf(e$content), want)
})

test_that("a listing_spec() object: the listing's own options, and what stays in code", {
  lst <- listing_spec(list(listing_col("USUBJID", rel_width = 2),
                           listing_col(c("AGE", "SEX"))),
                      blank_row = FALSE, sep = " - ")
  sp <- suppressMessages(tfl_as_listing_spec(lst, "L-2"))
  expect_identical(sp$listings$blank_row, "FALSE")
  # the listing's default separator goes to the columns without their own
  expect_identical(sp$listing_cols$sep, c("\" - \"", "\" - \""))
  expect_true(any(grepl("rel_width", attr(sp, "not_converted"))))
  expect_true(is.na(attr(sp, "same_pages")))
  expect_error(tfl_as_listing_spec(data.frame()), "must be a listing_spec")
})

test_that("the new listing columns are checked", {
  l <- data.frame(output_id = "L", dataset = "ADSL")
  cl <- data.frame(output_id = "L", vars = "USUBJID")
  expect_error(tfl_listing_spec(transform(l, blank_row = "maybe"), cl),
               "`blank_row` is TRUE or FALSE")
  expect_error(tfl_listing_spec(transform(l, wrap = "my wrap()"), cl),
               "`wrap` is the name of an R function")
  expect_error(tfl_listing_spec(l, transform(cl, align = "middle")),
               "`align` is left, center or right")
  # wrap by name: the type's own rule, named, changes nothing
  skip_if_not_installed("cards")
  d <- lr_data()
  sp <- tfl_listing_spec(l, transform(cl, width = "5"))
  sp2 <- tfl_listing_spec(transform(l, wrap = "rtfreporter::listing_wrap"),
                          transform(cl, width = "5"))
  expect_identical(lr_rtf(tfl_listing(d, sp2, "L")),
                   lr_rtf(tfl_listing(d, sp, "L")))
  expect_match(tfl_listing_code(sp2, "L", data.frame(dataset = "ADSL",
                                                     path = "a.rds",
                                                     derive = NA)),
               "wrap = rtfreporter::listing_wrap", all = FALSE, fixed = TRUE)
})
