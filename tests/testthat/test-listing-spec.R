ls_rows <- function() {
  list(
    listings = data.frame(
      output_id = c("L-1", "L-2"), type = c(NA, " "),
      dataset = c("ADAE", "ADSL"), where = c("AESEV == 'SEVERE'", NA),
      sort = c("USUBJID | -ASTDY", "-AGE"), max_rows = c("4", NA),
      stringsAsFactors = FALSE),
    listing_cols = data.frame(
      output_id = c("L-1", "L-1", "L-1", "L-2", "L-2"),
      vars  = c("USUBJID", "AEDECOD | AESEV", "ASTDT", "USUBJID", "AGE"),
      label = c("Subject", "Preferred term /\\nSeverity", "Start", NA, "Age"),
      width = c("10", "30", NA, NA, "5"),
      collapse_repeats = c("true", NA, NA, NA, NA),
      stringsAsFactors = FALSE),
    figures = data.frame(output_id = "F-1", datasets = "ADSL"))
}

ls_adae <- function() {
  data.frame(
    USUBJID = c("S-01", "S-01", "S-02", "S-02", "S-03", "S-03"),
    AEDECOD = c("Headache", "Nausea", "Rash", "Fatigue", "Cough", "Fever"),
    AESEV   = c("SEVERE", "SEVERE", "MILD", "SEVERE", NA, "SEVERE"),
    ASTDY   = c(3, 10, 5, 7, 2, 4),
    ASTDT   = as.Date("2024-01-01") + c(3, 10, 5, 7, 2, 4),
    stringsAsFactors = FALSE)
}

test_that("the definition is normalised: text, blanks NA, empty rows gone, other sheets ignored", {
  x <- ls_rows()
  x$listing_cols[6, ] <- list("L-2", " ", NA, "", NA)
  sp <- tfl_listing_spec(x, check = FALSE)
  expect_s3_class(sp, "tfl_listing_spec")
  expect_named(sp, c("listings", "listing_cols"))
  expect_true(is.na(sp$listings$type[2]))
  expect_equal(nrow(sp$listing_cols), 5L)
  expect_true(all(vapply(sp$listing_cols, is.character, NA)))
  expect_identical(tfl_listing_spec(sp), sp)
  expect_identical(tfl_listing_spec(x$listings, x$listing_cols), tfl_listing_spec(x))
  expect_output(print(sp), "2 listings")
})

test_that("the check says what is wrong, and check = FALSE keeps a draft", {
  x <- ls_rows()
  x$listings$dataset[2] <- NA
  x$listings$max_rows[1] <- "4.5"
  x$listings$where[2] <- "AGE >"
  x$listings$sort[2] <- "AGE DESC"
  x$listing_cols$width[2] <- "wide"
  x$listing_cols$collapse_repeats[1] <- "yes"
  x$listing_cols[6, ] <- list("L-9", "X", NA, NA, NA)
  e <- tryCatch(tfl_listing_spec(x), error = conditionMessage)
  for (m in c("listing L-2: no `dataset`", "`max_rows` is not a whole number",
              "`where` is not R code", "`sort` takes variable names",
              "listing L-1, column 2: `width`", "`collapse_repeats` is TRUE or FALSE",
              "output_id not in `listings`: L-9")) {
    expect_match(e, m, fixed = TRUE)
  }
  expect_s3_class(tfl_listing_spec(x, check = FALSE), "tfl_listing_spec")
  y <- ls_rows()
  y$listing_cols <- y$listing_cols[y$listing_cols$output_id == "L-1", ]
  expect_error(tfl_listing_spec(y), "listing L-2: no columns")
})

test_that("the workbook: written, read back, narrowed; other sheets ignored", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  sp <- tfl_listing_spec(ls_rows())
  f <- tempfile(fileext = ".xlsx")
  tfl_write_listing_spec(sp, f)
  expect_identical(tfl_read_listing_spec(f), sp)
  one <- tfl_read_listing_spec(f, output_id = "L-2")
  expect_equal(one$listings$output_id, "L-2")
  expect_true(all(one$listing_cols$output_id == "L-2"))
  expect_error(tfl_read_listing_spec(f, output_id = "L-9"), "No listing L-9")

  # tflplanner's workbook: a figures sheet besides, and a note column
  g <- tempfile(fileext = ".xlsx")
  x <- ls_rows()
  x$listings$note <- "for the reviewer"
  writexl::write_xlsx(x, g)
  expect_identical(tfl_read_listing_spec(g), sp)

  bad <- tempfile(fileext = ".xlsx")
  x$listings$colour <- "red"
  writexl::write_xlsx(x, bad)
  expect_error(tfl_read_listing_spec(bad), "does not read: colour")
  writexl::write_xlsx(list(figures = x$figures), bad)
  expect_error(tfl_read_listing_spec(bad), "no `listings` sheet")

  empty <- tempfile(fileext = ".xlsx")
  tfl_write_listing_spec(tfl_listing_spec(), empty)
  expect_equal(readxl::excel_sheets(empty), c("listings", "listing_cols"))
})

test_that("the code: from the definition or its workbook, one listing at a time", {
  sp <- tfl_listing_spec(ls_rows())
  cat <- data.frame(dataset = "ADAE", path = "data/adae.rds", derive = NA)
  code <- tfl_listing_code(sp, "L-1", cat)
  # its condition and its order, one statement (dplyr)
  expect_true(paste0("data <- adae |>\n  dplyr::filter(AESEV == 'SEVERE') |>\n",
                     "  dplyr::arrange(USUBJID, dplyr::desc(ASTDY))") %in% code)
  # the listing's code lists, on its columns: each a factor, it sorts so
  cl <- data.frame(output_id = "L-1", variable = "AESEV",
                   value = c("MILD", "MODERATE", "SEVERE"),
                   label = c("Mild", "Moderate", "Severe"), order = c("1", "2", "3"))
  lc <- tfl_listing_code(sp, "L-1", cat, codelists = cl)
  expect_true("cl_aesev <- c(MILD = \"Mild\", MODERATE = \"Moderate\", SEVERE = \"Severe\")" %in% lc)
  expect_true(any(grepl("dplyr::filter(AESEV == 'SEVERE') |>\n  set_levels(AESEV = cl_aesev) |>",
                        lc, fixed = TRUE)))
  # a rework before the order
  rw <- tfl_listing_code(sp, "L-1", cat, rework = "data$X <- 1")
  expect_lt(match("data$X <- 1", rw),
            match("data <- dplyr::arrange(data, USUBJID, dplyr::desc(ASTDY))", rw))
  expect_true(any(grepl("listing_col(c(\"AEDECOD\", \"AESEV\"), width = 30", code, fixed = TRUE)))
  expect_true(any(grepl("collapse_repeats = TRUE", code, fixed = TRUE)))
  expect_true("content <- as_rtftables(data, listing = lst, max_rows = 4)" %in% code)
  expect_error(tfl_listing_code(sp, datasets = cat), "2 listings")
  expect_error(tfl_listing_code(sp, "L-9", cat), "No listing L-9")
  expect_error(tfl_listing_code(ls_rows()$listings, "L-1", cat), "`spec` must be a listing spec")
  draft <- tfl_listing_spec(list(listings = data.frame(output_id = "L-3", sort = "X")),
                            check = FALSE)
  expect_null(tfl_listing_code(draft, "L-3", cat))
  skip_if_not_installed("writexl")
  f <- tempfile(fileext = ".xlsx")
  tfl_write_listing_spec(sp, f)
  expect_identical(tfl_listing_code(f, "L-1", cat), code)
})

test_that("tfl_listing() gives the pages the code gives", {
  sp <- tfl_listing_spec(ls_rows())
  adae <- ls_adae()
  rds <- normalizePath(tempfile(fileext = ".rds"), winslash = "/", mustWork = FALSE)
  saveRDS(adae, rds)
  cat <- data.frame(dataset = "ADAE", path = rds, derive = NA)
  env <- new.env(parent = asNamespace("rtfreporter"))
  eval(parse(text = tfl_listing_code(sp, "L-1", cat)), env)
  pages <- tfl_listing(adae, sp, "L-1")
  expect_identical(pages, env$content)

  # what the definition says happened: SEVERE only (NA dropped), sorted,
  # dates as text, 4 rows a page
  d <- env$data
  expect_equal(d$AEDECOD, c("Nausea", "Headache", "Fatigue", "Fever"))
  expect_type(d$ASTDT, "character")
  expect_gt(length(pages), 0L)
  expect_s3_class(pages[[1]], "rtftable")
})

test_that("tfl_listing(): one listing needs no output_id; a bad sort says so", {
  sp <- tfl_listing_spec(lapply(ls_rows(), function(d)
    d[d$output_id == "L-2", , drop = FALSE]))
  adsl <- data.frame(USUBJID = c("A", "B", "C"), AGE = c(30, 50, 40))
  pages <- tfl_listing(adsl, sp)
  expect_s3_class(pages[[1]], "rtftable")
  expect_error(tfl_listing(adsl["USUBJID"], sp), "sorts by AGE, which the data has not")
})

test_that("tfl_listing() in the old order (spec, data) says the order changed", {
  sp <- tfl_listing_spec(lapply(ls_rows(), function(d)
    d[d$output_id == "L-2", , drop = FALSE]))
  adsl <- data.frame(USUBJID = c("A", "B", "C"), AGE = c(30, 50, 40))
  msg <- "the order of the arguments changed: tfl_listing\\(data, spec\\)"
  expect_error(tfl_listing(sp, adsl), msg)
  expect_error(tfl_listing("listing.xlsx", adsl), msg)
})

test_that("a dataset the catalog has not stops the program, saying so", {
  cat <- data.frame(dataset = "ADSL", path = "data/adsl.rds", derive = NA)
  code <- tfl_read_data_code(cat, "ADAE")
  expect_identical(code,
                   "stop(\"tflspec: dataset ADAE is not in the data catalog.\")")
  expect_error(eval(str2lang(code)),
               "tflspec: dataset ADAE is not in the data catalog.", fixed = TRUE)
  expect_true("adsl <- readRDS(\"data/adsl.rds\")" %in% tfl_read_data_code(cat, "ADSL"))
})

test_that("an .rda (or .RData) file holding one dataset is read, in the code and by tfl_read_adam()", {
  d <- withr::local_tempdir()
  adsl <- data.frame(USUBJID = c("A", "B"), AGE = c(30, 40))
  save(adsl, file = file.path(d, "adsl.rda"))
  cat <- data.frame(dataset = "ADSL", path = file.path(d, "adsl.rda"), derive = NA)
  code <- tfl_read_data_code(cat, "ADSL")
  e <- new.env()
  eval(parse(text = code), envir = e)
  expect_identical(e$adsl, adsl)
  file.copy(file.path(d, "adsl.rda"), file.path(d, "adae.RData"))
  got <- tfl_read_adam(d)
  expect_identical(names(got), c("ADAE", "ADSL"))
  expect_identical(got$ADSL, adsl)
})
