# Analysis data: named data the analyses read (sheet analysis_data, #135)

ad_df <- function(...) as.data.frame(list(...), stringsAsFactors = FALSE)

ad_spec <- function(analysis_data, analyses, check = TRUE) {
  s <- list(
    study = ad_df(key = "id", value = "USUBJID"),
    datasets = ad_df(dataset = c("ADSL", "ADAE"),
                     path = c("adsl.rds", "adae.rds")),
    populations = ad_df(population_id = "SAF", dataset = "ADSL",
                        where = "SAFFL == \"Y\""),
    analysis_data = analysis_data,
    analyses = analyses)
  if (check) tfl_ard_spec(s) else s
}

ad_rows <- function() ad_df(
  data_id = c("adsl_saf", "adae_teae", "adae_ser", "adae_subj"),
  from = c("ADSL", "ADAE", "adae_teae", "adae_teae"),
  population_id = c("SAF", "SAF", NA, NA),
  where = c(NA, "TRTEMFL == \"Y\"", "AESER == \"Y\"", NA),
  add = c(NA, "TRT01A | AGEGR1", NA, NA),
  derive = c(NA, NA, "SER = 1", NA),
  distinct = c(NA, NA, NA, "USUBJID"))

an_rows <- function() ad_df(
  output_id = c("T1", "T1", "T1", "T2"),
  analysis_id = c("BIGN", "AE", "SER", "OLD"),
  method = c("cards::ard_tabulate", "cards::ard_stack_hierarchical",
             "cards::ard_tabulate", "cards::ard_tabulate"),
  data = c("adsl_saf", "adae_teae", "adae_ser", NA),
  dataset = c(NA, NA, NA, "ADAE"),
  population_id = c(NA, NA, NA, "SAF"),
  where = c(NA, NA, "AESEV == \"SEVERE\"", "TRTEMFL == \"Y\""),
  by = c(NA, "TRT01A", "TRT01A", "TRTA"),
  variables = c("TRT01A", "AEBODSYS | AEDECOD", "AGEGR1", "AESEV"),
  denominator = c(NA, "adsl_saf", "adae_subj", NA))

test_that("the analysis data is made once, in order, for what a report reads", {
  x <- ad_spec(ad_rows(), an_rows())
  code <- tfl_ard_code(x, save = FALSE, part = "body")
  # in the sheet's order; the population itself when the data is its dataset
  # (a data made in one statement: its condition, derive, one row per ...)
  at <- match(c("adsl_saf <- pop_saf",
                "adae_teae <- subset(adae, USUBJID %in% pop_saf$USUBJID & TRTEMFL == \"Y\")",
                "adae_ser <- adae_teae |>\n  subset(AESER == \"Y\") |>\n  transform(SER = 1)",
                "adae_subj <- dplyr::distinct(adae_teae, USUBJID, .keep_all = TRUE)"), code)
  expect_false(anyNA(at))
  expect_false(is.unsorted(at))
  # the population's columns (they replace one of the same name), derived
  # columns, one row per subject
  expect_true(any(code == paste0(
    "adae_teae <- dplyr::left_join(adae_teae[setdiff(names(adae_teae), ",
    "c(\"TRT01A\", \"AGEGR1\"))], pop_saf[c(\"USUBJID\", \"TRT01A\", ",
    "\"AGEGR1\")], by = \"USUBJID\")")))
  # an analysis's own condition on top, under a name of its own; the
  # denominator as it is; the analysis set is the data's
  expect_true(any(code == "adae_ser_1 <- subset(adae_ser, AESEV == \"SEVERE\")"))
  expect_true(any(grepl("cards::ard_tabulate(adae_ser_1,", code, fixed = TRUE)))
  expect_true(any(grepl("denominator = adae_subj)", code, fixed = TRUE)))
  expect_true(any(grepl("denominator = adsl_saf,", code, fixed = TRUE)))
  expect_true(any(code == "ards[[3]] <- .tag(.fmt(ard), \"T1\", \"SER\", \"SAF\")"))
  # a report that reads none makes none
  t2 <- tfl_ard_code(x, output_id = "T2", save = FALSE, part = "body")
  expect_false(any(grepl("adae_teae", t2, fixed = TRUE)))

  skip_if_not_installed("cards")
  dir <- withr_tempdir()
  saveRDS(cards::ADSL, file.path(dir, "adsl.rds"))
  saveRDS(cards::ADAE, file.path(dir, "adae.rds"))
  a <- tfl_build_ard(x, dir = dir, save = FALSE)
  expect_setequal(unique(a$analysis_id), c("BIGN", "AE", "SER", "OLD"))
  expect_identical(unique(a$population_id), "SAF")
  # the denominator: the safety set's subjects per group
  saf <- cards::ADSL[cards::ADSL$SAFFL == "Y", ]
  n <- a[a$analysis_id == "AE" & a$stat_name == "N", ]
  # (each group's, and the overall row's)
  expect_setequal(unique(unlist(n$stat)),
                  unique(c(as.numeric(table(saf$TRT01A)), nrow(saf))))
})

test_that("a spec without the sheet is as it was: code and fingerprints", {
  old <- an_rows()[4L, ]
  old$data <- NULL
  without <- ad_spec(NULL, old)
  with <- ad_spec(ad_rows(), an_rows())
  expect_identical(nrow(without$analysis_data), 0L)
  expect_identical(tfl_ard_code(without, save = FALSE, part = "body"),
                   tfl_ard_code(with, output_id = "T2", save = FALSE,
                                part = "body"))
  expect_identical(tfl_ard_spec_hash(without, "T2"),
                   tfl_ard_spec_hash(with, "T2"))
  # and a report that reads analysis data has it in its fingerprint
  ch <- ad_rows()
  ch$where[3L] <- "AESER == \"N\""
  expect_false(identical(tfl_ard_spec_hash(ad_spec(ch, an_rows()), "T1"),
                         tfl_ard_spec_hash(with, "T1")))
})

test_that("the sheet and the analyses' data are checked", {
  bad <- function(ad = ad_rows(), an = an_rows(), msg) {
    expect_error(ad_spec(ad, an), msg, fixed = TRUE)
  }
  d <- ad_rows(); d$data_id[2L] <- "ADAE_teae"
  bad(d, msg = "a lower-case letter first")
  d <- ad_rows(); d$data_id[2L] <- "adae"
  bad(d, msg = "the name of another object")
  d <- ad_rows(); d$data_id[3L] <- "adae_teae"
  bad(d, msg = "`data_id` repeated")
  d <- ad_rows(); d$from[2L] <- "adae_ser"
  bad(d, msg = "neither a dataset nor an analysis data above it")
  d <- ad_rows(); d$population_id[2L] <- "ITT"
  bad(d, msg = "population ITT is not in `populations`")
  d <- ad_rows(); d$population_id[2L] <- NA
  bad(d, msg = "`add` takes columns from the population's data")
  a <- an_rows(); a$data[2L] <- "adae_none"
  bad(an = a, msg = "data adae_none is not in `analysis_data`")
  a <- an_rows(); a$dataset[2L] <- "ADAE"
  bad(an = a, msg = "its `data` or a `dataset`; not both")
  a <- an_rows(); a$denominator[2L] <- "adae_none"
  bad(an = a, msg = "denominator(s) adae_none")
})

test_that("a workbook keeps the sheet; one written before it reads it empty", {
  skip_if_not_installed("writexl")
  x <- ad_spec(ad_rows(), an_rows())
  f <- file.path(withr_tempdir(), "ard_spec.xlsx")
  tfl_write_ard_spec(x, f)
  expect_true("analysis_data" %in% readxl::excel_sheets(f))
  y <- tfl_read_ard_spec(f)
  expect_identical(y$analysis_data, x$analysis_data[names(y$analysis_data)])
  expect_identical(tfl_ard_code(y, save = FALSE, part = "body"),
                   tfl_ard_code(x, save = FALSE, part = "body"))
  # a workbook of the four sheets (before analysis_data)
  old <- unclass(ad_spec(NULL, an_rows()[4L, setdiff(names(an_rows()), "data")],
                         check = FALSE))
  old$analysis_data <- NULL
  g <- file.path(withr_tempdir(), "old.xlsx")
  writexl::write_xlsx(old, g)
  z <- tfl_read_ard_spec(g)
  expect_identical(nrow(z$analysis_data), 0L)
  expect_true(all(is.na(z$analyses$data)))
})

test_that("ARS: the data's population, first dataset and conditions", {
  x <- ad_spec(ad_rows(), an_rows())
  x$analyses$purpose <- "PRIMARY OUTCOME MEASURE"
  ars <- tfl_ars(x)
  an <- Filter(function(z) grepl("SER", z$id, fixed = TRUE), ars$analyses)[[1L]]
  expect_identical(an$analysisSetId, "SAF")
  expect_identical(an$dataset, "ADAE")
  ds <- Filter(function(z) z$id == an$dataSubsetId, ars$dataSubsets)[[1L]]
  expect_identical(ds$name,
                   "(TRTEMFL == \"Y\") & (AESER == \"Y\") & (AESEV == \"SEVERE\")")
  un <- tfl_ars_unmapped(ars)
  expect_true(all(c("analysis_data$add", "analysis_data$derive") %in% un$item))
})

test_that("a report's own subjects, the columns kept, factors of derived columns (#137)", {
  ad <- ad_df(
    data_id = c("adsl_old", "adae_old"), from = c("ADSL", "ADAE"),
    population_id = c("SAF", NA), subjects = c(NA, "adsl_old"),
    where = c("AGE >= 65", "TRTEMFL == \"Y\""), add = c(NA, "TRT01A"),
    derive = c("OLD = ifelse(AGE >= 75, \"75+\", \"65-74\")", NA),
    keep = c(NA, "TRT01A | AEBODSYS | AEDECOD"))
  an <- ad_df(output_id = c("T1", "T1"), analysis_id = c("OLD", "AE"),
              method = c("cards::ard_tabulate", "cards::ard_stack_hierarchical"),
              data = c("adsl_old", "adae_old"), by = "TRT01A",
              variables = c("OLD", "AEBODSYS | AEDECOD"),
              denominator = c(NA, "adsl_old"))
  x <- ad_spec(ad, an)
  cl <- ad_df(output_id = c(NA, "T1", "T1"), variable = c("SEX", "OLD", "OLD"),
              value = c("F", "75+", "65-74"), order = c(1, 1, 2))
  code <- tfl_ard_code(x, output_id = "T1", save = FALSE, part = "body", codelists = cl)
  # the numerator kept to the denominator's subjects; add from that data
  expect_true(any(code == "adae_old <- subset(adae, USUBJID %in% adsl_old$USUBJID & TRTEMFL == \"Y\")"))
  expect_true(any(grepl("adsl_old[c(\"USUBJID\", \"TRT01A\")]", code, fixed = TRUE)))
  expect_true(any(code == "adae_old <- subset(adae_old, select = c(USUBJID, TRT01A, AEBODSYS, AEDECOD))"))
  # the derived column a factor: the report's own code list rows count
  expect_true(any(startsWith(code, "adsl_old <- pop_saf |>") & endsWith(code, " |>\n  .levels()")))
  expect_true(any(grepl("`OLD` = c(\"75+\", \"65-74\")", code, fixed = TRUE)))
  # the study's program: the study rows only (no OLD, no factor line)
  all <- tfl_ard_code(x, save = FALSE, part = "body", codelists = cl)
  expect_false(any(grepl("`OLD`", all, fixed = TRUE)))
  expect_false(any(all == "adsl_old <- .levels(adsl_old)"))
  # the ARD's analysis set: the one the subjects are of
  expect_true(any(grepl("\"T1\", \"AE\", \"SAF\")", code, fixed = TRUE)))
  # a report's rows are part of its fingerprint
  expect_false(identical(tfl_ard_spec_hash(x, "T1", codelists = cl),
                         tfl_ard_spec_hash(x, "T1", codelists = cl[1L, ])))
  # checks
  d <- ad; d$population_id[2L] <- "SAF"
  expect_error(ad_spec(d, an), "not both", fixed = TRUE)
  d <- ad; d$subjects[2L] <- "adae_old"
  expect_error(ad_spec(d, an), "is not an analysis data above it", fixed = TRUE)
  # ARS: the subjects' own condition has no place
  x$analyses$purpose <- "PRIMARY OUTCOME MEASURE"
  un <- tfl_ars_unmapped(tfl_ars(x))
  expect_true("analysis_data$subjects" %in% un$item)

  skip_if_not_installed("cards")
  dir <- withr_tempdir()
  saveRDS(cards::ADSL, file.path(dir, "adsl.rds"))
  saveRDS(cards::ADAE, file.path(dir, "adae.rds"))
  a <- tfl_build_ard(x, dir = dir, output_id = "T1", save = FALSE, codelists = cl)
  o <- a[a$analysis_id == "OLD" & a$stat_name == "n", ]
  expect_identical(unique(vapply(o$variable_level, function(v) as.character(v[[1L]]), "")),
                   c("75+", "65-74"))
})

test_that("a data written as R (`code`): made by it, checked, in the fingerprint", {
  d <- ad_rows()
  d$code <- NA_character_
  # adae_ser written as R instead of its columns
  d$where[3L] <- NA
  d$derive[3L] <- NA
  d$code[3L] <- "out <- adae_teae[adae_teae$AESER == \"Y\", ]\nout$SER <- 1\nout"
  x <- ad_spec(d, an_rows())
  code <- tfl_ard_code(x, output_id = "T1", save = FALSE, part = "body")
  expect_true(any(code == paste0("adae_ser <- local({\n  out <- adae_teae[adae_teae$AESER == \"Y\", ]\n",
                                 "  out$SER <- 1\n  out\n})")))
  # in the sheet's order, after what it reads
  expect_lt(match("adae_teae <- subset(adae, USUBJID %in% pop_saf$USUBJID & TRTEMFL == \"Y\")", code),
            grep("^adae_ser <- local", code))
  # checked: R, and the columns that make a data left blank
  bad <- d; bad$code[3L] <- "subset(adae_teae,"
  expect_error(ad_spec(bad, an_rows()), "`code` does not read as R", fixed = TRUE)
  bad <- d; bad$where[3L] <- "AESER == \"Y\""
  expect_error(ad_spec(bad, an_rows()), "`code` makes the data itself; `where`", fixed = TRUE)
  # a code blank in every row keeps the fingerprint; a code counts
  plain <- ad_spec(ad_rows(), an_rows())
  blank <- ad_rows(); blank$code <- NA_character_
  expect_identical(tfl_ard_spec_hash(ad_spec(blank, an_rows()), "T1"), tfl_ard_spec_hash(plain, "T1"))
  d2 <- d; d2$code[3L] <- sub("1", "2", d$code[3L])
  expect_false(identical(tfl_ard_spec_hash(x, "T1"), tfl_ard_spec_hash(ad_spec(d2, an_rows()), "T1")))

  skip_if_not_installed("cards")
  dir <- withr_tempdir()
  saveRDS(cards::ADSL, file.path(dir, "adsl.rds"))
  saveRDS(cards::ADAE, file.path(dir, "adae.rds"))
  a <- tfl_build_ard(x, dir = dir, output_id = "T1", save = FALSE)
  # the same ARD as the columns made it
  b <- tfl_build_ard(plain, dir = dir, output_id = "T1", save = FALSE)
  key <- function(z) z[z$analysis_id == "SER", c("group1_level", "variable_level", "stat_name", "stat")]
  expect_identical(key(a), key(b))
})

