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
  output_id = "T1",
  data_id = c("adsl_sub", "adae_teae", "adae_ser", "adae_subj"),
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
  data = c("adsl_sub", "adae_teae", "adae_ser", NA),
  dataset = c(NA, NA, NA, "ADAE"),
  population_id = c(NA, NA, NA, "SAF"),
  where = c(NA, NA, "AESEV == \"SEVERE\"", "TRTEMFL == \"Y\""),
  by = c(NA, "TRT01A", "TRT01A", "TRTA"),
  variables = c("TRT01A", "AEBODSYS | AEDECOD", "AGEGR1", "AESEV"),
  denominator = c(NA, "adsl_sub", "adae_subj", NA))

test_that("the analysis data is made once, in order, for what a report reads", {
  x <- ad_spec(ad_rows(), an_rows())
  code <- tfl_ard_code(x, save = FALSE, part = "body")
  # in the sheet's order; the population itself when the data is its dataset
  # (a data made in one statement: its condition, derive, one row per ...)
  # (the population's columns replace one of the same name)
  at <- match(c("adsl_sub2 <- filter(adsl, SAFFL == \"Y\")",
                paste0("adae_teae <- adae |>\n",
                       "  select(-any_of(c(\"TRT01A\", \"AGEGR1\"))) |>\n",
                       "  inner_join(\n",
                       "    select(adsl_sub2, USUBJID, TRT01A, AGEGR1),\n",
                       "    by = \"USUBJID\",\n",
                       "    relationship = \"many-to-one\"\n",
                       "  ) |>\n",
                       "  filter(TRTEMFL == \"Y\")"),
                "adae_ser <- adae_teae |>\n  filter(AESER == \"Y\") |>\n  mutate(SER = 1)",
                "adae_subj <- distinct(adae_teae, USUBJID, .keep_all = TRUE)"), code)
  expect_false(anyNA(at))
  expect_false(is.unsorted(at))
  # an analysis's own condition on top, under a name of its own; the
  # denominator as it is; the analysis set is the data's
  expect_true(any(code == "adae_sub <- filter(adae_ser, AESEV == \"SEVERE\")"))
  expect_true(any(grepl("ard_ser <- adae_sub |>\n  mutate(\n    across(where(is.character), ~ na_if(.x, \"\")),\n    across(any_of(\"AGEGR1\") & where(is.factor), droplevels)\n  ) |>\n  ard_tabulate(",
                        code, fixed = TRUE)))
  expect_true(any(grepl("denominator = adae_subj,", code, fixed = TRUE)))
  expect_true(any(grepl("denominator = adsl_sub,", code, fixed = TRUE)))
  # formatted in the call (cards::ard_tabulate() takes fmt_fun)
  expect_true(any(grepl("  apply_fmt_fun() |>\n  tag_ard(report_id, \"SER\", population = \"SAF\")",
                        code, fixed = TRUE)))
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
  # a derive piece that is not `name = expression` (R's `|` inside
  # parentheses is one piece; a bare expression is not)
  d <- ad_rows(); d$derive[2L] <- "FLAG = ifelse(is.na(AESER) | AESER != \"Y\", 0, 1) | paste(AEDECOD)"
  bad(d, msg = "`derive` piece 'paste(AEDECOD)' does not read as `name = expression`")
  d <- ad_rows(); d$derive[2L] <- "FLAG = ifelse(is.na(AESER) | AESER != \"Y\", 0, 1)"
  expect_s3_class(ad_spec(d, an_rows()), "tfl_ard_spec")
  a <- an_rows(); a$data[2L] <- "adae_none"
  bad(an = a, msg = "data adae_none is not an analysis data of T1")
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
    output_id = "T1",
    data_id = c("adsl_old", "adae_old"), from = c("ADSL", "ADAE"),
    population_id = c("SAF", NA), subjects_in = c(NA, "adsl_old"),
    where = c("AGE >= 65", "TRTEMFL == \"Y\""), add = c(NA, "TRT01A"),
    derive = c("OLD = ifelse(AGE >= 75, \"75+\", \"65-74\")", NA),
    keep = c(NA, "TRT01A | AEBODSYS | AEDECOD"))
  an <- ad_df(output_id = c("T1", "T1"), analysis_id = c("OLD", "AE"),
              method = c("cards::ard_tabulate", "cards::ard_stack_hierarchical"),
              data = c("adsl_old", "adae_old"), by = "TRT01A",
              variables = c("OLD", "AEBODSYS | AEDECOD"),
              denominator = c(NA, "adsl_old"))
  x <- ad_spec(ad, an)
  cl <- ad_df(output_id = c("T1", "T1", "T1"), variable = c("SEX", "OLD", "OLD"),
              value = c("F", "75+", "65-74"), order = c(1, 1, 2))
  code <- tfl_ard_code(x, output_id = "T1", save = FALSE, part = "body", codelists = cl)
  # the numerator joined to the denominator's subjects (their columns), then
  # its records
  expect_true(any(code == paste0(
    "adae_old <- adae |>\n",
    "  select(-any_of(\"TRT01A\")) |>\n",
    "  inner_join(\n",
    "    select(adsl_old, USUBJID, TRT01A),\n",
    "    by = \"USUBJID\",\n",
    "    relationship = \"many-to-one\"\n",
    "  ) |>\n",
    "  filter(TRTEMFL == \"Y\") |>\n",
    "  select(USUBJID, TRT01A, AEBODSYS, AEDECOD) |>\n",
    "  set_levels(OLD = cl_old)")))
  # the derived column a factor: the report's own code list rows count
  # (the analysis set it is made from, used for nothing else: in it, its
  # condition and the report's: two filter() calls)
  expect_true(any(code == paste0(
    "adsl_old <- adsl |>\n",
    "  filter(SAFFL == \"Y\") |>\n",
    "  filter(AGE >= 65) |>\n",
    "  mutate(OLD = ifelse(AGE >= 75, \"75+\", \"65-74\")) |>\n",
    "  set_levels(OLD = cl_old)")))
  expect_true("cl_old <- c(\"75+\", \"65-74\")" %in% code)
  expect_false(any(startsWith(code, "adsl_sub <- ")))
  # only what the analyses read: no SEX
  expect_false(any(grepl("SEX =", code, fixed = TRUE)))
  # the study's program of its one report: the same
  expect_identical(tfl_ard_code(x, save = FALSE, part = "body", codelists = cl), code)
  # the ARD's analysis set: the one the subjects are of
  expect_true(any(grepl("tag_ard(report_id, \"AE\", population = \"SAF\")", code, fixed = TRUE)))
  # a report's rows are part of its fingerprint
  expect_false(identical(tfl_ard_spec_hash(x, "T1", codelists = cl),
                         tfl_ard_spec_hash(x, "T1", codelists = cl[1:2, ])))
  # checks
  d <- ad; d$population_id[2L] <- "SAF"
  expect_error(ad_spec(d, an), "not both", fixed = TRUE)
  d <- ad; d$subjects_in[2L] <- "adae_old"
  expect_error(ad_spec(d, an), "is not an analysis data above it", fixed = TRUE)
  # ARS: the subjects' own condition has no place
  x$analyses$purpose <- "PRIMARY OUTCOME MEASURE"
  un <- tfl_ars_unmapped(tfl_ars(x))
  expect_true("analysis_data$subjects_in" %in% un$item)

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
  expect_true(any(code == "out <- adae_teae[adae_teae$AESER == \"Y\", ]"))
  expect_true(any(code == "adae_ser <- out"))
  expect_false(any(grepl("local(", code, fixed = TRUE)))
  # in the sheet's order, after what it reads
  expect_lt(grep("^adae_teae <- adae", code),
            grep("^adae_ser <- out", code))
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


test_that("an analysis data is a report's: the same name, another meaning elsewhere", {
  # T1's adsl_sub is the safety set; T2's is the safety set over 65
  ad <- ad_df(output_id = c("T1", "T2"), data_id = "adsl_sub", from = "ADSL",
              population_id = "SAF", where = c(NA, "AGE >= 65"))
  an <- ad_df(output_id = c("T1", "T2"), analysis_id = "AGE",
              method = "cards::ard_summary", data = "adsl_sub", variables = "AGE")
  x <- ad_spec(ad, an)
  c1 <- tfl_ard_code(x, output_id = "T1", save = FALSE, part = "body")
  c2 <- tfl_ard_code(x, output_id = "T2", save = FALSE, part = "body")
  expect_true(any(grepl("^adsl_sub <- filter\\(adsl, SAFFL == ", c1)))
  expect_false(any(grepl("AGE >= 65", c1, fixed = TRUE)))
  expect_true(any(grepl("AGE >= 65", c2, fixed = TRUE)))
  # the study's program: one part a report, each making its own adsl_sub
  all <- tfl_ard_code(x, save = FALSE, part = "body")
  expect_identical(sum(all == "# ---- T1 ----"), 1L)
  expect_identical(sum(all == "# ---- T2 ----"), 1L)
  expect_identical(sum(grepl("^adsl_sub <- ", all)), 2L)
  expect_lt(which(all == "# ---- T1 ----"), which(all == "# ---- T2 ----"))
  expect_identical(sum(all == "ards <- list()"), 1L)
  expect_silent(parse(text = all))
  # the fingerprints: each report's own rows; T2's changes when its adsl_sub does
  h1 <- tfl_ard_spec_hash(x, "T1")
  h2 <- tfl_ard_spec_hash(x, "T2")
  y <- x
  y$analysis_data$where[2L] <- "AGE >= 75"
  expect_identical(tfl_ard_spec_hash(y, "T1"), h1)
  expect_false(identical(tfl_ard_spec_hash(y, "T2"), h2))
})

test_that("a report's analysis data name only its own rows; a blank output_id is an error", {
  ad <- ad_df(output_id = c("T1", "T2"), data_id = c("adsl_sub", "adae_teae"),
              from = c("ADSL", "ADAE"), population_id = c("SAF", NA),
              subjects_in = c(NA, "adsl_sub"), where = c(NA, "TRTEMFL == \"Y\""))
  an <- ad_df(output_id = "T2", analysis_id = "AE", method = "cards::ard_tabulate",
              data = "adae_teae", variables = "AEDECOD")
  expect_error(ad_spec(ad, an), "`subjects_in` adsl_sub is not an analysis data above it in the report",
               fixed = TRUE)
  # an analysis reads another report's data
  ad2 <- ad_df(output_id = "T1", data_id = "adsl_sub", from = "ADSL", population_id = "SAF")
  an2 <- ad_df(output_id = "T2", analysis_id = "AGE", method = "cards::ard_summary",
               data = "adsl_sub", variables = "AGE")
  expect_error(ad_spec(ad2, an2), "data adsl_sub is not an analysis data of T2 (another report's",
               fixed = TRUE)
  # the same name twice in a report
  ad3 <- ad_df(output_id = "T1", data_id = c("adsl_sub", "adsl_sub"), from = "ADSL",
               population_id = "SAF")
  expect_error(ad_spec(ad3, an_rows()[1L, ]), "`data_id` repeated in the report", fixed = TRUE)
  # no report: said, with what to do
  ad4 <- ad_df(output_id = NA_character_, data_id = "adsl_sub", from = "ADSL", population_id = "SAF")
  expect_error(ad_spec(ad4, an_rows()[1L, ]), "`output_id` is blank", fixed = TRUE)
})

test_that("a data that keeps columns keeps the ones its analyses read", {
  ad <- ad_rows()
  ad$keep <- NA
  ad$keep[2L] <- "AEDECOD"
  code <- tfl_ard_code(ad_spec(ad, an_rows()), output_id = "T1", save = FALSE,
                       part = "body")
  # AE reads adae_teae by TRT01A, its variables AEBODSYS | AEDECOD: kept
  # after the ones asked for
  expect_true(any(grepl("  select(USUBJID, AEDECOD, TRT01A, AEBODSYS)", code, fixed = TRUE)))
})

test_that("asked twice, the fingerprint, the analysis data sheet and the code lists are the same (memos)", {
  x <- ad_spec(ad_rows(), an_rows())
  expect_identical(tfl_ard_spec_hash(x, "T1"), tfl_ard_spec_hash(x, "T1"))
  # the analysis data sheet normalized once while it is the same; a change
  # is seen, and the first sheet again
  s1 <- .adata_sheet(x)
  expect_identical(.adata_sheet(x), s1)
  y <- x
  y$analysis_data$where[2L] <- "AGE >= 75"
  s2 <- .adata_sheet(y)
  expect_false(identical(s2, s1))
  expect_identical(s2$where[2L], "AGE >= 75")
  expect_identical(.adata_sheet(x), s1)
  # bad code lists fail every call; good ones are checked once
  bad <- data.frame(variable = "SEX", value = "M", stringsAsFactors = FALSE)
  expect_error(.codelist_levels(bad, "T1"), "output_id")
  expect_error(.codelist_levels(bad, "T1"), "output_id")
  cl <- data.frame(output_id = "T1", variable = "SEX", value = c("M", "F"),
                   label = c("Male", "Female"), order = c(1, 2), stringsAsFactors = FALSE)
  lv <- .codelist_levels(cl, "T1")
  expect_identical(lv$SEX, c(M = "Male", F = "Female"))
  expect_identical(.codelist_levels(cl, "T1"), lv)
  expect_error(.codelist_levels(bad, "T1"), "output_id")
})

test_that("a data is made from, population, subjects_in, where, derive, keep, distinct, in that order", {
  ad <- ad_df(
    output_id = "T1", data_id = c("adsl_sub", "adae_x"), from = c("ADSL", "ADAE"),
    population_id = c("SAF", NA), subjects_in = c(NA, "adsl_sub"),
    where = c(NA, "TRTEMFL == \"Y\""),
    derive = c(NA, "SERF = AESER == \"Y\""),
    keep = c(NA, "AEDECOD | SERF"), distinct = c(NA, "USUBJID | SERF"))
  an <- ad_df(output_id = "T1", analysis_id = "AE", method = "cards::ard_tabulate",
              data = "adae_x", variables = "SERF")
  code <- tfl_ard_code(ad_spec(ad, an), output_id = "T1", save = FALSE, part = "body")
  i <- grep("^adae_x <-", code)
  expect_length(i, 1L)
  txt <- strsplit(code[i], "
", fixed = TRUE)[[1L]]
  pos <- function(p) grep(p, txt, fixed = TRUE)[1L]
  # the derived column is made before the columns are kept, and kept
  keep_at <- grep("select(USUBJID", txt, fixed = TRUE)
  expect_length(keep_at, 1L)
  expect_true(pos("filter(") < pos("mutate("))
  expect_true(pos("mutate(") < keep_at)
  expect_true(keep_at < pos("distinct("))
  expect_match(txt[keep_at], "SERF", fixed = TRUE)
})

test_that("an analysis_data sheet with a filled label column stops, an empty one is dropped", {
  ad <- ad_rows()
  ad$label <- c("Safety set", NA, NA, NA)
  expect_error(ad_spec(ad, an_rows()),
               "T1: data row `adsl_sub`: `label` is no longer a column of the analysis_data sheet",
               fixed = TRUE)
  ad$label <- c("", NA, " ", NA)
  x <- ad_spec(ad, an_rows())
  expect_false("label" %in% names(x$analysis_data))
  expect_identical(names(.adata_sheet(x)), .ard_spec_sheets$analysis_data)
  expect_false("label" %in% .ard_spec_sheets$analysis_data)
})

test_that("then: R piped onto the end of a data after its keys; read as R; not with code", {
  d <- ad_rows()
  d$then <- c(NA, NA, "dplyr::arrange(USUBJID) |> dplyr::slice_head(n = 1, by = USUBJID)", NA)
  x <- ad_spec(d, an_rows())
  code <- tfl_ard_code(x, output_id = "T1", save = FALSE, part = "body")
  i <- grep("^adae_ser <- adae_teae", code)
  expect_length(i, 1L)
  # after the row's own steps, a step each, as written
  at <- vapply(c("AESER == \"Y\"", "SER = 1", "arrange(USUBJID)", "slice_head(n = 1, by = USUBJID)"),
               function(s) regexpr(s, code[i], fixed = TRUE)[[1L]], 1L)
  expect_true(all(at > 0L))
  expect_false(is.unsorted(at))
  expect_match(code[i], "arrange\\(USUBJID\\) \\|>\n  (dplyr::)?slice_head\\(")
  # a row of only a then: its one step piped onto the data it is made from
  d1 <- ad_rows()
  d1$where[3L] <- NA
  d1$derive[3L] <- NA
  d1$then <- c(NA, NA, "(\\(z) z[z$AESER == \"Y\", ])()", NA)
  code1 <- tfl_ard_code(ad_spec(d1, an_rows()), output_id = "T1", save = FALSE, part = "body")
  expect_true(any(grepl("^adae_ser <- adae_teae \\|> \\(", code1)))
  # checked: R, and not with code (code makes the whole data)
  bad <- d
  bad$then[3L] <- "dplyr::filter("
  expect_error(ad_spec(bad, an_rows()), "`then` does not read as R", fixed = TRUE)
  bad <- d
  bad$where[3L] <- NA
  bad$derive[3L] <- NA
  bad$code <- c(NA, NA, "adae_teae", NA)
  expect_error(ad_spec(bad, an_rows()), "`code` makes the data itself; `then`", fixed = TRUE)
  # a then blank in every row keeps the fingerprint; a then counts
  plain <- ad_spec(ad_rows(), an_rows())
  blank <- ad_rows()
  blank$then <- NA_character_
  expect_identical(tfl_ard_spec_hash(ad_spec(blank, an_rows()), "T1"), tfl_ard_spec_hash(plain, "T1"))
  expect_false(identical(tfl_ard_spec_hash(x, "T1"), tfl_ard_spec_hash(plain, "T1")))

  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr", "1.1.0")
  dir <- tempfile("then-ard")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  saveRDS(cards::ADSL, file.path(dir, "adsl.rds"))
  saveRDS(cards::ADAE, file.path(dir, "adae.rds"))
  a <- tfl_build_ard(x, dir = dir, output_id = "T1", save = FALSE)
  # SER counts a record a subject now: its n the subjects with a serious AE
  ser <- a[a$analysis_id == "SER" & a$stat_name == "n", ]
  adae <- cards::ADAE
  adsl <- cards::ADSL
  teae <- adae[adae$TRTEMFL == "Y" & adae$USUBJID %in% adsl$USUBJID[adsl$SAFFL == "Y"], ]
  first <- teae[teae$AESER == "Y", ]
  first <- first[order(first$USUBJID), ]
  first <- first[!duplicated(first$USUBJID) & first$AESEV == "SEVERE", ]
  expect_identical(sum(as.integer(unlist(ser$stat))), nrow(first))
})
