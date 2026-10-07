# An analysis written out as a custom analysis's code gives the same ARD

test_that("an analysis as R: the same ARD as its columns gave", {
  df <- function(...) as.data.frame(list(...), stringsAsFactors = FALSE)
  s <- list(
    study = df(key = "id", value = "USUBJID"),
    datasets = df(dataset = c("ADSL", "ADAE"), path = c("adsl.rds", "adae.rds")),
    populations = df(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analysis_data = df(output_id = "T1", data_id = c("adsl_saf", "adae_teae"), from = c("ADSL", "ADAE"),
                       population_id = c("SAF", "SAF"), where = c(NA, "TRTEMFL == \"Y\""),
                       add = c(NA, "TRT01A")),
    analyses = df(
      output_id = "T1", analysis_id = c("BIGN", "AGE", "AE"),
      method = c("cards::ard_tabulate", "continuous", "cards::ard_stack_hierarchical"),
      data = c("adsl_saf", "adsl_saf", "adae_teae"),
      by = c(NA, "TRT01A", "TRT01A"),
      variables = c("TRT01A", "AGE", "AEBODSYS | AEDECOD"),
      denominator = c(NA, NA, "adsl_saf")))
  x <- tfl_ard_spec(s)
  as_custom <- function(x, id) {
    v <- tfl_ard_as_custom(x, "T1", id)
    y <- x
    i <- match(id, y$analyses$analysis_id)
    y$analyses$method[i] <- v$method
    y$analyses$code[i] <- v$code
    y$analyses$formats[i] <- v$formats
    y$analyses$denominator[i] <- NA
    y$analyses$strata[i] <- NA
    y$analyses$args[i] <- NA
    tfl_ard_spec(unclass(y))
  }
  v <- tfl_ard_as_custom(x, "T1", "AE")
  expect_identical(v$method, "custom")
  expect_match(v$code, "cards::ard_stack_hierarchical(data", fixed = TRUE)
  expect_match(v$code, "denominator = adsl_saf", fixed = TRUE)
  # the row's formats and its method's defaults, as the column writes them
  y <- x
  y$analyses$formats[2L] <- "mean=xx.xx"
  expect_identical(tfl_ard_as_custom(y, "T1", "AGE")$formats, "mean=xx.xx")
  expect_error(tfl_ard_as_custom(x, "T1", "NONE"), "no analysis")

  skip_if_not_installed("cards")
  dir <- withr_tempdir()
  saveRDS(cards::ADSL, file.path(dir, "adsl.rds"))
  saveRDS(cards::ADAE, file.path(dir, "adae.rds"))
  key <- function(z, id) {
    z <- as.data.frame(z[z$analysis_id == id, ])
    z <- z[intersect(c("variable", "variable_level", "group1_level", "stat_name", "stat", "stat_fmt"), names(z))]
    z[] <- lapply(z, function(v) vapply(v, function(e) paste(format(e), collapse = "|"), ""))
    z[do.call(order, z), , drop = FALSE]
  }
  b <- tfl_build_ard(x, dir = dir, save = FALSE)
  for (id in c("BIGN", "AGE", "AE")) {
    y <- as_custom(x, id)
    a <- tfl_build_ard(y, dir = dir, save = FALSE)
    k1 <- key(a, id); k2 <- key(b, id)
    rownames(k1) <- rownames(k2) <- NULL
    expect_identical(k1, k2, info = id)
  }
})
