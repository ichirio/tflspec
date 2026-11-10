# Canon 1.38: every ARD analysis that takes a denominator passes it
# explicitly, and the default is the report's ADSL population (adsl_<pop>,
# the object GROUPN counts).  summary, disposition and ae_overview; the
# Missing row of summary and disposition.  Fictional data only.

ad_pops <- list(list(id = "SAF", label = "Safety Analysis Set"))
ad_codelists <- list(TRT01A = c("Drug X", "Control"))
ad_expand <- function(name, ...) {
  tfl_template_expand(name, c(list(population = "SAF"), list(...)),
                      codelists = ad_codelists, population = ad_pops)
}
ad_an <- function(x, id) Filter(function(a) identical(a$id, id), x$ard$analyses)[[1L]]
ad_sets <- function(x) vapply(x$ard$analysis_data, `[[`, "", "id")

test_that("summary (data ADSL), disposition and ae_overview carry denominator = adsl_saf", {
  s <- ad_expand("summary", categorical = "SEX", missing_row = FALSE)
  expect_identical(ad_an(s, "CAT")$denominator, "adsl_saf")
  expect_identical(ad_an(s, "CAT")$denominator, ad_an(s, "GROUPN")$data)
  d <- ad_expand("disposition")
  expect_identical(ad_an(d, "DISP")$denominator, "adsl_saf")
  ae <- ad_expand("ae_overview")
  fl <- Filter(function(a) identical(a$method, "subjects"), ae$ard$analyses)
  expect_length(fl, 6L)
  for (a in fl) expect_identical(a$denominator, "adsl_saf", info = a$id)
  expect_identical(ad_an(ae, "GROUPN")$data, "adsl_saf")
  # header_n "denominator" with no denominator is still "set"
  h <- ad_expand("ae_overview", header_n = "denominator")
  expect_identical(ad_an(h, "GROUPN")$data, "adsl_saf")
})

test_that("summary on records keeps the records set as the denominator", {
  x <- ad_expand("summary", data = "ADAE", categorical = "AEBODSYS")
  cat <- ad_an(x, "CAT")
  expect_identical(cat$data, "adae_saf")
  expect_identical(cat$denominator, "adae_saf")
  expect_false(any(grepl("_m$", ad_sets(x))))
  # a given denominator keeps working
  y <- ad_expand("summary", categorical = "SEX", denominator = 50)
  expect_identical(ad_an(y, "CAT")$denominator, 50L)
})

test_that("missing_row on summary and disposition: TRUE (default), FALSE, !unset", {
  for (tn in c("summary", "disposition")) {
    args <- if (identical(tn, "summary")) list(categorical = "SEX") else list()
    id <- if (identical(tn, "summary")) "CAT" else "DISP"
    on <- do.call(ad_expand, c(list(tn), args))
    expect_true("adsl_saf_m" %in% ad_sets(on), info = tn)
    expect_identical(ad_an(on, id)$data, "adsl_saf_m", info = tn)
    expect_identical(ad_an(on, id)$denominator, "adsl_saf", info = tn)
    expect_identical(do.call(ad_expand, c(list(tn), args, list(missing_row = TRUE))), on)
    for (v in list(FALSE, tfl_unset())) {
      off <- do.call(ad_expand, c(list(tn), args, list(missing_row = v)))
      expect_false("adsl_saf_m" %in% ad_sets(off), info = tn)
      expect_identical(ad_an(off, id)$data, "adsl_saf", info = tn)
      # (still over the analysis set: missing values are only left out of the counts)
      expect_identical(ad_an(off, id)$denominator, "adsl_saf", info = tn)
    }
    a <- tfl_tpl_args(tn)
    expect_identical(a$effective[a$param == "missing_row"], "TRUE", info = tn)
    expect_false(grepl("percentages stay over", a$doc[a$param == "missing_row"]), info = tn)
  }
  # demog keeps its own text
  a <- tfl_tpl_args("demog")
  expect_match(a$doc[a$param == "missing_row"], "percentages stay over the analysis set.", fixed = TRUE)
})

test_that("missing_row = TRUE given with a data other than ADSL stops; the default has no effect", {
  expect_error(ad_expand("summary", data = "ADAE", categorical = "AEBODSYS", missing_row = TRUE),
               "summary: missing_row is for data ADSL")
  x <- ad_expand("summary", data = "ADAE", categorical = "AEBODSYS", missing_row = FALSE)
  expect_false(any(grepl("_m$", ad_sets(x))))
})

test_that("stf_t02_demog still rejects missing_row", {
  expect_false("missing_row" %in% tfl_tpl_args("stf_t02_demog")$param)
  expect_error(ad_expand("stf_t02_demog", missing_row = FALSE), "there is no `missing_row` parameter")
})
