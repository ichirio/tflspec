siera_df <- function(...) {
  rows <- list(...)
  cols <- unique(unlist(lapply(rows, names)))
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) as.character(r[[cn]] %||% NA), "")),
    stringsAsFactors = FALSE)
}

siera_spec <- function() {
  s <- list(
    study = siera_df(list(key = "id", value = "USUBJID")),
    datasets = siera_df(list(dataset = "ADSL", path = "adam/ADSL.rds"),
                        list(dataset = "ADAE", path = "adam/ADAE.rds")),
    populations = siera_df(list(population_id = "SAF", dataset = "ADSL",
                                where = "SAFFL == \"Y\"")),
    analyses = siera_df(
      list(output_id = "DM", analysis_id = "GROUPN", method = "categorical",
           population_id = "SAF", variables = "TRT01A"),
      list(output_id = "DM", analysis_id = "AGE", method = "continuous",
           population_id = "SAF", by = "TRT01A", variables = "AGE"),
      list(output_id = "DM", analysis_id = "CAT", method = "categorical",
           population_id = "SAF", by = "TRT01A",
           variables = "AGEGR1 | SEX | RACE"),
      list(output_id = "DM", analysis_id = "PCHI", method = "chisq",
           population_id = "SAF", by = "TRT01A", variables = "SEX"),
      list(output_id = "DM", analysis_id = "PT", method = "ttest",
           population_id = "SAF", by = "TRT01A", variables = "AGE"),
      list(output_id = "T-AE", analysis_id = "TEAE", method = "hierarchical",
           dataset = "ADAE", population_id = "SAF",
           where = "TRTEMFL == \"Y\"", by = "TRTA",
           variables = "AEBODSYS | AEDECOD", args = "over_variables = TRUE"),
      list(output_id = "CI", analysis_id = "GROUPN", method = "categorical",
           population_id = "SAF", variables = "TRT01A"),
      list(output_id = "CI", analysis_id = "SEX", method = "proportion_ci",
           population_id = "SAF", by = "TRT01A", variables = "SEX",
           args = "method = \"wilson\""),
      list(output_id = "CI", analysis_id = "RACE", method = "proportion_ci",
           population_id = "SAF", by = "TRT01A", variables = "RACE",
           args = "method = \"wilson\"")))
  s$analyses$purpose <- "SECONDARY OUTCOME MEASURE"
  for (n in names(.ard_spec_sheets)) {
    for (c in .ard_spec_sheets[[n]]) {
      if (!c %in% names(s[[n]])) s[[n]][[c]] <- NA_character_
    }
    s[[n]] <- s[[n]][.ard_spec_sheets[[n]]]
  }
  tfl_ard_spec(s)
}

test_that("the siera profile is runnable metadata and still CDISC ARS", {
  ars <- tfl_ars(siera_spec(), profile = "siera")
  expect_identical(attr(ars, "profile"), "siera")
  ids <- vapply(ars$analyses, `[[`, "", "id")
  # siera makes R names of ids
  expect_false(any(grepl("[^A-Za-z0-9_]", c(ids, vapply(ars$outputs, `[[`,
                                                         "", "id")))))
  expect_true("T_AE" %in% vapply(ars$outputs, `[[`, "", "id"))
  # each output's subject count first
  lc <- ars$mainListOfContents$contentsList$listItems
  firsts <- vapply(lc, function(z) z$sublist$listItems[[1L]]$analysisId, "")
  expect_true(all(grepl("GROUPN", firsts)))
  # the list of outputs siera reads
  expect_identical(
    vapply(ars$otherListsOfContents[[1L]]$contentsList$listItems, `[[`, "",
           "outputId"), c("DM", "T_AE", "CI"))
  # every method runs: a code template for siera
  for (m in ars$methods) {
    expect_identical(m$codeTemplate$context, "R (siera)")
    expect_false(grepl("opid", m$codeTemplate$code))
  }
  # a proportion with its CI is an analysis of the variable, by arm only
  ci <- Filter(function(a) a$id == "An_CI_SEX", ars$analyses)[[1L]]
  expect_identical(ci$variable, "SEX")
  expect_length(ci$orderedGroupings, 1L)
  # what siera cannot run is left out, and said
  expect_false("An_DM_PT" %in% ids)
  un <- tfl_ars_unmapped(ars)
  expect_true(any(un$where == "DM / PT" & un$item == "method"))
  # a hierarchy's second level: the pairs in the data
  m <- Filter(function(m) m$id == "Mth_nested3", ars$methods)
  expect_length(m, 1L)
  expect_match(m[[1L]]$codeTemplate$code, "semi_join", fixed = TRUE)
  # nothing in the way of siera, nor of CDISC
  expect_identical(nrow(tfl_check_ars(ars, schema = FALSE)), 0L)
  skip_if_not_installed("jsonvalidate")
  expect_identical(nrow(tfl_check_ars(ars)), 0L)
})

test_that("what siera needs beyond the model is checked", {
  sp <- siera_spec()
  sp$analyses <- sp$analyses[sp$analyses$output_id == "CI" &
                               sp$analyses$analysis_id != "RACE", ]
  sp$populations$where <- "SAFFL == \"Y\" & AGE >= 18"
  ars <- tfl_ars(sp, profile = "siera")
  ck <- tfl_check_ars(ars, schema = FALSE)
  expect_true(any(ck$field == "analyses" & grepl("three", ck$problem)))
  expect_true(any(ck$field == "condition"))
  # the same model checked as cdisc has no such rows
  expect_false(any(tfl_check_ars(ars, schema = FALSE,
                                 profile = "cdisc")$field == "analyses"))
})

# every number of one ARD, by its groups, variable, level and statistic
ard_numbers <- function(d) {
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  g <- sort(grep("^group[0-9]+$", names(d), value = TRUE))
  lv <- function(v) vapply(v, function(z) {
    z <- unlist(z)
    if (!length(z)) NA_character_ else as.character(z[1L])
  }, "")
  gl <- vapply(seq_len(nrow(d)), function(i) {
    k <- g[!is.na(vapply(g, function(k) as.character(d[[k]][i]), ""))]
    paste(sort(vapply(k, function(k) paste0(d[[k]][i], "=",
                                            lv(d[[paste0(k, "_level")]][i])),
                      "")), collapse = ";")
  }, "")
  num <- vapply(d$stat, function(z) {
    z <- unlist(z)
    if (is.numeric(z) && length(z)) signif(z[1L], 8) else NA_real_
  }, 0)
  out <- data.frame(key = paste(gl, d$variable, lv(d$variable_level),
                                d$stat_name, sep = "|"),
                    stat = d$stat_name, value = num,
                    stringsAsFactors = FALSE)
  out[!is.na(out$value) & !duplicated(out$key), ]
}

test_that("round trip: siera makes the same numbers from the ARS as tflspec from the spec", {
  skip_on_cran()
  skip_if_not_installed("siera", "0.5.6")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("dplyr")
  sp <- siera_spec()
  adsl <- as.data.frame(cards::ADSL)
  adsl$TRTA <- adsl$TRT01A
  adae <- as.data.frame(cards::ADAE)
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "adam"))
  saveRDS(adsl, file.path(dir, "adam", "ADSL.rds"))
  saveRDS(adae, file.path(dir, "adam", "ADAE.rds"))
  ard_tfl <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  ars <- tfl_ars(sp, profile = "siera")
  ard_ars <- suppressMessages(suppressWarnings(
    tfl_ars_ard(ars, list(ADSL = adsl, ADAE = adae))))
  # the spec's output ids come back
  expect_setequal(unique(ard_ars$OutputId), c("DM", "T-AE", "CI"))
  for (o in c("DM", "T-AE", "CI")) {
    t <- ard_tfl[ard_tfl$output_id == o, ]
    # "any event": cards names it by its overall row, the template by a flag
    any <- t$variable == "..ard_hierarchical_overall.."
    t$variable[any] <- ".flag_"
    t$variable_level[any] <- list("Y")
    a <- ard_numbers(t)
    b <- ard_numbers(ard_ars[ard_ars$OutputId == o, ])
    m <- merge(a, b, by = "key", suffixes = c(".tfl", ".ars"))
    # every number siera makes is tflspec's, and the same
    expect_identical(nrow(m), nrow(b), label = paste(o, "numbers siera made"))
    expect_true(all(abs(m$value.tfl - m$value.ars) < 1e-8), label = o)
    # and siera makes every count and percentage of an analysis tflspec
    # makes (the subject counts give n, not p)
    cnt <- a[a$stat %in% c("n", "p") & !(a$stat == "p" & !grepl("=", a$key) &
                                           grepl("^\\|TRT", a$key)), ]
    expect_true(all(cnt$key %in% b$key),
                label = paste(o, "counts and percentages"))
  }
  # the cdisc profile of the same spec is a valid ARS too
  expect_identical(nrow(tfl_check_ars(tfl_ars(sp), schema = FALSE)), 0L)
})

test_that("tfl_ars_ard() needs the siera profile and the data it reads", {
  skip_if_not_installed("siera")
  expect_error(tfl_ars_ard(tfl_ars(siera_spec()), list()), "siera")
  ars <- tfl_ars(siera_spec(), profile = "siera")
  expect_error(tfl_ars_ard(ars, list(ADSL = data.frame(USUBJID = "1"))),
               "ADAE")
})
