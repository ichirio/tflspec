# The stratified Wilson interval of a proportion through a SPEC
# (cardx::ard_categorical_ci(method = "strat_wilson"), the keyword
# proportion_ci): the stratum made a factor by the generated code, the
# settings cardx stops on or drops said when the definition is read, and
# the weights' count judged by the stratum's code list.

strat_row <- function(args = NULL, strata = NULL) {
  list(method = "proportion_ci", population_id = "SAF", by = "TRT01A",
       variables = "SEX", strata = strata, args = args)
}

strat_warnings <- function(r) {
  w <- character()
  withCallingHandlers(exact_spec(r), warning = function(e) {
    w <<- c(w, conditionMessage(e))
    invokeRestart("muffleWarning")
  })
  paste(w, collapse = "\n")
}

test_that("a stratified Wilson row's stratum is made a factor, and its numbers are cardx's", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("dplyr")
  r <- strat_row("method = \"strat_wilson\"", "AGEGR1")
  sp <- exact_spec(r)
  code <- tfl_ard_code(sp, save = FALSE)
  expect_true(any(grepl("mutate(adsl_sub, AGEGR1 = as.factor(AGEGR1))", code,
                        fixed = TRUE)))
  # (since tflspec#867 the program's ARD temporaries start with a dot)
  expect_true(any(grepl(".ard_a <- .ard_a_data |>", code, fixed = TRUE)))
  expect_true(any(grepl("strata = AGEGR1,", code, fixed = TRUE)))

  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  got <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  expect_false(any(lengths(got$error) > 0L))
  d <- dplyr::filter(adam$ADSL, SAFFL == "Y")
  d$AGEGR1 <- factor(d$AGEGR1)
  want <- cardx::ard_categorical_ci(d, by = TRT01A, variables = SEX,
                                    strata = AGEGR1, method = "strat_wilson")
  key <- function(a) {
    a <- as.data.frame(a)
    a <- a[a$stat_name %in% c("estimate", "conf.low", "conf.high"), ]
    stats::setNames(unlist(a$stat), paste(unlist(a$group1_level),
                                          unlist(a$variable_level), a$stat_name))
  }
  g <- key(got)
  w <- key(want)
  expect_length(g, 18L)
  expect_equal(g[names(w)], w)
  # (the character column cardx itself stops on: no numbers)
  bad <- cardx::ard_categorical_ci(dplyr::filter(adam$ADSL, SAFFL == "Y"),
                                   by = TRT01A, variables = SEX,
                                   strata = AGEGR1, method = "strat_wilson")
  expect_match(unique(unlist(bad$error)), "must be class <factor>")
})

test_that("a stratified Wilson row with a code list keeps the list's factor", {
  r <- list(analysis_id = "A", method = "proportion_ci", strata = "AGEGR1",
            args = "method = \"strat_wilsoncc\", weights = c(0.2, 0.3, 0.5)")
  expect_identical(.item_strat_factor(as.data.frame(r)), "AGEGR1 = as.factor(AGEGR1)")
  # (as.factor() keeps a factor and its levels as they are)
  f <- factor(c("b", "a"), levels = c("b", "a", "c"))
  expect_identical(as.factor(f), f)
  # no step for another method, no stratum, two strata, or another function
  none <- function(...) .item_strat_factor(as.data.frame(utils::modifyList(r, list(...))))
  expect_identical(none(args = "method = \"wilson\""), character())
  expect_identical(none(args = NA_character_), character())
  expect_identical(none(strata = NA_character_), character())
  expect_identical(none(strata = "AGEGR1 | SEX"), character())
  expect_identical(none(method = "continuous"), character())
  # the stratum written in args, by name or in quotes
  expect_identical(none(strata = NA_character_,
                        args = "method = \"strat_wilson\", strata = \"STRAT1R\""),
                   "STRAT1R = as.factor(STRAT1R)")
  expect_identical(none(method = "cardx::ard_categorical_ci", strata = NA_character_,
                        args = "method = \"strat_wilson\", strata = STRAT1R"),
                   "STRAT1R = as.factor(STRAT1R)")
})

test_that("the stratum's settings cardx stops on or drops are said when the definition is read", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  # a stratum with another method: the analysis fails
  w <- strat_warnings(strat_row("method = \"wilson\"", "AGEGR1"))
  expect_match(w, "T / A: ard_categorical_ci\\(method = \"wilson\"\\) has a stratum \\(AGEGR1\\)")
  expect_match(w, "object 'AGEGR1' not found", fixed = TRUE)
  # (cardx's default method, waldcc, the same)
  expect_match(strat_warnings(strat_row(NULL, "AGEGR1")),
               "method = \"waldcc\"\\) has a stratum")
  # weights / max.iterations with another method: dropped
  w <- strat_warnings(strat_row("method = \"wald\", weights = c(0.5, 0.5), max.iterations = 20"))
  expect_match(w, "ard_categorical_ci\\(method = \"wald\"\\) drops `weights` and `max.iterations` with no warning")
  # a stratified method with no stratum, or two
  expect_match(strat_warnings(strat_row("method = \"strat_wilsoncc\"")),
               "strat_wilsoncc\"\\) needs its stratum")
  w <- strat_warnings(strat_row("method = \"strat_wilson\"", "AGEGR1 | SEX"))
  expect_match(w, "takes one stratum column; got AGEGR1, SEX")
  expect_match(w, "interaction(AGEGR1, SEX, sep = \" / \")", fixed = TRUE)
  # what is right gives no warning
  expect_identical(strat_warnings(strat_row("method = \"strat_wilson\"", "AGEGR1")), "")
  expect_identical(strat_warnings(strat_row(
    "method = \"strat_wilson\", strata = AGEGR1, weights = c(0.2, 0.3, 0.5)")), "")
  expect_identical(strat_warnings(strat_row("method = \"wilson\"")), "")
  # a method held in an object of the program is the program's
  expect_identical(strat_warnings(strat_row("method = ci_method", "AGEGR1")), "")
})

test_that("the weights' count is judged by the stratum's code list", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  cl <- data.frame(output_id = "T", variable = "AGEGR1",
                   value = c("<65", "65-80", ">80"), order = 1:3,
                   stringsAsFactors = FALSE)
  code <- function(args) {
    tfl_ard_code(exact_spec(strat_row(args, "AGEGR1")), save = FALSE, codelists = cl)
  }
  expect_warning(code("method = \"strat_wilson\", weights = c(0.5, 0.5)"),
                 "T / A: ard_categorical_ci\\(weights = \\) has 2 weights, but the stratum AGEGR1 has 3 values in its code list \\(<65, 65-80, >80\\)")
  expect_no_warning(code("method = \"strat_wilson\", weights = c(0.2, 0.3, 0.5)"))
  # weights held in an object, or estimated, are not judged
  expect_no_warning(code("method = \"strat_wilson\", weights = w"))
  expect_no_warning(code("method = \"strat_wilson\""))

  # why: two weights for three strata are recycled, and the limits are wrong
  skip_if_not_installed("dplyr")
  d <- dplyr::filter(as.data.frame(cards::ADSL), SAFFL == "Y")
  d$AGEGR1 <- factor(d$AGEGR1)
  lim <- function(w) {
    a <- cardx::ard_categorical_ci(d, by = TRT01A, variables = SEX, strata = AGEGR1,
                                   method = "strat_wilson", weights = w)
    list(low = unlist(a$stat[a$stat_name == "conf.low"])[1L],
         est = unlist(a$stat[a$stat_name == "estimate"])[1L],
         warning = unique(unlist(a$warning)))
  }
  bad <- lim(c(0.5, 0.5))
  expect_gt(bad$low, bad$est)
  expect_match(bad$warning, "not a multiple of shorter object length")
  good <- lim(c(0.2, 0.3, 0.5))
  expect_lt(good$low, good$est)
})
