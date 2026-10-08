# The formats of an ARD spec in the cards call (fmt_fun) give the stat_fmt
# that formatting after the call (.fmt(), the route of every analysis
# before) gives: the same text, row by row.

fmt_spec <- function(rows, populations = NULL) {
  S <- .ard_spec_sheets
  tfl_ard_spec(list(
    study = exact_sheet(list(list(key = "id", value = "USUBJID")), S$study),
    datasets = exact_sheet(list(list(dataset = "ADSL", path = "adam/ADSL.rds"),
                                list(dataset = "ADAE", path = "adam/ADAE.rds")),
                           S$datasets),
    populations = exact_sheet(list(list(population_id = "SAF", dataset = "ADSL",
                                        where = "SAFFL == \"Y\"")), S$populations),
    analyses = exact_sheet(lapply(rows, function(r) {
      r$output_id <- r$output_id %||% "T"
      if (is.null(r$parent)) r$population_id <- r$population_id %||% "SAF"
      r
    }), S$analyses)))
}

# every row's stat_fmt as text, by its keys
fmt_text <- function(a) {
  a <- as.data.frame(a)
  one <- function(v) vapply(v, function(z) paste(format(unlist(z)), collapse = "|"), "")
  g <- sort(grep("^group[0-9]+(_level)?$", names(a), value = TRUE))
  key <- do.call(paste, c(lapply(c("analysis_id", g, "variable", "variable_level",
                                   "context", "stat_name"),
                                 function(k) one(a[[k]])), sep = "|"))
  stats::setNames(one(a$stat_fmt), key)
}

# the same spec with every format after the call
fmt_after <- function(sp, dir, ...) {
  testthat::local_mocked_bindings(.takes_fmt_fun = function(fn) FALSE,
                                  .package = "tflspec")
  code <- tfl_ard_code(sp, save = FALSE, ...)
  expect_false(any(!startsWith(code, "#") & grepl("~ .fmt", code, fixed = TRUE)))
  expect_false(any(grepl("^ards.*apply_fmt_fun", code)))
  build_quiet(sp, dir, ...)
}

# (ard_hierarchical() warns of ADAE's several records a subject)
build_quiet <- function(sp, dir, ...) {
  suppressMessages(suppressWarnings(tfl_build_ard(sp, dir = dir, save = FALSE, ...)))
}

fmt_rows <- list(
  list(analysis_id = "AGE", method = "continuous", by = "TRT01A",
       variables = "AGE | BMIBL", statistics = "N | mean | sd | median | min | max | cv",
       formats = "mean=xx.xx | BMIBL:sd=3 | BMIBL:min=xx.x"),
  list(analysis_id = "SEX", method = "categorical", by = "TRT01A",
       variables = "SEX | AGEGR1", formats = "AGEGR1:p=xx.xx%"),
  list(analysis_id = "MISS", method = "missing", by = "TRT01A", variables = "BMIBL"),
  list(analysis_id = "F", method = "dichotomous", by = "TRT01A", variables = "SEX",
       args = "value = list(SEX = \"F\")", formats = "p=xx%"),
  list(analysis_id = "SER", method = "subjects", dataset = "ADAE",
       where = "AESER == \"Y\"", by = "TRT01A", variables = "ANYSER",
       formats = "ANYSER:p=2"),
  list(analysis_id = "TT", method = "ttest", by = "SEX", variables = "AGE"),
  list(analysis_id = "CI", method = "mean_ci", by = "TRT01A", variables = "AGE"),
  list(analysis_id = "OWN", method = "categorical", by = "TRT01A",
       variables = "SEX", args = "fmt_fun = list(p = 3L)"),
  list(analysis_id = "POST", method = "continuous", by = "TRT01A",
       variables = "AGE", post = "dplyr::filter(stat_name != \"N\")"),
  list(analysis_id = "AE", method = "hierarchical", dataset = "ADAE",
       by = "TRTA", variables = "AEBODSYS | AEDECOD", formats = "p=xx.xx%"),
  # a stack: the by counts and the total N are the stack's own
  list(analysis_id = "DEMO", method = "cards::ard_stack", by = "TRT01A",
       args = ".total_n = TRUE", formats = "p=xx.xx% | n=2"),
  list(analysis_id = "S_CONT", parent = "DEMO", method = "continuous",
       variables = "AGE | BMIBL", statistics = "N | mean | sd",
       formats = "mean=xx.xx | sd=3"),
  list(analysis_id = "S_CAT", parent = "DEMO", method = "categorical",
       variables = "SEX", formats = "SEX:p=xx.xxx%"),
  list(analysis_id = "DEMO2", method = "cards::ard_stack", by = "TRT01A",
       args = ".missing = TRUE, .overall = TRUE"),
  list(analysis_id = "S2", parent = "DEMO2", method = "continuous",
       variables = "AGE", formats = "p_miss=xx.xx%"),
  list(analysis_id = "BYSEX", method = "cards::ard_strata", strata = "SEX"),
  list(analysis_id = "ST", parent = "BYSEX", method = "continuous",
       by = "TRT01A", variables = "AGE", formats = "AGE:median=3"),
  # ard_hierarchical() takes fmt_fun for every variable only (a column of
  # its own): a variable's own format, after
  list(analysis_id = "H", method = "cards::ard_hierarchical", dataset = "ADAE",
       by = "TRTA", variables = "AEBODSYS | AEDECOD",
       args = "denominator = population, id = USUBJID", formats = "p=xx.xx% | n=1"),
  list(analysis_id = "H2", method = "cards::ard_hierarchical", dataset = "ADAE",
       by = "TRTA", variables = "AEBODSYS | AEDECOD",
       args = "denominator = population, id = USUBJID", formats = "AEDECOD:p=xx.xx%"),
  list(analysis_id = "ROWS", method = "cards::ard_tabulate_rows", dataset = "ADAE",
       by = "TRTA", formats = "n=2"),
  list(analysis_id = "MX", method = "max", dataset = "ADAE", by = "TRTA",
       variables = "AESEV", formats = "AESEV:p=xx.xx%"),
  list(analysis_id = "MV", method = "cards::ard_mvsummary", by = "TRT01A",
       variables = "AGE | BMIBL", formats = "corr=2 | BMIBL:corr=3",
       args = "statistic = ~ list(corr = function(x, data, ...) cor(data$AGE, data$BMIBL, use = \"complete.obs\"))"))

test_that("formats in the call give the stat_fmt formats after it gave", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("dplyr")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  sp <- fmt_spec(fmt_rows)
  code <- tfl_ard_code(sp, save = FALSE)
  expect_silent(parse(text = code))
  new <- fmt_text(build_quiet(sp, dir))
  old <- fmt_text(fmt_after(sp, dir))
  expect_identical(names(new), names(old))
  expect_identical(new, old)

  # and with a company's catalog whose defaults are not cards' own
  st <- tfl_ard_statistics()
  st$fmt[st$statistic == "n"] <- "1"
  st$fmt[st$statistic == "p"] <- "xx.xx%"
  st$fmt[st$statistic == "mean"] <- "xx.xxx"
  new <- fmt_text(build_quiet(sp, dir, statistics = st))
  old <- fmt_text(fmt_after(sp, dir, statistics = st))
  expect_identical(new, old)
  # the catalog's n (1 decimal) in the calls; the stack's own n=2 on its rows
  expect_true(all(grepl("^[0-9]+[.][0-9]$", new[grepl("^SEX\\|.*\\|n$", names(new))])))
  expect_true(all(grepl("^[0-9]+[.][0-9]{2}$", new[grepl("^DEMO\\|.*\\|TRT01A\\|.*\\|n$", names(new))])))
})

test_that("stat_fmt of each kind of format", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  sp <- fmt_spec(fmt_rows)
  a <- build_quiet(sp, dir)
  v <- fmt_text(a)
  pick <- function(id, var, s, ctx = NULL) {
    k <- names(v)[startsWith(names(v), paste0(id, "|")) &
                    grepl(paste0("\\|", var, "\\|"), names(v)) &
                    endsWith(names(v), paste0("|", s)) &
                    (is.null(ctx) | grepl(paste0("\\|", ctx, "\\|"), names(v)))]
    unname(v[k[1L]])
  }
  # continuous: the defaults, the analysis's, a variable's own
  expect_equal(pick("AGE", "AGE", "mean"), "75.21")        # mean=xx.xx
  expect_equal(pick("AGE", "AGE", "sd"), "8.59")           # default xx.xx
  expect_equal(pick("AGE", "BMIBL", "sd"), "3.672")        # BMIBL:sd=3
  expect_equal(pick("AGE", "AGE", "N"), "86")              # default xx
  expect_equal(pick("AGE", "AGE", "min"), "52")            # default xx
  expect_equal(pick("AGE", "BMIBL", "min"), "15.1")        # BMIBL:min=xx.x
  expect_equal(pick("AGE", "AGE", "cv"), "11.4")           # computed, xx.x
  # categorical: a proportion as a percent
  expect_equal(pick("SEX", "SEX", "p"), "61.6")            # default xx.x%
  expect_equal(pick("SEX", "AGEGR1", "p"), "48.84")       # AGEGR1:p=xx.xx%
  expect_equal(pick("F", "SEX", "p"), "62")                # p=xx%
  expect_equal(pick("SER", "ANYSER", "p"), "0.00")         # ANYSER:p=2
  # p-values: 3 decimals, or <0.001
  expect_match(pick("TT", "AGE", "p.value"), "^0[.][0-9]{3}$")
  # a stack: each analysis's formats in its call, the stack's own rows after
  expect_equal(pick("S_CONT", "AGE", "mean"), "75.21")
  expect_equal(pick("S_CONT", "AGE", "sd"), "8.590")
  expect_equal(pick("S_CAT", "SEX", "p"), "61.628")
  expect_equal(pick("DEMO", "TRT01A", "p"), "33.86")       # the stack's p=xx.xx%
  expect_equal(pick("DEMO", "TRT01A", "n"), "86.00")       # the stack's n=2
  expect_equal(pick("S2", "AGE", "p_miss", "missing"), "0.00")  # .missing: after
  expect_equal(pick("ST", "AGE", "median"), "78.000")      # in ard_strata()
  expect_equal(pick("H", "AEDECOD", "p"), "1.16")
  expect_equal(pick("H", "AEDECOD", "n"), "1.0")
  expect_equal(pick("H2", "AEDECOD", "p"), "1.16")
  expect_equal(pick("ROWS", "..row_count..", "n"), "301.00")
  expect_equal(pick("MV", "AGE", "corr"), "0.03")
  expect_equal(pick("MV", "BMIBL", "corr"), "0.030")
})

test_that("an analysis's formats are in its cards call; the rest after it", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  code <- tfl_ard_code(fmt_spec(fmt_rows), save = FALSE)
  txt <- paste(code, collapse = "\n")
  # the helpers: the defaults as fmt_fun takes them
  expect_true(".pvalue <- function(x) ifelse(x < 0.001, \"<0.001\", sprintf(\"%.3f\", x))" %in% code)
  expect_match(txt, "`p.value` = .pvalue|p.value = .pvalue")
  expect_match(txt, "p = cards::label_round(1, scale = 100)", fixed = TRUE)
  expect_match(txt, ".fmts <- function(...) utils::modifyList(.fmt_default, list(...))",
               fixed = TRUE)
  # in the call: cards' summaries, counts, missing, a subject flag
  expect_match(txt, paste0("fmt_fun = list(\n      everything() ~ .fmts(mean = 2L),\n",
                           "      BMIBL ~ .fmts(mean = 2L, sd = 3L, min = 1L))"),
               fixed = TRUE)
  expect_match(txt, "fmt_fun = list(\n      everything() ~ .fmt_default,\n      AGEGR1 ~ .fmts(p = cards::label_round(2, scale = 100)))",
               fixed = TRUE)
  expect_match(txt, "fmt_fun = everything() ~ .fmts(p = cards::label_round(0, scale = 100))",
               fixed = TRUE)
  expect_match(txt, "value = list(ANYSER = TRUE),\n      fmt_fun = everything() ~ .fmts(p = 2L))",
               fixed = TRUE)
  expect_true("ards[[1]] <- .tag(cards::apply_fmt_fun(ard), \"T\", \"AGE\", \"SAF\")" %in% code)
  # after it: cardx, a fmt_fun of the analysis's own, post, ard_stack_hierarchical()
  expect_true("ards[[6]] <- .tag(.fmt(ard), \"T\", \"TT\", \"SAF\")" %in% code)
  expect_true("ards[[8]] <- .tag(.fmt(ard), \"T\", \"OWN\", \"SAF\")" %in% code)
  expect_true("ards[[9]] <- .tag(.fmt(ard), \"T\", \"POST\", \"SAF\")" %in% code)
  expect_true(any(grepl("^ards\\[\\[10\\]\\] <- \\.tag\\(\\.fmt\\(ard, list\\(p = ", code)))
  # a stack: in each call, and the stack's own rows after it
  expect_match(txt, "cards::ard_summary(variables = c(AGE, BMIBL),\n      statistic = ~ cards::continuous_summary_fns(c(\"N\", \"mean\", \"sd\")),\n      fmt_fun = everything() ~ .fmts(p = cards::label_round(2, scale = 100), n = 2L, mean = 2L, sd = 3L)),",
               fixed = TRUE)
  expect_true(any(grepl("skip = c(\"AGE\", \"BMIBL\", \"SEX\")), \"T\", c(`AGE` = \"S_CONT\"",
                        code, fixed = TRUE)))
  expect_match(txt, "id = USUBJID,\n      fmt_fun = everything() ~ .fmts(p = cards::label_round(2, scale = 100), n = 1L))",
               fixed = TRUE)
  expect_true("ards[[19]] <- .tag(.fmt(ard, list(`AEDECOD:p` = cards::label_round(2, scale = 100))), \"T\", \"H2\", \"SAF\")" %in% code)
  expect_match(txt, "cards::ard_tabulate_rows(adae_saf_1,\n    by = TRTA,\n    fmt_fun = everything() ~ .fmts(n = 2L))",
               fixed = TRUE)
  expect_match(txt, "id = USUBJID,\n    fmt_fun = everything() ~ .fmts(p = cards::label_round(2, scale = 100)))", fixed = TRUE)
  expect_true("ards[[21]] <- .tag(cards::apply_fmt_fun(ard), \"T\", \"MX\", \"SAF\")" %in% code)
  expect_match(txt, "fmt_fun = list(\n        everything() ~ .fmts(corr = 2L),\n        BMIBL ~ .fmts(corr = 3L))",
               fixed = TRUE)
  # in ard_strata(): the call's; ard_strata() adds no rows of its own
  expect_match(txt, ".f = ~ cards::ard_summary(.x,", fixed = TRUE)
  expect_true(any(grepl("<- .tag(cards::apply_fmt_fun(ard), \"T\", \"ST\", \"SAF\")",
                        code, fixed = TRUE)))
})
