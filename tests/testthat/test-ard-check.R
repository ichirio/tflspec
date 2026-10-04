test_that("tfl_check_ard() finds what a report's table reads and an ARD lacks", {
  skip_if_not_installed("cards")
  ard <- cards::ard_summary(cards::ADSL, by = ARM, variables = AGE,
                            statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd")))
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T1", cols = "ARM", rows = NA),
    cells = data.frame(output_id = "T1", variable = c("continuous", "AGE"),
                       row = c("Mean (SD)", "n"),
                       template = c("{mean} ({sd})", "{N}")))
  ok <- tfl_check_ard(ard, sp, output_id = "T1")
  expect_identical(ok$level[ok$level != "note"], character())

  less <- ard[ard$stat_name != "sd", ]
  p <- tfl_check_ard(less, sp, output_id = "T1")
  expect_true(any(p$check == "statistics" & grepl("{sd}", p$message, fixed = TRUE)))

  other <- cards::ard_summary(cards::ADSL, by = SEX, variables = BMIBL)
  p <- tfl_check_ard(other, sp, output_id = "T1")
  expect_true(any(p$check == "cols" & grepl("ARM", p$message)))
  expect_true(any(p$check == "cells" & grepl("AGE", p$message)))

  # the shape
  expect_identical(tfl_check_ard(list(1))$level, "error")
  p <- tfl_check_ard(data.frame(variable = "A", stat = 1))
  expect_match(p$message, "stat_name")
  # a result across the groups (a test) has group1 without group1_level,
  # as cardx's own tests give it: not an error
  p <- tfl_check_ard(data.frame(group1 = "ARM", variable = "AGE",
                                stat_name = "n", stat = 1))
  expect_false(any(p$level == "error"))
  # without a spec, the shape only
  expect_identical(tfl_check_ard(ard)$level[tfl_check_ard(ard)$level != "note"],
                   character())
})

test_that("report$ard_source is blank or import:<file>", {
  ok <- tfl_table_spec(report = data.frame(output_id = c("T1", "T2"),
                                           ard_source = c(NA, "import:cro.json")))
  expect_s3_class(ok, "tfl_table_spec")
  expect_error(tfl_table_spec(report = data.frame(output_id = "T1",
                                                  ard_source = "cro.json")),
               "import:<file>")
})

test_that("tfl_check_ard_function() tries a function of one's own", {
  skip_if_not_installed("cards")
  good <- function(data, by, variables, ...) {
    cards::ard_summary(data, by = {{ by }}, variables = {{ variables }},
                       statistic = ~ cards::continuous_summary_fns(c("N", "mean")))
  }
  p <- tfl_check_ard_function(good, cards::ADSL, by = ARM, variables = AGE,
                              stat_names = c("N", "mean"))
  expect_identical(p$level[p$level != "note"], character())
  expect_s3_class(attr(p, "ard"), "card")
  p <- tfl_check_ard_function(good, cards::ADSL, by = ARM, variables = AGE,
                              stat_names = c("N", "sd"))
  expect_true(any(p$check == "statistics" & grepl("sd", p$message)))
  plain <- function(data, ...) data.frame(x = 1)
  p <- tfl_check_ard_function(plain, cards::ADSL)
  expect_true(any(p$check == "result" & p$level == "error"))
  boom <- function(data, ...) stop("no such column")
  p <- tfl_check_ard_function(boom, cards::ADSL)
  expect_identical(p$check[p$level != "note"], "call")
  expect_match(p$message[p$check == "call"], "no such column")
  # one that does not say which statistics it gives: a note
  expect_true(any(p$check == "statistics" & p$level == "note"))
  # one that says so, the way cards' own do: checked without stat_names
  said <- cards::as_cards_fn(good, stat_names = c("N", "sd"))
  p <- tfl_check_ard_function(said, cards::ADSL, by = ARM, variables = AGE)
  expect_true(any(p$check == "statistics" & p$level == "error" &
                    grepl("sd", p$message)))
})

test_that("the three templates of an ARD function work as they are", {
  skip_if_not_installed("cards")
  skip_if_not_installed("broom")
  data <- cards::ADSL[cards::ADSL$ARM %in% c("Placebo", "Xanomeline High Dose"), ]
  for (type in c("summary", "test", "free")) {
    dir <- withr::local_tempdir()
    f <- file.path(dir, paste0("ard_mine_", type, ".R"))
    code <- tfl_ard_function_template(paste0("ard_mine_", type), type,
                                      file = f, test = TRUE)
    expect_true(file.exists(f))
    expect_true(file.exists(file.path(dir, paste0("test-ard_mine_", type, ".R"))))
    env <- new.env()
    sys.source(f, envir = env)
    fun <- get(paste0("ard_mine_", type), envir = env)
    expect_false(is.null(attr(fun, "stat_names")), info = type)
    p <- tfl_check_ard_function(fun, data, by = ARM, variables = AGE)
    expect_identical(p$message[p$level %in% c("error", "warning")], character(),
                     info = type)
    # and its test file passes
    withr::with_dir(dir, testthat::test_file(
      file.path(dir, paste0("test-ard_mine_", type, ".R")), reporter = "silent"))
  }
  expect_error(tfl_ard_function_template("ard x"), "function's name")
  f <- file.path(withr::local_tempdir(), "ard_a.R")
  tfl_ard_function_template("ard_a", file = f)
  expect_error(tfl_ard_function_template("ard_a", file = f), "overwrite")
  b <- tfl_ard_function_template("ard_b", "free")
  expect_true(startsWith(b[1], "#' "))                 # a roxygen title
  expect_true(any(startsWith(b, "ard_b <- cards::as_cards_fn(")))
  expect_false(any(grepl("{name}", b, fixed = TRUE)))
})

test_that("tfl_ard_conditions() lists what went wrong, one row per message", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  adsl <- cards::ADSL
  # a test that needs two groups, given three; a statistic that stops; one
  # that warns
  a <- cardx::ard_stats_t_test(adsl, by = ARM, variables = AGE)
  b <- cards::ard_summary(adsl, variables = AGE, statistic = ~ list(
    bad = function(x) stop("boom"),
    w = function(x) { warning("careful"); 1 }))
  x <- cards::bind_ard(a, b)
  x$output_id <- "T1"
  x$analysis_id <- rep(c("TTEST", "AGE"), c(nrow(a), nrow(b)))
  d <- tfl_ard_conditions(x)
  expect_identical(names(d), c("output_id", "analysis_id", "variable", "groups",
                               "level", "message", "statistics"))
  expect_identical(d$level, c("error", "error", "warning"))
  expect_identical(d$analysis_id, c("TTEST", "AGE", "AGE"))
  expect_identical(d$groups, c("ARM", "", ""))
  expect_match(d$message[1], "exactly 2 levels")
  expect_match(d$statistics[1], "^estimate, ")
  expect_identical(d$statistics[2:3], c("bad", "w"))
  # nothing wrong: no rows, the same columns
  ok <- tfl_ard_conditions(cards::ard_summary(adsl, variables = AGE))
  expect_identical(nrow(ok), 0L)
  expect_identical(names(ok), c("variable", "groups", "level", "message",
                                "statistics"))
})

test_that("tfl_ard_function_info() reads the functions of some files, without running them", {
  d <- withr::local_tempdir()
  for (t in c("summary", "test", "free")) {
    tfl_ard_function_template(paste0("ard_", t), t,
                              file = file.path(d, paste0("ard_", t, ".R")), test = TRUE)
  }
  writeLines(c("#' Plain one", "#'", "#' Does a thing,", "#' in two lines.",
               "#' @param data The data.", "#' @param by,k Its groups",
               "#'   and a number.", "",
               "ard_plain <- function(data, by = NULL, k = 2) stop(\"not run\")",
               "helper <- 1",
               "other <- function(x) x"),
             file.path(d, "plain.R"))
  writeLines("ard_bad <- function(", file.path(d, "bad.R"))
  info <- tfl_ard_function_info(list.files(d, full.names = TRUE))
  # the templates' functions (as_cards_fn), a plain function -- and not the
  # test files, nor a value
  expect_setequal(stats::na.omit(info$name),
                  c("ard_free", "ard_summary", "ard_test", "ard_plain", "other"))
  expect_false(any(grepl("^test-", basename(info$file))))
  t <- info[info$name %in% "ard_test", ]
  expect_identical(t$title, "A test across two groups")
  expect_match(t$description, "tidy_as_ard")
  expect_identical(t$stat_names, "statistic | p.value")
  expect_identical(t$args[[1]]$arg, c("data", "by", "variables", "..."))
  expect_match(t$args[[1]]$hint[2], "two groups")
  p <- info[info$name %in% "ard_plain", ]
  expect_identical(p$title, "Plain one")
  expect_identical(p$description, "Does a thing, in two lines.")
  expect_identical(p$stat_names, "")
  a <- p$args[[1]]
  expect_identical(a$default, c(NA, "NULL", "2"))
  expect_identical(a$hint, c("The data.", "Its groups and a number.",
                             "Its groups and a number."))
  # a function with no roxygen block above it: no title
  expect_true(is.na(info$title[info$name %in% "other"]))
  # a file that does not parse: a row that says so
  bad <- info[is.na(info$name), ]
  expect_identical(basename(bad$file), "bad.R")
  expect_match(bad$description, "unexpected")
  # nothing: the columns
  expect_identical(nrow(tfl_ard_function_info(character())), 0L)
})
