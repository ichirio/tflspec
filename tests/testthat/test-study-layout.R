# What the study's program generator reads as values (tflspec#910, G5-1a):
# the study's layout, the files of its programs and their names, what its
# setup files say, and one context of options while a program is made.

test_that("the study's layout: its folders, in order", {
  lay <- tfl_study_layout()
  expect_identical(lay, c(
    adam = "data/adam", sdtm = "data/sdtm", other = "data/other",
    spec = "spec", programs_ard = "programs/ard",
    programs_tfl = "programs/tfl", ard = "output/ard", tfl = "output/tfl",
    ard_import = "input/ard", toc_import = "input/toc", runs = "runs",
    logs_preview = "logs/preview", docs = "docs"))
})

test_that("the files of the programs: a report's program, RTF and ARD program", {
  f <- tfl_study_program_files(c(T1 = "t_1.R", `T 2/a` = NA),
                               rtf = c(`T 2/a` = "output/tfl/t2.rtf"))
  expect_identical(f$output_id, c("T1", "T 2/a"))
  expect_identical(f$program, c("programs/tfl/t_1.R", NA))
  expect_identical(f$rtf, c(NA, "output/tfl/t2.rtf"))
  expect_identical(f$ard_program, c("programs/ard/T1.R", "programs/ard/T_2_a.R"))
  expect_identical(attr(f, "problems"), list())
  # another layout
  lay <- c(programs_tfl = "code/tfl", programs_ard = "code/ard")
  g <- tfl_study_program_files(c(T1 = "t_1.R"), layout = lay)
  expect_identical(c(g$program, g$ard_program), c("code/tfl/t_1.R", "code/ard/T1.R"))
  expect_error(tfl_study_program_files(c(T1 = "t_1.R"), layout = c(programs_tfl = "x")),
               "programs_ard")
  expect_error(tfl_study_program_files("t_1.R"), "named by output id")
  # none
  z <- tfl_study_program_files(character())
  expect_identical(nrow(z), 0L)
  expect_identical(attr(z, "problems"), list())
})

test_that("the names of the programs: reserved and shared, in lower case, in order", {
  f <- tfl_study_program_files(
    c(T1 = "Report_Setup.R", T2 = "t2.R", T3 = "T2.R", T4 = NA, ard_setup = "own.R",
      Autoexec_ARD = "own2.R", `T.1` = "t5.R", `T 1` = "t6.R", `T_1` = "t7.R"),
    reserved = c("report_setup.R", "ard_setup.R", "autoexec_ard.R"))
  p <- attr(f, "problems")
  expect_identical(vapply(p, function(x) x$kind, ""),
                   c("reserved", "duplicate", "ard_reserved", "ard_reserved", "ard_duplicate"))
  expect_identical(p[[1L]], list(id = "T1", kind = "reserved", args = c("T1", "Report_Setup.R")))
  expect_identical(p[[2L]], list(id = "T3", kind = "duplicate", args = c("T2", "T3", "T2.R")))
  expect_identical(p[[3L]]$args, c("ard_setup", "ard_setup.R"))
  expect_identical(p[[4L]]$args, c("Autoexec_ARD", "Autoexec_ARD.R"))
  expect_identical(p[[5L]], list(id = "T_1", kind = "ard_duplicate",
                                 args = c("T 1", "T_1", "T_1.R")))
  # a report's ARD program is checked once; a report with no id is not checked
  g <- tfl_study_program_files(c(T1 = "a.R", T1 = "b.R"))
  expect_identical(vapply(attr(g, "problems"), function(x) x$kind, ""), character())
  h <- tfl_study_program_files(stats::setNames(c("a.R", "a.R"), c("", NA)))
  expect_identical(attr(h, "problems"), list())
})

test_that("what the setup files say: the packages they attach, in order, once", {
  v <- tfl_setup_values(list(
    c("library(cards)", "suppressPackageStartupMessages({", "  library(dplyr)",
      "  base::require(\"tidyr\")", "})", "x <- \"library(nope)\" # library(nope2)"),
    NULL,
    "library(cards)\nlibrary(package = rtfreporter)"))
  expect_identical(v$attached, c("cards", "dplyr", "tidyr", "rtfreporter"))
  # one file; none; code that does not parse: none
  expect_identical(tfl_setup_values("library(cards)")$attached, "cards")
  expect_identical(tfl_setup_values(list())$attached, character())
  expect_identical(tfl_setup_values(list("library(cards)", "library(dplyr"))$attached,
                   character())
  # (a name is read as the package's name, as library() reads it without
  # character.only)
  expect_identical(tfl_setup_values("p <- 'x'; library(p, character.only = TRUE)")$attached, "p")
  expect_error(tfl_setup_values(list(1)), "setup files' text")
})

test_that("what the setup files say: the time units, the last one winning", {
  d <- tfl_time_unit_days()
  expect_identical(tfl_setup_values(list())$time_units, d)
  v <- tfl_setup_values(list(
    c("days_per_month <- 30", "days_per_year = 365  # ours"),
    NULL,
    c("  days_per_month <- 31.5", "x <- days_per_week", "days_per_week <- seven")))
  want <- d
  want[["days_per_month"]] <- 31.5
  want[["days_per_year"]] <- 365
  expect_identical(v$time_units, want)
  # (as one string)
  expect_identical(tfl_setup_values("a <- 1\r\ndays_per_week <- 5\n")$time_units[["days_per_week"]], 5)
})

# a few of the makers that read options, together
.cc_make <- function() {
  c(.path_code("data/adam/adsl.rds"),
    paste(.attached(), collapse = ","),
    .ard_pipe_op(),
    format(1.23456789, scientific = FALSE, trim = TRUE),
    paste(.series_search_dirs(), collapse = "|"),
    deparse(getOption("tflspec.study_setup")),
    deparse(getOption("tflspec.fig_style")))
}

# every option the context sets, hostile
.cc_hostile <- function() {
  list(tflspec.paths = c(path_data = "data"), tflspec.attached = c("cards", "cardx"),
       tflspec.study_setup = TRUE, tflspec.series_dir = "C:/somewhere/series",
       tflspec.fig_style = list(settings = "x"), tflspec.ard_methods = "junk",
       tflspec.ard_statistics = "junk", tflspec.ard_args = "junk",
       rtfreporter.ard_pipe = "%>%", tflspec.ggplot2_version = "3.5",
       rtfreporter.font_size_half_points = 30, tflspec.outputs_dir = "elsewhere",
       tflspec.plain_ns = TRUE, tflspec.user_attached = "dplyr",
       tflspec.template_source = "nowhere", tflspec.data_expr = "x",
       tflspec.fig_path = "y", digits = 3L, scipen = 100L, OutDec = ",")
}

test_that("one context: hostile options give the text of clean ones", {
  ctx <- list(paths = c(path_adam = "data/adam"), attached = "dplyr", study_setup = TRUE)
  clean <- stats::setNames(rep(list(NULL), length(.code_context_options)),
                           unname(.code_context_options))
  withr::local_options(clean)
  want <- .with_code_context(.cc_make(), ctx)
  # the context's values are used
  expect_identical(want[1:3], c("file.path(path_adam, \"adsl.rds\")", "dplyr", "|>"))
  expect_identical(want[4L], "1.234568")
  expect_identical(want[6:7], c("TRUE", "NULL"))
  # the same with every option set otherwise
  withr::local_options(.cc_hostile())
  expect_identical(.with_code_context(.cc_make(), ctx), want)
  # and the session's options as they were, after an error too
  expect_identical(getOption("tflspec.paths"), c(path_data = "data"))
  expect_error(.with_code_context(stop("in the maker"), ctx), "in the maker")
  h <- .cc_hostile()
  for (k in names(h)) expect_equal(getOption(k), h[[k]], info = k)
  expect_error(.with_code_context(1, list(nope = 1)), "names only")
})

test_that("one context: a real maker's text is the same under hostile options", {
  skip_if_not_installed("cards")
  sp <- tfl_ard_spec(list(
    study = data.frame(key = "id", value = "USUBJID"),
    datasets = data.frame(dataset = "ADSL", path = "data/adam/adsl.rds"),
    populations = data.frame(population_id = "SAF", dataset = "ADSL",
                             where = "SAFFL == \"Y\""),
    analyses = data.frame(output_id = "T1", analysis_id = "AGE",
                          method = "continuous", population_id = "SAF",
                          by = "TRT01A", variables = "AGE")))
  ctx <- list(paths = c(path_adam = "data/adam"), attached = "cards", study_setup = TRUE)
  clean <- stats::setNames(rep(list(NULL), length(.code_context_options)),
                           unname(.code_context_options))
  withr::local_options(clean)
  want <- .with_code_context(tfl_ard_code(sp, part = "body"), ctx)
  withr::local_options(.cc_hostile())
  expect_identical(.with_code_context(tfl_ard_code(sp, part = "body"), ctx), want)
})
