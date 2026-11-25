# The study's setup programs (tflspec#910, G5-1b), moved from tflplanner:
# byte for byte what tflplanner wrote before the move, for a fictional
# study (XMP-204) and a company whose every input differs from the
# built-in one (tests/testthat/fixtures/setup-family/: the study's YAML,
# the company's inputs in company.dput, the programs in out/ with the
# `Generated` line masked).  They were frozen from tflplanner's own makers
# (its G5-1a branch, before the move), with the study's context a save sets
# (the packages its setup files attach).  No text may change before G5-1c;
# a wanted change after that writes them again from tfl_setup_program()
# (setup_fx_programs() below), and the diff is the review.

setup_fx <- function(...) test_path("fixtures", "setup-family", ...)

# a program's text as it is written (writeLines(): a line an element), its
# Generated line masked
setup_fx_text <- function(code) {
  code <- sub("^(#  Generated  :).*$", "\\1 <masked>", code)
  paste0(paste(enc2utf8(code), collapse = "\n"), "\n")
}

# a fixture's text, its line ends as \n whatever the checkout made them
setup_fx_read <- function(name) {
  f <- setup_fx("out", paste0(name, ".R"))
  txt <- rawToChar(readBin(f, "raw", file.size(f)))
  Encoding(txt) <- "UTF-8"
  gsub("\r\n", "\n", txt, fixed = TRUE)
}

setup_fx_inputs <- function() {
  study <- tfl_read_study_spec(setup_fx("study"))
  y <- yaml::read_yaml(setup_fx("study", "study.yml"))
  fields <- c("study_id", "title", "short_title", "compound", "phase", "description")
  meta <- stats::setNames(lapply(fields, function(k) y[[k]]), fields)
  standards <- eval(parse(setup_fx("company.dput"), keep.source = FALSE))
  list(study = study, meta = meta, standards = standards, date = as.Date("2026-11-20"))
}

# every setup program of the fixture study, as tflspec makes it
setup_fx_programs <- function(x = setup_fx_inputs()) {
  mk <- function(file, ...) {
    tfl_setup_program(file, meta = x$meta, study = x$study, standards = x$standards,
                      date = x$date, ...)
  }
  out <- list(company_setup = mk("company_setup"), study_user = mk("study_user"),
              study_setup = mk("study_setup"), study_helpers = mk("study_helpers"))
  # the packages the study's setup files attach (what its programs read)
  att <- tfl_setup_values(unname(out[c("company_setup", "study_setup", "study_user")]))$attached
  c(out, list(report_setup = mk("report_setup", attached = att),
              fig_setup = mk("fig_setup", attached = att),
              report_setup_plain = mk("report_setup", study_code = FALSE)))
}

test_that("the setup programs are tflplanner's, byte for byte (golden fixtures)", {
  x <- setup_fx_inputs()
  # the company's every input differs from tflspec's own
  b <- tfl_builtin_standards()
  expect_false(identical(x$standards$about, b$about))
  expect_false(identical(x$standards$setup_code, b$setup_code))
  fs <- tfl_fig_style()
  expect_false(identical(x$standards$figure_settings, fs$settings))
  expect_false(identical(x$standards$figure_colors, fs$colors))
  expect_false(identical(x$standards$figure_markers, fs$markers))
  progs <- setup_fx_programs(x)
  expect_setequal(names(progs), sub("[.]R$", "", list.files(setup_fx("out"))))
  for (k in names(progs)) {
    expect_identical(setup_fx_text(progs[[k]]), setup_fx_read(k), label = k)
  }
  # what the company gave reaches the programs
  expect_match(progs$company_setup[2L], "Example Pharma TFL standards 3.2 of 2026-10-01", fixed = TRUE)
  expect_true("# Example Pharma: the company setup of XMP-204" %in% progs$company_setup)
  expect_true(any(grepl("#123456", progs$fig_setup, fixed = TRUE)))
})

test_that("a session's options and the tflplanner home never reach the text", {
  x <- setup_fx_inputs()
  clean <- setup_fx_programs(x)
  withr::local_options(list(
    tflspec.paths = c(path_x = "data"), tflspec.attached = c("ggplot2", "cards"),
    tflspec.study_setup = FALSE, tflspec.series_dir = tempdir(),
    tflspec.fig_style = list(settings = data.frame()), rtfreporter.ard_pipe = "%>%",
    tflspec.plain_ns = TRUE, digits = 3, scipen = 50, OutDec = ","))
  expect_identical(setup_fx_programs(x), clean)
})

test_that("with no company inputs, tflspec's own; the designed figures' packages", {
  meta <- list(study_id = "XMP-9")
  seed <- tfl_setup_program("company_setup", meta = meta, date = as.Date("2026-11-20"))
  expect_identical(seed[2L], paste0("# standards (tflplanner draft standards 0.1 of 2026-09-27) ",
                                    "on 2026-11-20.  The study's own now: tflplanner"))
  expect_identical(utils::tail(seed, nrow(tfl_builtin_standards()$setup_code)),
                   tfl_builtin_standards()$setup_code$code)
  # an about without a name or version: "built-in"
  seed <- tfl_setup_program("company_setup", meta = meta, date = as.Date("2026-11-20"),
                            standards = list(about = data.frame(key = "owner", value = "x")))
  expect_match(seed[2L], "# standards (built-in) on 2026-11-20.", fixed = TRUE)
  fig <- tfl_setup_program("fig_setup", fig_libs = c("ggsurvfit", "dplyr", "cowplot"))
  at <- grep("^library\\(", fig)
  expect_identical(fig[at], paste0("library(", c("ggplot2", "patchwork", "dplyr", "cowplot",
                                                 "ggsurvfit"), ")"))
  # another layout: every path follows it
  lay <- tfl_study_layout()
  lay[c("programs_ard", "programs_tfl", "tfl")] <- c("code/ard", "code/tfl", "out/tfl")
  rs <- tfl_setup_program("report_setup", layout = lay)
  expect_true("source(\"code/study_setup.R\")" %in% rs)
  expect_true(any(grepl("\"out/tfl/report_status.csv\"", rs, fixed = TRUE)))
  expect_true(any(grepl("\"code/tfl/report_setup.R\"", rs, fixed = TRUE)))
  ss <- tfl_setup_program("study_setup", meta = meta, layout = lay)
  expect_true("source(\"code/study_helpers.R\")" %in% ss)
  expect_true(any(grepl("path_programs_tfl <- \"code/tfl\"", ss, fixed = TRUE)))
})

test_that("tfl_setup_program() says what is wrong with its arguments", {
  expect_error(tfl_setup_program("batch"), "`file` is one of study_setup")
  expect_error(tfl_setup_program("company_setup", standards = list(code_templates = 1)),
               "`standards` is a list with any of about")
  expect_error(tfl_setup_program("study_setup", meta = "XMP"), "`meta` is a list")
  expect_error(tfl_setup_program("study_setup", layout = c(adam = "a")), "`layout`")
})

test_that("while a program is made, the series are not looked for in the tflplanner home", {
  home <- .series_home_dir()
  expect_true(home %in% .series_search_dirs())
  d <- tempfile("series")
  inside <- .with_code_context(.series_search_dirs(), list(series_dir = d))
  expect_false(home %in% inside)
  expect_identical(inside[1L], d)
  # (and the home again afterwards, an error too)
  try(.with_code_context(stop("x")), silent = TRUE)
  expect_true(home %in% .series_search_dirs())
})
