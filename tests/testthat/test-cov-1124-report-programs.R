# Branches of R/report_programs.R (tfl_report_program(), tfl_ard_program(),
# G5-1c) that no other test reaches: values given as functions, the paths
# and the attached packages, a table of records, a template listing, a
# report's own code next to a design, the TODOs, the arguments.  The
# reports are the golden fixtures' (fixtures/programs-family/), changed
# here and there.  Fictional data only.

rpc_fx <- function(...) test_path("fixtures", "programs-family", ...)

# a fixture study's values, its spec reading from the fixture's study folder
rpc_inputs <- function(study) {
  x <- readRDS(rpc_fx("in", paste0(study, ".rds")))
  if (!is.null(x$study) && isTRUE(x$study_dir)) x$study$dir <- rpc_fx("study", study)
  x
}

# one report's program (or its data part) with the study's values
rpc_program <- function(x, r, part = "program", standards = x$standards, shared = x$shared, ...) {
  tfl_report_program(r, study = x$study, shared = shared, standards = standards,
                     attached = x$attached, layout = x$layout, date = x$date,
                     generator = "tflspec 0.0.0", part = part, ...)
}

# the code templates of the company without one of them
rpc_without_template <- function(x, name) {
  s <- x$standards
  ct <- s$code_templates
  s$code_templates <- ct[ct$name != name, , drop = FALSE]
  s
}

# a report of the sample study whose records are its data (a table of
# records: `data: {source: dataset}`)
rpc_records_table <- function(x) {
  r <- x$reports[["T-14-1-1"]]
  r$spec_output$data <- list(source = "dataset", dataset = "ADSL", population = "SAF")
  r
}

# the sample study's listing, made from a template (planned like a table of
# records)
rpc_template_listing <- function(x) {
  r <- x$reports[["L-16-2-7"]]
  r$template_listing <- TRUE
  r
}

# ---- the values and the helpers ----------------------------------------------

test_that("a shared value given as a function is called when the program needs it", {
  x <- rpc_inputs("sheets")
  id <- names(x$reports)[vapply(x$reports, function(v) identical(v$type, "table"), NA)][1L]
  want <- rpc_program(x, x$reports[[id]])
  called <- 0L
  sh <- x$shared
  sh$sheets <- function() {
    called <<- called + 1L
    x$shared$sheets
  }
  expect_identical(rpc_program(x, x$reports[[id]], shared = sh), want)
  expect_gt(called, 0L)
  # (a value as it is, and the error making it gave)
  expect_identical(.rp_value(2L), 2L)
  expect_error(.rp_value(simpleError("the sheets do not hold")), "the sheets do not hold")
})

test_that("a path under none of the folders' variables is written as it is", {
  withr::local_options(tflspec.paths = c(path_adam = "data/adam"))
  expect_identical(.rp_path_lit("elsewhere/adsl.rds"), "\"elsewhere/adsl.rds\"")
  expect_identical(.rp_path_lit("data/adam/adsl.rds"), "file.path(path_adam, \"adsl.rds\")")
})

test_that("code that does not parse keeps its pkg:: prefixes", {
  withr::local_options(tflspec.attached = "dplyr")
  bad <- "dplyr::filter(adsl, (("
  expect_identical(.rp_drop_attached(bad), bad)
  expect_identical(.rp_drop_attached("x <- dplyr::filter(adsl)"), "x <- filter(adsl)")
})

test_that("an RTF not named after the report is written by its own path", {
  expect_identical(.rp_report_path_code("output/tfl/other.rtf", "T-1"), "\"output/tfl/other.rtf\"")
  expect_identical(.rp_report_path_code("output/tfl/T-1.rtf", "T-1"),
                   "file.path(\"output/tfl\", paste0(report_id, \".rtf\"))")
})

test_that("a figure's size in words: fit to the body, or its own size on one side", {
  fit <- list(width_in = 9, height_in = 5, mode = c(width = "fit", height = "fit"))
  mixed <- list(width_in = 9, height_in = 5, mode = c(width = "fit", height = "native"))
  expect_identical(.rp_fig_size_words(fit), "9 x 5 in (the page's body)")
  expect_identical(.rp_fig_size_words(mixed), "9 x 5 in")
})

test_that("a figure with no design reads no ARD", {
  expect_false(.rp_fig_reads_ard(NULL))
})

test_that("a listing's normalization is not a table's built-in one", {
  ctx <- list(report = list(type = "listing", steps = list(data = NA, content = NA)))
  expect_false(.rp_table_norm_builtin(ctx))
})

# ---- a table --------------------------------------------------------------------

test_that("a table with no table_data template gets a TODO that stops", {
  x <- rpc_inputs("sample")
  code <- rpc_program(x, x$reports[["T-14-1-1"]], part = "data",
                      standards = rpc_without_template(x, "table_data"))
  expect_true(any(code == "# TODO: build the ARD, e.g."))
  expect_true(any(grepl("ard_stack(adsl", code, fixed = TRUE)))
  expect_true(any(grepl("data part of T-14-1-1.R is still to be written", code, fixed = TRUE)))
  # (todo = FALSE: nothing)
  expect_null(rpc_program(x, x$reports[["T-14-1-1"]], part = "data", todo = FALSE,
                          standards = rpc_without_template(x, "table_data")))
})

test_that("a report's ARD of its own name replaces `ard` in the table_data template", {
  x <- rpc_inputs("sample")
  r <- x$reports[["T-14-1-1"]]
  r$ard_object <- "ard_t1"
  code <- rpc_program(x, r, part = "data")
  expect_true(any(grepl("(^|\\n)ard_t1 <- readRDS", code)))
  expect_false(any(grepl("(^|\\n)ard <- readRDS", code)))
})

test_that("a table_data template that makes `data` is not normalized in the plan", {
  x <- rpc_inputs("sample")
  s <- x$standards
  s$code_templates$code[s$code_templates$name == "table_data"] <- "data <- readRDS(\"{ARD}\")"
  code <- rpc_program(x, x$reports[["T-14-1-1"]], standards = s)
  expect_true(any(grepl("this report's normalized ARD", code, fixed = TRUE)))
  expect_false(.rp_table_norm_builtin(list(report = x$reports[["T-14-1-1"]], standards = s,
                                           shared = x$shared, layout = x$layout)))
})

test_that("a Rework that leaves no `data` makes the report part a stop()", {
  x <- rpc_inputs("sample")
  r <- x$reports[["T-14-1-1"]]
  r$process <- "x <- 1"
  code <- rpc_program(x, r)
  expect_true(any(grepl("the data.steps leave no `data`", code, fixed = TRUE)))
})

test_that("a Rework with no table sheets is normalized with normalize_ard()'s defaults", {
  x <- rpc_inputs("edited")
  id <- "T-14-0-1"
  sh <- x$shared
  sh$table_sheets <- NULL
  sh$ard_spec <- NULL
  code <- rpc_program(x, x$reports[[id]], part = "data", shared = sh)
  expect_true(any(code == "data <- ard |> normalize_ard()"))
})

test_that("a Rework's normalization on the sheets: one short step on one line, more on several", {
  x <- rpc_inputs("edited")
  ctx <- list(report = x$reports[["T-14-0-1"]], shared = x$shared)
  local_mocked_bindings(tfl_table_code = function(...) {
    c("plan <- ard |>", "  normalize_ard(stat_labels = FALSE) |>", "  table_plan(x)")
  })
  expect_identical(.rp_rework_normalize_sheets(ctx),
                   "data <- ard |> normalize_ard(stat_labels = FALSE)")
  local_mocked_bindings(tfl_table_code = function(...) {
    c("plan <- ard |>", "  normalize_ard() |>", "  relevel_ard(TRT01A = c(\"A\", \"B\")) |>",
      "  table_plan(x)")
  })
  expect_identical(.rp_rework_normalize_sheets(ctx),
                   c("data <- ard |>", "  normalize_ard() |>",
                     "  relevel_ard(TRT01A = c(\"A\", \"B\"))"))
})

test_that("a table of records: its records as `data`, then its ARD", {
  x <- rpc_inputs("sample")
  r <- rpc_records_table(x)
  data <- rpc_program(x, r, part = "data")
  expect_true(any(data == "# the report's records"))
  expect_true(any(data == "# its ARD (the parts' rows)"))
  expect_true(any(grepl("^data <- adsl_sub", data)))
  code <- rpc_program(x, r)
  expect_true(any(startsWith(code, "# ---- data: this report's records and ARD")))
})

test_that("a table template's records are the resolved template's", {
  x <- rpc_inputs("sample")
  o <- tfl_output("L-T", "listing", template = list(name = "listing", args = list(
    data = "ADAE", columns = list(list(vars = "USUBJID", label = "Subject")))))
  code <- .rp_table_objects_code(o, x$study)
  expect_true(length(code) > 0L)
  expect_true(any(grepl("adae", unlist(code), fixed = TRUE)))
})

test_that("a table whose content is its own code: the ARD made, then the code", {
  x <- rpc_inputs("sample")
  r <- x$reports[["T-14-1-1"]]
  r$steps$content <- "content <- report_content(1)"
  code <- rpc_program(x, r)
  expect_true(any(startsWith(code, "# ---- data: this report's normalized ARD")))
  expect_true(any(startsWith(code, "# ---- content: your code")))
  expect_true(any(grepl("(^|\\n)ard <- readRDS", code)))
})

test_that("a table of records whose content is its own code: records, then the ARD", {
  x <- rpc_inputs("sample")
  r <- rpc_records_table(x)
  r$steps$content <- "content <- report_content(1)"
  code <- rpc_program(x, r)
  at <- function(p) which(startsWith(code, p))[1L]
  expect_lt(at("# ---- data: this report's records"), at("# ---- data: this report's normalized ARD"))
  expect_lt(at("# ---- data: this report's normalized ARD"), at("# ---- content: your code"))
})

test_that("a table with no table_data template and its own content code: the ARD is a TODO", {
  x <- rpc_inputs("sample")
  r <- x$reports[["T-14-1-1"]]
  r$steps$content <- "content <- report_content(1)"
  code <- rpc_program(x, r, standards = rpc_without_template(x, "table_data"))
  expect_true(any(grepl("the ARD of T-14-1-1.R is still to be written", code, fixed = TRUE)))
})

# ---- a listing ------------------------------------------------------------------

test_that("a listing of the listing sheets takes its Rework", {
  x <- rpc_inputs("sample")
  r <- x$reports[["L-16-2-7"]]
  r$process <- "data <- head(data, 5)"
  code <- rpc_program(x, r, part = "data")
  expect_true(any(grepl("head(data, 5)", code, fixed = TRUE)))
})

test_that("a template listing: its records as `data`, its Rework after them, its plan", {
  x <- rpc_inputs("sample")
  r <- rpc_template_listing(x)
  r$process <- "data <- head(data, 5)"
  data <- rpc_program(x, r, part = "data")
  expect_true(any(data == "# the listing's records"))
  expect_true(any(data == "# its rework"))
  code <- rpc_program(x, r)
  expect_true(any(startsWith(code, "# ---- data: the listing's records")))
  # (a listing of the listing sheets: its pages)
  expect_true(any(startsWith(rpc_program(x, x$reports[["L-16-2-7"]]), "# ---- data: the listing's pages")))
  expect_true(any(startsWith(code, "# the listing, as spec/outputs/L-16-2-7.yml defines it")))
})

test_that("a template listing with no Rework: its records only", {
  x <- rpc_inputs("sample")
  data <- rpc_program(x, rpc_template_listing(x), part = "data")
  expect_true(any(data == "# the listing's records"))
  expect_false(any(data == "# its rework"))
})

test_that("a template listing whose definition does not hold: the data part says so", {
  x <- rpc_inputs("sample")
  r <- rpc_template_listing(x)
  r$spec_output$template <- list(name = "no_such_template")
  data <- rpc_program(x, r, part = "data")
  expect_true(any(data == "# the listing's definition does not hold: see the report part"))
})

test_that("a listing whose content is its own code: its data from the listing sheets", {
  x <- rpc_inputs("sample")
  r <- x$reports[["L-16-2-7"]]
  r$steps$content <- "content <- as_rtftables(data)"
  code <- rpc_program(x, r)
  expect_true(any(startsWith(code, "# ---- data: the listing's data")))
  expect_true(any(startsWith(code, "# ---- content: your code")))
})

test_that("a listing of no listing rows whose content is its own code: its datasets read", {
  x <- rpc_inputs("sample")
  r <- x$reports[["L-16-2-7"]]
  r$steps$content <- "content <- as_rtftables(adae)"
  r$listing <- NULL
  r$datasets <- "ADAE"
  code <- rpc_program(x, r)
  expect_true(any(code == "# the report's data: ADAE (data catalog)"))
})

# ---- a figure -------------------------------------------------------------------

test_that("a figure with no design and no figure_plot template: a TODO plot", {
  x <- rpc_inputs("edited")
  code <- rpc_program(x, x$reports[["F-94"]], part = "data",
                      standards = rpc_without_template(x, "figure_plot"))
  expect_true(any(code == "# TODO: the plot, e.g."))
  expect_true(any(grepl("the plot of F-94.R is still to be written", code, fixed = TRUE)))
  # (with the company's figure_plot: its lines, no TODO of tflspec's)
  code <- rpc_program(x, x$reports[["F-94"]], part = "data")
  expect_false(any(code == "# TODO: the plot, e.g."))
})

test_that("a figure's key with no ARD source writes no ARD lines", {
  x <- rpc_inputs("edited")
  r <- x$reports[["F-9"]]
  r$ard_source <- list(kind = "none")
  code <- rpc_program(x, r)
  expect_false(any(grepl("ard_fingerprint(", code, fixed = TRUE)))
  expect_true(any(grepl("record_report(report_id)", code, fixed = TRUE)))
})

test_that("a design given as a plain list is the design it was made from", {
  x <- rpc_inputs("sample")
  r <- x$reports[["F-14-2-4"]]
  want <- rpc_program(x, r)
  r$design <- unclass(r$design)
  expect_identical(rpc_program(x, r), want)
})

test_that("a design with its own content code: the design's data part, then the code", {
  x <- rpc_inputs("sample")
  r <- x$reports[["F-14-2-4"]]
  r$steps$content <- "content <- plot"
  code <- rpc_program(x, r)
  expect_true(any(startsWith(code, "# ---- data: the figure's data")))
  expect_true(any(startsWith(code, "# ---- content: your code")))
  expect_false(any(startsWith(code, "# ---- plot")))
})

test_that("a design with its own data code: the code, then the design's plot part", {
  x <- rpc_inputs("sample")
  r <- x$reports[["F-14-2-4"]]
  r$steps$data <- "df <- adsl"
  code <- rpc_program(x, r)
  expect_true(any(startsWith(code, "# ---- data: your code")))
  expect_true(any(startsWith(code, "# ---- content: the figure")))
  expect_true(any(startsWith(code, "# ---- plot")))
})

test_that("a design that reads only analysis data: the data made by its ARD definition", {
  x <- rpc_inputs("sample")
  r <- x$reports[["F-14-2-4"]]
  r$design <- tfl_fig_design(data = list(list(step = "read", data = "adsl_saf")),
                             layers = list(list(layer = "point", x = "AGE", y = "AGE")))
  code <- rpc_program(x, r)
  expect_true(any(code == "# the report's analysis data (its ARD definition)"))
  expect_false(any(startsWith(code, "# the report's subjects")))
})

test_that("a design that reads a dataset and analysis data: the subjects with the data", {
  x <- rpc_inputs("sample")
  r <- x$reports[["F-14-2-4"]]
  r$design <- tfl_fig_design(data = list(list(step = "read", dataset = "ADSL"),
                                         list(step = "read", data = "adsl_saf")),
                             layers = list(list(layer = "point", x = "AGE", y = "AGE")))
  code <- rpc_program(x, r)
  expect_true(any(code == paste0("# the report's subjects: its analysis set (its data's population), ",
                                 "and its analysis data (its ARD definition)")))
})

# ---- the program, its arguments ---------------------------------------------------

test_that("a description that is not one string is left out of the banner", {
  x <- rpc_inputs("sample")
  r <- x$reports[["T-14-1-1"]]
  r$description <- c("one", "two")
  expect_false(any(grepl("Description:", rpc_program(x, r), fixed = TRUE)))
  r$description <- NA_character_
  expect_false(any(grepl("Description:", rpc_program(x, r), fixed = TRUE)))
})

test_that("the report part's content is the plan for a table, `content` for a listing, when not given", {
  x <- rpc_inputs("sample")
  r <- x$reports[["T-14-1-1"]]
  ctx <- list(report = utils::modifyList(r, list(ard_source = list(kind = "none"))),
              study = x$study, shared = x$shared, standards = x$standards, layout = x$layout)
  code <- .with_code_context(.rp_report_code_lines(ctx, TRUE),
                             .rp_context(x$attached, TRUE, NULL, x$layout))
  expect_identical(code, .with_code_context(.rp_report_code_lines(ctx, TRUE, "plan"),
                                            .rp_context(x$attached, TRUE, NULL, x$layout)))
  ctx$report <- x$reports[["L-16-2-7"]]
  code <- .with_code_context(.rp_report_code_lines(ctx, FALSE),
                             .rp_context(x$attached, TRUE, NULL, x$layout))
  expect_true(any(grepl("content", code, fixed = TRUE)))
})

test_that("the template series' folders reach the code context", {
  d <- withr_tempdir()
  expect_identical(.rp_context(character(), FALSE, d, tfl_study_layout()), list(series_dir = d))
  x <- rpc_inputs("sample")
  code <- rpc_program(x, x$reports[["T-14-1-1"]], series_dir = d)
  expect_true(any(startsWith(code, "#  Program    :")))
})

test_that("`shared` that is not a list stops", {
  expect_error(tfl_report_program(list(id = "T-1", type = "table", program = "T-1.R"),
                                  shared = "x"),
               "`shared` is a list of the study's values")
})

test_that("an ARD program's banner names the study ARD's default file when the study table has none", {
  sp <- tfl_ard_spec(list(
    datasets = data.frame(dataset = "ADSL", path = "data/adam/adsl.rds"),
    populations = data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analyses = data.frame(output_id = "T1", analysis_id = "AGE", method = "continuous",
                          population_id = "SAF", by = "TRT01A", variables = "AGE")))
  sp$study <- data.frame(key = "output", value = NA_character_)
  code <- tfl_ard_program(sp, "T1", date = as.Date("2026-01-01"))
  expect_true(any(grepl("T1 -> its rows of output/ard/ard.rds", code, fixed = TRUE)))
  sp$study <- data.frame(key = "output", value = "out/study_ard.rds")
  code <- tfl_ard_program(sp, "T1", date = as.Date("2026-01-01"))
  expect_true(any(grepl("T1 -> its rows of out/study_ard.rds", code, fixed = TRUE)))
})
