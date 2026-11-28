# The study's setup programs (tflspec#910, G5-1b), moved from tflplanner:
#
#   programs/company_setup.R  the company's code: a seed, written when the
#                             study is made and never again
#   programs/study_setup.R    the study's folders, what it is, its rounding
#                             and data, the helpers; it runs the company's
#                             code first and the user's last
#   programs/study_user.R     the user's code: a seed
#   programs/study_helpers.R  the functions the programs call
#   programs/tfl/report_setup.R  the study's font, tokens, header, footer
#   programs/tfl/fig_setup.R  the figure style and its helpers
#
# Each is a function of values (the study's fields and spec, the company's
# inputs, the layout, the date): no session, no option, no tflplanner home.
# The text is tflplanner's as it was, byte for byte (its words included:
# design Q1, option A), checked against golden fixtures
# (tests/testthat/fixtures/setup-family/).

# the files, by the name tfl_setup_program() takes
.setup_file_names <- c(study_setup = "study_setup.R", company_setup = "company_setup.R",
                       study_user = "study_user.R", study_helpers = "study_helpers.R",
                       report_setup = "report_setup.R", fig_setup = "fig_setup.R")

# the company's inputs the setup programs read (the company standards' tables)
.setup_standard_keys <- c("about", "setup_code", "figure_settings", "figure_colors",
                          "figure_markers")

# the study's fields study_setup.R defines (study_id, study_title ...)
.setup_study_fields <- c("study_id", "title", "short_title", "compound", "phase",
                         "description")

# the layout's folders of the input data: read from the data root
.setup_data_keys <- c("adam", "sdtm", "other")

# where each setup file goes, relative to the study folder: the study's
# setup files next to the folders of the ARD and report programs, the
# report and figure setups in the reports' folder
.setup_paths <- function(layout) {
  prog <- dirname(layout[["programs_ard"]])
  f <- .setup_file_names
  c(study_setup = file.path(prog, f[["study_setup"]]),
    company_setup = file.path(prog, f[["company_setup"]]),
    study_user = file.path(prog, f[["study_user"]]),
    study_helpers = file.path(prog, f[["study_helpers"]]),
    report_setup = file.path(layout[["programs_tfl"]], f[["report_setup"]]),
    fig_setup = file.path(layout[["programs_tfl"]], f[["fig_setup"]]))
}

# the study's folders as study_setup.R names them (path_adam ...)
.setup_path_vars <- function(layout) {
  stats::setNames(unname(layout), paste0("path_", names(layout)))
}

#' tflspec's built-in company inputs
#'
#' The company's inputs the study's programs read when a company has none
#' of its own: `about` (the standards' name, version, owner and date,
#' which a new study's `company_setup.R` names), `setup_code` (the lines
#' of R a new study's `company_setup.R` starts with, one a row, each with a
#' note: the packages every program uses, `options(warn = 1)` and the days
#' in a week, a month and a year, [tfl_time_unit_days()]) and
#' `code_templates` (the code a report program writes where the report says
#' none: `table_data` takes a table's rows of the study ARD, `table_process`
#' normalizes them, `figure_plot` is a figure's plot before it is written,
#' `setup` the code every report of a new study runs first, blank here).
#' The figure style's are [tfl_fig_style()], the ARD catalogs'
#' [tfl_ard_statistics()] and [tfl_ard_methods()].
#'
#' @return A list of three data frames: `about` (`key`, `value`),
#'   `setup_code` (`code`, `note`) and `code_templates` (`name`, `code`,
#'   `note`).
#' @seealso [tfl_setup_program()], [tfl_report_program()]
#' @examples
#' tfl_builtin_standards()$setup_code$code
#' @keywords internal
#' @export
tfl_builtin_standards <- function() {
  d <- tfl_time_unit_days()
  list(
    about = data.frame(key = c("name", "version", "owner", "updated"),
                       value = c("tflplanner draft standards", "0.1", "", "2026-09-27"),
                       stringsAsFactors = FALSE),
    # (no tflspec: the programs call the study's programs/study_helpers.R)
    setup_code = data.frame(
      code = c("library(cards)", "library(dplyr)", "library(rtfreporter)",
               "options(warn = 1)",
               # (as.character: not format(), which follows options(digits))
               sprintf("%s <- %s", names(d), as.character(unname(d)))),
      note = c("the ARD programs' cards calls",
               "the programs' data steps (filter(), mutate() ...)",
               "the report programs' rtfreporter calls",
               "warnings are printed as they happen: a run's log shows where",
               "the days in a week, a month, a year: a figure's time unit divides by them",
               NA, NA),
      stringsAsFactors = FALSE),
    # (a template's {OUTPUT_ID}, {ARD}, {ARD_PROGRAM}, {PROGRAM} and
    # {STUDY_ID} stand for the report's: tfl_report_program())
    code_templates = data.frame(
      name = c("table_data", "table_process", "figure_plot", "setup"),
      code = c(paste(
        "# this report's rows of the study ARD (made by {ARD_PROGRAM})",
        "ard <- readRDS(\"{ARD}\") |>",
        "  dplyr::filter(output_id == report_id) |>",
        "  dplyr::select(-c(output_id, analysis_id, population_id))",
        "if (!nrow(ard)) {",
        "  stop(\"The study ARD has no rows for \", report_id, \": run {ARD_PROGRAM} first.\")",
        "}",
        sep = "\n"),
        "data <- normalize_ard(ard)",
        paste(
        "# TODO: the plot (ggplot2), in the figure style of the company standards, e.g.",
        "#   plot <- ggplot2::ggplot(adsl, ggplot2::aes(AGE, fill = TRT01A)) +",
        "#     ggplot2::geom_histogram() + scale_fill_tfl(\"treatment\", levels(adsl$TRT01A)) +",
        "#     theme_tfl()",
        sep = "\n"),
        NA),
      note = c(
        "a table's data code when it has none: its rows of the study ARD",
        "a table's normalization when it has none: after table_data",
        "a figure's plot before it is written (a stop() follows it)",
        "the setup code every report of a new study runs first (blank: none)"),
      stringsAsFactors = FALSE))
}

#' A study's setup program
#'
#' The text of one of the setup programs every program of a study runs
#' first, from values alone: the study's fields and spec, the company's
#' inputs, the folder layout and the date.  Nothing is read from a file,
#' an option or a home, and nothing is written.
#'
#' * `study_setup`: `programs/study_setup.R`, what every program starts
#'   with: it runs the company's code (`company_setup.R`) first, defines the
#'   study's folders (`path_adam` ...: the `layout`) and fields
#'   (`study_id`, `study_title` ...: `meta`), sets its rounding and reads its
#'   data ([tfl_study_setup_code()] of `study`), sources the helpers, and
#'   runs the user's code (`study_user.R`) last, once per program run.
#' * `company_setup`: `programs/company_setup.R`, a seed: the company's
#'   setup code (`standards$setup_code`, `{STUDY_ID}` filled in), with
#'   two lines that say which standards (`standards$about`) and when.
#' * `study_user`: `programs/study_user.R`, a seed: a comment that says what
#'   goes there.
#' * `study_helpers`: `programs/study_helpers.R`: [tfl_helpers_code()] under
#'   a banner.
#' * `report_setup`: `programs/tfl/report_setup.R`, which every report
#'   program sources: the study's setup, the reports' font, tokens, header
#'   and footer ([tfl_report_setup_code()] of `study`) and
#'   `record_report()`, which records a report as made with the fingerprint
#'   of the study's setup in `report_status.csv`.
#' * `fig_setup`: `programs/tfl/fig_setup.R`: the study's setup, the
#'   packages the figures use, the figure style and its helpers
#'   ([tfl_fig_setup_code()] of the company's figure style) and
#'   `report_content()`, `fig_sized()` and `report_data()`, which a report of
#'   one's own code ends with.
#'
#' A seed is written when the study is made and never again; the others
#' are written again whenever they change.
#'
#' @param file Which program: `"study_setup"`, `"company_setup"`,
#'   `"study_user"`, `"study_helpers"`, `"report_setup"` or `"fig_setup"`.
#' @param meta The study's fields (study.yml's): a list with `study_id`,
#'   `title`, `short_title`, `compound`, `phase` and `description`; one left
#'   out is `NA`.
#' @param study The study's spec ([tfl_read_study_spec()]), or a table or
#'   report spec of the sheets: `study_setup.R`'s rounding and data and
#'   `report_setup.R`'s font, tokens, header and footer.  `NULL`: none.
#' @param standards The company's inputs: a list with any of `about`,
#'   `setup_code`, `figure_settings`, `figure_colors` and `figure_markers`,
#'   the company standards' tables.  One left out is tflspec's own
#'   ([tfl_builtin_standards()], [tfl_fig_style()]).
#' @param attached The packages the study's setup files attach
#'   ([tfl_setup_values()]): the report and figure setups call them without
#'   `pkg::`.
#' @param study_code `TRUE` (the default): the report and figure setups as
#'   a study's programs have them -- the folders through `study_setup.R`'s
#'   variables, `attached` without `pkg::`, the rounding left to
#'   `study_setup.R` ([tflspec_code_options]).  `FALSE`: as tflspec writes
#'   code on its own.
#' @param fig_libs The packages the study's designed figures use:
#'   `fig_setup.R` attaches ggplot2, patchwork and dplyr, then these.
#' @param layout The study's folders ([tfl_study_layout()]).
#' @param date The date the banner says (and `company_setup.R`'s copy date).
#' @param generator What made the program, as its banner's `Generated` line
#'   says it.
#' @return The program, one element per line (an element may hold more
#'   than one line).
#' @seealso [tfl_study_layout()], [tfl_setup_values()]
#' @examples
#' meta <- list(study_id = "ABC-1", title = "A study of Drug X")
#' head(tfl_setup_program("study_setup", meta = meta), 12)
#' tfl_setup_program("company_setup", meta = meta)
#' @keywords internal
#' @export
tfl_setup_program <- function(file, meta = list(), study = NULL, standards = list(),
                              attached = character(), study_code = TRUE,
                              fig_libs = character(), layout = tfl_study_layout(),
                              date = Sys.Date(),
                              generator = paste("tflspec", utils::packageVersion("tflspec"))) {
  fn <- "tfl_setup_program()"
  # (the values as the caller has them: worked out before the context is set)
  force(meta)
  force(study)
  force(standards)
  force(attached)
  force(fig_libs)
  force(date)
  force(generator)
  if (!is.character(file) || length(file) != 1L || !file %in% names(.setup_file_names)) {
    stop(fn, ": `file` is one of ", paste(names(.setup_file_names), collapse = ", "), ".",
         call. = FALSE)
  }
  if (!is.list(standards) || (length(standards) &&
      (is.null(names(standards)) || !all(names(standards) %in% .setup_standard_keys)))) {
    stop(fn, ": `standards` is a list with any of ",
         paste(.setup_standard_keys, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.null(meta) && !is.list(meta)) {
    stop(fn, ": `meta` is a list of the study's fields (study_id, title ...).", call. = FALSE)
  }
  layout <- .layout_check(layout, c("programs_ard", "programs_tfl", "tfl"), fn)
  vars <- .setup_path_vars(layout)
  # the options the makers read, from these values alone (.with_code_context())
  context <- switch(file,
    study_setup = list(paths = vars),
    report_setup = , fig_setup = if (isTRUE(study_code)) {
      list(paths = vars, attached = as.character(attached), study_setup = TRUE)
    } else list(),
    list())
  .with_code_context(switch(file,
    study_setup = .setup_study_setup(meta, study, layout, date, generator),
    company_setup = .setup_company_seed(meta, standards, layout, date),
    study_user = .setup_user_seed(layout),
    study_helpers = .setup_helpers(layout, date, generator),
    report_setup = .setup_report(study, layout, date, generator),
    fig_setup = .setup_fig(standards, fig_libs, layout, date)), context)
}

# A program's banner: a bar, the lines, a bar (a blank line a lone "#")
.program_banner <- function(...) {
  bar <- strrep("=", 76)
  body <- c(...)
  c(paste("#", bar), ifelse(nzchar(body), paste0("#  ", body), "#"),
    paste("#", bar))
}

.setup_generated <- function(generator, date) {
  paste0("Generated  : ", generator, ", ", format(date, "%Y-%m-%d"))
}

# the line a setup file starts with
.setup_source_line <- function(layout) {
  sprintf("source(%s)", encodeString(.setup_paths(layout)[["study_setup"]], quote = "\""))
}

# a value of the company's inputs, else tflspec's own
.setup_standard <- function(standards, key) {
  if (key %in% names(standards)) standards[[key]] else tfl_builtin_standards()[[key]]
}

# ---- programs/study_setup.R

.setup_study_setup <- function(meta, study, layout, date, generator) {
  p <- .setup_paths(layout)
  f <- .setup_file_names
  q <- function(x) encodeString(x, quote = "\"")
  c(.program_banner(
      paste("Program    :", p[["study_setup"]]),
      "What every program of the study starts with (ard_setup.R, report_setup.R",
      "and fig_setup.R source it).  tflplanner writes it on every save: do not",
      paste("edit it.  The company's code is in", f[["company_setup"]], "and yours in"),
      paste0(f[["study_user"]], ": this file runs them, the company's first and yours last."),
      .setup_generated(generator, date)),
    "",
    "# This file runs once per program run: a program sets the option",
    "# tflplanner.run when it starts (and clears it at its end), and this",
    "# file sourced again in that run (a figure report: report_setup.R, then",
    "# fig_setup.R) is skipped, so the company's code and yours run once and",
    "# what yours sets stays.  Sourced without a program's run (by hand, the",
    "# Plot Designer), it runs each time.  To run it again by hand:",
    "# rm(.tfl_free_ran)",
    sprintf(".tfl_free <- c(company = %s, user = %s)",
            q(p[["company_setup"]]), q(p[["study_user"]])),
    ".tfl_free_key <- paste(getwd(), getOption(\"tflplanner.run\", NA))",
    ".tfl_free_run <- is.null(getOption(\"tflplanner.run\")) ||",
    "  !identical(get0(\".tfl_free_ran\", envir = environment(), inherits = FALSE),",
    "             .tfl_free_key)",
    "if (.tfl_free_run) {",
    "",
    paste0("# ---- the company's code (", f[["company_setup"]], ")"),
    "if (file.exists(.tfl_free[[\"company\"]])) source(.tfl_free[[\"company\"]], local = environment())",
    "",
    .setup_study_body(meta, study, layout),
    paste0("# ---- your code (", f[["study_user"]], ")"),
    "if (file.exists(.tfl_free[[\"user\"]])) source(.tfl_free[[\"user\"]], local = environment())",
    ".tfl_free_ran <- .tfl_free_key",
    "}")
}

# The generated lines of study_setup.R: the study's folders and what the
# study is, as R variables, the study's rounding and data
# (tfl_study_setup_code(): the folders through the variables above them)
# and the helpers
.setup_study_body <- function(meta, study, layout) {
  val <- function(v) {
    v <- as.character(v %||% NA_character_)
    if (length(v) != 1L || is.na(v)) "NA_character_" else
      encodeString(enc2utf8(v), quote = "\"")
  }
  nm <- function(x) formatC(x, width = -max(nchar(x)))
  paths <- paste0("path_", names(layout))
  ids <- paste0("study_", sub("^study_", "", .setup_study_fields))
  rnd <- if (is.null(study)) character() else tfl_study_setup_code(study)
  c("# ---- the study: its folders, relative to the study folder (the programs",
    "#      run from it), and what it is (study.yml)",
    "# the data root: an official run copies the programs into its batch folder",
    "# and reads the data where run_paths.R says (the study's data folders);",
    "# anywhere else it is the study folder.  company_setup.R runs before these",
    "# lines: company code cannot use path_adam, path_sdtm or path_other.",
    ".tfl_data_root <- if (file.exists(\"run_paths.R\")) { source(\"run_paths.R\"); .tfl_data_root } else \".\"",
    paste(nm(paths), "<-", ifelse(names(layout) %in% .setup_data_keys,
      sprintf("file.path(.tfl_data_root, %s)", vapply(layout, val, "")),
      vapply(layout, val, ""))),
    "",
    paste(nm(ids), "<-", vapply(.setup_study_fields, function(k) val(meta[[k]]), "")),
    "",
    if (length(rnd)) c(rnd, ""),
    "# the functions the programs call (tflplanner writes them)",
    sprintf("source(%s)", encodeString(.setup_paths(layout)[["study_helpers"]], quote = "\"")),
    "")
}

# ---- the seeds: programs/company_setup.R and programs/study_user.R

.setup_company_seed <- function(meta, standards, layout, date) {
  about <- .setup_standard(standards, "about")
  v <- function(k) {
    x <- about$value[match(k, about$key)]
    if (length(x) && !is.na(x) && nzchar(x)) x else NA_character_
  }
  std <- paste(stats::na.omit(c(v("name"), v("version"))), collapse = " ")
  if (!nzchar(std)) std <- "built-in"
  upd <- v("updated")
  # the company's lines, the study's id in
  d <- .setup_standard(standards, "setup_code")
  code <- if (is.null(d)) character() else d$code
  code[is.na(code)] <- ""
  id <- meta[["study_id"]] %||% NA
  if (!is.na(id)) code <- gsub("{STUDY_ID}", id, code, fixed = TRUE)
  c(paste0("# The company's code (", .setup_paths(layout)[["company_setup"]],
           "): copied from the company"),
    paste0("# standards (", std, if (!is.na(upd)) paste0(" of ", upd), ") on ",
           format(date, "%Y-%m-%d"), ".  The study's own now: tflplanner"),
    "# never writes it again.  Every program runs it first (study_setup.R).",
    "",
    code)
}

.setup_user_seed <- function(layout) {
  c(paste0("# Your study's code (", .setup_paths(layout)[["study_user"]],
           "): helper functions, options,"),
    "# library() lines.  Every program runs it after the study's setup",
    "# (study_setup.R), before the ARD, report or figure setup.  tflplanner never",
    "# writes it again.  Values the study's YAML defines (the rounding, the",
    "# folders, the formats) are set there, not here.",
    "")
}

# ---- programs/study_helpers.R

.setup_helpers <- function(layout, date, generator) {
  c(.program_banner(
      paste("Program    :", .setup_paths(layout)[["study_helpers"]]),
      "The functions the study's programs call: the code lists on a data,",
      "the ARD's ids and formats, the study ARD.  The study's setup sources it.",
      .setup_generated(generator, date)),
    "",
    tfl_helpers_code())
}

# ---- programs/tfl/report_setup.R

.setup_report <- function(study, layout, date, generator) {
  own <- if (!is.null(study)) tfl_report_setup_code(study)
  c(.program_banner(
      paste("Program    :", .setup_paths(layout)[["report_setup"]]),
      "The study's font, header, footer and tokens: every report program sources it.",
      .setup_generated(generator, date),
      "",
      "Written from spec/defaults.yml: change them there (the study tab),",
      "then generate the programs again."),
    "",
    "# the study's setup: the company's, the study's folders and id, your own",
    .setup_source_line(layout),
    "",
    if (length(own)) c(own, ""),
    .setup_record_report(layout),
    "")
}

# The small function a report program ends with, written in
# report_setup.R: what was made, and with which study setup, into
# output/tfl/report_status.csv (tflplanner reads it: a report made with
# another study setup than the one there now is outdated), and -- a
# listing says it -- whether the report has no rows (`empty`: "TRUE",
# "FALSE", "" not said; the official run's batch.R notes an empty
# report).  The study setup's fingerprint: the md5s of company_setup.R,
# study_setup.R and study_user.R joined by "+" (those that are there),
# then "|" and report_setup.R's.
.setup_record_report <- function(layout) {
  p <- .setup_paths(layout)
  q <- function(x) paste(encodeString(x, quote = "\""), collapse = ", ")
  sf <- file.path(layout[["tfl"]], "report_status.csv")
  hash <- sprintf(paste0("paste(paste(unname(tools::md5sum(Filter(file.exists, c(%s)))),\n",
                         "                collapse = \"+\"),\n",
                         "          if (file.exists(%s)) unname(tools::md5sum(%s)) else \"\",\n",
                         "          sep = \"|\")"),
                  q(p[c("company_setup", "study_setup", "study_user")]),
                  q(p[["report_setup"]]), q(p[["report_setup"]]))
  c("# what was made, with which study setup, and -- a figure printing an",
    "# ARD's numbers -- from which definition of that ARD (tflplanner reads it);",
    "# `empty`: TRUE when the report has no rows (the official run notes it)",
    "record_report <- function(output_id, ard = \"\", empty = NA) {",
    paste0("  sf <- ", encodeString(sf, quote = "\"")),
    # the study setup's fingerprint on a line of its own
    paste0("  setup <- ", hash),
    "  row <- data.frame(output_id = output_id,",
    "                    built = format(Sys.time(), \"%Y-%m-%d %H:%M:%S\"),",
    "                    setup = setup,",
    "                    ard = ard,",
    "                    empty = if (isTRUE(empty)) \"TRUE\" else if (isFALSE(empty)) \"FALSE\" else \"\",",
    "                    stringsAsFactors = FALSE)",
    "  dir.create(dirname(sf), recursive = TRUE, showWarnings = FALSE)",
    "  st <- if (file.exists(sf)) utils::read.csv(sf, colClasses = \"character\")",
    "  if (!is.null(st) && nrow(st)) {",
    "    for (k in setdiff(names(row), names(st))) st[[k]] <- \"\"",
    "    row <- rbind(st[st$output_id != output_id, names(row), drop = FALSE], row)",
    "  }",
    "  utils::write.csv(row, sf, row.names = FALSE)",
    "  invisible(output_id)",
    "}")
}

# ---- programs/tfl/fig_setup.R

# fig_setup.R: the study's setup, the packages the figures' code uses (the
# designed figures': ggsurvfit for a KM ...), the company's figure style
# (tfl_fig_setup_code()), and the functions a report of one's own code ends
# with
.setup_fig <- function(standards, fig_libs, layout, date) {
  # (a table the company has not given: tflspec's own)
  pick <- function(key, part) {
    if (key %in% names(standards)) standards[[key]] else tfl_fig_style()[[part]]
  }
  style <- list(settings = pick("figure_settings", "settings"),
                colors = pick("figure_colors", "colors"),
                markers = pick("figure_markers", "markers"))
  code <- tfl_fig_setup_code(style, date = date)
  # after its banner (the comment lines it starts with)
  at <- match(FALSE, grepl("^#", code), nomatch = length(code) + 1L) - 1L
  rest <- utils::tail(code, length(code) - at)
  rest <- rest[cumsum(nzchar(rest)) > 0L]
  libs <- c("ggplot2", "patchwork", "dplyr")
  libs <- c(libs, sort(setdiff(unique(as.character(fig_libs)), libs)))
  c(utils::head(code, at),
    if (at) "",
    "# the study's setup: the company's, the study's folders and id, your own",
    .setup_source_line(layout),
    "",
    "# the packages the figures' code uses",
    paste0("library(", libs, ")"),
    "",
    rest,
    "",
    .setup_content_contract,
    "",
    .setup_report_content)
}

# The content contract of a report's own code (canon 11-28), written once in
# fig_setup.R before report_content() (whose text stays as it was: the
# app's preview has a copy of it): report_content_check() -- a table's code
# gives a table_plan; a figure's a ggplot, an rtfplot, a data frame or
# rtftable pages (or a list of them), as report_content() takes them.
# Anything else stops with a condition of class `tfl_content_contract` that
# names the report and what it gave.  A session runs this same text
# (.own_content_contract()).
.setup_content_contract <- c(
  "# ---- the content contract of a report's own code: a table's code gives a",
  "# table_plan; a figure's a ggplot, an rtfplot, a data frame or rtftable pages",
  "# (or a list of them); anything else stops (class tfl_content_contract)",
  "report_content_check <- function(x, kind, id) {",
  "  fig <- identical(kind, \"figure\")",
  "  ok <- function(p) if (fig) is.data.frame(p) || inherits(p, c(\"rtftable\", \"rtfplot\", \"ggplot\")) else",
  "    inherits(p, \"table_plan\")",
  "  items <- if (fig && is.list(x) && !ok(x)) x else list(x)",
  "  bad <- if (length(items)) which(!vapply(items, ok, NA))[1L] else 0L",
  "  if (!is.na(bad)) {",
  "    got <- if (identical(bad, 0L)) \"an empty list\" else if (!identical(items, list(x)))",
  "      sprintf(\"a list whose item %d is a %s\", bad, class(items[[bad]])[1L]) else",
  "      sprintf(\"a %s\", class(x)[1L])",
  "    want <- if (fig) \"a ggplot (or rtfplot figures, a data frame or rtftable pages, or a list of them)\" else",
  "      \"a table_plan (rtfreporter::table_plan())\"",
  "    stop(structure(class = c(\"tfl_content_contract\", \"error\", \"condition\"), list(",
  "      message = sprintf(\"%s: a %s's content code must return %s; it returned %s.\", id, kind, want, got),",
  "      call = NULL)))",
  "  }",
  "  x",
  "}")

# The small functions a report's own code is checked with, written once in
# fig_setup.R (which those reports source): report_content() -- a ggplot
# (alone or in a list) becomes a figure; anything else than the contract's
# -- a data frame, rtftable pages, a ggplot, an rtfplot, or a list of them
# -- stops, saying which item it is and what it was; fig_sized(); and
# report_data() -- the data step left a data frame.
.setup_report_content <- c(
  "# ---- a report of your own code: its `content` as rtfreporter takes it",
  "# (a ggplot becomes a figure; what is not a data frame, rtftable pages, a",
  "# ggplot or an rtfplot, or a list of them, stops, saying which item)",
  "report_content <- function(x) {",
  "  ok <- function(p) is.data.frame(p) ||",
  "    inherits(p, c(\"rtftable\", \"rtfplot\", \"ggplot\"))",
  "  fig <- function(p) if (inherits(p, \"ggplot\")) rtfplot(p) else p",
  "  one <- ok(x)",
  "  items <- if (one) list(x) else if (is.list(x)) x else list(x)",
  "  for (i in seq_along(items)) if (!ok(items[[i]])) {",
  "    stop(sprintf(\"`content`%s is %s: make it a data frame, rtftable pages, a ggplot or an rtfplot (or a list of them).\",",
  "                 if (one || !is.list(x)) \"\" else sprintf(\" (item %d)\", i),",
  "                 class(items[[i]])[1L]), call. = FALSE)",
  "  }",
  "  if (one) fig(x) else lapply(items, fig)",
  "}",
  "",
  "# ---- a figure at its size on the page (the report's, step 3): drawn so,",
  "# its text the size its theme says (a ggplot; anything else as it is)",
  "fig_sized <- function(x, width_in, height_in, dpi = 300) {",
  "  one <- function(p) if (inherits(p, \"ggplot\")) rtfplot(p,",
  "    width_twips = round(width_in * 1440), height_twips = round(height_in * 1440),",
  "    render_width = width_in, render_height = height_in, render_dpi = dpi) else p",
  "  if (inherits(x, \"ggplot\") || !is.list(x) || is.data.frame(x)) one(x) else lapply(x, one)",
  "}",
  "",
  "# ---- a data step of your own code: it left a data frame",
  "report_data <- function(x, what = \"data\") {",
  "  if (!is.data.frame(x)) {",
  "    stop(sprintf(\"The data step's code leaves `%s` as %s: make it a data frame.\",",
  "                 what, class(x)[1L]), call. = FALSE)",
  "  }",
  "  x",
  "}")
