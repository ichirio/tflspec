# The study's report programs and ARD programs (tflspec#910, G5-1c), moved
# from tflplanner:
#
#   programs/tfl/<program>.R   one report: the study's setup, its data, its
#                              content, the report around it, its RTF
#   programs/ard/ard_setup.R   what every ARD program starts with
#   programs/ard/<id>.R        one output's analyses into the study ARD
#
# Each is a function of values: the report's values (what it is, its own
# code, its definition), the values its study shares between its reports,
# the company's inputs, the layout and the date -- no session, no option,
# no tflplanner home.  The text is tflplanner's as it was, byte for byte
# (its words included: design Q1, option A), checked against golden
# fixtures (tests/testthat/fixtures/programs-family/).

# ---- the values -----------------------------------------------------------

# A value of the inputs, worked out when it is used: the value itself, or a
# function of no arguments that gives it (a study's spec of its sheets is
# made only for the reports that need it), or the error making it gave
.rp_value <- function(v) {
  if (is.function(v)) v() else if (inherits(v, "error")) stop(v) else v
}

# the company's inputs the report and ARD programs read (the company
# standards' tables)
.rp_standard_keys <- c("code_templates", "ard_statistics", "ard_methods", "settings")

# a table of the company's inputs; one left out: tflspec's own (NULL for the
# ARD catalogs: tfl_ard_code() takes its own then)
.rp_standard <- function(standards, key) {
  if (key %in% names(standards)) return(standards[[key]])
  if (identical(key, "code_templates")) tfl_builtin_standards()$code_templates
}

# a setting of the company's (its settings table), else `default`
.rp_setting <- function(standards, key, default = NA_character_) {
  s <- .rp_standard(standards, "settings")
  v <- if (!is.null(s)) s$value[match(key, s$key)]
  if (length(v) && !is.na(v) && nzchar(v)) v else default
}

# a company's code template (code_templates: table_data, table_process,
# figure_plot); NA when it has none
.rp_template <- function(standards, name) {
  d <- .rp_standard(standards, "code_templates")
  out <- stats::setNames(d$code, d$name)
  out[name]
}

# The report's ARD code (tfl_ard_code()) with the company's catalogs
.rp_ard_code <- function(ctx, spec, ...) {
  tfl_ard_code(spec, ..., statistics = .rp_standard(ctx$standards, "ard_statistics"),
               methods = .rp_standard(ctx$standards, "ard_methods"))
}

# ---- the pieces every program has ------------------------------------------

.rp_r_string <- function(x) encodeString(x, quote = "\"")

.rp_code_block <- function(code) {
  code <- gsub("\r\n?", "\n", code)
  sub("\n+$", "", code)
}

# a program's section line, as tflspec writes its own (a designed figure's
# `# ---- data ----` and `# ---- plot ----`): to the 78th column
.rp_section <- function(title) {
  head <- paste("# ----", title, "")
  paste0(head, strrep("-", max(3L, 78L - nchar(head))))
}

# one element a line; a blank line stays (strsplit would drop it)
.rp_split_lines <- function(code) {
  unlist(lapply(code, function(l) {
    if (!nzchar(l)) "" else strsplit(l, "\n", fixed = TRUE)[[1L]]
  }))
}

# code without the blank lines it starts with
.rp_drop_leading_blank <- function(code) {
  code[cumsum(nzchar(code)) > 0L]
}

# Does this code assign `data` itself?  (Then it needs no default
# normalization after it.)
.rp_makes_data <- function(code) {
  any(grepl("(^|[^A-Za-z0-9_.])data[[:space:]]*(<-|=)[^=]",
            strsplit(code, "\n", fixed = TRUE)[[1L]]))
}

# A path as the report programs' own lines write it: through the variable
# of the folder that holds it (option tflspec.paths, the context's), else as
# it is.  (tflplanner's rule as it was, byte for byte; .path_code() also
# reads a backslash as a slash -- one rule is a later change of text.)
.rp_path_lit <- function(path) {
  q <- encodeString(path, quote = "\"")
  pv <- getOption("tflspec.paths")
  if (!length(pv)) return(q)
  dirs <- sub("/+$", "", as.character(pv))
  hit <- which(startsWith(path, paste0(dirs, "/")) | path == dirs)
  if (!length(hit)) return(q)
  k <- hit[which.max(nchar(dirs[hit]))]
  rest <- substring(path, nchar(dirs[k]) + 2L)
  if (!nzchar(rest)) return(names(pv)[k])
  sprintf("file.path(%s, %s)", names(pv)[k], encodeString(rest, quote = "\""))
}

# The lines without `pkg::` for the packages the study's setup attaches
# (option tflspec.attached, the context's): the `pkg::` of each call in the
# parsed code (a string or a comment keeps its text).  Code that does not
# parse is left as it is.  (tflplanner's rule as it was, byte for byte; see
# .drop_attached_ns().)
.rp_drop_attached <- function(code) {
  pk <- getOption("tflspec.attached")
  if (!length(pk) || !length(code)) return(code)
  txt <- paste(code, collapse = "\n")
  pd <- tryCatch(utils::getParseData(parse(text = txt, keep.source = TRUE)),
                 error = function(e) NULL)
  if (is.null(pd) || !nrow(pd)) return(code)
  pd <- pd[pd$terminal, , drop = FALSE]
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  at <- which(pd$token == "SYMBOL_PACKAGE" & pd$text %in% pk)
  at <- at[at < nrow(pd) & pd$token[at + 1L] == "NS_GET"]
  if (!length(at)) return(code)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  n <- lengths(strsplit(paste0(code, "\n"), "\n", fixed = TRUE))
  length(lines) <- sum(n)
  lines[is.na(lines)] <- ""
  for (i in rev(at[order(pd$line1[at], pd$col1[at])])) {
    s <- lines[pd$line1[i]]
    lines[pd$line1[i]] <- paste0(substr(s, 1L, pd$col1[i] - 1L),
                                 substr(s, pd$col2[i + 1L] + 1L, nchar(s)))
  }
  # the lines as they came (an element may hold several)
  ends <- cumsum(n)
  vapply(seq_along(code), function(i)
    paste(lines[(ends[i] - n[i] + 1L):ends[i]], collapse = "\n"), "")
}

# `report_id <- "<id>"`: what a report's program sets at its head (the data
# templates read it)
.rp_report_id_line <- function(output_id) paste("report_id <-", .rp_r_string(output_id))

# The lines a generated program starts its run with, after its study.yml
# check, and ends it with: the run's token (option tflplanner.run), which
# study_setup.R reads so that it runs once in the run
.rp_run_start <- c("# this run of the program: the study's setup runs once in it",
                   "options(tflplanner.run = basename(tempfile(\"run\")))")
.rp_run_end <- c("", "# the end of this run", "options(tflplanner.run = NULL)")
.rp_study_guard <- c(
  "stopifnot(\"run it from the study folder\" = file.exists(\"study.yml\"))",
  .rp_run_start)

# The report's own id in its program is `report_id` (set at its head): a
# program copied for another report changes it in one place.  The RTF's
# path through its folder's variable; its name, when it is the report's id,
# made from report_id.
.rp_report_id_code <- function(code, output_id) {
  lit <- encodeString(output_id, quote = "\"")
  gsub(sprintf("file.path(path_programs_tfl, %s)", lit),
       "file.path(path_programs_tfl, report_id)", code, fixed = TRUE)
}
.rp_report_path_code <- function(path, output_id) {
  if (!identical(basename(path), paste0(output_id, ".rtf"))) return(.rp_path_lit(path))
  dir <- .rp_path_lit(dirname(path))
  sprintf("file.path(%s, paste0(report_id, \".rtf\"))", dir)
}

# ---- one report's pieces ------------------------------------------------------

# Is the step the report's own code?
.rp_is_code_step <- function(r, step) {
  v <- r$steps[[step]]
  !is.null(v) && !is.na(v)
}

# What a step of its own code leaves
.rp_step_leaves <- function(type, step) {
  if (identical(step, "content")) "content" else if (identical(type, "figure")) "df" else "data"
}

# The report as its file says it, with the study's spec: list(output,
# study); NULL when the study's spec has no such report
.rp_y <- function(ctx) {
  o <- ctx$report$output
  if (is.null(o) || is.null(ctx$study)) return(NULL)
  list(output = o, study = ctx$study)
}

# A table's content as its program writes it: the report resolved (its
# defaults and pattern applied), each item without the layer it came from
# (the program says no "(study default)")
.rp_code_content <- function(y) {
  r <- tfl_resolve_output(y$output, y$study)
  if (is.list(r$content) && !inherits(r$content, "tfl_r")) {
    r$content <- lapply(r$content, function(it) {
      if (is.list(it)) it["from"] <- list(NULL)
      it
    })
  }
  r
}

# A company template filled for a report: `{OUTPUT_ID}`, `{ARD}` (the study
# ARD), `{ARD_PROGRAM}`, `{PROGRAM}` and `{STUDY_ID}` (none here); the
# report's ARD by its own name (data.object) in table_data and
# table_process
.rp_fill_template <- function(ctx, name) {
  code <- .rp_template(ctx$standards, name)
  if (!length(code) || is.na(code)) return(NA_character_)
  r <- ctx$report
  lay <- ctx$layout
  sub <- c(
    `{OUTPUT_ID}` = r$id,
    `{ARD}` = ctx$shared$study_ard,
    `{ARD_PROGRAM}` = file.path(lay[["programs_ard"]], .ard_program_name(r$id)),
    `{PROGRAM}` = file.path(lay[["programs_tfl"]], r$program),
    `{STUDY_ID}` = "")
  # the study ARD as a string of R: through its folder's variable while a
  # program is written
  code <- gsub("\"{ARD}\"", .rp_path_lit(sub[["{ARD}"]]), code, fixed = TRUE)
  for (k in names(sub)) code <- gsub(k, sub[[k]], code, fixed = TRUE)
  obj <- if (name %in% c("table_data", "table_process")) r$ard_object
  if (!is.null(obj) && !identical(obj, "ard")) {
    code <- gsub("(?<![[:alnum:]_./])ard(?![[:alnum:]_./])", obj, code, perl = TRUE)
  }
  .rp_drop_attached(unname(code))
}

# a report whose ARD was made elsewhere and taken in (ard_source =
# import:<file>) reads that file instead of the study ARD
.rp_imported_ard_code <- function(file, output_id) {
  p <- encodeString(file.path("input", "ard", file), quote = "\"")
  id <- encodeString(output_id, quote = "\"")
  paste(
    sprintf("# ---- this output's ARD, made elsewhere and taken in (%s)",
            file.path("input", "ard", file)),
    sprintf("ard <- tflspec::tfl_read_ard(%s)", p),
    sprintf("if (\"output_id\" %%in%% names(ard)) ard <- ard[ard$output_id == %s, , drop = FALSE]", id),
    "ard <- ard[setdiff(names(ard), c(\"output_id\", \"analysis_id\", \"population_id\"))]",
    sprintf("if (!nrow(ard)) stop(\"The ARD taken in has no rows for %s.\")", output_id),
    sep = "\n")
}

# A table's ARD code: the company's template (table_data) that takes its
# rows of the study ARD, or the ARD taken in
.rp_ard_code_of <- function(ctx) {
  r <- ctx$report
  if (!identical(r$type, "table")) return(NA_character_)
  if (is.null(r$ard_import)) .rp_fill_template(ctx, "table_data") else
    .rp_imported_ard_code(r$ard_import, r$id)
}

# Is a table's normalization the built-in one, in its plan's pipe?  When
# nothing makes `data` -- no process code, the ARD code (or the data step's
# own code) leaves `ard`, the company's template is blank or left at
# `data <- normalize_ard(ard)` -- and the plan is the definition's (not a
# content step of its own code, which reads `data`).
.rp_table_norm_builtin <- function(ctx, code = NULL, proc = NULL) {
  r <- ctx$report
  if (!identical(r$type, "table")) return(FALSE)
  if (.rp_is_code_step(r, "content")) return(FALSE)
  if (is.null(proc)) proc <- r$process
  if (!is.na(proc)) return(FALSE)
  if (is.null(code)) {
    code <- if (.rp_is_code_step(r, "data")) r$steps$data else .rp_ard_code_of(ctx)
  }
  if (!is.na(code) && .rp_makes_data(code)) return(FALSE)
  tp <- .rp_fill_template(ctx, "table_process")
  is.na(tp) || identical(trimws(tp), "data <- normalize_ard(ard)")
}

# A table's normalization after its ARD: its Rework (process code) after
# the table's normalize_ard(), or (when the code before it did not make
# `data`) the company's template; the built-in normalization is the table's
# plan's, no line here
.rp_table_process_lines <- function(ctx, code, proc) {
  if (.rp_table_norm_builtin(ctx, code, proc)) return(NULL)
  if (!is.na(proc)) {
    c("", "# normalize and rework", .rp_rework_lines(ctx, code, proc))
  } else if (!.rp_makes_data(code)) {
    c("", .rp_code_block(.rp_fill_template(ctx, "table_process")))
  }
}

# A table's Rework as its program writes it: `data <- ard |>
# normalize_ard(...)` (the table's own arguments), then the code, which
# starts from `data` and leaves `data`.  `code` the table's ARD code: one
# that makes `data` itself has no `ard` to normalize.
.rp_rework_lines <- function(ctx, code, proc) {
  c(if (is.na(code) || !.rp_makes_data(code)) .rp_rework_normalize_lines(ctx),
    .rp_code_block(proc))
}

# The check made of a Rework: it names `data` (code that does not parse is
# left to R, which says so when it runs)
.rp_rework_need_data <- function(proc) {
  ok <- tryCatch("data" %in% all.names(parse(text = proc, keep.source = FALSE)),
                 error = function(e) TRUE)
  if (!ok) stop("the data.steps leave no `data`, the data the plan is made from.", call. = FALSE)
  invisible(TRUE)
}

# The lines before a Rework, as the table's plan would say them: the
# content's normalize_ard() with its arguments (the lines before a marker
# step), else, with no spec, the sheets' (.rp_rework_normalize_sheets())
.rp_rework_mark <- "data <- data  # rework"
.rp_rework_normalize_lines <- function(ctx) {
  y <- .rp_y(ctx)
  if (!is.null(y)) {
    o <- .rp_code_content(y)
    o$data <- list(source = "ard", object = "ard", steps = list(tfl_r(.rp_rework_mark)))
    code <- tryCatch(tfl_content_code(o, pipe = "|>"), error = function(e) NULL)
    i <- match(.rp_rework_mark, code)
    if (!is.na(i) && i > 1L) return(code[seq_len(i - 1L)])
  }
  .rp_rework_normalize_sheets(ctx)
}

# The same from the table sheets' spec: the steps before table_plan() in the
# pipe tfl_table_code() writes for the table's plan, as `data <- ard |>
# normalize_ard(...)`
.rp_rework_normalize_sheets <- function(ctx) {
  id <- ctx$report$id
  sp <- .rp_value(ctx$shared$table_sheets)
  code <- tryCatch(
    tfl_table_code(sp, id, pipe = "|>", ard_spec = .rp_value(ctx$shared$ard_spec)),
    error = function(e) tryCatch(tfl_table_code(sp, id, pipe = "|>"),
                                 error = function(e) NULL))
  i <- match("plan <- ard |>", code)
  j <- grep("^  table_plan\\(", code)[1L]
  if (is.na(i) || is.na(j) || j <= i + 1L) return("data <- ard |> normalize_ard()")
  steps <- code[(i + 1L):(j - 1L)]
  steps[length(steps)] <- sub(" \\|>$", "", steps[length(steps)])
  one <- paste("data <- ard |>", trimws(steps[1L]))
  if (length(steps) == 1L && nchar(one) <= 80L) one else c("data <- ard |>", steps)
}

# A table of records (its YAML's `data: {source: dataset, ...}`): the code
# that makes its `data` (tfl_data_code()); NULL for a table of the ARD
.rp_table_dataset_lines <- function(ctx) {
  o <- ctx$report$spec_output
  if (is.null(o) || inherits(o$content, "tfl_r")) return(NULL)
  dc <- .rp_table_objects_code(o, ctx$study)
  if (!length(dc)) NULL else unlist(dc, use.names = FALSE)
}

# ... by object, its ARD left out: list(data = <lines>, adpc = <lines>)
.rp_table_objects_code <- function(o, study) {
  # (a template report: its records are the template's, once it is resolved)
  if (is.list(o$template)) {
    o <- tfl_resolve_output(o, study)
  }
  own <- identical(o$data[["source"]], "dataset")
  parts <- any(vapply(o$content %||% list(), function(it) identical(it$fn, "plan_append"), NA))
  if (!own && !parts) return(list())
  tfl_data_code(o, study, ard = FALSE)
}

# the lines that read one dataset of the catalog into an object named
# after it (adsl, adae ...), with its derived columns
.rp_read_dataset_lines <- function(ctx, dataset) {
  tfl_read_data_code(ctx$shared$ard$datasets, dataset)
}

# the part of a listing's program between its setup and its report (part
# "data": its data only; "content": laid out from a `data` made before);
# NULL: the listing has no rows of the listing sheets
.rp_listing_lines <- function(ctx, rework = NA, part = "all") {
  l <- ctx$report$listing
  if (is.null(l)) return(NULL)
  tfl_listing_code(
    .rp_value(ctx$shared$listings), ctx$report$id,
    ctx$shared$ard$datasets, rework = if (!is.na(rework)) .rp_code_block(rework),
    type = .rp_setting(ctx$standards, "listing_type", "multiline"),
    codelists = .rp_value(ctx$shared$codelists), part = part,
    ard = if (isTRUE(l$own)) ctx$shared$ard)
}

.rp_fig_source_lines <- function(layout) {
  c("# the figure style of the company standards, the figures' packages, and the figure checks",
    sprintf("source(%s)", encodeString(file.path(layout[["programs_tfl"]],
                                                 .setup_file_names[["fig_setup"]]), quote = "\"")))
}

# The report's analysis set (A2 R1): its spec's, by the one reader
# (.out_population_or_data(): its data's `population`, its `common:` until
# `common` goes, else its subjects entries'); NA when it has none
.rp_population <- function(ctx) {
  r <- ctx$report
  .out_population_or_data(r$spec_output %||% r$output %||% list())
}

# A figure's subjects (A2 R1, R4): the subjects of the report's analysis set,
# made once by the program's data part as the ARD programs make them
# (.fig_subjects_part(): the object <dataset>_sub), with the report's
# analysis data `data`; the records the figure reads are joined to them.
# list(code, subjects); NULL only when the report has no analysis set.  A
# set whose subjects cannot be made (none of the study's, no ARD definition)
# is a stop() line saying so, and no subjects: never every subject drawn
# (.fig_subjects_part())
.rp_fig_subjects <- function(ctx, data = character()) {
  pop <- .rp_population(ctx)
  if (is.na(pop)) return(NULL)
  id <- ctx$report$id
  stop_line <- function(msg) list(code = sprintf("stop(%s)", encodeString(msg, quote = """)),
                                  subjects = NULL)
  if (is.null(ctx$shared$ard)) return(stop_line(.fig_subjects_no_ard(id, pop)))
  x <- .as_spec(ctx$shared$ard, "ard", "tfl_report_program")
  # (written as tfl_ard_code(part = "data") writes it: for its setup, which
  # attaches cards and dplyr)
  user <- .attached()
  old <- options(tflspec.user_attached = user,
                 tflspec.attached = union(user, c(.ard_pkgs, .ard_base_pkgs)))
  on.exit(options(old), add = TRUE)
  sj <- tryCatch(.fig_subjects_part(x, id, pop, data = data),
                 tfl_fig_subjects = function(e) stop_line(conditionMessage(e)))
  if (!is.null(sj$subjects)) sj$code <- .drop_attached_ns(sj$code)
  sj
}

# ... its lines in the program: the subjects (and the analysis data) made
.rp_fig_subjects_lines <- function(sj, data = character()) {
  if (is.null(sj)) return(NULL)
  c(paste0("# the report's subjects: its analysis set (its data's population)",
           if (length(data)) ", and its analysis data (its ARD definition)"),
    sj$code, "")
}

# the part of a figure's program between its setup and its report: a
# figure with no design and no code of its own yet -- its datasets read,
# the report's subjects, the company's plot template or a TODO
.rp_figure_lines <- function(ctx) {
  r <- ctx$report
  ds <- r$datasets
  if (!length(ds)) return(NULL)
  sj <- .rp_fig_subjects(ctx)
  tpl <- .rp_fill_template(ctx, "figure_plot")
  plot <- c(
    if (!is.na(tpl)) .rp_code_block(tpl) else c(
      "# TODO: the plot, e.g.",
      "#   plot <- ggplot2::ggplot(adsl, ggplot2::aes(AGE)) + ggplot2::geom_histogram()"),
    paste0("stop(\"tflplanner: the plot of ", r$program,
           " is still to be written.\")"))
  setup <- file.path(ctx$layout[["programs_tfl"]], .setup_file_names[["fig_setup"]])
  c(c(paste0("# the data: ", paste(ds, collapse = ", "), " (data catalog)"),
      unlist(lapply(ds, function(d) .rp_read_dataset_lines(ctx, d))),
      ""),
    .rp_fig_subjects_lines(sj),
    "# the figure style of the company standards, and the figure checks",
    sprintf("source(%s)", encodeString(setup, quote = "\"")),
    "",
    "# ---- the plot (ggplot2), written by hand: leaves `plot`",
    "#      (theme_tfl(), scale_colour_tfl(), tfl_marker() ... give the standard's look)",
    plot,
    c(
      "",
      "# the figure checks: dropped rows, colours against the standard",
      "tfl_check(plot)"))
}

# whether a figure's design reads an ARD (its ard_stats / ard_number pieces)
.rp_fig_reads_ard <- function(design) {
  if (is.null(design)) return(FALSE)
  isTRUE(tfl_fig_uses_ard(design))
}

# The lines of a figure's program that put its ARD in `ard` (none without a
# source; a design that reads one without a source stops, saying where to
# choose it)
.rp_fig_ard_lines <- function(ctx, design = NULL) {
  r <- ctx$report
  src <- r$ard_source
  lay <- ctx$layout
  path <- .rp_path_lit(ctx$shared$study_ard)
  switch(src$kind,
    none = if (.rp_fig_reads_ard(design)) c(
      "# the design reads an ARD (`ard`), but the figure has none",
      sprintf("stop(\"%s reads an ARD: choose it in the figure's step %s (analysis).\")",
              r$id, src$step),
      ""),
    own = , table = {
      prog <- file.path(lay[["programs_ard"]], .ard_program_name(src$id))
      id <- encodeString(src$id, quote = "\"")
      c(if (src$kind == "own") "# this figure's ARD (its rows of the study ARD)"
        else sprintf("# the ARD of %s (its rows of the study ARD)", src$id),
        paste0("ard <- readRDS(", path, ") |>"),
        sprintf("  filter(output_id == %s)", id),
        sprintf("if (!nrow(ard)) stop(\"The study ARD has no rows for %s: run %s first.\")",
                src$id, prog),
        "# (the definition it was made from: the report records it, #293)",
        sprintf("ard_built <- ard_fingerprint(%s)", id),
        "")
    },
    import = {
      p <- .rp_path_lit(file.path(lay[["ard_import"]], src$file))
      id <- encodeString(r$id, quote = "\"")
      c(sprintf("# this figure's ARD, made elsewhere and taken in (%s)",
                file.path("input", "ard", src$file)),
        sprintf("ard <- tflspec::tfl_read_ard(%s)", p),
        sprintf("if (\"output_id\" %%in%% names(ard)) ard <- ard[ard$output_id == %s, , drop = FALSE]", id),
        "")
    })
}

# The report whose ARD a figure reads from the study ARD (its own id, or
# the table's), when its design or its own code reads one; NA otherwise
.rp_fig_ard_need <- function(ctx) {
  r <- ctx$report
  if (!identical(r$type, "figure")) return(NA_character_)
  src <- r$ard_source
  if (!src$kind %in% c("own", "table")) return(NA_character_)
  own_code <- .rp_is_code_step(r, "data") || .rp_is_code_step(r, "content")
  if (!own_code && !.rp_fig_reads_ard(r$design)) return(NA_character_)
  src$id
}

# the size said in words (the program's comment)
.rp_fig_size_words <- function(sz) {
  m <- sz$mode %||% c(width = "native", height = "native")
  w <- if (all(m == "native")) " (its own size)" else
    if (all(m == "fit")) " (the page's body)" else ""
  sprintf("%s x %s in%s", sz$width_in, sz$height_in, w)
}

# the lines that draw a figure at its size on the page, its design's dpi:
# `obj` becomes the figure the report places
.rp_fig_sized_lines <- function(ctx, obj = "plot") {
  sz <- .rp_value(ctx$report$size)
  dpi <- as.numeric((ctx$report$design$plot$dpi %||% 300)[1L])
  c("",
    sprintf("# the figure drawn at its size on the page: %s, %s dpi",
            .rp_fig_size_words(sz), dpi),
    sprintf("%s <- fig_sized(%s, width_in = %s, height_in = %s, dpi = %s)",
            obj, obj, sz$width_in, sz$height_in, dpi))
}

# A figure report with the content key `figure: {name, args}` (tflspec#835):
# the report's datasets and analysis data (its ARD definition), its ARD
# (`ard`), then `content <- tflspec::<fn>(...)`.  A key that does not hold
# is a stop() line saying why.
.rp_fig_key_lines <- function(ctx) {
  r <- ctx$report
  call <- tryCatch(as.character(tfl_content_code(.rp_code_content(.rp_y(ctx)), pipe = "|>")),
                   error = function(e) c(
                     "# the figure's key does not hold:",
                     paste0("stop(", .rp_r_string(trimws(conditionMessage(e))), ")")))
  ids <- r$adata$ids %||% character()
  reads <- r$adata$reads %||% character()
  c(if (length(reads)) c(
      paste0("# the data: ", paste(reads, collapse = ", "), " (data catalog)"),
      unlist(lapply(reads, function(d) .rp_read_dataset_lines(ctx, d))),
      ""),
    if (length(ids)) c(
      "# the report's analysis data (its ARD definition)",
      .rp_ard_code(ctx, ctx$shared$ard, output_id = r$id, part = "data", data = ids),
      ""),
    .rp_fig_ard_lines(ctx),
    call)
}

# What a design's read steps read (each figure's of a composed one): the
# datasets, and the report's analysis data (by data_id)
.rp_fig_design_reads <- function(design) {
  steps <- c(design$data, unlist(lapply(design$plots %||% list(), function(p) p$data),
                                 recursive = FALSE))
  reads <- Filter(function(s) is.list(s) && identical(s$step, "read"), steps)
  data <- unlist(lapply(reads, .fig_read_data))
  ds <- unlist(lapply(reads, function(s) if (is.null(.fig_read_data(s))) .fv(s, "dataset", "read")))
  list(datasets = unique(toupper(ds)), data = unique(as.character(data)))
}

# A designed figure's data and plot sections, as its program has them
# (#293): `# ---- data ----` -- the code lists, the datasets read, the
# steps' pipes -- and `# ---- plot ----` -- the palette, the chain into
# `plot` -- then the figure checks and its size on the page
.rp_fig_design_plot <- function(ctx, design, codelists) {
  r <- ctx$report
  if (!inherits(design, "tfl_fig_design")) design <- .fig_design_from_list(design)
  # a report with an analysis set: its subjects made in the data part, the
  # datasets the design reads joined to them (A2: no step keeps a
  # population); the analysis data its reads name made with them
  reads_of <- .rp_fig_design_reads(design)
  sj <- if (length(reads_of$datasets)) .rp_fig_subjects(ctx, data = reads_of$data)
  if (!is.null(sj)) attr(design, "subjects") <- sj$subjects
  code <- tfl_fig_design_code(design, r$id, setup = TRUE, save = FALSE, name = "plot",
                              codelists = codelists)
  reads <- attr(code, "reads") %||% character()
  lines <- .rp_split_lines(as.character(code))
  read <- if (length(reads)) c(
    paste0("# the data: ", paste(reads, collapse = ", "), " (data catalog)"),
    unlist(lapply(reads, function(d) .rp_read_dataset_lines(ctx, d))),
    "")
  # the report's analysis data its reads name: made first, as its ARD
  # definition says (the ARD code's data part)
  adata <- attr(code, "data") %||% character()
  if (!is.null(sj)) {
    read <- c(read, .rp_fig_subjects_lines(sj, adata))
  } else if (length(adata)) {
    read <- c(read, "# the report's analysis data (its ARD definition)",
              .rp_ard_code(ctx, ctx$shared$ard, output_id = r$id, part = "data", data = adata),
              "")
  }
  # after the section's line and the code lists (when there are)
  at <- 1L
  if (length(lines) > 1L && startsWith(lines[[2L]], "# ---- code lists")) {
    blank <- which(lines == "")
    at <- blank[blank > 2L][1L]
  }
  c(utils::head(lines, at), .rp_fig_ard_lines(ctx, design), read, utils::tail(lines, -at),
    "", "# the figure checks: dropped rows, colours against the standard",
    "tfl_check(plot)",
    .rp_fig_sized_lines(ctx, "plot"))
}

# A designed figure's lines: the study's figure setup sourced (the program
# sources it at its top instead), then the design's code
.rp_fig_design_lines <- function(ctx, design, source = TRUE) {
  c(if (source) c(.rp_fig_source_lines(ctx$layout), ""),
    .rp_fig_design_plot(ctx, design, codelists = .rp_value(ctx$report$fig_codelists)))
}

# A designed figure's code in its two steps: `data` and `plot` -- for a
# report whose other step is its own code
.rp_fig_design_parts <- function(ctx, design) {
  lines <- .rp_fig_design_plot(ctx, design, codelists = .rp_value(ctx$shared$codelists))
  at <- match(TRUE, startsWith(lines, "# ---- plot"), nomatch = length(lines) + 1L)
  list(data = utils::head(lines, at - 1L),
       plot = utils::tail(lines, length(lines) - at + 1L))
}

# The datasets the report registers, read; a figure's subjects (its
# analysis set's, with its analysis data: the code reads them by name, A2
# Q5); and a figure's ARD as `ard` when its source gives one
.rp_code_inputs <- function(ctx) {
  r <- ctx$report
  ds <- r$datasets
  fig <- identical(r$type, "figure")
  adata <- if (fig) as.character(r$adata$ids %||% character())
  sj <- if (fig) .rp_fig_subjects(ctx, data = adata)
  ard <- if (fig && !identical(r$ard_source$kind, "none")) {
    a <- .rp_fig_ard_lines(ctx)
    if (length(a)) a
  }
  c(if (length(ds)) c(
      paste0("# the report's data: ", paste(ds, collapse = ", "), " (data catalog)"),
      unlist(lapply(ds, function(d) .rp_read_dataset_lines(ctx, d))), ""),
    .rp_fig_subjects_lines(sj, adata),
    ard)
}

# A step of the report's own code, as the program runs it: its code, then
# the check that it left what the next step reads
.rp_code_step_lines <- function(ctx, step) {
  r <- ctx$report
  leaves <- .rp_step_leaves(r$type, step)
  # (a figure's content: drawn at its size on the page first)
  sized <- if (identical(step, "content") && identical(r$type, "figure")) {
    utils::tail(.rp_fig_sized_lines(ctx, "content"), -1L)
  }
  c(paste0("# your code: leaves `", leaves, "`"),
    .rp_code_block(r$steps[[step]]),
    "",
    sized,
    if (identical(step, "content")) "content <- report_content(content)" else
      sprintf("%s <- report_data(%s, \"%s\")", leaves, leaves, leaves))
}

# The part of a program between its setup and its report when a step is
# the report's own code: its data (the datasets it registers and its ARD
# read, its code; or as the definition makes it), then its content (its
# code; or as the definition makes it -- a table's plan is the report
# part's)
.rp_code_report_lines <- function(ctx, proc = NA) {
  r <- ctx$report
  type <- r$type
  dc <- .rp_is_code_step(r, "data")
  cc <- .rp_is_code_step(r, "content")
  design <- if (identical(type, "figure")) r$design
  todo <- function(what) paste0("stop(\"tflplanner: the ", what, " of ", r$program,
                                " is still to be written.\")")
  data <- if (dc) {
    code <- r$steps$data
    c(.rp_section("data: your code"),
      .rp_code_inputs(ctx),
      if (identical(type, "table")) {
        # (leaving `ard`, it is normalized in the table's plan)
        leaves <- if (.rp_table_norm_builtin(ctx, code, proc)) "ard" else "data"
        c(if (identical(leaves, "ard")) "# your code: leaves `ard` (normalized in the table's plan) or `data`"
          else "# your code: leaves `data` (or `ard`, normalized below)",
          .rp_code_block(code),
          .rp_table_process_lines(ctx, code, proc),
          "", sprintf("%s <- report_data(%s, \"%s\")", leaves, leaves, leaves))
      }
      else .rp_code_step_lines(ctx, "data"))
  } else switch(type,
    table = {
      a <- .rp_ard_code_of(ctx)
      recs <- .rp_table_dataset_lines(ctx)
      ard_lines <- c(.rp_section(if (.rp_table_norm_builtin(ctx, a, proc)) "data: this report's ARD"
                                 else "data: this report's normalized ARD"),
                     if (is.na(a)) todo("ARD") else c(.rp_code_block(a), .rp_table_process_lines(ctx, a, proc)))
      # a table of records: its records as `data`; its ARD too when it has
      # analyses (a part's rows: plan_append())
      if (is.null(recs)) ard_lines
      else c(.rp_section("data: this report's records"), recs, if (!is.na(a)) c("", ard_lines))
    },
    listing = c(.rp_section("data: the listing's data"),
                .rp_listing_lines(ctx, rework = proc, part = "data") %||%
                  .rp_code_inputs(ctx)),
    figure = c(.rp_section("data: the figure's data"),
               if (!is.null(design)) .rp_fig_design_parts(ctx, design)$data else
                 .rp_code_inputs(ctx)))
  content <- if (cc) {
    c("", .rp_section("content: your code"),
      .rp_code_step_lines(ctx, "content"))
  } else switch(type,
    table = NULL,
    listing = c("", .rp_section("content: the listing"),
                .rp_listing_lines(ctx, part = "content") %||% todo("listing's columns")),
    figure = c("", .rp_section("content: the figure"),
               if (!is.null(design)) .rp_fig_design_parts(ctx, design)$plot else
                 todo("figure")))
  # (one blank line between the two)
  while (length(data) && identical(data[[length(data)]], "")) data <- data[-length(data)]
  # the figure style, report_content() and report_data() first (a designed
  # figure's program sources them at its top)
  c(if (is.null(design)) c(.rp_fig_source_lines(ctx$layout), ""), data, content)
}

# The data part of a report's program: the setup every report runs, then
# its data and its content as its definition makes them or as its own code
# does; with nothing to make it, a TODO that stops (`todo = FALSE`: NULL)
.rp_data_lines <- function(ctx, todo = TRUE) {
  r <- ctx$report
  lay <- ctx$layout
  code <- .rp_ard_code_of(ctx)
  proc <- r$process
  type <- r$type
  setup0 <- ctx$shared$setup
  setup <- if (!is.na(setup0)) c(.rp_code_block(setup0), "")
  # a step of the report's own code: the data and the content, each as its
  # definition or its code makes it
  if (.rp_is_code_step(r, "data") || .rp_is_code_step(r, "content")) {
    return(c(setup, .rp_code_report_lines(ctx, proc)))
  }
  # a template listing: its records as `data`, as a table of records has them
  if (isTRUE(r$template_listing)) {
    # (a definition that does not hold: the report part says so and stops)
    recs <- tryCatch(.rp_table_dataset_lines(ctx), error = function(e) "failed")
    if (identical(recs, "failed")) {
      return(c(setup, "# the listing's definition does not hold: see the report part"))
    }
    if (!is.null(recs)) {
      return(c(setup, "# the listing's records", recs,
               if (!is.na(proc)) c("", "# its rework", .rp_code_block(proc))))
    }
  }
  # a listing defined in rows: its process code is a rework of `data`
  if (identical(type, "listing")) {
    l <- .rp_listing_lines(ctx, rework = proc)
    if (!is.null(l)) return(c(setup, l))
  }
  # a figure: its design's code, or (none yet) its datasets and a plot to
  # write
  if (identical(type, "figure")) {
    design <- r$design
    f <- if (is.null(design)) {
      .rp_figure_lines(ctx)
    } else if (!is.null(design$figure)) {
      c(.rp_fig_source_lines(lay), "", .rp_fig_key_lines(ctx))
    } else {
      .rp_fig_design_lines(ctx, design)
    }
    if (!is.null(f)) return(c(setup, f))
  }
  # a table of records: its records as `data`, its ARD too when it has
  # analyses (a part's rows: plan_append())
  if (identical(type, "table")) {
    recs <- .rp_table_dataset_lines(ctx)
    if (!is.null(recs)) {
      return(c(setup, "# the report's records", recs,
               if (!is.na(code)) c("", "# its ARD (the parts' rows)", .rp_code_block(code))))
    }
  }
  if (is.na(code)) {
    if (!todo) return(NULL)
    stop_todo <- paste0("stop(\"tflplanner: the data part of ",
                        r$program, " is still to be written.\")")
    body <- switch(type,
      figure = c(
        "# TODO: make the figure(s), e.g.",
        "#   content <- list(ggplot2::ggplot(adsl, ggplot2::aes(AGE)) +",
        "#                     ggplot2::geom_histogram())",
        stop_todo),
      listing = c(
        "# TODO: make the listing pages, e.g.",
        paste0("#   adae    <- readRDS(", .rp_path_lit(file.path(lay[["adam"]], "adae.rds")), ")"),
        "#   content <- as_rtftables(adae[, c(\"USUBJID\", \"AEDECOD\")])",
        stop_todo),
      c("# TODO: build the ARD, e.g.",
        paste0("#   adsl <- readRDS(", .rp_path_lit(file.path(lay[["adam"]], "adsl.rds")), ")"),
        "#   ard  <- cards::ard_stack(adsl, .by = TRT01A, ...)",
        stop_todo))
    return(c(setup, body))
  }
  table <- identical(type, "table")
  proc_lines <- if (table) .rp_table_process_lines(ctx, code, proc)
  c(setup,
    .rp_code_block(code),
    proc_lines)
}

# The report part of a report program, written out from its definition: the
# table's plan, the document around it and where it goes.  A definition
# that does not hold yet gives a stop() line saying why, so the program is
# still written and says what to fix when run.
.rp_report_code_lines <- function(ctx, table, content = if (table) "plan" else "content") {
  r <- ctx$report
  id <- r$id
  lay <- ctx$layout
  # the report as its file says it, and the study's spec; none: the sheets
  y <- .rp_y(ctx)
  sheets <- function() .rp_value(ctx$shared$sheets)
  sp <- if (is.null(y)) sheets()
  tpl <- table && isTRUE(r$template_listing)
  tryCatch(c(
    if (table) c(
      if (!.rp_table_norm_builtin(ctx)) {
        # (a Rework that leaves no `data` is a definition that does not
        # hold; the check's value is a line of the program, as it was)
        if (!is.na(r$process)) .rp_rework_need_data(r$process)
      },
      paste0("# the ", if (tpl) "listing" else "table", ", as ",
             file.path(lay[["spec"]], "outputs", paste0(id, ".yml")), " defines it"),
      # the built-in normalization in its pipe; otherwise the data part
      # made `data`
      if (tpl || .rp_table_norm_builtin(ctx)) {
        if (!is.null(y)) {
          tfl_content_code(.rp_code_content(y), pipe = "|>")
        } else {
          tryCatch(tfl_table_code(sp, id, pipe = "|>", ard_spec = .rp_value(ctx$shared$ard_spec)),
                   error = function(e) tfl_table_code(sp, id, pipe = "|>"))
        }
      } else {
        # (a table normalized in its data part keeps the sheets' plan for
        # now)
        tfl_table_code(sp %||% sheets(), id, data = "data", pipe = "|>",
                       normalize = FALSE)
      },
      ""),
    paste0("# the report, as ", file.path(lay[["spec"]], "outputs", paste0(id, ".yml")),
           " defines it"),
    if (!is.null(y)) {
      c(.rp_report_id_code(tfl_report_code(y$output, content = content,
                                           setup = .rp_value(ctx$shared$report_setup),
                                           study = y$study), id),
        sprintf("generate_rtfreport(doc, %s, overwrite = TRUE)",
                .rp_report_path_code(tfl_report_path(y$output, study = y$study), id)))
    } else {
      c(.rp_report_id_code(tfl_report_code(sp, id, content = content,
                                           setup = .rp_value(ctx$shared$report_setup)), id),
        sprintf("generate_rtfreport(doc, %s, overwrite = TRUE)",
                .rp_report_path_code(tfl_report_path(sp, id), id)))
    },
    # a figure from an ARD: the definition of that ARD it was made from
    if (!is.na(.rp_fig_ard_need(ctx))) "record_report(report_id, ard = ard_built)"
    else "record_report(report_id)"),
    error = function(e) sprintf(
      "stop(%s)", .rp_r_string(paste0("tflplanner: the definition of ",
                                      id, " does not hold: ",
                                      conditionMessage(e)))))
}

# The whole program of one report
.rp_program <- function(ctx, date, generator) {
  r <- ctx$report
  lay <- ctx$layout
  id <- r$id
  desc <- r$description
  if (!is.null(desc) && (length(desc) != 1L || is.na(desc))) desc <- NULL
  titles <- r$titles %||% character()
  type <- r$type
  table <- identical(type, "table")
  # a template listing is planned like a table of records: `data`, then the
  # template's plan
  tpl_listing <- isTRUE(r$template_listing)
  # a table normalized in its plan's pipe keeps its ARD (`ard`), one
  # normalized in its data part `data`
  obj <- if (tpl_listing) "data" else if (!table) "content" else if (.rp_table_norm_builtin(ctx)) "ard" else "data"
  # a designed figure: its code is the design's (#293)
  design <- if (identical(type, "figure")) r$design
  # a step of the report's own code: its data and content parts are
  # .rp_code_report_lines()'s
  own <- .rp_is_code_step(r, "data") || .rp_is_code_step(r, "content")
  # a figure report with the content key `figure: {name, args}`
  # (tflspec#835): its program is tflspec's call
  fig_key <- !is.null(design$figure) && !own

  head <- .program_banner(
    paste("Program    :", file.path(lay[["programs_tfl"]], r$program)),
    paste0("Output     : ", id, " (", type, ") -> ", r$file),
    # wrapped as the ARD banner's Analyses: are
    if (!is.null(desc)) strwrap(desc, width = 74, initial = "Description: ",
                                prefix = strrep(" ", 13)),
    if (length(titles)) paste("Title      :", titles),
    .setup_generated(generator, date),
    if (own) paste0(
      "The data and content parts are your code where its steps are; the report part is written from ",
      lay[["spec"]], "/.") else if (fig_key) c(
      paste0("The data and content parts make `content` (tflspec's figure function, called as the report's ",
             "`figure:` says); the report part is written from ", lay[["spec"]], "/."),
      paste0("Edit its `figure:` key in ", lay[["spec"]], "/outputs/", id, ".yml, not this code.")
    ) else if (is.null(design)) paste0(
      "The data part makes `", obj, "`; the report part is written from ",
      lay[["spec"]], "/.") else c(
      paste0("The data and plot parts make `plot`; the report part is written from ",
             lay[["spec"]], "/."),
      paste0("Edit the design in the Plot Designer (", lay[["spec"]], "/outputs/",
             id, ".yml), not this code.")))

  setup0 <- ctx$shared$setup
  data <- if (is.null(design) || own) .rp_data_lines(ctx) else c(
    if (!is.na(setup0)) c(.rp_code_block(setup0), ""),
    if (fig_key) .rp_fig_key_lines(ctx) else
      .rp_fig_design_lines(ctx, design, source = FALSE))
  # (the deliverable is the report: a table's data is not saved -- the
  # study ARD holds it by output_id)
  data_part <- if (!is.null(design) || own) data else c(
    .rp_section(paste0("data: ", switch(type,
      figure = "the figure",
      listing = if (tpl_listing) "the listing's records" else "the listing's pages",
      if (!is.null(.rp_table_dataset_lines(ctx))) "this report's records and ARD"
      else if (identical(obj, "ard")) "this report's ARD" else "this report's normalized ARD"))),
    data)

  # a figure's plot goes to the report as it is, unless its code makes
  # `content` itself
  content <- if (.rp_is_code_step(r, "content")) "content" else
    if (table || tpl_listing) "plan" else if (identical(type, "figure") &&
      !any(grepl("^[[:space:]]*content[[:space:]]*(<-|=)[^=]", data))) "plot" else "content"
  report_part <- c(
    .rp_section("report"),
    .rp_report_code_lines(ctx, (table || tpl_listing) && identical(content, "plan"), content))

  c(head,
    "",
    .rp_study_guard,
    "# the study's setup, and the header, footer and tokens of every report",
    sprintf("source(%s)", .rp_r_string(file.path(lay[["programs_tfl"]],
                                                  .setup_file_names[["report_setup"]]))),
    if (!is.null(design)) .rp_fig_source_lines(lay),
    "",
    .rp_report_id_line(id),
    "",
    data_part,
    "",
    # (no blank line at the end: a file ends on its last line)
    report_part,
    .rp_run_end)
}

# ---- the code context ---------------------------------------------------------

# the options a report or ARD program's makers read, from the arguments
.rp_context <- function(attached, study_code, series_dir, layout) {
  c(if (isTRUE(study_code)) list(paths = .setup_path_vars(layout),
                                 attached = as.character(attached), study_setup = TRUE),
    if (length(series_dir)) list(series_dir = as.character(series_dir)))
}

# the arguments every maker here checks alike
.rp_check_common <- function(fn, standards, layout, keys) {
  if (!is.list(standards) || (length(standards) &&
      (is.null(names(standards)) || !all(names(standards) %in% .rp_standard_keys)))) {
    stop(fn, ": `standards` is a list with any of ",
         paste(.rp_standard_keys, collapse = ", "), ".", call. = FALSE)
  }
  .layout_check(layout, keys, fn)
}

# ---- the exported makers ------------------------------------------------------

#' A study's report program
#'
#' The text of one report's program (`programs/tfl/<program>.R`), from
#' values alone: the report's values, the values its study shares between
#' its reports, the company's inputs, the folder layout and the date.
#' Nothing is read from a file, an option or a home, and nothing is
#' written.  A program sources `report_setup.R` ([tfl_setup_program()]),
#' makes its data (the study's ARD rows, its records, its listing's data,
#' its figure's design or the report's own code), then lays the report out
#' as its definition says ([tfl_content_code()], [tfl_table_code()],
#' [tfl_report_code()]) and writes the RTF.
#'
#' `report` is a list of the report's values:
#' * `id`, `type` (`"table"`, `"listing"` or `"figure"`), `program` (the
#'   program's file name), `file` (the RTF, as the banner names it);
#' * `description`, `titles` (the banner's);
#' * `steps`: `list(data = , content = )`, a step's own code or `NA`;
#'   `datasets`: the datasets a step of its own code reads; `process`: a
#'   table's Rework (or a listing's), or `NA`;
#' * `ard_import`: the file of an ARD taken in, or `NULL`; `ard_object`:
#'   the name its program gives its ARD (`"ard"`);
#' * `output`: the report as its SPEC file says it, its page's text and
#'   tokens as the program writes them, or `NULL` (no SPEC: the study's
#'   sheets); `spec_output`: the report in the study's spec as it is;
#'   `template_listing`: a listing made from a template;
#' * `listing`: `NULL`, or `list(own = )` when the listing sheets define it
#'   (`own`: it starts from its analysis data or analysis set);
#' * a figure's: `design` ([tfl_fig_design()], or `NULL`), `ard_source`
#'   (`list(kind = , id = , file = , step = )`: none, own, table or
#'   import), `adata` (`list(ids = , reads = )`: its analysis data and the
#'   datasets they read), `fig_codelists` and `size` (its size on the page:
#'   `width_in`, `height_in`, `mode`).
#'
#' `shared` is a list of the study's values shared by its reports: `setup`
#' (the code every report runs first, or `NA`), `report_setup` (whether
#' `report_setup.R` says the study's tokens, header and footer), `ard` (the
#' ARD definition's tables, a `tfl_ard_spec`), `ard_spec` (the checked
#' [tfl_ard_spec()]), `study_ard` (the study ARD's path), `codelists`,
#' `listings` (the [tfl_listing_spec()] of the listing sheets), `sheets`
#' and `table_sheets` (the study's spec of its table and report sheets, and
#' of its table sheets).  A value may be given as a function of no
#' arguments: it is called only when the program needs it.
#'
#' @param report The report's values (above).
#' @param study The study's spec ([tfl_read_study_spec()]) the report's
#'   `output` is read with, or `NULL`.
#' @param shared The study's values shared by its reports (above).
#' @param standards The company's inputs: a list with any of
#'   `code_templates` (`name`, `code`: `table_data`, `table_process`,
#'   `figure_plot`), `ard_statistics`, `ard_methods` and `settings` (`key`,
#'   `value`: `listing_type`).  One left out is tflspec's own
#'   ([tfl_builtin_standards()], [tfl_ard_statistics()],
#'   [tfl_ard_methods()], `"multiline"`).
#' @param attached,study_code The study's code context, as
#'   [tfl_setup_program()] takes it.
#' @param series_dir The folders the template series are looked for in
#'   (before tflspec's own).
#' @param layout The study's folders ([tfl_study_layout()]).
#' @param date The date the banner says.
#' @param generator What made the program, as its banner's `Generated` line
#'   says it.
#' @param part `"program"`: the whole program; `"data"`: its data part only
#'   (the setup code, then what makes the data and the content), as a
#'   preview runs it.
#' @param todo `part = "data"`: `FALSE` gives `NULL` instead of a TODO that
#'   stops when nothing makes the data.
#' @return The program, one element per line (an element may hold more
#'   than one line).
#' @seealso [tfl_ard_program()], [tfl_setup_program()],
#'   [tfl_study_program_files()]
#' @examples
#' # a table whose data and content parts are its own code; it has no
#' # definition yet, so its report part is a stop() line saying so
#' r <- list(id = "T-1", type = "table", program = "T-1.R", file = "T-1.rtf",
#'           steps = list(data = "adsl <- readRDS(\"data/adam/adsl.rds\")",
#'                        content = "plan <- tflspec::table_plan(adsl)"),
#'           process = NA, ard_object = "ard")
#' sh <- list(setup = NA, study_ard = "output/ard/ard.rds")
#' code <- tfl_report_program(r, shared = sh)
#' code[19:31]
#' @keywords internal
#' @export
tfl_report_program <- function(report, study = NULL, shared = list(), standards = list(),
                               attached = character(), study_code = TRUE,
                               series_dir = NULL, layout = tfl_study_layout(),
                               date = Sys.Date(),
                               generator = paste("tflspec", utils::packageVersion("tflspec")),
                               part = c("program", "data"), todo = TRUE) {
  fn <- "tfl_report_program()"
  # (the values as the caller has them: worked out before the context is set)
  force(report)
  force(study)
  force(shared)
  force(standards)
  force(attached)
  force(series_dir)
  force(date)
  force(generator)
  part <- match.arg(part)
  if (!is.list(report) || !is.character(report$id) || length(report$id) != 1L ||
      !isTRUE(report$type %in% c("table", "listing", "figure")) ||
      !is.character(report$program) || length(report$program) != 1L) {
    stop(fn, ": `report` is a list with `id`, `type` (table, listing or figure) and `program`.",
         call. = FALSE)
  }
  if (!is.list(shared)) stop(fn, ": `shared` is a list of the study's values.", call. = FALSE)
  layout <- .rp_check_common(fn, standards, layout,
                             c("programs_ard", "programs_tfl", "spec", "adam", "ard_import"))
  report$steps <- report$steps %||% list(data = NA_character_, content = NA_character_)
  report$process <- report$process %||% NA_character_
  report$ard_object <- report$ard_object %||% "ard"
  report$ard_source <- report$ard_source %||% list(kind = "none")
  shared$setup <- shared$setup %||% NA_character_
  shared$report_setup <- shared$report_setup %||% FALSE
  shared$study_ard <- shared$study_ard %||% "output/ard/ard.rds"
  ctx <- list(report = report, study = study, shared = shared, standards = standards,
              layout = layout)
  .with_code_context(switch(part,
    program = .rp_program(ctx, date, generator),
    data = .rp_data_lines(ctx, todo = todo)),
    .rp_context(attached, study_code, series_dir, layout))
}

#' A study's ARD program
#'
#' The text of one output's ARD program (`programs/ard/<output_id>.R`), or
#' with no `output_id` of `programs/ard/ard_setup.R`, which every ARD
#' program sources, from values alone: the study's ARD definition, the
#' company's ARD catalogs, the layout and the date.  Nothing is written.
#' An output's program runs its analyses ([tfl_ard_code()]) and puts its
#' rows into the study ARD with its definition's fingerprint; the setup
#' sources the study's setup and holds the statistics and formats.
#'
#' @param spec The study's ARD definition ([tfl_ard_spec()]).
#' @param output_id The output; `NULL`: `ard_setup.R`.
#' @param standards The company's inputs: a list with any of
#'   `ard_statistics` and `ard_methods` (the catalogs, see
#'   [tfl_ard_statistics()], [tfl_ard_methods()]); one left out is
#'   tflspec's own.
#' @param dir The study folder: the fingerprint recorded with the ARD reads
#'   the study's own analysis functions from it.
#' @param codelists The reports' code lists, as [tfl_ard_code()] takes
#'   them.
#' @inheritParams tfl_report_program
#' @return The program, one element per line.
#' @seealso [tfl_report_program()]
#' @examples
#' sp <- tfl_ard_spec(list(
#'   datasets = data.frame(dataset = "ADSL", path = "data/adam/adsl.rds"),
#'   populations = data.frame(population_id = "SAF", dataset = "ADSL",
#'                            where = "SAFFL == \"Y\""),
#'   analyses = data.frame(output_id = "T1", analysis_id = "AGE",
#'                         method = "continuous", population_id = "SAF",
#'                         by = "TRT01A", variables = "AGE")))
#' head(tfl_ard_program(sp, "T1"), 8)
#' @keywords internal
#' @export
tfl_ard_program <- function(spec, output_id = NULL, standards = list(), dir = ".",
                            codelists = NULL, attached = character(), study_code = TRUE,
                            layout = tfl_study_layout(), date = Sys.Date(),
                            generator = paste("tflspec", utils::packageVersion("tflspec"))) {
  fn <- "tfl_ard_program()"
  force(spec)
  force(standards)
  force(codelists)
  force(attached)
  force(date)
  force(generator)
  if (!inherits(spec, "tfl_ard_spec")) {
    stop(fn, ": `spec` is the study's ARD definition (tfl_ard_spec()).", call. = FALSE)
  }
  if (!is.null(output_id) && (!is.character(output_id) || length(output_id) != 1L ||
                              is.na(output_id))) {
    stop(fn, ": `output_id` is one output's id, or NULL for ard_setup.R.", call. = FALSE)
  }
  layout <- .rp_check_common(fn, standards, layout, c("programs_ard", "programs_tfl"))
  ctx <- list(standards = standards, layout = layout)
  .with_code_context(
    if (is.null(output_id)) .rp_ard_setup(ctx, spec, date, generator) else
      .rp_ard_one(ctx, spec, output_id, date, generator, dir, codelists),
    .rp_context(attached, study_code, NULL, layout))
}

.rp_ard_setup_file <- "ard_setup.R"

# a value of the ARD definition's study table, else `default`
.rp_ard_study_value <- function(x, key, default) {
  v <- x$study$value[match(key, x$study$key)]
  if (length(v) && !is.na(v)) v else default
}

.rp_ard_setup <- function(ctx, spec, date, generator) {
  lay <- ctx$layout
  files <- .setup_paths(lay)[c("company_setup", "study_setup", "study_user")]
  names(files) <- c("company", "setup", "user")
  c(.program_banner(
      paste("Program    :", file.path(lay[["programs_ard"]], .rp_ard_setup_file)),
      "What every ARD program of the study starts with (it sources this).",
      .setup_generated(generator, date)),
    "",
    "# the study's setup: the company's, the study's folders and id, your own",
    .setup_source_line(lay),
    "",
    # (the ARD code's setup: library() of what the study's setup does not
    # attach, a blank line, the company's statistics and formats)
    .rp_drop_leading_blank(.rp_ard_code(ctx, spec, part = "setup")),
    "# the files each ARD is built with, recorded with it (tflspec's",
    "# save_ard(): tflplanner compares the study setup's)",
    "options(tflspec.ard_sources = c(",
    paste0("  ", names(files), " = ",
           encodeString(unname(files), quote = "\""),
           c(rep(",", length(files) - 1L), "))")),
    "")
}

.rp_ard_one <- function(ctx, spec, output_id, date, generator, dir, codelists) {
  lay <- ctx$layout
  a <- spec$analyses[spec$analyses$output_id %in% output_id, , drop = FALSE]
  if (!nrow(a)) stop("No analyses for ", output_id, call. = FALSE)
  labels <- ifelse(is.na(a$label), a$analysis_id,
                   paste0(a$analysis_id, " (", a$label, ")"))
  c(.program_banner(
      paste("Program    :", file.path(lay[["programs_ard"]], .ard_program_name(output_id))),
      paste0("Output     : ", output_id, " -> its rows of ",
             .rp_ard_study_value(spec, "output", "output/ard/ard.rds")),
      strwrap(paste(labels, collapse = ", "), width = 74,
              initial = "Analyses   : ", prefix = strrep(" ", 13)),
      .setup_generated(generator, date)),
    "",
    .rp_study_guard,
    sprintf("source(%s)", encodeString(file.path(lay[["programs_ard"]], .rp_ard_setup_file),
                                       quote = "\"")),
    "",
    # its rows into the study ARD with its definition's fingerprint
    .rp_ard_code(ctx, spec, output_id = output_id, part = "body", dir = dir,
                 codelists = codelists),
    .rp_run_end)
}
