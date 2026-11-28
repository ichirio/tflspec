# A report's own R content (`content: !r`, canon 11-28: "content: a plot
# or a table_plan made and handed on").  Two rules, the same in a session
# (tfl_content_run(): the preview, tfl_run_output(), tfl_check_build()) and
# in the report's program (tfl_report_program()):
#
#   * one way its code sees the report's data: the report's own data rows
#     (its analysis data, by id: ard.analysis_data), the study's ADaM the
#     code names (data.yml's datasets), and `ard` when the report has
#     analyses -- never tflspec's sample data on the search path
#     (.own_content_data(), .own_reads_given(); the program:
#     .rp_own_content_inputs());
#   * one contract for what it gives: a table's code a table_plan; a
#     figure's a ggplot (also, as before, rtfplot figures, a data frame or
#     rtftable pages, or a list of them); anything else stops with a
#     condition of class `tfl_content_contract` naming the report and what
#     it gave (report_content_check(), written once in fig_setup.R; the
#     session runs that same text).

# ---- the contract ---------------------------------------------------------------

# The kinds whose own content has a contract (a listing's own code gives
# its rtftable pages, as before)
.own_contract_kinds <- c("table", "figure")

# The program's check of a report's own content (report_content_check() in
# fig_setup.R, .setup_content_contract), run here as the program runs it:
# the same text, so the same rule and the same message
.own_contract_env <- new.env(parent = emptyenv())
.own_content_contract <- function(x, kind, id) {
  if (!kind %in% .own_contract_kinds) return(invisible(x))
  if (is.null(.own_contract_env$check)) {
    env <- new.env(parent = baseenv())
    eval(parse(text = .setup_content_contract), env)
    .own_contract_env$check <- get("report_content_check", envir = env)
  }
  .own_contract_env$check(x, kind, id)
  invisible(x)
}

# ---- the data its code sees ---------------------------------------------------------

# The code of a report's own content: its data steps and the content
.own_content_text <- function(output) {
  c(.content_r_steps(output), as.character(unclass(output$content)))
}

# The names code reads (every symbol it names) and the names it assigns
# (the left side of `<-`, `=`, `<<-`, `->`); NULL when it does not parse
# (R says so when it runs)
.own_code_names <- function(code) {
  ex <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  if (is.null(ex)) return(NULL)
  assigned <- character()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    f <- e[[1L]]
    if (is.symbol(f) && as.character(f) %in% c("<-", "=", "<<-") && length(e) == 3L &&
        is.symbol(e[[2L]])) {
      assigned <<- c(assigned, as.character(e[[2L]]))
    }
    for (a in as.list(e)[-1L]) if (!missing(a)) walk(a)
  }
  for (e in ex) walk(e)
  list(reads = unique(all.names(ex)), assigned = unique(assigned))
}

# The sample datasets tflspec ships (adsl, adae ...): on the search path
# when tflspec is attached, never a report's data
.own_bundled_names <- function() {
  tryCatch(as.character(utils::data(package = "tflspec")$results[, "Item"]),
           error = function(e) character())
}

# A dataset a report's own code names that the data given does not hold,
# and the code does not make itself, would be found on the search path --
# tflspec's sample data, silently: a clear stop instead (class
# `tfl_content_data`), for a table and a figure alike
.own_reads_given <- function(output, given) {
  nm <- .own_code_names(.own_content_text(output))
  if (is.null(nm)) return(invisible(TRUE))
  miss <- setdiff(intersect(nm$reads, .own_bundled_names()), c(given, nm$assigned))
  if (length(miss)) {
    msg <- sprintf(paste0("%s: the report's own code reads %s, which is not among the report's ",
                          "data (%s): list it in spec/data.yml (its ADaM) or in the report's ",
                          "ard.analysis_data; tflspec's sample data is never a report's."),
                   output$id %||% "the report", paste(toupper(miss), collapse = ", "),
                   if (length(given)) paste(given, collapse = ", ") else "none")
    stop(structure(class = c("tfl_content_data", "error", "condition"),
                   list(message = msg, call = NULL)))
  }
  invisible(TRUE)
}

# The report's analysis data ids (ard.analysis_data), in its order
.own_adata_ids <- function(output) {
  ids <- vapply(output$ard$analysis_data %||% list(), function(a) {
    if (is.list(a)) as.character(a$id %||% NA_character_)[1L] else NA_character_
  }, "")
  unique(ids[!is.na(ids) & nzchar(ids)])
}

# Whether the report has analyses of its own (an ARD to read)
.own_has_analyses <- function(output) length(output$ard$analyses %||% list()) > 0L

# The objects a report's own content reads, from the study's files in the
# study folder `dir`, by name: a figure's datasets its read steps name
# (.contents_fig_data()), the report's analysis data (its data part of the
# ARD program, tfl_ard_code(part = "data"): the datasets it reads and the
# data rows by id), each dataset of data.yml its code names (adsl, adae),
# and `ard` -- the report's rows of the study ARD -- when it has analyses
.own_content_data <- function(output, study, dir = ".") {
  data <- if (identical(output$kind, "figure")) .contents_fig_data(output, study, dir) else list()
  ids <- setdiff(.own_adata_ids(output), names(data))
  if (length(ids)) {
    old <- options(tflspec.plain_ns = TRUE, tflspec.study_setup = FALSE)
    on.exit(options(old), add = TRUE)
    code <- tfl_ard_code(study, output$id, part = "data", data = ids)
    henv <- new.env(parent = globalenv())
    eval(parse(text = tfl_helpers_code()), henv)
    env <- new.env(parent = henv)
    wd <- setwd(dir)
    on.exit(setwd(wd), add = TRUE)
    .eval_study_code(code, env, study, ".", output_id = output$id)
    for (n in setdiff(ls(env), names(data))) data[[n]] <- get(n, envir = env)
  }
  nm <- .own_code_names(.own_content_text(output))
  ds <- study$data$datasets %||% list()
  have <- tolower(vapply(ds, function(z) as.character(z$dataset %||% NA_character_)[1L], ""))
  for (r in setdiff(intersect(have, nm$reads), c(names(data), nm$assigned))) {
    z <- ds[[match(r, have)]]
    p <- z$path
    .data_file_must_exist(toupper(r), p, dir)
    if (!grepl("^([A-Za-z]:)?[/\\\\]", p)) p <- file.path(dir, p)
    data[[r]] <- .ai_read_adam(p, toupper(r), study)
  }
  if (.own_has_analyses(output) && is.null(data$ard)) {
    a <- suppressMessages(tfl_build_ard(study, dir = dir, output_id = output$id, save = FALSE))
    if (!is.null(a$output_id)) a <- a[a$output_id %in% output$id, , drop = FALSE]
    data$ard <- a
  }
  data
}

# ---- the run ---------------------------------------------------------------------------

# the content as the report's own code, run as the program runs it: the
# objects it reads bound by their names (`data` a named list: the datasets,
# the analysis data, `ard`; a data frame: the output's object, `ard` by
# default), the data's steps, then the code; what it leaves in `content`
# is the content, held to the contract
.content_own_run <- function(output, data) {
  env <- new.env(parent = globalenv())
  fig <- identical(output$kind, "figure")
  if (fig) .fig_reads_given(output, data)
  named <- is.list(data) && !is.data.frame(data) && !is.null(names(data))
  if (named) {
    # the code reads the objects by their names (adsl, adae, adsl_f, ard)
    for (n in names(data)) assign(n, data[[n]], envir = env)
  } else {
    assign(output$data$object %||% "ard", data, envir = env)
  }
  steps <- .content_r_steps(output)
  code <- c(steps, as.character(output$content))
  if (fig && named) {
    # after the study's figure setup, as its program does
    eval(parse(text = tfl_fig_setup_code()), env)
  }
  # (the study's helpers: the ARD's readers; any the code calls, tte_time()
  # of a time to event, fmt_num() of a number)
  if ((fig && named && !is.null(data$ard)) || .calls_helper(code)) {
    eval(parse(text = tfl_helpers_code()), env)
  }
  .own_reads_given(output, ls(env))
  if (length(steps)) eval(parse(text = steps), env)
  eval(parse(text = as.character(output$content)), env)
  if (!exists("content", envir = env, inherits = FALSE)) {
    .ard_stop("tfl_content_run(): the report's own code leaves no `content`.")
  }
  content <- get("content", envir = env, inherits = FALSE)
  .own_content_contract(content, output$kind, output$id %||% "the report")
  content
}
