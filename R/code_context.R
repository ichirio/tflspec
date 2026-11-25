# One context for making a program's text (tflspec#910, G5-1a).
#
# A program's text is a function of values, never of a session.  The code
# makers read a few options as they write (the study's folders and the
# packages its setup attaches, whether its setup sets the rounding, where
# the template series are, the figure style, the pipe ...).  The study's
# program generator therefore sets every one of them from its own
# arguments, and unsets the rest, while it makes the text, and puts them
# back as they were afterwards (after an error too): what a session has set
# (an app, a user's .Rprofile, an earlier run) never reaches the text, and
# neither does a home: the template series are the context's folders and
# tflspec's own (G5-1b).

# The options a maker reads, by the name a context gives them
.code_context_options <- c(
  # the study: its folders' variables, the packages its setup attaches,
  # whether its setup sets the rounding and reads the data
  paths = "tflspec.paths", attached = "tflspec.attached",
  study_setup = "tflspec.study_setup",
  # the company's inputs: the template series' folder, the figure style,
  # the ARD catalogs
  series_dir = "tflspec.series_dir", fig_style = "tflspec.fig_style",
  ard_methods = "tflspec.ard_methods", ard_statistics = "tflspec.ard_statistics",
  ard_args = "tflspec.ard_args",
  # how the code is written: the pipe, the ggplot2 it targets, the font size
  # a figure's page size is measured in, where the reports' SPEC files are
  ard_pipe = "rtfreporter.ard_pipe", ggplot2_version = "tflspec.ggplot2_version",
  font_size_half_points = "rtfreporter.font_size_half_points",
  outputs_dir = "tflspec.outputs_dir",
  # (never a maker's input: tflspec's own in-memory runs set these, or a
  # read of a template file for a moment; the quick figures' code reads the
  # last two)
  plain_ns = "tflspec.plain_ns", user_attached = "tflspec.user_attached",
  template_source = "tflspec.template_source",
  data_expr = "tflspec.data_expr", fig_path = "tflspec.fig_path")

# base R's options a maker's format() reads: R's defaults
.code_context_base <- list(digits = 7L, scipen = 0L, OutDec = ".")

# `expr` evaluated with the options of `context` (a named list, names from
# .code_context_options; a value NULL or left out: the option unset) and
# base R's defaults; the options as they were afterwards
.with_code_context <- function(expr, context = list()) {
  nm <- names(context)
  if (length(context) && (is.null(nm) || !all(nm %in% names(.code_context_options)))) {
    stop(".with_code_context(): `context` names only ",
         paste(names(.code_context_options), collapse = ", "), ".", call. = FALSE)
  }
  set <- rep(list(NULL), length(.code_context_options))
  names(set) <- unname(.code_context_options)
  for (k in nm) set[.code_context_options[[k]]] <- list(context[[k]])
  old <- options(c(set, .code_context_base))
  was <- .code_context_state$active
  .code_context_state$active <- TRUE
  on.exit({
    options(old)
    .code_context_state$active <- was
  }, add = TRUE)
  expr
}

# Inside a context: a program's text is being made.  The template series are
# then looked for only where the context says (its `series_dir`) and in
# tflspec's own, never in the tflplanner home (.series_search_dirs()).
.code_context_state <- new.env(parent = emptyenv())
.in_code_context <- function() isTRUE(.code_context_state$active)
