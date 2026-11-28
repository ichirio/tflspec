# The head and the needs line of a figure script of its own: the packages its
# code calls, the study helpers and time-unit constants it uses.

# a script of its own: the time-unit constants its code uses, defined
# (the figure setup defines them for a study's programs)
.fig_units_used <- function(code) {
  used <- unique(unlist(regmatches(code, gregexpr("days_per_(week|month|year)", code))))
  if (length(used)) c("", .fig_unit_guard(used)) else character()
}

# a script of its own: every study helper its code calls, and the ones
# those call (order_arms() calls order_levels(), which calls set_levels();
# fmt_pvalue() calls fmt_num()), in the helper file's order, each defined
# unless the study's setup has it (a study's programs get them from
# programs/study_helpers.R, sourced by their setup).  A helper calls other
# packages as pkg::fn (rtfreporter::round_num(), dplyr::, cards::): they
# need to be installed, not attached
.fig_helper_used <- function(code) {
  defs <- .study_helper_defs()
  used <- .fig_helpers_carried(code)
  if (!length(used)) return(character())
  one <- function(f) {
    l <- defs[[f]]
    c(paste0("if (!exists(\"", f, "\")) ", l[1L]), l[-1L])
  }
  c("", sprintf("# the study helper%s %s (tfl_helpers_code()), unless the study's setup has %s",
                if (length(used) > 1L) "s" else "", paste0(used, "()", collapse = ", "),
                if (length(used) > 1L) "them" else "it"),
    unlist(lapply(used, one)))
}

# the study helpers a script's code carries, in the helper file's order: the
# ones it calls and the ones those call
.fig_helpers_carried <- function(code) {
  defs <- .study_helper_defs()
  fns <- names(defs)
  used <- .helpers_called(code, fns)
  repeat {
    more <- union(used, .helpers_called(unlist(defs[used]), fns))
    if (length(more) == length(used)) break
    used <- more
  }
  intersect(fns, used)
}

# R's base packages: installed with R, never named as a need
.r_base_pkgs <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
                  "methods", "parallel", "splines", "stats", "stats4", "tcltk",
                  "tools", "utils")

# The packages `src` (code) calls as pkg::fn or pkg:::fn, read from its
# parse, not its text: a `pkg::` in a comment or a string is not a call.  Code
# that does not parse as a whole is read piece by piece; a piece that does not
# parse adds nothing.  A pkg:: on a continuation line of a call that does not
# parse whole is not seen (generated code always parses whole).
.code_ns_pkgs <- function(src) {
  src <- src[!is.na(src)]
  if (!length(src)) return(character())
  one <- function(txt) {
    pd <- tryCatch(utils::getParseData(parse(text = txt, keep.source = TRUE)),
                   error = function(e) NULL)
    if (is.null(pd) || !nrow(pd)) return(NULL)
    pd$text[pd$token == "SYMBOL_PACKAGE"]
  }
  pk <- one(paste(src, collapse = "\n"))
  if (is.null(pk)) pk <- unlist(lapply(src, one))
  unique(as.character(pk))
}

# The packages a script of its own needs installed, for its "# also needs"
# line: every package the script calls as pkg::fn -- `needs` (what its
# writer recorded), its own code (`code`: forest_or's
# rtfreporter::round_num()) and the study helpers it carries (fmt_num() calls
# rtfreporter::round_num()) -- without the packages it attaches (`libs`) and
# R's base packages (.r_base_pkgs).  Read from the parse (.code_ns_pkgs()): a
# comment's `pkg::` is not a call.  With the study's setup, or in a program,
# the program around the code attaches and checks its packages: `needs` as is.
.fig_script_needs <- function(code, needs = character(), libs = character(),
                              setup = FALSE) {
  if (isTRUE(setup) || isTRUE(getOption("tflspec.fig_in_program"))) {
    return(unique(needs))
  }
  used <- .fig_helpers_carried(code)
  helper_pk <- if (length(used)) .code_ns_pkgs(unlist(.study_helper_defs()[used]))
  pk <- unique(c(needs, .code_ns_pkgs(code), helper_pk))
  setdiff(pk, c(.r_base_pkgs, libs))
}

# The head of a figure script, the one rule every figure writer follows:
# what its code uses of the study's setup -- the time-unit constants
# (.fig_units_used()) and the study helpers (.fig_helper_used()) -- each
# defined unless the setup has it.  `setup = TRUE`, or a report's content
# (option tflspec.fig_in_program, set by .content_fig_code_out()): the code
# is a study program's part, after the study's setup, which has them all: none.
.fig_script_head <- function(code, setup = FALSE) {
  if (isTRUE(setup) || isTRUE(getOption("tflspec.fig_in_program"))) return(character())
  c(.fig_units_used(code), .fig_helper_used(code))
}
