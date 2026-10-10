# ============================================================================
#  Small helpers the spec code shares with the engine that moved to
#  rtfreporter (plan E of the design discussion): kept here so tflspec needs
#  nothing of rtfreporter's internals.
# ============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

.ard_stop <- function(...) stop(..., call. = FALSE)

.ard_need <- function(pkg, what) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    .ard_stop(sprintf("%s needs the '%s' package (install it, or see ?%s).",
                      what, pkg, "tflspec-spec"))
  }
  invisible(TRUE)
}

.ard_need <- function(pkg, what) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    .ard_stop(sprintf("%s needs the '%s' package (install it, or see ?%s).",
                      what, pkg, "tflspec-spec"))
  }
  invisible(TRUE)
}

# stable order of first appearance
.ard_first_seen <- function(x) {
  u <- unique(as.character(x))
  u[!is.na(u)]
}

.ard_tokens <- function(tpl) {
  m <- regmatches(tpl, gregexpr("[{][^{}]+[}]", tpl))[[1]]
  if (!length(m)) return(character(0))
  m
}

.ard_token_parts <- function(tok) {
  inner <- substr(tok, 2L, nchar(tok) - 1L)
  at <- regexpr(":", inner, fixed = TRUE)
  if (at < 0L) return(list(name = inner, spec = ""))
  list(name = substr(inner, 1L, at - 1L),
       spec = substr(inner, at + 1L, nchar(inner)))
}

# RStudio's own answer to "which pipe does this person write", read from the
# preference behind Ctrl+Shift+M: `insert_native_pipe_operator`, a boolean
# whose factory default is FALSE.  Asking the IDE beats guessing, and it is
# the same setting the author sees in Tools > Global Options > Code.
# Split in two so the mapping is testable without an RStudio to run in.
.ard_rstudio_pref <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) return(NULL)
  ok <- tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)
  if (!isTRUE(ok)) return(NULL)
  tryCatch(
    rstudioapi::readRStudioPreference("insert_native_pipe_operator", NULL),
    error = function(e) NULL)
}

# NULL means "no answer" -- not RStudio, no rstudioapi, or the read failed --
# and the caller decides what to do about that.
.ard_pipe_rstudio <- function(pref = .ard_rstudio_pref()) {
  if (!is.logical(pref) || length(pref) != 1L || is.na(pref)) return(NULL)
  if (pref) "|>" else "%>%"
}

# The pipe operator the generated script is written with, resolved once:
# an explicit `pipe =`, then `getOption("rtfreporter.ard_pipe")`, then
# RStudio's own setting, then `%>%`.
#
# `%>%` is the floor rather than `|>` because the two are NOT
# interchangeable, and base R has not closed the gap: as of R 4.6 the
# placeholder `_` may still appear only once in a call and only as a named
# argument (or as the head of a `$`/`[`/`[[`/`@` chain), while magrittr's
# `.` is positional and may appear twice.  The generated pipeline itself
# uses no placeholder, so the choice only decides what the author may add
# at the seam -- which is why following the IDE is safe here, and why a
# study that wants one answer for everybody pins it ONCE --
#     options(rtfreporter.ard_pipe = "|>")
.ard_pipe_op <- function(x = NULL) {
  asked <- !is.null(x)
  if (is.null(x)) {
    x <- getOption("rtfreporter.ard_pipe")
    asked <- !is.null(x)
  }
  if (is.null(x)) x <- "rstudio"
  if (!is.character(x) || length(x) != 1L ||
        !x %in% c("%>%", "|>", "rstudio")) {
    .ard_stop(paste0(
      "`pipe` must be \"%>%\" (magrittr), \"|>\" (base R, needs no ",
      "package)\n  or \"rstudio\" (whichever RStudio's Insert Pipe ",
      "Operator inserts)."))
  }
  if (identical(x, "rstudio")) {
    got <- .ard_pipe_rstudio()
    if (is.null(got)) {
      # Silent when this was merely the default; a caller who NAMED
      # "rstudio" asked a question and is owed the answer.
      if (asked) {
        message("`pipe = \"rstudio\"`: no RStudio preference to read -- ",
                "not running in\n  RStudio, or rstudioapi is not ",
                "installed.  Writing \"%>%\".")
      }
      got <- "%>%"
    }
    x <- got
  }
  x
}

# ---- what a function was given ---------------------------------------------

# A file that is not there, said one way everywhere:
#   tfl_read_table_spec(): no file 'nope.xlsx'.
.stop_no_file <- function(path, fn) {
  .ard_stop(sprintf("%s(): no file %s.", fn, encodeString(path, quote = "'")))
}

# What a value is, for a message: "a data.frame (3 x 2)", "a list", "NULL".
.what <- function(x) {
  if (is.null(x)) return("NULL")
  cl <- class(x)[1L]
  if (is.data.frame(x)) return(sprintf("a %s (%d x %d)", cl, nrow(x), ncol(x)))
  if (is.character(x)) return(sprintf("text (%s)", paste(
    encodeString(utils::head(x, 2L), quote = "'"), collapse = ", ")))
  paste0(if (grepl("^[aeiou]", cl)) "an " else "a ", cl)
}

# The spec a function was given, as the spec it needs: a spec object as it
# is, the path of its workbook read, the sheets of one as a list made the
# spec.  Anything else -- an ARD, a data frame, a number -- is refused
# naming what the function wants and what it got.
.as_spec <- function(x, kind = c("ard", "table", "listing"), fn,
                     arg = "spec", output_id = NULL) {
  kind <- match.arg(kind)
  want <- switch(kind,
    ard = "an ARD spec (tfl_ard_spec(), tfl_read_ard_spec())",
    table = "a table / report spec (tfl_read_table_spec(), tfl_read_report_spec())",
    listing = "a listing spec (tfl_listing_spec(), tfl_read_listing_spec())")
  refuse <- function() {
    .ard_stop(sprintf("%s(): `%s` must be %s or the path of its workbook; got %s.",
                      fn, arg, want, .what(x)))
  }
  if (is.character(x) && !is.object(x)) {
    if (!length(x) || anyNA(x)) refuse()
    return(switch(kind,
      ard = tfl_read_ard_spec(x),
      table = tfl_read_report_spec(x, output_id),
      listing = tfl_read_listing_spec(x, output_id = output_id)))
  }
  if (!is.list(x) || is.data.frame(x)) refuse()
  switch(kind,
    ard = {
      if (inherits(x, "tfl_ard_spec")) x
      else if ("analyses" %in% names(x)) structure(x, class = "tfl_ard_spec")
      else refuse()
    },
    table = {
      sheets <- c("study", names(.ard_spec_schema()))
      if (inherits(x, "tfl_table_spec")) x
      else if (length(names(x)) && all(names(x) %in% sheets)) tfl_table_spec(x)
      else refuse()
    },
    listing = if (inherits(x, "tfl_listing_spec")) x else refuse())
}


# The RTF of table pages, as bytes: what a reader gets, with the run time
# fixed so that two renderings of the same pages compare equal.
.spec_rtf <- function(pages) {
  old <- options(rtfreporter.render_time =
                   as.POSIXct("2000-01-01 00:00:00", tz = "UTC"))
  on.exit(options(old), add = TRUE)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  doc <- rtfreporter::rtf_tables(rtfreporter::rtf_document(), pages)
  suppressMessages(rtfreporter::generate_rtfreport(doc, f, overwrite = TRUE,
                                                   program = "program.R"))
  readBin(f, "raw", file.info(f)$size)
}
