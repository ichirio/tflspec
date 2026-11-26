# ============================================================================
#  tfl_check_spec()'s reading of a report's `!r` values: each is read as R
#  once (parse(), not run); one that does not read is an `error` row, with
#  its place in the SPEC (`content$plan_filter$args$where`) and the
#  parser's message.  The schema takes any `!r` for a slot of R
#  (.schema_ok()) and the code writer pastes the text as it is, so such a
#  value otherwise stops only the program that runs.
#
#  One walker, .r_walk(study, outs, ids) (tflspec#986): it reads the
#  reports it is given as they are in memory (no file, no data), finds each
#  `!r` by its place, and reads it by the kind of slot the schema says it
#  fills.  The report's `ard:` is left to the ARD check (tfl_ard_code()'s
#  problems) for now: its fields have their own forms (`args` the
#  arguments of a call, `derive` and `post` pieces between `|`).
# ============================================================================

# The `error` rows of every `!r` of the reports `ids` of `outs` (reports
# as read, not resolved: a template's own defaults are not the report's);
# `study` is for the study's own `!r` values (a later slice).  The rows
# are .check_rows()'s.
.r_walk <- function(study, outs, ids) {
  rows <- list(.check_rows())
  for (id in ids) rows[[length(rows) + 1L]] <- .r_walk_report(outs[[id]], id)
  do.call(rbind, rows)
}

# One report, one row per `!r` that does not read
.r_walk_report <- function(o, id) {
  x <- unclass(o)
  rows <- list(.check_rows())
  for (lf in .r_leaves(x[setdiff(names(x), "ard")], character(), NULL)) {
    m <- .r_parse_problem(lf$text, lf$kinds)
    if (is.null(m)) next
    shown <- .ai_one_line(lf$text)
    if (nchar(shown) > 60L) shown <- paste0(substr(shown, 1L, 57L), "...")
    at <- paste(lf$at, collapse = "$")
    rows[[length(rows) + 1L]] <- .check_rows(
      output_id = id, part = lf$at[1L], item = at, level = "error",
      message = sprintf("`!r %s` does not read as R (%s)", shown, m))
  }
  unique(do.call(rbind, rows))
}

# Every `!r` text under `x`: its place (a vector), the text and the kinds
# of slot it fills.  `ty`: the schema's type of the slot `x` is in (NULL:
# none known), kept down through lists; an item `{fn, args}` takes each
# argument's type from the schema of its function.  A blank text is no
# parse error (it is a note, R/check_spec.R); a string that is not `!r`
# is no R.
.r_leaves <- function(x, at, ty) {
  if (inherits(x, "tfl_r")) {
    txt <- as.character(unclass(x))
    txt <- txt[!is.na(txt) & nzchar(trimws(txt))]
    return(lapply(txt, function(t) list(at = at, text = t, kinds = .r_kinds(ty))))
  }
  if (is.character(x) || !is.list(x) || !length(x)) return(NULL)
  nm <- names(x) %||% rep("", length(x))
  # (an item by its function's name: content$plan_cells$args$categorical)
  fn <- vapply(x, function(e) if (is.list(e) && is.character(e$fn)) e$fn[1L] else "", "")
  nm[!nzchar(nm)] <- ifelse(nzchar(fn), fn, as.character(seq_along(x)))[!nzchar(nm)]
  ent <- if (is.character(x[["fn"]]) && length(x[["fn"]]) == 1L) tryCatch(tfl_schema(x[["fn"]]), error = function(e) NULL)
  out <- list()
  for (i in seq_along(x)) {
    sub <- x[[i]]
    if (!is.null(ent) && identical(nm[i], "args") && is.list(sub) && !inherits(sub, "tfl_r")) {
      a <- names(sub) %||% rep("", length(sub))
      for (j in seq_along(sub)) {
        t <- if (nzchar(a[j])) tryCatch(.schema_arg_type(ent, a[j]), error = function(e) NULL)
        out <- c(out, .r_leaves(sub[[j]], c(at, nm[i], if (nzchar(a[j])) a[j] else j), t))
      }
    } else {
      out <- c(out, .r_leaves(sub, c(at, nm[i]), if (!is.null(ent)) NULL else ty))
    }
  }
  out
}

# The kinds of slot a schema type holds, through its lists and alternatives
.r_kinds <- function(ty) {
  if (!is.list(ty)) return(character())
  unique(c(ty$kind, unlist(lapply(Filter(is.list, ty$args), .r_kinds))))
}

# The old per-report entry (R/check_spec.R)
.check_r_parse_rows <- function(o, id) .r_walk(NULL, stats::setNames(list(o), id), id)

# NULL when `text` reads as R, else the parser's message (with its line
# when the text has more than one).  By the slot's kinds: `r`, `formula`,
# `call` and the rest read as plain R (`~` is plain R, and a `call` that
# is not the right call is the schema's row); a slot of only `guard` takes
# `cond ~ text`, so `~ text` alone is no guard.
.r_parse_problem <- function(text, kinds = character()) {
  ex <- NULL
  e <- tryCatch({
    ex <- parse(text = text, keep.source = FALSE)
    NULL
  }, error = function(err) conditionMessage(err))
  if (is.null(e)) {
    if ("guard" %in% kinds && !any(c("r", "formula") %in% kinds) && length(ex) == 1L &&
        is.call(ex[[1L]]) && identical(ex[[1L]][[1L]], as.name("~")) && length(ex[[1L]]) == 2L) {
      return("a guard is `condition ~ text`, with both sides")
    }
    return(NULL)
  }
  first <- strsplit(e, "\n", fixed = TRUE)[[1L]][1L]
  line <- sub("^<text>:([0-9]+):.*$", "\\1", first)
  what <- trimws(sub("^<text>:[0-9]+:[0-9]+: ", "", first))
  if (grepl("\n", text, fixed = TRUE) && grepl("^[0-9]+$", line)) paste0("line ", line, ": ", what) else what
}
