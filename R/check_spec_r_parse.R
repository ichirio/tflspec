# ============================================================================
#  tfl_check_spec()'s reading of the SPEC's `!r` values: each is read as R
#  once (parse(), not run); one that does not read is an `error` row, with
#  its place in the SPEC (`content$plan_filter$args$where`) and the
#  parser's message.  The schema takes any `!r` for a slot of R
#  (.schema_ok()) and the code writer pastes the text as it is, so such a
#  value otherwise stops only the program that runs.
#
#  One walker, .r_walk(study, outs, ids) (tflspec#986): it reads the
#  reports it is given and the study's own files as they are in memory (no
#  file, no data), finds each `!r` by its place, and reads it by the kind
#  of slot it fills: the schema's kind for an item's argument, a fixed
#  kind for the fields of R of the ARD and of data.yml (.r_ard_kinds).
# ============================================================================

# The fields of R of a report's `analysis:` (analysis_data, analyses) and of
# data.yml (populations, records, datasets): no schema (they are a sheet's
# columns), a fixed kind each.  `where` one condition; `derive` pieces
# `name = expression` between `|`; `then` and `code` R code, several
# expressions; `args` the arguments of a call; `post` steps between `|`.
# Such a field is R whether or not it is written `!r` (the ARD program
# pastes it either way), so its plain text is read too.
.r_ard_kinds <- c(where = "where", derive = "derive", then = "code", code = "code",
                  args = "args", post = "post")

# The `error` rows of every `!r` of the reports `ids` of `outs` (reports
# as read, not resolved: a template's own defaults are not the report's)
# and, when `study` is given, of the study's own files.  The rows are
# .check_rows()'s.
.r_walk <- function(study, outs, ids) {
  rows <- list(.check_rows(), .r_walk_study(study))
  for (id in ids) rows[[length(rows) + 1L]] <- .r_walk_report(outs[[id]], id)
  do.call(rbind, rows)
}

# One report, one row per `!r` that does not read: `part` the report's key
# it is under (its `analysis:`'s rows part "ard", the ARD stage's), `item`
# its place, by the file's keys (`analysis$analyses$<id>$where`).  A key the
# walker cannot go through (a form the reader refuses) is left to the
# reader's row.
.r_walk_report <- function(o, id) {
  x <- unclass(o)
  lv <- list()
  for (k in names(x)) {
    # (a table's `data:` list of definitions not yet read into `analysis:`: by
    # the same fields of R)
    defs <- identical(k, "data") && is.list(x[[k]]) && is.null(names(x[[k]])) &&
      all(vapply(x[[k]], is.list, NA))
    lv <- c(lv, tryCatch(
      if (identical(k, "ard")) .r_ard_leaves(x[[k]], "analysis") else if (defs) .r_entry_leaves(x[[k]], k)
      else .r_leaves(x[k], character(), NULL),
      error = function(e) list()))
  }
  .r_rows(lv, id, function(at) if (identical(at[1L], "analysis")) "ard" else at[1L],
          function(at) paste(at, collapse = "$"))
}

# The study's own `!r` values, `output_id` NA and `part` "study": study.yml
# (its `setup` code block), defaults.yml (its content, page, styles,
# figure style and options) and data.yml (populations, records and
# datasets, by their fields of R).  `item` the place after the file's name
# (`data.yml: populations$SAF$where`).
.r_walk_study <- function(study) {
  if (!inherits(study, "tfl_study_spec")) return(.check_rows())
  s <- unclass(study)
  dd <- if (is.list(s$data)) s$data else list()
  roots <- list(
    list("study.yml", function() {
      lv <- .r_leaves(unclass(s$study), character(), NULL)
      lapply(lv, function(lf) {
        if (identical(lf$at, "setup")) lf$kinds <- "code"
        lf
      })
    }),
    list("defaults.yml", function() .r_leaves(unclass(s$defaults), character(), NULL)),
    list("defaults.yml", function() .r_leaves(list(styles = s$styles), character(), NULL)),
    list("defaults.yml", function() .r_leaves(s[c("figure_style", "options")], character(), NULL)),
    list("data.yml", function() .r_entry_leaves(dd$populations, "populations")),
    list("data.yml", function() .r_entry_leaves(dd$records, "records")),
    list("data.yml", function() .r_entry_leaves(dd$datasets, "datasets", "dataset")))
  rows <- list(.check_rows())
  for (r in roots) {
    lv <- tryCatch(r[[2L]](), error = function(e) list())
    rows[[length(rows) + 1L]] <- .r_rows(lv, NA_character_, function(at) "study",
                                         function(at) paste0(r[[1L]], ": ", paste(at, collapse = "$")))
  }
  unique(do.call(rbind, rows))
}

# The rows of the leaves that do not read (.r_parse_problem()); `part` and
# `item` make the row's part and item of a leaf's place
.r_rows <- function(leaves, output_id, part, item) {
  rows <- list(.check_rows())
  for (lf in leaves) {
    m <- .r_parse_problem(lf$text, lf$kinds)
    if (is.null(m)) next
    rows[[length(rows) + 1L]] <- .check_rows(
      output_id = output_id, part = part(lf$at), item = item(lf$at), level = "error",
      message = .r_bad_message(lf$text, m, lf$tagged))
  }
  unique(do.call(rbind, rows))
}

# The one text of a value that does not read: `!r <text>` (shortened) and
# the parser's message; a field of R written without `!r` is shown without
.r_bad_message <- function(text, m, tagged = TRUE) {
  shown <- .ai_one_line(text)
  if (nchar(shown) > 60L) shown <- paste0(substr(shown, 1L, 57L), "...")
  sprintf("`%s%s` does not read as R (%s)", if (tagged) "!r " else "", shown, m)
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
    return(lapply(txt, function(t) list(at = at, text = t, kinds = .r_kinds(ty), tagged = TRUE)))
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

# A report's `analysis:`: its analysis data and analyses by their fields of R
# (.r_entry_leaves()), any other key as the rest of the report
.r_ard_leaves <- function(a, at) {
  if (!is.list(a) || inherits(a, "tfl_r")) return(.r_leaves(a, at, NULL))
  out <- list()
  for (k in names(a) %||% rep("", length(a))) {
    out <- c(out, if (k %in% c("analysis_data", "analyses")) {
      .r_entry_leaves(a[[k]], c(at, k))
    } else {
      .r_leaves(a[k], at, NULL)
    })
  }
  out
}

# The entries of a list of definitions (a report's analysis data or
# analyses, data.yml's populations ...), each at its id (`idkey`; its
# index when it has none): a field of R (.r_ard_kinds) by its kind, `!r`
# or not; any other `!r` as the rest of the report
.r_entry_leaves <- function(entries, at, idkey = "id") {
  if (!is.list(entries) || !length(entries)) return(list())
  nm <- names(entries) %||% rep("", length(entries))
  out <- list()
  for (i in seq_along(entries)) {
    e <- entries[[i]]
    key <- if (nzchar(nm[i])) nm[i] else {
      v <- if (is.list(e)) as.character(unlist(e[[idkey]]))[1L] else NA_character_
      if (length(v) && !is.na(v) && nzchar(v)) v else as.character(i)
    }
    if (!is.list(e) || inherits(e, "tfl_r")) {
      out <- c(out, .r_leaves(e, c(at, key), NULL))
      next
    }
    fs <- names(e) %||% rep("", length(e))
    for (j in seq_along(e)) {
      v <- e[[j]]
      if (fs[j] %in% names(.r_ard_kinds) && is.character(v) && length(v) == 1L) {
        if (is.na(v) || !nzchar(trimws(v))) next
        out[[length(out) + 1L]] <- list(at = c(at, key, fs[j]), text = as.character(unclass(v)),
                                        kinds = unname(.r_ard_kinds[fs[j]]),
                                        tagged = inherits(v, "tfl_r"))
      } else {
        out <- c(out, .r_leaves(v, c(at, key, if (nzchar(fs[j])) fs[j] else j), NULL))
      }
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

# A field of R of an ARD sheet (`field`: a name of .r_ard_kinds) that does
# not read: the walker's text (.r_bad_message()), else NULL.  The ARD's own
# check (tfl_ard_spec()) says it with these words.
.r_ard_problem <- function(text, field) {
  if (is.null(text) || length(text) != 1L || is.na(text) || !nzchar(trimws(text))) return(NULL)
  m <- .r_parse_problem(as.character(unclass(text)), unname(.r_ard_kinds[field]))
  if (is.null(m)) NULL else .r_bad_message(text, m, inherits(text, "tfl_r"))
}

# The ARD check's problems of a report (tfl_ard_code()'s `problems`: sheet,
# row, field, message) less those that say again what a walker's row says:
# a field of R that does not read, at the place of an `error` row of `wk`
# (the report's rows; and the study's, data.yml's, on a full check)
.r_ard_problems_left <- function(p, wk) {
  if (!is.data.frame(p) || !nrow(p) || !nrow(wk)) return(p)
  at <- ifelse(p$sheet %in% c("analysis_data", "analyses"),
               paste("analysis", p$sheet, p$row, p$field, sep = "$"),
               paste0("data.yml: ", p$sheet, "$", p$row, "$", p$field))
  said <- at %in% wk$item[wk$level == "error"] & p$field %in% names(.r_ard_kinds) &
    grepl("does not read as R|is not a call|the condition is not R", p$message)
  p[!said, , drop = FALSE]
}

# NULL when `text` reads as R, else the parser's message (with its line
# when the text has more than one).  By the slot's kinds: `r`, `formula`,
# `call` and the rest read as plain R (`~` is plain R, and a `call` that
# is not the right call is the schema's row); a slot of `guard` (and no `r`
# or `formula`; a `call` alternative names its own function, not `~`) takes
# `cond ~ text`, so `~ text` alone is no guard.  The ARD's kinds
# (.r_ard_kinds): `args` reads as a call's arguments (.args_problem()),
# `derive` and `post` piece by piece, `where` as one condition, `code` as
# the rest.
.r_parse_problem <- function(text, kinds = character()) {
  if ("args" %in% kinds) {
    m <- .r_parse_message(.args_problem(text), text)
    if (!is.null(m)) return(m)
    # (`method = ` reads, as an argument with no value: the call would not run)
    a <- .args_list(text)
    empty <- vapply(seq_along(a), function(i) identical(a[[i]], quote(expr = )), NA)
    if (any(empty)) {
      nm <- names(a) %||% rep("", length(a))
      return(sprintf("argument %s has no value", if (nzchar(nm[empty][1L])) sprintf("`%s`", nm[empty][1L])
                     else as.character(which(empty)[1L])))
    }
    return(NULL)
  }
  if ("derive" %in% kinds) {
    for (piece in .split_bar(text)) {
      m <- .r_parse_problem(sprintf("list(%s)", piece))
      if (!is.null(m)) return(sprintf("piece `%s`: %s", piece, m))
      if (!grepl("^[A-Za-z.][A-Za-z0-9._]* *=[^=]", piece)) {
        return(sprintf(paste("piece `%s` is not `name = expression` (pieces are separated by `|`",
                             "outside parentheses; R's own `|` goes inside them)"), piece))
      }
    }
    return(NULL)
  }
  if ("post" %in% kinds) {
    for (piece in .split_post(text)) {
      m <- .r_parse_problem(piece)
      if (!is.null(m)) return(sprintf("step `%s`: %s", piece, m))
    }
    return(NULL)
  }
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
    if ("where" %in% kinds && length(ex) != 1L) {
      return(sprintf("a condition is one expression; this is %d", length(ex)))
    }
    return(NULL)
  }
  .r_parse_message(e, text)
}

# A parser's message as a row says it: its first line, without `<text>:`
# and the column; "line N: " first when the text has more than one line
# (the end of the input is on the last line, not the one R counts after it)
.r_parse_message <- function(e, text) {
  if (is.null(e)) return(NULL)
  first <- strsplit(e, "\n", fixed = TRUE)[[1L]][1L]
  line <- sub("^<text>:([0-9]+):.*$", "\\1", first)
  what <- trimws(sub("^<text>:[0-9]+:[0-9]+: ", "", first))
  if (!grepl("\n", text, fixed = TRUE) || !grepl("^[0-9]+$", line)) return(what)
  n <- length(strsplit(sub("\n+$", "", text), "\n", fixed = TRUE)[[1L]])
  paste0("line ", min(as.integer(line), n), ": ", what)
}
