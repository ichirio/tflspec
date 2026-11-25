# A report's code list that refers to one of the study's lists (A7): `SEX:
# {list: SEX}`, all the codes of the study's list SEX in its order, or `SEX:
# {list: SEX, codes: [F, M]}`, those of its codes in the order written.  The
# labels are the study's.  The reference stays as written in the report's
# file and on the sheets (the codelists sheet's `list` cell); where the
# study is known it is made into the rows a list written in place has
# (`{value, label}`), so a program is written the same either way.

# Is a report's code list a reference to the study's (a map with `list:`)?
.cl_is_ref <- function(l) {
  is.list(l) && !is.null(names(l)) && "list" %in% names(l)
}

# A reference as read: list(list = <name>) or list(list = <name>, codes =
# list(<code>, ...)).  `v`, `id`: the variable and the report, for the error.
.cl_ref_norm <- function(l, v, id = NULL) {
  bad <- function(what) {
    stop(if (!is.null(id)) paste0(id, ": "), "codelists: ", v, ": ", what,
         " A reference to the study's list is {list: <name>} or {list: <name>, codes: [<code>, ...]}.",
         call. = FALSE)
  }
  more <- setdiff(names(l), c("list", "codes"))
  if (length(more)) bad(paste0("`", more[1L], "` is not a key of a reference."))
  nm <- l$list
  if (!is.character(nm) || length(nm) != 1L || is.na(nm) || !nzchar(trimws(nm))) {
    bad("`list` is the name of one of the study's lists.")
  }
  out <- list(list = trimws(nm))
  if (!is.null(l$codes)) {
    one <- vapply(as.list(l$codes), function(k) is.atomic(k) && length(k) == 1L && !is.na(k), NA)
    if (!length(one) || !all(one)) bad("`codes` is a list of the list's codes.")
    out$codes <- as.list(as.character(unlist(l$codes, use.names = FALSE)))
    dup <- .cl_codes_dup(out$codes)
    if (!is.null(dup)) bad(paste0("`codes` has \"", dup, "\" twice (a code is a level once)."))
  }
  out
}

# the first code a pick has twice, else NULL
.cl_codes_dup <- function(codes) {
  codes <- unlist(codes, use.names = FALSE)
  k <- anyDuplicated(codes)
  if (k) codes[[k]]
}

# A reference's rows: its codes, each {value, label} with the study's label
# (a list of codes alone: the code), as a list written in place has them.
# `study_cl`: the study's lists (study$codelists); `what`: who says it.
.cl_ref_rows <- function(ref, v, study_cl, id, what) {
  where <- sprintf("%s: report %s, the code list of %s", what, id %||% "(no id)", v)
  x <- study_cl[[ref$list]]
  if (is.null(x)) {
    stop(where, " refers to ", .cl_ref_no_list(ref$list, names(study_cl)), ".", call. = FALSE)
  }
  codes <- .study_list_codes(x)
  lab <- stats::setNames(as.character(unname(x)), codes)
  want <- if (is.null(ref$codes)) codes else unlist(ref$codes, use.names = FALSE)
  miss <- setdiff(want, codes)
  if (length(miss)) {
    stop(where, " picks ", .cl_ref_no_codes(ref$list, miss, codes), ".", call. = FALSE)
  }
  lapply(want, function(k) list(value = k, label = lab[[k]]))
}

# The same for the sheets: a list or a code the study has not got gives one
# row of no code, so the `list` cell is kept and the study still opens (the
# check, and the programs, say it)
.cl_ref_rows_or_cell <- function(ref, v, study_cl, id) {
  tryCatch(.cl_ref_rows(ref, v, study_cl, id, "sheets"), error = function(e) list(list()))
}

# A study's list's codes, in its order (study$codelists$X: the codes, named
# by the code when labelled)
.study_list_codes <- function(x) as.character(names(x) %||% unname(x))

# The words for a list the study has not got and for codes its list has
# not got: one text for the reader that stops and for tfl_check_spec()'s
# row (R/check_spec_study_refs.R).  `have`: the study's list names.
.cl_ref_no_list <- function(name, have) {
  paste0("the study's list ", name, ", which the study has not got (spec/defaults.yml `codelists:` has ",
         if (length(have)) paste(have, collapse = ", ") else "none", ")")
}
.cl_ref_no_codes <- function(name, miss, codes) {
  paste0(paste0("\"", miss, "\"", collapse = ", "), " from the study's list ", name,
         ", which has not got ", if (length(miss) > 1L) "them" else "it",
         " (its codes: ", paste(codes, collapse = ", "), ")")
}

# A report's code lists with each reference made into its rows
.codelists_refs <- function(cl, study_cl, id, what) {
  for (v in names(cl)) {
    if (.cl_is_ref(cl[[v]])) cl[[v]] <- .cl_ref_rows(cl[[v]], v, study_cl, id, what)
  }
  cl
}

# A reference as the sheets' `list` cell -- `SEX` (all its codes), `SEX: F |
# M` (those codes) -- and back.  A list name with `:` or a code with `|`
# would be read back as something else: it stops (`where`: who says it).
.cl_ref_cell <- function(ref, where = NULL) {
  codes <- unlist(ref$codes)
  cell <- if (is.null(codes)) ref$list else paste0(ref$list, ": ", paste(codes, collapse = " | "))
  bad <- function(why, how) {
    stop(if (!is.null(where)) paste0(where, ": "), "the reference to the study's list ", ref$list,
         " cannot be the codelists sheet's `list` cell \"", cell, "\": ", why, " ", how, call. = FALSE)
  }
  if (grepl(":", ref$list, fixed = TRUE)) {
    bad("a list name with \":\" is read back as the name before it.",
        "Rename the study's list (a name such as SEX), or write the report's list in place.")
  }
  pipe <- codes[grepl("|", codes, fixed = TRUE)]
  if (length(pipe)) {
    bad(paste0("a code with \"|\" (\"", pipe[1L], "\") is read back as two codes."),
        "Write the report's list in place ({value, label} rows), or take the list whole ({list: <name>}).")
  }
  cell
}
# (`where`: the report and the variable, for the error: a cell with no list
# name, or a code picked twice, would be written as a file the reader refuses)
.cl_ref_from_cell <- function(cell, where = NULL) {
  out <- list(list = trimws(sub(":.*$", "", cell)))
  codes <- if (grepl(":", cell, fixed = TRUE)) .conv_split(sub("^[^:]*:", "", cell))
  if (length(codes)) out$codes <- as.list(codes)
  bad <- function(why) {
    stop(if (!is.null(where)) paste0(where, ": "), "the codelists sheet's `list` cell \"", cell, "\" ", why,
         " It is the study's list (`SEX`), or the list and the codes picked (`SEX: F | M`).", call. = FALSE)
  }
  if (!nzchar(out$list)) bad("names no list.")
  dup <- .cl_codes_dup(out$codes)
  if (!is.null(dup)) bad(paste0("picks \"", dup, "\" twice (a code is a level once)."))
  out
}

# The `list` cell a report's rows of one variable give, else NULL (all
# blank).  The rows agree: every cell the same, or every cell blank; else it
# stops, naming the cells.
.cl_list_cell <- function(rows) {
  lc <- rows$list
  if (is.null(lc) || !length(lc)) return(NULL)
  lc <- trimws(as.character(lc))
  blank <- is.na(lc) | !nzchar(lc)
  if (all(blank)) return(NULL)
  cells <- unique(ifelse(blank, "(blank)", lc))
  if (length(cells) > 1L) {
    id <- if (is.null(rows$output_id)) NA_character_ else rows$output_id[1L]
    stop("The codelists sheet: report ", id, ", variable ", rows$variable[1L],
         ": its rows' `list` cells differ (", paste0("\"", cells, "\"", collapse = ", "), "). ",
         "Give every row of the variable the same `list` cell (the study's list it is), ",
         "or leave them all blank (the rows are the list).", call. = FALSE)
  }
  cells
}

# Are a reference's rows on a sheet (`value`, `label`, `order`) other than
# the study's rows they were written from (`want`, .cl_ref_rows())?
.cl_ref_rows_edited <- function(rows, want) {
  val <- as.character(rows$value)
  lab <- if (is.null(rows$label)) rep(NA_character_, length(val)) else as.character(rows$label)
  if (!is.null(rows$order)) {
    o <- suppressWarnings(as.numeric(rows$order))
    k <- order(is.na(o), o, seq_along(val))
    val <- val[k]
    lab <- lab[k]
  }
  lab <- ifelse(is.na(lab) | !nzchar(trimws(lab)), val, lab)
  # (a reference has no parent codes: an `under` cell is an edit too)
  un <- rows[["under"]]
  !identical(val, vapply(want, function(r) r$value, "")) ||
    !identical(lab, vapply(want, function(r) r$label, "")) ||
    (!is.null(un) && any(!is.na(un) & nzchar(trimws(as.character(un)))))
}

# The one source of a reference's codes where rows reach a program: the
# rows of each report's variable that have a `list` cell are made again from
# the study's list (.cl_ref_rows()), never taken as the sheet has them.
# `rows`: code list rows (`output_id`, `variable`, `value`, `label`, ...,
# `list`); `study_cl`: the study's lists (study$codelists), NULL when they
# are not known.  A list or code the study has not got: `unknown = "stop"`
# (a program is written: the error names the report, the variable and the
# list), `"drop"` (the variable's rows left out: the sheets are read, not a
# program written).  With no study lists, the rows are as written, unless
# they have no code (a reference the study has not got): that stops (or is
# left out) the same way.
.cl_rows_from_study <- function(rows, study_cl, what, unknown = c("stop", "drop")) {
  unknown <- match.arg(unknown)
  if (!is.data.frame(rows) || !nrow(rows) || is.null(rows[["list"]])) return(rows)
  lc <- trimws(as.character(rows[["list"]]))
  if (!any(!is.na(lc) & nzchar(lc))) return(rows)
  oid <- if (is.null(rows[["output_id"]])) rep(NA_character_, nrow(rows)) else as.character(rows[["output_id"]])
  key <- paste(oid, rows[["variable"]], sep = " / ")
  out <- lapply(unique(key), function(k) {
    g <- rows[key == k, , drop = FALSE]
    cell <- .cl_list_cell(g)
    if (is.null(cell)) return(g)
    id <- oid[key == k][1L]
    v <- g[["variable"]][1L]
    where <- sprintf("%s: report %s, the code list of %s", what, id, v)
    if (is.null(study_cl)) {
      if (!anyNA(g[["value"]])) return(g)
      if (unknown == "drop") return(g[0L, , drop = FALSE])
      stop(where, " refers to the study's list in its `list` cell \"", cell, "\", and the sheets have no code ",
           "of it: the study has not got that list or code (spec/defaults.yml `codelists:`).", call. = FALSE)
    }
    ref <- .cl_ref_from_cell(cell, sprintf("report %s, variable %s", id, v))
    rr <- if (unknown == "stop") .cl_ref_rows(ref, v, study_cl, id, what) else
      tryCatch(.cl_ref_rows(ref, v, study_cl, id, what), error = function(e) NULL)
    if (is.null(rr)) return(g[0L, , drop = FALSE])
    new <- g[rep(1L, length(rr)), , drop = FALSE]
    new[["value"]] <- vapply(rr, function(r) r$value, "")
    if (!is.null(new[["label"]])) new[["label"]] <- vapply(rr, function(r) r$label, "")
    if (!is.null(new[["order"]])) {
      new[["order"]] <- if (is.numeric(g[["order"]])) seq_along(rr) else as.character(seq_along(rr))
    }
    if (!is.null(new[["under"]])) new[["under"]] <- rep(g[["under"]][NA_integer_], length(rr))
    new
  })
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

# ---- the study's lists on the sheets ------------------------------------------

# The study_codelists sheet: the study's lists (defaults.yml `codelists:`),
# a code a row -- the list's name, the code, its label (blank: the list has
# none), its place
.study_codelists_columns <- c("name", "value", "label", "order")

# The study's lists (study$codelists, by name: named by the code when they
# have labels) as defaults.yml's `codelists:` has them
.study_codelists_out <- function(cl) {
  lapply(cl %||% list(), function(x) {
    if (is.null(names(x))) return(as.list(as.character(x)))
    lapply(seq_along(x), function(i) list(value = names(x)[[i]], label = as.character(x[[i]])))
  })
}

# The sheet back as defaults.yml's `codelists:`, each list in its order
# (`order`, else the rows'): `{value, label}` rows, or the codes alone when
# no row has a label
.conv_study_codelists <- function(d) {
  if (is.null(d) || !nrow(d)) return(list())
  d <- d[!is.na(d$name) & nzchar(trimws(d$name)) & !is.na(d$value), , drop = FALSE]
  out <- list()
  for (nm in unique(d$name)) {
    r <- d[d$name == nm, , drop = FALSE]
    o <- if (is.null(r$order)) rep(NA_real_, nrow(r)) else suppressWarnings(as.numeric(r$order))
    r <- r[order(is.na(o), o, seq_len(nrow(r))), , drop = FALSE]
    lab <- if (is.null(r$label)) rep(NA_character_, nrow(r)) else as.character(r$label)
    lab[!is.na(lab) & !nzchar(trimws(lab))] <- NA_character_
    out[[trimws(nm)]] <- if (all(is.na(lab))) as.list(as.character(r$value)) else
      lapply(seq_len(nrow(r)), function(i) {
        m <- list(value = as.character(r$value[i]))
        if (!is.na(lab[i])) m$label <- lab[i]
        m
      })
  }
  out
}
