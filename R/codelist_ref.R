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
  }
  out
}

# A reference's rows: its codes, each {value, label} with the study's label
# (a list of codes alone: the code), as a list written in place has them.
# `study_cl`: the study's lists (study$codelists); `what`: who says it.
.cl_ref_rows <- function(ref, v, study_cl, id, what) {
  where <- sprintf("%s: report %s, the code list of %s", what, id %||% "(no id)", v)
  x <- study_cl[[ref$list]]
  if (is.null(x)) {
    have <- names(study_cl)
    stop(where, " refers to the study's list ", ref$list, ", which the study has not got ",
         "(spec/defaults.yml `codelists:` has ",
         if (length(have)) paste(have, collapse = ", ") else "none", ").", call. = FALSE)
  }
  codes <- as.character(names(x) %||% unname(x))
  lab <- stats::setNames(as.character(unname(x)), codes)
  want <- if (is.null(ref$codes)) codes else unlist(ref$codes, use.names = FALSE)
  miss <- setdiff(want, codes)
  if (length(miss)) {
    stop(where, " picks ", paste0("\"", miss, "\"", collapse = ", "), " from the study's list ",
         ref$list, ", which has not got ", if (length(miss) > 1L) "them" else "it",
         " (its codes: ", paste(codes, collapse = ", "), ").", call. = FALSE)
  }
  lapply(want, function(k) list(value = k, label = lab[[k]]))
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
.cl_ref_from_cell <- function(cell) {
  out <- list(list = trimws(sub(":.*$", "", cell)))
  codes <- if (grepl(":", cell, fixed = TRUE)) .conv_split(sub("^[^:]*:", "", cell))
  if (length(codes)) out$codes <- as.list(codes)
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
  !identical(val, vapply(want, function(r) r$value, "")) ||
    !identical(lab, vapply(want, function(r) r$label, ""))
}

# ---- the study's lists on the sheets ------------------------------------------

# The study_codelists sheet: the study's lists (defaults.yml `codelists:`),
# a code a row -- the list's name, the code, its label (blank: the list has
# none), its place
.study_codelists_columns <- c("name", "value", "label", "order")

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
