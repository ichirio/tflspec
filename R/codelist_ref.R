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
# M` (those codes) -- and back
.cl_ref_cell <- function(ref) {
  if (is.null(ref$codes)) return(ref$list)
  paste0(ref$list, ": ", paste(unlist(ref$codes), collapse = " | "))
}
.cl_ref_from_cell <- function(cell) {
  out <- list(list = trimws(sub(":.*$", "", cell)))
  codes <- if (grepl(":", cell, fixed = TRUE)) .conv_split(sub("^[^:]*:", "", cell))
  if (length(codes)) out$codes <- as.list(codes)
  out
}

# The `list` cell a variable's rows give (the first not blank), else NULL
.cl_list_cell <- function(rows) {
  lc <- rows$list
  if (is.null(lc)) return(NULL)
  lc <- lc[!is.na(lc) & nzchar(trimws(lc))]
  if (length(lc)) lc[[1L]]
}
