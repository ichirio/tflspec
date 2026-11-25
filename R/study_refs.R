# A report's references to the study's code lists (canon 2026-11-17): a
# content argument written `!r study$codelists$SEX` (a template's default
# names one, `demog`'s `value_labels`) is R the report's program runs, where
# the study's setup has made `study`.  The package's own runners
# (tfl_content_run()) have no setup: the resolved report carries the
# study's lists (attribute "study_codelists") and the references are put in
# as their values.  A reference to a list the study does not have is taken
# out when the report is resolved (a column with no list keeps its codes):
# generation needs that for a template's default (demog's `value_labels`
# names SEX, which a study may not have).  One the report writes itself is
# not lost in silence: tfl_check_spec() reports it as an error
# (tfl_study_refs(), tfl_check_study_refs(), R/check_spec_study_refs.R).

# Does an R text name the study's code lists?
.study_ref_rx <- "\\bstudy\\$codelists\\b"
.has_study_ref <- function(x) {
  inherits(x, "tfl_r") && any(grepl(.study_ref_rx, as.character(unclass(x))))
}

# The list a reference names: "SEX" of study$codelists$SEX (or
# study$codelists[["SEX"]]); NA when `e` is not one
.study_ref_name <- function(e) {
  if (!is.call(e) || length(e) != 3L) return(NA_character_)
  op <- as.character(e[[1L]])
  if (!op %in% c("$", "[[")) return(NA_character_)
  inner <- e[[2L]]
  if (!is.call(inner) || !identical(inner[[1L]], as.name("$")) ||
      !identical(inner[[2L]], as.name("study")) || !identical(inner[[3L]], as.name("codelists"))) {
    return(NA_character_)
  }
  nm <- e[[3L]]
  # (`$SEX` names the list; `[[v]]` takes a variable's value, not written out)
  if (op == "$" && is.name(nm)) return(as.character(nm))
  if (is.character(nm) && length(nm) == 1L) nm else NA_character_
}

# An expression with the references to lists the study has not got taken
# out: the reference itself (NULL), or an argument of c() (dropped; c()
# left with nothing is NULL)
.study_ref_prune_expr <- function(e, have) {
  nm <- .study_ref_name(e)
  if (!is.na(nm)) return(if (nm %in% have) e else NULL)
  if (is.call(e) && identical(e[[1L]], as.name("c"))) {
    args <- as.list(e)[-1L]
    keep <- lapply(args, .study_ref_prune_expr, have = have)
    keep <- keep[!vapply(keep, is.null, NA)]
    if (!length(keep)) return(NULL)
    return(as.call(c(list(as.name("c")), keep)))
  }
  e
}

# The content's references to lists the study has not got, taken out (a
# value left with nothing: its key gone).  `have`: the study's list names.
# Only for generation (tfl_resolve_output()), where a template's default
# may name a list the study has not got; the report's own references are
# checked, not pruned in silence: tfl_check_study_refs() (tfl_check_spec()).
.study_refs_prune <- function(content, have) {
  if (inherits(content, "tfl_r") || !is.list(content)) return(content)
  one <- function(v) {
    if (.has_study_ref(v)) {
      txt <- as.character(unclass(v))
      e <- tryCatch(str2lang(txt), error = function(err) NULL)
      if (is.null(e)) return(v)
      p <- .study_ref_prune_expr(e, have)
      if (is.null(p)) return(NULL)
      if (identical(p, e)) return(v)
      return(tfl_r(paste(deparse(p, width.cutoff = 500L), collapse = " ")))
    }
    if (is.list(v) && !is.object(v)) {
      out <- lapply(v, one)
      return(out[!vapply(out, is.null, NA)])
    }
    v
  }
  lapply(content, function(it) {
    if (!inherits(it, "tfl_item") || !is.list(it$args)) return(it)
    it$args <- one(it$args)
    it
  })
}

# The content with the references put in as their values (the package's
# runners: no setup has made `study`); a value that comes to NULL drops its
# key.  `lists`: the study's code lists by name.
.study_refs_eval <- function(content, lists) {
  if (inherits(content, "tfl_r") || !is.list(content)) return(content)
  env <- new.env(parent = baseenv())
  assign("study", list(codelists = lists %||% list()), envir = env)
  one <- function(v) {
    if (.has_study_ref(v)) return(eval(str2lang(as.character(unclass(v))), env))
    if (is.list(v) && !is.object(v)) {
      out <- lapply(v, one)
      return(out[!vapply(out, is.null, NA)])
    }
    v
  }
  lapply(content, function(it) {
    if (!inherits(it, "tfl_item") || !is.list(it$args)) return(it)
    it$args <- one(it$args)
    it
  })
}

# A template's plan_labels items as one (tfl_template_resolve()): a
# variable's heading (one text) and its value labels (a reference to a
# study list) are one entry, `SEX = c(SEX = "Sex, n (%)", study$codelists$SEX)`,
# as plan_labels() takes them.  A reference for a column none of the
# template's analyses reads (`cols`) is left out.
.tr_labels_join <- function(content, cols) {
  is_lab <- vapply(content, function(it) is.list(it) && identical(names(it), "plan_labels"), NA)
  if (!any(is_lab)) return(content)
  at <- which(is_lab)
  joined <- list()
  for (k in at) {
    a <- content[[k]]$plan_labels
    if (!is.list(a)) next
    for (v in names(a)) {
      x <- a[[v]]
      if (is.null(x) || inherits(x, "tfl_unset")) next
      if (.has_study_ref(x) && !v %in% cols) next
      old <- joined[[v]]
      joined[[v]] <- if (is.null(old)) x else .tr_label_pair(v, old, x)
    }
  }
  first <- at[1L]
  content[[first]] <- list(plan_labels = joined)
  drop <- at[-1L]
  if (!length(joined)) drop <- c(first, drop)
  if (length(drop)) content <- content[-drop]
  content
}

# Two entries of one variable as one: a heading and value labels (either
# order) as c(VAR = "heading", <labels>); else the later wins
.tr_label_pair <- function(v, a, b) {
  txt <- function(x) if (inherits(x, "tfl_r")) as.character(unclass(x)) else NULL
  head <- function(x) is.character(x) && !inherits(x, "tfl_r") && length(x) == 1L && is.null(names(x))
  if (head(a) && .has_study_ref(b)) {
    return(tfl_r(sprintf("c(%s = %s, %s)", .spec_name(v), encodeString(a, quote = "\""), txt(b))))
  }
  if (head(b) && .has_study_ref(a)) {
    return(tfl_r(sprintf("c(%s = %s, %s)", .spec_name(v), encodeString(b, quote = "\""), txt(a))))
  }
  b
}

#' The study's code lists a report refers to
#'
#' Every name of the study's code lists (`spec/defaults.yml` `codelists:`,
#' `study$codelists`) that a report writes itself: a code list that refers
#' to one of the study's (`SEX: {list: SEX}`), each code it picks from it
#' (`{list: AGEGR1, codes: [">=65", "<65"]}`), and each
#' `study$codelists$X` (or `study$codelists[["X"]]`) in the report's own R
#' text (`!r`), wherever the report has it (its content, its template's
#' arguments, ...).  A name that is not written out (`study$codelists[[v]]`)
#' is not one.  What a template gives by default (`demog`'s
#' `value_labels`) is not the report's: a list the study has not got is
#' left out of it when the report is resolved ([tfl_resolve_output()]), and
#' it is not listed here.  [tfl_check_spec()] gives an `error` row for
#' each name or code the study has not got.
#'
#' @param output A report as read, [tfl_read_output()] (not resolved:
#'   resolved, its references are rows and its template's defaults are in
#'   it).
#' @return A data frame, a row a reference: `part` (`"codelists"` for the
#'   report's code lists, else the report's key that holds the R text:
#'   `"content"`, `"template"`, ...), `item` (the variable of a code list;
#'   for R text, where it is under `part`, such as `plan_labels / SEX`),
#'   `list` (the study's list) and `code` (a code picked from it; `NA` for
#'   the list itself).  A list's row comes before its codes' rows.  Zero
#'   rows when the report refers to none.
#' @seealso [tfl_check_spec()], [tfl_read_study_spec()]
#' @examples
#' o <- tfl_output("T-1", "table",
#'   codelists = list(SEX = list(list = "SEX"),
#'                    AGEGR1 = list(list = "AGEGR1", codes = list(">=65", "<65"))),
#'   content = list(tfl_item("plan_labels", list(RACE = tfl_r("study$codelists$RACE")))))
#' tfl_study_refs(o)
#' @export
tfl_study_refs <- function(output) {
  if (!inherits(output, "tfl_output")) {
    stop("tfl_study_refs(): `output` is a report as read (tfl_read_output()).", call. = FALSE)
  }
  if (isTRUE(attr(output, "resolved"))) {
    stop("tfl_study_refs(): ", output$id, " is resolved; give the report as read (tfl_read_output()): ",
         "resolved, its references are rows and its template's defaults are in it.", call. = FALSE)
  }
  rows <- list()
  add <- function(part, item, list, code = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.frame(part = part, item = item, list = list, code = code,
                                             stringsAsFactors = FALSE)
  }
  cl <- output$codelists %||% list()
  for (v in names(cl)) {
    if (!.cl_is_ref(cl[[v]])) next
    ref <- .cl_ref_norm(cl[[v]], v, output$id)
    add("codelists", v, ref$list)
    for (k in unlist(ref$codes, use.names = FALSE)) add("codelists", v, ref$list, k)
  }
  for (key in setdiff(names(output), "codelists")) {
    for (r in .study_refs_walk(output[[key]], character())) {
      for (nm in r$names) add(key, paste(r$at, collapse = " / "), nm)
    }
  }
  if (!length(rows)) {
    return(data.frame(part = character(), item = character(), list = character(), code = character(),
                      stringsAsFactors = FALSE))
  }
  out <- unique(do.call(rbind, rows))
  rownames(out) <- NULL
  out
}

# The R texts under a value that name the study's lists: a list of
# list(at = <the keys down to it>, names = <the lists it names>); an item or
# a call is named by its function
.study_refs_walk <- function(v, at) {
  if (inherits(v, "tfl_r")) {
    nm <- .study_ref_names(v)
    return(if (length(nm)) list(list(at = at, names = nm)))
  }
  if (!is.list(v)) return(NULL)
  if (inherits(v, c("tfl_item", "tfl_call"))) {
    at <- c(at, v$fn)
    return(c(.study_refs_walk(v$args, at), .study_refs_walk(v$items, at)))
  }
  nms <- names(v) %||% rep("", length(v))
  out <- list()
  for (i in seq_along(v)) {
    out <- c(out, .study_refs_walk(v[[i]], c(at, if (!is.na(nms[i]) && nzchar(nms[i])) nms[i])))
  }
  out
}

# The study's lists an R text names (study$codelists$SEX,
# study$codelists[["SEX"]]), each once, in the order written; none for a
# text that does not parse (its own check says so) or a name not written out
.study_ref_names <- function(x) {
  if (!.has_study_ref(x)) return(character())
  ex <- tryCatch(parse(text = as.character(unclass(x)), keep.source = FALSE), error = function(e) NULL)
  found <- character()
  walk <- function(e) {
    nm <- .study_ref_name(e)
    if (!is.na(nm)) {
      found <<- c(found, nm)
      return(invisible())
    }
    if (is.call(e)) {
      parts <- as.list(e)
      for (i in seq_along(parts)) {
        if (!identical(parts[[i]], quote(expr = ))) walk(parts[[i]])
      }
    }
    invisible()
  }
  for (e in as.list(ex)) walk(e)
  unique(found)
}
