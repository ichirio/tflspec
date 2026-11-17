# A report's references to the study's code lists (canon 2026-11-17): a
# content argument written `!r study$codelists$SEX` (a template's default
# names one, `demog`'s `value_labels`) is R the report's program runs, where
# the study's setup has made `study`.  The package's own runners
# (tfl_content_run()) have no setup: the resolved report carries the
# study's lists (attribute "study_codelists") and the references are put in
# as their values.  A reference to a list the study does not have is taken
# out when the report is resolved (a column with no list keeps its codes).

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
  if (is.name(nm)) as.character(nm) else if (is.character(nm) && length(nm) == 1L) nm else NA_character_
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
