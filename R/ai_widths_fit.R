# ============================================================================
#  A listing's column widths fitted to its data (11/06, the user)
# ----------------------------------------------------------------------------
#  Choosing the widths is the hardest part of a listing, and rtfreporter has a
#  function that estimates them from the page, the font and the data:
#  fit_listing_widths().  So where the study's data can be read, the columns of
#  a listing that have no width get the fitted one, written as an ordinary
#  number to change (listing_col(width = ); for the `listing` template, the
#  `width` of its entry in `template: args: columns:`).  Where it cannot (no
#  data yet, a column the data lacks, an old rtfreporter, a page too narrow
#  for the columns), the listing gets the first guess, .ai_listing_widths(),
#  as before.  Reading the data never stops the writer.
# ============================================================================

# Where a listing's columns are, and which have no width: a code listing's
# one plan_listing item (`at`), or the `listing` template's `columns`;
# `miss` the positions of the columns with no width.  NULL for anything else
# (several plan_listing items, a report's own code).
.ai_listing_cols <- function(o) {
  if (!identical(o$kind, "listing") || inherits(o$content, "tfl_r")) return(NULL)
  at <- which(vapply(o$content %||% list(), function(it) identical(it$fn, "plan_listing"), NA))
  if (length(at) == 1L) {
    cols <- o$content[[at]]$args$cols %||% list()
    if (!length(cols)) return(NULL)
    miss <- vapply(cols, function(cl) inherits(cl, "tfl_call") && is.null(cl$args$width), NA)
    return(list(how = "code", at = at, n = length(cols), miss = which(miss)))
  }
  cols <- o$template$args$columns
  if (!length(at) && identical(o$template$name, "listing") && is.list(cols) && length(cols)) {
    miss <- vapply(cols, function(cl) is.list(cl) && is.null(cl$width), NA)
    return(list(how = "template", n = length(cols), miss = which(miss)))
  }
  NULL
}

# Does the listing have a column whose width is to be set (a code listing's
# by .ai_widths_unset(), or the `listing` template's)?
.ai_listing_todo <- function(o) {
  if (length(.ai_widths_unset(list(o = o)))) return(TRUE)
  info <- .ai_listing_cols(o)
  !is.null(info) && identical(info$how, "template") && length(info$miss) > 0L
}

# The fit's time grows with the number of records (about 30 s at 500,000),
# and its widths come from a quantile of the cells' widths, so a sample gives
# them as well: on 50,000 and 500,000 records the 5,000-row sample gave the
# same widths.  So above this many records the fit is made on an even-spaced
# sample of that size (no random numbers: the same data, the same widths).
.ai_fit_rows <- 5000L

.ai_fit_sample <- function(recs, n = .ai_fit_rows) {
  if (nrow(recs) <= n) return(recs)
  recs[unique(round(seq(1, nrow(recs), length.out = n))), , drop = FALSE]
}

# The widths fit_listing_widths() gives the listing's columns -- one a
# column, in order -- from its data as its program reads it (the `where`,
# the subjects, the code lists: .listing_data_of()), at the report's page
# and font.  The working directory is the study folder; `st` the study.  An
# error says why it cannot (the caller falls back to the guess).
.ai_listing_fit_widths <- function(o, st) {
  fit <- tryCatch(getExportedValue("rtfreporter", "fit_listing_widths"),
                  error = function(e) NULL)
  if (!is.function(fit)) stop("rtfreporter has no fit_listing_widths()", call. = FALSE)
  ro <- tfl_resolve_output(o, st)
  pl <- .content_table_pipeline(ro$content, "listing")
  step <- Filter(function(cl) identical(cl$fun, "plan_listing"), pl$steps)
  if (length(step) != 1L) stop("the report has no one plan_listing()", call. = FALSE)
  a <- lapply(step[[1L]]$args, .spec_eval, env = emptyenv())
  nm <- names(a) %||% rep("", length(a))
  cols <- unname(a[!nzchar(nm)])
  # a column the plan hides (the `listing` template's `by` variable it does
  # not print, plan_hide()) prints nothing and needs no width: left out of
  # the fit, so the printed columns share the page, and NA for it
  hidden <- unlist(lapply(Filter(function(cl) identical(cl$fun, "plan_hide"), pl$steps), function(cl) {
    unlist(lapply(cl$args, .spec_eval, env = emptyenv()))
  }))
  shown <- !vapply(cols, function(cl) length(cl$vars) > 0L && all(cl$vars %in% hidden), NA)
  if (!any(shown)) stop("the listing prints no column", call. = FALSE)
  all_cols <- cols
  cols <- cols[shown]
  opts <- a[nzchar(nm) & names(a) %in% c("type", "sep", "spacer", "spacer_rel_width",
                                         "layout", "wrap")]
  opts <- Filter(Negate(is.null), opts)
  spec <- do.call(rtfreporter::listing_spec, c(list(cols), opts))
  recs <- suppressMessages(suppressWarnings(as.data.frame(.listing_data_of(ro, st, "."))))
  if (!nrow(recs)) stop("the listing's data has no records", call. = FALSE)
  gone <- setdiff(unique(unlist(lapply(all_cols, function(cl) cl$vars))), names(recs))
  if (length(gone)) {
    stop(sprintf("%s not in the listing's data", paste(gone, collapse = ", ")), call. = FALSE)
  }
  recs <- .ai_fit_sample(recs)
  pg <- .page_args(ro$page$rtf_page)
  sty <- .page_style(ro$page)
  font <- sty$table$font %||% sty$font
  size <- sty$table$font_size_half_points %||% sty$font_size_half_points %||% 18L
  fitted <- suppressMessages(suppressWarnings(fit(
    recs, spec, page = if (length(pg)) pg, font = if (is.character(font) && length(font) == 1L) font,
    font_size_half_points = as.integer(size))))
  w <- vapply(fitted$cols, function(cl) as.integer(cl$width), 1L)
  if (length(w) != length(cols) || anyNA(w)) stop("the columns could not be matched", call. = FALSE)
  out <- rep(NA_integer_, length(all_cols))
  out[shown] <- w
  out
}

# The listings among `outs` given widths where their columns have none:
# fitted to the data where it can be read (.ai_listing_fit_widths()), else
# the guess (.ai_listing_widths(): a code listing's; the `listing` template
# has none to give, so it is left as it is).  A column with a width is
# never touched: the fit only gives the others theirs.  Reading the data
# never stops it.  `dir` the study folder; `vars` the ADaM's variables, for
# the guess.  Returns the outputs, and the ids of the listings given
# `written` widths, `fitted` ones among them.
.ai_listing_widths_set <- function(outs, dir, vars = NULL) {
  written <- fitted <- character()
  todo <- names(outs)[vapply(outs, function(o) {
    identical(o$kind, "listing") && .ai_listing_todo(o)
  }, NA)]
  if (!length(todo)) return(list(outs = outs, written = written, fitted = fitted))
  old <- tryCatch(setwd(dir), error = function(e) NULL)
  if (!is.null(old)) on.exit(setwd(old), add = TRUE)
  st <- if (!is.null(old)) tryCatch(tfl_read_study_spec("."), error = function(e) NULL)
  for (id in todo) {
    o <- outs[[id]]
    info <- .ai_listing_cols(o)
    w <- if (!is.null(st) && !is.null(info)) {
      tryCatch(.ai_listing_fit_widths(o, st), error = function(e) NULL)
    }
    # (the template's own columns: a hidden `by` column the template adds
    # after them is not one of them)
    if (!is.null(w) && identical(info$how, "template") && length(w) > info$n &&
        all(is.na(w[-seq_len(info$n)]))) {
      w <- w[seq_len(info$n)]
    }
    if (!is.null(w) && length(w) == info$n) {
      for (k in info$miss[!is.na(w[info$miss])]) {
        if (identical(info$how, "code")) {
          o$content[[info$at]]$args$cols[[k]]$args$width <- w[k]
        } else {
          o$template$args$columns[[k]]$width <- w[k]
        }
      }
      outs[[id]] <- o
      fitted <- c(fitted, id)
      written <- c(written, id)
    } else if (!identical(info$how, "template")) {
      outs[[id]] <- .ai_listing_widths(o, vars)
      written <- c(written, id)
    }
  }
  list(outs = outs, written = written, fitted = fitted)
}
