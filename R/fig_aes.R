# ============================================================================
#  A mapping's value: a column, or an expression of the mapping's form
# ----------------------------------------------------------------------------
#  A layer's `mapping:` maps an aesthetic to a column (`colour: TRTA`).
#  What clinical figures map besides a column is a short list, and a SPEC
#  writes it as its text, untagged, as the code writes it:
#
#    mapping: {x: factor(AVISITN), fill: TRTA}
#    mapping: {y: after_stat(density)}                    # a stat's variable
#    mapping: {y: after_stat(count / sum(count))}
#    mapping: {x: reorder(USUBJID, -PCHG)}                # a waterfall's bars
#    mapping: {fill: after_scale(alpha(colour, 0.4))}
#    mapping: {x: stage(AVAL, after_stat = x)}
#    mapping: {ymin: mean - se, ymax: mean + se}           # mean +/- SE
#    mapping: {label: paste0(n, " (", pct, "%)")}
#    mapping: {colour: '"Overall"'}                       # a constant label
#    mapping: {fill: PCHG <= -30}                         # a condition
#    mapping: {fill: ifelse(PCHG <= -30, "Response", "No response")}
#    mapping: {colour: cut(PCHG, c(-Inf, -30, 20, Inf))}
#    mapping: {shape: is.na(ADT)}
#
#  The form: a call of one of `.fig_aes_fns` (factor(), as.numeric(),
#  interaction(), reorder(), log10(), paste0(), sprintf(), after_stat(),
#  after_scale(), stage(), ifelse(), cut(), is.na() ...), of the
#  arithmetic or of a comparison (<, <=, ==, !=, %in%, !, &, |), its
#  arguments columns (the names), constants, the arithmetic of them and
#  a few summaries (sum(), max(), mean() ...); a text in quotes.
#  Anything else is R (`!r`).  The check reads the form (a text that is
#  neither a column's name nor of the form warns) and the variables of an
#  after_stat(): those the layer's stat computes, or an aesthetic.
# ============================================================================

# the functions a mapping's expression is a call of
.fig_aes_fns <- c("factor", "as.factor", "ordered", "as.numeric", "as.integer",
                  "as.character", "interaction", "reorder", "rev", "abs", "sqrt",
                  "exp", "log", "log2", "log10", "log1p", "round", "signif",
                  "paste", "paste0", "sprintf", "format", "cut_width", "cut_interval",
                  "cut_number", "after_stat", "after_scale", "stage",
                  # a condition and its categories (a waterfall's response,
                  # a value above the normal range, a missing date)
                  "ifelse", "cut", "is.na", "nchar", "toupper", "tolower",
                  "substr", "trimws")

# the operators a mapping's expression may be of, at its top too
# (ymin: mean - se)
.fig_aes_ops <- c("+", "-", "*", "/", "^", "(",
                  "<", ">", "<=", ">=", "==", "!=", "%in%", "!", "&", "|")

# the functions and operators inside it, besides those
.fig_aes_inner <- c(.fig_aes_ops, "c", "sum", "max", "min", "mean",
                    "median", "length", "alpha", "pmin", "pmax", "cumsum",
                    "[", "duplicated", "unique", "rank", "n_distinct")

# Is `e` (a language object) a mapping's expression of the form: a call
# of `.fig_aes_fns` or of the arithmetic, whose parts are names, constants
# or calls of the form?
.fig_aes_form <- function(e) {
  if (!is.call(e) || !is.name(e[[1L]])) return(FALSE)
  if (!as.character(e[[1L]]) %in% c(.fig_aes_fns, .fig_aes_ops)) return(FALSE)
  part <- function(x) {
    if (is.name(x)) return(nzchar(as.character(x)))
    if (is.atomic(x)) return(length(x) == 1L)
    if (!is.call(x) || !is.name(x[[1L]])) return(FALSE)
    if (!as.character(x[[1L]]) %in% c(.fig_aes_fns, .fig_aes_inner)) return(FALSE)
    all(vapply(as.list(x)[-1L], part, NA))
  }
  all(vapply(as.list(e)[-1L], part, NA))
}

# A mapping's value of the SPEC as its R: a column's name (`TRTA`, or
# `` `a b` `` in backticks) its name, an expression of the form
# (`factor(AVISITN)`, `mean - se`) its call, a text in quotes
# (`'"Overall"'`) that text, a constant label; NULL for a text that is
# none of them (written as a column's name, in backticks, as before)
.fig_aes_lang <- function(v) {
  if (!is.character(v) || length(v) != 1L || is.na(v)) return(NULL)
  if (make.names(v) == v) return(as.name(v))
  e <- tryCatch(str2lang(v), error = function(err) NULL)
  if (is.name(e) || (is.character(e) && length(e) == 1L)) return(e)
  if (!is.null(e) && .fig_aes_form(e)) e
}

# A mapping's expression of the code as the SPEC's text: of the form, its
# text (read back the same), a text constant in its quotes; NULL for any
# other (R, !r)
.fig_aes_text <- function(e) {
  if (is.character(e) && length(e) == 1L && !is.na(e)) return(.fig_deparse(e))
  if (!.fig_aes_form(e)) return(NULL)
  txt <- .fig_deparse(e)
  back <- tryCatch(str2lang(txt), error = function(err) NULL)
  if (identical(back, e)) txt
}

# The variables an after_stat() names in a mapping's expression (the
# names inside after_stat(), and stage()'s after_stat =)
.fig_aes_stat_vars <- function(e) {
  out <- character()
  walk <- function(x, inside = FALSE) {
    if (is.name(x)) {
      if (inside) out <<- c(out, as.character(x))
      return()
    }
    if (!is.call(x)) return()
    f <- if (is.name(x[[1L]])) as.character(x[[1L]]) else ""
    a <- as.list(x)[-1L]
    nm <- names(a) %||% rep("", length(a))
    for (i in seq_along(a)) {
      walk(a[[i]], inside || identical(f, "after_stat") ||
             (identical(f, "stage") && identical(nm[i], "after_stat")))
    }
  }
  walk(e)
  unique(out)
}

# The variables a stat computes, which an after_stat() names (by the
# stat's class; ggplot2's help pages, "Computed variables").  A stat not
# here is not judged.
.fig_stat_vars <- list(
  StatCount = c("count", "prop", "width"),
  StatBin = c("count", "density", "ncount", "ndensity", "width"),
  StatDensity = c("density", "count", "scaled", "ndensity", "n", "wdensity"),
  StatEcdf = c("ecdf"),
  StatSummary = c("ymin", "ymax"),
  StatSummaryBin = c("ymin", "ymax", "width"),
  StatBoxplot = c("width", "ymin", "lower", "middle", "upper", "ymax", "notchupper",
                  "notchlower", "relvarwidth", "outliers"),
  StatYdensity = c("density", "scaled", "count", "n", "violinwidth", "width", "quantile"),
  StatSmooth = c("ymin", "ymax", "se"),
  StatQq = c("sample", "theoretical"),
  StatQqLine = c("slope", "intercept"),
  StatSum = c("n", "prop"),
  StatBin2d = c("count", "density", "ncount", "ndensity"),
  StatBinhex = c("count", "density", "ncount", "ndensity"),
  StatDensity2d = c("level", "nlevel", "piece", "density", "ndensity", "count"),
  StatContour = c("level", "nlevel", "piece", "level_low", "level_high", "level_mid"),
  StatSummary2d = c("value"),
  StatQuantile = c("quantile")
)

# the variables of any layer's data after its stat, besides the
# aesthetics
.fig_stat_any <- c("x", "y", "group", "PANEL", "flipped_aes", "xmin", "xmax", "ymin", "ymax")

# A mapping's values judged: a text neither a column's name nor of the
# form warns (it would be written as a column of that name); an
# after_stat()'s variable that the layer's stat does not compute warns.
# `args`, the item's (its stat, its geom).
.schema_check_aes_values <- function(fn, ent, args, bad) {
  mapping <- args$mapping
  if (!is.list(mapping) || is.null(names(mapping))) return(invisible())
  vars <- NULL
  for (k in names(mapping)) {
    v <- mapping[[k]]
    if (!is.character(v) || length(v) != 1L || is.object(v) || is.na(v)) next
    e <- .fig_aes_lang(v)
    if (is.null(e)) {
      bad("mapping", sprintf(paste0("the mapping's `%s: %s` is neither a column's name nor an ",
                                    "expression of a mapping's form (factor(), after_stat(), ",
                                    "reorder() ... of columns): written as a column of that name; ",
                                    "!r for other R"), k, v), "warning")
      next
    }
    used <- .fig_aes_stat_vars(e)
    if (!length(used)) next
    if (is.null(vars)) vars <- .schema_layer_stat_vars(fn, args, ent) %||% NA
    if (identical(vars, NA)) next
    for (u in setdiff(used, vars$known)) {
      bad("mapping", sprintf("the mapping's `%s: %s`: %s computes no variable `%s` (it computes: %s)",
                             k, v, vars$stat, u, paste(vars$own, collapse = ", ")), "warning")
    }
  }
  invisible()
}

# The variables the layer's stat computes and those it has besides (the
# aesthetics): list(stat, own, known), or NULL when the stat is not known
.schema_layer_stat_vars <- function(fn, args, ent) {
  l <- .schema_layer_obj(fn, args)
  if (is.null(l)) return(NULL)
  cls <- class(l$stat)[1L]
  own <- .fig_stat_vars[[cls]]
  if (is.null(own)) return(NULL)
  aes <- unique(c(ent$aes %||% character(), l$geom$aesthetics(), l$stat$aesthetics()))
  list(stat = cls, own = own, known = unique(c(own, .fig_stat_any, aes)))
}
