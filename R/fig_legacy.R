# ggplot2's spellings of before 3.5 / 4.0, in the code a SPEC is read from
# (tfl_args_from_r(), ggplot code read term by term): read as ggplot2 4.0
# writes them.  Only where ggplot2 itself takes the old spelling as the new
# one, so the figure draws the same:
#
#   a line's `size`                linewidth (the layers whose geom renames
#                                  it: geom_line, geom_hline, geom_errorbar,
#                                  geom_boxplot ...; not geom_bar / geom_col,
#                                  whose size ggplot2 4.0 drops)
#   element_line / _rect(size =)   linewidth
#   stat_summary(fun.y =)          fun (fun.ymin, fun.ymax: fun.min, fun.max)
#   aes(y = ..count..), stat(count) after_stat(count) (the whole value:
#                                  after_stat(count / sum(count)))
#   aes_string(x = "AVAL")         aes(x = AVAL), when every value is a text
#   geom_errorbarh(height =)       geom_errorbar(width =, orientation = "y")
#   guide_legend(title.position =, keywidth =, label.hjust = ...)
#                                  the guide's theme = theme(legend.title.position
#                                  =, legend.key.width = unit(), legend.text =
#                                  element_text(hjust =) ...)
#   guides(fill = FALSE), guide = FALSE   "none"
#   theme(legend.position = c(x, y))      "inside" and legend.position.inside
#   theme(legend.title.align =)    legend.title = element_text(hjust =)
#   expand_scale()                 expansion()
#
# What ggplot2 does not translate the same (guide_legend(ticks = FALSE),
# geom_bar(size =)) is left as written; the check says what it is now.

# `e` (a call) with the old spellings in it, at any depth, as ggplot2 4.0
# writes them.  rename_fn = FALSE keeps the function (tfl_args_from_r():
# the arguments of the function asked for).
.fig_lang_modern <- function(e, rename_fn = TRUE) {
  if (!is.call(e)) return(e)
  f <- .fig_deparse(e[[1L]])
  # a function written out, a formula, a quoted expression: as they are
  if (f %in% c("function", "~", "quote", "bquote", "expression")) return(e)
  if (length(e) > 1L) {
    # (an empty argument, km[km$censored, ], is no value to look into)
    for (i in 2:length(e)) {
      if (is.call(e[[i]])) e[[i]] <- .fig_lang_modern(e[[i]], TRUE)
    }
  }
  fb <- sub("^ggplot2:::?", "", f)
  if (identical(fb, "aes_string")) e <- .fig_legacy_aes_string(e)
  fb <- sub("^ggplot2:::?", "", .fig_deparse(e[[1L]]))
  if (identical(fb, "aes")) return(.fig_legacy_aes(e))
  if (identical(fb, "geom_errorbarh") && rename_fn) {
    e <- .fig_legacy_errorbarh(e)
    fb <- "geom_errorbar"
  }
  if (grepl("^(geom|stat)_", fb)) e <- .fig_legacy_layer(e, fb)
  if (fb %in% .fig_legacy_guides) e <- .fig_legacy_guide(e)
  if (identical(fb, "guides")) e <- .fig_legacy_false_none(e, NULL)
  if (startsWith(fb, "scale_")) e <- .fig_legacy_false_none(e, "guide")
  if (identical(fb, "theme")) e <- .fig_legacy_theme(e)
  if (fb %in% c("element_line", "element_rect")) e <- .fig_legacy_rename(e, "size", "linewidth")
  if (identical(fb, "expand_scale")) e[[1L]] <- .fig_legacy_same_ns(e[[1L]], "expansion")
  e
}

# the guides whose arguments of before 3.5.0 are the guide's theme now
.fig_legacy_guides <- c("guide_legend", "guide_colourbar", "guide_colorbar", "guide_bins",
                        "guide_coloursteps", "guide_colorsteps")

# a call's argument names ("" for a positional one)
.fig_legacy_names <- function(e) {
  nm <- names(as.list(e)) %||% rep("", length(e))
  nm[-1L]
}

# the function `to`, by the namespace the call wrote (ggplot2::expand_scale:
# ggplot2::expansion)
.fig_legacy_same_ns <- function(head, to) {
  if (is.call(head) && length(head) == 3L) {
    head[[3L]] <- as.name(to)
    return(head)
  }
  as.name(to)
}

# an argument renamed, when the new name is not given too
.fig_legacy_rename <- function(e, from, to) {
  nm <- names(e)
  if (is.null(nm) || !from %in% nm || to %in% nm) return(e)
  names(e)[match(from, nm)] <- to
  e
}

# `FALSE` (a guide turned off before ggplot2 3.3.4) as "none": every
# argument (guides()), or the one named
.fig_legacy_false_none <- function(e, arg) {
  nm <- names(e) %||% rep("", length(e))
  for (i in seq_along(nm)[-1L]) {
    if (!nzchar(nm[i]) || (!is.null(arg) && !identical(nm[i], arg))) next
    if (isFALSE(e[[i]])) e[[i]] <- "none"
  }
  e
}

# a stat's variable by ggplot2's old spellings (..count.., stat(count))
.fig_legacy_dotted <- "^[.][.]([a-zA-Z._]+)[.][.]$"

.fig_legacy_has_stat <- function(x) {
  if (is.name(x)) return(grepl(.fig_legacy_dotted, as.character(x)))
  if (!is.call(x)) return(FALSE)
  if (identical(x[[1L]], as.name("stat")) && length(x) == 2L) return(TRUE)
  for (i in seq_along(x)[-1L]) {
    if ((is.call(x[[i]]) || (is.name(x[[i]]) && nzchar(as.character(x[[i]])))) &&
        .fig_legacy_has_stat(x[[i]])) return(TRUE)
  }
  FALSE
}

.fig_legacy_strip_stat <- function(x) {
  if (is.name(x)) {
    s <- as.character(x)
    return(if (grepl(.fig_legacy_dotted, s)) as.name(sub(.fig_legacy_dotted, "\\1", s)) else x)
  }
  if (!is.call(x)) return(x)
  if (identical(x[[1L]], as.name("stat")) && length(x) == 2L) return(.fig_legacy_strip_stat(x[[2L]]))
  for (i in seq_along(x)[-1L]) {
    if (is.call(x[[i]]) || (is.name(x[[i]]) && nzchar(as.character(x[[i]])))) {
      x[[i]] <- .fig_legacy_strip_stat(x[[i]])
    }
  }
  x
}

# aes(): a value with a stat's variable by its old spelling as
# after_stat() of the whole value, as ggplot2 evaluates it
.fig_legacy_aes <- function(e) {
  for (i in seq_along(e)[-1L]) {
    if (!(is.name(e[[i]]) || is.call(e[[i]])) || !.fig_legacy_has_stat(e[[i]])) next
    v <- e[[i]]
    inner <- is.call(v) && identical(v[[1L]], as.name("after_stat"))
    v <- .fig_legacy_strip_stat(v)
    e[[i]] <- if (inner) v else call("after_stat", v)
  }
  e
}

# aes_string(x = "AVISITN", y = "AVAL"): aes(x = AVISITN, y = AVAL) when
# each value is one text that is R; as it is otherwise (an object's value)
.fig_legacy_aes_string <- function(e) {
  a <- as.list(e)[-1L]
  if (!length(a)) return(e)
  nm <- names(a) %||% rep("", length(a))
  un <- which(!nzchar(nm))
  if (length(un) > 2L) return(e)
  nm[un] <- c("x", "y")[seq_along(un)]
  if (anyDuplicated(nm)) return(e)
  vals <- lapply(a, function(v) {
    if (!is.character(v) || length(v) != 1L || is.na(v)) return(NULL)
    tryCatch(str2lang(v), error = function(err) NULL)
  })
  if (any(vapply(vals, is.null, NA))) return(e)
  names(vals) <- nm
  as.call(c(list(.fig_legacy_same_ns(e[[1L]], "aes")), vals))
}

# geom_errorbarh(height =): geom_errorbar(width =, orientation = "y"), what
# ggplot2 4.0's geom_errorbarh() calls
.fig_legacy_errorbarh <- function(e) {
  e[[1L]] <- .fig_legacy_same_ns(e[[1L]], "geom_errorbar")
  e <- .fig_legacy_rename(e, "height", "width")
  if (!"orientation" %in% names(e)) e$orientation <- "y"
  e
}

# whether a layer's geom takes `size` as its linewidth (ggplot2's
# rename_size), the geom a stat is given by name (stat_summary(geom =
# "errorbar")) or its default
.fig_legacy_renames_size <- function(fb, geom = NULL) {
  key <- paste(fb, geom %||% "", sep = "|")
  memo <- .fig_registry$renames_size %||% list()
  if (!is.null(memo[[key]])) return(memo[[key]])
  fun <- if (requireNamespace("ggplot2", quietly = TRUE) && fb %in% getNamespaceExports("ggplot2")) {
    getExportedValue("ggplot2", fb)
  }
  out <- FALSE
  if (is.function(fun)) {
    args <- if (!is.null(geom) && "geom" %in% names(formals(fun))) list(geom = geom) else list()
    l <- tryCatch(suppressWarnings(suppressMessages(do.call(fun, args))), error = function(err) NULL)
    g <- if (inherits(l, "LayerInstance")) l$geom
    out <- !is.null(g) && isTRUE(g$rename_size) && !"size" %in% g$aesthetics()
  }
  memo[[key]] <- out
  .fig_registry$renames_size <- memo
  out
}

# a layer: a line's size as its linewidth; stat_summary()'s fun.y,
# fun.ymin, fun.ymax as fun, fun.min, fun.max
.fig_legacy_layer <- function(e, fb) {
  nm <- names(e) %||% rep("", length(e))
  if ("size" %in% nm && !"linewidth" %in% nm) {
    g <- e$geom
    g <- if (is.character(g) && length(g) == 1L) g
    map <- e$mapping %||% Find(function(v) is.call(v) && identical(.fig_deparse(v[[1L]]), "aes"),
                               as.list(e)[-1L][!nzchar(nm[-1L])])
    mapped <- if (is.call(map)) names(as.list(map)[-1L]) else character()
    if (!"linewidth" %in% mapped && .fig_legacy_renames_size(fb, g)) {
      e <- .fig_legacy_rename(e, "size", "linewidth")
    }
  }
  if (fb %in% c("stat_summary", "stat_summary_bin")) {
    e <- .fig_legacy_rename(e, "fun.y", "fun")
    e <- .fig_legacy_rename(e, "fun.ymin", "fun.min")
    e <- .fig_legacy_rename(e, "fun.ymax", "fun.max")
  }
  e
}

# an element (element_text(...) as a call) with `vals` added where it has
# none of its own; NULL when `el` is not an element_text() call
.fig_legacy_add_args <- function(el, vals, fn = "element_text") {
  if (is.null(el)) el <- call(fn)
  if (!is.call(el) || !identical(sub("^ggplot2:::?", "", .fig_deparse(el[[1L]])), fn)) return(NULL)
  for (k in names(vals)) {
    if (!is.null(vals[[k]]) && !k %in% names(el)) el[[k]] <- vals[[k]]
  }
  el
}

# a key's width or height: a number in the guide's default.unit (lines),
# a unit() as it is; NULL for any other (an object)
.fig_legacy_unit <- function(v, du) {
  if (is.numeric(v) && length(v) == 1L) return(call("unit", v, du))
  if (is.call(v) && sub("^(ggplot2|grid):::?", "", .fig_deparse(v[[1L]])) == "unit") return(v)
  NULL
}

# a guide's arguments of before 3.5.0 as its theme, as ggplot2's
# deprecated_guide_args() sets them (a setting the theme has wins); one it
# cannot write (an object where a number goes) is left as written
.fig_legacy_guide <- function(e) {
  nm <- names(e) %||% rep("", length(e))
  a <- stats::setNames(as.list(e), nm)
  old <- intersect(nm, names(.schema_guide_old_args))
  if (!length(old)) return(e)
  th <- a$theme
  if (!is.null(th) && !(is.call(th) && identical(sub("^ggplot2:::?", "", .fig_deparse(th[[1L]])), "theme"))) {
    return(e)
  }
  set <- if (is.null(th)) list() else as.list(th)[-1L]
  used <- character()
  put <- function(k, v, from) {
    if (is.null(v)) return(invisible())
    if (is.null(set[[k]])) set[[k]] <<- v
    used <<- c(used, from)
  }
  put("legend.title.position", a$title.position, "title.position")
  put("legend.text.position", a$label.position, "label.position")
  put("legend.byrow", a$byrow, "byrow")
  du <- if (is.character(a$default.unit) && length(a$default.unit) == 1L) a$default.unit else "lines"
  if (identical(du, "line")) du <- "lines"
  for (side in c("width", "height")) {
    from <- intersect(c(paste0("key", side), paste0("bar", side)), nm)
    if (!length(from)) next
    u <- .fig_legacy_unit(a[[from[1L]]], du)
    if (!is.null(u)) put(paste0("legend.key.", side), u, from)
  }
  if (isFALSE(a$label)) {
    put("legend.text", call("element_blank"), c("label", intersect(c("label.theme", "label.hjust", "label.vjust"), nm)))
  } else if (any(c("label.theme", "label.hjust", "label.vjust") %in% nm)) {
    el <- .fig_legacy_add_args(a$label.theme, list(hjust = a$label.hjust, vjust = a$label.vjust))
    if (!is.null(el)) put("legend.text", el, intersect(c("label.theme", "label.hjust", "label.vjust"), nm))
  }
  if (any(c("title.hjust", "title.vjust") %in% nm)) {
    el <- .fig_legacy_add_args(a$title.theme, list(hjust = a$title.hjust, vjust = a$title.vjust))
    if (!is.null(el)) put("legend.title", el, intersect(c("title.theme", "title.hjust", "title.vjust"), nm))
  } else if ("title.theme" %in% nm) {
    put("legend.title", a$title.theme, "title.theme")
  }
  fr <- intersect(c("frame.colour", "frame.linewidth", "frame.linetype"), nm)
  if (length(fr)) {
    put("legend.frame", as.call(c(list(as.name("element_rect")),
                                  stats::setNames(a[fr], sub("^frame[.]", "", fr)))), fr)
  }
  tk <- intersect(c("ticks.colour", "ticks.linewidth"), nm)
  if (length(tk) && !isFALSE(a$ticks)) {
    put("legend.ticks", as.call(c(list(as.name("element_line")),
                                  stats::setNames(a[tk], sub("^ticks[.]", "", tk)))), tk)
  }
  ax <- intersect(c("axis.colour", "axis.linewidth", "axis.arrow"), nm)
  if (length(ax) && !isFALSE(a$axis)) {
    put("legend.axis.line", as.call(c(list(as.name("element_line")),
                                      stats::setNames(a[ax], sub("^axis[.]", "", ax)))), ax)
  }
  if (!length(used)) return(e)
  if (any(grepl("^(key|bar)(width|height)$", used))) used <- c(used, intersect("default.unit", nm))
  # the theme where the guide had it, else where its first old argument was
  at <- if ("theme" %in% nm) match("theme", nm) else min(match(used, nm))
  newth <- as.call(c(list(if (is.null(th)) as.name("theme") else th[[1L]]), set))
  keep <- !nm %in% c(used, "theme")
  keep[1L] <- TRUE
  out <- as.list(e)
  pos <- seq_along(out)
  before <- out[keep & pos < at]
  after <- out[keep & pos > at]
  as.call(c(before, list(theme = newth), after))
}

# theme(): a numeric legend.position as "inside" with its place in
# legend.position.inside; legend.title.align / legend.text.align as the
# element's hjust
.fig_legacy_theme <- function(e) {
  nm <- names(e) %||% rep("", length(e))
  lp <- e$legend.position
  num <- function(v) is.numeric(v) && length(v) == 2L
  if (is.call(lp) && identical(lp[[1L]], as.name("c")) && length(lp) == 3L &&
      all(vapply(as.list(lp)[-1L], function(x) is.numeric(x) && length(x) == 1L, NA))) {
    lp <- as.numeric(c(lp[[2L]], lp[[3L]]))
  }
  if (num(lp)) {
    i <- match("legend.position", nm)
    pos <- e[[i]]
    e[[i]] <- "inside"
    out <- as.list(e)
    ins <- match("legend.position.inside", names(out))
    if (!is.na(ins)) {
      out[[ins]] <- pos
    } else {
      out <- c(out[seq_len(i)], list(legend.position.inside = pos), out[-seq_len(i)])
    }
    e <- as.call(out)
  }
  for (part in c("title", "text")) {
    old <- paste0("legend.", part, ".align")
    k <- paste0("legend.", part)
    nm <- names(e) %||% rep("", length(e))
    if (!old %in% nm) next
    v <- e[[old]]
    el <- .fig_legacy_add_args(e[[k]], list(hjust = v))
    if (is.null(el) || (!is.null(e[[k]]) && "hjust" %in% names(e[[k]]))) next
    i <- match(old, nm)
    if (k %in% nm) {
      e[[k]] <- el
      e[[old]] <- NULL
    } else {
      names(e)[i] <- k
      e[[i]] <- el
    }
  }
  e
}

# the check's word for a layer's argument of before ggplot2 3.4 / 3.5
# (geom_line(size =)), or NULL
.fig_legacy_arg_text <- function(fn, a) {
  fb <- .schema_fn_name(fn)
  if (identical(a, "size") && grepl("^(geom|stat)_", fb) && .fig_legacy_renames_size(fb)) {
    return(sprintf("%s(size = ) is the line's width: linewidth since ggplot2 3.4.0 (write linewidth)", fb))
  }
  if (identical(a, "size") && fb %in% c("geom_bar", "geom_col")) {
    return(sprintf(paste0("%s(size = ) is the bars' outline width: linewidth since ggplot2 3.4.0 ",
                          "(ggplot2 4.0 ignores size; write linewidth)"), fb))
  }
  if (identical(fb, "theme") && a %in% c("legend.title.align", "legend.text.align")) {
    return(sprintf(paste0("theme(%s = ) is deprecated since ggplot2 3.5.0: write %s = {fn: element_text, ",
                          "args: {hjust: ...}}"), a, sub("[.]align$", "", a)))
  }
  NULL
}
