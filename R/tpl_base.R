# PROTOTYPE (2026-11-17, canon d399dac / 8b37892): a report template that
# names a base template of its series and writes only what it changes.
#
#   template: tflspec_dm
#   base: tflspec_summary
#   label: Demographics
#   params:
#     continuous: {suggest: [AGE, ...]}   # merged into the base's param
#
# The user decides later whether this form stays (canon 1.11 and 11-12 say a
# template holds its full set of defaults and names no other template).
# Everything is here and in two calls (.template_read(),
# tfl_template_resolve()), so it is easy to drop.
#
# Merge rules (one level only):
#   params:            a base param, key by key (`default:`, `suggest:`, ...);
#                      a new param after the base's
#   data:, analysis:   rows by id, key by key; a new id after the base's
#   any other key      the report template's replaces the base's (label,
#                      extends, style, content, page, ...)
# A base that has a base, or a template naming itself, stops.

.tpl_base_merge <- function(y, template, read) {
  base <- y$base
  if (is.null(base)) return(y)
  who <- sprintf("template %s", template)
  if (!is.character(base) || length(base) != 1L || !grepl("^[a-z][a-z0-9_]*$", base)) {
    stop(who, ": base: is the name of a template of the same series.", call. = FALSE)
  }
  series <- sub("/.*$", "", template)
  if (identical(base, sub("^.*/", "", template))) {
    stop(who, ": base: names the template itself.", call. = FALSE)
  }
  b <- read(.template_file(paste0(series, "/", base)))
  if (!is.null(b$base)) {
    stop(who, ": its base ", base, " has a base too (base: is one level only).", call. = FALSE)
  }
  out <- b
  for (k in setdiff(names(y), c("params", "data", "analysis", "base"))) out[k] <- list(y[[k]])
  for (p in names(y$params)) {
    if (p %in% names(out$params)) {
      for (k in names(y$params[[p]])) out$params[[p]][k] <- list(y$params[[p]][[k]])
    } else {
      out$params[p] <- list(y$params[[p]])
    }
  }
  out$data <- .tpl_base_rows(b$data, y$data)
  out$analysis <- .tpl_base_rows(b$analysis, y$analysis)
  # which template set each param's default: the report template's own
  # `default:`, else the base's
  own <- vapply(names(out$params), function(p) "default" %in% names(y$params[[p]]), NA)
  attr(out, "base") <- paste0(series, "/", base)
  attr(out, "set_by") <- stats::setNames(ifelse(own, template, paste0(series, "/", base)),
                                         names(out$params))
  out
}

# rows (data: or analysis:) by id, key by key; a new id after the base's
.tpl_base_rows <- function(rows, add) {
  for (r in add %||% list()) {
    ids <- vapply(rows, function(e) e$id %||% NA_character_, "")
    k <- match(r$id %||% NA_character_, ids)
    if (is.na(k)) {
      rows <- c(rows, list(r))
    } else {
      for (key in names(r)) rows[[k]][key] <- list(r[[key]])
    }
  }
  rows
}
