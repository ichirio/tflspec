# ---- the study's page configurations ------------------------------------------
#
# A page configuration is a style set (defaults.yml `styles:`, a report's
# `style:`).  Its `page:` half is the report's whole page for the page step:
# every setting function (rtf_page, rtf_style, rtf_document, rtf_tables,
# rtf_figures, rtf_titles, rtf_footnotes, rtf_header, rtf_footer) and the
# fixed lines of the four bands (header, footer, titles, footnotes) in two
# blocks, `first` and `last` (plain lines are `first`).  Its `content:` half
# is optional: the table look that goes with it, a layer of the content
# step over defaults.yml's `content:`.
#
# The page step reads its configuration and nothing else from the study:
# configuration < template < report < force.  A band prints three blocks:
# the configuration's `first` lines, the body (the template's and the
# report's lines), the configuration's `last` lines.

.page_bands <- c("header", "footer", "titles", "footnotes")
.page_blocks <- c("first", "last")
.page_setting_keys <- c("rtf_page", "rtf_style", "rtf_document", "rtf_tables", "rtf_figures",
                        "rtf_titles", "rtf_footnotes", "rtf_header", "rtf_footer")
.page_config_keys <- c(.page_setting_keys, .page_bands)

# The built-in configurations tflspec ships; a study's set may not take one's
# name (.style_names_check())
.page_builtin_note <- function(name) {
  sprintf("%s is a built-in: copy it and save it under your own name.", name)
}

# A band's lines as a map by line number: a sequence is numbered from 1
.page_lines_norm <- function(x) {
  if (is.null(x) || inherits(x, "tfl_unset") || !is.list(x) || !length(x)) return(list())
  if (is.null(names(x))) names(x) <- as.character(seq_along(x))
  x
}

# A configuration's band as its blocks: plain lines are `first`; a map with
# `first` and / or `last` holds the blocks.
.page_config_band <- function(x, band, who) {
  if (is.null(x) || inherits(x, "tfl_unset")) return(NULL)
  if (!is.list(x)) {
    stop(sprintf("%s: page: %s is lines (by number) or a map of the blocks first and last.",
                 who, band), call. = FALSE)
  }
  nm <- names(x)
  if (length(nm) && all(nm %in% .page_blocks)) {
    out <- lapply(x, .page_lines_norm)
    return(out[vapply(out, length, 1L) > 0L])
  }
  if (length(nm) && any(nm %in% .page_blocks)) {
    stop(sprintf("%s: page: %s mixes numbered lines and the blocks first / last; a configuration's lines are its blocks.",
                 who, band), call. = FALSE)
  }
  lines <- .page_lines_norm(x)
  if (length(lines)) list(first = lines) else NULL
}

# A style set as read: only `page:` and `content:`; its page keys a closed
# list; its bands in blocks.
.style_set_from_list <- function(y, name) {
  who <- sprintf("defaults.yml styles: %s", name)
  if (is.null(y)) y <- list()
  if (!is.list(y)) stop(who, ": a page configuration is a map (page:, content:).", call. = FALSE)
  bad <- setdiff(names(y), c("page", "content"))
  if (length(bad)) {
    stop(sprintf("%s: unknown key %s; a page configuration holds page: and content: only.",
                 who, paste(sQuote(bad, FALSE), collapse = ", ")), call. = FALSE)
  }
  l <- .layer_from_list(y, paste0("style:", name))
  l$page <- .page_config_check(l$page, who)
  l
}

.page_config_check <- function(pg, who) {
  if (is.null(pg)) return(list())
  if (!is.list(pg)) stop(who, ": page: is a map of the page's keys.", call. = FALSE)
  pg <- .page_style_norm(pg)
  if ("tokens" %in% names(pg)) {
    stop(sprintf("%s: page: tokens moved to study.yml `tokens:` (a token's text is a study value, not a layout).",
                 who), call. = FALSE)
  }
  bad <- setdiff(names(pg), .page_config_keys)
  if (length(bad)) {
    stop(sprintf("%s: page: has no key %s; a page configuration holds %s.", who,
                 paste(sQuote(bad, FALSE), collapse = ", "),
                 paste(.page_config_keys, collapse = ", ")), call. = FALSE)
  }
  for (b in intersect(.page_bands, names(pg))) pg[[b]] <- .page_config_band(pg[[b]], b, who)
  pg
}

# The study's tokens (study.yml `tokens:`), a map NAME -> text
.study_tokens <- function(study) {
  tk <- study$study$tokens
  if (is.null(tk) || !length(tk)) return(list())
  if (!.is_map(tk)) stop("study.yml: tokens: is a map ({NAME: text}).", call. = FALSE)
  .page_args(tk)
}

# The configurations the setup code writes a header and footer for: the
# study's own sets, and the default one (a built-in one too)
.setup_style_names <- function(study) unique(c(names(study$styles), .style_default_name(study)))

# The names of the style sets: the study's, then the built-in ones
.style_names <- function(study) {
  unique(c(names(study$styles), .tpl_style_builtin_names))
}

# The name of the study's default configuration: study.yml settings.style,
# else tfl_standard
.style_default_name <- function(study) {
  nm <- study$study$settings$style
  if (is.character(nm) && length(nm) == 1L && !is.na(nm) && nzchar(nm)) nm else "tfl_standard"
}

# A band's lines in order: a map by line number, sorted by the number
.page_sorted <- function(m) {
  m <- .page_args(m)
  if (!length(m)) return(m)
  m[order(suppressWarnings(as.numeric(names(m))))]
}

# A report's own band: the lines by number are the body; the keys `first` and
# `last` change the configuration's blocks line by line.
.page_own_band <- function(x) {
  out <- list(first = NULL, body = list(), last = NULL)
  if (is.null(x) || !is.list(x)) return(out)
  if (is.null(names(x))) x <- .page_lines_norm(x)
  nm <- names(x)
  for (blk in .page_blocks) {
    v <- x[[blk]]
    out[blk] <- list(if (inherits(v, "tfl_unset")) v else if (!is.null(v)) .page_lines_norm(v))
  }
  out$body <- .page_args(x[!nm %in% .page_blocks])
  out
}

# A report's page (the settings and tokens) over its configuration, and the
# bands' three blocks put in order -- the configuration's `first` lines, the
# body, its `last` lines -- as one numbered map: what every reader of a
# resolved report's `page:` reads.  `tokens`: the study's, under the
# report's.
.page_resolve <- function(cfg, tokens, own) {
  cfg <- cfg %||% list()
  own <- own %||% list()
  settings <- .merge_map(.page_style_norm(cfg[setdiff(names(cfg), .page_bands)]),
                         .page_style_norm(own[setdiff(names(own), c(.page_bands, "tokens"))]))
  out <- if (is.list(settings)) settings else list()
  tk <- .merge_map(as.list(tokens %||% list()), own$tokens)
  if (length(tk)) out$tokens <- tk
  for (b in .page_bands) {
    o <- own[[b]]
    if (inherits(o, "tfl_unset")) next
    blocks <- .page_config_band(cfg[[b]], b, "the page configuration") %||% list()
    first <- blocks$first %||% list()
    last <- blocks$last %||% list()
    ob <- .page_own_band(o)
    first <- if (inherits(ob$first, "tfl_unset")) list() else .merge_map(first, ob$first)
    last <- if (inherits(ob$last, "tfl_unset")) list() else .merge_map(last, ob$last)
    lines <- c(unname(.page_sorted(first)), unname(.page_sorted(ob$body)), unname(.page_sorted(last)))
    if (length(lines)) out[[b]] <- stats::setNames(lines, seq_along(lines))
  }
  out
}

# A style set's page as a resolved page: its blocks in order, with no
# report over it
.page_config_flat <- function(set) .page_resolve(set$page, NULL, NULL)

# The style set a report names (its `style:`, else the study's default,
# else tfl_standard): the study's own, else a built-in one
.style_of <- function(output, study) {
  nm <- output$style %||% .style_default_name(study)
  if (!is.character(nm) || length(nm) != 1L || is.na(nm)) {
    .ard_stop(sprintf("%s: `style` is one of the study's styles' names.", output$id))
  }
  st <- study$styles[[nm]] %||% if (nm %in% .tpl_style_builtin_names) .tpl_style_builtin_set(nm)
  if (is.null(st)) {
    .ard_stop(sprintf("%s: no style %s in defaults.yml's styles: or among the built-in sets (%s).",
                      output$id, sQuote(nm, FALSE), paste(.style_names(study), collapse = ", ")))
  }
  if (!identical(output$kind, "table")) st$content <- list()
  st
}

# The style set and its name a report's page comes from
.report_style <- function(output, study) {
  lb <- .tpl_layers_below(output, study)
  list(name = lb$style_name %||% output$style %||% .style_default_name(study),
       set = lb$style %||% .style_of(output, study))
}

# The study's default configuration's page (the one a report with no
# `style:` gets), as the blocks
.study_default_page <- function(study) {
  nm <- .style_default_name(study)
  st <- tryCatch(.tpl_style_set(nm, study), error = function(e) NULL)
  st$page %||% list()
}
