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

# The line numbers the sheets give a configuration's band lines (the inverse
# of how the converter splits the default rows): the `first` lines keep their
# own numbers; the `last` lines keep theirs when all are above the first lines'
# and above `rep_max` (the highest of the reports' own lines), else they are
# numbered above those, in order.  list(first = , last = ): the number by the
# line's key.
.cfg_numbers <- function(blocks, rep_max = 0) {
  num <- function(x) suppressWarnings(as.numeric(x))
  first <- as.character(names(.page_sorted(blocks$first %||% list())))
  last <- as.character(names(.page_sorted(blocks$last %||% list())))
  top <- max(c(num(first), rep_max, 0), na.rm = TRUE)
  keep <- length(last) && !anyNA(num(last)) && min(num(last)) > top
  last_n <- if (keep || !length(last)) last else as.character(as.integer(top + seq_along(last)))
  list(first = stats::setNames(as.list(first), first), last = stats::setNames(as.list(last_n), last))
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
    body <- Filter(Negate(is.null), lapply(.page_sorted(ob$body), .page_line_drop))
    lines <- c(unname(.page_sorted(first)), unname(body), unname(.page_sorted(last)))
    if (length(lines)) out[[b]] <- stats::setNames(lines, seq_along(lines))
  }
  out
}

# ---- the layers of a report's page ---------------------------------------------
#
# The page step layers as the other steps do (PRINCIPLES 2, 11-28): the
# study's page settings (the set `style:` chooses) < the template's `page:`
# (the defaults, the report's shape) < the report's `page:` < force.  One
# layer over another changes only what it writes: a call's arguments
# (rtf_page, rtf_style ...) and `tokens` by name, a band by its blocks and
# its lines by number, a line by its cells.  An unnamed sequence of lines
# replaces the body of that band (or that block) on purpose.  `~` says
# nothing; `!unset` stays a marker over the layers, so it can still drop a
# cell, a line, a block or a band of a layer below: .page_resolve(), the
# final step over the page settings, takes the markers out.

# A page layer over the page below it (template < report, report < force)
.page_over <- function(base, over) {
  if (inherits(over, "tfl_unset")) return(over)
  if (is.null(over) || !is.list(over) || !length(over)) return(base)
  if (is.null(base) || inherits(base, "tfl_unset") || !is.list(base) || !length(base)) return(over)
  base <- .page_style_norm(base)
  over <- .page_style_norm(over)
  for (k in names(over)) {
    v <- over[[k]]
    if (is.null(v)) next
    base[k] <- list(if (k %in% .page_bands) .page_band_over(base[[k]], v) else .page_keep_over(base[[k]], v))
  }
  base
}

# A map over a map by name, recursively, its `!unset` kept as a marker (not
# a removed key: the layer below the base may still have the key); a
# value that is not a map replaces
.page_keep_over <- function(base, over) {
  if (is.null(over)) return(base)
  if (!.is_map(over) || !.is_map(base)) return(over)
  for (k in names(over)) {
    v <- over[[k]]
    if (is.null(v)) next
    base[k] <- list(.page_keep_over(base[[k]], v))
  }
  base
}

# A band (or one block of a band) over the one below: its blocks `first`
# and `last` one by one, its lines by number, a line by its cells; an
# unnamed sequence replaces the numbered lines (the blocks stay)
.page_band_over <- function(base, over, blocks = TRUE) {
  if (inherits(over, "tfl_unset") || !is.list(over)) return(over)
  if (is.null(base) || inherits(base, "tfl_unset") || !is.list(base)) return(over)
  base <- .page_lines_norm(base)
  if (is.null(names(over))) {
    keep <- if (blocks) base[intersect(names(base), .page_blocks)] else list()
    return(c(keep, .page_lines_norm(over)))
  }
  for (k in names(over)) {
    v <- over[[k]]
    if (is.null(v)) next
    base[k] <- list(if (blocks && k %in% .page_blocks) {
      .page_band_over(base[[k]], v, blocks = FALSE)
    } else .page_keep_over(base[[k]], v))
  }
  base
}

# A body line with its `!unset` cells taken out; a line left with no cell
# is no line
.page_line_drop <- function(line) {
  if (!.is_map(line)) return(line)
  line <- .drop_unset(line)
  if (length(line)) line
}

# The page layers of a report as read: `style` (the page settings of its
# set), `template` (the template's `page:`, its "{name}" filled; NULL for a
# report on no template, or on a registry template, which writes no page)
# and `report` (its own `page:`)
.page_layers_of <- function(output, study) {
  ts <- .tpl_spec_of(output)
  tpl <- if (!is.null(ts) && grepl("/", ts$name, fixed = TRUE)) {
    # (its code lists' references to the study's read first, as
    # tfl_resolve_output() reads them before the template)
    output$codelists <- .codelists_refs(output$codelists, study$codelists, output$id,
                                        "tfl_page_origins()")
    attr(tfl_template_resolve(.output_to_list(output), study = .tr_study_of(study)), "page_template")
  }
  list(style = .report_style(output, study)$set$page %||% list(), template = tpl,
       report = output$page)
}

#' Where each field of a report's page comes from
#'
#' `tfl_page_origins()` resolves a report's page as [tfl_resolve_output()]
#' does -- the study's page settings (the set `style:` chooses) < the
#' template's `page:` < the report's `page:` -- and says, for each resolved
#' field, which layer gave it: a call's argument (`rtf_page`
#' `margin_left_in`), a token, or a cell of a band's line.  A band's lines
#' are numbered as the resolved page numbers them: the page settings'
#' `first` lines, the body (the template's and the report's lines), the
#' page settings' `last` lines.
#'
#' @param output A `tfl_output` as read from its file (not resolved).
#' @param study A `tfl_study_spec`.
#' @return A data frame, one row a resolved field: `key` (a band, a
#'   setting function or `tokens`), `block` (`"first"`, `"body"` or
#'   `"last"` for a band's line, else `NA`), `line` (the line's number in
#'   the resolved band, else `NA`), `name` (the cell, `left` / `center` /
#'   `right`, or the argument or token name), `value` (as text) and
#'   `origin` (`"style"`, `"template"`, `"report"`, or `"study"` for a token
#'   of study.yml).
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' s <- tfl_read_study_spec(d)
#' tfl_page_origins(tfl_read_outputs(d)[["T-14-1-1"]], s)
#' @export
tfl_page_origins <- function(output, study) {
  if (!inherits(output, "tfl_output")) stop("tfl_page_origins(): `output` is a tfl_output.", call. = FALSE)
  if (!inherits(study, "tfl_study_spec")) stop("tfl_page_origins(): `study` is a tfl_study_spec.", call. = FALSE)
  if (isTRUE(attr(output, "resolved"))) {
    stop("tfl_page_origins(): give the report as read (tfl_read_output(), not resolved).", call. = FALSE)
  }
  lay <- .page_layers_of(output, study)
  tokens <- .study_tokens(study)
  run <- function(f) {
    .page_resolve(f(lay$style, "style"), f(tokens, "study", map = TRUE),
                  .page_over(f(lay$template, "template"), f(lay$report, "report")))
  }
  val <- run(function(x, lab, map = FALSE) x)
  org <- run(function(x, lab, map = FALSE) .page_tag(x, lab, map = map))
  blk <- run(function(x, lab, map = FALSE) .page_tag(x, NA_character_, blocks = TRUE, map = map))
  rows <- list()
  walk <- function(v, o, b, path) {
    if (.is_map(v)) {
      for (k in names(v)) walk(v[[k]], o[[k]], b[[k]], c(path, k))
      return(invisible())
    }
    band <- path[1L] %in% .page_bands
    rows[[length(rows) + 1L]] <<- data.frame(
      key = path[1L], block = if (band) as.character(b) else NA_character_,
      line = if (band) as.integer(path[2L]) else NA_integer_,
      name = if (band) path[3L] %||% NA_character_ else paste(path[-1L], collapse = "."),
      value = .page_value_text(v), origin = as.character(o), stringsAsFactors = FALSE)
  }
  for (k in names(val)) walk(val[[k]], org[[k]], blk[[k]], k)
  if (!length(rows)) {
    return(data.frame(key = character(), block = character(), line = integer(), name = character(),
                      value = character(), origin = character(), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# A page layer with each value replaced by `lab` (its maps, blocks and lines
# kept, its `~` and `!unset` as they are), so the same resolve that puts
# the values in place puts the labels there.  `blocks = TRUE`: a band's
# value is labelled by its block (`first`, `last`, the lines by number
# `body`).  `map = TRUE`: the layer is one map (the tokens).
.page_tag <- function(page, lab, blocks = FALSE, map = FALSE) {
  leaf <- function(v, l = lab) if (is.null(v) || inherits(v, "tfl_unset")) v else l
  tag_map <- function(x, l = lab) {
    if (!.is_map(x)) return(leaf(x, l))
    x[] <- lapply(x, tag_map, l = l)
    x
  }
  tag_lines <- function(x, l) {
    if (!is.list(x) || inherits(x, "tfl_unset")) return(leaf(x, l))
    x[] <- lapply(x, tag_map, l = l)
    x
  }
  tag_band <- function(x) {
    if (!is.list(x) || inherits(x, "tfl_unset")) return(leaf(x))
    nm <- names(x) %||% rep("", length(x))
    x[] <- lapply(seq_along(x), function(i) {
      is_blk <- nm[i] %in% .page_blocks
      l <- if (blocks) if (is_blk) nm[i] else "body" else lab
      if (is_blk) tag_lines(x[[i]], l) else tag_map(x[[i]], l)
    })
    x
  }
  if (is.null(page) || inherits(page, "tfl_unset") || !is.list(page)) return(page)
  if (map) return(tag_map(page))
  for (k in names(page)) {
    page[k] <- list(if (k %in% .page_bands) tag_band(page[[k]]) else tag_map(page[[k]]))
  }
  page
}

# A page value as text (a number, a word, a call)
.page_value_text <- function(v) {
  if (is.atomic(v) && !is.object(v)) return(paste(as.character(v), collapse = ", "))
  paste(deparse(v), collapse = " ")
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
