# ---- a report's page: as its document calls ----------------------------------
#
# A report's `page:` (resolved: the study's defaults, its pattern and its
# own, tfl_resolve_output()) is the rtfreporter calls that dress its pages,
# each key one function and its arguments:
#
#   rtf_page, rtf_default_format      the page and the text defaults,
#                                     rtf_document()'s page and default_format
#   rtf_document                      font_table, watermark, program ...
#   rtf_tables / rtf_figures          the content's own arguments
#   rtf_titles / rtf_footnotes        their font size ...
#   header, footer, titles, footnotes the lines, by their number: each
#                                     {left, center, right}
#   tokens                            {NAME: value}, the text of {NAME}
#
# .page_report_steps() makes the same calls .report_spec_steps() makes from
# the report sheets: rtf_document() |> rtf_section() |> rtf_tables() (or
# rtf_figures()) |> rtf_titles() |> rtf_footnotes().

.page_report_steps <- function(output, study = NULL, content = "content",
                               setup = FALSE) {
  # what is the study's (its defaults) and what the report's (its pattern
  # and its own): the setup code says the study's once
  split <- if (setup) .page_split(output, study)
  if (!isTRUE(attr(output, "resolved")) && !is.null(study)) {
    output <- tfl_resolve_output(output, study)
  }
  pg <- output$page %||% list()
  id <- output$id %||% NA_character_
  kind <- output$kind %||% "table"
  tokens <- .page_tokens(pg, id, kind)
  args_of <- function(fn) .page_args(pg[[fn]])
  doc_a <- args_of("rtf_document")
  fmt <- args_of("rtf_default_format")
  for (w in intersect(c("title_width", "footnote_width"), names(fmt))) {
    v <- suppressWarnings(as.numeric(fmt[[w]]))
    if (!is.na(v)) fmt[[w]] <- v
  }
  page <- args_of("rtf_page")
  # (all of it for a figure's size: its font's lines too)
  fmt_all <- fmt
  doc_tokens <- tokens
  if (setup) {
    # the study's font and size, its tokens: the setup's options() say them
    if ("font" %in% split$page_study) doc_a$font_table <- NULL
    if ("font_size_half_points" %in% split$page_study) fmt$font_size_half_points <- NULL
    doc_tokens <- tokens[setdiff(names(tokens), split$tokens_study)]
    if (!length(doc_tokens)) doc_tokens <- NULL
  }
  # the program: the TOC's, else the study's (settings: program:
  # "{output_id}.R"), in the study's program folder; with none, the
  # report's ID is rtfreporter's last resort (program_fallback)
  entry <- .page_toc_entry(study, id)
  prog <- .ard_spec_fill_id(entry[["program"]] %||% study$study$settings$program, id)
  dir <- study$study$settings$program_dir %||% NA_character_
  in_dir <- function(p) {
    if (is.null(p) || is.na(dir)) return(p)
    sep <- if (grepl("\\", dir, fixed = TRUE)) "\\" else "/"
    paste(sub("[\\\\/]+$", "", dir), p, sep = sep)
  }
  # the RTF file the document is written to (tfl_report_path()), with a study
  file <- if (inherits(study, "tfl_study_spec")) .rtf_file_said(.output_rtf_path(output, study))
  doc <- .spec_sym("doc")
  st <- list(.spec_call("rtf_document",
                        font_table = doc_a$font_table,
                        page = if (length(page)) page,
                        default_format = if (length(fmt))
                          do.call(.spec_call, c(list("rtf_default_format"), fmt)),
                        watermark = doc_a$watermark,
                        program = in_dir(prog),
                        program_fallback = if (is.null(prog) && !is.na(id)) id,
                        tokens = doc_tokens,
                        file = file))
  hdr <- .page_section_band(pg, "header", tokens, split)
  ftr <- .page_section_band(pg, "footer", tokens, split)
  if (length(hdr) || length(ftr)) {
    st[[length(st) + 1L]] <- .spec_call("rtf_section", doc, header = hdr, footer = ftr)
  }
  tt <- .page_band(pg, "titles", tokens)
  fn <- .page_band(pg, "footnotes", tokens)
  if (identical(kind, "figure")) {
    fa <- args_of("rtf_figures")
    # its size on the page: inches as twips, or the page's body (fit)
    # worked out from the page; blank, the figure's own size
    r <- list(figure_width_in = .page_twips_in(fa$width_twips),
              figure_height_in = .page_twips_in(fa$height_twips))
    sz <- .fig_page_size(r, c(page, fmt_all), length(tt) + length(fn))
    twips <- function(v) as.integer(round(v * 1440))
    st[[length(st) + 1L]] <- .spec_call("rtf_figures", doc, .spec_sym(content),
      width_twips = if (sz$mode[["width"]] != "native") twips(sz$width_in),
      height_twips = if (sz$mode[["height"]] != "native") twips(sz$height_in))
  } else {
    ta <- args_of("rtf_tables")
    st[[length(st) + 1L]] <- do.call(.spec_call, c(list("rtf_tables", doc, .spec_sym(content)),
      ta[intersect(c("auto_section", "section_label_align", "auto_title",
                     "title_label_align", "font_size_half_points"), names(ta))]))
  }
  if (length(tt)) {
    st[[length(st) + 1L]] <- .spec_call("rtf_titles", doc, list(tt),
      font_size_half_points = args_of("rtf_titles")$font_size_half_points)
  }
  if (length(fn)) {
    st[[length(st) + 1L]] <- .spec_call("rtf_footnotes", doc, list(fn),
      font_size_half_points = args_of("rtf_footnotes")$font_size_half_points)
  }
  st
}

# The running header or footer as rtf_section() takes it: rtf_header(lines).
# After the study's setup code (`split`), a report without lines of its
# own says the study's by name (study_header); one whose own lines come
# before or after the study's, the study's by the name of their lines
# (study_header_lines) -- when they are the study's lines as the setup
# writes them.
.page_section_band <- function(pg, band, tokens, split = NULL) {
  b <- .page_band(pg, band, tokens)
  if (!length(b)) return(NULL)
  fn <- paste0("rtf_", band)
  if (is.null(split)) return(.spec_call(fn, b))
  nm <- .setup_band_names[[band]]
  study_lines <- split$bands_study[[band]]
  if (!isTRUE(split$bands_own[[band]]) && length(study_lines)) return(.spec_sym(nm))
  if (length(study_lines)) {
    st <- stats::setNames(list(study_lines), band)
    sb <- .page_band(st, band, tokens)
    n <- length(sb)
    if (n && length(b) > n && identical(sb, .page_band(st, band, NULL))) {
      lines <- .spec_sym(paste0(nm, "_lines"))
      if (identical(utils::tail(b, n), sb)) {
        return(.spec_call(fn, .spec_call("c", utils::head(b, -n), lines)))
      }
      if (identical(utils::head(b, n), sb)) {
        return(.spec_call(fn, .spec_call("c", lines, utils::tail(b, -n))))
      }
    }
  }
  .spec_call(fn, b)
}

# The study's and the report's parts of a report's page (the report as
# read, before its layers are put together): the study's is its defaults,
# the report's its pattern and its own.  `page_study`: the study's font
# and size the report does not say; `tokens_study`: the study's tokens the
# report does not say; `bands_own`: the report has lines of its own in its
# header or footer (an !unset line too); `bands_study`: the study's lines
# the report's do not replace.
.page_split <- function(output, study) {
  if (isTRUE(attr(output, "resolved")) || is.null(study)) {
    .ard_stop(paste0("tfl_report_code(setup = TRUE): give the report as read ",
                     "(tfl_read_output(), not resolved) and its `study`."))
  }
  chain <- output$extends
  if (identical(chain, "none")) chain <- character()
  study_pg <- if ("defaults" %in% chain) study$defaults$page %||% list() else list()
  layers <- c(lapply(setdiff(chain, "defaults"), function(nm) .layer_of(nm, study)$page),
              list(output$page))
  said <- function(key, sub = NULL) any(vapply(layers, function(l) {
    v <- l[[key]]
    if (!is.null(sub)) v <- if (.is_map(v)) v[[sub]] else NULL
    !is.null(v) && length(v) > 0L
  }, NA))
  font_said <- function(p) length(p$rtf_document$font_table %||% list()) > 0L
  size_said <- function(p) !is.null(p$rtf_default_format$font_size_half_points)
  page_study <- c(if (font_said(study_pg) && !said("rtf_document", "font_table")) "font",
                  if (size_said(study_pg) && !said("rtf_default_format", "font_size_half_points"))
                    "font_size_half_points")
  own_tokens <- unique(unlist(lapply(layers, function(l) names(l$tokens))))
  tokens_study <- setdiff(names(.page_args(study_pg$tokens)), own_tokens)
  bands_own <- c(header = said("header"), footer = said("footer"))
  bands_study <- lapply(c(header = "header", footer = "footer"), function(b) {
    own_keys <- unique(unlist(lapply(layers, function(l) names(l[[b]]))))
    m <- .page_args(study_pg[[b]])
    m[setdiff(names(m), own_keys)]
  })
  list(page_study = page_study, tokens_study = tokens_study,
       bands_own = bands_own, bands_study = bands_study)
}

# a function's arguments as page: says them: a map, unset ones gone
.page_args <- function(m) {
  if (is.null(m) || inherits(m, "tfl_unset") || !length(m)) return(list())
  Filter(function(v) !inherits(v, "tfl_unset") && !is.null(v), m)
}

# a figure's size as page: has it: twips back to inches, a word (fit) as is
.page_twips_in <- function(v) {
  if (is.null(v)) return(NULL)
  n <- suppressWarnings(as.numeric(v))
  if (is.na(n)) v else n / 1440
}

# the report's entry of the study's TOC
.page_toc_entry <- function(study, id) {
  for (e in study$toc$outputs %||% list()) if (identical(e$id, id)) return(e)
  list()
}

# The report's tokens, as rtf_document() takes them: page:'s, then the
# report's own (OUTPUT_ID, OUTPUT_LABEL) when its lines say them -- as
# .ard_spec_tokens() gives them from the tokens sheet.
.page_tokens <- function(pg, id, kind) {
  tk <- .page_args(pg$tokens)
  out <- lapply(tk, function(v) if (is.na(v)) "" else as.character(v))
  used <- unlist(lapply(c("header", "footer", "titles", "footnotes"), function(s)
    unlist(.page_args(pg[[s]]))))
  used <- unique(unlist(regmatches(used, gregexpr("(?<=\\{)[A-Z][A-Z0-9_]*(?=\\})", used,
                                                  perl = TRUE))))
  want <- intersect(.ard_spec_output_tokens, used)
  if (length(want)) {
    words <- .ard_spec_kind_words
    for (k in names(words)) {
      w <- out[[paste0("OUTPUT_KIND_", toupper(k))]]
      if (!is.null(w) && nzchar(w)) words[[k]] <- w
    }
    own <- list(OUTPUT_ID = if (is.na(id)) "" else id,
                OUTPUT_LABEL = .ard_spec_output_label(id, kind, words))
    for (k in setdiff(want, names(out))) out[[k]] <- own[[k]] %||% ""
    mine <- intersect(.ard_spec_output_tokens, names(out))
    out <- out[c(setdiff(names(out), mine), mine)]
  }
  if (!length(out)) return(NULL)
  out
}

# A band's lines in their order, each c(l =, c =, r =) as rtf_header() and
# rtf_titles() take them: a line its tokens leave empty is left out, a
# blank line is centred.
.page_band <- function(pg, band, tokens) {
  m <- .page_args(pg[[band]])
  if (!length(m)) return(NULL)
  m <- m[order(suppressWarnings(as.numeric(names(m))))]
  lines <- lapply(m, function(cells) {
    cells <- .page_args(cells)
    v <- c(l = cells$left %||% NA, c = cells$center %||% NA, r = cells$right %||% NA)
    v[!is.na(v)]
  })
  keep <- !vapply(lines, function(v) .ard_spec_empty_line(v, tokens), NA)
  lines <- lines[keep]
  if (!length(lines)) return(NULL)
  unname(lapply(lines, function(v) if (!length(v)) c(c = "") else v))
}
