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
  pg <- .page_place(output$page %||% list(), study$study$settings, output$id)
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
    if ("font_lang" %in% split$page_study) doc_a$font_lang <- NULL
    if ("font_size_half_points" %in% split$page_study) fmt$font_size_half_points <- NULL
    doc_tokens <- tokens[setdiff(names(tokens), split$tokens_study)]
    if (!length(doc_tokens)) doc_tokens <- NULL
  }
  # the program: the TOC's, else the study's (settings: program:
  # "{output_id}.R"), in the study's program folder; with none, the
  # report's ID is rtfreporter's last resort (program_fallback)
  entry <- .page_toc_entry(study, id)
  prog <- .ard_spec_fill_id(entry[["program"]] %||% study$study$settings[["program"]], id)
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
                        font_table = .font_table_value(doc_a$font_table),
                        font_lang = .font_slots_value(doc_a$font_lang),
                        page = if (length(page)) page,
                        default_format = if (length(fmt))
                          do.call(.spec_call, c(list("rtf_default_format"), fmt)),
                        watermark = doc_a$watermark,
                        program = in_dir(prog),
                        program_fallback = if (is.null(prog) && !is.na(id)) in_dir(id),
                        tokens = doc_tokens,
                        file = file))
  hdr <- .page_section_band(pg, "header", tokens, split)
  ftr <- .page_section_band(pg, "footer", tokens, split)
  if (length(hdr) || length(ftr)) {
    st[[length(st) + 1L]] <- .spec_call("rtf_section", doc, header = hdr, footer = ftr)
  }
  tt <- .page_band(pg, "titles", tokens)
  fn <- .page_band(pg, "footnotes", tokens)
  flow <- .page_flow_args
  if (identical(kind, "figure")) {
    fa <- args_of("rtf_figures")
    # (the report sheet says page_break, space_before for both kinds: under
    # rtf_tables; the figure's own win)
    ft <- args_of("rtf_tables")
    fa <- c(fa, ft[setdiff(intersect(c("page_break", "space_before"), names(ft)), names(fa))])
    # a figure of items: its image is its tfl_image item (R/fig_content.R)
    img <- if (.fig_new_content(output$content)) .fig_split_content(output$content)$image
    # its size on the page: inches as twips, or the page's body (fit)
    # worked out from the page; blank, the figure's own size
    sz <- .fig_image_size(img, pg, length(tt) + length(fn))
    twips <- function(v) as.integer(round(v * 1440))
    if (!is.null(img)) {
      # drawn at its size on the page, at its resolution: what the page
      # shows is the plot as drawn, never stretched into the box
      fig <- .spec_call("rtfplot", .spec_sym(content),
                        render_width = sz$width_in, render_height = sz$height_in,
                        render_dpi = as.numeric(img$dpi %||% 300))
      st[[length(st) + 1L]] <- do.call(.spec_call, c(list("rtf_figures", doc, fig, align = fa$align), flow(fa)))
    } else {
      st[[length(st) + 1L]] <- do.call(.spec_call, c(list("rtf_figures", doc, .spec_sym(content),
        width_twips = if (sz$mode[["width"]] != "native") twips(sz$width_in),
        height_twips = if (sz$mode[["height"]] != "native") twips(sz$height_in),
        align = fa$align), flow(fa)))
    }
  } else {
    ta <- args_of("rtf_tables")
    st[[length(st) + 1L]] <- do.call(.spec_call, c(list("rtf_tables", doc, .spec_sym(content)),
      ta[intersect(c("auto_section", "section_label_align", "auto_title",
                     "title_label_align", "font_size_half_points"), names(ta))], flow(ta)))
  }
  # the titles' and footnotes' own style: rtf_titles() / rtf_footnotes()'s
  # arguments as the page says them
  band_style <- c("font_size_half_points", "row_height_twips", "markup", "font", "align")
  if (length(tt)) {
    a <- args_of("rtf_titles")
    if (!is.null(a[["font"]])) a[["font"]] <- .font_slots_value(a[["font"]])
    st[[length(st) + 1L]] <- do.call(.spec_call, c(list("rtf_titles", doc, list(tt)),
                                                   a[intersect(band_style, names(a))]))
  }
  if (length(fn)) {
    a <- args_of("rtf_footnotes")
    if (!is.null(a[["font"]])) a[["font"]] <- .font_slots_value(a[["font"]])
    st[[length(st) + 1L]] <- do.call(.spec_call, c(list("rtf_footnotes", doc, list(fn)),
                                                   a[intersect(band_style, names(a))]))
  }
  st
}

# Where a report's titles and footnotes print: the study's settings
# `titles_at: header | titles` and `footnotes_at: footer | footnotes`
# (decided 10/17).  The words are the report's, in its `page.titles` /
# `page.footnotes`; `header` puts the titles' lines in the running header
# after the study's lines, `footer` the footnotes' lines in the running
# footer before them; `titles` / `footnotes` (and no setting) keep them
# above and below the table (rtf_titles(), rtf_footnotes()).
.page_place <- function(pg, settings, id = NA_character_) {
  at <- function(key, ok) {
    v <- settings[[key]]
    if (is.null(v)) return(NULL)
    if (!is.character(v) || length(v) != 1L || !v %in% ok) {
      .ard_stop(sprintf("study.yml's settings: %s is %s.", key,
                        paste(sprintf("\"%s\"", ok), collapse = " or ")))
    }
    v
  }
  move <- function(pg, from, to, after) {
    lines <- .page_args(pg[[from]])
    if (!length(lines)) return(pg)
    lines <- lines[order(suppressWarnings(as.numeric(names(lines))))]
    band <- .page_args(pg[[to]])
    num <- suppressWarnings(as.numeric(names(band)))
    num <- num[!is.na(num)]
    keys <- if (after) max(c(0, num)) + seq_along(lines)
            else min(c(1, num)) - rev(seq_along(lines)) / (length(lines) + 1)
    pg[[to]] <- c(band, stats::setNames(unname(lines), format(keys, trim = TRUE)))
    pg[[from]] <- NULL
    pg
  }
  if (identical(at("titles_at", c("header", "titles")), "header")) {
    pg <- move(pg, "titles", "header", after = TRUE)
  }
  if (identical(at("footnotes_at", c("footer", "footnotes")), "footer")) {
    pg <- move(pg, "footnotes", "footer", after = FALSE)
  }
  pg
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
  lang_said <- function(p) length(p$rtf_document$font_lang %||% list()) > 0L
  size_said <- function(p) !is.null(p$rtf_default_format$font_size_half_points)
  page_study <- c(if (font_said(study_pg) && !said("rtf_document", "font_table")) "font",
                  if (lang_said(study_pg) && !said("rtf_document", "font_lang")) "font_lang",
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

# A content's place on the page, as rtfreporter's rtf_tables() /
# rtf_figures() take it: on the page of the content before it
# (page_break = FALSE), the empty lines between (space_before), a table's
# rows on that page (first_rows) and the title of the pages after
# (cont_title).  `a` a map of arguments (a report's page.rtf_tables, or one
# content's place in a report of several); only these kept, and only when
# not a new page (the default, said by nothing).
.page_flow_args <- function(a) {
  a <- a[intersect(c("page_break", "space_before", "first_rows", "cont_title"), names(a))]
  if (!isFALSE(a$page_break)) a$page_break <- NULL
  if (!isFALSE(a$page_break) || identical(as.integer(a$space_before), 0L)) a$space_before <- NULL
  a
}

# A font as rtfreporter takes it (its Fonts per script): one family, or a
# family per script slot (latin, east_asian, complex, hansi) -- YAML's map
# a named character vector; a font table's entries each so; font_lang the
# same, a language per slot.  NULL stays NULL.
.font_slots_value <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  if (is.list(x) && !is.null(names(x))) return(unlist(x))
  x
}
.font_table_value <- function(ft) {
  if (is.null(ft) || !length(ft) || !is.list(ft)) return(ft)
  lapply(ft, function(f) {
    if (is.list(f) && !is.null(f$name)) f$name <- .font_slots_value(f$name)
    f
  })
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

# A figure's size on the page (inches): its tfl_image's width_in /
# height_in (a number, or fit: the page's body), else the page's
# rtf_figures (twips), else the figure's own; `page` the report's page:
# (resolved), `lines` its titles' and footnotes' lines
.fig_image_size <- function(img, page, lines = 0L) {
  fa <- .page_args(page$rtf_figures)
  pg <- c(.page_args(page$rtf_page), .page_args(page$rtf_default_format))
  r <- list(figure_width_in = img$width_in %||% .page_twips_in(fa$width_twips),
            figure_height_in = img$height_in %||% .page_twips_in(fa$height_twips))
  .fig_page_size(r, pg, lines)
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
