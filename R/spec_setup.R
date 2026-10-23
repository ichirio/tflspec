# ---- a study's setup of its report programs, from its YAML spec ---------------
#
# tfl_report_setup_code(study) and tfl_report_tokens(output, study): what
# the sheets' versions write from the default rows of the page, tokens,
# header and footer sheets, written from the study's defaults
# (spec/defaults.yml, `page:`) and from a report of the YAML spec.  The
# lines are the ones a report's program refers to after the setup
# (tfl_report_code(output, setup = TRUE, study = ): study_header,
# study_header_lines ...), made the same way (.page_band()).

# The setup code of a study's spec, one element a line (none: the study's
# defaults say no font, size, tokens, header or footer)
.study_setup_code <- function(study) {
  pg <- study$defaults$page %||% list()
  out <- character()
  # the company's font and size, for every report
  opts <- list()
  ft <- .page_args(pg$rtf_document)$font_table
  if (length(ft)) {
    first <- if (is.list(ft) && length(ft) && is.list(ft[[1L]])) ft[[1L]]$name else ft[[1L]]
    # (one family, or a family per script slot: a named vector)
    first <- .font_slots_value(first)
    if (!is.null(first) && !is.na(first[1L]) && nzchar(first[1L])) {
      opts[[.page_options[["font"]]]] <- if (is.null(names(first))) as.character(first[1L]) else first
    }
  }
  # the language of each font slot (rtfreporter's font_lang)
  fl <- .font_slots_value(.page_args(pg$rtf_document)$font_lang)
  if (length(fl)) opts[["rtfreporter.font_lang"]] <- fl
  # rtfreporter's options the study says by name (defaults.yml's options:);
  # they win over the page's font and size
  for (k in names(study$options)) {
    v <- study$options[[k]]
    opts[[k]] <- if (is.list(v) && !is.null(names(v))) unlist(v) else v
  }
  sz <- .page_args(pg$rtf_default_format)$font_size_half_points
  if (!is.null(sz)) {
    n <- suppressWarnings(as.integer(sz))
    opts[[.page_options[["font_size_half_points"]]]] <- if (is.na(n)) sz else n
  }
  if (length(opts)) {
    out <- c(out, .spec_call_code(do.call(.spec_call, c(list("options"), opts)), 0L))
  }
  # the study's tokens
  tk <- .page_args(pg$tokens)
  if (length(tk)) {
    v <- vapply(tk, function(x) if (length(x) != 1L || is.na(x)) "" else as.character(x), "")
    keep <- !trimws(v) %in% .ard_spec_omit
    if (any(keep)) {
      tok <- stats::setNames(as.list(unname(v[keep])), trimws(names(v)[keep]))
      out <- c(out, .spec_call_code(.spec_call("options", rtfreporter.tokens = tok), 0L))
    }
  }
  # the running header and footer: their lines, and the band
  for (band in names(.setup_band_names)) {
    b <- .page_band(pg, band, NULL)
    if (!length(b)) next
    # a line of tokens of one's own goes when a report's are empty
    own <- setdiff(.page_used_tokens(pg[band]), .ard_spec_rtf_tokens)
    nm <- .setup_band_names[[band]]
    lines <- paste0(nm, "_lines")
    # (its look as the study's page says it: page.rtf_header / rtf_footer)
    style <- .band_style(pg, band)
    if (is.null(style$drop_empty_rows) && length(own)) style$drop_empty_rows <- TRUE
    call <- do.call(.spec_call, c(list(paste0("rtf_", band), .spec_sym(lines)), style))
    out <- c(out,
             paste(lines, "<-", .spec_call_code(b, 0L, lead = nchar(lines) + 4L)),
             paste(nm, "<-", .spec_call_code(call, 0L, lead = nchar(nm) + 4L)))
  }
  if (!length(out)) return(character())
  .drop_attached_ns(unlist(strsplit(out, "\n", fixed = TRUE)))
}

# The {NAME} tokens the lines of page: bands say (header, footer, titles,
# footnotes; as many as are given)
.page_used_tokens <- function(pg) {
  txt <- unlist(lapply(pg, function(m) unlist(lapply(.page_args(m), .page_args))))
  txt <- as.character(txt[!is.na(txt)])
  m <- unlist(regmatches(txt, gregexpr("\\{[A-Z][A-Z0-9_]*\\}", txt)))
  unique(substr(m, 2L, nchar(m) - 1L))
}

# A report's tokens (YAML): as its document takes them, rtf_document(tokens
# = ) of tfl_report_code(output, study = ) -- the study's, its pattern's
# and its own, and OUTPUT_ID / OUTPUT_LABEL when its lines say them
.output_tokens <- function(output, study = NULL) {
  if (!isTRUE(attr(output, "resolved")) && !is.null(study)) {
    output <- tfl_resolve_output(output, study)
  }
  tk <- .page_tokens(output$page %||% list(), output$id %||% NA_character_,
                     output$kind %||% "table")
  if (is.null(tk)) return(stats::setNames(character(), character()))
  vapply(tk, as.character, "")
}
