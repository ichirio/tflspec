# ============================================================================
#  A report's file name from the study's pattern (decided 10/24)
# ----------------------------------------------------------------------------
#  The study's `settings: file` (and a report's or TOC entry's own) is a
#  pattern: `{output_id}` as ever, and the parts of the report's number --
#  `{type}` (t / f / l), `{num}` (15.1.5.1), `{num:N}` (its first N levels
#  joined, 151), `{part}` (a), `{part_no}` (1) -- and `{shell_code}`, the
#  id written at the top of the report's shell (t_dem_sub_01, or
#  151_t_dem_sub_01), its TOC entry's `shell_code`.  `[ ... ]` is dropped
#  whole when a field in it is empty: `[m{part_no}_]{shell_code}.rtf` is
#  m1_151_t_dem_sub_01.rtf for a part and 151_t_dem_sub_01.rtf without.
#  The TOC's template number is never used (one template serves several
#  shells).
# ============================================================================

# The fields of a report's number: its id read as a report number
.file_name_fields <- function(id, entry = list()) {
  key <- tryCatch(.ai_id_key(id), error = function(e) NA_character_)
  f <- list(output_id = id, type = "", num = "", part = "", part_no = "",
            shell_code = as.character(entry[["shell_code"]] %||% ""))
  if (!is.na(key) && nzchar(key)) {
    if (grepl("^[TFL]", key)) f$type <- tolower(substr(key, 1L, 1L))
    if (grepl("[A-Z]$", key) && grepl("[0-9]", key)) {
      f$part <- tolower(substr(key, nchar(key), nchar(key)))
      f$part_no <- as.character(match(f$part, letters))
    }
    f$num <- gsub("^[A-Z]+|[A-Z]+$", "", key)
  }
  f
}

# The pattern filled: each `{field}` (`{num:N}` the number's first N
# levels), a `[ ... ]` dropped when a field in it is empty; a value's
# characters a file name cannot hold made `_`
.file_name_fill <- function(x, id, entry = list()) {
  if (is.null(x) || is.na(id)) return(x)
  f <- .file_name_fields(id, entry)
  value <- function(name) {
    if (grepl("^num:[0-9]+$", name)) {
      n <- as.integer(sub("^num:", "", name))
      lv <- strsplit(f$num, ".", fixed = TRUE)[[1L]]
      return(if (length(lv)) paste(utils::head(lv, n), collapse = "") else "")
    }
    v <- f[[name]]
    if (is.null(v)) return(NA_character_)
    if (identical(name, "output_id")) v else gsub("[^A-Za-z0-9._-]", "_", v)
  }
  fill <- function(s) {
    m <- gregexpr("\\{[A-Za-z_]+(:[0-9]+)?\\}", s)[[1L]]
    if (m[1L] < 0L) return(list(text = s, empty = FALSE))
    names <- substring(s, m + 1L, m + attr(m, "match.length") - 2L)
    vals <- vapply(names, value, "")
    keep <- !is.na(vals)
    out <- s
    for (k in which(keep)) out <- sub(paste0("{", names[k], "}"), vals[[k]], out, fixed = TRUE)
    list(text = out, empty = any(keep & !nzchar(vals)))
  }
  # the optional parts first: dropped when a field in them is empty
  repeat {
    m <- regexpr("\\[[^][]*\\]", x)
    if (m < 0L) break
    seg <- substring(x, m + 1L, m + attr(m, "match.length") - 2L)
    r <- fill(seg)
    x <- paste0(substring(x, 1L, m - 1L), if (r$empty) "" else r$text,
                substring(x, m + attr(m, "match.length")))
  }
  fill(x)$text
}

# A report's TOC entry: its own, else (a part, t.15.1.5.1.a) its number's
.file_toc_entry <- function(study, id) {
  e <- .page_toc_entry(study, id)
  if (length(e)) return(e)
  key <- tryCatch(.ai_id_key(id), error = function(e) NA_character_)
  if (is.na(key) || !grepl("[0-9][A-Z]$", key)) return(list())
  base <- sub("[A-Z]$", "", key)
  for (e in study$toc$outputs %||% list()) {
    k <- tryCatch(.ai_id_key(e$id %||% ""), error = function(err) NA_character_)
    if (identical(k, base)) return(e)
  }
  list()
}

# The id written at the top of a shell sheet (its first rows): the whole
# of `151_t_dem_sub_01` or `t_dem_sub_01`; `pattern` the study's own
# (settings: shell_code), else this form.  NA when none.
.shell_code_pattern <- "^([0-9]+_)?[tlf]_[A-Za-z0-9_]+$"

.shell_code_of <- function(cells, pattern = NULL, rows = 5L) {
  if (is.null(cells) || !nrow(cells)) return(NA_character_)
  d <- cells[cells$row <= min(cells$row) + rows - 1L, , drop = FALSE]
  d <- d[order(d$row, d$col), , drop = FALSE]
  v <- trimws(as.character(d$text))
  hit <- v[!is.na(v) & grepl(pattern %||% .shell_code_pattern, v, perl = TRUE)]
  if (length(hit)) hit[1L] else NA_character_
}
