# ============================================================================
#  The study ARD in other formats: JSON / YAML / XPT, and back
# ----------------------------------------------------------------------------
#  The rds the ARD program writes stays the record (it keeps everything:
#  factors and their levels, the formatting functions).  These are copies
#  for a reader that does not use R, or for a tool that wants one format.
#
#  JSON / YAML "rows" (the default) is one record per statistic, with a
#  `levels` section -- each variable's levels in their order, which a JSON
#  object's keys cannot be trusted to keep -- and the column types, so
#  tfl_read_ard() gives the ARD back (all but the formatting functions).
#  "nested" is cards' own shape (as_nested_list()), for a reader that knows
#  it; it is not read back.  XPT holds a flat table only.
# ============================================================================

.ard_io_format <- function(path, format) {
  if (!is.null(format)) {
    return(match.arg(tolower(format), c("json", "yaml", "xpt", "rds")))
  }
  ext <- tolower(tools::file_ext(path))
  switch(ext, json = "json", yaml = , yml = "yaml", xpt = "xpt", rds = "rds",
         .ard_stop(sprintf(paste0(
           "The format of %s is not known from its extension; give `format` ",
           "(json, yaml, xpt, rds)."), path)))
}

.ard_level_cols <- function(d) {
  g <- grep("^group[0-9]+_level$", names(d), value = TRUE)
  c(g, intersect("variable_level", names(d)))
}

# each variable's levels, in their order, from the factors in the ARD
.ard_levels <- function(d) {
  out <- list()
  for (lc in .ard_level_cols(d)) {
    key <- if (lc == "variable_level") d$variable else d[[sub("_level$", "", lc)]]
    for (i in seq_len(nrow(d))) {
      v <- d[[lc]][[i]]
      if (is.factor(v) && !is.na(key[i]) && is.null(out[[key[i]]])) {
        out[[key[i]]] <- levels(v)
      }
    }
  }
  out
}

# one cell of a list column, for a record: NULL -> NA, a factor -> its
# label, one value -> that value, several -> as they are
.ard_cell <- function(v) {
  if (is.null(v) || !length(v)) return(NA)
  if (is.function(v)) return(NA)
  if (is.factor(v)) v <- as.character(v)
  if (inherits(v, "Date") || inherits(v, "POSIXt")) v <- as.character(v)
  if (length(v) == 1L) v[[1L]] else unname(as.list(v))
}

.ard_column_types <- function(d) {
  vapply(d, function(col) {
    if (is.list(col)) "list" else if (is.numeric(col)) "number" else
      if (is.logical(col)) "logical" else "character"
  }, "")
}

#' Write the study ARD as JSON, YAML or XPT
#'
#' A copy of the ARD for a reader that does not use R; the rds the ARD
#' program writes stays the record.  What each format keeps:
#'
#' | format | shape | what is lost |
#' |---|---|---|
#' | rds | the ARD | nothing |
#' | JSON / YAML | `"rows"` (default): one record per statistic, with each variable's **levels** in order and the column types | the formatting functions (`fmt_fun`); `stat_fmt` keeps their result |
#' | JSON / YAML | `"nested"`: cards' [cards::as_nested_list()] | the formatting functions, `stat_label`, the id columns, the column types; the levels' order is only the keys' (not guaranteed) |
#' | XPT | a flat table (version 8; 5 shortens the column names, see below) | factors and their levels, the formatting functions, `warning` / `error`; `stat` becomes text when an analysis gives text as well as numbers |
#'
#' A format that loses something says so in a warning.  [tfl_read_ard()]
#' reads `"rows"` back (and XPT as far as it can).  XPT version 5 allows
#' 8-character column names: `group1_level` is `GRP1LVL`, `variable_level`
#' `VARLVL`, `stat_name` `STATNAME`, `stat_label` `STATLBL`, `output_id`
#' `OUTID`, `analysis_id` `ANID`, `population_id` `POPID`, `variable`
#' `VARIABLE`, `context` `CONTEXT`, `stat_fmt` `STATFMT`.
#'
#' @param ard An ARD (the study ARD, [tfl_build_ard()], or any cards ARD).
#' @param path The file to write.
#' @param format `"json"`, `"yaml"`, `"xpt"` or `"rds"`; `NULL` takes it from
#'   the file's extension.
#' @param shape For JSON / YAML: `"rows"` or `"nested"`.
#' @param xpt_version For XPT: `8` (default) or `5`.
#' @return `path`, invisibly, with the attribute `lost`: what the format
#'   could not keep.
#' @seealso [tfl_read_ard()]
#' @export
tfl_write_ard <- function(ard, path, format = NULL, shape = c("rows", "nested"),
                          xpt_version = 8L) {
  if (!is.data.frame(ard) || !all(c("variable", "stat_name", "stat") %in% names(ard))) {
    .ard_stop(sprintf(
      "tfl_write_ard(): `ard` must be an ARD (variable, stat_name, stat columns); got %s.",
      .what(ard)))
  }
  format <- .ard_io_format(path, format)
  shape <- match.arg(shape)
  d <- as.data.frame(ard, stringsAsFactors = FALSE)
  lost <- character()
  if (format == "rds") {
    saveRDS(ard, path)
    return(invisible(structure(path, lost = lost)))
  }
  if ("fmt_fun" %in% names(d) &&
      any(vapply(d$fmt_fun, is.function, NA))) {
    lost <- c(lost, "the formatting functions (fmt_fun); stat_fmt keeps their result")
  }
  if (format %in% c("json", "yaml") && shape == "nested") {
    if (!requireNamespace("cards", quietly = TRUE)) {
      .ard_stop("tfl_write_ard(shape = \"nested\") needs the cards package.")
    }
    ids <- intersect(c("output_id", "analysis_id", "population_id"), names(d))
    x <- cards::as_card(d[setdiff(names(d), ids)], check = FALSE)
    obj <- cards::as_nested_list(x)
    lost <- c(lost, "stat_label", if (length(ids)) "the id columns",
              "the column types",
              "the levels' order (only the order of the keys, which JSON / YAML do not guarantee)")
  } else if (format %in% c("json", "yaml")) {
    cols <- setdiff(names(d), "fmt_fun")
    rows <- lapply(seq_len(nrow(d)), function(i) {
      r <- lapply(cols, function(cn) {
        v <- d[[cn]]
        if (is.list(v)) .ard_cell(v[[i]]) else if (is.factor(v)) as.character(v[i]) else v[i]
      })
      stats::setNames(r, cols)
    })
    obj <- list(
      meta = list(format = "tflspec ARD", version = 1L,
                  written = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                  tflspec = as.character(utils::packageVersion("tflspec")),
                  cards = if (requireNamespace("cards", quietly = TRUE))
                    as.character(utils::packageVersion("cards")) else NA),
      columns = as.list(.ard_column_types(d[cols])),
      levels = .ard_levels(d),
      rows = rows)
  }
  if (format == "json") {
    writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE,
                                null = "null", na = "null", digits = NA),
               path, useBytes = TRUE)
  } else if (format == "yaml") {
    yaml::write_yaml(obj, path, precision = 15L)
  } else {
    if (!requireNamespace("haven", quietly = TRUE)) {
      .ard_stop("tfl_write_ard(format = \"xpt\") needs the haven package.")
    }
    flat <- d[setdiff(names(d), c("fmt_fun", "warning", "error"))]
    for (cn in names(flat)) {
      if (is.list(flat[[cn]])) {
        flat[[cn]] <- vapply(flat[[cn]], function(v) {
          v <- .ard_cell(v)
          if (length(v) == 1L && is.na(v)) NA_character_ else
            paste(as.character(unlist(v)), collapse = " | ")
        }, "")
      }
      if (is.factor(flat[[cn]])) flat[[cn]] <- as.character(flat[[cn]])
    }
    num <- suppressWarnings(as.numeric(flat$stat))
    if (all(is.na(num) == is.na(flat$stat))) {
      flat$stat <- num
    } else {
      lost <- c(lost, "the type of stat (text as well as numbers: all text)")
    }
    lost <- c(lost, "factors and their levels",
              if (any(c("warning", "error") %in% names(d))) "warning / error")
    long <- vapply(flat, function(v) is.character(v) && any(nchar(v) > 200, na.rm = TRUE), NA)
    if (any(long)) {
      lost <- c(lost, sprintf("text longer than 200 characters (%s) is cut",
                              paste(names(flat)[long], collapse = ", ")))
      flat[long] <- lapply(flat[long], substr, 1L, 200L)
    }
    xpt_version <- as.integer(xpt_version)
    if (xpt_version == 5L) {
      names(flat) <- .ard_xpt5_names(names(flat))
      lost <- c(lost, "the column names (8 characters)")
    }
    haven::write_xpt(flat, path, version = xpt_version, name = "ARD")
  }
  if (length(lost)) {
    warning(sprintf("tfl_write_ard(): %s does not keep %s.", basename(path),
                    paste(lost, collapse = "; ")), call. = FALSE)
  }
  invisible(structure(path, lost = lost))
}

.ard_xpt5_names <- function(nm) {
  map <- c(output_id = "OUTID", analysis_id = "ANID", population_id = "POPID",
           variable = "VARIABLE", variable_level = "VARLVL",
           context = "CONTEXT", stat_name = "STATNAME", stat_label = "STATLBL",
           stat = "STAT", stat_fmt = "STATFMT", pairwise = "PAIRWISE")
  out <- ifelse(nm %in% names(map), map[nm], nm)
  g <- grepl("^group[0-9]+(_level)?$", nm)
  out[g] <- toupper(sub("^group([0-9]+)_level$", "GRP\\1LVL",
                        sub("^group([0-9]+)$", "GRP\\1", nm[g])))
  substr(toupper(out), 1L, 8L)
}

#' Read an ARD written by tfl_write_ard()
#'
#' The `"rows"` JSON / YAML gives the ARD back: its columns with their
#' types, each variable's levels as factors in their order, the
#' statistics as numbers or text as written (the formatting functions are
#' not kept; `stat_fmt` is).  An XPT or CSV is read as far as a flat table
#' allows: the level columns become list columns again, with no factors.
#' An rds is read as it is.
#'
#' @param path The file.
#' @param format As [tfl_write_ard()]; `NULL` from the extension (`.csv` too).
#' @return A cards ARD (class `card` when cards is installed).
#' @seealso [tfl_write_ard()]
#' @export
tfl_read_ard <- function(path, format = NULL) {
  if (is.null(format) && tolower(tools::file_ext(path)) == "csv") format <- "csv"
  format <- if (identical(format, "csv")) "csv" else .ard_io_format(path, format)
  if (format == "rds") return(readRDS(path))
  if (format %in% c("json", "yaml")) {
    obj <- if (format == "json") jsonlite::fromJSON(path, simplifyVector = FALSE)
           else yaml::read_yaml(path)
    if (is.null(obj$rows) || is.null(obj$columns)) {
      .ard_stop(sprintf(paste0(
        "tfl_read_ard(): %s is not the \"rows\" shape of tfl_write_ard() ",
        "(cards' nested shape cannot be read back)."), basename(path)))
    }
    cols <- names(obj$columns)
    d <- as.data.frame(lapply(stats::setNames(cols, cols), function(cn) {
      vals <- lapply(obj$rows, function(r) {
        v <- r[[cn]]
        if (is.null(v)) NA else if (is.list(v)) unlist(v) else v
      })
      if (identical(obj$columns[[cn]], "list")) I(vals) else
        unlist(lapply(vals, function(v) if (length(v)) v[1L] else NA))
    }), stringsAsFactors = FALSE, check.names = FALSE)
    for (cn in cols) if (inherits(d[[cn]], "AsIs")) d[[cn]] <- unclass(d[[cn]])
    for (cn in intersect(c("warning", "error"), names(d))) {
      d[[cn]] <- lapply(d[[cn]], function(v) if (length(v) == 1L && is.na(v)) NULL else v)
    }
    lv <- obj$levels %||% list()
    for (lc in .ard_level_cols(d)) {
      key <- if (lc == "variable_level") d$variable else d[[sub("_level$", "", lc)]]
      d[[lc]] <- lapply(seq_len(nrow(d)), function(i) {
        v <- d[[lc]][[i]]
        if (length(v) == 1L && is.na(v)) return(NULL)
        l <- lv[[key[i]]]
        if (!is.null(l)) factor(as.character(v), levels = unlist(l)) else v
      })
    }
    d$stat <- lapply(d$stat, function(v) if (length(v) == 1L && is.na(v)) NULL else v)
  } else {
    d <- if (format == "csv") utils::read.csv(path, stringsAsFactors = FALSE,
                                              colClasses = "character")
         else as.data.frame(haven::read_xpt(path))
    back <- c(OUTID = "output_id", ANID = "analysis_id", POPID = "population_id",
              VARIABLE = "variable", VARLVL = "variable_level",
              CONTEXT = "context", STATNAME = "stat_name",
              STATLBL = "stat_label", STAT = "stat", STATFMT = "stat_fmt",
              PAIRWISE = "pairwise")
    up <- toupper(names(d))
    hit <- up %in% names(back)
    names(d)[hit] <- back[up[hit]]
    names(d) <- sub("^GRP([0-9]+)LVL$", "group\\1_level",
                    sub("^GRP([0-9]+)$", "group\\1", names(d)))
    for (lc in c(.ard_level_cols(d), "stat")) {
      d[[lc]] <- lapply(d[[lc]], function(v) {
        if (is.na(v) || identical(v, "")) return(NULL)
        if (lc == "stat") {
          n <- suppressWarnings(as.numeric(v))
          if (!is.na(n)) return(n)
        }
        v
      })
    }
  }
  if (requireNamespace("cards", quietly = TRUE)) {
    d <- cards::as_card(d, check = FALSE)
  }
  d
}
