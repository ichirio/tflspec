# The one-time conversion of a study's sheets (the xlsx / JSON definition of
# tflspec 0.0.24 and tflplanner 0.0.2) to the YAML spec
# (_archive/spec-format-redesign-2026-10-14.md).  What cannot be carried
# over, or changes form, is listed in `_not_converted.yml`.

#' Convert a study's definition sheets to the YAML spec
#'
#' Reads the definition files of a study folder as tflplanner wrote them --
#' `study.yml`, `spec/table_spec.xlsx`, `spec/report_spec.xlsx`,
#' `spec/ard_definition.json` (or `spec/ard_spec.xlsx`),
#' `spec/listing_figure_spec.xlsx`, `spec/figures/<id>.yml` -- and writes
#' the YAML spec to `to`: `study.yml`, `spec/data.yml`, `spec/defaults.yml`,
#' `spec/codelists.yml`, `spec/patterns/<name>.yml`, `spec/toc.yml` and one
#' `spec/outputs/<id>.yml` a report.  Loose files (a table and a report
#' workbook alone) convert too: `from` may be a folder holding them.
#'
#' A table's `content:` (its layers) is made by `content_fn(spec,
#' output_id)` -- the bridge from the sheets to the plan's layers; without
#' one, the content is left empty and said so in `_not_converted.yml`.
#' Cells that hold R (`where`, `derive`, `args`, `post`, `code`, `when`) are
#' kept as `!r` text and listed; `(none)` becomes `!unset`.
#'
#' Use, in R:
#' ```r
#' r <- tfl_convert_sheets_to_yaml("C:/studies/ABC-101", "C:/studies/ABC-101-yaml")
#' r$files                      # what was written
#' r$not_converted              # what was not, or changed form (also _not_converted.yml)
#' tfl_read_outputs(r$to)       # the reports, read back
#' ```
#'
#' @param from The study folder (or a folder of the definition files).
#' @param to The folder to write to (made if missing; `spec/` under it).
#' @param content_fn A function `(spec, output_id)` returning a list of
#'   [tfl_item()] for a table (`output_id = NA` for the defaults).  `NULL`:
#'   the package's own bridge from the sheets to the layers
#'   (`.plan_content_from_sheets()`) when it is there, else none.
#' @param overwrite Write over files already in `to`?
#' @return Invisibly, a list: `to`, `files` (paths written), `not_converted`
#'   (a data frame: `output`, `sheet`, `row`, `column`, `reason`, `value`).
#' @export
tfl_convert_sheets_to_yaml <- function(from, to, content_fn = NULL, overwrite = FALSE) {
  if (!dir.exists(from)) stop("tfl_convert_sheets_to_yaml(): no folder ", from, call. = FALSE)
  src <- .conv_sources(from)
  if (!length(src$books) && is.null(src$ard) && is.null(src$lf) && !length(src$figs)) {
    stop("tfl_convert_sheets_to_yaml(): no definition file in ", from, call. = FALSE)
  }
  if (dir.exists(to) && !overwrite && length(list.files(to, recursive = TRUE))) {
    stop("tfl_convert_sheets_to_yaml(): ", to, " is not empty (overwrite = TRUE to write over it).",
         call. = FALSE)
  }
  if (is.null(content_fn)) content_fn <- get0(".plan_content_from_sheets", mode = "function", inherits = FALSE)
  nc <- .nc_new()
  note <- function(output, sheet, row, column, reason, value = NA) {
    nc$rows[[length(nc$rows) + 1L]] <<- data.frame(
      output = .na_scalar(output), sheet = sheet, row = as.character(row),
      column = .na_scalar(column), reason = reason,
      value = if (is.null(value)) NA_character_ else .na_scalar(as.character(value)[1L]),
      stringsAsFactors = FALSE)
  }

  sp <- if (length(src$books)) tfl_read_table_spec(src$books) else NULL
  plan <- .conv_read_planner_sheet(src$books)
  ard <- if (!is.null(src$ard)) .conv_read_ard(src$ard) else NULL
  lf <- if (!is.null(src$lf)) .conv_read_lf(src$lf) else NULL
  meta <- if (!is.null(src$study)) yaml::read_yaml(src$study) else list()

  files <- character()
  put <- function(rel, x) {
    f <- file.path(to, rel)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    writeLines(yaml::as.yaml(x, indent.mapping.sequence = TRUE, handlers = .yaml_out_handlers()),
               f, useBytes = TRUE)
    files <<- c(files, f)
  }

  # study.yml: the meta, the ARD's study keys, the setup code, and the
  # version stamp (the spec's version, the packages' when written)
  study <- meta[setdiff(names(meta), c("created", "updated", "tflplanner"))]
  study$spec_version <- .spec_version
  study$packages <- .spec_packages()
  if (!is.null(ard$study) && nrow(ard$study)) {
    keys <- stats::setNames(as.list(ard$study$value), ard$study$key)
    keys <- keys[!vapply(keys, is.na, NA)]
    if (length(keys)) study$keys <- keys
  }
  if (!is.null(plan)) {
    srow <- plan[is.na(plan$output_id) | !nzchar(plan$output_id), , drop = FALSE]
    if (nrow(srow) && !is.na(srow$setup_code[1L])) study$setup <- tfl_r(srow$setup_code[1L])
  }
  put("study.yml", study)

  # spec/data.yml: the data catalog
  if (!is.null(ard)) {
    data <- list()
    if (nrow(ard$datasets)) data$datasets <- .conv_rows(ard$datasets, note, NA, "datasets")
    if (nrow(ard$populations)) {
      data$populations <- .conv_rows(ard$populations, note, NA, "populations",
                                     r_cols = c("where", "derive"), rename = c(population_id = "id"))
    }
    put("spec/data.yml", data)
  }

  # the report-half sheets by scope: defaults (blank output_id), patterns
  # (@name), each report
  scope <- function(d, id) {
    if (is.null(d) || !nrow(d)) return(d[0, , drop = FALSE])
    oid <- d$output_id
    if (is.na(id)) d[is.na(oid) | !nzchar(oid), , drop = FALSE]
    else d[!is.na(oid) & oid == id, , drop = FALSE]
  }
  page_of <- function(id) .conv_page(sp, id, scope, note)

  # spec/defaults.yml
  defaults <- list()
  if (!is.null(sp)) {
    if (!is.null(content_fn)) defaults$content <- .conv_items(content_fn(sp, NA_character_))
    else if (any(vapply(c("tables", "variables", "cells", "layout", "columns", "style", "col_header", "cell_styles"),
                        function(s) nrow(scope(sp[[s]], NA)) > 0L, NA))) {
      note(NA, "table sheets", "(defaults)", NA, "content: not converted -- no sheets-to-content bridge given (content_fn)")
    }
    pg <- page_of(NA_character_)
    if (length(pg)) defaults$page <- pg
  }
  put("spec/defaults.yml", defaults)

  # spec/codelists.yml: the lists with no report (templates)
  if (!is.null(sp$codelists) && nrow(scope(sp$codelists, NA))) {
    put("spec/codelists.yml", .conv_codelists(scope(sp$codelists, NA)))
  }

  # spec/patterns/<name>.yml
  pats <- unique(unlist(lapply(.ard_spec_pattern_sheets, function(s) {
    d <- sp[[s]]
    if (is.null(d) || !nrow(d)) return(character())
    oid <- d$output_id
    oid[!is.na(oid) & startsWith(oid, "@")]
  })))
  for (p in pats) {
    pg <- page_of(p)
    put(file.path("spec", "patterns", paste0(substring(p, 2L), ".yml")), list(page = pg))
  }

  # spec/toc.yml and spec/outputs/<id>.yml
  ids <- character()
  if (!is.null(plan)) ids <- plan$output_id[!is.na(plan$output_id) & nzchar(plan$output_id)]
  ids <- unique(c(ids, if (!is.null(sp$report)) sp$report$output_id[!is.na(sp$report$output_id) & !startsWith(sp$report$output_id, "@")],
                  if (!is.null(ard)) ard$analyses$output_id, if (!is.null(lf)) lf$listings$output_id,
                  names(src$figs)))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  toc <- list()
  for (id in ids) {
    prow <- if (!is.null(plan)) plan[!is.na(plan$output_id) & plan$output_id == id, , drop = FALSE] else NULL
    rrow <- scope(sp$report, id)
    kind <- if (nrow(rrow) && !is.na(rrow$type[1L])) rrow$type[1L] else
      if (id %in% names(src$figs)) "figure" else if (!is.null(lf) && id %in% lf$listings$output_id) "listing" else "table"
    if (identical(kind, "user")) { note(id, "report", id, "type", "a report of type user is converted as a table whose data is its code"); kind <- "table" }
    entry <- list(id = id, kind = kind)
    if (!is.null(prow) && nrow(prow)) {
      if (!is.na(prow$description[1L])) entry$label <- prow$description[1L]
      if (!is.na(prow$section[1L])) entry$section <- prow$section[1L]
      if (!is.na(prow$population[1L])) entry$population <- prow$population[1L]
      if (!is.na(prow$batches[1L])) entry$sets <- .conv_split(prow$batches[1L])
      if (!is.na(prow$datasets[1L])) entry$datasets <- .conv_split(prow$datasets[1L])
    }
    if (nrow(rrow)) for (k in c("file", "program")) if (!is.na(rrow[[k]][1L])) entry[[k]] <- rrow[[k]][1L]
    toc[[length(toc) + 1L]] <- entry

    out <- .conv_output(id, kind, entry, sp, ard, lf, src$figs[[id]], prow, scope, page_of, content_fn, note)
    f <- file.path(to, "spec", "outputs", paste0(id, ".yml"))
    tfl_write_output(out, f)
    files <- c(files, f)
  }
  put("spec/toc.yml", list(outputs = toc))

  nc_df <- .nc_frame(nc)
  put("spec/_not_converted.yml", list(not_converted = lapply(seq_len(nrow(nc_df)), function(i) as.list(nc_df[i, ]))))
  invisible(list(to = to, files = files, not_converted = nc_df))
}

# -- sources ------------------------------------------------------------------

.conv_sources <- function(from) {
  sp <- if (dir.exists(file.path(from, "spec"))) file.path(from, "spec") else from
  books <- file.path(sp, c("table_spec.xlsx", "report_spec.xlsx"))
  books <- books[file.exists(books)]
  ard <- c(file.path(sp, "ard_definition.json"), file.path(sp, "ard_spec.xlsx"))
  ard <- ard[file.exists(ard)]
  lf <- file.path(sp, "listing_figure_spec.xlsx")
  figs <- list.files(file.path(sp, "figures"), "\\.ya?ml$", full.names = TRUE)
  study <- file.path(from, "study.yml")
  list(books = books, ard = if (length(ard)) ard[1L],
       lf = if (file.exists(lf)) lf,
       figs = stats::setNames(as.list(figs), tools::file_path_sans_ext(basename(figs))),
       study = if (file.exists(study)) study)
}

# the ARD definition: JSON (columns as arrays) or the workbook; the sheets
# as data frames of text
.conv_read_ard <- function(path) {
  if (grepl("[.]json$", path, ignore.case = TRUE)) {
    j <- yaml::read_yaml(path)
    out <- lapply(names(.ard_spec_sheets), function(s) {
      d <- j[[s]]
      cols <- .ard_spec_sheets[[s]]
      if (is.null(d) || !length(d)) return(data.frame(matrix(character(), 0, length(cols), dimnames = list(NULL, cols)), stringsAsFactors = FALSE))
      n <- max(vapply(d, length, 1L))
      df <- as.data.frame(lapply(d, function(v) { v <- as.character(unlist(lapply(v, function(x) if (is.null(x)) NA_character_ else x))); length(v) <- n; v }),
                          stringsAsFactors = FALSE, check.names = FALSE)
      for (c in setdiff(cols, names(df))) df[[c]] <- NA_character_
      if ("output_id" %in% names(df) && !"output_id" %in% cols) cols <- c("output_id", cols)
      df[intersect(c(cols, setdiff(names(df), cols)), names(df))]
    })
    names(out) <- names(.ard_spec_sheets)
    return(out)
  }
  tfl_read_ard_spec(path)
}

# tflplanner's own sheet of the report workbook (the report list, the setup
# code): not a spec sheet, read as it is
.conv_read_planner_sheet <- function(books) {
  for (f in books) {
    sh <- intersect(c("_tflplanner", "_rtfplanner"), readxl::excel_sheets(f))
    if (length(sh)) {
      d <- as.data.frame(readxl::read_excel(f, sh[1L], col_types = "text"), stringsAsFactors = FALSE)
      for (cn in c("output_id", "description", "setup_code", "process_code", "section",
                   "population", "datasets", "batches")) if (!cn %in% names(d)) d[[cn]] <- NA_character_
      d$output_id <- trimws(d$output_id)
      return(d)
    }
  }
  NULL
}

.conv_read_lf <- function(path) {
  sheets <- readxl::excel_sheets(path)
  rd <- function(s) if (s %in% sheets) as.data.frame(readxl::read_excel(path, s, col_types = "text"), stringsAsFactors = FALSE) else NULL
  list(listings = rd("listings"), listing_cols = rd("listing_cols"), figures = rd("figures"))
}

# -- pieces -------------------------------------------------------------------

.conv_split <- function(v) {
  if (is.na(v) || !nzchar(trimws(v))) return(NULL)
  x <- trimws(strsplit(v, "|", fixed = TRUE)[[1L]])
  x[nzchar(x)]
}

# one cell: NA left out, "(none)" an unset, R text an !r (noted), "|" lists split
.conv_cell <- function(v, col, r_cols, list_cols, note, output, sheet, row) {
  if (is.null(v) || is.na(v) || !nzchar(trimws(v))) return(NULL)
  v <- gsub("\r\n", "\n", v, fixed = TRUE)   # a cell's line ends as the YAML's
  v <- gsub("\r", "\n", v, fixed = TRUE)
  if (identical(trimws(v), "(none)")) return(tfl_unset())
  if (col %in% r_cols) {
    note(output, sheet, row, col, "kept as R text (!r)", v)
    return(tfl_r(v))
  }
  if (col %in% list_cols) return(as.list(.conv_split(v)))
  if (v %in% c("TRUE", "FALSE")) return(identical(v, "TRUE"))
  n <- suppressWarnings(as.numeric(v))
  if (!is.na(n) && grepl("^-?[0-9.]+$", v)) return(if (n == round(n) && !grepl("[.]", v)) as.integer(n) else n)
  v
}

# the rows of a sheet as a list of maps (column -> value), NA cells left out
.conv_rows <- function(d, note, output, sheet, r_cols = character(), list_cols = character(),
                       rename = character(), drop = c("output_id", "note")) {
  if (is.null(d) || !nrow(d)) return(list())
  cols <- setdiff(names(d), drop)
  lapply(seq_len(nrow(d)), function(i) {
    m <- list()
    for (cn in cols) {
      v <- .conv_cell(d[[cn]][i], cn, r_cols, list_cols, note, output, sheet, i)
      if (!is.null(v)) m[[if (cn %in% names(rename)) rename[[cn]] else cn]] <- v
    }
    m
  })
}

# the report half of one scope as `page:`: lines by number, tokens by name,
# the page and report rows as maps
.conv_page <- function(sp, id, scope, note) {
  if (is.null(sp)) return(list())
  page <- list()
  for (s in c("header", "footer", "titles", "footnotes")) {
    d <- scope(sp[[s]], id)
    if (is.null(d) || !nrow(d)) next
    m <- list()
    for (i in seq_len(nrow(d))) {
      line <- as.character(d$line[i])
      cells <- list()
      for (cn in intersect(c("left", "center", "right"), names(d))) {
        v <- .conv_cell(d[[cn]][i], cn, character(), character(), note, id, s, i)
        if (!is.null(v)) cells[[cn]] <- v
      }
      # a line of (none) in every cell: the line itself unset
      if (length(cells) && all(vapply(cells, inherits, NA, "tfl_unset"))) cells <- tfl_unset()
      m[[line]] <- cells
    }
    page[[s]] <- m
  }
  d <- scope(sp$tokens, id)
  if (!is.null(d) && nrow(d)) {
    m <- list()
    for (i in seq_len(nrow(d))) {
      v <- .conv_cell(d$value[i], "value", character(), character(), note, id, "tokens", i)
      if (!is.null(v)) m[[d$name[i]]] <- v
    }
    page$tokens <- m
  }
  for (s in c("page", "report")) {
    d <- scope(sp[[s]], id)
    if (is.null(d) || !nrow(d)) next
    rows <- .conv_rows(d[1L, , drop = FALSE], note, id, s, drop = c("output_id", "note", "type", "pattern"))
    if (length(rows[[1L]])) page[[s]] <- rows[[1L]]
  }
  page
}

.conv_codelists <- function(d) {
  out <- list()
  for (v in unique(d$variable)) {
    r <- d[d$variable == v, , drop = FALSE]
    if (!is.null(r$order)) r <- r[order(suppressWarnings(as.numeric(r$order)), na.last = TRUE), , drop = FALSE]
    out[[v]] <- lapply(seq_len(nrow(r)), function(i) {
      m <- list(value = r$value[i])
      if (!is.na(r$label[i])) m$label <- r$label[i]
      m
    })
  }
  out
}

.conv_items <- function(items) {
  if (!is.list(items)) stop("content_fn() must return a list of tfl_item().", call. = FALSE)
  items
}

# one report's file
.conv_output <- function(id, kind, entry, sp, ard, lf, fig, prow, scope, page_of, content_fn, note) {
  rrow <- scope(sp$report, id)
  pattern <- if (nrow(rrow) && !is.null(rrow$pattern) && !is.na(rrow$pattern[1L])) rrow$pattern[1L]
  extends <- c(if (!is.null(pattern)) paste0("patterns/", sub("^@", "", pattern)), "defaults")
  data <- NULL
  content <- list()
  codelists <- list()
  extra <- list()
  if (kind == "figure" && !is.null(fig)) {
    y <- yaml::read_yaml(fig, handlers = .yaml_handlers())
    data <- list(source = "code")
    if (!is.null(y$data)) data$steps <- y$data
    if (!is.null(y$stats)) data$stats <- y$stats
    content <- lapply(y$layers %||% list(), function(l) tfl_item(l$layer %||% "layer", l[setdiff(names(l), "layer")]))
    for (k in c("template", "ggplot2_version", "plot", "plots", "compose")) if (!is.null(y[[k]])) extra[[k]] <- y[[k]]
    note(id, "figure", id, NA, "a figure's layers are content items `layer: {fields}` as before; plot/plots/compose kept beside them")
  } else if (kind == "listing" && !is.null(lf)) {
    lr <- lf$listings[!is.na(lf$listings$output_id) & lf$listings$output_id == id, , drop = FALSE]
    lc <- lf$listing_cols[!is.na(lf$listing_cols$output_id) & lf$listing_cols$output_id == id, , drop = FALSE]
    data <- list(source = "dataset")
    if (nrow(lr)) {
      for (k in c("dataset")) if (!is.na(lr[[k]][1L])) data[[k]] <- lr[[k]][1L]
      if (!is.na(lr$where[1L])) { data$where <- tfl_r(lr$where[1L]); note(id, "listings", 1, "where", "kept as R text (!r)", lr$where[1L]) }
      if (!is.na(lr$sort[1L])) data$sort <- as.list(.conv_split(lr$sort[1L]))
    }
    cols <- .conv_rows(lc, note, id, "listing_cols", list_cols = "vars")
    content <- lapply(cols, function(m) tfl_item("listing_col", m))
    if (nrow(lr)) {
      spec_args <- .conv_rows(lr[1L, , drop = FALSE], note, id, "listings",
                              drop = c("output_id", "note", "dataset", "where", "sort", "max_rows"))[[1L]]
      if (length(spec_args)) content <- c(content, list(tfl_item("listing_spec", spec_args)))
      if (!is.na(lr$max_rows[1L])) content <- c(content, list(tfl_item("paginate", list(max_rows = as.integer(lr$max_rows[1L])))))
    }
  } else {
    data <- list(source = "ard")
    if (!is.null(ard)) {
      ad <- ard$analysis_data
      if (!is.null(ad$output_id)) ad <- ad[!is.na(ad$output_id) & ad$output_id == id, , drop = FALSE]
      if (nrow(ad)) data$analysis_data <- .conv_rows(ad, note, id, "analysis_data", r_cols = c("where", "derive", "code"),
                                                      list_cols = c("add", "keep", "distinct"), rename = c(data_id = "id", population_id = "population"))
      an <- ard$analyses[!is.na(ard$analyses$output_id) & ard$analyses$output_id == id, , drop = FALSE]
      if (nrow(an)) data$analyses <- .conv_rows(an, note, id, "analyses", r_cols = c("where", "args", "post", "code"),
                                                 list_cols = c("by", "strata", "variables", "statistics", "formats"),
                                                 rename = c(analysis_id = "id", population_id = "population"))
    }
    if (!is.null(prow) && nrow(prow) && !is.na(prow$process_code[1L])) {
      data$process <- tfl_r(prow$process_code[1L])
      note(id, "_tflplanner", id, "process_code", "the table's rework step kept as R text (data.process)", prow$process_code[1L])
    }
    if (!is.null(sp)) {
      cl <- scope(sp$codelists, id)
      if (!is.null(cl) && nrow(cl)) codelists <- .conv_codelists(cl)
      if (!is.null(content_fn)) content <- .conv_items(content_fn(sp, id))
      else if (any(vapply(c("tables", "variables", "cells", "digits", "layout", "columns", "style", "col_header", "cell_styles"),
                          function(s) nrow(scope(sp[[s]], id)) > 0L, NA))) {
        note(id, "table sheets", id, NA, "content: not converted -- no sheets-to-content bridge given (content_fn)")
      }
    }
  }
  out <- tfl_output(id, kind, label = entry$label %||% NA, population = entry$population %||% NA,
                    extends = extends, data = data, codelists = codelists, content = content,
                    page = page_of(id))
  for (k in names(extra)) out[[k]] <- extra[[k]]
  out
}

# -- the record of what was not converted -------------------------------------

.nc_new <- function() { e <- new.env(); e$rows <- list(); e }
.nc_frame <- function(nc) {
  if (!length(nc$rows)) {
    return(data.frame(output = character(), sheet = character(), row = character(),
                      column = character(), reason = character(), value = character(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, nc$rows)
}
