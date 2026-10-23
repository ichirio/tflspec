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
#' @param figures How a figure is written: `"design"` (default) its
#'   designer's pieces as the content's items, as the figure designer reads
#'   them; `"items"` its content of function calls, plot by plot
#'   ([tfl_fig_content_from_design()]: `ggplot`, `geom_line`, ...,
#'   `tfl_image`), the form the designer is moving to.
#' @return Invisibly, a list: `to`, `files` (paths written), `not_converted`
#'   (a data frame: `output`, `sheet`, `row`, `column`, `reason`, `value`).
#' @export
tfl_convert_sheets_to_yaml <- function(from, to, content_fn = NULL, overwrite = FALSE,
                                       figures = c("design", "items")) {
  figures <- match.arg(figures)
  if (!dir.exists(from)) stop("tfl_convert_sheets_to_yaml(): no folder ", from, call. = FALSE)
  src <- .conv_sources(from)
  if (!length(src$books) && is.null(src$ard) && is.null(src$lf) && !length(src$figs)) {
    stop("tfl_convert_sheets_to_yaml(): no definition file in ", from, call. = FALSE)
  }
  sheets <- list(
    table = if (length(src$books)) tfl_read_table_spec(src$books),
    planner = .conv_read_planner_sheet(src$books),
    ard = if (!is.null(src$ard)) .conv_read_ard(src$ard),
    listing = if (!is.null(src$lf)) .conv_read_lf(src$lf),
    figures = lapply(src$figs, .conv_read_fig),
    study = if (!is.null(src$study)) yaml::read_yaml(src$study) else list())
  tfl_yaml_from_sheets(sheets, to, content_fn = content_fn, overwrite = overwrite,
                       figures = figures)
}

#' @rdname tfl_convert_sheets_to_yaml
#' @param sheets The definition as objects, a list: `table` (a
#'   `tfl_table_spec`, or its sheets; the report sheets with them),
#'   `planner` (tflplanner's `_tflplanner` sheet: the report list and the
#'   setup code), `ard` (the ARD sheets), `listing` (`listings`,
#'   `listing_cols`), `figures` (a figure's design list, by id), `study`
#'   (study.yml's fields).  What the app holds, or what the workbooks read
#'   as.
#' @export
tfl_yaml_from_sheets <- function(sheets, to, content_fn = NULL, overwrite = FALSE,
                                 figures = c("design", "items")) {
  figures <- match.arg(figures)
  # a cell's line breaks as the YAML's (Excel keeps a cell's as CRLF): every
  # sheet's text, before the bridge reads it (a header's "{col}\r\n(N={n})")
  sheets[c("table", "ard", "listing")] <- lapply(sheets[c("table", "ard", "listing")], .conv_lf_sheets)
  # a table's font size is its plan's (plan_style()), not the document's
  # rtf_tables(): the report sheet's table_font_size_half_points onto the
  # style sheet (where it is blank)
  if (!is.null(sheets$table)) sheets$table <- .conv_font_to_style(sheets$table)
  sp <- sheets$table
  plan <- sheets$planner
  ard <- sheets$ard
  lf <- sheets$listing
  figs <- sheets$figures %||% list()
  meta <- sheets$study %||% list()
  if (dir.exists(to) && !overwrite && length(list.files(to, recursive = TRUE))) {
    stop("tfl_yaml_from_sheets(): ", to, " is not empty (overwrite = TRUE to write over it).",
         call. = FALSE)
  }
  # the package's bridge: the defaults' rows alone for output_id = NA, a
  # report's own rows (own = TRUE) for an id -- a report's file says only
  # what is its own; a content_fn given is called (spec, output_id)
  if (is.null(content_fn)) {
    bridge <- .bridge_fn(".plan_content_from_sheets")
    if (!is.null(bridge)) {
      # (with the ARD definition: the orders the code lists give, which the
      # ARD already has, are not written into the report's file)
      content_fn <- function(sp, id) {
        if (is.null(id) || is.na(id)) bridge(sp, NA) else
          bridge(sp, id, own = TRUE, ard_spec = if (length(ard$analyses)) ard)
      }
    }
  }
  nc <- .nc_new()
  note <- function(output, sheet, row, column, reason, value = NA) {
    nc$rows[[length(nc$rows) + 1L]] <<- data.frame(
      output = .na_scalar(output), sheet = sheet, row = as.character(row),
      column = .na_scalar(column), reason = reason,
      value = if (is.null(value)) NA_character_ else .na_scalar(as.character(value)[1L]),
      stringsAsFactors = FALSE)
  }


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
  # the table / report workbooks' study sheet (rounding, output_path,
  # program_dir): the study's settings
  if (!is.null(sp$study) && nrow(sp$study)) {
    st <- sp$study[!is.na(sp$study$key) & !is.na(sp$study$value), , drop = FALSE]
    if (nrow(st)) study$settings <- stats::setNames(as.list(st$value), st$key)
  }
  # every report's file and program (the report sheet's study row: "{output_id}.rtf")
  if (!is.null(sp$report) && nrow(sp$report)) {
    dr <- sp$report[is.na(sp$report$output_id) | !nzchar(sp$report$output_id), , drop = FALSE]
    for (k in c("file", "program")) {
      if (nrow(dr) && k %in% names(dr) && !is.na(dr[[k]][1L])) study$settings[[k]] <- dr[[k]][1L]
    }
  }
  if (!is.null(ard$study) && nrow(ard$study)) {
    keys <- stats::setNames(as.list(ard$study$value), ard$study$key)
    keys <- keys[!vapply(keys, is.na, NA)]
    if (length(keys)) study$keys <- keys
  }
  if (!is.null(plan)) {
    srow <- plan[is.na(plan$output_id) | !nzchar(plan$output_id), , drop = FALSE]
    if (nrow(srow) && !is.na(srow$setup_code[1L])) study$setup <- tfl_r(gsub("\r\n?", "\n", srow$setup_code[1L]))
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
    # the named sets of records
    if (!is.null(ard$records) && nrow(ard$records)) {
      data$records <- .conv_rows(ard$records, note, NA, "records", r_cols = "where",
                                 rename = c(record_id = "id"))
    }
    # the study's named values (no output_id)
    ob <- ard$objects
    if (!is.null(ob) && nrow(ob)) {
      ob <- ob[is.na(ob$output_id), , drop = FALSE]
      if (nrow(ob)) data$objects <- .conv_rows(ob, note, NA, "objects", r_cols = "code",
                                               rename = c(object_id = "id"))
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
    if (!is.null(content_fn)) {
      # (the bridge's scope of the defaults: the rows with no report)
      items <- tryCatch(.conv_items(content_fn(sp, NA_character_)), error = function(e) e)
      if (inherits(items, "error")) {
        note(NA, "table sheets", "(defaults)", NA, paste0("defaults content: not converted -- ", conditionMessage(items)))
      } else {
        # every table starts from the ARD made a table's rows (the program's
        # normalize_ard()): once, for all of them
        defaults$content <- .items_to_list(c(list(tfl_item("normalize_ard", list())), items))
      }
    } else if (any(vapply(c("tables", "variables", "cells", "layout", "columns", "style", "col_header", "cell_styles"),
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
  table_ids <- unlist(lapply(names(.ard_spec_schema()), function(s) {
    d <- sp[[s]]
    if (is.null(d) || !nrow(d) || is.null(d$output_id)) return(character())
    d$output_id[!is.na(d$output_id) & !startsWith(d$output_id, "@")]
  }))
  ids <- unique(c(ids, if (!is.null(sp$report)) sp$report$output_id[!is.na(sp$report$output_id) & !startsWith(sp$report$output_id, "@")],
                  table_ids,
                  if (!is.null(ard)) ard$analyses$output_id, if (!is.null(lf)) lf$listings$output_id,
                  names(figs)))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  toc <- list()
  for (id in ids) {
    prow <- if (!is.null(plan)) plan[!is.na(plan$output_id) & plan$output_id == id, , drop = FALSE] else NULL
    rrow <- scope(sp$report, id)
    kind <- if (nrow(rrow) && !is.na(rrow$type[1L])) rrow$type[1L] else
      if (id %in% names(figs)) "figure" else if (!is.null(lf) && id %in% lf$listings$output_id) "listing" else "table"
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

    out <- .conv_output(id, kind, entry, sp, ard, lf, figs[[id]], prow, scope, page_of, content_fn, note,
                        figures = figures)
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
                       rename = character(), drop = "output_id") {
  if (is.null(d) || !nrow(d)) return(list())
  cols <- setdiff(names(d), drop)
  lapply(seq_len(nrow(d)), function(i) {
    if ("note" %in% drop && "note" %in% names(d) && !is.na(d$note[i]) && nzchar(trimws(d$note[i]))) {
      note(output, sheet, i, "note", "a row's note is not kept here (its row is a call's arguments)", d$note[i])
    }
    m <- list()
    for (cn in cols) {
      v <- .conv_cell(d[[cn]][i], cn, r_cols, list_cols, note, output, sheet, i)
      if (!is.null(v)) m[[if (cn %in% names(rename)) rename[[cn]] else cn]] <- v
    }
    m
  })
}

# a page text cell as the sheets mean it, so the YAML says what the sheets
# say: a written \n a line break; a line's left / center / right cell
# quoted its text, spaces kept ("  Indented") (.ard_spec_value(type =
# "text")); a token's value keeps its quotes (.ard_spec_tokens())
.conv_text <- function(v, unquote = TRUE) {
  if (!is.character(v) || length(v) != 1L || is.na(v) || inherits(v, "tfl_r")) return(v)
  if (unquote) v <- .ard_spec_unquote(v)
  gsub("\\n", "\n", v, fixed = TRUE)
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
        v <- .conv_text(.conv_cell(d[[cn]][i], cn, character(), character(), note, id, s, i))
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
      v <- .conv_text(.conv_cell(d$value[i], "value", character(), character(), note, id, "tokens", i),
                      unquote = FALSE)
      if (!is.null(v)) m[[d$name[i]]] <- v
    }
    page$tokens <- m
  }
  for (s in c("page", "report")) {
    d <- scope(sp[[s]], id)
    if (is.null(d) || !nrow(d)) next
    rows <- .conv_rows(d[1L, , drop = FALSE], note, id, s,
                       drop = c("output_id", "note", "type", "pattern", "file", "program"))
    for (k in names(rows[[1L]])) page <- .page_put(page, s, k, rows[[1L]][[k]], note, id)
  }
  page
}

# The page and report sheets' columns as the rtfreporter functions' arguments
# they are (design: page: by function; S1's table, 2026-10-15): a column ->
# list(fn, arg, how)
.page_map <- list(
  page = c(
    paper_size = "rtf_page", orientation = "rtf_page", width_in = "rtf_page",
    height_in = "rtf_page", margin_top_in = "rtf_page", margin_bottom_in = "rtf_page",
    margin_left_in = "rtf_page", margin_right_in = "rtf_page",
    header_dist_in = "rtf_page", footer_dist_in = "rtf_page",
    font_size_half_points = "rtf_default_format", title_format = "rtf_default_format",
    footnote_format = "rtf_default_format", title_width = "rtf_default_format",
    footnote_width = "rtf_default_format", markup = "rtf_default_format"),
  report = c(
    auto_section = "rtf_tables.auto_section", section_align = "rtf_tables.section_label_align",
    auto_title = "rtf_tables.auto_title", title_align = "rtf_tables.title_label_align",
    title_font_size_half_points = "rtf_titles.font_size_half_points",
    footnote_font_size_half_points = "rtf_footnotes.font_size_half_points",
    watermark = "rtf_document.watermark"))

.page_put <- function(page, sheet, col, v, note, id) {
  # (the data's and the content's code are not the page's: .conv_output())
  if (identical(sheet, "report") && col %in% c("data_code", "content_code")) return(page)
  if (identical(sheet, "page") && identical(col, "font")) {
    page$rtf_document$font_table <- list(list(name = v))
    return(page)
  }
  if (identical(sheet, "report") && col %in% c("page_header", "page_footer")) {
    if (identical(v, FALSE)) page[[if (col == "page_header") "header" else "footer"]] <- tfl_unset()
    return(page)
  }
  if (identical(sheet, "report") && col %in% c("figure_width_in", "figure_height_in")) {
    n <- suppressWarnings(as.numeric(v))
    arg <- if (col == "figure_width_in") "width_twips" else "height_twips"
    page$rtf_figures[[arg]] <- if (is.na(n)) v else as.integer(round(n * 1440))
    return(page)
  }
  m <- .page_map[[sheet]][col]
  if (is.na(m)) {
    # a column that is no rtfreporter argument (content_code, data_code,
    # ard_source): kept under report, said
    page$report[[col]] <- v
    note(id, sheet, 1, col, "kept under page.report (not an rtfreporter argument)", v)
    return(page)
  }
  fn <- sub("[.].*$", "", m)
  arg <- if (grepl(".", m, fixed = TRUE)) sub("^[^.]*[.]", "", m) else col
  page[[fn]][[arg]] <- v
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
      if (!is.null(r$under) && !is.na(r$under[i]) && nzchar(r$under[i])) m$under <- r$under[i]
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
.conv_output <- function(id, kind, entry, sp, ard, lf, fig, prow, scope, page_of, content_fn, note,
                         figures = "design") {
  rrow <- scope(sp$report, id)
  pattern <- if (nrow(rrow) && !is.null(rrow$pattern) && !is.na(rrow$pattern[1L])) rrow$pattern[1L]
  extends <- c(if (!is.null(pattern)) paste0("patterns/", sub("^@", "", pattern)), "defaults")
  data <- NULL
  content <- list()
  codelists <- list()
  extra <- list()
  items <- NULL
  if (kind == "figure" && !is.null(fig) && identical(figures, "items")) {
    # the design's content of function calls (R/fig_content.R); a design
    # it cannot convert stays the designer's, said
    y <- if (is.character(fig)) .conv_read_fig(fig) else fig
    items <- tryCatch(tfl_fig_content_from_design(y), error = function(e) {
      note(id, "figure", id, NA, paste("kept as the designer's pieces:", conditionMessage(e)))
      NULL
    })
  }
  if (!is.null(items)) {
    data <- items$data
    content <- items$content
    if (!is.null(items$template)) extra$template <- items$template
    if (!is.null(y$ggplot2_version)) extra$ggplot2_version <- y$ggplot2_version
  } else if (kind == "figure" && !is.null(fig)) {
    y <- if (is.character(fig)) .conv_read_fig(fig) else fig
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
      if (!is.na(lr$where[1L])) { data$where <- tfl_r(gsub("\r\n?", "\n", lr$where[1L])); note(id, "listings", 1, "where", "kept as R text (!r)", lr$where[1L]) }
      if (!is.na(lr$sort[1L])) data$sort <- as.list(.conv_split(lr$sort[1L]))
    }
    # rtfreporter's chain: plan_listing(listing_col(...), ..., type, sep ...)
    # |> plan_blanks(where = "records") |> plan_paginate_rows(max_rows)
    cols <- .conv_rows(lc, note, id, "listing_cols", list_cols = "vars", drop = c("output_id", "note"))
    args <- list(cols = lapply(cols, function(m) tfl_call("listing_col", m)))
    blank_row <- NULL
    if (nrow(lr)) {
      more <- .conv_rows(lr[1L, , drop = FALSE], note, id, "listings",
                         drop = c("output_id", "note", "dataset", "where", "sort", "max_rows", "blank_row"))[[1L]]
      args <- c(args, more)
      if (!is.na(lr$blank_row[1L]) && identical(lr$blank_row[1L], "TRUE")) blank_row <- TRUE
    }
    content <- list(tfl_item("plan_listing", args))
    # its font size (the report sheet's, on the style sheet): the plan's
    fs <- if (!is.null(sp$style) && nrow(sp$style) && "font_size_half_points" %in% names(sp$style)) {
      v <- sp$style$font_size_half_points[!is.na(sp$style$output_id) & sp$style$output_id == id]
      v[!is.na(v) & nzchar(v)]
    }
    if (length(fs)) content <- c(content, list(tfl_item("plan_style", list(font_size_half_points = as.integer(fs[1L])))))
    if (isTRUE(blank_row)) content <- c(content, list(tfl_item("plan_blanks", list(where = "records"))))
    if (nrow(lr) && !is.na(lr$max_rows[1L])) {
      content <- c(content, list(tfl_item("plan_paginate_rows", list(max_rows = as.integer(lr$max_rows[1L])))))
    }
  } else if (kind %in% c("figure", "listing")) {
    # a figure with no design, a listing with no rows yet: nothing of a
    # table's (its data its own code, or a dataset)
    data <- list(source = if (kind == "figure") "code" else "dataset")
  } else {
    # the data stage of a table: the study ARD (`object`), then its steps
    # (the rework step of the sheets, R as it was)
    data <- list(source = "ard", object = "ard")
    if (!is.null(prow) && nrow(prow) && !is.na(prow$process_code[1L])) {
      data$steps <- list(tfl_r(gsub("\r\n?", "\n", prow$process_code[1L])))
      note(id, "_tflplanner", id, "process_code", "the table's rework step kept as R text (data.steps)", prow$process_code[1L])
    }
    if (!is.null(sp)) {
      if (!is.null(content_fn)) {
        content <- .conv_items(content_fn(sp, id))
        nz <- .conv_normalize_args(sp, ard, id, note)
        if (length(nz)) content <- c(list(tfl_item("normalize_ard", nz)), content)
      }
      else if (any(vapply(c("tables", "variables", "cells", "digits", "layout", "columns", "style", "col_header", "cell_styles"),
                          function(s) nrow(scope(sp[[s]], id)) > 0L, NA))) {
        note(id, "table sheets", id, NA, "content: not converted -- no sheets-to-content bridge given (content_fn)")
      }
    }
  }
  # its code lists (a figure's ARD values too)
  if (!is.null(sp)) {
    cl <- scope(sp$codelists, id)
    if (!is.null(cl) && nrow(cl)) codelists <- .conv_codelists(cl)
  }
  # the listing/figure workbook's figures row: the datasets a report written
  # by hand reads, and whether it reads its ARD (ard TRUE)
  fr <- lf$figures
  fr <- if (is.null(fr) || !nrow(fr)) NULL else fr[!is.na(fr$output_id) & fr$output_id == id, , drop = FALSE]
  if (!is.null(fr) && nrow(fr)) {
    if ("datasets" %in% names(fr) && !is.na(fr$datasets[1L]) && nzchar(trimws(fr$datasets[1L]))) {
      data$datasets <- as.list(.conv_split(fr$datasets[1L]))
    }
    if ("ard" %in% names(fr) && identical(toupper(trimws(fr$ard[1L] %||% "")), "TRUE")) data$ard <- TRUE
  }
  # the report's ARD analyses and their analysis data, whatever its kind (a
  # figure's hazard ratios too): its ard: section, the ARD stage's
  ard_part <- NULL
  if (!is.null(ard)) {
    ob <- ard$objects
    if (!is.null(ob) && nrow(ob)) {
      ob <- ob[!is.na(ob$output_id) & ob$output_id == id, , drop = FALSE]
      if (nrow(ob)) ard_part$objects <- .conv_rows(ob, note, id, "objects", r_cols = "code",
                                                   rename = c(object_id = "id"))
    }
    ad <- ard$analysis_data
    if (!is.null(ad$output_id)) ad <- ad[!is.na(ad$output_id) & ad$output_id == id, , drop = FALSE]
    if (nrow(ad)) ard_part$analysis_data <- .conv_rows(ad, note, id, "analysis_data", r_cols = c("where", "derive", "code"),
                                                    list_cols = c("add", "keep", "distinct"), rename = c(data_id = "id", population_id = "population"))
    an <- ard$analyses[!is.na(ard$analyses$output_id) & ard$analyses$output_id == id, , drop = FALSE]
    if (nrow(an)) {
      ard_part$analyses <- .conv_rows(an, note, id, "analyses", r_cols = c("where", "args", "post", "code"),
                                      list_cols = c("by", "strata", "variables", "statistics", "formats"),
                                      rename = c(analysis_id = "id", population_id = "population"))
      # a denominator of numbers: a number, or a map by the by's values
      ard_part$analyses <- lapply(ard_part$analyses, function(m) {
        if (!is.null(m$denominator)) m$denominator <- .den_yaml(m$denominator)
        m
      })
    }
  }
  # a report whose data or content is its R code (the report row's
  # data_code / content_code): the data as code, the content as one !r
  if (nrow(rrow)) {
    dc <- if ("data_code" %in% names(rrow)) rrow$data_code[1L] else NA
    cc <- if ("content_code" %in% names(rrow)) rrow$content_code[1L] else NA
    if (!is.na(dc) && nzchar(trimws(dc))) {
      # the data as code; what else the data stage says (a figure's steps)
      # is kept beside it, and said
      if (length(setdiff(names(data), "source"))) {
        note(id, "report", id, "data_code", "the data is its code; the data stage's other parts are kept beside it")
      }
      data$source <- "code"
      data$code <- tfl_r(gsub("\r\n?", "\n", dc))
    }
    if (!is.na(cc) && nzchar(trimws(cc))) {
      if (length(content)) note(id, "report", id, "content_code", "the content is its code: the layers made from the sheets are not used")
      content <- tfl_r(gsub("\r\n?", "\n", cc))
    }
  }
  # the note of a table's tables row (free text): the report's
  tnote <- if (!is.null(sp$tables) && "note" %in% names(sp$tables)) {
    tr <- sp$tables[!is.na(sp$tables$output_id) & sp$tables$output_id == id, , drop = FALSE]
    if (nrow(tr) && !is.na(tr$note[1L]) && nzchar(trimws(tr$note[1L]))) tr$note[1L]
  }
  out <- tfl_output(id, kind, label = entry$label %||% NA, population = entry$population %||% NA,
                    extends = extends, data = data, codelists = codelists, content = content,
                    page = page_of(id), ard = ard_part)
  for (k in names(extra)) out[[k]] <- extra[[k]]
  if (!is.null(tnote)) out$note <- tnote
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

# the package's own bridge between the sheets and the layers, when it is
# there (NULL before it is)
.bridge_fn <- function(name) {
  get0(name, envir = topenv(environment()), mode = "function", inherits = FALSE)
}

# A table's normalize_ard() arguments, as tfl_normalize_code() reads them:
# its tables row (overall_label, stack, stack_sep, stack_nest) and its
# hierarchical analysis (hierarchy).  Nothing: the defaults' normalize_ard().
.conv_normalize_args <- function(sp, ard, id, note = function(...) NULL) {
  t <- sp$tables
  t <- if (is.null(t)) NULL else t[!is.na(t$output_id) & t$output_id == id, , drop = FALSE]
  val <- function(col) {
    if (is.null(t) || !nrow(t) || !col %in% names(t)) return(NULL)
    v <- t[[col]][1L]
    if (is.na(v) || !nzchar(trimws(v))) NULL else trimws(v)
  }
  out <- list()
  if (!is.null(ard)) {
    an <- ard$analyses
    an <- an[!is.na(an$output_id) & an$output_id == id &
               an$method %in% c("hierarchical", "cards::ard_stack_hierarchical",
                                "cards::ard_stack_hierarchical_count"), , drop = FALSE]
    if (nrow(an)) out$hierarchy <- .split_bar(an$variables[1L])
  }
  # no ARD definition to say it (an ARD made elsewhere): a table whose rows
  # are groups and whose label is a variable is a hierarchy -- its groups,
  # then the label's variable (rows `group1 = AEBODSYS`, label `label =
  # AEDECOD`: SOC / PT); said, to check
  has_ard <- !is.null(ard) && any(!is.na(ard$analyses$output_id) & ard$analyses$output_id == id)
  if (is.null(out$hierarchy) && !has_ard) {
    rows <- val("rows")
    lv <- val("label")
    grp <- character()
    if (!is.null(rows)) {
      for (p in trimws(strsplit(rows, "|", fixed = TRUE)[[1L]])) {
        kv <- trimws(strsplit(p, "=", fixed = TRUE)[[1L]])
        if (length(kv) == 2L && grepl("^group[0-9]+$", kv[1L])) grp[kv[1L]] <- kv[2L]
      }
    }
    lab_var <- if (!is.null(lv)) {
      kv <- trimws(strsplit(lv, "=", fixed = TRUE)[[1L]])
      v <- kv[length(kv)]
      if (length(kv) %in% 1:2 && grepl("^[A-Za-z][A-Za-z0-9_.]*$", v) && !v %in% c(".label", "NA")) v
    }
    if (length(grp) && !is.null(lab_var)) {
      grp <- grp[order(as.integer(sub("^group", "", names(grp))))]
      out$hierarchy <- unname(c(grp, lab_var))
      note(id, "tables", 1, "rows", paste0(
        "normalize_ard()'s hierarchy taken from the table's rows and label (no hierarchical ",
        "analysis in an ARD definition): check it"), paste(out$hierarchy, collapse = " | "))
    }
  }
  lab <- val("overall_label")
  if (!is.null(lab)) out$overall <- lab
  stk <- val("stack")
  if (!is.null(stk)) {
    g <- .ard_spec_stat_groups(stk, "stack", "layer", "subgroup = ALL, SEX, AGEGR1, AGEGR1 * SEX")
    out$stack <- lapply(g, function(v) {
      parts <- lapply(strsplit(v, "*", fixed = TRUE), trimws)
      if (all(lengths(parts) == 1L)) unlist(parts) else parts
    })
    sep <- val("stack_sep")
    if (!is.null(sep)) out$stack_sep <- .ard_spec_unquote(sep)
    if (identical(toupper(val("stack_nest") %||% ""), "TRUE")) out$stack_nest <- TRUE
  }
  out
}

# An old figure design (spec/figures/<id>.yml) read as the designer wrote and
# read it (tfl_read_fig_design()): YAML 1.1, so its yes / no are TRUE / FALSE
# (x_log: yes, na.rm: yes), unlike the new spec's reader
.conv_read_fig <- function(path) yaml::read_yaml(path, handlers = list(r = tfl_r))

# the data frames of a list of sheets with their text's CRLF / CR as LF
.conv_lf_sheets <- function(x) {
  if (is.null(x)) return(x)
  cls <- class(x)
  x <- lapply(unclass(x), function(d) {
    if (!is.data.frame(d)) return(d)
    for (j in seq_along(d)) if (is.character(d[[j]])) d[[j]] <- gsub("\r\n?", "\n", d[[j]])
    d
  })
  class(x) <- cls
  x
}

# The report sheet's table_font_size_half_points as the style sheet's
# font_size_half_points of the same report (or the study's row), where that
# is blank; the report column then blank
.conv_font_to_style <- function(sp) {
  r <- sp$report
  if (is.null(r) || !nrow(r) || !"table_font_size_half_points" %in% names(r)) return(sp)
  cls <- class(sp)
  st <- sp$style
  if (is.null(st)) st <- data.frame(output_id = character(), stringsAsFactors = FALSE)
  if (!"font_size_half_points" %in% names(st)) st$font_size_half_points <- rep(NA_character_, nrow(st))
  if (!"output_id" %in% names(st)) st$output_id <- rep(NA_character_, nrow(st))
  same <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  for (i in seq_len(nrow(r))) {
    v <- r$table_font_size_half_points[i]
    if (is.na(v) || !nzchar(trimws(v))) next
    if (identical(r$type[i] %||% NA_character_, "figure")) next
    id <- r$output_id[i]
    if (!is.na(id) && !nzchar(id)) id <- NA_character_
    k <- which(same(st$output_id, id))
    if (!length(k)) {
      st[nrow(st) + 1L, ] <- NA
      k <- nrow(st)
      st$output_id[k] <- id
    }
    k <- k[1L]
    if (is.na(st$font_size_half_points[k]) || !nzchar(st$font_size_half_points[k])) {
      st$font_size_half_points[k] <- as.character(v)
    }
    r$table_font_size_half_points[i] <- NA_character_
  }
  sp$report <- r
  sp$style <- st
  class(sp) <- cls
  sp
}
