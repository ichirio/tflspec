# The YAML spec as the sheets tflplanner's screens still work on (stage 1's
# adapter): the inverse of the one-time conversion.  The report list and
# the page parts come back as the report sheets and the `_tflplanner`
# sheet, a report's `content:` as the table sheets through the bridge
# (`.plan_sheets_from_content()`), the data as the ARD sheets, a listing's
# items as the listing sheets, a figure's parts as its design list.

#' The YAML spec as sheets
#'
#' Reads a study's YAML spec and gives the sheets the workbooks held: for
#' the app's grids and for the generators that still read sheets.  The
#' study's defaults are the rows with a blank `output_id`, a pattern's the
#' `@<name>` rows, a report's its own rows (the files as written, not
#' layered -- the sheets' readers layer them as they always did).
#'
#' @param dir The study folder (or its `spec/`).
#' @return A list: `table` (the table and report sheets, with `study`),
#'   `planner` (the `_tflplanner` sheet: the report list and the setup),
#'   `ard` (the ARD sheets), `listing` (`listings`, `listing_cols`),
#'   `figures` (a figure's design list, by id), `study` (study.yml's
#'   fields), `not_converted` (what the sheets cannot hold).
#' @export
tfl_sheets_from_yaml <- function(dir) {
  s <- tfl_read_study_spec(dir)
  outs <- tfl_read_outputs(dir)
  bridge <- .bridge_fn(".plan_sheets_from_content")
  nc <- list()
  note <- function(output, part, reason) nc[[length(nc) + 1L]] <<- data.frame(
    output = .na_scalar(output), part = part, reason = reason, stringsAsFactors = FALSE)
  rows <- list()
  add <- function(sheet, r) { if (length(r)) rows[[sheet]] <<- c(rows[[sheet]], list(r)) }
  cell <- function(v) .sheet_cell(v)

  # -- the table sheets: the bridge, scope by scope --------------------------
  table_rows <- function(items, id) {
    if (!length(items)) return()
    if (is.null(bridge)) { note(id, "content", "no bridge from the layers to the sheets (tflspec's .plan_sheets_from_content)"); return() }
    sp <- bridge(items, if (is.na(id)) NULL else id)
    for (sh in names(sp)) {
      d <- sp[[sh]]
      if (!is.data.frame(d) || !nrow(d)) next
      if (!"output_id" %in% names(d)) d$output_id <- id
      d$output_id[is.na(d$output_id) & !is.na(id)] <- id
      for (i in seq_len(nrow(d))) add(sh, as.list(d[i, , drop = FALSE]))
    }
    for (x in attr(sp, "not_converted") %||% character()) note(id, "content", x)
  }
  table_rows(s$defaults$content, NA_character_)
  for (nm in names(s$patterns)) table_rows(s$patterns[[nm]]$content, paste0("@", nm))

  # -- the page parts ----------------------------------------------------------
  page_rows <- function(page, id) {
    for (band in c("header", "footer", "titles", "footnotes")) {
      m <- page[[band]]
      for (line in names(m)) {
        cells <- m[[line]]
        r <- list(output_id = id, line = line)
        if (inherits(cells, "tfl_unset")) { r$left <- "(none)"; add(band, r); next }
        for (cn in c("left", "center", "right")) if (!is.null(cells[[cn]])) r[[cn]] <- cell(cells[[cn]])
        add(band, r)
      }
    }
    for (nm in names(page$tokens)) add("tokens", list(output_id = id, name = nm, value = cell(page$tokens[[nm]])))
    for (sh in c("page", "report")) {
      m <- page[[sh]]
      if (length(m)) {
        r <- list(output_id = id)
        for (k in names(m)) r[[k]] <- cell(m[[k]])
        add(sh, r)
      }
    }
    for (k in setdiff(names(page), c("header", "footer", "titles", "footnotes", "tokens", "page", "report"))) {
      note(id, paste0("page.", k), "the document's calls (rtf_*) have no sheet yet")
    }
  }
  page_rows(s$defaults$page, NA_character_)
  for (nm in names(s$patterns)) page_rows(s$patterns[[nm]]$page, paste0("@", nm))

  # -- the code list templates --------------------------------------------------
  for (v in names(s$codelists)) for (i in seq_along(s$codelists[[v]])) {
    e <- s$codelists[[v]][[i]]
    add("codelists", list(output_id = NA_character_, variable = v, value = cell(e$value), label = cell(e$label), order = as.character(i)))
  }

  # -- the ARD's study-wide sheets ----------------------------------------------
  ard <- list()
  keys <- s$study$keys %||% list()
  ard$study <- data.frame(key = names(keys), value = vapply(keys, cell, ""), stringsAsFactors = FALSE)
  ard$datasets <- .rows_df(lapply(s$data$datasets %||% list(), function(e) lapply(e, cell)), .ard_spec_sheets$datasets)
  ard$populations <- .rows_df(lapply(s$data$populations %||% list(), function(e) {
    m <- lapply(e, cell); if (!is.null(m$id)) { m$population_id <- m$id; m$id <- NULL }; m
  }), .ard_spec_sheets$populations)
  adata <- list(); analyses <- list()
  listings <- list(); listing_cols <- list()
  figures <- list()
  toc_index <- stats::setNames(s$toc$outputs %||% list(), vapply(s$toc$outputs %||% list(), function(e) e$id %||% "", ""))

  # -- each report ----------------------------------------------------------------
  planner <- list()
  for (id in names(outs)) {
    o <- outs[[id]]
    e <- toc_index[[id]] %||% list()
    planner[[length(planner) + 1L]] <- list(
      output_id = id, description = cell(e$label %||% o$label), setup_code = NA_character_,
      process_code = .first_r_step(o$data$steps),
      section = cell(e$section), population = cell(e$population %||% o$population),
      datasets = cell(e$datasets), batches = cell(e$sets))
    # the report row: its kind, its pattern, its file and program
    rr <- list(output_id = id, type = o$kind)
    pat <- grep("^patterns/", o$extends, value = TRUE)
    if (length(pat)) rr$pattern <- paste0("@", substring(pat[1L], 10L))
    for (k in c("file", "program")) if (!is.null(e[[k]])) rr[[k]] <- cell(e[[k]])
    own_report <- o$page$report
    o$page$report <- NULL
    for (k in names(own_report)) rr[[k]] <- cell(own_report[[k]])
    add("report", rr)
    page_rows(o$page, id)
    for (v in names(o$codelists)) for (i in seq_along(o$codelists[[v]])) {
      cl <- o$codelists[[v]][[i]]
      add("codelists", list(output_id = id, variable = v, value = cell(cl$value), label = cell(cl$label), order = as.character(i)))
    }
    if (o$kind == "table") {
      table_rows(o$content, id)
      for (a in o$data$analysis_data %||% list()) {
        m <- lapply(a, cell); m$data_id <- m$id; m$id <- NULL
        if (!is.null(m$population)) { m$population_id <- m$population; m$population <- NULL }
        adata[[length(adata) + 1L]] <- c(list(output_id = id), m)
      }
      for (a in o$data$analyses %||% list()) {
        m <- lapply(a, cell); m$analysis_id <- m$id; m$id <- NULL
        if (!is.null(m$population)) { m$population_id <- m$population; m$population <- NULL }
        if (!is.null(a$ref)) note(id, paste0("analyses.", a$id), "a reference to another report's analysis (ref) has no sheet")
        analyses[[length(analyses) + 1L]] <- c(list(output_id = id), m)
      }
      if (!is.null(o$data$code)) note(id, "data.code", "a report whose data is its code: kept as the table's data step is not a sheet")
    } else if (o$kind == "listing") {
      lr <- list(output_id = id)
      for (k in c("dataset", "where", "sort")) if (!is.null(o$data[[k]])) lr[[k]] <- cell(o$data[[k]])
      for (it in o$content) {
        if (identical(it$fn, "listing_col")) listing_cols[[length(listing_cols) + 1L]] <- c(list(output_id = id), lapply(it$args, cell))
        else if (identical(it$fn, "listing_spec")) for (k in names(it$args)) lr[[k]] <- cell(it$args[[k]])
        else if (identical(it$fn, "paginate")) { if (!is.null(it$args$max_rows)) lr$max_rows <- cell(it$args$max_rows) }
        else note(id, paste0("content.", it$fn), "a listing item the listing sheets do not hold")
      }
      listings[[length(listings) + 1L]] <- lr
    } else if (o$kind == "figure") {
      fd <- list()
      for (k in c("template", "ggplot2_version")) if (!is.null(o[[k]])) fd[[k]] <- o[[k]]
      if (!is.null(o$data$steps)) fd$data <- o$data$steps
      if (!is.null(o$data$stats)) fd$stats <- o$data$stats
      if (!is.null(o$plot)) fd$plot <- o$plot
      fd$layers <- lapply(o$content, function(it) c(list(layer = it$fn), it$args))
      for (k in c("plots", "compose")) if (!is.null(o[[k]])) fd[[k]] <- o[[k]]
      figures[[id]] <- fd
    }
  }
  # the setup code on the planner sheet's study row
  planner <- c(list(list(output_id = NA_character_, description = "(every report)",
                         setup_code = if (!is.null(s$study$setup)) cell(s$study$setup) else NA_character_,
                         process_code = NA_character_, section = NA_character_, population = NA_character_,
                         datasets = NA_character_, batches = NA_character_)), planner)
  ard$analysis_data <- .rows_df(adata, c("output_id", .ard_spec_sheets$analysis_data))
  ard$analyses <- .rows_df(analyses, .ard_spec_sheets$analyses)

  # the table and report sheets as data frames (the study sheet from study.yml's settings)
  table <- list()
  settings <- s$study$settings %||% list()
  table$study <- data.frame(key = names(settings), value = vapply(settings, cell, ""), stringsAsFactors = FALSE)
  for (sh in c(names(.ard_spec_schema()), .ard_spec_pattern_sheets)) {
    table[[sh]] <- .rows_df(rows[[sh]] %||% list(), c("output_id", setdiff(.sheet_columns(sh), "output_id")))
  }
  list(table = table, planner = .rows_df(planner, c("output_id", "description", "setup_code", "process_code",
                                                     "section", "population", "datasets", "batches")),
       ard = ard,
       listing = list(listings = .rows_df(listings, c("output_id", .listing_sheets$listings)),
                      listing_cols = .rows_df(listing_cols, c("output_id", .listing_sheets$listing_cols))),
       figures = figures, study = s$study,
       not_converted = if (length(nc)) do.call(rbind, nc) else
         data.frame(output = character(), part = character(), reason = character(), stringsAsFactors = FALSE))
}

# a sheet's columns as the readers know them (the schema's, else the
# report half's)
.sheet_columns <- function(sh) {
  sc <- .ard_spec_schema()[[sh]]
  if (!is.null(sc)) return(sc)
  switch(sh,
    report = c("output_id", "type", "file", "program", "pattern"),
    page = "output_id",
    header = , footer = , titles = , footnotes = c("output_id", "line", "left", "center", "right"),
    tokens = c("output_id", "name", "value"),
    "output_id")
}

# a value as a sheet's cell: a vector `a | b`, an !r its text, an !unset
# "(none)", a logical TRUE / FALSE, a map or a call: not a cell (NA)
.sheet_cell <- function(v) {
  if (is.null(v)) return(NA_character_)
  if (inherits(v, "tfl_unset")) return("(none)")
  if (inherits(v, "tfl_r")) return(as.character(v))
  if (inherits(v, "tfl_call")) return(NA_character_)
  if (is.list(v)) {
    if (!is.null(names(v))) return(NA_character_)
    v <- unlist(v)
  }
  if (is.logical(v)) v <- ifelse(v, "TRUE", "FALSE")
  paste(as.character(v), collapse = " | ")
}

# rows (named lists of cells) as a data frame of text with the columns
# given first, then the rows' others
.rows_df <- function(rows, cols) {
  more <- setdiff(unique(unlist(lapply(rows, names))), cols)
  cols <- c(cols, more)
  d <- as.data.frame(lapply(cols, function(cn) vapply(rows, function(r) {
    v <- r[[cn]]
    if (is.null(v) || !length(v)) NA_character_ else as.character(v)[1L]
  }, "")), stringsAsFactors = FALSE)
  names(d) <- cols
  d
}

# a table's rework step: the first `!r` among its data steps (a figure's
# steps are maps, not R)
.first_r_step <- function(steps) {
  for (s in steps %||% list()) if (inherits(s, "tfl_r")) return(as.character(s))
  NA_character_
}
