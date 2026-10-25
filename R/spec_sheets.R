# The YAML spec as the sheets tflplanner's screens still work on (stage 1's
# adapter): the inverse of the one-time conversion.  The report list and
# the page parts come back as the report sheets and the `_tflplanner`
# sheet, a report's `content:` as the table sheets through the bridge
# (`.plan_sheets_from_content()`), the data as the ARD sheets, a listing's
# items as the listing sheets, a figure's parts as its design list.

#' The YAML spec as sheets
#'
#' `tfl_sheets_from_yaml()` reads a study's YAML spec and gives the sheets
#' the workbooks held: for the app's grids and for the generators that
#' still read sheets.  The study's defaults are the rows with a blank
#' `output_id`, a pattern's the `@<name>` rows, a report's its own rows (the
#' files as written, not layered -- the sheets' readers layer them as they
#' always did).  `tfl_sheets_from_spec()` does the same from a spec already
#' read (or changed in memory, not written yet): `tfl_sheets_from_yaml(dir)`
#' is `tfl_sheets_from_spec(tfl_read_study_spec(dir), tfl_read_outputs(dir))`.
#'
#' @param dir The study folder (or its `spec/`).
#' @param study The study's spec, as [tfl_read_study_spec()] reads it.
#' @param outputs The reports, as [tfl_read_outputs()] reads them: a list of
#'   `tfl_output` (as written, not resolved), named by id.
#' @return A list: `table` (the table and report sheets, with `study`),
#'   `planner` (the `_tflplanner` sheet: the report list and the setup),
#'   `ard` (the ARD sheets), `listing` (`listings`, `listing_cols`),
#'   `figures` (a figure's design list, by id), `study` (study.yml's
#'   fields), `not_converted` (what the sheets cannot hold).
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' sh <- tfl_sheets_from_yaml(d)
#' sh$planner$output_id
#' # the same from the spec in memory, a title changed and not written
#' s <- tfl_read_study_spec(d)
#' outs <- tfl_read_outputs(d)
#' outs[["T-14-1-1"]]$label <- "Demographics (draft)"
#' tfl_sheets_from_spec(s, outs)$planner$description
#' @export
tfl_sheets_from_yaml <- function(dir) {
  tfl_sheets_from_spec(tfl_read_study_spec(dir), tfl_read_outputs(dir))
}

#' @rdname tfl_sheets_from_yaml
#' @export
tfl_sheets_from_spec <- function(study, outputs) {
  if (!inherits(study, "tfl_study_spec")) {
    .ard_stop("tfl_sheets_from_spec(): `study` is a study's spec, tfl_read_study_spec().")
  }
  if (!is.list(outputs) || inherits(outputs, "tfl_output") ||
      !all(vapply(outputs, inherits, NA, "tfl_output"))) {
    .ard_stop("tfl_sheets_from_spec(): `outputs` is a list of reports, tfl_read_outputs().")
  }
  s <- study
  outs <- outputs
  # the TOC's order (the sheets' rows, the ARD program's sections), then the rest
  toc_ids <- vapply(s$toc$outputs %||% list(), function(e) e$id %||% NA_character_, "")
  ids <- vapply(outs, function(o) o$id, "")
  outs <- outs[order(match(ids, toc_ids, nomatch = length(toc_ids) + 1L), seq_along(ids))]
  # a template SPEC: the sheets hold its items (tfl_template_to_spec()),
  # the ARD's analyses among them
  # (one that cannot be expanded -- a KM type with no `data:` steps -- stays
  # as it is, its template and args kept, and is named in a warning: the
  # study still reads, the report can be opened and fixed; running it says
  # the error again)
  outs <- lapply(outs, function(o) {
    if (is.null(.tpl_spec_of(o))) return(o)
    tryCatch(tfl_template_to_spec(o, s), error = function(e) {
      warning(sprintf("%s: kept as it is (its template not expanded): %s", o$id,
                      conditionMessage(e)), call. = FALSE)
      o
    })
  })
  bridge <- .bridge_fn(".plan_sheets_from_content")
  nc <- list()
  note <- function(output, part, reason) nc[[length(nc) + 1L]] <<- data.frame(
    output = .na_scalar(output), part = part, reason = reason, stringsAsFactors = FALSE)
  rows <- list()
  add <- function(sheet, r) { if (length(r)) rows[[sheet]] <<- c(rows[[sheet]], list(r)) }
  cell <- function(v) .sheet_cell(v)

  # -- the table sheets: the bridge, scope by scope --------------------------
  table_rows <- function(items, id) {
    # normalize_ard() is the tables row's overall_label / stack columns (its
    # hierarchy is the ARD's analysis): taken out before the bridge
    is_nz <- vapply(items, function(it) identical(it$fn, "normalize_ard"), NA)
    nz <- items[is_nz]
    items <- items[!is_nz]
    if (length(nz) && !is.na(id) && length(nz[[1L]]$args)) norm_rows[[id]] <<- nz[[1L]]$args
    # (a second normalize_ard(): kept with the items the sheets cannot say)
    more <- nz[-1L]
    nz <- nz[seq_len(min(1L, length(nz)))]
    key <- if (is.na(id)) ".defaults" else id
    if (!length(items)) { if (length(more)) more_rows[[key]] <<- more; return() }
    if (is.null(bridge)) { note(id, "content", "no bridge from the layers to the sheets (tflspec's .plan_sheets_from_content)"); return() }
    # (the bridge reads normalize_ard()'s stack: the columns the plan makes)
    # an item any of whose arguments the sheets cannot say goes whole, as
    # YAML, in the tables row's content_more -- never on both (#448): taken
    # out, and the bridge run again on the rest
    keep <- seq_along(items)
    repeat {
      sp <- bridge(c(nz, items[keep]), if (is.na(id)) NULL else id)
      out <- attr(sp, "not_converted_at") %||% integer()
      if (!length(out)) break
      keep <- keep[-out]
      if (!length(keep)) { sp <- list(); break }
    }
    more <- c(more, items[setdiff(seq_along(items), keep)])
    if (length(more)) more_rows[[key]] <<- more
    for (sh in names(sp)) {
      d <- sp[[sh]]
      if (!is.data.frame(d) || !nrow(d)) next
      if (!"output_id" %in% names(d)) d$output_id <- id
      d$output_id[is.na(d$output_id) & !is.na(id)] <- id
      for (i in seq_len(nrow(d))) add(sh, as.list(d[i, , drop = FALSE]))
    }
    for (x in attr(sp, "not_converted") %||% character()) note(id, "content", x)
  }
  norm_rows <- list()
  more_rows <- list()
  table_rows(s$defaults$content, NA_character_)
  for (nm in names(s$patterns)) table_rows(s$patterns[[nm]]$content, paste0("@", nm))

  # -- the page parts ----------------------------------------------------------
  page_rows <- function(page, id) {
    for (band in c("header", "footer", "titles", "footnotes")) {
      m <- page[[band]]
      if (inherits(m, "tfl_unset")) next
      for (line in names(m)) {
        cells <- m[[line]]
        r <- list(output_id = id, line = line)
        if (inherits(cells, "tfl_unset")) { r$left <- "(none)"; add(band, r); next }
        for (cn in c("left", "center", "right")) if (!is.null(cells[[cn]])) r[[cn]] <- cell(cells[[cn]])
        add(band, r)
      }
    }
    for (nm in names(page$tokens)) add("tokens", list(output_id = id, name = nm, value = cell(page$tokens[[nm]])))
    # the rtfreporter functions' arguments back as the page and report columns
    pr <- .page_columns(page)
    for (sh in c("page", "report")) if (length(pr[[sh]])) add(sh, c(list(output_id = id), pr[[sh]]))
    for (k in setdiff(names(page), .page_known)) note(id, paste0("page.", k), "no sheet for it")
  }
  page_rows(s$defaults$page, NA_character_)
  for (nm in names(s$patterns)) page_rows(s$patterns[[nm]]$page, paste0("@", nm))

  # -- the code list templates --------------------------------------------------
  for (v in names(s$codelists)) for (i in seq_along(s$codelists[[v]])) {
    e <- s$codelists[[v]][[i]]
    add("codelists", list(output_id = NA_character_, variable = v, value = cell(e$value), label = cell(e$label), order = as.character(i),
                          under = cell(e$under)))
  }

  # -- the ARD's study-wide sheets ----------------------------------------------
  ard <- list()
  keys <- s$study$keys %||% list()
  ard$study <- data.frame(key = names(keys), value = vapply(keys, cell, ""), stringsAsFactors = FALSE)
  ard$datasets <- .rows_df(lapply(s$data$datasets %||% list(), function(e) lapply(e, cell)), .ard_spec_sheets$datasets)
  ard$populations <- .rows_df(lapply(s$data$populations %||% list(), function(e) {
    m <- lapply(e, cell); if (!is.null(m$id)) { m$population_id <- m$id; m$id <- NULL }; m
  }), .ard_spec_sheets$populations)
  # named sets of records (data.yml `records:`)
  ard$records <- .rows_df(lapply(s$data$records %||% list(), function(e) {
    m <- lapply(e, cell); if (!is.null(m$id)) { m$record_id <- m$id; m$id <- NULL }; m
  }), .ard_spec_sheets$records)
  # named values: the study's (data.yml `objects:`), then the reports'
  objs <- lapply(s$data$objects %||% list(), function(e) {
    m <- lapply(e, cell); m$object_id <- m$id; m$id <- NULL; c(list(output_id = NA_character_), m)
  })
  # condition-defined groups: a row a level (data.yml `groups:`, each with
  # its `levels:`, a level and the condition its records meet)
  ard$groups <- .rows_df(unlist(lapply(s$data$groups %||% list(), function(g) {
    lapply(g$levels %||% list(), function(l) {
      list(group_id = cell(g$id), label = cell(g$label), level = cell(l$level),
           where = cell(l$where))
    })
  }), recursive = FALSE), .ard_spec_sheets$groups)
  adata <- list(); analyses <- list()
  listings <- list(); listing_cols <- list(); fig_rows <- list()
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
    # the report row: its kind, pattern, file and program here, its
    # rtfreporter arguments from page_rows() -- one row
    before <- length(rows$report %||% list())
    page_rows(o$page, id)
    after <- rows$report %||% list()
    if (length(after) > before) {
      extra <- after[[length(after)]]
      for (k in setdiff(names(extra), "output_id")) rr[[k]] <- extra[[k]]
      rows$report <- after[-length(after)]
    }
    if (inherits(o$content, "tfl_r")) rr$content_code <- cell(o$content)
    if (identical(o$data$source, "code") && !is.null(o$data$code)) rr$data_code <- cell(o$data$code)
    add("report", rr)
    for (v in names(o$codelists)) for (i in seq_along(o$codelists[[v]])) {
      cl <- o$codelists[[v]][[i]]
      add("codelists", list(output_id = id, variable = v, value = cell(cl$value), label = cell(cl$label), order = as.character(i),
                            under = cell(cl$under)))
    }
    code_content <- inherits(o$content, "tfl_r")
    # the datasets a report written by hand reads, and whether it reads its
    # ARD: the listing/figure workbook's figures row
    if (!is.null(o$data$datasets) || isTRUE(o$data$ard)) {
      fig_rows[[length(fig_rows) + 1L]] <- list(
        output_id = id, datasets = if (!is.null(o$data$datasets)) paste(unlist(o$data$datasets), collapse = " | ") else NA_character_,
        ard = if (isTRUE(o$data$ard)) "TRUE" else NA_character_)
    }
    # the report's ARD analyses and analysis data, whatever its kind
    for (a in o$ard$analysis_data %||% list()) {
      m <- lapply(a, cell); m$data_id <- m$id; m$id <- NULL
      if (!is.null(m$population)) { m$population_id <- m$population; m$population <- NULL }
      adata[[length(adata) + 1L]] <- c(list(output_id = id), m)
    }
    for (e in o$ard$objects %||% list()) {
      m <- lapply(e, cell); m$object_id <- m$id; m$id <- NULL
      objs[[length(objs) + 1L]] <- c(list(output_id = id), m)
    }
    for (a in o$ard$analyses %||% list()) {
      if (!is.null(a$denominator)) a$denominator <- .den_cell(a$denominator)
      # (`by: []`: no columns, said so)
      if ("by" %in% names(a) && !length(a$by)) a$by <- tfl_unset()
      m <- lapply(a, cell); m$analysis_id <- m$id; m$id <- NULL
      if (!is.null(m$population)) { m$population_id <- m$population; m$population <- NULL }
      if (!is.null(a$ref)) note(id, paste0("analyses.", a$id), "a reference to another report's analysis (ref) has no sheet")
      analyses[[length(analyses) + 1L]] <- c(list(output_id = id), m)
    }
    if (o$kind == "table") {
      if (!code_content) table_rows(o$content, id)
    } else if (o$kind == "listing") {
      lr <- list(output_id = id)
      for (k in c("dataset", "data", "where", "sort")) if (!is.null(o$data[[k]])) lr[[k]] <- cell(o$data[[k]])
      if (!is.null(o$data$population)) lr$population_id <- cell(o$data$population)
      # what the listing sheets' columns cannot say is kept as YAML: the
      # items in the listing's content_more, a column's other arguments in
      # its row's more (read back by .conv_output(): nothing lost)
      more <- list()
      said <- character()
      for (it in if (code_content) list() else o$content) {
        a <- it$args
        if (identical(it$fn, "plan_listing") && !"plan_listing" %in% said) {
          said <- c(said, it$fn)
          for (cl in a$cols %||% list()) {
            if (!inherits(cl, "tfl_call")) next
            row <- c(list(output_id = id), lapply(cl$args[intersect(names(cl$args), .listing_col_said)], cell))
            rest <- cl$args[setdiff(names(cl$args), .listing_col_said)]
            if (length(rest)) row$more <- .sheet_more_out(rest)
            listing_cols[[length(listing_cols) + 1L]] <- row
          }
          ks <- setdiff(names(a), "cols")
          for (k in intersect(ks, .listing_row_said)) lr[[k]] <- cell(a[[k]])
          if (length(setdiff(ks, .listing_row_said))) {
            more <- c(more, list(tfl_item("plan_listing", a[setdiff(ks, .listing_row_said)])))
          }
        } else if (identical(it$fn, "plan_blanks") && identical(names(a), "where") &&
                   identical(a$where, "records") && is.null(lr$blank_row)) {
          lr$blank_row <- "TRUE"
        } else if (identical(it$fn, "plan_paginate_rows") && identical(names(a), "max_rows") &&
                   is.null(lr$max_rows)) {
          lr$max_rows <- cell(a$max_rows)
        } else more <- c(more, list(it))
      }
      if (length(more)) lr$content_more <- .sheet_more_out(more, items = TRUE)
      listings[[length(listings) + 1L]] <- lr
    } else if (o$kind == "figure" && .fig_new_content(o$content)) {
      # a figure of items (R/fig_content.R): its data and items as they are
      figures[[id]] <- list(template = o$template, data = o$data$steps,
                            content = .items_to_list(o$content))
    } else if (o$kind == "figure") {
      # (the report's design: an item of a function call its piece)
      oc <- o
      if (code_content) oc$content <- list()
      fd <- .fig_design_of_output(oc)
      if (!is.null(o$template)) fd$template <- o$template
      # a figure that is its own code and nothing of the designer's has no
      # design
      if (!(code_content && identical(names(fd), "layers"))) figures[[id]] <- fd
    }
  }
  # the setup code on the planner sheet's study row
  planner <- c(list(list(output_id = NA_character_, description = "(every report)",
                         setup_code = if (!is.null(s$study$setup)) cell(s$study$setup) else NA_character_,
                         process_code = NA_character_, section = NA_character_, population = NA_character_,
                         datasets = NA_character_, batches = NA_character_)), planner)
  ard$objects <- .rows_df(objs, .ard_spec_sheets$objects)
  ard$analysis_data <- .rows_df(adata, c("output_id", .ard_spec_sheets$analysis_data))
  ard$analyses <- .rows_df(analyses, .ard_spec_sheets$analyses)

  # the table and report sheets as data frames (the study sheet from study.yml's settings)
  table <- list()
  settings <- s$study$settings %||% list()
  # every report's file and program go back to the report sheet's study row
  fp <- settings[intersect(c("file", "program"), names(settings))]
  settings <- settings[setdiff(names(settings), c("file", "program"))]
  table$study <- data.frame(key = as.character(names(settings)), value = vapply(settings, cell, ""), stringsAsFactors = FALSE)
  for (sh in c(names(.ard_spec_schema()), .ard_spec_pattern_sheets)) {
    table[[sh]] <- .rows_df(rows[[sh]] %||% list(), c("output_id", setdiff(.sheet_columns(sh), "output_id")))
  }
  if (length(fp)) {
    rp <- table$report
    i <- which(is.na(rp$output_id))
    if (!length(i)) { rp[nrow(rp) + 1L, ] <- NA; i <- nrow(rp) }
    for (k in names(fp)) { if (!k %in% names(rp)) rp[[k]] <- NA_character_; rp[[k]][i[1L]] <- cell(fp[[k]]) }
    table$report <- rp
  }
  # a report's note: its tables row's
  for (id in names(outs)) {
    nt <- outs[[id]]$note
    if (is.null(nt) || !identical(outs[[id]]$kind, "table")) next
    tb <- table$tables
    i <- match(id, tb$output_id)
    if (is.na(i)) { tb[nrow(tb) + 1L, ] <- NA; i <- nrow(tb); tb$output_id[i] <- id }
    if (!"note" %in% names(tb)) tb$note <- NA_character_
    tb$note[i] <- as.character(nt)
    table$tables <- tb
  }
  # a table's normalize_ard() arguments on its tables row
  for (id in names(norm_rows)) {
    a <- norm_rows[[id]]
    tb <- table$tables
    i <- match(id, tb$output_id)
    if (is.na(i)) { tb[nrow(tb) + 1L, ] <- NA; i <- nrow(tb); tb$output_id[i] <- id }
    put <- function(col, v) { if (!col %in% names(tb)) tb[[col]] <<- NA_character_; tb[[col]][i] <<- v }
    if (!is.null(a$overall)) put("overall_label", as.character(a$overall))
    if (!is.null(a$stack)) {
      put("stack", paste(vapply(names(a$stack), function(n) {
        v <- a$stack[[n]]
        parts <- if (is.list(v)) vapply(v, function(p) paste(unlist(p), collapse = " * "), "") else v
        paste0(n, " = ", paste(parts, collapse = ", "))
      }, ""), collapse = " | "))
      if (!is.null(a$stack_sep)) put("stack_sep", encodeString(as.character(a$stack_sep), quote = "\""))
      if (isTRUE(a$stack_nest)) put("stack_nest", "TRUE")
    }
    table$tables <- tb
  }
  # a table's items the sheets cannot say on its tables row (content_more)
  for (key in names(more_rows)) {
    tb <- table$tables
    oid <- if (identical(key, ".defaults")) NA_character_ else key
    i <- match(oid, tb$output_id)
    if (is.na(i)) { tb[nrow(tb) + 1L, ] <- NA; i <- nrow(tb); tb$output_id[i] <- oid }
    if (!"content_more" %in% names(tb)) tb$content_more <- NA_character_
    tb$content_more[i] <- .sheet_more_out(more_rows[[key]], items = TRUE)
    table$tables <- tb
  }
  list(table = table, planner = .rows_df(planner, c("output_id", "description", "setup_code", "process_code",
                                                     "section", "population", "datasets", "batches")),
       ard = ard,
       listing = list(listings = .rows_df(listings, c("output_id", .listing_sheets$listings)),
                      listing_cols = .rows_df(listing_cols, c("output_id", .listing_sheets$listing_cols)),
                      figures = .rows_df(fig_rows, c("output_id", "datasets", "ard"))),
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
  # (a field left blank: a blank cell, not the text NA)
  if (is.atomic(v) && length(v) == 1L && is.na(v)) return(NA_character_)
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

.page_known <- c("header", "footer", "titles", "footnotes", "tokens", "page", "report",
                 "rtf_page", "rtf_default_format", "rtf_document", "rtf_tables", "rtf_titles",
                 "rtf_footnotes", "rtf_figures", "rtf_header", "rtf_footer", "generate_rtfreport")

# `page:`'s rtfreporter arguments as the page and report sheets' columns
# (the inverse of the converter's .page_put()): list(page = , report = ),
# each a named list of cells
.page_columns <- function(page) {
  cell <- .sheet_cell
  pr <- list(page = list(), report = list())
  for (sh in names(.page_map)) {
    m <- .page_map[[sh]]
    for (col in names(m)) {
      fn <- sub("[.].*$", "", m[[col]])
      arg <- if (grepl(".", m[[col]], fixed = TRUE)) sub("^[^.]*[.]", "", m[[col]]) else col
      v <- page[[fn]][[arg]]
      if (!is.null(v)) pr[[sh]][[col]] <- cell(v)
    }
  }
  ft <- page$rtf_document$font_table
  # (a family per script slot, a language per slot: "latin = X | east_asian = Y")
  slots <- function(v) {
    v <- .font_slots_value(v)
    if (is.null(names(v))) cell(v) else paste(names(v), "=", unname(v), collapse = " | ")
  }
  if (length(ft) && !is.null(ft[[1L]]$name)) pr$page$font <- slots(ft[[1L]]$name)
  if (length(page$rtf_document$font_lang)) pr$page$font_lang <- slots(page$rtf_document$font_lang)
  if (inherits(page$header, "tfl_unset")) pr$report$page_header <- "FALSE"
  if (inherits(page$footer, "tfl_unset")) pr$report$page_footer <- "FALSE"
  for (a in c("width_twips", "height_twips")) {
    v <- page$rtf_figures[[a]]
    if (!is.null(v)) pr$report[[if (a == "width_twips") "figure_width_in" else "figure_height_in"]] <-
      if (is.numeric(v)) format(v / 1440) else cell(v)
  }
  for (k in names(page$report)) pr$report[[k]] <- cell(page$report[[k]])
  # what no column says (the titles' row height, the bands' look, a second
  # font ...): kept as YAML in the page sheet's page_more, read back as it was
  more <- .page_more(page)
  if (length(more)) {
    txt <- yaml::as.yaml(.calls_out(more), indent.mapping.sequence = TRUE,
                         handlers = .yaml_out_handlers())
    pr$page$page_more <- sub("\n$", "", txt)
  }
  pr
}

# The rtfreporter arguments of a page no page or report sheet column says:
# list(<fn> = list(<arg> = value)); a font table of more than one font whole
.page_more <- function(page) {
  said <- character()
  for (sh in names(.page_map)) {
    m <- .page_map[[sh]]
    said <- c(said, ifelse(grepl(".", m, fixed = TRUE), m, paste(m, names(m), sep = ".")))
  }
  said <- c(said, "rtf_document.font_lang", "rtf_figures.width_twips", "rtf_figures.height_twips")
  ft <- page$rtf_document$font_table
  if (length(ft) <= 1L) said <- c(said, "rtf_document.font_table")
  out <- list()
  for (fn in intersect(names(page), c("rtf_page", "rtf_default_format", "rtf_document",
                                      "rtf_tables", "rtf_figures", "rtf_titles", "rtf_footnotes",
                                      "rtf_header", "rtf_footer", "generate_rtfreport"))) {
    a <- page[[fn]]
    if (!is.list(a) || inherits(a, "tfl_unset")) next
    for (k in names(a)) if (!paste(fn, k, sep = ".") %in% said) out[[fn]][[k]] <- a[[k]]
  }
  out
}
