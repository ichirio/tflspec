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
#' What a report extends: its row on the `report` sheet has a `pattern` cell
#' (the first pattern, `@<name>`), and an `extends` cell (`a | b`, or `none`)
#' only when the chain is not `[<pattern>, defaults]` as the `pattern` cell
#' alone says it: `extends: none`, a pattern after the defaults, a pattern
#' alone, two patterns; a map by step as `content=none; page=patterns/a |
#' defaults`.  `tfl_yaml_from_sheets()` reads the cell as the chain,
#' in its order; an `extends` cell whose first pattern is not the `pattern`
#' cell's is an error (one of the two was edited).
#'
#' @param dir The study folder (or its `spec/`).
#' @param templates What a report written as a template SPEC becomes.
#'   `"sheet"` (the default): one row of the `templates` sheet (and its
#'   `template_rows`), by the template's name and arguments, as
#'   [tfl_read_template_sheets()] reads them; the report's page parts,
#'   codelists and report row are the other sheets' as for any report. Nothing is expanded; a
#'   report is written out as its items only by [tfl_template_to_spec()],
#'   called on purpose.  `"expand"`: the report is expanded first
#'   ([tfl_template_to_spec()]) and its items are on the table and ARD
#'   sheets, for what is made from the items (the ARD, the preview).
#' @param study The study's spec, as [tfl_read_study_spec()] reads it.
#' @param outputs The reports, as [tfl_read_outputs()] reads them: a list of
#'   `tfl_output` (as written, not resolved), named by id.
#' @return A list: `table` (the table and report sheets, with `study`),
#'   `planner` (the `_tflplanner` sheet: the report list and the setup),
#'   `ard` (the ARD sheets), `listing` (`listings`, `listing_cols`),
#'   `figures` (a figure's design list, by id), `study` (study.yml's
#'   fields), `templates` and `template_rows` (the template reports; with
#'   `templates = "expand"` both are empty), `not_converted` (what the
#'   sheets cannot hold).
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
tfl_sheets_from_yaml <- function(dir, templates = c("sheet", "expand")) {
  tfl_sheets_from_spec(tfl_read_study_spec(dir), tfl_read_outputs(dir, skip_bad = TRUE),
                       templates = templates)
}

#' @rdname tfl_sheets_from_yaml
#' @export
tfl_sheets_from_spec <- function(study, outputs, templates = c("sheet", "expand")) {
  templates <- match.arg(templates)
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
  # a template SPEC is one row of the `templates` sheet; only
  # `templates = "expand"` writes its items out first
  if (identical(templates, "expand")) {
    outs <- lapply(outs, function(o) {
      if (is.null(.tpl_spec_of(o))) return(o)
      tryCatch(tfl_template_to_spec(o, s), error = function(e) {
        warning(sprintf("%s: kept as it is (its template not expanded): %s", o$id,
                        conditionMessage(e)), call. = FALSE)
        o
      })
    })
  }
  trows <- list()
  tparam_rows <- list()
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
    # the defaults have no tables row to read the arguments from, and a
    # second normalize_ard() no row of its own: all of them are kept whole
    # with the items the sheets cannot say (#689).  A single one with
    # arguments is tried on the tables row below, and kept whole too when
    # reading the row does not give its arguments back.
    whole <- length(nz) > 1L ||
      (is.na(id) && any(vapply(nz, function(it) length(it$args) > 0L, NA)))
    if (length(nz) && !whole && !is.na(id) && length(nz[[1L]]$args)) norm_rows[[id]] <<- nz[[1L]]$args
    more <- if (whole) nz else list()
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
      sp <- tryCatch(bridge(c(nz, items[keep]), if (is.na(id)) NULL else id), error = function(e) e)
      # (the sheets refuse the rows of an item: which one, tried each alone;
      # that one goes whole too, not the report's whole content)
      if (inherits(sp, "error")) {
        refused <- vapply(keep, function(k) inherits(tryCatch(
          bridge(c(nz, items[k]), if (is.na(id)) NULL else id), error = function(e) e), "error"), NA)
        if (!any(refused)) stop(sp)
        out <- which(refused)
        keep <- keep[-out]
        if (!length(keep)) { sp <- list(); break }
        next
      }
      out <- attr(sp, "not_converted_at") %||% integer()
      if (!length(out)) break
      keep <- keep[-out]
      if (!length(keep)) { sp <- list(); break }
    }
    # (and the rest of a plan_columns() / plan_col_header() / plan_labels()
    # whose columns say a part: the arguments no column says)
    more <- c(more, items[setdiff(seq_along(items), keep)], attr(sp, "not_converted_rest"))
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
  # (a pattern has rows of the report and page sheets only -- no tables row
  # -- so its content cannot be put on the sheets: said, by pattern, #689)
  for (nm in names(s$patterns)) {
    pc <- s$patterns[[nm]]$content
    if (inherits(pc, "tfl_r") || length(pc)) {
      fns <- if (inherits(pc, "tfl_r")) "R code" else unique(vapply(pc, function(it) it$fn, ""))
      note(paste0("@", nm), "content", paste0(
        "a pattern's content (", paste(fns, collapse = ", "), ") is not kept: a pattern's rows are the ",
        "report and page sheets', none holds its layers; the reports that extend it do not get them back"))
    }
  }

  # -- the page parts ----------------------------------------------------------
  page_rows <- function(page, id) {
    for (band in c("header", "footer", "titles", "footnotes")) {
      m <- page[[band]]
      if (inherits(m, "tfl_unset")) next
      for (line in names(m)) {
        cells <- m[[line]]
        r <- list(output_id = id, line = line)
        if (inherits(cells, "tfl_unset")) { r$left <- "(none)"; add(band, r); next }
        for (cn in c("left", "center", "right", "row_height_twips")) {
          if (!is.null(cells[[cn]])) r[[cn]] <- cell(cells[[cn]])
        }
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
    tpl <- .tpl_spec_of(o)
    planner[[length(planner) + 1L]] <- list(
      output_id = id, description = cell(e$label %||% o$label), setup_code = NA_character_,
      process_code = .first_r_step(o$data$steps),
      section = cell(e$section), population = cell(e$population %||% .out_population(o)),
      datasets = cell(e$datasets %||% .records_dataset(o)), batches = cell(e$sets))
    if (!is.null(.records_dataset(o)) && !is.null(o$data$where)) {
      note(id, "data", "a records table's where (its dataset's condition) stays in YAML")
    }
    # the report row: its kind, its pattern, its file and program
    rr <- list(output_id = id, type = o$kind)
    # (the report's `common:`: its population is the planner's / templates' column,
    # the other two the report row's)
    for (k in c("treatment", "population_label")) if (!is.null(o$common[[k]])) rr[[k]] <- cell(o$common[[k]])
    # (a map by step: the cell says it step by step, `content=none; page=a | b`
    # (Q6 of PLAN-5B); a template report that writes none takes its template's)
    if (is.list(o$extends)) {
      rr$extends <- paste(sprintf("%s=%s", names(o$extends),
                                  vapply(o$extends, paste, "", collapse = " | ")), collapse = "; ")
    }
    pat <- if (!is.list(o$extends)) grep("^patterns/", o$extends, value = TRUE)
    if (length(pat)) rr$pattern <- paste0("@", substring(pat[1L], 10L))
    # (what the pattern cell alone reads back as is [first pattern, defaults];
    # any other chain -- the order flipped, `none`, a pattern alone, two
    # patterns -- is said in the `extends` cell)
    if (!is.null(o$extends) && !is.list(o$extends) && !identical(as.character(unlist(o$extends)),
                                          c(if (length(pat)) pat[1L], "defaults"))) {
      rr$extends <- .sheet_cell(o$extends)
    }
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
    if (is.null(tpl)) {
      if (inherits(o$content, "tfl_r")) rr$content_code <- cell(o$content)
      if (identical(o$data$source, "code") && !is.null(o$data$code)) rr$data_code <- cell(o$data$code)
      # the ARD's name in the report's program, when not ard
      if (!is.null(o$data$object) && !identical(o$data$object, "ard")) rr$ard_object <- cell(o$data$object)
    }
    add("report", rr)
    for (v in names(o$codelists)) for (i in seq_along(o$codelists[[v]])) {
      cl <- o$codelists[[v]][[i]]
      add("codelists", list(output_id = id, variable = v, value = cell(cl$value), label = cell(cl$label), order = as.character(i),
                            under = cell(cl$under)))
    }
    # a template report: its name and arguments on the templates sheet, and
    # that is all of the template; the items are made again from them
    if (!is.null(tpl)) {
      enc <- .tpl_sheets_encode(id, o$kind, .na_scalar(o$label), tpl, .out_population(o))
      trows[[length(trows) + 1L]] <- enc$row
      tparam_rows <- c(tparam_rows, enc$rows)
      if (length(enc$lost)) {
        warning(sprintf("%s: template arguments the templates sheet cannot hold are not kept: %s",
                        id, paste(enc$lost, collapse = ", ")), call. = FALSE)
        note(id, "template", paste0("template arguments the templates sheet cannot hold: ",
                                    paste(enc$lost, collapse = ", ")))
      }
      own <- c(if (length(o$content)) "content", if (length(o$ard)) "ard",
               if (length(o$data$steps)) "data steps")
      if (length(own)) {
        warning(sprintf("%s: the report's own %s stay in YAML; the templates sheet has no place for them",
                        id, paste(own, collapse = ", ")), call. = FALSE)
        note(id, "template", paste0("the report's own ", paste(own, collapse = ", "),
                                    " stay in YAML: the templates sheet has no place for them"))
      }
      next
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
      a <- .entry_pop_inject(a, .out_population(o))
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
      if (!code_content) {
        tryCatch(table_rows(o$content, id), error = function(e) note(
          id, "content", paste("its content could not be put on the sheets:", conditionMessage(e))))
      }
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
            # (a column written as a plain list, as R code or the xlsx import
            # gives it, is the same call)
            if (!inherits(cl, "tfl_call") && is.list(cl) && identical(cl$fn, "listing_col")) {
              cl <- tfl_call("listing_col", cl$args %||% list(), package = cl$package)
            }
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
      # a figure of items (R/fig_content.R): its data and items as they are,
      # the design list's `content` (read back by .conv_output(), #689)
      fd <- list(template = o$template, data = o$data$steps, content = .items_to_list(o$content))
      for (k in c("ggplot2_version", "plot", "plots", "compose")) if (!is.null(o[[k]])) fd[[k]] <- o[[k]]
      figures[[id]] <- fd
      # what the design list has no place for is said, by the report
      lost <- c(setdiff(names(o$data), c("steps", "code", "datasets", "ard", "source")),
                if (!is.null(o$data$source) && !identical(o$data$source, "code")) "source")
      lost <- c(lost, setdiff(names(o), c(.output_fields, "template", "ggplot2_version", "plot", "plots", "compose")))
      if (length(lost)) {
        note(id, "figure", paste0("a figure of items: ", paste(unique(lost), collapse = ", "),
                                   " is not kept (the design list has no place for it)"))
      }
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
  # a pattern with no page rows keeps its place: one report row, `@<name>`
  # and nothing else (#662)
  for (nm in names(s$patterns)) {
    has <- any(vapply(.ard_spec_pattern_sheets, function(sh) any(vapply(
      rows[[sh]] %||% list(), function(r) identical(r$output_id, paste0("@", nm)), NA)), NA))
    if (!has) add("report", list(output_id = paste0("@", nm)))
  }
  # defaults.yml's options: and styles:, and the style: a report picks, have
  # no sheet or column to be put on: said (#689)
  if (length(s$options)) {
    note(NA_character_, "options", paste0(
      "defaults.yml options: (", paste(names(s$options), collapse = ", "), ") is not kept: no sheet or column holds it"))
  }
  if (length(s$styles)) {
    note(NA_character_, "styles", paste0(
      "defaults.yml styles: (", paste(names(s$styles), collapse = ", "), ") is not kept: no sheet or column holds it"))
  }
  for (id in names(outs)) {
    if (length(outs[[id]]$style)) {
      note(id, "style", paste0("style: ", paste(unlist(outs[[id]]$style), collapse = ", "),
                               " is not kept: no sheet or column holds it; the report no longer picks its look"))
    }
  }
  # the layers' code lists have no sheet of their own (a report's own are
  # its rows): they stay in the YAML
  # (in the YAML they stay; a study written from the sheets has not got them,
  # and a report that reads the variable is then made without the list: its
  # ARD program loses the set_levels() step -- the values' order, and the table its labels, #689)
  cl_said <- function(cl, whose, file) paste0(
    whose, " code lists (", file, ": ", paste(names(cl), collapse = ", "), ") stay in YAML, not on the sheets: ",
    "a study written from the sheets has not got them, so a report that reads one of these variables ",
    "is made without the list: its ARD program does not set the values' order, and its table has no labels for them")
  if (length(s$defaults$codelists)) {
    note(NA_character_, "codelists", cl_said(s$defaults$codelists, "the defaults'", "defaults.yml"))
  }
  for (nm in names(s$patterns)) {
    if (length(s$patterns[[nm]]$codelists)) {
      note(paste0("@", nm), "codelists", cl_said(s$patterns[[nm]]$codelists, "a pattern's",
                                                  paste0("patterns/", nm, ".yml")))
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
  # a table's normalize_ard() arguments: what the columns can say (overall,
  # stack, stack_sep, stack_nest) is always on its tables row; when reading
  # the row (and the report's hierarchical analysis) gives all the
  # arguments back, that is all; else the rest -- hierarchy, keys,
  # drop_contexts, drop_key_variables, which have no column, or a hierarchy
  # other than the first hierarchical analysis's -- is an item in
  # content_more, possibly with no arguments (.nz_with_row() reads the two
  # together)
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
    got <- tryCatch(.conv_normalize_args(list(tables = tb), ard, id), error = function(e) NULL)
    # (an overall runs only with a hierarchy: one a card leaves to the ARD is the
    # one read back; a stack alone runs without, so its missing hierarchy means something)
    if (!is.null(got) && !is.null(a$overall) && is.null(a$hierarchy) && !is.null(got$hierarchy)) {
      a$hierarchy <- got$hierarchy
    }
    if (is.null(got) || !identical(.nz_canon(got), .nz_canon(a))) {
      # (a stack_sep / stack_nest with no stack: the row cannot say it)
      on_row <- c("overall", "stack", if (!is.null(a$stack)) c("stack_sep", "stack_nest"))
      rest <- a[setdiff(names(a), on_row)]
      more_rows[[id]] <- c(list(tfl_item("normalize_ard", rest)), more_rows[[id]])
    }
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
       listing = list(listings = .rows_df(listings, .listing_sheets$listings),
                      listing_cols = .rows_df(listing_cols, .listing_sheets$listing_cols),
                      figures = .rows_df(fig_rows, c("output_id", "datasets", "ard"))),
       figures = figures, study = s$study,
       templates = .rows_df(trows, c("output_id", "kind", "label", "population", "template", "by",
                                     "where", "args")),
       template_rows = .rows_df(tparam_rows, c("output_id", "param", "value", "label")),
       not_converted = if (length(nc)) do.call(rbind, nc) else
         data.frame(output = character(), part = character(), reason = character(), stringsAsFactors = FALSE))
}

# a sheet's columns as the readers know them (the schema's, else the
# report half's)
.sheet_columns <- function(sh) {
  sc <- .ard_spec_schema()[[sh]]
  if (!is.null(sc)) return(sc)
  switch(sh,
    report = c("output_id", "type", "file", "program", "pattern", "extends"),
    page = "output_id",
    header = , footer = , titles = , footnotes = c("output_id", "line", "left", "center", "right",
                                                   "row_height_twips"),
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

# normalize_ard()'s arguments in one form to compare (the sheets give them
# back as a bar-split vector, YAML may hold a list of single values): names
# in order, a list of single values as a vector
.nz_canon <- function(x) {
  if (is.list(x)) {
    x <- lapply(unclass(x), .nz_canon)
    if (!is.null(names(x))) x <- x[order(names(x))]
    else if (length(x) && all(vapply(x, function(e) is.atomic(e) && length(e) == 1L, NA))) x <- unlist(x)
    return(x)
  }
  attributes(x) <- NULL
  x
}

# A table's normalize_ard() item from content_more with the tables row's
# overall_label, stack, stack_sep and stack_nest on it, where the row has
# them (they win over the same names in the item): the item holds what the
# columns cannot say, and its arguments stand as written -- the hierarchy
# comes from it, not from the hierarchical analysis.  `tables`: the table's
# tables rows (the first is read).  With none or two such items, `items` as
# they are (a second normalize_ard() is whole, with no row).
.nz_with_row <- function(items, tables) {
  is_nz <- vapply(items, function(it) identical(it$fn, "normalize_ard"), NA)
  if (sum(is_nz) != 1L || !is.data.frame(tables) || !nrow(tables)) return(items)
  # (a table's own row, not the defaults' blank one)
  t <- tables
  if ("output_id" %in% names(t) && any(!is.na(t$output_id))) t <- t[!is.na(t$output_id), , drop = FALSE]
  t <- t[1L, , drop = FALSE]
  t$output_id <- "x"
  row <- .conv_normalize_args(list(tables = t), NULL, "x")
  k <- which(is_nz)
  args <- items[[k]]$args %||% list()
  for (n in intersect(names(row), c("overall", "stack", "stack_sep", "stack_nest"))) args[n] <- list(row[[n]])
  items[[k]]$args <- args
  items
}

# a table's rework step: the first `!r` among its data steps (a figure's
# steps are maps, not R)
.first_r_step <- function(steps) {
  for (s in steps %||% list()) if (inherits(s, "tfl_r")) return(as.character(s))
  NA_character_
}

.page_known <- c("header", "footer", "titles", "footnotes", "tokens", "page", "report",
                 "rtf_page", "rtf_style", "rtf_default_format", "rtf_document", "rtf_tables",
                 "rtf_titles",
                 "rtf_footnotes", "rtf_figures", "rtf_header", "rtf_footer", "generate_rtfreport")

# `page:`'s rtfreporter arguments as the page and report sheets' columns
# (the inverse of the converter's .page_put()): list(page = , report = ),
# each a named list of cells
.page_columns <- function(page) {
  cell <- .sheet_cell
  page <- .page_style_norm(page)
  pr <- list(page = list(), report = list())
  for (sh in names(.page_map)) {
    m <- .page_map[[sh]]
    for (col in names(m)) {
      fn <- sub("[.].*$", "", m[[col]])
      arg <- if (grepl(".", m[[col]], fixed = TRUE)) sub("^[^.]*[.]", "", m[[col]]) else col
      v <- page[[fn]][[arg]]
      # (a call -- a watermark's look, rtf_watermark() -- is no cell: page_more)
      if (!is.null(v) && !inherits(v, "tfl_call")) pr[[sh]][[col]] <- cell(v)
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
  page <- .page_style_norm(page)
  said <- character()
  for (sh in names(.page_map)) {
    m <- .page_map[[sh]]
    said <- c(said, ifelse(grepl(".", m, fixed = TRUE), m, paste(m, names(m), sep = ".")))
  }
  said <- c(said, "rtf_document.font_lang", "rtf_figures.width_twips", "rtf_figures.height_twips")
  if (inherits(page$rtf_document$watermark, "tfl_call")) {
    said <- setdiff(said, "rtf_document.watermark")
  }
  ft <- page$rtf_document$font_table
  if (length(ft) <= 1L) said <- c(said, "rtf_document.font_table")
  out <- list()
  for (fn in intersect(names(page), c("rtf_page", "rtf_style", "rtf_document",
                                      "rtf_tables", "rtf_figures", "rtf_titles", "rtf_footnotes",
                                      "rtf_header", "rtf_footer", "generate_rtfreport"))) {
    a <- page[[fn]]
    if (!is.list(a) || inherits(a, "tfl_unset")) next
    for (k in names(a)) if (!paste(fn, k, sep = ".") %in% said) out[[fn]][[k]] <- a[[k]]
  }
  out
}
