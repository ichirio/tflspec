# Template SPEC in Excel, many reports at once: sheet `templates` (one row
# a report: its template and the arguments that are one value) and sheet
# `template_rows` (the arguments that are lists: one row a value).
# Prototype: the form is to be decided on these examples (DM and AE).

#' Read reports' template SPECs from a workbook
#'
#' A workbook of two sheets, many reports at once:
#' \describe{
#'   \item{`templates`}{one row a report: `output_id`, `kind`, `label`,
#'     `population`, `template`, `by`, `where`, and `args` -- the other
#'     arguments of one value, `name = value; name = value`.}
#'   \item{`template_rows`}{the arguments that are lists, one row a value:
#'     `output_id`, `param` (`continuous`, `categorical`, `hierarchy` ...),
#'     `value` (`AGE`) and `label` (its heading, for `labels`).}
#' }
#' A template's condition rows (its `conditions`: a row a condition) are
#' rows of a third sheet, `conditions` (the ARD definition's: `output_id`,
#' `analysis_id`, `row`, then a column a key), with `analysis_id` blank --
#' the report's template's rows, not an analysis's.  An argument that is a
#' map (`formats`, `analysis_ids`) has no place on the sheets: it stays in
#' the report's YAML file.
#'
#' @param path The workbook.
#' @return A list of reports, by id: each `id`, `kind`, `label`,
#'   `extends` and `template` (`name`, `args`), as a report file
#'   ([tfl_write_template_yaml()]) holds it.  The `population` column is
#'   the argument of the template's set parameter (the one that fills its
#'   subjects row's set; [tfl_report_set()]).
#' @examples
#' f <- system.file("extdata", "template-spec", "templates.xlsx", package = "tflspec")
#' r <- tfl_read_template_sheets(f)
#' str(r[["T-14-1-1"]]$template)
#' @export
tfl_read_template_sheets <- function(path) {
  fn <- "tfl_read_template_sheets()"
  sh <- readxl::excel_sheets(path)
  if (!"templates" %in% sh) .ard_stop(sprintf("%s: %s has no sheet `templates`.", fn, basename(path)))
  rd <- function(s) {
    d <- as.data.frame(readxl::read_excel(path, sheet = s, col_types = "text"),
                       stringsAsFactors = FALSE)
    d[!is.na(d$output_id) & nzchar(d$output_id), , drop = FALSE]
  }
  t <- rd("templates")
  rows <- if ("template_rows" %in% sh) rd("template_rows") else NULL
  conds <- if ("conditions" %in% sh) rd("conditions") else NULL
  .tpl_sheets_decode(t, rows, conds)
}

# The sheets (as data frames of text) as the reports' template SPECs: the
# templates sheet `t`, the template_rows `rows`, and the conditions sheet
# `conds` (a template's condition rows: its analysis_id blank)
.tpl_sheets_decode <- function(t, rows, conds = NULL) {
  fn <- "tfl_read_template_sheets()"
  val <- function(x) if (is.null(x) || is.na(x) || !nzchar(trimws(x))) NULL else trimws(x)
  out <- list()
  for (i in seq_len(nrow(t))) {
    id <- t$output_id[i]
    # (the `population` column is the template's set parameter, whatever
    # its name: the one that fills its subjects row, .tpl_set_param(); a
    # template that names none takes the column as `population`)
    sp <- .tpl_set_param(val(t$template[i]) %||% "")
    if (is.na(sp)) sp <- "population"
    pop <- val(t$population[i])
    args <- Filter(Negate(is.null), list(by = val(t$by[i]), where = val(t$where[i])))
    if (!is.null(pop)) args[[sp]] <- pop
    for (kv in strsplit(val(t$args[i]) %||% "", ";", fixed = TRUE)[[1L]]) {
      kv <- trimws(strsplit(kv, "=", fixed = TRUE)[[1L]])
      if (length(kv) < 2L) next
      v <- paste(kv[-1L], collapse = "=")
      # (the set in the column and in the `args` cell: given once, A2-T Q6)
      if (identical(kv[1L], sp) && !is.null(pop) && !identical(pop, v)) {
        .ard_stop(sprintf("%s: %s: %s.", fn, id, .set_twice_text(
          sprintf("the `population` column (%s)", pop), sprintf("the `args` cell's `%s` (%s)", sp, v))))
      }
      n <- suppressWarnings(as.numeric(v))
      args[[kv[1L]]] <- if (!is.na(n)) n else if (v %in% c("TRUE", "FALSE")) as.logical(v) else v
    }
    r <- rows[rows$output_id == id, , drop = FALSE]
    for (p in unique(r$param)) {
      k <- r[r$param == p, , drop = FALSE]
      args[[p]] <- k$value
      lab <- k$label[!is.na(k$label) & nzchar(k$label)]
      if (length(lab)) {
        args$labels <- c(args$labels, stats::setNames(lab, k$value[!is.na(k$label) & nzchar(k$label)]))
      }
    }
    cd <- .tpl_cond_rows_in(conds, id)
    if (length(cd)) args$conditions <- cd
    out[[id]] <- list(id = id, kind = val(t$kind[i]) %||% "table", label = val(t$label[i]),
                      extends = NULL, template = list(name = val(t$template[i]), args = args))
  }
  out
}

# A report's template SPEC as its rows of the two sheets: the form
# tfl_read_template_sheets() reads.  `population`, `by` and `where` of one
# value are the sheet's own columns; other arguments of one value are in the
# `args` cell (`name = value; name = value`); a list of values is a row a
# value on `template_rows`, the heading of a value (`labels`) in its row's
# `label`.  The template's condition rows (`conditions`, a list of maps or a
# data frame: a row a condition) are rows of the ARD's conditions sheet,
# their analysis_id blank.  An argument that is a map (`formats`,
# `analysis_ids`) is not on the sheets: it stays in the report's YAML file,
# and is not named.  Returns the `templates` row, the `template_rows` rows,
# the `conditions` rows, and the names of the arguments the sheets do not
# give back as they were (a list of maps other than the conditions, a value
# with `;`), found by reading the rows back.
.tpl_sheets_encode <- function(id, kind, label, spec, population = NULL) {
  args <- spec$args
  one <- function(v) is.atomic(v) && length(v) == 1L && !is.na(v) && nzchar(as.character(v))
  # (the `population` column is the template's set parameter, .tpl_set_param();
  # a report that does not give it has the set it says beside it,
  # `population`, written there: the sheets write the set where it is read)
  sp <- .tpl_set_param(spec$name %||% "")
  if (is.na(sp)) sp <- "population"
  if (is.null(args[[sp]]) && one(population) && !is.na(.tpl_set_param(spec$name %||% ""))) {
    args[[sp]] <- as.character(population)
  }
  cols <- list()
  cells <- character()
  prm <- list()
  cond <- if (.tpl_is_cond_rows(args$conditions)) .item_rows_out(args$conditions, id, NA_character_)
  for (nm in setdiff(names(args), c("labels", if (length(cond)) "conditions"))) {
    v <- args[[nm]]
    if (is.list(v) && is.null(names(v)) && length(v) &&
        all(vapply(v, function(e) is.atomic(e) && length(e) == 1L && !is.na(e), NA))) v <- unlist(v)
    if (!is.atomic(v) || !is.null(names(v)) || !length(v) || anyNA(v)) next
    v <- as.character(v)
    if (length(v) == 1L && nm %in% c(sp, "by", "where") && nzchar(v)) {
      cols[[if (identical(nm, sp)) "population" else nm]] <- v
    } else if (length(v) == 1L && nzchar(v) && !grepl("[;=]", paste0(nm, v))) {
      cells <- c(cells, paste0(nm, " = ", v))
    } else {
      prm[[nm]] <- v
    }
  }
  lab <- args$labels
  lab <- if (is.atomic(lab) || is.list(lab)) unlist(lab) else NULL
  labels <- list()
  for (n in names(lab)) {
    hit <- names(prm)[vapply(prm, function(v) n %in% v, NA)]
    if (!length(hit)) {
      sc <- grep(paste0(" = ", n, "$"), cells)
      if (length(sc)) {
        nm <- sub(" = .*$", "", cells[sc[1L]])
        prm[[nm]] <- n
        cells <- cells[-sc[1L]]
        hit <- nm
      }
    }
    if (length(hit)) labels[[length(labels) + 1L]] <- list(param = hit[1L], value = n, label = lab[[n]])
  }
  rws <- list()
  for (p in names(prm)) for (v in prm[[p]]) {
    l <- NA_character_
    for (x in labels) if (identical(x$param, p) && identical(x$value, v)) { l <- x$label; break }
    rws[[length(rws) + 1L]] <- list(output_id = id, param = p, value = v, label = l)
  }
  row <- list(output_id = id, kind = kind, label = label, population = cols$population,
              template = spec$name, by = cols$by, where = cols$where,
              args = if (length(cells)) paste(cells, collapse = "; "))
  # read back: what is not the same is named
  f <- function(x) if (is.null(x) || is.na(x)) NA_character_ else x
  t1 <- data.frame(output_id = id, kind = f(kind), label = f(label), population = f(row$population),
                   template = f(spec$name), by = f(row$by), where = f(row$where),
                   args = f(row$args), stringsAsFactors = FALSE)
  r1 <- if (length(rws)) data.frame(output_id = vapply(rws, `[[`, "", "output_id"),
                                    param = vapply(rws, `[[`, "", "param"),
                                    value = vapply(rws, `[[`, "", "value"),
                                    label = vapply(rws, function(r) f(r$label), ""),
                                    stringsAsFactors = FALSE)
  c1 <- if (length(cond)) .rows_df(cond, .ard_spec_sheets$conditions)
  back <- .tpl_sheets_decode(t1, r1, c1)[[id]]$template$args
  norm <- function(v) if (is.null(v)) NULL else {
    u <- unlist(v)
    list(names = names(u), value = as.character(u))
  }
  same <- function(nm) {
    if (identical(nm, "conditions")) return(identical(.tpl_cond_norm(args[[nm]]), .tpl_cond_norm(back[[nm]])))
    identical(norm(args[[nm]]), norm(back[[nm]]))
  }
  # (a map stays in the report's YAML file: not lost, but named as `maps`)
  maps <- setdiff(names(args)[vapply(args, .tpl_is_map_arg, NA)], "labels")
  lost <- setdiff(names(args)[!vapply(names(args), same, NA)], maps)
  list(row = row, rows = rws, cond = cond, lost = lost, maps = maps)
}

# A template argument that is a map (`formats`, `analysis_ids`): the sheets
# do not hold it
.tpl_is_map_arg <- function(v) !is.null(names(v)) && !inherits(v, "tfl_r") && !is.data.frame(v)

# A template report's template read back from the sheets, with its map
# arguments (`maps`: the ones the sheets said they do not hold) taken from
# the report's file already at `f` when that is the same template's, in
# that file's order; a map the file does not give back is named in a
# warning and a note (`note`, tfl_yaml_from_sheets()'s)
.tpl_maps_back <- function(tpl, f, maps, id, note) {
  old <- if (file.exists(f)) tryCatch(tfl_read_output(f), error = function(e) NULL)
  ot <- if (inherits(old, "tfl_output")) tryCatch(.tpl_spec_of(old), error = function(e) NULL)
  if (!is.null(ot) && identical(ot$name, tpl$name)) {
    a <- tpl$args %||% list()
    for (nm in setdiff(names(ot$args), c(names(a), "labels"))) {
      if (.tpl_is_map_arg(ot$args[[nm]])) a[nm] <- list(ot$args[[nm]])
    }
    tpl$args <- a[c(intersect(names(ot$args), names(a)), setdiff(names(a), names(ot$args)))]
  }
  gone <- setdiff(maps, names(tpl$args))
  if (length(gone)) {
    warning(sprintf("%s: template arguments the templates sheet cannot hold are not kept: %s",
                    id, paste(gone, collapse = ", ")), call. = FALSE)
    note(id, "template", NA, NA, paste0("template arguments the templates sheet cannot hold (no report ",
                                        "file to take them from): ", paste(gone, collapse = ", ")))
  }
  tpl
}

# A template's `conditions` written as rows: a data frame, or a list of
# maps (a row a condition)
.tpl_is_cond_rows <- function(v) {
  is.data.frame(v) ||
    (is.list(v) && is.null(names(v)) && length(v) > 0L &&
       all(vapply(v, function(e) is.list(e) && !is.null(names(e)), NA)))
}

# A template's condition rows from the conditions sheet `cs`: the report's
# rows with a blank analysis_id, in `row` order, each a map of its cells
# (a blank cell no key; row_analysis_id the row's analysis_id, `where` an R
# condition, as an item's rows read); NULL for none
.tpl_cond_rows_in <- function(cs, output_id) {
  if (!is.data.frame(cs) || !nrow(cs)) return(NULL)
  blank <- function(v) is.na(v) | !nzchar(trimws(as.character(v)))
  d <- cs[cs$output_id %in% output_id & blank(cs$analysis_id), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d <- d[order(suppressWarnings(as.integer(d$row))), , drop = FALSE]
  keys <- setdiff(names(d), c(.ard_spec_sheets$conditions, "note"))
  d <- d[, keys, drop = FALSE]
  names(d)[names(d) == "row_analysis_id"] <- "analysis_id"
  .cond_rows_yaml(d)
}

# Condition rows in one form to compare: a row its cells as text, by key
# (the order of a row's keys says nothing); none for no rows
.tpl_cond_norm <- function(v) {
  if (is.data.frame(v)) {
    v <- lapply(seq_len(nrow(v)), function(k) lapply(v, function(x) x[[k]]))
  }
  if (!length(v)) return(NULL)
  lapply(v, function(r) {
    u <- unlist(r)
    u <- u[!is.na(u)]
    u <- u[order(names(u))]
    list(names = names(u), value = as.character(u))
  })
}

#' Write a report's template SPEC as its YAML file
#'
#' @param report One report of [tfl_read_template_sheets()].
#' @param path The file (`spec/outputs/<id>.yml`).
#' @return `path`, invisibly.
#' @export
tfl_write_template_yaml <- function(report, path) {
  r <- report
  r$template$args <- lapply(r$template$args, function(v) if (length(names(v))) as.list(v) else v)
  writeLines(yaml::as.yaml(Filter(Negate(is.null), r), indent.mapping.sequence = TRUE),
             path, useBytes = TRUE)
  invisible(path)
}
