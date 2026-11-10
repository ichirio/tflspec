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
#'
#' @param path The workbook.
#' @return A list of reports, by id: each `id`, `kind`, `label`,
#'   `population`, `extends` and `template` (`name`, `args`), as a report
#'   file ([tfl_write_template_yaml()]) holds it.
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
  .tpl_sheets_decode(t, rows)
}

# The two sheets (as data frames of text) as the reports' template SPECs
.tpl_sheets_decode <- function(t, rows) {
  val <- function(x) if (is.null(x) || is.na(x) || !nzchar(trimws(x))) NULL else trimws(x)
  out <- list()
  for (i in seq_len(nrow(t))) {
    id <- t$output_id[i]
    args <- Filter(Negate(is.null), list(population = val(t$population[i]),
                                         by = val(t$by[i]), where = val(t$where[i])))
    cell_pop <- NULL
    for (kv in strsplit(val(t$args[i]) %||% "", ";", fixed = TRUE)[[1L]]) {
      kv <- trimws(strsplit(kv, "=", fixed = TRUE)[[1L]])
      if (length(kv) < 2L) next
      v <- paste(kv[-1L], collapse = "=")
      if (identical(kv[1L], "population")) cell_pop <- v
      n <- suppressWarnings(as.numeric(v))
      args[[kv[1L]]] <- if (!is.na(n)) n else if (v %in% c("TRUE", "FALSE")) as.logical(v) else v
    }
    # (a template that takes the analysis set from the report's `population:`
    # has it as no argument: the `population` column is the report's, and so is
    # a `population=` of the `args` cell when the column is empty; two different
    # ones stop)
    tp <- tryCatch(.tpl_get(val(t$template[i]) %||% "", fn), error = function(e) NULL)
    pop <- val(t$population[i])
    if (isTRUE(tp$params$population$hidden)) {
      if (!is.null(pop) && !is.null(cell_pop) && !identical(pop, cell_pop)) {
        .ard_stop(sprintf(paste0("%s: %s: the `population` column says %s and the `args` cell %s; ",
                                 "the analysis set is the report's `population:`: give it once."),
                          fn, id, pop, cell_pop))
      }
      pop <- pop %||% cell_pop
      args$population <- NULL
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
    out[[id]] <- list(id = id, kind = val(t$kind[i]) %||% "table", label = val(t$label[i]),
                      population = pop, extends = "defaults",
                      template = list(name = val(t$template[i]), args = args))
  }
  out
}

# A report's template SPEC as its rows of the two sheets: the form
# tfl_read_template_sheets() reads.  `population`, `by` and `where` of one
# value are the sheet's own columns; other arguments of one value are in the
# `args` cell (`name = value; name = value`); a list of values is a row a
# value on `template_rows`, the heading of a value (`labels`) in its row's
# `label`.  Returns the `templates` row, the `template_rows` rows, and the
# names of the arguments the sheets do not give back as they were (a map, a
# list of maps, a value with `;`), found by reading the rows back.
.tpl_sheets_encode <- function(id, kind, label, spec) {
  args <- spec$args
  one <- function(v) is.atomic(v) && length(v) == 1L && !is.na(v) && nzchar(as.character(v))
  cols <- list()
  cells <- character()
  prm <- list()
  for (nm in setdiff(names(args), "labels")) {
    v <- args[[nm]]
    if (is.list(v) && is.null(names(v)) && length(v) &&
        all(vapply(v, function(e) is.atomic(e) && length(e) == 1L && !is.na(e), NA))) v <- unlist(v)
    if (!is.atomic(v) || !is.null(names(v)) || !length(v) || anyNA(v)) next
    v <- as.character(v)
    if (length(v) == 1L && nm %in% c("population", "by", "where") && nzchar(v)) {
      cols[[nm]] <- v
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
  back <- .tpl_sheets_decode(t1, r1)[[id]]$template$args
  norm <- function(v) if (is.null(v)) NULL else {
    u <- unlist(v)
    list(names = names(u), value = as.character(u))
  }
  lost <- names(args)[!vapply(names(args), function(nm) identical(norm(args[[nm]]), norm(back[[nm]])), NA)]
  list(row = row, rows = rws, lost = lost)
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
