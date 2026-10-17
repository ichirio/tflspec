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
  val <- function(x) if (is.null(x) || is.na(x) || !nzchar(trimws(x))) NULL else trimws(x)
  out <- list()
  for (i in seq_len(nrow(t))) {
    id <- t$output_id[i]
    args <- Filter(Negate(is.null), list(population = val(t$population[i]),
                                         by = val(t$by[i]), where = val(t$where[i])))
    for (kv in strsplit(val(t$args[i]) %||% "", ";", fixed = TRUE)[[1L]]) {
      kv <- trimws(strsplit(kv, "=", fixed = TRUE)[[1L]])
      if (length(kv) < 2L) next
      v <- paste(kv[-1L], collapse = "=")
      n <- suppressWarnings(as.numeric(v))
      args[[kv[1L]]] <- if (!is.na(n)) n else v
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
                      population = val(t$population[i]), extends = "defaults",
                      template = list(name = val(t$template[i]), args = args))
  }
  out
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
