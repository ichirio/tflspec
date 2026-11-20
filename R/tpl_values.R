#' A series template's arguments as resolved, one row an argument
#'
#' The record of a template's arguments that a template card shows: for
#' each param, the value in effect and where it comes from, read by the one
#' resolver of the series tags ([tfl_template_resolve()]: `!series`,
#' `!study`, `!fixed`, `!unset`).  Unlike that function it needs no whole
#' report: with no arguments it gives the values a report that writes none
#' would take, and a required argument with no value is `NULL`, not an
#' error.
#'
#' The `source` words are the resolver's `"origins"`:
#' \describe{
#'   \item{`report`}{the report's value (`args`, or the report's
#'     `template:`).}
#'   \item{`template`}{the template's own default (written in the
#'     template, untagged; also `!unset` and no default).}
#'   \item{`series`}{`!series <key>`: the series' value (`series.yml`
#'     `values:`).}
#'   \item{`study_series`}{`!series <key>`: the study's value of the series
#'     (`study.yml` `series_settings:`).}
#'   \item{`study_settings`}{`!study <key>`: the study's setting
#'     (`study.yml` `settings:`).}
#'   \item{`fallback`}{`!study <key>` and the study has no value (or there
#'     is no study): the reference's fallback, which in a series template
#'     is always `!unset`.}
#'   \item{`fixed`}{`!fixed`: the template fixes it; a report cannot change
#'     it.}
#' }
#' A value that is `!unset` (the argument is left out of the call) is a
#' [tfl_unset()] object in `value`, whatever its source.
#'
#' A map argument whose default is `!series` is taken as the report writes
#' it, not yet merged by name over the series' map.
#'
#' @param template A series template, `"<series>/<template>"`
#'   (`"tflspec_v1/demog"`), or a report SPEC that names one (`template:
#'   name:`): a list, as read, or its YAML file.  A template that is not a
#'   series template (the registry's `"demog"`) stops.
#' @param study The study: a [tfl_read_study_spec()] object, its
#'   `study.yml` read as a list, the file or the study folder; `NULL` for
#'   none (a `!series` reference then takes the series' value, a `!study`
#'   one its fallback).
#' @param args The report's arguments, a named list (flat, or by step
#'   section as a report's `template:` writes them: `analysis:`,
#'   `content:`, ...); may be empty.  With a report in `template`, they go
#'   over the report's own.
#' @return A data frame, one row a param of the template, in the order of
#'   its `params:` ([tfl_arg_defs()]):
#'   \describe{
#'     \item{name}{the param.}
#'     \item{value}{(a list column) the value in effect.}
#'     \item{source}{where it comes from, one of the words above.}
#'     \item{key}{the `!series` or `!study` key the value was read through,
#'       else `NA` (also when the report gives the value).}
#'     \item{fallback}{`TRUE` for a `!study` reference that found nothing
#'       (`source` `"fallback"`).}
#'     \item{default}{(a list column) the template's default as written, a
#'       tagged one a `tfl_tagged` object, as [tfl_arg_defs()] gives it.}
#'   }
#'   Its attribute `"data"` holds the template's `data:` rows (with a
#'   report's rows merged in by id) with the arguments filled in, as
#'   [tfl_template_resolve()] makes them.
#' @seealso [tfl_template_resolve()], [tfl_arg_defs()]
#' @examples
#' v <- tfl_template_values("tflspec_v1/demog")
#' v[, c("name", "source", "key")]
#' v$value[[match("total", v$name)]]
#' v <- tfl_template_values("tflspec_v1/demog",
#'                          study = list(series_settings = list(tflspec_v1 = list(total = TRUE))),
#'                          args = list(by = "TRT01A"))
#' v[v$name %in% c("by", "total"), c("name", "source", "key")]
#' attr(v, "data")[[1]]$population
#' @export
tfl_template_values <- function(template, study = NULL, args = list()) {
  who <- "tfl_template_values()"
  report <- NULL
  if (is.character(template) && length(template) == 1L && !is.na(template) &&
      (file.exists(template) && !dir.exists(template))) {
    report <- .series_read_yaml(template)
  } else if (is.list(template)) {
    report <- template
  }
  if (!is.null(report)) {
    template <- if (is.list(report)) report$template$name
    if (is.null(template)) stop(who, ": the report names no template (template: name:).", call. = FALSE)
  }
  if (!is.character(template) || length(template) != 1L || is.na(template) ||
      !grepl("^[^/]+/[^/]+$", template)) {
    shown <- if (is.character(template) && length(template) == 1L) template else deparse(template)[1L]
    stop(who, ": \"", shown, "\" is not a series template; write <series>/<template> ",
         "(\"tflspec_v1/demog\") or give a report that names one.", call. = FALSE)
  }
  if (!is.null(args) && (!is.list(args) || (length(args) && is.null(names(args))))) {
    stop(who, ": `args` is a named list of the report's arguments.", call. = FALSE)
  }
  if (inherits(study, "tfl_study_spec")) study <- study$study %||% list()
  study <- .tr_study(study, who)

  tf <- .template_file(template)
  series <- tfl_series_read(sub("/.*$", "", template), dir = dirname(dirname(dirname(tf))))
  tpl <- .series_resolve(.series_read_yaml(tf), series, study, template)
  defs <- tfl_arg_defs(template = template)

  given <- list()
  if (!is.null(report)) {
    report <- .series_resolve(report, series, study, template, origin = "report")
    given <- .tr_given(report$template, defs$name, template, who)
  }
  if (length(args)) {
    own <- .series_resolve(args, series, study, template, origin = "report")
    own <- .tr_given(own, defs$name, template, who)
    given[names(own)] <- own
  }
  a <- .tr_args(tpl, attr(tpl, "origins"), given, defs, template, who)

  key <- vapply(seq_len(nrow(defs)), function(i) {
    d <- defs$default[[i]]
    if (defs$name[[i]] %in% names(given) || !inherits(d, "tfl_tagged") ||
        !attr(d, "tag") %in% c("series", "study")) return(NA_character_)
    as.character(unlist(d$value))[1L]
  }, "")
  value <- lapply(defs$name, function(p) if (isTRUE(a$unset[[p]])) tfl_unset() else a$env[[p]])
  out <- data.frame(name = defs$name, stringsAsFactors = FALSE)
  out$value <- I(value)
  out$source <- unname(a$origins[defs$name])
  out$key <- key
  out$fallback <- out$source == "fallback"
  out$default <- I(defs$default)
  rownames(out) <- NULL

  env <- .tr_derive(tpl$derive, a$env, defs$name, template, a$origins)
  attr(out, "data") <- .tr_data_rows(tpl$data, report$data, env)
  out
}
