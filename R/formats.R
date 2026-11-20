# ---- formats: one SPEC notation, the ARD's or plan_digits()' ---------------

#' A SPEC's formats, checked once, as the ARD or plan_digits() writes them
#'
#' A SPEC says the digits of a table's numbers in one map, `formats`,
#' whichever way the template formats them (canon 2026-11-11): the ARD's
#' formats (cards' `fmt_fun`, the ARD's `stat_fmt`) or the table's
#' `rtfreporter::plan_digits()`.  `tfl_formats()` checks the map once and
#' writes it as that way takes it; the YAML stays the map.
#'
#' The map's keys:
#' * a statistic (`N: 0`, `mean: 1`, `sd: 2`): its format everywhere;
#' * a name whose value is such a map, or `name:statistic`: an exception.
#'   The name is a parameter, a PARAMCD value (`AUCLST: {mean: 2}`), when
#'   the analysis's `by` has PARAMCD and the name is not one of its
#'   `variables`; else it is an analysis variable (`AGE: {sd: 2}`, the same
#'   as `"AGE:sd": 2`).  An exception wins over the statistic's format for
#'   that statistic only.
#'
#' A format is a number of decimals (`1`), a number of significant digits
#' (`4s`), or one only the ARD reads: `xx.x`, `xx.x%` (a proportion as a
#' percent), `xx.x*1000` (times 1000) or `pvalue` (`<0.001`, else 3
#' decimals).  In the ARD a parameter's own format is a step after the
#' format step, `cards::update_ard_fmt_fun(stat_names = "mean", fmt_fun =
#' 2L, filter = group2_level == "AUCLST")` (its group the place of PARAMCD
#' in `by`), and `4s` is `function(x) rtfreporter::fmt_signif(x, 4L)`, the
#' rule a table's `{mean:.4s}` prints by.
#'
#' @param formats The map (a named list, as YAML reads it).  `NULL` or an
#'   empty map: none.
#' @param route `"ard"` (default): the analysis's `formats` text, as the ARD
#'   definition's `analyses` sheet holds it (`N=0 | mean=1 | AUCLST:mean=2`).
#'   `"plan_digits"`: the arguments of `plan_digits()`.
#' @param by,variables The analysis's `by` and `variables` (names): whether
#'   a name in `formats` is a parameter or a variable.  Only
#'   `route = "plan_digits"` needs them; the ARD reads its own.
#' @return `route = "ard"`: one text, `NA` for none.  `route =
#'   "plan_digits"`: a named list, `continuous` the statistics' formats,
#'   `PARAMCD` the parameters' (a list by PARAMCD value) and a variable's own
#'   under its name; an empty list for none.
#' @examples
#' f <- list(N = 0, mean = 1, sd = 2, AUCLST = list(mean = 2, sd = "4s"))
#' tfl_formats(f)
#' tfl_formats(f, "plan_digits", by = c("TRT01A", "PARAMCD"), variables = "AVAL")
#' @export
tfl_formats <- function(formats, route = c("ard", "plan_digits"), by = NULL,
                        variables = NULL) {
  route <- match.arg(route)
  f <- .formats_flat(formats)
  if (route == "ard") {
    if (!length(f)) return(NA_character_)
    return(paste(paste0(names(f), "=", f), collapse = " | "))
  }
  bad <- names(f)[!grepl("^([0-9]+|[1-9][0-9]*s)$", f)]
  if (length(bad)) {
    .ard_stop(sprintf(paste(
      "formats: %s: plan_digits() takes a number of decimals (1) or significant",
      "digits (4s); xx.x%%, xx.x*1000 and pvalue are the ARD's formats"),
      paste(bad, collapse = ", ")))
  }
  val <- lapply(f, function(v) if (grepl("^[0-9]+$", v)) as.integer(v) else v)
  own <- grepl(":", names(f), fixed = TRUE)
  out <- list()
  if (any(!own)) out$continuous <- val[!own]
  key <- sub(":.*$", "", names(f))
  stat <- sub("^[^:]*:", "", names(f))
  par <- own & "PARAMCD" %in% by & !key %in% variables
  # a variable's own under its name; the parameters' under PARAMCD, by value
  for (i in which(own & !par)) {
    if (is.null(out[[key[i]]])) out[[key[i]]] <- list()
    out[[key[i]]][stat[i]] <- val[i]
  }
  for (i in which(par)) {
    if (is.null(out$PARAMCD)) out$PARAMCD <- list()
    if (is.null(out$PARAMCD[[key[i]]])) out$PARAMCD[[key[i]]] <- list()
    out$PARAMCD[[key[i]]][stat[i]] <- val[i]
  }
  out
}

# formats (a map, a name's own a map of its own or `name:stat`) as a named
# character vector, `stat` / `name:stat` -> format, checked: the names, a
# format each (.fmt_ok())
.formats_flat <- function(formats) {
  if (is.null(formats) || inherits(formats, "tfl_unset") || !length(formats)) {
    return(stats::setNames(character(), character()))
  }
  # the sheet's text (`mean=1 | AUC:mean=2`), or its parts as a sequence
  if (is.null(names(formats)) && all(vapply(formats, function(v) is.character(v) && length(v) == 1L, NA))) {
    formats <- as.list(.parse_formats(paste(unlist(formats), collapse = " | ")))
  }
  formats <- as.list(formats)
  nm <- names(formats)
  if (is.null(nm) || !all(nzchar(nm))) {
    .ard_stop("formats: is a map ({statistic: format}, a name's own {name: {statistic: format}}).")
  }
  one <- function(v, k) {
    if (is.numeric(v) && length(v) == 1L && !is.na(v)) {
      if (v < 0 || v != round(v)) {
        .ard_stop(sprintf("formats: %s: a number of decimals is a whole number (%s).", k, v))
      }
      return(format(v, scientific = FALSE))
    }
    if (!(is.character(v) && length(v) == 1L && !is.na(v))) {
      .ard_stop(sprintf("formats: %s: a format is one value (a number of decimals, 4s, xx.x ...).", k))
    }
    v
  }
  out <- character()
  for (k in nm) {
    v <- formats[[k]]
    if (is.list(v)) {
      if (grepl(":", k, fixed = TRUE)) .ard_stop(sprintf("formats: %s: a name's own is {name: {statistic: format}}.", k))
      sn <- names(v)
      if (is.null(sn) || !all(nzchar(sn))) .ard_stop(sprintf("formats: %s: is a map ({statistic: format}).", k))
      for (s in sn) out[paste0(k, ":", s)] <- one(v[[s]], paste0(k, ":", s))
    } else {
      out[k] <- one(v, k)
    }
  }
  bad <- names(out)[!.fmt_ok(out)]
  if (length(bad)) {
    .ard_stop(sprintf(paste(
      "formats: %s: a format is a number of decimals (1), significant digits (4s),",
      "xx.x, xx.x%%, xx.x*1000 or pvalue"), paste(bad, collapse = ", ")))
  }
  out
}
