#' The names a SPEC's R code uses
#'
#' Which of `names` each piece of R code reads -- a `where`, a `derive`, an
#' analysis's `args` or `code`, an `!r` value.  For a screen that renames a
#' data or an object: the code that still says the old name is found, to
#' warn of it (the code is not changed).  The code is parsed and its symbols
#' read, so a name inside a string or a comment is not one; code that does
#' not parse is searched for the names as whole words.
#'
#' @param code A character vector of R code (or a list of [tfl_r()] values).
#' @param names The names to look for (data, objects).
#' @return A data frame, one row a use: `code` (the element's position in
#'   `code`, or its name when `code` is named) and `name`.  No rows when
#'   none is used.
#' @examples
#' tfl_code_refs(c(where = "AGE >= 65 & USUBJID %in% adsl_sub$USUBJID",
#'                 derive = "n_jp / 2"),
#'               c("adsl_sub", "n_jp", "adae_sub"))
#' @export
tfl_code_refs <- function(code, names) {
  if (is.list(code)) code <- vapply(code, function(x) paste(as.character(x), collapse = "\n"), "")
  if (!is.character(code) || !is.character(names)) {
    .ard_stop("tfl_code_refs(): `code` and `names` are character vectors.")
  }
  at <- if (!is.null(base::names(code)) && all(nzchar(base::names(code)))) base::names(code)
        else as.character(seq_along(code))
  rows <- lapply(seq_along(code), function(i) {
    x <- code[[i]]
    if (is.na(x) || !nzchar(trimws(x))) return(NULL)
    e <- tryCatch(parse(text = x, keep.source = FALSE), error = function(e) NULL)
    used <- if (!is.null(e)) {
      intersect(names, all.names(e))
    } else {
      # (not R yet: the names as whole words)
      names[vapply(names, function(n) grepl(paste0("(^|[^A-Za-z0-9._])", gsub(".", "\\.", n, fixed = TRUE),
                                                  "($|[^A-Za-z0-9._])"), x), NA)]
    }
    if (length(used)) data.frame(code = at[i], name = used, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(code = character(), name = character(),
                                          stringsAsFactors = FALSE)), rows))
  rownames(out) <- NULL
  out
}

#' Every data and object a report's program makes, by name
#'
#' The names a report's program gives what it makes: the analysis data of
#' its ARD definition (`data_id`, and the analysis sets' and records' own
#' `<dataset>_sub`), the ARD (`ard`), a figure's data steps that name what
#' they make (`out:`, a fit's `name:`).  For a screen that lists a report's
#' data and lets a person rename one (the rename itself is the SPEC's,
#' [tfl_code_refs()] finds the code that still says the old name).
#'
#' @param output A report ([tfl_read_output()]); one with several objects
#'   (`contents:`) gives each object's.
#' @param study The study ([tfl_read_study_spec()]).
#' @return A data frame, one row a name: `output_id`, `object` (the object
#'   of a report with `contents:`, else `NA`), `name`, `kind` (`subset`: a
#'   data the program keeps; `ard`; `fit`: a model a figure's step fits),
#'   `from` (what it is made from: a dataset, a data above, `NA`), `used_by`
#'   (the analyses or steps that read it, ` | ` between them).
#' @seealso [tfl_ard_data_names()], [tfl_code_refs()]
#' @export
tfl_output_data_names <- function(output, study) {
  if (!inherits(output, "tfl_output")) .ard_stop("tfl_output_data_names(): `output` is a report (tfl_read_output()).")
  row <- function(object, name, kind, from = NA_character_, used_by = NA_character_) {
    data.frame(output_id = output$id, object = object, name = name, kind = kind,
               from = from, used_by = used_by, stringsAsFactors = FALSE)
  }
  out <- list(data.frame(output_id = character(), object = character(), name = character(),
                         kind = character(), from = character(), used_by = character(),
                         stringsAsFactors = FALSE))
  # the ARD's: the analysis data by name, what each analysis reads
  ad <- output$ard$analysis_data %||% list()
  an <- output$ard$analyses %||% list()
  if (length(an)) {
    dn <- tryCatch(tfl_ard_data_names(study, output_id = output$id), error = function(e) NULL)
    defined <- vapply(ad, function(a) as.character(a$id %||% NA_character_), "")
    froms <- vapply(ad, function(a) as.character(a$from %||% NA_character_), "")
    names_used <- if (!is.null(dn)) unique(dn$data[dn$data != "NULL"]) else character()
    for (nm in unique(c(defined, names_used))) {
      if (is.na(nm)) next
      by <- if (!is.null(dn)) dn$analysis_id[dn$data %in% nm] else character()
      out[[length(out) + 1L]] <- row(NA_character_, nm, "subset",
                                     if (nm %in% defined) froms[match(nm, defined)] else
                                       toupper(sub("_sub[0-9]*$", "", nm)),
                                     if (length(by)) paste(by, collapse = " | ") else NA_character_)
    }
    out[[length(out) + 1L]] <- row(NA_character_, "ard", "ard")
  }
  # a figure's data steps that name what they make
  steps_names <- function(o, object) {
    st <- o$data$steps %||% list()
    prev <- NA_character_
    for (s in st) {
      if (!is.list(s)) next
      if (!is.null(s$dataset)) prev <- toupper(as.character(s$dataset))
      nm <- s$out %||% s$name
      if (!is.null(nm)) {
        kind <- if (identical(s$step, "survfit") || !is.null(s$name)) "fit" else "subset"
        out[[length(out) + 1L]] <<- row(object, as.character(nm), kind, prev, s$step %||% NA_character_)
        if (identical(kind, "subset")) prev <- as.character(nm)
      }
    }
  }
  if (!is.null(output$contents)) {
    for (o in .output_objects(output)) {
      if (!is.null(output$contents[[attr(o, "object")]]$data)) steps_names(o, attr(o, "object"))
    }
  } else {
    steps_names(output, NA_character_)
  }
  r <- do.call(rbind, out)
  rownames(r) <- NULL
  r
}
