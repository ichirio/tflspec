# ============================================================================
#  tfl_adam_metadata() -- the ADaM metadata an assistant may name
# ============================================================================
#
#  An assistant writing specs from a TOC and shells has to name datasets,
#  variables and their levels, and without being told it guesses them from
#  the ADaM IG -- plausibly, and without saying so.  The fix is the study's
#  own metadata, and never the data: a define.xml or an ADaM spec says it
#  outright, and failing both the datasets' column information does.  One
#  frame, one row a variable, so the chat kit, tfl_ai_context() and
#  tflplanner's bulk import read the same names.

#' The ADaM metadata to give an AI assistant
#'
#' One data frame, one row a variable, of what an assistant may name when it
#' writes specs: the datasets, their variables, labels and kinds, and each
#' variable's levels (its code list's values).  It is what
#' [tflspec_ai_manual()] writes as `08_adam_metadata.csv`, what
#' [tfl_ai_context()] takes as `datasets`, and what [tfl_ai_review()] checks
#' a spec's names against.
#'
#' Give **one** source.  When several are given, the first of this order is
#' used, and a message says which:
#'
#' 1. `define`: the study's `define.xml` ([tfl_read_define()]), the delivered
#'    datasets as described for submission;
#' 2. `adam_spec`: the ADaM specification workbook
#'    ([tfl_read_define_spec()]), for when the define is older than the data
#'    or not written yet;
#' 3. `datasets`: the datasets themselves, of which only the column
#'    information is taken: the names, the labels (the `label` attribute),
#'    the kinds, the levels of a factor, and the distinct values of a short
#'    coded character column (at most `max_levels` of them).  A subject's
#'    values never are: identifiers (`...ID`), dates and times (`...DTC`,
#'    `...DT`, `...DTM`, `...TM`) and free text (a value longer than 40
#'    characters) are left without levels.
#'
#' Two sources that disagree (a variable renamed in the spec but not yet in
#' the define) would leave the assistant to choose, which is why one is used.
#'
#' @param define A `define.xml`, or a [tfl_read_define()] result.
#' @param adam_spec An ADaM spec workbook (`.xlsx`, read by
#'   [tfl_read_define_spec()] with its default profile), or a
#'   [tfl_read_define_spec()] result.
#' @param datasets The ADaM datasets: a named list of data frames
#'   (`list(ADSL = adsl, ADAE = adae)`), or a metadata frame of this form
#'   already.
#' @param max_levels For `datasets`: the most distinct values a character
#'   column may have for them to be listed as its levels.
#' @return A data frame: `dataset`, `dataset_label`, `variable`, `label`,
#'   `kind` (`character`, `numeric`, `date`, `factor`, `logical`), `levels`
#'   (the values, `" | "` between them; `NA` for none), `codelist` (the code
#'   list's name or OID; `NA` for none) and `source` (`"define.xml"`,
#'   `"ADaM spec"` or `"datasets"`), with `attr(, "source")`.
#' @examples
#' if (requireNamespace("xml2", quietly = TRUE)) {
#'   md <- tfl_adam_metadata(define = system.file("extdata", "define",
#'                                                "define.xml",
#'                                                package = "tflspec"))
#'   md[md$variable %in% c("SEX", "AGEGR1"), c("dataset", "variable", "levels")]
#' }
#' # from the data: names, labels, kinds and short code lists only
#' adsl <- data.frame(USUBJID = c("01-001", "01-002"), SEX = c("F", "M"),
#'                    AGE = c(63, 71))
#' tfl_adam_metadata(datasets = list(ADSL = adsl))
#' @seealso [tflspec_ai_manual()] for the kit it goes into.
#' @export
tfl_adam_metadata <- function(define = NULL, adam_spec = NULL, datasets = NULL,
                              max_levels = 30L) {
  fn <- "tfl_adam_metadata()"
  given <- c(define = !is.null(define), adam_spec = !is.null(adam_spec),
             datasets = !is.null(datasets))
  if (!any(given)) {
    .ard_stop(sprintf(
      "%s: give one of `define`, `adam_spec` or `datasets`.", fn))
  }
  use <- names(given)[given][1L]
  if (sum(given) > 1L) {
    message(sprintf(
      "%s: using `%s`; %s not used (one source, so the names cannot disagree).",
      fn, use, paste(sprintf("`%s`", setdiff(names(given)[given], use)),
                     collapse = " and ")))
  }
  out <- switch(use,
    define = .ai_md_define(.ai_md_read(define, "define", fn), "define.xml"),
    adam_spec = .ai_md_define(.ai_md_read(adam_spec, "adam_spec", fn),
                              "ADaM spec"),
    datasets = .ai_md_data(datasets, max_levels, fn))
  attr(out, "source") <- unique(out$source)
  out
}

.ai_md_cols <- c("dataset", "dataset_label", "variable", "label", "kind",
                 "levels", "codelist", "source")

.ai_md_read <- function(x, what, fn) {
  if (inherits(x, "tfl_define")) return(x)
  if (!is.character(x) || length(x) != 1L || !file.exists(x)) {
    .ard_stop(sprintf(
      "%s: `%s` is a file that exists, or what tfl_read_define%s() returns.",
      fn, what, if (what == "define") "" else "_spec"))
  }
  if (what == "define") tfl_read_define(x) else tfl_read_define_spec(x)
}

# A define (from the XML or from a spec) as the metadata frame.
.ai_md_define <- function(def, source) {
  v <- def$variables
  ds <- def$datasets
  cl <- def$codelists
  kind <- tolower(as.character(v$type))
  kind <- ifelse(kind %in% c("text", "string"), "character",
          ifelse(kind %in% c("integer", "float", "double", "decimal"), "numeric",
          ifelse(grepl("date|time", kind), "date",
          ifelse(is.na(kind) | !nzchar(kind), NA_character_, kind))))
  lv <- vapply(as.character(v$codelist), function(id) {
    if (is.na(id) || !nzchar(id) || is.null(cl)) return(NA_character_)
    x <- cl[cl$codelist %in% id, , drop = FALSE]
    if (!nrow(x)) return(NA_character_)
    if ("order" %in% names(x) && !all(is.na(x$order))) {
      x <- x[order(suppressWarnings(as.numeric(x$order))), , drop = FALSE]
    }
    paste(unique(as.character(x$value)), collapse = " | ")
  }, "", USE.NAMES = FALSE)
  cl_name <- vapply(as.character(v$codelist), function(id) {
    if (is.na(id) || !nzchar(id)) return(NA_character_)
    nm <- if (!is.null(cl)) unique(cl$name[cl$codelist %in% id]) else character()
    nm <- nm[!is.na(nm) & nzchar(nm)]
    if (length(nm)) nm[1L] else id
  }, "", USE.NAMES = FALSE)
  out <- data.frame(
    dataset = toupper(as.character(v$dataset)),
    dataset_label = if (!is.null(ds)) {
      as.character(ds$label[match(toupper(v$dataset), toupper(ds$dataset))])
    } else NA_character_,
    variable = as.character(v$variable),
    label = as.character(v$label),
    kind = kind, levels = lv, codelist = cl_name, source = source,
    stringsAsFactors = FALSE)
  .ai_md_tidy(out)
}

# The datasets themselves: their column information, never a subject's values.
.ai_md_data <- function(x, max_levels, fn) {
  if (is.data.frame(x)) {
    d <- x
    for (cn in setdiff(.ai_md_cols, names(d))) d[[cn]] <- NA_character_
    base <- .ai_ctx_datasets(d[c("dataset", "dataset_label", "variable",
                                 "label", "kind", "levels")], fn)
    base$codelist <- as.character(d$codelist)
    base$source <- ifelse(is.na(d$source), "datasets", as.character(d$source))
    return(.ai_md_tidy(base))
  }
  if (!.ai_is_data_list(x)) {
    .ard_stop(sprintf(paste0(
      "%s: `datasets` is a named list of data frames (the datasets), or a ",
      "metadata frame (one row a variable)."), fn))
  }
  base <- .ai_ctx_from_data(x)
  base$levels <- unlist(lapply(names(x), function(nm) {
    vapply(x[[nm]], .ai_md_levels, "", max_levels = max_levels,
           USE.NAMES = FALSE)
  }), use.names = FALSE)
  # identifiers, dates and times carry a subject's values: never listed
  hide <- grepl("ID$|(DTC|DT|DTM|TM)$", base$variable)
  base$levels[hide] <- NA_character_
  base$codelist <- NA_character_
  base$source <- "datasets"
  .ai_md_tidy(base)
}

.ai_md_levels <- function(v, max_levels) {
  if (is.factor(v)) return(paste(levels(v), collapse = " | "))
  if (!is.character(v)) return(NA_character_)
  u <- unique(v[!is.na(v) & nzchar(v)])
  if (!length(u) || length(u) > max_levels || any(nchar(u) > 40L)) {
    return(NA_character_)
  }
  paste(sort(u), collapse = " | ")
}

.ai_md_tidy <- function(d) {
  for (cn in setdiff(.ai_md_cols, names(d))) d[[cn]] <- NA_character_
  d <- d[.ai_md_cols]
  d[] <- lapply(d, function(v) {
    v <- trimws(as.character(v))
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    v
  })
  d$dataset <- toupper(d$dataset)
  rownames(d) <- NULL
  d
}
