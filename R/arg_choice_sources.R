# ============================================================================
#  The closed list of choice sources (canon 2, row 11-28, CG-4)
# ----------------------------------------------------------------------------
#  Where a pick's choices come from: 14 words, one list, kept here.  tflspec
#  checks every definition's `choices:` against it; tflplanner and the AI
#  kit read it through tfl_choice_sources() and keep no copy of their own.
# ============================================================================

# One row a word.  brackets: "none" (the word alone), "optional" (the word
# alone or with what is in its brackets) or "required".  takes: what its
# brackets hold.  free: a pick of it also takes a value not in the list.
.arg_choice_table <- data.frame(
  word = c("vars", "levels", "codelist", "columns", "populations", "study_data",
           "objects", "stats", "format_sets", "styles", "figure_style", "tokens",
           "fixed", "from_args"),
  brackets = c("optional", "required", "required", "optional", "none", "none",
               "required", "none", "none", "none", "required", "none",
               "none", "required"),
  takes = c("data", "variable", "name", "object", NA, NA,
            "kind", NA, NA, NA, "kind", NA,
            NA, "arguments"),
  free = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
           FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
           FALSE, FALSE),
  label_en = c("Columns", "Values", "Code list", "Columns", "Analysis sets", "Study data",
               "This report's objects", "Statistics", "Format sets", "Styles",
               "Figure style", "Values to insert", "Fixed values", "Arguments"),
  label_ja = c("\u5217", "\u5024", "\u30b3\u30fc\u30c9\u30ea\u30b9\u30c8", "\u5217",
               "\u89e3\u6790\u5bfe\u8c61\u96c6\u56e3", "\u8a66\u9a13\u306e\u30c7\u30fc\u30bf",
               "\u3053\u306e\u5e33\u7968\u306e\u30aa\u30d6\u30b8\u30a7\u30af\u30c8",
               "\u7d71\u8a08\u91cf", "\u66f8\u5f0f\u306e\u7d44", "\u30b9\u30bf\u30a4\u30eb",
               "\u56f3\u306e\u30b9\u30bf\u30a4\u30eb", "\u5dee\u3057\u8fbc\u3080\u5024",
               "\u6c7a\u307e\u3063\u305f\u5024", "\u5f15\u6570"),
  description = c(
    "The columns of a data row of this report or a study dataset (vars(<data>)); alone: the current input's.",
    "The values of a variable (levels(<var>)); another argument's variable: levels(from_args(<arg>)).",
    "The terms of a code list of the study (codelist(<name>)).",
    "The columns of an object (columns(<object>); columns(ard): the ARD's key columns); alone: the current content's.",
    "The study's analysis sets.",
    "The study's datasets.",
    "The objects made above in this report, of one kind (objects(<kind>)); never the study's datasets.",
    "The statistics' names.",
    "The study's format sets (the values of `formats`); none listed until format sets are built.",
    "The study's styles.",
    "The study's figure style's colours in their order, or its marker shapes (figure_style(<palette|marker>)).",
    "The values to insert ({STUDY} ...).",
    "The definition's values: (or its enum's).",
    "The values of the card's other arguments (from_args(<arg>, ...))."),
  stringsAsFactors = FALSE)

# the words, in the list's order
.arg_choice_sources <- .arg_choice_table$word

# the words that let a pick take a value not in the list (`free`) unless the
# definition says otherwise
.arg_free_sources <- .arg_choice_table$word[.arg_choice_table$free]

# what objects(<kind>) lists: the objects made above in this report of one
# kind; what figure_style(<kind>) lists: the study's figure style's colours
# in their order, or its marker shapes
.arg_object_kinds <- c("data", "ard", "model", "value", "plot")
.arg_figure_style_kinds <- c("palette", "marker")

# the words that are gone, and what to write instead
.arg_choice_old <- c(report_data = "objects(data)",
                     ard = "objects(ard) (the ARD itself) or columns(ard) (its key columns)",
                     study_info = "tokens")

#' The closed list of choice sources
#'
#' The 14 words a definition's `choices:` may use (an argument of a template
#' or of the code SPEC's schema, [tfl_arg_defs()]' `choices` column): where
#' a pick's choices come from.  This is the one list; tflplanner and the AI
#' kit read it here.
#'
#' A `choices:` entry writes one word, with what it depends on in brackets
#' (`vars(adsl_sub)`, `levels(SEX)`, `levels(from_args(by))`,
#' `figure_style(palette)`); several separated by `", "` give the list's
#' groups in that order (`"objects(data), study_data"`).  In the brackets of
#' `vars()`, `levels()` and `columns()`, `from_args(<arg>)` names the value
#' of another argument of the card.  `vars` and `columns` may also be
#' written alone (the current input's columns, the current content's).
#' `fixed` is filled from the definition's `values:` or its enum.  An old
#' word (`report_data`, `ard`, `study_info`) stops with the word to write.
#'
#' @return A data frame, one row a word, in the list's order:
#'   \describe{
#'     \item{word}{the word.}
#'     \item{brackets}{`none` (written alone), `optional` or `required`.}
#'     \item{takes}{what its brackets hold: `data` (a data row id or a
#'       study dataset), `variable`, `name` (a code list's), `object`, `kind`
#'       (one of the attribute `"kinds"`'s for that word), `arguments`;
#'       `NA` for none.}
#'     \item{free}{a pick of this word also takes a value not in the list,
#'       unless the definition says otherwise.}
#'     \item{label_en, label_ja}{the name of its group in a list of several.}
#'     \item{description}{what it lists.}
#'   }
#'   The attribute `"kinds"` is a list by word of the kinds its brackets
#'   take (`objects`: data, ard, model, value, plot; `figure_style`:
#'   palette, marker); the attribute `"old"` a named character vector, an
#'   old word and what to write instead.
#' @seealso [tfl_arg_defs()]
#' @examples
#' tfl_choice_sources()$word
#' attr(tfl_choice_sources(), "kinds")$objects
#' @export
tfl_choice_sources <- function() {
  out <- .arg_choice_table
  attr(out, "kinds") <- list(objects = .arg_object_kinds, figure_style = .arg_figure_style_kinds)
  attr(out, "old") <- .arg_choice_old
  out
}

# the source words of a choices entry, in order, without their brackets
.arg_choice_words <- function(choices) {
  if (is.null(choices) || !length(choices) || all(is.na(choices))) {
    return(character())
  }
  x <- paste(as.character(choices), collapse = ", ")
  # commas inside brackets belong to the word's argument
  depth <- 0L
  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  for (i in seq_along(chars)) {
    if (chars[i] == "(") depth <- depth + 1L
    if (chars[i] == ")") depth <- depth - 1L
    if (chars[i] == "," && depth > 0L) chars[i] <- "\001"
  }
  parts <- trimws(strsplit(paste(chars, collapse = ""), ",", fixed = TRUE)[[1L]])
  parts <- gsub("\001", ",", parts, fixed = TRUE)
  parts[nzchar(parts)]
}

.arg_check_choices <- function(choices, where) {
  entries <- .arg_choice_words(choices)
  words <- trimws(sub("[(].*$", "", entries))
  q <- function(x) paste(encodeString(x, quote = "\""), collapse = ", ")
  old <- intersect(words, names(.arg_choice_old))
  if (length(old)) {
    stop(sprintf("%s: the choice source %s is gone; write %s.", where, q(old[1L]),
                 .arg_choice_old[[old[1L]]]), call. = FALSE)
  }
  bad <- setdiff(words, .arg_choice_sources)
  if (length(bad)) {
    stop(sprintf("%s: unknown choice source %s; the sources are %s.", where, q(bad),
                 paste(.arg_choice_sources, collapse = ", ")), call. = FALSE)
  }
  has <- grepl("[(]", entries)
  shape <- .arg_choice_table$brackets[match(words, .arg_choice_sources)]
  if (any(has & shape == "none")) {
    stop(sprintf("%s: %s: this choice source is written alone, without brackets.", where,
                 q(entries[has & shape == "none"])), call. = FALSE)
  }
  if (any(!has & shape == "required")) {
    w <- words[!has & shape == "required"][1L]
    takes <- .arg_choice_table$takes[match(w, .arg_choice_sources)]
    stop(sprintf("%s: %s: write %s(<%s>).", where, q(w), w, takes), call. = FALSE)
  }
  form <- "^[a-z_]+\\s*[(].*[)]$"
  if (any(has & !grepl(form, entries))) {
    stop(sprintf("%s: %s: a choice source's brackets close at its end.", where,
                 q(entries[has & !grepl(form, entries)])), call. = FALSE)
  }
  inner <- trimws(sub("^[a-z_]+\\s*[(](.*)[)]$", "\\1", entries))
  inner[!has] <- ""
  if (any(has & !nzchar(inner))) {
    stop(sprintf("%s: %s: the brackets are empty.", where, q(entries[has & !nzchar(inner)][1L])),
         call. = FALSE)
  }
  # vars(), levels(), columns(), codelist(): one name, or (not codelist)
  # from_args(<arg>) (the value of another argument).  Nothing else nests.
  name <- "^[A-Za-z.][A-Za-z0-9._]*$"
  one <- words %in% c("vars", "levels", "columns", "codelist") & has
  dep <- "^from_args\\s*[(]\\s*[A-Za-z.][A-Za-z0-9._]*\\s*[)]$"
  ok <- grepl(name, inner) | (words != "codelist" & grepl(dep, inner))
  nest <- one & !ok
  if (any(nest)) {
    stop(sprintf("%s: %s() takes %s, not %s.", where, words[nest][1L],
                 if (words[nest][1L] == "codelist") "one name" else "one name or from_args(<arg>)",
                 q(inner[nest][1L])), call. = FALSE)
  }
  for (w in c("objects", "figure_style")) {
    kinds <- if (w == "objects") .arg_object_kinds else .arg_figure_style_kinds
    k <- words == w & !inner %in% kinds
    if (any(k)) {
      stop(sprintf("%s: %s: write %s(<kind>), the kind one of %s.", where, q(entries[k]), w,
                   paste(kinds, collapse = ", ")), call. = FALSE)
    }
  }
  names_re <- "^[A-Za-z.][A-Za-z0-9._]*(\\s*,\\s*[A-Za-z.][A-Za-z0-9._]*)*$"
  if (any(words == "from_args" & !grepl(names_re, inner))) {
    stop(sprintf("%s: %s: from_args() takes the names of the card's arguments.", where,
                 q(entries[words == "from_args"][1L])), call. = FALSE)
  }
  invisible(choices)
}

# A choices entry that names another argument of the card bare where its
# value is meant: levels(by) for levels(from_args(by)).  `names`: the card's
# arguments.
.arg_check_choice_deps <- function(choices, names, where) {
  entries <- .arg_choice_words(choices)
  inner <- trimws(sub("^levels[(](.*)[)]$", "\\1", entries[grepl("^levels[(].*[)]$", entries)]))
  dep <- inner[inner %in% names]
  if (length(dep)) {
    stop(sprintf("%s: levels(%s) names the argument `%s`; write levels(from_args(%s)).", where,
                 dep[1L], dep[1L], dep[1L]), call. = FALSE)
  }
  invisible(choices)
}
