# ============================================================================
#  What an agent learned from past TFLs, set against the company standard
#  (tflspec#547, step 2)
# ----------------------------------------------------------------------------
#  tfl_ai_style_prompt() asks for these conventions, a kind of report at a
#  time; tfl_ai_style_read() reads them; tfl_ai_style_diff() lines them up
#  with the company standard's, for the person to adopt a line at a time.
#  The standard itself is the app's (tflplanner's company standards): it
#  comes here as a data frame of kind, key and value.
# ============================================================================

# The kinds of report, as tfl_rtf_style_tables() guesses them; `all` for
# what every kind shares
.ai_style_kinds <- c("all", "demog", "ae_soc_pt", "ae", "param_change", "shift", "listing",
                     "figure", "other")

# The conventions the prompt asks for: `from` "numbers" when R's tables
# give them, "files" when the agent reads them in the reports
.ai_style_keys <- data.frame(
  key = c("rules", "spanner_rule", "stub_ratio", "font", "orientation", "titles", "footnotes",
          "n_form", "categorical", "zero", "continuous", "digits", "stub_heading", "first_row",
          "sort"),
  what = c(
    "where lines are drawn (`above_header | under_header | end`)",
    "a spanner's underline: over its columns only, the whole width, or none",
    "the stub's width to a value column's (`3`)",
    "the font and its size (`Courier New 9 pt`)",
    "the page (`landscape`, `portrait`)",
    "the titles' place, alignment and lines (`page header, centred: Table <number> / <title> / (<population>)`)",
    "the footnotes' place and alignment, and the lines every report of the kind has, with placeholders",
    "the column header's N (`(N=xx)`, `N = xx`)",
    "a count with its percent (`xx (xx.x%)`, `xx (xx.x)`)",
    "a zero count (`0`, `0 (0.0%)`, `-`)",
    "the statistics of a continuous variable, in order, with their labels (`n | Mean (SD) | Median | Min, Max`)",
    "the decimals, as a rule (`mean and median one more than the data, SD two more`) or numbers",
    "the stub's column heading (`Characteristic`, empty)",
    "a first row before the rest (`Any TEAE`)",
    "the rows' order (`SOC and PT by descending frequency in the total column`)"),
  from = c("numbers", "numbers", "numbers", "numbers", "numbers", "files", "files",
           "files", "files", "files", "files", "files", "files", "files", "files"),
  stringsAsFactors = FALSE)

#' Set what an agent learned from past TFLs against the company standard
#'
#' Lines up each convention [tfl_ai_style_read()] read with the company
#' standard's for the same kind of report and name, so that a person can
#' adopt them a line at a time.  A standard's value for kind `all` stands
#' for every kind that has none of its own.
#'
#' @param learned What [tfl_ai_style_read()] returns, or the agent's file.
#' @param standard The company standard's conventions: a data frame with
#'   `kind`, `key` and `value` (one row a convention; kind `all` for every
#'   kind), or `NULL` for none.
#' @return A data frame, one row a convention either side has: `kind`,
#'   `key`, `standard`, `learned`, `seen`, `of`, `adopt` (the learned one's,
#'   see [tfl_ai_style_read()]), and `status`: `"same"`, `"differs"`,
#'   `"new"` (the standard has none) or `"not learned"` (the agent found
#'   none).  Values are compared with their spaces trimmed.  Ordered by
#'   kind (as [tfl_ai_style_prompt()] lists them) and then as learned.
#' @seealso [tfl_ai_style_prompt()]
#' @examples
#' f <- tempfile(fileext = ".yml")
#' writeLines(c("tfl_style_learned: {version: 2, files: 3, tables: 9}",
#'              "kinds:",
#'              "  demog:",
#'              "    n_form: {value: \"(N=xx)\", seen: 3, of: 3}",
#'              "    zero: {value: \"0\", seen: 1, of: 3}"), f)
#' std <- data.frame(kind = c("all", "demog"), key = c("n_form", "categorical"),
#'                   value = c("N = xx", "xx (xx.x%)"))
#' tfl_ai_style_diff(f, std)
#' @export
tfl_ai_style_diff <- function(learned, standard = NULL) {
  if (is.character(learned) && length(learned) == 1L) learned <- tfl_ai_style_read(learned)
  if (!inherits(learned, "tfl_ai_style")) {
    .ard_stop("tfl_ai_style_diff(): `learned` is what tfl_ai_style_read() returns, or its file.")
  }
  if (is.null(standard)) standard <- data.frame(kind = character(), key = character(), value = character())
  if (!is.data.frame(standard) || !all(c("kind", "key", "value") %in% names(standard))) {
    .ard_stop("tfl_ai_style_diff(): `standard` is a data frame with `kind`, `key` and `value`.")
  }
  standard <- data.frame(kind = as.character(standard$kind), key = as.character(standard$key),
                         value = as.character(standard$value), stringsAsFactors = FALSE)
  squish <- function(v) gsub("\\s+", " ", trimws(v), perl = TRUE)
  std_of <- function(k, key) {
    v <- standard$value[standard$kind == k & standard$key == key]
    if (!length(v) && k != "all") v <- standard$value[standard$kind == "all" & standard$key == key]
    if (length(v)) v[1L] else NA_character_
  }
  l <- as.data.frame(learned)
  std <- vapply(seq_len(nrow(l)), function(i) std_of(l$kind[i], l$key[i]), "")
  status <- ifelse(is.na(std), "new", ifelse(squish(std) == squish(l$value), "same", "differs"))
  out <- data.frame(kind = l$kind, key = l$key, standard = std, learned = l$value,
                    seen = l$seen, of = l$of, adopt = l$adopt, status = status,
                    stringsAsFactors = FALSE)
  # the standard's conventions the agent found none of (its own kinds only)
  seen_pair <- paste(l$kind, l$key)
  rest <- standard[!paste(standard$kind, standard$key) %in% seen_pair &
                     !(standard$kind == "all" & standard$key %in% l$key), , drop = FALSE]
  if (nrow(rest)) {
    out <- rbind(out, data.frame(kind = rest$kind, key = rest$key, standard = rest$value,
                                 learned = NA_character_, seen = NA_integer_, of = NA_integer_,
                                 adopt = NA_character_, status = "not learned",
                                 stringsAsFactors = FALSE))
  }
  ord <- match(out$kind, .ai_style_kinds)
  ord[is.na(ord)] <- length(.ai_style_kinds) + 1L
  out <- out[order(ord, seq_len(nrow(out))), , drop = FALSE]
  rownames(out) <- NULL
  out
}
