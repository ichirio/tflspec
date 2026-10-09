# ============================================================================
#  The problems of a definition, as rows
# ----------------------------------------------------------------------------
#  The constructors (tfl_table_spec(), tfl_ard_spec(), tfl_listing_spec())
#  find their problems by collecting them, one row each: the report, the
#  sheet, the row's key (the way the sheet is keyed: `AGE` on `variables`,
#  the analysis_id on `analyses`), the column and the message.  A constructor
#  still stops -- tfl_table_spec() on the first problem, as it always did,
#  tfl_ard_spec() and tfl_listing_spec() with all of them, one line each --
#  and its condition carries the rows as `problems`, so a GUI reads where
#  the problem is instead of parsing the message.  tfl_review_spec() takes
#  the same rows without stopping.
# ============================================================================

# One problem; `i` (a check's own row index) is mapped to the row's key by
# .spec_problems_keyed()
.pb <- function(message, i = NA_integer_, field = "") {
  data.frame(i = as.integer(i), field = field, message = message,
             stringsAsFactors = FALSE)
}

.pb_none <- function() .pb(character(), integer(), character())

# A check's rows (`i`, `field`, `message`) as the rows of a definition's
# problems: the report (`output_id`) and the key of row `i` of `d`
.spec_problems_keyed <- function(p, d, sheet, keys = .ard_spec_keys[[sheet]]) {
  if (is.null(p) || !nrow(p)) return(.spec_problems_empty())
  ok <- !is.na(p$i) & p$i <= NROW(d)
  oid <- rep(NA_character_, nrow(p))
  if (!is.null(d) && "output_id" %in% names(d)) {
    oid[ok] <- as.character(d$output_id[p$i[ok]])
  }
  row <- rep("", nrow(p))
  if (length(keys) && !anyNA(keys) && all(keys %in% names(d))) {
    k <- do.call(paste, c(lapply(keys, function(cn) {
      v <- as.character(d[[cn]])
      ifelse(is.na(v), "", v)
    }), sep = " / "))
    row[ok] <- k[p$i[ok]]
  }
  data.frame(output_id = oid, sheet = sheet, row = row,
             field = ifelse(is.na(p$field), "", p$field),
             message = p$message, stringsAsFactors = FALSE)
}

.spec_problems_empty <- function() {
  data.frame(output_id = character(), sheet = character(), row = character(),
             field = character(), message = character(),
             stringsAsFactors = FALSE)
}

.spec_problem <- function(message, sheet = "", output_id = NA_character_,
                          row = "", field = "") {
  data.frame(output_id = as.character(output_id), sheet = sheet, row = row,
             field = field, message = message, stringsAsFactors = FALSE)
}

# Stop with `message`, the condition carrying the problem rows: a GUI
# reads them as conditionCall-free data (`cnd$problems`)
.spec_stop <- function(message, problems, class) {
  rownames(problems) <- NULL
  stop(structure(class = c(class, "tflspec_spec_error", "error", "condition"),
                 list(message = message, call = NULL, problems = problems)))
}

# A check of rows that stops on its first problem, as the checks did
# before they collected
.pb_stop_first <- function(p) {
  if (nrow(p)) .ard_stop(p$message[1L])
  invisible(NULL)
}
