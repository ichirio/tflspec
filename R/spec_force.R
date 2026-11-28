# ============================================================================
#  The study's force (canon 1.33; PRINCIPLES.md 11-10 :363-:369, :378, 11-11
#  :400): what every report of the study uses, whatever a layer, a template
#  or the report itself says.  Only the rounding is forced.
#
#    spec/defaults.yml
#      force:
#        content:
#          - plan_digits:
#              rounding: sas                 # or r
#
#  `force:` is a map by step; its content part has the form of `content:`.
#  Any other step or card stops ("only the rounding is forced"), as a row
#  of the file's one stop of its keys (R/spec_keys.R).  The one reader of
#  the study's rounding is .study_rounding() (exported: tfl_study_rounding(),
#  which also sets it); the writers write it in one form, the block form
#  above, through .force_of() / .force_out() (YAML lists) or .force_lines()
#  (text).
#  The rounding goes to every program's setup, the reports' and the ARDs'
#  (options(rtfreporter.rounding = , cards.round_type = )), to an in-process
#  build's options (.study_rounding_option()), and over a table's own
#  plan_digits(rounding = ) when it is resolved (.force_over()).  A study
#  that forces no rounding writes no options().
# ============================================================================

# What the force may hold, said where anything else is written (the block
# form under it: .force_yaml())
.force_only <- "only the rounding is forced (canon 1.33), sas or r"

# defaults.yml's `force:` (`x`, as YAML read it) as the study holds it:
# list(content = <the one plan_digits(rounding = ) item>), list() for none.
# `where`: the file, for the messages.  A force that holds anything else is
# a problem of the file's keys (.spec_keys_raise(): one classed stop; while
# a study is read, gathered with study.yml's), and reads as none.
.force_read <- function(x, where = "spec/defaults.yml") {
  if (is.null(x)) return(list())
  bad <- function(key, what) {
    .spec_keys_raise(.spec_keys_rows(where, key, "value", sprintf("%s; %s.", what, .force_only),
                                     more = paste(.force_yaml("sas"), collapse = "\n")))
    list()
  }
  if (!.is_map(x)) return(bad("force", "is a map by step"))
  other <- setdiff(names(x), "content")
  if (length(other)) return(bad(paste0("force: ", other[1L]), "is not a step the force holds"))
  cont <- x$content
  if (!is.list(cont) || !length(cont) || !is.null(names(cont)) || inherits(cont, "tfl_r")) {
    return(bad("force: content", "is a list of cards, as `content:` is"))
  }
  bad_value <- function(r) {
    bad("force: content: plan_digits: rounding", sprintf(
      "takes \"sas\" or \"r\", not %s",
      if (is.character(r)) paste0("\"", paste(r, collapse = ", "), "\"") else "that value"))
  }
  # (the value checked before the card is made: a card checks its own
  # rounding, with its own words)
  for (cd in cont) {
    a <- if (.is_map(cd) && length(cd) == 1L) cd[[1L]]
    if (.is_map(a) && "rounding" %in% names(a) && !.force_ok(a$rounding)) return(bad_value(a$rounding))
  }
  items <- tryCatch(lapply(cont, .item_from_list), error = function(e) e)
  if (inherits(items, "error")) return(bad("force: content", conditionMessage(items)))
  for (it in items) {
    if (!identical(.schema_fn_name(it$fn %||% ""), "plan_digits") ||
        !identical(names(it$args), "rounding")) {
      return(bad(paste0("force: content: ", it$fn %||% "?"),
                 sprintf("is `%s: {%s}`", it$fn %||% "?", paste(names(it$args), collapse = ", "))))
    }
  }
  if (length(items) > 1L) return(bad("force: content", "has more than one card"))
  it <- items[[1L]]
  r <- it$args$rounding
  if (!.force_ok(r)) return(bad_value(r))
  it$from <- "force"
  list(content = list(it))
}

# A forced rounding as it is spelled: "sas" or "r"
.force_ok <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && x %in% c("sas", "r")

# The forced rounding is "sas" or "r" (canon 11-10 :368), spelled so
.force_check <- function(x, where) {
  if (!.force_ok(x)) {
    stop(where, " takes \"sas\" or \"r\", not ",
         if (is.character(x)) paste0("\"", paste(x, collapse = ", "), "\"") else "that value",
         call. = FALSE)
  }
  x
}

# The study's rounding, "sas" or "r"; NULL when the study forces none.  The
# one reader: a study spec's force (defaults.yml `force:`); a spec of the
# sheets (a table / report / ARD spec), its `study` sheet's key `rounding`
# (the sheets' place for it, which the converter writes to `force:`).
.study_rounding <- function(study) {
  if (is.list(study) && is.data.frame(study$study)) {
    st <- study$study
    v <- if (nrow(st) && all(c("key", "value") %in% names(st))) st$value[match("rounding", st$key)]
    return(if (length(v) && !is.na(v)) as.character(v) else NULL)
  }
  for (it in study$force$content %||% list()) {
    if (identical(.schema_fn_name(it$fn %||% ""), "plan_digits") && !is.null(it$args$rounding)) {
      return(as.character(it$args$rounding))
    }
  }
  NULL
}

#' The study's rounding, forced on every report
#'
#' The one public reader and writer of the study's rounding: `spec/defaults.yml`
#' `force:` (canon 1.33), which every report and ARD program of the study uses
#' (`options(rtfreporter.rounding = , cards.round_type = )` in their setup),
#' whatever a layer, a template or the report itself says.
#' [tfl_write_study_part()] writes it, in its one form:
#'
#' ```yaml
#' force:
#'   content:
#'     - plan_digits:
#'         rounding: sas
#' ```
#'
#' @param study A study read with [tfl_read_study_spec()].
#' @param value `"sas"` (half away from zero) or `"r"` (half to even) to set
#'   the rounding; `NULL` (or `NA`, `""`) to force none.  Missing: the
#'   study's rounding is returned.
#' @return With `value` missing: `"sas"`, `"r"`, or `NULL` when the study
#'   forces none.  Else the study, its `force` set (write it with
#'   [tfl_write_study_part()]).
#' @examples
#' d <- system.file("extdata", "spec-yaml", package = "tflspec")
#' s <- tfl_read_study_spec(d)
#' tfl_study_rounding(s)
#' s <- tfl_study_rounding(s, "r")
#' tfl_study_rounding(s)
#' @export
tfl_study_rounding <- function(study, value) {
  if (!inherits(study, "tfl_study_spec")) {
    .ard_stop("tfl_study_rounding(): `study` is a study read with tfl_read_study_spec().")
  }
  if (missing(value)) return(.study_rounding(study))
  study$force <- .force_of(value)
  study
}

# ---- the writers: one form ---------------------------------------------------

# The force of `rounding` as the study holds it (what .force_read() gives);
# list() for NULL, NA or ""
.force_of <- function(rounding) {
  if (is.null(rounding) || length(rounding) != 1L || is.na(rounding) || !nzchar(rounding)) return(list())
  r <- .force_check(tolower(as.character(rounding)), "the study's rounding")
  list(content = list(tfl_item("plan_digits", list(rounding = r), "force")))
}

# A study's force as defaults.yml's `force:` is written (the YAML list); NULL
# when it forces nothing
.force_out <- function(force) {
  if (!length(force$content)) return(NULL)
  list(content = .items_to_list(force$content))
}

# defaults.yml's `force:` block as text lines, for the writers that write
# the file as text (character() when no rounding)
.force_lines <- function(rounding) {
  f <- .force_of(rounding)
  if (!length(f)) return(character())
  c("# the study's rounding, forced on every report (sas: half away from zero; r: half to even)",
    .force_yaml(f$content[[1L]]$args$rounding))
}

# defaults.yml's `force:` of `rounding`, its YAML lines: the one form, the
# block form tfl_write_study_part() writes
.force_yaml <- function(rounding) {
  c("force:", "  content:", "    - plan_digits:", sprintf("        rounding: %s", rounding))
}

# ---- the old forms of the rounding -------------------------------------------

# A study's content layer (spec/defaults.yml `content:`, a style set's
# `content:`): a plan_digits(rounding = ) card there is an old form (canon
# 11-10 :369, :378) and stops.  `from`: the layer ("defaults", "style:<name>").
.layer_rounding_gone <- function(items, from) {
  if (inherits(items, "tfl_r")) return(invisible())
  for (it in items) {
    if (inherits(it, "tfl_item") && identical(.schema_fn_name(it$fn %||% ""), "plan_digits") &&
        !is.null(it$args$rounding)) {
      r <- it$args$rounding
      r <- if (is.character(r) && length(r) == 1L && r %in% c("sas", "r")) r else "sas"
      where <- if (identical(from, "defaults")) "spec/defaults.yml `content:`" else
        sprintf("spec/defaults.yml `styles: %s: content:`", sub("^style:", "", from))
      .old_form_stop("content_rounding", where,
                     instead = .force_instead(r, "and take `rounding` out of the card"),
                     more = .force_yaml(r))
    }
  }
  invisible()
}

# What to write instead of an old rounding: defaults.yml's `force:` (the
# stop gives its YAML, the block form, .force_yaml(), below the line)
.force_instead <- function(rounding = "sas", more = NULL) {
  paste0("the study's rounding is forced (canon 1.33): write it in spec/defaults.yml `force:`",
         if (!is.null(more)) paste0(", ", more), " (", rounding,
         "; tfl_study_rounding() sets it)")
}

# ---- layer 5: over a report's own rounding -----------------------------------

# A table's content resolved, with the study's force over it (canon 11-10
# :365-:367): a plan_digits(rounding = ) that is not the forced value, or an
# `!unset` of it in the report's own items (`own`), is warned of (a
# `tflspec_force_over` warning: `id`, `item`, `value`, `forced`, `text`) and
# takes the forced value.  The same value: nothing to say.  A report of a
# kind with no table content (listing, figure) is left as it is, in silence;
# so is a study that forces nothing.  The forced rounding reaches the
# reports that say none through the setup's options().
.force_over <- function(items, study, output, own = NULL) {
  forced <- .study_rounding(study)
  if (is.null(forced) || inherits(items, "tfl_r") || !identical(output$kind %||% "table", "table")) {
    return(items)
  }
  say <- function(value) {
    text <- sprintf(paste0("plan_digits(rounding = %s) is not used: the study forces the rounding ",
                           "\"%s\" (spec/defaults.yml `force:`, canon 1.33)"), value, forced)
    warning(structure(class = c("tflspec_force_over", "warning", "condition"),
                      list(message = paste0(output$id %||% "the report", ": ", text), call = NULL,
                           id = output$id, item = "plan_digits$rounding", value = value,
                           forced = forced, text = text)))
  }
  # (an `!unset` of the rounding is gone from the resolved items: the
  # report's own say it)
  if (!inherits(own, "tfl_r")) {
    for (it in own %||% list()) {
      if (inherits(it, "tfl_item") && identical(.schema_fn_name(it$fn %||% ""), "plan_digits") &&
          inherits(it$args$rounding, "tfl_unset")) say("!unset")
    }
  }
  lapply(items, function(it) {
    if (!inherits(it, "tfl_item") || !identical(.schema_fn_name(it$fn %||% ""), "plan_digits") ||
        is.null(it$args$rounding)) return(it)
    r <- it$args$rounding
    if (is.character(r) && length(r) == 1L && identical(r, forced)) return(it)
    say(if (is.character(r) && length(r) == 1L) sprintf("\"%s\"", r) else "<R code>")
    it$args$rounding <- forced
    if (!is.null(it$arg_from)) it$arg_from[["rounding"]] <- "force"
    it
  })
}
