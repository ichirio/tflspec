# ============================================================================
#  The study's force (canon 1.33; PRINCIPLES.md 11-10 :363-:369, :378, 11-11
#  :400): what every report of the study uses, whatever a layer, a template
#  or the report itself says.  Only the rounding is forced.
#
#    spec/defaults.yml
#      force:
#        content:
#          - plan_digits: {rounding: sas}      # or r
#
#  `force:` is a map by step; its content part has the form of `content:`.
#  Any other step or card stops ("only the rounding is forced").  The one
#  reader of the study's rounding is .study_rounding(); the writers write it
#  through .force_of() / .force_out() (YAML lists) or .force_lines() (text).
#  The rounding goes to every program's setup, the reports' and the ARDs'
#  (options(rtfreporter.rounding = , cards.round_type = )), to an in-process
#  build's options (.study_rounding_option()), and over a table's own
#  plan_digits(rounding = ) when it is resolved (.force_over()).  A study
#  that forces no rounding writes no options().
# ============================================================================

# What the force may hold, said where anything else is written
.force_only <- paste0("only the rounding is forced (canon 1.33): ",
                      "force: {content: [{plan_digits: {rounding: sas}}]} (sas or r)")

# defaults.yml's `force:` (`x`, as YAML read it) as the study holds it:
# list(content = <the one plan_digits(rounding = ) item>), list() for none.
# `where`: the file, for the messages.
.force_read <- function(x, where = "spec/defaults.yml") {
  if (is.null(x)) return(list())
  stop_only <- function(what) {
    stop(sprintf("%s: `force:` %s; %s.", where, what, .force_only), call. = FALSE)
  }
  if (!.is_map(x)) stop_only("is a map by step")
  for (k in setdiff(names(x), "content")) stop_only(sprintf("has `%s:`", k))
  cont <- x$content
  if (!is.list(cont) || !length(cont) || !is.null(names(cont)) || inherits(cont, "tfl_r")) {
    stop_only("`content:` is a list of cards, as `content:` is")
  }
  items <- lapply(cont, .item_from_list)
  for (it in items) {
    if (!identical(.schema_fn_name(it$fn %||% ""), "plan_digits") ||
        !identical(names(it$args), "rounding")) {
      stop_only(sprintf("has `%s: {%s}`", it$fn %||% "?", paste(names(it$args), collapse = ", ")))
    }
  }
  if (length(items) > 1L) stop_only("has more than one card")
  it <- items[[1L]]
  it$args$rounding <- .force_check(it$args$rounding, paste0(where, ": force: plan_digits rounding"))
  it$from <- "force"
  list(content = list(it))
}

# The forced rounding is "sas" or "r" (canon 11-10 :368), spelled so
.force_check <- function(x, where) {
  if (!(is.character(x) && length(x) == 1L && !is.na(x) && x %in% c("sas", "r"))) {
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

# defaults.yml's `force:` of `rounding`, its YAML lines
.force_yaml <- function(rounding) {
  c("force:", "  content:", sprintf("    - plan_digits: {rounding: %s}", rounding))
}

# ---- the old forms of the rounding -------------------------------------------

# The `force:` that says `rounding`, as text for an old form's message
.force_text <- function(rounding = "sas") {
  sprintf("force: {content: [{plan_digits: {rounding: %s}}]}", rounding)
}

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

# What to write instead of an old rounding: defaults.yml's `force:`, its
# YAML on one line (the stop gives the block form, .force_yaml(), below)
.force_instead <- function(rounding = "sas", more = NULL) {
  paste0("the study's rounding is forced (canon 1.33): write it in spec/defaults.yml `force:`",
         if (!is.null(more)) paste0(", ", more), " -- ", .force_text(rounding))
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
