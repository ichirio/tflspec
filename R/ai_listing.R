# ============================================================================
#  AI-assisted drafting: a listing, in the table task
# ----------------------------------------------------------------------------
#  A TOC row of a listing is drafted with the tables (its family `listing`,
#  its answer a report of `kind: listing`): its records (`data: {source:
#  dataset, dataset, population, where, sort}`, or one of the report's
#  analysis data) and one plan_listing item, its columns listing_col()s.
#  What defines it is checked as a table's is: the analysis set, and a
#  dataset of parameters kept to the shell's (or PARAM shown) -- without
#  them it is no report, an error (10/24).
# ============================================================================

# The functions a listing's content may call
.ai_listing_functions <- c("plan_listing", "plan_paginate_rows", "plan_blanks")

.ai_check_listing_output <- function(o, at, context, vars) {
  p <- .ai_problems()
  add <- function(where, what, severity = "error") {
    p <<- .ai_problem(p, paste(at, where), what, severity)
  }
  for (e in o[["extends"]]) {
    ok <- e %in% c("defaults", "none") ||
      (startsWith(e, "patterns/") && (is.null(context$patterns) ||
                                        substring(e, 10L) %in% context$patterns))
    if (!ok) add("extends", sprintf("%s is not defaults or a pattern of the study", sQuote(e, FALSE)))
  }
  pops <- context$populations
  chk_pop <- function(where, x) {
    if (length(pops) && !is.null(x) && !x %in% pops) {
      add(where, sprintf("%s is not an analysis set of spec/data.yml (%s)", sQuote(x, FALSE),
                         paste(pops, collapse = ", ")))
    }
  }
  if (!is.na(o[["population"]])) chk_pop("population", o[["population"]])
  if (is.na(o[["label"]])) add("label", "the report has no label (its title)", "warning")
  if (length(o[["ard"]]$analyses %||% list())) {
    add("ard analyses", "a listing has no analyses: its records are its `data`")
  }
  known_ds <- if (!is.null(vars)) unique(vars$dataset)
  bds <- if (!is.null(vars)) unique(vars$dataset[vars$variable %in% "PARAMCD"]) else character()
  par_rx <- "\\bPARAM(CD|N)?\\b"
  says_par <- function(x) any(grepl(par_rx, unlist(lapply(x, as.character))))
  d <- o[["data"]]
  if (!is.list(d) || !length(d)) {
    add("data", paste("says no records: `{source: dataset, dataset: <ADaM>, population: <analysis set>,",
                      "where: ..., sort: [...]}`"))
    d <- list()
  } else if (!identical(.ai_scalar_text(d[["source"]]), "dataset")) {
    add("data source", "is `dataset` for a listing (its records, read from a dataset or an analysis data)")
  }
  data_id <- .ai_scalar_text(d[["data"]])
  ds <- toupper(.ai_scalar_text(d[["dataset"]]) %||% "")
  pop <- .ai_scalar_text(d[["population"]])
  chk_pop("data population", pop)
  have <- character()
  has_pop <- !is.null(pop)
  kept <- says_par(d[["where"]])
  ad <- o[["ard"]]$analysis_data %||% list()
  ad_ids <- vapply(ad, function(a) .ai_scalar_text(a[["id"]]) %||% NA_character_, "")
  if (!is.null(data_id)) {
    if (nzchar(ds) || has_pop) {
      add("data", paste("reads its `data` (an analysis data) or a `dataset` and `population`; not both",
                        "(the analysis data has them)"))
    }
    if (!data_id %in% ad_ids) {
      add("data data", sprintf("%s is not an analysis_data id of this report%s", sQuote(data_id, FALSE),
                               if (length(ad_ids)) sprintf(" (%s)", paste(ad_ids, collapse = ", ")) else ""))
      ds <- ""
    } else {
      # the data it is made from, to its dataset: its population, its
      # parameters, the columns it adds or derives
      id <- data_id
      seen <- character()
      coded <- FALSE
      while (!is.na(id) && id %in% ad_ids && !id %in% seen) {
        seen <- c(seen, id)
        a <- ad[[match(id, ad_ids)]]
        coded <- coded || !is.null(a[["code"]])
        chk_pop(sprintf("ard.analysis_data %s population", id), .ai_scalar_text(a[["population"]]))
        has_pop <- has_pop || !is.null(a[["population"]]) || !is.null(a[["subjects"]])
        kept <- kept || says_par(a[["where"]]) || says_par(a[["code"]])
        have <- c(have, .ai_derived_names(a[["derive"]]), unlist(a[["add"]]))
        id <- .ai_scalar_text(a[["from"]]) %||% NA_character_
      }
      # (a step of R code makes columns the check cannot know, as a
      # table's: its columns are not checked against the dataset's)
      ds <- toupper(if (!is.na(id) && !id %in% ad_ids && !coded) id else "")
    }
  } else if (!nzchar(ds)) {
    add("data dataset", "is missing: the dataset the listing's records are read from")
  }
  if (!is.null(known_ds) && nzchar(ds) && !ds %in% known_ds) {
    add(if (is.null(data_id)) "data dataset" else "ard.analysis_data from",
        sprintf("%s is not among the datasets given (%s)", ds, paste(known_ds, collapse = ", ")))
    ds <- ""
  }
  if (!has_pop) {
    add("data population", paste(
      "the listing names no analysis set (`data.population`, or its analysis data's):",
      "write the one the shell says; if the shell does not say it, ask it in a question --",
      "do not guess"))
  }
  # the content: one plan_listing, its columns
  if (inherits(o[["content"]], "tfl_r")) {
    add("content", "is R code; write the items (plan_listing and its listing_col columns)", "warning")
    return(p)
  }
  fns <- vapply(o[["content"]], `[[`, "", "fn")
  cols <- list()
  if (sum(fns == "plan_listing") != 1L) {
    add("content", "has no plan_listing item (one: the listing's columns)")
  }
  for (j in seq_along(o[["content"]])) {
    it <- o[["content"]][[j]]
    w <- sprintf("content[%d] %s", j, it$fn)
    if (!it$fn %in% .ai_listing_functions) {
      add(w, sprintf("%s is not a function of a listing's content; they are %s", it$fn,
                     paste(.ai_listing_functions, collapse = ", ")))
      next
    }
    chk <- tfl_schema_check(it)
    for (r in seq_len(nrow(chk))) {
      sev <- if (identical(chk$level[r], "error") || grepl("has no argument", chk$message[r], fixed = TRUE))
        "error" else "warning"
      add(paste(w, if (nzchar(chk$arg[r])) paste("field", chk$arg[r]) else ""), chk$message[r], sev)
    }
    if (identical(it$fn, "plan_listing")) {
      cols <- it$args$cols %||% list()
      if (!length(cols)) add(w, "has no `cols`: the shell's columns, a listing_col each")
      for (k in seq_along(cols)) {
        cl <- cols[[k]]
        if (!inherits(cl, "tfl_call") || !identical(cl$fn, "listing_col")) {
          add(sprintf("%s cols[%d]", w, k), "is not a listing_col: `{fn: listing_col, args: {vars: [...], label: ...}}`")
        } else if (!length(unlist(cl$args$vars))) {
          add(sprintf("%s cols[%d]", w, k), "has no `vars`: the variables the column shows")
        }
      }
    }
  }
  shown <- unique(unlist(lapply(cols, function(cl) if (inherits(cl, "tfl_call")) unlist(cl$args$vars))))
  # the columns, the order, the condition: the dataset's (or made on the way)
  if (!is.null(vars) && nzchar(ds)) {
    p <- rbind(p, .ai_check_vars(shown, vars, ds, have, paste(at, "content plan_listing cols")))
    p <- rbind(p, .ai_check_vars(sub("^-", "", unlist(d[["sort"]])), vars, ds, have,
                                 paste(at, "data sort")))
    p <- rbind(p, .ai_check_vars(.ai_r_names(d[["where"]]), vars, ds, have, paste(at, "data where"),
                                 "warning"))
  }
  # a dataset of parameters: kept to the shell's, or the parameter shown
  if (ds %in% bds && !kept && !any(grepl(par_rx, shown))) {
    add("data where", sprintf(paste(
      "reads %s, a dataset of parameters (PARAMCD), but keeps no parameter: write the shell's",
      "(where: PARAMCD == \"...\"), or show PARAM in a column for a listing of them all;",
      "if the shell does not say which, ask it in a question -- do not guess"), ds))
  }
  if (!length(o[["page"]]$titles)) add("page", "has no titles", "warning")
  p$where <- trimws(p$where)
  p
}
