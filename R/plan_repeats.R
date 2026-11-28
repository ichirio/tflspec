# ---- a table's verb called twice: one call -------------------------------------
#
# A table needs each plan verb once.  A content read with a verb twice is
# put together as rtfreporter puts the two layers together (.plan_merge():
# a later layer wins argument by argument, plan_labels / plan_levels /
# plan_cells ... key by key -- their keys are the call's arguments), so
# the program and the sheets say one call; a message says it was done.
# plan_hide()'s columns add up.  Verbs whose calls add up as layers of
# their own (each filter, comparison, cell style, step after; titles or
# footnotes of other pages) are left as they are.  A figure's content is
# not touched: there a function called many times is the rule.
#
# A code SPEC file's own cards are not put together: a verb that does not
# stack (not one of .stack_verbs, R/spec_study.R, nor kept or added up
# here) written in two cards of one file stops, naming the verb and the
# file (PRINCIPLES 11-10, 1.13 / 1.32).  What is put together is a template
# report's: its template's items and the cards appended after them
# (tfl_template_resolve(), no file), where a later card overrides by
# argument (11-09, 1.4).

# the verbs whose second call is a layer of its own (not merged)
.plan_repeat_kept <- c("plan_filter", "plan_compare", "plan_stat_groups", "plan_cell_style",
                       "plan_after", "plan_titles", "plan_footnotes")
# the verbs whose arguments add up (their values put together)
.plan_repeat_add <- c("plan_hide")

# `file`: the code SPEC file the items are the own cards of (NULL: a
# template report's items, put together)
.plan_merge_repeats <- function(items, id = NA_character_, file = NULL) {
  if (!is.list(items) || inherits(items, "tfl_r") || length(items) < 2L) return(items)
  fns <- vapply(items, function(it) if (inherits(it, "tfl_item")) it$fn else "", "")
  rep <- unique(fns[duplicated(fns) & nzchar(fns) & !fns %in% .plan_repeat_kept &
                      grepl("^(plan_|table_plan$)", fns)])
  if (!length(rep)) return(items)
  bad <- setdiff(rep, c(.plan_repeat_add, .stack_verbs))
  if (!is.null(file) && length(bad)) {
    stop(sprintf(paste0("tfl_read_output(): %s (%s): %s in more than one card; a code SPEC ",
                        "writes a verb that does not stack once -- put its arguments in one card."),
                 if (is.na(id)) "content" else id, file, paste0(bad, "()", collapse = ", ")),
         call. = FALSE)
  }
  drop <- integer()
  for (fn in rep) {
    at <- which(fns == fn)
    first <- items[[at[1L]]]
    for (j in at[-1L]) {
      a <- items[[j]]$args
      if (fn %in% .plan_repeat_add) {
        # (the same argument: its values together, each once)
        for (k in names(a)) {
          v <- unique(c(unlist(first$args[[k]]), unlist(a[[k]])))
          first$args[[k]] <- if (is.list(a[[k]]) || is.list(first$args[[k]])) as.list(v) else v
        }
      } else {
        # (a later card's `~` is not written: the earlier's value stays, L463)
        for (k in names(a)) if (!is.null(a[[k]])) first$args[k] <- list(a[[k]])
      }
    }
    items[[at[1L]]] <- first
    drop <- c(drop, at[-1L])
  }
  message(sprintf("%s: %s called more than once; put together in one call, a later one's arguments winning%s.",
                  if (is.na(id)) "content" else id, paste0(rep, "()", collapse = ", "),
                  if (any(rep %in% .plan_repeat_add)) " (plan_hide()'s columns added up)" else ""))
  items[-drop]
}
