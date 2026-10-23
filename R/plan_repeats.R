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

# the verbs whose second call is a layer of its own (not merged)
.plan_repeat_kept <- c("plan_filter", "plan_compare", "plan_stat_groups", "plan_cell_style",
                       "plan_after", "plan_titles", "plan_footnotes")
# the verbs whose arguments add up (their values put together)
.plan_repeat_add <- c("plan_hide")

.plan_merge_repeats <- function(items, id = NA_character_) {
  if (!is.list(items) || inherits(items, "tfl_r") || length(items) < 2L) return(items)
  fns <- vapply(items, function(it) if (inherits(it, "tfl_item")) it$fn else "", "")
  rep <- unique(fns[duplicated(fns) & nzchar(fns) & !fns %in% .plan_repeat_kept &
                      grepl("^(plan_|table_plan$)", fns)])
  if (!length(rep)) return(items)
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
        for (k in names(a)) first$args[k] <- list(a[[k]])
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
