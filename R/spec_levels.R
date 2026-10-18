# plan_levels() where the ARD already says it (the owner, 2026-10-18): a
# report's code lists are applied when its ARD is made (set_levels() in the
# ARD program: each listed column a factor in its list's order, its values
# the list's labels), and cards keeps a factor's order in the ARD, which
# normalize_ard() keeps in the table.  So a plan_levels() entry that says
# a code list again, or the variables in the ARD's own order, says nothing
# the table does not already have: the program leaves it out, and so does
# the run (the two read one list, .content_table_pipeline()).
#
# Kept: an entry that differs from the code list (a report's own order), a
# key no analysis of the report reads (a column the plan makes), a key an
# analysis that is not cards' reads (cardx's survfit names the arms from
# the strata, in their alphabetical order), and everything of a report whose
# data steps make its data or whose ARD the report does not define.

# Does an analysis's ARD keep its data's factor order?  cards' functions do;
# a cardx one, a study's own or code is not known to.
.ard_keeps_order <- function(a) {
  m <- a$method %||% ""
  keys <- .ard_methods_builtin()
  k <- .method_key(m, keys)
  call <- if (is.na(k)) m else keys$call[k]
  startsWith(call, "cards::") || identical(call, "(subjects)")
}

# A code list's values as the ARD has them (its labels, else its values);
# a list of the values themselves as it is
.cl_values <- function(cl) {
  if (is.character(cl)) return(unname(cl))
  vapply(cl, function(r) as.character(r$label %||% r$value), "")
}

.levels_prune <- function(content, output) {
  ard <- output$ard
  if (inherits(content, "tfl_r") || !length(ard$analyses) || length(.content_r_steps(output))) {
    return(content)
  }
  an <- ard$analyses
  ids <- vapply(an, function(a) a$id %||% NA_character_, "")
  reads <- function(a) unique(c(a$by, a$strata, a$variables))
  keeps <- vapply(an, .ard_keeps_order, NA)
  # the rows' variables in the ARD's order: those of the analyses by a
  # group (a parent's group too: ard_stack()'s children), not the column
  # key's own count
  grouped <- vapply(an, function(a) {
    p <- a$parent
    length(a$by) > 0L || (!is.null(p) && length(an[[match(p, ids)]]$by %||% NULL) > 0L)
  }, NA)
  ard_vars <- unique(unlist(lapply(an[grouped], `[[`, "variables")))
  out <- list()
  for (it in content) {
    if (!identical(.schema_fn_name(it$fn), "plan_levels")) {
      out <- c(out, list(it))
      next
    }
    a <- it$args
    named_order <- function(x) {
      nm <- names(unlist(x))
      !is.null(nm) && any(!is.na(nm) & nzchar(nm))
    }
    for (k in setdiff(names(a), c("variable", ".drop_empty"))) {
      # (a named order gives its values' text: a change the ARD does not say)
      if (named_order(a[[k]])) next
      by_k <- vapply(an, function(x) k %in% reads(x), NA)
      cl <- output$codelists[[k]]
      if (any(by_k) && all(keeps[by_k]) && !is.null(cl) && !inherits(a[[k]], "tfl_r") &&
          identical(unname(as.character(unlist(a[[k]]))), .cl_values(cl))) {
        a[[k]] <- NULL
      }
    }
    v <- a$variable
    if (!is.null(v) && all(keeps) && !named_order(v)) {
      v <- as.character(unlist(v))
      if (length(v) <= length(ard_vars) && identical(v, ard_vars[seq_along(v)])) a$variable <- NULL
    }
    if (!length(a)) next
    it$args <- a
    out <- c(out, list(it))
  }
  out
}
