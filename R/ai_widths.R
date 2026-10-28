# ============================================================================
#  The AI's reports always say their column widths (10/26, the user)
# ----------------------------------------------------------------------------
#  A report with no widths prints rtfreporter's equal columns -- a stub as
#  narrow as a number -- and a person who fixes it has to add an argument
#  rather than change a value.  So a report of the AI's always has them: a
#  table's `plan_columns(widths = )`, a listing's `listing_col(width = )`.
#  The AI is asked to write them (the shell's proportions); what it leaves
#  out, R writes: a table's from its printed table once it is built
#  (rtfreporter's default_col_widths(): the row headings by their content,
#  the value columns sharing the rest), one per printed column by name, a
#  listing's from its columns' variables and headers when the answer is
#  read -- each with the assumption that its widths are the default ones,
#  set for now.
# ============================================================================

# Whether a report (resolved: the defaults' items too) says its table's
# widths
.ai_has_widths <- function(o) {
  if (inherits(o$content, "tfl_r")) return(TRUE)
  any(vapply(o$content %||% list(), function(it) {
    identical(it$fn, "plan_columns") && length(it$args$widths) > 0L
  }, NA))
}

# A table's widths from its built table: rtfreporter's default_col_widths()
# over its first page, the stub columns (those that are not values) the row
# headings -- one width a printed column, by name.  (An rtfreporter before
# default_col_widths(): each stub column 3 to a value column's 1, a
# hierarchy's 4, several stub columns 2 each.)  The report (`own`, as its
# file has it) with them as a plan_columns item, after its table_plan (an
# item there without widths gets them); NULL when it says them already or
# the plan's columns cannot be read.
.ai_table_widths <- function(own, resolved, plan) {
  if (.ai_has_widths(resolved) || inherits(own$content, "tfl_r")) return(NULL)
  cols <- tryCatch(rtfreporter::plan_layers(plan)$columns, error = function(e) NULL)
  if (is.null(cols) || !length(cols$names)) return(NULL)
  stub <- setdiff(cols$names, cols$spread)
  if (!length(stub)) return(NULL)
  ns <- asNamespace("rtfreporter")
  if (exists("default_col_widths", envir = ns, inherits = FALSE)) {
    body <- tryCatch({
      pg <- suppressMessages(suppressWarnings(rtfreporter::plan_apply(plan, "pages")))
      t1 <- if (inherits(pg, "rtftable")) pg else pg[[1L]]
      t1$data
    }, error = function(e) NULL)
    if (is.data.frame(body) && ncol(body)) {
      rt <- which(!names(body) %in% cols$spread)
      if (length(rt)) {
        w <- get("default_col_widths", envir = ns)(body, row_title = rt)
        return(.ai_put_widths(own, as.list(w)))
      }
    }
  }
  fns <- vapply(resolved$content %||% list(), `[[`, "", "fn")
  hier <- any(vapply(resolved$content[fns == "normalize_ard"], function(it) {
    length(it$args$hierarchy) > 0L
  }, NA))
  w <- stats::setNames(as.list(rep(if (length(stub) > 1L) 2L else if (hier) 4L else 3L,
                                   length(stub))), stub)
  if (length(cols$spread)) w[[".values"]] <- 1L
  .ai_put_widths(own, w)
}

# The report with the widths `w` in its plan_columns item (after its
# table_plan; an item there without widths gets them)
.ai_put_widths <- function(own, w) {
  items <- own$content %||% list()
  at <- which(vapply(items, function(it) identical(it$fn, "plan_columns"), NA))
  if (length(at)) {
    items[[at[1L]]]$args$widths <- w
  } else {
    item <- tfl_item("plan_columns", list(widths = w))
    tp <- which(vapply(items, function(it) identical(it$fn, "table_plan"), NA))
    pos <- if (length(tp)) tp[1L] else length(items)
    items <- append(items, list(item), after = pos)
  }
  own$content <- items
  own
}

# A listing's columns given a width (characters) where they have none: the
# longest of its header's lines and of what its variables print -- an id
# 12, a date 10, a number 8, a text 20 (the ADaM's kind, else the name's
# ending) -- at most 40
.ai_listing_widths <- function(o, vars = NULL) {
  if (!identical(o$kind, "listing") || inherits(o$content, "tfl_r")) return(o)
  ds <- toupper(as.character(o$data[["dataset"]] %||% ""))
  kind_of <- function(v) {
    k <- if (!is.null(vars)) {
      hit <- vars$kind[vars$variable == v & (!nzchar(ds) | vars$dataset == ds)]
      if (length(hit)) hit[1L]
    }
    if (grepl("^(USUBJID|SUBJID|SITEID)$", v)) return(12L)
    if (identical(k, "date") || grepl("(DT|DTC|DTM)$", v)) return(10L)
    if (identical(k, "num") || grepl("(DY|N|NUM|SEQ|AVAL|BASE|CHG)$", v)) return(8L)
    20L
  }
  for (j in seq_along(o$content)) {
    it <- o$content[[j]]
    if (!identical(it$fn, "plan_listing")) next
    cols <- it$args$cols %||% list()
    for (k in seq_along(cols)) {
      cl <- cols[[k]]
      if (!inherits(cl, "tfl_call") || !is.null(cl$args$width)) next
      v <- as.character(unlist(cl$args$vars))
      lab <- as.character(cl$args$label %||% "")
      head_w <- max(nchar(strsplit(gsub("\\n", "\n", lab, fixed = TRUE), "\n", fixed = TRUE)[[1L]]),
                    0L)
      w <- max(c(head_w, vapply(v, kind_of, 0L)))
      cl$args$width <- as.integer(min(40L, max(4L, w)))
      cols[[k]] <- cl
    }
    o$content[[j]]$args$cols <- cols
  }
  o
}

# The widths R gave a study's tables (ai/widths.yml, by report): an answer
# read again (every run.R reads them all) keeps them, and the report its
# key -- not built again for widths it had
.ai_widths_file <- function(dir) file.path(dir, "ai", "widths.yml")

.ai_widths_read <- function(dir) {
  f <- .ai_widths_file(dir)
  if (!file.exists(f)) return(list())
  tryCatch(yaml::read_yaml(f), error = function(e) list()) %||% list()
}

.ai_widths_remember <- function(dir, id, w) {
  x <- .ai_widths_read(dir)
  x[[id]] <- w
  .ai_write_text(yaml::as.yaml(x), .ai_widths_file(dir))
}

# A table of the answer with no widths of its own: those R gave it before
.ai_widths_again <- function(o, known) {
  if (!identical(o$kind, "table") || is.null(known[[o$id]]) || .ai_has_widths(o) ||
      inherits(o$content, "tfl_r")) return(o)
  .ai_put_widths(o, known[[o$id]])
}

# The tables of an answer just written with no widths: each built (its
# ARD, its plan) from the study folder, its widths written into its file
# and remembered; a table that cannot be built is left to run.R
.ai_widths_on_read <- function(outs, paths, dir, remember = TRUE) {
  todo <- which(vapply(outs, function(o) {
    identical(o$kind, "table") && !inherits(o$content, "tfl_r") && !.ai_has_widths(o)
  }, NA))
  set <- character()
  if (!length(todo) || !requireNamespace("rtfreporter", quietly = TRUE)) return(invisible(set))
  paths <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  st <- tryCatch(tfl_read_study_spec("."), error = function(e) NULL)
  if (is.null(st)) return(invisible(set))
  for (i in todo) {
    o <- outs[[i]]
    w <- tryCatch({
      ro <- tfl_resolve_output(o, st)
      if (.ai_has_widths(ro)) NULL else {
        ard <- suppressMessages(suppressWarnings(
          tfl_build_ard(st, dir = ".", output_id = o$id, save = FALSE)))
        if (!is.null(ard$output_id)) ard <- ard[ard$output_id %in% o$id, , drop = FALSE]
        plan <- suppressMessages(suppressWarnings(tfl_content_run(ro, ard)))
        .ai_table_widths(o, ro, plan)
      }
    }, error = function(e) NULL)
    if (is.null(w)) next
    tfl_write_output(w, paths[i])
    pc <- Filter(function(it) identical(it$fn, "plan_columns"), w$content)
    if (isTRUE(remember)) .ai_widths_remember(".", o$id, pc[[1L]]$args$widths)
    set <- c(set, o$id)
  }
  invisible(set)
}

# The reports of an answer whose widths R sets (a table with none, a
# listing with a column with none): their ids
.ai_widths_unset <- function(outs) {
  names(outs)[vapply(outs, function(o) {
    if (inherits(o$content, "tfl_r")) return(FALSE)
    if (identical(o$kind, "table")) return(!.ai_has_widths(o))
    if (!identical(o$kind, "listing")) return(FALSE)
    any(vapply(o$content %||% list(), function(it) {
      identical(it$fn, "plan_listing") && any(vapply(it$args$cols %||% list(), function(cl) {
        inherits(cl, "tfl_call") && is.null(cl$args$width)
      }, NA))
    }, NA))
  }, NA)]
}

# The assumption a report whose widths R set carries: they are the default
# ones, for now (none when the answer gave them, read from the shell)
.ai_widths_assumptions <- function(ids) {
  if (!length(ids)) return(NULL)
  data.frame(output_id = ids, where = "content", item = "widths",
             assumption = paste("Column widths: the default proportions, set for now because",
                                "the shell's widths were not read (rtfreporter::default_col_widths():",
                                "the row headings by their content, the value columns sharing the rest)"),
             value = "default_col_widths()",
             question = "Are the column widths as the shell wants?",
             stringsAsFactors = FALSE)
}
