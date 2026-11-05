# ============================================================================
#  The AI's reports always say their column widths (10/26, the user)
# ----------------------------------------------------------------------------
#  A report with no widths prints rtfreporter's equal columns -- a stub as
#  narrow as a number -- and a person who fixes it has to add an argument
#  rather than change a value.  So a report of the AI's always has them: a
#  table's `plan_columns(widths = )`, a listing's `listing_col(width = )`.
#  The AI is asked to write them (the shell's proportions); what it leaves
#  out, R writes: a table's from its printed table once it is built
#  (tflspec's own rule, .tfl_default_widths(): each value column its share
#  of the page, the row headings the rest, in three tiers), one
#  per printed column by name, a listing's from its columns' variables and
#  headers when the answer is read -- each with the assumption that its
#  widths are the default ones, set for now.
# ============================================================================

# Whether a report (resolved: the defaults' items too) says its table's
# widths
.ai_has_widths <- function(o) {
  if (inherits(o$content, "tfl_r")) return(TRUE)
  any(vapply(o$content %||% list(), function(it) {
    identical(it$fn, "plan_columns") && length(it$args$widths) > 0L
  }, NA))
}

# A table's widths from its built table: tflspec's rule
# (.tfl_default_widths()) over its first page, the stub columns (those that
# are not values) the row headings -- one width a printed column, by name.
# (A table whose page cannot be read: each stub column 3 to a value
# column's 1, a hierarchy's 4, several stub columns 2 each.)  The report
# (`own`, as its file has it) with them as a plan_columns item, after its
# table_plan (an item there without widths gets them); NULL when it says
# them already or the plan's columns cannot be read.
.ai_table_widths <- function(own, resolved, plan) {
  if (.ai_has_widths(resolved) || inherits(own$content, "tfl_r")) return(NULL)
  cols <- tryCatch(rtfreporter::plan_layers(plan)$columns, error = function(e) NULL)
  if (is.null(cols) || !length(cols$names)) return(NULL)
  stub <- setdiff(cols$names, cols$spread)
  if (!length(stub)) return(NULL)
  fns <- vapply(resolved$content %||% list(), `[[`, "", "fn")
  hier <- any(vapply(resolved$content[fns == "normalize_ard"], function(it) {
    length(it$args$hierarchy) > 0L
  }, NA))
  body <- tryCatch({
    pg <- suppressMessages(suppressWarnings(rtfreporter::plan_apply(plan, "pages")))
    t1 <- if (inherits(pg, "rtftable")) pg else pg[[1L]]
    t1$data
  }, error = function(e) NULL)
  if (is.data.frame(body) && ncol(body)) {
    rt <- which(!names(body) %in% cols$spread)
    if (length(rt)) {
      w <- .tfl_default_widths(body, rt)
      w <- stats::setNames(as.list(as.vector(w)), names(w))
      # value columns past the first page (pages split across, pk_conc's
      # split_at): every value column the first page's width, as `.values`
      if (length(setdiff(cols$spread, names(body))) && length(rt) < length(w)) {
        w <- c(w[rt], list(.values = w[[setdiff(seq_along(w), rt)[1L]]]))
      }
      return(.ai_put_widths(own, w))
    }
  }
  w <- stats::setNames(as.list(rep(if (length(stub) > 1L) 2L else if (hier) 4L else 3L,
                                   length(stub))), stub)
  if (length(cols$spread)) w[[".values"]] <- 1L
  .ai_put_widths(own, w)
}

# What the row headings keep at least, in characters: their content (the
# longest cell + 2), but no more than this -- a short stub stops at its
# content, a long one (an AE term, a race) keeps this much (#541)
.tfl_widths_stub_keep <- 30L

# tflspec's default column widths (#541, the user's rule of 11/2), in
# characters of a `page` of 120, one a column of the printed table `body`
# by name; `stub` the row-heading columns (positions), `n` value columns.
# The first of three that leaves the row headings what they keep
# (.tfl_widths_stub_keep, capped at their content):
# 1. each value column 60 / (n + 1) % of the page, at least 10 characters
#    + 2 (a cell on one line); the row headings the rest -- one value
#    column 70:30, two 60:20:20, three 55:15:15:15;
# 2. each value column 6 + 2 characters (a cell on two lines), the row
#    headings the rest -- kept, too, when that is less than they keep but
#    still two value columns (16) or more;
# 3. every column equal, the row headings two: 2 / (n + 2) of the page.
# Several row-heading columns share theirs by their content.  The tier
# used is the result's attribute "tier".
.tfl_default_widths <- function(body, stub, page = 120L) {
  txt <- function(v) {
    v <- as.character(v)
    v[is.na(v)] <- ""
    nchar(gsub("\u00a0", " ", v, fixed = TRUE), type = "width")
  }
  vals <- setdiff(seq_along(body), stub)
  n <- length(vals)
  own <- pmax(1L, vapply(stub, function(j) max(c(0L, txt(body[[j]]))) + 2L, 0L))
  keep <- min(sum(own), .tfl_widths_stub_keep)
  tiers <- list(max(as.integer(floor(page * 0.6 / (n + 1L))), 12L), 8L)
  tier <- 3L
  v <- as.integer(floor(page / (n + 2L)))
  for (k in seq_along(tiers)) {
    left <- page - n * tiers[[k]]
    if (left >= keep || (k == 2L && left >= 2L * tiers[[k]])) {
      tier <- k
      v <- tiers[[k]]
      break
    }
  }
  rest <- if (tier == 3L) 2L * v else page - n * v
  sw <- floor(rest * own / sum(own))
  sw[1L] <- rest - sum(sw[-1L])
  w <- integer(ncol(body))
  w[stub] <- as.integer(sw)
  w[vals] <- v
  structure(stats::setNames(w, names(body)), tier = tier)
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
# key -- not built again for widths it had.  Each entry holds the widths and
# a fingerprint of the draft they were made for (.ai_widths_print()): they
# are put back only while the draft is that one, else made again from its
# build.
.ai_widths_file <- function(dir) file.path(dir, "ai", "widths.yml")

.ai_widths_read <- function(dir) {
  f <- .ai_widths_file(dir)
  if (!file.exists(f)) return(list())
  tryCatch(yaml::read_yaml(f), error = function(e) list()) %||% list()
}

.ai_widths_remember <- function(dir, id, w, print = NULL) {
  x <- .ai_widths_read(dir)
  x[[id]] <- c(if (!is.null(print)) list(draft = print), list(widths = w))
  .ai_write_text(yaml::as.yaml(x), .ai_widths_file(dir))
}

# A fingerprint of a report as the widths are made from it: resolved over
# its layers (a layer's code list counts), the widths of its plan_columns
# left out (so it is the same before and after they are written), as the
# text its file would hold.  NULL when it cannot be resolved.
.ai_widths_print <- function(o, st) {
  tryCatch({
    ro <- tfl_resolve_output(o, st)
    items <- ro$content
    if (is.list(items) && !inherits(items, "tfl_r")) {
      items <- lapply(items, function(it) {
        if (identical(it$fn, "plan_columns")) it$args$widths <- NULL
        it$from <- NULL
        it
      })
      keep <- vapply(items, function(it) !(identical(it$fn, "plan_columns") && !length(it$args)), NA)
      ro$content <- items[keep]
    }
    y <- .output_to_list(ro)
    # (the draft's own notes are not what the columns are made from)
    y$ai <- NULL
    txt <- yaml::as.yaml(y, handlers = .yaml_out_handlers())
    rlang::hash(yaml::as.yaml(yaml::yaml.load(txt)))
  }, error = function(e) NULL)
}

# A table of the answer with no widths of its own: those R gave it before,
# while its draft is the one they were made for (`st`: the study)
.ai_widths_again <- function(o, known, st) {
  e <- known[[o$id]]
  if (!identical(o$kind, "table") || is.null(e) || .ai_has_widths(o) ||
      inherits(o$content, "tfl_r")) return(o)
  if (!is.list(e) || !is.character(e$draft) || !is.list(e$widths) || is.null(st)) return(o)
  now <- .ai_widths_print(o, st)
  if (is.null(now) || !identical(now, e$draft)) return(o)
  .ai_put_widths(o, e$widths)
}

# The tables of an answer just written with no widths: each built (its
# ARD, its plan) from the study folder, its widths written into its file
# and remembered; a table that cannot be built is left to run.R
.ai_widths_on_read <- function(outs, paths, dir, remember = TRUE) {
  todo <- which(vapply(outs, function(o) {
    identical(o$kind, "table") && !inherits(o$content, "tfl_r") && !.ai_has_widths(o)
  }, NA))
  set <- character()
  if (!length(todo)) return(invisible(set))
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
    if (isTRUE(remember)) .ai_widths_remember(".", o$id, pc[[1L]]$args$widths, .ai_widths_print(o, st))
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
.ai_widths_assumptions <- function(ids, language = "en") {
  if (!length(ids)) return(NULL)
  ja <- .ai_is_ja(language)
  data.frame(output_id = ids, where = "content", item = "widths",
             assumption = if (ja) {
               paste0("\u5217\u5e45: \u65e2\u5b9a\u306e\u6bd4\u7387\u3092\u4eee\u306b\u8a2d\u5b9a\u3057\u307e\u3057\u305f",
                      "\uff08\u30b7\u30a7\u30eb\u306e\u5217\u5e45\u3092\u8aad\u307f\u53d6\u308c\u306a\u304b\u3063\u305f\u305f\u3081\u3002",
                      "tflspec \u306e\u898f\u5247: \u5024\u306e\u5217\u306f\u305d\u308c\u305e\u308c\u30da\u30fc\u30b8\u306e\u4e00\u5b9a\u306e\u5272\u5408\u3001",
                      "\u884c\u898b\u51fa\u3057\u306f\u6b8b\u308a\uff09")
             } else {
               paste("Column widths: the default proportions, set for now because",
                     "the shell's widths were not read (tflspec's rule: each value column",
                     "its share of the page, the row headings the rest)")
             },
             value = "default widths",
             question = if (ja) "\u5217\u5e45\u306f\u30b7\u30a7\u30eb\u306e\u3068\u304a\u308a\u3067\u3059\u304b\uff1f"
                        else "Are the column widths as the shell wants?",
             stringsAsFactors = FALSE)
}

# Is the chat's language (study.yml's `ai: language`, "ja" or its name)
# Japanese?  The texts the tool adds to a report's assumptions follow it
.ai_is_ja <- function(language) {
  x <- tolower(as.character(language %||% "en")[1L])
  !is.na(x) && x %in% c("ja", "jp", "japanese", "ja-jp", "\u65e5\u672c\u8a9e")
}
