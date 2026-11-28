# ============================================================================
#  One report, several objects on its pages (canon 11-28, tflspec#1025)
# ----------------------------------------------------------------------------
#  One report is one template.  Its content makes named objects -- object
#  cards (`- {name, from, calls}`), or its own code (`content: !r`) that
#  leaves a named list -- and its `page: layout:` places them, by name, in
#  order, each row with rtfreporter's rtf_tables() items:
#
#    content:
#      - {name: demog, from: ard, calls: [{table_plan: {...}}, ...]}
#      - {name: severe, from: adae, calls: [!r 'dplyr::filter(AESEV == "SEVERE")']}
#      - {name: ae_list, from: severe, calls: [{table_plan: {}}, ...]}
#    page:
#      layout:
#        - {object: demog}
#        - {object: ae_list, page_break: false, space_before: 1}
#
#  `page_break` not written is a new page (TRUE, as rtfreporter's); a row
#  that goes on under the object before it writes `false`.  One object may
#  keep the fixed name (`plan`); two or more are named.  No `layout:`: the
#  last object is placed, as before.  Stops (class `tfl_page_layout`): a
#  name the content does not make, a name placed twice, an object whose
#  kind cannot be told, `first_rows` on a figure or on a row that starts a
#  fresh page.  A table, listing or figure made, not placed and not used
#  by another object: a check message (tfl_check_build()), not a stop;
#  helper objects (a data frame) get nothing.
#
#  One placement rule for the preview, the direct run and the program:
#  .page_layout_rows() says which objects go where; the session (tfl_report())
#  and the program (tfl_report_code()) write the same rtf_tables() calls
#  from it (.page_layout_steps(), R/spec_report.R).
#
#  Phase 1: tables and listings.  Figures and mixed reports come in phase
#  2: a figure report with a layout, or a figure placed, stops saying so.
# ============================================================================

# The keys of a layout row: the object's name and rtf_tables() /
# rtf_figures()'s items
.layout_keys <- c("object", "page_break", "space_before", "first_rows", "cont_title",
                  "titles", "footnotes", "width_in", "height_in")

# A stop of the page layout (class `tfl_page_layout`, and `cls` before it)
.layout_stop <- function(id, msg, cls = NULL) {
  m <- sprintf("%s: page.layout: %s", id %||% "the report", msg)
  stop(structure(class = c(cls, "tfl_page_layout", "error", "condition"),
                 list(message = m, call = NULL)))
}

# Does the report place its objects by a layout?
.has_layout <- function(output) {
  lay <- output$page$layout
  is.list(lay) && length(lay) > 0L && !inherits(lay, "tfl_unset")
}

# The objects the report's content makes, by name, in order, as far as it
# can be told before it runs: its object cards' names; one fixed name
# (`plan`) for a flat content; NULL for its own code (the names its list
# has are known when it runs)
.content_object_names <- function(output) {
  ct <- output$content
  if (inherits(ct, "tfl_r")) return(NULL)
  if (.content_has_objects(ct)) return(vapply(ct, function(o) as.character(o$fn), ""))
  "plan"
}

# The layout's rows, checked, named by their object, in their order; NULL
# with no `layout:` (the last object is placed, as before).  The stops that
# can be told from the SPEC: here, before anything runs.
.page_layout_rows <- function(output) {
  if (!.has_layout(output)) return(NULL)
  id <- output$id
  if (identical(output$kind, "figure")) {
    .layout_stop(id, paste0("a figure report's layout comes with phase 2 (figures and mixed ",
                            "reports); for now a figure report places its one figure: remove ",
                            "`layout:`."), "tfl_page_layout_phase2")
  }
  lay <- output$page$layout
  if (!is.null(names(lay))) {
    .layout_stop(id, "is a sequence of rows, `- {object: <name>, page_break: false, ...}`.")
  }
  objs <- vapply(seq_along(lay), function(i) {
    r <- lay[[i]]
    o <- if (is.list(r)) r[["object"]]
    if (!is.character(o) || length(o) != 1L || is.na(o) || !nzchar(o)) {
      .layout_stop(id, sprintf("row %d names no object (`object: <name>`).", i))
    }
    bad <- setdiff(names(r), .layout_keys)
    if (length(bad)) {
      .layout_stop(id, sprintf("row %d (%s): `%s` is not a layout key; the keys are %s.", i, o,
                               bad[1L], paste(.layout_keys, collapse = ", ")))
    }
    o
  }, "")
  if (anyDuplicated(objs)) {
    .layout_stop(id, sprintf("places %s twice; an object is placed once.",
                             paste(unique(objs[duplicated(objs)]), collapse = ", ")))
  }
  made <- .content_object_names(output)
  if (!is.null(made)) {
    miss <- setdiff(objs, made)
    if (length(miss)) {
      .layout_stop(id, sprintf("places %s, which the content does not make (it makes %s).",
                               paste(miss, collapse = ", "), paste(made, collapse = ", ")))
    }
  }
  for (i in seq_along(lay)) {
    r <- lay[[i]]
    if (!is.null(r$first_rows) && (i == 1L || !isFALSE(r$page_break))) {
      .layout_stop(id, sprintf(paste0("%s: `first_rows` is the rows a table puts on the page of ",
                                      "the object before it; this row starts a fresh page%s."),
                               objs[i], if (i == 1L) " (the first object)" else
                                 ": write `page_break: false`"))
    }
  }
  stats::setNames(lay, objs)
}

# The objects placed, in order: the layout's, else the last object
.placed_objects <- function(output) {
  rows <- .page_layout_rows(output)
  if (!is.null(rows)) return(names(rows))
  made <- .content_object_names(output)
  if (is.null(made)) "content" else made[length(made)]
}

# What an object is, for placing it: a table (a plan, a data frame, rtftable
# pages: rtf_tables()), a figure (a ggplot, rtfplot figures, a patchwork:
# rtf_figures()), or NA (cannot be told)
.object_place_kind <- function(x) {
  if (inherits(x, c("table_plan", "rtftable")) || is.data.frame(x)) return("table")
  if (inherits(x, c("ggplot", "rtfplot", "patchwork", "gg"))) return("figure")
  if (is.list(x) && length(x) && !is.object(x)) {
    k <- unique(vapply(x, function(e) {
      if (inherits(e, "rtftable")) "table" else if (inherits(e, c("ggplot", "rtfplot", "gg")))
        "figure" else NA_character_
    }, ""))
    if (length(k) == 1L && !is.na(k)) return(k)
  }
  NA_character_
}

# The objects the layout places, from what the content made (`objects`, a
# named list; a flat content's one plan, by its fixed name): each there,
# each a table or a listing (a figure: phase 2), as rtf_tables() takes it --
# checked before anything is rendered.  The objects, by name.
.layout_objects_check <- function(output, objects) {
  id <- output$id
  rows <- .page_layout_rows(output)
  if (is.null(rows)) return(invisible(objects))
  # (one object by its fixed name, a flat content's `plan`: that object)
  made <- .content_object_names(output)
  if (identical(made, "plan") && !.content_has_objects(output$content) &&
      !(is.list(objects) && !is.object(objects) && !is.null(names(objects)))) {
    objects <- list(plan = objects)
  }
  if (!is.list(objects) || is.object(objects) || is.null(names(objects))) {
    .layout_stop(id, sprintf(paste0("the content leaves %s, not its objects by name; it makes ",
                                    "the objects the layout places (%s), each named."),
                             .what(objects), paste(names(rows), collapse = ", ")))
  }
  miss <- setdiff(names(rows), names(objects))
  if (length(miss)) {
    .layout_stop(id, sprintf("places %s, which the content does not make (it makes %s).",
                             paste(miss, collapse = ", "),
                             if (length(objects)) paste(names(objects), collapse = ", ") else "none"))
  }
  for (nm in names(rows)) {
    k <- .object_place_kind(objects[[nm]])
    if (identical(k, "figure") && !is.null(rows[[nm]]$first_rows)) {
      .layout_stop(id, sprintf("%s is a figure: `first_rows` is a table's.", nm))
    }
    if (is.na(k)) {
      .layout_stop(id, sprintf(paste0("%s is %s: what it is cannot be told; a placed object is a ",
                                      "table (a table_plan, a data frame, rtftable pages)."),
                               nm, .what(objects[[nm]])), "tfl_page_layout_kind")
    }
    if (identical(k, "figure")) {
      .layout_stop(id, sprintf(paste0("%s is a figure: figures in a layout come with phase 2 ",
                                      "(figures and mixed reports); place tables and listings."),
                               nm), "tfl_page_layout_phase2")
    }
  }
  invisible(objects)
}

# The objects made but not placed and not used by another object (its
# `from:`), that are tables, listings or figures: a check message each;
# a helper (a data frame, anything else) says nothing
.layout_unplaced <- function(output, objects) {
  if (!is.list(objects) || is.object(objects) || is.null(names(objects))) return(character())
  placed <- .placed_objects(output)
  ct <- output$content
  used <- if (.content_has_objects(ct)) {
    unlist(lapply(ct, function(o) o[["input"]]))
  }
  left <- setdiff(names(objects), c(placed, used))
  Filter(function(nm) {
    x <- objects[[nm]]
    !is.data.frame(x) && !is.na(.object_place_kind(x))
  }, left)
}

# ---- the content cards, run as the program runs them ------------------------

# The card's kind: a listing's (its plan_listing, or the report a listing),
# else a table's
.card_kind <- function(card, kind) {
  fns <- vapply(card$items, function(it) .schema_fn_name(it$fn), "")
  if ("plan_listing" %in% fns || identical(kind, "listing")) "listing" else "table"
}

# The report as one card makes it: the card's calls its content, its kind;
# a card with `from:` reads that object (no data steps, no data object)
.card_output <- function(output, card) {
  sub <- output
  sub$content <- card$items
  sub$kind <- .card_kind(card, output$kind)
  sub$page$layout <- NULL
  if (!is.null(card[["input"]])) {
    sub$data$steps <- NULL
    sub$data$object <- NULL
  }
  sub
}

# A study's dataset (data.yml) by its name in lower case, read from the
# study folder; NULL when data.yml does not list it
.study_dataset_read <- function(study, name, dir = ".") {
  ds <- study$data$datasets %||% list()
  have <- tolower(vapply(ds, function(z) as.character(z$dataset %||% NA_character_)[1L], ""))
  i <- match(tolower(name), have)
  if (is.na(i)) return(NULL)
  p <- ds[[i]]$path
  .data_file_must_exist(toupper(name), p, dir)
  if (!grepl("^([A-Za-z]:)?[/\\\\]", p)) p <- file.path(dir, p)
  .ai_read_adam(p, toupper(name), study)
}

# A report's content cards, run: each object made in order, as its program
# writes it (.content_objects_code()) -- a placed card with the table's own
# rules (tfl_content_run() of the card), a helper card as its statement.
# `data`: what the report's content reads (the ARD, the records; a named
# list: each object by its name).  The objects a card's `from:` names that
# are not made above: the study's datasets (`study`, its folder `dir`).
# Returns every object, by name, in order.
.content_objects_run <- function(output, data, study = NULL, dir = ".") {
  ct <- output$content
  .content_objects_check(output)
  placed <- .placed_objects(output)
  env <- new.env(parent = asNamespace("rtfreporter"))
  named <- is.list(data) && !is.data.frame(data) && !is.object(data) && !is.null(names(data))
  if (named) {
    for (n in names(data)) assign(n, data[[n]], envir = env)
  } else {
    assign(output$data$object %||% (if (identical(output$kind, "listing")) "data" else "ard"),
           data, envir = env)
  }
  st <- if (is.character(study) && length(study) == 1L) tfl_read_study_spec(study) else study
  out <- list()
  for (card in ct) {
    nm <- as.character(card$fn)
    from <- card[["input"]]
    if (!is.null(from) && !exists(from, envir = env, inherits = FALSE)) {
      x <- if (!is.null(st)) .study_dataset_read(st, from, dir)
      if (is.null(x)) {
        .ard_stop(sprintf("%s: the content object %s reads `%s`, which no object above makes and %s.",
                          output$id, nm, from,
                          if (is.null(st)) "no study is given" else "spec/data.yml does not list"))
      }
      assign(from, x, envir = env)
    }
    obj <- if (nm %in% placed) {
      input <- if (!is.null(from)) get(from, envir = env, inherits = FALSE) else data
      suppressMessages(tfl_content_run(.card_output(output, card), input, study = study))
    } else {
      eval(parse(text = .content_object_lines(card)), env)
      get(nm, envir = env, inherits = FALSE)
    }
    assign(nm, obj, envir = env)
    out[[nm]] <- obj
  }
  out
}

# ---- content objects (B-5) -------------------------------------------------

# A table's or listing's content that holds objects in the map form
# (`- {name: plan, from: ard, calls: [...]}`, a tfl_plot of form "map")
.content_is_object <- function(it) inherits(it, "tfl_plot") && identical(it[["form"]], "map")

.content_has_objects <- function(content) {
  is.list(content) && !inherits(content, "tfl_r") && length(content) > 0L &&
    any(vapply(content, .content_is_object, NA))
}

# The checks of a content that holds objects, the same for its program and
# its run: all objects or none; the placed object(s) -- the layout's, else
# the last, which the program reads as `plan` -- with one input each
.content_objects_check <- function(output, plan = "plan") {
  ct <- output$content
  if (!all(vapply(ct, .content_is_object, NA))) {
    .ard_stop(paste0("tfl_content_code(): a ", output$kind, "'s content holds objects ",
                     "(`name:`, `from:`, `calls:`) or a flat list of calls, not both."))
  }
  nms <- vapply(ct, function(o) as.character(o$fn), "")
  if (anyDuplicated(nms)) {
    .ard_stop(sprintf("tfl_content_code(): %s makes %s twice; each content object has its own name.",
                      output$id %||% "the report", paste(unique(nms[duplicated(nms)]), collapse = ", ")))
  }
  layout <- .has_layout(output)
  placed <- .placed_objects(output)
  last <- ct[[length(ct)]]
  # (no layout: the last object is the plan the program reads; a layout
  # places its objects by their own names, canon 11-28)
  if (!layout && !identical(last$fn, plan)) {
    .ard_stop(sprintf(paste0("tfl_content_code(): the last object of a %s's content is `%s`; the ",
                             "program reads `%s`: name it `%s`, or place it by its name in ",
                             "`page: layout:`."),
                      output$kind, last$fn, plan, plan))
  }
  for (o in ct[nms %in% placed]) {
    if (!is.null(o[["input"]]) && !is.null(output$data$object)) {
      .ard_stop(sprintf(paste0("tfl_content_code(): `%s` takes `from: %s` and the report's `data: ",
                               "{object: %s}` is written too: two inputs; drop one of them."),
                        o$fn, o[["input"]], output$data$object))
    }
    if (!is.null(o[["input"]]) && (length(.content_r_steps(output)) || .content_rework(output))) {
      .ard_stop(sprintf(paste0("tfl_content_code(): `%s` takes `from: %s` and the report's data steps ",
                               "are written too: two inputs; drop one of them."),
                        o$fn, o[["input"]]))
    }
  }
  invisible(placed)
}

# The program of a content that holds objects: each object its statement, in
# order.  A placed object -- the layout's (each by its own name), else the
# last, the plan the program reads (`plan`) -- is written as a table's
# content is, the table's own rules (the study's rounding, labels, BIGN,
# `data:`), from its `from:` (piped) or, without one, from the report's data
# as today's content is; the other objects are helpers, each its statement.
# (A listing's records are written once: before the first placed object.)
.content_objects_code <- function(output, data, plan, pipe, template, study) {
  ct <- output$content
  placed <- .content_objects_check(output, plan)
  layout <- .has_layout(output)
  out <- character()
  first <- TRUE
  for (o in ct) {
    if (!o$fn %in% placed) {
      out <- c(out, .content_object_lines(o, pipe), "")
      next
    }
    piped <- !is.null(o[["input"]])
    sub <- .card_output(output, o)
    out <- c(out, .content_code(sub, data = if (piped) o[["input"]] else data,
                                plan = if (layout) o$fn else plan, pipe = pipe,
                                template = template, study = if (first) study, piped = piped), "")
    first <- FALSE
  }
  while (length(out) && identical(out[[length(out)]], "")) out <- out[-length(out)]
  out
}

# A helper object (not the plan): `name <- from |> call1() |> call2()`, or
# `name <- call1(...) |> call2()` without a `from:`; each call joined as its
# function's `join` says (tfl_fn_defs(); `|>` for R), the same chain as a
# figure's plot with `from:` (.fig_pipe_chain())
.content_object_lines <- function(o, pipe = NULL) {
  op <- .ard_pipe_op(pipe)
  input <- o[["input"]]
  is_r <- vapply(o$items, function(it) identical(it$fn, ".r"), NA)
  calls <- .content_calls(o$items[!is_r])
  k <- 0L
  txt <- character()
  joins <- character()
  for (i in seq_along(o$items)) {
    it <- o$items[[i]]
    if (is_r[i]) {
      txt <- c(txt, as.character(it$args$code))
      joins <- c(joins, "|>")
      next
    }
    k <- k + 1L
    head <- is.null(input) && !length(txt)
    txt <- c(txt, .spec_call_code(.content_print(calls[[k]]), if (head) 0L else 2L,
                                  lead = if (head) nchar(o$fn) + 4L else 2L))
    joins <- c(joins, .fig_fn_join(.schema_fn_name(it$fn)))
  }
  lines <- .fig_pipe_chain(o$fn, input, txt, joins)
  unlist(strsplit(sub(" \\|>$", paste0(" ", op), lines), "\n", fixed = TRUE))
}
