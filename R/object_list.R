# ============================================================================
#  A report's named objects: the list, their uses, a rename, a default name
# ----------------------------------------------------------------------------
#  Every stage of a report makes named R objects: the data rows (`id`), the
#  objects sheet, the analysis items (ard_<id>, a model's own name), a
#  figure's named plots (p_<name>) and data steps, and each stage's result
#  (ard, plan / content, doc).  tfl_output_objects() lists them, with the
#  R name the program gives each -- through the same builders the program
#  is written with (.ard_item_names(), .ard_model_name(), .fig_plot_name())
#  -- and where each is used: a field that names it (a structured
#  reference, `ref`) or R code that reads it (`r`, tfl_code_refs()).
#  tfl_output_rename() rewrites the references, lists the R that still
#  says the old name; tfl_output_object_uses() says whether an object can
#  be deleted; tfl_default_name() names a new one, as a screen and the AI
#  kit both do.  tfl_check_spec() calls .check_object_rows(): a name a
#  figure's field gives that nothing above defines.
# ============================================================================

# Each stage's block keys, in one place: the key that names a block (its
# name; NULL: the stage's default name only) and the keys that name another
# block as its input or an argument.  TODO (design B, OBJECTS.md b-2): an
# analysis item's `name`, the page's `name` and `from` are new keys the
# coordinator names; when they exist, add them here (name_key: analysis =
# "name", page = "name"; refs: page = "from") and the list, the uses and
# the rename follow.
.obj_stage_keys <- list(
  data = list(name_key = "id", refs = c("from", "subjects_in")),
  analysis = list(name_key = NULL, refs = c("data", "denominator")),
  page = list(name_key = NULL, refs = character())
)

# the kinds of a report's objects a person names, and the stage results
.obj_kinds <- c("data", "object", "ard", "model", "value", "plot")
.obj_result_kinds <- c("ard", "plan", "content", "doc")

# an entry's field as one text, or NA
.obj_chr <- function(x) {
  if (is.null(x) || is.list(x) || !length(x) || is.na(x[1L])) return(NA_character_)
  as.character(x[1L])
}

# The template's own data rows and analysis items, as its definition says
# them (a series template's file; a registry template expanded with the
# report's arguments), or NULL: what the report cannot rename
.obj_template_defs <- function(output, study = NULL) {
  ts <- tryCatch(.tpl_spec_of(output), error = function(e) NULL)
  if (is.null(ts)) return(NULL)
  if (grepl("/", ts$name, fixed = TRUE)) {
    y <- tryCatch(.series_read_yaml(.template_file(ts$name)), error = function(e) NULL)
    if (is.null(y)) return(NULL)
    return(list(name = ts$name, data = y$data %||% list(), analyses = y$analysis %||% list()))
  }
  x <- tryCatch(suppressMessages(suppressWarnings(
    tfl_template_expand(ts$name, ts$args, population = study$data$populations))),
    error = function(e) NULL)
  if (is.null(x)) return(NULL)
  list(name = ts$name, data = x$ard$analysis_data %||% list(), analyses = x$ard$analyses %||% list())
}

# A report's entries (the template's first, the report's own merged in by
# id, as the template resolves them), each with `fixed` (the template's)
.obj_merge <- function(tpl, own) {
  ids <- function(l) vapply(l, function(e) .obj_chr(e$id), "")
  out <- lapply(tpl, function(e) c(e[setdiff(names(e), "fixed")], list(fixed = TRUE)))
  for (e in own %||% list()) {
    k <- match(.obj_chr(e$id), ids(out))
    if (is.na(k) || is.na(.obj_chr(e$id))) {
      out <- c(out, list(c(e, list(fixed = FALSE))))
    } else {
      out[[k]] <- c(utils::modifyList(out[[k]][setdiff(names(out[[k]]), "fixed")], e),
                    list(fixed = TRUE))
    }
  }
  out
}

# The report's parts each object of it has: a one-object report is its
# own; one with `contents:` each object (its name in `object`)
.obj_parts <- function(output) {
  if (is.null(output$contents)) {
    return(list(list(object = NA_character_, kind = output$kind %||% "table",
                     content = output$content, steps = output$data$steps %||% list(),
                     data_object = output$data$object)))
  }
  objs <- tryCatch(.output_objects(output), error = function(e) NULL)
  if (is.null(objs)) return(list())
  lapply(names(objs), function(nm) {
    o <- objs[[nm]]
    list(object = nm, kind = o$kind %||% "table", content = o$content,
         steps = output$contents[[nm]]$data$steps %||% list(), data_object = o$data$object)
  })
}

# A figure's data steps that name what they make: the step, its name, the
# name it reads (a dataset read, upper case), a fit's kind
.obj_fig_steps <- function(steps) {
  out <- list()
  prev <- NA_character_
  for (i in seq_along(steps)) {
    s <- steps[[i]]
    if (!is.list(s)) next
    if (!is.null(s$dataset)) prev <- toupper(as.character(s$dataset))
    nm <- s$out %||% s$name
    if (is.null(nm)) next
    kind <- if (identical(s$step, "survfit") || !is.null(s$name)) "model" else "data"
    out[[length(out) + 1L]] <- list(i = i, name = as.character(nm), kind = kind, from = prev,
                                    step = s$step %||% NA_character_)
    if (identical(kind, "data")) prev <- as.character(nm)
  }
  out
}

# The R in an item's arguments: each `!r` value, by its argument
.obj_item_r <- function(args, at = character()) {
  out <- character()
  for (k in names(args) %||% rep("", length(args))) {
    v <- if (nzchar(k)) args[[k]] else NULL
    if (inherits(v, "tfl_r")) {
      out[[paste(c(at, k), collapse = ".")]] <- paste(as.character(v), collapse = "\n")
    } else if (is.list(v)) {
      out <- c(out, .obj_item_r(v, c(at, k)))
    }
  }
  out
}

# Everything tfl_output_objects() and the functions after it read: the
# objects and their uses
.obj_table <- function(output, study = NULL) {
  if (!inherits(output, "tfl_output")) {
    .ard_stop("tfl_output_objects(): `output` is a report (tfl_read_output()).")
  }
  tpl <- .obj_template_defs(output, study)
  data <- .obj_merge(tpl$data, output$ard$analysis_data)
  an <- .obj_merge(tpl$analyses, output$ard$analyses)
  obs <- output$ard$objects %||% list()
  rows <- list()
  row <- function(object, spec_name, code_name, stage, kind, where, fixed, from = NA_character_,
                  name_source = "written") {
    rows[[length(rows) + 1L]] <<- data.frame(
      output_id = output$id, object = object, spec_name = spec_name, code_name = code_name,
      stage = stage, kind = kind, where = where, name_source = name_source, fixed = fixed,
      from = from, stringsAsFactors = FALSE)
  }
  uses <- list()
  # (`pos`: the row of the object the use is in, for "only what is made
  # above"; NA when it is not one place)
  use <- function(name, stage, where, item, arg, how, pos = length(rows)) {
    if (is.na(name) || !nzchar(name)) return(invisible())
    uses[[length(uses) + 1L]] <<- data.frame(name = name, stage = stage, where = where,
                                             item = item, arg = arg, how = how, pos = pos,
                                             stringsAsFactors = FALSE)
  }
  code <- list()
  r_at <- function(stage, where, item, txt) {
    for (k in names(txt)) {
      if (is.na(txt[[k]]) || !nzchar(trimws(txt[[k]]))) next
      code[[length(code) + 1L]] <<- list(stage = stage, where = where, item = item, arg = k,
                                         code = txt[[k]])
    }
  }
  r_fields <- function(e, fields) {
    txt <- character()
    for (f in intersect(fields, names(e))) {
      v <- e[[f]]
      if (inherits(v, "tfl_r") || (is.character(v) && length(v) >= 1L)) {
        txt[[f]] <- paste(as.character(v), collapse = "\n")
      }
    }
    txt
  }

  # ---- data: the objects sheet, the data rows ----
  ob_ids <- vapply(obs, function(e) .obj_chr(e$id %||% e$object_id), "")
  for (i in seq_along(obs)) {
    row(NA_character_, ob_ids[i], ob_ids[i], "data", "object", sprintf("objects[%d]", i), FALSE)
    r_at("data", sprintf("objects[%d]", i), ob_ids[i], r_fields(obs[[i]], "code"))
  }
  d_ids <- vapply(data, function(e) .obj_chr(e$id), "")
  for (i in seq_along(data)) {
    e <- data[[i]]
    w <- sprintf("data[%d]", i)
    row(NA_character_, d_ids[i], d_ids[i], "data", "data", w, isTRUE(e$fixed), .obj_chr(e$from))
    for (f in .obj_stage_keys$data$refs) use(.obj_chr(e[[f]]), "data", w, d_ids[i], f, "ref")
    r_at("data", w, d_ids[i], r_fields(e, c("where", "derive", "code", "then")))
  }
  # the data the program makes itself (an analysis set's, a condition's):
  # tfl_ard_data_names(), with the study
  dn <- if (!is.null(study) && length(an)) {
    tryCatch(tfl_ard_data_names(study, output_id = output$id), error = function(e) NULL)
  }
  auto <- if (!is.null(dn)) setdiff(unique(dn$data[dn$data != "NULL"]), c(d_ids, ob_ids)) else character()
  for (nm in auto) {
    row(NA_character_, NA_character_, nm, "data", "data", "data (the program's)", TRUE,
        toupper(sub("_sub[0-9]*$", "", nm)), "default")
  }

  # ---- analysis: each item under its R name, then the ARD ----
  top <- which(vapply(an, function(e) is.na(.obj_chr(e$parent)), NA))
  a_ids <- vapply(an, function(e) .obj_chr(e$id), "")
  types <- vapply(an, function(e) .obj_chr(e$type), "")
  types[is.na(types)] <- "ard"
  a_pos <- rep(NA_integer_, length(an))
  if (length(top)) {
    pops <- unique(stats::na.omit(vapply(c(data, an), function(e) .obj_chr(e$population), "")))
    dsets <- unique(stats::na.omit(vapply(data, function(e) .obj_chr(e$from), "")))
    taken <- c(.r_name(setdiff(dsets, d_ids)), if (length(pops)) paste0("pop_", .r_name(pops)),
               d_ids, ob_ids, auto)
    ard_names <- .ard_item_names(a_ids[top], c(taken, .ard_fun_names()))
    avoid <- c(taken, ard_names, paste0(ard_names, "_conditions"), paste0(ard_names, "_one"),
               "ard", "ards", "report_id", .ard_helper_names)
    for (j in seq_along(top)) {
      i <- top[j]
      e <- an[[i]]
      cn <- ard_names[j]
      if (identical(types[i], "model")) {
        cn <- .ard_model_name(a_ids[i], avoid)
        avoid <- c(avoid, cn)
      }
      w <- sprintf("analysis[%d]", i)
      # (the item's name is its stage's default, from its id: no name key yet)
      row(NA_character_, a_ids[i], cn, "analysis", types[i], w, isTRUE(e$fixed), .obj_chr(e$data),
          "default")
      a_pos[i] <- length(rows)
    }
    if (any(types[top] == "ard")) {
      row(NA_character_, NA_character_, "ard", "analysis", "ard", "analysis", TRUE, name_source = "default")
    }
  }
  for (i in seq_along(an)) {
    e <- an[[i]]
    w <- sprintf("analysis[%d]", i)
    # (an item under another: at its parent's place)
    ap <- if (!is.na(a_pos[i])) a_pos[i] else a_pos[match(.obj_chr(e$parent), a_ids)]
    for (f in .obj_stage_keys$analysis$refs) {
      use(.obj_chr(e[[f]]), "analysis", w, a_ids[i], f, "ref", ap)
    }
    r_at("analysis", w, a_ids[i], r_fields(e, c("where", "post", "code", "args")))
  }

  # ---- content: a figure's data steps and named plots, the result ----
  parts <- .obj_parts(output)
  for (p in parts) {
    sw <- if (is.na(p$object)) "data.steps" else sprintf("contents.%s.data.steps", p$object)
    named <- .obj_fig_steps(p$steps)
    at_i <- vapply(named, function(s) s$i, 0L)
    for (i in seq_along(p$steps)) {
      s <- p$steps[[i]]
      if (!is.list(s)) next
      j <- match(i, at_i)
      if (!is.na(j)) {
        row(p$object, named[[j]]$name, named[[j]]$name, "data", named[[j]]$kind,
            sprintf("%s[%d]", sw, i), FALSE, named[[j]]$from)
      }
      use(.obj_chr(s[["in"]]), "data", sprintf("%s[%d]", sw, i), s$step %||% NA_character_, "in", "ref",
          if (!is.na(j)) length(rows) else length(rows) + 1L)
    }
    cw <- if (is.na(p$object)) "content" else sprintf("contents.%s.content", p$object)
    use(.obj_chr(p$data_object), "content", if (is.na(p$object)) "data" else
      sprintf("contents.%s.data", p$object), "data", "object", "ref", length(rows) + 1L)
    ct <- p$content
    if (inherits(ct, "tfl_r")) {
      txt <- paste(as.character(ct), collapse = "\n")
      r_at("content", cw, "content", c(content = txt))
      # a step written as R: its top-level assignments are its objects
      # (canon 1748456); the stage's result is the row below
      for (nm in setdiff(.obj_r_assigned(txt), "content")) {
        row(p$object, nm, nm, "content", "object", paste0(cw, " (R)"), FALSE)
      }
    } else {
      is_plot <- vapply(ct, inherits, NA, "tfl_plot")
      one <- sum(is_plot) == 1L
      # (an item in a plot is at the plot's row; one after the plots, at
      # the content's own -- below every plot)
      item_uses <- function(it, w, plot = NA_character_) {
        at_pos <- if (is.na(plot)) length(rows) + 1L else length(rows)
        fn <- .schema_fn_name(it$fn %||% "")
        item <- if (is.na(plot)) fn else paste0(plot, "$", fn)
        if (identical(fn, ".r")) {
          r_at("content", w, item, c(code = paste(as.character(it$args$code), collapse = "\n")))
          return(invisible())
        }
        if (identical(p$kind, "figure")) {
          oa <- tryCatch(.fig_objects_of(fn), error = function(e) .fig_object_args)
          for (k in intersect(oa, names(it$args))) {
            v <- it$args[[k]]
            if (is.character(v) && length(v) == 1L) use(v, "content", w, item, k, "ref", at_pos)
          }
          if (identical(fn, "wrap_plots")) {
            for (v in as.character(unlist(it$args$plots))) use(v, "content", w, item, "plots", "ref", at_pos)
          }
        }
        r_at("content", w, item, .obj_item_r(it$args))
      }
      for (i in seq_along(ct)) {
        it <- ct[[i]]
        w <- sprintf("%s[%d]", cw, i)
        # (TODO, OBJECTS.md B-6: a table's `- name: [items]` is read as a
        # figure's plot today; when tables take named blocks, list them here)
        if (is_plot[i] && !identical(p$kind, "figure")) next
        if (is_plot[i]) {
          row(p$object, it$fn, if (one) "content" else .fig_plot_name(it$fn), "content", "plot", w, FALSE)
          for (k in seq_along(it$items)) {
            item_uses(it$items[[k]], sprintf("%s[%d]", w, k), it$fn)
          }
        } else {
          item_uses(it, w)
        }
      }
    }
    if (is.na(p$object)) {
      res <- if (inherits(ct, "tfl_r") || identical(p$kind, "figure")) "content" else "plan"
      row(NA_character_, NA_character_, res, "content", res, "content", TRUE, name_source = "default")
    }
  }

  # ---- the template's arguments that name a data of the report ----
  ts <- tryCatch(.tpl_spec_of(output), error = function(e) NULL)
  if (!is.null(ts) && length(ts$args)) {
    defs <- tryCatch(tfl_arg_defs(template = ts$name), error = function(e) NULL)
    if (is.data.frame(defs) && "choices" %in% names(defs)) {
      on <- defs$name[grepl("report_data", as.character(defs$choices), fixed = TRUE)]
      for (k in intersect(on, names(ts$args))) {
        v <- ts$args[[k]]
        if (is.character(v) && length(v) == 1L) {
          use(v, "template", "template.args", ts$name, k, "ref", NA_integer_)
        }
      }
    }
  }

  # ---- page ----
  row(NA_character_, NA_character_, "doc", "page", "doc", "page", TRUE, name_source = "default")
  pg <- if (is.list(output$page)) output$page else list()
  for (f in .obj_stage_keys$page$refs) use(.obj_chr(pg[[f]]), "page", "page", "page", f, "ref")

  objects <- do.call(rbind, c(list(.obj_empty()), rows))
  # the R code that reads a name of them
  nms <- unique(stats::na.omit(c(objects$spec_name, objects$code_name)))
  for (cc in code) {
    hit <- tfl_code_refs(stats::setNames(cc$code, cc$arg), nms)
    for (n in hit$name) use(n, cc$stage, cc$where, cc$item, cc$arg, "r", NA_integer_)
  }
  uses <- do.call(rbind, c(list(.obj_uses_empty()), uses))
  # each object's users: the items that name it (the analyses that read a
  # data, with the study, as the ARD program reads it)
  objects$used_by <- vapply(seq_len(nrow(objects)), function(i) {
    by <- uses$item[uses$name %in% stats::na.omit(c(objects$spec_name[i], objects$code_name[i]))]
    if (!is.null(dn) && identical(objects$stage[i], "data")) {
      by <- c(by, dn$analysis_id[dn$data %in% objects$code_name[i]])
    }
    by <- unique(by[!is.na(by)])
    if (length(by)) paste(by, collapse = " | ") else NA_character_
  }, "")
  # used (card rule 11): something names it or reads it; a stage's result
  # and an ARD item (bound into the ARD) count as used, so do the plots a
  # figure with no wrap_plots puts together, the only plot (the content) and
  # the data a listing prints (its last)
  wrapped <- any(uses$arg == "plots")
  last_data <- utils::tail(which(objects$kind == "data" & grepl("^data\\[", objects$where)), 1L)
  objects$used <- !is.na(objects$used_by) | objects$name_source == "default" & objects$fixed |
    objects$kind == "ard" |
    (objects$kind == "plot" & (!wrapped | objects$code_name == "content")) |
    (identical(output$kind, "listing") & seq_len(nrow(objects)) %in% last_data)
  rownames(objects) <- NULL
  rownames(uses) <- NULL
  list(objects = objects, uses = uses)
}

# The list as tfl_output_objects() gives it: one name an object (the name
# the SPEC writes; an analysis item's and a stage result's, the stage's
# default the program gives), an analysis item's id, whether it can change
.obj_public <- function(objects) {
  an <- objects$stage == "analysis" & !is.na(objects$spec_name)
  data.frame(output_id = objects$output_id, object = objects$object,
             name = ifelse(an | is.na(objects$spec_name), objects$code_name, objects$spec_name),
             id = ifelse(an, objects$spec_name, NA_character_),
             stage = objects$stage, kind = objects$kind, where = objects$where,
             name_source = objects$name_source, changeable = !objects$fixed,
             used = objects$used, from = objects$from, used_by = objects$used_by,
             stringsAsFactors = FALSE)
}

# The names a step written as R assigns at its top level (`x <- ...`,
# `x = ...`, `... -> x`), in order; attribute `assign`: TRUE when it calls
# assign() (names not seen); NULL when it does not parse
.obj_r_assigned <- function(code) {
  e <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  if (is.null(e)) return(NULL)
  out <- character()
  for (x in as.list(e)) {
    if (is.call(x) && as.character(x[[1L]])[1L] %in% c("<-", "=", "<<-") && is.symbol(x[[2L]])) {
      out <- c(out, as.character(x[[2L]]))
    } else if (is.call(x) && identical(as.character(x[[1L]])[1L], "->") && is.symbol(x[[3L]])) {
      out <- c(out, as.character(x[[3L]]))
    }
  }
  structure(unique(out), assign = "assign" %in% all.names(e))
}

.obj_empty <- function() {
  data.frame(output_id = character(), object = character(), spec_name = character(),
             code_name = character(), stage = character(), kind = character(), where = character(),
             name_source = character(), fixed = logical(), from = character(),
             stringsAsFactors = FALSE)
}
.obj_uses_empty <- function() {
  data.frame(name = character(), stage = character(), where = character(), item = character(),
             arg = character(), how = character(), pos = integer(), stringsAsFactors = FALSE)
}

#' A report's named objects, and where each is used
#'
#' Every object a report's program makes under a name, one row each in
#' the order the report defines them: its data rows (`data:`, the name is
#' the row's `id`), the objects sheet's objects, its analysis items (an ARD
#' or a value item's default name made from its `id`, a model item's `id`
#' in lower case), a figure's data steps that name what they make and its
#' named plots (`- km: [items]`), and the result of each stage (`ard`,
#' `plan` or `content`, `doc`).  A written name is the R object's name
#' (card rule 7); a default name is made by the same code that writes the
#' program.  For a screen that lists a report's objects, lets a person
#' pick one where a field names one, rename one ([tfl_output_rename()]) or
#' delete one ([tfl_output_object_uses()]).
#'
#' @param output A report ([tfl_read_output()]).
#' @param study The study ([tfl_read_study_spec()]), or `NULL`.  With it
#'   the list also has the data the ARD program makes itself (an analysis
#'   set's subjects, an analysis's own condition), and a data's `used_by`
#'   the analyses that read it as the program does ([tfl_ard_data_names()]);
#'   a registry template's rows are expanded with the study's analysis
#'   sets.
#' @return A data frame, one row an object: `output_id`; `object` (the
#'   object of a report with `contents:`, else `NA`); `name` (`adsl_sub`,
#'   `km`, an ARD item's default name, `ard`); `id` (an analysis item's id,
#'   the ARS analysis id; else `NA`); `stage` (`data`, `analysis`,
#'   `content`, `page`); `kind` (`data`, `object`, `ard`, `model`, `value`,
#'   `plot`, and the stage results `ard`, `plan`, `content`, `doc`);
#'   `where` (the stage and the position: `data[2]`, `analysis[1]`,
#'   `content[3]`, `data.steps[2]`); `name_source` (`written`: the SPEC
#'   writes the name; `default`: the stage's default -- an analysis item's
#'   from its id, a stage's result); `changeable` (`FALSE`: the report
#'   cannot rename it -- the template defines it, it is a stage's result,
#'   the program makes it); `used` (something names it or reads it; a
#'   stage's result and an ARD item count as used); `from` (what it is made
#'   from: a dataset or a data above, an item's `data`, `NA`); `used_by`
#'   (the items that name it or whose R reads it, ` | ` between them).  Its
#'   attribute `uses` is the uses, one row a use: `name` (the name as
#'   written there), `stage`, `where`, `item` (the data row's or the item's
#'   id, the content function), `arg` (the field), `how` (`ref`: a field
#'   that names it; `r`: R code that reads it, found by
#'   [tfl_code_refs()]).
#' @examples
#' out <- tfl_output("F-1", "figure", content = list(
#'   tfl_plot("km", list(tfl_item("ggsurvfit", list(x = "fit")))),
#'   tfl_plot("risk", list(tfl_item("tfl_risk_panel", list(fit = "fit")))),
#'   tfl_item("wrap_plots", list(plots = list("km", "risk"), ncol = 1))))
#' x <- tfl_output_objects(out)
#' x[, c("name", "stage", "kind", "where", "used")]
#' attr(x, "uses")
#' @seealso [tfl_output_rename()], [tfl_output_object_uses()],
#'   [tfl_default_name()], [tfl_code_refs()]
#' @export
tfl_output_objects <- function(output, study = NULL) {
  x <- .obj_table(output, study)
  structure(.obj_public(x$objects), uses = x$uses[setdiff(names(x$uses), "pos")])
}

# the row of `name`: by its SPEC name, else by its R name
.obj_find <- function(objects, name) {
  k <- which(!is.na(objects$spec_name) & objects$spec_name == name)
  if (!length(k)) k <- which(objects$code_name == name)
  k
}

#' Where a report uses one of its objects: can it be deleted
#'
#' The uses of one object of a report ([tfl_output_objects()]): each field
#' that names it and each piece of R code that reads it.  No row: nothing
#' uses it, it can be deleted.
#'
#' @param output A report ([tfl_read_output()]).
#' @param name The object's name (`adsl_sub`, `km`), or an analysis
#'   item's id.
#' @param study The study, or `NULL` (as [tfl_output_objects()]).
#' @return A data frame, one row a use: `name` (as written there),
#'   `stage`, `where`, `item`, `arg`, `how` (`ref` or `r`).
#' @examples
#' out <- tfl_output("F-1", "figure", content = list(
#'   tfl_plot("km", list(tfl_item("ggsurvfit", list(x = "fit")))),
#'   tfl_plot("risk", list(tfl_item("tfl_risk_panel", list(fit = "fit")))),
#'   tfl_item("wrap_plots", list(plots = list("km", "risk")))))
#' tfl_output_object_uses(out, "risk")
#' @seealso [tfl_output_objects()], [tfl_output_rename()]
#' @export
tfl_output_object_uses <- function(output, name, study = NULL) {
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    .ard_stop("tfl_output_object_uses(): `name` is one object's name.")
  }
  x <- .obj_table(output, study)
  k <- .obj_find(x$objects, name)
  if (!length(k)) {
    .ard_stop(sprintf("tfl_output_object_uses(): %s has no object %s.", output$id, name))
  }
  nms <- unique(stats::na.omit(c(x$objects$spec_name[k], x$objects$code_name[k])))
  u <- x$uses[x$uses$name %in% nms, setdiff(names(x$uses), "pos"), drop = FALSE]
  rownames(u) <- NULL
  u
}

# The name rule of a kind's name (a data row's id, a plot's)
.obj_name_ok <- function(kind, name) {
  switch(kind,
    data = .adata_name_ok(name),
    plot = grepl("^[A-Za-z][A-Za-z0-9_]*$", name) && !name %in% c(.fig_p_fns, .fig_compose_fns) &&
      !exists(name, envir = asNamespace("ggplot2"), inherits = FALSE),
    object = identical(make.names(name), name),
    nzchar(trimws(name)))
}

# The one name check of a report (card rule 7): the kind's rule, then a
# name given above in the report, a study dataset's (as written or as its R
# name), another of the program's names, a function's.  NA, or what is
# wrong: "rule", "repeated", "dataset", "other", "function".
.obj_name_problem <- function(name, kind, above = character(), others = character(),
                              datasets = character(), functions = character()) {
  if (!isTRUE(.obj_name_ok(kind, name))) return("rule")
  if (name %in% above) return("repeated")
  if (name %in% c(datasets, .r_name(datasets))) return("dataset")
  if (name %in% others) return("other")
  if (name %in% functions) return("function")
  NA_character_
}

#' Rename an object of a report
#'
#' A report with one of its objects renamed: the object's own name (a
#' data row's `id`, an objects sheet's id, a figure's plot or data step)
#' and every field that names it -- a data row's `from` and
#' `subjects_in`, an analysis's `data` and `denominator`, a table's
#' `data: {object:}`, a figure's object arguments (`ggsurvfit`'s `x`,
#' `data`, `fit`) and `wrap_plots`'s `plots`, a template's argument that
#' names a data.  An analysis item's name is its stage's default, made
#' from its `id`: renaming the item changes its `id` (and an item's
#' `parent` that names it), and its R name follows.  R code that reads
#' the name (a `where`, a `derive`, an `!r`) is not changed: it is listed,
#' to change by hand; a text that says an analysis id (a content filter
#' on `analysis_id`) is not found.
#'
#' Stops for a name the report cannot change (`fixed` in
#' [tfl_output_objects()]: the template's, the program's), a new name
#' another object has, and a new name its kind does not take (a data
#' row's: lower case letters, digits, `_` and `.`, a letter first; a
#' plot's: letters, digits and `_`, a letter first, not a function's
#' name).
#'
#' @param output A report ([tfl_read_output()]).
#' @param old,new The object's name and its new name, as the SPEC writes
#'   them.
#' @param study The study, or `NULL` (as [tfl_output_objects()]).
#' @return The report, renamed.  Its attribute `r_uses`: the R code that
#'   still says the old name (the rows of [tfl_output_object_uses()] whose
#'   `how` is `r`).
#' @examples
#' out <- tfl_output("F-1", "figure", content = list(
#'   tfl_plot("km", list(tfl_item("ggsurvfit", list(x = "fit")))),
#'   tfl_plot("risk", list(tfl_item("tfl_risk_panel", list(fit = "fit")))),
#'   tfl_item("wrap_plots", list(plots = list("km", "risk")))))
#' r <- tfl_output_rename(out, "km", "km_os")
#' r$content[[3]]$args$plots
#' @seealso [tfl_output_objects()], [tfl_output_object_uses()]
#' @export
tfl_output_rename <- function(output, old, new, study = NULL) {
  who <- "tfl_output_rename()"
  if (!is.character(old) || length(old) != 1L || is.na(old) ||
      !is.character(new) || length(new) != 1L || is.na(new)) {
    .ard_stop(sprintf("%s: `old` and `new` are one name each.", who))
  }
  x <- .obj_table(output, study)
  ob <- x$objects
  k <- which(!is.na(ob$spec_name) & ob$spec_name == old)
  if (!length(k)) {
    a <- which(ob$code_name == old & ob$stage == "analysis" & !is.na(ob$spec_name))
    if (length(a)) {
      .ard_stop(sprintf(paste0("%s: %s is analysis item %s's default name, made from its id: ",
                               "rename the item by its id (%s)."), who, old, ob$spec_name[a[1L]],
                        ob$spec_name[a[1L]]))
    }
    if (any(ob$code_name == old)) {
      .ard_stop(sprintf("%s: %s cannot be renamed: it is a stage's result or the program's own.",
                        who, old))
    }
    .ard_stop(sprintf("%s: %s has no object %s to rename (its names: %s).", who, output$id, old,
                      paste(unique(stats::na.omit(ob$spec_name)), collapse = ", ")))
  }
  k <- k[1L]
  r <- ob[k, ]
  if (identical(old, new)) return(structure(output, r_uses = x$uses[0L, , drop = FALSE]))
  if (isTRUE(r$fixed)) {
    .ard_stop(sprintf("%s: %s is %s's name: the report cannot change it.", who, old,
                      if (r$stage == "data" && !is.na(r$spec_name)) "the template" else "the program"))
  }
  if (!.obj_name_ok(r$kind, new)) {
    .ard_stop(sprintf("%s: %s is not a name of a %s.", who, new, r$kind))
  }
  # the new R name, by the builder that gives the old one
  new_code <- switch(r$kind,
    ard = , value = if (r$stage == "analysis") .ard_item_names(new) else new,
    model = if (r$stage == "analysis") .ard_model_name(new) else new,
    plot = if (identical(r$code_name, "content")) "content" else .fig_plot_name(new),
    new)
  others <- ob[-k, , drop = FALSE]
  clash <- intersect(c(new, new_code), stats::na.omit(c(others$spec_name, others$code_name)))
  if (length(clash)) {
    .ard_stop(sprintf("%s: %s already names an object of %s.", who, clash[1L], output$id))
  }
  # a field names the object by its SPEC name, or (a model) its R name
  swap <- function(v) {
    if (!is.character(v) || length(v) != 1L || is.na(v)) return(v)
    if (identical(v, old)) return(new)
    if (identical(v, r$code_name) && !identical(r$code_name, "content")) return(new_code)
    v
  }
  rn_entries <- function(l, fields, own = FALSE) {
    lapply(l, function(e) {
      if (!is.list(e)) return(e)
      if (own && identical(.obj_chr(e$id), old)) e$id <- new
      if (own && identical(.obj_chr(e$object_id), old)) e$object_id <- new
      for (f in intersect(fields, names(e))) e[[f]] <- swap(e[[f]])
      e
    })
  }
  is_data <- r$stage == "data" && grepl("^data\\[", r$where)
  is_ob <- grepl("^objects\\[", r$where)
  is_an <- r$stage == "analysis"
  is_step <- grepl("data\\.steps\\[", r$where)
  if (!is.null(output$ard$analysis_data)) {
    output$ard$analysis_data <- rn_entries(output$ard$analysis_data, .obj_stage_keys$data$refs, is_data)
  }
  if (!is.null(output$ard$objects)) output$ard$objects <- rn_entries(output$ard$objects, character(), is_ob)
  if (!is.null(output$ard$analyses)) {
    output$ard$analyses <- rn_entries(output$ard$analyses,
                                      c(.obj_stage_keys$analysis$refs, if (is_an) "parent"), is_an)
  }
  # (TODO, OBJECTS.md b-2: the page's `from` once the key exists --
  # .obj_stage_keys$page$refs)
  for (f in intersect(.obj_stage_keys$page$refs, names(output$page))) {
    output$page[[f]] <- swap(output$page[[f]])
  }
  rn_steps <- function(steps) {
    lapply(steps, function(s) {
      if (!is.list(s)) return(s)
      if (is_step) {
        for (f in intersect(c("out", "name"), names(s))) if (identical(s[[f]], old)) s[[f]] <- new
      }
      if (identical(s[["in"]], old)) s[["in"]] <- new
      s
    })
  }
  rn_items <- function(items) {
    if (inherits(items, "tfl_r") || !is.list(items)) return(items)
    lapply(items, function(it) {
      if (inherits(it, "tfl_plot")) {
        if (identical(r$kind, "plot") && identical(it$fn, old)) it$fn <- new
        it$items <- rn_items(it$items)
        return(it)
      }
      fn <- .schema_fn_name(it$fn %||% "")
      if (identical(fn, ".r") || !is.list(it$args)) return(it)
      oa <- tryCatch(.fig_objects_of(fn), error = function(e) .fig_object_args)
      for (a in intersect(oa, names(it$args))) it$args[[a]] <- swap(it$args[[a]])
      if (identical(fn, "wrap_plots") && !is.null(it$args$plots)) {
        it$args$plots <- lapply(it$args$plots, swap)
      }
      it
    })
  }
  if (identical(output$kind, "figure")) {
    if (!is.null(output$data$steps)) output$data$steps <- rn_steps(output$data$steps)
    output$content <- rn_items(output$content)
  }
  if (is.list(output$data) && !is.null(output$data$object)) output$data$object <- swap(output$data$object)
  for (nm in names(output$contents)) {
    co <- output$contents[[nm]]
    if (!is.null(co$data$steps)) co$data$steps <- rn_steps(co$data$steps)
    if (!is.null(co$data$object)) co$data$object <- swap(co$data$object)
    co$content <- rn_items(co$content)
    output$contents[[nm]] <- co
  }
  # a template's argument that names a data
  if (is.list(output$template) && length(x$uses) &&
      any(x$uses$stage == "template" & x$uses$name == old)) {
    for (a in x$uses$arg[x$uses$stage == "template" & x$uses$name == old]) {
      if (identical(output$template$args[[a]], old)) output$template$args[[a]] <- new
      for (part in c("ard", "content")) {
        if (identical(output$template[[part]][[a]], old)) output$template[[part]][[a]] <- new
      }
    }
  }
  nms <- unique(stats::na.omit(c(r$spec_name, r$code_name)))
  ru <- x$uses[x$uses$how == "r" & x$uses$name %in% nms, setdiff(names(x$uses), "pos"), drop = FALSE]
  rownames(ru) <- NULL
  structure(output, r_uses = ru)
}

#' The default name of a new object of a report
#'
#' The name a screen and the AI kit give an object a person adds to a
#' report, so that both give the same: a data row `<from, lower case>_sub`
#' (`adsl_sub`), a plot `plot_1`, `plot_2` ..., a model item's id
#' `<method>_fit`, a value item's `<method>_value`; `_2`, `_3` ... when the
#' report has the name already (as the SPEC writes it or as the program
#' does).  An ARD item's R name is the program's, from its `id`; a
#' stage's result is `plan` (a table's or a listing's content),
#' `content` (a figure's) and `doc` (the page).
#'
#' @param output A report ([tfl_read_output()]).
#' @param kind `"data"`, `"plot"`, `"model"`, `"value"`, `"ard"`,
#'   `"plan"`, `"content"` or `"doc"`.
#' @param from A data row's: what it is made from (`"ADSL"`, a data above).
#' @param method A model's or a value's: its method (`"survfit"`).
#' @param id An ARD item's: its id (the ARS analysis id).
#' @return The name, a string.
#' @examples
#' out <- tfl_output("T-1", "table")
#' tfl_default_name(out, "data", from = "ADSL")
#' tfl_default_name(out, "plot")
#' tfl_default_name(out, "model", method = "survival::survfit")
#' @seealso [tfl_output_objects()]
#' @export
tfl_default_name <- function(output, kind = c("data", "plot", "model", "value", "ard", "plan",
                                             "content", "doc"),
                             from = NULL, method = NULL, id = NULL) {
  kind <- match.arg(kind)
  one <- function(x, what) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
      .ard_stop(sprintf("tfl_default_name(): a %s's name needs `%s`, one text.", kind, what))
    }
    x
  }
  ob <- .obj_table(output)$objects
  have <- unique(stats::na.omit(c(ob$spec_name, ob$code_name)))
  word <- function(x) gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(sub("^.*::", "", x))))
  first_free <- function(base) {
    if (!base %in% have) return(base)
    k <- 2L
    while (paste0(base, "_", k) %in% have) k <- k + 1L
    paste0(base, "_", k)
  }
  switch(kind,
    data = first_free(paste0(word(one(from, "from")), "_sub")),
    plot = {
      k <- 1L
      while (any(c(paste0("plot_", k), .fig_plot_name(paste0("plot_", k))) %in% have)) k <- k + 1L
      paste0("plot_", k)
    },
    model = first_free(paste0(word(one(method, "method")), "_fit")),
    value = first_free(paste0(word(one(method, "method")), "_value")),
    # (the program's own name for the item, by its builder)
    ard = .ard_item_names(one(id, "id"), have),
    kind)
}

#' Check a report's names: the card rules
#'
#' The rows [tfl_check_spec()] gives for a report's names (card rules 5, 7
#' and 11): a field that names an object made below it, or (a figure's
#' `wrap_plots` `plots`, an object argument such as `ggsurvfit`'s `x`) one
#' nothing above makes, an error; a name given twice in the report, a
#' study dataset's or a function's name, an error; an object the report
#' names and nothing uses, a note.  For a screen that moves a card or a
#' block: the report as it would be is checked, and a move that gives an
#' error row is not kept.
#'
#' @param output A report ([tfl_read_output()]).
#' @param study The study ([tfl_read_study_spec()]), or `NULL`: its
#'   datasets' names are not taken.
#' @return A data frame, the columns of [tfl_check_spec()]'s: `output_id`,
#'   `part`, `item`, `level`, `message`.  No row: the names keep the rules.
#' @examples
#' out <- tfl_output("F-1", "figure", content = list(
#'   tfl_plot("km", list(tfl_item("ggsurvfit", list(x = "fit")))),
#'   tfl_item("wrap_plots", list(plots = list("km", "risk")))))
#' tfl_output_name_check(out)
#' @seealso [tfl_check_spec()], [tfl_output_objects()]
#' @export
tfl_output_name_check <- function(output, study = NULL) {
  if (!inherits(output, "tfl_output")) {
    .ard_stop("tfl_output_name_check(): `output` is a report (tfl_read_output()).")
  }
  .check_object_rows(output, output$id, .obj_study_datasets(study))
}

# the study's datasets' names (no object of a report takes one)
.obj_study_datasets <- function(study) {
  sd <- study$data$datasets %||% list()
  unique(stats::na.omit(as.character(if (is.data.frame(sd)) sd$dataset else
    vapply(sd, function(e) as.character(e$dataset %||% NA)[1L], ""))))
}

# ---- tfl_check_spec(): the names a report's fields give ---------------------

# The common card rules (canon 93e2bd3) a report's names keep, as check rows:
# 5) a field names only an object made above it -- an error (a figure's
#    field that names an object nothing above makes, wrap_plots's plots
#    not a plot: the code is written as it is and stops when it runs);
# 7) a name once in a report, not a study dataset's or a function's -- an
#    error;
# 11) an object the report names and nothing uses -- a note.
# `datasets`: the study's datasets.
.check_object_rows <- function(o, id, datasets = character()) {
  rows <- list(.check_rows())
  add <- function(...) rows[[length(rows) + 1L]] <<- .check_rows(output_id = id, ...)
  x <- tryCatch(.obj_table(o), error = function(e) NULL)
  if (is.null(x)) return(rows[[1L]])
  ob <- x$objects
  u_ref <- x$uses[x$uses$how == "ref", , drop = FALSE]
  # 5) a field that names an object made below it (or itself)
  for (i in which(!is.na(u_ref$pos))) {
    k <- .obj_find(ob, u_ref$name[i])
    if (!length(k) || k[1L] < u_ref$pos[i]) next
    add(part = u_ref$stage[i], item = paste0(u_ref$item[i], "$", u_ref$arg[i]), level = "error",
        message = sprintf(paste0("%s names `%s`, which is made below it (%s): a field names only an object ",
                                 "made above it; move `%s` up or name another"),
                          u_ref$arg[i], u_ref$name[i], ob$where[k[1L]], u_ref$name[i]))
  }
  # 7) a name twice, a dataset's or a function's name (a data row's id: the
  # ARD definition's own check)
  mine <- !is.na(ob$spec_name) & ob$name_source == "written"
  dup <- unique(ob$spec_name[mine][duplicated(ob$spec_name[mine])])
  for (nm in dup) {
    add(part = "names", item = nm, level = "error", message = sprintf(
      "`%s` names %d objects of the report (%s): a name is given once", nm, sum(ob$spec_name %in% nm),
      paste(ob$where[ob$spec_name %in% nm], collapse = ", ")))
  }
  own <- which(mine & !(ob$kind == "data" & grepl("^data\\[", ob$where)))
  fns <- unique(c(.ard_fun_names(), getNamespaceExports("ggplot2")))
  for (k in own) {
    nm <- ob$spec_name[k]
    kind <- if (ob$kind[k] %in% c("plot", "object")) ob$kind[k] else "object"
    pr <- .obj_name_problem(nm, kind, datasets = datasets, functions = fns)
    if (is.na(pr)) next
    what <- switch(pr,
                   rule = "not an R name", dataset = "a study dataset's name",
                   "function" = "a function's name", NULL)
    if (is.null(what)) next
    add(part = "names", item = nm, level = "error", message = sprintf(
      "`%s` (%s) is %s: name the object otherwise", nm, ob$where[k], what))
  }
  # a step written as R: its result is assigned; assign()'s names cannot be
  # checked (canon 1748456)
  for (pp in .obj_parts(o)) {
    if (!inherits(pp$content, "tfl_r")) next
    nm <- .obj_r_assigned(paste(as.character(pp$content), collapse = "\n"))
    if (is.null(nm)) next
    at <- if (is.na(pp$object)) "content" else paste0(pp$object, ": content")
    if (!"content" %in% nm && !isTRUE(attr(nm, "assign"))) {
      add(part = "names", item = at, level = "note", message = paste0(
        "the content written as R assigns no `content` at its top level: the page reads `content`"))
    }
    if (isTRUE(attr(nm, "assign"))) {
      add(part = "names", item = at, level = "note", message = paste0(
        "the content written as R calls assign(): the names it makes cannot be checked"))
    }
  }
  # 11) a name nothing uses (not the template's, not a stage's result; a
  # listing prints its last data)
  if (is.null(o$figure)) {
    for (k in which(mine & !ob$fixed & !ob$used)) {
      add(part = "names", item = ob$spec_name[k], level = "note", message = sprintf(
        "`%s` (%s) is made and not used: nothing names it and no R reads it", ob$spec_name[k], ob$where[k]))
    }
  }
  for (p in .obj_parts(o)) {
    ct <- p$content
    if (inherits(ct, "tfl_r") || !is.list(ct) || !length(ct)) next
    is_plot <- vapply(ct, inherits, NA, "tfl_plot")
    at <- if (is.na(p$object)) "" else paste0(p$object, ": ")
    if (!identical(p$kind, "figure") || !tryCatch(.fig_new_content(ct), error = function(e) FALSE)) next
    plots <- vapply(ct[is_plot], function(it) as.character(it$fn), "")
    # the names above the content: the report's data, objects and items,
    # the figure's data steps (each step's, the data's own df), the ARD
    io <- tryCatch(.fig_step_io(p$steps), error = function(e) list())
    known <- unique(stats::na.omit(c(
      ob$spec_name[ob$stage != "content"], ob$code_name[ob$stage != "content"],
      "df", "ard", vapply(io, function(z) as.character(z$out %||% NA_character_), ""),
      tryCatch(tfl_fig_data_name(list(data = p$steps)), error = function(e) NULL))))
    u <- x$uses[x$uses$how == "ref" & x$uses$stage == "content", , drop = FALSE]
    pre <- if (is.na(p$object)) "^content\\[" else paste0("^contents\\.", p$object, "\\.content\\[")
    u <- u[grepl(pre, u$where), , drop = FALSE]
    for (i in seq_len(nrow(u))) {
      fn <- sub("^.*\\$", "", u$item[i])
      if (identical(u$arg[i], "plots") && identical(fn, "wrap_plots")) {
        if (!u$name[i] %in% plots) {
          add(part = "content", item = paste0(at, "wrap_plots(plots)"), level = "error",
              message = sprintf(paste0("wrap_plots's plots names `%s`, not a plot of the figure (%s): ",
                                       "name one of its plots (- name: [items])"),
                                u$name[i], if (length(plots)) paste(plots, collapse = ", ") else "none"))
        }
        next
      }
      # (a value that is not one R name -- an expression -- is R, not a name)
      if (!identical(make.names(u$name[i]), u$name[i]) || u$name[i] %in% known) next
      add(part = "content", item = sprintf("%s%s(%s)", at, u$item[i], u$arg[i]), level = "error",
          message = sprintf(paste0("%s's %s names `%s`, which nothing above the figure's content makes ",
                                   "(its data, data steps or analysis items: %s): name one of them"),
                            fn, u$arg[i], u$name[i],
                            paste(setdiff(known, c("df", "ard")), collapse = ", ")))
    }
  }
  do.call(rbind, rows)
}
