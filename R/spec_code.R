# ============================================================================
#  A table or report definition -> the R code that makes it
# ----------------------------------------------------------------------------
#  tfl_table_plan() and tfl_report() turn a definition workbook into an
#  object.  tfl_table_code() and tfl_report_code() write the same thing as
#  a program: the plan_*() verbs (tables) and the rtfreporter calls
#  (reports) the workbook stands for.
#
#  Both halves come from ONE list of steps, so the object and the program
#  cannot disagree: a step is a call -- a function and its arguments, as
#  values -- which .spec_eval() runs and .spec_code() writes out.
#  .plan_spec_steps() says which verbs a table definition is;
#  .report_spec_steps() which rtfreporter calls a report definition is.
# ============================================================================

# a call to write or to run: `fun` and its arguments (values, or calls /
# symbols of their own)
.spec_call <- function(fun, ...) {
  a <- list(...)
  a <- a[!vapply(a, is.null, logical(1L))]
  structure(list(fun = fun, args = a), class = "tfl_spec_call")
}

# a name in the program (`doc`, `plan`): its value comes from `env` when run
.spec_sym <- function(name) structure(list(name = name), class = "tfl_spec_sym")

# an R expression written in a workbook (`tables$keep_where`): as it is in
# the program, the language object when run (plan_filter() reads it unevaluated)
.spec_expr <- function(text) structure(list(text = text), class = "tfl_spec_expr")

# a one-sided formula written in a workbook (`where` of a cell style):
# `~ <text>` in the program, a formula when run
.spec_formula <- function(text) {
  structure(list(text = text), class = "tfl_spec_formula")
}

# A function a step names: tflspec's own, else rtfreporter's (the plan
# verbs, rtf_document() and friends).
.spec_fun <- function(name) {
  if (grepl("::", name, fixed = TRUE)) {
    return(getExportedValue(sub("::.*$", "", name), sub("^.*::", "", name)))
  }
  if (exists(name, envir = asNamespace("tflspec"), inherits = FALSE)) {
    return(get(name, envir = asNamespace("tflspec"), mode = "function"))
  }
  .spec_need_rtfreporter()
  get(name, envir = asNamespace("rtfreporter"), mode = "function")
}

.spec_need_rtfreporter <- function() {
  if (!requireNamespace("rtfreporter", quietly = TRUE)) {
    .ard_stop(paste0("This needs the rtfreporter package (the table engine ",
                     "and the RTF renderer): install it, and library(rtfreporter) ",
                     "for the code tflspec writes."))
  }
}

.spec_eval <- function(x, env) {
  if (inherits(x, "tfl_spec_sym")) return(get(x$name, envir = env))
  if (inherits(x, "tfl_spec_expr")) return(str2lang(x$text))
  if (inherits(x, "tfl_spec_formula")) {
    # over the table's columns, with base R (the program's own objects
    # are its code's, tfl_table_code())
    return(stats::as.formula(paste("~", x$text), env = globalenv()))
  }
  if (inherits(x, "tfl_spec_call")) {
    f <- .spec_fun(x$fun)
    return(do.call(f, lapply(x$args, .spec_eval, env = env)))
  }
  if (is.list(x) && !is.object(x)) {
    return(lapply(x, .spec_eval, env = env))
  }
  x
}

# an argument name as it can be written: bare, or quoted
.spec_name <- function(n) {
  if (!nzchar(n)) return("")
  if (identical(make.names(n), n)) n else encodeString(n, quote = "\"")
}

# ---- writing a value as R --------------------------------------------------

# One line when it fits in `width` at this `indent`; otherwise one element a
# line, indented.  Values read back as what they were (checked by the
# tests: the written program makes the same pages).
.spec_code <- function(x, indent = 0L, width = 80L) {
  tb <- .spec_tribble(x, indent)
  if (!is.null(tb)) return(tb)
  one <- .spec_code_line(x)
  if (nchar(one) + indent <= width) return(one)
  parts <- .spec_parts(x)
  if (is.null(parts)) return(one)
  pad <- strrep(" ", indent + 2L)
  el <- vapply(seq_along(parts$items), function(i) {
    nm <- parts$names[i]
    v <- .spec_code(parts$items[[i]], indent + 2L, width)
    paste0(pad, if (nzchar(nm)) paste0(.spec_name(nm), " = "), v)
  }, "")
  paste0(parts$open, "(\n", paste(el, collapse = ",\n"), "\n",
         strrep(" ", indent), ")")
}

# A data frame of a few columns as a person writes a grid: tribble() rows,
# the columns aligned -- when the program attaches dplyr (which exports
# it); else NULL (data.frame(), a column an argument)
.spec_tribble <- function(x, indent = 0L) {
  if (!is.data.frame(x) || !nrow(x) || !"dplyr" %in% .attached()) return(NULL)
  if (!all(vapply(x, function(v) is.atomic(v) && is.null(dim(v)), NA))) return(NULL)
  if (!identical(make.names(names(x)), names(x))) return(NULL)
  cells <- rbind(paste0("~", names(x)), matrix(vapply(x, function(v) {
    out <- vapply(as.list(v), .spec_code_line, "")
    if (!all(is.na(v))) out[is.na(v)] <- "NA"
    out
  }, character(nrow(x))), nrow = nrow(x)))
  w <- apply(cells, 2L, function(col) max(nchar(col)))
  pad <- strrep(" ", indent + 2L)
  rows <- vapply(seq_len(nrow(cells)), function(i) {
    cl <- vapply(seq_along(w), function(j)
      if (j < length(w)) formatC(paste0(cells[i, j], ","), width = -(w[j] + 1L))
      else paste0(cells[i, j], if (i < nrow(cells)) ","), "")
    paste0(pad, sub(" +$", "", paste(cl, collapse = " ")))
  }, "")
  paste0("tribble(\n", paste(rows, collapse = "\n"), "\n", strrep(" ", indent), ")")
}

# the head and the elements of a value that can be written over lines
.spec_parts <- function(x) {
  nm <- function(v) names(v) %||% rep("", length(v))
  if (inherits(x, "tfl_spec_call")) {
    return(list(open = x$fun, items = x$args, names = nm(x$args)))
  }
  if (inherits(x, "cell_rows")) {
    v <- unclass(x)
    return(list(open = "cell_rows", items = v, names = nm(v)))
  }
  if (is.data.frame(x)) {
    # check.names = FALSE only when a name needs it
    v <- c(as.list(x), if (!identical(make.names(names(x)), names(x)))
      list(check.names = FALSE))
    return(list(open = "data.frame", items = v, names = nm(v)))
  }
  if (is.list(x) && !is.object(x)) {
    return(list(open = "list", items = x, names = nm(x)))
  }
  if (is.atomic(x) && length(x) > 1L &&
      !length(setdiff(names(attributes(x)), "names"))) {
    v <- as.list(x)
    return(list(open = "c", items = v, names = nm(x)))
  }
  NULL
}

.spec_code_line <- function(x) {
  if (inherits(x, "tfl_spec_sym")) return(x$name)
  if (inherits(x, "tfl_spec_expr")) return(x$text)
  if (inherits(x, "tfl_spec_formula")) return(paste("~", x$text))
  p <- .spec_parts(x)
  if (!is.null(p) && !(is.atomic(x) && !is.list(x))) {
    el <- vapply(seq_along(p$items), function(i)
      paste0(if (nzchar(p$names[i])) paste0(.spec_name(p$names[i]), " = "),
             .spec_code_line(p$items[[i]])), "")
    return(paste0(p$open, "(", paste(el, collapse = ", "), ")"))
  }
  # whole numbers as a person writes them: c(0, -1), not 0:-1 or c(0L, -1L)
  if (is.integer(x) && length(x) > 1L && is.null(names(x)) && !anyNA(x)) {
    return(paste0("c(", paste(x, collapse = ", "), ")"))
  }
  if (is.integer(x) && length(x) == 1L && is.null(names(x)) && !is.na(x)) {
    return(as.character(x))
  }
  paste(deparse(x, width.cutoff = 500L,
                control = c("keepNA", "keepInteger", "niceNames",
                            "showAttributes")),
        collapse = " ")
}

# a call, on one line when it fits after `indent` characters
# `lead`: the columns the line has besides the call (the name it is
# assigned to, a pipe's indent and its " |>")
.spec_call_code <- function(cl, indent = 2L, lead = indent) {
  one <- .spec_code_line(cl)
  if (nchar(one) + lead <= 80L) return(one)
  .spec_code(cl, indent)
}

# ---- tables ----------------------------------------------------------------

# The roles a table definition gives table_plan() (its `tables` sheet):
# which column goes across, which go down, which carries the row text.
# Everything else on that sheet is a layer, said by the verb it belongs to.
.plan_role_names <- c("cols", "rows", "label", "stat", "show_keys")
.plan_spec_roles <- function(sp) {
  sa <- .ard_spec_table_args(sp)
  sa[intersect(.plan_role_names, names(sa))]
}

# The label column's name the roles give, as the plan will call it: one
# column is itself, several coalesce into `label` (or the name given).
.spec_label_name <- function(roles) {
  lb <- roles[["label"]]
  if (is.null(lb)) return("label")
  if (length(lb) == 1L && !is.list(lb) && is.na(lb)) return(character(0))
  nm <- names(lb)
  if (is.list(lb)) {
    if (length(lb) == 1L && is.character(lb[[1L]]) && length(lb[[1L]]) >= 2L) {
      return(if (is.null(nm) || !nzchar(nm[1L])) "label" else nm[1L])
    }
  } else if (is.character(lb) && length(lb) >= 2L) {
    return(if (is.null(nm) || !any(nzchar(nm))) "label" else nm[nzchar(nm)][1L])
  }
  if (!is.null(nm) && nzchar(nm[1L])) nm[1L] else "label"
}

# Digits as plan_digits() takes them: numbers when every entry is a count
# of decimals, text when any is "<k>s" (significant digits).
.spec_digit_values <- function(x) {
  n <- suppressWarnings(as.integer(x))
  if (!anyNA(n)) n else as.character(x)
}

# plan_levels()'s names in one order, whatever the sheets' rows: the
# variables' order (`variable`), the column keys, the row keys and a
# stack's, then the categorical variables -- each group as the roles name it
.spec_levels_order <- function(lv, roles, sp) {
  if (length(lv) < 2L) return(lv)
  flat <- function(x) {
    x <- unlist(x)
    if (is.null(x)) return(character())
    c(rbind(names(x) %||% rep("", length(x)), unname(x)))
  }
  keys <- unique(c("variable", flat(roles[["cols"]]), flat(roles[["rows"]]),
                   .ard_spec_plan_cols(sp)))
  keys <- keys[nzchar(keys)]
  rank <- match(names(lv), keys)
  lv[order(is.na(rank), rank)]
}

# The verbs a table definition stands for, in the order a report is built:
# what tfl_table_plan() runs, and what tfl_table_code() writes.  `roles` are
# the plan's (the workbook's, and any given in the call), which decide the
# label column's name.
.plan_spec_steps <- function(roles, sp) {
  st <- list()
  add <- function(fun, ...) st[[length(st) + 1L]] <<- .spec_call(fun, ...)
  sa <- .ard_spec_table_args(sp)
  if (!is.null(sa[["sort"]]) || !is.null(sa[["sort_stat"]])) {
    add("plan_sort", sa[["sort"]], stat = sa[["sort_stat"]])
  }
  if (!is.null(sa[["rounding"]])) add("plan_digits", rounding = sa[["rounding"]])
  lv <- .ard_spec_levels(sp)
  # the variables' order: the values of the ARD's `variable` column
  vo <- .ard_spec_variable_order(sp)
  if (length(vo) && is.null(lv[["variable"]])) lv <- c(list(variable = vo), lv)
  lv <- .spec_levels_order(lv, roles, sp)
  de <- .ard_spec_drop_empty(sp)
  if (length(lv) || length(de)) {
    do.call(add, c(list("plan_levels"), as.list(lv),
                   if (length(de)) list(.drop_empty = de)))
  }
  lb <- .ard_spec_labels(sp)
  vl <- .ard_spec_value_labels(sp)
  if (length(lb) || length(vl)) do.call(add, c(list("plan_labels"), as.list(lb), vl))
  ne <- .ard_spec_nest(sp)
  if (length(ne)) do.call(add, c(list("plan_nest"), ne))
  if (!is.null(sa[["total"]])) {
    add("plan_total", label = sa[["total"]], position = sa[["total_position"]])
  }
  if (!is.null(sa[["stat_groups"]])) {
    do.call(add, c(list("plan_stat_groups"), sa[["stat_groups"]]))
  }
  cm <- .ard_spec_cells(sp)
  how <- sa[intersect(c("stats", "value", "na"), names(sa))]
  if (length(cm) || length(how)) do.call(add, c(list("plan_cells"), cm, how))
  fw <- .ard_spec_filter(sp)
  if (!is.null(fw)) {
    do.call(add, c(list("plan_filter", .spec_expr(fw$where)),
                   list(any = fw$any, parents = fw$parents)))
  }
  cp <- .ard_spec_compare(sp)
  if (!is.null(cp)) {
    do.call(add, c(list("plan_compare"), cp$cols,
                   list(by = cp$by, position = cp$position)))
  }

  # rows with no template: the display format of one statistic, for a
  # table that lays the statistics out as rows
  f <- sp$cells[is.na(sp$cells$template), , drop = FALSE]
  if (nrow(f)) {
    if (!identical(sa[["stats"]], "rows")) {
      .ard_stop(paste0(
        "The `cells` sheet has rows with no `template` -- a statistic's ",
        "display format --
  but the table is not `stats = rows`.  ",
        "Give those rows a template, or set
  `tables$stats` to `rows`."))
    }
    if (any(!is.na(f$variable) | !is.na(f$context))) {
      .ard_stop(paste0(
        "A `cells` row with no template formats one statistic across the ",
        "table;
  leave its `variable` and `context` blank."))
    }
    if (any(is.na(f$row))) {
      .ard_stop(paste0("A `cells` row with no template needs `row`: the ",
                       "statistic it formats, as the label column prints it."))
    }
    # decimals, or "3s" for three significant digits, keyed by the row
    # label: plan_digits(.rows = ) applies them to every value column
    dg <- ifelse(!is.na(f$signif), paste0(f$signif, "s"), f$digits)
    add("plan_digits", .rows = stats::setNames(
      .spec_digit_values(dg), f$row))
  }

  lay <- if (nrow(sp$layout)) .ard_spec_typed(sp$layout[1L, ], "layout")
         else list()
  pick <- function(prefix, map) {
    out <- list()
    for (k in names(map)) {
      v <- lay[[paste0(prefix, k)]]
      if (!is.null(v)) out[[map[[k]]]] <- v
    }
    out
  }
  a <- pick("stub_", c(vars = "vars", name = "name", indent = "indent",
                       summary = "group_summary", before = "before"))
  if (length(a)) do.call(add, c(list("plan_stub"), a))
  if (isTRUE(lay[["group_page"]])) {
    add("plan_paginate_group", col = lay[["group_col"]],
        keep = if (identical(lay[["group_keep"]], FALSE)) FALSE,
        heading = lay[["group_heading"]], heading_at = lay[["group_heading_at"]])
  }
  a <- pick("group_", c(mode = "mode", collapse = "collapse"))
  if (length(a)) do.call(add, c(list("plan_row_group"), a))
  a <- pick("blank_", c(where = "where", first = "first", last = "last",
                        counted = "counted"))
  if (length(a)) do.call(add, c(list("plan_blanks"), a))
  a <- pick("pages_", c(max_rows = "max_rows", split = "split",
                        break_before = "break_before",
                        min_group_rows = "min_group_rows",
                        cont_label = "cont_label", page_by = "page_by",
                        heading = "heading", heading_at = "heading_at"))
  if (length(a)) do.call(add, c(list("plan_paginate_rows"), a))
  a <- pick("colpages_", c(at = "at", cut_by = "cut_by", every = "every",
                           keep = "keep", fit = "fit",
                           allow_span_break = "allow_span_break",
                           order = "order"))
  if (length(a)) do.call(add, c(list("plan_paginate_cols"), a))

  sty <- if (nrow(sp$style)) .ard_spec_typed(sp$style[1L, ], "style")
         else list()
  # the report sheet's table_font_size_half_points: the table's own font size
  # (rtfreporter: a table's appearance is its plan's, not rtf_tables()'s)
  if (is.null(sty[["font_size_half_points"]])) {
    rp <- .ard_spec_report_font(sp)
    if (!is.null(rp)) sty[["font_size_half_points"]] <- rp
  }
  cl <- sp$columns
  ct <- lapply(seq_len(nrow(cl)), function(i)
    .ard_spec_typed(cl[i, , drop = FALSE], "columns"))
  flag <- function(k) vapply(ct, function(r) isTRUE(r[[k]]), NA)
  # auto_width sits on the style sheet (one row per report) but is a
  # question about the columns, so it goes to plan_columns() below
  auto_width <- sty[["auto_width"]]
  sty[["auto_width"]] <- NULL
  # and col_header_align is the header's, plan_col_header()
  hd_align <- sty[["col_header_align"]]
  sty[["col_header_align"]] <- NULL
  # a kind of row's rules, `top | bottom`, as the rtf_border() they stand for
  for (z in grep("^border_", names(sty), value = TRUE)) {
    sty[[z]] <- do.call(.spec_call, c(list("rtf_border"), as.list(
      stats::setNames(rep(TRUE, length(sty[[z]])), sty[[z]]))))
  }
  if (length(sty)) do.call(add, c(list("plan_style"), sty))
  # each cell style row, one plan_cell_style()
  for (i in seq_len(nrow(sp$cell_styles))) {
    cs <- .ard_spec_typed(sp$cell_styles[i, , drop = FALSE], "cell_styles")
    w <- cs[["where"]]
    cs[["where"]] <- if (!is.null(w)) .spec_formula(w)
    if (isTRUE(cs[["header"]])) cs[["header"]] <- TRUE else cs[["header"]] <- NULL
    cs <- cs[c("cols", "header", "where", "bold", "italic", "align", "color",
               "background", "underline", "indent_twips")]
    cs <- cs[!vapply(cs, is.null, NA)]
    do.call(add, c(list("plan_cell_style"), cs))
  }
  if (any(flag("hide"))) add("plan_hide", cl$column[flag("hide")])
  hd <- sp$col_header
  if (nrow(hd)) {
    cells <- hd[setdiff(names(hd), "output_id")]
    keep <- vapply(cells, function(v) any(!is.na(v)), NA)
    cells <- cells[keep]
    rownames(cells) <- NULL
    hn <- sa[["header_n"]]
    add("plan_col_header", header = cells,
        values = if (!is.null(hn))
          if (is.null(names(hn))) list(n = hn) else as.list(hn),
        col_header_align = hd_align)
  } else if (!is.null(hd_align)) {
    add("plan_col_header", col_header_align = hd_align)
  }
  w <- vapply(ct, function(r) r[["rel_width"]] %||% NA_real_, NA_real_)
  if (any(!is.na(w)) || any(flag("decimal_split")) || any(flag("row_title")) ||
      !is.null(auto_width) || !is.null(sa[["sep"]])) {
    add("plan_columns",
        widths = if (any(!is.na(w)))
          stats::setNames(w[!is.na(w)], cl$column[!is.na(w)]),
        decimal = if (any(flag("decimal_split"))) cl$column[flag("decimal_split")],
        row_title = if (any(flag("row_title"))) cl$column[flag("row_title")],
        auto_width = auto_width, sep = sa[["sep"]])
  }
  # in the recommended order -- the schema's ranks: the mapping (cells,
  # totals, comparisons, the rows kept), then the finishing (levels,
  # labels, digits, sort, header, stub, pages, columns, style); a verb said
  # twice (plan_digits' rounding, then its rows) keeps its order
  rank <- vapply(st, function(x) as.numeric(tfl_schema(x$fun)$rank %||% 1000), 0)
  st[order(rank, seq_along(st))]
}

#' The code of a table's plan, from its definition
#'
#' Writes the [rtfreporter::table_plan()] pipeline a table definition stands
#' for, one pipe from the ARD: [rtfreporter::normalize_ard()] (the hierarchy,
#' the overall row and the stack the definitions say), the
#' roles of its `tables` sheet in `table_plan()`, and one `plan_*()` verb
#' for each thing the other sheets say -- the same verbs, with the same
#' values, that `tfl_table_plan(data, spec)` applies.  The program then no
#' longer reads the workbook, and what the workbook cannot say (a cell style,
#' a step after the pages are made) is written under it by hand.
#' [tfl_as_table_spec()] goes the other way.
#'
#' @param spec A table definition ([tfl_read_table_spec()],
#'   [tfl_table_spec()]), or the path(s) of its workbook(s).
#' @param output_id The table, when the definition has several.
#' @param data The name of the ARD in the program: the pipe starts from it
#'   (`plan <- ard |> normalize_ard(...) |> table_plan(...)`).  With
#'   `normalize = FALSE`, the name of the data already normalized.
#' @param plan The name the plan is assigned to.
#' @param pipe `"|>"` or `"%>%"`; `NULL` follows
#'   `getOption("rtfreporter.ard_pipe")` (see [rtfreporter::plan_template()]).
#' @param ard_spec The ARD definition ([tfl_read_ard_spec()]), for
#'   [rtfreporter::normalize_ard()]'s `hierarchy`; `NULL` for none.
#' @param normalize `TRUE` to write [rtfreporter::normalize_ard()] at the
#'   head of the pipe (its `hierarchy`, `overall` and `stack` from the
#'   definitions); `FALSE` when the program normalizes the ARD itself
#'   ([tfl_normalize_code()]).
#' @return The code, one element per line.
#' @seealso [tfl_report_code()] for the report around it.
#' @examples
#' spec <- tfl_read_table_spec(
#'   system.file("extdata", "ard-spec", "DM.xlsx", package = "tflspec"))
#' cat(tfl_table_code(spec), sep = "\n")
#' @export
tfl_table_code <- function(spec, output_id = NULL, data = "ard",
                           plan = "plan", pipe = NULL, ard_spec = NULL,
                           normalize = TRUE) {
  sp <- .ard_spec_scope(.as_spec(spec, "table", "tfl_table_code",
                                 output_id = output_id), output_id)
  items <- .plan_content_items(sp)
  if (isTRUE(normalize)) {
    nz <- .normalize_spec_args(sp, ard_spec, output_id %||% attr(sp, "output_id"),
                               "tfl_table_code", guess = TRUE)
    items <- c(list(tfl_item("normalize_ard", nz)), items)
  }
  .content_table_code(items, data = data, plan = plan, pipe = pipe)
}

# A table's content -- its items, in order (tfl_item()) -- as the program:
# `plan <- data |> table_plan(...) |> plan_*() ...`, the code lists of
# plan_levels() at the head.  An item from the study's defaults or a
# pattern carries a comment saying so.  What tfl_table_code() writes from
# a workbook, and tfl_content_code() from a spec file.
# A table's content as its pipeline: the steps on the data before the
# plan (normalize_ard(), the items before table_plan()), the head
# table_plan(), and the layers -- each a call as R takes it.  What
# tfl_content_code() writes and tfl_content_run() runs: the one list both
# read, so the two cannot differ.
.content_table_pipeline <- function(content, kind = "table") {
  fns <- vapply(content, function(it) .schema_fn_name(it$fn), "")
  # a listing has no roles: its plan is table_plan(data) with none
  if (identical(kind, "listing") && !"table_plan" %in% fns) {
    content <- c(list(tfl_item("table_plan", list())), content)
    fns <- c("table_plan", fns)
  }
  is_head <- fns == "table_plan"
  if (sum(is_head) != 1L) {
    .ard_stop(paste0("A table's content has one table_plan() item (its roles); ",
                     "this one has ", sum(is_head), "."))
  }
  pre <- fns %in% .content_data_steps
  content <- .content_cells_order(content, fns)
  # table_plan() is the head (the roles), not a layer: wherever the
  # layering put it, it starts the plan; the data's steps come before it,
  # the layers after, each keeping its order
  content <- c(content[pre], content[is_head], content[!pre & !is_head])
  calls <- .content_calls(content)
  n_pre <- sum(pre)
  list(pre = calls[seq_len(n_pre)], head = calls[[n_pre + 1L]],
       steps = calls[-seq_len(n_pre + 1L)],
       from = vapply(content[-seq_len(n_pre + 1L)], function(it) it$from %||% "own", ""))
}

# the functions a table's content may apply to the data before its plan
.content_data_steps <- c("normalize_ard")

# The content without its data steps (normalize_ard()): when the report's
# `data.steps` make the data, the normalization is theirs, not the
# study defaults' as well
.content_drop_data_steps <- function(content) {
  Filter(function(it) !.schema_fn_name(it$fn) %in% .content_data_steps, content)
}

# plan_cells()'s entries in the order the sheets path writes them
# (.ard_spec_cells()): the keys of the variables the content names (its
# `variables` sheet, in that sheet's order) first, then the others (the
# kinds, the contexts, `default`) as they come; stats / value / na after.
# A layered content has the study default's keys first, which the sheets
# give after the table's own variables.
.content_cells_order <- function(content, fns) {
  i <- which(fns == "plan_cells")
  if (length(i) != 1L) return(content)
  args <- content[[i]]$args
  nm <- names(args)
  if (is.null(nm) || length(args) < 2L) return(content)
  sh <- tryCatch(.plan_sheets_from_content(content), error = function(e) NULL)
  v <- sh$variables
  ord <- if (is.data.frame(v) && nrow(v)) {
    if ("order" %in% names(v) && any(!is.na(v$order))) {
      v <- v[order(suppressWarnings(as.numeric(v$order)), na.last = TRUE), , drop = FALSE]
    }
    as.character(v$variable)
  } else character()
  how <- nm %in% c("stats", "value", "na", "notes") | !nzchar(nm)
  rank <- match(sub("\r.*$", "", nm), ord)
  rank[nm == "default" | how] <- NA
  o <- order(how, is.na(rank), rank)
  if (identical(o, seq_along(args))) return(content)
  content[[i]]$args <- args[o]
  content
}

.content_table_code <- function(content, data = "data", plan = "plan",
                                pipe = NULL, kind = "table") {
  op <- .ard_pipe_op(pipe)
  pl <- .content_table_pipeline(content, kind)
  head <- pl$head
  steps <- pl$steps
  from <- pl$from
  # the data's own steps make a pipe of their own into table_plan()
  if (length(pl$pre)) {
    steps <- c(list(head), steps)
    from <- c("own", from)
    head <- NULL
  } else {
    head$args <- c(list(.spec_sym(data)), head$args)
  }
  # the levels as code lists at the head, cl_<variable> as the ARD programs
  # write them, plan_levels() naming them
  cl <- character()
  for (i in seq_along(steps)) {
    if (!identical(steps[[i]]$fun, "plan_levels")) next
    a <- steps[[i]]$args
    # (the variables' order stays as it is: `variable` is no data variable)
    v <- setdiff(names(a), c(".drop_empty", "variable"))
    v <- v[vapply(a[v], is.character, NA)]
    if (!length(v)) next
    cl <- c("# the code lists: each value in its order, as the table shows it",
            .codelists_lines(a[v]), "")
    for (k in v) a[[k]] <- .spec_sym(.cl_name(k))
    steps[[i]]$args <- a
  }
  first <- if (is.null(head)) data else .spec_call_code(.content_print(head), 0L)
  pre_lines <- vapply(pl$pre, function(s) paste0("  ", .spec_call_code(.content_print(s))), "")
  lines <- c(first, pre_lines,
             vapply(steps, function(s) paste0("  ", .spec_call_code(.content_print(s))), ""))
  lines[-length(lines)] <- paste(lines[-length(lines)], op)
  lines[1L] <- paste(plan, "<-", lines[1L])
  from <- c(rep("own", length(pre_lines)), from)
  # an item the report does not say itself: where it comes from
  note <- c("", vapply(from, function(f) {
    if (identical(f, "own")) return("")
    if (identical(f, "defaults")) return("  # (study default)")
    if (startsWith(f, "template:")) return(paste0("  # (template ", sub("^template:", "", f), ")"))
    if (startsWith(f, "style:")) return(paste0("  # (study style: ", sub("^style:", "", f), ")"))
    paste0("  # (pattern ", sub("^pattern:", "", f), ")")
  }, ""))
  lines <- unlist(lapply(seq_along(lines), function(i)
    c(if (nzchar(note[i])) note[i], lines[i])))
  .drop_attached_ns(c(cl, unlist(strsplit(lines, "\n", fixed = TRUE))))
}

# ---- content: the items of a spec, one a function call -----------------------

# A table workbook's content: its roles as the table_plan() item, then one
# item a verb (.plan_spec_steps()), each argument as a spec file holds it
# (.schema_from_r()).  The bridge from the sheets: a workbook and a spec
# file reach the same code through it, and the converter to spec files
# writes its result.
.plan_content_items <- function(sp) {
  roles <- .plan_spec_roles(sp)
  steps <- .plan_spec_steps(roles, sp)
  c(list(tfl_item("table_plan", .schema_from_r("table_plan", roles))),
    lapply(steps, function(st) tfl_item(st$fun, .schema_from_r(st$fun, st$args))))
}

#' The content of a table workbook as spec items
#'
#' @param spec A table definition ([tfl_read_table_spec()], [tfl_table_spec()])
#'   or the path(s) of its workbook(s).
#' @param output_id The table, when the definition has several; `NA` for the
#'   workbook's defaults alone (the rows with no output_id, and the study's
#'   rounding): the study's `defaults.yml` content, with no table_plan() item.
#' @param own With `output_id`: `TRUE` for the table's own rows alone, the
#'   defaults left out -- what its spec file says itself (a value equal to
#'   the default is kept, as it was written).  `FALSE`: the defaults and the
#'   table's rows together, as the plan applies them.
#' @return The items ([tfl_item()]), in the order the plan applies them:
#'   `table_plan()` with the roles first.
#' @keywords internal
#' @noRd
.plan_content_from_sheets <- function(spec, output_id = NULL, own = FALSE) {
  defaults <- length(output_id) == 1L && is.na(output_id)
  sp <- .as_spec(spec, "table", ".plan_content_from_sheets",
                 output_id = if (defaults || isTRUE(own)) NULL else output_id)
  if (defaults || isTRUE(own)) {
    keep <- if (defaults) function(id) is.na(id) else function(id) !is.na(id) & id == output_id
    for (nm in names(.ard_spec_schema())) {
      d <- sp[[nm]]
      if (is.data.frame(d) && "output_id" %in% names(d)) {
        sp[[nm]] <- d[keep(d$output_id), , drop = FALSE]
      }
    }
    # the study's rounding is the defaults', not a table's own
    if (!defaults) sp$study <- sp$study[0L]
    attr(sp, "output_id") <- if (defaults) NULL else output_id
  } else {
    sp <- .ard_spec_scope(sp, output_id)
  }
  items <- .plan_content_items(sp)
  if (defaults && !length(items[[1L]]$args)) items <- items[-1L]
  items
}

# A call as the program writes it: an argument the schema marks
# `print: unnamed` (plan_col_header()'s header) goes without its name, as
# a person writes it -- the value is the same, by position.
.content_print <- function(cl) {
  ent <- tfl_schema(cl$fun)
  if (is.null(ent$args) || !length(cl$args)) return(cl)
  nm <- names(cl$args)
  for (a in names(ent$args)) {
    if (identical(ent$args[[a]]$print, "unnamed") && a %in% nm) nm[nm == a] <- ""
  }
  names(cl$args) <- nm
  cl
}

# The items as the calls they stand for (.spec_call()), their arguments as
# R takes them (.schema_to_r()).
.content_calls <- function(content) {
  lapply(content, function(it) {
    do.call(.spec_call, c(list(.schema_fn_name(it$fn)),
                          .schema_to_r(it$fn, it$args %||% list())))
  })
}

# ---- content -> sheets (the bridge's other way) -----------------------------

# The variables rows' order with each nested one placed after its parent:
# `vars` a named list of rows (`order` from plan_labels, `under` "RACE:
# Asian" from plan_nest); a nested row with no order goes after its parent
# and the parent's children already placed, and the orders are renumbered.
.nest_order <- function(vars) {
  has <- Filter(function(r) !is.null(r$order), vars)
  if (!length(has)) return(vars)
  parent_of <- function(v) {
    u <- vars[[v]]$under
    if (is.null(u)) NA_character_ else trimws(sub(":.*$", "", u))
  }
  seq_v <- names(has)[order(vapply(has, function(r) as.numeric(r$order), 0))]
  for (v in names(vars)) {
    p <- parent_of(v)
    if (!is.null(vars[[v]]$order) || is.na(p)) next
    at <- match(p, seq_v)
    if (is.na(at)) next
    while (at < length(seq_v) && identical(parent_of(seq_v[at + 1L]), p)) at <- at + 1L
    seq_v <- append(seq_v, v, after = at)
  }
  for (i in seq_along(seq_v)) vars[[seq_v[i]]]$order <- i
  vars
}

# A table's content written to the sheets a workbook has -- the inverse of
# .plan_content_items(), verb by verb, in the sheets' own forms (`a | b`,
# `name = value`, a newline as \n).  What a sheet cannot say (plan_after(),
# a second plan_digits(), an expression as a label ...) is listed in
# attr(, "not_converted") and left out, never written approximately.  The
# app reads its grids from this while the workbook form is still its own.
.plan_sheets_from_content <- function(items, output_id = NULL) {
  id <- if (is.null(output_id)) NA_character_ else output_id
  lost <- character()
  miss <- function(fn, what) lost <<- c(lost, sprintf("%s: %s", fn, what))
  bar <- function(x) paste(as.character(x), collapse = " | ")
  # the sheets read raw text (tables, variables, cells, col_header) take a
  # value as it is; a leading or trailing blank is kept by quoting it
  esc <- function(x) {
    x <- as.character(x)
    ifelse(grepl("^\\s|\\s$", x), paste0("\"", x, "\""), x)
  }
  # roles: `name = column`, a constant heading as its quoted text
  refs <- function(x) {
    nm <- names(x) %||% rep("", length(x))
    v <- vapply(seq_along(x), function(i) {
      e <- x[[i]]
      if (inherits(e, "formula")) paste0("\"", eval(e[[2L]], baseenv()), "\"")
      else as.character(e)
    }, "")
    bar(ifelse(nzchar(nm) & nm != v, paste(nm, "=", v), v))
  }
  lgl <- function(x) if (isTRUE(x)) "TRUE" else if (identical(x, FALSE)) "FALSE"
  tb <- list(); study <- list()
  vars <- list(); cells <- list(); lay <- list(); sty <- list()
  labelled <- character()
  cols <- list(); cstyles <- list(); hdr <- list()
  var_row <- function(v) if (is.null(vars[[v]])) vars[[v]] <<- list(variable = v)
  col_row <- function(cn) if (is.null(cols[[cn]])) cols[[cn]] <<- list(column = cn)
  cell <- function(variable = NA_character_, context = NA_character_,
                   row = NA_character_, when = NA_character_, template = NA_character_,
                   digits = NA_character_, signif = NA_character_) {
    cells[[length(cells) + 1L]] <<- list(variable = variable, context = context,
      row = row, when = when, template = template, digits = digits, signif = signif)
  }
  # a chain: templates, guards (cond ~ "text") among them
  chain_rows <- function(v, variable, context = NA_character_, row = NA_character_) {
    els <- if (is.list(v)) v else as.list(v)
    for (e in els) {
      if (inherits(e, "formula") && length(e) == 3L) {
        cell(variable, context, row, paste(deparse(e[[2L]]), collapse = " "), eval(e[[3L]], baseenv()))
      } else if (is.character(e) && length(e) == 1L) {
        cell(variable, context, row, template = e)
      } else {
        return(FALSE)
      }
    }
    TRUE
  }
  # one plan_cells() / plan_compare() entry as `cells` rows of its key
  entry_rows <- function(fn, k, v) {
    parts <- strsplit(k, "\r", fixed = TRUE)[[1L]]
    var <- if (identical(k, "default")) NA_character_ else parts[1L]
    ctx <- if (length(parts) > 1L) parts[2L] else NA_character_
    if (inherits(v, "cell_rows")) {
      v <- unclass(v)
      for (lb in names(v)) {
        if (!chain_rows(v[[lb]], var, ctx, if (nzchar(lb)) lb else NA_character_)) {
          miss(fn, sprintf("%s: a line that is not templates and guards", k)); return()
        }
      }
      return()
    }
    if (is.character(v) && !is.null(names(v)) && all(nzchar(names(v))) &&
        !identical(fn, "plan_compare")) {
      for (lb in names(v)) cell(var, ctx, lb, template = v[[lb]])
      return()
    }
    if (is.list(v) && !is.null(names(v)) && all(nzchar(names(v)))) {
      for (lb in names(v)) {
        if (!chain_rows(v[[lb]], var, ctx, lb)) {
          miss(fn, sprintf("%s: a row that is not templates and guards", k)); return()
        }
      }
      return()
    }
    if (!chain_rows(v, var, ctx)) miss(fn, sprintf("%s: not templates and guards", k))
  }
  # the key columns the plan makes (a stack's pair, the statistics' groups):
  # their values' labels are the report's code list rows
  own <- ".stat_group"
  for (it in items) if (identical(it$fn, "normalize_ard") && length(it$args$stack)) {
    own <- c(own, names(it$args$stack), paste0(names(it$args$stack), "_level"))
  }
  items <- Filter(function(it) !identical(it$fn, "normalize_ard"), items)
  codes <- list()
  seen <- character()
  # the variables' rows as the sheet had them: the code writes
  # plan_levels() in the rows' order and plan_labels() in `order`'s; the
  # recommended order puts plan_levels() first, and plan_levels() is read
  # first wherever a file writes it
  calls <- .content_calls(items)
  fns <- vapply(calls, function(cl) cl$fun, "")
  first <- match("plan_labels", fns)
  if (!is.na(first) && any(fns[-seq_len(first)] == "plan_levels")) {
    lv <- which(fns == "plan_levels" & seq_along(fns) > first)
    calls <- append(calls[-lv], calls[lv], after = first - 1L)
  }
  for (cl in calls) {
    fn <- cl$fun
    a <- cl$args
    nm <- names(a) %||% rep("", length(a))
    pos <- a[!nzchar(nm)]
    named <- a[nzchar(nm)]
    if (fn %in% seen && !fn %in% c("plan_cell_style", "plan_levels", "plan_labels",
                                   "plan_digits")) {
      miss(fn, "a second call (a sheet holds one)"); next
    }
    seen <- c(seen, fn)
    switch(fn,
      table_plan = {
        if (!is.null(named$cols)) tb$cols <- bar(named$cols)
        if (!is.null(named$rows)) tb$rows <- refs(named$rows)
        if ("label" %in% names(named)) {
          lb <- named$label
          tb$label <- if (length(lb) == 1L && !is.list(lb) && is.na(lb)) "NA" else refs(lb)
        }
        if (!is.null(named$show_keys)) tb$show_keys <- bar(named$show_keys)
        if (!is.null(named$stat)) miss(fn, "stat (your statistic columns) stays in code")
      },
      plan_sort = {
        if (length(pos)) {
          v <- unlist(pos, use.names = FALSE)
          tb$sort <- if (is.logical(v)) lgl(v) else bar(v)
        }
        if (!is.null(named$stat)) tb$sort_stat <- named$stat
        if (!is.null(named$keep)) miss(fn, "keep")
      },
      plan_digits = {
        if (!is.null(named$rounding)) study$rounding <- named$rounding
        if (!is.null(named$.rows)) {
          r <- named$.rows
          for (lb in names(r)) {
            v <- as.character(r[[lb]])
            if (grepl("s$", v)) cell(row = lb, signif = sub("s$", "", v))
            else cell(row = lb, digits = v)
          }
        }
        other <- setdiff(names(named), c("rounding", ".rows"))
        if (length(other)) miss(fn, paste0("digits by ", paste(other, collapse = ", "),
                                           " (write them into the templates)"))
      },
      plan_levels = {
        # the variables' order: the variables sheet's `order` (read after
        # the values' levels, whose rows keep the sheet's place)
        vo <- named$variable
        if (is.list(vo) && all(vapply(vo, is.character, NA))) vo <- unlist(vo)
        if (is.character(vo)) named$variable <- NULL else vo <- NULL
        for (v in setdiff(names(named), ".drop_empty")) {
          if (!is.character(named[[v]])) { miss(fn, v); next }
          var_row(v); vars[[v]]$levels <- bar(named[[v]])
        }
        for (v in named$.drop_empty) { var_row(v); vars[[v]]$empty_levels <- "hide" }
        for (k in seq_along(vo)) { var_row(vo[k]); vars[[vo[k]]]$order <- k }
      },
      plan_labels = {
        for (v in names(named)) {
          x <- named[[v]]
          if (is.list(x) && length(x) &&
              all(vapply(x, function(e) is.character(e) && length(e) == 1L, NA))) {
            x <- unlist(x)
          }
          # a value's labels: a column the plan makes, its code list rows; a
          # data variable's are its code list's already (the ARD has them)
          if (is.character(x) && !is.null(names(x)) && all(nzchar(names(x)))) {
            if (!v %in% own) {
              miss(fn, sprintf(paste0("%s: a data variable's value labels go in the ",
                                      "report's code list (the ARD program puts them on the data)"), v))
              next
            }
            for (k in seq_along(x)) {
              codes[[length(codes) + 1L]] <- list(variable = v, value = names(x)[k],
                                                  label = unname(x[k]))
            }
            next
          }
          if (!is.character(x) || length(x) != 1L) {
            miss(fn, sprintf("%s: a value's labels stay in code", v)); next
          }
          var_row(v); vars[[v]]$label <- x
          labelled <- c(labelled, v)
        }
      },
      plan_nest = {
        for (v in names(named)) {
          x <- named[[v]]
          if (is.list(x) && length(x) == 1L && is.character(x[[1L]])) x <- unlist(x)
          if (!is.character(x) || length(x) != 1L || is.null(names(x))) { miss(fn, v); next }
          var_row(v); vars[[v]]$under <- paste0(names(x), ": ", x)
        }
      },
      plan_total = {
        tb$total <- named$label %||% "Total"
        if (!is.null(named$position)) tb$total_position <- named$position
      },
      plan_stat_groups = {
        tb$stat_groups <- paste(vapply(names(named), function(g)
          paste(g, "=", paste(named[[g]], collapse = ", ")), ""), collapse = " | ")
      },
      plan_cells = {
        for (k in c("stats", "value")) if (!is.null(named[[k]])) tb[[k]] <- named[[k]]
        if (!is.null(named$na)) tb$na <- esc(named$na)
        if (!is.null(named$notes)) miss(fn, "notes")
        for (k in setdiff(names(named), c("stats", "value", "na", "notes"))) {
          entry_rows(fn, k, named[[k]])
        }
        if (length(pos)) {
          if (length(pos) == 1L) entry_rows(fn, "default", pos[[1L]])
          else miss(fn, "several unnamed entries")
        }
      },
      plan_filter = {
        w <- vapply(pos, function(e) if (inherits(e, "tfl_spec_expr")) e$text
                    else paste(deparse(e), collapse = " "), "")
        tb$keep_where <- if (length(w) > 1L) paste0("(", w, ")", collapse = " & ") else w
        if (identical(named$any, FALSE)) tb$keep_any <- "FALSE"
        if (identical(named$parents, "drop")) tb$keep_parents <- "drop"
        if (!is.null(named$by)) miss(fn, "by")
      },
      plan_compare = {
        cn <- setdiff(names(named), c("by", "position"))
        tb$compare <- bar(cn)
        if (!is.null(named$by)) tb$compare_by <- named$by
        if (!is.null(named$position)) tb$compare_position <- named$position
        for (k in cn) {
          v <- named[[k]]
          if (is.character(v) && !is.null(names(v))) {
            for (ctx in names(v)) cell(k, if (ctx == "default") NA_character_ else ctx, template = v[[ctx]])
          } else {
            entry_rows(fn, k, v)
          }
        }
      },
      plan_stub = {
        map <- c(vars = "stub_vars", name = "stub_name", indent = "stub_indent",
                 group_summary = "stub_summary", before = "stub_before")
        for (k in intersect(names(named), names(map))) lay[[map[[k]]]] <- named[[k]]
      },
      plan_paginate_group = {
        lay$group_page <- TRUE
        if (!is.null(named$col)) lay$group_col <- named$col
        if (!is.null(named$keep)) lay$group_keep <- named$keep
        if (!is.null(named[["heading"]])) lay$group_heading <- named[["heading"]]
        if (!is.null(named[["heading_at"]])) lay$group_heading_at <- named[["heading_at"]]
      },
      plan_row_group = {
        if (!is.null(named$mode)) lay$group_mode <- named$mode
        if (!is.null(named$collapse)) lay$group_collapse <- named$collapse
        if (!is.null(named$group_col)) miss(fn, "group_col")
      },
      plan_blanks = {
        map <- c(where = "blank_where", first = "blank_first", last = "blank_last",
                 counted = "blank_counted")
        for (k in intersect(names(named), names(map))) lay[[map[[k]]]] <- named[[k]]
      },
      plan_paginate_rows = {
        for (k in names(named)) lay[[paste0("pages_", k)]] <- named[[k]]
      },
      plan_paginate_cols = {
        for (k in names(named)) lay[[paste0("colpages_", k)]] <- named[[k]]
      },
      plan_style = {
        for (k in names(named)) {
          v <- named[[k]]
          if (startsWith(k, "border_")) {
            if (!inherits(v, "tfl_spec_call") || !identical(v$fun, "rtf_border") ||
                !all(vapply(v$args, isTRUE, NA))) {
              miss(fn, sprintf("%s: a border with more than its sides", k)); next
            }
            sty[[k]] <- if (length(v$args)) bar(names(v$args)) else "none"
          } else sty[[k]] <- v
        }
      },
      plan_cell_style = {
        r <- named
        if (!is.null(r$where)) r$where <- if (inherits(r$where, "tfl_spec_formula")) r$where$text
                                         else paste(deparse(r$where), collapse = " ")
        if (!is.null(r$border)) { miss(fn, "border"); r$border <- NULL }
        cstyles[[length(cstyles) + 1L]] <- r
      },
      plan_hide = {
        for (cn in unlist(pos, use.names = FALSE)) { col_row(cn); cols[[cn]]$hide <- TRUE }
      },
      plan_col_header = {
        h <- named$header
        if (!is.null(named$lines)) {
          h <- do.call(rbind, lapply(seq_along(named$lines), function(i) {
            l <- named$lines[[i]]
            data.frame(line = as.character(i), cols = names(l), text = unname(l),
                       stringsAsFactors = FALSE)
          }))
        }
        if (is.data.frame(h)) {
          for (i in seq_len(nrow(h))) {
            r <- as.list(h[i, , drop = FALSE])
            hdr[[length(hdr) + 1L]] <- r
          }
        }
        if (!is.null(named$values)) {
          v <- named$values
          if (all(vapply(v, function(e) is.character(e) && length(e) == 1L, NA))) {
            tb$header_n <- if (identical(names(v), "n")) v$n
                           else paste(paste(names(v), "=", unlist(v)), collapse = " | ")
          } else miss(fn, "values other than page / table")
        }
        if (!is.null(named$col_header_align)) sty$col_header_align <- named$col_header_align
      },
      plan_columns = {
        w <- named$widths
        if (!is.null(w)) {
          if (is.null(names(w))) miss(fn, "widths by position (a sheet names its columns)")
          else for (cn in names(w)) { col_row(cn); cols[[cn]]$rel_width <- w[[cn]] }
        }
        for (cn in named$decimal) { col_row(cn); cols[[cn]]$decimal_split <- TRUE }
        rt <- named$row_title
        if (is.numeric(rt)) miss(fn, "row_title by position")
        else for (cn in rt) { col_row(cn); cols[[cn]]$row_title <- TRUE }
        if (!is.null(named$auto_width)) sty$auto_width <- named$auto_width
        if (!is.null(named$sep)) tb$sep <- named$sep
      },
      miss(fn, "a verb no sheet holds"))
  }
  # one sheet's rows as text, the way a workbook holds them
  txt <- function(v) {
    if (is.null(v)) return(NA_character_)
    if (is.logical(v)) return(lgl(v) %||% NA_character_)
    if (length(v) > 1L) return(bar(v))
    as.character(v)
  }
  frame <- function(rows) {
    if (!length(rows)) return(NULL)
    keys <- unique(unlist(lapply(rows, names)))
    df <- as.data.frame(lapply(stats::setNames(keys, keys), function(k)
      vapply(rows, function(r) txt(r[[k]]), "")), stringsAsFactors = FALSE)
    cbind(output_id = rep(id, nrow(df)), df, stringsAsFactors = FALSE)
  }
  # with no `order` (no plan_levels(variable = )), the labelled variables'
  # rows in plan_labels()'s order, so the labels come back as written
  if (!any(vapply(vars, function(r) !is.null(r$order), NA))) {
    at <- which(names(vars) %in% labelled)
    vars[at] <- vars[unique(labelled)]
    names(vars)[at] <- unique(labelled)
  }
  # a nested variable (plan_nest) with no heading of its own: its place is
  # right after the variable it goes under (and that one's children before it)
  vars <- .nest_order(vars)
  sheets <- list(
    tables = frame(list(lapply(tb, txt))),
    variables = frame(unname(vars)),
    cells = frame(cells),
    layout = if (length(lay)) frame(list(lay)),
    style = if (length(sty)) frame(list(sty)),
    columns = frame(unname(cols)),
    cell_styles = frame(cstyles),
    col_header = frame(hdr),
    study = if (length(study)) unlist(study))
  sp <- do.call(tfl_table_spec, Filter(Negate(is.null), sheets))
  if (length(codes)) {
    ord <- stats::ave(seq_along(codes), vapply(codes, `[[`, "", "variable"), FUN = seq_along)
    for (i in seq_along(codes)) codes[[i]]$order <- ord[i]
    sp$codelists <- frame(codes)
  }
  attr(sp, "not_converted") <- lost
  sp
}

#' Make a report's content, from its spec file, without its code
#'
#' Runs what [tfl_content_code()] writes: the same items, in the same order,
#' each applied with `do.call()` -- for a table, the data's steps
#' (`normalize_ard()`), `table_plan()` and its layers.  The two read one
#' list of calls, so running the written code gives the same object; it is
#' the quick way for a preview, the app or an assistant's check.  A report
#' whose content is its own code (`content: !r`) runs it, after the
#' data's steps, and gives what it leaves in `content`.
#'
#' @param output A report ([tfl_output()]), resolved.
#' @param data The data the content starts from: the ARD (or the normalized
#'   ARD when the content has no `normalize_ard()` item).  With
#'   `data.steps` (R that makes `data` from the output's `data.object`,
#'   `ard` by default), the object the steps start from.  For a figure, a
#'   named list of the objects its code reads: the datasets by their names
#'   in lower case (`adtte`), and `ard`.
#' @return For a table, the [rtfreporter::table_plan()]; for a figure, the
#'   ggplot.
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   ard <- cards::ard_stack(cards::ADSL, .by = ARM,
#'                           cards::ard_tabulate(variables = SEX))
#'   out <- tfl_output("T-1", "table", content = list(
#'     tfl_item("normalize_ard", list()),
#'     tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable"))),
#'     tfl_item("plan_cells", list(categorical = "{n} ({p:.1f%})"))))
#'   p <- tfl_content_run(out, ard)
#'   class(p)
#' }
#' @seealso [tfl_content_code()]
#' @export
tfl_content_run <- function(output, data) {
  if (!inherits(output, "tfl_output")) {
    .ard_stop("tfl_content_run(): `output` is a report, tfl_output().")
  }
  if (inherits(output$content, "tfl_r")) return(.content_own_run(output, data))
  if (identical(output$kind, "figure")) return(.content_fig_run(output, data))
  if (!output$kind %in% c("table", "listing")) {
    .ard_stop(sprintf(paste0("tfl_content_run(): a %s's content is not run ",
                             "yet; tables, listings and figures, for now."), output$kind))
  }
  .spec_need_rtfreporter()
  for (it in output$content) .schema_need_version(it)
  # plan_levels() the ARD already says: left out, as the program does
  if (identical(output$kind, "table")) output$content <- .levels_prune(output$content, output)
  steps <- .content_r_steps(output)
  if (length(steps)) {
    # the steps run as the program runs them: the output's object bound
    # to `data`, and what they leave in `data` is the plan's data
    env <- new.env(parent = globalenv())
    assign(output$data$object %||% "ard", data, envir = env)
    eval(parse(text = steps), env)
    if (!exists("data", envir = env, inherits = FALSE)) {
      .ard_stop("tfl_content_run(): the data.steps leave no `data`, the data the plan is made from.")
    }
    data <- get("data", envir = env, inherits = FALSE)
    return(.content_table_run(.content_drop_data_steps(output$content), data,
                              kind = output$kind))
  }
  .content_table_run(output$content, data, kind = output$kind)
}

# the content as the report's own code, run as the program runs it: the
# output's object bound, the data's steps, then the code; what it leaves
# in `content` is the content
.content_own_run <- function(output, data) {
  env <- new.env(parent = globalenv())
  if (identical(output$kind, "figure") && is.list(data) && !is.data.frame(data) &&
      !is.null(names(data))) {
    # a figure's own code reads the datasets by their names (adsl, adae),
    # after the study's figure setup, as its program does
    for (n in names(data)) assign(n, data[[n]], envir = env)
    eval(parse(text = tfl_fig_setup_code()), env)
    if (!is.null(data$ard)) eval(parse(text = tfl_helpers_code()), env)
  } else {
    assign(output$data$object %||% "ard", data, envir = env)
  }
  steps <- .content_r_steps(output)
  if (length(steps)) eval(parse(text = steps), env)
  eval(parse(text = as.character(output$content)), env)
  if (!exists("content", envir = env, inherits = FALSE)) {
    .ard_stop("tfl_content_run(): the report's own code leaves no `content`.")
  }
  get("content", envir = env, inherits = FALSE)
}

.content_table_run <- function(content, data, kind = "table") {
  pl <- .content_table_pipeline(content, kind)
  x <- data
  run <- function(cl, first) {
    f <- .spec_fun(cl$fun)
    do.call(f, c(list(first), lapply(cl$args, .spec_eval, env = emptyenv())))
  }
  for (cl in pl$pre) x <- run(cl, x)
  p <- run(pl$head, x)
  for (cl in pl$steps) p <- run(cl, p)
  p
}

#' The code of a report's content, from its spec file
#'
#' Writes the program a report's `content:` stands for -- for a table, the
#' `table_plan()` pipeline: one line an item, in the items' order, with
#' their arguments as R takes them (`!r` as code, `{fn:, args:}` as the call
#' it names, maps and sequences as the vectors and lists each argument
#' takes; [tfl_schema()]).  An item the report takes from the study's
#' defaults or a pattern says so in a comment above it.  The output is the
#' report after its `extends` are applied (`tfl_resolve_output()`), so no
#' `!unset` is left.
#'
#' A table's `plan_levels()` that says what its ARD already says is left
#' out, here and in [tfl_content_run()]: a column's order that is the
#' report's code list (applied when the ARD is made, a factor cards keeps
#' in its order), and the variables in the order of the ARD's analyses.
#' An order of the report's own, a column no analysis reads, a column an
#' analysis that is not cards' reads (cardx's survfit names the arms from
#' the strata) and a report whose data steps make its data keep theirs.
#'
#' A report whose content is its own code (`content: !r`, any kind: the
#' code leaves `content`) is that code, after the data's steps.  A figure's
#' content is its layers, as the figure designer has them
#' ([tfl_fig_design_code()] of the design the report stands for: its
#' `data` steps and statistics, the layers, its code lists): the figure
#' in `content`, not saved.
#'
#' @param output A report ([tfl_output()]), resolved.
#' @param data The name of the object the pipe starts from; `NULL`: the
#'   output's `data$object`, else `ard` when the content normalizes it
#'   (a `normalize_ard()` item), else `data`.  With `data.steps` (R that
#'   makes `data` from the output's object, the old process_code), the
#'   steps come first, as they are written, and the pipe starts from `data`.
#' @param plan The name the plan is assigned to.
#' @param pipe `"|>"` or `"%>%"`; `NULL` follows
#'   `getOption("rtfreporter.ard_pipe")`.
#' @param template For a resolved template report (`template: {name,
#'   args}`, [tfl_resolve_output()]): `"call"` (default) writes the
#'   template's call, `plan <- tpl_<name>(ard, ...)` with the arguments its
#'   SPEC gives and the study's defaults it took, then the study's and the
#'   report's own lines after it, each marked; `"items"` writes the items
#'   it stands for, as for any report (and so does `"call"`, with a message,
#'   when the call cannot say the report's own table roles, data or
#'   normalization).
#' @return The code, one element a line.
#' @examples
#' out <- tfl_output("T-1", "table", content = list(
#'   tfl_item("table_plan", list(cols = "TRTA", rows = list(group = "variable"))),
#'   tfl_item("plan_cells", list(categorical = "{n} ({p:.1f%})")),
#'   tfl_item("plan_sort", list(keys = c("-n")), from = "defaults")))
#' cat(tfl_content_code(out), sep = "\n")
#' @seealso [tfl_table_code()] for a table workbook; [tfl_schema()].
#' @export
tfl_content_code <- function(output, data = NULL, plan = "plan", pipe = NULL,
                             template = c("call", "items")) {
  if (!inherits(output, "tfl_output")) {
    .ard_stop("tfl_content_code(): `output` is a report, tfl_output().")
  }
  # the content as the report's own code (`content: !r`, the old
  # content_code): written as it is, after the data's steps; it leaves
  # `content`
  if (inherits(output$content, "tfl_r")) {
    steps <- .content_r_steps(output)
    own <- unlist(strsplit(as.character(output$content), "\n", fixed = TRUE))
    return(c(steps, if (length(steps)) "", own))
  }
  # a figure: its layers, as the figure designer writes them
  if (identical(output$kind, "figure")) return(.content_fig_code(output))
  if (!output$kind %in% c("table", "listing")) {
    .ard_stop(sprintf(paste0("tfl_content_code(): a %s's content is not written ",
                             "yet; tables, listings and figures, for now."), output$kind))
  }
  for (it in output$content) .schema_need_version(it)
  # plan_levels() the ARD already says (a code list applied when the ARD is
  # made, the variables in its order): not written (R/spec_levels.R)
  if (identical(output$kind, "table")) output$content <- .levels_prune(output$content, output)
  # a template SPEC (resolved): the template's call and the lines after it
  # (the A use); `template = "items"` writes the items it stands for (B)
  tp <- attr(output, "template")
  if (!is.null(tp) && identical(match.arg(template), "call") && identical(output$kind, "table")) {
    if (isTRUE(tp$ok)) {
      return(.content_tpl_code(output, data = data %||% output$data$object %||% "ard",
                               plan = plan, pipe = pipe))
    }
    message(sprintf(paste0("%s: the template's call cannot say this report's own data, table ",
                           "roles or normalize_ard() arguments; its items are written."), output$id))
  }
  # the object the pipe starts from: the output's own name for it, else
  # the ARD when the content normalizes it, else the normalized data
  # the data's steps (`data.steps`, the old process_code): R that makes
  # `data` from the output's object, written as it is, before the plan
  steps <- .content_r_steps(output)
  if (length(steps)) {
    # the steps make the data: no normalize_ard() of the defaults after them
    return(c(steps, "", .content_table_code(.content_drop_data_steps(output$content),
                                            data = data %||% "data",
                                            plan = plan, pipe = pipe, kind = output$kind)))
  }
  if (is.null(data)) {
    pre <- any(vapply(output$content, function(it)
      .schema_fn_name(it$fn) %in% .content_data_steps, NA))
    data <- output$data$object %||% if (pre) "ard" else "data"
  }
  .content_table_code(output$content, data = data, plan = plan, pipe = pipe,
                      kind = output$kind)
}

# A table's or a listing's data steps as code, one element a line: each
# `!r` step of `data.steps` as it is written.  A step that is not R (a
# figure's step map) is not a table's.
.content_r_steps <- function(output) {
  # the data step as the report's own code (`data: {source: code, code:
  # !r}`, the old data_code): that code is the step, whatever else data says
  if (identical(output$data$source, "code") && !is.null(output$data$code)) {
    return(unlist(strsplit(as.character(output$data$code), "\n", fixed = TRUE)))
  }
  st <- output$data$steps
  if (is.null(st) || !length(st)) return(character())
  if (inherits(st, "tfl_r") || is.character(st)) st <- as.list(st)
  bad <- !vapply(st, function(x) is.character(x) && length(x) == 1L, NA)
  if (any(bad)) {
    .ard_stop(sprintf(paste0("A %s's data.steps are R (`!r`); step %s is not."),
                      output$kind %||% "table", paste(which(bad), collapse = ", ")))
  }
  unlist(strsplit(vapply(st, as.character, ""), "\n", fixed = TRUE))
}

#' The line that normalizes a table's ARD
#'
#' A table's program makes `data` from its rows of the study ARD with
#' [rtfreporter::normalize_ard()] before the plan.  What the normalization
#' needs to know is the report's, so it is written from its definitions:
#' the nested variables of its `hierarchical` analysis (the ARD
#' definition's `variables`, `AEBODSYS | AEDECOD`) as its `hierarchy` --
#' each level's own row, SOC and PT, with its counts -- and the row of the
#' hierarchy's overall counts (the table definition's
#' `tables$overall_label`, "Subjects with at least one TEAE", from the
#' analysis's `args` `over_variables = TRUE`) as its `overall`; and the
#' parallel subgroups stacked into one pair of keys (`tables$stack`,
#' `subgroup = ALL, SEX, AGEGR1`) as its `stack`.  A report with none:
#' `data <- normalize_ard(ard)`.
#'
#' @param spec A table definition ([tfl_read_table_spec()]), or the path(s)
#'   of its workbook(s).
#' @param ard_spec The ARD definition ([tfl_read_ard_spec()]), for the
#'   hierarchy; `NULL` for none.
#' @param output_id The table, when the definitions have several.
#' @param ard,data The names of the ARD and of the normalized data.
#' @return The line, one string.
#' @examples
#' sp <- tfl_table_spec(tables = data.frame(
#'   output_id = "T-2", cols = "TRTA", rows = "group1 = AEBODSYS",
#'   overall_label = "Subjects with at least one TEAE"))
#' ard <- tfl_ard_spec(list(
#'   datasets = data.frame(dataset = "ADAE", path = "adae.rds"),
#'   analyses = data.frame(output_id = "T-2", analysis_id = "AE",
#'                         method = "hierarchical", dataset = "ADAE", by = "TRTA",
#'                         variables = "AEBODSYS | AEDECOD",
#'                         args = "over_variables = TRUE")))
#' tfl_normalize_code(sp, ard, "T-2")
#' @export
tfl_normalize_code <- function(spec, ard_spec = NULL, output_id = NULL,
                               ard = "ard", data = "data") {
  sp <- .ard_spec_scope(.as_spec(spec, "table", "tfl_normalize_code",
                                 output_id = output_id), output_id)
  a <- .normalize_spec_args(sp, ard_spec, output_id %||% attr(sp, "output_id"),
                            "tfl_normalize_code")
  layers_code <- function(v) {
    if (!is.list(v)) return(vec_code(v))
    paste0("list(", paste(vapply(v, function(p)
      if (length(p) == 1L) encodeString(p, quote = "\"") else vec_code(p), ""),
      collapse = ", "), ")")
  }
  args <- c(ard,
            if (!is.null(a$hierarchy)) paste("hierarchy =", vec_code(a$hierarchy)),
            if (!is.null(a$overall)) paste("overall =", encodeString(a$overall, quote = "\"")),
            if (!is.null(a$stack)) paste0("stack = list(", paste(
              sprintf("%s = %s", names(a$stack), vapply(a$stack, layers_code, "")),
              collapse = ", "), ")"),
            if (!is.null(a$stack_sep))
              paste("stack_sep =", encodeString(a$stack_sep, quote = "\"")),
            if (isTRUE(a$stack_nest)) "stack_nest = TRUE")
  sprintf("%s <- normalize_ard(%s)", data, paste(args, collapse = ", "))
}

# A table's normalize_ard() arguments from its definition: its tables row
# (overall_label, stack, stack_sep, stack_nest) and its hierarchical
# analysis in the ARD definition (hierarchy), as R values; none given, an
# empty list (normalize_ard() with its defaults).  `guess`: with no ARD
# definition for the table, the hierarchy its rows and label say, as the
# spec files' converter takes it (.conv_normalize_args()).
.normalize_spec_args <- function(sp, ard_spec, id, fn, guess = FALSE) {
  t <- sp$tables
  lab <- if (nrow(t) && "overall_label" %in% names(t)) t$overall_label[1L] else NA
  lab <- if (!is.na(lab) && nzchar(trimws(lab))) trimws(lab)
  stk <- if (nrow(t) && "stack" %in% names(t)) t$stack[1L] else NA
  stk <- if (!is.na(stk) && nzchar(trimws(stk)))
    .ard_spec_stat_groups(stk, "stack", "layer", "subgroup = ALL, SEX, AGEGR1, AGEGR1 * SEX")
  # a layer `AGEGR1 * SEX` is the variables crossed: then the layers are a list
  if (!is.null(stk)) stk <- lapply(stk, function(v) {
    parts <- lapply(strsplit(v, "*", fixed = TRUE), trimws)
    if (all(lengths(parts) == 1L)) unlist(parts) else parts
  })
  sep <- if (nrow(t) && "stack_sep" %in% names(t)) t$stack_sep[1L] else NA
  sep <- if (!is.na(sep) && nzchar(sep)) .ard_spec_unquote(sep)
  nest <- nrow(t) && "stack_nest" %in% names(t) &&
    toupper(trimws(as.character(t$stack_nest[1L]))) %in% "TRUE"
  hier <- character()
  if (!is.null(ard_spec)) {
    an <- ard_spec$analyses
    an <- an[!is.na(an$output_id) & an$output_id %in% id &
               an$method %in% c("hierarchical", "cards::ard_stack_hierarchical",
                                "cards::ard_stack_hierarchical_count"), , drop = FALSE]
    if (nrow(an)) hier <- .split_bar(an$variables[1L])
  }
  if (guess && !length(hier) && length(id) == 1L) {
    g <- .conv_normalize_args(sp, ard_spec, id)$hierarchy
    if (length(g)) hier <- g
  }
  if (!is.null(lab) && !length(hier)) {
    .ard_stop(sprintf(paste0(
      "%s(): %s has an `overall_label` (%s) but no ",
      "hierarchical analysis%s: the overall row is a hierarchy's."),
      fn, id %||% "the table", sQuote(lab),
      if (is.null(ard_spec)) " (give `ard_spec`)" else " in the ARD definition"))
  }
  out <- list()
  if (length(hier)) out$hierarchy <- hier
  if (!is.null(lab)) out$overall <- lab
  if (!is.null(stk)) {
    out$stack <- stk
    if (!is.null(sep)) out$stack_sep <- sep
    if (isTRUE(nest)) out$stack_nest <- TRUE
  }
  out
}

#' A table's plan from its definition
#'
#' Builds the [rtfreporter::table_plan()] a table definition stands for: the roles of its
#' `tables` sheet go into `table_plan()`, and the other sheets become the
#' plan's first layers through the same verbs [tfl_table_code()] writes ---
#' so a verb written afterwards still wins, which is how one report departs
#' from the study's workbook in a line of code.  Like `table_plan()`, the data
#' comes first, so it pipes.
#'
#' @param data The normalized ARD, as [rtfreporter::table_plan()] takes it.
#' @param spec A table definition ([tfl_read_table_spec()],
#'   [tfl_table_spec()]), or the path(s) of its workbook(s).
#' @param output_id The table, when the definition has several.
#' @param ... Roles for [rtfreporter::table_plan()] (`cols`, `rows`, `label`,
#'   `stat`); one given here wins over the workbook's.  Everything else is
#'   a layer: add its verb after this call.
#' @return An [rtfreporter::table_plan()].
#' @seealso [tfl_table_code()], the same steps as code; [tfl_as_table_spec()],
#'   the other way round.
#' @examples
#' if (requireNamespace("cards", quietly = TRUE) &&
#'     requireNamespace("rtfreporter", quietly = TRUE)) {
#'   spec <- tfl_read_table_spec(system.file("extdata", "ard-spec", "DM.xlsx",
#'                                           package = "tflspec"))
#'   ard <- cards::ard_stack(adsl[adsl$SAFFL == "Y", ], .by = TRT01A,
#'                           cards::ard_continuous(variables = AGE),
#'                           cards::ard_categorical(variables = c(AGEGR1, SEX)))
#'   plan <- tfl_table_plan(rtfreporter::normalize_ard(ard), spec)
#'   # this report's own change, under what the spec says
#'   plan <- rtfreporter::plan_paginate_rows(plan, max_rows = 40)
#'   head(rtfreporter::plan_apply(plan, "table"))
#' }
#' @export
tfl_table_plan <- function(data, spec, output_id = NULL, ...) {
  .spec_need_rtfreporter()
  sp <- .ard_spec_scope(.as_spec(spec, "table", "tfl_table_plan",
                                 output_id = output_id), output_id)
  roles <- .plan_spec_roles(sp)
  dots <- list(...)
  other <- setdiff(names(dots) %||% rep("", length(dots)), .plan_role_names)
  if (length(other)) {
    .ard_stop(sprintf(paste0(
      "tfl_table_plan(): %s %s not a role.  The roles are %s; anything ",
      "else is a layer:
  pipe the plan into its verb, e.g. ",
      "plan_cells(stats = \"rows\", notes = FALSE)."),
      paste(sQuote(other), collapse = ", "),
      if (length(other) > 1L) "are" else "is",
      paste(.plan_role_names, collapse = ", ")))
  }
  for (r in names(dots)) if (!is.null(dots[[r]])) roles[[r]] <- dots[[r]]
  p <- do.call(rtfreporter::table_plan, c(list(data), roles))
  items <- .plan_content_items(sp)[-1L]
  for (st in .content_calls(items)) {
    f <- .spec_fun(st$fun)
    p <- do.call(f, c(list(p), lapply(st$args, .spec_eval, env = emptyenv())))
  }
  p
}

# ---- reports ---------------------------------------------------------------

# The rtfreporter calls a report definition stands for, as `doc <- ...`
# steps: what tfl_report() runs and what tfl_report_code() writes.
# `content` is the name of the report's content in the program.
.report_spec_steps <- function(sp, content = "content", setup = FALSE) {
  r <- .ard_spec_report_row(sp)
  pg <- if (nrow(sp$page)) .ard_spec_typed(sp$page[1L, ], "page") else list()
  # (all of it for a figure's size: its font's lines too)
  pg_all <- pg
  # the study's font and size: the setup's options() say them
  if (setup) pg[attr(sp, "page_study")] <- NULL
  geo <- c("paper_size", "orientation", "width_in", "height_in",
           "margin_top_in", "margin_bottom_in", "margin_left_in",
           "margin_right_in", "header_dist_in", "footer_dist_in")
  page <- pg[intersect(geo, names(pg))]
  fmt <- pg[intersect(c("font_size_half_points", "title_format",
                        "footnote_format", "title_width", "footnote_width",
                        "markup"), names(pg))]
  for (w in intersect(c("title_width", "footnote_width"), names(fmt))) {
    v <- suppressWarnings(as.numeric(fmt[[w]]))
    if (!is.na(v)) fmt[[w]] <- v
  }
  dir <- .ard_spec_study_value(sp, "program_dir")
  in_dir <- function(p) {
    if (is.null(p) || is.na(dir)) return(p)
    sep <- if (grepl("\\", dir, fixed = TRUE)) "\\" else "/"
    paste(sub("[\\\\/]+$", "", dir), p, sep = sep)
  }
  # a program written in the sheet is said; with none, the report's ID is
  # rtfreporter's last resort, after the file name of the program that runs
  prog <- in_dir(r[["program"]])
  fallback <- in_dir(r[["program_fallback"]])
  doc <- .spec_sym("doc")
  tokens <- .ard_spec_tokens(sp, r)
  # the study's tokens left to options(rtfreporter.tokens = ): the
  # document's own then (rtfreporter fills from both, the document's first)
  doc_tokens <- if (setup) .ard_spec_tokens(sp, r, study = FALSE) else tokens
  # the RTF file the document is written to (tfl_report_path())
  file <- .rtf_file_said(.sheet_rtf_path(sp))
  st <- list(.spec_call("rtf_document",
                        font_table = if (!is.null(pg[["font"]])) list(list(name = pg[["font"]])),
                        page = if (length(page)) page,
                        default_format = if (length(fmt))
                          do.call(.spec_call, c(list("rtf_default_format"), fmt)),
                        watermark = r$watermark,
                        program = prog,
                        program_fallback = fallback,
                        tokens = doc_tokens,
                        file = file))
  # a report may go without the study's running header or footer -- one
  # that puts its run line under the table instead, say
  band <- function(sheet) {
    if (identical(r[[paste0("page_", sheet)]], FALSE)) return(NULL)
    # the study's, defined once by the setup code: the report says its name
    # (rtfreporter leaves out a line its tokens leave empty), unless the
    # report has its own
    if (setup && !isTRUE(attr(sp, "bands_own")[[sheet]]) &&
        length(.ard_spec_band(sp, sheet))) {
      return(.spec_sym(.setup_band_names[[sheet]]))
    }
    b <- .ard_spec_band(sp, sheet, tokens)
    if (!length(b)) return(NULL)
    # its own lines with the study's, the study's by the name the setup
    # gives them (study_footer_lines): a change of the study's lines reaches
    # the report -- when they are the study's lines as the setup writes them
    if (setup) {
      rows <- attr(sp, "bands_study")[[sheet]]
      st <- stats::setNames(list(rows), sheet)
      sb <- if (NROW(rows)) .ard_spec_band(st, sheet, tokens)
      n <- length(sb)
      if (n && length(b) > n && identical(sb, .ard_spec_band(st, sheet))) {
        lines <- .spec_sym(paste0(.setup_band_names[[sheet]], "_lines"))
        if (identical(utils::tail(b, n), sb)) {
          return(.spec_call(paste0("rtf_", sheet),
                            .spec_call("c", utils::head(b, -n), lines)))
        }
        if (identical(utils::head(b, n), sb)) {
          return(.spec_call(paste0("rtf_", sheet),
                            .spec_call("c", lines, utils::tail(b, -n))))
        }
      }
    }
    .spec_call(paste0("rtf_", sheet), b)
  }
  hdr <- band("header")
  ftr <- band("footer")
  if (length(hdr) || length(ftr)) {
    st[[length(st) + 1L]] <- .spec_call("rtf_section", doc, header = hdr,
                                        footer = ftr)
  }
  if (identical(r$type %||% "table", "figure")) {
    # its size on the page (tfl_fig_page_size()): a number or the page's
    # body (fit) said; blank, the figure's own size (rtfreporter's)
    twips <- function(v) as.integer(round(v * 1440))
    lines <- length(.ard_spec_band(sp, "titles")) + length(.ard_spec_band(sp, "footnotes"))
    sz <- .fig_page_size(r, pg_all, lines)
    st[[length(st) + 1L]] <- .spec_call("rtf_figures", doc, .spec_sym(content),
      width_twips = if (sz$mode[["width"]] != "native") twips(sz$width_in),
      height_twips = if (sz$mode[["height"]] != "native") twips(sz$height_in))
  } else {
    # (a table's font size is its plan's, plan_style(): .plan_spec_steps();
    # a listing's pages are made apart, so it is still said here)
    st[[length(st) + 1L]] <- .spec_call("rtf_tables", doc, .spec_sym(content),
      auto_section = r$auto_section,
      section_label_align = r$section_align,
      auto_title = r$auto_title,
      title_label_align = r$title_align,
      font_size_half_points = if (identical(r$type %||% "table", "listing"))
        r$table_font_size_half_points)
  }
  tt <- .ard_spec_band(sp, "titles", tokens)
  if (length(tt)) {
    st[[length(st) + 1L]] <- .spec_call("rtf_titles", doc, list(tt),
      font_size_half_points = r$title_font_size_half_points)
  }
  fn <- .ard_spec_band(sp, "footnotes", tokens)
  if (length(fn)) {
    st[[length(st) + 1L]] <- .spec_call("rtf_footnotes", doc, list(fn),
      font_size_half_points = r$footnote_font_size_half_points)
  }
  st
}

# the names the setup code gives the study's header and footer
.setup_band_names <- c(header = "study_header", footer = "study_footer")

#' The study's setup code for its report programs
#'
#' What every report of a study shares, as code written once: the study's
#' font and size (the page sheet's default row: `font`,
#' `font_size_half_points`) as `options(rtfreporter.font = ,
#' rtfreporter.font_size_half_points = )`, its
#' tokens (the tokens sheet's default rows, a blank `output_id`:
#' `COMPANY`, `STUDY_ID` ...) as
#' `options(rtfreporter.tokens = list(...))`, and its running header and
#' footer (the header and footer sheets' default rows) as
#' `study_header <- rtf_header(..., drop_empty_rows = TRUE)` and
#' `study_footer <- rtf_footer(...)` (a line a report's tokens leave empty
#' is left out of that report's file).
#' A study's report programs source it and are written with
#' `tfl_report_code(setup = TRUE)`: each then says only its own tokens
#' (`OUTPUT_LABEL`, `OUTPUT_TITLE` ...), and the study's values are in one
#' place, in the definition and in the code.
#'
#' For a study of the YAML spec, give the study ([tfl_read_study_spec()]):
#' its defaults' `page:` (`spec/defaults.yml`) are the study's --
#' `rtf_document: font_table` (the first font's name) and
#' `rtf_default_format: font_size_half_points`, `tokens`, `header` and
#' `footer` -- and the lines are the ones
#' `tfl_report_code(output, setup = TRUE, study = )` refers to.
#'
#' @param spec A report definition (the study's default rows: one with
#'   several reports serves), or a study's spec ([tfl_read_study_spec()]).
#' @return The code, one element per line; none when the study has no
#'   tokens, header or footer.
#' @examples
#' sp <- tfl_table_spec(
#'   header = data.frame(output_id = NA, line = c("1", "2"),
#'                       left = c("{COMPANY}", "PROTOCOL: {STUDY_ID}"),
#'                       right = c(NA, "Page {PAGE} of {TOTAL_PAGES}")),
#'   tokens = data.frame(output_id = NA, name = c("COMPANY", "STUDY_ID"),
#'                       value = c("Sample Pharma", "ABC-123")))
#' cat(tfl_report_setup_code(sp), sep = "\n")
#' @export
tfl_report_setup_code <- function(spec) {
  if (inherits(spec, "tfl_study_spec")) return(.study_setup_code(spec))
  sp <- .as_spec(spec, "table", "tfl_report_setup_code")
  study <- function(d) if (!is.null(d)) d[is.na(d$output_id), , drop = FALSE]
  out <- character()
  # the company's font and size, for every report
  pg <- study(sp$page)
  if (!is.null(pg) && nrow(pg)) {
    pg <- .ard_spec_typed(pg[1L, ], "page")[names(.page_options)]
    pg <- pg[!vapply(pg, is.null, NA)]
    if (length(pg)) {
      names(pg) <- .page_options[names(pg)]
      out <- c(out, .spec_call_code(do.call(.spec_call, c(list("options"), pg)), 0L))
    }
  }
  d <- study(sp$tokens)
  if (!is.null(d) && nrow(d)) {
    v <- ifelse(is.na(d$value), "", d$value)
    keep <- !trimws(v) %in% .ard_spec_omit
    if (any(keep)) {
      tok <- stats::setNames(as.list(v[keep]), trimws(d$name[keep]))
      out <- c(out, .spec_call_code(.spec_call("options", rtfreporter.tokens = tok), 0L))
    }
  }
  for (sheet in names(.setup_band_names)) {
    one <- sp
    one[[sheet]] <- study(sp[[sheet]])
    b <- .ard_spec_band(one, sheet)
    if (!length(b)) next
    # a line of tokens of one's own goes when a report's are empty
    own <- setdiff(.ard_spec_used_tokens(one[sheet]), .ard_spec_rtf_tokens)
    nm <- .setup_band_names[[sheet]]
    lines <- paste0(nm, "_lines")
    call <- .spec_call(paste0("rtf_", sheet), .spec_sym(lines),
                       drop_empty_rows = if (length(own)) TRUE)
    out <- c(out,
             paste(lines, "<-", .spec_call_code(b, 0L, lead = nchar(lines) + 4L)),
             paste(nm, "<-", .spec_call_code(call, 0L, lead = nchar(nm) + 4L)))
  }
  if (!length(out)) return(character())
  .drop_attached_ns(unlist(strsplit(out, "\n", fixed = TRUE)))
}

#' A report's tokens
#'
#' The values a report's header, footer, titles and footnotes fill their
#' `{TOKENS}` with -- the study's, the report's own rows, and the report's
#' own tokens it says (`OUTPUT_LABEL` "Table 14.1.1" ...) -- as
#' [tfl_report_code()] gives them to `rtf_document(tokens = )`: for a
#' preview of the page.
#'
#' @param spec A report definition ([tfl_read_report_spec()]) narrowed to one
#'   report, or the path(s) to read it from; or a report of the YAML spec
#'   ([tfl_output()]), as read or resolved.
#' @param study For a YAML report as read: the study
#'   ([tfl_read_study_spec()]), its defaults and patterns.
#' @inheritParams tfl_report
#' @return A named character vector (none: empty).
#' @examples
#' sp <- tfl_table_spec(
#'   report = data.frame(output_id = "T-14-1-1", type = "table"),
#'   header = data.frame(output_id = NA, line = "1", center = "{OUTPUT_LABEL}"))
#' tfl_report_tokens(sp, "T-14-1-1")
#' @export
tfl_report_tokens <- function(spec, output_id = NULL, study = NULL) {
  if (inherits(spec, "tfl_output")) return(.output_tokens(spec, study))
  sp <- .ard_spec_scope(.as_spec(spec, "table", "tfl_report_tokens",
                                 output_id = output_id), output_id)
  tk <- .ard_spec_tokens(sp, .ard_spec_report_row(sp))
  if (is.null(tk)) return(stats::setNames(character(), character()))
  vapply(tk, as.character, "")
}

#' The code of a report's document, from its definition
#'
#' Writes the rtfreporter calls a report definition stands for -- the
#' page, the running header and footer, the content, the titles and
#' footnotes -- as `doc <- rtf_document(...)` and one `doc <- rtf_*(doc, ...)`
#' a step: what [tfl_report()] does, as a program that needs only
#' rtfreporter.  The document says its file, `rtf_document(file = )` --
#' [tfl_report_path()]'s path -- so `generate_rtfreport(doc)` writes it.
#'
#' @inheritParams tfl_report
#' @param content The name of the report's content in the program: a plan
#'   ([tfl_table_code()]), `rtftable` pages ([tfl_listing_code()]) or the
#'   figures.
#' @param doc The name the document is assigned to.
#' @param setup `TRUE`: the program runs after the study's setup code
#'   ([tfl_report_setup_code()]), so it leaves out what that defines: the
#'   study's tokens (rtfreporter fills from the document's first, then
#'   `options(rtfreporter.tokens = )`), and the study's header and footer,
#'   said by name (`study_header`, `study_footer`; a line the report's
#'   tokens leave empty, a blank `<{OUTPUT_POPULATION}>`, is left out when
#'   the file is written) -- unless the report has its own, written here
#'   as before.
#'   `FALSE` (default): the program stands alone.  For a YAML report, the
#'   study's are its defaults (`spec/defaults.yml`), the report's its
#'   pattern and its own: give the report as read ([tfl_read_output()]) and
#'   its `study`.
#' @return The code, one element per line.
#' @examples
#' spec <- tfl_read_report_spec(
#'   system.file("extdata", "ard-spec", c("report.xlsx", "study.xlsx"),
#'               package = "tflspec"), output_id = "DM")
#' cat(tfl_report_code(spec, content = "plan"), sep = "\n")
#' @export
tfl_report_code <- function(spec, output_id = NULL, content = "content",
                            doc = "doc", setup = FALSE, study = NULL) {
  if (inherits(spec, "tfl_output")) {
    return(.report_steps_code(.page_report_steps(spec, study, content, isTRUE(setup)), doc))
  }
  sp <- .ard_spec_scope(.as_spec(spec, "table", "tfl_report_code",
                                 output_id = output_id), output_id)
  .report_steps_code(.report_spec_steps(sp, content, isTRUE(setup)), doc)
}

# the document's calls as one pipe: rtf_document() |> rtf_section() |>
# rtf_tables() ...
.report_steps_code <- function(steps, doc) {
  # the document's program: its path through the study's path variables
  # (tflspec.paths), as the program's other paths are
  for (k in intersect(c("program", "program_fallback"), names(steps[[1L]]$args))) {
    v <- steps[[1L]]$args[[k]]
    if (is.character(v) && length(v) == 1L) steps[[1L]]$args[[k]] <- .spec_sym(.path_code(v))
  }
  # one object, one pipe: rtf_document() |> rtf_section() |> rtf_tables() ...
  more <- length(steps) > 1L
  out <- paste(doc, "<-", .spec_call_code(steps[[1L]], 0L,
                                          lead = nchar(doc) + 4L + if (more) 3L else 0L))
  for (i in seq_along(steps)[-1L]) {
    st <- steps[[i]]
    if (length(st$args) && inherits(st$args[[1L]], "tfl_spec_sym") &&
        identical(st$args[[1L]]$name, "doc")) {
      st$args <- st$args[-1L]
    }
    lead <- 2L + if (i < length(steps)) 3L else 0L
    out <- paste0(out, " |>\n  ", gsub("\n", "\n  ", .spec_call_code(st, 0L, lead = lead),
                                       fixed = TRUE))
  }
  .drop_attached_ns(unlist(strsplit(out, "\n", fixed = TRUE)))
}

# The report sheet's table_font_size_half_points of a definition narrowed to
# one report (its own row, else the study's), an integer; NULL when it says
# none or has no report sheet
.ard_spec_report_font <- function(sp) {
  r <- sp$report
  if (is.null(r) || !nrow(r) || !"table_font_size_half_points" %in% names(r)) return(NULL)
  v <- r$table_font_size_half_points
  id <- attr(sp, "output_id")
  own <- if (!is.null(id)) v[!is.na(r$output_id) & r$output_id == id] else character()
  dflt <- v[is.na(r$output_id) | !nzchar(r$output_id)]
  x <- c(own[!is.na(own) & nzchar(own)], dflt[!is.na(dflt) & nzchar(dflt)])
  if (!length(x)) return(NULL)
  n <- suppressWarnings(as.integer(x[1L]))
  if (is.na(n)) NULL else n
}
