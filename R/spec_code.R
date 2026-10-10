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

# a one-sided formula written in a workbook (`where` of a cell style):
# `~ <text>` in the program, a formula when run
.spec_formula <- function(text) {
  structure(list(text = text), class = "tfl_spec_formula")
}

# A function a step names: tflspec's own, else rtfreporter's (the plan
# verbs, rtf_document() and friends).
.spec_fun <- function(name) {
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
    v <- c(as.list(x), list(check.names = FALSE))
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
  paste(deparse(x, width.cutoff = 500L,
                control = c("keepNA", "keepInteger", "niceNames",
                            "showAttributes")),
        collapse = " ")
}

# a call, on one line when it fits after `indent` characters
.spec_call_code <- function(cl, indent = 2L) {
  one <- .spec_code_line(cl)
  if (nchar(one) + indent <= 80L) return(one)
  .spec_code(cl, indent)
}

# ---- tables ----------------------------------------------------------------

# The roles a table definition gives table_plan() (its `tables` sheet):
# which column goes across, which go down, which carries the row text.
# Everything else on that sheet is a layer, said by the verb it belongs to.
.plan_role_names <- c("cols", "rows", "label", "stat")
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
  de <- .ard_spec_drop_empty(sp)
  if (length(lv) || length(de)) {
    do.call(add, c(list("plan_levels"), as.list(lv),
                   if (length(de)) list(.drop_empty = de)))
  }
  lb <- .ard_spec_labels(sp)
  if (length(lb)) do.call(add, c(list("plan_labels"), as.list(lb)))
  ne <- .ard_spec_nest(sp)
  if (length(ne)) do.call(add, c(list("plan_nest"), ne))
  if (!is.null(sa[["total"]])) {
    add("plan_total", label = sa[["total"]], position = sa[["total_position"]])
  }
  cm <- .ard_spec_cells(sp)
  how <- sa[intersect(c("stats", "value", "na"), names(sa))]
  if (length(cm) || length(how)) do.call(add, c(list("plan_cells"), cm, how))

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
        keep = if (identical(lay[["group_keep"]], FALSE)) FALSE)
  }
  a <- pick("group_", c(mode = "mode", collapse = "collapse"))
  if (length(a)) do.call(add, c(list("plan_row_group"), a))
  a <- pick("blank_", c(where = "where", first = "first", last = "last",
                        counted = "counted"))
  if (length(a)) do.call(add, c(list("plan_blanks"), a))
  a <- pick("pages_", c(max_rows = "max_rows", split = "split",
                        break_before = "break_before",
                        min_group_rows = "min_group_rows",
                        cont_label = "cont_label", page_by = "page_by"))
  if (length(a)) do.call(add, c(list("plan_paginate_rows"), a))
  a <- pick("colpages_", c(at = "at", cut_by = "cut_by", every = "every",
                           keep = "keep", fit = "fit",
                           allow_span_break = "allow_span_break",
                           order = "order"))
  if (length(a)) do.call(add, c(list("plan_paginate_cols"), a))

  sty <- if (nrow(sp$style)) .ard_spec_typed(sp$style[1L, ], "style")
         else list()
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
  st
}

#' The code of a table's plan, from its definition
#'
#' Writes the [rtfreporter::table_plan()] pipeline a table definition stands for: the
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
#' @param data The name of the normalized ARD in the program.
#' @param plan The name the plan is assigned to.
#' @param pipe `"|>"` or `"%>%"`; `NULL` follows
#'   `getOption("rtfreporter.ard_pipe")` (see [rtfreporter::plan_template()]).
#' @return The code, one element per line.
#' @seealso [tfl_report_code()] for the report around it.
#' @examples
#' spec <- tfl_read_table_spec(
#'   system.file("extdata", "ard-spec", "DM.xlsx", package = "tflspec"))
#' cat(tfl_table_code(spec), sep = "\n")
#' @export
tfl_table_code <- function(spec, output_id = NULL, data = "data",
                           plan = "plan", pipe = NULL) {
  sp <- .ard_spec_scope(.as_spec(spec, "table", "tfl_table_code",
                                 output_id = output_id), output_id)
  roles <- .plan_spec_roles(sp)
  op <- .ard_pipe_op(pipe)
  head <- do.call(.spec_call, c(list("table_plan", .spec_sym(data)), roles))
  steps <- .plan_spec_steps(roles, sp)
  lines <- c(.spec_call_code(head, 0L),
             vapply(steps, function(s) paste0("  ", .spec_call_code(s)), ""))
  lines[-length(lines)] <- paste(lines[-length(lines)], op)
  lines[1L] <- paste(plan, "<-", lines[1L])
  .drop_attached_ns(unlist(strsplit(lines, "\n", fixed = TRUE)))
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
#' \dontrun{
#' spec <- tfl_read_table_spec("study.xlsx", output_id = "DM")
#' plan <- ard |> normalize_ard() |> tfl_table_plan(spec)
#' plan |> plan_paginate_rows(max_rows = 40)   # this report's own change
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
  for (st in .plan_spec_steps(roles, sp)) {
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
  st <- list(.spec_call("rtf_document",
                        font_table = if (!is.null(pg[["font"]])) list(list(name = pg[["font"]])),
                        page = if (length(page)) page,
                        default_format = if (length(fmt))
                          do.call(.spec_call, c(list("rtf_default_format"), fmt)),
                        watermark = r$watermark,
                        program = prog,
                        program_fallback = fallback,
                        tokens = doc_tokens))
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
    .spec_call(paste0("rtf_", sheet), b)
  }
  hdr <- band("header")
  ftr <- band("footer")
  if (length(hdr) || length(ftr)) {
    st[[length(st) + 1L]] <- .spec_call("rtf_section", doc, secinfo = list(
      header = hdr, footer = ftr)[
        c(if (length(hdr)) "header", if (length(ftr)) "footer")])
  }
  if (identical(r$type %||% "table", "figure")) {
    twips <- function(v) if (!is.null(v)) as.integer(round(v * 1440))
    st[[length(st) + 1L]] <- .spec_call("rtf_figures", doc, .spec_sym(content),
      width_twips = twips(r$figure_width_in),
      height_twips = twips(r$figure_height_in))
  } else {
    st[[length(st) + 1L]] <- .spec_call("rtf_tables", doc, .spec_sym(content),
      auto_section = r$auto_section,
      section_label_align = r$section_align,
      auto_title = r$auto_title,
      title_label_align = r$title_align,
      font_size_half_points = r$table_font_size_half_points)
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
#' @param spec A report definition (the study's default rows: one with
#'   several reports serves).
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
    call <- .spec_call(paste0("rtf_", sheet), b,
                       drop_empty_rows = if (length(own)) TRUE)
    out <- c(out, paste(.setup_band_names[[sheet]], "<-", .spec_call_code(call, 0L)))
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
#' @inheritParams tfl_report
#' @return A named character vector (none: empty).
#' @examples
#' sp <- tfl_table_spec(
#'   report = data.frame(output_id = "T-14-1-1", type = "table"),
#'   header = data.frame(output_id = NA, line = "1", center = "{OUTPUT_LABEL}"))
#' tfl_report_tokens(sp, "T-14-1-1")
#' @export
tfl_report_tokens <- function(spec, output_id = NULL) {
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
#' rtfreporter.  Write it out with
#' `generate_rtfreport(doc, "<file>.rtf")` ([tfl_report_path()] says which).
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
#'   `FALSE` (default): the program stands alone.
#' @return The code, one element per line.
#' @examples
#' spec <- tfl_read_report_spec(
#'   system.file("extdata", "ard-spec", c("report.xlsx", "study.xlsx"),
#'               package = "tflspec"), output_id = "DM")
#' cat(tfl_report_code(spec, content = "plan"), sep = "\n")
#' @export
tfl_report_code <- function(spec, output_id = NULL, content = "content",
                            doc = "doc", setup = FALSE) {
  sp <- .ard_spec_scope(.as_spec(spec, "table", "tfl_report_code",
                                 output_id = output_id), output_id)
  steps <- .report_spec_steps(sp, content, isTRUE(setup))
  out <- vapply(steps, function(s) {
    if (length(s$args) && inherits(s$args[[1L]], "tfl_spec_sym") &&
        identical(s$args[[1L]]$name, "doc")) {
      s$args[[1L]] <- .spec_sym(doc)
    }
    paste(doc, "<-", .spec_call_code(s, 0L))
  }, "")
  .drop_attached_ns(unlist(strsplit(out, "\n", fixed = TRUE)))
}
