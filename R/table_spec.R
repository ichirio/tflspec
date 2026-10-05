# ============================================================================
#  The table and report definition: the workbook, read, written, checked,
#  and turned into a plan (tfl_table_plan(), R/spec_code.R) or written from
#  one (tfl_as_table_spec()).  This is the spec side of tflspec: it reaches
#  the plan only through table_plan() and its verbs, plan_apply() and
#  plan_layers() -- never through the plan's own fields.
# ============================================================================

# ============================================================================
#  the definition file
# ============================================================================
#
#  A definition file is a WORKBOOK with one sheet per grain, so that each
#  fact is written once, at the grain it belongs to:
#
#      study      one row per study fact      rounding (key / value)
#      tables     one row per report          cols, rows, label, sort ...
#      variables  one row per variable        display label, order, levels
#      cells      one row per line of a cell  template, guard, digits
#      layout     one row per report          pages, groups, blanks, stub
#      columns    one row per printed column  width, row title, decimals
#      style      one row per report          border, heights, font
#      cell_styles one row per styling        cells chosen, bold, colour
#      col_header one row per header cell     line, columns, span, text
#
#  `study` holds what is ONE for the whole study by definition -- the
#  rounding family, so that no two tables of one study can disagree.  The
#  others start with `output_id` and follow ONE rule: a blank
#  `output_id` is a study-wide default, and a row naming the report
#  replaces the default row with the same key.  A sheet added later -- the
#  titles, the footnotes, the page header -- joins under the same rule, so
#  the names it will take are reserved now (`.ard_spec_reserved`).

.ard_spec_version <- 1L

# The display sheets carry values of more than one type, and a cell of a
# workbook is text.  Each column says what it holds, so a value is checked
# where it is written -- `pages_max_rows = twenty` names its sheet and row
# at read time, not as an error from as_rtftables() three stages later.
#   int   a whole number              list  `a | b | c`
#   num   a number                    ids   `a | b`, or `1 | 2` (positions)
#   bool  TRUE / FALSE (yes / no)     flex  TRUE / FALSE, or an `ids` list
#   text  as written; quote it ("...") to keep leading or trailing spaces
.ard_spec_types <- list(
  layout = c(
    pages_max_rows = "int", pages_split = "text",
    pages_break_before = "ids",
    pages_min_group_rows = "int", pages_cont_label = "text",
    pages_page_by = "list",
    group_col = "text", group_mode = "text", group_collapse = "flex",
    group_page = "bool", group_keep = "bool",
    blank_where = "text", blank_first = "bool", blank_last = "bool",
    blank_counted = "bool",
    stub_vars = "list", stub_name = "text", stub_indent = "int",
    stub_summary = "text", stub_before = "bool",
    colpages_every = "int", colpages_at = "ids", colpages_keep = "ids",
    colpages_order = "list", colpages_cut_by = "text",
    colpages_fit = "bool", colpages_allow_span_break = "bool"),
  style = c(
    border = "text", align_count_pct = "bool", auto_width = "bool",
    row_height_twips = "int", row_height_exact = "bool",
    header_row_height_twips = "int", blank_row_height_twips = "int",
    cell_padding_left_twips = "int", cell_padding_right_twips = "int",
    font = "text", font_size_half_points = "int", table_align = "text",
    cell_valign = "text", markup = "text", blank_row_normalize = "text",
    border_header = "sides", border_spanning = "sides",
    border_body = "sides", border_first_row = "sides",
    border_last_row = "sides",
    # the table's default look (rtf_table_style()'s fields) and width
    header_align = "text", header_bold = "bool", header_italic = "bool",
    align = "text", bold = "bool", italic = "bool", underline = "bool",
    table_width_twips = "int", table_width_pct = "num",
    table_width_pct_of_writable = "num",
    # plan_col_header()'s: how the header text sits
    col_header_align = "text"),
  # one row per plan_cell_style(): the cells chosen by `cols`, `header`
  # and `where` (an R condition over the table's columns), and their look
  cell_styles = c(
    cols = "list", header = "bool", where = "text", bold = "bool",
    italic = "bool", align = "text", color = "text", background = "text",
    underline = "bool", indent_twips = "int"),
  columns = c(
    column = "text", rel_width = "num", row_title = "bool",
    decimal_split = "bool", hide = "bool"),
  col_header = c(
    line = "int", cols = "text", span = "text", text = "text",
    align = "text", bold = "bool", border_top = "text",
    border_bottom = "text"),
  # the report half
  report = c(
    type = "text", file = "text", program = "text", auto_section = "bool",
    section_align = "text", auto_title = "bool", title_align = "text",
    table_font_size_half_points = "int", title_font_size_half_points = "int",
    footnote_font_size_half_points = "int", page_header = "bool",
    page_footer = "bool", watermark = "text", figure_width_in = "num",
    figure_height_in = "num", ard_source = "text"),
  page = c(
    paper_size = "text", orientation = "text", width_in = "num",
    height_in = "num", margin_top_in = "num", margin_bottom_in = "num",
    margin_left_in = "num", margin_right_in = "num",
    header_dist_in = "num", footer_dist_in = "num",
    font_size_half_points = "int", title_format = "text",
    footnote_format = "text", title_width = "text",
    footnote_width = "text", markup = "text"),
  header    = c(line = "int", left = "text", center = "text", right = "text"),
  footer    = c(line = "int", left = "text", center = "text", right = "text"),
  titles    = c(line = "int", left = "text", center = "text", right = "text"),
  footnotes = c(line = "int", left = "text", center = "text", right = "text"),
  # tokens of one's own: {STUDY} in a header, footer, title or footnote
  tokens    = c(name = "text", value = "text"))

.ard_spec_unquote <- function(x) {
  q <- regmatches(x, regexec("^([\"'])(.*)\\1$", x))[[1L]]
  if (length(q)) q[3L] else x
}

# One cell to its value.  NA stays NULL: a blank cell says nothing, and
# nothing is what the verb it feeds then receives.
.ard_spec_value <- function(x, type, where) {
  if (is.null(x) || is.na(x)) return(NULL)
  bad <- function(what) {
    .ard_stop(sprintf("%s must be %s; got %s.", where, what, sQuote(x)))
  }
  bool <- function(v) {
    v <- toupper(trimws(v))
    if (v %in% c("TRUE", "YES", "Y", "1")) return(TRUE)
    if (v %in% c("FALSE", "NO", "N", "0")) return(FALSE)
    NA
  }
  ids <- function(v) {
    p <- .ard_spec_split(v)
    n <- suppressWarnings(as.integer(p))
    if (length(p) && !anyNA(n) && all(grepl("^[0-9]+$", p))) n else p
  }
  switch(type,
    int = {
      v <- suppressWarnings(as.numeric(x))
      if (is.na(v) || v != round(v)) bad("a whole number")
      as.integer(v)
    },
    num = {
      v <- suppressWarnings(as.numeric(x))
      if (is.na(v)) bad("a number")
      v
    },
    bool = {
      v <- bool(x)
      if (is.na(v)) bad("TRUE or FALSE")
      v
    },
    flex = {
      # a number here is a column position, never TRUE
      v <- if (grepl("^[0-9 |]+$", x)) NA else bool(x)
      if (!is.na(v)) v else ids(x)
    },
    list = .ard_spec_split(x),
    ids  = ids(x),
    sides = {
      # the rules of one kind of row: the sides drawn, or `none`
      v <- tolower(.ard_spec_split(x))
      if (identical(v, "none")) return(character())
      if (!length(v) || !all(v %in% c("top", "bottom", "left", "right"))) {
        bad("sides from top | bottom | left | right, or none")
      }
      v
    },
    text = gsub("\\n", "\n", gsub("\r\n", "\n",
                                   .ard_spec_unquote(x), fixed = TRUE),
                fixed = TRUE))
}

# A row of a display sheet as a named list of typed values (NULLs dropped).
.ard_spec_typed <- function(row, sheet) {
  ty <- .ard_spec_types[[sheet]]
  out <- list()
  for (cn in names(ty)) {
    v <- .ard_spec_value(row[[cn]], ty[[cn]],
                         sprintf("`%s$%s`", sheet, cn))
    if (!is.null(v)) out[[cn]] <- v
  }
  out
}


.ard_spec_schema <- function() {
  list(
    tables    = c("output_id", "cols", "rows", "label", "stats", "value",
                  "sep", "sort", "sort_stat", "na", "header_n"),
    variables = c("output_id", "variable", "label", "order", "levels",
                  "empty_levels"),
    # the study's code list: a value's text and place
    codelists = c("output_id", "variable", "value", "label", "order"),
    cells     = c("output_id", "variable", "context", "row", "when",
                  "template", "digits", "signif"),
    # the table half: what as_rtftables() / rtftable() are told, read by
    # tfl_table_plan() and resolved like the plan's own verbs
    layout    = c("output_id", names(.ard_spec_types$layout)),
    columns   = c("output_id", names(.ard_spec_types$columns)),
    style     = c("output_id", names(.ard_spec_types$style)),
    cell_styles = c("output_id", names(.ard_spec_types$cell_styles)),
    col_header = c("output_id", names(.ard_spec_types$col_header)),
    # the report half: tfl_read_report_spec() / tfl_report()
    report    = c("output_id", names(.ard_spec_types$report)),
    page      = c("output_id", names(.ard_spec_types$page)),
    header    = c("output_id", names(.ard_spec_types$header)),
    footer    = c("output_id", names(.ard_spec_types$footer)),
    titles    = c("output_id", names(.ard_spec_types$titles)),
    footnotes = c("output_id", names(.ard_spec_types$footnotes)),
    tokens    = c("output_id", names(.ard_spec_types$tokens)))
}

# The facts the `study` sheet may state, one value each for the whole
# study, and what each accepts.
.ard_spec_study_keys <- list(rounding = c("r", "sas"),
                             # any text: where the files go, where the
                             # programs are (for {PROGRAM})
                             output_path = NULL, program_dir = NULL)

# What a report's own row replaces a default row by.  `tables` has one row
# per report, so its key is the report itself.
.ard_spec_keys <- list(tables    = character(),
                       variables = "variable",
                       codelists = c("variable", "value"),
                       cells     = c("variable", "context", "row"),
                       layout    = character(),
                       columns   = "column",
                       style     = character(),
                       # the cell styles are one list of rules: a report's
                       # own rows replace the defaults whole
                       cell_styles = NA_character_,
                       # a header is one thing: a report's own cells replace
                       # the default header whole (see .ard_spec_scope)
                       col_header = NA_character_,
                       report    = character(),
                       page      = character(),
                       # the line sheets: the line is the key
                       header    = "line",
                       footer    = "line",
                       titles    = "line",
                       footnotes = "line",
                       # a report's own value replaces the default of its name
                       tokens    = "name")

# Sheets a later version will read (the rest of the RTF deliverable).  A
# workbook that already carries one is told so, not refused: the file can be
# written ahead of the reader.
.ard_spec_reserved <- c("figures")

# Columns an older workbook may still carry, and what became of them when
# the plan verbs were redesigned (rtfreporter#498).  No column is read under
# its former name: the workbook is told what to write instead.
.ard_spec_retired <- list(
  layout = c(stub_into = "renamed `stub_name`",
             colpages_carry = "renamed `colpages_keep`",
             group_show = "renamed `group_keep`",
             pages_by = paste0("removed: one page per value is ",
                               "`group_page = TRUE` with `group_col`")),
  # names that said no unit (iter02 of the brush-up, #64)
  columns = c(width = "renamed `rel_width` (a relative width)"),
  report = c(
    table_font_size = "renamed `table_font_size_half_points`",
    title_font_size = "renamed `title_font_size_half_points`",
    footnote_font_size = "renamed `footnote_font_size_half_points`"))

# A sheet in the shape the schema says: every column present, text trimmed,
# blank cells NA, wholly blank rows gone.  A column the sheet does not read is
# an error -- a typo in a header would otherwise be a setting that silently
# never applies -- except `note`, which is there for people.
.ard_spec_sheet <- function(d, sheet) {
  allowed <- .ard_spec_schema()[[sheet]]
  if (is.null(d)) d <- data.frame()
  d <- as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)
  names(d) <- trimws(names(d))
  extra <- setdiff(names(d), c(allowed, "note"))
  if (length(extra)) {
    home <- vapply(extra, function(cn) {
      why <- .ard_spec_retired[[sheet]][cn]
      if (!is.null(why) && !is.na(why)) return(paste0(" (", why, ")"))
      if (cn %in% names(.ard_spec_study_keys)) {
        return(" (one per study: a key of the `study` sheet)")
      }
      hit <- names(Filter(function(s) cn %in% s, .ard_spec_schema()))
      if (length(hit)) paste0(" (a `", hit[1L], "` column)") else ""
    }, "")
    .ard_stop(paste0(
      "The `", sheet, "` sheet has ",
      if (length(extra) == 1L) "a column" else "columns",
      " it does not read: ",
      paste0(sQuote(extra), home, collapse = ", "), ".\n",
      "  Its columns are: ", paste(allowed, collapse = ", "), ".\n",
      "  Free text goes in a `note` column."))
  }
  n <- nrow(d)
  out <- lapply(allowed, function(cn) {
    v <- if (cn %in% names(d)) d[[cn]] else rep(NA, n)
    if (identical(cn, "order")) return(suppressWarnings(as.numeric(v)))
    v <- trimws(as.character(v))
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    v
  })
  out <- as.data.frame(stats::setNames(out, allowed),
                       stringsAsFactors = FALSE)
  if ("note" %in% names(d)) out$note <- as.character(d$note)
  body <- setdiff(allowed, "output_id")
  keep <- rowSums(!is.na(out[body])) > 0L
  out[keep, , drop = FALSE]
}

# One key per row, as a string, for the "same key" of the scoping rule.
.ard_spec_rowkey <- function(d, sheet) {
  cols <- .ard_spec_keys[[sheet]]
  if (!length(cols) || !nrow(d)) return(rep("", nrow(d)))
  do.call(paste, c(lapply(d[cols], function(v) ifelse(is.na(v), "", v)),
                   sep = "\r"))
}

# Two rows for one key in one scope.  In `cells` that is a chain (the rows
# are tried in sheet order), so only `tables` and `variables` can clash.
.ard_spec_dupes <- function(sp) {
  # one row per report on the sheets keyed by the report alone
  for (sh in c("tables", "layout", "style", "report", "page")) {
    t <- sp[[sh]]
    dup <- duplicated(t$output_id)
    if (any(dup)) {
      id <- t$output_id[dup][1L]
      .ard_stop(paste0(
        "The `", sh, "` sheet has two rows for ",
        if (is.na(id)) "the default (blank `output_id`)" else sQuote(id),
        ".\n  One row per report; merge them."))
    }
  }
  for (sh in c("variables", "codelists", "columns", "header", "footer",
                "titles", "footnotes", "tokens")) {
    v <- sp[[sh]]
    if (is.null(v)) next
    key <- .ard_spec_keys[[sh]]
    kv <- do.call(paste, c(lapply(key, function(k) v[[k]]), sep = " / "))
    k <- paste(v$output_id, kv, sep = "\r")
    if (any(duplicated(k))) {
      i <- which(duplicated(k))[1L]
      .ard_stop(paste0(
        "The `", sh, "` sheet has two rows for ", sQuote(kv[i]),
        if (!is.na(v$output_id[i])) paste0(" in ", sQuote(v$output_id[i]))
        else " among the defaults",
        ".\n  One row per ", paste(key, collapse = " / "), "; merge them."))
    }
  }
  invisible(NULL)
}

# The `cell_styles` rows as plan_cell_style() takes them: something to
# style, a `where` that is an R condition, no `where` on the header, and
# -- as rtfreporter keeps one conditional rule per look -- one `where` row
# per look and report.
.ard_spec_check_cell_styles <- function(d) {
  looks <- c("bold", "italic", "align", "color", "background", "underline",
             "indent_twips")
  for (i in seq_len(nrow(d))) {
    r <- d[i, , drop = FALSE]
    at <- sprintf("`cell_styles` row %d", i)
    if (all(is.na(unlist(r[intersect(looks, names(r))])))) {
      .ard_stop(paste0(at, " styles nothing: give bold, italic, underline, ",
                       "align, indent_twips, color or background."))
    }
    if (!is.na(r$where)) {
      ok <- tryCatch(is.call(str2lang(r$where)) || is.name(str2lang(r$where)),
                     error = function(e) FALSE)
      if (!ok) .ard_stop(sprintf("%s: `where` is not an R condition: %s", at,
                                 sQuote(r$where)))
      if (isTRUE(.ard_spec_value(r$header, "bool", at))) {
        .ard_stop(paste0(at, ": the header has no rows for `where` to ",
                         "choose; leave one of them blank."))
      }
    }
  }
  w <- d[!is.na(d$where), , drop = FALSE]
  for (k in looks) {
    v <- w[!is.na(w[[k]]), , drop = FALSE]
    dup <- duplicated(ifelse(is.na(v$output_id), "", v$output_id))
    if (any(dup)) {
      .ard_stop(sprintf(paste0(
        "`cell_styles`: two rows with a `where` set `%s` for %s; a plan keeps ",
        "one conditional rule per look.
  Join the conditions with | in ",
        "one row."), k, if (is.na(v$output_id[dup][1L])) "the defaults"
        else sQuote(v$output_id[dup][1L])))
    }
  }
  invisible(NULL)
}

# The `study` sheet: `key` / `value`, one row per fact.  A named vector or
# list is taken too, so `study = c(rounding = "sas")` reads as it looks.
.ard_spec_study <- function(d) {
  if (is.null(d)) d <- data.frame(key = character(), value = character())
  if (!is.data.frame(d)) {
    if (is.null(names(d)) || !all(nzchar(names(d)))) {
      .ard_stop("`study` must be a key / value frame or a named vector.")
    }
    d <- data.frame(key = names(d), value = as.character(unlist(d)),
                    stringsAsFactors = FALSE)
  }
  d <- as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE)
  names(d) <- trimws(names(d))
  extra <- setdiff(names(d), c("key", "value", "note"))
  if (length(extra) || !all(c("key", "value") %in% names(d))) {
    .ard_stop(paste0("The `study` sheet has the columns `key` and `value` ",
                     "(and `note`), one row per study fact."))
  }
  out <- data.frame(key = trimws(as.character(d$key)),
                    value = trimws(as.character(d$value)),
                    stringsAsFactors = FALSE)
  out$value[!is.na(out$value) & !nzchar(out$value)] <- NA_character_
  if ("note" %in% names(d)) out$note <- as.character(d$note)
  out <- out[!is.na(out$key) & nzchar(out$key), , drop = FALSE]
  # the keys of an ARD spec sharing the workbook are the ARD spec's
  out <- out[!out$key %in% .spec_kind_keys$ard, , drop = FALSE]
  bad <- setdiff(out$key, names(.ard_spec_study_keys))
  if (length(bad)) {
    .ard_stop(sprintf("The `study` sheet has %s %s; it reads: %s.",
                      if (length(bad) == 1L) "a key it does not read:" else
                        "keys it does not read:",
                      paste(sQuote(bad), collapse = ", "),
                      paste(names(.ard_spec_study_keys), collapse = ", ")))
  }
  # a key left blank states nothing: drop it where another row states it
  # (two workbooks both list every key, one of them blank)
  said <- out$key[!is.na(out$value)]
  out <- out[!is.na(out$value) | !out$key %in% said, , drop = FALSE]
  out <- out[!(is.na(out$value) & duplicated(out$key)), , drop = FALSE]
  if (any(duplicated(out$key))) {
    .ard_stop(sprintf(paste0("The `study` sheet states %s twice; a study ",
                             "has one."), sQuote(out$key[duplicated(out$key)][1L])))
  }
  for (k in out$key) {
    v <- out$value[out$key == k]
    ok <- .ard_spec_study_keys[[k]]
    if (!is.na(v) && !is.null(ok) && !v %in% ok) {
      .ard_stop(sprintf("`study` %s must be %s; got %s.", sQuote(k),
                        paste(sQuote(ok), collapse = " or "), sQuote(v)))
    }
  }
  rownames(out) <- NULL
  out
}

.ard_spec_study_value <- function(sp, key) {
  st <- sp$study
  if (is.null(st) || !nrow(st)) return(NA_character_)
  v <- st$value[st$key == key]
  if (length(v)) v[1L] else NA_character_
}

# The layout before #474's rework was ONE sheet with every column on it.
# Say what moved where rather than listing columns it does not know.
.ard_spec_old_layout <- function(d) {
  .ard_stop(paste0(
    "This is the one-sheet layout, which is no longer read.  A definition ",
    "file now has\n  three sheets, one per grain:\n",
    "    tables     output_id, cols, rows, label, sort, ...\n",
    "    variables  output_id, variable, label, order, levels\n",
    "    cells      output_id, variable, context, row, when, template, ",
    "digits, signif\n",
    "  `label` / `order` / `levels` move to `variables` (once per variable), ",
    "`round` becomes\n  `rounding` on the `study` sheet (one per study), and the ",
    "rest stays on `cells`.  ",
    "tfl_table_spec_template() writes the new layout."))
}

#' A workbook-shaped definition of how an ARD becomes a table
#'
#' @description
#' `tfl_table_spec()` validates the definition that [rtfreporter::widen_ard()] accepts as
#' `spec =`, and [tfl_read_table_spec()] builds one from a workbook.  It holds
#' what would otherwise be repeated in every script --- which keys go
#' across and down, the display label and order of each variable, and the
#' template and digits of every row --- in **one sheet per grain**, so
#' that each fact is written once:
#'
#' | sheet | one row per | holds |
#' |---|---|---|
#' | `study` | study fact | the rounding family (`key` / `value`) |
#' | `tables` | report | the roles and the table-wide options |
#' | `variables` | variable | display label, order, level order |
#' | `cells` | line of a cell | template, guard, digits |
#' | `layout` | report | pages, groups, blank rows, stub |
#' | `columns` | printed column | width, row title, decimal split, hidden |
#' | `style` | report | border, row heights, font |
#' | `cell_styles` | styling | the cells chosen, bold, italic, colour |
#' | `col_header` | header cell | line, columns, span, text, borders |
#'
#' The first four say how the ARD becomes a table data frame; the last
#' four how that becomes `rtftable` pages.  [rtfreporter::table_plan()] reads them all
#' (`tfl_table_plan(data, )`), as the first layers of a plan, so a verb
#' written after it still wins.
#'
#' @section `study`:
#' What is **one for the whole study** by definition, as `key` / `value`
#' rows.  Today that is `rounding` --- `r` (half to even) or `sas` (half
#' away from zero), see [rtfreporter::round_num()] --- so that no two tables of one
#' study can round differently.  Blank leaves it to
#' `getOption("rtfreporter.rounding")`.
#'
#' @section One rule on the other sheets:
#' Every sheet but `study` starts with `output_id`.  **Blank means a study-wide
#' default; a row naming the report replaces the default row with the same
#' key** --- the whole `tables` row for that report, the `variables` row
#' for that variable, the `cells` rows for that variable / context / row.
#' So one workbook can hold a house style and every report's own changes to
#' it.  [tfl_read_table_spec()] narrows it to one report with `output_id =`.
#'
#' Explicit [rtfreporter::widen_ard()] arguments win over the spec, and the spec wins
#' over the defaults.
#'
#' Lists inside a cell are `|`-separated (`TR01AG1 | SEROSTAT`).  A column
#' called `note` is allowed on any sheet and never read; any other column a
#' sheet does not know is an error, so a mistyped header cannot become a
#' setting that silently never applies.
#'
#' @section `tables`:
#' \describe{
#'   \item{`cols`}{Column keys, outermost first: `TR01AG1 | SEROSTAT`.}
#'   \item{`rows`}{Row keys, in output order.  `name = column` renames
#'     (`group1 = AEBODSYS`); a quoted value is a constant heading
#'     (`group1 = "Worst Post-Baseline Values"`).}
#'   \item{`label`}{The row-label source, in the same notation
#'     (`label = AEDECOD`).  Blank keeps `.label`; `NA` builds it to tell
#'     rows apart and then drops it; `NULL` leaves it out.}
#'   \item{`stats`, `value`, `na`}{How a cell is filled, as
#'     [rtfreporter::plan_cells()] takes them: `stats = rows` puts each
#'     statistic on a row of its own.}
#'   \item{`sort`}{`TRUE`, `FALSE`, or the keys in order, `-` for
#'     descending: `.overall | group1 | .depth | -n | label`.}
#'   \item{`sort_stat`}{The statistic totalled across the columns for a
#'     frequency order ([rtfreporter::plan_sort()]'s `stat`).}
#'   \item{`sep`}{What joins several column keys into one column's name
#'     (`Placebo____n` by default; `_` gives `Placebo_n`), the name the
#'     `columns` sheet refers to ([rtfreporter::plan_columns()]'s `sep`).}
#'   \item{`header_n`}{Which population a `col_header` text's `{n}` is,
#'     on pages split by a group value: `page` (each page's own --- the
#'     subjects with that lab test) or `table` (the analysis set, the
#'     ARD rows without the page key).  Several at once name their
#'     tokens: `n = page | N = table` gives `{n}` and `{N}`.  Blank: the
#'     page's, with a warning when the ARD states both.  See
#'     [rtfreporter::plan_col_header()]'s `values`.}
#' }
#'
#' @section `variables`:
#' \describe{
#'   \item{`variable`}{An analysis variable, or any column key (`BASEGR`,
#'     `ATPT`, or the label column's own name).}
#'   \item{`label`}{Display text replacing the variable's name.}
#'   \item{`order`}{Number; the order the variables appear in.}
#'   \item{`levels`}{The order of its values: `Grade 0 | Grade 1 | Total`.}
#'   \item{`empty_levels`}{`show` (blank, the default) or `hide`: whether
#'     its values that no record has -- a code list's value, counted 0 in
#'     every column of an ARD made with the study's code lists -- get a row
#'     (`plan_levels(.drop_empty = )`).}
#' }
#'
#' @section `cells`:
#' \describe{
#'   \item{`variable`, `context`}{What the row applies to.  Either may be
#'     blank; both blank is the default for everything.  `variable` may
#'     also be a context or kind (`continuous`, `categorical`), as a
#'     `cells` map key may.}
#'   \item{`row`}{The row label (`Mean (SD)`).  Blank means one row per
#'     level, labelled by the level.}
#'   \item{`when`}{Optional guard, ordinary R over the statistics:
#'     `n == 0`.  **Several rows with the same variable / context / row are
#'     one chain**, tried in sheet order; the first whose guard holds and
#'     whose template resolves wins.}
#'   \item{`template`}{The cell recipe: `{mean} ({sd})`.}
#'   \item{`digits`}{Decimal places for tokens that name none, per token
#'     when comma-separated: `1,2` for `{mean} ({sd})`.}
#'   \item{`signif`}{Significant digits; wins over `digits`.}
#' }
#' For a `stats = rows` table (one statistic per row, the raw value in the
#' cell) a row with **no template** is instead that statistic's display
#' format: `row` names the statistic as the label column prints it (`N`,
#' `Mean`) and `digits` / `signif` say how many, for every value column
#' (`plan_digits(.rows = c(Mean = 2, SD = "3s"))`).
#'
#' @section `layout`:
#' One row per report, each column one argument of the plan verb its
#' prefix names:
#' \describe{
#'   \item{`pages_*`}{[rtfreporter::plan_paginate_rows()]: `max_rows`, `split`,
#'     `break_before` (row positions to cut before, `20 | 40`, with
#'     `split = rows`), `min_group_rows`, `cont_label`, `page_by` (BY
#'     pages: the column(s) partitioning the body first, the row budget
#'     inside each).}
#'   \item{`group_*`}{`mode` and `collapse` of [rtfreporter::plan_row_group()].
#'     `group_page = TRUE` is [rtfreporter::plan_paginate_group()]: one page per
#'     value of `group_col` (blank: the outermost row key), and
#'     `group_keep = FALSE` leaves that column unprinted.}
#'   \item{`blank_*`}{[rtfreporter::plan_blanks()]: `where`, `first`, `last`,
#'     `counted`.}
#'   \item{`stub_*`}{[rtfreporter::plan_stub()]: `vars`, `name`, `indent`, `summary`,
#'     `before`.}
#'   \item{`colpages_*`}{[rtfreporter::plan_paginate_cols()]: `every`, `at`,
#'     `cut_by` (a separator in the column names, `____`: one block per
#'     key), `keep`, `fit` (`TRUE`: every block on page 1's scale; `FALSE`:
#'     each column keeps its width), `allow_span_break`, `order`.}
#' }
#'
#' @section `cell_styles`:
#' One row per [rtfreporter::plan_cell_style()], in order; a report's own
#' rows replace the default rows whole.
#' \describe{
#'   \item{`cols`}{The columns styled, `|` between them (`.values` for every
#'     value column); blank, every column.}
#'   \item{`header`}{`TRUE` styles the column header instead of the body.}
#'   \item{`where`}{An R condition over the table's columns choosing the
#'     rows (`label == "Any TEAE"`, `is.na(label)`); blank, every row.  A
#'     plan keeps one conditional rule per look, so two rows with a `where`
#'     may not set the same look -- join their conditions with `|`.}
#'   \item{`bold`, `italic`, `underline`, `align`, `color`, `background`,
#'     `indent_twips`}{The look: `TRUE` / `FALSE`, `left` / `center` /
#'     `right`, a colour (`#CC0000`), a left indent in twips (not on the
#'     header).}
#' }
#'
#' @section `columns`:
#' One row per printed column, by **name** --- the finished table's, so
#' a folded stub is the name given to `stub_name`.  `.values` stands for
#' every spread column, however many the data turned out to have.
#' \describe{
#'   \item{`rel_width`}{Relative width.  Named columns win over `.values`;
#'     when widths are given, every printed column needs one.}
#'   \item{`row_title`}{`TRUE` for a row-heading column.}
#'   \item{`decimal_split`}{`TRUE` to line up the decimal points
#'     ([rtfreporter::set_decimal_split()]).}
#'   Together with `style`'s `auto_width`, these are
#'   [rtfreporter::plan_columns()].
#'   \item{`hide`}{`TRUE` to use the column without printing it.}
#' }
#'
#' @section `style`:
#' One row per report, each column an argument of
#' [rtfreporter::plan_style()]: `border`, `align_count_pct`, `font`,
#' `font_size_half_points`, `row_height_twips`, `row_height_exact`,
#' `header_row_height_twips`, `blank_row_height_twips`,
#' `cell_padding_left_twips`, `cell_padding_right_twips`, `cell_valign`,
#' `table_align`, `markup`, `blank_row_normalize`, and the rules of one
#' kind of row, `border_header`, `border_spanning`, `border_body`,
#' `border_first_row`, `border_last_row`: the sides drawn (`top | bottom`)
#' or `none`, as [rtfreporter::rtf_border()] takes them.  `border` and the
#' `border_*` columns are one or the other.  The table's default look,
#' `header_align`, `header_bold`, `header_italic`, `align`, `bold`,
#' `italic`, `underline` ([rtfreporter::rtf_table_style()]'s fields, made
#' into one style with the `border_*` columns), and its width,
#' `table_width_twips`, `table_width_pct`, `table_width_pct_of_writable`.
#' `auto_width` is [rtfreporter::plan_columns()]'s and `col_header_align`
#' [rtfreporter::plan_col_header()]'s.
#'
#' Values are checked where they are written: a number, `TRUE` / `FALSE`
#' or a `|`-list, as the column needs.  Quote a text value (`" (Cont.)"`)
#' to keep its leading or trailing spaces.
#'
#' @section `col_header`:
#' **One row per header cell**; `line` 1 is the top row.  A report's own
#' cells replace the default header whole.
#' \describe{
#'   \item{`cols`}{The columns the cell sits over, `|`-separated: a
#'     column name, `.values` (every spread column), a position or range
#'     (`3`, `3:31`, `3:last`), or `KEY = value` --- the spread columns
#'     whose column key `KEY` has that value (`variable = n`).}
#'   \item{`span`}{Blank: one cell over all of `cols`.  `each`: one cell
#'     per column.  A column key (`TR01AG1`): one cell per value of that
#'     key, over its columns --- an arm's spanner, however many arms.}
#'   \item{`text`}{The label.  A line break is Alt+Enter or `\\n`.  The
#'     tokens of [rtfreporter::plan_col_header()] work: `{col}` (the column's own
#'     value), `{col1}`, `{col2}` (its keys, outermost first), `{n}` (the
#'     population of what the cell stands for: its column, or over an
#'     arm's spanner the arm), `{n1}`, `{n2}` (the population at that
#'     depth of the keys) and `{n:sum}` (the total over the cell's
#'     columns).  `{n}` is read from the ARD whenever a text uses it; a
#'     number the ARD does not state prints `NA` with a warning, and
#'     `plan_col_header(values = )` after `tfl_table_plan()` supplies it.
#'     Quote a text to keep leading spaces: `"  Category"`.}
#'   \item{`align`, `bold`, `border_top`, `border_bottom`}{As
#'     [rtfreporter::col_cell()] / [rtfreporter::rtf_border()] take them (`single`, `none`, ...).}
#' }
#'
#' @section Reserved for the rest of the report:
#' A later version will read the sheet `figures` under the same
#' `output_id` rule,
#' so the whole RTF deliverable can be defined in one workbook.  They are reported, not refused, when present today.  An
#' `about` sheet (`key` / `value`) may state `spec_version`; sheets whose
#' name starts with `_` are ignored.
#'
#' @param tables,variables,cells,layout,columns,style,col_header,cell_styles
#'   Data frames with the columns above; missing columns are added as `NA`.
#' @param report,page,header,footer,titles,footnotes,tokens The report sheets, as
#'   data frames with the columns [tfl_read_report_spec()] describes.  `tables` may instead be a named list of the
#'   sheets, or an `tfl_table_spec` (returned as it is).
#' @param study The `study` sheet: a `key` / `value` frame, or a named
#'   vector such as `c(rounding = "sas")`.
#'
#' @return An object of class `tfl_table_spec`: a list of the sheets' data
#'   frames.
#'
#' @section The table engine:
#' The ARD functions and the plan are rtfreporter's:
#' `help("ard-tables", package = "rtfreporter")`.
#'
#' @seealso [tfl_read_table_spec()], [tfl_write_table_spec()], [tfl_table_spec_template()]
#' @export
tfl_table_spec <- function(tables = NULL, variables = NULL, cells = NULL,
                       study = NULL, layout = NULL, columns = NULL,
                       style = NULL, col_header = NULL, report = NULL,
                       page = NULL, header = NULL, footer = NULL,
                       titles = NULL, footnotes = NULL, cell_styles = NULL,
                       tokens = NULL) {
  if (inherits(tables, "tfl_table_spec")) return(tables)
  if (is.data.frame(tables) && "template" %in% names(tables) &&
      is.null(variables) && is.null(cells)) {
    .ard_spec_old_layout(tables)
  }
  args <- list(study = study, tables = tables, variables = variables,
               cells = cells, layout = layout, columns = columns,
               style = style, cell_styles = cell_styles,
               col_header = col_header, report = report,
               page = page, header = header, footer = footer,
               titles = titles, footnotes = footnotes, tokens = tokens)
  if (is.list(tables) && !is.data.frame(tables)) {
    x <- tables
    bad <- setdiff(names(x), c("study", names(.ard_spec_schema())))
    if (length(bad) || is.null(names(x))) {
      .ard_stop(paste0("A spec list holds the sheets ",
                       paste(c("study", names(.ard_spec_schema())),
                             collapse = ", "), "; it also had: ",
                       paste(sQuote(bad), collapse = ", ")))
    }
    args <- x
  }
  sp <- c(list(study = .ard_spec_study(args$study)),
          stats::setNames(lapply(names(.ard_spec_schema()), function(sh)
            .ard_spec_sheet(args[[sh]], sh)), names(.ard_spec_schema())))
  t <- sp$tables
  chk <- function(v, ok, what) {
    bad <- !is.na(v) & !v %in% ok
    if (any(bad)) {
      .ard_stop(sprintf("`tables$%s` must be %s; got %s.", what,
                        paste(sQuote(ok), collapse = " or "),
                        sQuote(v[bad][1L])))
    }
  }
  .ard_spec_check_tokens(sp$tokens)
  chk(t$stats, c("cells", "rows"), "stats")
  chk(t$value, c("stat", "stat_fmt"), "value")
  if (any(is.na(sp$variables$variable))) {
    .ard_stop("Every `variables` row needs a `variable`.")
  }
  el <- tolower(trimws(sp$variables$empty_levels))
  if (any(!is.na(el) & !el %in% c("show", "hide"))) {
    .ard_stop(sprintf("`variables$empty_levels` must be 'show' or 'hide'; got %s.",
                      sQuote(sp$variables$empty_levels[!is.na(el) &
                        !el %in% c("show", "hide")][1L])))
  }
  # a row with no template is a stats = rows display format, which needs
  # the format it is there to give
  nofmt <- is.na(sp$cells$template) & is.na(sp$cells$digits) &
    is.na(sp$cells$signif)
  if (any(nofmt)) {
    i <- which(nofmt)[1L]
    .ard_stop(sprintf(paste0(
      "`cells` row %d has no `template`, and no `digits` / `signif` ",
      "either.\n  A row is a template, or -- for a stats = rows table -- ",
      "the format of one statistic."), i))
  }
  if (any(is.na(sp$columns$column))) {
    .ard_stop("Every `columns` row needs a `column`.")
  }
  if (any(is.na(sp$col_header$line) | is.na(sp$col_header$cols))) {
    .ard_stop("Every `col_header` row needs a `line` and `cols`.")
  }
  .ard_spec_check_cell_styles(sp$cell_styles)
  # where a report's ARD comes from: its ARD definition (blank), or an ARD
  # made elsewhere and taken in (import:<the file in input/ard/>)
  src <- sp$report$ard_source %||% character()
  bad <- !is.na(src) & !grepl("^import:[^[:space:]]", src)
  if (any(bad)) {
    .ard_stop(sprintf(paste0(
      "`report$ard_source` is blank (the report's ARD definition) or ",
      "import:<file> (an ARD taken in); not %s."),
      paste(sQuote(unique(src[bad])), collapse = ", ")))
  }
  for (sh in names(.ard_spec_types)) {
    for (i in seq_len(nrow(sp[[sh]]))) {
      .ard_spec_typed(sp[[sh]][i, , drop = FALSE], sh)
    }
  }
  .ard_spec_dupes(sp)
  class(sp) <- "tfl_table_spec"
  sp
}

#' @export
print.tfl_table_spec <- function(x, ...) {
  ids <- .ard_first_seen(stats::na.omit(unlist(lapply(x, `[[`, "output_id"))))
  cat("<tfl_table_spec>",
      if (!is.null(attr(x, "output_id"))) paste0(" for ",
        sQuote(attr(x, "output_id")))
      else if (length(ids)) paste0(" for ", length(ids), " report",
                                   if (length(ids) > 1L) "s" else "",
                                   ": ", paste(ids, collapse = ", "))
      else "", "\n", sep = "")
  st <- x$study
  if (!is.null(st) && nrow(st)) {
    cat("  study      ", paste0(st$key, " = ",
                                ifelse(is.na(st$value), "(blank)", st$value),
                                collapse = ", "), "\n", sep = "")
  }
  for (s in names(.ard_spec_schema())) {
    cat(sprintf("  %-10s %3d row%s\n", s, nrow(x[[s]]),
                if (nrow(x[[s]]) == 1L) "" else "s"))
  }
  invisible(x)
}

# Narrow a workbook to one report, applying the one rule on every sheet.
# `NULL` is fine for a workbook that defines one report (or none, only
# defaults); for several it has to be said which.
.ard_spec_scope <- function(sp, output_id = NULL) {
  if (!is.null(attr(sp, "output_id"))) {
    if (is.null(output_id) || identical(output_id, attr(sp, "output_id"))) {
      return(sp)
    }
  }
  ids <- .ard_first_seen(stats::na.omit(unlist(
    lapply(sp[names(.ard_spec_schema())], `[[`, "output_id"))))
  if (is.null(output_id)) {
    if (length(ids) > 1L) {
      .ard_stop(paste0(
        "This spec defines ", length(ids), " reports (",
        paste(sQuote(ids), collapse = ", "), "); say which one:\n",
        "    tfl_read_table_spec(path, output_id = ", dQuote(ids[1L], FALSE), ")"))
    }
    if (!length(ids)) return(sp)
    output_id <- ids
  }
  if (!length(ids)) return(sp)          # a file of defaults serves any report
  if (!is.character(output_id) || length(output_id) != 1L ||
      is.na(output_id)) {
    .ard_stop("`output_id` must be a single string.")
  }
  has_default <- any(vapply(sp[names(.ard_spec_schema())],
                            function(d) any(is.na(d$output_id)), NA))
  if (!output_id %in% ids) {
    if (!has_default) {
      .ard_stop(sprintf(paste0(
        "`output_id`: this spec has no row for %s and no default rows ",
        "either, so nothing would apply.\n  It defines: %s"),
        sQuote(output_id), paste(sQuote(ids), collapse = ", ")))
    }
    # Most reports in a shared file are covered by its defaults, so this is
    # normal; it is still worth saying, because a mistyped id looks exactly
    # the same from here.
    message(sprintf(paste0(
      "tfl_read_table_spec(): no row names %s, so the default rows are used.",
      "\n  The file defines: %s"),
      sQuote(output_id), paste(sQuote(ids), collapse = ", ")))
  }
  for (s in names(.ard_spec_schema())) {
    d <- sp[[s]]
    d <- d[is.na(d$output_id) | d$output_id == output_id, , drop = FALSE]
    mine <- !is.na(d$output_id)
    if (!length(.ard_spec_keys[[s]])) {
      # one row: the report's own values over the defaults, column by column
      if (sum(mine) && sum(!mine)) {
        row <- d[!mine, , drop = FALSE]
        own <- d[mine, , drop = FALSE]
        for (cn in names(own)) if (!is.na(own[[cn]])) row[[cn]] <- own[[cn]]
        d <- row
      }
    } else if (identical(.ard_spec_keys[[s]], NA_character_)) {
      if (any(mine)) d <- d[mine, , drop = FALSE]
    } else {
      k <- .ard_spec_rowkey(d, s)
      d <- d[mine | !(k %in% k[mine]), , drop = FALSE]
    }
    d$output_id <- rep(output_id, nrow(d))
    rownames(d) <- NULL
    sp[[s]] <- d
  }
  attr(sp, "output_id") <- output_id
  sp
}

# `a | b | c` -> c("a", "b", "c")
.ard_spec_split <- function(x) {
  if (is.null(x) || is.na(x)) return(character())
  v <- trimws(strsplit(x, "|", fixed = TRUE)[[1L]])
  v[nzchar(v)]
}

# One reference, as written in a `tables` cell: `col`, `name = col`, or a
# quoted constant (`name = "Worst Post-Baseline Values"`), which is the
# formula-template form of the argument.
.ard_spec_ref <- function(x) {
  m <- regmatches(x, regexec("^([A-Za-z.][A-Za-z0-9._]*)\\s*=\\s*(.+)$", x))[[1L]]
  nm <- if (length(m)) m[2L] else ""
  v  <- trimws(if (length(m)) m[3L] else x)
  q <- regmatches(v, regexec("^([\"'])(.*)\\1$", v))[[1L]]
  val <- if (length(q)) eval(call("~", q[3L]), baseenv()) else v
  list(name = nm, value = val)
}

.ard_spec_refs <- function(x) {
  parts <- lapply(.ard_spec_split(x), .ard_spec_ref)
  if (!length(parts)) return(NULL)
  nms  <- vapply(parts, `[[`, "", "name")
  vals <- lapply(parts, `[[`, "value")
  if (any(vapply(vals, inherits, NA, "formula"))) {
    # a list keeps no name for an element that had none, and an unnamed
    # row key then has no output column; its own name is the one it means
    own <- !nzchar(nms) & !vapply(vals, inherits, NA, "formula")
    nms[own] <- unlist(vals[own])
    return(stats::setNames(vals, nms))
  }
  v <- unlist(vals)
  if (any(nzchar(nms))) names(v) <- nms
  v
}

# The table-wide arguments one `tables` row supplies, as widen_ard() takes
# them.  Only what the row says is returned: an argument it leaves blank
# keeps widen_ard()'s own default.
.ard_spec_table_args <- function(sp) {
  out <- list()
  r <- .ard_spec_study_value(sp, "rounding")
  if (!is.na(r)) out$rounding <- r
  t <- sp$tables
  if (!nrow(t)) return(out)
  t <- t[1L, , drop = FALSE]
  if (!is.na(t$cols)) out$cols <- .ard_spec_split(t$cols)
  if (!is.na(t$rows)) out$rows <- .ard_spec_refs(t$rows)
  if (!is.na(t$label)) {
    out["label"] <- list(switch(t$label, "NA" = NA, "NULL" = NULL,
                                .ard_spec_refs(t$label)))
  }
  for (a in c("stats", "value", "sep", "sort_stat", "na")) {
    if (!is.na(t[[a]])) out[[a]] <- t[[a]]
  }
  if (!is.na(t$sort)) {
    out$sort <- switch(toupper(t$sort), "TRUE" = TRUE, "FALSE" = FALSE,
                       .ard_spec_split(t$sort))
  }
  if ("header_n" %in% names(t) && !is.na(t$header_n)) {
    out$header_n <- .ard_spec_header_n(t$header_n)
  }
  out
}

# `header_n`: which population a header's {n} is -- `page` or `table` --
# or several tokens at once, `n = page | N = table`.
.ard_spec_header_n <- function(x) {
  items <- .ard_spec_split(x)
  ok <- c("page", "table")
  bad <- function(v) .ard_stop(sprintf(paste0(
    "`tables$header_n`: %s is not a population.  Write `page` (each ",
    "page's own, e.g. the\n  subjects with that lab test), `table` (the ",
    "analysis set), or several:\n  `n = page | N = table`."), sQuote(v)))
  named <- grepl("=", items, fixed = TRUE)
  if (!any(named)) {
    if (length(items) != 1L || !items %in% ok) bad(x)
    return(items)
  }
  if (!all(named)) bad(x)
  kv <- regmatches(items, regexec("^\\s*([A-Za-z][A-Za-z0-9_.]*)\\s*=\\s*(\\S+)\\s*$",
                                  items))
  out <- list()
  for (i in seq_along(kv)) {
    m <- kv[[i]]
    if (length(m) != 3L || !m[3L] %in% ok) bad(items[i])
    out[[m[2L]]] <- m[3L]
  }
  out
}

.ard_spec_variables <- function(sp) {
  v <- sp$variables
  if (any(!is.na(v$order))) v <- v[order(v$order, na.last = TRUE), , drop = FALSE]
  v
}

# The code list's rows of each variable, in their order: numbered first,
# then the rest as written.
.ard_spec_codelists <- function(sp) {
  cl <- sp$codelists
  if (is.null(cl) || !nrow(cl)) return(list())
  cl <- cl[!is.na(cl$variable) & !is.na(cl$value), , drop = FALSE]
  o <- suppressWarnings(as.numeric(cl$order))
  cl <- cl[order(is.na(o), o, seq_len(nrow(cl))), , drop = FALSE]
  split(cl, factor(cl$variable, levels = unique(cl$variable)))
}

# plan_labels(): a variable's label (variables$label) and, from the code
# list, its values' text -- one entry a variable, since one key cannot hold
# both: SEX = c(SEX = "Sex", F = "Female"), the variable's own name its
# label (rtfreporter#514).
.ard_spec_labels <- function(sp) {
  v <- .ard_spec_variables(sp)
  v <- v[!is.na(v$label), , drop = FALSE]
  out <- if (nrow(v)) as.list(stats::setNames(v$label, v$variable)) else list()
  for (cl in .ard_spec_codelists(sp)) {
    cl <- cl[!is.na(cl$label), , drop = FALSE]
    if (!nrow(cl)) next
    var <- cl$variable[1L]
    out[[var]] <- c(if (!is.null(out[[var]])) stats::setNames(out[[var]], var),
                    stats::setNames(cl$label, cl$value))
  }
  if (!length(out)) return(NULL)
  if (all(lengths(out) == 1L) && all(vapply(out, function(x) is.null(names(x)), NA))) {
    return(unlist(out))
  }
  out
}

# plan_levels(.drop_empty = ): the variables whose values no record has
# are not shown (variables$empty_levels = hide).
.ard_spec_drop_empty <- function(sp) {
  v <- sp$variables
  if (is.null(v$empty_levels)) return(NULL)
  hide <- !is.na(v$empty_levels) & tolower(trimws(v$empty_levels)) == "hide"
  out <- unique(v$variable[hide])
  if (length(out)) out
}

# plan_levels(): a variable's own `levels` (variables sheet), else the code
# list's order of its values.
.ard_spec_levels <- function(sp) {
  v <- sp$variables[!is.na(sp$variables$levels), , drop = FALSE]
  out <- stats::setNames(lapply(v$levels, .ard_spec_split), v$variable)
  for (cl in .ard_spec_codelists(sp)) {
    var <- cl$variable[1L]
    if (is.null(out[[var]])) out[[var]] <- cl$value
  }
  if (!length(out)) return(NULL)
  out
}

# Rewrite "{mean} ({sd})" + digits "1,2" into "{mean:.1f} ({sd:.2f})", so what
# the spec asked for is visible in the template itself.  `signif` wins over
# `digits`; a token that already carries an inline spec is left alone.
.ard_apply_digits <- function(tpl, digits, sgnf) {
  toks <- .ard_tokens(tpl)
  if (!length(toks)) return(tpl)
  num <- function(v) {
    if (length(v) != 1L || is.na(v) || !nzchar(as.character(v)))
      return(integer(0))
    suppressWarnings(as.integer(trimws(strsplit(as.character(v), ",")[[1]])))
  }
  dg <- num(digits); sg <- num(sgnf)
  if (!length(dg) && !length(sg)) return(tpl)
  out <- tpl
  for (i in seq_along(toks)) {
    p <- .ard_token_parts(toks[i])
    if (nzchar(p$spec)) next
    pick <- function(v) if (!length(v)) NA_integer_ else v[min(i, length(v))]
    s <- pick(sg)
    new <- if (!is.na(s)) paste0(".", s, "s") else {
      dd <- pick(dg)
      if (is.na(dd)) NA_character_ else paste0(".", dd, "f")
    }
    if (is.na(new)) next
    out <- sub(toks[i], paste0("{", p$name, ":", new, "}"), out, fixed = TRUE)
  }
  out
}

# One `cells` row as one element of a chain: its template with the digits
# written in, guarded by `when` when there is one.  The guard is parsed
# here, so a malformed one names its row.
.ard_spec_chain_el <- function(r, i) {
  tpl <- .ard_apply_digits(r$template, r$digits, r$signif)
  if (is.na(r$when)) return(tpl)
  cond <- tryCatch(str2lang(r$when), error = function(e) {
    .ard_stop(sprintf("`cells` row %d: `when` is not valid R: %s\n  %s",
                      i, sQuote(r$when), conditionMessage(e)))
  })
  eval(call("~", cond, tpl), baseenv())
}

# The `cells` sheet as widen_ard()'s `cells` map.  Rows sharing variable /
# context / row are one chain, in sheet order; the map key is the variable,
# the context, both (a variable summarised two ways), or `default`.
.ard_spec_cells <- function(sp) {
  s <- sp$cells
  s <- s[!is.na(s$template), , drop = FALSE]    # the rest are rows formats
  if (!nrow(s)) return(NULL)
  ord <- .ard_spec_variables(sp)$variable
  key <- ifelse(!is.na(s$variable) & !is.na(s$context),
                paste(s$variable, s$context, sep = "\r"),
                ifelse(!is.na(s$variable), s$variable,
                       ifelse(!is.na(s$context), s$context, "default")))
  first <- .ard_first_seen(key)
  rank <- match(sub("\r.*$", "", first), ord)
  first <- first[order(is.na(rank), rank)]
  out <- list()
  for (k in first) {
    idx  <- which(key == k)
    rows <- s$row[idx]
    rows[is.na(rows)] <- ""
    labs <- .ard_first_seen(rows)
    chains <- lapply(labs, function(lb) {
      els <- lapply(idx[rows == lb], function(i)
        .ard_spec_chain_el(s[i, , drop = FALSE], i))
      if (all(vapply(els, is.character, NA))) unlist(els) else els
    })
    guarded <- any(vapply(chains, is.list, NA))
    out[[k]] <- if (identical(labs, "")) chains[[1L]]
                else if (guarded) do.call(rtfreporter::cell_rows, stats::setNames(chains, labs))
                else stats::setNames(chains, labs)
  }
  out
}

# Workbook sheets -> the three frames.  Reserved sheets are reported, `_`
# sheets and empty ones ignored, `about` checked for the version; anything
# else is a sheet nobody reads, and says so.
.ard_spec_from_sheets <- function(sheets, where) {
  nm <- names(sheets)
  low <- tolower(nm)
  if (!any(low %in% c("study", names(.ard_spec_schema())))) {
    one <- sheets[[1L]]
    if (length(sheets) >= 1L && "template" %in% names(one)) {
      .ard_spec_old_layout(one)
    }
    .ard_stop(paste0(sQuote(where), " has none of the sheets `tables`, ",
                     "`variables`, `cells`."))
  }
  if ("about" %in% low) {
    a <- sheets[[which(low == "about")[1L]]]
    if (all(c("key", "value") %in% names(a))) {
      ver <- suppressWarnings(as.integer(a$value[a$key == "spec_version"]))
      if (length(ver) && !is.na(ver[1L]) && ver[1L] > .ard_spec_version) {
        .ard_stop(sprintf(paste0(
          "%s is spec_version %d; this tflspec reads up to %d.  ",
          "Update the package."), sQuote(where), ver[1L], .ard_spec_version))
      }
    }
  }
  empty <- vapply(sheets, function(d) !nrow(d), NA) & low != "study"
  # the sheets of another spec sharing the workbook (tfl_write_specs())
  theirs <- low %in% unlist(.spec_kind_sheets()[c("ard", "listing")])
  skip <- startsWith(nm, "_") | low == "about" | empty | theirs
  res <- low %in% .ard_spec_reserved & !skip
  if (any(res)) {
    message(sprintf(paste0(
      "tfl_read_table_spec(): %s %s reserved for a later version and not read yet."),
      paste(sQuote(nm[res]), collapse = ", "),
      if (sum(res) == 1L) "is" else "are"))
  }
  other <- !skip & !res & !low %in% c("study", names(.ard_spec_schema()))
  if (any(other)) {
    .ard_stop(paste0(
      sQuote(where), " has ",
      if (sum(other) == 1L) "a sheet" else "sheets", " nobody reads: ",
      paste(sQuote(nm[other]), collapse = ", "), ".\n",
      "  Rename it to start with `_` to keep it as notes."))
  }
  get <- function(s) {
    i <- which(low == s)
    if (length(i)) sheets[[i[1L]]] else NULL
  }
  tfl_table_spec(stats::setNames(lapply(c("study", names(.ard_spec_schema())),
                                    get),
                             c("study", names(.ard_spec_schema()))))
}

# A definition is ONE file holding several sheets, which is what a workbook
# is; a CSV holds one table, so it cannot be one.  Say so by extension
# rather than letting readxl fail on a file it cannot open.
.ard_spec_xlsx_path <- function(path, fn) {
  if (!is.character(path) || length(path) != 1L ||
      !grepl("[.]xlsx$", path, ignore.case = TRUE)) {
    .ard_stop(paste0(
      "`", fn, "()` takes an .xlsx workbook: a definition is one file ",
      "with the sheets\n  `tables`, `variables` and `cells`, which a CSV ",
      "(one table per file) cannot hold."))
  }
  invisible(path)
}

#' Read an ARD table definition from a workbook
#'
#' @param path An `.xlsx` workbook (needs \pkg{readxl}) with the sheets
#'   of [tfl_table_spec()] (`study`, `tables`, `variables`, `cells`, `layout`,
#'   `columns`, `style`, `col_header`).  Any of them may be absent.  A definition is one file with several sheets,
#'   so it is an Excel workbook and nothing else.
#' @param output_id The report to narrow the workbook to.  Rows with a blank
#'   `output_id` are the study's defaults and stay; a row naming this
#'   report replaces the default with the same key.  `NULL` (default)
#'   reads the whole workbook; what needs one report --- [rtfreporter::table_plan()],
#'   [tfl_report()] --- then takes a workbook of one report as it is and
#'   asks which of several.
#'
#' @return An [tfl_table_spec()].
#'
#' @section The table engine:
#' The ARD functions and the plan are rtfreporter's:
#' `help("ard-tables", package = "rtfreporter")`.
#'
#' @seealso [tfl_table_spec()], [tfl_write_table_spec()]
#' @export
tfl_read_table_spec <- function(path, output_id = NULL) {
  if (!is.character(path) || !length(path)) {
    .ard_stop("`path` must name one or more .xlsx workbooks.")
  }
  for (f in path) .ard_spec_xlsx_path(f, "tfl_read_table_spec")
  .ard_need("readxl", "tfl_read_table_spec()")
  sheets <- list()
  from <- character()
  for (f in path) {
    if (!file.exists(f)) .stop_no_file(f, "tfl_read_table_spec")
    nms <- readxl::excel_sheets(f)
    for (sh in nms) {
      low <- tolower(sh)
      # notes and the version stamp may repeat; a sheet that says something
      # may be said once
      if (startsWith(sh, "_") || low == "about") {
        if (!low %in% tolower(names(sheets))) {
          sheets[[sh]] <- .xlsx_lf(as.data.frame(readxl::read_excel(
            f, sheet = sh, col_types = "text"), stringsAsFactors = FALSE))
        }
        next
      }
      d <- .xlsx_lf(as.data.frame(readxl::read_excel(f, sheet = sh,
                                                     col_types = "text"),
                                  stringsAsFactors = FALSE))
      hit <- match(low, tolower(names(sheets)))
      if (!is.na(hit)) {
        old <- names(sheets)[hit]
        # an empty sheet says nothing, and `study` is facts: one workbook's
        # keys and another's are the study's (a key said twice still errs)
        if (!nrow(d)) next
        if (!nrow(sheets[[old]])) {
          sheets[[old]] <- d
          from[[old]] <- basename(f)
          next
        }
        if (identical(low, "study")) {
          keep <- intersect(names(sheets[[old]]), names(d))
          sheets[[old]] <- rbind(sheets[[old]][keep], d[keep])
          next
        }
        .ard_stop(sprintf(paste0(
          "The sheet %s has rows in both %s and %s.\n  One workbook says ",
          "each thing: keep its rows in one of them."), sQuote(sh),
          sQuote(from[[old]]), sQuote(basename(f))))
      }
      sheets[[sh]] <- d
      from[[sh]] <- basename(f)
    }
  }
  sp <- .ard_spec_from_sheets(sheets, paste(basename(path), collapse = " + "))
  # the whole study, unless one report is asked for: a workbook is edited,
  # combined and compared whole, and whatever needs ONE report
  # (table_plan(), tfl_report(), tfl_report_path()) narrows it and says so
  if (is.null(output_id)) sp else .ard_spec_scope(sp, output_id)
}

#' Write a table or report definition to a workbook
#'
#' Each writer writes the sheets its kind of spec needs and no more:
#' `tfl_write_table_spec()` the table sheets (`tables`, `variables`,
#' `cells`, `layout`, `columns`, `style`, `col_header`),
#' `tfl_write_report_spec()` the report sheets (`report`, `page`, `header`,
#' `footer`, `titles`, `footnotes`, `tokens`), each with the `study` sheet (showing the
#' keys that kind reads: `rounding`; `output_path`, `program_dir`) and an
#' `about` sheet stating `spec_version`.  Its own sheets are written even
#' when empty, so their columns are there to fill in; a sheet of the other
#' half is written only when the spec has rows in it, so nothing is dropped.
#' What each column means is a comment on its header cell
#' ([tfl_spec_columns()]).  Both halves in one workbook:
#' [tfl_write_specs()].
#'
#' @param spec An [tfl_table_spec()] (or what it accepts).
#' @param path Destination `.xlsx`.
#'
#' @return `path`, invisibly.
#'
#' @section The table engine:
#' The ARD functions and the plan are rtfreporter's:
#' `help("ard-tables", package = "rtfreporter")`.
#'
#' @seealso [tfl_table_spec()], [tfl_read_table_spec()]
#' @export
tfl_write_table_spec <- function(spec, path) {
  .ard_spec_xlsx_path(path, "tfl_write_table_spec")
  sp <- tfl_table_spec(spec)
  # the study sheet always shows its keys, blank or not, so the one place
  # the study's rounding is decided is visible in every workbook
  sheets <- c(list(study = .spec_study_sheet(sp$study, "table")),
              .spec_half_sheets(sp, "table"),
              list(about = .spec_about()))
  .write_spec_book(sheets, path)
}

#' Scaffold a definition workbook from an ARD
#'
#' Walks the ARD and writes the three [tfl_table_spec()] sheets: one `tables` row,
#' one `variables` row per analysis variable (its levels filled in for a
#' categorical one), and one `cells` row per row template --- a continuous
#' variable gets the templates its statistics can fill, a categorical one
#' `{n} ({p})`.  Edit the labels, templates and digits, and hand it back
#' through `spec =`.
#'
#' @param ard A cards/cardx ARD.
#' @param path Optional destination; when given the spec is also written there
#'   with [tfl_write_table_spec()].
#' @param cols The column keys for the `tables` row, if known.
#' @param output_id The report the rows belong to; `NULL` writes them as
#'   defaults.
#'
#' @return An [tfl_table_spec()], invisibly when `path` is given.
#'
#' @section The table engine:
#' The ARD functions and the plan are rtfreporter's:
#' `help("ard-tables", package = "rtfreporter")`.
#'
#' @seealso [tfl_table_spec()], `plan_template(form = "spread")`
#' @export
tfl_table_spec_template <- function(ard, path = NULL, cols = NULL,
                              output_id = NULL) {
  d <- rtfreporter::normalize_ard(ard, drop_key_variables = FALSE)
  if (".key_own" %in% names(d)) d <- d[!(d$.key_own %in% TRUE), , drop = FALSE]
  vars <- .ard_first_seen(d$variable)
  id <- if (is.null(output_id)) NA_character_ else output_id
  vrows <- list(); crows <- list()
  cell <- function(v, ctx, row, tpl) data.frame(
    output_id = id, variable = v, context = ctx, row = row,
    when = NA_character_, template = tpl, digits = NA_character_,
    signif = NA_character_, stringsAsFactors = FALSE)
  for (i in seq_along(vars)) {
    v <- vars[i]
    s <- d[d$variable == v, , drop = FALSE]
    ctx <- s$context[1L]
    lv <- NA_character_
    if (identical(ctx, "continuous") || identical(ctx, "summary")) {
      have <- .ard_first_seen(s$stat_name)
      cand <- list(c("n", "{N}"), c("Mean (SD)", "{mean} ({sd})"),
                   c("Median", "{median}"), c("Min, Max", "{min}, {max}"))
      for (cc in cand) {
        need <- gsub("[{}]", "", .ard_tokens(cc[2]))
        if (all(need %in% have)) {
          crows[[length(crows) + 1L]] <- cell(v, NA_character_, cc[1], cc[2])
        }
      }
    } else {
      l <- .ard_first_seen(stats::na.omit(s$variable_level))
      if (length(l)) lv <- paste(l, collapse = " | ")
      crows[[length(crows) + 1L]] <- cell(v, NA_character_, NA_character_,
                                          "{n} ({p})")
    }
    vrows[[i]] <- data.frame(output_id = id, variable = v, label = v,
                             order = i, levels = lv,
                             stringsAsFactors = FALSE)
  }
  tables <- data.frame(output_id = id,
                       cols = if (length(cols)) paste(cols, collapse = " | ")
                              else NA_character_,
                       stringsAsFactors = FALSE)
  sp <- tfl_table_spec(tables,
                 if (length(vrows)) do.call(rbind, vrows) else NULL,
                 if (length(crows)) do.call(rbind, crows) else NULL)
  if (is.null(path)) return(sp)
  tfl_write_table_spec(sp, path)
  invisible(sp)
}



# ============================================================================
#  the report half: from the table's pages to an RTF document
# ============================================================================
#
#  The same workbook rules, one level up.  `report` and `page` have one row
#  per report (blank output_id = the study's default); `header`, `footer`,
#  `titles` and `footnotes` have one row per LINE, and the line number is
#  the key -- so the study's page header is written once, on lines 1-2 of
#  the defaults, a report's titles follow on its own lines 3.., and a
#  run-information footer on line 99 closes every report's footnotes.

# One band of rows (header, footer, titles, footnotes) as the rows its
# constructor takes: c(l = , c = , r = ), in line order.  A line with no
# text is a blank line, which is how a title block gets its spacing.  A
# line that says `(none)` is no line at all: a report's row of that kind
# takes the study's line of the same number out instead of replacing it
# (the run line 99 a report does without).
.ard_spec_omit <- "(none)"

# ---- the tokens sheet --------------------------------------------------------
# Tokens of one's own -- {STUDY}, {CUTOFF} -- for a report's header, footer,
# titles and footnotes: rtfreporter::rtf_document(tokens = ).  One row a
# token (output_id, name, value); a blank output_id is the study's default,
# a report's row of the same name replaces it, and "(none)" takes it out.
# The names follow rtfreporter's rule: upper case, a letter then letters,
# digits or _, never one of rtfreporter's own tokens.
.ard_spec_own_rx <- "^[A-Z][A-Z0-9_]*$"
.ard_spec_rtf_tokens <- c("PAGE", "TOTAL_PAGES", "BOOK_PAGE", "AUTO_PAGE",
                          "AUTO_TOTAL_PAGES", "SECTION_PAGES", "PROGRAM",
                          "PROGRAM_FULL", "PROGRAM_NAME", "PROGRAM_DIR",
                          "DATETIME")

.ard_spec_check_tokens <- function(d) {
  if (is.null(d) || !nrow(d)) return(invisible(NULL))
  nm <- trimws(d$name)
  if (any(is.na(nm) | !nzchar(nm))) {
    .ard_stop("Every `tokens` row needs a `name` (STUDY, DATA_CUTOFF ...).")
  }
  bad <- nm[!grepl(.ard_spec_own_rx, nm)]
  if (length(bad)) {
    .ard_stop(paste0(
      "`tokens$name` is upper case -- a letter, then letters, digits or _ ",
      "(STUDY, DATA_CUTOFF): not ", paste(sQuote(unique(bad)), collapse = ", "), "."))
  }
  own <- intersect(nm, .ard_spec_rtf_tokens)
  if (length(own)) {
    .ard_stop(paste0(
      "`tokens`: ", paste0("{", own, "}", collapse = ", "),
      " is rtfreporter's own token; give yours another name."))
  }
  invisible(NULL)
}

# A report's tokens (the sheet already scoped to it), as rtf_document()
# takes them: a named list; NULL when there is none.
.ard_spec_tokens <- function(sp) {
  d <- sp$tokens
  if (is.null(d) || !nrow(d)) return(NULL)
  v <- ifelse(is.na(d$value), "", d$value)
  keep <- !trimws(v) %in% .ard_spec_omit
  if (!any(keep)) return(NULL)
  stats::setNames(as.list(v[keep]), trimws(d$name[keep]))
}
.ard_spec_band <- function(sp, sheet) {
  d <- sp[[sheet]]
  if (is.null(d) || !nrow(d)) return(NULL)
  d <- d[order(suppressWarnings(as.numeric(d$line))), , drop = FALSE]
  cells <- intersect(c("left", "center", "right"), names(d))
  gone <- Reduce(`|`, lapply(d[cells], function(v) trimws(v) %in% .ard_spec_omit),
                 rep(FALSE, nrow(d)))
  d <- d[!gone, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  lapply(seq_len(nrow(d)), function(i) {
    r <- .ard_spec_typed(d[i, , drop = FALSE], sheet)
    cells <- c(l = r[["left"]] %||% NA, c = r[["center"]] %||% NA,
               r = r[["right"]] %||% NA)
    cells <- cells[!is.na(cells)]
    # a blank line is centred, as an unnamed c("") is
    if (!length(cells)) c(c = "") else cells
  })
}

# `{output_id}` in a report's file or program name.
.ard_spec_fill_id <- function(x, id) {
  if (is.null(x) || is.na(id)) return(x)
  gsub("{output_id}", id, x, fixed = TRUE)
}

.ard_spec_report_row <- function(sp) {
  r <- if (nrow(sp$report)) .ard_spec_typed(sp$report[1L, ], "report") else list()
  id <- attr(sp, "output_id") %||% NA_character_
  r$file <- .ard_spec_fill_id(r$file %||% "{output_id}.rtf", id)
  r$program <- .ard_spec_fill_id(r$program %||% "{output_id}", id)
  r
}

#' Read a report definition: the table and everything around it
#'
#' @description
#' A **report definition** is the [tfl_table_spec()] sheets plus the ones that
#' say how a report's pages are dressed:
#'
#' | sheet | one row per | holds |
#' |---|---|---|
#' | `report` | report | `type`, `file`, `program`, `auto_section`, font sizes |
#' | `page` | report | paper, orientation, margins, the document's text defaults |
#' | `header` | line of the page header | `line`, `left`, `center`, `right` |
#' | `footer` | line of the page footer | the same |
#' | `titles` | line above the table | the same |
#' | `footnotes` | line below the table | the same |
#' | `tokens` | token of one's own | `name`, `value`: `{STUDY}` in any cell |
#'
#' plus `study` keys `output_path` (where the RTF files go) and
#' `program_dir` (where the programs are, for `{PROGRAM}`).
#'
#' The one `output_id` rule applies, and on the line sheets **the line is
#' the key**: the study's page header is written once, as default lines 1
#' and 2, a report's titles follow on its own lines 3, 4, ..., and a
#' run-information line 99 in the default footer
#' (`{PROGRAM}      Generated on: {DATETIME}`) closes every report's
#' footnotes.  A line with no text is a blank line; a report's line that
#' says `(none)` (in any of its cells) takes the default line of that
#' number out instead -- the run line a report does without.  The page tokens
#' (`{PAGE}`, `{TOTAL_PAGES}`, ...) and the run tokens ([rtfreporter::generate_rtfreport()])
#' work in every cell.
#'
#' The `tokens` sheet names tokens of one's own: `name` `STUDY`, `value`
#' `ABC-123`, and a header, footer, title or footnote says `{STUDY}`
#' ([rtfreporter::rtf_document()]'s `tokens`).  A blank `output_id` is the
#' study's default; a report's row of the same name replaces it, and one
#' that says `(none)` takes it out.  A name is upper case -- a letter, then
#' letters, digits or `_` -- and not one of rtfreporter's own tokens.
#'
#' The sheets may be in **one workbook or several** --- a `report.xlsx` a
#' lead keeps (the list of outputs, titles, footnotes) and a `tables.xlsx`
#' the programmers keep: give `path` as a vector and the sheets are read
#' together.  A sheet found in two of them is an error.
#'
#' @param path One or more `.xlsx` workbooks.
#' @param output_id The report to narrow the definition to.
#'
#' @return A [tfl_table_spec()] carrying the report sheets as well; it serves
#'   `tfl_table_plan()` and [tfl_report()] alike.
#'
#' @section `report`:
#' `type` (`table`, `listing`, `figure`; default `table`), `file` (default
#' `{output_id}.rtf`), `program` (default `{output_id}`, joined to
#' `study$program_dir` for `{PROGRAM}`), `auto_section`, `section_align`,
#' `auto_title`, `title_align` (as [rtfreporter::rtf_tables()] takes them), and
#' `table_font_size_half_points`, `title_font_size_half_points`,
#' `footnote_font_size_half_points`,
#' and `page_header` / `page_footer` (`FALSE` drops that running band for
#' the report, the study's default lines included), `watermark` (a word
#' drawn behind every page, `rtf_document(watermark = )`: `DRAFT`), and
#' `figure_width_in` / `figure_height_in` (a figure report's figure size in
#' inches, [rtfreporter::rtf_figures()]).
#'
#' @section `page`:
#' `paper_size`, `orientation`, `width_in`, `height_in`, `margin_top_in`,
#' `margin_bottom_in`, `margin_left_in`, `margin_right_in`,
#' `header_dist_in`, `footer_dist_in` (the page, as [rtfreporter::rtf_document()] takes
#' it), and `font_size_half_points`, `title_format`, `footnote_format`,
#' `title_width`, `footnote_width`, `markup` ([rtfreporter::rtf_default_format()]).
#'
#' @section The table engine:
#' The ARD functions and the plan are rtfreporter's:
#' `help("ard-tables", package = "rtfreporter")`.
#'
#' @seealso [tfl_report()], [tfl_report_path()], [tfl_read_table_spec()]
#' @export
tfl_read_report_spec <- function(path, output_id = NULL) {
  tfl_read_table_spec(path, output_id)
}

#' Build a report's RTF document from its definition
#'
#' `tfl_report()` is the document half of a report definition: the page,
#' the running header and footer, the titles and footnotes, and the
#' table's pages --- an [rtfreporter::table_plan()] or anything [rtfreporter::rtf_tables()] takes ---
#' in one [rtfreporter::rtf_document()] ready for [rtfreporter::generate_rtfreport()].  The document
#' carries its program, so `{PROGRAM}` needs nothing more.
#'
#' ```r
#' spec <- tfl_read_report_spec(c("report.xlsx", "tables.xlsx"), output_id = id)
#' plan <- ard |> normalize_ard() |> tfl_table_plan(spec)
#' generate_rtfreport(tfl_report(spec, content = plan), tfl_report_path(spec),
#'                    overwrite = TRUE)
#' ```
#'
#' @param spec A report definition ([tfl_read_report_spec()]) narrowed to one
#'   report, or the path(s) to read it from.
#' @param output_id The report, when `spec` is a path or still defines
#'   several.
#' @param content The report's content: an [rtfreporter::table_plan()] or `rtftable`
#'   pages for a table or listing, figures for a `type = figure` report.
#'
#' @return An [rtfreporter::rtf_document()].
#'
#' @section The table engine:
#' The ARD functions and the plan are rtfreporter's:
#' `help("ard-tables", package = "rtfreporter")`.
#'
#' @seealso [tfl_read_report_spec()], [tfl_report_path()]
#' @export
tfl_report <- function(spec, output_id = NULL, content) {
  .spec_need_rtfreporter()
  if (!is.null(output_id) && !(is.character(output_id) &&
                               length(output_id) == 1L)) {
    .ard_stop(paste0(
      "tfl_report(): `output_id` must be a single string; got ",
      .what(output_id), ".\n  The arguments are tfl_report(spec, output_id, ",
      "content): name the content -- tfl_report(spec, content = plan)."))
  }
  if (missing(content)) {
    .ard_stop("tfl_report(): give the report's `content` (a plan, pages or figures).")
  }
  sp <- .ard_spec_scope(.as_spec(spec, "table", "tfl_report",
                                 output_id = output_id), output_id)
  # the rtfreporter calls the definition stands for (R/spec_code.R): run
  # here, written out by tfl_report_code() -- one list, so the two agree
  env <- new.env(parent = emptyenv())
  env$content <- content
  for (st in .report_spec_steps(sp, "content")) {
    env$doc <- .spec_eval(st, env)
  }
  env$doc
}

#' Where a report's RTF file goes
#'
#' `file.path(study$output_path, report$file)`, with `{output_id}` filled:
#' the path [rtfreporter::generate_rtfreport()] writes to.
#'
#' @inheritParams tfl_report
#' @return A single path.
#'
#' @section The table engine:
#' The ARD functions and the plan are rtfreporter's:
#' `help("ard-tables", package = "rtfreporter")`.
#'
#' @seealso [tfl_report()]
#' @export
tfl_report_path <- function(spec, output_id = NULL) {
  sp <- .ard_spec_scope(.as_spec(spec, "table", "tfl_report_path",
                                 output_id = output_id), output_id)
  r <- .ard_spec_report_row(sp)
  out <- .ard_spec_study_value(sp, "output_path")
  if (is.na(out)) r$file else file.path(out, r$file)
}

#' Write a plan as a table definition workbook
#'
#' @description
#' `tfl_as_table_spec()` turns an [rtfreporter::table_plan()] --- typically one a report
#' already has as code --- into a [tfl_table_spec()], the definition
#' [tfl_write_table_spec()] writes as an Excel workbook.  It is how an existing
#' report becomes the **template for a new study**: write the workbook,
#' edit its labels, levels and output ids, and read it back with
#' `tfl_table_plan(data, tfl_read_table_spec(path, output_id = ))`.
#'
#' Everything is read from what the plan **resolves to**, against its own
#' data: the roles, levels, labels and cell templates (digits written in),
#' the pages, groups, blank rows and stub, the column widths **by name**
#' (`.values` when every value column shares one), and the column header
#' --- with a literal that is a column's own key value turned back into
#' `{col}` / `{col1}`, a repeated per-column cell into `span = each`, and
#' one spanner per arm into `span = <key>`, so the header keeps up with a
#' study that has a different number of arms or time points.
#'
#' What a workbook cannot say is **listed, not dropped**: a `plan_after()`
#' step (except `set_decimal_split()`, which becomes
#' `columns$decimal_split` on the value columns), a guarded label, a
#' column-scoped `labels` entry, `plan_cell_style()`, a literal `n`.  The
#' result is then run back through `tfl_table_plan()` on the plan's data,
#' and whether it gives **the same pages** is reported.
#'
#' @param x An [rtfreporter::table_plan()], a **named list** of them (the names are the
#'   output ids; one workbook for the study), or anything [tfl_table_spec()]
#'   takes.
#' @param output_id The report the rows belong to.  `NULL` writes them as
#'   defaults (blank `output_id`).
#' @param compare `TRUE` (default) rebuilds the pages from the workbook and
#'   compares their RTF, byte by byte, with the plan's (the titles and
#'   footnotes aside: they are the report's).  The RTF is what a reader
#'   gets: two page objects that differ in how they hold the same output
#'   compare as the same.
#'
#' @return A [tfl_table_spec()], with attributes `"not_converted"` (what the
#'   workbook could not carry) and `"same_pages"` (`TRUE` / `FALSE`: the
#'   same RTF, or `NA` when not compared).
#'
#' @section Lifecycle:
#' **Spike.**  See [rtfreporter::table_plan()].
#'
#' @examples
#' \dontrun{
#' p <- ard |> normalize_ard() |> table_plan(cols = "TRT01P") |> ...
#' tfl_as_table_spec(p, output_id = "T14-1-1") |> tfl_write_table_spec("study.xlsx")
#'
#' # a whole study at once
#' tfl_as_table_spec(list(DM = p_dm, AE = p_ae)) |> tfl_write_table_spec("study.xlsx")
#' }
#' @seealso [tfl_table_spec()], [tfl_write_table_spec()], [rtfreporter::table_plan()]
#' @export
tfl_as_table_spec <- function(x, output_id = NULL, compare = TRUE) {
  .spec_need_rtfreporter()
  if (inherits(x, "tfl_table_spec")) return(x)
  if (is.list(x) && !inherits(x, "table_plan") && length(x) &&
      all(vapply(x, inherits, NA, "table_plan"))) {
    ids <- names(x)
    if (is.null(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
      .ard_stop(paste0("A list of plans needs unique names: they are the ",
                       "output ids of the workbook."))
    }
    parts <- lapply(ids, function(id) tfl_as_table_spec(x[[id]], id, compare))
    rnd <- unique(stats::na.omit(vapply(parts, function(s)
      .ard_spec_study_value(s, "rounding"), "")))
    if (length(rnd) > 1L) {
      .ard_stop(paste0("The plans round differently (",
                       paste(rnd, collapse = " / "), "); a study has one ",
                       "rounding.  Make them agree first."))
    }
    sheets <- setdiff(names(.ard_spec_schema()), character())
    out <- lapply(sheets, function(s) do.call(rbind, lapply(parts, `[[`, s)))
    names(out) <- sheets
    out$study <- if (length(rnd)) c(rounding = rnd) else NULL
    sp <- tfl_table_spec(out)
    attr(sp, "not_converted") <- unlist(lapply(seq_along(parts), function(i)
      if (length(attr(parts[[i]], "not_converted")))
        paste0(ids[i], ": ", attr(parts[[i]], "not_converted"))))
    attr(sp, "same_pages") <- stats::setNames(
      vapply(parts, function(s) attr(s, "same_pages") %||% NA, NA), ids)
    return(sp)
  }
  if (!inherits(x, "table_plan")) return(tfl_table_spec(x))
  .plan_to_spec(x, output_id, compare)
}

# One kind of row's rules as the `style` sheet writes them: the sides drawn
# (`top | bottom`), or `none`.  Only a default rule on the four outer sides
# converts; a style, width, colour or inside rule stays in code (NULL).
.spec_border_sides <- function(b) {
  if (!inherits(b, "rtf_border")) return(NULL)
  plain <- rtfreporter::rtf_border(top = TRUE)$top
  if (!is.null(b$inside_h) || !is.null(b$inside_v)) return(NULL)
  sides <- c("top", "bottom", "left", "right")
  on <- sides[!vapply(b[sides], is.null, NA)]
  if (!all(vapply(b[on], identical, NA, plain))) return(NULL)
  if (length(on)) paste(on, collapse = " | ") else "none"
}

# ---------------------------------------------------------------------------
#  A plan written as code, back to a workbook
# ---------------------------------------------------------------------------
#
#  The inverse of tfl_table_plan(): every layer the workbook can say is
#  written to its sheet, read from what the plan RESOLVES to rather than
#  from how it was typed, so two plans that mean the same thing give the
#  same workbook.  What a sheet cannot say -- a function, a guarded label,
#  a positional list that no name reproduces -- is named, not dropped in
#  silence, and the workbook is run back through tfl_table_plan() against
#  the plan's own data to say whether it gives the same pages.  All of it
#  is read through plan_layers() and plan_apply(): nothing here
#  looks inside a plan.
.plan_to_spec <- function(p, output_id, compare) {
  id <- if (is.null(output_id)) NA_character_ else output_id
  lost <- character()
  miss <- function(...) lost <<- c(lost, sprintf(...))
  q <- function(x) if (grepl("^\\s|\\s$", x)) paste0("\"", x, "\"") else x
  qq <- function(v) vapply(v, function(x) if (is.na(x)) NA_character_ else q(x), "")
  bar <- function(x) paste(x, collapse = " | ")

  # everything is read from what the plan resolves to and from
  # plan_layers(): never from the plan's own fields
  a <- suppressMessages(rtfreporter::plan_apply(p, "args"))
  s <- a$widen
  L <- suppressMessages(rtfreporter::plan_layers(p))
  pages <- L$pages
  pnames <- L$columns$names
  spread <- L$columns$spread
  ly <- L$layers

  if ("stat" %in% names(L$roles)) {
    miss("table_plan(stat = ): naming your statistic columns stays in code")
  }

  # -- tables ---------------------------------------------------------------
  ref <- function(v, nm) {
    if (inherits(v, "formula")) {
      if (length(v) == 3L) return(NA_character_)
      v <- paste0("\"", eval(v[[2L]], environment(v)), "\"")
    } else v <- as.character(v)
    if (nzchar(nm) && !identical(nm, v)) paste(nm, "=", v) else v
  }
  refs <- function(x, what) {
    if (is.null(x)) return(NA_character_)
    nms <- names(x) %||% rep("", length(x))
    out <- vapply(seq_along(x), function(i) ref(x[[i]], nms[i]), "")
    if (anyNA(out)) {
      miss("%s: a guarded (condition ~ template) element stays in code", what)
      out <- out[!is.na(out)]
    }
    if (length(out)) bar(out) else NA_character_
  }
  lab <- if (!"label" %in% names(s)) NA_character_
    else if (is.null(s$label)) "NULL"
    else if (length(s$label) == 1L && !is.list(s$label) && is.na(s$label)) "NA"
    else if (identical(unname(s$label), ".label") && is.null(names(s$label)))
      NA_character_
    else refs(s$label, "label")
  srt <- s$sort
  tables <- data.frame(
    output_id = id,
    cols = bar(unlist(s$cols)),
    rows = refs(s$rows, "rows"),
    label = lab,
    stats = s$stats %||% NA_character_,
    value = s$value %||% NA_character_,
    sep = if (is.null(s$sep) || identical(s$sep, "____")) NA_character_
          else s$sep,
    sort = if (is.null(srt)) NA_character_
           else if (is.logical(srt)) as.character(srt) else bar(srt),
    sort_stat = s$sort_stat %||% NA_character_,
    na = if (is.null(s$na) || is.na(s$na)) NA_character_ else q(s$na),
    stringsAsFactors = FALSE)

  # -- variables --------------------------------------------------------------
  lbl <- s$labels
  if (is.list(lbl)) {
    plain <- vapply(lbl, function(v) is.character(v) && length(v) == 1L &&
                      is.null(names(v)), NA)
    if (any(!plain)) {
      miss("labels: a column-scoped entry stays in code (plan_labels())")
    }
    lbl <- unlist(lbl[plain])
  }
  hide <- unique(unlist(ly$levels$drop_empty))
  vars <- unique(c(names(lbl), names(s$levels), hide))
  variables <- data.frame(
    output_id = rep(id, length(vars)), variable = vars,
    label = unname(ifelse(vars %in% names(lbl), lbl[vars], NA_character_)),
    order = ifelse(vars %in% names(lbl), match(vars, names(lbl)), NA),
    levels = vapply(vars, function(v)
      if (is.null(s$levels[[v]])) NA_character_ else bar(s$levels[[v]]), ""),
    empty_levels = ifelse(vars %in% hide, "hide", NA_character_),
    stringsAsFactors = FALSE)

  # -- cells ------------------------------------------------------------------
  crow <- function(var, ctx, row, when, tpl, digits = NA, signif = NA)
    data.frame(output_id = id, variable = var, context = ctx, row = row,
               when = when, template = tpl, digits = digits,
               signif = signif, stringsAsFactors = FALSE)
  cl <- list()
  if (length(L$cells) && !identical(s$stats, "rows")) {
    for (ce in L$cells) {
      e <- ce$entry
      for (i in seq_along(e$chains)) {
        rw <- if (is.null(e$labels) || !nzchar(e$labels[i])) NA_character_
              else e$labels[i]
        for (el in e$chains[[i]]) {
          wh <- if (is.null(el$cond)) NA_character_
                else paste(deparse(el$cond), collapse = " ")
          cl[[length(cl) + 1L]] <- crow(ce$variable, ce$context, rw, wh, el$tpl)
        }
      }
    }
  }
  # plan_digits(.rows = ): one statistic's digits in every value column,
  # the `cells` rows with no template
  rd <- ly[["digits"]]$.rows
  for (st in names(rd)) {
    v <- as.character(rd[[st]])
    sig <- grepl("s$", v)
    cl[[length(cl) + 1L]] <- crow(NA, NA, st, NA, NA,
      if (!sig) v else NA, if (sig) sub("s$", "", v) else NA)
  }
  cells <- if (length(cl)) do.call(rbind, cl) else NULL

  # -- layout -----------------------------------------------------------------
  lay <- list(output_id = id)
  put <- function(nm, v) {
    if (is.null(v)) return(invisible())
    lay[[nm]] <<- if (is.logical(v) && length(v) == 1L) as.character(v)
                  else if (is.character(v) && length(v) == 1L) q(v)
                  else bar(v)
  }
  st <- ly[["stub"]]
  if (length(st)) {
    put("stub_vars", st$vars); put("stub_name", st$name)
    put("stub_indent", st$indent); put("stub_summary", st$group_summary)
    if (isTRUE(st$before)) put("stub_before", TRUE)
  }
  g <- ly[["group"]]
  put("group_mode", g$group_by); put("group_collapse", g$collapse_repeats)
  if (isTRUE(g$.page)) {
    put("group_page", TRUE); put("group_col", g$group_col)
    if (identical(g$.keep, FALSE)) put("group_keep", FALSE)
  }
  b <- ly[["blanks"]]
  if (!is.null(b$blank_rows) && !(is.character(b$blank_rows) &&
                                   length(b$blank_rows) == 1L)) {
    miss("plan_blanks(where = ): only a named rule (\"between_groups\") converts")
  } else put("blank_where", b$blank_rows)
  put("blank_first", b$blank_row_first); put("blank_last", b$blank_row_end)
  put("blank_counted", b$count_blank_rows)
  pg <- ly[["pages"]]
  put("pages_max_rows", pg$max_rows); put("pages_split", pg$split)
  put("pages_min_group_rows", pg$min_group_rows)
  put("pages_cont_label", pg$cont_label)
  put("pages_break_before", pg$split_rows)
  put("pages_page_by", pg$page_by)
  cp <- ly[["colpages"]]
  put("colpages_every", cp$every); put("colpages_at", cp$at)
  put("colpages_keep", cp$keep); put("colpages_order", cp$order)
  put("colpages_fit", cp$fit)
  put("colpages_allow_span_break", cp$allow_span_break)
  if (is.character(cp$cut_by) && length(cp$cut_by) == 1L) {
    put("colpages_cut_by", cp$cut_by)
  } else if (!is.null(cp$cut_by)) {
    miss("plan_paginate_cols(cut_by = ): only a separator converts (blocks are `colpages_at`)")
  }
  if ("col_header" %in% names(cp)) {
    miss("plan_paginate_cols(col_header = ) stays in code")
  }
  layout <- if (length(lay) > 1L) as.data.frame(lay, stringsAsFactors = FALSE)

  # -- style and columns ------------------------------------------------------
  sty <- ly[["style"]]
  style <- list(output_id = id)
  sc <- ly[["columns"]]
  # the default look is kept as `.style_<field>` in the plan
  if (length(sty)) names(sty) <- sub("^[.]style_", "", names(sty))
  for (nm in names(sty)) {
    v <- sty[[nm]]
    if (startsWith(nm, "border_")) {
      sides <- .spec_border_sides(v)
      if (is.null(sides)) miss("plan_style(%s = ): only a plain rtf_border() converts", nm)
      else style[[nm]] <- sides
    } else if (nm %in% names(.ard_spec_types$style) && is.atomic(v) &&
               length(v) == 1L) {
      style[[nm]] <- as.character(v)
    } else if (nm %in% names(.ard_spec_types$style)) {
      miss("plan_style(%s = ): only a single value converts", nm)
    } else {
      miss("plan_style(%s = ) stays in code", nm)
    }
  }
  if (!is.null(sc$auto_width)) style$auto_width <- as.character(sc$auto_width)
  if (!is.null(sc$cell_format)) {
    miss("plan_columns(cell_format = ): a function stays in code")
  }
  if (!is.null(sc$column_widths_twips)) {
    miss("plan_columns(column_widths_twips = ) stays in code (rel_width with table_width_twips sets the same widths)")
  }
  hl <- ly[["header"]]
  ha <- hl[["text_align"]]
  if (is.character(ha) && length(ha) == 1L) style$col_header_align <- ha
  else if (!is.null(ha)) miss("plan_col_header(col_header_align = ): only a single value converts")
  if (!is.null(hl[["names_sep"]])) {
    miss("plan_col_header(header_sep = ) stays in code (a definition's column names are joined by tables$sep)")
  }
  style <- if (length(style) > 1L) as.data.frame(style, stringsAsFactors = FALSE)

  # -- cell styles ------------------------------------------------------------
  # by column or in the header (a value), and by condition: a look written
  # `~ ifelse(<condition>, <value>, NA)`, which is what plan_cell_style(where
  # = ) makes; a look computed row by row otherwise stays in code
  csr <- list()
  cs_row <- function(cols = NA, header = NA, where = NA, look) {
    r <- data.frame(output_id = id, cols = cols, header = header,
                    where = where, bold = NA_character_,
                    italic = NA_character_, align = NA_character_,
                    color = NA_character_, background = NA_character_,
                    underline = NA_character_, indent_twips = NA_character_,
                    stringsAsFactors = FALSE)
    for (k in names(look)) r[[k]] <- as.character(look[[k]])
    csr[[length(csr) + 1L]] <<- r
  }
  looks <- c("bold", "italic", "align", "color", "background", "underline",
             "indent_twips")
  for (l in ly[["restyle"]]) {
    ar <- l$args
    lk <- ar[intersect(names(ar), looks)]
    plain <- all(vapply(lk, function(v) is.atomic(v) && length(v) == 1L, NA))
    if (!is.null(ar$border) || !plain) {
      miss("plan_cell_style(border = ) and a look that is not one value stay in code")
      next
    }
    cs_row(cols = if (length(ar$cols)) bar(ar$cols) else NA,
           header = if (identical(l$fun, "style_header")) "TRUE" else NA,
           look = lk)
  }
  for (k in intersect(names(ly[["styles"]]), looks)) {
    f <- ly[["styles"]][[k]]
    one <- function(f) {
      e <- if (inherits(f, "formula") && length(f) == 2L) f[[2L]]
      if (is.call(e) && identical(e[[1L]], as.name("ifelse")) &&
          length(e) == 4L && length(e[[4L]]) == 1L && is.na(e[[4L]]) &&
          is.atomic(e[[3L]]) && length(e[[3L]]) == 1L) {
        list(where = paste(deparse(e[[2L]]), collapse = " "), value = e[[3L]])
      }
    }
    if (inherits(f, "formula")) {
      o <- one(f)
      if (is.null(o)) {
        miss("plan_cell_style(%s = ~ ...): a look computed row by row stays in code (write it with where = )", k)
      } else cs_row(where = o$where, look = stats::setNames(list(o$value), k))
    } else if (is.list(f) && length(f) && !is.null(names(f))) {
      os <- lapply(f, one)
      same <- !any(vapply(os, is.null, NA)) &&
        length(unique(lapply(os, deparse))) == 1L
      if (!same) {
        miss("plan_cell_style(%s = ): a look that differs by column stays in code", k)
      } else cs_row(cols = bar(names(f)), where = os[[1L]]$where,
                    look = stats::setNames(list(os[[1L]]$value), k))
    }
  }
  cell_styles <- if (length(csr)) do.call(rbind, csr)

  colw <- rep(NA_real_, length(pnames)); names(colw) <- pnames
  w <- L$columns$widths
  # widths one a column in order (plan_columns(widths = ) without names),
  # read back from the pages
  positional <- length(sc$widths) && is.null(names(sc$widths))
  if (positional && length(w) == length(pnames)) colw[] <- w
  if (length(sc$widths) && !is.null(names(sc$widths))) {
    for (k in names(sc$widths)) {
      if (identical(k, ".values")) colw[spread] <- sc$widths[[k]]
      else if (k %in% pnames) colw[[k]] <- sc$widths[[k]]
    }
  }
  rt <- sc$row_title
  rt <- if (is.null(rt)) character() else if (is.numeric(rt)) pnames[rt] else rt
  hide <- setdiff(a$rtf$drop_cols %||% character(),
                  if (identical(g$.keep, FALSE))
                    c(g$group_col, L$group_col) else NULL)
  dec <- sc$decimal %||% character()
  for (l in ly[["after"]]) {
    for (f in l$steps) {
      if (any(grepl("set_decimal_split", deparse(f), fixed = TRUE))) {
        dec <- c(dec, ".values")
        miss("plan_after(set_decimal_split()): taken as the value columns")
      } else {
        miss("a plan_after() step stays in code")
      }
    }
  }
  one_w <- length(spread) && all(!is.na(colw[spread])) &&
    length(unique(colw[spread])) == 1L
  crows <- list()
  cadd <- function(col, width = NA, title = NA, dsplit = NA, hid = NA)
    crows[[length(crows) + 1L]] <<- data.frame(
      output_id = id, column = col, rel_width = as.character(width),
      row_title = title, decimal_split = dsplit, hide = hid,
      stringsAsFactors = FALSE)
  for (nm in setdiff(pnames, if (one_w) spread)) {
    if (is.na(colw[[nm]]) && !nm %in% c(rt, dec, hide)) next
    cadd(nm, colw[[nm]], if (nm %in% rt) "TRUE" else NA,
         if (nm %in% dec) "TRUE" else NA)
  }
  if (one_w || ".values" %in% dec) {
    cadd(".values", if (one_w) colw[[spread[1L]]] else NA, NA,
         if (".values" %in% dec) "TRUE" else NA)
  }
  for (nm in hide) cadd(nm, hid = "TRUE")
  columns <- if (length(crows)) do.call(rbind, crows)

  # -- col_header -------------------------------------------------------------
  H <- L$header
  col_header <- NULL
  if (!is.null(H$cells)) {
    hc <- H$cells
    hc$text <- qq(hc$text)
    col_header <- cbind(data.frame(output_id = rep(id, nrow(hc)),
                                   stringsAsFactors = FALSE), hc)
  }
  if (identical(H$source, "resolved") && isTRUE(H$literal_n)) {
    miss("plan_col_header(values = ): a literal N stays in code (use {n})")
  }
  tables$header_n <- H$n_text %||% NA_character_

  # -- titles / footnotes -----------------------------------------------------
  # a plan's titles and footnotes are the report's: they go to the
  # `titles` / `footnotes` sheets, which tfl_report() reads (a title is
  # centred, a footnote at the left, as the plan puts them)
  band <- function(k, side) {
    b <- ly[[k]]
    if (is.null(b)) return(NULL)
    if (!identical(names(b), "block")) {
      miss("plan_%s(pages = ): %s that differ by page stay in code", k, k)
      return(NULL)
    }
    lines <- b$block
    ok <- vapply(lines, function(z) is.character(z) && length(z) == 1L, NA)
    if (!all(ok)) {
      miss("plan_%s(): a line that is not plain text stays in code", k)
    }
    txt <- unlist(lines[ok])
    if (!length(txt)) return(NULL)
    data.frame(output_id = id, line = as.character(seq_along(txt)),
               left = if (side == "left") txt else NA_character_,
               center = if (side == "center") txt else NA_character_,
               right = NA_character_, stringsAsFactors = FALSE)
  }
  titles <- band("titles", "center")
  footnotes <- band("footnotes", "left")

  study <- if (!is.null(s$rounding)) c(rounding = s$rounding)
  sp <- tfl_table_spec(tables, variables, cells, study = study, layout = layout,
                   columns = columns, style = style, col_header = col_header,
                   titles = titles, footnotes = footnotes,
                   cell_styles = cell_styles)

  same <- NA
  if (isTRUE(compare)) {
    back <- tryCatch(suppressMessages(rtfreporter::plan_apply(
      rtfreporter::plan_cells(tfl_table_plan(L$data, sp), notes = FALSE),
      "pages")),
      error = function(e) e)
    # the titles and footnotes are compared as sheets: the report puts
    # them on the pages, not the plan the workbook gives
    bare <- function(pg) lapply(pg, function(t) {
      attr(t, "rtf_titles") <- NULL
      attr(t, "rtf_footnotes") <- NULL
      t
    })
    same <- !inherits(back, "error") &&
      identical(.spec_rtf(bare(back)), .spec_rtf(bare(pages)))
    if (inherits(back, "error")) {
      miss("the workbook does not run: %s", conditionMessage(back))
    }
  }
  if (length(lost) || isFALSE(same)) {
    message(sprintf(
      "tfl_as_table_spec()%s: %s\n%s",
      if (is.na(id)) "" else paste0(" [", id, "]"),
      if (isTRUE(same)) "the workbook gives the same RTF as the plan"
      else if (isFALSE(same)) "the workbook does NOT give the same RTF"
      else "not compared",
      if (length(lost)) paste0("  not converted:\n",
                               paste0("    - ", unique(lost), collapse = "\n"))
      else ""))
  }
  attr(sp, "not_converted") <- unique(lost)
  attr(sp, "same_pages") <- same
  sp
}
