# ---- demog and stf_t02_demog: two standalone templates (migration batch 1) -------
#
# Each is a template of its own (canon 1.11): its params come from the shared
# helper below and its body is the shared `.tpl_demog_body`; neither is defined
# through the other, and nothing is looked up through another template's name
# at run time.  The purpose and scope fix what they must be (rows are the
# characteristics' blocks, columns the arms, subjects counted once, a block
# never split over pages, the Total column last); the rest is a parameter with
# a default.  One value a parameter: the report's, else the template's default,
# which may refer to the study standard (`study =`); defaults.yml, patterns and
# a style set do not apply to the report as layers.
#
# Each parameter says what `!unset` does to it (`unset =`, in one place):
#   "omit"     the argument is left out of the inner call;
#   "step_off" the step the parameter stands for is not made, no code for it;
#   "stop"     the body needs a value: `!unset` stops, naming the parameter.

.tpl_demog_labels <- c(AGE = "Age (years)", AGEGR1 = "Age group, n (%)", SEX = "Sex, n (%)",
                       RACE = "Race, n (%)", ETHNIC = "Ethnicity, n (%)")

# What the cells show is placement only: bare tokens; {mean} and {p} read the
# ARD's own formatted statistic (stat_fmt).  The digits are the ARD's `formats`:
# a statistic's, and optionally a variable's (BMIBL: {mean: 2}).
.tpl_demog_cells <- list(n = "{N}", `Mean (SD)` = .tpl_mean_sd("{mean}", "{sd}"),
                         Median = "{median}", `Min, Max` = "{min}, {max}")
# The ARD's formats when the template is given none: each statistic's format in the active
# statistics catalog (tfl_ard_statistics(), fmt_default in the program), so a company catalog's
# digits reach a bare {sd} or {p} as they did before.  A plain xx.x is the number of decimals.
.tpl_demog_formats_active <- function() {
  st <- tfl_ard_statistics()
  one <- function(s, kind) {
    f <- st$fmt[st$statistic == s & st$kind == kind]
    f <- f[!is.na(f) & nzchar(f)]
    if (!length(f)) return(NULL)
    f <- f[1L]
    if (grepl("^xx([.]x+)?$", f)) nchar(sub("^xx[.]?", "", f)) else f
  }
  m <- c(stats::setNames(lapply(c("N", "mean", "sd", "median", "min", "max"), one, kind = "continuous"),
                         c("N", "mean", "sd", "median", "min", "max")),
         list(p = one("p", "categorical")))
  Filter(Negate(is.null), m)
}

# The parameters of the two templates.  `defaults`: the template's own
# defaults by name (stf_t02_demog's); `fixed`: the names whose default does not
# refer to the study standard (the template's look, not the study's);
# `fixed_value`: the names whose value the template fixes (canon 1.30: no
# parameter, a report that gives one stops).
.tpl_demog_params <- function(defaults = list(), fixed = character(), fixed_value = character()) {
  p <- list(
    tfl_param("population", required = TRUE, unset = "stop",
              doc = "the analysis set's id: the report's `population:` field, not a template parameter"),
    tfl_param("continuous", NULL, tier = "required", unset = "stop",
              doc = "the continuous variables, in order (required: this or `categorical`)"),
    tfl_param("categorical", NULL, tier = "required", unset = "stop",
              doc = "the categorical variables, in order (required: this or `continuous`)"),
    tfl_param("by", required = TRUE, study = FALSE, unset = "stop",
              doc = "the column variable (the arms); not a default and not the study's treatment"),
    tfl_param("subset", NULL, unset = "step_off",
              doc = "which subjects (R, on the analysis set's ADSL): PART == \"Part 1\"; unset: all"),
    tfl_param("data", "ADSL", unset = "stop", doc = "a subject-level dataset"),
    tfl_param("data_id", NULL, doc = "the analysis data's id when `data` is not ADSL; NULL: <data>_<population>"),
    # (fixed in both, canon 1.31: the percentages are over the analysis set)
    tfl_param("denominator", NULL, doc = "fixed: the percentages are over the analysis set"),
    tfl_param("formats", NULL, tier = "usual",
              doc = paste("the ARD's formats (precision): per statistic, and per variable",
                          "(BMIBL: {mean: 2, sd: 3}); given by name, merged over the statistics",
                          "catalog's (tfl_ard_statistics(), by default N 0, mean 1, sd 2, median 1,",
                          "min 0, max 0, p xx.x%); a number is the decimals, \"xx.x%\" a proportion",
                          "as a percent; unset: none given here")),
    tfl_param("total", FALSE, tier = "usual", unset = "step_off",
              doc = "TRUE: a Total column, last (each analysis again without `by`)"),
    # (#770's: the percentages are over the analysis set, a Missing row for a
    # variable that has missing values)
    tfl_param("missing_row", TRUE, type = "flag", unset = "step_off", doc = paste(
      "TRUE (default): a Missing row, last, for each categorical variable that has missing values",
      "(NA or blank). FALSE: no Missing row; missing values are left out of the counts, and the",
      "percentages stay over the analysis set.")),
    tfl_param("missing_label", "Missing", type = "text", unset = "stop", doc = "that row's label"),
    tfl_param("order", NULL,
              doc = "the blocks' order (plan_levels(variable = )); NULL: the continuous, then the categorical"),
    tfl_param("labels", .tpl_demog_labels, tier = "usual", unset = "step_off",
              doc = paste("each variable's heading; given by name, merged (RACE: ~ drops one);",
                          "a variable it does not name has none; unset: no headings")),
    tfl_param("categorical_levels", NULL, type = "list", multiple = FALSE, fields = c("variable", "values"),
              doc = paste("per categorical variable, its values in display order, each shown even when 0",
                          "(RACE: [ASIAN, WHITE]); NULL: the code list's order, else the data's")),
    tfl_param("cells", .tpl_demog_cells, tier = "usual", study = "plan_cells.continuous",
              doc = "the rows of each continuous variable: where each statistic goes (placement only, {mean})"),
    tfl_param("sd_missing", "-", type = "text", study = "settings.sd_missing", unset = "step_off",
              doc = paste("what prints in place of the SD when a group has no SD (n = 1):",
                          "{mean} (<sd_missing>); \"\" prints the mean alone; unset: no such guard;",
                          "no effect when `cells` is given")),
    tfl_param("count_cell", NULL, study = c("plan_cells.categorical", "plan_cells.default"),
              doc = "a categorical value's cell, n (%); NULL: the study standard's"),
    tfl_param("total_label", "Total", type = "text", doc = "the Total column's heading"),
    tfl_param("header_n", "denominator", type = "choice", choices = c("denominator", "set", "none"),
              unset = "step_off", maps_to = character(),
              doc = paste("the arm headings' N: \"denominator\" or \"set\": the arm's subjects in the",
                          "analysis set (`denominator` does not move it); \"none\" no N; unset: no N.",
                          "It acts on the heading `col_header` gives as a text")),
    tfl_param("col_header", NULL, type = "text", study = c("plan_col_header.lines", "plan_col_header.header"),
              doc = paste("the arm heading: a text ({col} and {n}, a line a line), the lines of",
                          "plan_col_header(lines = ), or its cells (header = ); NULL: none")),
    tfl_param("heading", NULL, type = "text", maps_to = "plan_stub.heading",
              doc = paste("the row-title (stub) column's heading, a line a line.  Given, it is the",
                          "stub's (plan_stub(heading = ), the stub being row_label, first)")),
    tfl_param("stub_name", NULL, type = "text", study = "plan_stub.name",
              doc = "the row-title column's name (plan_stub(name = ))"),
    tfl_param("stub_before", NULL, type = "flag", study = "plan_stub.before",
              doc = "TRUE: the row-title column before the row keys (plan_stub(before = ))"),
    tfl_param("widths", NULL, type = "text", multiple = TRUE, study = "plan_columns.widths",
              doc = paste("NULL: the columns' content; \"auto\": the row-title column 35% of the width,",
                          "the arm columns sharing the rest; numbers: the relative widths, one a",
                          "printed column; or the widths by column name, as plan_columns(widths = )",
                          "takes them (row_label: 40, .values: 18)")),
    tfl_param("max_rows", 30L, type = "number", study = "plan_paginate_rows.max_rows",
              doc = "rows a page (plan_paginate_rows(max_rows = ))"),
    tfl_param("rounding", NULL, type = "choice", choices = c("sas", "r"), study = "settings.rounding",
              doc = paste("the rounding of the cells' numbers (plan_digits(rounding = )): sas, half away",
                          "from zero, or r, half to even; NULL: none given here (the study's setup)")),
    tfl_param("blanks", NULL, type = "text", study = "plan_blanks.where", maps_to = "plan_blanks.where",
              doc = paste("blank rows: TRUE between the blocks, FALSE none, or where (plan_blanks(where = ));",
                          "FALSE also sets both ends off; NULL: none made here")),
    tfl_param("blanks_first", NULL, type = "flag", study = "plan_blanks.first",
              doc = "a blank row before the first block (plan_blanks(first = ))"),
    tfl_param("blanks_last", NULL, type = "flag", study = "plan_blanks.last",
              doc = "a blank row after the last block (plan_blanks(last = ))"),
    tfl_param("blanks_counted", NULL, type = "flag", study = "plan_blanks.counted",
              doc = "blank rows count in a page's rows (plan_blanks(counted = ))"),
    tfl_param("border_header", NULL, type = "list", multiple = FALSE, fields = c("edge", "line"), study = "plan_style.border_header",
              doc = "the rule round the column heading (plan_style(border_header = ))"),
    tfl_param("border_group", NULL, type = "list", multiple = FALSE, fields = c("edge", "line"),
              doc = paste("a rule under each block (plan_style(border_group = ), e.g. list(bottom = TRUE));",
                          "NULL: none")),
    tfl_param("style", "standard", type = "text", study = "settings.style", unset = "step_off",
              doc = paste("the style set (font, borders, page) the report starts from: a set of the",
                          "study's defaults.yml `styles:` or a built-in one (standard, stf_t02);",
                          "unset: none")))
  names(p) <- vapply(p, `[[`, "", "name")
  for (k in names(defaults)) {
    if (!k %in% names(p)) .ard_stop(sprintf(".tpl_demog_params(): no parameter %s.", k))
    p[[k]]["default"] <- list(defaults[[k]])
  }
  fixed_value <- union("denominator", fixed_value)
  for (k in c(fixed, fixed_value)) p[[k]]["study"] <- list(FALSE)
  for (k in fixed_value) {
    p[[k]]$hidden <- TRUE
    p[[k]]$fixed_value <- TRUE
  }
  p[[1L]]$hidden <- TRUE
  unname(p)
}

# The continuous statistics the cells name: their tokens ({mean:.1f} -> mean),
# in order; {q1} and {q3} are the ARD's p25 and p75
.tpl_cell_stats <- function(cells) {
  txt <- unlist(cells, use.names = FALSE)
  tok <- unlist(regmatches(txt, gregexpr("\\{[A-Za-z][A-Za-z0-9_.]*", txt)))
  tok <- sub("^\\{", "", tok)
  tok[tok == "q1"] <- "p25"
  tok[tok == "q3"] <- "p75"
  unique(tok)
}

# The cells with {q1} / {q3} written as the ARD's p25 / p75
.tpl_cell_alias <- function(x) {
  if (is.list(x)) return(lapply(x, .tpl_cell_alias))
  if (!is.character(x)) return(x)
  x <- gsub("\\{q1([}:])", "{p25\\1", x)
  gsub("\\{q3([}:])", "{p75\\1", x)
}

# The Mean (SD) cell with the SD's missing mark (`sd_missing`), or with no
# guard at all when the parameter is unset (NULL): the step is not made.  Cells
# a report gave are left as given.
.tpl_demog_cells_sd <- function(cells, sd_missing) {
  if (!identical(cells, .tpl_demog_cells) || !"Mean (SD)" %in% names(cells)) return(cells)
  if (is.null(sd_missing)) {
    cells[["Mean (SD)"]] <- "{mean} ({sd})"
    return(cells)
  }
  .tpl_sd_missing(cells, .tpl_demog_cells, sd_missing, mean = "{mean}", sd = "{sd}")
}

# `widths = "auto"` of a template report that was resolved with no code list for `by`
# (the arms are not known then): the arms come from the ARD when the plan is made, as
# tfl_tpl() takes them, so the report and the program's call agree.  The plan_columns
# item is where the template puts it (before plan_paginate_rows); a plan_columns the
# layers or the report give is left alone.
.tpl_auto_widths <- function(output, data) {
  tp <- attr(output, "template")
  if (is.null(tp) || !identical(output$kind, "table") || !is.data.frame(data)) return(output$content)
  t <- tryCatch(.tpl_get(tp$name, "tfl_content_run()"), error = function(e) NULL)
  if (is.null(t$params$widths)) return(output$content)
  w <- if ("widths" %in% names(tp$args)) tp$args$widths else t$params$widths$default
  by <- tp$args$by
  if (!identical(w, "auto") || !is.character(by) || length(by) != 1L || !is.null(output$codelists[[by]]) ||
      !all(c("group1", "group1_level") %in% names(data))) return(output$content)
  fns <- vapply(output$content, function(it) it$fn, "")
  if ("plan_columns" %in% fns) return(output$content)
  arms <- unique(as.character(unlist(data$group1_level[!is.na(data$group1) & data$group1 == by])))
  arms <- arms[!is.na(arms) & nzchar(arms)]
  if (!length(arms)) return(output$content)
  total <- if ("total" %in% names(tp$args)) tp$args$total else t$params$total$default
  n <- length(arms) + isTRUE(total)
  it <- tfl_item("plan_columns", list(widths = c(35, rep(65 / n, n))), from = paste0("template:", tp$name))
  at <- match("plan_paginate_rows", fns)
  if (is.na(at)) c(output$content, list(it)) else append(output$content, list(it), after = at - 1L)
}

# The Missing row's data step: #770's (.tpl_missing_level_code()) with the
# label `label`.  It runs on the data with the code lists on
# (`categorical_levels`, the report's): a variable with a list keeps its
# values and gets the label last only when it has missing values, as one
# without a list; none has a Missing row of 0 (canon 1.31).
.tpl_demog_missing_code <- function(set, vars, label = "Missing") {
  if (identical(label, "Missing")) return(.tpl_missing_level_code(set, vars))
  q <- encodeString(label, quote = "\"")
  v <- paste0("c(", paste0("\"", vars, "\"", collapse = ", "), ")")
  paste0(
    "dplyr::mutate(", set, ", dplyr::across(dplyr::any_of(", v, "), function(x) {\n",
    "  chr <- trimws(as.character(x))\n",
    "  miss <- is.na(chr) | !nzchar(chr)\n",
    "  if (!any(miss)) return(x)\n",
    "  chr[miss] <- ", q, "\n",
    "  values <- if (is.factor(x)) levels(x) else sort(unique(chr[!miss]))\n",
    "  factor(chr, levels = unique(c(values, ", q, ")))\n",
    "}))")
}

.tpl_demog_body <- function(a, ctx) {
  cont <- a$continuous
  cat <- a$categorical
  tn <- ctx$template$name %||% "demog"
  if (!length(cont) && !length(cat)) {
    .ard_stop(sprintf(paste0("template %s: give `continuous` or `categorical`, or both ",
                             "(the analysis variables are required: they differ in each study's shell)."), tn))
  }
  # a value the template fixes (canon 1.30) is no parameter of it (a call
  # that passes the fixed value itself on, as a company's template built on
  # this one's parameters does, changes nothing and goes through)
  fx <- Filter(function(p) isTRUE(p$fixed_value), .tpl_get(tn, "template")$params)
  given_fx <- c(intersect(names(fx), ctx$unset),
                Filter(function(k) !identical(a[[k]], fx[[k]]$default), intersect(names(fx), ctx$given)))
  if (length(given_fx)) {
    .ard_stop(sprintf("%s: there is no `%s` parameter (the template fixes it): leave it out.",
                      tn, given_fx[1L]))
  }
  nn <- function(x) Filter(Negate(is.null), x)
  pop <- a$population
  subj <- paste0("adsl_", tolower(pop))
  # the categorical variables' percentages are over the analysis set (canon
  # 1.31, #770): the report's ADSL after its population and filters, the set
  # GROUPN counts -- the column headings' N -- passed explicitly; not a parameter
  cat_den <- subj
  hn <- if (is.null(a$header_n)) "none" else .tpl_header_n_stack(a, ctx, den = NULL)
  grp_set <- subj
  sets <- list(list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
                    population = pop))
  set <- subj
  if (!identical(toupper(a[["data"]]), "ADSL")) {
    set <- a[["data_id"]] %||% paste0(tolower(a[["data"]]), "_", tolower(pop))
    sets <- c(sets, list(list(id = set, from = a[["data"]], subjects = subj)))
  }
  # the Missing row (#770's): a categorical variable that has missing values
  # (NA or blank) gets a level of them, last; decided on the data as the
  # program runs, so a variable with none has no such row
  ml <- if (isTRUE(a$missing_row)) a$missing_label else NULL
  if (isTRUE(a$missing_row) && (!is.character(ml) || length(ml) != 1L || is.na(ml) || !nzchar(ml))) {
    .ard_stop(sprintf("template %s: `missing_label` is one text (\"Missing\").", tn))
  }
  # the values per variable (shown even when 0): the code lists and the order
  vals <- a$categorical_levels
  if (length(vals) && (is.null(names(vals)) || !all(nzchar(names(vals))))) {
    .ard_stop(sprintf("template %s: `categorical_levels` is named by variable: RACE: [ASIAN, WHITE].", tn))
  }
  bad <- setdiff(names(vals), cat)
  if (length(bad) && "categorical_levels" %in% ctx$given) {
    .ard_stop(sprintf("template %s: `categorical_levels` names %s, not in `categorical`.", tn,
                      paste(bad, collapse = ", ")))
  }
  # (the template's own default names the values of variables the report may not use)
  vals <- vals[names(vals) %in% cat]
  vals <- lapply(vals, function(v) as.character(unlist(v)))
  # (the lists have no Missing value: the Missing step, after them, adds the
  # level to a variable that has missing values only -- canon 1.31)
  codelists <- if (length(vals)) {
    cl <- lapply(vals, function(v1) lapply(v1, function(x) list(value = x, label = x)))
    Filter(length, cl)
  }
  lev <- vals
  cat_set <- set
  if (!is.null(ml) && length(cat)) {
    cat_set <- paste0(set, "_m")
    sets <- c(sets, list(list(
      id = cat_set, label = "with a Missing level for a variable that has missing values",
      from = set, code = tfl_r(.tpl_demog_missing_code(set, cat, ml)))))
  }
  ov <- if (isTRUE(a[["total"]])) TRUE
  # (the statistics computed: those `formats` names, and any the cells name)
  # (NULL: the active catalog's; `!unset`: none, the ARD's own)
  # (a fixed `formats`, stf_t02_demog's, is the whole set: no catalog under it)
  fm <- if ("formats" %in% ctx$unset) list() else if ("formats" %in% names(fx)) a$formats else
    .tpl_merge_named(.tpl_demog_formats_active(), a$formats)
  nested <- fm[vapply(fm, is.list, NA)]
  flat <- fm[!vapply(fm, is.list, NA)]
  cont_flat <- flat[!names(flat) %in% c("n", "p")]
  stats <- unique(c(names(cont_flat), .tpl_cell_stats(a$cells)))
  fmt_text <- function(vars, keep) {
    one <- function(v) as.character(v)[1L]
    k <- flat[names(flat) %in% keep]
    parts <- if (length(k)) paste0(names(k), "=", vapply(k, one, "")) else character()
    for (v in intersect(names(nested), vars)) {
      for (st in names(nested[[v]])) parts <- c(parts, paste0(v, ":", st, "=", one(nested[[v]][[st]])))
    }
    if (length(parts)) paste(parts, collapse = " | ")
  }
  analyses <- c(
    list(nn(list(id = "GROUPN", label = "Subjects per group", method = "categorical", data = grp_set,
                 variables = a$by, purpose = "PRIMARY OUTCOME MEASURE"))),
    if (length(cont)) list(nn(list(
      id = "CONT", label = "Continuous", method = "continuous", data = set,
      by = a$by, overall = ov, variables = cont,
      statistics = if (length(stats)) stats,
      formats = fmt_text(cont, names(cont_flat)),
      purpose = "PRIMARY OUTCOME MEASURE"))),
    if (length(cat)) list(nn(list(
      id = "CAT", label = "Categorical", method = "categorical", data = cat_set,
      by = a$by, overall = ov, variables = cat, statistics = c("n", "p"),
      formats = fmt_text(cat, c("n", "p")),
      denominator = cat_den, purpose = "PRIMARY OUTCOME MEASURE"))))
  vars <- c(cont, cat)
  cells <- nn(list(
    continuous = if (length(cont) && length(a$cells)) as.list(
      .tpl_cell_alias(.tpl_demog_cells_sd(a$cells, a[["sd_missing"]]))),
    categorical = a$count_cell))
  # the arms and the columns of the table, for the widths
  w <- a$widths
  widths_item <- NULL
  if (is.character(w) && length(w) == 1L && identical(w, "auto")) {
    arms <- .tpl_levels(a$by, ctx$codelists)[[a$by]]
    ncol_val <- if (length(arms)) length(arms) + isTRUE(a$total) else NA_integer_
    if (!is.na(ncol_val)) {
      widths_item <- tfl_item("plan_columns", list(widths = c(35, rep(65 / ncol_val, ncol_val))))
    }
  } else if (length(w) && is.numeric(unlist(w)) && all(unlist(w) > 0) && length(names(w)) &&
             all(nzchar(names(w)))) {
    # (by column name, as the study's plan_columns(widths = ) gives them)
    widths_item <- tfl_item("plan_columns", list(widths = as.list(w)))
  } else if (length(w) && is.numeric(unlist(w)) && all(unlist(w) > 0)) {
    widths_item <- tfl_item("plan_columns", list(widths = as.numeric(unlist(w))))
  } else if (length(w)) {
    .ard_stop(sprintf(paste0("template %s: `widths` is \"auto\", the relative widths ",
                             "(positive numbers), one a printed column, or the widths by column name."), tn))
  }
  # the arm heading: a text ({col} and the N on a line), or the lines a study gives
  ch <- a$col_header
  hdr <- NULL
  if (is.character(ch) && length(ch) == 1L) {
    txt <- ch
    if (identical(hn, "none")) {
      ln <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
      txt <- paste(ln[!grepl("{n}", ln, fixed = TRUE)], collapse = "\n")
    }
    hdr <- tfl_item("plan_col_header", list(header = list(
      nn(list(line = "1", cols = "row_label", text = .tpl_lines(a$heading))),
      list(line = "1", cols = ".values", span = "each", text = txt))))
  } else if (length(ch) && is.list(ch) && all(vapply(ch, function(e) is.list(e) && "cols" %in% names(e), NA))) {
    # (a list of cell records, line / cols / text: plan_col_header(header = ))
    hdr <- tfl_item("plan_col_header", list(header = unname(ch)))
  } else if (length(ch)) {
    hdr <- tfl_item("plan_col_header", list(lines = ch))
  }
  # the blank rows
  bl <- a$blanks
  blank <- NULL
  if (!is.null(bl) || length(nn(list(a$blanks_first, a$blanks_last, a$blanks_counted)))) {
    off <- identical(bl, FALSE)
    # (FALSE: no `where` either; a code SPEC written from the report,
    # tfl_template_to_spec(), writes it `!unset` when the study's layers below
    # give one: bug 9)
    where <- if (isTRUE(bl)) "between_groups" else if (is.character(bl)) bl
    blank <- tfl_item("plan_blanks", nn(list(
      where = where,
      # (blanks = FALSE is no blank rows at all, whatever is said of the ends)
      first = if (off) FALSE else a$blanks_first,
      last = if (off) FALSE else a$blanks_last,
      counted = a$blanks_counted)))
  }
  # the stub: the study's name and place, and the heading
  stub <- NULL
  if (length(nn(list(a$stub_name, a$stub_before, a$heading)))) {
    hd <- !is.null(a$heading)
    stub <- tfl_item("plan_stub", nn(list(
      name = a$stub_name %||% if (hd) "row_label",
      before = a$stub_before %||% if (hd) TRUE,
      heading = if (hd) paste(a$heading, collapse = "\n"))))
  }
  # (a rule written as its edges, list(bottom = TRUE), is rtf_border(bottom = TRUE))
  rule <- function(x) if (is.list(x) && !inherits(x, c("tfl_call", "tfl_r"))) tfl_call("rtf_border", x) else x
  look <- nn(list(border_header = rule(a$border_header), border_group = rule(a$border_group)))
  ord <- c(list(variable = a$order), lev)
  ord <- ord[!vapply(ord, is.null, NA)]
  content <- c(
    list(tfl_item("table_plan", list(cols = a$by, rows = list(group = "variable")))),
    if (!is.null(a$rounding)) list(tfl_item("plan_digits", list(rounding = a$rounding))),
    if (length(cells)) list(tfl_item("plan_cells", cells)),
    if (length(ord)) list(tfl_item("plan_levels", ord)),
    if (length(a$labels) && length(intersect(vars, names(a$labels)))) list(tfl_item(
      "plan_labels", list(variable = as.list(a$labels[intersect(vars, names(a$labels))])))),
    if (isTRUE(a[["total"]])) list(tfl_item("plan_total", nn(list(
      label = a$total_label, position = "last")))),
    if (!is.null(hdr)) list(hdr),
    if (!is.null(blank)) list(blank),
    if (!is.null(stub)) list(stub),
    if (length(look)) list(tfl_item("plan_style", look)),
    if (!is.null(widths_item)) list(widths_item),
    # (a block is never split over pages)
    list(tfl_item("plan_paginate_rows", nn(list(
      max_rows = if (!is.null(a$max_rows)) as.integer(a$max_rows), split = "group_safe")))))
  out <- list(ard = list(analysis_data = sets, analyses = analyses),
              data = list(source = "ard", object = "ard"), content = content)
  if (length(codelists)) out$codelists <- codelists
  out
}

# Where each parameter acts: what is computed / how it is shown
.tpl_demog_parts <- list(
  ard = c("by", "data", "data_id", "subset", "continuous", "categorical", "total", "denominator",
          "formats", "missing_row", "missing_label"),
  content = c("order", "labels", "categorical_levels", "cells", "sd_missing", "count_cell",
              "total_label", "header_n", "col_header", "heading", "stub_name", "stub_before",
              "widths", "max_rows", "rounding", "blanks", "blanks_first", "blanks_last", "blanks_counted",
              "border_header", "border_group", "style"))

# (`extends`: its family's `[defaults]`, canon 1.27: the study's defaults are
# laid under the template's items, and its references read them)
.tfl_template("demog", kind = "table", layout = "summary", label = "Demographics",
              params = .tpl_demog_params(), body = .tpl_demog_body, ai = TRUE,
              parts = .tpl_demog_parts, extends = "defaults",
              example = list(population = "SAF", by = "TRT01A", continuous = "AGE",
                             categorical = c("AGEGR1", "SEX", "RACE", "ETHNIC")))

# stf_t02_demog: the look of FDA's Standard Safety Tables and Figures Integrated
# Guide v2.0 (April 2025), section 2.1.2, Table 2, "Demographics and Baseline
# Clinical Characteristics".  A template of its own with other defaults -- the
# labels, cells, Total and Missing, the values shown, the heading text, no blank
# rows, the style set "stf_t02" (fixed: the look is the template's, so no
# reference to the study's style; the set also holds the rule under each block,
# `border_group`, so the parameter stays at its none default here).  Its set is
# values only (canon 1.30): no default refers to the study standard, and the
# digits (`formats`), the % denominator and the Missing row are fixed.  The
# variables are not among them (canon 1.12): `continuous` and `categorical` are
# required.  It
# follows the guide's style; it is not endorsed by FDA and not an official FDA
# format.
.tpl_stf_t02_defaults <- list(
  order = c("SEX", "AGE", "AGEGR1", "RACE", "ETHNIC", "COUNTRY"),
  labels = c(SEX = "Sex, n (%)", AGE = "Age, years", AGEGR1 = "Age groups (years), n (%)",
             RACE = "Race, n (%)", ETHNIC = "Ethnicity, n (%)",
             COUNTRY = "Country of participation, n (%)"),
  cells = list(`Mean (SD)` = "{mean} ({sd})", `Median (min, max)` = "{median} ({min}, {max})"),
  count_cell = "{n} ({p})",
  categorical_levels = list(
    RACE = c("AMERICAN INDIAN OR ALASKA NATIVE", "ASIAN", "BLACK OR AFRICAN AMERICAN",
             "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "WHITE", "MULTIPLE", "OTHER",
             "NOT REPORTED", "UNKNOWN"),
    ETHNIC = c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO", "NOT REPORTED", "UNKNOWN")),
  total = TRUE, total_label = "Total Population", missing_row = TRUE,
  col_header = "{col}\nN={n}", heading = "Characteristic", widths = "auto",
  blanks = FALSE, style = "stf_t02",
  # (fixed, canon 1.30: the digits are the template's -- the built-in
  # catalog's values -- not the study's or a company catalog's)
  formats = list(N = 0L, mean = 1L, sd = 2L, median = 1L, min = 0L, max = 0L, p = "xx.x%"))

.tfl_template("stf_t02_demog", kind = "table", layout = "summary",
              label = "Demographics (FDA STF Table 2)",
              params = .tpl_demog_params(
                .tpl_stf_t02_defaults,
                # (canon 1.30: no reference to the study standard, values only; the
                # digits and the Missing row fixed, as the % denominator is)
                fixed = c("col_header", "stub_name", "stub_before", "blanks", "blanks_first",
                          "blanks_last", "blanks_counted", "border_header", "style", "cells",
                          "count_cell", "sd_missing", "rounding", "widths", "max_rows"),
                fixed_value = c("formats", "missing_row", "style")),
              body = .tpl_demog_body, ai = FALSE, parts = .tpl_demog_parts,
              # (canon 1.27, 1.30: the study's content is not laid, fixed)
              extends = list(content = "none"), extends_fixed = "content",
              example = list(population = "SAF", by = "TRT01A", continuous = "AGE",
                             categorical = c("SEX", "AGEGR1", "RACE", "ETHNIC", "COUNTRY")))
