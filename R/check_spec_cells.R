# ============================================================================
#  tfl_check_spec(): every cell template of a report, before any build
# ----------------------------------------------------------------------------
#  An AI draft (or a hand-written one) can write a cell the build then prints
#  wrongly, or not at all.  The warnings (never errors; each says the fix):
#
#  - a `{token}` naming a statistic the report cannot have (`{pct}`, `{Mean}`,
#    `{n}` in a continuous row): the statistics are the ARD's -- the catalog
#    tfl_ard_statistics() by kind, what the report's analyses and the
#    template's own defaults add;
#  - a format after `:` that rtfreporter cannot format (`{mean:.1}`);
#  - a percentage written with no `%` in its format (`{p:.1f}` prints the
#    proportion, 0.3, not 30.0).
#
#  A bare `{mean}` and `{x:stat_fmt}` read the ARD's own format: never warned.
#  Where the statistics cannot be known (the report's own `!r` ARD, an
#  analysis written as code) the statistics are not checked.
#
#  The cells are the report's: plan_cells() / plan_compare() entries (a
#  template, a list of guards and templates, cell_rows()), and the template
#  arguments that are cell templates.
# ============================================================================

# The template parameters that are cell templates (the registry types them
# as text or as a list of label / template)
.cell_params <- c("cells", "count_cell", "cell", "cat_cell")

# The templates in one value of a cell: text, a list of guards and
# templates, cell_rows() as a call or as R.  An `!r` (a guard, a call) gives
# the texts it quotes: its code is not read.
.cell_templates <- function(x) {
  if (inherits(x, "tfl_r")) {
    m <- regmatches(unclass(x), gregexpr("\"([^\"\\\\]|\\\\.)*\"|'([^'\\\\]|\\\\.)*'", unclass(x)))[[1L]]
    return(substr(m, 2L, nchar(m) - 1L))
  }
  if (inherits(x, "tfl_call")) return(.cell_templates(x$args))
  if (is.character(x)) return(as.character(x))
  if (is.list(x)) return(unlist(lapply(unclass(x), .cell_templates), use.names = FALSE))
  character()
}

# {name} and {name:spec}: name and spec of each token of a text
.cell_tokens <- function(txt) {
  tok <- regmatches(txt, gregexpr("[{][^{}]+[}]", txt))[[1L]]
  lapply(tok, function(t) {
    inner <- substr(t, 2L, nchar(t) - 1L)
    at <- regexpr(":", inner, fixed = TRUE)
    if (at < 0L) list(token = t, name = inner, spec = NULL)
    else list(token = t, name = substr(inner, 1L, at - 1L),
              spec = substr(inner, at + 1L, nchar(inner)))
  })
}

# Can rtfreporter format this spec?  Its own formatter answers (a spec is
# fine when it does not stop); `%` alone is a plan's (plan_digits() fills it)
.cell_spec_ok <- function(spec) {
  if (spec %in% c("", "%", "stat", "stat_fmt")) return(TRUE)
  f <- get0(".ard_format_value", envir = asNamespace("rtfreporter"), inherits = FALSE)
  if (is.function(f)) {
    r <- tryCatch(f(0.5, NA_character_, spec, "sas"), error = function(e) e)
    return(!inherits(r, "error"))
  }
  grepl("^([.][0-9]+[fs]|d)%?$", spec)
}

# The spec written as rtfreporter has it, when it can be guessed
.cell_spec_fix <- function(spec) {
  d <- regmatches(spec, regexpr("[0-9]+", spec))
  pct <- grepl("%", spec, fixed = TRUE)
  ty <- if (grepl("s", spec, fixed = TRUE)) "s" else if (identical(sub("%", "", spec, fixed = TRUE), "d")) "d" else "f"
  if (!length(d) && !identical(ty, "d")) return(NULL)
  paste0(if (identical(ty, "d")) "d" else paste0(".", d, ty), if (pct) "%")
}

# The statistics a name may be: the catalog by kind, with what the report's
# analyses name; NULL when they cannot be known (an analysis written as code)
.cell_known <- function(o) {
  ard <- o$ard
  if (inherits(ard, "tfl_r")) return(NULL)
  an <- if (is.list(ard)) ard$analyses else NULL
  extra <- character()
  for (a in an) {
    if (!is.list(a) || inherits(a, "tfl_r") || length(a$code) || inherits(a$method, "tfl_r")) return(NULL)
    st <- as.character(unlist(a$statistics))
    extra <- c(extra, .ars_method_stats(as.character(a$method %||% "")),
               unlist(regmatches(st, gregexpr("[A-Za-z][A-Za-z0-9_.]*", st))))
  }
  cat <- tfl_ard_statistics()
  cont <- cat$statistic[cat$kind == "continuous"]
  catg <- cat$statistic[cat$kind == "categorical"]
  all <- unique(c(cat$statistic, extra))
  # (a continuous row has no n or p; a count's cell no mean)
  list(all = all, categorical = setdiff(all, setdiff(cont, catg)),
       continuous = setdiff(all, setdiff(catg, cont)),
       percent = cat$statistic[grepl("%$", cat$fmt)])
}

# What a name may be in a plan_cells() key: a kind of variable narrows it
.cell_allowed <- function(known, key) {
  if (key %in% c("categorical", "hierarchical", "dichotomous")) return(known$categorical)
  if (identical(key, "continuous")) return(known$continuous)
  known$all
}

.check_cell_rows <- function(o, id) {
  rows <- list(.check_rows())
  add <- function(...) rows[[length(rows) + 1L]] <<- .check_rows(output_id = id, ...)
  known <- .cell_known(o)
  # the template's own defaults name statistics too (a PK cell's `{value}`)
  tpl <- o$template
  tn <- as.character((if (is.list(tpl)) tpl$name else tpl) %||% "")
  own <- character()
  t <- if (length(tn) == 1L && nzchar(tn)) tryCatch(.tpl_get(tn, "tfl_check_spec()"), error = function(e) NULL)
  for (p in t$params) {
    for (x in .cell_templates(p$default)) own <- c(own, vapply(.cell_tokens(x), `[[`, "", "name"))
  }
  # ... and so do the cells the template makes (an override copies them)
  if (!is.null(t)) {
    ex <- tryCatch(suppressMessages(suppressWarnings(tfl_template_expand(tn, tpl$args %||% list()))),
                   error = function(e) NULL)
    for (it in ex$content) {
      if (identical(it$fn, "plan_cells")) {
        for (x in .cell_templates(it$args)) own <- c(own, vapply(.cell_tokens(x), `[[`, "", "name"))
      }
    }
  }
  # one cell: its tokens against the statistics the key allows
  one <- function(txt, at, allowed, key = "") {
    toks <- .cell_tokens(txt)
    has_pct <- grepl("%", txt, fixed = TRUE)
    for (k in toks) {
      if (startsWith(k$name, ".")) next
      if (!is.null(allowed) && !(k$name %in% c(allowed, own))) {
        near <- allowed[tolower(allowed) == tolower(k$name)]
        if (!length(near)) {
          alias <- c(pct = "p", percent = "p", perc = "p", count = "n", total = "N", std = "sd",
                     stdev = "sd", stddev = "sd", q1 = "p25", q3 = "p75", minimum = "min", maximum = "max")
          near <- unname(alias[tolower(k$name)])
          near <- near[!is.na(near) & near %in% allowed]
        }
        shown <- if (identical(key, "categorical")) c("n", "N", "p")
        else if (identical(key, "continuous")) c("N", "mean", "sd", "median", "p25", "p75", "min", "max")
        else utils::head(setdiff(allowed, own), 12L)
        add(part = "content", item = at, level = "warning", message = sprintf(paste0(
          "the cell template \"%s\" names \"%s\", which is no statistic of this cell (the statistics here are %s%s): ",
          "write one of them"), txt, k$name, paste(shown, collapse = ", "),
          if (length(near)) sprintf("; did you mean \"%s\"?", near[1L]) else ""))
        next
      }
      if (is.null(k$spec)) next
      # (Python's `.1%` is the error check_spec_static.R gives)
      if (grepl("^[.][0-9]+%$", k$spec)) next
      if (!.cell_spec_ok(k$spec)) {
        fix <- .cell_spec_fix(k$spec)
        add(part = "content", item = at, level = "warning", message = sprintf(paste0(
          "the cell template \"%s\" has the format \"%s\", which rtfreporter cannot format: ",
          "write %s (the formats are .1f, .1f%% for a percentage, .3s, d, stat, stat_fmt)"),
          txt, k$spec, if (is.null(fix)) sprintf("{%s:.1f}", k$name) else sprintf("{%s:%s}", k$name, fix)))
        next
      }
      if (!is.null(known) && k$name %in% known$percent && grepl("^([.][0-9]+[fs]|d)$", k$spec)) {
        add(part = "content", item = at, level = "warning", message = sprintf(paste0(
          "the cell template \"%s\" shows the proportion %s as it is (0.3, not 30.0): ",
          "write {%s:%s%%} (it multiplies by 100 and prints no %% sign; put the %% in the column heading, ",
          "\"n (%%)\", or after the brace)"), txt, k$name, k$name, k$spec))
      }
    }
  }
  for (it in o$content) {
    if (!is.list(it) || !(it$fn %||% "") %in% c("plan_cells", "plan_compare")) next
    nm <- names(it$args) %||% rep("", length(it$args))
    for (i in seq_along(it$args)) {
      al <- if (is.null(known)) NULL else if (identical(it$fn, "plan_cells")) .cell_allowed(known, nm[i]) else known$all
      for (txt in .cell_templates(it$args[[i]])) one(txt, paste0(it$fn, "(", nm[i], ")"), al, nm[i])
    }
  }
  args <- if (is.list(tpl)) tpl$args %||% list() else list()
  for (a in intersect(names(args), .cell_params)) {
    al <- if (is.null(known)) NULL else if (identical(a, "count_cell")) known$categorical else known$all
    for (txt in .cell_templates(args[[a]])) one(txt, paste0("template$args$", a), al, if (identical(a, "count_cell")) "categorical" else "")
  }
  do.call(rbind, rows)
}
