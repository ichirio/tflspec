# Every argument of every code-SPEC function (tfl_schema()'s functions that
# are no figure's, and the two row cards `data` and `analysis`) through the
# YAML files and into the code: for each value arg_samples() makes, the
# report with the argument set is written (tfl_write_output()), read back
# (tfl_read_output()) and must give the argument identical (after the
# reader's own normalisation only); its code (tfl_content_code(),
# tfl_report_code(), and tfl_ard_code() for the ARD's parts) must differ
# from the code with the argument absent.  dev-rep RULES section 4: whether
# everything is there is a test, not a review.  Fictional data only.

# Where each function's arguments go in a report: `out` the report,
# `where` its part (`content` an item; `page` a key of page:; `data` the
# second data row; `analysis` an analysis; `ard` an analysis of the
# function's method; `call` the value of another function's argument),
# `base` the arguments it always has.
ea_hosts <- function(fn) {
  ent <- tfl_schema(fn)
  page_fns <- c("rtf_page", "rtf_style", "rtf_default_format", "rtf_document", "rtf_tables",
                "rtf_figures", "rtf_titles", "rtf_footnotes", "generate_rtfreport",
                "rtf_header", "rtf_footer")
  calls <- list(
    rtf_border = list(host = "plan_style", arg = "border_header"),
    rtf_border_line = list(host = "plan_style", arg = "border_header", inner = "rtf_border",
                           inner_arg = "top"),
    rtf_col_header = list(host = "plan_col_header", arg = "header"),
    cell_rows = list(host = "plan_cells", arg = "continuous"),
    continuous_summary_fns = list(host = "ard_summary", arg = "statistic", key = "AGE"),
    listing_col = list(host = "plan_listing", arg = "cols", list = TRUE),
    rtf_watermark = list(host = "rtf_document", arg = "watermark"))
  base <- ea_base[[fn]] %||% list()
  if (fn %in% names(calls)) {
    h <- calls[[fn]]
    return(c(list(where = "call", out = ea_hosts(h$host)$out, base = base), h))
  }
  out <- switch(fn, plan_listing = "L-1", rtf_figures = "F-1", "T-14-1-1")
  if (fn %in% page_fns) return(list(where = "page", out = out, base = base))
  if (fn %in% c("data", "analysis")) return(list(where = fn, out = out, base = base))
  if (identical(ent$package, "cards") || identical(ent$package, "cardx")) {
    return(list(where = "ard", out = out, base = base, method = paste0(ent$package, "::", fn)))
  }
  list(where = "content", out = out, base = base)
}

# the arguments a function's item always has (its required ones, those its
# code needs)
ea_base <- list(
  data = list(id = "adae_sub", from = "ADAE"),
  analysis = list(id = "CONT", method = "cards::ard_summary", data = "adsl_sub", by = "TRT01A",
                  variables = "AGE"),
  ard_summary = list(by = "TRT01A", variables = "AGE"),
  ard_tabulate = list(by = "TRT01A", variables = "SEX"),
  ard_tabulate_value = list(by = "TRT01A", variables = "SEX"),
  ard_hierarchical = list(by = "TRT01A", variables = "SEX"),
  ard_stack_hierarchical = list(by = "TRT01A", variables = "SEX"),
  ard_missing = list(by = "TRT01A", variables = "AGE"),
  ard_categorical_ci = list(by = "TRT01A", variables = "SEX"),
  ard_stats_t_test = list(by = "TRT01A", variables = "AGE"),
  ard_stats_wilcox_test = list(by = "TRT01A", variables = "AGE"),
  ard_stats_chisq_test = list(by = "TRT01A", variables = "SEX"),
  ard_stats_fisher_test = list(by = "TRT01A", variables = "SEX"),
  ard_stats_kruskal_test = list(by = "TRT01A", variables = "AGE"),
  ard_total_n = list(by = "TRT01A"),
  ard_stack = list(by = "TRT01A"),
  ard_strata = list(by = "TRT01A"),
  ard_stats_oneway_test = list(by = "TRT01A"),
  plan_append = list(name = "Part 2", data = list(source = "ard"),
                     content = list(tfl_item("table_plan", list(cols = "TRT01A"))))
)

# arguments a case needs beside the one walked, by `fn.arg` (for it and for
# the case with it absent)
ea_with <- list(
  data.add = list(population = "SAF"),
  analysis.conditions = list(params = "sexv", where = tfl_r("SEX == \"{sexv}\"")),
  analysis.code = list(method = "custom")
)

# An argument whose value changes nothing in the code, and why (one a line;
# `fn.arg`, or `fn.arg=value` for one value).
ea_allow <- c(
  rtf_tables.space_before = "rtfreporter takes it only with page_break: false (.page_flow_args())",
  rtf_figures.space_before = "rtfreporter takes it only with page_break: false (.page_flow_args())",
  generate_rtfreport.overwrite = "the run writes the RTF (tfl_runner_code()), not the report's code",
  analysis.params = "params name the conditions' columns: with no conditions they change nothing",
  `analysis.overall=FALSE` = "FALSE is what an analysis with no overall does",
  `data.subjects_in="none"` = "none keeps all: what a row with no subjects_in does (#927)"
)

# Arguments named as an analysis's own key (its id, its method): YAML cannot
# write them as keys of the analysis -- the key is the analysis's own -- so
# they go in its `args:` (R), tested in test-ard-yaml-keys.R; not walked
ea_own_key <- c("ard_hierarchical.id", "ard_stack_hierarchical.id", "ard_categorical_ci.method")

# Cases a GitHub issue holds (each skipped with its number): `fn.arg` for
# every value, `fn.arg=!unset` or `fn.arg=<value>` for one
ea_issues <- c(
  # !unset in a data row or an analysis stops the ARD program
  as_tag("tflspec#926", paste0(c(
    paste0("data.", c("population", "subjects_in", "derive", "arrange", "add")),
    paste0("analysis.", c("statistics", "denominator", "overall", "formats", "post")),
    paste0(c("ard_tabulate", "ard_tabulate_value", "ard_hierarchical", "ard_stack_hierarchical",
             "ard_categorical_ci"), ".denominator"),
    "ard_stack_hierarchical.overall"), "=!unset"))
)

# the issue a walk's row is held by, or NA
ea_issue_of <- function(r, issues = ea_issues) {
  k1 <- paste0(r$fn, ".", r$arg, "=", ifelse(r$unset, "!unset", r$value))
  k2 <- paste0(r$fn, ".", r$arg)
  unname(ifelse(k1 %in% names(issues), issues[k1], issues[k2]))
}

# a report with the function's arguments `args` in its place
ea_put <- function(o, fn, h, args) {
  switch(h$where,
    content = {
      i <- which(vapply(o$content, `[[`, "", "fn") == fn)[1L]
      if (is.na(i)) o$content[[length(o$content) + 1L]] <- tfl_item(fn, args) else
        o$content[[i]]$args <- args
    },
    page = o$page[[fn]] <- args,
    data = {
      # (a new id: the analyses that read the row read it by its new name)
      old <- o$ard$analysis_data[[2L]]$id
      o$ard$analysis_data[[2L]] <- args
      new <- args$id
      if (is.character(new) && !identical(new, old)) {
        o$ard$analyses <- lapply(o$ard$analyses, function(a) {
          if (identical(a$data, old)) a$data <- new
          a
        })
      }
    },
    analysis = o$ard$analyses[[2L]] <- args,
    ard = {
      a <- list(id = "WALK", method = h$method, data = "adsl_sub")
      a[names(args)] <- args
      o$ard$analyses[[length(o$ard$analyses) + 1L]] <- a
    },
    call = {
      v <- tfl_call(fn, args)
      if (!is.null(h$inner)) v <- tfl_call(h$inner, stats::setNames(list(v), h$inner_arg))
      if (!is.null(h$key)) v <- stats::setNames(list(v), h$key)
      if (isTRUE(h$list)) v <- list(v)
      hh <- ea_hosts(h$host)
      a <- hh$base
      a[h$arg] <- list(v)
      o <- ea_put(o, h$host, hh, a)
    })
  o
}

# the function's arguments, read back from the report
ea_get <- function(o, fn, h) {
  switch(h$where,
    content = {
      i <- which(vapply(o$content, `[[`, "", "fn") == fn)[1L]
      if (is.na(i)) NULL else o$content[[i]]$args
    },
    page = o$page[[fn]],
    data = o$ard$analysis_data[[2L]],
    analysis = o$ard$analyses[[2L]],
    ard = {
      a <- o$ard$analyses[[length(o$ard$analyses)]]
      if (identical(a$id, "WALK")) a[setdiff(names(a), c("id", "method", "data"))]
    },
    call = {
      v <- ea_get(o, h$host, ea_hosts(h$host))[[h$arg]]
      if (isTRUE(h$list)) v <- v[[1L]]
      if (!is.null(h$key)) v <- v[[h$key]]
      if (!is.null(h$inner)) v <- v$args[[h$inner_arg]]
      v$args
    })
}

# the reader's own normalisation of a value, where it has one: a named
# vector is a map (as_read()); a data row's derive map its pieces
ea_norm <- function(fn, key, v) {
  if (identical(fn, "data") && identical(key, "derive") && is.list(v) && !is.object(v)) {
    return(tflspec:::.derive_map_in(list(derive = v))$derive)
  }
  as_read(v)
}

# the report's code: its content, its document, and for the ARD's parts the
# ARD program (an error is its message); `resolved` the report resolved
ea_code <- function(st, o, ard) {
  one <- function(expr) tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))
  r <- one(tfl_resolve_output(o, st))
  code <- c(if (inherits(r, "tfl_output")) one(suppressMessages(tfl_content_code(r, study = st))) else r,
            one(tfl_report_code(o, study = st)),
            if (ard) one(suppressMessages(tfl_ard_code(st, output_id = o$id, save = FALSE))))
  list(code = code, resolved = if (inherits(r, "tfl_output")) r)
}

# write the report into the study, read it back
ea_write_read <- function(d, o) {
  f <- file.path(d, "spec", "outputs", paste0(o$id, ".yml"))
  tfl_write_output(o, f)
  tfl_read_output(f)
}

# one function's walk: a row a value (fn, arg, value, read, changed, error,
# layered: the layers below give the same value)
ea_walk_fn <- function(d, outs, fn, first_only = FALSE) {
  h <- ea_hosts(fn)
  o0 <- outs[[h$out]]
  on.exit(ea_write_read(d, o0), add = TRUE)
  defs <- tfl_arg_defs(fn = fn)
  # (the study's files but the reports' do not change: read once; the ARD's
  # code reads the reports from the folder)
  st <- tfl_read_study_spec(d)
  ard <- h$where %in% c("data", "analysis", "ard") ||
    identical(h$where, "call") && identical(ea_hosts(h$host)$where, "ard")
  # (the checks' warnings -- an argument the schema does not know -- are
  # not what is walked)
  run <- function(args) suppressWarnings({
    back <- ea_write_read(d, ea_put(o0, fn, h, args))
    c(list(back = back), ea_code(st, back, ard))
  })
  absent_cache <- list()
  rows <- list()
  for (i in seq_len(nrow(defs))) {
    def <- defs[i, ]
    vals <- arg_samples(def, fn)
    # (on CI: the first value of each argument)
    if (first_only) vals <- vals[1L]
    key <- if (identical(def$name, "...")) {
      if (identical(def$dots_style, "positional")) def$dots_key else "AGE"
    } else def$name
    base <- h$base
    base[names(ea_with[[paste(fn, def$name, sep = ".")]])] <- ea_with[[paste(fn, def$name, sep = ".")]]
    absent <- base[setdiff(names(base), key)]
    ak <- as_key(absent)
    if (is.null(absent_cache[[ak]])) absent_cache[[ak]] <- run(absent)
    a0 <- absent_cache[[ak]]
    for (v in vals) {
      args <- base
      args[key] <- list(v)
      got <- run(args)
      rb <- ea_get(got$back, fn, h)[[key]]
      want <- ea_norm(fn, key, v)
      below <- if (!is.null(a0$resolved)) ea_get(a0$resolved, fn, h)[[key]]
      err <- grep("^ERROR:", got$code, value = TRUE)
      rows[[length(rows) + 1L]] <- data.frame(
        fn = fn, arg = def$name, value = as_key(v),
        read = identical(rb, want), read_as = as_key(rb),
        changed = !identical(got$code, a0$code),
        layered = !is.null(below) && identical(as_read(below), want),
        error = paste(setdiff(err, grep("^ERROR:", a0$code, value = TRUE)), collapse = " | "),
        unset = inherits(v, "tfl_unset"), stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

# what a walk's rows say is wrong: one line a case
ea_problems <- function(r) {
  if (is.null(r) || !nrow(r)) return(character())
  allowed <- paste(r$fn, r$arg, sep = ".") %in% names(ea_allow) |
    paste0(r$fn, ".", r$arg, "=", r$value) %in% names(ea_allow)
  bad <- !r$read | nzchar(r$error) | (!r$changed & !r$unset & !r$layered & !allowed)
  r <- r[bad, , drop = FALSE]
  if (!nrow(r)) return(character())
  sprintf("%s(%s = %s): %s", r$fn, r$arg, substr(r$value, 1L, 80L),
          ifelse(!r$read, paste("read back as", substr(r$read_as, 1L, 80L)),
                 ifelse(nzchar(r$error), substr(r$error, 1L, 200L), "the code does not change")))
}

ea_fns <- function() {
  s <- tfl_schema()
  c(names(s)[!vapply(s, function(e) !is.null(e$figure), NA)], "data", "analysis")
}

test_that("arg_samples() stops on a type it does not know, naming the argument", {
  def <- tfl_arg_defs(fn = "plan_sort")[1L, ]
  def$type <- "widget<string>"
  expect_error(arg_samples(def, "plan_sort"), "plan_sort\\(stat\\).*type kind \"widget\"")
  def$input <- "slider"
  expect_error(arg_samples(def, "plan_sort"), "plan_sort\\(stat\\).*input form \"slider\"")
})

test_that("arg_samples() knows every argument of the code SPEC", {
  n <- 0L
  for (fn in ea_fns()) {
    defs <- tfl_arg_defs(fn = fn)
    for (i in seq_len(nrow(defs))) {
      v <- arg_samples(defs[i, ], fn)
      n <- n + 1L
      expect_true(length(v) >= 1L && length(v) <= 4L, label = paste(fn, defs$name[i]))
    }
  }
  expect_gt(n, 350L)
})

test_that("every code-SPEC argument survives the YAML file and reaches the code", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- as_study(withr_tempdir())
  outs <- tfl_read_outputs(d)
  # (on CI: one value an argument; locally every value)
  first_only <- identical(Sys.getenv("CI"), "true")
  for (fn in ea_fns()) {
    r <- ea_walk_fn(d, outs, fn, first_only = first_only)
    p <- ea_problems(r[is.na(ea_issue_of(r)) & !paste(r$fn, r$arg, sep = ".") %in% ea_own_key, ,
                       drop = FALSE])
    expect(length(p) == 0L, paste(c(paste0(fn, ":"), p), collapse = "\n  "))
  }
})

# the cases an issue holds: they pass once it is fixed (then take them off
# ea_issues and this skip)
for (iss in unique(ea_issues)) {
  local({
    n <- iss
    test_that(paste0("the code-SPEC cases of ", n, " survive the YAML file"), {
      skip(n)
      d <- as_study(withr_tempdir())
      outs <- tfl_read_outputs(d)
      fns <- unique(sub("[.].*$", "", names(ea_issues)[ea_issues == n]))
      for (fn in fns) {
        r <- ea_walk_fn(d, outs, fn)
        p <- ea_problems(r[ea_issue_of(r) %in% n, , drop = FALSE])
        expect(length(p) == 0L, paste(c(paste0(fn, ":"), p), collapse = "\n  "))
      }
    })
  })
}
