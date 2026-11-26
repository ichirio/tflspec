# The value maker of the every-argument walks (test-every-arg-yaml.R,
# test-every-param-yaml.R): for one row of tfl_arg_defs(), 2-3 values that
# its type takes and that differ from its default, `!unset` too where the
# argument allows it.  A type or an input form this file does not know stops,
# naming the function and the argument: a new one cannot slip through.
# Fictional values only (the bundled ADaM data's column names).

# the input forms and the type kinds known here (a new one stops)
as_forms <- c("pick", "text", "condition", "table", "r")
as_kinds <- c("string", "integer", "number", "logical", "column", "level", "stat",
              "template", "enum", "colour", "date", "function", "tilde", "r",
              "formula", "guard", "object", "call", "list", "vec", "map", "rows",
              "step", "either")

# a scalar kind's values: two of them, all valid for the kind
as_scalars <- list(
  string = c("Text A", "Text B"),
  integer = c(3L, 5L),
  number = c(2.5, 4.25),
  column = c("AGE", "SEX"),
  level = c("F", "M"),
  stat = c("mean", "sd"),
  template = c("{mean} ({sd})", "{n}"),
  colour = c("#1F77B4", "#D62728"),
  date = c("2024-01-31", "2024-06-30"),
  `function` = c("mean", "median"),
  tilde = c("y ~ x", "~ AGE")
)

# R code as `!r`, by kind
as_r <- list(
  r = c("AGE > 65", "SEX == \"F\""),
  formula = c("~ AGE", "~ SEX"),
  guard = c("AGE > 65 ~ \"Older\"", "SEX == \"F\" ~ \"Female\""),
  object = c("adsl", "adae")
)

# a call<fn>: one call of each function a type names, and its arguments
as_calls <- list(
  rtf_border = function() tfl_call("rtf_border", list(top = TRUE, bottom = TRUE)),
  rtf_border_line = function() tfl_call("rtf_border_line", list(style = "double")),
  rtf_col_header = function() tfl_call("rtf_col_header", list(rows = list(c("A", "B")))),
  cell_rows = function() tfl_call("cell_rows", list(Mean = "{mean}")),
  continuous_summary_fns = function() {
    tfl_call("continuous_summary_fns", list(summaries = c("N", "mean")), package = "cards")
  },
  listing_col = function() tfl_call("listing_col", list(vars = "USUBJID")),
  rtf_watermark = function() tfl_call("rtf_watermark", list(text = "DRAFT")),
  vars = function() tfl_call("vars", list("SEX"), package = "ggplot2"),
  unit = function() tfl_call("unit", list(1, "cm"), package = "grid")
)

as_stop <- function(where, what) {
  stop(sprintf("arg_samples(): %s: %s -- teach helper-arg-samples.R this one.", where, what),
       call. = FALSE)
}

# values of a parsed type (tflspec:::.schema_type()), `n` of them at most;
# a value made of `!r` keeps its tag
as_values <- function(ty, where, n = 2L) {
  k <- ty$kind
  if (!k %in% as_kinds) as_stop(where, sprintf("type kind \"%s\"", k))
  if (k %in% names(as_scalars)) return(utils::head(as.list(as_scalars[[k]]), n))
  if (k %in% names(as_r)) return(lapply(utils::head(as_r[[k]], n), tfl_r))
  switch(k,
    logical = utils::head(list(TRUE, FALSE), n),
    enum = {
      v <- unlist(ty$args)
      utils::head(as.list(unique(c(v[1L], v[length(v)]))), n)
    },
    call = {
      fns <- unlist(ty$args)
      if (!length(fns)) return(list(as_calls$rtf_border()))
      fns <- sub("^.*::", "", fns)
      # (a pattern, label_*: one of the names R knows is no sample here)
      known <- fns[fns %in% names(as_calls)]
      if (!length(known)) as_stop(where, sprintf("call<%s>", paste(fns, collapse = ", ")))
      lapply(utils::head(known, n), function(f) as_calls[[f]]())
    },
    list = {
      el <- as_values(ty$args[[1L]], where, 2L)
      if (length(el) < 2L) el <- c(el, el)
      list(if (all(vapply(el, as_scalar, NA))) unlist(el) else el)
    },
    vec = {
      keys <- as_keys(ty$args[[1L]], where)
      el <- as_values(ty$args[[2L]], where, 2L)
      if (length(el) < 2L) el <- c(el, el)
      if (all(vapply(el, as_scalar, NA))) {
        list(stats::setNames(unlist(el), keys))
      } else {
        list(stats::setNames(el, keys))
      }
    },
    map = {
      keys <- as_keys(ty$args[[1L]], where)
      el <- as_values(ty$args[[2L]], where, 2L)
      if (length(el) < 2L) el <- c(el, el)
      list(stats::setNames(el, keys))
    },
    rows = {
      f <- unlist(ty$args)
      list(stats::setNames(as.data.frame(lapply(f, function(x) c("A", "B")),
                                         stringsAsFactors = FALSE), f))
    },
    step = as_stop(where, sprintf("step<%s> (give it in as_special)", unlist(ty$args))),
    either = {
      # the first value of each member, then the first member's second
      out <- list()
      for (m in ty$args) {
        v <- tryCatch(as_values(m, where, 1L), error = function(e) {
          if (grepl("teach helper-arg-samples", conditionMessage(e))) stop(e)
          NULL
        })
        out <- c(out, v)
      }
      out <- c(out, as_values(ty$args[[1L]], where, 2L)[-1L])
      utils::head(out[!duplicated(vapply(out, as_key, ""))], n)
    })
}

# one plain value (not R code, not a vector of several)
as_scalar <- function(e) is.atomic(e) && !is.object(e) && length(e) == 1L && is.null(names(e))

# two keys of a map's or a named vector's key type
as_keys <- function(ty, where) {
  k <- ty$kind
  v <- switch(k,
    string = c("AGE", "SEX"),
    column = c("AGE", "SEX"),
    stat = c("mean", "sd"),
    level = c("F", "M"),
    enum = unique(c(unlist(ty$args)[1L], utils::tail(unlist(ty$args), 1L))),
    as_stop(where, sprintf("a key of kind \"%s\"", k)))
  v
}

# an issue's number by each of `keys` (a named vector)
as_tag <- function(issue, keys) stats::setNames(rep(issue, length(keys)), keys)

# a value's identity, for leaving out repeats and the default
as_key <- function(v) paste(deparse(unclass(v)), collapse = "")

# values a few arguments take that their type cannot say, by `fn.arg`
# (valid in the walks' study: its data rows adsl_sub and adae_sub, its
# populations, the ARD's statistics)
as_special <- list(
  plan_append.data = function() list(list(source = "dataset", dataset = "ADSL"),
                                    list(source = "dataset", dataset = "ADAE")),
  plan_append.content = function() list(
    list(tfl_item("table_plan", list(cols = "TRT01A")))),
  plan_col_header.header = function() list(
    data.frame(line = c(1L, 1L), cols = c("Drug X", "Control"), text = c("A", "B"),
               stringsAsFactors = FALSE)),
  data.id = function() list("adae_one", "adae_two"),
  data.from = function() list("ADSL", "adsl_sub"),
  data.population = function() list("SAF", "ITT", tfl_unset()),
  data.subjects_in = function() list("adsl_sub", "none", tfl_unset()),
  data.code = function() list(tfl_r("adae_sub <- adae")),
  data.then = function() list(tfl_r("head(10)")),
  analysis.method = function() list("cards::ard_tabulate"),
  analysis.data = function() list("adae_sub"),
  analysis.statistics = function() list(c("N", "mean"), tfl_unset()),
  analysis.denominator = function() list("adsl_sub", tfl_unset()),
  analysis.formats = function() list(list(mean = 2L, sd = 3L), tfl_unset()),
  analysis.conditions = function() list(list(list(sexv = "F"), list(sexv = "M"))),
  data.add = function() list(c("AGE", "SEX"), tfl_unset()),
  analysis.ards = function() list(list(list(by = "TRT01A", variables = "AGE"))),
  analysis.analysis_ids = function() list(list(AGE = "AGE_ID")),
  analysis.post = function() list(tfl_r("dplyr::filter(stat_name != \"N\")"), tfl_unset()),
  analysis.code = function() list(tfl_r("cards::ard_tabulate(adsl_sub, by = TRT01A, variables = SEX)")),
  ard_tabulate.denominator = function() list("column", "cell", tfl_unset()),
  ard_hierarchical.denominator = function() list("adsl_sub", tfl_unset()),
  ard_stack_hierarchical.denominator = function() list("adsl_sub", tfl_unset()),
  ard_categorical_ci.denominator = function() list("column", "cell", tfl_unset()),
  ard_tabulate_value.denominator = function() list("column", "cell", tfl_unset()),
  ard_stats_oneway_test.formula = function() list(tfl_r("AGE ~ TRT01A")),
  ard_stats_t_test.conf.level = function() list(0.9),
  ard_stats_wilcox_test.conf.level = function() list(0.9),
  ard_stats_fisher_test.conf.level = function() list(0.9),
  ard_categorical_ci.conf.level = function() list(0.9),
  # the templates' params (tflspec_v1), valid for the walks' study
  `tflspec_v1/*.denominator` = function() list("adsl_all"),
  `tflspec_v1/*.formats` = function() list(list(mean = 3L, sd = 4L, p = "xx.xx%"), tfl_unset()),
  `tflspec_v1/*.population` = function() list("ITT", "EFF", tfl_unset()),
  `tflspec_v1/*.continuous` = function() list(c("AGE", "TRTDURD"), tfl_unset()),
  `tflspec_v1/*.categorical` = function() list(c("SEX", "RACE"), tfl_unset()),
  `tflspec_v1/*.value_labels` = function() list(list(SEX = list(F = "Women", M = "Men")), tfl_unset()),
  `tflspec_v1/*.by` = function(def) {
    if (identical(def$type, "column")) list("TRT01P") else list("TRT01P", c("TRT01A", "SEX"))
  },
  # (a table whose own table_plan has the columns TRT01A; tflspec_shift's
  # header spans one by column.  tflspec_test/: the test series'
  # templates, helper-series.R)
  `tflspec_test/lab_tox_shift.by` = function() list("TRT01A"),
  `tflspec_test/orr_subgroup.by` = function() list("TRT01A"),
  `tflspec_v1/tflspec_shift.by` = function() list("TRT01P"),
  `tflspec_v1/tflspec_hierarchy.analysis_ids` = function() list(list(ANY = "ANY_AE", L2 = "PT"), tfl_unset()),
  `tflspec_v1/forest_hr.comparison` = function() list("Drug X", "Control", tfl_unset()),
  `tflspec_v1/forest_hr.ref` = function() list("Control", "Drug X", tfl_unset()),
  `tflspec_v1/forest_hr.n_by_arm` = function() list(c("Drug X", "Control"), tfl_unset()),
  `tflspec_v1/km_cards.param` = function() list("PFS"),
  `tflspec_test/lab_tox_shift.conditions` = function() list(list(
    list(tox_dir = "High", param = "Bilirubin (umol/L)"))),
  `tflspec_test/listing.from` = function() list("ADLB"),
  `tflspec_test/listing.columns` = function() list(c("USUBJID", "AESEV"), tfl_unset()),
  `tflspec_test/listing.by` = function() list("AESEV", tfl_unset()),
  `tflspec_test/orr_subgroup.category` = function() list("AVALC", tfl_unset()),
  `tflspec_test/orr_subgroup.responder` = function() list("Non-response", tfl_unset()),
  `tflspec_v1/tflspec_hierarchy.from` = function() list("ADLB"),
  `tflspec_v1/tflspec_hierarchy.hierarchy` = function() list(c("AESOC", "AEDECOD")),
  `tflspec_v1/tflspec_listing.from` = function() list("ADLB"),
  `tflspec_v1/tflspec_listing.subject_vars` = function() list(c("AGE", "SEX"), tfl_unset()),
  `tflspec_v1/tflspec_listing.sort` = function() list(c("AESEV", "AEDECOD"), tfl_unset()),
  `tflspec_v1/tflspec_listing.columns` = function() list(list(
    list(vars = "USUBJID", label = "Subject"), list(vars = "AESEV", label = "Severity"))),
  `tflspec_v1/tflspec_listing.by` = function() list("AESEV", tfl_unset()),
  `tflspec_v1/tflspec_shift.from` = function() list("ADAE"),
  `tflspec_v1/tflspec_shift.baseline` = function() list("BTOXGR", tfl_unset()),
  `tflspec_v1/tflspec_shift.post` = function() list("ATOXGR", tfl_unset()),
  `tflspec_v1/tflspec_shift.param_var` = function() list("PARAM", tfl_unset()),
  `tflspec_v1/tflspec_hierarchy.sort_column` = function() list("AEDECOD", tfl_unset())
)

#' 2-3 values of an argument, valid for its type and not its default
#'
#' @param def One row of tfl_arg_defs().
#' @param fn The function (or template) it is an argument of, for messages.
#' @param default The value it has when absent (a template's `!series`
#'   default: the series' value).
#' @return A list of values; `tfl_unset()` last where the argument's
#'   `unset` allows it.
arg_samples <- function(def, fn = "?", default = def$default[[1L]]) {
  where <- paste0(fn, "(", def$name, ")")
  form <- def$input
  if (is.na(form) || !form %in% as_forms) as_stop(where, sprintf("input form \"%s\"", form))
  sp <- as_special[[paste0(fn, ".", def$name)]] %||%
    as_special[[paste0(sub("/.*$", "/*", fn), ".", def$name)]]
  vals <- if (!is.null(sp)) (if (length(formals(sp))) sp(def) else sp()) else {
    fixed <- unlist(def$values[[1L]])
    if (length(fixed) && is.atomic(fixed) && identical(form, "pick") &&
        !grepl("^list<", def$type)) {
      as.list(unique(c(fixed[1L], fixed[length(fixed)])))
    } else {
      as_values(tflspec:::.schema_type(def$type), where, 3L)
    }
  }
  # (an R form with a type that is not R: its R too)
  if (identical(form, "r") && !any(vapply(vals, inherits, NA, "tfl_r"))) {
    vals <- c(vals, list(tfl_r("NULL")))
  }
  # not the default
  d <- default
  if (!is.null(d) && !inherits(d, "tfl_tagged")) {
    vals <- vals[vapply(vals, function(v) as_key(as_read(v)), "") != as_key(as_read(d))]
  }
  unset <- vapply(vals, inherits, NA, "tfl_unset")
  vals <- utils::head(vals[!unset], 3L)
  if (!length(vals)) as_stop(where, "no value differs from its default")
  if (identical(def$unset, "omit") || any(unset)) vals <- c(vals, list(tfl_unset()))
  vals
}

# ---- the walks' study ---------------------------------------------------------

# A study of the bundled ADaM data (the fictional sample study,
# inst/extdata/spec-yaml, its T-14-1-1, on adsl and adae), in a temp folder:
# its reports' files are written and read back there.
as_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adlb", "adtte", "adrs")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "SAMPLE-1"))
  dir.create(file.path(d, "spec", "outputs"), showWarnings = FALSE)
  # (T-14-1-1 with a second data row, adae_sub, an analysis may name)
  t1 <- readLines(system.file("extdata", "spec-yaml", "outputs", "T-14-1-1.yml", package = "tflspec"))
  at <- grep("^    from: ADSL$", t1)
  t1 <- append(t1, c("  - id: adae_sub", "    from: ADAE"), at)
  t1 <- append(t1, c("    - id: AE", "      method: cards::ard_tabulate", "      data: adae_sub",
                     "      by: [TRT01A]", "      variables: [AESEV]"), grep("^codelists:", t1) - 1L)
  writeLines(t1,
             file.path(d, "spec", "outputs", "T-14-1-1.yml"))
  writeLines(c("id: L-1", "kind: listing", "label: Adverse events",
               "common: {population: SAF}",
               "data: {source: dataset, dataset: ADAE, population: SAF}",
               "content:", "  - plan_listing:", "      cols:",
               "        - {fn: listing_col, args: {vars: [USUBJID], label: Subject}}",
               "        - {fn: listing_col, args: {vars: [AEDECOD], label: Preferred term}}",
               "page:", "  titles: {'1': {center: Listing 1}}"),
             file.path(d, "spec", "outputs", "L-1.yml"))
  writeLines(c("id: F-1", "kind: figure", "label: Age by treatment duration",
               "common: {population: SAF}",
               "data: {source: dataset, dataset: ADSL, population: SAF}",
               "content:", "  - point: {x: AGE, y: TRTDURD}",
               "page:", "  titles: {'1': {center: Figure 1}}"),
             file.path(d, "spec", "outputs", "F-1.yml"))
  d
}

# a value as the reader gives it back: a named vector is a map, read as a
# named list; a data frame its rows, each a map of its cells (the reader's
# own normalisation, nothing more)
as_read <- function(v) {
  if (is.data.frame(v)) {
    return(lapply(seq_len(nrow(v)), function(i) as_read(as.list(v[i, , drop = FALSE]))))
  }
  if (inherits(v, c("tfl_r", "tfl_unset"))) return(v)
  if (is.atomic(v) && length(names(v))) return(as.list(v))
  if (is.list(v)) {
    cls <- class(v)
    v <- lapply(unclass(v), as_read)
    class(v) <- if (identical(cls, "list")) NULL else cls
  }
  v
}
