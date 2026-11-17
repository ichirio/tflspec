# ============================================================================
#  The stratified Wilson interval of a proportion (since 2026-11-17):
#  cardx::ard_categorical_ci(method = "strat_wilson" / "strat_wilsoncc",
#  strata =, weights =, max.iterations =), the keyword `proportion_ci` -- a
#  response rate's CI over the randomisation strata.  Seen with cardx 0.3.4:
#
#  * the stratum must be a factor: proportion_ci_strat_wilson() stops on a
#    character or numeric column ("The `strata` argument must be class
#    <factor>"), and cardx keeps that error in the ARD, so the cells come
#    out empty.  ADaM's stratum columns (STRAT1R, AGEGR1 ...) are text, so
#    the generated code makes the row's stratum a factor first
#    (.item_strat_factor(): `dplyr::mutate(AGEGR1 = as.factor(AGEGR1))`;
#    as.factor() keeps a code list's factor as it is);
#  * `strata` with any other method (the default "waldcc" included) is
#    passed on unevaluated, and the analysis fails with "object 'AGEGR1'
#    not found" (the cells come out empty);
#  * `weights` and `max.iterations` with any other method are dropped with
#    no warning;
#  * `weights` of the wrong length are recycled: R's warning is kept in the
#    ARD, and the confidence limits are wrong (a lower limit of 0.776 for a
#    proportion of 0.616 on cards' ADSL, three strata and two weights).
#
#  The first three are said when the definition is read
#  (.strat_ci_problems()); the weights' count is judged by the stratum's
#  code list when the code is written (.check_strat_weights()).
# ============================================================================

# The methods of ard_categorical_ci() that read `strata`
.strat_ci_methods <- c("strat_wilson", "strat_wilsoncc")

# The arguments only those methods read
.strat_ci_args <- c("weights", "max.iterations")

# An analysis row of cardx::ard_categorical_ci() (by its keyword or its
# name): list(method = the method written (cardx's default "waldcc" when
# none; NA when an object of the program), strata = the stratum's columns
# (the row's `strata` cell and `args`' `strata =`), args = `args` as a
# list).  NULL for a row of another function.
.strat_ci_row <- function(r, keys = tfl_ard_methods()) {
  m <- .item_cell(r, "method")
  if (is.na(m)) return(NULL)
  k <- .method_key(m, keys)
  call <- if (is.na(k)) m else keys$call[k]
  if (!identical(call, "cardx::ard_categorical_ci")) return(NULL)
  args <- .item_cell(r, "args")
  a <- if (is.na(args)) list() else
    tryCatch(.args_list(args), error = function(e) list())
  method <- "waldcc"
  if (!is.null(a[["method"]])) {
    v <- .literal_value(a[["method"]])
    method <- if (is.character(v) && length(v) == 1L) v else NA_character_
  }
  st <- .split_bar(.item_cell(r, "strata"))
  if (!is.null(a[["strata"]])) st <- c(st, .strat_ci_names(a[["strata"]]))
  list(method = method, strata = unique(st), args = a)
}

# The columns a `strata =` written in `args` names: AGEGR1, "AGEGR1",
# c(A, B); another expression as it is written
.strat_ci_names <- function(e) {
  if (is.name(e)) return(as.character(e))
  if (is.character(e)) return(e)
  if (is.call(e) && identical(e[[1L]], as.name("c"))) {
    return(unlist(lapply(as.list(e)[-1L], .strat_ci_names)))
  }
  paste(deparse(e), collapse = "")
}

# The stratum a row of the stratified Wilson interval makes a factor: one
# column, by its name; character() for any other row
.strat_ci_stratum <- function(r) {
  s <- .strat_ci_row(r)
  if (is.null(s) || !isTRUE(s$method %in% .strat_ci_methods) ||
      length(s$strata) != 1L) return(character())
  v <- s$strata
  if (!identical(make.names(v), v)) return(character())
  v
}

# The step that makes the stratum a factor (in the row's own data step:
# .item_steps())
.item_strat_factor <- function(r) {
  v <- .strat_ci_stratum(r)
  if (!length(v)) return(character())
  sprintf("%s = as.factor(%s)", v, v)
}

# What is wrong with the rows of ard_categorical_ci() as to their stratum:
# one line a problem ("T1 / ORR: ...")
.strat_ci_problems <- function(x) {
  a <- x$analyses
  if (is.null(a) || !nrow(a)) return(character())
  keys <- tfl_ard_methods()
  out <- character()
  for (i in seq_len(nrow(a))) {
    s <- .strat_ci_row(a[i, , drop = FALSE], keys)
    if (is.null(s) || is.na(s$method)) next
    tag <- paste(a$output_id[i], a$analysis_id[i], sep = " / ")
    meth <- sprintf("ard_categorical_ci(method = \"%s\")", s$method)
    if (s$method %in% .strat_ci_methods) {
      if (!length(s$strata)) {
        out <- c(out, sprintf(paste(
          "%s: %s needs its stratum: write one column in `strata`",
          "(STRAT1R); cardx would stop (\"The `strata` argument must be",
          "length 1\")"), tag, meth))
      } else if (length(s$strata) > 1L) {
        out <- c(out, sprintf(paste(
          "%s: %s takes one stratum column; got %s -- make one column of",
          "their combinations in the row's `derive` (STRATA =",
          "interaction(%s, sep = \" / \")) and write it in `strata`;",
          "cardx would stop (\"The `strata` argument must be length 1\")"),
          tag, meth, paste(s$strata, collapse = ", "),
          paste(s$strata, collapse = ", ")))
      }
      next
    }
    if (length(s$strata)) {
      out <- c(out, sprintf(paste(
        "%s: %s has a stratum (%s), which only method = \"strat_wilson\" /",
        "\"strat_wilsoncc\" read: cardx would pass it on unevaluated and the",
        "analysis would fail (\"object '%s' not found\") -- write method =",
        "\"strat_wilson\" for a stratified interval, or leave `strata` blank"),
        tag, meth, paste(s$strata, collapse = ", "), s$strata[1L]))
    }
    given <- intersect(.strat_ci_args, names(s$args))
    if (length(given)) {
      out <- c(out, sprintf(paste(
        "%s: %s drops %s with no warning: only method = \"strat_wilson\" /",
        "\"strat_wilsoncc\" read %s"),
        tag, meth, paste0("`", given, "`", collapse = " and "),
        if (length(given) > 1L) "them" else "it"))
    }
  }
  out
}

# The weights of a stratified Wilson row, judged by its stratum's code list
# when the code is written (`levels`: each report's code lists, as
# .codelist_levels_by() gives them): one weight a value of the list, in its
# order.  Fewer or more would be recycled by cardx, with R's warning kept
# in the ARD and the confidence limits wrong.  A warning.
.check_strat_weights <- function(a, levels) {
  if (!length(levels) || is.null(a) || !nrow(a)) return(invisible(NULL))
  out <- character()
  for (i in seq_len(nrow(a))) {
    v <- .strat_ci_stratum(a[i, , drop = FALSE])
    if (!length(v)) next
    cl <- levels[[a$output_id[i]]][[v]]
    if (!length(cl)) next
    args <- .item_cell(a[i, , drop = FALSE], "args")
    w <- tryCatch(.args_list(args)[["weights"]], error = function(e) NULL)
    w <- if (is.null(w)) NULL else .literal_value(w)
    if (!is.numeric(w) || length(w) == length(cl)) next
    out <- c(out, sprintf(paste(
      "%s / %s: ard_categorical_ci(weights = ) has %d weight%s, but the",
      "stratum %s has %d values in its code list (%s): cardx would recycle",
      "them and the confidence limits would be wrong -- give one weight a",
      "value, in the code list's order"),
      a$output_id[i], a$analysis_id[i], length(w), if (length(w) == 1L) "" else "s",
      v, length(cl), paste(names(cl) %||% cl, collapse = ", ")))
  }
  if (length(out)) warning(paste(out, collapse = "\n  "), call. = FALSE)
  invisible(NULL)
}
