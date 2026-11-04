# A language-neutral condition: the subset of R a reader in another
# language (Python) can parse without R.  An untagged string in a slot
# that takes only R (`plan_filter(where)`) is one; `!r` keeps full R.
# design: S1's memo, 2026-11-04.

.neutral_cmp <- c("==", "!=", "<", "<=", ">", ">=")

# TRUE when `text` is a neutral condition, else FALSE with attr "why" (the
# first thing refused)
.neutral_expr <- function(text) {
  no <- function(why) structure(FALSE, why = why)
  if (!is.character(text) || length(text) != 1L || is.na(text)) return(no("not a string"))
  e <- tryCatch(str2lang(text), error = function(err) NULL)
  if (is.null(e)) return(no("it does not read as R"))
  if (!is.call(e) || .neutral_lit(e)) {
    return(no("a condition is a comparison or a test, not a lone name or value"))
  }
  why <- .neutral_node(e)
  if (is.null(why)) TRUE else no(why)
}

.neutral_lit <- function(e) {
  (is.double(e) && length(e) == 1L && !is.na(e)) ||
    (is.character(e) && length(e) == 1L && !is.na(e)) ||
    (is.logical(e) && length(e) == 1L && !is.na(e)) ||
    (is.call(e) && identical(e[[1L]], as.name("-")) && length(e) == 2L &&
       is.double(e[[2L]]) && length(e[[2L]]) == 1L)
}

# NULL when the node is in the whitelist, else what is refused
.neutral_node <- function(e) {
  if (is.name(e)) return(NULL)
  if (!is.call(e)) {
    if (.neutral_lit(e)) return(NULL)
    return(paste0("`", paste(deparse(e), collapse = ""), "`"))
  }
  if (.neutral_lit(e)) return(NULL)
  f <- e[[1L]]
  fn <- if (is.name(f)) as.character(f) else paste(deparse(f), collapse = "")
  n <- length(e) - 1L
  ok <- (fn %in% c(.neutral_cmp, "&", "|") && n == 2L) ||
    (fn %in% c("!", "(", "is.na") && n == 1L) ||
    (fn == "%in%" && n == 2L)
  if (!ok) return(paste0("`", fn, "`"))
  if (fn == "%in%") {
    r <- e[[3L]]
    if (!(is.call(r) && identical(r[[1L]], as.name("c")) && length(r) > 1L &&
          all(vapply(as.list(r)[-1L], .neutral_lit, NA)))) {
      return("`%in%` takes c(...) of values on its right")
    }
    return(.neutral_node(e[[2L]]))
  }
  for (a in as.list(e)[-1L]) {
    w <- .neutral_node(a)
    if (!is.null(w)) return(w)
  }
  NULL
}

# the types that take only R (`r`, `formula`, `list<r>`, an either of these):
# where an untagged neutral string is a condition; a type that also takes a
# string (`string|r`) keeps the string, and R stays `!r`
.schema_pure_r <- function(ty) {
  if (!is.list(ty)) return(FALSE)
  k <- ty$kind
  if (k %in% c("r", "formula")) return(TRUE)
  if (k == "list") return(.schema_pure_r(ty$args[[1L]]))
  if (k == "either") return(all(vapply(ty$args, .schema_pure_r, NA)))
  FALSE
}

# an untagged character in a pure slot -> tfl_r where it is neutral
# (the reader's normalizer); what is not stays a string, the check says so
.neutral_wrap <- function(v) {
  if (inherits(v, c("tfl_r", "tfl_unset"))) return(v)
  if (is.character(v)) {
    one <- function(s) if (isTRUE(.neutral_expr(s))) tfl_r(s) else s
    return(if (length(v) == 1L) one(v) else lapply(as.list(v), one))
  }
  if (is.list(v) && !is.object(v)) return(lapply(v, .neutral_wrap))
  v
}

.neutral_in <- function(fn, args) {
  ent <- tryCatch(tfl_schema(fn), error = function(e) NULL)
  if (is.null(ent)) return(args)
  for (a in names(args)) {
    if (.schema_pure_r(.schema_arg_type(ent, a))) args[[a]] <- .neutral_wrap(args[[a]])
  }
  args
}

# The other way, for the writer: a neutral `tfl_r` of a slot of only R becomes
# the plain string (a mixed slot, a data step keep `!r`)
.neutral_out <- function(fn, args) {
  ent <- tryCatch(tfl_schema(fn), error = function(e) NULL)
  if (is.null(ent)) return(args)
  plain <- function(v) {
    if (inherits(v, "tfl_r") && isTRUE(.neutral_expr(as.character(unclass(v))))) {
      return(as.character(unclass(v)))
    }
    if (is.list(v) && !is.object(v)) return(lapply(v, plain))
    v
  }
  for (a in names(args)) {
    if (.schema_pure_r(.schema_arg_type(ent, a))) args[[a]] <- plain(args[[a]])
  }
  args
}

.neutral_out_items <- function(items) {
  lapply(items, function(it) {
    if (inherits(it, "tfl_plot")) {
      it$items <- .neutral_out_items(it$items)
    } else if (inherits(it, "tfl_item") && !identical(it$fn, ".r") && !.is_part(it)) {
      it$args <- .neutral_out(it$fn, it$args)
    }
    it
  })
}

# the check's wording of a string (or strings) a pure slot refuses
.neutral_refusal <- function(v, a) {
  plain <- function(x) is.character(x) && !inherits(x, c("tfl_r", "tfl_unset"))
  s <- if (plain(v)) v else unlist(Filter(plain, as.list(v)), use.names = FALSE)
  for (x in s) {
    r <- .neutral_expr(x)
    if (!isTRUE(r)) {
      return(sprintf(paste0(
        "`%s` is a string that is not a neutral condition (column names, numbers, strings, ",
        "== != < <= > >=, & | !, %%in%% c(...), is.na(), parentheses; refused: %s); ",
        "write it as `!r <R>`."), a, attr(r, "why")))
    }
  }
  NULL
}

# `!r` holding a string literal ("AGE >= 18"): the condition itself is meant
.neutral_literal_msg <- function(v, a) {
  els <- if (inherits(v, "tfl_r")) list(v) else if (is.list(v) && !is.object(v)) v else list()
  for (x in els) {
    if (inherits(x, "tfl_r")) {
      e <- tryCatch(str2lang(as.character(unclass(x))), error = function(err) NULL)
      if (is.character(e) && length(e) == 1L) {
        return(sprintf(paste0("`%s` is `!r` holding a string literal (\"%s\"); write the ",
                              "condition itself, without the quotes."), a, e))
      }
    }
  }
  NULL
}
