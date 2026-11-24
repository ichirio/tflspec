# R that lost its `!r` tag (#893).  The reader keeps R as tfl_r(): the
# `!r` tag, or a neutral condition in a slot of only R (R/neutral.R).  A
# string left where the type takes it only as a guard (`cond ~ "text"`) is R
# whose tag was lost (written without it, or quoted): rtfreporter would get
# the code as a template, and the cell would print it.  The reader stops
# there, naming the argument, the report and how to write it.
#
# The schema's type alone decides, never the text: the value does not fit
# the type as written, and it fits once its strings are R.  A type with a
# member that takes the string as text keeps it (`template|list<guard|
# template>`, `column|guard`: a template, a column is a string).  An `r` or
# a `formula` keeps a string as it did: the reader opens it as written (the
# GUI shows it), the check says what it is (.schema_fits(), R/neutral.R) and
# the code writes it or stops (.schema_value_to_r()).

# The arguments of one item (`fn`), as read; each `{fn:, args:}` inside a
# value by its own function's types.  Stops at the first R without its tag.
# (The schema file's own entry: a guard is only there, and tfl_schema()'s
# formals are not needed.)
.r_tag_check <- function(fn, args) {
  # (an item whose map is itself `{fn:, args:}`, `- call: {fn: survfit2,
  # ...}`: its arguments are that call, checked as it)
  if (inherits(args, "tfl_call")) {
    .r_tag_check(args$fn, args$args %||% list())
    return(args)
  }
  ent <- if (is.character(fn) && length(fn) == 1L && !is.na(fn)) {
    .schema_all(figure = FALSE)[[.schema_fn_name(fn)]]
  }
  if (is.list(ent) && is.list(args)) {
    for (a in setdiff(names(args), "")) {
      ty <- .schema_arg_type(ent, a)
      if (!is.null(ty) && .r_tag_lost(args[[a]], ty)) .r_tag_stop(fn, a, args[[a]])
    }
  }
  for (cl in .r_tag_calls(args)) .r_tag_check(cl$fn, cl$args %||% list())
  args
}

# TRUE when `v` holds a string that only R fits: `v` does not have the type
# `ty` as it is, and has it with each of its strings as `!r` (an `r` or a
# `formula` member taking a string as well)
.r_tag_lost <- function(v, ty) {
  if (!.schema_has(ty, "guard") || !length(.r_tag_texts(v))) return(FALSE)
  ty <- .r_tag_text_r(ty)
  !.schema_ok(v, ty) && .schema_ok(.r_tag_all(v), ty)
}

# the type with each `r` and `formula` member as `r|string`
.r_tag_text_r <- function(ty) {
  if (ty$kind %in% c("r", "formula")) {
    return(list(kind = "either", args = list(ty, list(kind = "string", args = list()))))
  }
  if (ty$kind %in% c("either", "list", "vec", "map")) ty$args <- lapply(ty$args, .r_tag_text_r)
  ty
}

# the plain strings of a value (no tfl_r, no `!unset`), the calls' left out
.r_tag_texts <- function(v) {
  if (inherits(v, c("tfl_r", "tfl_unset", "tfl_call"))) return(character())
  if (is.character(v)) return(v[!is.na(v)])
  if (is.list(v) && !is.object(v)) return(unlist(lapply(v, .r_tag_texts), use.names = FALSE))
  character()
}

# the value with each plain string as tfl_r (a vector of them a list),
# names kept
.r_tag_all <- function(v) {
  if (inherits(v, c("tfl_r", "tfl_unset", "tfl_call"))) return(v)
  one <- function(s) if (is.na(s)) s else tfl_r(s)
  if (is.character(v)) {
    if (length(v) == 1L && is.null(names(v))) return(one(v))
    return(stats::setNames(lapply(unname(v), one), names(v)))
  }
  if (is.list(v) && !is.object(v)) return(lapply(v, .r_tag_all))
  v
}

# the `{fn:, args:}` calls a value holds, outermost first
.r_tag_calls <- function(v) {
  if (inherits(v, "tfl_call")) return(list(v))
  if (is.list(v) && !is.object(v)) return(do.call(c, c(list(list()), lapply(unname(v), .r_tag_calls))))
  list()
}

# The stop: the file, the report, the argument, the text read, and the
# value as `!r` (YAML's single quotes, a quote in it doubled)
.r_tag_stop <- function(fn, a, v) {
  s <- .r_tag_texts(v)[[1L]]
  where <- .spec_read_file()
  id <- .spec_read$id
  if (!is.null(id)) where <- paste0(where, ": ", id)
  stop(sprintf(paste0(
    "%s: %s(%s = ) takes R here (a guard), but \"%s\" was read as text (its `!r` tag ",
    "was lost: written without it, or quoted).  Write it as `!r '%s'`, as a guard is ",
    "written: `!r 'n == 0 ~ \"0\"'`."),
    where, .schema_fn_name(fn), a, s, gsub("'", "''", s, fixed = TRUE)), call. = FALSE)
}
