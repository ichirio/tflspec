# ---- a table's parts: another plan's rows under its own ---------------------
# rtfreporter's plan_layer() (#737): a table whose rows come from two sources
# -- a listing of the records above, their summary below -- is two ordinary
# plans, the second laid under the first.  The SPEC writes it as rtfreporter
# does: one content item whose arguments are the part's own `data` (where
# its rows come from: `source: ard`, `source: dataset`) and its own
# `content` (an ordinary table's items), with its `name` and `absent`:
#
#   content:
#     - table_plan: {cols: ATPT, rows: {subject: !r '~ "{USUBJID}/{AGE}/{SEX}"'}, value: AVAL}
#     - plan_cells: {default: "{value:.3s}", na: BLQ}
#     - plan_layer:
#         name: summary
#         data: {source: ard}
#         content:
#           - normalize_ard: {}
#           - table_plan: {cols: ATPT, rows: {block: !r '~ "Summary Statistics"'}}
#           - plan_cells: {continuous: {N: "{N:.0f}", Mean: "{mean:.4s}"}}
#
# The program writes each part's plan first, under the part's name, from
# its own data object, then the table's plan with the part laid in:
#
#   summary <- ard |> normalize_ard() |> table_plan(...) |> plan_cells(...)
#   plan <- data |> table_plan(...) |> plan_cells(...) |> plan_layer(summary, name = "summary")
#
# The function's and its argument's names are said here only, so the
# rtfreporter verb's name can change in one place.

.spec_part <- list(fn = "plan_layer", arg = "layer")

# Is an item a part?
.is_part <- function(it) {
  !is.null(it$fn) && identical(.schema_fn_name(it$fn), .spec_part$fn)
}

# A part item as read from YAML: its content as items, its data as a
# report's data
.part_in <- function(args) {
  if (!is.null(args$content)) {
    if (!is.list(args$content) || !is.null(names(args$content))) {
      .ard_stop(sprintf("%s: `content` is a sequence of items (`- fn: {args}`).", .spec_part$fn))
    }
    args$content <- lapply(args$content, .item_from_list)
  }
  if (!is.null(args$data)) args$data <- .data_steps_in(.calls_in(args$data))
  args
}

# ... and as written
.part_out <- function(args) {
  out <- .calls_out(args[setdiff(names(args), c("content", "data"))])
  if (!is.null(args$data)) out$data <- .calls_out(args$data)
  if (!is.null(args$content)) out$content <- .items_to_list(args$content)
  out[c(intersect(c("name", "absent", "data"), names(out)),
        setdiff(names(out), c("name", "absent", "data", "content")),
        intersect("content", names(out)))]
}

# The parts of a content, in order: their names (the program's object for
# each), data objects and contents
.content_parts <- function(content) {
  parts <- Filter(.is_part, content)
  if (!length(parts)) return(list())
  nm <- vapply(seq_along(parts), function(i) parts[[i]]$args$name %||% paste0("layer", i + 1L), "")
  if (anyDuplicated(nm)) {
    .ard_stop(sprintf("%s: two parts are called %s; give each its own `name`.",
                      .spec_part$fn, paste(sQuote(unique(nm[duplicated(nm)])), collapse = ", ")))
  }
  lapply(seq_along(parts), function(i) {
    a <- parts[[i]]$args
    if (!length(a$content)) {
      .ard_stop(sprintf("%s(\"%s\"): its `content` is the part's own table items.", .spec_part$fn, nm[i]))
    }
    list(name = nm[i], var = make.names(nm[i]), object = .part_object(a$data),
         data = a$data, content = a$content, absent = a$absent)
  })
}

# The object a part's plan starts from: its data's `object`; the ARD
# (`ard`) for `source: ard`; the dataset's name in lower case for
# `source: dataset`
.part_object <- function(data) {
  if (!is.null(data$object)) return(as.character(data$object))
  src <- data$source %||% "ard"
  if (identical(src, "ard")) return("ard")
  if (identical(src, "dataset") && !is.null(data$dataset)) return(tolower(as.character(data$dataset)[1L]))
  .ard_stop(sprintf("%s: a part's data is `source: ard` or `source: dataset` with its `dataset`.",
                    .spec_part$fn))
}

# The call a part item stands for in the table's plan: the verb with the
# part's plan (its object) and its name / absent
.part_call <- function(it, i) {
  a <- it$args
  nm <- a$name %||% paste0("layer", i + 1L)
  args <- stats::setNames(list(.spec_sym(make.names(nm))), .spec_part$arg)
  args$name <- nm
  if (!is.null(a$absent)) args$absent <- a$absent
  do.call(.spec_call, c(list(.spec_part$fn), args))
}

# The parts' programs, before the table's: one plan each, from its object
.content_parts_code <- function(content, pipe = NULL) {
  parts <- .content_parts(content)
  unlist(lapply(parts, function(p) {
    c(sprintf("# the part \"%s\": its rows go under the table's", p$name),
      .content_table_code(p$content, data = p$object, plan = p$var, pipe = pipe, kind = "table"), "")
  }))
}

# The parts' plans, run: each from its data object (`data` a named list of
# the objects the table reads: the table's own and each part's), in an
# environment the table's plan reads them from
.content_parts_env <- function(content, data) {
  env <- new.env(parent = emptyenv())
  for (p in .content_parts(content)) {
    d <- if (is.list(data) && !is.data.frame(data) && !is.null(data[[p$object]])) data[[p$object]] else NULL
    if (is.null(d)) {
      .ard_stop(sprintf("%s(\"%s\"): no `%s` to make its rows from (the part's data).",
                        .spec_part$fn, p$name, p$object))
    }
    assign(p$var, .content_table_run(p$content, d, kind = "table"), envir = env)
  }
  env
}
