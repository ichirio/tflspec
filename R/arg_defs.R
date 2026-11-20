# ============================================================================
#  Argument definitions: one source for every card (code SPEC and template)
# ----------------------------------------------------------------------------
#  canon 11-14 (line 461) and the user's principle of 2026-11-16: a template
#  SPEC and a code SPEC share one mechanism.  A template is one large card
#  made from its `params:`, a code SPEC many small cards made from the schema
#  (inst/spec/schema.yml).  tfl_arg_defs() reads either and returns the same
#  table, so the screen (tflplanner) and the AI kit read one definition.
#
#    tfl_arg_defs(fn = "plan_cells")             a schema function's args
#    tfl_arg_defs(fn = "data")                   the keys of a data: row
#    tfl_arg_defs(fn = "analysis")               the keys of an analysis item
#    tfl_arg_defs(template = "tflspec_v1/demog") a template's params:
# ============================================================================

# the 4 input forms: pick (choose from a list; one or several follows the
# type: list<...> = several, logical = a switch), text (type it; a number type
# is a number box), condition (write a condition, the columns as candidates),
# table (fill a table)
.arg_input_forms <- c("pick", "text", "condition", "table")

# the closed list of choice sources (14 words, canon 11-14).  An entry writes
# one with what it depends on in brackets ("vars(adsl_sub)", "levels(SEX)",
# "from_args(continuous, categorical)"); several separated by ", " give the
# list's groups in that order ("report_data, study_data").
.arg_choice_sources <- c("vars", "levels", "codelist", "populations", "report_data",
                         "study_data", "ard", "columns", "study_info", "tokens",
                         "stats", "fixed", "styles", "from_args")

# the words that let a pick take a value not in the list (`free`) unless the
# definition says otherwise
.arg_free_sources <- c("tokens", "study_info", "codelist")

.arg_steps <- c("data", "analysis", "content", "page")
.arg_default_kinds <- c("value", "study", "series", "unset", "fixed")
.arg_tiers <- c("required", "usual", "other")

# the YAML tags a template's values may carry, kept as written (the value is
# resolved for a study later: tfl_template_resolve())
.arg_tags <- c("series", "study", "fixed", "unset", "r")

# the source words of a choices entry, in order, without their brackets
.arg_choice_words <- function(choices) {
  if (is.null(choices) || !length(choices) || all(is.na(choices))) {
    return(character())
  }
  x <- paste(as.character(choices), collapse = ", ")
  # commas inside brackets belong to the word's argument
  depth <- 0L
  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  for (i in seq_along(chars)) {
    if (chars[i] == "(") depth <- depth + 1L
    if (chars[i] == ")") depth <- depth - 1L
    if (chars[i] == "," && depth > 0L) chars[i] <- "\001"
  }
  parts <- trimws(strsplit(paste(chars, collapse = ""), ",", fixed = TRUE)[[1L]])
  parts <- gsub("\001", ",", parts, fixed = TRUE)
  parts[nzchar(parts)]
}

.arg_check_choices <- function(choices, where) {
  entries <- .arg_choice_words(choices)
  words <- sub("[(].*$", "", entries)
  bad <- setdiff(words, .arg_choice_sources)
  # vars(<name>) is a data row id; vars(from_args(<arg>)) the columns of the
  # data another argument names.  Nothing else nests in vars().
  inner <- sub("^vars[(](.*)[)]$", "\\1", entries[words == "vars" & grepl("^vars[(].*[)]$", entries)])
  inner <- inner[grepl("[(]", inner) & !grepl("^from_args[(].*[)]$", inner)]
  if (length(inner)) {
    stop(sprintf("%s: vars() takes a data row id or from_args(<arg>), not %s.", where,
                 paste(encodeString(inner, quote = "\""), collapse = ", ")), call. = FALSE)
  }
  if (length(bad)) {
    stop(sprintf("%s: unknown choice source %s; the sources are %s.", where,
                 paste(encodeString(bad, quote = "\""), collapse = ", "),
                 paste(.arg_choice_sources, collapse = ", ")), call. = FALSE)
  }
  invisible(choices)
}

# A tagged YAML value as written: the value with its tag (a template's
# defaults stay unresolved here).
.arg_tagged <- function(tag) {
  force(tag)
  function(x) structure(list(value = x), class = "tfl_tagged", tag = tag)
}

# (YAML 1.1's implicit booleans, n, y, yes, no, on, off, stay text as
# .yaml_handlers() keeps them: a cells key `n:` is the statistic n)
.arg_yaml_handlers <- function() {
  c(stats::setNames(lapply(.arg_tags, .arg_tagged), .arg_tags),
    .yaml_handlers()[c("bool#yes", "bool#no")])
}

.arg_default_kind <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (inherits(x, "tfl_unset")) return("unset")
  if (inherits(x, "tfl_tagged")) {
    tag <- attr(x, "tag")
    return(switch(tag, series = "series", study = "study", fixed = "fixed",
                  unset = "unset", "value"))
  }
  "value"
}

# the input form a type takes when the definition does not say
.arg_input_for_type <- function(type, name = "") {
  type <- as.character(type %||% "string")
  if (identical(type, "logical") || startsWith(type, "enum[")) return("pick")
  if (grepl("^(list<)?(column|level)>?$", type)) return("pick")
  if (identical(type, "r")) return(if (identical(name, "where")) "condition" else "text")
  if (grepl("^(rows|map|vec)<", type)) return("table")
  "text"
}

# a schema function's stage as a step: the content stage's mapping and
# finishing, the page's output
.arg_step_for_stage <- function(stage) {
  if (identical(stage, "output")) "page" else "content"
}

# the plan_paginate_*() functions are the page step
.arg_page_fns <- c("plan_paginate_rows", "plan_paginate_cols", "plan_paginate_group")

# What a schema function's argument gets where schema.yml is silent: a column
# argument without choices picks from the ARD (normalize_ard) or from the
# table's columns (plan_*); normalize_ard's hierarchy is ordered.
.arg_schema_fill <- function(fn, args) {
  column_type <- function(d) {
    is.list(d) && grepl("^(list<)?column>?$", as.character(d$type %||% ""))
  }
  for (n in names(args)) {
    d <- args[[n]]
    if (column_type(d) && is.null(d$choices)) {
      if (identical(fn, "normalize_ard")) {
        args[[n]]$choices <- "ard"
      } else if (startsWith(fn, "plan_")) {
        args[[n]]$choices <- "columns"
      }
    }
    if (identical(fn, "normalize_ard") && n %in% c("hierarchy", ".hierarchy")) {
      args[[n]]$ordered <- TRUE
    }
  }
  args
}

.arg_enum_values <- function(type) {
  if (!startsWith(type %||% "", "enum[")) return(NULL)
  strsplit(sub("^enum\\[(.*)\\]$", "\\1", type), "|", fixed = TRUE)[[1L]]
}

# One row of the table from one definition (a list as written in the schema
# or a template's params:).
.arg_row <- function(name, d, step, label_prefix, where) {
  if (!is.list(d)) d <- list(default = d)
  type <- as.character(d$type %||% "string")
  input <- as.character(d$input %||% .arg_input_for_type(type, name))
  if (!input %in% .arg_input_forms) {
    stop(sprintf("%s: argument `%s`: input %s is not one of %s.", where, name,
                 encodeString(input, quote = "\""), paste(.arg_input_forms, collapse = ", ")),
         call. = FALSE)
  }
  st <- as.character(d$step %||% step)
  if (!st %in% .arg_steps) {
    stop(sprintf("%s: argument `%s`: step %s is not one of %s.", where, name,
                 encodeString(st, quote = "\""), paste(.arg_steps, collapse = ", ")),
         call. = FALSE)
  }
  choices <- d$choices
  values <- d$values %||% .arg_enum_values(type)
  if (is.null(choices) && length(values)) choices <- "fixed"
  .arg_check_choices(choices, sprintf("%s: argument `%s`", where, name))
  words <- sub("[(].*$", "", .arg_choice_words(choices))
  free <- d$free %||% (if (input == "pick" && length(words)) any(words %in% .arg_free_sources) else NA)
  ordered <- d$ordered %||% (if (input == "pick" && startsWith(type, "list<")) FALSE else NA)
  has_default <- "default" %in% names(d)
  default <- if (has_default) d$default else NULL
  required <- isTRUE(d$required) || (identical(d$tier, "required") && !has_default)
  tier <- as.character(d$tier %||% (if (required) "required" else "usual"))
  row <- data.frame(name = name, stringsAsFactors = FALSE)
  row$label_key <- as.character(d$label_key %||% paste0(label_prefix, name))
  row$step <- st
  row$input <- input
  row$type <- type
  row$required <- required
  row$default <- I(list(default))
  row$default_kind <- if (has_default) .arg_default_kind(default) else NA_character_
  row$choices <- I(list(.arg_choice_words(choices)))
  row$values <- I(list(values))
  row$free <- as.logical(free)
  row$ordered <- as.logical(ordered)
  row$columns <- I(list(d$columns))
  row$fixed_rows <- I(list(NULL))
  row$tier <- tier
  row$description <- as.character(d$description %||% d$help %||% "")
  row
}

.arg_table <- function(defs, step, label_prefix, where) {
  if (!length(defs)) {
    return(.arg_row("x", list(), step, label_prefix, where)[0L, , drop = FALSE])
  }
  rows <- Map(function(n, d) .arg_row(n, d, step, label_prefix, where), names(defs), defs)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# ---- the keys of a data: row and of an analysis item ------------------------
# (the code SPEC's own cards; the same keys in a template)

.data_row_arg_defs <- function() {
  list(
    id = list(type = "string", input = "text", required = TRUE, tier = "required",
              description = "The row's name; the analysis items read it by this id."),
    from = list(type = "string", input = "pick", choices = "report_data, study_data", free = FALSE,
                required = TRUE, tier = "required",
                description = "The dataset the row reads: a study dataset or a row above."),
    population = list(type = "string", input = "pick", choices = "populations", free = FALSE,
                      description = "The analysis set the row keeps (data.yml's where)."),
    subjects_in = list(type = "string", input = "pick", choices = "report_data, fixed", free = FALSE,
                       values = "none",
                       description = "The row whose subjects are kept (inner join); none keeps all."),
    add = list(type = "list<column>", input = "pick", choices = "vars(from_args(subjects_in))",
               description = "Columns of the subjects' row added by USUBJID."),
    where = list(type = "r", input = "condition", choices = "vars(from_args(from))",
                 description = "The records kept."),
    derive = list(type = "map<column,r>", input = "table", choices = "vars(from_args(from))",
                  description = "Columns made: name = R."),
    keep = list(type = "list<column>", input = "pick", choices = "vars(from_args(from))", ordered = TRUE,
                description = "The columns kept (USUBJID and add: always)."),
    distinct = list(type = "list<column>", input = "pick", choices = "vars(from_args(from))",
                    description = "One row a set of these columns."),
    arrange = list(type = "list<column>", input = "pick", choices = "vars(from_args(from))", ordered = TRUE,
                   description = "The row order (a minus for descending)."),
    then = list(type = "r", input = "text",
                description = "R after the keys above, on the row's data."),
    code = list(type = "r", input = "text", tier = "other",
                description = "R for a row the keys cannot say.")
  )
}

.analysis_item_arg_defs <- function() {
  methods <- tryCatch(as.character(tfl_ard_methods()$method), error = function(e) NULL)
  list(
    id = list(type = "string", input = "text", required = TRUE, tier = "required",
              description = "The item's id: the ARD's analysis_id (a variable's own id: analysis_ids)."),
    label = list(type = "string", input = "text", description = "The item's name for people."),
    type = list(type = "enum[ard|model|value]", input = "pick", default = "ard",
                description = "What the item makes: an ARD (only these go to the ARD), a model, a value."),
    method = list(type = "string", input = "pick", choices = "fixed", values = methods, free = TRUE,
                  required = TRUE, tier = "required",
                  description = "The function called (categorical, continuous, hierarchical, ...)."),
    data = list(type = "string", input = "pick", choices = "report_data", free = FALSE,
                required = TRUE, tier = "required", description = "The data: row the item reads."),
    params = list(type = "list<string>", input = "text", tier = "other",
                  description = "The columns of the conditions table, in order."),
    conditions = list(type = "rows<map<string,string|r>>", input = "table",
                      # (the item's params are the columns; these two have their own form)
                      columns = list("params", where = list(type = "r", input = "condition"),
                                     analysis_id = list(type = "string", input = "text",
                                                        required = FALSE)),
                      tier = "other",
                      description = "One row a run of the item: a split of the data or one analysis."),
    where = list(type = "r", input = "condition", choices = "vars(from_args(data))",
                 description = "The records the item uses (a condition row's where: \"{where}\")."),
    derive = list(type = "map<column,r>", input = "table", choices = "vars(from_args(data))", tier = "other",
                  description = "Columns made for the item: name = R."),
    levels = list(type = "map<column,list<level>>", input = "table", choices = "vars(from_args(data))",
                  tier = "other", description = "The values of a column, in order (set_levels)."),
    by = list(type = "list<column>", input = "pick", choices = "vars(from_args(data))", ordered = TRUE,
              description = "The grouping columns."),
    variables = list(type = "list<column>", input = "pick", choices = "vars(from_args(data))", ordered = TRUE,
                     description = "The analysis variables."),
    statistics = list(type = "list<string>", input = "pick", choices = "stats", ordered = TRUE,
                      description = "The statistics kept."),
    denominator = list(type = "string", input = "pick", choices = "report_data", free = FALSE,
                       description = "The data: row of the % denominator."),
    overall = list(type = "logical", input = "pick", description = "A total over the by groups."),
    formats = list(type = "map<string,number|string>", input = "table", choices = "stats", free = TRUE,
                   description = "Digits by statistic; VAR:stat for one variable."),
    analysis_ids = list(type = "map<column,string>", input = "table", choices = "from_args(variables)",
                        tier = "other",
                        description = "A variable's analysis_id (default: the variable's name)."),
    args = list(type = "r", input = "text", tier = "other",
                description = "The function's other arguments, as R."),
    ards = list(type = "rows<map<string,list<column>>>", input = "table",
                columns = list(by = list(type = "list<column>", input = "pick", choices = "vars(from_args(data))"),
                               variables = list(type = "list<column>", input = "pick",
                                                choices = "vars(from_args(data))")),
                tier = "other",
                description = "Several calls of the method, one analysis_id (no id of their own)."),
    post = list(type = "r", input = "text", tier = "other", description = "R on the item's result."),
    code = list(type = "r", input = "text", tier = "other",
                description = "R for an item the keys cannot say.")
  )
}

# ---- a template's params: ---------------------------------------------------

# The rows a template writes with fixed values in the same list as a table
# param's "{name}" (forest_hr's conditions: the ALL row): a data frame, its
# attribute "position" each row's place, "before" or "after" "{name}"; NULL
# when there are none.
.arg_fixed_rows <- function(body, name) {
  spot <- paste0("{", name, "}")
  found <- NULL
  walk <- function(x) {
    if (!is.null(found) || !is.list(x)) return(invisible())
    if (is.null(names(x))) {
      at <- which(vapply(x, function(e) identical(e, spot), TRUE))
      if (length(at)) {
        is_map <- vapply(x, function(e) is.list(e) && !is.null(names(e)), TRUE)
        idx <- setdiff(which(is_map), at[1L])
        if (length(idx)) {
          found <<- list(rows = x[idx], pos = ifelse(idx < at[1L], "before", "after"))
        }
        return(invisible())
      }
    }
    for (e in x) walk(e)
  }
  walk(body)
  if (is.null(found)) return(NULL)
  cell <- function(v) {
    if (is.null(v)) return(NA_character_)
    if (inherits(v, "tfl_r")) return(as.character(unclass(v)))
    if (inherits(v, "tfl_tagged")) return(as.character(unlist(v$value))[1L])
    paste(as.character(unlist(v)), collapse = ", ")
  }
  cols <- unique(unlist(lapply(found$rows, names)))
  df <- as.data.frame(stats::setNames(lapply(cols, function(cn)
    vapply(found$rows, function(r) cell(r[[cn]]), "")), cols), stringsAsFactors = FALSE)
  attr(df, "position") <- found$pos
  df
}

# The template file of "<series>/<template>": with `study_dir`, the study's
# copy (spec/series/<series>/templates/<template>.yml) and nothing else; else
# the first series folder that has it (the option tflspec.series_dir,
# tflplanner's home standards/series/, the package's own series).
.template_file <- function(template, study_dir = NULL) {
  if (!is.character(template) || length(template) != 1L || !grepl("^[^/]+/[^/]+$", template)) {
    stop(sprintf("template %s: write <series>/<template>, e.g. \"tflspec_v1/demog\".",
                 encodeString(as.character(template)[1L], quote = "\"")), call. = FALSE)
  }
  parts <- strsplit(template, "/", fixed = TRUE)[[1L]]
  if (!grepl("^[a-z][a-z0-9_]*$", parts[1L])) {
    stop(sprintf("series %s: a series name is lower case letters, digits and _, starting with a letter.",
                 encodeString(parts[1L], quote = "\"")), call. = FALSE)
  }
  if (!is.null(study_dir)) {
    f <- file.path(.study_series_root(study_dir), parts[1L], "templates", paste0(parts[2L], ".yml"))
    if (file.exists(f)) return(f)
    stop(sprintf("template %s is not copied into the study (no %s): %s.", template, f,
                 .study_copy_hint(study_dir, template)), call. = FALSE)
  }
  dirs <- .series_search_dirs()
  dirs <- dirs[nzchar(dirs)]
  for (d in dirs) {
    f <- file.path(d, parts[1L], "templates", paste0(parts[2L], ".yml"))
    if (file.exists(f)) return(f)
  }
  stop(sprintf("template %s: no file %s in the series folders (%s).", template,
               file.path(parts[1L], "templates", paste0(parts[2L], ".yml")),
               paste(dirs, collapse = ", ")), call. = FALSE)
}

.template_read <- function(template, study_dir = NULL) {
  f <- .template_file(template, study_dir)
  y <- yaml::read_yaml(f, handlers = .arg_yaml_handlers())
  attr(y, "file") <- f
  y
}

#' The definitions of the arguments a card takes
#'
#' One definition of every argument, for the code SPEC's cards and the
#' templates' cards alike: the screen (tflplanner) and the AI kit read this
#' table only.  A code SPEC's card is a schema function (`inst/spec/schema.yml`)
#' or one of its two row cards (`"data"`, `"analysis"`); a template is one
#' large card made from its `params:`.
#'
#' @param fn A function of the schema (`"plan_cells"`), or `"data"` (the keys
#'   of a `data:` row) or `"analysis"` (the keys of an analysis item).
#' @param template A template, `"<series>/<template>"` (`"tflspec_v1/demog"`).
#'   Its file is found in the series folders: the option `tflspec.series_dir`,
#'   tflplanner's home `standards/series/`, the series tflspec ships.
#' @param study_dir A study folder: the template is read from the study's
#'   copy ([tfl_copy_templates()]) instead, a template of the study's own
#'   (copied with `as`) too.  `NULL` (default) for the series folders.
#' @return A data frame, one row an argument in the definition's order (the
#'   card's order):
#'   \describe{
#'     \item{name}{the argument.}
#'     \item{label_key}{the key of its label in the screen's text (en / ja).}
#'     \item{step}{`data`, `analysis`, `content` or `page`: the card's group.}
#'     \item{input}{`pick`, `text`, `condition` or `table`.}
#'     \item{type}{the data type, in the schema's notation: `logical`,
#'       `number`, `string`, `r`, `column`, `level`, `list<...>` (several),
#'       `enum[a|b]`, `map<...>`, `vec<...>`, `rows<...>` (a table),
#'       `template`, and their unions with `|`.}
#'     \item{required}{`TRUE` for an argument that has no default and must be
#'       given.}
#'     \item{default}{(a list column) the default as written; a tagged value
#'       (`!series K`, `!study K`, `!fixed v`, `!r`) is a `tfl_tagged` object.
#'       It is resolved for a study by [tfl_template_resolve()].}
#'     \item{default_kind}{`value`, `study`, `series`, `unset` or `fixed`;
#'       `NA` when there is no default.}
#'     \item{choices}{(a list column) the sources of the choices, in order:
#'       the closed words `vars(<data>)`, `levels(<var>)`, `codelist(<name>)`,
#'       `populations`, `report_data`, `study_data`, `ard`, `columns(<object>)`,
#'       `study_info`, `tokens`, `stats`, `fixed`, `styles`,
#'       `from_args(<arg>, ...)`.  Another word stops.}
#'     \item{values}{(a list column) the fixed values (`fixed`, an `enum`).}
#'     \item{free}{a pick that also takes a value not in the list.}
#'     \item{ordered}{a pick of several whose order matters.}
#'     \item{columns}{(a list column) a table's columns, defined the same way;
#'       `"params"` for a template's conditions table (its columns are the item's
#'       `params:`); in a list, an unnamed `"params"` marks where the item's
#'       params go (an item's conditions: `"params"`, `where`, an optional
#'       `analysis_id`).}
#'     \item{fixed_rows}{(a list column) a template's table param: the rows
#'       the template writes with fixed values next to its `"{name}"` (the
#'       ALL row of forest_hr's conditions), a data frame whose attribute
#'       `"position"` gives each row's place, `"before"` or `"after"` the
#'       report's rows; `NULL` otherwise.}
#'     \item{tier}{`required`, `usual` or `other` (shown folded).}
#'     \item{description}{one line.}
#'   }
#' @seealso [tfl_schema()] (the schema's functions), [tfl_template_resolve()]
#'   (a report's template with its values filled in, and where each came from).
#' @examples
#' d <- tfl_arg_defs(fn = "analysis")
#' d[, c("name", "input", "type", "tier")]
#' head(tfl_arg_defs(fn = "plan_cells")[, c("name", "input", "type")])
#' @export
tfl_arg_defs <- function(fn = NULL, template = NULL, study_dir = NULL) {
  if (is.null(fn) == is.null(template)) {
    stop("tfl_arg_defs(): give `fn` or `template`, one of them.", call. = FALSE)
  }
  if (!is.null(template)) {
    y <- .template_read(template, study_dir)
    defs <- y$params %||% list()
    if (!is.list(defs) || (length(defs) && is.null(names(defs)))) {
      stop(sprintf("template %s: params: is a map of argument definitions.", template),
           call. = FALSE)
    }
    out <- .arg_table(defs, "analysis", paste0("template.", sub("/", ".", template, fixed = TRUE), "."),
                      sprintf("template %s", template))
    y$params <- NULL
    for (i in seq_len(nrow(out))) out$fixed_rows[i] <- list(.arg_fixed_rows(y, out$name[i]))
    return(out)
  }
  fn <- as.character(fn)[1L]
  if (identical(fn, "data")) {
    return(.arg_table(.data_row_arg_defs(), "data", "data.", "data row"))
  }
  if (identical(fn, "analysis")) {
    return(.arg_table(.analysis_item_arg_defs(), "analysis", "analysis.", "analysis item"))
  }
  ent <- tfl_schema(fn)
  if (is.null(ent)) {
    stop(sprintf("tfl_arg_defs(): %s is not a function of the schema.",
                 encodeString(fn, quote = "\"")), call. = FALSE)
  }
  args <- .arg_schema_fill(fn, ent$args %||% list())
  step <- if (fn %in% .arg_page_fns) "page" else .arg_step_for_stage(ent$stage)
  .arg_table(args, step, paste0(.schema_fn_name(fn), "."), fn)
}
