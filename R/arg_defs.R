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
#    tfl_arg_defs(template = "tflspec_v1/tflspec_summary") a template's params:
# ============================================================================

# the 5 input forms: pick (choose from a list; one or several follows the
# type: list<...> = several, logical = a switch), text (type it; a number type
# is a number box), condition (write a condition, the columns as candidates),
# table (fill a table), r (an R expression: the one form with a "write in R"
# field; canon 17dfbb8)
.arg_input_forms <- c("pick", "text", "condition", "table", "r")

# the closed list of tflplanner's special editors (contract v3 section 9,
# point 16).  A definition (an argument, or a `dots:` entry) writes
# `editor: <word>` to name the editor the planner draws for it; unset, the
# planner draws the field from its input and type.  An argument with an
# editor still has its `input:` (the value form, for the CUI and the AI kit).
#   roles         table_plan's ordered column roles (cols, rows, label)
#   cells         plan_cells' per-variable statistic templates
#   header_lines  plan_col_header's header lines
#   digits        plan_digits' decimals per variable and statistic
#   levels        plan_levels' ordered values per variable
#   sort_keys     plan_sort's per-level sort keys
.arg_editors <- c("roles", "cells", "header_lines", "digits", "levels", "sort_keys")

# the closed list of choice sources (16 words, canon 11-14, the
# figure_style word of 2026-11-18 and objects(<kind>), B-2).  An entry writes one with what it
# depends on in brackets ("vars(adsl_sub)", "levels(SEX)",
# "from_args(continuous, categorical)", "figure_style(palette)"); several
# separated by ", " give the list's groups in that order
# ("report_data, study_data").
.arg_choice_sources <- c("vars", "levels", "codelist", "populations", "report_data",
                         "study_data", "ard", "columns", "study_info", "tokens",
                         "stats", "fixed", "styles", "from_args", "figure_style",
                         "objects")

# what figure_style(<kind>) lists: the study's figure style's colours in
# their order, or its marker shapes
.arg_figure_style_kinds <- c("palette", "marker")

# the words that let a pick take a value not in the list (`free`) unless the
# definition says otherwise
.arg_free_sources <- c("tokens", "study_info", "codelist")

.arg_steps <- c("data", "analysis", "content", "page")
.arg_default_kinds <- c("value", "study", "series", "unset", "fixed")
# required: must be given; key: shown even at its default; usual and other:
# folded
.arg_tiers <- c("required", "key", "usual", "other")
# what "(none)" (`!unset`) does, tfl_param()'s words (canon 1.17-1.20):
# omit (the argument is left out), step_off (its step is not made), stop
# (the body needs a value: "(none)" is not offered)
.arg_unset_words <- c("omit", "step_off", "stop")

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
  # figure_style(<kind>): the kind one of palette, marker
  fs <- entries[words == "figure_style"]
  fs_kind <- sub("^figure_style[(]\\s*(.*?)\\s*[)]$", "\\1", fs, perl = TRUE)
  fs_bad <- fs[!grepl("^figure_style[(].*[)]$", fs) | !fs_kind %in% .arg_figure_style_kinds]
  if (length(fs_bad)) {
    stop(sprintf("%s: %s: write figure_style(<kind>), the kind one of %s.", where,
                 paste(encodeString(fs_bad, quote = "\""), collapse = ", "),
                 paste(.arg_figure_style_kinds, collapse = ", ")), call. = FALSE)
  }
  # objects(<kind>): the objects made above and the study's datasets, of
  # one kind -- a word of tfl_fn_defs()' `returns` (B-2: one closed list)
  ob <- entries[words == "objects"]
  ob_kind <- sub("^objects[(]\\s*(.*?)\\s*[)]$", "\\1", ob, perl = TRUE)
  ob_bad <- ob[!grepl("^objects[(].*[)]$", ob) | !ob_kind %in% .fn_def_returns]
  if (length(ob_bad)) {
    stop(sprintf("%s: %s: write objects(<kind>), the kind one of %s.", where,
                 paste(encodeString(ob_bad, quote = "\""), collapse = ", "),
                 paste(.fn_def_returns, collapse = ", ")), call. = FALSE)
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

# A type's members: split at `|` outside <> and [] (`enum[a|b]` and
# `map<string, column|r>` are one member each)
.type_members <- function(type, sep = "|") {
  ch <- strsplit(as.character(type %||% "string")[1L], "", fixed = TRUE)[[1L]]
  depth <- cumsum(ch %in% c("<", "[")) - cumsum(ch %in% c(">", "]"))
  cut <- which(ch == sep & depth == 0L)
  if (!length(cut)) return(trimws(paste(ch, collapse = "")))
  from <- c(1L, cut + 1L)
  to <- c(cut - 1L, length(ch))
  trimws(vapply(seq_along(from), function(i) {
    paste(ch[seq_len(to[i] - from[i] + 1L) + from[i] - 1L], collapse = "")
  }, ""))
}

# A table's columns from its type, where the definition writes none (the
# field contract, section 4): the first table member's.  map<K, V> and
# vec<K, V> (or a list of them) give a key column of K and a value column of
# V, each with the input section 2 gives it, a value that is itself a map its
# own key and value; rows<f1, f2, ..> gives a column a field.  NULL: no
# table member (step<content>, whose items are cards).
.arg_type_columns <- function(type) {
  m <- sub("^list<((map|vec)<.*>)>$", "\\1", .type_members(type))
  t <- m[grepl("^(map|vec|rows)<", m)][1L]
  if (is.na(t)) return(NULL)
  inner <- .type_members(sub("^[a-z]+<(.*)>$", "\\1", t), sep = ",")
  if (startsWith(t, "rows<")) {
    out <- lapply(inner, function(f) list(label = list(en = f, ja = f)))
    return(stats::setNames(out, inner))
  }
  list(key = .arg_type_column(inner[1L], "key"),
       value = .arg_type_column(inner[length(inner)], "value"))
}

# ... one column: its type, input and label (by what the key is: a column,
# a statistic, else a name; the value "Value")
.arg_type_column <- function(type, role) {
  label <- if (identical(role, "value")) list(en = "Value", ja = "\u5024") else
    switch(.type_deciding(type),
           column = list(en = "Column", ja = "\u5217"),
           stat = list(en = "Statistic", ja = "\u7d71\u8a08\u91cf"),
           list(en = "Name", ja = "\u540d\u524d"))
  col <- list(type = type, input = .arg_input_for_type(type), label = label)
  if (grepl("^(map|vec)<", .type_deciding(type))) col$columns <- .arg_type_columns(.type_deciding(type))
  col
}

# The member that decides the field: the first that is not `r` ("r" when r
# is all there is)
.type_deciding <- function(type) {
  m <- .type_members(type)
  m <- m[m != "r"]
  if (length(m)) m[1L] else "r"
}

# the input one member takes (the field contract, 2026-11-22, section 2)
.arg_member_input <- function(m) {
  if (grepl("^(rows|map|vec|step)<", m) || grepl("^list<(map|vec)<", m)) return("table")
  if (identical(m, "logical") || startsWith(m, "enum[") ||
      grepl("^(list<)?(column|level|stat)>?$", m) || startsWith(m, "list<enum[")) return("pick")
  if (m %in% c("r", "formula", "guard") || grepl("^list<(r|call)[<>]", m)) return("r")
  # (the rest -- string, number, colour, template, date, and object, call and
  # call<fn>, which the planner draws by the type: a pick of the objects, the
  # call's own small form -- is text: decided 2026-11-22, the call form in F3)
  "text"
}

# the input form a type takes when the definition does not say: the first
# member that is not `r` decides it.  (A `where` says `input: condition`
# itself; no argument's name decides it.)
.arg_input_for_type <- function(type) {
  .arg_member_input(.type_deciding(type))
}

# a schema function's stage as a step: the content stage's mapping and
# finishing, the page's output
.arg_step_for_stage <- function(stage) {
  if (identical(stage, "output")) "page" else "content"
}

# the plan_paginate_*() functions are the page step
.arg_page_fns <- c("plan_paginate_rows", "plan_paginate_cols", "plan_paginate_group")

# What a schema function's argument gets where schema.yml is silent: a
# statistic member (`stat`, `list<stat>`) picks from the statistics' names.
# That is a rule of the type.  A column argument's choices (ard, columns,
# vars) and an ordered list (`ordered: true`) are written in the definition,
# not worked out here from the function's name or package.
.arg_schema_fill <- function(args) {
  for (n in names(args)) {
    d <- args[[n]]
    if (is.list(d) && is.null(d$choices) &&
        any(grepl("^(list<)?stat>?$", .type_members(as.character(d$type %||% "string"))))) {
      args[[n]]$choices <- "stats"
    }
  }
  args
}

# The fixed values of a type's members: each `enum[..]` member's words (a
# `list<enum[..]>`'s too), and TRUE / FALSE when a `logical` is beside other
# members (`logical|enum[single|double]`: TRUE, FALSE, single, double;
# `logical|string`: TRUE, FALSE; a plain logical has none: a switch)
.arg_enum_values <- function(type) {
  m <- .type_members(type %||% "")
  m <- sub("^list<(enum[[].*[]])>$", "\\1", m)
  e <- m[startsWith(m, "enum[")]
  vals <- unlist(lapply(e, function(x) strsplit(sub("^enum[[]([^]]*)[]].*$", "\\1", x), "|", fixed = TRUE)[[1L]]))
  if ("logical" %in% m && sum(m != "r") > 1L) vals <- c("TRUE", "FALSE", vals)
  if (!length(vals)) return(NULL)
  unique(vals)
}

# A label or a help as written: `{en: , ja: }` or a plain text (English, ja
# NA).  Returns c(en, ja).
.arg_en_ja <- function(x, where, what) {
  na <- c(en = NA_character_, ja = NA_character_)
  if (is.null(x)) return(na)
  if (is.list(x) && !is.null(names(x))) {
    bad <- setdiff(names(x), c("en", "ja"))
    ok <- !length(bad) && all(vapply(x, function(v) is.null(v) || (is.character(v) && length(v) == 1L), NA))
    if (ok) {
      return(c(en = as.character(x$en %||% NA_character_), ja = as.character(x$ja %||% NA_character_)))
    }
  } else if (is.character(x) && length(x) == 1L) {
    return(c(en = x, ja = NA_character_))
  }
  stop(sprintf("%s: %s is a text or {en: , ja: }.", where, what), call. = FALSE)
}

# A definition's fixed values with their labels: the values written as
# plain values or as `{value: , label: {en, ja}}`, else an enum type's; a
# `labels:` map (value: label) labels them too.  list(values, labels): the
# values a character vector (NULL for none), the labels a data frame
# (value, label_en, label_ja) or NULL when no value has one.
.arg_values <- function(d, type, where) {
  # ([[ ]]: a `$` would read `labels` for a missing `label`)
  raw <- d[["values"]]
  if (is.null(raw)) {
    vals <- .arg_enum_values(type)
    lab <- vector("list", length(vals))
  } else {
    raw <- if (is.list(raw) && !is.null(names(raw)) && "value" %in% names(raw)) list(raw) else as.list(raw)
    vals <- vapply(raw, function(v) {
      if (is.list(v)) v <- v[["value"]]
      if (is.null(v) || length(v) != 1L || is.list(v)) {
        stop(sprintf("%s: values: an entry is a value or {value: , label: }.", where), call. = FALSE)
      }
      as.character(v)
    }, "")
    lab <- lapply(raw, function(v) if (is.list(v)) v[["label"]])
  }
  if (!length(vals)) return(list(values = NULL, labels = NULL))
  extra <- d[["labels"]]
  if (!is.null(extra) && (!is.list(extra) || is.null(names(extra)))) {
    stop(sprintf("%s: labels: is a map, a value or a column to its label.", where), call. = FALSE)
  }
  ens <- lapply(seq_along(vals), function(i) {
    .arg_en_ja(extra[[vals[i]]] %||% lab[[i]], where, sprintf("the label of value %s", vals[i]))
  })
  en <- vapply(ens, `[[`, "", "en")
  ja <- vapply(ens, `[[`, "", "ja")
  labels <- if (any(!is.na(en) | !is.na(ja))) {
    data.frame(value = vals, label_en = en, label_ja = ja, stringsAsFactors = FALSE)
  }
  # (plain values stay as written)
  list(values = if (is.atomic(d[["values"]]) && !is.null(d[["values"]])) d[["values"]] else vals,
       labels = labels)
}

# A table's columns as written, each named column's definition given its
# label_en / label_ja / help_en / help_ja (from its own `label:` / `help:`,
# or the argument's `labels:` map); "params" and other entries kept.
.arg_columns <- function(cols, labels, where) {
  if (!is.list(cols) || is.null(names(cols))) return(cols)
  for (k in names(cols)[nzchar(names(cols))]) {
    cd <- cols[[k]]
    if (!is.list(cd)) next
    w <- sprintf("%s: column `%s`", where, k)
    l <- .arg_en_ja(labels[[k]] %||% cd[["label"]], w, "label")
    h <- .arg_en_ja(cd[["help"]], w, "help")
    cd$label_en <- l[["en"]]
    cd$label_ja <- l[["ja"]]
    cd$help_en <- if (is.na(h[["en"]])) as.character(cd[["description"]] %||% NA_character_) else h[["en"]]
    cd$help_ja <- h[["ja"]]
    # a map's value that is itself a map has its own key / value columns
    if (is.list(cd[["columns"]])) cd$columns <- .arg_columns(cd[["columns"]], NULL, w)
    cols[[k]] <- cd
  }
  cols
}

# A table's new_row as written: the values a row the user adds starts
# with, by column; each a named column of `cols`.  NULL for none.
.arg_new_row <- function(nr, cols, where) {
  if (is.null(nr)) return(NULL)
  if (!is.list(nr) || is.null(names(nr)) || !all(nzchar(names(nr)))) {
    stop(sprintf("%s: new_row is a map, column: value.", where), call. = FALSE)
  }
  known <- if (is.list(cols)) names(cols)[nzchar(names(cols))] else character()
  bad <- setdiff(names(nr), known)
  if (length(bad)) {
    stop(sprintf("%s: new_row column `%s` is not one of the table's columns (%s).", where, bad[1L],
                 if (length(known)) paste(known, collapse = ", ") else "none written"),
         call. = FALSE)
  }
  nr
}

# A passes_to as written (`passes_to:`, or `maps_to:` in a template, the
# same thing): where the value goes, "<fn>.<arg>" or "<fn>.<arg>.<name>"
# (the study keys' grammar), one or a list.  character() for none.
.arg_passes_re <- "^[A-Za-z][A-Za-z0-9_]*[.][.]?[A-Za-z_][A-Za-z0-9_]*([.][A-Za-z0-9_]+)?$"
.arg_passes_to <- function(d, where) {
  if (!is.null(d[["passes_to"]]) && !is.null(d[["maps_to"]])) {
    stop(sprintf("%s: passes_to: and maps_to: say the same; write passes_to: only.", where),
         call. = FALSE)
  }
  x <- d[["passes_to"]] %||% d[["maps_to"]]
  if (is.null(x)) return(character())
  ok <- (is.character(x) || (is.list(x) && all(vapply(x, function(v) is.character(v) && length(v) == 1L, NA)))) &&
    length(x) > 0L
  x <- if (ok) as.character(unlist(x)) else character()
  if (!ok || anyNA(x) || !all(grepl(.arg_passes_re, x))) {
    stop(sprintf(paste0("%s: passes_to: is \"<fn>.<arg>\" or \"<fn>.<arg>.<name>\" ",
                        "(\"plan_blanks.where\"), one or a list."), where), call. = FALSE)
  }
  x
}

# A `groups:` list (a function's or a template's): one entry `{id, label}`
# a group, in the card's order.  A data frame (id, label_en, label_ja); no
# rows when there is none.
.arg_groups <- function(groups, where) {
  empty <- data.frame(id = character(), label_en = character(), label_ja = character(),
                      stringsAsFactors = FALSE)
  if (is.null(groups) || !length(groups)) return(empty)
  if (!is.list(groups) || !is.null(names(groups))) {
    stop(sprintf("%s: groups: is a list of {id: , label: {en: , ja: }}.", where), call. = FALSE)
  }
  rows <- lapply(groups, function(g) {
    id <- if (is.list(g)) g[["id"]]
    if (!is.character(id) || length(id) != 1L || !grepl("^[A-Za-z][A-Za-z0-9_]*$", id)) {
      stop(sprintf("%s: groups: each entry is {id: , label: }, the id a name (letters, digits, _).",
                   where), call. = FALSE)
    }
    l <- .arg_en_ja(g[["label"]], sprintf("%s: group `%s`", where, id), "label")
    data.frame(id = id, label_en = l[["en"]], label_ja = l[["ja"]], stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (anyDuplicated(out$id)) {
    stop(sprintf("%s: groups: the id %s is given twice.", where,
                 encodeString(out$id[anyDuplicated(out$id)], quote = "\"")), call. = FALSE)
  }
  out
}

# One row of the table from one definition (a list as written in the schema
# or a template's params:).
.arg_row <- function(name, d, step, where) {
  if (!is.list(d)) d <- list(default = d)
  where_arg <- sprintf("%s: argument `%s`", where, name)
  type <- as.character(d$type %||% "string")
  input <- as.character(d$input %||% .arg_input_for_type(type))
  if (!input %in% .arg_input_forms) {
    stop(sprintf("%s: argument `%s`: input %s is not one of %s.", where, name,
                 encodeString(input, quote = "\""), paste(.arg_input_forms, collapse = ", ")),
         call. = FALSE)
  }
  editor <- d[["editor"]] %||% NA_character_
  if (!(is.character(editor) && length(editor) == 1L &&
        (is.na(editor) || editor %in% .arg_editors))) {
    stop(sprintf("%s: argument `%s`: editor %s is not one of %s.", where, name,
                 encodeString(paste(as.character(editor), collapse = ", "), quote = "\""),
                 paste(.arg_editors, collapse = ", ")), call. = FALSE)
  }
  st <- as.character(d$step %||% step)
  if (!st %in% .arg_steps) {
    stop(sprintf("%s: argument `%s`: step %s is not one of %s.", where, name,
                 encodeString(st, quote = "\""), paste(.arg_steps, collapse = ", ")),
         call. = FALSE)
  }
  choices <- d$choices
  vl <- .arg_values(d, type, where_arg)
  values <- vl$values
  if (is.null(choices) && length(values)) choices <- "fixed"
  .arg_check_choices(choices, where_arg)
  words <- sub("[(].*$", "", .arg_choice_words(choices))
  # (a text member beside a pick -- a string, a number, an integer, or a
  # list of them -- lets it take a value not in the list: row_title's
  # column numbers, wrap's text)
  beside <- any(grepl("^(list<)?(string|number|integer)>?$", .type_members(type)))
  free <- d$free %||% (if (input == "pick" && (length(words) || beside)) {
    any(words %in% .arg_free_sources) || beside
  } else NA)
  ordered <- d$ordered %||% (if (input == "pick" && startsWith(.type_deciding(type), "list<")) FALSE else NA)
  has_default <- "default" %in% names(d)
  default <- if (has_default) d$default else NULL
  required <- isTRUE(d$required) || (identical(d$tier, "required") && !has_default)
  tier <- as.character(d$tier %||% (if (required) "required" else "usual"))
  if (length(tier) != 1L || !tier %in% .arg_tiers) {
    stop(sprintf("%s: tier %s is not one of %s.", where_arg,
                 encodeString(paste(tier, collapse = ", "), quote = "\""),
                 paste(.arg_tiers, collapse = ", ")), call. = FALSE)
  }
  unset <- as.character(d[["unset"]] %||% (if (required) "stop" else "omit"))
  if (length(unset) != 1L || !unset %in% .arg_unset_words) {
    stop(sprintf("%s: unset %s is not one of %s.", where_arg,
                 encodeString(paste(unset, collapse = ", "), quote = "\""),
                 paste(.arg_unset_words, collapse = ", ")), call. = FALSE)
  }
  group <- d[["group"]] %||% NA_character_
  if (!(is.character(group) && length(group) == 1L)) {
    stop(sprintf("%s: group is one group's id.", where_arg), call. = FALSE)
  }
  member_of <- d[["member_of"]] %||% NA_character_
  if (!(is.character(member_of) && length(member_of) == 1L)) {
    stop(sprintf("%s: member_of is the name of one argument (a list).", where_arg), call. = FALSE)
  }
  label <- .arg_en_ja(d[["label"]], where_arg, "label")
  help <- .arg_en_ja(d[["help"]], where_arg, "help")
  description <- as.character(d[["description"]] %||% help[["en"]])
  if (!length(description) || is.na(description)) description <- ""
  row <- data.frame(name = name, stringsAsFactors = FALSE)
  row$label_en <- label[["en"]]
  row$label_ja <- label[["ja"]]
  row$help_en <- if (is.na(help[["en"]]) && nzchar(description)) description else help[["en"]]
  row$help_ja <- help[["ja"]]
  row$step <- st
  row$group <- group
  row$input <- input
  row$type <- type
  row$required <- required
  row$default <- I(list(default))
  row$default_kind <- if (has_default) .arg_default_kind(default) else NA_character_
  row$unset <- unset
  row$choices <- I(list(.arg_choice_words(choices)))
  row$values <- I(list(values))
  row$value_labels <- I(list(vl$labels))
  row$free <- as.logical(free)
  row$ordered <- as.logical(ordered)
  # (a table whose definition writes no columns: gets them from its type)
  cols <- d[["columns"]] %||% (if (identical(input, "table")) .arg_type_columns(type))
  row$columns <- I(list(.arg_columns(cols, d[["labels"]], where_arg)))
  row$new_row <- I(list(.arg_new_row(d[["new_row"]], d[["columns"]], where_arg)))
  row$fixed_rows <- I(list(NULL))
  row$tier <- tier
  row$passes_to <- I(list(.arg_passes_to(d, where_arg)))
  row$member_of <- member_of
  row$description <- description
  row$editor <- editor
  # the `...` row only (from .arg_schema_dots()): how the dots' entries are
  # written; NA on every other row
  row$dots_style <- as.character(d[["dots_style"]] %||% NA_character_)
  row$dots_key <- as.character(d[["dots_key"]] %||% NA_character_)
  row$dots_map <- as.logical(d[["dots_map"]] %||% NA)
  row
}

# The table of a definition list (`defs`, by argument) and its `groups:`
# (attr "groups": id, label_en, label_ja, in the card's order).  An
# argument's group must be one of them; a member_of one of the arguments.
.arg_table <- function(defs, step, where, groups = NULL) {
  g <- .arg_groups(groups, where)
  if (!length(defs)) {
    out <- .arg_row("x", list(), step, where)[0L, , drop = FALSE]
  } else {
    rows <- Map(function(n, d) .arg_row(n, d, step, where), names(defs), defs)
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
  }
  bad <- !is.na(out$group) & !out$group %in% g$id
  if (any(bad)) {
    stop(sprintf("%s: argument `%s`: group %s is not in groups: (%s).", where, out$name[bad][1L],
                 encodeString(out$group[bad][1L], quote = "\""),
                 if (nrow(g)) paste(g$id, collapse = ", ") else "none written"), call. = FALSE)
  }
  bad <- !is.na(out$member_of) & (!out$member_of %in% out$name | out$member_of == out$name)
  if (any(bad)) {
    stop(sprintf("%s: argument `%s`: member_of %s is not another argument of the card.", where,
                 out$name[bad][1L], encodeString(out$member_of[bad][1L], quote = "\"")),
         call. = FALSE)
  }
  attr(out, "groups") <- g
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
    then = list(type = "r", input = "r",
                description = "R after the keys above, on the row's data."),
    code = list(type = "r", input = "r", tier = "other",
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
    args = list(type = "r", input = "r", tier = "other",
                description = "The function's other arguments, as R."),
    ards = list(type = "rows<map<string,list<column>>>", input = "table",
                columns = list(by = list(type = "list<column>", input = "pick", choices = "vars(from_args(data))"),
                               variables = list(type = "list<column>", input = "pick",
                                                choices = "vars(from_args(data))")),
                tier = "other",
                description = "Several calls of the method, one analysis_id (no id of their own)."),
    post = list(type = "r", input = "r", tier = "other", description = "R on the item's result."),
    code = list(type = "r", input = "r", tier = "other",
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
    stop(sprintf("template %s: write <series>/<template>, e.g. \"tflspec_v1/tflspec_summary\".",
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
    # (a retired template: its replacement, not a copy that cannot be made)
    note <- .series_retired_note(template)
    stop(sprintf("template %s is not copied into the study (no %s): %s.", template, f,
                 if (nzchar(note)) note else .study_copy_hint(study_dir, template)), call. = FALSE)
  }
  dirs <- .series_search_dirs()
  dirs <- dirs[nzchar(dirs)]
  for (d in dirs) {
    f <- file.path(d, parts[1L], "templates", paste0(parts[2L], ".yml"))
    if (file.exists(f)) return(f)
  }
  note <- .series_retired_note(template)
  stop(sprintf("template %s: no file %s in the series folders (%s).%s", template,
               file.path(parts[1L], "templates", paste0(parts[2L], ".yml")),
               paste(dirs, collapse = ", "), if (nzchar(note)) paste0(" ", note, ".") else ""),
       call. = FALSE)
}

# The `groups:` of the series a template file belongs to (its folder's
# series.yml, next to templates/); NULL when it writes none
.arg_series_groups <- function(file) {
  f <- file.path(dirname(dirname(file)), "series.yml")
  if (!file.exists(f)) return(NULL)
  yaml::read_yaml(f, handlers = .arg_yaml_handlers())[["groups"]]
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
#' @param template A template, `"<series>/<template>"` (`"tflspec_v1/tflspec_summary"`).
#'   Its file is found in the series folders: the option `tflspec.series_dir`,
#'   tflplanner's home `standards/series/`, the series tflspec ships.
#' @param study_dir A study folder: the template is read from the study's
#'   copy ([tfl_copy_templates()]) instead, a template of the study's own
#'   (copied with `as`) too.  `NULL` (default) for the series folders.
#' @return A data frame, one row an argument in the definition's order (the
#'   card's order); a schema function with `...` (its `dots:`) has a last
#'   row `...`, of the dots' type.  Its attribute `"groups"` is the card's
#'   groups in their order, a data frame `id`, `label_en`, `label_ja` from
#'   the function's `groups:` list or, for a template, its series'
#'   `series.yml` (a template's own `groups:` replaces it), entries `{id: ,
#'   label: {en: , ja: }}`; no rows when none is written (one unnamed
#'   group).  For `fn`, its attribute `"fn"` is the function's one-row data
#'   frame of [tfl_fn_defs()] (its join, the argument the pipe fills, what
#'   it gives; `"data"` is the `data_row` row), `NULL` when it has none.
#'   The columns:
#'   \describe{
#'     \item{name}{the argument.}
#'     \item{label_en, label_ja}{its label, from `label: {en: , ja: }`; a
#'       plain `label:` is English (`label_ja` `NA`); `NA` when there is none.}
#'     \item{help_en, help_ja}{its help, from `help: {en: , ja: }` or a
#'       plain `help:` (English), else the description.}
#'     \item{step}{`data`, `analysis`, `content` or `page`: the card's part.}
#'     \item{group}{the id of its group (one of the attribute `"groups"`),
#'       from `group:`; groups divide a step.  `NA`: the unnamed group.}
#'     \item{input}{`pick`, `text`, `condition`, `table` or `r` (an R
#'       expression, the one form whose field can be written in R; the
#'       default for a type `r`, `formula` or `guard`, `condition` for a
#'       `where`).}
#'     \item{type}{the data type, in the schema's notation: `logical`,
#'       `number`, `string`, `r`, `column`, `level`, `list<...>` (several),
#'       `enum[a|b]`, `map<...>`, `vec<...>`, `rows<...>` (a table),
#'       `template`, and their unions with `|`.}
#'     \item{required}{`TRUE` for an argument that has no default and must be
#'       given.}
#'     \item{default}{(a list column) the default as written; a tagged value
#'       (`!series K`, `!study K`, `!study {key: K, fallback: V}`, `!fixed v`, `!r`) is a `tfl_tagged` object.
#'       It is resolved for a study by [tfl_template_resolve()].  A schema
#'       function's argument with no default written takes the function's own
#'       default when that is one `TRUE` or `FALSE` and the argument's type is
#'       a logical kind; any other formal default gives none.}
#'     \item{default_kind}{`value`, `study`, `series`, `unset` or `fixed`;
#'       `NA` when there is no default.}
#'     \item{unset}{what "(none)" (`!unset`) does, from `unset:` (the words
#'       of [tfl_param()]): `omit` (the argument is left out), `step_off`
#'       (its step is not made) or `stop` (it needs a value: "(none)" is not
#'       offered); by default `stop` for a required argument, else `omit`.}
#'     \item{choices}{(a list column) the sources of the choices, in order:
#'       the closed words `vars(<data>)`, `levels(<var>)`, `codelist(<name>)`,
#'       `populations`, `report_data`, `study_data`, `ard`, `columns(<object>)`,
#'       `study_info`, `tokens`, `stats`, `fixed`, `styles`,
#'       `from_args(<arg>, ...)`, `figure_style(<palette|marker>)` (the
#'       study's figure style's colours in their order, or its marker
#'       shapes), `objects(<kind>)` (the objects made above and the
#'       study's datasets, of one kind of [tfl_fn_defs()]' `returns`).
#'       Another word stops.}
#'     \item{values}{(a list column) the fixed values (`fixed`, an `enum`).
#'       `values:` writes each as a value or `{value: , label: {en: , ja: }}`.}
#'     \item{value_labels}{(a list column) the values' labels, a data frame
#'       `value`, `label_en`, `label_ja` (from the values' `label:` or the
#'       argument's `labels:` map, value: label); `NULL` when none has one.}
#'     \item{free}{a pick that also takes a value not in the list.}
#'     \item{ordered}{a pick of several whose order matters.}
#'     \item{columns}{(a list column) a table's columns, defined the same way;
#'       `"params"` for a template's conditions table (its columns are the item's
#'       `params:`); in a list, an unnamed `"params"` marks where the item's
#'       params go (an item's conditions: `"params"`, `where`, an optional
#'       `analysis_id`).  A named column's definition also has `label_en`,
#'       `label_ja`, `help_en`, `help_ja`, from its own `label:` / `help:`
#'       or the argument's `labels:` map (column: label).  A `map<K, V>`'s
#'       two columns are written `columns: {key: {label: }, value: {label: }}`;
#'       a value that is itself a map has its own `columns:` (the same form).}
#'     \item{new_row}{(a list column) a table's values for a row the user
#'       adds, by column (`new_row: {key: subgroup}`); `NULL` when none.  Each
#'       name must be one of the argument's `columns:`.}
#'     \item{fixed_rows}{(a list column) a template's table param: the rows
#'       the template writes with fixed values next to its `"{name}"` (the
#'       ALL row of forest_hr's conditions), a data frame whose attribute
#'       `"position"` gives each row's place, `"before"` or `"after"` the
#'       report's rows; `NULL` otherwise.}
#'     \item{tier}{`required` (must be given), `key` (shown even at its
#'       default), `usual` or `other` (both shown folded).  A schema
#'       function's argument in its `visible:` list or marked `main: true` is
#'       `key`.}
#'     \item{passes_to}{(a list column) where the value goes, as written in
#'       `passes_to:` (a template's `maps_to:` is the same):
#'       `"<fn>.<arg>"` or `"<fn>.<arg>.<name>"`, the study keys' grammar,
#'       one or several; empty for none.}
#'     \item{member_of}{the list argument it is a member of (written
#'       `member_of:`, after its parent); `NA` otherwise.}
#'     \item{description}{one line.}
#'     \item{editor}{the tflplanner special editor the definition names, from
#'       `editor:`: one of `roles`, `cells`, `header_lines`, `digits`,
#'       `levels`, `sort_keys` (the closed list kept here); `NA` when none (the
#'       planner draws the field from `input` and `type`).  A row with an
#'       editor still has its `input`, the value form for the CUI and the AI
#'       kit.  A schema function's `dots:` may write it too (the `...` row).}
#'     \item{dots_style}{the `...` row only: `named` (the item's other keys are
#'       the entries, by name) or `positional` (the entries are the value of
#'       the one key `dots_key`), from the `dots:` entry's `style:`; `NA` on
#'       every other row.}
#'     \item{dots_key}{the `...` row only: the key of positional dots
#'       (`plan_sort`: `keys`); `NA` for named dots and on other rows.}
#'     \item{dots_map}{the `...` row only: `TRUE` when R code may also give
#'       the entries as one named vector (`plan_labels(c(AGE = "Age"))`);
#'       `NA` on other rows.}
#'   }
#'
#'   A definition stops, saying what to write, for an unknown `tier` or
#'   `unset` or `editor` word, a `group` not in `groups:`, a `member_of` that is not
#'   another argument, a malformed `passes_to`, a label or help that is
#'   neither a text nor `{en: , ja: }`, and a choice word outside the 15.
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
    # the groups: the series' series.yml (one list for all its templates,
    # canon db1bae9); a template's own groups: replaces it
    groups <- y[["groups"]] %||% .arg_series_groups(attr(y, "file"))
    out <- .arg_table(defs, "analysis", sprintf("template %s", template), groups)
    y$params <- NULL
    y[["groups"]] <- NULL
    for (i in seq_len(nrow(out))) out$fixed_rows[i] <- list(.arg_fixed_rows(y, out$name[i]))
    return(out)
  }
  fn <- as.character(fn)[1L]
  if (identical(fn, "data")) {
    out <- .arg_table(.data_row_arg_defs(), "data", "data row")
    attr(out, "fn") <- .fn_def_row(fn)
    return(out)
  }
  if (identical(fn, "analysis")) {
    out <- .arg_table(.analysis_item_arg_defs(), "analysis", "analysis item")
    attr(out, "fn") <- .fn_def_row(fn)
    return(out)
  }
  ent <- tfl_schema(fn)
  if (is.null(ent)) {
    stop(sprintf("tfl_arg_defs(): %s is not a function of the schema.",
                 encodeString(fn, quote = "\"")), call. = FALSE)
  }
  args <- .arg_schema_key(.arg_schema_fill(.arg_schema_dots(ent$args %||% list(), ent$dots)),
                          ent$visible)
  args <- .arg_formal_defaults(args, ent$formal_defaults)
  step <- if (fn %in% .arg_page_fns) "page" else .arg_step_for_stage(ent$stage)
  out <- .arg_table(args, step, fn, ent[["groups"]])
  attr(out, "fn") <- .fn_def_row(fn)
  out
}

# An argument whose definition writes no `default` takes the function's own
# default when that is one non-NA logical (`formal_defaults`, from
# .schema_add_formals()) and the argument's type is a logical kind (its
# deciding member is `logical`), as the planner's card does.  A written
# default wins; `...` gets none; a required argument keeps its tier, so
# `required` does not change.
.arg_formal_defaults <- function(args, formal_defaults) {
  for (n in setdiff(names(formal_defaults), "...")) {
    d <- args[[n]]
    v <- formal_defaults[[n]]
    if (!is.list(d) || "default" %in% names(d) || isTRUE(d$required) ||
        identical(d$tier, "required")) next
    if (!(is.logical(v) && length(v) == 1L && !is.na(v)) ||
        !identical(.type_deciding(as.character(d$type %||% "string")), "logical")) next
    args[[n]]["default"] <- list(v)
  }
  args
}

# A schema function's `...` (its `dots:`): one row, last, with the dots'
# type and description; an args entry `...` (its label) adds to it.  No
# dots: an args entry `...` stays as written.  An args entry with no type
# of its own names what the dots take (the positional dots' `key:`, a
# named dots' name: plan_sort keys, plan_filter where, plan_cells
# default): it takes the dots' type
.arg_schema_dots <- function(args, dots) {
  if (!is.list(dots)) return(args)
  for (n in setdiff(names(args), "...")) {
    if (is.list(args[[n]]) && is.null(args[[n]]$type)) {
      args[[n]]$type <- dots$type %||% "string"
      if (is.null(args[[n]]$input)) args[[n]]$input <- dots$input
    }
  }
  d <- list(type = dots$type %||% "string", description = dots$description %||% "")
  if (!is.null(dots$input)) d$input <- dots$input
  if (!is.null(dots$choices)) d$choices <- dots$choices
  if (!is.null(dots$values)) d$values <- dots$values
  if (isTRUE(dots$free)) d$free <- TRUE
  if (!is.null(dots$label)) d$label <- dots$label
  if (!is.null(dots$editor)) d$editor <- dots$editor
  d$dots_style <- dots$style %||% NA_character_
  d$dots_key <- dots$key %||% NA_character_
  d$dots_map <- isTRUE(dots$map)
  own <- args[["..."]]
  if (is.list(own)) for (k in names(own)) d[k] <- list(own[[k]])
  args[["..."]] <- NULL
  args[["..."]] <- d
  args
}

# A schema function's tier `key` (shown even at its default), from the
# function's `visible:` list and its arguments' `main: true`; a written
# tier and a required argument keep theirs.
.arg_schema_key <- function(args, visible) {
  visible <- as.character(unlist(visible))
  for (n in names(args)) {
    d <- args[[n]]
    if (!is.list(d) || !is.null(d$tier) || isTRUE(d$required)) next
    if (n %in% visible || isTRUE(d$main)) args[[n]]$tier <- "key"
  }
  args
}
