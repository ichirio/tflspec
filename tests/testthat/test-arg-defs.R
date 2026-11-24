cols <- c("name", "label_en", "label_ja", "help_en", "help_ja", "step", "group", "input", "type",
          "required", "default", "default_kind", "unset", "choices", "values", "value_labels",
          "free", "ordered", "columns", "new_row", "fixed_rows", "tier", "passes_to", "member_of",
          "description", "editor", "dots_style", "dots_key", "dots_map")

test_that("tfl_arg_defs() takes fn or template, one of them", {
  expect_error(tfl_arg_defs(), "one of them")
  expect_error(tfl_arg_defs(fn = "data", template = "tflspec_v1/demog"), "one of them")
})

test_that("the data row's and the analysis item's keys are defined", {
  d <- tfl_arg_defs(fn = "data")
  expect_identical(names(d), cols)
  expect_true(all(c("id", "from", "population", "subjects_in", "add", "where", "derive",
                    "keep", "distinct", "arrange", "then", "code") %in% d$name))

  expect_false("when" %in% d$name)
  expect_true(all(d$step == "data"))
  expect_identical(d$input[d$name == "where"], "condition")
  a <- tfl_arg_defs(fn = "analysis")
  expect_true(all(c("id", "type", "method", "data", "params", "conditions", "derive",
                    "levels", "ards", "analysis_ids") %in% a$name))

  expect_false("when" %in% a$name)
  cc <- a$columns[[which(a$name == "conditions")]]
  expect_identical(cc[[1L]], "params")
  expect_identical(cc$where$input, "condition")
  expect_false(cc$analysis_id$required)
  expect_true(a$free[a$name == "formats"])
  expect_identical(a$values[[which(a$name == "type")]], c("ard", "model", "value"))
  expect_identical(a$default_kind[a$name == "type"], "value")
  expect_true(a$required[a$name == "method"])
  expect_true(all(a$input %in% c("pick", "text", "condition", "table", "r")))
  expect_identical(a$input[a$name == "code"], "r")
})

test_that("a schema function's arguments come back in the same table", {
  d <- tfl_arg_defs(fn = "plan_cells")
  expect_identical(names(d), cols)
  expect_true("stats" %in% d$name)
  expect_identical(d$input[d$name == "stats"], "pick")
  expect_identical(d$values[[which(d$name == "stats")]], c("cells", "rows"))
  expect_true(all(d$step == "content"))
  expect_error(tfl_arg_defs(fn = "no_such_function"), "not a function of the schema")
})

test_that("generic defs name another argument with from_args(), a plain vars() is a data row", {
  d <- tfl_arg_defs(fn = "data")
  ch <- function(x, n) x$choices[[which(x$name == n)]]
  expect_identical(ch(d, "where"), "vars(from_args(from))")
  expect_identical(ch(d, "add"), "vars(from_args(subjects_in))")
  expect_identical(ch(d, "subjects_in"), c("report_data", "fixed"))
  expect_identical(d$values[[which(d$name == "subjects_in")]], "none")
  a <- tfl_arg_defs(fn = "analysis")
  expect_identical(ch(a, "variables"), "vars(from_args(data))")
  expect_silent(.arg_check_choices("vars(from_args(from))", "x"))
  expect_error(.arg_check_choices("vars(report_data(from))", "x"), "vars\\(\\) takes")
})

test_that("a schema column argument without choices gets ard (normalize_ard) or columns (plan_*)", {
  n <- tfl_arg_defs(fn = "normalize_ard")
  expect_identical(n$choices[[which(n$name == "keys")]], "ard")
  expect_identical(n$choices[[which(n$name == "hierarchy")]], "ard")
  expect_true(n$ordered[n$name == "hierarchy"])
  p <- tfl_arg_defs(fn = "plan_paginate_rows")
  expect_identical(p$choices[[which(p$name == "page_by")]], "columns")
  g <- tfl_arg_defs(fn = "plan_paginate_group")
  expect_identical(g$choices[[which(g$name == "col")]], "columns")
})

test_that("the paginate functions are the page step, other stages stay content", {
  for (f in c("plan_paginate_rows", "plan_paginate_cols", "plan_paginate_group")) {
    expect_true(all(tfl_arg_defs(fn = f)$step == "page"), info = f)
  }
  expect_true(all(tfl_arg_defs(fn = "normalize_ard")$step == "content"))
  expect_true(all(tfl_arg_defs(fn = "plan_blanks")$step == "content"))
})

test_that("an r argument is the r input; a condition is written, not by the name", {
  expect_identical(.arg_input_for_type("r"), "r")
  expect_identical(.arg_input_for_type("formula"), "r")
  expect_identical(.arg_input_for_type("string"), "text")
  # (no name decides it: a where without input: condition is r)
  expect_identical(tfl_arg_defs(fn = "data")$input[tfl_arg_defs(fn = "data")$name == "where"], "condition")
})

test_that("a ... row only for a function with dots:", {
  for (f in names(tfl_schema())) {
    e <- tfl_schema(f)
    if (!is.list(e) || is.null(e$args) && is.null(e$dots)) next
    d <- tfl_arg_defs(fn = f)
    if (is.list(e$dots)) {
      expect_identical(utils::tail(d$name, 1L), "...", info = f)
      expect_identical(d$type[d$name == "..."], as.character(e$dots$type %||% "string"), info = f)
    } else if (!"..." %in% names(e$args)) {
      expect_false("..." %in% d$name, info = f)
    }
  }
})

test_that("a template's params: are read the same way, tags kept", {
  dir <- tempfile("series")
  dir.create(file.path(dir, "test_v1", "templates"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  old <- options(tflspec.series_dir = dir)
  on.exit(options(old), add = TRUE)
  writeLines(c(
    "template: demog",
    "label: Demographics",
    "params:",
    "  by:",
    "    type: list<column>",
    "    input: pick",
    "    choices: vars(adsl_sub)",
    "    required: true",
    "    step: analysis",
    "    description: The groups across.",
    "  total:",
    "    type: logical",
    "    default: !series total",
    "    description: A Total column.",
    "  missing_row:",
    "    type: logical",
    "    default: true",
    "  conditions:",
    "    type: rows<map<string,string|r>>",
    "    input: table",
    "    columns: params",
    "    default: []",
    "data:",
    "  - id: adsl_sub",
    "    from: ADSL",
    "analysis:",
    "  - id: BIGN",
    "    method: categorical",
    "    data: adsl_sub",
    "    variables: [\"{by}\"]"),
    file.path(dir, "test_v1", "templates", "demog.yml"))
  d <- tfl_arg_defs(template = "test_v1/demog")
  expect_identical(names(d), cols)
  expect_identical(d$name, c("by", "total", "missing_row", "conditions"))
  expect_true(d$required[1L])
  expect_identical(d$choices[[1L]], "vars(adsl_sub)")
  expect_identical(d$input[d$name == "total"], "pick")
  expect_identical(d$default_kind, c(NA, "series", "value", "value"))
  expect_s3_class(d$default[[2L]], "tfl_tagged")
  expect_identical(d$default[[2L]]$value, "total")
  expect_identical(d$columns[[4L]], "params")
  expect_error(tfl_arg_defs(template = "test_v1/none"), "no file")
  expect_error(tfl_arg_defs(template = "demog"), "<series>/<template>")
  expect_error(tfl_arg_defs(template = "Test/demog"), "lower case")
})

test_that("a choice source outside the 15 words stops", {
  expect_error(.arg_check_choices("vars(adsl_sub), datasets", "x"), "unknown choice source")
  expect_silent(.arg_check_choices("from_args(continuous, categorical), report_data", "x"))
  expect_identical(.arg_choice_words("from_args(continuous, categorical), report_data"),
                   c("from_args(continuous, categorical)", "report_data"))
})

test_that("fixed_rows: the rows a template writes next to a table param's \"{name}\"", {
  body <- list(analysis = list(list(id = "HR", conditions = list(
    list(subgroup = "ALL", subgroup_lvl = "ALL", where = tfl_r("TRUE")), "{conditions}"))))
  fr <- tflspec:::.arg_fixed_rows(body, "conditions")
  expect_identical(fr$subgroup, "ALL")
  expect_identical(fr$where, "TRUE")
  expect_identical(attr(fr, "position"), "before")
  expect_null(tflspec:::.arg_fixed_rows(body, "by"))
  d <- tfl_arg_defs(template = "tflspec_v1/forest_hr")
  fx <- d$fixed_rows[[which(d$name == "conditions")]]
  expect_identical(fx$subgroup, "ALL")
  expect_null(d$fixed_rows[[which(d$name == "by")]])
  expect_null(tfl_arg_defs(fn = "analysis")$fixed_rows[[1L]])
})

test_that("every argument of the file's schema functions has a type (F1); a name the dots take, the dots' type", {
  # (the declarations as written: tfl_arg_defs() fills a missing type with
  # "string", so its table cannot show one missing)
  ents <- tflspec:::.schema_all(figure = FALSE)
  # (plan_append's data and content are step<data> / step<content>: none open)
  open <- character()
  miss <- unlist(lapply(names(ents), function(f) {
    e <- ents[[f]]
    args <- e$args %||% list()
    dots_type <- if (is.list(e$dots)) e$dots$type
    n <- names(args)[vapply(args, function(d) is.list(d) && is.null(d$type), NA)]
    # a name the dots take (and `...` itself) has the dots' type
    if (!is.null(dots_type)) n <- character()
    if (length(n)) paste0(f, ".", n) else character()
  }))
  expect_identical(setdiff(miss, open), character())
  d <- tfl_arg_defs(fn = "plan_filter")
  expect_identical(d$type[d$name == "where"], d$type[d$name == "..."])
  expect_identical(d$type[d$name == "where"], tfl_schema("plan_filter")$dots$type)
  d <- tfl_arg_defs(fn = "plan_cells")
  expect_identical(d$type[d$name == "default"], d$type[d$name == "..."])
})

# ---- the field contract, section 2 (2026-11-22): the first member that is
# not r decides the input; the other members' fixed values join its values;
# the exceptions say input: themselves

test_that("a union's input: the first member that is not r, the exceptions written (contract 2)", {
  row <- function(fn, arg) {
    d <- tfl_arg_defs(fn = fn)
    expect_true(arg %in% d$name, label = paste(fn, arg))
    d[d$name == arg, , drop = FALSE]
  }
  cases <- list(
    # fn, arg, input (by the rule unless the schema says it)
    c("ard_summary", "variables", "pick"),        # list<column>|column
    c("plan_columns", "row_title", "pick"),       # list<column>|list<integer>
    c("rtf_border", "top", "pick"),               # logical|enum[..]|call<..>
    c("plan_listing", "wrap", "pick"),            # logical|string
    c("tfl_image", "width_in", "text"),           # number|enum[fit]
    c("rtf_figures", "width_twips", "text"),      # integer|enum[fit]
    c("plan_sort", "keys", "text"),               # list<string>|logical (the dots' type)
    c("scale_x_continuous", "breaks", "text"),    # list<number>|call<..>|r
    c("ggplot", "data", "text"),                  # object|r (drawn by the type)
    c("plan_cell_style", "color", "text"),        # colour|r|map<..> (the colour field)
    c("plan_blanks", "where", "pick"),            # enum[..]|list<integer>|r
    c("plan_append", "data", "table"),            # step<data>
    c("plan_append", "content", "table"),         # step<content>
    # the exceptions (input: written)
    c("plan_digits", "...", "table"), c("plan_labels", "...", "table"),
    c("plan_cells", "...", "table"), c("plan_compare", "...", "table"),
    c("plan_style", "font", "table"), c("rtf_style", "font", "table"),
    c("rtf_titles", "font", "table"), c("rtf_footnotes", "font", "table"),
    c("rtf_header", "font", "table"), c("rtf_footer", "font", "table"),
    c("tfl_ard_number", "group", "table"),
    c("plan_row_group", "collapse", "pick"),
    c("plan_paginate_cols", "at", "pick"), c("plan_paginate_cols", "keep", "pick"),
    c("facet_wrap", "facets", "pick"), c("facet_grid", "rows", "pick"),
    c("facet_grid", "cols", "pick"), c("facet_nested", "rows", "pick"),
    c("facet_nested", "cols", "pick"),
    # where: written, no name decides it
    c("plan_cell_style", "where", "condition"), c("plan_filter", "where", "condition"),
    c("plan_filter", "...", "condition"))
  for (k in cases) {
    expect_identical(row(k[1L], k[2L])$input, k[3L], label = paste(k[1L], k[2L]))
  }
  # the other members' fixed values join the field's
  expect_identical(row("tfl_image", "width_in")$values[[1L]], "fit")
  expect_identical(row("rtf_border", "top")$values[[1L]],
                   c("TRUE", "FALSE", "single", "double", "thick", "dash", "dot", "none"))
  expect_identical(row("plan_cell_style", "bold")$values[[1L]], c("TRUE", "FALSE"))
  # a plain logical stays a switch: no values
  expect_identical(row("plan_filter", "any")$type, "logical")
  expect_null(row("plan_filter", "any")$values[[1L]])
  # a column member gets the columns; a list member is several (ordered FALSE)
  expect_identical(row("plan_columns", "row_title")$choices[[1L]], "columns")
  expect_false(row("plan_columns", "row_title")$ordered)
  expect_true(is.na(row("rtf_border", "top")$ordered))
  # a text member beside a pick: a value not in the list too
  expect_true(row("plan_listing", "wrap")$free)
  expect_true(row("plan_columns", "row_title")$free)
  # a logical beside other members: TRUE / FALSE (GT's review of #895)
  expect_identical(row("plan_listing", "wrap")$values[[1L]], c("TRUE", "FALSE"))
  expect_identical(row("plan_listing", "record")$values[[1L]], c("TRUE", "FALSE"))
  # a cards function's column: the columns of its data (by the package, not the name)
  expect_identical(row("ard_summary", "variables")$choices[[1L]], "vars")
  expect_identical(row("ard_tabulate", "by")$choices[[1L]], "vars")
  # the 20 exceptions are still needed: the rule alone gives another input
  ex <- cases[14:33]
  ents <- tflspec:::.schema_all(figure = TRUE)
  for (k in ex) {
    e <- ents[[k[1L]]]
    ty <- if (identical(k[2L], "...")) e$dots$type else e$args[[k[2L]]]$type %||% e$dots$type
    expect_false(identical(tflspec:::.arg_input_for_type(ty), k[3L]), label = paste(k[1L], k[2L], "needs its input:"))
  }
})

# F12 (a), 2026-11-22 (the coordinator, after #895 turned about 74 fields into
# R fields and no test saw it): a field whose first member that is not `r` is
# object, call or call<fn> is drawn by its type (input text), never as R; a
# list of calls (list<call<..>>, list<call>) is R.  The expected input of each
# family is written here, the family read by this test's own split.
test_that("object, call and call<fn> fields are drawn by their type, not as R", {
  fixtures <- list(
    c("ggplot", "data", "text"),           # object|r
    c("ggsurvfit", "x", "text"),           # object
    c("geom_line", "position", "text"),    # call|string|r
    c("plan_style", "border_header", "text"), # call<rtf_border>
    c("plan_listing", "...", "r"),         # list<call<listing_col>>
    c("ard_stack", "...", "r"))            # list<call>
  for (k in fixtures) {
    d <- tfl_arg_defs(fn = k[1L])
    expect_identical(d$input[d$name == k[2L]], k[3L], label = paste(k[1L], k[2L]))
  }
  # every such row of the schema (the definition writes no input:)
  first_member <- function(type) {
    ch <- strsplit(type, "", fixed = TRUE)[[1L]]
    depth <- cumsum(ch %in% c("<", "[")) - cumsum(ch %in% c(">", "]"))
    cut <- c(0L, which(ch == "|" & depth == 0L), length(ch) + 1L)
    m <- trimws(vapply(seq_len(length(cut) - 1L), function(i)
      paste(ch[seq_len(cut[i + 1L] - cut[i] - 1L) + cut[i]], collapse = ""), ""))
    m <- m[m != "r"]
    if (length(m)) m[1L] else "r"
  }
  family <- function(m) {
    if (grepl("^list<call", m)) return("list<call>")
    if (m %in% c("object", "call") || startsWith(m, "call<")) return("drawn")
    NA_character_
  }
  want <- c(drawn = "text", `list<call>` = "r")
  ents <- tflspec:::.schema_all(figure = TRUE)
  seen <- character()
  for (fn in names(ents)) {
    e <- ents[[fn]]
    rows <- e$args
    if (!is.null(e$dots)) rows[["..."]] <- e$dots
    todo <- Filter(function(a) is.list(a) && is.character(a$type) && is.null(a$input) &&
                     !is.na(family(first_member(a$type))), rows)
    if (!length(todo)) next
    d <- tfl_arg_defs(fn = fn)
    for (a in names(todo)) {
      f <- family(first_member(todo[[a]]$type))
      seen <- union(seen, f)
      expect_identical(d$input[d$name == a], want[[f]], label = paste(fn, a, todo[[a]]$type))
    }
  }
  # (both families are in the schema: the check above is not empty)
  expect_setequal(seen, names(want))
})

test_that("the members are split at the top level only", {
  m <- tflspec:::.type_members
  expect_identical(m("enum[a|b]"), "enum[a|b]")
  expect_identical(m("map<string, column|r>|r"), c("map<string, column|r>", "r"))
  expect_identical(m("logical|enum[x|y]|call<f>"), c("logical", "enum[x|y]", "call<f>"))
  expect_identical(tflspec:::.type_deciding("r|call<vars>|list<column>|column"), "call<vars>")
  expect_identical(tflspec:::.type_deciding("r"), "r")
})

test_that("an argument with no written default takes a logical formal default when its type is logical (contract 14i)", {
  fd <- list(a = TRUE, b = 2, c = "x", d = NULL, f = FALSE, g = TRUE)
  out <- .arg_formal_defaults(list(a = list(type = "logical"), b = list(type = "number"),
                                   c = list(type = "string"), d = list(type = "string"),
                                   e = list(type = "logical"), f = list(type = "logical", default = TRUE),
                                   g = list(type = "string"),
                                   `...` = list(type = "logical")), fd)
  expect_identical(out$a$default, TRUE)
  expect_identical(out$f$default, TRUE)
  expect_false("default" %in% names(out$b))
  expect_false("default" %in% names(out$c))
  expect_false("default" %in% names(out$d))
  expect_false("default" %in% names(out$e))
  expect_false("default" %in% names(out$g))
  expect_false("default" %in% names(out$`...`))
  # the deciding member of a union decides: logical|string is a logical kind
  u <- .arg_formal_defaults(list(a = list(type = "logical|string")), list(a = FALSE))
  expect_identical(u$a$default, FALSE)
  # a required argument keeps its tier: no default added
  req <- .arg_formal_defaults(list(a = list(type = "logical", tier = "required")), fd)
  expect_false("default" %in% names(req$a))
})

test_that("the formals' defaults recorded are literal constants only", {
  skip_if_not_installed("rtfreporter")
  ent <- tfl_schema("plan_cells")
  expect_true(all(vapply(ent$formal_defaults, function(v) {
    is.null(v) || (length(v) == 1L && (is.logical(v) || is.numeric(v) || is.character(v)))
  }, NA)))
  expect_false(any(c(".plan", "...") %in% names(ent$formal_defaults)))
  expect_identical(ent$data_args, ".plan")
  expect_false(".plan" %in% ent$formals)
  expect_true("ard" %in% .schema_data_args)
})

test_that("plan_cell_style and plan_filter take their logical formal defaults, a call or string gives none", {
  skip_if_not_installed("rtfreporter")
  d <- tfl_arg_defs(fn = "plan_cell_style")
  expect_identical(d$default[[which(d$name == "header")]], FALSE)
  expect_identical(d$default_kind[d$name == "header"], "value")
  f <- tfl_arg_defs(fn = "plan_filter")
  expect_identical(f$default[[which(f$name == "any")]], TRUE)
  # parents = c("keep", "drop") is a call: not taken as the default
  expect_false(identical(f$default[[which(f$name == "parents")]], c("keep", "drop")))
  # a string formal default is not taken
  e <- tfl_arg_defs(fn = "plan_empty")
  expect_true(is.na(e$default_kind[e$name == "text"]))
  expect_false(f$required[f$name == "any"])
  expect_identical(tfl_schema("plan_filter")$data_args, ".plan")
})

test_that("a column field's choices are written in its definition", {
  y <- yaml::read_yaml(system.file("spec", "schema.yml", package = "tflspec"))
  # (a column member of the type, a union's too: `list<column>|column`)
  col <- function(d) {
    is.list(d) && any(grepl("^(list<)?column>?$", tflspec:::.type_members(as.character(d$type %||% "string"))))
  }
  n <- 0L
  for (fn in names(y)) {
    word <- if (identical(fn, "normalize_ard")) "ard" else if (startsWith(fn, "plan_")) "columns" else
      if (identical(y[[fn]]$package, "cards")) "vars"
    if (is.null(word) || !is.list(y[[fn]]$args)) next
    for (a in names(y[[fn]]$args)) {
      d <- y[[fn]]$args[[a]]
      if (col(d)) {
        n <- n + 1L
        expect_identical(d$choices, word, info = paste(fn, a))
      }
    }
  }
  expect_gt(n, 30L)
  expect_identical(y$plan_hide$dots$choices, "columns")
  expect_true(isTRUE(y$normalize_ard$args$hierarchy$ordered))
})

test_that("the titles, footnotes and row arguments carry a label and a help (tflplanner#783)", {
  want <- list(
    plan_titles = c("...", "pages", "border"),
    plan_footnotes = c("...", "pages", "border"),
    plan_cell_style = "row")
  for (fn in names(want)) {
    d <- tfl_arg_defs(fn = fn)
    for (a in want[[fn]]) {
      r <- d[d$name == a, ]
      info <- paste(fn, a)
      expect_identical(nrow(r), 1L, info = info)
      expect_true(nzchar(r$label_en) && nzchar(r$label_ja), info = info)
      expect_true(!is.na(r$help_en) && nzchar(r$help_en), info = info)
    }
  }
  # (the dots key has no args row: tflplanner draws its field from the
  # dots, whose label and description are the field's)
  for (fn in c("plan_titles", "plan_footnotes")) {
    expect_false(tfl_schema(fn)$dots$key %in% tfl_arg_defs(fn = fn)$name, info = fn)
  }
  d <- tfl_arg_defs(fn = "plan_style")
  cols <- d$columns[[which(d$name == "font")]]
  expect_identical(cols$key$label_en, "Part")
  expect_identical(cols$key$label_ja, "\u90e8\u5206")
  expect_identical(cols$value$label_en, "Font")
  expect_identical(cols$value$label_ja, "\u30d5\u30a9\u30f3\u30c8")
})

test_that("the editor words are used, each with an input, and sit where tflplanner's editors are", {
  all <- do.call(rbind, lapply(names(tfl_schema()), function(fn) {
    d <- tfl_arg_defs(fn = fn)
    if (nrow(d)) cbind(fn = fn, d[, c("name", "input", "editor")])
  }))
  ed <- all[!is.na(all$editor), ]
  # F12: every word of the closed list is used, and only those words
  expect_true(all(tflspec:::.arg_editors %in% ed$editor))
  expect_true(all(ed$editor %in% tflspec:::.arg_editors))
  # a row with an editor still has its value form
  expect_true(all(!is.na(ed$input) & ed$input %in% tflspec:::.arg_input_forms))
  got <- stats::setNames(ed$editor, paste(ed$fn, ed$name, sep = "."))
  want <- c(table_plan.cols = "roles", table_plan.rows = "roles", table_plan.label = "roles",
            plan_cells.value = "cells", plan_cells.categorical = "cells",
            plan_cells.continuous = "cells", plan_cells.default = "cells",
            plan_col_header.header = "header_lines", plan_col_header.values = "header_lines",
            plan_sort.keys = "sort_keys", plan_sort.stat = "sort_keys",
            plan_digits.... = "digits", plan_levels.... = "levels")
  expect_identical(got[order(names(got))], want[order(names(want))])
  # an argument without one: NA
  expect_true(is.na(tfl_arg_defs(fn = "plan_cells")$editor[tfl_arg_defs(fn = "plan_cells")$name == "na"]))
  expect_true(all(is.na(tfl_arg_defs(fn = "data")$editor)))
})

test_that("an unknown editor word is an error naming the allowed words", {
  expect_error(tflspec:::.arg_row("x", list(type = "string", editor = "wizard"), "content", "fn"),
               "editor \"wizard\" is not one of roles, cells, header_lines, digits, levels, sort_keys")
  expect_error(tflspec:::.arg_row("x", list(type = "string", editor = c("roles", "cells")),
                                  "content", "fn"), "is not one of roles")
  r <- tflspec:::.arg_row("x", list(type = "string", editor = "cells"), "content", "fn")
  expect_identical(r$editor, "cells")
})

test_that("the ... row carries how the dots are written; other rows have NA", {
  dots <- function(fn) {
    d <- tfl_arg_defs(fn = fn)
    d[d$name == "...", c("dots_style", "dots_key", "dots_map")]
  }
  l <- dots("plan_labels")
  expect_identical(l$dots_style, "named")
  expect_true(is.na(l$dots_key))
  expect_true(l$dots_map)
  s <- dots("plan_sort")
  expect_identical(s$dots_style, "positional")
  expect_identical(s$dots_key, "keys")
  expect_false(s$dots_map)
  # not a ... row: NA (plan_sort's `keys` row, a function without dots)
  d <- tfl_arg_defs(fn = "plan_sort")
  expect_true(all(is.na(d$dots_style[d$name != "..."])))
  expect_true(all(is.na(d$dots_map[d$name != "..."])))
  n <- tfl_arg_defs(fn = "plan_cells")
  expect_identical(n$dots_style[n$name == "..."], "named")
  e <- tfl_arg_defs(fn = "table_plan")
  expect_false("..." %in% e$name)
  expect_true(all(is.na(e$dots_style) & is.na(e$dots_key) & is.na(e$dots_map)))
})

test_that("a bare call lists the names a person picks, and a border call is not R (field type rules G1, G10)", {
  skip_on_cran()
  # (G1) a type whose deciding member is a bare `call`: the function's short
  # names are the definition's `values:`, the field a pick of them
  # (theme, strip, first, text_x ...: one call of several functions with no
  # list of names yet; they stay R)
  no_names <- c("theme", "strip", "first", "text_x", "text_y", "background_x", "background_y")
  bare <- character()
  for (fn in names(tfl_schema())) {
    d <- tfl_arg_defs(fn = fn)
    for (i in seq_len(nrow(d))) {
      if (identical(.type_deciding(d$type[[i]]), "call") && !d$name[[i]] %in% no_names) {
        bare <- c(bare, paste0(fn, ".", d$name[[i]]))
        expect_gt(length(d$values[[i]]), 0L, label = paste(fn, d$name[[i]], "values"))
      }
    }
  }
  expect_gte(length(bare), 21L)
  # (a layer from the shared catalog, geom_bar, has the names too)
  expect_true(all(c("dodge", "stack") %in% tfl_arg_defs(fn = "geom_bar")$values[[which(tfl_arg_defs(fn = "geom_bar")$name == "position")]]))
  expect_true(all(c("geom_line.position", "guides.colour", "labeller..rows", "ard_strata..f") %in% bare))
  p <- tfl_arg_defs(fn = "geom_line")
  expect_true(all(c("dodge", "stack", "jitter") %in% p$values[[which(p$name == "position")]]))
  g <- tfl_arg_defs(fn = "guides")
  expect_true(all(c("legend", "none") %in% g$values[[which(g$name == "...")]]))
  # (G10) a border is the rtf_border() group, as the other nine are: its type
  # decides the input, none is written
  for (k in list(c("plan_cell_style", "border"), c("plan_titles", "border"),
                 c("plan_footnotes", "border"))) {
    d <- tfl_arg_defs(fn = k[[1L]])
    expect_identical(d$type[d$name == k[[2L]]], "call<rtf_border>")
    expect_identical(d$input[d$name == k[[2L]]], "text", label = paste(k, collapse = "."))
  }
})
