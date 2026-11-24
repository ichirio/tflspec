# The columns every card's definition carries (P0 of the card generator,
# canon 411bf4d): group and the groups attribute, label and help in en / ja,
# tier key, unset, passes_to, member_of, value labels, the ... row and the
# figure_style choice word.

# the columns' values, checked for one table
check_defs <- function(d, info) {
  expect_true(is.data.frame(d), info = info)
  expect_false("label_key" %in% names(d), info = info)
  expect_true(all(d$tier %in% c("required", "key", "usual", "other")), info = info)
  expect_true(all(d$unset %in% c("omit", "step_off", "stop")), info = info)
  expect_true(all(d$unset[d$required] == "stop"), info = info)
  expect_true(all(d$input %in% c("pick", "text", "condition", "table", "r")), info = info)
  g <- attr(d, "groups")
  expect_true(is.data.frame(g), info = info)
  expect_identical(names(g), c("id", "label_en", "label_ja"), info = info)
  expect_true(all(is.na(d$group) | d$group %in% g$id), info = info)
  expect_true(all(is.na(d$member_of) | d$member_of %in% d$name), info = info)
  for (k in c("label_en", "label_ja", "help_en", "help_ja", "member_of", "group")) {
    expect_type(d[[k]], "character")
  }
  for (p in d$passes_to) {
    expect_type(p, "character")
    expect_true(all(grepl("^[A-Za-z][A-Za-z0-9_]*[.]", p)), info = info)
  }
  for (v in d$value_labels) {
    if (!is.null(v)) expect_identical(names(v), c("value", "label_en", "label_ja"), info = info)
  }
}

test_that("every schema function's definition reads, with valid new columns", {
  fns <- names(tfl_schema())
  fns <- fns[!startsWith(fns, ".")]
  expect_gt(length(fns), 100L)
  for (f in fns) {
    d <- tryCatch(tfl_arg_defs(fn = f), error = function(e) conditionMessage(e))
    expect_true(is.data.frame(d), info = paste(f, if (is.character(d)) d))
    if (is.data.frame(d)) check_defs(d, f)
  }
  check_defs(tfl_arg_defs(fn = "data"), "data")
  check_defs(tfl_arg_defs(fn = "analysis"), "analysis")
})

test_that("every template of the series reads, with valid new columns", {
  dir <- system.file("series", package = "tflspec")
  n <- 0L
  for (s in list.dirs(dir, recursive = FALSE, full.names = FALSE)) {
    for (f in list.files(file.path(dir, s, "templates"), pattern = "[.]yml$")) {
      tpl <- paste0(s, "/", sub("[.]yml$", "", f))
      d <- tfl_arg_defs(template = tpl)
      check_defs(d, tpl)
      # the templates keep usual (folded) as written
      expect_false(any(d$tier == "key"), info = tpl)
      n <- n + 1L
    }
  }
  expect_gte(n, 11L)
})

test_that("tier key comes from the function's visible: list and main: true", {
  d <- tfl_arg_defs(fn = "ggsurvfit")
  vis <- intersect(unlist(tfl_schema("ggsurvfit")$visible), d$name)
  expect_true(length(vis) > 0L)
  expect_true(all(d$tier[d$name %in% vis] == "key"))
  expect_false(any(d$tier[!d$name %in% vis] == "key"))
  m <- tfl_arg_defs(fn = "plan_columns")
  expect_identical(m$tier[m$name == "row_title"], "key")
  # straight from definitions: a written tier and a required one keep theirs
  a <- .arg_schema_key(list(x = list(type = "string"), y = list(type = "string", tier = "other"),
                            z = list(type = "string", required = TRUE), w = list(main = TRUE)),
                       list("x", "y", "z"))
  expect_identical(a$x$tier, "key")
  expect_identical(a$y$tier, "other")
  expect_null(a$z$tier)
  expect_identical(a$w$tier, "key")
})

test_that("groups: order and headings come back as the attribute", {
  defs <- list(a = list(type = "string", group = "main"),
               b = list(type = "string", group = "look"),
               c = list(type = "string"))
  groups <- list(list(id = "look", label = list(en = "Look", ja = "Mitame")),
                 list(id = "main", label = "Main"))
  d <- .arg_table(defs, "content", "f", groups)
  g <- attr(d, "groups")
  expect_identical(g$id, c("look", "main"))
  expect_identical(g$label_en, c("Look", "Main"))
  expect_identical(g$label_ja, c("Mitame", NA))
  expect_identical(d$group, c("main", "look", NA))
  # none written: one unnamed group
  d0 <- tfl_arg_defs(fn = "plan_cells")
  expect_identical(nrow(attr(d0, "groups")), 0L)
  expect_true(all(is.na(d0$group)))
  expect_error(.arg_table(list(a = list(group = "nope")), "content", "f", groups),
               "group \"nope\" is not in groups: \\(look, main\\)")
  expect_error(.arg_table(list(a = list(group = "x")), "content", "f"), "none written")
  expect_error(.arg_table(defs, "content", "f", list(list(id = "a"), list(id = "a"))), "twice")
  expect_error(.arg_table(defs, "content", "f", list(list(label = "x"))), "each entry")
})

test_that("a template's groups:, labels, help, unset, passes_to and member_of", {
  dir <- tempfile("series")
  dir.create(file.path(dir, "test_v1", "templates"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  old <- options(tflspec.series_dir = dir)
  on.exit(options(old), add = TRUE)
  writeLines(c(
    "template: demog",
    "label: Demographics",
    "groups:",
    "  - {id: vars, label: {en: Variables, ja: Hensu}}",
    "  - {id: look, label: Look}",
    "params:",
    "  by:",
    "    type: list<column>",
    "    choices: vars(adsl_sub)",
    "    required: true",
    "    group: vars",
    "    label: {en: Groups, ja: Gun}",
    "    help: {en: The groups across., ja: Yoko no gun.}",
    "  blanks:",
    "    type: logical",
    "    default: true",
    "    tier: key",
    "    group: look",
    "    maps_to: plan_blanks.where",
    "  heading:",
    "    type: string",
    "    unset: stop",
    "    label: Heading",
    "    passes_to: [plan_stub.heading, plan_col_header.header.stub]",
    "  risk:",
    "    type: map<string,number>",
    "    description: The risk table's options.",
    "  times:",
    "    type: list<number>",
    "    member_of: risk",
    "  position:",
    "    type: string",
    "    values:",
    "      - {value: top, label: {en: Top, ja: Ue}}",
    "      - bottom",
    "    labels: {bottom: Bottom}",
    "  cols:",
    "    type: rows<vars,label>",
    "    input: table",
    "    columns:",
    "      vars: {type: list<column>, label: {en: Variables, ja: Hensu}}",
    "      label: {type: string, description: The heading.}",
    "    labels: {label: Heading}",
    "  formats:",
    "    type: map<string,number>",
    "    input: table",
    "    columns:",
    "      key: {label: {en: Statistic, ja: Tokeiryo}}",
    "      value: {label: {en: Decimals, ja: Keta}}",
    "data:",
    "  - id: adsl_sub",
    "    from: ADSL"),
    file.path(dir, "test_v1", "templates", "demog.yml"))
  d <- tfl_arg_defs(template = "test_v1/demog")
  r <- function(n) which(d$name == n)
  expect_identical(attr(d, "groups")$id, c("vars", "look"))
  expect_identical(attr(d, "groups")$label_ja, c("Hensu", NA))
  expect_identical(d$group, c("vars", "look", NA, NA, NA, NA, NA, NA))
  expect_identical(d$label_en[r("by")], "Groups")
  expect_identical(d$label_ja[r("by")], "Gun")
  expect_identical(d$help_en[r("by")], "The groups across.")
  expect_identical(d$help_ja[r("by")], "Yoko no gun.")
  expect_identical(d$description[r("by")], "The groups across.")
  expect_identical(d$label_en[r("heading")], "Heading")
  expect_identical(d$label_ja[r("heading")], NA_character_)
  expect_identical(d$help_en[r("risk")], "The risk table's options.")
  expect_identical(d$help_en[r("heading")], NA_character_)
  expect_identical(d$tier[r("blanks")], "key")
  expect_identical(d$unset[r("by")], "stop")
  expect_identical(d$unset[r("blanks")], "omit")
  expect_identical(d$unset[r("heading")], "stop")
  # maps_to is read as passes_to
  expect_identical(d$passes_to[[r("blanks")]], "plan_blanks.where")
  expect_identical(d$passes_to[[r("heading")]], c("plan_stub.heading", "plan_col_header.header.stub"))
  expect_identical(d$passes_to[[r("by")]], character())
  expect_identical(d$member_of[r("times")], "risk")
  expect_identical(d$member_of[r("risk")], NA_character_)
  expect_identical(d$values[[r("position")]], c("top", "bottom"))
  vl <- d$value_labels[[r("position")]]
  expect_identical(vl$label_en, c("Top", "Bottom"))
  expect_identical(vl$label_ja, c("Ue", NA))
  expect_identical(d$choices[[r("position")]], "fixed")
  cc <- d$columns[[r("cols")]]
  expect_identical(cc$vars$label_ja, "Hensu")
  expect_identical(cc$label$label_en, "Heading")
  expect_identical(cc$label$help_en, "The heading.")
  expect_identical(cc$label$type, "string")
  # a map's two columns, key and value
  fc <- d$columns[[r("formats")]]
  expect_identical(fc$key$label_en, "Statistic")
  expect_identical(fc$value$label_ja, "Keta")
})

test_that("a template's groups come from its series.yml, its own groups: replaces them", {
  dir <- tempfile("series")
  dir.create(file.path(dir, "test_v1", "templates"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  old <- options(tflspec.series_dir = dir)
  on.exit(options(old), add = TRUE)
  writeLines(c("name: test_v1", "values: {}", "groups:",
               "  - {id: data, label: {en: Data, ja: Deta}}",
               "  - {id: vars, label: {en: Variables, ja: Hensu}}"),
             file.path(dir, "test_v1", "series.yml"))
  writeLines(c("template: a", "params:",
               "  by: {type: list<column>, group: vars}",
               "  population: {type: string, group: data}"),
             file.path(dir, "test_v1", "templates", "a.yml"))
  d <- tfl_arg_defs(template = "test_v1/a")
  expect_identical(attr(d, "groups")$id, c("data", "vars"))
  expect_identical(d$group, c("vars", "data"))
  writeLines(c("template: b", "groups: [{id: own, label: Own}]", "params:",
               "  by: {type: list<column>, group: vars}"),
             file.path(dir, "test_v1", "templates", "b.yml"))
  expect_error(tfl_arg_defs(template = "test_v1/b"), "group \"vars\" is not in groups: \\(own\\)")
})

test_that("enum values keep their simple form; a labels: map labels them", {
  d <- .arg_table(list(a = list(type = "enum[x|y]"),
                       b = list(type = "enum[x|y]", labels = list(x = list(en = "Ex", ja = "E")))),
                  "content", "f")
  expect_identical(d$values[[1L]], c("x", "y"))
  expect_null(d$value_labels[[1L]])
  expect_identical(d$value_labels[[2L]]$label_en, c("Ex", NA))
  expect_identical(d$value_labels[[2L]]$label_ja, c("E", NA))
})

test_that("the schema's plain and en / ja labels both read", {
  d <- tfl_arg_defs(fn = "plan_total")
  expect_identical(d$label_en[d$name == "label"], "Heading")
  expect_false(is.na(d$label_ja[d$name == "label"]))
  expect_match(d$help_en[d$name == "label"], "Total column")
  p <- .arg_table(list(a = list(label = "Plain")), "content", "f")
  expect_identical(p$label_en, "Plain")
  expect_true(is.na(p$label_ja))
  k <- tfl_arg_defs(fn = "ggsurvfit")
  expect_identical(k$label_en[k$name == "x"], "Fit")
  expect_false(is.na(k$label_ja[k$name == "x"]))
})

test_that("the ... row has the dots' type, and a label the args give it", {
  d <- tfl_arg_defs(fn = "plan_nest")
  expect_identical(utils::tail(d$name, 1L), "...")
  expect_identical(sum(d$name == "..."), 1L)
  expect_identical(d$type[d$name == "..."], tfl_schema("plan_nest")$dots$type)
  expect_identical(d$label_en[d$name == "..."], "Rows under a level")
  s <- tfl_arg_defs(fn = "plan_sort")
  expect_identical(s$type[s$name == "..."], "list<string>|logical")
})

test_that("a bad tier, unset, group, member_of, passes_to or label stops", {
  t1 <- function(d) .arg_table(list(a = d), "content", "f")
  expect_error(t1(list(tier = "often")), "tier \"often\" is not one of required, key, usual, other")
  expect_error(t1(list(unset = "maybe")), "unset \"maybe\" is not one of omit, step_off, stop")
  expect_identical(t1(list(unset = "step_off"))$unset, "step_off")
  expect_error(t1(list(passes_to = "plan_blanks")), "passes_to: is")
  expect_error(t1(list(passes_to = "plan blanks.where")), "passes_to: is")
  expect_error(t1(list(passes_to = "a.b", maps_to = "a.b")), "write passes_to: only")
  expect_error(t1(list(member_of = "nope")), "member_of \"nope\" is not another argument")
  expect_error(t1(list(member_of = "a")), "not another argument")
  expect_error(t1(list(label = list(en = "A", de = "B"))), "label is a text or")
  expect_error(t1(list(help = 3)), "help is a text or")
  expect_error(t1(list(values = list(list(label = "x")))), "values: an entry")
  expect_silent(t1(list(passes_to = "plan_digits..rows")))
  expect_silent(t1(list(passes_to = list("plan_cells.categorical", "plan_cells.default"))))
})

test_that("input r: written in a definition it comes through; r types take it", {
  # (the field contract, 2026-11-22: no name decides the input -- a where
  # that is a condition says input: condition)
  d <- .arg_table(list(a = list(type = "string", input = "r"), b = list(type = "r"),
                       where = list(type = "r"), w2 = list(type = "r", input = "condition"),
                       c = list(type = "string")), "content", "f")
  expect_identical(d$input, c("r", "r", "r", "condition", "text"))
  expect_error(.arg_table(list(a = list(input = "code")), "content", "f"),
               "input \"code\" is not one of pick, text, condition, table, r")
  dr <- tfl_arg_defs(fn = "data")
  expect_identical(dr$input[dr$name %in% c("then", "code")], c("r", "r"))
  expect_identical(dr$input[dr$name == "where"], "condition")
})

test_that("the plan verbs carry labels in en and ja, their fields a form and a default", {
  verbs <- c("normalize_ard", "table_plan", "plan_cells", "plan_digits", "plan_levels", "plan_labels",
             "plan_nest", "plan_total", "plan_compare", "plan_stat_groups", "plan_sort", "plan_filter",
             "plan_hide", "plan_col_header", "plan_stub", "plan_blanks", "plan_paginate_rows",
             "plan_paginate_group", "plan_paginate_cols", "plan_row_group", "plan_columns", "plan_style",
             "plan_cell_style", "plan_append", "plan_empty", "plan_titles", "plan_footnotes",
             "plan_after")
  for (v in verbs) {
    l <- tfl_schema(v)$label
    expect_true(is.list(l) && is.character(l$en) && is.character(l$ja), info = v)
    d <- tfl_arg_defs(fn = v)
    own <- d[d$name != "...", , drop = FALSE]
    expect_false(anyNA(own$label_en), info = v)
    expect_false(anyNA(own$label_ja), info = v)
  }
  dv <- function(fn, a) {
    d <- tfl_arg_defs(fn = fn)
    d$default[[which(d$name == a)]]
  }
  expect_false(dv("plan_stub", "before"))
  expect_true(dv("plan_paginate_group", "keep"))
  expect_true(dv("plan_sort", "keep"))
  expect_identical(dv("plan_empty", "when"), "auto")
  expect_identical(dv("plan_empty", "position"), "last")
  inp <- function(fn, a) {
    d <- tfl_arg_defs(fn = fn)
    d$input[d$name == a]
  }
  expect_identical(inp("normalize_ard", "drop_contexts"), "text")
  expect_identical(inp("normalize_ard", "stack"), "table")
  expect_identical(inp("normalize_ard", "combine"), "table")
  expect_identical(inp("plan_cell_style", "border"), "text")
  expect_identical(inp("plan_columns", "column_widths_twips"), "text")
  expect_identical(inp("plan_titles", "pages"), "text")
  expect_identical(inp("plan_footnotes", "pages"), "text")
  expect_identical(inp("plan_filter", "by"), "pick")
  expect_identical(inp("plan_row_group", "group_col"), "pick")
  w <- tfl_arg_defs(fn = "plan_columns")
  expect_identical(w$input[w$name == "widths"], "table")
  wc <- w$columns[[which(w$name == "widths")]]
  expect_identical(names(wc), c("column", "width"))
  expect_identical(wc$column$input, "pick")
  expect_identical(wc$width$type, "number")
  # a key from main: true
  expect_identical(tfl_arg_defs(fn = "plan_stub")$tier[tfl_arg_defs(fn = "plan_stub")$name == "before"], "key")
})

test_that("figure_style(<palette|marker>) is the 15th choice word", {
  expect_identical(length(.arg_choice_sources), 15L)
  expect_silent(.arg_check_choices("figure_style(palette)", "x"))
  expect_silent(.arg_check_choices("figure_style(marker), fixed", "x"))
  d <- .arg_table(list(col = list(type = "string", input = "pick", choices = "figure_style(palette)")),
                  "content", "f")
  expect_identical(d$choices[[1L]], "figure_style(palette)")
  expect_error(.arg_check_choices("figure_style(fonts)", "x"), "figure_style\\(<kind>\\)")
  expect_error(.arg_check_choices("figure_style", "x"), "figure_style\\(<kind>\\)")
  expect_error(.arg_check_choices("figure_styles(palette)", "x"), "unknown choice source")
})
