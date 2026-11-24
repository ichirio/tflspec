fn_cols <- c("fn", "step", "label_en", "label_ja", "join", "first_arg_name", "returns")

test_that("tfl_fn_defs() has one row for every schema function but rtf_*", {
  d <- tfl_fn_defs()
  expect_identical(names(d), fn_cols)
  fns <- names(yaml::read_yaml(system.file("spec", "schema.yml", package = "tflspec")))
  fns <- fns[!startsWith(fns, "rtf_") & !startsWith(fns, ".")]
  expect_true(length(fns) > 300L)
  expect_identical(setdiff(fns, d$fn), character())
  expect_identical(anyDuplicated(d$fn), 0L)
  expect_false(any(startsWith(d$fn, "rtf_")))
  expect_true(all(d$step %in% c("data", "analysis", "content")))
  expect_true(all(d$join %in% c("|>", "+")))
  expect_true(all(d$returns %in% c("data", "ard", "model", "value", "object", "plot", "plan",
                                   "content", "doc", "unknown")))
  # NA exactly for the + joins
  expect_identical(is.na(d$first_arg_name), d$join == "+")
  expect_false(is.unsorted(match(d$step, c("data", "analysis", "content"))))
})

test_that("a + layer has no first argument and gives a plot", {
  d <- tfl_fn_defs()
  for (f in c("geom_line", "geom_point", "scale_x_continuous", "labs", "theme_tfl",
              "add_risktable")) {
    r <- d[d$fn == f, ]
    expect_identical(r$join, "+", info = f)
    expect_identical(r$first_arg_name, NA_character_, info = f)
    expect_identical(r$returns, "plot", info = f)
    expect_identical(r$step, "content", info = f)
  }
  expect_identical(d$label_en[d$fn == "geom_line"], "Lines")
})

test_that("a table's functions pipe the plan; a plot's head starts its object", {
  d <- tfl_fn_defs()
  pc <- d[d$fn == "plan_cells", ]
  expect_identical(pc$join, "|>")
  expect_identical(pc$returns, "plan")
  expect_identical(pc$step, "content")
  expect_false(is.na(pc$first_arg_name))
  tp <- d[d$fn == "table_plan", ]
  expect_identical(tp$join, "|>")
  expect_identical(tp$returns, "plan")
  expect_false(is.na(tp$first_arg_name))
  expect_identical(tp$label_en, "Table structure")
  expect_true(startsWith(tp$label_ja, "Table "))
  gs <- d[d$fn == "ggsurvfit", ]
  expect_identical(gs$join, "|>")
  expect_identical(gs$first_arg_name, "x")
  expect_identical(gs$returns, "plot")
  gp <- d[d$fn == "ggplot", ]
  expect_identical(gp$first_arg_name, "data")
  expect_identical(d$returns[d$fn == "tfl_risk_panel"], "plot")
  expect_identical(d$first_arg_name[d$fn == "tfl_risk_panel"], "fit")
  # the arrangement (B-11): wrap_plots's own keys in the schema
  wp <- d[d$fn == "wrap_plots", ]
  expect_identical(wp$join, "|>")
  expect_identical(wp$first_arg_name, "plots")
  expect_identical(wp$returns, "plot")
})

test_that("the data row and the analysis methods have their rows", {
  d <- tfl_fn_defs()
  dr <- d[d$fn == "data_row", ]
  expect_identical(nrow(dr), 1L)
  expect_identical(dr$step, "data")
  expect_identical(dr$join, "|>")
  expect_identical(dr$first_arg_name, "from")
  expect_identical(dr$returns, "data")
  expect_identical(d$fn[d$step == "data"], "data_row")
  a <- tfl_fn_defs("analysis")
  m <- tfl_ard_methods()
  calls <- sub("^.*::", "", m$call[grepl("::", m$call, fixed = TRUE)])
  expect_identical(setdiff(calls, a$fn), character())
  am <- a[a$fn %in% calls, ]
  expect_true(all(am$join == "|>"))
  expect_true(all(am$first_arg_name == "data"))
  expect_true(all(am$returns == "ard"))
  expect_false(any(c("(code)", "(subjects)") %in% a$fn))
  s <- a[a$fn == "ard_summary", ]
  expect_identical(s$label_en, "Summary statistics")
  expect_identical(a$label_en[a$fn == "ard_survival_survfit"], "Kaplan-Meier estimates")
})

test_that("the step filter keeps one step; another step stops", {
  for (st in c("data", "analysis", "content")) {
    d <- tfl_fn_defs(st)
    expect_true(nrow(d) > 0L, info = st)
    expect_true(all(d$step == st), info = st)
  }
  expect_identical(nrow(tfl_fn_defs("data")) + nrow(tfl_fn_defs("analysis")) +
                     nrow(tfl_fn_defs("content")), nrow(tfl_fn_defs()))
  expect_error(tfl_fn_defs("page"), "data, analysis, content")
  expect_error(tfl_fn_defs(c("data", "content")), "is not one of")
})

test_that("a schema entry's join, first_arg_name and returns win over the rules", {
  methods <- data.frame(method = "zz_count", label = "Zz counts", call = "zzpkg::ard_zz_count",
                        stringsAsFactors = FALSE)
  schema <- list(
    # (an rtfreporter function that is not a figure's builds the plan;
    # zz_tail, of another package, says nothing: unknown)
    plan_zz_mark = list(package = "rtfreporter", label = list(en = "Zz mark", ja = "Zz")),
    zz_layer = list(package = "zzpkg", figure = "layer"),
    zz_head = list(package = "zzpkg", figure = "compose", join = "|>", first_arg_name = "pieces",
                   returns = "object"),
    zz_tail = list(package = "zzpkg", args = list(stuff = list(type = "string"))),
    rtf_zz = list(package = "zzpkg"))
  d <- tflspec:::.fn_defs_build(schema, methods)
  expect_identical(d$fn, c("data_row", "ard_zz_count", "plan_zz_mark", "zz_layer", "zz_head",
                           "zz_tail"))
  expect_identical(d$returns, c("data", "ard", "plan", "plot", "object", "unknown"))
  expect_identical(d$join, c("|>", "|>", "|>", "+", "|>", "|>"))
  expect_identical(d$first_arg_name, c("from", "data", NA, NA, "pieces", "stuff"))
  expect_identical(d$label_ja[d$fn == "plan_zz_mark"], "Zz")
  expect_identical(d$label_en[d$fn == "ard_zz_count"], "Zz counts")
})

test_that("an unknown join or returns word stops, naming the words", {
  methods <- data.frame(method = character(), label = character(), call = character(),
                        stringsAsFactors = FALSE)
  bad_join <- list(zz_fn = list(package = "zzpkg", join = "%>%"))
  expect_error(tflspec:::.fn_defs_build(bad_join, methods), "\\|>, \\+")
  bad_ret <- list(zz_fn = list(package = "zzpkg", returns = "table"))
  expect_error(tflspec:::.fn_defs_build(bad_ret, methods), "data, ard, model")
})

test_that("tfl_arg_defs(fn = ) carries the function's row as the attribute fn", {
  a <- attr(tfl_arg_defs(fn = "plan_cells"), "fn")
  expect_identical(names(a), fn_cols)
  expect_identical(nrow(a), 1L)
  expect_identical(a$fn, "plan_cells")
  g <- attr(tfl_arg_defs(fn = "geom_line"), "fn")
  expect_identical(g$join, "+")
  expect_identical(attr(tfl_arg_defs(fn = "data"), "fn")$fn, "data_row")
  expect_null(attr(tfl_arg_defs(fn = "analysis"), "fn"))
  expect_null(attr(tfl_arg_defs(fn = "rtf_page"), "fn"))
  expect_null(attr(tfl_arg_defs(template = "tflspec_v1/demog"), "fn"))
})

test_that("objects(<kind>) is a choice word, its kind one of the returns words (B-2)", {
  expect_identical(length(.arg_choice_sources), 16L)
  expect_true("objects" %in% .arg_choice_sources)
  for (k in .fn_def_returns) expect_silent(.arg_check_choices(sprintf("objects(%s)", k), "x"))
  expect_silent(.arg_check_choices("objects(ard), study_data", "x"))
  d <- .arg_table(list(x = list(type = "string", input = "pick", choices = "objects(plot)")),
                  "content", "f")
  expect_identical(d$choices[[1L]], "objects(plot)")
  expect_error(.arg_check_choices("objects(ggplot)", "x"), "objects\\(<kind>\\)")
  expect_error(.arg_check_choices("objects", "x"), "objects\\(<kind>\\)")
  # (the same words as tfl_fn_defs()' returns: one closed list)
  expect_true(all(tfl_fn_defs()$returns %in% .fn_def_returns))
})
