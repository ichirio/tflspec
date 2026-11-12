# A template parameter's reference to the study standard (tfl_param(study = )):
# one generic reader, the three declaration forms, what the help shows, the
# style helper, the figure style part, the set's extends, no study templates
# (migration batch 0b)

sr_study <- function(settings = list(), defaults = NULL) {
  d <- tempfile()
  dir.create(d)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"),
                       full.names = TRUE), d, recursive = TRUE)
  yaml::write_yaml(list(study_id = "SR-1", settings = c(list(treatment = "TRT01P"), settings)),
                   file.path(d, "study.yml"))
  if (!is.null(defaults)) writeLines(defaults, file.path(d, "defaults.yml"))
  s <- tfl_read_study_spec(d)
  unlink(d, recursive = TRUE)
  s
}

# a template with one parameter of each form, none of them tflspec's own
sr_register <- function(name, params) {
  tfl_template(name, "table", "summary",
               params = c(list(tfl_param("population", "SAF", type = "text")), params),
               body = function(a, ctx) tfl_template_expand(
                 "summary", list(by = "TRT01A", population = a$population, continuous = "AGE"),
                 codelists = ctx$codelists, population = ctx$population),
               example = list(population = "SAF"))
  invisible(name)
}
sr_resolve <- function(name, args, s) {
  tfl_resolve_output(tfl_output("T-9", "table", common = list(population = "SAF"),
                                template = list(name = name, args = args)), s)
}

test_that("one generic reader: any settings.<key> works with no code change", {
  suppressMessages(sr_register("sr_any", list(
    tfl_param("zeta", "fb", type = "text", study = "settings.zeta"),
    tfl_param("eta", NULL, type = "text", study = "settings.eta"),
    tfl_param("theta", type = "text", required = TRUE, study = "settings.theta"))))
  on.exit(rm("sr_any", envir = .tpl_registry))
  # a key nobody wrote code for: the study's value, the report's wins
  s <- sr_study(list(zeta = "from the study", eta = "E", theta = "T"))
  ro <- sr_resolve("sr_any", list(), s)
  a <- attr(ro, "template")
  expect_identical(a$args[c("zeta", "eta", "theta")],
                   list(zeta = "from the study", eta = "E", theta = "T"))
  expect_true(all(c("zeta", "eta", "theta") %in% a$study))
  expect_identical(a$study_keys[c("zeta", "eta")],
                   c(zeta = "settings.zeta", eta = "settings.eta"))
  expect_message(ro2 <- sr_resolve("sr_any", list(zeta = "own"), s),
                 "overrides the study standard \\(settings.zeta\\)")
  expect_identical(attr(ro2, "template")$args$zeta, "own")
  # a nested settings key by its path
  expect_equal(.tpl_study_read("settings.a.b", NULL, sr_study(list(a = list(b = 3)))), 3)
  expect_null(.tpl_study_read("settings.a.c", NULL, sr_study(list(a = list(b = 3)))))
})

test_that("the three forms: with a fallback, with none (unset), required (stops)", {
  suppressMessages(sr_register("sr_forms", list(
    tfl_param("zeta", "fb", type = "text", study = "settings.zeta"),
    tfl_param("eta", NULL, type = "text", study = "settings.eta"),
    tfl_param("theta", type = "text", required = TRUE, study = "settings.theta"))))
  on.exit(rm("sr_forms", envir = .tpl_registry))
  s <- sr_study(list(theta = "T"))
  a <- attr(sr_resolve("sr_forms", list(), s), "template")
  # the study has no zeta: the fallback applies in the body; none is written
  expect_false("zeta" %in% names(a$args))
  expect_false("eta" %in% names(a$args))
  expect_identical(tfl_template_params("sr_forms")$zeta$default, "fb")
  expect_true(is.null(tfl_template_params("sr_forms")$eta$default))
  # required: neither the report nor the study gives it
  expect_error(sr_resolve("sr_forms", list(), sr_study()), "theta")
  # the report gives it
  expect_silent(sr_resolve("sr_forms", list(theta = "own"), sr_study()))
})

test_that("a reference to a verb's argument reads spec/defaults.yml only", {
  suppressMessages(sr_register("sr_verb", list(
    tfl_param("cell", NULL, type = "text", study = "plan_cells.categorical"))))
  on.exit(rm("sr_verb", envir = .tpl_registry))
  s <- sr_study()
  expect_identical(.tpl_study_read("plan_cells.categorical", NULL, s), "{n:.0f} ({p:.1f%})")
  expect_identical(attr(sr_resolve("sr_verb", list(), s), "template")$args$cell,
                   "{n:.0f} ({p:.1f%})")
  # a named-dots argument's own name as the last part
  s2 <- sr_study(defaults = c("content:", "  - plan_cells:", "      continuous:",
                              "        n: \"{N:.0f}\""))
  expect_identical(.tpl_study_read("plan_cells.continuous.n", NULL, s2), "{N:.0f}")
  expect_null(.tpl_study_read("plan_cells.continuous.x", NULL, s2))
  expect_null(.tpl_study_read("plan_cells.categorical", NULL, s2))
  # a pattern's item and a style set's are not the study standard
  s3 <- s
  s3$patterns <- list(p = list(content = list(tfl_item("plan_cells", list(categorical = "X")))))
  s3$defaults$content <- list()
  expect_null(.tpl_study_read("plan_cells.categorical", NULL, s3))
  # the report's own field
  expect_identical(.tpl_study_read("output.common.population", list(common = list(population = "SAF")), s), "SAF")
  expect_null(.tpl_study_read("output.common.population", list(common = list()), s))
})

test_that("a key the reader cannot follow stops where the parameter is declared", {
  expect_error(tfl_param("x", study = "plan_nonsense.arg"), "not a verb")
  expect_error(tfl_param("x", study = "settings"), "key of the study standard")
  expect_error(tfl_param("x", study = c("settings.a", "b")), "key of the study standard")
  expect_s3_class(tfl_param("x", study = c("settings.a", "settings.b")), "tfl_param")
  expect_error(tfl_param("x", study = c("settings.a", "plan_nonsense.arg")), "not a verb")
  expect_s3_class(tfl_param("x", study = "settings.anything"), "tfl_param")
  expect_s3_class(tfl_param("x", study = "figure_style.km.theme"), "tfl_param")
  expect_s3_class(tfl_param("x", study = "plan_digits.rounding"), "tfl_param")
  expect_s3_class(tfl_param("x", study = FALSE), "tfl_param")
})

test_that("tfl_tpl_args(): the reference, its fallback and where the value is from", {
  a <- tfl_tpl_args("demog")
  r <- function(a, nm) a[a$param == nm, , drop = FALSE]
  expect_identical(r(a, "sd_missing")$study_key, "settings.sd_missing")
  expect_identical(r(a, "sd_missing")$default, "study: settings.sd_missing, else -")
  expect_identical(r(a, "sd_missing")$fallback, "-")
  expect_identical(r(a, "sd_missing")$layer, "fallback")
  expect_identical(r(a, "count_cell")$default, "study: plan_cells.categorical, plan_cells.default, no fallback")
  expect_identical(r(a, "count_cell")$layer, "default")
  expect_true(is.na(r(a, "count_cell")$fallback))
  expect_true(is.na(r(a, "total")$study_key))
  s <- sr_study(list(sd_missing = "NA"))
  b <- tfl_tpl_args("demog", study = s, population = "SAF")
  expect_identical(r(b, "sd_missing")$layer, "study: settings.sd_missing")
  expect_identical(r(b, "sd_missing")$effective, "NA")
  expect_identical(r(b, "count_cell")$layer, "study: plan_cells.categorical")
  # demog's analysis set is the report's, not a parameter of the table; summary's is
  expect_identical(nrow(r(b, "population")), 0L)
  expect_identical(r(tfl_tpl_args("ae_soc_pt", study = s, population = "SAF"), "population")$layer, "report")
  # the Rd table and the help say it too
  rd <- paste(.tpl_rd("demog"), collapse = "\n")
  expect_match(rd, "study: settings.sd_missing, else -", fixed = TRUE)
  expect_match(rd, "study: plan_cells.categorical, plan_cells.default, no fallback", fixed = TRUE)
  out <- capture.output(tfl_tpl_help("demog"))
  expect_true(any(grepl("[study: settings.sd_missing, else -]", out, fixed = TRUE)))
})

test_that("ai/templates.md: a parameter that refers to the study standard says so", {
  txt <- paste(.ai_templates_list("table"), collapse = "\n")
  expect_match(txt, "`sd_missing` (text) = study: settings.sd_missing, else -", fixed = TRUE)
  expect_match(txt, "study: plan_cells.categorical, plan_cells.default, no fallback", fixed = TRUE)
  expect_match(paste(.ai_templates_file_text(), collapse = "\n"),
               "study: <key>, else <fallback>", fixed = TRUE)
})

test_that("the figure style the study keeps: defaults.yml's figure_style:, the built-in one behind it", {
  s0 <- sr_study()
  expect_identical(s0$figure_style, list())
  # no part: tflspec's built-in style
  expect_identical(.fig_style_study(NULL), .fig_style_builtin())
  built <- .fs_get("theme", "km", .fig_style_builtin())
  expect_identical(.tpl_study_read("figure_style.km.theme", NULL, s0), built)
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(d)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"),
                       full.names = TRUE), d, recursive = TRUE)
  cat(file = file.path(d, "defaults.yml"), append = TRUE, sep = "\n",
      "figure_style:", "  settings:",
      "    - {type: km, key: theme, value: classic}", "    - {key: base_size, value: 12}")
  s <- tfl_read_study_spec(d)
  expect_identical(.tpl_study_read("figure_style.km.theme", NULL, s), "classic")
  expect_identical(.tpl_study_read("figure_style.base_size", NULL, s), "12")
  # a key the study's table lacks: the built-in one
  expect_identical(.tpl_study_read("figure_style.km.censor_shape", NULL, s),
                   .fs_get("censor_shape", "km", .fig_style_builtin()))
  # not a key at all: none
  expect_null(.tpl_study_read("figure_style.km.no_such_key", NULL, s))
  # the study's palettes and markers are the built-in ones when it gives none
  expect_identical(.fig_style_study(s$figure_style)$colors, .fig_style_builtin()$colors)
})

test_that("the style helper: defaults.yml's styles: first, then the built-in sets", {
  s <- sr_study(defaults = c("content:", "  - plan_cells: {categorical: x}",
                             "styles:", "  compact:", "    content:",
                             "      - plan_paginate_rows: {max_rows: 40}"))
  # the study's own set
  l <- .tpl_style_set("compact", s)
  expect_identical(l$content[[1L]]$fn, "plan_paginate_rows")
  expect_identical(l$content[[1L]]$args$max_rows, 40L)
  # the built-in `standard`: an empty set, whether or not the study has one
  l0 <- .tpl_style_set("standard", s)
  expect_length(l0$content, 0L)
  expect_identical(names(l0), c("data", "codelists", "content", "page"))
  # a built-in name always is the built-in: a study's set of that name does not replace it
  s2 <- sr_study(defaults = c("styles:", "  standard:", "    content:",
                              "      - plan_paginate_rows: {max_rows: 12}"))
  expect_length(.tpl_style_set("standard", s2)$content, 0L)
  expect_identical(.tpl_style_set("stf_t02", s2)$content[[1L]]$fn, "plan_style")
  # no such set: the names that exist
  expect_error(.tpl_style_set("nope", s), "no style \"nope\".*compact, standard")
  expect_error(.tpl_style_set(c("a", "b"), s), "one style")
  # no study: the built-in sets alone
  expect_length(.tpl_style_set("standard", NULL)$content, 0L)
})

test_that("the set's extends: demog, summary, disposition, ae_overview, param_change [defaults], stf_t02_demog {content: none}; the other 23 the old order", {
  # (extends [defaults], batch 2 (ii)): summary, disposition, ae_overview and
  # param_change are layered; 29 templates, 6 layered, 23 the old order
  expect_identical(sort(Filter(function(n) .tpl_layered(n), tfl_templates()$name)),
                   c("ae_overview", "demog", "disposition", "param_change", "stf_t02_demog", "summary"))
  expect_length(Filter(function(n) !.tpl_layered(n), tfl_templates()$name), 23L)
  # (an alias is its template: stf_t14_ae_soc_pt is ae_soc_pt's, not migrated)
  expect_false(.tpl_layered("stf_t14_ae_soc_pt"))
  expect_identical(.tpl_get("demog", "test")$extends, "defaults")
  expect_identical(.tpl_get("stf_t02_demog", "test")$extends_fixed, "content")
  s <- sr_study()
  o <- tfl_output("T-9", "table", common = list(population = "SAF"),
                  template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE")))
  ro1 <- tfl_resolve_output(o, s)
  # demog's set is [defaults] (canon 1.27): the study's content is laid under
  # the template's items -- a verb it does not make reaches it ...
  s1 <- s
  s1$defaults$content <- c(s1$defaults$content,
                           list(tfl_item("plan_cell_style", list(bold = TRUE), from = "defaults")))
  from1 <- vapply(tfl_resolve_output(o, s1)$content, function(it) it$from %||% "", "")
  expect_true("defaults" %in% from1)
  expect_true(any(startsWith(from1, "template:")))
  # ... the study's page is kept
  expect_true(length(ro1$page) > 0L)
  # written {content: none}: the template's items alone, the page kept
  o$extends <- list(content = "none")
  ron <- tfl_resolve_output(o, s)
  fromn <- vapply(ron$content, function(it) it$from %||% "", "")
  expect_false(any(c("defaults", "style:compact") %in% fromn))
  expect_true(all(startsWith(fromn, "template:") | fromn == "own"))
  expect_identical(ron$page, ro1$page)
  # stf_t02_demog: no defaults content, its style:stf_t02 items, the page kept
  o2 <- tfl_output("T-9", "table", common = list(population = "SAF"),
                   template = list(name = "stf_t02_demog", args = list(by = "TRT01A", continuous = "AGE")))
  ro2 <- tfl_resolve_output(o2, s)
  from2 <- vapply(ro2$content, function(it) it$from %||% "", "")
  expect_false("defaults" %in% from2)
  expect_true("style:stf_t02" %in% from2)
  expect_identical(ro2$page$header, ro1$page$header)
  expect_identical(ro2$page$rtf_page$orientation, "landscape")
  # (extends [defaults], batch 2 (ii)): summary has the same set as demog: the
  # study's content is laid under its items, its own items marked template:summary
  o0 <- tfl_output("T-9", "table", common = list(population = "SAF"),
                   template = list(name = "summary", args = list(by = "TRT01A", continuous = "AGE")))
  from0 <- vapply(tfl_resolve_output(o0, s1)$content, function(it) it$from %||% "", "")
  expect_true("defaults" %in% from0)
  expect_true("template:summary" %in% from0)
  expect_identical(tfl_resolve_output(o0, s)$page, ro1$page)
  # another template of the same study is not migrated: the old order
  o3 <- tfl_output("T-10", "table", common = list(population = "SAF"),
                   template = list(name = "ae_soc_pt", args = list(population = "SAF",
                                                                  hierarchy = c("AEBODSYS", "AEDECOD"))))
  expect_true("defaults" %in% vapply(tfl_resolve_output(o3, s)$content,
                                      function(it) it$from %||% "", ""))
})

test_that("a study has no templates: a spec/templates/ folder stops the read", {
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "spec", "templates"), recursive = TRUE)
  writeLines("tflspec::tfl_template_variant('x', from = 'demog')",
             file.path(d, "spec", "templates", "x.R"))
  expect_error(tfl_read_study_spec(d), "a study defines reports, not templates")
  expect_false("x" %in% tfl_templates()$name)
})
