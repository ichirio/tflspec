# A template's `extends` (PLAN-5B, 5b-1 and 5b-2): the value's forms, the
# set's default and its fixed step, one layer order, the reference range,
# the step_off rule (maps_to), the route record and the one reader.  demog's
# set is [defaults] (5b-2), stf_t02_demog's a fixed {content: none}.

te_study <- function(defaults, patterns = list(), settings = list()) {
  d <- tempfile("te")
  dir.create(file.path(d, "outputs"), recursive = TRUE)
  yaml::write_yaml(list(study_id = "TE-1", settings = c(list(treatment = "TRT01P"), settings)),
                   file.path(d, "study.yml"))
  writeLines(defaults, file.path(d, "defaults.yml"))
  if (length(patterns)) dir.create(file.path(d, "patterns"))
  for (nm in names(patterns)) writeLines(patterns[[nm]], file.path(d, "patterns", paste0(nm, ".yml")))
  s <- tfl_read_study_spec(d)
  unlink(d, recursive = TRUE)
  s
}

# one plan_style argument from each layer, `font` from all of them
te_layers <- function(settings = list()) {
  te_study(c(
    "content:",
    "  - plan_style: {font: D, header_bold: true, border_header: {fn: rtf_border, args: {top: true}}}",
    "  - plan_stub: {name: Dstub, before: true, heading: Study heading}",
    "  - plan_blanks: {where: between_groups, first: true, last: true}",
    "  - plan_paginate_rows: {max_rows: 40}",
    "styles:",
    "  compact:",
    "    content:",
    "      - plan_style: {font: S, header_align: center}"),
    patterns = list(x = c(
      "content:",
      "  - plan_style: {font: P, align: right}")),
    settings = settings)
}

te_demog <- function(..., args = list(), name = "demog") {
  tfl_output("T-9", "table", population = "SAF", ...,
             template = list(name = name, args = c(list(by = "TRT01A", continuous = "AGE"), args)))
}

te_item <- function(ro, fn) Filter(function(it) identical(it$fn, fn), ro$content)[[1L]]

test_that("extends' forms: files, none, a map by step; NULL for a template report; checked", {
  expect_identical(tfl_output("T-1", "table")$extends, "defaults")
  expect_null(te_demog()$extends)
  o <- tfl_output("T-1", "table", extends = list(content = "none", data = list("patterns/x", "defaults")))
  expect_identical(o$extends, list(content = "none", data = c("patterns/x", "defaults")))
  e <- tfl_output_extends(o)
  expect_identical(names(e), c("data", "ard", "content"))
  expect_identical(e$content, character())
  expect_identical(e$data, c("patterns/x", "defaults"))
  expect_identical(tfl_output_extends(o, "data"), c("patterns/x", "defaults"))
  # the page step has no files: the page comes from the page configuration
  expect_error(tfl_output("T-1", "table", extends = list(page = "none")), "no step page")
  expect_identical(tfl_output_extends(tfl_output("T-1", "table", extends = "none"), "data"), character())
  expect_error(tfl_output("T-1", "table", extends = list(foo = "none")),
               "no step 'foo'; the steps: data, ard, content")
  expect_error(tfl_output("T-1", "table", extends = list(content = 1)), "a map by step")
  expect_error(tfl_output_extends(o, "plot"), "one of data, ard, content")
  # a template report's: its template's, a map's other steps its template's too (Q1)
  expect_identical(tfl_output_extends(te_demog())$content, "defaults")
  expect_identical(tfl_output_extends(te_demog(extends = list(data = "none")))$content, "defaults")
  expect_identical(tfl_output_extends(te_demog(extends = list(data = "none")))$data, character())
  expect_identical(tfl_output_extends(te_demog(name = "stf_t02_demog"))$content, character())
  expect_identical(tfl_output_extends(te_demog(extends = "defaults"))$content, "defaults")
  # written and read back: a map as a map, NULL not written
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  tfl_write_output(o, f)
  expect_true(any(grepl("^  content: none$", readLines(f))))
  expect_identical(tfl_read_output(f)$extends, o$extends)
  tfl_write_output(te_demog(), f)
  expect_false(any(grepl("^extends", readLines(f))))
  expect_null(tfl_read_output(f)$extends)
  # (a template report's `extends: [defaults]`, as every writer wrote it before:
  # not written, its template's)
  writeLines(c("id: T-9", "kind: table", "population: SAF", "extends: [defaults]", "template:",
               "  name: stf_t02_demog", "  args: {by: TRT01A, continuous: AGE}"), f)
  expect_null(tfl_read_output(f)$extends)
  # a code SPEC that writes none: defaults
  writeLines(c("id: T-1", "kind: table", "content: []"), f)
  expect_identical(tfl_read_output(f)$extends, "defaults")
})

test_that("T1: one order -- report > template > style > pattern > defaults; a code SPEC the same", {
  s <- te_layers()
  o <- te_demog(extends = c("patterns/x", "defaults"), style = "compact",
                content = list(tfl_item("plan_style", list(font = "R"))))
  ro <- tfl_resolve_output(o, s, provenance = TRUE)
  ps <- te_item(ro, "plan_style")
  expect_identical(ps$args$font, "R")
  expect_identical(unname(ps$arg_from[c("font", "border_header", "header_align", "align", "header_bold")]),
                   c("own", "template:demog", "style:compact", "pattern:x", "defaults"))
  # (border_header: the template's, as its reference read it -- the one rule)
  rt <- attr(ro, "template")$routes
  expect_identical(rt$route[rt$param == "border_header"], "study:plan_style.border_header")
  # a code SPEC: the same layers, no template
  c0 <- tfl_output("T-1", "table", extends = c("patterns/x", "defaults"), style = "compact",
                   content = list(tfl_item("plan_style", list(font = "R"))))
  rc <- tfl_resolve_output(c0, s, provenance = TRUE)
  pc <- te_item(rc, "plan_style")
  expect_identical(unname(pc$arg_from[c("font", "header_align", "align", "header_bold", "border_header")]),
                   c("own", "style:compact", "pattern:x", "defaults", "defaults"))
})

test_that("T2: the map forms on demog and stf_t02_demog; an unknown step stops", {
  s <- te_layers()
  from <- function(ro) vapply(ro$content, function(it) it$from %||% "", "")
  # {content: none} written on demog: no study content, the page kept
  r1 <- tfl_resolve_output(te_demog(extends = list(content = "none")), s)
  expect_false(any(c("defaults", "pattern:x") %in% from(r1)))
  expect_identical(r1$page$header[["1"]]$left, "{COMPANY}")
  # none written: its set's [defaults], the same as written
  r0 <- tfl_resolve_output(te_demog(), s, provenance = TRUE)
  expect_true("defaults" %in% unlist(lapply(r0$content, function(it) it$arg_from)))
  expect_identical(lapply(r0$content, unclass),
                   lapply(tfl_resolve_output(te_demog(extends = "defaults"), s, provenance = TRUE)$content,
                          unclass))
  # [patterns/x, defaults] lays the pattern
  r2 <- tfl_resolve_output(te_demog(extends = c("patterns/x", "defaults")), s, provenance = TRUE)
  expect_true("pattern:x" %in% unlist(lapply(r2$content, function(it) it$arg_from)))
  # stf_: its content still none, whatever the other steps lay
  r3 <- tfl_resolve_output(te_demog(name = "stf_t02_demog", extends = list(data = "patterns/x")), s)
  expect_false(any(c("defaults", "pattern:x") %in% from(r3)))
  expect_true("style:stf_t02" %in% from(r3))
  expect_error(te_demog(extends = list(contents = "none")), "no step 'contents'")
})

test_that("T3: stf_t02_demog fixes the content step; `template:` holds no extends or style", {
  s <- te_layers()
  msg <- "template stf_t02_demog fixes extends' content step to none; write the other steps as a map"
  expect_error(tfl_resolve_output(te_demog(name = "stf_t02_demog", extends = c("patterns/x", "defaults")), s),
               msg)
  expect_error(tfl_resolve_output(te_demog(name = "stf_t02_demog", extends = list(content = "defaults")), s),
               msg)
  expect_error(tfl_output_extends(te_demog(name = "stf_t02_demog", extends = "patterns/x")), msg)
  # (a plain [defaults], as saved files write it, reads as not written)
  expect_identical(tfl_output_extends(te_demog(name = "stf_t02_demog", extends = "defaults")),
                   tfl_output_extends(te_demog(name = "stf_t02_demog")))
  # (`none` for every step keeps the content step none)
  expect_identical(tfl_output_extends(te_demog(name = "stf_t02_demog", extends = "none"), "content"),
                   character())
  o <- tfl_output("T-9", "table", population = "SAF",
                  template = list(name = "stf_t02_demog", extends = "none",
                                  args = list(by = "TRT01A", continuous = "AGE")))
  expect_error(tfl_resolve_output(o, s), "`extends` is the report's field, not `template:`'s")
  o$template$extends <- NULL
  o$template$style <- "compact"
  expect_error(tfl_resolve_output(o, s), "`style` is the report's field")
  # a template's registration checks its set
  expect_error(.tfl_template("te_bad", "table", "summary", list(), function(a, ctx) list(),
                             extends_fixed = "content"), "names steps")
  expect_error(.tfl_template("te_bad", "table", "summary", list(), function(a, ctx) list(),
                             extends = list(plot = "none")), "no step 'plot'")
})

test_that("T4: one rule -- a reference wins, the study's argument does not reach under it", {
  s <- te_layers()
  ro <- tfl_resolve_output(te_demog(extends = "defaults"), s, provenance = TRUE)
  rt <- attr(ro, "template")$routes
  expect_identical(rt$route[rt$param == "stub_name"], "study:plan_stub.name")
  st <- te_item(ro, "plan_stub")
  expect_identical(st$args$name, "Dstub")
  expect_false(identical(unname(st$arg_from["name"]), "defaults"))
  # none written: demog's set, [defaults], the same
  r0 <- tfl_template_routes(te_demog(), s)
  expect_identical(r0$route[r0$param == "stub_name"], "study:plan_stub.name")
  # under {content: none} a reference reads nothing: its fallback, else unset
  rn <- tfl_template_routes(te_demog(extends = list(content = "none")), s)
  expect_identical(rn$route[rn$param == "stub_name"], "unset")
  expect_identical(rn$route[rn$param == "max_rows"], "fallback")
  expect_null(tfl_study_value(s, "plan_stub.name", te_demog(extends = list(content = "none")))$value)
})

test_that("T5: the range -- the pattern over the defaults, the style set, !unset, no value", {
  s <- te_study(c(
    "content:",
    "  - plan_stub: {name: Dstub}",
    "  - plan_paginate_rows: {max_rows: 40}",
    "styles:",
    "  compact:",
    "    content:",
    "      - plan_stub: {name: Sstub}"),
    patterns = list(x = c("content:", "  - plan_stub: {name: Pstub}"),
                    u = c("content:", "  - plan_paginate_rows:", "      max_rows: !unset")))
  o <- te_demog(extends = c("patterns/x", "defaults"))
  rt <- tfl_template_routes(o, s)
  expect_identical(rt$value[rt$param == "stub_name"], "Pstub")
  expect_identical(tfl_study_value(s, "plan_stub.name", o)[c("value", "key", "from")],
                   list(value = "Pstub", key = "plan_stub.name", from = "pattern:x"))
  # the style set's value is read, nearest
  rs <- tfl_template_routes(te_demog(extends = c("patterns/x", "defaults"), style = "compact"), s)
  expect_identical(rs$value[rs$param == "stub_name"], "Sstub")
  # !unset in the pattern: the fallback
  ru <- tfl_template_routes(te_demog(extends = c("patterns/u", "defaults")), s)
  expect_identical(ru$route[ru$param == "max_rows"], "fallback")
  expect_identical(ru$value[ru$param == "max_rows"], "30")
  expect_identical(rt$route[rt$param == "max_rows"], "study:plan_paginate_rows.max_rows")
  # a reference with no value and no fallback: unset, not in the call
  expect_identical(rt$route[rt$param == "count_cell"], "unset")
  ro <- tfl_resolve_output(o, s)
  expect_null(te_item(ro, "plan_cells")$args$categorical)
  # no report: the defaults and the settings.style set
  expect_identical(tfl_study_value(s, "plan_stub.name")$value, "Dstub")
  s2 <- te_study(readLines(textConnection(c("content:", "  - plan_stub: {name: Dstub}", "styles:",
                                            "  compact:", "    content:", "      - plan_stub: {name: Sstub}"))),
                 settings = list(style = "compact"))
  expect_identical(tfl_study_value(s2, "plan_stub.name")[c("value", "from")],
                   list(value = "Sstub", from = "style:compact"))
  expect_identical(tfl_study_value(s2, c("plan_nope.x", "settings.style"))[c("value", "key", "from")],
                   list(value = "compact", key = "settings.style", from = "settings"))
  expect_null(tfl_study_value(s2, "plan_nope.x")$value)
})

test_that("T6: the program -- the call, then only what the template did not write", {
  s <- te_layers()
  for (o in list(te_demog(), te_demog(extends = list(content = "none")))) {
    ro <- tfl_resolve_output(o, s)
    tp <- attr(ro, "template")
    expect_true(tp$ok)
    for (it in tp$rest) {
      same <- Filter(function(t) identical(t$fn, it$fn), tp$items)
      if (!length(same) || !.arg_keyed(it$fn, ro$content)) next
      # (plan_stub's `before` is said again: rtfreporter's plan_stub() always sets it)
      expect_length(setdiff(intersect(names(it$args), names(same[[1L]]$args)),
                            if (it$fn == "plan_stub") "before"), 0L)
    }
    code <- tfl_content_code(ro)
    expect_true(any(grepl("tfl_tpl(", code, fixed = TRUE)))
  }
})

test_that("T7: a stop value takes its maps_to out of the study's layers (A, B, the record)", {
  s <- te_layers()
  o <- te_demog(extends = "defaults", args = list(blanks = FALSE))
  ro <- tfl_resolve_output(o, s)
  bl <- te_item(ro, "plan_blanks")
  expect_null(bl$args$where)
  expect_false(bl$args$first)
  rt <- attr(ro, "template")$routes
  expect_identical(rt$route[rt$param == "plan_blanks.where"], "removed (blanks: FALSE)")
  expect_false(any(grepl("between_groups", tfl_content_code(ro), fixed = TRUE)))
  # B writes it !unset: its layers would give it back
  b <- tfl_template_to_spec(o, s)
  expect_s3_class(Filter(function(it) it$fn == "plan_blanks", b$content)[[1L]]$args$where, "tfl_unset")
  expect_identical(b$extends, "defaults")
  expect_identical(b$style, "tfl_standard")
  rb <- tfl_resolve_output(b, s)
  expect_identical(lapply(ro$content, unclass), lapply(rb$content, unclass))
  # none written: the set's [defaults], the same
  expect_identical(lapply(tfl_resolve_output(te_demog(args = list(blanks = FALSE)), s)$content, unclass),
                   lapply(ro$content, unclass))
  # heading: !unset -- no study heading
  rh <- tfl_resolve_output(te_demog(extends = "defaults", args = list(heading = tfl_unset())), s)
  expect_null(te_item(rh, "plan_stub")$args$heading)
  expect_identical(te_item(tfl_resolve_output(te_demog(extends = "defaults"), s), "plan_stub")$args$heading,
                   "Study heading")
  # header_n maps to nothing in the study: "none" takes nothing out
  expect_identical(tfl_template_params("demog")$header_n$maps_to, character())
  rn <- tfl_template_routes(te_demog(extends = "defaults", args = list(header_n = "none")), s)
  expect_false(any(startsWith(rn$route, "removed")))
  # the declaration is checked
  expect_error(tfl_param("x", maps_to = "nope"), "maps_to")
  expect_error(tfl_param("x", maps_to = "plan_nope.where"), "not a plan verb")
})

test_that("T8: one record -- tfl_template_routes(), the resolved attribute, tfl_tpl_args()$route", {
  s <- te_layers()
  o <- te_demog()
  rt <- tfl_template_routes(o, s)
  expect_identical(rt, attr(tfl_resolve_output(o, s), "template")$routes)
  expect_identical(names(rt), c("param", "step", "value", "route", "key", "fallback", "fixed", "reaches"))
  expect_identical(utils::tail(rt$param, 2L), c("extends", "style"))
  expect_identical(rt$value[rt$param == "extends"], "defaults")
  expect_identical(rt$route[rt$param == "extends"], "template")
  ta <- tfl_tpl_args("demog", study = s, population = "SAF")
  # (the parameters the report does not give)
  both <- intersect(setdiff(ta$param, c("style", "by", "continuous")), rt$param)
  expect_identical(ta$route[match(both, ta$param)], rt$route[match(both, rt$param)])
  # the reader the record's keys come from
  k <- rt$key[rt$param == "stub_name"]
  expect_identical(tfl_study_value(s, k, o)$value, "Dstub")
  # the fixed ones of stf_t02_demog; its extends fixed
  rf <- tfl_template_routes(te_demog(name = "stf_t02_demog"), s)
  expect_identical(rf$route[rf$param %in% c("formats", "missing_row")], c("fixed", "fixed"))
  expect_true(rf$fixed[rf$param == "extends"])
  expect_identical(rf$value[rf$param == "style"], "stf_t02")
  expect_error(tfl_template_routes(tfl_output("T-1", "table"), s), "not a template report")
})

test_that("T9: the style first -- the field, settings.style; stf_t02_demog's is fixed", {
  s <- te_layers()
  r1 <- tfl_resolve_output(te_demog(extends = "defaults", style = "compact"), s, provenance = TRUE)
  expect_identical(attr(r1, "template")$style, "compact")
  expect_true("style:compact" %in% unlist(lapply(r1$content, function(it) it$arg_from)))
  sc <- te_layers(settings = list(style = "compact"))
  r2 <- tfl_resolve_output(te_demog(extends = "defaults"), sc, provenance = TRUE)
  expect_identical(attr(r2, "template")$style, "compact")
  rt <- attr(r2, "template")$routes
  expect_identical(rt$route[rt$param == "style"], "study:settings.style")
  expect_identical(lapply(r1$content, unclass), lapply(r2$content, unclass))
  expect_error(tfl_resolve_output(te_demog(name = "stf_t02_demog", style = "compact"), s),
               "template stf_t02_demog fixes its style \\(stf_t02\\); leave `style:` out")
  # its own style written is no change
  expect_no_error(tfl_resolve_output(te_demog(name = "stf_t02_demog", style = "stf_t02"), s))
})

test_that("T10: the templates sheet leaves extends unwritten; an stf_t02_demog row builds", {
  f <- system.file("extdata", "template-spec", "templates.xlsx", package = "tflspec")
  skip_if(!nzchar(f))
  skip_if_not_installed("readxl")
  r <- tfl_read_template_sheets(f)
  expect_true(all(vapply(r, function(x) is.null(x$extends), NA)))
  x <- r[[1L]]
  o <- tfl_output(x$id, x$kind, label = x$label %||% NA_character_, population = x$population,
                  template = list(name = "stf_t02_demog", args = list(by = "TRT01A", continuous = "AGE")))
  expect_identical(tfl_output_extends(o, "content"), character())
  expect_s3_class(tfl_resolve_output(o, te_layers()), "tfl_output")
})

# a plan's RTF as text (its date taken out): the program's and the run's
te_rtf <- function(ro, s, plan) {
  f <- tempfile("te", fileext = ".rtf")
  on.exit(unlink(f))
  suppressMessages(rtfreporter::generate_rtfreport(tfl_report(ro, content = plan, study = s), f,
                                                   overwrite = TRUE))
  x <- readLines(f, warn = FALSE)
  gsub("[0-9]{2}[A-Z][a-z]{2}[0-9]{4}( +[0-9]{2}:[0-9]{2}(:[0-9]{2})?)?", "<date>", x)
}

# the plan the report's program makes, its code run
te_program_plan <- function(ro, ard) {
  e <- new.env()
  e$ard <- ard
  suppressMessages(eval(parse(text = tfl_content_code(ro, pipe = "|>")), envir = e))
  e$plan
}

test_that("T11: tfl_tpl(study = ) carries the study's items; the program runs as tfl_run_output() does", {
  skip_if_not_installed("cards")
  s <- te_layers()
  saf <- adsl[adsl$SAFFL == "Y", ]
  # (stf_t02_demog's Total column: the overall rows too)
  ard <- cards::bind_ard(cards::ard_tabulate(saf, variables = TRT01A),
                         cards::ard_summary(saf, by = TRT01A, variables = AGE),
                         cards::ard_summary(saf, variables = AGE))
  own <- list(tfl_item("plan_style", list(header_bold = FALSE)),
              tfl_item("plan_stub", list(heading = "Own heading")))
  cases <- list(
    plain = te_demog(),
    own = te_demog(content = own),
    blanks = te_demog(args = list(blanks = FALSE)),
    header_n = te_demog(args = list(header_n = "none")),
    heading = te_demog(args = list(heading = tfl_unset())),
    style = te_demog(style = "compact"),
    stf = te_demog(name = "stf_t02_demog"))
  for (nm in names(cases)) {
    ro <- suppressMessages(tfl_resolve_output(cases[[nm]], s))
    code <- strsplit(paste(tfl_content_code(ro, pipe = "|>"), collapse = "\n"), "\n", fixed = TRUE)[[1L]]
    # the study's items in the call, after a note of their layer; none after it
    at <- grep("^  study = list[(]$", code)
    expect_length(at, 1L)
    end <- grep("^[)]", code)[1L]
    expect_gt(end, at)
    after <- code[seq_along(code) > end]
    expect_false(any(grepl("# (study", after, fixed = TRUE)), info = nm)
    expect_false(any(grepl("# (pattern", after, fixed = TRUE)), info = nm)
    # the program's plan is the run's (tfl_run_output() runs the resolved items)
    expect_identical(te_rtf(ro, s, te_program_plan(ro, ard)),
                     te_rtf(ro, s, suppressMessages(tfl_content_run(ro, ard))), info = nm)
  }
  code <- function(nm) {
    x <- tfl_content_code(suppressMessages(tfl_resolve_output(cases[[nm]], s)))
    strsplit(paste(x, collapse = "\n"), "\n", fixed = TRUE)[[1L]]
  }
  # the report's own items stay after the call
  expect_match(code("own"), "Own heading", fixed = TRUE, all = FALSE)
  expect_identical(grep("# (own)", code("own"), fixed = TRUE) > grep("^[)]", code("own"))[1L], c(TRUE, TRUE))
  # a stop value does not bring the study's default back (SM 3.5)
  expect_false(any(grepl("between_groups", code("blanks"), fixed = TRUE)))
  expect_false(any(grepl("Study heading", code("heading"), fixed = TRUE)))
  expect_match(code("plain"), "plan_stub = list(heading = \"Study heading\")", fixed = TRUE, all = FALSE)
  # the style set's items, noted as such
  expect_match(code("style"), "# (study style: compact)", fixed = TRUE, all = FALSE)
  # (extends [defaults], batch 2 (ii)): summary carries the study's items too, as demog does
  rs <- tfl_resolve_output(te_demog(name = "summary"), s)
  expect_true(any(grepl("study = list(", tfl_content_code(rs), fixed = TRUE)))
  # a template not yet migrated keeps the study's lines after the call
  ro3 <- tfl_output("T-10", "table", population = "SAF",
                    template = list(name = "ae_soc_pt", args = list(population = "SAF",
                                                                   hierarchy = c("AEBODSYS", "AEDECOD"))))
  rs <- tfl_resolve_output(ro3, s)
  expect_false(any(grepl("study = list(", tfl_content_code(rs), fixed = TRUE)))
})

test_that("T12: tfl_tpl(study = ) -- under the template, by tflspec's merge; checked", {
  skip_if_not_installed("cards")
  saf <- adsl[adsl$SAFFL == "Y", ]
  ard <- cards::bind_ard(cards::ard_tabulate(saf, variables = TRT01A),
                         cards::ard_summary(saf, by = TRT01A, variables = AGE))
  tab <- function(p) suppressMessages(rtfreporter::plan_apply(p, "table"))
  call <- function(...) tfl_tpl(ard, "demog", by = "TRT01A", continuous = "AGE", population = "SAF", ...)
  ap <- function(p) suppressMessages(rtfreporter::plan_apply(p))
  # the template's plan_stub(name) wins over the study's; the study's heading reaches
  p1 <- call(stub_name = "row_label", study = list(plan_stub = list(name = "zzz", heading = "Study")))
  p2 <- call(stub_name = "row_label", study = list(plan_stub = list(heading = "Study")))
  expect_identical(ap(p1), ap(p2))
  expect_false(identical(ap(p2), ap(call(stub_name = "row_label"))))
  expect_identical(names(tab(p1)), names(tab(call(stub_name = "row_label"))))
  # an argument the template does not write: the study's
  expect_identical(names(ap(call(study = list(plan_stub = list(name = "zzz"))))[[1L]]$data)[1L], "zzz")
  # one verb again: another layer's arguments merge into one item
  expect_identical(.tpl_study_items(list(plan_style = list(font = "A"), plan_style = list(align = "right"))),
                   list(tfl_item("plan_style", list(font = "A", align = "right"), from = "defaults")))
  expect_length(.tpl_study_items(list(plan_cell_style = list(cols = "a"), plan_cell_style = list(cols = "b"))), 2L)
  expect_identical(.tpl_study_items(NULL), list())
  # checked
  expect_error(call(study = list(plot = list(x = 1))), "plan verbs")
  expect_error(call(study = list(plan_style = "right")), "named list of its arguments")
  expect_error(call(study = list(list(align = "right"))), "named list of its arguments")
})
