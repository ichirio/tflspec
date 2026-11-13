# The study's styles (defaults.yml's `styles:`, a report's `style:`), a
# template's look under the study's layers, and where a report's titles and
# footnotes print (study.yml's settings titles_at / footnotes_at).

st_study <- function() tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
st_report <- function() tfl_read_outputs(system.file("extdata", "spec-yaml", package = "tflspec"))[["T-14-1-1"]]
st_compact <- function() list(
  data = list(), codelists = list(),
  content = list(tfl_item("plan_style", list(font_size_half_points = 14L), from = "style:compact")),
  page = list(rtf_page = list(margin_left_in = 0.5)))

test_that("a style: the report picks it, its content and page over the defaults, marked; read and written", {
  s <- st_study()
  s$styles <- list(compact = st_compact())
  o <- st_report()
  o$style <- "compact"
  ro <- tfl_resolve_output(o, s)
  ps <- ro$content[[match("plan_style", vapply(ro$content, `[[`, "", "fn"))]]
  expect_identical(ps$from, "style:compact")
  expect_identical(ro$page$rtf_page$margin_left_in, 0.5)
  expect_match(tfl_content_code(ro), "# (study style: compact)", fixed = TRUE, all = FALSE)
  # no style: standard, none in the study -- the defaults alone
  r0 <- tfl_resolve_output(st_report(), s)
  expect_identical(r0$content[[match("plan_style", vapply(r0$content, `[[`, "", "fn"))]]$from, "defaults")
  o$style <- "tight"
  expect_error(tfl_resolve_output(o, s), "no style 'tight'")
  # written into defaults.yml and read back
  d <- withr::local_tempdir()
  dir.create(file.path(d, "spec"))
  s$dir <- d
  tfl_write_study_part(s, "defaults")
  back <- tfl_read_study_spec(d)
  expect_identical(names(back$styles), "compact")
  expect_identical(back$styles$compact$content[[1L]]$from, "style:compact")
  expect_identical(back$styles$compact$page$rtf_page$margin_left_in, 0.5)
})

test_that("a template's look is under the study's layers (they say the look), its shape over them; A = B (summary is layered, ae_soc_pt is not)", {
  skip_if_not_installed("cards")
  s <- st_study()
  k <- length(s$defaults$content) + 1L
  s$defaults$content[[k]] <- tfl_item("plan_paginate_rows", list(max_rows = 40L), from = "defaults")
  o <- tfl_output("T-9", "table", common = list(population = "SAF"),
                  template = list(name = "summary", args = list(
                    by = "TRT01A", continuous = "AGE", categorical = c("AGEGR1", "SEX"))))
  # (extends [defaults], batch 2 (ii)): summary is layered now -- the study's
  # max_rows reaches it through its own reference, the item is the template's
  ro <- tfl_resolve_output(o, s)
  pr <- ro$content[[match("plan_paginate_rows", vapply(ro$content, `[[`, "", "fn"))]]
  expect_identical(pr$args, list(max_rows = 40L, split = "group_safe"))
  expect_identical(pr$from, "template:summary")
  code <- tfl_content_code(ro, pipe = "|>")
  expect_match(code, "  max_rows = 40,", fixed = TRUE, all = FALSE)
  saf <- adsl[adsl$SAFFL == "Y", ]
  ard <- cards::bind_ard(cards::ard_tabulate(saf, variables = TRT01A),
                         cards::ard_summary(saf, by = TRT01A, variables = AGE),
                         cards::ard_tabulate(saf, by = TRT01A, variables = c(AGEGR1, SEX)))
  e <- new.env()
  e$ard <- ard
  suppressMessages(eval(parse(text = code), envir = e))
  tab <- function(p) suppressMessages(rtfreporter::plan_apply(p, "table"))
  b <- suppressMessages(tfl_content_run(ro, ard))
  expect_identical(tab(e$plan), tab(b))
  w <- tfl_template_to_spec(o, s)
  expect_identical(tab(suppressMessages(tfl_content_run(tfl_resolve_output(w, s), ard))), tab(b))
  # an unlayered template (ae_soc_pt): the study's look under, the old order
  o <- tfl_output("T-9", "table", common = list(population = "SAF"),
                  template = list(name = "ae_soc_pt", args = list(population = "SAF",
                                                                 hierarchy = c("AEBODSYS", "AEDECOD"))))
  ro <- tfl_resolve_output(o, s)
  pr <- ro$content[[match("plan_paginate_rows", vapply(ro$content, `[[`, "", "fn"))]]
  # (the study's max_rows over the layout's split, argument by argument)
  expect_identical(pr$args, list(max_rows = 40L, split = "group_force"))
  expect_identical(pr$from, "defaults")
  expect_match(tfl_content_code(ro, pipe = "|>"), "plan_paginate_rows(max_rows = 40)", fixed = TRUE, all = FALSE)
  # B written out: the study's look stays the study's
  w <- tfl_template_to_spec(o, s)
  wp <- Filter(function(it) identical(it$fn, "plan_paginate_rows"), w$content)
  expect_false(any(vapply(wp, function(it) "max_rows" %in% names(it$args), NA)))
  # a report's own look still wins
  o$content <- list(tfl_item("plan_paginate_rows", list(max_rows = 24L)))
  r2 <- tfl_resolve_output(o, s)
  expect_identical(r2$content[[match("plan_paginate_rows", vapply(r2$content, `[[`, "", "fn"))]]$args,
                   list(max_rows = 24L, split = "group_force"))
  # demog is under no study layer but its own references (canon 1.29): its max_rows
  # refers to the study's plan_paginate_rows.max_rows, so the study's 40 reaches it
  od <- tfl_output("T-9", "table", common = list(population = "SAF"),
                   template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE")))
  rd <- tfl_resolve_output(od, s)
  pd <- rd$content[[match("plan_paginate_rows", vapply(rd$content, `[[`, "", "fn"))]]
  expect_identical(pd$args$max_rows, 40L)
})

test_that("titles_at / footnotes_at: the report's titles in the running header, its footnotes in the footer", {
  s <- st_study()
  o <- st_report()
  o$page$footnotes <- list(`1` = list(left = "SD: standard deviation."))
  ro <- tfl_resolve_output(o, s)
  code <- paste(tfl_report_code(ro, content = "plan", study = s), collapse = "\n")
  expect_match(code, "rtf_titles(", fixed = TRUE)
  expect_match(code, "rtf_footnotes(", fixed = TRUE)
  s$study$settings <- list(titles_at = "header", footnotes_at = "footer")
  code <- paste(tfl_report_code(ro, content = "plan", study = s), collapse = "\n")
  expect_false(grepl("rtf_titles(", code, fixed = TRUE))
  expect_false(grepl("rtf_footnotes(", code, fixed = TRUE))
  # the title after the study's header lines, the footnote before its footer lines
  expect_lt(regexpr("Page {PAGE}", code, fixed = TRUE), regexpr('c(c = "Table 14.1.1")', code, fixed = TRUE))
  expect_lt(regexpr("SD: standard deviation.", code, fixed = TRUE), regexpr("{PROGRAM}", code, fixed = TRUE))
  s$study$settings <- list(titles_at = "titles", footnotes_at = "footnotes")
  expect_match(paste(tfl_report_code(ro, content = "plan", study = s), collapse = "\n"),
               "rtf_titles(", fixed = TRUE)
  s$study$settings <- list(titles_at = "top")
  expect_error(tfl_report_code(ro, content = "plan", study = s), "titles_at")
})

test_that("tfl_styles() lists the built-in sets and the study's, and marks the default", {
  s <- st_study()
  ts <- tfl_styles(s)
  expect_true(all(c("name", "source", "default", "page_keys", "content_verbs") %in% names(ts)))
  expect_identical(ts$source[match(c("standard", "stf_t02", "small"), ts$name)],
                   c("built-in", "built-in", "study"))
  expect_identical(ts$page_keys[ts$name == "small"], "rtf_page, rtf_style")
  expect_identical(ts$content_verbs[ts$name == "small"], "plan_style, plan_paginate_rows")
  expect_identical(ts$name[ts$default], "standard")
  # the study's default pick (settings.style) is marked; a path is read
  s$study$settings <- list(style = "small")
  expect_identical(tfl_styles(s)$name[tfl_styles(s)$default], "small")
  expect_identical(tfl_styles(system.file("extdata", "spec-yaml", package = "tflspec")), ts)
  expect_error(tfl_styles(1L), "tfl_study_spec")
})

test_that("the sample study's `small` set: a report with style: small gets its margins, font and rows", {
  s <- st_study()
  o <- st_report()
  o$style <- "small"
  # the sample report says its own rows a page (30): its own item wins; without it the set's 42 reaches the table
  o$content <- Filter(function(it) it$fn != "plan_paginate_rows", o$content)
  ro <- tfl_resolve_output(o, s)
  expect_identical(ro$page$rtf_page$margin_left_in, 0.5)
  expect_identical(ro$page$rtf_page$margin_right_in, 0.5)
  expect_identical(ro$page$rtf_style$font_size_half_points, 14L)
  fns <- vapply(ro$content, `[[`, "", "fn")
  ps <- ro$content[[match("plan_style", fns)]]
  expect_identical(ps$args$font_size_half_points, 14L)
  pr <- ro$content[[match("plan_paginate_rows", fns)]]
  expect_identical(pr$args$max_rows, 42L)
  expect_identical(tfl_resolve_output(st_report(), s)$content[[match("plan_paginate_rows", vapply(tfl_resolve_output(st_report(), s)$content, `[[`, "", "fn"))]]$args$max_rows, 30L)
  # the default report is unchanged
  r0 <- tfl_resolve_output(st_report(), s)
  expect_null(r0$page$rtf_page$margin_left_in)
})
