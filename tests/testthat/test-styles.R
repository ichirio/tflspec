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

test_that("a template's look is under the study's layers (they say the look), its shape over them; A = B", {
  skip_if_not_installed("cards")
  s <- st_study()
  k <- length(s$defaults$content) + 1L
  s$defaults$content[[k]] <- tfl_item("plan_paginate_rows", list(max_rows = 40L), from = "defaults")
  o <- tfl_output("T-9", "table", population = "SAF",
                  template = list(name = "demog", args = list(population = "SAF",
                    continuous = "AGE", categorical = c("AGEGR1", "SEX"))))
  ro <- tfl_resolve_output(o, s)
  pr <- ro$content[[match("plan_paginate_rows", vapply(ro$content, `[[`, "", "fn"))]]
  expect_identical(pr$args, list(max_rows = 40L))
  expect_identical(pr$from, "defaults")
  code <- tfl_content_code(ro, pipe = "|>")
  expect_match(code, "plan_paginate_rows(max_rows = 40)", fixed = TRUE, all = FALSE)
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
  # B written out: the study's look stays the study's, the same table
  w <- tfl_template_to_spec(o, s)
  expect_false("plan_paginate_rows" %in% vapply(w$content, `[[`, "", "fn"))
  expect_identical(tab(suppressMessages(tfl_content_run(tfl_resolve_output(w, s), ard))), tab(b))
  # a report's own look still wins
  o$content <- list(tfl_item("plan_paginate_rows", list(max_rows = 24L)))
  r2 <- tfl_resolve_output(o, s)
  expect_identical(r2$content[[match("plan_paginate_rows", vapply(r2$content, `[[`, "", "fn"))]]$args,
                   list(max_rows = 24L))
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
