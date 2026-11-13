# split (how a page break is placed), page_by and page_heading: the template's
# params (PAGE-PART PP-3).  The demographics family fixes `split`; the others
# have it in their set; page_by / page_heading replace param_page / param_heading

sp_args <- function(name, args = list(), fn = "plan_paginate_rows") {
  x <- tfl_template_expand(name, args)
  Filter(function(it) identical(it$fn, fn), x$content)
}
sp_dm <- list(population = "SAF", by = "TRT01A", continuous = "AGE", categorical = "SEX")

test_that("split: the template's default is today's behaviour, in the set", {
  expect_identical(sp_args("demog", sp_dm)[[1L]]$args, list(max_rows = 30L, split = "group_safe"))
  expect_identical(sp_args("ae_soc_pt", list(population = "SAF"))[[1L]]$args,
                   list(max_rows = 22L, split = "group_force"))
  expect_identical(sp_args("ae_pt_freq", list(population = "SAF"))[[1L]]$args,
                   list(max_rows = 22L, split = "group_safe"))
  expect_identical(sp_args("ae_max_grade", list(population = "SAF"))[[1L]]$args,
                   list(max_rows = 22L, split = "group_force"))
  for (k in c("demog", "stf_t02_demog", "summary", "disposition", "ae_soc_pt", "ae_pt_freq",
              "ae_max_grade", "ae_eair", "lab_abnormal", "shift", "lab_shift", "pk_conc",
              "param_change", "ae_overview")) {
    p <- tfl_template_params(k)$split
    expect_false(is.null(p), info = k)
    expect_identical(p$type, "choice", info = k)
    expect_true(all(c("none", "group_safe", "group_force") %in% p$choices), info = k)
  }
})

test_that("split: the demographics family fixes it", {
  for (k in c("demog", "summary", "disposition", "stf_t02_demog")) {
    a <- switch(k, disposition = list(population = "SAF", by = "TRT01A", categorical = "EOSSTT"),
                stf_t02_demog = list(population = "SAF", by = "TRT01A", continuous = "AGE"),
                sp_dm)
    expect_true(isTRUE(tfl_template_params(k)$split$fixed_value), info = k)
    expect_error(tfl_template_expand(k, c(a, list(split = "group_force"))),
                 "no `split` parameter", fixed = TRUE, info = k)
    expect_error(tfl_template_expand(k, c(a, list(split = tfl_unset()))),
                 "split", info = k)
    # the fixed value itself goes through
    expect_no_error(tfl_template_expand(k, c(a, list(split = "group_safe"))))
  }
})

test_that("split: another template's choice, validated", {
  x <- sp_args("ae_soc_pt", list(population = "SAF", split = "group_safe"))[[1L]]
  expect_identical(x$args, list(max_rows = 22L, split = "group_safe"))
  expect_identical(sp_args("ae_eair", list(population = "SAF", split = "none"))[[1L]]$args$split, "none")
  expect_error(tfl_template_expand("ae_soc_pt", list(population = "SAF", split = "pages")), "split|arg")
  expect_error(tfl_template_expand("ae_soc_pt", list(population = "SAF", split = tfl_unset())), "split")
})

test_that("split on ae_soc_pt changes the breaks of the table", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
  d <- tempfile("sp-study")
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "SP-1")
  run <- function(id, args) {
    f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
    tfl_write_output(tfl_output(id, "table", common = list(population = "SAF"), extends = "defaults",
                                template = list(name = "ae_soc_pt", args = args)), f)
    st <- tfl_read_study_spec(d)
    ard <- suppressMessages(tfl_build_ard(st, output_id = id, dir = d, save = FALSE))
    ro <- tfl_resolve_output(tfl_read_output(f), st)
    pages <- suppressMessages(rtfreporter::plan_apply(
      suppressMessages(tfl_content_run(ro, ard)), "pages"))
    lapply(pages, function(p) as.data.frame(p$data))
  }
  a <- run("T-SP1", list(population = "SAF"))
  a2 <- run("T-SP2", list(population = "SAF", split = "group_force"))
  b <- run("T-SP3", list(population = "SAF", split = "group_safe"))
  # (the default is group_force: written out it is the same pages)
  expect_identical(a, a2)
  # group_safe cuts the pages elsewhere
  expect_false(identical(a, b))
  # a split with no row budget stops (the template's max_rows is 22)
  expect_error(run("T-SP4", list(population = "SAF", split = "none")), "no effect with split")
})

test_that("page_by and page_heading replace param_page and param_heading", {
  a <- list(population = "SAF", data = "ADLB", by = "TRT01A")
  # the default: a page a parameter
  x <- tfl_template_expand("param_change", c(a, list()))
  pg <- Filter(function(it) identical(it$fn, "plan_paginate_rows"), x$content)[[1L]]$args
  expect_identical(pg$page_by, "group1")
  expect_identical(pg$heading, "Parameter: {value}")
  y <- tfl_template_expand("param_change", c(a, list(page_heading = "Test: {value}")))
  pg <- Filter(function(it) identical(it$fn, "plan_paginate_rows"), y$content)[[1L]]$args
  expect_identical(pg$heading, "Test: {value}")
  # !unset: no page a value (the step is not made), the split stays
  z <- tfl_template_expand("param_change", c(a, list(page_by = tfl_unset())))
  pz <- Filter(function(it) identical(it$fn, "plan_paginate_rows"), z$content)[[1L]]$args
  expect_null(pz$page_by)
  expect_null(pz$heading)
  expect_identical(pz$split, "group_safe")
  expect_false("plan_hide" %in% vapply(z$content, `[[`, "", "fn"))
  # shift
  s <- list(population = "SAF")
  e <- tfl_template_expand("shift", s)
  fn <- vapply(e$content, `[[`, "", "fn")
  expect_true(all(c("plan_paginate_rows", "plan_hide") %in% fn))
  expect_identical(e$content[[match("plan_paginate_rows", fn)]]$args$page_by, "group")
  u <- tfl_template_expand("shift", c(s, list(page_by = tfl_unset())))
  expect_false(any(c("plan_paginate_rows", "plan_hide") %in% vapply(u$content, `[[`, "", "fn")))
  # a variable that is not the parameter's stops
  expect_error(tfl_template_expand("shift", c(s, list(page_by = "AVISIT"))), "page_by")
  # the old names are unknown parameters
  for (k in c("param_page", "param_heading")) {
    expect_error(tfl_template_expand("shift", c(s, stats::setNames(list(TRUE), k))),
                 sprintf("has no parameter '%s'", k), info = k)
    expect_error(tfl_template_expand("param_change", c(a, stats::setNames(list(TRUE), k))),
                 sprintf("has no parameter '%s'", k), info = k)
    expect_error(tfl_template_expand("lab_shift", list(population = "SAF", visit = "Week 8",
                                                       params = "ALT",
                                                       param_page = FALSE)),
                 "has no parameter 'param_page'", fixed = TRUE)
  }
  expect_true(all(c("page_by", "page_heading") %in% names(tfl_template_params("lab_shift"))))
  expect_false(any(c("param_page", "param_heading") %in% names(tfl_template_params("lab_shift"))))
})
