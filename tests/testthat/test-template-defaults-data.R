# A template's defaults and the report's data: headings given by name;
# default variables the data does not have

test_that("labels given by name change those names only; a NULL drops one", {
  lab <- function(args) {
    x <- tfl_template_expand("demog", c(list(population = "SAF"), args))
    unlist(Filter(function(it) it$fn == "plan_labels", x$content)[[1L]]$args$variable)
  }
  d <- lab(list())
  r <- lab(list(labels = c(RACE = "Race, n (%) [a]")))
  expect_identical(names(r), names(d))
  expect_identical(r[["RACE"]], "Race, n (%) [a]")
  expect_identical(r[names(r) != "RACE"], d[names(d) != "RACE"])
  # as YAML gives it (a list), `ETHNIC: ~` dropping one
  r2 <- lab(list(labels = list(RACE = "Race [a]", ETHNIC = NULL)))
  expect_false("ETHNIC" %in% names(r2))
  expect_identical(r2[["RACE"]], "Race [a]")
  # the rows of the cells are the ones given (no merge)
  x <- tfl_template_expand("demog", list(population = "SAF", cells = list(n = "{N}")))
  pc <- Filter(function(it) it$fn == "plan_cells", x$content)[[1L]]$args$continuous
  expect_identical(pc, list(n = "{N}"))
  expect_true(tfl_template_params("demog")$labels$merge)
  expect_null(tfl_template_params("demog")$cells$merge)
})

test_that("a heading given by name: A (the call) = B (the items)", {
  skip_if_not_installed("cards")
  saf <- adsl[adsl$SAFFL == "Y", ]
  ard <- cards::bind_ard(cards::ard_tabulate(saf, variables = TRT01A),
                         cards::ard_summary(saf, by = TRT01A, variables = AGE),
                         cards::ard_tabulate(saf, by = TRT01A,
                                             variables = c(AGEGR1, SEX, RACE, ETHNIC)))
  args <- list(population = "SAF", labels = c(AGE = "Age (years) [a]"))
  tab <- function(p) suppressMessages(rtfreporter::plan_apply(p, "table"))
  a <- tab(do.call(tpl_demog, c(list(ard), args)))
  x <- tfl_template_expand("demog", args)
  b <- tab(suppressMessages(tfl_content_run(tfl_output("T-1", "table", content = x$content),
                                            rtfreporter::normalize_ard(ard))))
  expect_identical(b, a)
  expect_true(any(grepl("Age (years) [a]", unlist(lapply(a, as.character)), fixed = TRUE)))
})
