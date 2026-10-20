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

test_that("a template's default variables: optional, any_of(); a report's own: as written", {
  x <- tfl_template_expand("demog", list(population = "SAF"))
  an <- function(x, id) Filter(function(a) identical(a$id, id), x$ard$analyses)[[1L]]
  expect_true(an(x, "CAT")$optional)
  expect_true(an(x, "CONT")$optional)
  y <- tfl_template_expand("demog", list(population = "SAF", categorical = c("SEX", "RACE")))
  expect_null(an(y, "CAT")$optional)
  expect_true(an(y, "CONT")$optional)
  z <- tfl_template_expand("disposition", list(population = "SAF"))
  expect_true(an(z, "DISP")$optional)
})

test_that("an ADSL with no ETHNIC: demog's table without it, said; A = B", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  sl <- adsl
  sl$ETHNIC <- NULL
  haven::write_xpt(tflspec:::.ai_chr(sl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "NO-ETH")
  o <- tfl_output("T-9-1", "table", population = "SAF", label = "Demographics",
                  extends = "defaults", template = list(name = "demog"))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-9-1.yml"))
  st <- tfl_read_study_spec(d)
  code <- tfl_ard_code(st, output_id = "T-9-1", save = FALSE, part = "body")
  expect_match(paste(code, collapse = "\n"),
               "variables = any_of(c(\"AGEGR1\", \"SEX\", \"RACE\", \"ETHNIC\"))", fixed = TRUE)
  expect_message(ard <- tfl_build_ard(st, output_id = "T-9-1", dir = d, save = FALSE),
                 "ETHNIC not in the data; left out")
  ro <- tfl_resolve_output(tfl_read_output(file.path(d, "spec", "outputs", "T-9-1.yml")), st)
  tab <- function(p) {
    t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
    t[] <- lapply(t, as.character)
    t
  }
  b <- tab(suppressMessages(tfl_content_run(ro, ard)))
  expect_false(any(grepl("Ethnic", unlist(b))))
  expect_true(any(grepl("Race", unlist(b))))
  e <- new.env()
  e$ard <- ard
  suppressMessages(eval(parse(text = tfl_content_code(ro, pipe = "|>")), envir = e))
  expect_identical(tab(e$plan), b)
})
