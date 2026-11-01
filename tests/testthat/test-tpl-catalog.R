# The templates' catalog: other collections' items, each an alias of a template

test_that("the catalog: every STF item once, aliases unique, templates registered", {
  cat <- tflspec:::.tpl_catalog()
  expect_true(all(cat$source %in% tflspec:::.tpl_sources))
  expect_true(all(cat$status %in% c("same", "close", "missing", "not_output")))
  al <- cat$alias[!is.na(cat$alias)]
  expect_false(anyDuplicated(al) > 0L)
  expect_true(all(grepl("^[a-z0-9_]+$", al)))
  tpl <- cat$template[!is.na(cat$template)]
  expect_true(all(tpl %in% tfl_templates()$name))
  # an item a template writes has one; one none writes yet has none
  expect_true(all(!is.na(cat$template[cat$status %in% c("same", "close")])))
  expect_true(all(is.na(cat$template[cat$status %in% c("missing", "not_output")])))
  stf <- tfl_templates(source = "FDA STF IG v2")
  expect_identical(sort(stf$item), sort(c(sprintf("T%02d", 1:60), sprintf("F%02d", 1:22))))
  expect_named(stf, c("source", "item", "title", "alias", "template", "status", "kind", "note"))
})

test_that("an alias runs its template as itself", {
  a <- list(population = "SAF")
  # the same items; each says the name it was written with
  ea <- tfl_template_expand("stf_t14_ae_soc_pt", a)
  et <- tfl_template_expand("ae_soc_pt", a)
  expect_identical(unique(vapply(ea$content, `[[`, "", "from")), "template:stf_t14_ae_soc_pt")
  drop_from <- function(e) {
    e$content <- lapply(e$content, function(it) {
      it$from <- NULL
      it
    })
    e
  }
  expect_identical(drop_from(ea), drop_from(et))
  expect_identical(tfl_template_params("stf_t14_ae_soc_pt"), tfl_template_params("ae_soc_pt"))
  expect_identical(tfl_tpl_args("stf_t14_ae_soc_pt"), tfl_tpl_args("ae_soc_pt"))
  # an item no template writes yet: says so
  miss <- tfl_templates(source = "FDA STF IG v2")
  miss <- miss$alias[miss$status == "missing"][1L]
  expect_error(tfl_template_expand(miss, a), "no template writes it yet")
  expect_error(tfl_template_expand("no_such_name", a), "no template")
})

test_that("tfl_templates(): the templates with their aliases; a collection's items", {
  tt <- tfl_templates()
  expect_true("aliases" %in% names(tt))
  expect_match(tt$aliases[tt$name == "ae_soc_pt"], "stf_t14_ae_soc_pt", fixed = TRUE)
  own <- tfl_templates(source = "tflspec")
  expect_identical(sort(own$template), sort(tt$name))
  expect_error(tfl_templates(source = "FDA"), "one of")
})

test_that("the AI's list names a template once, its aliases on a line; an alias answers", {
  l <- tflspec:::.ai_templates_list("table")
  expect_identical(sum(grepl("^- `ae_soc_pt`", l)), 1L)
  expect_false(any(grepl("^- `stf_t14_ae_soc_pt`", l)))
  expect_true(any(grepl("aliases (the same template): `stf_t14_ae_soc_pt`", l, fixed = TRUE)))
  p <- tflspec:::.ai_check_template(list(name = "stf_t14_ae_soc_pt",
                                         args = list(population = "SAF")), "T-1")
  expect_false(any(p$severity == "error"))
})

test_that("a template's help page lists its aliases, and says FDA endorses none", {
  rd <- paste(tflspec:::.tpl_rd("ae_soc_pt"), collapse = "\n")
  expect_match(rd, "@section Aliases:", fixed = TRUE)
  expect_match(rd, "stf_t14_ae_soc_pt", fixed = TRUE)
  expect_match(rd, "not endorsed or recommended by FDA", fixed = TRUE)
  expect_false(grepl("@section Aliases:", paste(tflspec:::.tpl_rd("ancova"), collapse = "\n"),
                     fixed = TRUE))
})

test_that("every collection is cited: its publication, version, date, URL and terms", {
  src <- tflspec:::.tpl_sources_table()
  cat <- tflspec:::.tpl_catalog()
  expect_true(all(unique(cat$source) %in% src$source))
  expect_false(any(is.na(src$document) | is.na(src$url) | is.na(src$accessed) | is.na(src$terms)))
  expect_true(all(grepl("^https://", src$url)))
  rd <- paste(tflspec:::.tpl_rd("ae_soc_pt"), collapse = "\n")
  expect_match(rd, "Sources:", fixed = TRUE)
  expect_match(rd, "not endorsed or recommended by FDA, PhUSE or CDISC", fixed = TRUE)
  expect_match(rd, "fda.gov/media/187065", fixed = TRUE)
  expect_match(paste(tflspec:::.tpl_rd_sources(), collapse = "\n"), "@section Sources:", fixed = TRUE)
})
