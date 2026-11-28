# A list<...> argument a report gives is a sequence of values: never empty,
# no "" or NA entry, not a map where only sequences are declared.  An empty
# field is "not given" (the template's default); "(none)" is !unset.
# Fictional study settings only.

# a tflspec_hierarchy report on ADAE; `...` more arguments (heading = ...)
lg_hier <- function(...) {
  list(id = "X", kind = "table", label = "Records by hierarchy", template = c(list(
    name = "tflspec_v1/tflspec_hierarchy", from = "ADAE", by = "TRT01A",
    hierarchy = c("AEBODSYS", "AEDECOD")), list(...)))
}

lg_fns <- function(o) vapply(o$content, function(it) .schema_fn_name(it$fn), "")

lg_keys <- function(o) {
  k <- which(lg_fns(o) == "plan_sort")
  if (!length(k)) return(NULL)
  as.character(unlist(o$content[[k[[1L]]]]$args$keys))
}

lg_msg <- "Leave heading out for the template's default, or write !unset for none."

test_that("tflspec_hierarchy: heading given empty, with an empty entry or as a map stops", {
  bad <- list(
    blank = list(value = "", why = "an empty entry"),
    blank_in_list = list(value = list(""), why = "an empty entry"),
    blank_line = list(value = c("Body system", ""), why = "an empty entry"),
    empty_list = list(value = list(), why = "an empty list"),
    empty_chr = list(value = character(0), why = "an empty list"),
    na_entry = list(value = c("Body system", NA), why = "an NA entry"),
    map = list(value = list(a = ""), why = "a map"),
    map_of_lines = list(value = list(`- Body system` = "", `- Term` = ""), why = "a map"))
  for (nm in names(bad)) {
    b <- bad[[nm]]
    err <- tryCatch(tfl_template_resolve(lg_hier(heading = b$value)), error = conditionMessage)
    expect_true(is.character(err), info = nm)
    expect_match(err, "tflspec_v1/tflspec_hierarchy's heading is a list (list<string>); the report gives",
                 fixed = TRUE, info = nm)
    expect_match(err, b$why, fixed = TRUE, info = nm)
    expect_match(err, lg_msg, fixed = TRUE, info = nm)
  }
})

test_that("tflspec_hierarchy: the map a rows field writes, read from YAML, stops", {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(c("id: X", "kind: table", "label: Records by hierarchy",
               "template:", "  name: tflspec_v1/tflspec_hierarchy", "  from: ADAE",
               "  analysis:", "    by: [TRT01A]", "    hierarchy: [AEBODSYS, AEDECOD]",
               "  content:", "    heading: {'- Body system': '', '- Term': ''}"), f)
  expect_error(tfl_template_resolve(.series_read_yaml(f)),
               "the report gives a map (name: value)", fixed = TRUE)
})

test_that("tflspec_hierarchy: heading left out is the default; !unset is none; plan_sort keys as before", {
  keys <- c(".overall", "-n@group1", "group1", ".depth", "-n", "label")
  o <- tfl_template_resolve(lg_hier())
  expect_null(attr(o, "args")$heading)
  expect_identical(attr(o, "origins")[["heading"]], "template")
  expect_identical(lg_keys(o), keys)
  u <- tfl_template_resolve(lg_hier(heading = tfl_unset()))
  expect_null(attr(u, "args")$heading)
  expect_identical(lg_keys(u), keys)
  # a sequence of lines, as a list or a character vector
  for (v in list(list("Body system", "  Term"), c("Body system", "  Term"))) {
    g <- tfl_template_resolve(lg_hier(heading = v))
    expect_identical(as.character(unlist(attr(g, "args")$heading)), c("Body system", "  Term"))
    expect_identical(attr(g, "origins")[["heading"]], "report")
    expect_identical(lg_keys(g), keys)
  }
})

test_that("tflspec_hierarchy: a required list argument (hierarchy, by) is checked too", {
  hier <- function(by, hierarchy) {
    list(id = "X", kind = "table", template = list(
      name = "tflspec_v1/tflspec_hierarchy", from = "ADAE", by = by, hierarchy = hierarchy))
  }
  expect_error(tfl_template_resolve(hier("TRT01A", list(""))),
               "tflspec_v1/tflspec_hierarchy's hierarchy is a list (list<column>); the report gives an empty entry",
               fixed = TRUE)
  expect_error(tfl_template_resolve(hier(c(arm = "TRT01A"), "AEDECOD")),
               "tflspec_v1/tflspec_hierarchy's by is a list (list<column>); the report gives a map",
               fixed = TRUE)
})

test_that("km_cards: x_breaks as a map, empty or with an NA entry stops", {
  kmc <- function(x_breaks) {
    list(id = "F-9", kind = "figure", label = "Kaplan-Meier", template = list(
      name = "tflspec_v1/km_cards", data = list(by = "TRT01P", param = "OS"),
      content = list(x_breaks = x_breaks)))
  }
  kst <- list(settings = list())
  # "0\n30" read as "name | text" rows: {"0": "", "30": ""}
  expect_error(tfl_template_resolve(kmc(list(`0` = "", `30` = "")), study = kst),
               "tflspec_v1/km_cards's x_breaks is a list (list<number>); the report gives a map",
               fixed = TRUE)
  expect_error(tfl_template_resolve(kmc(list()), study = kst), "an empty list", fixed = TRUE)
  expect_error(tfl_template_resolve(kmc(list(0, NA)), study = kst), "an NA entry", fixed = TRUE)
  expect_error(tfl_template_resolve(kmc(""), study = kst),
               "Leave x_breaks out for the template's default, or write !unset for none.", fixed = TRUE)
})

test_that(".tr_list_value(): only a report's value of a list type is checked", {
  chk <- function(val, type, origin = "report") {
    .tr_list_value(val, type, "p", origin, "s/t", "f()")
  }
  # not a list type, or not the report's value: not checked
  expect_silent(chk("", "string"))
  expect_silent(chk(list(), "list<column>", origin = "template"))
  expect_silent(chk(NULL, "list<string>"))
  expect_silent(chk(tfl_r("c(1, 2)"), "list<number>|r"))
  # a list type beside a map type takes a map
  expect_silent(chk(list(a = "x"), "list<string>|map<string, string>"))
  expect_error(chk(list(a = ""), "list<string>|map<string, string>"), "an empty entry", fixed = TRUE)
  # a list beside a one-value type: one value is fine, "" is not
  expect_silent(chk("AGE", "list<column>|column"))
  expect_error(chk("", "list<column>|column"), "an empty entry", fixed = TRUE)
  # a list of maps: each entry a map is fine
  expect_silent(chk(list(list(a = "x"), list(a = "y")), "list<map<string,string>>"))
})
