# A list<...> argument a report gives is a sequence of values: never empty,
# no "" or NA entry, not a map where only sequences are declared.  An empty
# field is "not given" (the template's default); "(none)" is !unset.
# Fictional study settings only.

lg_study <- function() list(settings = list(rounding = "sas", output_path = "output/tfl"))

# an ae_soc_pt report; `content` the report's content: arguments
lg_ae <- function(content = list()) {
  list(id = "T-9", kind = "table", label = "TEAEs by SOC / PT",
       template = c(list(name = "tflspec_v1/ae_soc_pt", analysis = list(by = "TRT01A")),
                    if (length(content)) list(content = content)))
}

lg_fns <- function(o) vapply(o$content, function(it) .schema_fn_name(it$fn), "")

lg_keys <- function(o) {
  k <- which(lg_fns(o) == "plan_sort")
  if (!length(k)) return(NULL)
  as.character(unlist(o$content[[k[[1L]]]]$args$keys))
}

lg_msg <- "Leave sort out for the template's default, or write !unset for none."

test_that("ae_soc_pt: sort given empty, with an empty entry or as a map stops", {
  bad <- list(
    blank = list(value = "", why = "an empty entry"),
    blank_in_list = list(value = list(""), why = "an empty entry"),
    empty_list = list(value = list(), why = "an empty list"),
    empty_chr = list(value = character(0), why = "an empty list"),
    na_entry = list(value = c("label", NA), why = "an NA entry"),
    map = list(value = list(a = ""), why = "a map"),
    map_of_lines = list(value = list(`- .overall` = "", `- group1` = "", `- label` = ""),
                        why = "a map"))
  for (nm in names(bad)) {
    b <- bad[[nm]]
    content <- list(sort = b$value)
    expect_error(tfl_template_resolve(lg_ae(content), study = lg_study()),
                 "tflspec_v1/ae_soc_pt's sort is a list (list<string>); the report gives",
                 fixed = TRUE, info = nm)
    expect_error(tfl_template_resolve(lg_ae(content), study = lg_study()), b$why,
                 fixed = TRUE, info = nm)
    expect_error(tfl_template_resolve(lg_ae(content), study = lg_study()), lg_msg,
                 fixed = TRUE, info = nm)
  }
})

test_that("ae_soc_pt: the map the add-report dialog wrote, read from YAML, stops", {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(c("id: T-9", "kind: table", "label: TEAEs by SOC / PT",
               "template:", "  name: tflspec_v1/ae_soc_pt", "  analysis:", "    by: TRT01A",
               "  content:",
               "    sort: {'- .overall': '', '- group1': '', '- .depth': '', '- -n': '', '- label': ''}"),
             f)
  expect_error(tfl_template_resolve(.series_read_yaml(f), study = lg_study()),
               "the report gives a map (name: value)", fixed = TRUE)
})

test_that("ae_soc_pt: sort left out is the template's default; !unset is none; keys given are used", {
  o <- tfl_template_resolve(lg_ae(), study = lg_study())
  expect_identical(lg_keys(o), c(".overall", "group1", ".depth", "-n", "label"))
  expect_identical(attr(o, "origins")[["sort"]], "template")
  # !unset: no keys, so no plan_sort (as before this guard)
  u <- tfl_template_resolve(lg_ae(list(sort = tfl_unset())), study = lg_study())
  expect_null(lg_keys(u))
  # a sequence of keys, as a list or a character vector
  for (v in list(list("label", "-n"), c("label", "-n"))) {
    g <- tfl_template_resolve(lg_ae(list(sort = v)), study = lg_study())
    expect_identical(lg_keys(g), c("label", "-n"))
  }
})

test_that("other list arguments: tflspec_hierarchy heading, km_cards x_breaks", {
  hier <- function(heading) {
    list(id = "X", kind = "table", template = list(
      name = "tflspec_v1/tflspec_hierarchy", from = "ADAE", by = "TRT01A",
      hierarchy = c("AEBODSYS", "AEDECOD"), heading = heading))
  }
  expect_error(tfl_template_resolve(hier(list(""))),
               "tflspec_v1/tflspec_hierarchy's heading is a list (list<string>); the report gives an empty entry",
               fixed = TRUE)
  expect_error(tfl_template_resolve(hier(c(`Body system` = ""))),
               "Leave heading out for the template's default, or write !unset for none.", fixed = TRUE)
  kmc <- function(x_breaks) {
    list(id = "F-9", kind = "figure", label = "Kaplan-Meier", template = list(
      name = "tflspec_v1/km_cards", data = list(by = "TRT01P", param = "OS"),
      content = list(x_breaks = x_breaks)))
  }
  kst <- list(settings = list(rounding = "sas"))
  # "0\n30" read as "name | text" rows: {"0": "", "30": ""}
  expect_error(tfl_template_resolve(kmc(list(`0` = "", `30` = "")), study = kst),
               "tflspec_v1/km_cards's x_breaks is a list (list<number>); the report gives a map",
               fixed = TRUE)
  expect_error(tfl_template_resolve(kmc(list()), study = kst), "an empty list", fixed = TRUE)
  expect_error(tfl_template_resolve(kmc(list(0, NA)), study = kst), "an NA entry", fixed = TRUE)
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
