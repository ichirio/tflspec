# The code lists a data's filter narrows (R/codelist_narrow.R): the factor is
# made after the filter, with the levels the filter's simple conditions
# exclude removed (canon 1.42).

test_that("the simple conditions of a filter are read: %in%, ==, !=, !, the value on either side", {
  one <- function(txt, vars = c("PARAMCD", "SEX")) .narrow_conditions(list(str2lang(txt)), vars)
  x <- one("PARAMCD %in% c(\"ALT\", \"AST\")")
  expect_identical(x$include, list(PARAMCD = c("ALT", "AST")))
  expect_identical(one("PARAMCD == \"ALT\"")$include, list(PARAMCD = "ALT"))
  expect_identical(one("\"ALT\" == PARAMCD")$include, list(PARAMCD = "ALT"))
  expect_identical(one("PARAMCD != \"ALT\"")$exclude, list(PARAMCD = "ALT"))
  expect_identical(one("\"ALT\" != PARAMCD")$exclude, list(PARAMCD = "ALT"))
  expect_identical(one("!PARAMCD %in% c(\"A\", \"B\")")$exclude, list(PARAMCD = c("A", "B")))
  expect_identical(one("!(PARAMCD %in% c(\"A\", \"B\"))")$exclude, list(PARAMCD = c("A", "B")))
  expect_identical(one("!(PARAMCD == \"A\")")$exclude, list(PARAMCD = "A"))
  expect_identical(one("(PARAMCD %in% \"A\")")$include, list(PARAMCD = "A"))
  # joined by a top-level & only; the parts on other columns say nothing
  x <- one("PARAMCD %in% c(\"ALT\", \"AST\") & AVISIT == \"Week 2\" & SEX != \"M\"")
  expect_identical(x$include, list(PARAMCD = c("ALT", "AST")))
  expect_identical(x$exclude, list(SEX = "M"))
  expect_length(x$other, 0L)
  # several parts on one column combine: the includes meet, the excludes join
  x <- one(paste("PARAMCD %in% c(\"A\", \"B\", \"C\") & PARAMCD != \"B\" &",
                 "PARAMCD != \"C\" & PARAMCD %in% c(\"A\", \"B\")"))
  expect_identical(x$include, list(PARAMCD = c("A", "B")))
  expect_identical(x$exclude, list(PARAMCD = c("B", "C")))
  # more than one filter: the same
  conds <- list(quote(PARAMCD %in% c("A", "B", "C")), quote(PARAMCD != "A"))
  expect_identical(.narrow_codes(list(PARAMCD = c("A", "B", "C", "D")), conds)$PARAMCD,
                   c("B", "C"))
})

test_that("any other condition that names the column narrows nothing and is reported", {
  one <- function(txt) .narrow_conditions(list(str2lang(txt)), c("PARAMCD", "SEX"))
  for (txt in c("PARAMCD == \"A\" | PARAMCD == \"B\"", "grepl(\"^A\", PARAMCD)",
                "PARAMCD == PARAM", "PARAMCD %in% others", "PARAMCD > \"A\"",
                "(PARAMCD == \"A\") | SEX == \"F\"")) {
    x <- one(txt)
    expect_length(x$include, 0L)
    expect_length(x$exclude, 0L)
    expect_true("PARAMCD" %in% x$other, info = txt)
  }
  # (a column that is not listed is nobody's business)
  expect_length(.narrow_conditions(list(quote(AVISIT == "W2" | AVISIT == "W4")), "PARAMCD")$other, 0L)
  # a good part beside a bad one on the same column: the good one narrows
  x <- one("PARAMCD %in% c(\"A\", \"B\") & grepl(\"A\", PARAMCD)")
  expect_identical(x$include, list(PARAMCD = c("A", "B")))
  expect_identical(x$other, "PARAMCD")
})

test_that("a narrowed list is written in the list's order, from the codes it has", {
  named <- c(ALT = "Alanine", AST = "Aspartate", BILI = "Bilirubin", GLUC = "Glucose")
  plain <- c("ALT", "AST", "BILI", "GLUC")
  # the order is the list's, not the condition's
  expect_identical(.narrow_expr(named, "cl_p", include = c("AST", "ALT")), "cl_p[c(\"ALT\", \"AST\")]")
  expect_identical(.narrow_expr(plain, "cl_p", include = c("AST", "ALT")),
                   "intersect(cl_p, c(\"ALT\", \"AST\"))")
  expect_identical(.narrow_expr(named, "cl_p", exclude = "BILI"),
                   "cl_p[setdiff(names(cl_p), c(\"BILI\"))]")
  expect_identical(.narrow_expr(plain, "cl_p", exclude = "BILI"), "setdiff(cl_p, c(\"BILI\"))")
  # a code the list does not have is not written; nothing left out: no narrowing
  expect_identical(.narrow_expr(named, "cl_p", include = c("ALT", "NOPE")), "cl_p[c(\"ALT\")]")
  expect_null(.narrow_expr(named, "cl_p", exclude = "NOPE"))
  expect_null(.narrow_expr(named, "cl_p", include = plain))
  expect_null(.narrow_expr(named, "cl_p"))
  # an include and an exclude together: the include, less the excluded
  expect_identical(.narrow_expr(named, "cl_p", include = c("ALT", "AST"), exclude = "ALT"),
                   "cl_p[c(\"AST\")]")
})

narrow_spec <- function(where) {
  sp <- exact_spec(list(method = "continuous", dataset = "ADLB", population_id = "SAF",
                        by = "TRTA | PARAMCD", variables = "AVAL", where = where))
  b <- sp$analyses
  b$analysis_id <- "B"
  b$method <- "categorical"
  b$by <- "TRTA"
  b$variables <- "SEX"
  sp$analyses <- rbind(sp$analyses, b)
  sp
}
narrow_cl <- function() {
  data.frame(output_id = "T", variable = c(rep("PARAMCD", 5), "SEX", "SEX"),
             value = c("ALT", "AST", "BILI", "CREAT", "GLUC", "M", "F"),
             label = c("Alanine", "Aspartate", "Bilirubin", "Creatinine", "Glucose", "Male", "Female"),
             order = c(1:5, 1:2))
}
narrow_levels <- function(ard, id) {
  sort(unique(as.character(unlist(ard$group2_level[ard$analysis_id == id]))))
}

test_that("the program writes the narrowed list in set_levels(), and the ARD has those levels only", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  sp <- narrow_spec("PARAMCD %in% c(\"AST\", \"ALT\") & AVISIT == \"Week 2\"")
  cl <- narrow_cl()
  code <- paste(tfl_ard_code(sp, part = "body", codelists = cl), collapse = "\n")
  expect_match(code, "  set_levels(PARAMCD = cl_paramcd[c(\"ALT\", \"AST\")], SEX = cl_sex)", fixed = TRUE)
  # the lists are written whole at the head; the dataset is read with no step of theirs
  expect_match(code, "cl_paramcd <- c(", fixed = TRUE)
  expect_match(code, "  GLUC = \"Glucose\"", fixed = TRUE)
  expect_match(code, "adlb <- readRDS(\"adam/ADLB.rds\") |> order_levels(from = adsl)", fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE, codelists = cl))
  # two parameters, not the list's five: the filter left two
  expect_identical(narrow_levels(ard, "A"), c("ALT", "AST"))
  sx <- ard[ard$analysis_id == "B" & ard$variable == "SEX", ]
  expect_identical(unique(as.character(unlist(sx$variable_level))), c("M", "F"))
  # an exclusion
  spx <- narrow_spec("!PARAMCD %in% c(\"GLUC\", \"BILI\")")
  expect_match(paste(tfl_ard_code(spx, part = "body", codelists = cl), collapse = "\n"),
               "PARAMCD = cl_paramcd[setdiff(names(cl_paramcd), c(\"BILI\", \"GLUC\"))]",
               fixed = TRUE)
  # an include and an exclude together
  spi <- narrow_spec(paste("PARAMCD %in% c(\"ALT\", \"AST\", \"CREAT\", \"BILI\", \"GLUC\") &",
                           "!PARAMCD %in% c(\"GLUC\", \"BILI\")"))
  expect_match(paste(tfl_ard_code(spi, part = "body", codelists = cl), collapse = "
"),
               "cl_paramcd[c(\"ALT\", \"AST\", \"CREAT\")]", fixed = TRUE)
  ardx <- suppressMessages(tfl_build_ard(spi, dir = dir, save = FALSE, codelists = cl))
  expect_identical(narrow_levels(ardx, "A"), c("ALT", "AST", "CREAT"))
})

test_that("a list with no labels is narrowed by intersect() / setdiff()", {
  skip_on_cran()
  skip_if_not_installed("cards")
  sp <- narrow_spec("PARAMCD %in% c(\"AST\", \"ALT\")")
  cl <- narrow_cl()
  cl$label[cl$variable == "PARAMCD"] <- NA
  code <- paste(tfl_ard_code(sp, part = "body", codelists = cl), collapse = "\n")
  expect_match(code, "cl_paramcd <- c(\"ALT\", \"AST\", \"BILI\", \"CREAT\", \"GLUC\")", fixed = TRUE)
  expect_match(code, "set_levels(PARAMCD = intersect(cl_paramcd, c(\"ALT\", \"AST\"))", fixed = TRUE)
  spx <- narrow_spec("PARAMCD != \"GLUC\"")
  expect_match(paste(tfl_ard_code(spx, part = "body", codelists = cl), collapse = "\n"),
               "set_levels(PARAMCD = setdiff(cl_paramcd, c(\"GLUC\"))", fixed = TRUE)
})

test_that("a condition that is not a simple one keeps every level and says so", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  cl <- narrow_cl()
  sp <- narrow_spec("PARAMCD == \"ALT\" | PARAMCD == \"AST\"")
  expect_message(code <- tfl_ard_code(sp, part = "body", codelists = cl),
                 "adlb_saf: the filter on PARAMCD is not a plain one", fixed = TRUE)
  expect_match(paste(code, collapse = "\n"), "set_levels(PARAMCD = cl_paramcd, SEX = cl_sex)", fixed = TRUE)
  spg <- narrow_spec("grepl(\"^A\", PARAMCD)")
  expect_message(tfl_ard_code(spg, part = "body", codelists = cl), "PARAMCD", fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE, codelists = cl))
  # all the list's levels, the ones the filter leaves counted, the rest 0
  expect_length(narrow_levels(ard, "A"), 5L)
  # a condition on a column the lists do not have: silent
  expect_no_message(tfl_ard_code(narrow_spec("AVISIT == \"Week 2\" | AVISIT == \"Week 4\""),
                                 part = "body", codelists = cl))
})

test_that("an analysis data made from a filtered one inherits its narrowing", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  base <- narrow_spec("AVISIT == \"Week 2\"")
  S <- .ard_spec_sheets
  sp <- tfl_ard_spec(list(
    study = base$study, datasets = base$datasets, populations = base$populations,
    analysis_data = exact_sheet(list(
      list(output_id = "T", data_id = "adlb_p", from = "ADLB", population_id = "SAF",
           where = "PARAMCD %in% c(\"ALT\", \"AST\", \"BILI\")"),
      list(output_id = "T", data_id = "adlb_w", from = "adlb_p", where = "PARAMCD != \"BILI\"")),
      S$analysis_data),
    analyses = exact_sheet(list(list(output_id = "T", analysis_id = "A", method = "continuous",
                                     data = "adlb_w", by = "TRTA | PARAMCD", variables = "AVAL")),
                           S$analyses)))
  cl <- narrow_cl()
  code <- paste(tfl_ard_code(sp, part = "body", codelists = cl), collapse = "\n")
  expect_match(code, "set_levels(PARAMCD = cl_paramcd[c(\"ALT\", \"AST\")])", fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE, codelists = cl))
  expect_identical(narrow_levels(ard, "A"), c("ALT", "AST"))
})

test_that("the code lists are on every analysis set, always, and not on the dataset read", {
  skip_on_cran()
  skip_if_not_installed("cards")
  sp <- exact_spec(list(method = "categorical", dataset = "ADSL", population_id = "SAF",
                        by = "ARM", variables = "SEX"))
  cl <- data.frame(output_id = "T", variable = "SEX", value = c("M", "F"),
                   label = c("Male", "Female"), order = c("1", "2"))
  code <- paste(tfl_ard_code(sp, part = "body", codelists = cl), collapse = "\n")
  expect_match(code, "adsl <- readRDS(\"adam/ADSL.rds\") |> order_levels()", fixed = TRUE)
  expect_match(code, "set_levels(SEX = cl_sex)", fixed = TRUE)
  # an analysis set with every record of its dataset has them too
  sp$populations$where <- NA
  code <- paste(tfl_ard_code(sp, part = "body", codelists = cl), collapse = "\n")
  expect_match(code, "set_levels(SEX = cl_sex)", fixed = TRUE)
  expect_false(grepl("order_levels() |>", code, fixed = TRUE))
})

test_that("the table side narrows a list as the program does", {
  sp <- narrow_spec("PARAMCD %in% c(\"AST\", \"ALT\") & AVISIT == \"Week 2\"")
  cl <- narrow_cl()
  ts <- tfl_table_spec(list(
    tables = data.frame(output_id = "T", cols = "TRTA", rows = "group = variable"),
    codelists = cl))
  lists <- .ard_spec_read_lists(.ard_spec_scope(ts, "T"), sp)
  expect_identical(lists$PARAMCD, c(ALT = "Alanine", AST = "Aspartate"))
  expect_identical(lists$SEX, c(M = "Male", F = "Female"))
  # without the ARD definition: every listed variable, whole
  expect_length(.ard_spec_read_lists(.ard_spec_scope(ts, "T"))$PARAMCD, 5L)
  # the labels of the values the report's analyses read, in the plan
  vl <- .ard_spec_value_labels(.ard_spec_scope(ts, "T"), sp)
  expect_identical(vl$PARAMCD, c(ALT = "Alanine", AST = "Aspartate"))
})

test_that("a code-listed demog table: the ARD holds F, the table shows Female (plan_labels)", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
  saf <- as.data.frame(tflspec::adsl[tflspec::adsl$SAFFL == "Y", ])
  saf$SEX <- as.character(saf$SEX)
  ard <- cards::bind_ard(
    cards::ard_tabulate(saf, variables = TRT01A),
    cards::ard_summary(saf, by = TRT01A, variables = AGE),
    cards::ard_tabulate(saf, by = TRT01A, variables = SEX))
  expect_true("F" %in% unlist(ard$variable_level))
  expect_false("Female" %in% unlist(ard$variable_level))
  cl <- list(SEX = c(F = "Female", M = "Male"))
  plan <- suppressMessages(tfl_tpl(ard, "demog", population = "SAF", by = "TRT01A",
                                   continuous = "AGE", categorical = "SEX",
                                   count_cell = "{n} ({p})", codelists = cl))
  tb <- suppressMessages(rtfreporter::plan_apply(plan, "table"))
  lab <- as.character(tb$label)
  expect_true(all(c("Female", "Male") %in% lab))
  expect_false(any(c("F", "M") %in% lab))
  # the same from the report's code lists as the YAML holds them, and the items
  rows <- list(SEX = list(list(value = "F", label = "Female"), list(value = "M", label = "Male")))
  x <- tfl_template_expand("demog", list(population = "SAF", by = "TRT01A", continuous = "AGE",
                                         categorical = "SEX"), codelists = rows)
  pl <- Filter(function(it) it$fn == "plan_labels", x$content)
  expect_length(pl, 1L)
  expect_identical(pl[[1L]]$args$SEX, c(F = "Female", M = "Male"))
  # the report's own text for a code wins; the rest of the list is added
  own <- tfl_template_expand("demog", list(population = "SAF", by = "TRT01A", continuous = "AGE",
                                           categorical = "SEX", labels = c(SEX = "Sex")),
                             codelists = rows)
  pl <- Filter(function(it) it$fn == "plan_labels", own$content)
  expect_length(pl, 1L)
  expect_identical(unname(pl[[1L]]$args$variable[["SEX"]]), "Sex")
  expect_identical(pl[[1L]]$args$SEX, c(F = "Female", M = "Male"))
})

test_that("a variable's one unnamed heading text is kept, keyed by its name, beside the code labels", {
  content <- list(tfl_item("plan_labels", list(SEX = "Sex")))
  out <- tflspec:::.tpl_labels_items(content, list(SEX = c(F = "Female", M = "Male")))
  expect_identical(out[[1L]]$args$SEX, c(SEX = "Sex", F = "Female", M = "Male"))
})
