# orr's `by_levels` are the arms' code list of the template (the data step
# puts the arms in that order), which the report's own list for the column
# wins over.  ae_max_grade orders its grades on the table through the plan
# (cards' hierarchical ARD sorts them alphabetically).  shift's `levels` stay
# a plan_levels(): a code list would stop the program on a value it lacks.

lvs_study <- function(root, data = c("adsl", "adlb")) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in data) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "LV-1")
  d
}

lvs_report <- function(d, name, args, population = "SAF", codelists = list()) {
  f <- file.path(d, "spec", "outputs", "T-LV.yml")
  tfl_write_output(tfl_output("T-LV", "table", population = population, extends = "defaults",
                              codelists = codelists, template = list(name = name, args = args)), f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-LV", dir = d, save = FALSE))
  list(st = st, ard = ard, ro = tfl_resolve_output(tfl_read_output(f), st),
       code = tfl_ard_code(st, output_id = "T-LV"))
}

# the first value of a list cell
lvs_chr <- function(z) as.character(unlist(z))[1L]

# the levels of a variable in the order the ARD has them
lvs_seen <- function(ard, var) {
  v <- ard[ard$variable == var & ard$stat_name == "n", ]
  unique(vapply(v$variable_level, lvs_chr, ""))
}

lvs_table <- function(r) {
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(
    suppressMessages(tfl_content_run(r$ro, r$ard)), "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  t
}

lvs_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtfreporter")
}

test_that("shift: the template gives the post-baseline categories as .label in plan_levels, no code list", {
  x <- tfl_template_expand("shift", list(population = "SAF", missing_label = "Missing"))
  expect_null(x$codelists$ANRIND)
  a <- Filter(function(it) it$fn == "plan_levels", x$content)[[1L]]$args
  expect_identical(a$.label, c("LOW", "NORMAL", "HIGH"))
  expect_false(".drop_empty" %in% names(a))
})

test_that("orr: by_levels are the arms' code list of the template; the report's list wins", {
  x <- tfl_template_expand("orr", list(by_levels = c("B", "A")))
  expect_identical(vapply(x$codelists$TRT01P, `[[`, "", "value"), c("B", "A"))
  expect_false("TRT01P" %in% names(Filter(function(it) it$fn == "plan_levels", x$content)[[1L]]$args))
  cl <- list(list(value = "A", label = "A"), list(value = "B", label = "B"))
  y <- tfl_template_expand("orr", list(by_levels = c("B", "A")), codelists = list(TRT01P = cl))
  expect_null(y$codelists)
  # none given: nothing added
  expect_null(tfl_template_expand("orr", list())$codelists)
  lvs_skip()
  skip_if_not_installed("cardx")
  arms <- rev(sort(as.character(unique(tflspec::adsl$TRT01P))))
  r <- lvs_report(lvs_study(withr::local_tempdir(), c("adsl", "adrs")), "orr",
                  list(population = "ITT", by_levels = arms), population = "ITT")
  expect_true(any(grepl("cl_trt01p <- c(", r$code, fixed = TRUE)))
  nm <- names(lvs_table(r))
  at <- vapply(arms, function(a) grep(a, nm, fixed = TRUE)[1L], 0L)
  expect_false(anyNA(at))
  expect_false(is.unsorted(at))
})

test_that("ae_max_grade: the grades are shown in grade_levels' order (the data step's factor does not reach cards' hierarchical rows)", {
  x <- tfl_template_expand("ae_max_grade", list(population = "SAF", grade_levels = c("MILD", "SEVERE")))
  expect_null(x$codelists)
  a <- Filter(function(it) it$fn == "plan_levels", x$content)[[1L]]$args
  expect_identical(a$label, c("MILD", "SEVERE"))
  keys <- Filter(function(it) it$fn == "plan_sort", x$content)[[1L]]$args$keys
  expect_identical(keys[length(keys)], "levels(label)")
  # as columns: the any-grade column stays first
  y <- tfl_template_expand("ae_max_grade", list(population = "SAF", grade_levels = c("MILD", "SEVERE"),
                                                grade_in = "cols"))
  expect_identical(Filter(function(it) it$fn == "plan_levels", y$content)[[1L]]$args$AESEV,
                   c("Any grade", "MILD", "SEVERE"))
  # the report's code list gives the labels
  cl <- list(list(value = "SEVERE", label = "Severe"), list(value = "MILD", label = "Mild"))
  z <- tfl_template_expand("ae_max_grade", list(population = "SAF"), codelists = list(AESEV = cl))
  expect_identical(Filter(function(it) it$fn == "plan_levels", z$content)[[1L]]$args$label,
                   c("Severe", "Mild"))
  # no list: the plan says no order
  n <- tfl_template_expand("ae_max_grade", list(population = "SAF"))
  expect_identical(names(Filter(function(it) it$fn == "plan_levels", n$content)[[1L]]$args), "AEBODSYS")
})

test_that("ae_max_grade: the maximum by grade_levels, the grades shown in that order", {
  lvs_skip()
  # the list's last grade is the highest: here SEVERE is the lowest
  lv <- c("SEVERE", "MODERATE", "MILD")
  r <- lvs_report(lvs_study(withr::local_tempdir(), c("adsl", "adae")), "ae_max_grade",
                  list(population = "SAF", grade_levels = lv))
  t <- lvs_table(r)
  # (the labels of the Any TEAE block's rows: the overall row, then the grades)
  lab <- t$label[t$group1 == "Any TEAE"]
  expect_identical(lab[!is.na(lab)], lv)
  # the highest record of a subject is the one latest in the list
  ae <- tflspec::adae[tflspec::adae$SAFFL == "Y" & tflspec::adae$TRTEMFL == "Y", ]
  top <- tapply(match(ae$AESEV, lv), ae$USUBJID, max)
  v <- r$ard[r$ard$variable == "AESEV" & r$ard$stat_name == "n", ]
  on_all <- vapply(v$group2_level, function(z) identical(lvs_chr(z), "Any TEAE"), NA)
  got <- vapply(lv, function(g) {
    k <- on_all & vapply(v$variable_level, function(z) identical(lvs_chr(z), g), NA)
    sum(unlist(v$stat[k]))
  }, 0)
  expect_identical(unname(as.integer(got)), as.integer(table(factor(lv[top], levels = lv))))
})
