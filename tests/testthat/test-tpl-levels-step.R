# The lists a template used to say with plan_levels() are the data step's
# now: shift's `levels` of the post-baseline category and orr's `by_levels`
# are code lists of the template, which the report's own list for the same
# variable wins over.  ae_max_grade's `grade_levels` order the grades on the
# table through the plan (cards' hierarchical ARD sorts them alphabetically).

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

test_that("shift: the post-baseline categories are a code list of the template, not a plan_levels()", {
  x <- tfl_template_expand("shift", list(population = "SAF", missing_label = "Missing"))
  expect_identical(vapply(x$codelists$ANRIND, `[[`, "", "value"), c("LOW", "NORMAL", "HIGH"))
  # (the baseline's Missing and Total rows are made by the analysis data's
  # code, so its order stays a plan_levels())
  expect_null(x$codelists$BNRIND)
  a <- Filter(function(it) it$fn == "plan_levels", x$content)[[1L]]$args
  expect_identical(names(a), c("BNRIND", ".drop_empty"))
  expect_identical(a$BNRIND, c("LOW", "NORMAL", "HIGH", "Missing", "Total"))
  expect_identical(a$.drop_empty, "ANRIND")
  # a report's list for the column: the template adds nothing, and does not
  # hide the list's empty levels
  cl <- list(list(value = "HIGH", label = "High"), list(value = "NORMAL", label = "Normal"),
             list(value = "LOW", label = "Low"))
  y <- tfl_template_expand("shift", list(population = "SAF"), codelists = list(ANRIND = cl))
  expect_null(y$codelists$ANRIND)
  expect_identical(names(Filter(function(it) it$fn == "plan_levels", y$content)[[1L]]$args),
                   "BNRIND")
  # lab_shift is the same
  z <- tfl_template_expand("lab_shift", list(population = "SAF", visit = "WEEK 8", params = "ALT"))
  expect_identical(vapply(z$codelists$ANRIND, `[[`, "", "value"), c("LOW", "NORMAL", "HIGH"))
})

test_that("shift: the program puts the categories in the data step; the report's list wins", {
  lvs_skip()
  args <- list(population = "SAF", where = 'PARAMCD %in% c("ALT", "HGB")', param_page = FALSE)
  r <- lvs_report(lvs_study(withr::local_tempdir()), "shift", args)
  expect_true(any(grepl("cl_anrind <- c(LOW = \"LOW\", NORMAL = \"NORMAL\", HIGH = \"HIGH\")",
                        r$code, fixed = TRUE)))
  expect_true(any(grepl("ANRIND = cl_anrind", r$code, fixed = TRUE)))
  # the plan says no order of the post-baseline categories
  plan_code <- paste(tfl_content_code(r$ro, pipe = "|>"), collapse = "\n")
  expect_false(grepl(".label = ", plan_code, fixed = TRUE))
  expect_identical(lvs_seen(r$ard, "ANRIND"), c("LOW", "NORMAL", "HIGH"))
  # the report's own list wins, labels and all
  cl <- list(list(value = "HIGH", label = "High"), list(value = "NORMAL", label = "Normal"),
             list(value = "LOW", label = "Low"))
  r2 <- lvs_report(lvs_study(withr::local_tempdir()), "shift", args, codelists = list(ANRIND = cl))
  expect_identical(vapply(r2$ro$codelists$ANRIND, `[[`, "", "value"), c("HIGH", "NORMAL", "LOW"))
  expect_false(any(grepl("cl_anrind <- c(LOW", r2$code, fixed = TRUE)))
  expect_identical(lvs_seen(r2$ard, "ANRIND"), c("High", "Normal", "Low"))
  cols <- grep("____", names(lvs_table(r2)), value = TRUE)
  expect_identical(unique(sub("^.*____", "", cols)), c("High", "Normal", "Low"))
})

test_that("shift: a category the data has no record of is not a column (the order never added one)", {
  lvs_skip()
  args <- list(population = "SAF", where = 'PARAMCD %in% c("ALT", "HGB")', param_page = FALSE,
               levels = c("LOW", "NORMAL", "HIGH", "CRITICAL"))
  r <- lvs_report(lvs_study(withr::local_tempdir()), "shift", args)
  expect_true("CRITICAL" %in% lvs_seen(r$ard, "ANRIND"))
  nm <- names(lvs_table(r))
  expect_false(any(grepl("CRITICAL", nm, fixed = TRUE)))
  expect_true(any(grepl("____HIGH$", nm)))
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
