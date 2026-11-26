# The references to the study's code lists (A7 S2): tfl_study_refs() lists
# what a report names itself -- `{list: X}`, the codes its `codes:` picks,
# `study$codelists$X` in its own R text -- and tfl_check_spec() gives an
# error row for each name or code the study has not got, through one check
# that takes the study and the reports in memory (tfl_check_study_refs()); the
# xlsx import runs it before it writes.  A template's default reference is
# not the report's: left out when the report is resolved, not reported.
# A study made on the package's fictional adsl, with SEX and AGEGR1 lists,
# and the fixture's T-14-1-1.

srf_study <- function(codelists, labels = NULL, env = parent.frame()) {
  skip_if_not_installed("haven")
  root <- withr::local_tempdir(.local_envir = env)
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "SRF-1"))
  cat(c("codelists:", "  SEX: [{value: F, label: Female}, {value: M, label: Male}]",
        "  AGEGR1: [{value: \"<65\", label: \"<65 years\"}, {value: \">=65\", label: \">=65 years\"}]", ""),
      sep = "\n", file = file.path(d, "spec", "defaults.yml"), append = TRUE)
  dir.create(file.path(d, "spec", "outputs"), showWarnings = FALSE)
  file.copy(system.file("extdata", "spec-yaml", "outputs", "T-14-1-1.yml", package = "tflspec"),
            file.path(d, "spec", "outputs"))
  srf_put(d, codelists, labels)
  d
}
# the report's `codelists:` (and its plan_labels line) as given
srf_put <- function(d, codelists, labels = NULL) {
  f <- file.path(d, "spec", "outputs", "T-14-1-1.yml")
  y <- readLines(f)
  i <- grep("^codelists:", y)
  j <- grep("^content:", y)
  y <- c(y[seq_len(i - 1L)], "codelists:", paste0("  ", codelists), y[j:length(y)])
  if (!is.null(labels)) y[grep("^  - plan_labels:", y)] <- paste0("  - plan_labels: ", labels)
  writeLines(y, f)
}
srf_trt <- c("TRT01A:", "  - {value: Drug X, label: Drug X 200 mg}",
             "  - {value: Control, label: Control 100 mg}")
srf_refs <- function(p) p[p$part == "codelists", c("output_id", "part", "item", "level", "message")]

test_that("tfl_study_refs() lists a report's own references: lists, picked codes, R text", {
  o <- tfl_output("T-1", "table",
    codelists = list(SEX = list(list = "SEX"), AGEGR1 = list(list = "AGEGR1", codes = list(">=65", "<65")),
                     TRT01A = list(list(value = "A", label = "A"))),
    content = list(tfl_item("plan_labels", list(
      RACE = tfl_r("c(RACE = \"Race\", study$codelists$RACE)"),
      ETHNIC = tfl_r("study$codelists[[\"ETHNIC\"]]"),
      X = tfl_r("study$codelists[[v]]")))))
  r <- tfl_study_refs(o)
  expect_identical(names(r), c("part", "item", "list", "code"))
  expect_identical(r$part, c(rep("codelists", 4L), "content", "content"))
  expect_identical(r$item, c("SEX", "AGEGR1", "AGEGR1", "AGEGR1", "plan_labels / RACE", "plan_labels / ETHNIC"))
  expect_identical(r$list, c("SEX", "AGEGR1", "AGEGR1", "AGEGR1", "RACE", "ETHNIC"))
  expect_identical(r$code, c(NA, NA, ">=65", "<65", NA, NA))
  expect_identical(nrow(tfl_study_refs(tfl_output("T-2", "table"))), 0L)
  expect_error(tfl_study_refs(list()), "a report as read", fixed = TRUE)
})

test_that("an unknown {list:} name is an error row with the report, the variable and the name", {
  d <- srf_study(c(srf_trt, "SEX: {list: GENDER}"))
  p <- tfl_check_spec(d, adam = FALSE)
  r <- srf_refs(p)
  expect_identical(nrow(r), 1L)
  expect_identical(r$output_id, "T-14-1-1")
  expect_identical(r$item, "SEX")
  expect_identical(r$level, "error")
  expect_match(r$message, "the code list of SEX refers to the study's list GENDER, which the study has not got",
               fixed = TRUE)
  expect_match(r$message, "has SEX, AGEGR1)", fixed = TRUE)
  expect_identical(tfl_check_reports(p)$level, "ERROR")
  # (its other checks would stop on the same: not run, so no second row of it)
  expect_false(any(grepl("GENDER", p$message[p$part != "codelists"], fixed = TRUE)))
})

test_that("a codes: pick not in the study's list is an error row naming the code", {
  d <- srf_study(c(srf_trt, "SEX: {list: SEX, codes: [F, U]}"))
  r <- srf_refs(tfl_check_spec(d, adam = FALSE))
  expect_identical(nrow(r), 1L)
  expect_identical(r$item, "SEX")
  expect_match(r$message, "the code list of SEX picks \"U\" from the study's list SEX, which has not got it (its codes: F, M)",
               fixed = TRUE)
  # (the codes all in the list: no row)
  srf_put(d, c(srf_trt, "SEX: {list: SEX, codes: [M, F]}"))
  expect_identical(nrow(srf_refs(tfl_check_spec(d, adam = FALSE))), 0L)
})

test_that("an unknown study$codelists$X in the report's own R is an error row, not dropped in silence", {
  d <- srf_study(srf_trt, labels = "{SEX: !r study$codelists$SEX, RACE: !r study$codelists$RACE}")
  r <- srf_refs(tfl_check_spec(d, adam = FALSE))
  expect_identical(nrow(r), 1L)
  expect_identical(r$item, "content: plan_labels / RACE")
  expect_match(r$message, "`study$codelists$RACE` names the study's list RACE, which the study has not got",
               fixed = TRUE)
  # (generation still leaves it out: the program is written)
  st <- tfl_read_study_spec(d)
  ro <- tfl_resolve_output(tfl_read_outputs(d)[["T-14-1-1"]], st)
  lab <- Filter(function(it) identical(it$fn, "plan_labels"), ro$content)[[1L]]$args
  expect_false("RACE" %in% names(lab))
})

test_that("a template's default reference is pruned and not reported; the report's own one is", {
  skip_on_cran()
  local_test_series()
  d <- srf_study(srf_trt)
  # (the study has SEX, not RACE or ETHNIC: demog's default value_labels name all three)
  tmpl <- c("id: T-DM", "kind: table", "label: Demographics", "common:", "  population: SAF",
            "template:", "  name: tflspec_test/demog",
            "  analysis:", "    by: TRT01A", "    continuous: [AGE]", "    categorical: [SEX, RACE]")
  f <- file.path(d, "spec", "outputs", "T-DM.yml")
  writeLines(tmpl, f)
  suppressMessages(tfl_copy_templates(d, "tflspec_test/demog"))
  o <- tfl_read_outputs(d)[["T-DM"]]
  expect_identical(nrow(tfl_study_refs(o)), 0L)
  p <- tfl_check_spec(d, output_id = "T-DM", adam = FALSE)
  expect_identical(nrow(srf_refs(p)), 0L)
  ro <- tfl_resolve_output(o, tfl_read_study_spec(d))
  lab <- Filter(function(it) identical(it$fn, "plan_labels"), ro$content)[[1L]]$args
  expect_true("SEX" %in% names(lab))
  expect_false("RACE" %in% names(lab))
  # the report's own value_labels naming RACE: reported
  writeLines(c(tmpl, "  content:", "    value_labels:", "      RACE: !r study$codelists$RACE"), f)
  o <- tfl_read_outputs(d)[["T-DM"]]
  r <- tfl_study_refs(o)
  expect_identical(r$part, "template")
  expect_identical(r$item, "content / value_labels / RACE")
  rr <- srf_refs(tfl_check_spec(d, output_id = "T-DM", adam = FALSE))
  expect_identical(rr$item, "template: content / value_labels / RACE")
})

test_that("the check of the reports in memory gives the rows tfl_check_spec() gives on the folder", {
  d <- srf_study(c(srf_trt, "SEX: {list: GENDER}", "AGEGR1: {list: AGEGR1, codes: [\">=65\", \"<70\"]}"),
                 labels = "{RACE: !r study$codelists$RACE}")
  st <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  mem <- tfl_check_study_refs(st, outs)
  expect_identical(attr(mem, "unresolved"), "T-14-1-1")
  attr(mem, "unresolved") <- NULL
  rownames(mem) <- NULL
  disk <- srf_refs(tfl_check_spec(d, adam = FALSE))
  rownames(disk) <- NULL
  expect_identical(nrow(mem), 3L)
  # (and the study's list and the code each row names: what a writer
  # compares, not the message)
  expect_identical(names(mem), c("output_id", "part", "item", "level", "message", "list", "code"))
  expect_setequal(paste(mem$item, mem$list, mem$code), c("SEX GENDER NA", "AGEGR1 AGEGR1 <70",
                                                         "content: plan_labels / RACE RACE NA"))
  expect_identical(mem[names(disk)], disk)
  # (a study changed in memory, not on disk: the in-memory check sees it)
  st$codelists$GENDER <- c(F = "Female", M = "Male")
  st$codelists$RACE <- c("WHITE", "ASIAN")
  st$codelists$AGEGR1 <- c("<65", ">=65", "<70")
  expect_identical(nrow(tfl_check_study_refs(st, outs)), 0L)
  # (one report, or a list unnamed: by its id)
  one <- tfl_check_study_refs(st, tfl_output("T-9", "table", codelists = list(SEX = list(list = "NOPE"))))
  expect_identical(one$output_id, "T-9")
  expect_identical(attr(one, "unresolved"), "T-9")
  expect_error(tfl_check_study_refs(st, list(1)), "a list of reports", fixed = TRUE)
  expect_error(tfl_check_study_refs(list(), outs), "a study read with", fixed = TRUE)
})

test_that("the xlsx import refuses a change that names a list the study has not got; an old error does not stop it", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  d <- srf_study(c(srf_trt, "SEX: {list: SEX}"))
  f <- file.path(d, "view.xlsx")
  tfl_write_spec_xlsx(d, f)
  x <- openxlsx::read.xlsx(f, "codelists")
  x[] <- lapply(x, as.character)
  put <- function(x) {
    wb <- openxlsx::loadWorkbook(f)
    openxlsx::removeWorksheet(wb, "codelists")
    openxlsx::addWorksheet(wb, "codelists")
    openxlsx::writeData(wb, "codelists", x, keepNA = FALSE)
    openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  }
  rf <- file.path(d, "spec", "outputs", "T-14-1-1.yml")
  before <- readLines(rf)
  x$list[x$variable == "SEX"] <- "GENDER"
  put(x)
  expect_error(suppressMessages(tfl_import_spec_xlsx(d, f)),
               "nothing written: the workbook's changes name the study's code lists wrongly", fixed = TRUE)
  expect_error(suppressMessages(tfl_import_spec_xlsx(d, f)),
               "T-14-1-1, SEX: the code list of SEX refers to the study's list GENDER", fixed = TRUE)
  expect_identical(readLines(rf), before)
  # (a code the list has not got: refused the same way)
  x$list[x$variable == "SEX"] <- "SEX: F | U"
  put(x)
  expect_error(suppressMessages(tfl_import_spec_xlsx(d, f)), "picks \"U\" from the study's list SEX", fixed = TRUE)
  expect_identical(readLines(rf), before)
  # (write = FALSE writes nothing and reports the change)
  r <- suppressMessages(tfl_import_spec_xlsx(d, f, write = FALSE))
  expect_true(any(r$changes$column %in% "list"))
  # an error the file has already (a RACE reference in its own R): the
  # workbook's good change is written
  srf_put(d, c(srf_trt, "SEX: {list: SEX}"), labels = "{RACE: !r study$codelists$RACE}")
  tfl_write_spec_xlsx(d, f)
  x <- openxlsx::read.xlsx(f, "codelists")
  x[] <- lapply(x, as.character)
  x$list[x$variable == "SEX"] <- "SEX: M"
  put(x)
  suppressMessages(tfl_import_spec_xlsx(d, f))
  expect_identical(tfl_read_outputs(d)[["T-14-1-1"]]$codelists$SEX, list(list = "SEX", codes = list("M")))
})

test_that("a code twice in a study list: the read keeps the first label, tfl_check_spec() has an error row", {
  r <- tflspec:::.study_codelists_read(list(SEX = list(list(value = "F", label = "Female"),
                                                       list(value = "M", label = "Male"),
                                                       list(value = "F", label = "Woman"))))
  expect_identical(unclass(r$SEX), c(F = "Female", M = "Male"))
  p <- attr(r, "problems")
  expect_identical(p$list, "SEX")
  expect_identical(p$code, "F")
  expect_identical(p$level, "error")
  expect_match(p$message, "code list SEX: the code \"F\" is in it 2 times, with the labels \"Female\", \"Woman\"; the study reads the first (\"Female\")",
               fixed = TRUE)
  r2 <- tflspec:::.study_codelists_read(list(RACE = list("WHITE", "ASIAN", "WHITE")))
  expect_identical(r2$RACE, c("WHITE", "ASIAN"))
  expect_match(attr(r2, "problems")$message, "the code \"WHITE\" is in it 2 times", fixed = TRUE)
  ok <- tflspec:::.study_codelists_read(list(SEX = list("F", "M")))
  expect_identical(ok, list(SEX = c("F", "M")))
  expect_null(attr(ok, "problems"))
  # (on a study: it opens, and the check's study row is an error)
  d <- srf_study(srf_trt)
  cat("  RACE: [WHITE, ASIAN, WHITE]\n", file = file.path(d, "spec", "defaults.yml"), append = TRUE)
  st <- tfl_read_study_spec(d)
  expect_identical(st$codelists$RACE, c("WHITE", "ASIAN"))
  ck <- tfl_check_spec(d, adam = FALSE)
  row <- ck[ck$part == "study" & ck$item == "codelists: RACE" & ck$level == "error", , drop = FALSE]
  expect_identical(nrow(row), 1L)
  expect_true(is.na(row$output_id))
  expect_identical(row$level, "error")
  expect_match(row$message, "the labels \"WHITE\", \"WHITE\"", fixed = TRUE)
})

test_that("a code that is a number in defaults.yml is a warning row asking to quote it (N4)", {
  r <- tflspec:::.study_codelists_read(list(VISITN = list(list(value = 1, label = "Week 1"),
                                                          list(value = "02", label = "Week 2"))))
  expect_identical(unclass(r$VISITN), c("1" = "Week 1", "02" = "Week 2"))
  p <- attr(r, "problems")
  expect_identical(p$code, "1")
  expect_identical(p$level, "warning")
  expect_match(p$message, "code list VISITN: the code 1 is a number in spec/defaults.yml, not text", fixed = TRUE)
  expect_match(p$message, "Quote it as written (`value: \"1\"`)", fixed = TRUE)
  d <- srf_study(srf_trt)
  cat("  VISX: [{value: 01, label: Week 1}, {value: \"2\", label: Week 2}]\n",
      file = file.path(d, "spec", "defaults.yml"), append = TRUE)
  ck <- tfl_check_spec(d, adam = FALSE)
  row <- ck[ck$part == "study" & ck$item == "codelists: VISX", , drop = FALSE]
  expect_identical(row$level, "warning")
  expect_match(row$message, "the code 1 is a number", fixed = TRUE)
})

test_that("a code list written as a map of codes to labels stops, naming the right forms (S4)", {
  # the study's list
  expect_error(tflspec:::.study_codelists_read(list(SEX = list(F = "Female", M = "Male"))),
               paste0("spec/defaults.yml `codelists:` SEX: a map of codes to labels ({F: Female, ...}) is not a ",
                      "code list (its codes would be lost). Write its rows, [{value: F, label: Female}, ...], ",
                      "or its codes alone, [F, ...]."), fixed = TRUE)
  d <- srf_study(srf_trt)
  cat("  RACE: {WHITE: White, ASIAN: Asian}\n", file = file.path(d, "spec", "defaults.yml"), append = TRUE)
  expect_error(tfl_read_study_spec(d), "RACE: a map of codes to labels ({WHITE: White, ...})", fixed = TRUE)
  # a report's list
  expect_error(tflspec:::.codelists_norm(list(SEX = list(F = "Female")), "T-1"),
               paste0("T-1: codelists: SEX: a map of codes to labels ({F: Female, ...}) is not a code list ",
                      "(its codes would be lost). Write its rows, [{value: F, label: Female}, ...], or refer ",
                      "to the study's list, {list: SEX}."), fixed = TRUE)
  d2 <- srf_study(c(srf_trt, "SEX: {F: Female, M: Male}"))
  expect_error(tfl_read_output(file.path(d2, "spec", "outputs", "T-14-1-1.yml")),
               "T-14-1-1: codelists: SEX: a map of codes to labels", fixed = TRUE)
  # (the right forms still read)
  expect_identical(tflspec:::.codelists_norm(list(SEX = list("F", "M")))$SEX[[1L]], list(value = "F", label = "F"))
  expect_identical(tflspec:::.codelists_norm(list(SEX = list(list = "SEX")))$SEX, list(list = "SEX"))
})

test_that("an empty study list is kept by name: a reference to it says it is empty (S5)", {
  r <- tflspec:::.study_codelists_read(list(SEX = list("F", "M"), RACE = list(), ETHNIC = NULL))
  expect_identical(names(r), c("SEX", "RACE", "ETHNIC"))
  expect_length(r$RACE, 0L)
  expect_error(tflspec:::.cl_ref_rows(list(list = "RACE"), "RACE", r, "T-1", "test"),
               "test: report T-1, the code list of RACE refers to the study's list RACE, which is empty (spec/defaults.yml `codelists:` RACE has no codes).",
               fixed = TRUE)
  d <- srf_study(c(srf_trt, "SEX: {list: GENDER}", "RACE: {list: RACE}"),
                 labels = "{ETHNIC: !r study$codelists$ETHNIC}")
  cat("  RACE: []\n  ETHNIC: []\n", file = file.path(d, "spec", "defaults.yml"), append = TRUE)
  rr <- srf_refs(tfl_check_spec(d, adam = FALSE))
  expect_match(rr$message[rr$item == "RACE"],
               "the code list of RACE refers to the study's list RACE, which is empty (spec/defaults.yml `codelists:` RACE has no codes). Give the list its codes",
               fixed = TRUE)
  expect_match(rr$message[rr$item == "content: plan_labels / ETHNIC"],
               "names the study's list ETHNIC, which is empty", fixed = TRUE)
  # (a list the study has not got is still said so, the empty ones among its names)
  expect_match(rr$message[rr$item == "SEX"], "GENDER, which the study has not got", fixed = TRUE)
})

test_that("a writer's new rows are by report, place, list and code, not by message (S3)", {
  d <- srf_study(c(srf_trt, "SEX: {list: X}", "AGEGR1: {list: Z}"))
  st <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  old <- tfl_check_study_refs(st, outs)
  expect_identical(old$list, c("X", "Z"))
  # broken on X and Z; add X: Z's row has other words (the study's list
  # names), but it is not new, so a save goes through
  st$codelists$X <- c(F = "Female", M = "Male")
  new <- tfl_check_study_refs(st, outs)
  expect_identical(new$list, "Z")
  expect_false(identical(new$message, old$message[old$list == "Z"]))
  expect_identical(nrow(tflspec:::.check_rows_new(new, old)), 0L)
  # a break the edit makes is new: a code taken out of a list a pick names
  srf_put(d, c(srf_trt, "SEX: {list: SEX, codes: [F, M]}", "AGEGR1: {list: Z}"))
  st <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  old <- tfl_check_study_refs(st, outs)
  st$codelists$SEX <- c(F = "Female")
  add <- tflspec:::.check_rows_new(tfl_check_study_refs(st, outs), old)
  expect_identical(add$item, "SEX")
  expect_identical(add$code, "M")
  # two codes missing, one put back: the row left is not new
  srf_put(d, c(srf_trt, "SEX: {list: SEX, codes: [F, U, X]}"))
  outs <- tfl_read_outputs(d)
  st <- tfl_read_study_spec(d)
  old <- tfl_check_study_refs(st, outs)
  expect_identical(old$code, "U | X")
  st$codelists$SEX <- c(F = "Female", M = "Male", U = "Unknown")
  new <- tfl_check_study_refs(st, outs)
  expect_identical(new$code, "X")
  expect_identical(nrow(tflspec:::.check_rows_new(new, old)), 0L)
})

test_that("the xlsx import says when the study cannot be read, not skip the check in silence (S6, N6)", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")
  d <- srf_study(c(srf_trt, "SEX: {list: SEX}"))
  f <- file.path(d, "view.xlsx")
  tfl_write_spec_xlsx(d, f)
  x <- openxlsx::read.xlsx(f, "codelists")
  x[] <- lapply(x, as.character)
  x$list[x$variable == "SEX"] <- "SEX: M"
  wb <- openxlsx::loadWorkbook(f)
  openxlsx::removeWorksheet(wb, "codelists")
  openxlsx::addWorksheet(wb, "codelists")
  openxlsx::writeData(wb, "codelists", x, keepNA = FALSE)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  cat("  RACE: {WHITE: White}\n", file = file.path(d, "spec", "defaults.yml"), append = TRUE)
  expect_message(tfl_import_spec_xlsx(d, f),
                 "the references of the changed reports to the study's code lists are not checked: the study cannot be read",
                 fixed = TRUE)
})

test_that("every study code-list block shipped in tflspec and its test fixtures reads with no problem (S5)", {
  files <- c(list.files(system.file(package = "tflspec"), "^defaults[.]yml$", recursive = TRUE, full.names = TRUE),
             list.files(test_path("fixtures"), "^defaults[.]yml$", recursive = TRUE, full.names = TRUE))
  n <- 0L
  for (f in files) {
    cl <- yaml::read_yaml(f, handlers = tflspec:::.yaml_handlers())$codelists
    if (is.null(cl)) next
    r <- tflspec:::.study_codelists_read(cl)
    expect_null(attr(r, "problems"), label = f)
    n <- n + 1L
  }
  # (the sheets of the fixtures' studies, when they have the study's lists)
  for (f in list.files(test_path("fixtures"), "[.]rds$", recursive = TRUE, full.names = TRUE)) {
    sh <- readRDS(f)
    if (!is.list(sh) || is.null(sh[["study_codelists"]])) next
    r <- tflspec:::.study_codelists_read(tflspec:::.conv_study_codelists(sh[["study_codelists"]]))
    expect_null(attr(r, "problems"), label = f)
    n <- n + 1L
  }
  expect_gt(n, 0L)
})

test_that("C04 says a code not in the study's list", {
  r <- tfl_review_rules()
  c04 <- r[r$rule == "C04", ]
  expect_identical(c04$message, "The code list of %s in %s has a code not in the study's list.")
  expect_match(c04$hint, "Add the code to the study's list", fixed = TRUE)
})
