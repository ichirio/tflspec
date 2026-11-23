# A template report's labels (`labels`, `value_labels`, `param_labels`) as
# one plan_labels() call, whichever way the report is made (tflspec#856):
# the YAML route (tfl_resolve_output(), the template's items) and the
# planner's (tfl_template_to_spec(), the report written out and read back)
# give the same item and the same program line; the sheets write it the
# same too.  The data are the package's (fictional) and a tiny made-up ADSL.

tlp_study <- function(d, adsl = NULL) {
  dir.create(file.path(d, "adam"), recursive = TRUE)
  ds <- list(adsl = adsl %||% tflspec:::.ai_chr(tflspec::adsl))
  if (is.null(adsl)) {
    ds$adlb <- tflspec:::.ai_chr(tflspec::adlb)
    ds$advs <- tflspec:::.ai_chr(tflspec::advs)
  }
  for (x in names(ds)) haven::write_xpt(ds[[x]], file.path(d, "adam", paste0(x, ".xpt")))
  tfl_ai_study_init(d, adam = "adam", study_id = "LBL-1")
  d
}

# the plan_labels() call(s) of a program, each as its lines (trimmed, no pipe)
tlp_block <- function(code) {
  code <- unname(as.character(code))
  out <- character()
  for (s in grep("plan_labels(", code, fixed = TRUE)) {
    j <- s
    n <- function(x, ch) sum(gregexpr(ch, x, fixed = TRUE)[[1L]] > 0L)
    while (n(paste(code[s:j], collapse = "\n"), "(") > n(paste(code[s:j], collapse = "\n"), ")")) {
      j <- j + 1L
    }
    out <- c(out, trimws(sub("\\|>\\s*$", "", code[s:j])))
  }
  out
}

tlp_labels <- function(x) {
  lapply(Filter(function(it) identical(it$fn, "plan_labels"), x$content), `[[`, "args")
}

# The report `tpl` (its `template:`) in the study `d`, both ways: `a` the
# YAML route's resolved report, `b` the code SPEC tfl_template_to_spec()
# makes, written and read back and resolved; `b_spec` that code SPEC
tlp_both <- function(d, id, tpl, codelists = list()) {
  o <- tfl_output(id, "table", common = list(population = "SAF"), template = tpl, codelists = codelists)
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  tfl_write_output(o, f)
  invisible(tfl_copy_templates(d))
  st <- tfl_read_study_spec(d)
  a <- tfl_resolve_output(tfl_read_output(f), st)
  b <- tfl_template_to_spec(tfl_read_output(f), st)
  b$id <- paste0(id, "-B")
  fb <- file.path(d, "spec", "outputs", paste0(b$id, ".yml"))
  tfl_write_output(b, fb)
  rb <- tfl_resolve_output(tfl_read_output(fb), st)
  unlink(fb)
  list(a = a, b = rb, b_spec = b, study = st, file = f)
}

# Each template with a labels argument (series and the package's own), its
# arguments and the heading the program says
tlp_cases <- list(
  "tflspec_v1/demog" = list(
    tpl = list(name = "tflspec_v1/demog",
               analysis = list(by = "TRT01A", continuous = "AGE", categorical = c("SEX", "RACE")),
               content = list(labels = list(AGE = "Age (years)", SEX = "Sex, n (%)", RACE = "Race"),
                              value_labels = list(SEX = list(F = "Female", M = "Male")))),
    line = "variable = c(AGE = \"Age (years)\", SEX = \"Sex, n (%)\", RACE = \"Race\"),"),
  "tflspec_v1/tflspec_summary" = list(
    tpl = list(name = "tflspec_v1/tflspec_summary",
               analysis = list(by = "TRT01A", continuous = "AGE", categorical = "SEX"),
               content = list(labels = list(AGE = "Age (years)", SEX = "Sex"),
                              value_labels = list(SEX = list(F = "Female", M = "Male")))),
    line = "variable = c(AGE = \"Age (years)\", SEX = \"Sex\"),"),
  demog = list(
    tpl = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX",
                                           labels = list(AGE = "Age (years)", SEX = "Sex, n (%)"))),
    line = "plan_labels(variable = c(AGE = \"Age (years)\", SEX = \"Sex, n (%)\"))"),
  summary = list(
    tpl = list(name = "summary", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX",
                                             labels = list(AGE = "Age (years)", SEX = "Sex"))),
    line = "plan_labels(variable = c(AGE = \"Age (years)\", SEX = \"Sex\"))"),
  stf_t02_demog = list(
    tpl = list(name = "stf_t02_demog", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX",
                                                   labels = list(AGE = "Age (years)", SEX = "Sex"))),
    line = "plan_labels(variable = c(AGE = \"Age (years)\", SEX = \"Sex\"))"),
  disposition = list(
    tpl = list(name = "disposition", args = list(by = "TRT01A", categorical = "EOSSTT",
                                                 labels = list(EOSSTT = "Status, n (%)"))),
    line = "plan_labels(variable = c(EOSSTT = \"Status, n (%)\"))"),
  shift = list(
    tpl = list(name = "shift", args = list(population = "SAF",
                                           param_labels = list(ALT = "Alanine aminotransferase (U/L)"))),
    line = "plan_labels(PARAMCD = c(ALT = \"Alanine aminotransferase (U/L)\"))"),
  lab_shift = list(
    tpl = list(name = "lab_shift", args = list(population = "SAF", visit = "V", params = "ALT",
                                               param_labels = list(ALT = "Alanine aminotransferase (U/L)"))),
    line = "plan_labels(PARAMCD = c(ALT = \"Alanine aminotransferase (U/L)\"))"),
  param_change = list(
    tpl = list(name = "param_change", args = list(population = "SAF", by = "TRT01A", data = "ADVS",
                                                  param_labels = list(SYSBP = "Systolic BP (mmHg)"))),
    line = "PARAMCD = c(SYSBP = \"Systolic BP (mmHg)\")")
)

test_that("each template's labels: the same plan_labels() item and line on both paths", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- tlp_study(withr::local_tempdir())
  for (nm in names(tlp_cases)) {
    cs <- tlp_cases[[nm]]
    x <- tlp_both(d, paste0("T-", gsub("[^A-Za-z0-9]", "-", nm)), cs$tpl)
    la <- tlp_labels(x$a)
    expect_length(la, 1L)
    expect_identical(la, tlp_labels(x$b), info = nm)
    ca <- tlp_block(suppressMessages(tfl_content_code(x$a, template = "items")))
    cb <- tlp_block(suppressMessages(tfl_content_code(x$b)))
    expect_true(length(ca) > 0L, info = nm)
    expect_identical(ca, cb, info = nm)
    expect_true(cs$line %in% ca, info = paste(nm, paste(ca, collapse = " ")))
    # a variable's heading is never a flat entry of its own
    expect_false(any(grepl("^plan_labels\\(AGE = ", ca)), info = nm)
    unlink(x$file)
  }
})

test_that("a series template's labels: the sheets write the planner's plan_labels() line", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- tlp_study(withr::local_tempdir())
  # (the report's code list of the arms: its labels after the headings, as
  # the sheets write them -- the twin of tflplanner#711)
  arms <- list(TRT01A = list(list(value = "Drug X", label = "Drug X 200 mg"),
                             list(value = "Control", label = "Control 100 mg")))
  for (nm in c("tflspec_v1/demog", "tflspec_v1/tflspec_summary")) {
    id <- paste0("T-", gsub("[^A-Za-z0-9]", "-", nm))
    x <- tlp_both(d, id, tlp_cases[[nm]]$tpl, codelists = arms)
    sh <- suppressWarnings(tfl_sheets_from_spec(x$study, stats::setNames(list(tfl_read_output(x$file)), id),
                                                templates = "expand"))
    cb <- tlp_block(suppressMessages(tfl_content_code(x$b)))
    expect_identical(tlp_block(tfl_table_code(sh$table, id)), cb, info = nm)
    expect_identical(match("TRT01A = c(\"Drug X\" = \"Drug X 200 mg\", Control = \"Control 100 mg\"),", cb), 3L,
                     info = paste(cb, collapse = " "))
    unlink(x$file)
  }
})

test_that(".plan_labels_args(): headings under `variable`, key columns and value labels by name", {
  f <- tflspec:::.plan_labels_args
  expect_identical(f(list(AGE = "Age", SEX = list(F = "Female"), TRT01A = "Arm"), c("TRT01A", "variable")),
                   list(variable = list(AGE = "Age"), SEX = list(F = "Female"), TRT01A = "Arm"))
  # a `variable` entry of the map's own comes first; a reference stays by name
  r <- tfl_r("study$codelists$SEX")
  expect_identical(f(list(SEX = r, variable = list(BMI = "BMI"), AGE = "Age"), "variable"),
                   list(variable = list(BMI = "BMI", AGE = "Age"), SEX = r))
  expect_identical(f(list(PARAMCD = c(ALT = "ALT (U/L)")), "variable"), list(PARAMCD = c(ALT = "ALT (U/L)")))
  # a value-labels map's own heading joins the others: rtfreporter reads
  # every heading from `variable` once it is given
  expect_identical(f(list(AGE = "Age", SEX = list(SEX = "Sex", F = "Female")), c("TRT01A", "variable")),
                   list(variable = list(AGE = "Age", SEX = "Sex"), SEX = list(F = "Female")))
  # a `variable` entry in R: the map as it is
  vr <- list(variable = tfl_r("study_headings"), AGE = "Age")
  expect_identical(f(vr, "variable"), vr)
  # a table whose rows are not `variable`: the map as it is
  expect_identical(f(list(AGE = "Age"), "PARAMCD"), list(AGE = "Age"))
  expect_identical(f(NULL), list())
})

test_that("the generated program runs: the plan of a tiny made-up ADSL, its headings applied", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  # (made up: six subjects)
  adsl <- data.frame(STUDYID = "LBL-1", USUBJID = sprintf("LBL-1-%03d", 1:6),
                     TRT01A = rep(c("Drug A", "Placebo"), 3), AGE = c(34, 51, 47, 62, 29, 58),
                     SEX = c("F", "M", "M", "F", "F", "M"), SAFFL = "Y", stringsAsFactors = FALSE)
  d <- tlp_study(withr::local_tempdir(), adsl = adsl)
  tpl <- list(name = "tflspec_v1/demog",
              analysis = list(by = "TRT01A", continuous = "AGE", categorical = "SEX"),
              content = list(labels = list(AGE = "Age (years)", SEX = "Sex, n (%)"),
                             value_labels = list(SEX = list(F = "Female", M = "Male"))))
  x <- tlp_both(d, "T-RUN", tpl)
  ard <- suppressMessages(tfl_build_ard(x$study, output_id = "T-RUN", dir = d, save = FALSE))
  code <- suppressMessages(tfl_content_code(x$b))
  expect_true("variable = c(AGE = \"Age (years)\", SEX = \"Sex, n (%)\")," %in% tlp_block(code))
  env <- new.env(parent = asNamespace("rtfreporter"))
  env$ard <- ard
  eval(parse(text = code), env)
  tb <- suppressMessages(rtfreporter::plan_apply(env$plan, "table"))
  txt <- unlist(lapply(tb, as.character), use.names = FALSE)
  expect_true(all(c("Age (years)", "Sex, n (%)", "Female", "Male") %in% txt))
  # the program this report had before (the heading flat, SEX's in its
  # value labels) gives the same table: the change is the program's text,
  # not its output
  expect_true("SEX = c(F = \"Female\", M = \"Male\")" %in% tlp_block(code))
  flat <- sub("variable = c(AGE = \"Age (years)\", SEX = \"Sex, n (%)\"),", "AGE = \"Age (years)\",",
              code, fixed = TRUE)
  flat <- sub("SEX = c(F = \"Female\", M = \"Male\")", "SEX = c(SEX = \"Sex, n (%)\", F = \"Female\", M = \"Male\")",
              flat, fixed = TRUE)
  expect_false(identical(flat, code))
  env2 <- new.env(parent = asNamespace("rtfreporter"))
  env2$ard <- ard
  eval(parse(text = flat), env2)
  expect_identical(suppressMessages(rtfreporter::plan_apply(env2$plan, "table")), tb)
})
