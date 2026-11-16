# The same report as a template SPEC (T) and as a code SPEC (C) gives the
# same ARD and the same RTF.  The twins are fixtures/twin/: T-14-1-1T / C
# (DM: tflspec's inst/extdata/spec-yaml/outputs/T-14-1-1.yml, with the
# canon analysis ids) and T-14-3-1T / C (AE: tflplanner's SAMPLE-01
# T-14-3-1, with the canon ids).  Both go through the app's path: T written
# as the code SPEC tfl_template_to_spec() makes of it, then for each
# tfl_build_ard() and tfl_run_output() (the ARD built, the content run, the
# RTF written).  What may differ: the output id and the run's date and
# time -- nothing else.

twin_fx <- function(f) test_path("fixtures", "twin", f)

# A study of the package's data (fictional) with `defaults` as its
# defaults.yml; `style`: FALSE drops study.yml's settings.style (the
# spec-yaml study's page is the built-in tfl_standard).  The spec-yaml
# defaults have no normalize_ard card, which table_plan() now needs: it is
# put first, the same for both twins.
twin_study <- function(d, defaults, style = TRUE) {
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "TWIN-1")
  dl <- readLines(defaults)
  if (!any(grepl("normalize_ard", dl, fixed = TRUE))) {
    dl <- append(dl, "  - normalize_ard: {}", after = match("content:", dl))
  }
  writeLines(dl, file.path(d, "spec", "defaults.yml"))
  if (!style) {
    sy <- readLines(file.path(d, "study.yml"))
    writeLines(sy[!grepl("^  style:", sy)], file.path(d, "study.yml"))
  }
  d
}

# The pair `name` (T-14-1-1, T-14-3-1) in the study `d`: T as its code SPEC
# (the planner's path), the ARD of each and, with `rtf`, the RTF of each.
# `drop`: a line both files have, left out of both
twin_build <- function(name, d, rtf = TRUE, drop = NULL) {
  out <- file.path(d, "spec", "outputs")
  for (s in c("T", "C")) {
    x <- readLines(twin_fx(paste0(name, s, ".yml")))
    if (!is.null(drop)) {
      expect_identical(sum(x == drop), 1L)
      x <- x[x != drop]
    }
    writeLines(x, file.path(out, paste0(name, s, ".yml")))
  }
  ft <- file.path(out, paste0(name, "T.yml"))
  st <- tfl_read_study_spec(d)
  tfl_write_output(tfl_template_to_spec(tfl_read_output(ft), st), ft)
  st <- tfl_read_study_spec(d)
  ids <- c(T = paste0(name, "T"), C = paste0(name, "C"))
  ard <- lapply(ids, function(id)
    suppressMessages(tfl_build_ard(st, dir = d, output_id = id, save = FALSE)))
  path <- if (rtf) lapply(ids, function(id) tfl_run_output(id, dir = d))
  list(ard = ard, rtf = path)
}

# An ARD as text, a row a line: the ids, the groups, the variable, the
# statistic, its value and its formatted value -- not output_id (the twins'
# own ids).  The rows in their order.
twin_ard <- function(a) {
  a <- as.data.frame(a)
  cols <- c("analysis_id", "population_id", grep("^group[0-9]+(_level)?$", names(a), value = TRUE),
            "variable", "variable_level", "context", "stat_name", "stat", "stat_fmt")
  cols <- intersect(cols, names(a))
  cell <- function(v) if (is.list(v)) vapply(v, function(x)
    if (is.null(x) || !length(x)) "" else paste(format(unlist(x), digits = 15), collapse = "|"), "")
    else ifelse(is.na(v), "", as.character(v))
  do.call(paste, c(lapply(a[cols], cell), sep = " / "))
}

# An RTF's text with what may differ masked: the run's date and time, and
# the output id (the program's name; the header's label, "Table 14.3.1T",
# is made from the id)
twin_rtf <- function(p, id) {
  x <- readLines(p, warn = FALSE)
  x <- gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}([T ][0-9]{2}:[0-9]{2}(:[0-9]{2})?)?", "<date>", x)
  x <- gsub("[0-9]{2}[A-Z][a-z]{2}[0-9]{4} +[0-9]{2}:[0-9]{2}", "<date>", x)
  x <- gsub(id, "<id>", x, fixed = TRUE)
  gsub(gsub("-", ".", sub("^T-", "", id), fixed = TRUE), "<id>", x, fixed = TRUE)
}

twin_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
}

test_that("DM: the template SPEC and the code SPEC give the same ARD and the same RTF", {
  twin_skip()
  d <- twin_study(file.path(tempdir(), "twin-dm"),
                  system.file("extdata", "spec-yaml", "defaults.yml", package = "tflspec"),
                  style = FALSE)
  on.exit(unlink(d, recursive = TRUE))
  # (the code SPEC's last card, plan_cell_style(where = p > 0.5), stops the
  # content run of both twins alike: `where` is over the table's columns,
  # and the table has no column p.  The RTFs are compared without it; the
  # ARD does not read it.)
  r <- twin_build("T-14-1-1", d, drop = "  - plan_cell_style: {where: p > 0.5, bold: true}")
  expect_identical(unique(r$ard$C$analysis_id), c("BIGN", "AGE", "AGEGR1", "SEX", "RACE", "ECOGBL"))
  expect_identical(twin_ard(r$ard$T), twin_ard(r$ard$C))
  expect_identical(twin_rtf(r$rtf$T, "T-14-1-1T"), twin_rtf(r$rtf$C, "T-14-1-1C"))
})

test_that("AE: the template SPEC and the code SPEC give the same ARD", {
  twin_skip()
  d <- twin_study(file.path(tempdir(), "twin-ae"), twin_fx("defaults-sample01.yml"))
  on.exit(unlink(d, recursive = TRUE))
  r <- twin_build("T-14-3-1", d)
  expect_identical(unique(r$ard$C$analysis_id), c("BIGN", "AE_ANY", "AE_L1", "AE_L2"))
  expect_identical(twin_ard(r$ard$T), twin_ard(r$ard$C))
  # (the template has no content: block; T's content is the report's own,
  # the code SPEC's cards, so its RTF is C's too)
  expect_identical(twin_rtf(r$rtf$T, "T-14-3-1T"), twin_rtf(r$rtf$C, "T-14-3-1C"))
})
