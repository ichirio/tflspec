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
  # the column headers carry each arm's N: the arm labels from the code
  # list (Drug X -> "Drug X 200 mg") are matched to BIGN's values
  for (p in r$rtf) {
    x <- paste(readLines(p, warn = FALSE), collapse = "\n")
    expect_match(x, "N=[0-9]+")
    expect_false(grepl("N=NA", x, fixed = TRUE))
  }
})

# The call `plan_labels(...)` of a program, from its "plan_labels(" to the
# bracket that closes it (a label may hold brackets: "Age (years)")
twin_labels_call <- function(code) {
  x <- paste(code, collapse = "\n")
  s <- regexpr("plan_labels(", x, fixed = TRUE)
  expect_true(s > 0L)
  ch <- strsplit(substring(x, s), "")[[1L]]
  depth <- 0L
  quote <- ""
  for (i in seq_along(ch)) {
    if (nzchar(quote)) {
      if (ch[i] == "\\") ch[i + 1L] <- "" else if (ch[i] == quote) quote <- ""
    } else if (ch[i] %in% c("\"", "'")) {
      quote <- ch[i]
    } else if (ch[i] == "(") {
      depth <- depth + 1L
    } else if (ch[i] == ")") {
      depth <- depth - 1L
      if (depth == 0L) return(gsub("\\s+", " ", paste(ch[seq_len(i)], collapse = "")))
    }
  }
  NA_character_
}

test_that("template labels: the YAML path and the code SPEC (the planner's path) write the same plan_labels", {
  twin_skip()
  d <- twin_study(file.path(tempdir(), "twin-labels"),
                  system.file("extdata", "spec-yaml", "defaults.yml", package = "tflspec"),
                  style = FALSE)
  on.exit(unlink(d, recursive = TRUE))
  file.copy(twin_fx("T-14-1-1T.yml"), file.path(d, "spec", "outputs", "T-14-1-1T.yml"))
  st <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  # the planner's path: the template as its code SPEC, written as it is
  code_spec <- tfl_content_code(tfl_resolve_output(tfl_template_to_spec(outs[["T-14-1-1T"]], st), st),
                                pipe = "|>", template = "items")
  # the YAML path: the expanded template on the sheets, then the sheets' program
  sh <- tfl_sheets_from_spec(st, outs, templates = "expand")
  from_sheets <- tfl_table_code(tfl_table_spec(sh$table), "T-14-1-1T", pipe = "|>")
  a <- twin_labels_call(code_spec)
  b <- twin_labels_call(from_sheets)
  expect_identical(b, a)
  expect_match(b, "plan_labels\\( ?AGE = \"Age \\(years\\)\"")
  expect_false(grepl("variable = ", b, fixed = TRUE))
})

test_that("AE: the template SPEC and the code SPEC give the same ARD", {
  twin_skip()
  d <- twin_study(file.path(tempdir(), "twin-ae"), twin_fx("defaults-sample01.yml"))
  on.exit(unlink(d, recursive = TRUE))
  r <- twin_build("T-14-3-1", d)
  expect_identical(unique(r$ard$C$analysis_id), c("BIGN", "AE_ANY", "AE_L1", "AE_L2"))
  expect_identical(twin_ard(r$ard$T), twin_ard(r$ard$C))
  # (T has no content cards of its own: the template's content, its
  # defaults the code SPEC's cards, and max_rows 22, gives C's RTF)
  expect_identical(twin_rtf(r$rtf$T, "T-14-3-1T"), twin_rtf(r$rtf$C, "T-14-3-1C"))
})

test_that("tflspec_summary: DM's arguments give demog's ARD", {
  twin_skip()
  d <- twin_study(file.path(tempdir(), "twin-summary"),
                  system.file("extdata", "spec-yaml", "defaults.yml", package = "tflspec"),
                  style = FALSE)
  on.exit(unlink(d, recursive = TRUE))
  out <- file.path(d, "spec", "outputs")
  x <- readLines(twin_fx("T-14-1-1T.yml"))
  y <- sub("^id: T-14-1-1T$", "id: T-14-1-1S", x)
  y <- sub("^  name: tflspec_v1/demog$", "  name: tflspec_v1/tflspec_summary", y)
  expect_identical(sum(y != x), 2L)
  ids <- c(demog = "T-14-1-1T", summary = "T-14-1-1S")
  writeLines(x, file.path(out, "T-14-1-1T.yml"))
  writeLines(y, file.path(out, "T-14-1-1S.yml"))
  st <- tfl_read_study_spec(d)
  for (id in ids) {
    f <- file.path(out, paste0(id, ".yml"))
    tfl_write_output(tfl_template_to_spec(tfl_read_output(f), st), f)
  }
  st <- tfl_read_study_spec(d)
  ard <- lapply(ids, function(id)
    suppressMessages(tfl_build_ard(st, dir = d, output_id = id, save = FALSE)))
  expect_identical(unique(ard$summary$analysis_id),
                   c("BIGN", "AGE", "AGEGR1", "SEX", "RACE", "ECOGBL"))
  expect_identical(twin_ard(ard$summary), twin_ard(ard$demog))
})

test_that("tflspec_summary: a report's filter on adsl_sub reaches BIGN (header N and % denominator)", {
  twin_skip()
  d <- twin_study(file.path(tempdir(), "twin-filter"),
                  system.file("extdata", "spec-yaml", "defaults.yml", package = "tflspec"),
                  style = FALSE)
  on.exit(unlink(d, recursive = TRUE))
  x <- readLines(twin_fx("T-14-1-1T.yml"))
  x <- sub("^id: T-14-1-1T$", "id: T-14-1-1F", x)
  x <- sub("^  name: tflspec_v1/demog$", "  name: tflspec_v1/tflspec_summary", x)
  x <- append(x, c("data:", "  - id: adsl_sub", "    where: !r SEX == \"F\""),
              after = match("codelists:", x) - 1L)
  f <- file.path(d, "spec", "outputs", "T-14-1-1F.yml")
  writeLines(x, f)
  st <- tfl_read_study_spec(d)
  tfl_write_output(tfl_template_to_spec(tfl_read_output(f), st), f)
  st <- tfl_read_study_spec(d)
  ard <- as.data.frame(suppressMessages(
    tfl_build_ard(st, dir = d, output_id = "T-14-1-1F", save = FALSE)))
  txt <- function(v) vapply(v, function(e) as.character(unlist(e))[1L], "")
  num <- function(v) vapply(v, function(e) as.numeric(unlist(e))[1L], 0)
  a <- tflspec::adsl
  a <- a[a$SAFFL %in% "Y" & a$SEX %in% "F", ]
  want <- table(as.character(a$TRT01A))
  # the header N: BIGN's n per arm, the filtered subjects
  big <- ard[ard$analysis_id == "BIGN" & ard$stat_name == "n", ]
  expect_equal(stats::setNames(num(big$stat), txt(big$variable_level))[names(want)],
               stats::setNames(as.numeric(want), names(want)))
  # the percentages: each arm's N is the same BIGN, and p = n / N
  sx <- ard[ard$analysis_id == "SEX", ]
  n_arm <- sx[sx$stat_name == "N", ]
  expect_equal(stats::setNames(num(n_arm$stat), txt(n_arm$group1_level))[names(want)],
               stats::setNames(as.numeric(want), names(want)))
  p <- sx[sx$stat_name == "p" & txt(sx$variable_level) == "F", ]
  expect_equal(num(p$stat), rep(1, nrow(p)))
  expect_false(any(txt(sx$variable_level) == "M" & sx$stat_name == "n" & num(sx$stat) > 0))
})

test_that("tflspec_summary: nest moves rows, the header has the arms and N, labels and value_labels both print", {
  twin_skip()
  d <- twin_study(file.path(tempdir(), "twin-sumparams"),
                  system.file("extdata", "spec-yaml", "defaults.yml", package = "tflspec"),
                  style = FALSE)
  on.exit(unlink(d, recursive = TRUE))
  mk <- function(id, content) {
    f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
    writeLines(c(paste0("id: ", id), "kind: table", "label: Summary",
                 "common: {population: SAF, treatment: TRT01A}",
                 "template:", "  name: tflspec_v1/tflspec_summary",
                 "  analysis:", "    by: [TRT01A]", "    categorical: [SEX, RACE, ETHNIC]",
                 "    missing_row: false", "  content:", content), f)
    st <- tfl_read_study_spec(d)
    tfl_write_output(tfl_template_to_spec(tfl_read_output(f), st), f)
    st <- tfl_read_study_spec(d)
    suppressMessages(tfl_build_ard(st, dir = d, output_id = id, save = FALSE))
    readLines(tfl_run_output(id, dir = d), warn = FALSE)
  }
  has <- function(x, s) grep(s, x, fixed = TRUE)
  a <- mk("T-SUM-A", "    max_rows: 30")
  b <- mk("T-SUM-B", c("    nest: {ETHNIC: {RACE: ASIAN}}",
                       "    labels: {SEX: \"Sex, n (%)\"}",
                       "    value_labels: {SEX: {F: Female, M: Male}}"))
  # (the study's defaults give the header: the arm line and the (N=n) line)
  for (x in list(a, b)) {
    expect_length(has(x, "Characteristic"), 1L)
    expect_true(length(has(x, "(N=80)")) == 1L && length(has(x, "(N=78)")) == 1L)
    expect_true(has(x, "Characteristic") == has(x, "(N=80)"))
    expect_true(has(x, "Drug X") == has(x, "Characteristic") - 1L)
  }
  # without nest the ETHNIC block follows RACE's; with it the rows sit under ASIAN
  # (RACE in the order of the sample defaults.yml's adam_meta, CL.RACE:
  # WHITE, BLACK OR AFRICAN AMERICAN, ASIAN)
  rows <- function(x, s) min(has(x, s))
  expect_true(rows(a, "ASIAN") == rows(a, "BLACK OR AFRICAN AMERICAN") + 1L)
  expect_true(rows(a, "HISPANIC OR LATINO") > rows(a, "ASIAN"))
  expect_true(rows(b, "HISPANIC OR LATINO") == rows(b, "ASIAN") + 1L)
  expect_true(rows(b, "BLACK OR AFRICAN AMERICAN") > rows(b, "WHITE"))
  expect_true(rows(b, "ASIAN") > rows(b, "BLACK OR AFRICAN AMERICAN"))
  expect_length(has(a, "ETHNIC"), 1L)
  expect_length(has(b, "ETHNIC"), 0L)
  # labels and value_labels in one plan_labels: the heading and the values both print
  expect_length(has(b, "Sex, n (%)"), 1L)
  expect_length(has(b, "Female"), 1L)
  expect_length(has(b, "Male"), 1L)
  expect_length(has(a, "Female"), 0L)
})
