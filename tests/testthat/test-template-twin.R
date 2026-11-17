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

test_that("tflspec_dm (a report template on tflspec_summary): DM's arguments give demog's ARD", {
  twin_skip()
  d <- twin_study(file.path(tempdir(), "twin-dm-base"),
                  system.file("extdata", "spec-yaml", "defaults.yml", package = "tflspec"),
                  style = FALSE)
  on.exit(unlink(d, recursive = TRUE))
  out <- file.path(d, "spec", "outputs")
  x <- readLines(twin_fx("T-14-1-1T.yml"))
  y <- sub("^id: T-14-1-1T$", "id: T-14-1-1D", x)
  y <- sub("^  name: tflspec_v1/demog$", "  name: tflspec_v1/tflspec_dm", y)
  expect_identical(sum(y != x), 2L)
  ids <- c(demog = "T-14-1-1T", dm = "T-14-1-1D")
  writeLines(x, file.path(out, "T-14-1-1T.yml"))
  writeLines(y, file.path(out, "T-14-1-1D.yml"))
  st <- tfl_read_study_spec(d)
  for (id in ids) {
    f <- file.path(out, paste0(id, ".yml"))
    tfl_write_output(tfl_template_to_spec(tfl_read_output(f), st), f)
  }
  st <- tfl_read_study_spec(d)
  ard <- lapply(ids, function(id)
    suppressMessages(tfl_build_ard(st, dir = d, output_id = id, save = FALSE)))
  expect_identical(twin_ard(ard$dm), twin_ard(ard$demog))
})

test_that("disposition: tflspec_summary with the report's values and tflspec_disposition give the same ARD", {
  twin_skip()
  d <- twin_study(file.path(tempdir(), "twin-disp"),
                  system.file("extdata", "spec-yaml", "defaults.yml", package = "tflspec"),
                  style = FALSE)
  on.exit(unlink(d, recursive = TRUE))
  out <- file.path(d, "spec", "outputs")
  ids <- c(summary = "T-14-1-2A", nest = "T-14-1-2B")
  files <- c(summary = "disp-summary.yml", nest = "disp-nest.yml")
  for (k in names(ids)) {
    file.copy(test_path("fixtures", "summary", files[[k]]), file.path(out, paste0(ids[[k]], ".yml")))
  }
  st <- tfl_read_study_spec(d)
  for (id in ids) {
    f <- file.path(out, paste0(id, ".yml"))
    tfl_write_output(tfl_template_to_spec(tfl_read_output(f), st), f)
  }
  st <- tfl_read_study_spec(d)
  ard <- lapply(ids, function(id)
    suppressMessages(tfl_build_ard(st, dir = d, output_id = id, save = FALSE)))
  expect_identical(unique(ard$nest$analysis_id),
                   c("BIGN", "EOTSTT", "DCTREAS", "EOSSTT", "DCSREAS"))
  expect_identical(twin_ard(ard$nest), twin_ard(ard$summary))
  # the enrolled subjects: 160, both arms (TRT01P)
  big <- as.data.frame(ard$nest)
  big <- big[big$analysis_id == "BIGN" & big$stat_name == "n", ]
  expect_equal(sum(vapply(big$stat, function(e) as.numeric(unlist(e))[1L], 0)), 160)
})
