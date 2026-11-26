# A2 (one population rule), GT's part: a figure of a report with an analysis
# set (its data's `population:`; `common: population` until it goes; one
# reader, .out_population_or_data()) reads the records of its subjects only -- the
# subjects made once by the report's data part (.fig_subjects_part(), the
# ARD programs' .ard_data_part()), every dataset the figure reads joined to
# them (.subjects_join()).  No step keeps a population: a `flag` step is a
# record flag's, and still filters.

a2f_spec <- function(adata = NULL) {
  tfl_ard_spec(c(list(
    datasets = data.frame(dataset = c("ADSL", "ADTTE", "ADPP"),
                          path = c("adsl.rds", "adtte.rds", "adpp.rds")),
    populations = data.frame(population_id = c("SAF", "ENR", "PKS"),
                             dataset = c("ADSL", "ADSL", "ADPP"),
                             where = c("SAFFL == \"Y\"", NA, "PKFL == \"Y\"")),
    analyses = data.frame(output_id = "T-1", analysis_id = "A1", method = "categorical",
                          dataset = "ADSL", population_id = "SAF", by = "TRT01A",
                          variables = "SEX")),
    if (!is.null(adata)) list(analysis_data = adata)))
}

# code as one string, its layout's spaces and line breaks as one space
a2f_flat <- function(code) {
  txt <- gsub("\\s+", " ", paste(unlist(code), collapse = "\n"))
  gsub(" \\)", ")", gsub("\\( ", "(", txt))
}

a2f_join <- function(obj) {
  sprintf("inner_join(select(%s, USUBJID), by = \"USUBJID\", relationship = \"many-to-one\")", obj)
}

test_that("a report's subjects: its analysis set's, made by the data part (R1, R4)", {
  x <- a2f_spec()
  # no analysis set: none
  expect_null(.fig_subjects_part(x, "F-1", NA))
  expect_null(.fig_subjects_part(x, "F-1", NULL))
  # a set the study does not define: it stops, naming the set and the
  # study's sets (never every subject drawn: .tpl_population_check()'s rule)
  e <- tryCatch(.fig_subjects_part(x, "F-1", "NOPE"), error = identity)
  expect_s3_class(e, "tfl_fig_subjects")
  expect_match(conditionMessage(e), "F-1: the report's analysis set: 'NOPE' is not an analysis set",
               fixed = TRUE)
  expect_match(conditionMessage(e), "(SAF, ENR, PKS)", fixed = TRUE)
  # (a study that defines none: the same stop, the shared text: .pop_unknown_text())
  x0 <- x
  x0$populations <- NULL
  expect_error(.fig_subjects_part(x0, "F-1", "SAF"),
               "'SAF' is not an analysis set of spec/data.yml (it names none: register the set under `populations:`)",
               fixed = TRUE)
  # a set of ADSL with a condition: <dataset>_sub, as the ARD programs name it
  sj <- .fig_subjects_part(x, "F-1", "SAF")
  expect_identical(sj$subjects, list(object = "adsl_sub", dataset = "ADSL", id = "USUBJID"))
  # (lines only: no section heading, the program writes its sections)
  expect_false(any(grepl("^# ---- .* ----$", unlist(sj$code))))
  expect_true(any(grepl("^adsl_sub <- ", unlist(sj$code))))
  expect_match(a2f_flat(sj$code), "SAFFL == \"Y\"", fixed = TRUE)
  # a set of another dataset: that dataset's subjects
  sj <- .fig_subjects_part(x, "F-1", "PKS")
  expect_identical(sj$subjects$object, "adpp_sub")
  expect_identical(sj$subjects$dataset, "ADPP")
  expect_match(a2f_flat(sj$code), "PKFL == \"Y\"", fixed = TRUE)
  # a set with no condition (.pop_identity()): its dataset as it is
  sj <- .fig_subjects_part(x, "F-1", "ENR")
  expect_identical(sj$subjects$object, "adsl")
  expect_false(any(grepl("^adsl_sub <- ", unlist(sj$code))))
})

test_that("a design's read of a dataset is joined to the subjects; a record flag still filters", {
  subj <- list(object = "adsl_sub", dataset = "ADSL", id = "USUBJID")
  steps <- list(list(step = "read", dataset = "ADTTE"), list(step = "param", value = "OS"),
                list(step = "flag", variable = "ANL01FL"))
  r <- .fig_steps_code(steps, subjects = subj)
  txt <- a2f_flat(r$code)
  expect_match(txt, a2f_join("adsl_sub"), fixed = TRUE)
  # (first of the pipe: the records of the subjects, then the design's steps)
  expect_lt(regexpr("inner_join(", txt, fixed = TRUE), regexpr("PARAMCD == \"OS\"", txt, fixed = TRUE))
  expect_match(txt, "ANL01FL == \"Y\"", fixed = TRUE)
  expect_identical(r$reads, "ADTTE")
  # no analysis set: the dataset as it is, no join
  r0 <- .fig_steps_code(steps)
  expect_false(grepl("inner_join(", a2f_flat(r0$code), fixed = TRUE))
  expect_match(a2f_flat(r0$code), "ANL01FL == \"Y\"", fixed = TRUE)
  # a set of another dataset: joined to its subjects
  r <- .fig_steps_code(steps, subjects = list(object = "adpp_sub", dataset = "ADPP", id = "USUBJID"))
  expect_match(a2f_flat(r$code), a2f_join("adpp_sub"), fixed = TRUE)
  # the set's own dataset read: the subjects themselves, no join
  r <- .fig_steps_code(list(list(step = "read", dataset = "ADSL"),
                            list(step = "filter", expr = "AGE >= 18")), subjects = subj)
  expect_match(a2f_flat(r$code), "df <- adsl_sub |>", fixed = TRUE)
  expect_false(grepl("inner_join(", a2f_flat(r$code), fixed = TRUE))
})

test_that("a design's read of the report's analysis data: made by the data part, read as it is (subjects_in: none)", {
  x <- a2f_spec(adata = data.frame(output_id = "F-1", data_id = "adtte_all", from = "ADTTE",
                                   population_id = NA_character_, subjects_in = "none"))
  sj <- .fig_subjects_part(x, "F-1", "SAF", data = "adtte_all")
  code <- unlist(sj$code)
  expect_true(any(grepl("^adsl_sub <- ", code)))
  expect_true(any(grepl("^adtte_all <- ", code)))
  # (`subjects_in: none` keeps all its records: no join of its own)
  expect_false(any(grepl("inner_join", code[grepl("^adtte_all <- ", code)], fixed = TRUE)))
  # the figure reads it by name: no join added
  r <- .fig_steps_code(list(list(step = "read", data = "adtte_all")), subjects = sj$subjects)
  expect_identical(r$data, "adtte_all")
  expect_false(grepl("inner_join(", a2f_flat(r$code), fixed = TRUE))
})

test_that("one reader of a report's analysis set (#965's): its data's, its `common:`, its subjects entries'", {
  subj <- function(pop) list(id = "adsl_sub", from = "ADSL", population = pop)
  o <- list(kind = "figure", common = list(population = "C"),
            data = list(source = "code", population = "D"),
            ard = list(analysis_data = list(subj("E"))))
  # the data map's own first (.report_set(): that and `common:` alone)
  expect_identical(.out_population_or_data(o), "D")
  expect_identical(.report_set(o), "D")
  # then `common:` until it goes, then the subjects entries (Codex 8A: the
  # order of the two is the coordinator's)
  o$data$population <- NULL
  expect_identical(.out_population_or_data(o), "C")
  o$common <- NULL
  expect_identical(.out_population_or_data(o), "E")
  expect_true(is.na(.report_set(o)))
  o$ard$analysis_data <- list()
  expect_true(is.na(.out_population_or_data(o)))
  # a table's flat `data:` rows: its subjects entry (a records entry is not one)
  t <- list(kind = "table", data = list(subj("SAF"), list(id = "adae_sub", from = "ADAE",
                                                           subjects_in = "adsl_sub")))
  expect_identical(.out_population_or_data(t), "SAF")
  # entries naming several sets: none of them (A2: one set a report)
  t$data <- list(subj("SAF"), list(id = "adsl_itt", from = "ADSL", population = "ITT"))
  expect_true(is.na(.out_population_or_data(t)))
  t$common <- list(population = "C")
  expect_identical(.out_population_or_data(t), "C")
  # a records part's own map, else the report's `common:`
  expect_identical(.data_population(list(population = "PKS"), t), "PKS")
  expect_identical(.data_population(list(dataset = "ADAE"), t), "C")
  # the report programs': its spec's, by the same reader
  expect_identical(.rp_population(list(report = list(
    spec_output = list(data = list(source = "code", population = "SAF"))))), "SAF")
  expect_identical(.rp_population(list(report = list(
    spec_output = list(common = list(population = "SAF"))))), "SAF")
  expect_true(is.na(.rp_population(list(report = list(id = "F-1")))))
})

# ---- the report programs (the golden fixtures' edited study) ---------------

a2f_fx <- function() {
  dir <- test_path("fixtures", "programs-family")
  x <- readRDS(file.path(dir, "in", "edited.rds"))
  if (!is.null(x$study) && isTRUE(x$study_dir)) x$study$dir <- file.path(dir, "study", "edited")
  x
}

a2f_prog <- function(x, r) {
  code <- tfl_report_program(r, study = x$study, shared = x$shared, standards = x$standards,
                             attached = x$attached, study_code = x$study_code,
                             layout = x$layout, date = x$date, generator = "tflplanner 0.0.0",
                             todo = FALSE)
  strsplit(paste(code, collapse = "\n"), "\n", fixed = TRUE)[[1L]]
}

# a report of the fixture with its analysis set on its data (the canon's
# place), or in `common:` (until it goes)
a2f_pop <- function(r, pop, where = c("data", "common")) {
  where <- match.arg(where)
  r$spec_output <- if (where == "data") {
    list(id = r$id, kind = "figure", data = list(source = "code", population = pop))
  } else list(id = r$id, kind = "figure", common = list(population = pop))
  r
}

a2f_head <- "# the report's subjects: its analysis set (its data's population)"

test_that("a designed figure's program: the subjects in its data part, its reads joined to them", {
  x <- a2f_fx()
  r <- x$reports[["F-14-2-3"]]
  # without a set (the fixture's figure may carry one: the sample's sets are on
  # the data now)
  before <- a2f_prog(x, a2f_pop(r, NULL))
  expect_false(any(startsWith(before, a2f_head)))
  expect_false(grepl(a2f_join("adsl_sub"), a2f_flat(before), fixed = TRUE))
  p <- a2f_prog(x, a2f_pop(r, "SAF"))
  expect_true(any(startsWith(p, a2f_head)))
  i_sub <- which(startsWith(p, "adsl_sub <- "))
  i_rec <- which(startsWith(p, "adtte_sub <- adtte |>"))
  expect_length(i_sub, 1L)
  expect_lt(i_sub, i_rec[1L])
  expect_match(a2f_flat(p), a2f_join("adsl_sub"), fixed = TRUE)
  # one data section: the subjects part brings no heading of its own
  expect_identical(sum(startsWith(p, "# ---- data ")), 1L)
  expect_identical(sum(startsWith(before, "# ---- data ")), 1L)
  # the set in `common:` (until it goes): the same subjects, the same join
  expect_identical(a2f_prog(x, a2f_pop(r, "SAF", "common")), p)
  # a set the study does not define: no subjects, no join -- the program
  # stops there, naming the set and the study's sets (must-fix 1)
  p0 <- a2f_prog(x, a2f_pop(r, "NOPE"))
  at <- which(startsWith(p0, a2f_head))
  expect_length(at, 1L)
  expect_true(startsWith(p0[at + 1L],
                         "stop(\"F-14-2-3: the report's analysis set: 'NOPE' is not an analysis set"))
  expect_match(p0[at + 1L], "spec/data.yml (SAF", fixed = TRUE)
  expect_false(grepl("inner_join(select(", a2f_flat(p0), fixed = TRUE))
  expect_lt(at, which(startsWith(p0, "# ---- plot"))[1L])
  # no ARD definition to make the subjects from: the same, said so
  sj <- .rp_fig_subjects(list(report = list(id = "F-1", spec_output = list(
    data = list(source = "code", population = "SAF"))), shared = list()))
  expect_null(sj$subjects)
  expect_true(startsWith(sj$code, "stop(\"F-1: the report's analysis set SAF: its subjects are made from"))
  # no set: nothing
  expect_null(.rp_fig_subjects(list(report = list(id = "F-1"), shared = list())))
})

test_that("a figure of its own code: the subjects made before your code (Q5)", {
  x <- a2f_fx()
  p <- a2f_prog(x, a2f_pop(x$reports[["F-91"]], "SAF"))
  at <- which(startsWith(p, a2f_head))
  expect_length(at, 1L)
  expect_lt(at, which(startsWith(p, "# your code: leaves"))[1L])
  expect_true(any(startsWith(p, "adsl_sub <- ")))
  # a figure with no design yet: the same, before the plot to write
  p <- a2f_prog(x, a2f_pop(x$reports[["F-94"]], "SAF"))
  at <- which(startsWith(p, a2f_head))
  expect_length(at, 1L)
  expect_lt(at, which(startsWith(p, "# ---- the plot (ggplot2), written by hand"))[1L])
})

test_that("a figure of a `figure:` key: its data part is its ARD definition's, as before", {
  x <- a2f_fx()
  r <- x$reports[["F-9"]]
  before <- a2f_prog(x, r)
  expect_identical(a2f_prog(x, a2f_pop(r, "SAF")), before)
})

test_that("a figure of a `figure:` key: its content code is written with the report's study (its set)", {
  x <- a2f_fx()
  skip_if(is.null(x$study), "the fixture has no study")
  seen <- list()
  testthat::local_mocked_bindings(tfl_content_code = function(output, ..., study = NULL) {
    if (!is.null(output$figure)) seen[[length(seen) + 1L]] <<- list(study = study)
    "content <- NULL"
  })
  p <- a2f_prog(x, a2f_pop(x$reports[["F-9"]], "SAF"))
  expect_length(seen, 1L)
  expect_false(is.null(seen[[1L]]$study))
  expect_identical(seen[[1L]]$study, x$study)
  expect_true("content <- NULL" %in% p)
})

# ---- a figure made in a session (canon 11-15: every way applies the set) ----

test_that("a figure made in a session applies its report's set: tfl_content_code(), tfl_content_run(), tfl_run_output()", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("withr")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  # half the subjects out of the safety set
  adsl <- tflspec::adsl
  adsl$SAFFL <- ifelse(seq_len(nrow(adsl)) %% 2L == 0L, "Y", "N")
  saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
  adtte <- tflspec::adtte
  expect_true(any(!unique(adtte$USUBJID[adtte$PARAMCD == "OS"]) %in% saf))
  haven::write_xpt(.ai_chr(adsl), file.path(d, "adam", "adsl.xpt"))
  haven::write_xpt(.ai_chr(adtte), file.path(d, "adam", "adtte.xpt"))
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "XYZ"))
  out <- file.path(d, "spec", "outputs")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  # a designed figure of ADTTE's OS records, its analysis set on its data
  f <- tfl_output("F-1", "figure",
                  content = list(tfl_item("point", list(data = "df", x = "AVAL", y = "AVAL"))))
  f$data <- list(source = "code", population = "SAF",
                 steps = list(list(step = "read", dataset = "ADTTE"),
                              list(step = "param", value = "OS")))
  tfl_write_output(f, file.path(out, "F-1.yml"))
  st <- tfl_read_study_spec(d)
  ro <- tfl_resolve_output(tfl_read_outputs(d)[["F-1"]], st)
  ids_of <- function(p) {
    unique(unlist(c(list(if (is.data.frame(p$data)) p$data$USUBJID),
                    lapply(p$layers, function(l) if (is.data.frame(l$data)) l$data$USUBJID))))
  }
  # tfl_content_code(): the subjects part first (the program's), the read joined to it
  code <- tfl_content_code(ro, study = st)
  expect_identical(code[[1L]], a2f_head)
  expect_true(any(grepl("^adsl_sub <- ", code)))
  expect_match(a2f_flat(code), a2f_join("adsl_sub"), fixed = TRUE)
  # (with no study: its stop() line, never every subject)
  code0 <- tfl_content_code(ro)
  expect_identical(code0[[1L]], a2f_head)
  expect_true(startsWith(code0[[2L]], "stop(\"F-1: the report's analysis set SAF: its subjects are made"))
  expect_match(code0[[2L]], "give tfl_content_code() the study", fixed = TRUE)
  expect_false(grepl("inner_join(select(", a2f_flat(code0), fixed = TRUE))
  # tfl_content_run(): only the set's subjects drawn
  data <- .contents_fig_data(ro, st, d)
  p <- tfl_content_run(ro, data, study = st)
  ids <- ids_of(p)
  expect_true(length(ids) > 0L)
  expect_true(all(ids %in% saf))
  # (with no study: an error, never every subject)
  e <- tryCatch(tfl_content_run(ro, data), error = identity)
  expect_s3_class(e, "tfl_fig_subjects")
  expect_match(conditionMessage(e), "give tfl_content_run() the study", fixed = TRUE)
  # tfl_run_output(): its content as tfl_run_output() makes it, then its RTF
  p2 <- withr::with_dir(d, .output_build_content(tfl_read_outputs(d)[["F-1"]], st))
  expect_identical(sort(ids_of(p2)), sort(ids))
  expect_true(file.exists(tfl_run_output("F-1", d)))
  # a set the study does not define: each of the three stops (item 2's rule)
  f$data$population <- "NOPE"
  tfl_write_output(f, file.path(out, "F-1.yml"))
  ro <- tfl_resolve_output(tfl_read_outputs(d)[["F-1"]], st)
  code <- tfl_content_code(ro, study = st)
  expect_true(startsWith(code[[2L]], "stop(\"F-1: the report's analysis set"))
  expect_match(code[[2L]], "NOPE", fixed = TRUE)
  expect_false(grepl("inner_join(select(", a2f_flat(code), fixed = TRUE))
  e <- tryCatch(tfl_content_run(ro, data, study = st), error = identity)
  expect_s3_class(e, "tfl_fig_subjects")
  expect_match(conditionMessage(e), "NOPE", fixed = TRUE)
  expect_error(tfl_run_output("F-1", d), "NOPE", fixed = TRUE)
  # no set: as before, every record its steps keep, no study needed
  f$data$population <- NULL
  tfl_write_output(f, file.path(out, "F-1.yml"))
  ro <- tfl_resolve_output(tfl_read_outputs(d)[["F-1"]], st)
  expect_false(identical(tfl_content_code(ro)[[1L]], a2f_head))
  expect_true(any(!ids_of(tfl_content_run(ro, data)) %in% saf))
})
