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
  # no analysis set, or one the definition does not define: none
  expect_null(.fig_subjects_part(x, "F-1", NA))
  expect_null(.fig_subjects_part(x, "F-1", NULL))
  expect_null(.fig_subjects_part(x, "F-1", "NOPE"))
  # a set of ADSL with a condition: <dataset>_sub, as the ARD programs name it
  sj <- .fig_subjects_part(x, "F-1", "SAF")
  expect_identical(sj$subjects, list(object = "adsl_sub", dataset = "ADSL", id = "USUBJID"))
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
  before <- a2f_prog(x, r)
  expect_false(any(startsWith(before, a2f_head)))
  expect_false(grepl(a2f_join("adsl_sub"), a2f_flat(before), fixed = TRUE))
  p <- a2f_prog(x, a2f_pop(r, "SAF"))
  expect_true(any(startsWith(p, a2f_head)))
  i_sub <- which(startsWith(p, "adsl_sub <- "))
  i_rec <- which(startsWith(p, "adtte_sub <- adtte |>"))
  expect_length(i_sub, 1L)
  expect_lt(i_sub, i_rec[1L])
  expect_match(a2f_flat(p), a2f_join("adsl_sub"), fixed = TRUE)
  # the set in `common:` (until it goes): the same subjects, the same join
  expect_identical(a2f_prog(x, a2f_pop(r, "SAF", "common")), p)
  # a set the ARD definition does not define: as with none
  expect_identical(a2f_prog(x, a2f_pop(r, "NOPE")), before)
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
