# Branches of tfl_check_spec()'s checks added from 2026-11-24 that no other
# test reaches: the N check (R/check_spec_n.R), the header N
# (R/check_spec_header_n.R), the `!=` check (R/check_spec_ne_na.R), the `!r`
# walker (R/check_spec_r_parse.R), the population steps
# (R/check_spec_static.R), the study's code list references
# (R/check_spec_study_refs.R) and the define's codes
# (R/check_spec_define_codes.R).  Fictional data only.

# T-9: BIGN of ADSL's SAF subjects, the SYSBP summary of ADVS's records
c24_spec <- function() {
  tfl_ard_spec(list(
    datasets = data.frame(dataset = c("ADSL", "ADVS"), path = c("adsl.rds", "advs.rds")),
    populations = data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analysis_data = data.frame(output_id = "T-9", data_id = c("adsl_saf", "advs_saf"),
                               from = c("ADSL", "ADVS"), population_id = c("SAF", "SAF")),
    analyses = data.frame(output_id = "T-9", analysis_id = c("BIGN", "SYSBP"),
                          method = c("categorical", "continuous"), data = c("adsl_saf", "advs_saf"),
                          by = c(NA, "TRT01A"), variables = c("TRT01A", "AVAL"))))
}

c24_pops <- data.frame(population_id = "SAF", dataset = "ADSL")

# ---- the N check (R/check_spec_n.R) ---------------------------------------------

test_that("an analysis with no data and no dataset takes its rows from its population's dataset", {
  x <- list(populations = c24_pops)
  r <- list(data = NA, dataset = NA, population_id = "SAF")
  expect_identical(.n_rows_from(x, NULL, r), "ADSL")
})

test_that("an analysis with no data, no dataset and no population has rows of no dataset", {
  x <- list(populations = c24_pops)
  expect_identical(.n_rows_from(x, NULL, list(data = NA, dataset = NA)), NA_character_)
})

test_that("an analysis data sheet without code/then/distinct columns: the variables are all known", {
  ad <- data.frame(data_id = "adsl_saf", from = "ADSL")
  r <- data.frame(analysis_id = "SYSBP", data = NA)
  expect_identical(.n_known_vars(ad, r, c("TRT01A", "SEX")), c("TRT01A", "SEX"))
})

test_that("a subject set whose dataset cannot be read keeps no rows (not known)", {
  df <- data.frame(USUBJID = c("S1", "S2"))
  sets <- list(list(dataset = "ADSL", where = character()))
  expect_null(.n_keep_subjects(df, sets, "USUBJID", function(ds) NULL))
})

test_that("a subject set without the subject key keeps no rows (not known)", {
  df <- data.frame(USUBJID = c("S1", "S2"))
  sets <- list(list(dataset = "ADSL", where = character()))
  expect_null(.n_keep_subjects(df, sets, "USUBJID", function(ds) data.frame(SUBJID = "S1")))
})

test_that("a BIGN whose rows come from no dataset gives nothing to compare", {
  x <- c24_spec()
  x$analyses$data[1L] <- NA
  expect_length(.n_count_pairs(x, "T-9"), 0L)
})

test_that("the N check with data that cannot be read: no row", {
  x <- c24_spec()
  expect_length(.n_count_pairs(x, "T-9"), 1L)
  testthat::local_mocked_bindings(.ard_spec_of_study = function(...) list(spec = x))
  bad <- file.path(tempfile("cov1124-no-adam"), "C:", "nowhere")
  rows <- .check_n_rows(list(dir = tempdir()), bad, "T-9")
  expect_identical(nrow(rows), 0L)
  expect_named(rows, names(.check_rows()))
})

test_that("the N check's warning counts two subjects or more in the plural", {
  pr <- .n_count_pairs(c24_spec(), "T-9")[[1L]]
  dn <- data.frame(USUBJID = c("S1", "S2"), TRT01A = c("Drug X", "Control"), SAFFL = "Y")
  dr <- data.frame(USUBJID = c("S1", "S2"), TRT01A = c("Control", "Drug X"))
  p <- .n_pair_rows(pr, dn, dr, "USUBJID")
  expect_identical(nrow(p), 1L)
  expect_match(p$message, "they differ for 2 subjects of the data (S1: Drug X in ADSL, Control in ADVS)",
               fixed = TRUE)
})

# ---- the header N (R/check_spec_header_n.R) -------------------------------------

c24_header_out <- function(cols = TRUE, ard = NULL) {
  content <- list(tfl_item("plan_col_header", list(header = "{col}\n(N={n})")))
  if (cols) content <- c(list(tfl_item("table_plan", list(cols = "TRT01A"))), content)
  o <- tfl_output("T-9", "table", content = content)
  o$ard <- ard
  o
}

test_that("a header {n} with analyses that are not a list of entries: the report has no BIGN", {
  o <- c24_header_out(ard = list(analyses = data.frame(id = "BIGN")))
  p <- .check_header_n_rows(o, "T-9")
  expect_identical(nrow(p), 1L)
  expect_match(p$message, "the report has none", fixed = TRUE)
  expect_match(p$message, ", variables: [TRT01A]", fixed = TRUE)
})

test_that("a header {n} with no BIGN and no column variable names none in the analysis to add", {
  o <- c24_header_out(cols = FALSE, ard = list(analyses = list(list(id = "AGE", method = "continuous"))))
  p <- .check_header_n_rows(o, "T-9")
  expect_identical(nrow(p), 1L)
  expect_match(p$message, "method: categorical, data: <the subjects' data>` (the subjects", fixed = TRUE)
  expect_false(grepl("variables: [", p$message, fixed = TRUE))
})

# ---- the `!=` check (R/check_spec_ne_na.R) --------------------------------------

test_that("the `!=` check: an analysis of other reports only gives no condition to look at", {
  x <- c24_spec()
  x$analyses$where <- c(NA, "TRT01A != \"Control\"")
  testthat::local_mocked_bindings(.ard_spec_of_study = function(...) list(spec = x))
  adam <- list(ADSL = data.frame(USUBJID = "S1", SAFFL = "Y", TRT01A = NA_character_))
  rows <- .check_ne_na_rows(list(dir = tempdir()), adam, "T-OTHER", study_rows = FALSE)
  expect_identical(nrow(rows), 0L)
})

test_that("the study's data catalogue: an absolute path is read as it is, not under the study folder", {
  d <- tempfile("cov1124-adam")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "adsl.rds")
  saveRDS(data.frame(USUBJID = c("S1", "S2"), SAFFL = c("Y", NA)), f)
  study <- list(dir = file.path(d, "elsewhere"),
                data = list(datasets = list(list(dataset = "ADSL", path = normalizePath(f, "/")))))
  get <- .ne_na_reader(study, TRUE)
  expect_identical(get("adsl")$USUBJID, c("S1", "S2"))
})

test_that("the study's data catalogue: a Windows drive path that is not there gives no data", {
  study <- list(dir = tempdir(),
                data = list(datasets = list(list(dataset = "ADSL", path = "C:\\nowhere\\cov1124\\adsl.xpt"))))
  get <- .ne_na_reader(study, TRUE)
  expect_null(get("ADSL"))
})

test_that("a condition with no call in it (a name, a constant) has no `!=` part", {
  expect_length(.ne_na_hits("SAFFL"), 0L)
  expect_length(.ne_na_hits("TRUE"), 0L)
})

test_that("the same `var != value` twice in one condition is said once", {
  df <- data.frame(SEX = c("F", NA, "M"))
  m <- .ne_na_messages("SEX != \"M\" & SEX != \"M\"", df, "ADSL")
  expect_length(m, 1L)
  expect_match(m, "drops 1 row of ADSL where SEX is NA", fixed = TRUE)
})

# ---- the `!r` walker (R/check_spec_r_parse.R) -----------------------------------

test_that("a table's `data:` list of definitions is read by its fields of R", {
  o <- list(id = "T-9", data = list(list(id = "adsl_saf", from = "ADSL", where = "SAFFL ==")))
  p <- .r_walk_report(o, "T-9")
  expect_identical(nrow(p), 1L)
  expect_identical(c(p$part, p$item, p$level), c("data", "data$adsl_saf$where", "error"))
  expect_match(p$message, "`SAFFL ==` does not read as R", fixed = TRUE)
})

test_that("a study whose data.yml is not a map: its fields of R are none", {
  st <- structure(list(study = list(), data = "not a map"), class = "tfl_study_spec")
  expect_identical(nrow(.r_walk_study(st)), 0L)
})

test_that("an item's argument without a name is placed by its position", {
  x <- list(fn = "plan_filter", args = list(tfl_r("AGE >")))
  lv <- .r_leaves(x, "content", NULL)
  expect_length(lv, 1L)
  expect_identical(lv[[1L]]$at, c("content", "args", "1"))
})

test_that("a key of an item that is not its arguments is read with no slot kind", {
  x <- list(fn = "plan_filter", note = list(tfl_r("x +")))
  lv <- .r_leaves(x, "content", list(kind = "where"))
  expect_length(lv, 1L)
  expect_identical(lv[[1L]]$at, c("content", "note", "1"))
  expect_identical(lv[[1L]]$kinds, character())
})

test_that("a report's analysis step's key other than its analysis data and analyses is read as the rest", {
  # (its item named as the file writes it, `analysis`; part "ard", the ARD stage's, #3)
  p <- .r_walk_report(list(id = "T-9", ard = list(notes = tfl_r("1 +"))), "T-9")
  expect_identical(nrow(p), 1L)
  expect_identical(c(p$part, p$item), c("ard", "analysis$notes"))
})

test_that("an entry of a named map of definitions is placed by its name", {
  lv <- .r_entry_leaves(list(SAF = list(where = "SAFFL ==")), "populations")
  expect_identical(lv[[1L]]$at, c("populations", "SAF", "where"))
  expect_identical(lv[[1L]]$kinds, "where")
})

test_that("an entry that is R text itself, not a map, is read as R", {
  lv <- .r_entry_leaves(list(tfl_r("x ==")), "analyses")
  expect_length(lv, 1L)
  expect_identical(lv[[1L]]$at, c("analyses", "1"))
  expect_true(lv[[1L]]$tagged)
})

test_that("an entry's field without a name is placed by its position", {
  lv <- .r_entry_leaves(list(list(id = "a1", tfl_r("y <-"))), "analyses")
  expect_length(lv, 1L)
  expect_identical(lv[[1L]]$at, c("analyses", "a1", "2"))
})

test_that("an entry's named field that is not a field of R is placed by its name", {
  lv <- .r_entry_leaves(list(list(id = "a1", label = tfl_r("y <-"))), "analyses")
  expect_length(lv, 1L)
  expect_identical(lv[[1L]]$at, c("analyses", "a1", "label"))
  expect_identical(lv[[1L]]$kinds, character())
})

# ---- the population steps (R/check_spec_static.R) -------------------------------

test_that("a population that is not a map, or has no id, reads no variables", {
  pops <- list("SAF", list(where = "SAFFL == \"Y\""), list(id = "ITT", where = "ITTFL == \"Y\""))
  expect_identical(.check_pop_vars(pops), list(ITT = "ITTFL"))
})

test_that("a flag step with no variable keeps no population", {
  pops <- list(list(id = "SAF", where = "SAFFL == \"Y\""))
  expect_null(.step_keeps_population(list(step = "flag"), pops))
  expect_null(.step_keeps_population(list(step = "filter", expr = NA), pops))
})

# ---- the study's code lists (R/check_spec_study_refs.R) -------------------------

c24_study <- function() tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))

test_that("a report whose reference cannot be read is an error row and unresolved", {
  o <- tfl_output("T-9", "table")
  o$codelists <- list(SEX = list(list = "SEX", order = "F"))
  p <- tfl_check_study_refs(c24_study(), o)
  expect_identical(nrow(p), 1L)
  expect_identical(c(p$output_id, p$part, p$item, p$level), c("T-9", "codelists", "", "error"))
  expect_match(p$message, "`order` is not a key of a reference", fixed = TRUE)
  expect_identical(attr(p, "unresolved"), "T-9")
})

test_that("R text right under a report's key names the key alone as its place", {
  o <- tfl_output("T-9", "table")
  o$footnotes <- tfl_r("study$codelists$NOPE")
  p <- tfl_check_study_refs(c24_study(), o)
  expect_identical(nrow(p), 1L)
  expect_identical(c(p$item, p$list), c("footnotes", "NOPE"))
  expect_match(p$message, "the program would find no list there", fixed = TRUE)
})

# ---- the define's codes (R/check_spec_define_codes.R) ---------------------------

test_that("a report's data definitions that are not maps name no dataset", {
  o <- list(ard = list(analysis_data = list("adsl_saf", list(id = "advs_saf", from = "ADVS"))))
  expect_identical(.report_datasets(o), "ADVS")
})

test_that("a code list in place of bare values and {value} rows gives each code", {
  expect_identical(.inplace_codes(list("M", list(value = "F"), list(label = "none"))), c("M", "F"))
})
