# A listing's records -- a listing's, or a table of records' (#861's pk_conc)
# -- take the report's code lists on every column that has one, not only the
# columns the listing shows: a table of records makes its columns across of
# them, in the list's order.  A study's order is its ADaM metadata's
# (defaults.yml `adam_meta:`, put on as the data is read, #857); the
# report's list wins over it.  Package fictional data (adsl, adlb); the
# expected orders are the lists written here.

# `defaults`: lines added to defaults.yml; `levels`: study.yml's
# settings: adam_levels (none when not written)
ll_study <- function(defaults = NULL, levels = NULL) {
  skip_if_not_installed("haven")
  d <- file.path(tempfile("ll"), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adlb")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "LL-1"))
  if (!is.null(levels)) {
    st <- yaml::read_yaml(file.path(d, "study.yml"))
    st$settings$adam_levels <- levels
    yaml::write_yaml(st, file.path(d, "study.yml"))
  }
  if (length(defaults)) cat(c(defaults, ""), sep = "\n",
                            file = file.path(d, "spec", "defaults.yml"), append = TRUE)
  d
}

# ALT's visits as ADLB has them, the list's order their reverse (not the
# alphabetical order)
ll_visits <- function() {
  v <- unique(as.character(tflspec::adlb$AVISIT[tflspec::adlb$PARAMCD == "ALT"]))
  rev(sort(v[!is.na(v) & nzchar(v)]))
}
ll_list <- function(v) {
  paste0("AVISIT: [", paste0("{value: \"", v, "\"}", collapse = ", "), "]")
}
# the study's ADaM metadata: ADLB's AVISIT in the order `v`
ll_adam_meta <- function(v) {
  c("adam_meta:", "  variables:",
    "    - {dataset: ADLB, variable: AVISIT, codelist: CL.AVISIT}",
    "  codelists:",
    sprintf("    - {codelist: CL.AVISIT, name: Visit, value: \"%s\", order: %d}", v, seq_along(v)))
}

# a table of ALT's records, its visits across
ll_report <- function(d, id, codelists = NULL) {
  writeLines(c(paste("id:", id), "kind: table", "label: ALT by visit",
               "common: {population: SAF}", "extends: [defaults]",
               "data:", "  source: dataset", "  dataset: ADLB", "  population: SAF",
               "  where: !r 'PARAMCD == \"ALT\"'",
               if (length(codelists)) c("codelists:", paste0("  ", codelists)),
               "content:",
               "  - table_plan:", "      cols: AVISIT",
               "      rows: {subject: !r '~ \"{USUBJID}\"'}", "      value: AVAL",
               "  - plan_cells: {default: \"{value:.1f}\", na: \"-\"}"),
             file.path(d, "spec", "outputs", paste0(id, ".yml")))
}

ll_records <- function(d, id) {
  st <- tfl_read_study_spec(d)
  ro <- tfl_resolve_output(tfl_read_outputs(d)[[id]], st)
  list(st = st, ro = ro, recs = as.data.frame(tflspec:::.listing_data_of(ro, st, d)))
}

test_that("a listing's data part puts a code list on a column it does not show", {
  cat <- data.frame(dataset = "ADLB", path = "data/adlb.rds", derive = NA)
  sp <- tfl_listing_spec(data.frame(output_id = "L-1", dataset = "ADLB", sort = "USUBJID"),
                         data.frame(output_id = "L-1", vars = "USUBJID"), check = FALSE)
  cl <- data.frame(output_id = "L-1", variable = c("AVISIT", "AVISIT", "NOTHERE"),
                   value = c("WEEK 2", "BASELINE", "X"), label = NA, order = c("1", "2", "1"))
  code <- tfl_listing_code(sp, "L-1", cat, codelists = cl, part = "data")
  expect_true("cl_avisit <- c(\"WEEK 2\", \"BASELINE\")" %in% code)
  expect_true(any(grepl("set_levels(AVISIT = cl_avisit, NOTHERE = cl_nothere)", code, fixed = TRUE)))
  # run: the listed column a factor in the list's order; one without a list
  # as it was; a list of a column the records do not have changes nothing
  adlb <- data.frame(USUBJID = c("1", "2", "3"), AVISIT = c("BASELINE", "WEEK 2", "BASELINE"),
                     PARAMCD = c("B", "A", "C"), stringsAsFactors = FALSE)
  env <- new.env()
  eval(parse(text = tfl_helpers_code()), env)
  assign("adlb", adlb, envir = env)
  eval(parse(text = code[!grepl("readRDS|^# ---- input", code)]), env)
  out <- env$data
  expect_identical(levels(out$AVISIT), c("WEEK 2", "BASELINE"))
  expect_identical(as.character(out$AVISIT), adlb$AVISIT)
  expect_identical(out$PARAMCD, adlb$PARAMCD)
  expect_false("NOTHERE" %in% names(out))
})

test_that("a table of records: the report's code list orders its columns across", {
  skip_on_cran()
  skip_if_not_installed("cards")
  d <- ll_study()
  on.exit(unlink(dirname(d), recursive = TRUE), add = TRUE)
  v <- ll_visits()
  ll_report(d, "T-LV", ll_list(v))
  r <- ll_records(d, "T-LV")
  expect_identical(levels(r$recs$AVISIT), v)
  # a column without a list stays as it was
  expect_true(is.character(r$recs$PARAMCD))
  expect_true(is.character(r$recs$USUBJID))
  # the program writes it: the list at its head, set_levels() on the records
  code <- tfl_data_code("T-LV", r$st, ard = FALSE)$data
  expect_true(any(startsWith(code, "cl_avisit <- ")))
  expect_true(any(grepl("set_levels(AVISIT = cl_avisit)", code, fixed = TRUE)))
  # the table's columns in the list's order
  skip_if_not_installed("rtfreporter")
  p <- tfl_content_run(r$ro, r$recs)
  t <- as.data.frame(rtfreporter::plan_apply(p, "table"))
  expect_identical(intersect(names(t), v), v)
})

test_that("a table of records: the study's ADaM metadata orders it, the report's list wins; none, as read", {
  skip_on_cran()
  skip_if_not_installed("cards")
  v <- ll_visits()
  # the study's display lists (defaults.yml codelists:) are lists by name and
  # reach no report (#857); its ADaM metadata orders the records
  d <- ll_study(defaults = c(ll_adam_meta(v), "codelists:", paste0("  ", ll_list(rev(v)))),
                levels = "define_xml")
  on.exit(unlink(dirname(d), recursive = TRUE), add = TRUE)
  ll_report(d, "T-LS")
  expect_identical(levels(ll_records(d, "T-LS")$recs$AVISIT), v)
  # the report's own list over the study's
  ll_report(d, "T-LR", ll_list(rev(v)))
  expect_identical(levels(ll_records(d, "T-LR")$recs$AVISIT), rev(v))
  # a study without either: the records' visits as read
  d2 <- ll_study()
  on.exit(unlink(dirname(d2), recursive = TRUE), add = TRUE)
  ll_report(d2, "T-LN")
  recs <- ll_records(d2, "T-LN")$recs
  expect_true(is.character(recs$AVISIT))
  expect_false(any(grepl("set_levels(", tfl_data_code("T-LN", d2, ard = FALSE)$data, fixed = TRUE)))
})
