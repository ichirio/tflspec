# A lab shift table by direction through a SPEC: the ARD definition makes
# the records toward low and toward high (ADaM's ATOXGRL / BTOXGRL and
# ATOXGRH / BTOXGRH) with a direction column, and the content's
# normalize_ard(combine = ) reads the two directions' variables as one
# table dimension (rtfreporter#1081).  And a `by` / `variables` / `strata`
# cell that lists its columns with commas or spaces instead of `|`.

sc_combine <- list(worst_gr = c("ATOXGRL", "ATOXGRH"),
                   base_gr = c("BTOXGRL", "BTOXGRH"))

# Fictional records: Albumin has a grade toward low only, Potassium both
sc_adam <- function() {
  gr <- function(x) factor(x, levels = c("0", "1", "2", "3"))
  adsl <- data.frame(USUBJID = sprintf("X-%02d", 1:6), SAFFL = "Y")
  adlb <- data.frame(
    USUBJID = rep(adsl$USUBJID, 2),
    PARAM = rep(c("Albumin", "Potassium"), each = 6),
    BTOXGRL = gr(c("0", "0", "1", "0", "2", "0", "0", "1", "0", "0", "0", "0")),
    ATOXGRL = gr(c("1", "0", "1", "2", "2", "0", "0", "1", "1", "0", "0", "0")),
    BTOXGRH = gr(c(NA, NA, NA, NA, NA, NA, "0", "0", "0", "1", "0", "0")),
    ATOXGRH = gr(c(NA, NA, NA, NA, NA, NA, "0", "2", "0", "1", "3", "0")))
  list(ADSL = adsl, ADLB = adlb)
}

sc_df <- function(...) as.data.frame(list(...), stringsAsFactors = FALSE)

sc_ard_spec <- function(by = c("PARAM | tox_dir | BTOXGRL", "PARAM | tox_dir | BTOXGRH"),
                        strata = NA) {
  list(
    study = sc_df(key = "id", value = "USUBJID"),
    datasets = sc_df(dataset = c("ADSL", "ADLB"),
                     path = c("adam/ADSL.rds", "adam/ADLB.rds")),
    populations = sc_df(population_id = "SAF", dataset = "ADSL",
                        where = "SAFFL == \"Y\""),
    analysis_data = sc_df(output_id = "T", data_id = c("adsl_sub", "adlb_lo", "adlb_hi"),
                          from = c("ADSL", "ADLB", "ADLB"), population_id = "SAF",
                          where = c(NA, "!is.na(BTOXGRL)", "!is.na(BTOXGRH)"),
                          derive = c(NA, "tox_dir = \"Low\"", "tox_dir = \"High\"")),
    analyses = sc_df(output_id = "T", analysis_id = c("LO", "HI"), method = "categorical",
                     data = c("adlb_lo", "adlb_hi"), by = by,
                     variables = c("ATOXGRL", "ATOXGRH"), strata = strata))
}

sc_output <- function(combine = sc_combine) {
  tfl_output("T", "table", content = list(
    tfl_item("normalize_ard", list(combine = combine)),
    tfl_item("table_plan", list(cols = "base_gr",
                                rows = list(PARAM = "PARAM", tox_dir = "tox_dir"))),
    tfl_item("plan_levels", list(tox_dir = list("Low", "High"))),
    tfl_item("plan_cells", list(default = "{n}", notes = FALSE))))
}

test_that("normalize_ard(combine = ): the schema's entry and the code", {
  expect_identical(nrow(tfl_schema_check(tfl_item("normalize_ard", list(combine = sc_combine)))), 0L)
  # (a list of each new name's variables, as a YAML file reads it)
  expect_identical(nrow(tfl_schema_check(tfl_item("normalize_ard", list(
    combine = list(worst_gr = list("ATOXGRL", "ATOXGRH")))))), 0L)
  # a bare variable is not a map of new names
  p <- tfl_schema_check(tfl_item("normalize_ard", list(combine = "ATOXGRL")))
  expect_identical(p$arg, "combine")
  expect_match(p$message, "map<string, list<column>>", fixed = TRUE)
  expect_match(tfl_schema("normalize_ard")$args$combine$help, "ATOXGRL", fixed = TRUE)
  code <- paste(tfl_content_code(sc_output(), pipe = "|>"), collapse = "\n")
  expect_match(code, paste0("normalize_ard(\n    combine = list(worst_gr = c(\"ATOXGRL\", \"ATOXGRH\"), ",
                            "base_gr = c(\"BTOXGRL\", \"BTOXGRH\"))\n  )"), fixed = TRUE)
  skip_if_not_installed("rtfreporter")
  skip_if_not("combine" %in% names(formals(rtfreporter::normalize_ard)),
              "rtfreporter without normalize_ard(combine = ) (rtfreporter#1081)")
  expect_true(all(names(tfl_schema("normalize_ard")$args) %in%
                    names(formals(rtfreporter::normalize_ard))))
})

test_that("a shift table by direction: the ARD definition and the content, run", {
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not("combine" %in% names(formals(rtfreporter::normalize_ard)),
              "rtfreporter without normalize_ard(combine = ) (rtfreporter#1081)")
  adam <- sc_adam()
  dir <- tempfile("shift")
  dir.create(file.path(dir, "adam"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  for (nm in names(adam)) saveRDS(adam[[nm]], file.path(dir, "adam", paste0(nm, ".rds")))
  s <- tfl_ard_spec(sc_ard_spec())
  code <- paste(tfl_ard_code(s, save = FALSE, part = "body"), collapse = "\n")
  expect_match(code, "by = c(PARAM, tox_dir, BTOXGRL)", fixed = TRUE)
  expect_match(code, "mutate(tox_dir = \"High\")", fixed = TRUE)
  ard <- tfl_build_ard(s, dir = dir, save = FALSE)
  if (!is.data.frame(ard)) ard <- ard[[1L]]
  env <- new.env()
  env$ard <- ard
  eval(parse(text = c("library(rtfreporter)", tfl_content_code(sc_output(), pipe = "|>"))), env)
  tab <- as.data.frame(suppressMessages(rtfreporter::plan_apply(env$plan, "table")))
  # the baseline grade across, the worst grade down, Low before High
  expect_identical(unique(paste(tab$PARAM, tab$tox_dir)),
                   c("Albumin Low", "Potassium Low", "Potassium High"))
  # every count against the records themselves
  for (dir_ in c("Low", "High")) {
    sfx <- if (dir_ == "Low") "L" else "H"
    rec <- adam$ADLB[!is.na(adam$ADLB[[paste0("BTOXGR", sfx)]]), ]
    for (k in seq_len(nrow(tab))[tab$tox_dir == dir_]) {
      r <- rec[rec$PARAM == tab$PARAM[k] &
                 as.character(rec[[paste0("ATOXGR", sfx)]]) == tab$label[k], ]
      for (b in intersect(c("0", "1", "2", "3"), names(tab))) {
        expect_identical(as.integer(tab[[b]][k]),
                         sum(as.character(r[[paste0("BTOXGR", sfx)]]) == b),
                         info = paste(tab$PARAM[k], dir_, tab$label[k], b))
      }
    }
  }
})

test_that("a by / variables / strata cell lists its columns with `|`, not commas or spaces", {
  err <- function(...) tryCatch({
    tfl_ard_spec(sc_ard_spec(...))
    ""
  }, error = function(e) conditionMessage(e))
  # commas: written as it is, `by = PARAM, tox_dir, BTOXGRL` would give
  # ard_tabulate() tox_dir as its `strata` (by position)
  m <- err(by = c("PARAM, tox_dir, BTOXGRL", "PARAM | tox_dir | BTOXGRH"))
  expect_match(m, "T / LO: `by` lists its columns with `|`: PARAM | tox_dir | BTOXGRL, not 'PARAM, tox_dir, BTOXGRL'",
               fixed = TRUE)
  expect_false(grepl("T / HI", m, fixed = TRUE))
  # spaces (a cell that would not parse), mixed with `|`
  m <- err(by = c("PARAM | tox_dir BTOXGRL", "PARAM | tox_dir | BTOXGRH"))
  expect_match(m, "PARAM | tox_dir | BTOXGRL, not 'tox_dir BTOXGRL'", fixed = TRUE)
  m <- err(strata = c("SEX,AGEGR1", NA))
  expect_match(m, "`strata` lists its columns with `|`: SEX | AGEGR1", fixed = TRUE)
  s <- sc_ard_spec()
  s$analyses$variables[1L] <- "ATOXGRL\nBTOXGRL"
  m <- tryCatch(tfl_ard_spec(s), error = function(e) conditionMessage(e))
  expect_match(m, "`variables` lists its columns with `|`: ATOXGRL | BTOXGRL", fixed = TRUE)
  # one R expression each piece: a selector, a vector, a hierarchy
  for (v in c("c(ATOXGRL, BTOXGRL)", "starts_with(\"ATOX\")", "ATOXGRL | BTOXGRL",
              "where(is.factor)", "-USUBJID")) {
    s <- sc_ard_spec()
    s$analyses$variables[1L] <- v
    expect_no_error(tfl_ard_spec(s))
  }
  expect_identical(err(), "")
})
