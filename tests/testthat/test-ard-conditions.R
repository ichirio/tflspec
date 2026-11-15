# An item's params and conditions: one process (where, derive, levels, one
# or more calls) run once for each row of its conditions table -- the
# table (ard_<id>_conditions), the process as a function of one row
# (ard_<id>_one) and purrr::pmap() over the table

cd_df <- function(rows, cols) {
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) as.character(r[[cn]] %||% NA), "")), stringsAsFactors = FALSE)
}

cd_chem6 <- c("Alanine Aminotransferase (U/L)", "Albumin (g/L)", "Aspartate Aminotransferase (U/L)",
              "Creatinine (umol/L)", "Potassium (mmol/L)", "Sodium (mmol/L)")
cd_low <- cd_chem6[c(2L, 5L, 6L)]
cd_high <- cd_chem6[c(1L, 3L, 4L, 5L, 6L)]

# fictional worst post-baseline chemistry: 40 subjects, six parameters, a
# grade in each direction the parameter is graded in
cd_adlb <- function() {
  set.seed(20261116)
  d <- expand.grid(USUBJID = sprintf("S%03d", 1:40), PARAM = cd_chem6, stringsAsFactors = FALSE)
  g <- function(n) as.character(sample(0:4, n, TRUE, prob = c(.6, .2, .1, .07, .03)))
  n <- nrow(d)
  d$LBTOXL <- ifelse(d$PARAM %in% cd_low, "LOW", "")
  d$LBTOXH <- ifelse(d$PARAM %in% cd_high, "HIGH", "")
  d$ATOXGRL <- ifelse(d$PARAM %in% cd_low, g(n), NA)
  d$BTOXGRL <- ifelse(d$PARAM %in% cd_low, g(n), NA)
  d$ATOXGRH <- ifelse(d$PARAM %in% cd_high, g(n), NA)
  d$BTOXGRH <- ifelse(d$PARAM %in% cd_high, g(n), NA)
  d
}

cd_cond_rows <- function(id, rows) {
  out <- list()
  for (k in seq_along(rows)) for (p in names(rows[[k]])) {
    out[[length(out) + 1L]] <- list(output_id = "T", analysis_id = id, row = as.character(k),
                                    param = p, value = rows[[k]][[p]])
  }
  out
}

cd_shift_rows <- function() {
  gr <- "0 | 1 | 2 | 3 | 4"
  first <- list(output_id = "T", analysis_id = "SHIFT", label = "Shift in each direction",
                method = "categorical", dataset = "ADLB", where = "{where}",
                derive = "tox_dir = \"{tox_dir}\"",
                levels = paste0("PARAM = {graded_params}; tox_dir = {tox_dir}; ATOXGRL = ", gr,
                                "; BTOXGRL = ", gr, "; ATOXGRH = ", gr, "; BTOXGRH = ", gr),
                params = "tox_dir | graded_params | worst | base | where",
                by = "PARAM | tox_dir | {base}", variables = "{worst}", statistics = "n | p",
                denominator = "n_subj")
  call <- function(by, v) list(output_id = "T", analysis_id = "SHIFT", method = "categorical",
                               by = by, variables = v, statistics = "n | p", denominator = "n_subj")
  list(first, call("PARAM | tox_dir", "{worst}"), call("PARAM | tox_dir", "{base}"),
       call("PARAM", "tox_dir"))
}

cd_shift_conditions <- function() {
  cd_cond_rows("SHIFT", list(
    list(tox_dir = "Low", graded_params = paste(cd_low, collapse = " | "), worst = "ATOXGRL",
         base = "BTOXGRL", where = "!r LBTOXL != \"\" & !is.na(BTOXGRL) & !is.na(ATOXGRL)"),
    list(tox_dir = "High", graded_params = paste(cd_high, collapse = " | "), worst = "ATOXGRH",
         base = "BTOXGRH", where = "!r LBTOXH != \"\" & !is.na(BTOXGRH) & !is.na(ATOXGRH)")))
}

cd_spec <- function(analyses, conditions = list(), check = TRUE, more = list()) {
  S <- .ard_spec_sheets
  s <- c(list(
    study = cd_df(list(list(key = "id", value = "USUBJID")), S$study),
    datasets = cd_df(list(list(dataset = "ADLB", path = "adlb.rds"),
                          list(dataset = "ADSL", path = "adsl.rds")), S$datasets),
    objects = cd_df(list(list(output_id = "T", object_id = "n_subj",
                              code = "dplyr::n_distinct(adlb$USUBJID)")), S$objects),
    analyses = cd_df(analyses, S$analyses),
    conditions = cd_df(conditions, S$conditions)), more)
  if (check) tfl_ard_spec(s) else s
}

cd_dir <- function(adlb = cd_adlb(), adsl = NULL) {
  dir <- withr_tempdir(parent.frame())
  saveRDS(adlb, file.path(dir, "adlb.rds"))
  saveRDS(adsl %||% data.frame(USUBJID = "S001"), file.path(dir, "adsl.rds"))
  dir
}

test_that("Lab shift: the table, the function of one row, pmap(); readable lines", {
  sp <- cd_spec(cd_shift_rows(), cd_shift_conditions())
  code <- tfl_ard_code(sp, output_id = "T", save = FALSE, part = "body")
  txt <- paste(code, collapse = "\n")
  expect_match(txt, "ard_shift_conditions <- tribble(\n  ~tox_dir, ~graded_params, ~worst, ~base, ~where,",
               fixed = TRUE)
  expect_match(txt, "quote(LBTOXL != \"\" & !is.na(BTOXGRL) & !is.na(ATOXGRL))", fixed = TRUE)
  expect_match(txt, "c(\"Albumin (g/L)\", \"Potassium (mmol/L)\", \"Sodium (mmol/L)\")", fixed = TRUE)
  expect_match(txt, "ard_shift_one <- function(tox_dir, graded_params, worst, base, where) {",
               fixed = TRUE)
  expect_match(txt, "filter(!!where)", fixed = TRUE)
  expect_match(txt, "mutate(tox_dir = !!tox_dir)", fixed = TRUE)
  expect_match(txt, "PARAM = graded_params,", fixed = TRUE)
  expect_match(txt, "tox_dir = tox_dir,", fixed = TRUE)
  expect_match(txt, "by = c(PARAM, tox_dir, all_of(base))", fixed = TRUE)
  expect_match(txt, "variables = all_of(worst)", fixed = TRUE)
  expect_match(txt, "ard_shift <- purrr::pmap(ard_shift_conditions, ard_shift_one) |>\n  bind_rows()",
               fixed = TRUE)
  # one tag for the item, its four calls bound
  expect_identical(lengths(regmatches(txt, gregexpr("tag_ard(report_id, \"SHIFT\")", txt, fixed = TRUE))), 1L)
  expect_identical(lengths(regmatches(txt, gregexpr("ard_tabulate(", txt, fixed = TRUE))), 4L)
  # (not expanded a row at a time; the derived column is no input's)
  expect_false(grepl("\"Low\"", sub("(?s)^.*ard_shift_one", "", txt, perl = TRUE)))
  expect_false(any(grepl("tox_dir", grep("order_levels", code, value = TRUE), fixed = TRUE)))
  expect_true(all(nchar(code) <= 100L))
  expect_true(is.language(str2lang(paste0("{", txt, "\n}"))))
})

test_that("Lab shift: 576 rows, the needed ones; a direction's pair with no record is 0", {
  skip_if_not_installed("cards")
  skip_if_not_installed("purrr")
  adlb <- cd_adlb()
  dir <- cd_dir(adlb)
  sp <- cd_spec(cd_shift_rows(), cd_shift_conditions())
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, output_id = "T", save = FALSE))
  # 8 graded pairs x (25 cells + 5 + 5 + the corner) x n, p
  expect_identical(nrow(ard), 576L)
  expect_identical(unique(ard$analysis_id), "SHIFT")
  key <- vapply(seq_len(nrow(ard)), function(i) paste(
    unlist(ard$group1_level[i]), unlist(ard$group2_level[i]), ard$group3[i] %||% "",
    unlist(ard$group3_level[i]), ard$variable[i], unlist(ard$variable_level[i]), ard$stat_name[i]), "")
  expect_false(anyDuplicated(key) > 0L)
  # each cell's n: the records of the pair, by hand
  n_of <- function(p, b, w) {
    d <- adlb[adlb$PARAM == p & adlb$LBTOXH != "" & !is.na(adlb$BTOXGRH) & !is.na(adlb$ATOXGRH), ]
    sum(d$BTOXGRH == b & d$ATOXGRH == w)
  }
  cell <- ard[ard$variable == "ATOXGRH" & !is.na(ard$group3) & ard$stat_name == "n", ]
  lv <- function(v) vapply(v, function(z) as.character(z %||% NA), "")
  got <- unlist(cell$stat)
  want <- mapply(n_of, lv(cell$group1_level), lv(cell$group3_level), lv(cell$variable_level))
  expect_equal(as.numeric(got), as.numeric(want))
  # no ALT Low (not graded low): none of its rows
  expect_false(any(lv(ard$group1_level) == cd_chem6[1L] & lv(ard$group2_level) %in% "Low"))
  # Sodium High with no record: its rows are there, every n 0
  a2 <- adlb[!(adlb$PARAM == "Sodium (mmol/L)"), ]
  a2 <- rbind(a2, transform(adlb[adlb$PARAM == "Sodium (mmol/L)", ], LBTOXH = ""))
  dir2 <- cd_dir(a2)
  ard2 <- suppressMessages(tfl_build_ard(sp, dir = dir2, output_id = "T", save = FALSE))
  expect_identical(nrow(ard2), 576L)
  na_hi <- ard2[lv(ard2$group1_level) == "Sodium (mmol/L)" & ard2$stat_name == "n" &
                  (lv(ard2$group2_level) %in% "High" | lv(ard2$variable_level) %in% "High"), ]
  expect_gt(nrow(na_hi), 0L)
  expect_true(all(unlist(na_hi$stat) == 0))
  # a record of a parameter the direction does not grade stops (its levels)
  a3 <- adlb
  i <- which(a3$PARAM == cd_chem6[1L])[1L]
  a3$LBTOXL[i] <- "LOW"
  a3$ATOXGRL[i] <- "1"
  a3$BTOXGRL[i] <- "0"
  dir3 <- cd_dir(a3)
  expect_error(suppressMessages(tfl_build_ard(sp, dir = dir3, output_id = "T", save = FALSE)),
               "PARAM has values its code list does not")
})

test_that("Overall response by subgroup: each level's rate and CI, as by hand", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("purrr")
  adsl <- as.data.frame(cards::ADSL)
  adsl$RESP <- ifelse(!is.na(adsl$BMIBL) & adsl$BMIBL >= 25, "Response", "Non-response")
  dir <- cd_dir(adsl = adsl)
  orr <- list(output_id = "T", analysis_id = "ORR", label = "Response rate in each subgroup",
              method = "proportion_ci", dataset = "ADSL", where = "{where}",
              derive = "subgroup = \"{subgroup}\" | sublevel = \"{sublevel}\"",
              params = "subgroup | sublevel | where | conf",
              by = "subgroup | sublevel | TRT01A", variables = "RESP",
              args = "value = list(RESP = \"Response\"), method = \"clopper-pearson\", conf.level = {conf}",
              post = "dplyr::filter(stat_name %in% c(\"N\", \"n\", \"estimate\", \"conf.low\", \"conf.high\"))")
  conds <- cd_cond_rows("ORR", list(
    list(subgroup = "ALL", sublevel = "ALL", where = "!r TRUE", conf = "!r 0.9"),
    list(subgroup = "SEX", sublevel = "F", where = "!r SEX == \"F\"", conf = "!r 0.9"),
    list(subgroup = "SEX", sublevel = "M", where = "!r SEX == \"M\"", conf = "!r 0.9"),
    list(subgroup = "AGEGR1", sublevel = "<65", where = "!r AGEGR1 == \"<65\"", conf = "!r 0.9")))
  sp <- cd_spec(list(orr), conds)
  code <- paste(tfl_ard_code(sp, output_id = "T", save = FALSE, part = "body"), collapse = "\n")
  expect_match(code, "\"ALL\", \"ALL\", quote(TRUE), 0.9,", fixed = TRUE)
  expect_match(code, "mutate(subgroup = !!subgroup, sublevel = !!sublevel)", fixed = TRUE)
  expect_match(code, "conf.level = conf", fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, output_id = "T", save = FALSE))
  lv <- function(v) vapply(v, function(z) as.character(z %||% NA), "")
  for (k in 1:4) {
    w <- c("TRUE", "SEX == \"F\"", "SEX == \"M\"", "AGEGR1 == \"<65\"")[k]
    sl <- c("ALL", "F", "M", "<65")[k]
    d <- adsl[eval(str2lang(w), adsl), ]
    for (arm in unique(d$TRT01A)) {
      x <- sum(d$RESP[d$TRT01A == arm] == "Response")
      n <- sum(d$TRT01A == arm)
      ci <- stats::binom.test(x, n, conf.level = 0.9)$conf.int
      got <- ard[lv(ard$group2_level) == sl & lv(ard$group3_level) == arm, ]
      st <- function(s) as.numeric(unlist(got$stat[got$stat_name == s]))
      expect_equal(st("estimate"), x / n, info = paste(sl, arm))
      expect_equal(st("conf.low"), ci[1L], tolerance = 1e-8, info = paste(sl, arm))
      expect_equal(st("conf.high"), ci[2L], tolerance = 1e-8, info = paste(sl, arm))
    }
  }
  expect_setequal(unique(ard$stat_name), c("N", "n", "estimate", "conf.low", "conf.high"))
  # forgetting the subgroup's columns in by: the same keys in each run, stopped
  orr2 <- orr
  orr2$by <- "TRT01A"
  orr2$derive <- NA
  orr2$params <- "where | conf"
  conds2 <- Filter(function(r) r$param %in% c("where", "conf"), conds)
  # (with another item: the report's items bound by bind_items(), which checks)
  n_arm <- list(output_id = "T", analysis_id = "N", method = "categorical", dataset = "ADSL",
                variables = "TRT01A")
  sp2 <- cd_spec(list(orr2, n_arm), conds2)
  expect_error(suppressMessages(tfl_build_ard(sp2, dir = dir, output_id = "T", save = FALSE)),
               "a statistic twice in one item")
})

test_that("an item's calls without conditions; derive and levels on one call", {
  skip_if_not_installed("cards")
  adlb <- cd_adlb()
  dir <- cd_dir(adlb)
  r <- cd_shift_rows()
  r <- lapply(r, function(z) {
    z$params <- NULL
    z
  })
  r[[1L]]$where <- "LBTOXH != \"\" & !is.na(BTOXGRH) & !is.na(ATOXGRH)"
  r[[1L]]$derive <- "tox_dir = \"High\""
  r[[1L]]$levels <- "tox_dir = High; ATOXGRH = 0 | 1 | 2 | 3 | 4; BTOXGRH = 0 | 1 | 2 | 3 | 4"
  r[[1L]]$by <- "PARAM | tox_dir | BTOXGRH"
  r[[1L]]$variables <- "ATOXGRH"
  r[[2L]]$variables <- "ATOXGRH"
  r[[3L]]$variables <- "BTOXGRH"
  sp <- cd_spec(r)
  code <- tfl_ard_code(sp, output_id = "T", save = FALSE, part = "body")
  txt <- paste(code, collapse = "\n")
  expect_match(txt, "ard_shift_data <- adlb_sub |>", fixed = TRUE)
  expect_match(txt, "ard_shift <- bind_rows(", fixed = TRUE)
  expect_false(grepl("pmap", txt, fixed = TRUE))
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, output_id = "T", save = FALSE))
  # 5 graded parameters x 36 statistics x n, p
  expect_identical(nrow(ard), 360L)
  # one call: one pipe, its derive and levels steps before the call
  one <- cd_spec(list(r[[1L]]))
  txt1 <- paste(tfl_ard_code(one, output_id = "T", save = FALSE, part = "body"), collapse = "\n")
  expect_match(txt1, "mutate(tox_dir = \"High\") |>\n  set_levels(", fixed = TRUE)
})

test_that("the checks: a {name} not a param, rows and params, groups, R in a value field", {
  rows <- cd_shift_rows()
  conds <- cd_shift_conditions()
  bad <- rows
  bad[[1L]]$by <- "PARAM | tox_dir | {baseline}"
  expect_error(cd_spec(bad, conds), "uses \\{baseline\\}, not a param")
  expect_error(cd_spec(rows), "`params` without `conditions`")
  nop <- rows
  nop[[1L]]$params <- NA
  expect_error(cd_spec(nop, conds), "`conditions` without `params`")
  miss <- Filter(function(r) !(r$row == "2" && r$param == "base"), conds)
  expect_error(cd_spec(rows, miss), "condition 2 gives no base")
  g <- rows
  g[[1L]]$groups <- "AGE"
  expect_error(cd_spec(g, conds, check = TRUE), "`groups` and `params`")
  mixed <- rows
  mixed[[1L]]$by <- "PARAM | tox_dir | {base}FL"
  expect_error(cd_spec(mixed, conds), "a column is a whole \\{param\\}")
  ina <- rows
  ina[[2L]]$args <- "na.rm = {where}"
  expect_error(cd_spec(ina, conds), "an R expression \\(!r\\)")
  later <- rows
  later[[3L]]$where <- "TRUE"
  expect_error(cd_spec(later, conds), "on a row after an item's first")
  apart <- c(rows[1:2], list(list(output_id = "T", analysis_id = "OTHER", method = "categorical",
                                  dataset = "ADLB", variables = "PARAM")), rows[3:4])
  expect_error(cd_spec(apart, conds), "the rows of an item's ards are one after another")
  nm <- rows
  nm[[1L]]$params <- "tox_dir | graded_params | worst | base | where | data"
  expect_error(cd_spec(nm, conds), "params data")
  nob <- rows
  nob[[1L]]$by <- "PARAM | {base}"
  nob[[1L]]$params <- NA
  expect_error(cd_spec(nob, list()), "declares no `params`")
})

test_that("the warnings: a key not a param, a . key, a param not used, a blank by value", {
  rows <- cd_shift_rows()
  conds <- c(cd_shift_conditions(), cd_cond_rows("SHIFT", list(
    list(note = "x", .later = "y"), list())))
  conds <- lapply(conds, function(r) {
    if (r$param %in% c("note", ".later")) r$row <- "1"
    r
  })
  w <- character()
  withCallingHandlers(cd_spec(rows, conds), warning = function(e) {
    w <<- c(w, conditionMessage(e))
    invokeRestart("muffleWarning")
  })
  w <- paste(w, collapse = "\n")
  expect_match(w, "conditions give note, not a param: left out of the program's table")
  expect_match(w, "conditions give .later: a key starting with `.` is tflspec's own")
  code <- suppressWarnings(tfl_ard_code(cd_spec(rows, conds), output_id = "T", save = FALSE))
  expect_false(any(grepl("note", code, fixed = TRUE)))
  un <- rows
  un[[1L]]$params <- "tox_dir | graded_params | worst | base | where | spare"
  conds2 <- c(cd_shift_conditions(), cd_cond_rows("SHIFT", list(list(spare = "1"), list(spare = "2"))))
  expect_warning(cd_spec(un, conds2), "param spare is not used by the process")
  blank <- cd_shift_conditions()
  blank[[1L]]$value <- ""
  expect_warning(cd_spec(rows, blank), "is \"\" in a condition and makes a `by` column")
})

test_that("YAML: params, conditions, ards, derive and levels <-> the sheets, and back", {
  d <- withr_tempdir()
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"), full.names = TRUE),
            d, recursive = TRUE)
  y <- c(
    "id: T-SHIFT", "kind: table", "label: Shift",
    "data:", "  - id: adsl_sub", "    from: ADSL",
    "ard:", "  analyses:",
    "    - id: SHIFT",
    "      label: Shift in each direction",
    "      data: adsl_sub",
    "      params: [tox_dir, graded_params, worst, base, where, conf]",
    "      conditions:",
    "        - {tox_dir: Low, graded_params: [A, B], worst: ATOXGRL, base: BTOXGRL,",
    "           where: !r LBTOXL != \"\" & !is.na(ATOXGRL), conf: 0.9}",
    "        - {tox_dir: High, graded_params: [B], worst: ATOXGRH, base: BTOXGRH,",
    "           where: !r LBTOXH != \"\", conf: 0.95}",
    "      where: !r \"{where}\"",
    "      derive: !r tox_dir = \"{tox_dir}\"",
    "      levels: {PARAM: \"{graded_params}\", tox_dir: \"{tox_dir}\", SEX: [F, M]}",
    "      method: categorical",
    "      statistics: [n, p]",
    "      args: !r conf.level = {conf}",
    "      ards:",
    "        - {by: [PARAM, tox_dir, \"{base}\"], variables: [\"{worst}\"]}",
    "        - {by: [PARAM, tox_dir], variables: [\"{worst}\"]}",
    "        - {by: [PARAM], variables: [tox_dir], statistics: [n]}",
    "content:", "  - table_plan: {cols: tox_dir}")
  writeLines(y, file.path(d, "outputs", "T-SHIFT.yml"))
  sh <- tfl_sheets_from_yaml(d)
  an <- sh$ard$analyses[sh$ard$analyses$output_id == "T-SHIFT", ]
  expect_identical(an$analysis_id, rep("SHIFT", 3L))
  expect_identical(an$by, c("PARAM | tox_dir | {base}", "PARAM | tox_dir", "PARAM"))
  expect_identical(an$statistics, c("n | p", "n | p", "n"))
  expect_identical(an$method, rep("categorical", 3L))
  expect_identical(an$where[1L], "{where}")
  expect_true(all(is.na(an$where[2:3])))
  expect_identical(an$levels[1L], "PARAM = {graded_params}; tox_dir = {tox_dir}; SEX = F | M")
  expect_identical(an$params[1L], "tox_dir | graded_params | worst | base | where | conf")
  cs <- sh$ard$conditions
  expect_identical(nrow(cs), 12L)
  expect_identical(cs$value[cs$row == "1" & cs$param == "graded_params"], "A | B")
  expect_identical(cs$value[cs$row == "1" & cs$param == "where"], "!r LBTOXL != \"\" & !is.na(ATOXGRL)")
  expect_identical(cs$value[cs$row == "2" & cs$param == "conf"], "!r 0.95")
  out <- withr_tempdir()
  suppressMessages(tfl_yaml_from_sheets(sh, out))
  back <- tfl_sheets_from_yaml(out)
  mine <- function(s) {
    a <- s$ard$analyses
    a[a$output_id == "T-SHIFT", .ard_spec_sheets$analyses]
  }
  expect_identical(mine(back), mine(sh))
  expect_identical(back$ard$conditions, sh$ard$conditions)
  o <- tfl_read_outputs(out)[["T-SHIFT"]]
  it <- o$ard$analyses[[1L]]
  expect_identical(it$method, "categorical")
  expect_length(it$ards, 3L)
  expect_identical(it$ards[[3L]]$statistics, "n")
  expect_identical(it$conditions[[2L]]$conf, 0.95)
  expect_s3_class(it$conditions[[1L]]$where, "tfl_r")
  expect_identical(it$levels$PARAM, "{graded_params}")
  # an element's own post: the item's
  y2 <- sub("statistics: [n]}", "post: !r dplyr::filter(n > 0)}", y, fixed = TRUE)
  writeLines(y2, file.path(d, "outputs", "T-SHIFT.yml"))
  expect_error(tfl_sheets_from_yaml(d), "`ards:` element 3 has `post`")
})

test_that("the study's program: the item in its report's part", {
  rows <- cd_shift_rows()
  other <- list(output_id = "T2", analysis_id = "N", method = "categorical", dataset = "ADLB",
                variables = "PARAM")
  sp <- cd_spec(c(rows, list(other)), cd_shift_conditions())
  code <- tfl_ard_code(sp, save = FALSE, part = "body")
  txt <- paste(code, collapse = "\n")
  i_part <- regexpr("# ---- T2 ----", txt, fixed = TRUE)
  i_fun <- regexpr("ard_shift_one <- function(", txt, fixed = TRUE)
  expect_gt(i_fun, 0L)
  expect_lt(i_fun, i_part)
  expect_match(txt, "ards[[report_id]] <- ard_shift", fixed = TRUE)
  expect_true(is.language(str2lang(paste0("{", txt, "\n}"))))
})
