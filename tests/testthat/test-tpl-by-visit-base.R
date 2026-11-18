# tflspec_v1/tflspec_by_visit: the series base template for the value and
# the change from baseline by parameter and visit, a page a parameter.
# The package's sample data (fictional) only.

bvb_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
}

bvb_visits <- c("BASELINE", "CYCLE 2 DAY 1", "CYCLE 3 DAY 1")

# A study of the sample data: ADSL and ADLB, the sample-01 defaults
bvb_study <- function(d) {
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in list(list("adsl", tflspec::adsl), list("adlb", tflspec::adlb))) {
    haven::write_xpt(tflspec:::.ai_chr(x[[2L]]), file.path(d, "adam", paste0(x[[1L]], ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "BVB-1")
  file.copy(test_path("fixtures", "twin", "defaults-sample01.yml"),
            file.path(d, "spec", "defaults.yml"), overwrite = TRUE)
  d
}

# The report's rec_sub row (two parameters, three visits, the baseline
# among them) and the visits' code list (the study's order)
bvb_rec <- c("  - id: rec_sub",
             paste0("    where: !r PARAMCD %in% c(\"ALT\", \"HGB\") & AVISIT %in% c(",
                    paste0("\"", bvb_visits, "\"", collapse = ", "), ")"))
bvb_cl <- c("codelists:",
            paste0("  AVISIT: [", paste0("{value: ", bvb_visits, "}", collapse = ", "), "]"))

# A report of the template: `tpl` the lines under `template:` (after its
# name), `more` the report's other lines
bvb_report <- function(id, tpl, data = bvb_rec, more = bvb_cl) {
  c(paste0("id: ", id), "kind: table", "label: Laboratory values by visit",
    "common:", "  population: SAF",
    "template:", "  name: tflspec_v1/tflspec_by_visit", tpl,
    if (length(data)) c("data:", data), more,
    "style: compact")
}

bvb_tpl <- c("  from: ADLB", "  analysis:", "    by: [TRT01A]")

# The report written to the study, as its code SPEC; its ARD and RTF
bvb_run <- function(d, id, lines, rtf = TRUE) {
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  writeLines(lines, f)
  st <- tfl_read_study_spec(d)
  tfl_write_output(tfl_template_to_spec(tfl_read_output(f), st), f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, dir = d, output_id = id, save = FALSE))
  list(ard = as.data.frame(ard), rtf = if (rtf) tfl_run_output(id, dir = d))
}

# An ARD's list cell as text ("" for none)
bvb_chr <- function(v) {
  if (!is.list(v)) return(ifelse(is.na(v), "", as.character(v)))
  vapply(v, function(x) if (is.null(x) || !length(x) || is.na(x[[1L]])) "" else
    as.character(x[[1L]]), "")
}

# A statistic of an ARD: "arm / parameter / visit / variable" -> value
bvb_stat <- function(a, stat) {
  a <- a[a$analysis_id != "BIGN" & a$stat_name == stat & bvb_chr(a$group3) != "", ]
  stats::setNames(as.numeric(unlist(a$stat)),
                  paste(bvb_chr(a$group1_level), bvb_chr(a$group2_level),
                        bvb_chr(a$group3_level), a$variable, sep = " / "))
}

# The same with dplyr from the sample data
bvb_direct <- function(fun) {
  sl <- tflspec::adsl
  arm <- as.character(sl$TRT01A)
  saf <- data.frame(USUBJID = as.character(sl$USUBJID), TRT01A = arm,
                    stringsAsFactors = FALSE)[sl$SAFFL %in% "Y" & !is.na(arm) & nzchar(arm), ]
  lb <- tflspec::adlb
  lb$TRT01A <- NULL
  x <- dplyr::inner_join(lb, saf, by = "USUBJID", relationship = "many-to-one")
  x <- dplyr::filter(x, PARAMCD %in% c("ALT", "HGB"), AVISIT %in% bvb_visits)
  one <- function(v) {
    y <- x[!is.na(x[[v]]), ]
    y$val <- y[[v]]
    s <- dplyr::summarise(dplyr::group_by(y, TRT01A, PARAMCD, AVISIT), stat = fun(val),
                          .groups = "drop")
    stats::setNames(s$stat, paste(s$TRT01A, s$PARAMCD, s$AVISIT, v, sep = " / "))
  }
  c(one("AVAL"), one("CHG"))
}

test_that("tflspec_by_visit: the cells as dplyr's, the baseline's change empty, a page a parameter", {
  bvb_skip()
  d <- bvb_study(file.path(tempdir(), "bvb-main"))
  on.exit(unlink(d, recursive = TRUE))
  r <- bvb_run(d, "B-1", bvb_report("B-1", bvb_tpl))
  # an analysis a parameter (the value and the change alike), BIGN first
  expect_identical(unique(as.character(r$ard$analysis_id)), c("BIGN", "ALT", "HGB"))
  for (s in list(list("N", length), list("mean", mean), list("median", stats::median),
                 list("max", max))) {
    got <- bvb_stat(r$ard, s[[1L]])
    want <- bvb_direct(s[[2L]])
    expect_equal(got[sort(names(want))], want[sort(names(want))], info = s[[1L]])
  }
  # the baseline: its value's statistics, no change rows at all (no N = 0)
  n <- bvb_stat(r$ard, "N")
  expect_true(all(n > 0))
  expect_true(any(grepl("/ BASELINE / AVAL$", names(n))))
  expect_false(any(grepl("/ BASELINE / CHG$", names(n))))
  expect_true(any(grepl("/ CYCLE 2 DAY 1 / CHG$", names(n))))
  # a page a parameter, its name in the titles; both headings printed
  txt <- readLines(r$rtf, warn = FALSE)
  expect_identical(sum(grepl("Parameter: ALT", txt, fixed = TRUE)), 1L)
  expect_identical(sum(grepl("Parameter: HGB", txt, fixed = TRUE)), 1L)
  expect_true(any(grepl("Change from Baseline", txt, fixed = TRUE)))
  expect_true(any(grepl("Mean (SD)", txt, fixed = TRUE)))
})

test_that("tflspec_by_visit: layout, total, cells and sd_missing as resolved", {
  res <- function(...) {
    tfl_template_resolve(list(id = "X", kind = "table", template = list(
      name = "tflspec_v1/tflspec_by_visit", from = "ADLB", by = "TRT01A", ...)))
  }
  item <- function(r, fn) Filter(function(i) identical(i$fn, fn), r$content)[[1L]]$args
  calls <- function(r) vapply(r$ard$analyses[2:3], function(a) length(a$ards), 1L)
  r <- res()
  expect_identical(item(r, "table_plan")$cols, c("TRT01A", "variable"))
  expect_identical(unlist(item(r, "table_plan")$rows), c(param = "PARAMCD", visit = "AVISIT"))
  expect_identical(item(r, "plan_labels")$variable,
                   list(AVAL = "Value", CHG = "Change from Baseline"))
  expect_identical(calls(r), c(1L, 1L))
  expect_identical(item(r, "plan_cells")$continuous[["Mean (SD)"]],
                   c("{mean} ({sd})", "{mean} (-)"))
  r <- res(layout = "rows", total = TRUE, change = "PCHG", sd_missing = "")
  expect_identical(item(r, "table_plan")$cols, "TRT01A")
  expect_identical(unlist(item(r, "table_plan")$rows),
                   c(param = "PARAMCD", visit = "AVISIT", item = "variable"))
  expect_identical(names(item(r, "plan_labels")$variable), c("AVAL", "PCHG"))
  expect_identical(calls(r), c(2L, 2L))
  expect_identical(item(r, "plan_cells")$continuous[["Mean (SD)"]], c("{mean} ({sd})", "{mean}"))
  # the Total column is for layout: rows only
  expect_error(res(total = TRUE), "total needs layout: rows")
  # cells the report gives are its own: no fallback added
  own <- list(n = "{N}", "Mean (SD)" = "{mean} ({sd})")
  r <- res(cells = own)
  expect_identical(item(r, "plan_cells")$continuous, own)
  # the default cells of the derive: the param's default
  t <- yaml::read_yaml(system.file("series", "tflspec_v1", "templates", "tflspec_by_visit.yml",
                                   package = "tflspec"))
  expect_identical(names(t$derive$by_visit_cells[["else"]]), names(t$params$cells$default))
})

test_that("tflspec_by_visit: the Total column, layout rows and analysis_ids run", {
  bvb_skip()
  d <- bvb_study(file.path(tempdir(), "bvb-opts"))
  on.exit(unlink(d, recursive = TRUE))
  r <- bvb_run(d, "B-2", bvb_report("B-2", c(bvb_tpl, "    total: true",
                                              "    analysis_ids: {ALT: ALT_VISIT}",
                                              "  content:", "    layout: rows")))
  expect_identical(unique(as.character(r$ard$analysis_id)), c("BIGN", "ALT_VISIT", "HGB"))
  # the Total column: the parameter and the visit alone, as all the groups
  tot <- r$ard[r$ard$analysis_id == "ALT_VISIT" & r$ard$stat_name == "N" &
                 bvb_chr(r$ard$group1) == "PARAMCD", ]
  expect_gt(nrow(tot), 0L)
  n <- bvb_stat(r$ard, "N")
  for (i in seq_len(nrow(tot))) {
    vis <- bvb_chr(tot$group2_level)[i]
    v <- tot$variable[i]
    sub <- n[grepl(paste0(" / ALT / ", vis, " / ", v, "$"), names(n))]
    expect_identical(as.numeric(unlist(tot$stat))[i], sum(sub), info = paste(vis, v))
  }
  expect_false(any(bvb_chr(tot$group2_level) == "BASELINE" & tot$variable == "CHG"))
  txt <- readLines(r$rtf, warn = FALSE)
  expect_true(any(grepl("Total", txt, fixed = TRUE)))
  # (the rows layout's ALT runs past max_rows: a second ALT page, no HGB rows on it)
  expect_gte(sum(grepl("Parameter: ALT", txt, fixed = TRUE)), 1L)
  expect_gte(sum(grepl("Parameter: HGB", txt, fixed = TRUE)), 1L)
})

test_that("tflspec_by_visit: from and by are required", {
  expect_error(tfl_template_resolve(list(id = "X", kind = "table", template = list(
    name = "tflspec_v1/tflspec_by_visit", by = "TRT01A"))), "needs from")
  expect_error(tfl_template_resolve(list(id = "X", kind = "table", template = list(
    name = "tflspec_v1/tflspec_by_visit", from = "ADLB"))), "needs by")
})

test_that("a map key that is a whole {name} is that name's text", {
  env <- list(value = "AVAL", lab = "Value", many = c("a", "b"))
  expect_identical(.tr_fill(list("{value}" = "{lab}", "{other}" = "x"), env),
                   list(AVAL = "Value", "{other}" = "x"))
  expect_error(.tr_fill(list("{many}" = "x"), env), "one text")
})
