# tflspec_v1/tflspec_shift: the series base template for a shift table, a
# category at baseline (rows) by the same after it (columns under each
# group), a page a parameter.  The package's sample data (fictional) only.

shb_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
}

# The sample ADLB: one ALT subject with no category at baseline (the
# Missing row)
shb_adlb <- function() {
  lb <- tflspec::adlb
  one <- lb$USUBJID[lb$PARAMCD == "ALT"][1L]
  lb$BNRIND[lb$PARAMCD == "ALT" & lb$USUBJID == one] <- NA
  lb
}

# A study of the sample data: ADSL and ADLB, the sample-01 defaults
shb_study <- function(d) {
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in list(list("adsl", tflspec::adsl), list("adlb", shb_adlb()))) {
    haven::write_xpt(tflspec:::.ai_chr(x[[2L]]), file.path(d, "adam", paste0(x[[1L]], ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "SHB-1")
  file.copy(test_path("fixtures", "twin", "defaults-sample01.yml"),
            file.path(d, "spec", "defaults.yml"), overwrite = TRUE)
  d
}

# The study rule of the worked example (the template's help and the AI
# kit): a subject's worst category after baseline for a parameter, High
# when it has both Low and High
shb_rule <- "dplyr::arrange(match(ANRIND, c(\"HIGH\", \"LOW\", \"NORMAL\"))) |> dplyr::distinct(USUBJID, PARAMCD, .keep_all = TRUE)"

# The report's rec_sub row: after baseline, two parameters, then the rule
shb_rec <- c("  - id: rec_sub",
             "    where: !r PARAMCD %in% c(\"ALT\", \"HGB\") & !ABLFL %in% \"Y\" & AVISIT != \"WORST POST-BASELINE\"",
             paste0("    then: !r '", shb_rule, "'"))

# A report of the template: `tpl` the lines under `template:` (after its
# name), `more` the report's other lines
shb_report <- function(id, tpl, data = shb_rec, more = character()) {
  c(paste0("id: ", id), "kind: table", "label: Shift from baseline",
    "common:", "  population: SAF",
    "template:", "  name: tflspec_v1/tflspec_shift", tpl,
    if (length(data)) c("data:", data), more,
    "style: compact")
}

shb_tpl <- c("  analysis:", "    by: [TRT01A]")

# The report written to the study, as its code SPEC; its ARD and RTF
shb_run <- function(d, id, lines, rtf = TRUE) {
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  writeLines(lines, f)
  st <- tfl_read_study_spec(d)
  tfl_write_output(tfl_template_to_spec(tfl_read_output(f), st), f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, dir = d, output_id = id, save = FALSE))
  list(ard = as.data.frame(ard), rtf = if (rtf) tfl_run_output(id, dir = d))
}

# An ARD's list cell as text ("" for none)
shb_chr <- function(v) {
  if (!is.list(v)) return(ifelse(is.na(v), "", as.character(v)))
  vapply(v, function(x) if (is.null(x) || !length(x) || is.na(x[[1L]])) "" else
    as.character(x[[1L]]), "")
}

# The cells' counts of an ARD: "arm / parameter / post / baseline" -> n
shb_cells <- function(a) {
  a <- a[a$analysis_id != "BIGN" & a$stat_name == "n" & shb_chr(a$group3) != "", ]
  stats::setNames(as.integer(unlist(a$stat)),
                  paste(shb_chr(a$group1_level), shb_chr(a$group2_level),
                        shb_chr(a$group3_level), shb_chr(a$variable_level), sep = " / "))
}

# The same counted with dplyr from the sample data
shb_direct <- function() {
  sl <- tflspec::adsl
  arm <- as.character(sl$TRT01A)
  saf <- data.frame(USUBJID = as.character(sl$USUBJID), TRT01A = arm,
                    stringsAsFactors = FALSE)[sl$SAFFL %in% "Y" & !is.na(arm) & nzchar(arm), ]
  lb <- shb_adlb()
  lb$TRT01A <- NULL
  for (v in c("USUBJID", "PARAMCD", "ANRIND", "BNRIND", "AVISIT", "ABLFL")) {
    lb[[v]] <- as.character(lb[[v]])
  }
  x <- dplyr::inner_join(lb, saf, by = "USUBJID", relationship = "many-to-one")
  x <- dplyr::filter(x, PARAMCD %in% c("ALT", "HGB"), !ABLFL %in% "Y",
                     AVISIT != "WORST POST-BASELINE")
  x <- dplyr::distinct(dplyr::arrange(x, match(ANRIND, c("HIGH", "LOW", "NORMAL"))),
                       USUBJID, PARAMCD, .keep_all = TRUE)
  x$BNRIND[is.na(x$BNRIND) | !nzchar(x$BNRIND)] <- "Missing"
  x <- x[!is.na(x$ANRIND) & nzchar(x$ANRIND), ]
  n <- dplyr::count(x, TRT01A, PARAMCD, ANRIND, BNRIND)
  stats::setNames(n$n, paste(n$TRT01A, n$PARAMCD, n$ANRIND, n$BNRIND, sep = " / "))
}

test_that("tflspec_shift: the worst record by then, counts as dplyr's, a page a parameter", {
  shb_skip()
  d <- shb_study(file.path(tempdir(), "shb-main"))
  on.exit(unlink(d, recursive = TRUE))
  r <- shb_run(d, "S-1", shb_report("S-1", shb_tpl))
  # an analysis a parameter, BIGN first
  expect_identical(unique(as.character(r$ard$analysis_id)), c("BIGN", "ALT", "HGB"))
  got <- shb_cells(r$ard)
  want <- shb_direct()
  expect_identical(got[got > 0L][sort(names(want))], want[sort(names(want))])
  expect_true(all(got[setdiff(names(got), names(want))] == 0L))
  # the Missing row: the one ALT subject with no baseline category
  miss <- got[grepl("/ ALT / .* / Missing$", names(got))]
  expect_identical(sum(miss), 1L)
  expect_true(all(got[grepl("/ HGB / .* / Missing$", names(got))] == 0L))
  # the Total row: the categories after baseline by the parameter
  tr <- r$ard[r$ard$analysis_id %in% c("ALT", "HGB") & r$ard$variable == "ANRIND" &
                r$ard$stat_name == "n", ]
  expect_gt(nrow(tr), 0L)
  # a page a parameter, its name in the titles; the Total row printed
  txt <- readLines(r$rtf, warn = FALSE)
  expect_identical(sum(grepl("Parameter: ALT", txt, fixed = TRUE)), 1L)
  expect_identical(sum(grepl("Parameter: HGB", txt, fixed = TRUE)), 1L)
  expect_true(any(grepl("Missing", txt, fixed = TRUE)))
  expect_true(any(grepl("Total", txt, fixed = TRUE)))
})

test_that("tflspec_shift: the Total row and column, on and off, and their calls", {
  res <- function(...) {
    tfl_template_resolve(list(id = "X", kind = "table", template = list(
      name = "tflspec_v1/tflspec_shift", by = "TRT01A", ...)))
  }
  calls <- function(r) length(r$ard$analyses[[2L]]$ards)
  total <- function(r) {
    hit <- Filter(function(i) identical(i$fn, "plan_total"), r$content)
    if (length(hit)) hit[[1L]]$args$direction else "none"
  }
  r <- res()
  expect_identical(c(calls(r), total(r)), c("2", "rows"))
  r <- res(post_total = TRUE)
  expect_identical(c(calls(r), total(r)), c("4", "both"))
  r <- res(baseline_total = FALSE, post_total = TRUE)
  expect_identical(c(calls(r), total(r)), c("2", "cols"))
  r <- res(baseline_total = FALSE)
  expect_identical(c(calls(r), total(r)), c("1", "none"))
  shb_skip()
  d <- shb_study(file.path(tempdir(), "shb-total"))
  on.exit(unlink(d, recursive = TRUE))
  both <- shb_run(d, "S-2", shb_report("S-2", c(shb_tpl, "    post_total: true")))
  # the Total column: the categories at baseline by the parameter (two
  # groups), as the cells summed over the categories after baseline
  tc <- both$ard[both$ard$analysis_id == "ALT" & both$ard$variable == "BNRIND" &
                   both$ard$stat_name == "n" & shb_chr(both$ard$group3) == "", ]
  cells <- shb_cells(both$ard)
  alt <- cells[grepl("/ ALT /", names(cells))]
  for (i in seq_len(nrow(tc))) {
    arm <- shb_chr(tc$group1_level)[i]
    base <- shb_chr(tc$variable_level)[i]
    sub <- alt[startsWith(names(alt), paste0(arm, " / ALT / ")) & endsWith(names(alt), paste0(" / ", base))]
    expect_identical(as.integer(unlist(tc$stat))[i], sum(sub), info = paste(arm, base))
  }
  # the corner: the subjects of a parameter in each group
  expect_gt(sum(both$ard$analysis_id %in% c("ALT", "HGB") & both$ard$variable == "PARAMCD"), 0L)
  expect_true(file.exists(both$rtf))
  none <- shb_run(d, "S-3", shb_report("S-3", c(shb_tpl, "    baseline_total: false")), rtf = FALSE)
  expect_false(any(none$ard$variable == "ANRIND"))
})

test_that("tflspec_shift: missing_row off, analysis_ids, and the study's code lists", {
  shb_skip()
  d <- shb_study(file.path(tempdir(), "shb-opts"))
  on.exit(unlink(d, recursive = TRUE))
  r <- shb_run(d, "S-4", shb_report("S-4", c(shb_tpl, "    missing_row: false",
                                              "    analysis_ids: {ALT: ALT_SHIFT}")), rtf = FALSE)
  expect_identical(unique(as.character(r$ard$analysis_id)), c("BIGN", "ALT_SHIFT", "HGB"))
  expect_false(any(grepl("Missing$", names(shb_cells(r$ard)))))
  # the categories' order and the rows with no subject: the code lists
  # (the sample study has none for BNRIND / ANRIND: the report's own here)
  cl <- c("codelists:",
          "  BNRIND: [{value: LOW}, {value: NORMAL}, {value: HIGH}]",
          "  ANRIND: [{value: LOW}, {value: NORMAL}, {value: HIGH}]")
  r <- shb_run(d, "S-5", shb_report("S-5", shb_tpl, more = cl), rtf = FALSE)
  cells <- shb_cells(r$ard)
  for (p in c("ALT", "HGB")) {
    lv <- unique(sub("^.* / ", "", names(cells)[grepl(paste0("/ ", p, " /"), names(cells))]))
    expect_identical(lv, c("LOW", "NORMAL", "HIGH", "Missing"), info = p)
  }
})

test_that("tflspec_shift: by is required", {
  expect_error(tfl_template_resolve(list(id = "X", kind = "table",
                                         template = list(name = "tflspec_v1/tflspec_shift"))),
               "needs by")
})
