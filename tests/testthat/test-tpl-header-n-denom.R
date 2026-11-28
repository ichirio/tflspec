# The column headers' N and the percentages' denominator are two params of
# tflspec_summary, tflspec_hierarchy and tflspec_shift (canon 11-28,
# CANON-6): col_header_n_data (the data row BIGN counts, default adsl_sub)
# and denominator (the data row the other analyses divide by; empty: the
# same row).  Fictional data only.

# (each template's required arguments besides by)
hnd_need <- list(tflspec_summary = list(categorical = "SEX"),
                 tflspec_hierarchy = list(from = "ADAE", hierarchy = c("AEBODSYS", "AEDECOD")),
                 tflspec_shift = list())

hnd_res <- function(template, ...) {
  suppressWarnings(tfl_template_resolve(list(id = "X", kind = "table", template = c(list(
    name = paste0("tflspec_v1/", template), by = "TRT01A"), hnd_need[[template]], list(...)))))
}

hnd_items <- function(r) {
  a <- r$ard$analyses
  stats::setNames(lapply(a, function(x) list(data = x$data, denominator = x$denominator)),
                  vapply(a, function(x) x$id, ""))
}

hnd_tpls <- c(summary = "tflspec_summary", hierarchy = "tflspec_hierarchy", shift = "tflspec_shift")
# (each template's first analysis with percentages: summary's SEX,
# hierarchy's first level, shift's SHIFT)
hnd_pct <- function(it) {
  it <- it[setdiff(names(it), "BIGN")]
  it[!vapply(it, function(x) is.null(x$denominator), NA)][[1L]]
}

test_that("the two params: defaults, each alone, the row's name written out", {
  for (k in names(hnd_tpls)) {
    defs <- tfl_arg_defs(template = paste0("tflspec_v1/", hnd_tpls[[k]]))
    expect_true(all(c("col_header_n_data", "denominator") %in% defs$name), info = k)
    expect_identical(defs$step[defs$name == "col_header_n_data"], "analysis", info = k)
    # (defaults: the header's N and the percentages on adsl_sub, by name)
    it <- hnd_items(hnd_res(hnd_tpls[[k]]))
    expect_identical(it$BIGN$data, "adsl_sub", info = k)
    expect_identical(hnd_pct(it)$denominator, "adsl_sub", info = k)
    # (col_header_n_data alone: both follow it)
    it <- hnd_items(hnd_res(hnd_tpls[[k]], col_header_n_data = "adsl_all"))
    expect_identical(it$BIGN$data, "adsl_all", info = k)
    expect_identical(hnd_pct(it)$denominator, "adsl_all", info = k)
    # (denominator alone: the percentages only, never "BIGN")
    it <- hnd_items(hnd_res(hnd_tpls[[k]], denominator = "rec_den"))
    expect_identical(it$BIGN$data, "adsl_sub", info = k)
    expect_identical(hnd_pct(it)$denominator, "rec_den", info = k)
    dens <- unlist(lapply(it, `[[`, "denominator"))
    expect_false("BIGN" %in% dens, info = k)
    # (a denominator alone is the canon's own use, the subjects tested:
    # nothing said)
    expect_no_warning(tfl_template_resolve(list(id = "X", kind = "table", template = c(list(
      name = paste0("tflspec_v1/", hnd_tpls[[k]]), by = "TRT01A"), hnd_need[[hnd_tpls[[k]]]],
      list(denominator = "rec_den")))))
  }
})

test_that("a denominator row of one row a subject and parameter keeps its distinct columns and the groups", {
  r <- tfl_template_resolve(list(
    id = "X", kind = "table",
    data = list(list(id = "rec_den", from = "rec_sub", distinct = c("USUBJID", "PARAMCD"))),
    template = list(name = "tflspec_v1/tflspec_shift", by = "TRT01A", denominator = "rec_den",
                    col_header_n_data = "adsl_sub")))
  rows <- r$ard$analysis_data
  den <- rows[[match("rec_den", vapply(rows, function(e) e$id, ""))]]
  expect_identical(as.character(unlist(den$keep)), c("USUBJID", "PARAMCD", "TRT01A"))
  # (a row with its own keep, or one not named as the denominator: as written)
  r <- tfl_template_resolve(list(
    id = "X", kind = "table",
    data = list(list(id = "rec_den", from = "rec_sub", distinct = c("USUBJID", "PARAMCD"),
                     keep = c("PARAMCD", "TRT01A", "PARCAT1"))),
    template = list(name = "tflspec_v1/tflspec_shift", by = "TRT01A", denominator = "rec_den",
                    col_header_n_data = "adsl_sub")))
  rows <- r$ard$analysis_data
  den <- rows[[match("rec_den", vapply(rows, function(e) e$id, ""))]]
  expect_identical(as.character(unlist(den$keep)), c("PARAMCD", "TRT01A", "PARCAT1"))
})

# ---- the ARD, on fictional data --------------------------------------------

hnd_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
}

# Arm A: 4 subjects, 2 tested for ALT (S1, S2), 1 for HGB (S1); arm B: 3
# subjects, all tested for ALT, none for HGB (the empty cells)
hnd_study <- function(d) {
  unlink(d, recursive = TRUE)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  adsl <- data.frame(STUDYID = "FIC-1", USUBJID = paste0("S", 1:7),
                     TRT01A = c(rep("Arm A", 4L), rep("Arm B", 3L)), SAFFL = "Y",
                     stringsAsFactors = FALSE)
  adlb <- data.frame(STUDYID = "FIC-1", USUBJID = c("S1", "S2", "S1", "S5", "S6", "S7"),
                     PARAMCD = c("ALT", "ALT", "HGB", "ALT", "ALT", "ALT"),
                     BNRIND = c("NORMAL", "LOW", "NORMAL", "NORMAL", "HIGH", "NORMAL"),
                     ANRIND = c("HIGH", "NORMAL", "NORMAL", "NORMAL", "HIGH", "LOW"),
                     ABLFL = "", AVISIT = "WEEK 2", stringsAsFactors = FALSE)
  haven::write_xpt(adsl, file.path(d, "adam", "adsl.xpt"))
  haven::write_xpt(adlb, file.path(d, "adam", "adlb.xpt"))
  invisible(utils::capture.output(tfl_ai_study_init(d, adam = "adam", study_id = "FIC-1")))
  file.copy(test_path("fixtures", "twin", "defaults-sample01.yml"),
            file.path(d, "spec", "defaults.yml"), overwrite = TRUE)
  d
}

# A shift report: `tpl` lines under template: analysis:, `data` its own rows
# after rec_sub's; its code SPEC written, its ARD (and RTF)
hnd_run <- function(d, id, tpl = character(), data = character(), rtf = FALSE) {
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  writeLines(c(paste0("id: ", id), "kind: table", "label: Shift", "common:", "  population: SAF",
               "template:", "  name: tflspec_v1/tflspec_shift", "  analysis:", "    by: [TRT01A]",
               "    post_total: true", tpl,
               "data:", "  - id: rec_sub", "    where: !r PARAMCD %in% c(\"ALT\", \"HGB\")", data,
               "style: compact"), f)
  invisible(tfl_copy_templates(d))
  st <- tfl_read_study_spec(d)
  tfl_write_output(tfl_template_to_spec(tfl_read_output(f), st), f)
  st <- tfl_read_study_spec(d)
  ard <- as.data.frame(suppressMessages(tfl_build_ard(st, dir = d, output_id = id, save = FALSE)))
  list(ard = ard, rtf = if (rtf) tfl_run_output(id, dir = d))
}

hnd_chr <- function(v) {
  vapply(v, function(x) if (is.null(x) || !length(x) || is.na(x[[1L]])) "" else
    as.character(x[[1L]]), "")
}

# A statistic of the ARD by its keys: "group1 / group2 / group3 / variable
# = level" -> value
hnd_stat <- function(a, stat) {
  a <- a[a$stat_name == stat, , drop = FALSE]
  stats::setNames(as.numeric(unlist(a$stat)),
                  paste(a$analysis_id, hnd_chr(a$group1_level), hnd_chr(a$group2_level),
                        hnd_chr(a$group3_level), paste0(a$variable, "=", hnd_chr(a$variable_level)),
                        sep = " / "))
}

hnd_den <- c("  - id: rec_den", "    from: rec_sub", "    distinct: [USUBJID, PARAMCD]")
hnd_tested <- c("    col_header_n_data: adsl_sub", "    denominator: rec_den")

test_that("the subjects tested as the denominator: BIGN the arm's, each parameter's N its own", {
  hnd_skip()
  d <- hnd_study(file.path(tempdir(), "hnd-tested"))
  on.exit(unlink(d, recursive = TRUE))
  r <- hnd_run(d, "S-1", hnd_tested, hnd_den, rtf = TRUE)
  n <- hnd_stat(r$ard, "n")
  bign <- n[startsWith(names(n), "BIGN /")]
  expect_identical(unname(bign[grepl("Arm A$", names(bign))]), 4)
  expect_identical(unname(bign[grepl("Arm B$", names(bign))]), 3)
  N <- hnd_stat(r$ard, "N")
  p <- hnd_stat(r$ard, "p")
  # (each cell's N: the arm's subjects tested for the parameter)
  cells <- N[grepl("^(ALT|HGB) / Arm A / ", names(N)) & !startsWith(names(N), "BIGN")]
  expect_true(all(cells[grepl("^ALT ", names(cells))] == 2))
  expect_true(all(cells[grepl("^HGB ", names(cells))] == 1))
  expect_true(all(N[grepl("^ALT / Arm B / ", names(N))] == 3))
  # (p is n over that N: S1 ALT NORMAL -> HIGH, 1 of 2)
  k <- "ALT / Arm A / ALT / HIGH / BNRIND=NORMAL"
  expect_identical(unname(n[k]), 1)
  expect_identical(unname(p[k]), 0.5)
  k <- "HGB / Arm A / HGB / NORMAL / BNRIND=NORMAL"
  expect_identical(unname(c(n[k], N[k], p[k])), c(1, 1, 1))
  # (the header's N still the arm's: N=4 and N=3, and the cell 1 (50.0))
  x <- paste(readLines(r$rtf, warn = FALSE), collapse = "\n")
  expect_match(x, "N=4", fixed = TRUE)
  expect_match(x, "N=3", fixed = TRUE)
  expect_false(grepl("N=NA", x, fixed = TRUE))
})

test_that("the shift corner: each parameter's numerator and denominator its own, empty cells left empty", {
  hnd_skip()
  d <- hnd_study(file.path(tempdir(), "hnd-corner"))
  on.exit(unlink(d, recursive = TRUE))
  r <- hnd_run(d, "S-2", hnd_tested, hnd_den)
  corner <- r$ard[r$ard$variable == "PARAMCD", , drop = FALSE]
  key <- paste(corner$analysis_id, hnd_chr(corner$group1_level), corner$stat_name)
  got <- stats::setNames(as.numeric(unlist(corner$stat)), key)
  # (arm A: ALT 2 of 2 tested, HGB 1 of 1; arm B: ALT 3 of 3 -- before,
  # every parameter's rows pooled: N 3 in arm A, ALT 2/3 and HGB 1/3)
  expect_identical(unname(got[c("ALT Arm A n", "ALT Arm A N", "ALT Arm A p")]), c(2, 2, 1))
  expect_identical(unname(got[c("HGB Arm A n", "HGB Arm A N", "HGB Arm A p")]), c(1, 1, 1))
  expect_identical(unname(got[c("ALT Arm B n", "ALT Arm B N", "ALT Arm B p")]), c(3, 3, 1))
  # (no p over 100%; each row's analysis is its own value's)
  expect_true(all(got[endsWith(names(got), " p")] <= 1))
  expect_identical(unname(hnd_chr(corner$variable_level)), as.character(corner$analysis_id))
  # (arm B tested nobody for HGB: no corner row, as its other cells)
  expect_false(any(grepl("^HGB Arm B", key)))
  n <- hnd_stat(r$ard, "n")
  expect_true(all(n[grepl("^HGB / Arm B", names(n))] == 0))
  # (the same calls on adsl_sub, the default: the corner is one call, the
  # arm's N; no slicing)
  r0 <- hnd_run(d, "S-3")
  c0 <- r0$ard[r0$ard$variable == "PARAMCD" & r0$ard$stat_name == "N", , drop = FALSE]
  expect_true(all(as.numeric(unlist(c0$stat))[hnd_chr(c0$group1_level) == "Arm A"] == 4))
  prog <- tfl_ard_code(tflspec:::.ard_spec_of_study(tfl_read_study_spec(d), output_id = "S-3")$spec,
                       output_id = "S-3", save = FALSE)
  expect_false(any(grepl("a value of PARAMCD at a time", prog, fixed = TRUE)))
  prog <- tfl_ard_code(tflspec:::.ard_spec_of_study(tfl_read_study_spec(d), output_id = "S-2")$spec,
                       output_id = "S-2", save = FALSE)
  expect_true(any(grepl("a value of PARAMCD at a time", prog, fixed = TRUE)))
})

test_that("ARS: a denominator row of the subjects tested is not mapped as the analysis set's", {
  hnd_skip()
  d <- hnd_study(file.path(tempdir(), "hnd-ars"))
  on.exit(unlink(d, recursive = TRUE))
  ars_of <- function(id, tpl, data) {
    hnd_run(d, id, tpl, data)
    sp <- tflspec:::.ard_spec_of_study(tfl_read_study_spec(d), output_id = id)$spec
    suppressWarnings(tfl_ars(sp))
  }
  refs <- function(ars, out) {
    an <- Filter(function(z) grepl(paste0("^An_", out, "_SHIFT"), z$id), ars$analyses)
    unlist(lapply(an, function(z) vapply(z$referencedAnalysisOperations, `[[`, "", "analysisId")))
  }
  ars <- ars_of("S-6", hnd_tested, hnd_den)
  un <- tfl_ars_unmapped(ars)
  expect_true(any(un$item == "denominator" & grepl("rec_den", un$reason, fixed = TRUE) &
                    grepl("none is mapped", un$reason, fixed = TRUE)))
  expect_false(any(grepl("BIGN", refs(ars, "S-6"), fixed = TRUE)))
  # (the default: the analysis set's subjects, BIGN, nothing said)
  ars <- ars_of("S-7", character(), character())
  expect_false(any(tfl_ars_unmapped(ars)$item == "denominator" &
                     grepl("SHIFT", tfl_ars_unmapped(ars)$where, fixed = TRUE)))
  expect_true(any(grepl("BIGN", refs(ars, "S-7"), fixed = TRUE)))
})
