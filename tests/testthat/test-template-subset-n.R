# The subjects of a template's report: N, and the denominator of every
# subject percentage, are the analysis set as data.yml defines it, narrowed
# by `subset` -- ae_overview, denominator = "data", shift and lab_shift,
# pk_conc, the max grade of an arm with no record, a listing's subset.

sn_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  sl <- tflspec::adsl
  sl$PART <- ifelse(seq_len(nrow(sl)) %% 2L == 1L, "Part 1", "Part 2")
  for (x in list(list("adsl", sl), list("adae", tflspec::adae[setdiff(names(tflspec::adae), "TRT01AN")]), list("adlb", tflspec::adlb),
                 list("adpc", pk_adpc(sl)))) {
    haven::write_xpt(tflspec:::.ai_chr(x[[2L]]), file.path(d, "adam", paste0(x[[1L]], ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "SN-1")
  list(dir = d, adsl = sl)
}

# the output built to its RTF text
sn_rtf <- function(d, id, name, args, kind = "table") {
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  tfl_write_output(tfl_output(id, kind, population = if (kind == "table") "SAF",
                              extends = "defaults",
                              template = list(name = name, args = args)), f)
  out <- suppressWarnings(suppressMessages(tfl_run_output(id, dir = d)))
  expect_true(file.exists(out))
  paste(readLines(out, warn = FALSE), collapse = "\n")
}

sn_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtfreporter")
}

# the "Arm ... (N=k)" of a table's header, k by arm
sn_n <- function(x, arms) {
  x <- gsub("[{}]", " ", gsub("\\\\[a-zA-Z]+-?[0-9]* ?", " ", x))
  unname(vapply(arms, function(a) {
    m <- regmatches(x, regexpr(paste0(a, "[^()N]{0,8}\\(N=[0-9]+"), x))
    if (length(m)) as.integer(sub("^.*N=", "", m)) else NA_integer_
  }, 0L))
}

test_that("ae_overview with subset: every percentage is over the subset's subjects", {
  sn_skip()
  s <- sn_study(withr::local_tempdir())
  sub <- "PART == \"Part 1\""
  x <- sn_rtf(s$dir, "T-N1", "ae_overview", list(population = "SAF", by = "TRT01A", subset = sub))
  saf <- s$adsl[s$adsl$SAFFL == "Y" & s$adsl$PART == "Part 1", ]
  n <- table(saf$TRT01A)
  expect_match(x, "Drug X", fixed = TRUE)
  # the ARD: p = n / the subset's N, in each row and arm
  st <- tfl_read_study_spec(s$dir)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-N1", dir = s$dir, save = FALSE))
  rows <- ard[ard$analysis_id == "ANY" & ard$stat_name %in% c("n", "N", "p") &
                vapply(ard$variable_level, function(v) identical(unlist(v), TRUE), NA), ]
  arm <- vapply(rows$group1_level, function(v) as.character(unlist(v))[1L], "")
  for (a in names(n)) {
    r <- rows[arm == a, ]
    k <- unlist(r$stat[r$stat_name == "n"])
    expect_identical(unlist(r$stat[r$stat_name == "N"]), as.integer(n[[a]]))
    expect_equal(unlist(r$stat[r$stat_name == "p"]), k / n[[a]])
  }
})

test_that("denominator = \"data\": the records' subjects, within the subset", {
  sn_skip()
  s <- sn_study(withr::local_tempdir())
  sub <- "PART == \"Part 1\""
  for (tpl in c("ae_soc_pt", "hierarchical", "ae_ser_soc_pt", "ae_disc_soc_pt")) {
    a <- list(population = "SAF", subset = sub, denominator = "data")
    if (tpl == "hierarchical") a$hierarchy <- c("AEBODSYS", "AEDECOD")
    # (the records, narrowed to the subset, one row a subject, are the denominator)
    x <- tfl_template_expand(tpl, a)
    expect_identical(x$ard$analyses[[1L]]$denominator, sub("_sub$", "_den", x$ard$analysis_data[[2L]]$id))
    expect_match(x$ard$analysis_data[[3L]]$code, "dplyr::distinct(adae_sub, USUBJID, TRT01A)", fixed = TRUE)
    expect_identical(as.character(x$ard$analysis_data[[1L]]$where), sub)
  }
  # the header N of ae_soc_pt: Part 1's subjects with a record kept
  x <- sn_rtf(s$dir, "T-N2", "ae_soc_pt", list(population = "SAF", subset = sub,
                                                where = "TRTEMFL == \"Y\"", denominator = "data"))
  ae <- tflspec::adae
  p1 <- s$adsl$USUBJID[s$adsl$SAFFL == "Y" & s$adsl$PART == "Part 1"]
  ae <- ae[ae$TRTEMFL == "Y" & ae$USUBJID %in% p1, ]
  n <- tapply(ae$USUBJID, ae$TRT01A, function(u) length(unique(u)))
  expect_identical(sn_n(x, names(n)), as.integer(n))
  # the default: the subset's subjects, record or none
  y <- sn_rtf(s$dir, "T-N2", "ae_soc_pt", list(population = "SAF", subset = sub))
  m <- table(s$adsl$TRT01A[s$adsl$USUBJID %in% p1])
  expect_identical(sn_n(y, names(m)), as.integer(m))
})

test_that("shift and lab_shift with subset (and the default worst) build; N is the subset's", {
  sn_skip()
  s <- sn_study(withr::local_tempdir())
  sub <- "PART == \"Part 1\""
  p1 <- s$adsl$USUBJID[s$adsl$SAFFL == "Y" & s$adsl$PART == "Part 1"]
  m <- table(s$adsl$TRT01A[s$adsl$USUBJID %in% p1])
  x <- sn_rtf(s$dir, "T-N3", "shift", list(population = "SAF", data = "ADLB", subset = sub,
                                           where = "PARAMCD == \"ALT\" & AVISIT == \"CYCLE 2 DAY 1\""))
  n <- sn_n(x, names(m))
  expect_false(anyNA(n))
  expect_true(all(n <= as.integer(m)))
  y <- sn_rtf(s$dir, "T-N4", "lab_shift", list(population = "SAF", visit = "CYCLE 2 DAY 1", params = "ALT",
                                               subset = sub))
  n <- sn_n(y, names(m))
  expect_false(anyNA(n))
  expect_true(all(n <= as.integer(m)))
  # without subset the page N reaches further (all of the safety set's subjects)
  z <- sn_rtf(s$dir, "T-N4", "lab_shift", list(population = "SAF", visit = "CYCLE 2 DAY 1", params = "ALT"))
  expect_true(sum(sn_n(z, names(m))) > sum(n))
})

test_that("pk_conc with subset builds; BLQ is printed for a BLQ record only; one `by` has a heading", {
  sn_skip()
  s <- sn_study(withr::local_tempdir())
  sub <- "PART == \"Part 1\""
  pc <- pk_adpc(s$adsl)
  p1 <- s$adsl$USUBJID[s$adsl$PART == "Part 1"]
  x <- sn_rtf(s$dir, "T-N5", "pk_conc", list(population = "SAF", subset = sub,
                                             blq = "BLQFL == \"Y\""))
  # (the BLQ records of Part 1's subjects print BLQ; a time point with no
  # record, or a value missing and not BLQ, prints nothing)
  want <- sum(pc$BLQFL == "Y" & pc$USUBJID %in% p1)
  expect_identical(lengths(regmatches(x, gregexpr("BLQ", x))), want)
  expect_false(any(grepl(pc$USUBJID[!pc$USUBJID %in% p1][1L], x, fixed = TRUE)))
  # one by variable: the default heading, and the variable's own name
  for (h in list(NULL, "{TRT01A}")) {
    a <- list(population = "SAF", by = "TRT01A", blq = "BLQFL == \"Y\"")
    if (!is.null(h)) a$heading <- h
    y <- sn_rtf(s$dir, "T-N6", "pk_conc", a)
    expect_match(y, "Control|Drug X")
  }
  e <- tfl_template_expand("pk_conc", list(population = "SAF", blq = "BLQFL == \"Y\""))
  expect_identical(as.character(e$ard$analysis_data[[2L]]$derive),
                   "ATPT = reorder_by(ATPT, ATPTN) | AVAL = ifelse(BLQFL == \"Y\", -Inf, AVAL)")
})

test_that("ae_max_grade keeps an arm that where leaves no record, with its N and 0 (0.0)", {
  sn_skip()
  s <- sn_study(withr::local_tempdir())
  w <- "TRTEMFL == \"Y\" & TRT01A == \"Drug X\""
  for (sub in list(NULL, "PART == \"Part 1\"")) {
    a <- list(population = "SAF", where = w)
    if (!is.null(sub)) a$subset <- sub
    x <- sn_rtf(s$dir, "T-N7", "ae_max_grade", a)
    keep <- s$adsl$SAFFL == "Y" & (is.null(sub) | s$adsl$PART == "Part 1")
    n <- table(s$adsl$TRT01A[keep])
    expect_identical(sn_n(x, names(n)), as.integer(n))
    expect_match(x, "0 \\(0\\.0\\)")
  }
})

test_that("a listing with subset runs to its RTF, Part 1's subjects only", {
  sn_skip()
  s <- sn_study(withr::local_tempdir())
  cols <- list(list(vars = "USUBJID", label = "Subject"), list(vars = "AEDECOD", label = "PT"))
  x <- sn_rtf(s$dir, "L-N1", "listing", list(data = "ADAE", subset = "PART == \"Part 1\"",
                                             columns = cols), kind = "listing")
  has_ae <- s$adsl$USUBJID %in% tflspec::adae$USUBJID
  p2 <- s$adsl$USUBJID[s$adsl$PART == "Part 2" & has_ae]
  p1 <- s$adsl$USUBJID[s$adsl$PART == "Part 1" & has_ae]
  expect_true(any(vapply(p1, grepl, NA, x = x, fixed = TRUE)))
  expect_false(any(vapply(p2, grepl, NA, x = x, fixed = TRUE)))
})
