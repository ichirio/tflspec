# The subjects of a template's report: N, and the denominator of every
# subject percentage, are the analysis set as data.yml defines it, narrowed
# by `subset` -- ae_overview, shift and lab_shift,
# the max grade of an arm with no record.

sn_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  sl <- tflspec::adsl
  sl$PART <- ifelse(seq_len(nrow(sl)) %% 2L == 1L, "Part 1", "Part 2")
  for (x in list(list("adsl", sl), list("adae", tflspec::adae[setdiff(names(tflspec::adae), "TRT01AN")]), list("adlb", tflspec::adlb))) {
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
  x <- sn_rtf(s$dir, "T-N1", "ae_overview", list(population = "SAF", subset = sub))
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

test_that("shift and lab_shift with subset (and the default worst) build; the subset's subjects only", {
  sn_skip()
  s <- sn_study(withr::local_tempdir())
  sub <- "PART == \"Part 1\""
  lb <- tflspec::adlb
  lb <- lb[lb$PARAMCD == "ALT" & lb$AVISIT == "CYCLE 2 DAY 1" & lb$USUBJID %in%
             s$adsl$USUBJID[s$adsl$SAFFL == "Y"], ]
  p1 <- s$adsl$USUBJID[s$adsl$PART == "Part 1"]
  # the cells of the Total row, over the baseline and post categories
  total <- function(x) {
    m <- regmatches(x, regexpr("Total\\\\cell[^\n]*", x))
    sum(as.integer(regmatches(m, gregexpr("(?<=ri0 )[0-9]+(?=\\\\cell)", m, perl = TRUE))[[1L]]))
  }
  x <- sn_rtf(s$dir, "T-N3", "shift", list(population = "SAF", data = "ADLB", subset = sub,
                                           where = "PARAMCD == \"ALT\" & AVISIT == \"CYCLE 2 DAY 1\""))
  expect_identical(total(x), length(unique(lb$USUBJID[lb$USUBJID %in% p1])))
  y <- sn_rtf(s$dir, "T-N4", "lab_shift", list(population = "SAF", visit = "CYCLE 2 DAY 1", params = "ALT",
                                               subset = sub))
  expect_identical(total(y), length(unique(lb$USUBJID[lb$USUBJID %in% p1])))
  # without subset the table reaches further (all of the safety set's subjects)
  z <- sn_rtf(s$dir, "T-N4", "lab_shift", list(population = "SAF", visit = "CYCLE 2 DAY 1", params = "ALT"))
  expect_identical(total(z), length(unique(lb$USUBJID)))
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
