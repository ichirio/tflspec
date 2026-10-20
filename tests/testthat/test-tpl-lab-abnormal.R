# lab_abnormal: subjects with laboratory values in abnormal categories, n/N (%)

la_args <- list(population = "SAF", levels = c("LOW", "HIGH"), where = "ONTRTFL == \"Y\"")

# the ARD as the study's program makes it: the records kept, the N (the
# subjects with a value of the parameter) and each subject once a category
la_ard <- function(x, lb = adlb) {
  saf <- adsl[adsl$SAFFL == "Y", ]
  d <- x$ard$analysis_data
  adlb_saf <- lb[lb$USUBJID %in% saf$USUBJID, ]
  if (length(d[[2L]]$where)) adlb_saf <- adlb_saf[eval(parse(text = d[[2L]]$where), adlb_saf), ]
  adlb_saf_n <- eval(parse(text = d[[3L]]$code))
  adlb_saf_cat <- if (length(d) == 5L) {
    adlb_saf_worst <- eval(parse(text = d[[4L]]$code))
    eval(parse(text = d[[5L]]$code))
  } else {
    v <- adlb_saf[eval(parse(text = d[[4L]]$where), adlb_saf), ]
    v <- eval(parse(text = sprintf("dplyr::mutate(v, %s)", d[[4L]]$derive)))
    dplyr::distinct(v, USUBJID, TRT01A, PARAM, ANRIND)
  }
  ct <- rlang::sym(x$ard$analyses[[2L]]$variables)
  cards::bind_ard(
    cards::ard_tabulate(saf, variables = TRT01A),
    cards::ard_tabulate(adlb_saf_cat, by = c(TRT01A, PARAM), variables = !!ct,
                        denominator = adlb_saf_n))
}
la_tab <- function(p) {
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  t
}
la_skip <- function() {
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
}
# a small criterion: ALT and AST above 1, 1.1 and 1.2 x ULN (levels 1-3)
la_crit <- function() {
  lb <- adlb
  r <- lb$AVAL / lb$ANRHI
  lb$MCRIT1ML <- ifelse(r > 1.2, "3", ifelse(r > 1.1, "2", ifelse(r > 1, "1", "0")))
  lb
}
# the subjects of an arm and a parameter in a level (at or above it: cumulative)
la_count <- function(lb, param, arm, keep) {
  saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
  r <- lb[lb$USUBJID %in% saf & lb$PARAM == param & lb$TRT01A == arm, ]
  c(n = length(unique(r$USUBJID[keep(r)])), N = length(unique(r$USUBJID)))
}

test_that("expanded: the records kept, the N's data, each subject once a category", {
  x <- tfl_template_expand("lab_abnormal", la_args)
  d <- x$ard$analysis_data
  expect_identical(vapply(d, `[[`, "", "id"),
                   c("adsl_saf", "adlb_saf", "adlb_saf_n", "adlb_saf_cat"))
  expect_identical(as.character(d[[2L]]$where), "ONTRTFL == \"Y\"")
  expect_identical(d[[3L]]$code, "dplyr::distinct(adlb_saf, USUBJID, TRT01A, PARAM)")
  expect_identical(as.character(d[[4L]]$where), "ANRIND %in% c(\"LOW\", \"HIGH\")")
  expect_identical(d[[4L]]$distinct, "USUBJID | TRT01A | PARAM | ANRIND")
  an <- x$ard$analyses[[2L]]
  expect_identical(an$denominator, "adlb_saf_n")
  expect_identical(an$by, c("TRT01A", "PARAM"))
  # the parameters, baseline normal: the records kept
  y <- tfl_template_expand("lab_abnormal", c(la_args[-3L], list(
    params = c("ALT", "AST"), baseline_normal = TRUE)))
  expect_identical(as.character(y$ard$analysis_data[[2L]]$where),
                   "PARAMCD %in% c(\"ALT\", \"AST\") & BNRIND == \"NORMAL\"")
  # cumulative: the worst level, then every level up to it
  z <- tfl_template_expand("lab_abnormal", list(population = "SAF", category = "MCRIT1ML",
                                                levels = c("1", "2", "3"), cumulative = TRUE))
  expect_identical(vapply(z$ard$analysis_data, `[[`, "", "id")[4:5],
                   c("adlb_saf_worst", "adlb_saf_cat"))
  expect_match(z$ard$analysis_data[[4L]]$code, "max(match(MCRIT1ML, c(\"1\", \"2\", \"3\")))",
               fixed = TRUE)
  expect_error(tfl_template_expand("lab_abnormal", list(population = "SAF", cumulative = TRUE)),
               "needs `levels`")
})

test_that("LOW / HIGH: n/N (%), N the subjects with a value of the parameter", {
  la_skip()
  x <- tfl_template_expand("lab_abnormal", la_args)
  t <- la_tab(do.call(tpl_lab_abnormal, c(list(la_ard(x)), la_args)))
  expect_identical(names(t), c("group", "label", "Drug X", "Control"))
  expect_identical(unique(t$label), c("LOW", "HIGH"))
  alt <- "Alanine Aminotransferase (U/L)"
  k <- la_count(adlb[adlb$ONTRTFL == "Y", ], alt, "Drug X", function(r) r$ANRIND == "HIGH")
  expect_identical(t[["Drug X"]][t$group == alt & t$label == "HIGH"],
                   sprintf("%d/%d (%.1f)", k[["n"]], k[["N"]], 100 * k[["n"]] / k[["N"]]))
})

test_that("cumulative: a subject in every level up to their worst", {
  la_skip()
  a <- list(population = "SAF", category = "MCRIT1ML", levels = c("1", "2", "3"),
            cumulative = TRUE, params = c("ALT", "AST"))
  lb <- la_crit()
  x <- tfl_template_expand("lab_abnormal", a)
  t <- la_tab(do.call(tpl_lab_abnormal, c(list(la_ard(x, lb)), a)))
  ast <- "Aspartate Aminotransferase (U/L)"
  for (lev in 1:3) {
    k <- la_count(lb, ast, "Control", function(r) r$MCRIT1ML %in% as.character(lev:3))
    expect_identical(t$Control[t$group == ast & t$label == as.character(lev)],
                     sprintf("%d/%d (%.1f)", k[["n"]], k[["N"]], 100 * k[["n"]] / k[["N"]]),
                     info = lev)
  }
})

test_that("baseline normal: n and N of the subjects normal at baseline", {
  la_skip()
  a <- c(la_args, list(baseline_normal = TRUE))
  x <- tfl_template_expand("lab_abnormal", a)
  t <- la_tab(do.call(tpl_lab_abnormal, c(list(la_ard(x)), a)))
  hgb <- "Hemoglobin (g/L)"
  lb <- adlb[adlb$ONTRTFL == "Y" & adlb$BNRIND == "NORMAL", ]
  k <- la_count(lb, hgb, "Drug X", function(r) r$ANRIND == "LOW")
  expect_identical(t[["Drug X"]][t$group == hgb & t$label == "LOW"],
                   sprintf("%d/%d (%.1f)", k[["n"]], k[["N"]], 100 * k[["n"]] / k[["N"]]))
  k0 <- la_count(adlb[adlb$ONTRTFL == "Y", ], hgb, "Drug X", function(r) r$ANRIND == "LOW")
  expect_lt(k[["N"]], k0[["N"]])
})

test_that("A (the call) = B (the items as the report's content)", {
  la_skip()
  cum <- list(population = "SAF", category = "MCRIT1ML", levels = c("1", "2", "3"),
              cumulative = TRUE)
  for (a in list(la_args, cum)) {
    x <- tfl_template_expand("lab_abnormal", a)
    ard <- la_ard(x, la_crit())
    ta <- la_tab(do.call(tpl_lab_abnormal, c(list(ard), a)))
    b <- suppressMessages(tfl_content_run(tfl_output("T-1", "table", content = x$content),
                                          rtfreporter::normalize_ard(ard)))
    expect_identical(la_tab(b), ta, info = a$category)
  }
})

test_that("the RTF: the heading, the arm and its N, a parameter over its categories", {
  la_skip()
  x <- tfl_template_expand("lab_abnormal", la_args)
  p <- do.call(tpl_lab_abnormal, c(list(la_ard(x)), la_args))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f))
  suppressWarnings(suppressMessages(rtfreporter::generate_rtfreport(
    rtfreporter::rtf_tables(rtfreporter::rtf_document(), rtfreporter::plan_apply(p)), f)))
  r <- paste(readLines(f), collapse = "\n")
  n <- sum(adsl$SAFFL == "Y" & adsl$TRT01A == "Control")
  expect_match(r, sprintf("Control\\line (N=%d)", n), fixed = TRUE)
  expect_match(r, "Parameter / Category", fixed = TRUE)
})
