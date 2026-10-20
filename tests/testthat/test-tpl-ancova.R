# ancova: the change from baseline at one visit, LS means and their difference

an_args <- list(population = "SAF", data = "ADVS", param = "SYSBP", visit = "CYCLE 2 DAY 1")

# a third arm: half of Drug X's subjects as Drug Y
an_three <- function(x) {
  ids <- sort(unique(adsl$USUBJID[adsl$TRT01A == "Drug X"]))
  y <- ids[seq_along(ids) %% 2L == 0L]
  x$TRT01A <- as.character(x$TRT01A)
  x$TRT01A[x$USUBJID %in% y] <- "Drug Y"
  x
}
an_arms <- function(three) if (three) c("Drug X", "Drug Y", "Control") else c("Drug X", "Control")
an_data <- function(shift = 0, three = FALSE) {
  saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
  d <- advs[advs$USUBJID %in% saf & advs$PARAMCD == "SYSBP" & advs$AVISIT == "CYCLE 2 DAY 1", ]
  if (three) d <- an_three(d)
  # the arms' order of a code list: Control last
  d$TRT01A <- factor(d$TRT01A, levels = an_arms(three))
  d$CHG[d$TRT01A == "Drug X"] <- d$CHG[d$TRT01A == "Drug X"] + shift
  d
}
# the program's fmt_pvalue() (inst/helpers/study_helpers.R)
an_pvalue <- function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
# the ARD as the study's program makes it: cardx's LS means; the
# differences by the analysis's own code; the arm (the difference's first)
# as the group
an_ard <- function(x, shift = 0, three = FALSE) {
  d <- an_data(shift, three)
  as_arm <- function(a, what, level) {
    a$group1_level <- level
    a$variable <- what
    a$variable_level <- list(NULL)
    a
  }
  m <- cardx::ard_emmeans_emmeans(d, formula = CHG ~ TRT01A + BASE, method = "lm")
  k <- eval(parse(text = x$ard$analyses[[3L]]$code), list(data = d))
  k <- cards::apply_fmt_fun(cards::update_ard_fmt_fun(k, stat_names = "p.value",
                                                      fmt_fun = an_pvalue))
  sl <- adsl[adsl$SAFFL == "Y", ]
  if (three) sl <- an_three(sl)
  sl$TRT01A <- factor(sl$TRT01A, levels = an_arms(three))
  cards::bind_ard(
    cards::ard_tabulate(sl, variables = TRT01A),
    as_arm(m, "LSMEAN", m$variable_level),
    as_arm(k, "DIFF", lapply(k$variable_level, sub, pattern = " - .*", replacement = "")))
}
an_tab <- function(p) {
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  t
}
an_skip <- function() {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("emmeans")
}

test_that("expanded: the records of the parameter and visit, the model by cardx", {
  x <- tfl_template_expand("ancova", an_args)
  d <- x$ard$analysis_data
  expect_identical(vapply(d, `[[`, "", "id"), c("adsl_saf", "advs_saf_sysbp"))
  expect_identical(as.character(d[[2L]]$where),
                   "PARAMCD == \"SYSBP\" & AVISIT == \"CYCLE 2 DAY 1\"")
  an <- x$ard$analyses
  expect_identical(vapply(an, `[[`, "", "method"),
                   c("categorical", "cardx::ard_emmeans_emmeans", "custom"))
  expect_match(an[[3L]]$code, "ref <- tail(levels(factor(data$TRT01A)), 1)", fixed = TRUE)
  expect_match(an[[3L]]$code,
               "cardx::ard_emmeans_contrast(formula = CHG ~ TRT01A + BASE, method = \"lm\")",
               fixed = TRUE)
  expect_identical(as.character(an[[2L]]$args), "formula = CHG ~ TRT01A + BASE, method = \"lm\"")
  y <- tfl_template_expand("ancova", c(an_args, list(response = "PCHG",
                                                     covariates = c("BASE", "AGE"),
                                                     where = "ANL01FL == \"Y\"")))
  expect_match(y$ard$analyses[[3L]]$code,
               "formula = PCHG ~ TRT01A + BASE + AGE, method = \"lm\"", fixed = TRUE)
  z <- tfl_template_expand("ancova", c(an_args, list(ref = "Drug Y")))
  expect_match(z$ard$analyses[[3L]]$code, "ref <- \"Drug Y\"", fixed = TRUE)
  expect_identical(tfl_template_params("ancova")$ref[c("type", "of")],
                   list(type = "levels", of = "by"))
  expect_match(y$ard$analysis_data[[2L]]$where, "& ANL01FL == \"Y\"", fixed = TRUE)
  expect_error(tfl_template_expand("ancova", list(population = "SAF")), "needs")
})

test_that("the LS means and their difference: as emmeans gives them", {
  an_skip()
  x <- tfl_template_expand("ancova", an_args)
  t <- an_tab(do.call(tpl_ancova, c(list(suppressMessages(an_ard(x))), an_args)))
  expect_identical(names(t), c("group", "label", "Drug X", "Control"))
  em <- emmeans::emmeans(stats::lm(CHG ~ TRT01A + BASE, data = an_data()), "TRT01A")
  s <- as.data.frame(summary(em))
  for (arm in c("Drug X", "Control")) {
    r <- s[s$TRT01A == arm, ]
    expect_identical(t[[arm]][t$label == "LS mean (SE)"],
                     sprintf("%.2f (%.2f)", r$emmean, r$SE), info = arm)
    expect_identical(t[[arm]][t$label == "95% CI"][1L],
                     sprintf("(%.2f, %.2f)", r$lower.CL, r$upper.CL), info = arm)
  }
  pr <- as.data.frame(summary(emmeans::contrast(em, "pairwise"), infer = TRUE))
  diff <- t$label == "Difference (SE)"
  expect_identical(t[["Drug X"]][diff], sprintf("%.2f (%.2f)", pr$estimate, pr$SE))
  expect_true(is.na(t$Control[diff]))
  expect_identical(t[["Drug X"]][t$label == "p-value"], an_pvalue(pr$p.value))
})

test_that("a p-value below 0.0001: as the program's fmt_pvalue() writes it", {
  an_skip()
  x <- tfl_template_expand("ancova", an_args)
  expect_identical(x$ard$analyses[[3L]]$formats, "p.value=pvalue")
  t <- an_tab(do.call(tpl_ancova, c(list(suppressMessages(an_ard(x, shift = 10))), an_args)))
  fit <- stats::lm(CHG ~ TRT01A + BASE, data = an_data(shift = 10))
  expect_lt(summary(fit)$coefficients["TRT01AControl", "Pr(>|t|)"], 1e-4)
  expect_identical(t[["Drug X"]][t$label == "p-value"], "<0.001")
})

test_that("A (the call) = B (the items as the report's content)", {
  an_skip()
  x <- tfl_template_expand("ancova", an_args)
  ard <- suppressMessages(an_ard(x))
  ta <- an_tab(do.call(tpl_ancova, c(list(ard), an_args)))
  b <- suppressMessages(tfl_content_run(tfl_output("T-1", "table", content = x$content),
                                        rtfreporter::normalize_ard(ard)))
  expect_identical(an_tab(b), ta)
})

test_that("three arms: each other arm minus the reference, in its own column", {
  an_skip()
  # the reference: the last arm (Control), or `ref`
  for (ref in list(NULL, "Drug Y")) {
    a <- c(an_args, list(ref = ref))
    x <- tfl_template_expand("ancova", a)
    used <- ref %||% "Control"
    expect_message(ard <- an_ard(x, three = TRUE),
                   paste("ancova: the reference arm is", used), fixed = TRUE)
    t <- an_tab(do.call(tpl_ancova, c(list(ard), a)))
    expect_identical(names(t), c("group", "label", "Drug X", "Drug Y", "Control"))
    d <- an_data(three = TRUE)
    d$TRT01A <- stats::relevel(d$TRT01A, ref = used)
    fit <- stats::lm(CHG ~ TRT01A + BASE, data = d)
    diff <- t$label == "Difference (SE)"
    expect_true(is.na(t[[used]][diff]), info = used)
    for (arm in setdiff(an_arms(TRUE), used)) {
      est <- stats::coef(fit)[[paste0("TRT01A", arm)]]
      se <- sqrt(diag(stats::vcov(fit)))[[paste0("TRT01A", arm)]]
      expect_identical(t[[arm]][diff], sprintf("%.2f (%.2f)", est, se), info = paste(used, arm))
    }
  }
})

test_that("in a study: the ARD program runs, the table from its ARD", {
  skip_on_cran()
  an_skip()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "advs")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "ANC-1")
  o <- tfl_output("T-9-3", "table", population = "SAF", label = "ANCOVA of the change",
                  extends = "defaults", template = list(name = "ancova", args = an_args))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-9-3.yml"))
  st <- tfl_read_study_spec(d)
  expect_message(ard <- tfl_build_ard(st, output_id = "T-9-3", dir = d, save = FALSE),
                 "ancova: the reference arm is")
  expect_true(nrow(ard) > 0L)
  ro <- tfl_resolve_output(tfl_read_output(file.path(d, "spec", "outputs", "T-9-3.yml")), st)
  t <- an_tab(suppressMessages(tfl_content_run(ro, ard)))
  expect_true(nrow(t) > 0L)
  expect_true(any(t$label == "p-value"))
})
