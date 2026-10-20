# ancova: the change from baseline at one visit, LS means and their difference

an_args <- list(population = "SAF", data = "ADVS", param = "SYSBP", visit = "CYCLE 2 DAY 1")

an_data <- function(shift = 0) {
  saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
  d <- advs[advs$USUBJID %in% saf & advs$PARAMCD == "SYSBP" & advs$AVISIT == "CYCLE 2 DAY 1", ]
  # the arms' order of a code list: the arm compared with last
  d$TRT01A <- factor(d$TRT01A, levels = c("Drug X", "Control"))
  d$CHG[d$TRT01A == "Drug X"] <- d$CHG[d$TRT01A == "Drug X"] + shift
  d
}
# the program's fmt_pvalue() (inst/helpers/study_helpers.R)
an_pvalue <- function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
# the ARD as the study's program makes it: cardx's LS means and their
# difference, the arm (the difference's first) as the group
an_ard <- function(shift = 0) {
  d <- an_data(shift)
  as_arm <- function(a, what, level) {
    a$group1_level <- level
    a$variable <- what
    a$variable_level <- list(NULL)
    a
  }
  m <- cardx::ard_emmeans_emmeans(d, formula = CHG ~ TRT01A + BASE, method = "lm")
  k <- cardx::ard_emmeans_contrast(d, formula = CHG ~ TRT01A + BASE, method = "lm")
  k <- cards::apply_fmt_fun(cards::update_ard_fmt_fun(k, stat_names = "p.value",
                                                      fmt_fun = an_pvalue))
  cards::bind_ard(
    cards::ard_tabulate(adsl[adsl$SAFFL == "Y", ], variables = TRT01A),
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
                   c("categorical", "cardx::ard_emmeans_emmeans", "cardx::ard_emmeans_contrast"))
  expect_identical(as.character(an[[2L]]$args), "formula = CHG ~ TRT01A + BASE, method = \"lm\"")
  y <- tfl_template_expand("ancova", c(an_args, list(response = "PCHG",
                                                     covariates = c("BASE", "AGE"),
                                                     where = "ANL01FL == \"Y\"")))
  expect_identical(as.character(y$ard$analyses[[3L]]$args),
                   "formula = PCHG ~ TRT01A + BASE + AGE, method = \"lm\"")
  expect_match(y$ard$analysis_data[[2L]]$where, "& ANL01FL == \"Y\"", fixed = TRUE)
  expect_error(tfl_template_expand("ancova", list(population = "SAF")), "needs")
})

test_that("the LS means and their difference: as emmeans gives them", {
  an_skip()
  t <- an_tab(do.call(tpl_ancova, c(list(an_ard()), an_args)))
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
  t <- an_tab(do.call(tpl_ancova, c(list(an_ard(shift = 10)), an_args)))
  fit <- stats::lm(CHG ~ TRT01A + BASE, data = an_data(shift = 10))
  expect_lt(summary(fit)$coefficients["TRT01AControl", "Pr(>|t|)"], 1e-4)
  expect_identical(t[["Drug X"]][t$label == "p-value"], "<0.001")
})

test_that("A (the call) = B (the items as the report's content)", {
  an_skip()
  x <- tfl_template_expand("ancova", an_args)
  ard <- an_ard()
  ta <- an_tab(do.call(tpl_ancova, c(list(ard), an_args)))
  b <- suppressMessages(tfl_content_run(tfl_output("T-1", "table", content = x$content),
                                        rtfreporter::normalize_ard(ard)))
  expect_identical(an_tab(b), ta)
})
