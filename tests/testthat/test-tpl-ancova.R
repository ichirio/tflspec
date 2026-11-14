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
an_arms <- function(three) if (three) c("Control", "Drug X", "Drug Y") else c("Control", "Drug X")
an_data <- function(shift = 0, three = FALSE) {
  saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
  d <- advs[advs$USUBJID %in% saf & advs$PARAMCD == "SYSBP" & advs$AVISIT == "CYCLE 2 DAY 1", ]
  if (three) d <- an_three(d)
  # the arms' order of a code list: Control (the reference) first
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
  expect_identical(vapply(d, `[[`, "", "id"), c("adsl_sub", "advs_sub"))
  expect_identical(as.character(d[[2L]]$where),
                   "PARAMCD == \"SYSBP\" & AVISIT == \"CYCLE 2 DAY 1\"")
  an <- x$ard$analyses
  expect_identical(vapply(an, `[[`, "", "method"),
                   c("categorical", "cardx::ard_emmeans_emmeans", "custom"))
  expect_match(an[[3L]]$code, "ref <- levels(factor(data$TRT01A))[1L]", fixed = TRUE)
  expect_match(an[[3L]]$code, "fit <- stats::lm(CHG ~ TRT01A + BASE, data = ref_data)", fixed = TRUE)
  expect_match(an[[3L]]$code, "emmeans::emmeans(fit, specs = ~ TRT01A, data = ref_data)", fixed = TRUE)
  expect_match(an[[3L]]$code,
               "emmeans::contrast(method = \"trt.vs.ctrl\", ref = 1L, adjust = \"none\")",
               fixed = TRUE)
  expect_identical(as.character(an[[2L]]$args), "formula = CHG ~ TRT01A + BASE, method = \"lm\"")
  y <- tfl_template_expand("ancova", c(an_args, list(response = "PCHG",
                                                     covariates = c("BASE", "AGE"),
                                                     where = "ANL01FL == \"Y\"")))
  expect_match(y$ard$analyses[[3L]]$code, "stats::lm(PCHG ~ TRT01A + BASE + AGE, data = ref_data)",
               fixed = TRUE)
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
  expect_identical(names(t), c("group", "label", "Control", "Drug X"))
  em <- emmeans::emmeans(stats::lm(CHG ~ TRT01A + BASE, data = an_data()), "TRT01A")
  s <- as.data.frame(summary(em))
  for (arm in c("Drug X", "Control")) {
    r <- s[s$TRT01A == arm, ]
    expect_identical(t[[arm]][t$label == "LS mean (SE)"],
                     sprintf("%.2f (%.2f)", r$emmean, r$SE), info = arm)
    expect_identical(t[[arm]][t$label == "95% CI"][1L],
                     sprintf("(%.2f, %.2f)", r$lower.CL, r$upper.CL), info = arm)
  }
  pr <- as.data.frame(summary(emmeans::contrast(em, "revpairwise"), infer = TRUE))
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
  expect_lt(summary(fit)$coefficients["TRT01ADrug X", "Pr(>|t|)"], 1e-4)
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
  # the reference: the first arm (Control), or `ref`
  for (ref in list(NULL, "Drug Y")) {
    a <- c(an_args, list(ref = ref))
    x <- tfl_template_expand("ancova", a)
    used <- ref %||% "Control"
    expect_message(ard <- an_ard(x, three = TRUE),
                   paste("ancova: the reference arm is", used), fixed = TRUE)
    t <- an_tab(do.call(tpl_ancova, c(list(ard), a)))
    expect_identical(names(t), c("group", "label", "Control", "Drug X", "Drug Y"))
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

# the differences by the analysis's own code (the rows before the post step)
an_diffs <- function(adjust = NULL, three = TRUE) {
  a <- c(an_args, if (!is.null(adjust)) list(adjust = adjust))
  x <- tfl_template_expand("ancova", a)
  k <- suppressMessages(eval(parse(text = x$ard$analyses[[3L]]$code),
                             list(data = an_data(three = three))))
  one <- function(s) unlist(k$stat[k$stat_name == s])
  data.frame(contrast = unlist(k$variable_level[k$stat_name == "estimate"]),
             estimate = one("estimate"), conf.low = one("conf.low"),
             conf.high = one("conf.high"), p.value = one("p.value"))
}

test_that("adjust: the default is no adjustment, as lm and a t-test give it by hand", {
  an_skip()
  expect_identical(tfl_template_params("ancova")$adjust$default, "none")
  x <- tfl_template_expand("ancova", an_args)
  expect_match(x$ard$analyses[[3L]]$code, "adjust = \"none\"", fixed = TRUE)
  k <- an_diffs()
  d <- an_data(three = TRUE)
  d$TRT01A <- stats::relevel(d$TRT01A, ref = "Control")
  fit <- stats::lm(CHG ~ TRT01A + BASE, data = d)
  cf <- summary(fit)$coefficients
  q <- stats::qt(0.975, fit$df.residual)
  for (arm in c("Drug X", "Drug Y")) {
    r <- cf[paste0("TRT01A", arm), ]
    i <- k$contrast == paste(arm, "- Control")
    expect_equal(k$estimate[i], r[["Estimate"]], info = arm)
    expect_equal(k$conf.low[i], r[["Estimate"]] - q * r[["Std. Error"]], info = arm)
    expect_equal(k$conf.high[i], r[["Estimate"]] + q * r[["Std. Error"]], info = arm)
    expect_equal(k$p.value[i], r[["Pr(>|t|)"]], info = arm)
  }
})

test_that("adjust = \"dunnettx\" and the others: emmeans' own numbers", {
  an_skip()
  d <- an_data(three = TRUE)
  em <- emmeans::emmeans(stats::lm(CHG ~ TRT01A + BASE, data = d), ~ TRT01A)
  for (adj in c("dunnettx", "sidak", "bonferroni", "holm", "scheffe")) {
    ref <- as.data.frame(suppressMessages(summary(
      emmeans::contrast(em, "trt.vs.ctrl", ref = 1L, adjust = adj), infer = TRUE)))
    k <- an_diffs(adj)
    expect_identical(k$contrast, as.character(ref$contrast), info = adj)
    expect_equal(k$estimate, ref$estimate, info = adj)
    expect_equal(k$conf.low, ref$lower.CL, info = adj)
    expect_equal(k$conf.high, ref$upper.CL, info = adj)
    expect_equal(k$p.value, ref$p.value, info = adj)
    expect_match(tfl_template_expand("ancova", c(an_args, list(adjust = adj)))$ard$analyses[[3L]]$code,
                 sprintf("adjust = \"%s\")", adj), fixed = TRUE)
  }
  # an adjusted p-value is never below the unadjusted one
  expect_true(all(an_diffs("dunnettx")$p.value >= an_diffs("none")$p.value))
})

test_that("adjust = \"tukey\": the all-pairs Tukey family, as cardx's pairwise contrast gave it", {
  an_skip()
  x <- tfl_template_expand("ancova", c(an_args, list(adjust = "tukey")))
  code <- x$ard$analyses[[3L]]$code
  expect_match(code, "method = \"revpairwise\", adjust = \"tukey\"", fixed = TRUE)
  expect_no_match(code, "trt.vs.ctrl", fixed = TRUE)
  k <- an_diffs("tukey")
  # cardx's pairwise contrast with the reference arm last, as before this
  # argument put it first: the same numbers
  d <- an_data(three = TRUE)
  d$TRT01A <- factor(d$TRT01A, levels = c("Drug X", "Drug Y", "Control"))
  old <- cardx::ard_emmeans_contrast(d, formula = CHG ~ TRT01A + BASE, method = "lm",
                                     response_type = "continuous")
  g <- function(s) unlist(old$stat[old$stat_name == s])
  lab <- unlist(old$variable_level[old$stat_name == "estimate"])
  keep <- endsWith(lab, " - Control")
  expect_identical(sum(keep), 2L)
  expect_identical(k$contrast, lab[keep])
  expect_equal(k$estimate, g("estimate")[keep])
  expect_equal(k$conf.low, g("conf.low")[keep])
  expect_equal(k$conf.high, g("conf.high")[keep])
  expect_equal(k$p.value, g("p.value")[keep])
  # not the single-step trt.vs.ctrl family (emmeans changes tukey to sidak there)
  s <- an_diffs("sidak")
  expect_false(isTRUE(all.equal(k$p.value, s$p.value)))
})

test_that("adjust: every emmeans value runs; any other is refused with a message", {
  an_skip()
  ch <- tfl_template_params("ancova")$adjust$choices
  expect_setequal(ch, c("none", "tukey", "dunnettx", "sidak", "bonferroni", "holm", "hommel",
                        "hochberg", "BH", "BY", "fdr", "mvt", "scheffe"))
  for (adj in ch) {
    k <- an_diffs(adj)
    expect_identical(nrow(k), 2L, info = adj)
    expect_true(all(is.finite(k$p.value)), info = adj)
  }
  for (bad in list("Tukey", "foo", c("none", "tukey"), 1, NA_character_)) {
    expect_error(tfl_template_expand("ancova", c(an_args, list(adjust = bad))),
                 "ancova: `adjust` is .*must be one of \"none\", \"tukey\", \"dunnettx\"",
                 info = format(bad))
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
  o <- tfl_output("T-9-3", "table", common = list(population = "SAF"), label = "ANCOVA of the change",
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

test_that("a where with | keeps its brackets: only the parameter's records at the visit", {
  w <- "ONTRTFL == \"Y\" | ABLFL == \"Y\""
  x <- tfl_template_expand("ancova", c(an_args, list(where = w)))
  wh <- as.character(x$ard$analysis_data[[2L]]$where)
  expect_identical(wh, paste0("PARAMCD == \"SYSBP\" & AVISIT == \"CYCLE 2 DAY 1\" & (", w, ")"))
  kept <- advs[eval(parse(text = wh), advs), ]
  expect_gt(nrow(kept), 0L)
  expect_identical(unique(kept$PARAMCD), "SYSBP")
  expect_identical(unique(kept$AVISIT), "CYCLE 2 DAY 1")
})

# ---- the reference is the first level (#722): Placebo = 1, Drug B = 2, Drug A = 3 ----
# (alphabetical order is Drug A, Drug B, Placebo, so the default shows the level order)
rf_arms <- c("Placebo", "Drug B", "Drug A")
rf_data <- function() {
  set.seed(722)
  n <- 60L
  d <- data.frame(USUBJID = sprintf("DMY-%03d", seq_len(n)),
                  TRT01A = rep(rf_arms, each = 20L), TRT01AN = rep(1:3, each = 20L),
                  BASE = stats::rnorm(n, 100, 10))
  d$CHG <- -0.1 * d$BASE + c(0, -3, -6)[d$TRT01AN] + stats::rnorm(n, 0, 4)
  # the data step's factor (order_levels()): the arm in the order of TRT01AN
  d$TRT01A <- factor(d$TRT01A, levels = rf_arms)
  d
}
# the analysis's own code on these data: the message, the rows before the post step
rf_run <- function(ref = NULL, adjust = "none", d = rf_data()) {
  a <- c(an_args, if (!is.null(ref)) list(ref = ref), list(adjust = adjust))
  x <- tfl_template_expand("ancova", a)
  msg <- character()
  k <- withCallingHandlers(eval(parse(text = x$ard$analyses[[3L]]$code), list(data = d)),
                           message = function(m) {
                             msg <<- c(msg, trimws(conditionMessage(m)))
                             invokeRestart("muffleMessage")
                           })
  one <- function(s) unlist(k$stat[k$stat_name == s])
  list(msg = msg, x = x, k = data.frame(
    contrast = unlist(k$variable_level[k$stat_name == "estimate"]),
    estimate = one("estimate"), se = one("std.error"), df = one("df"),
    conf.low = one("conf.low"), conf.high = one("conf.high"), p.value = one("p.value")))
}

test_that("reference: the default is the first level (Placebo), `ref` puts another arm first", {
  an_skip()
  d <- rf_data()
  for (ref in list(NULL, "Placebo", "Drug B")) {
    used <- ref %||% "Placebo"
    for (adj in c("none", "tukey")) {
      r <- rf_run(ref, adj)
      expect_identical(r$msg, paste("ancova: the reference arm is", used), info = paste(used, adj))
      # by hand: lm on the arm relevelled to the reference; each other arm minus it
      d2 <- d
      d2$TRT01A <- stats::relevel(d2$TRT01A, ref = used)
      fit <- stats::lm(CHG ~ TRT01A + BASE, data = d2)
      cf <- summary(fit)$coefficients
      others <- setdiff(rf_arms, used)
      expect_identical(r$k$contrast, paste(others, "-", used), info = paste(used, adj))
      for (arm in others) {
        i <- r$k$contrast == paste(arm, "-", used)
        e <- cf[paste0("TRT01A", arm), "Estimate"]
        se <- cf[paste0("TRT01A", arm), "Std. Error"]
        tt <- e / se
        q <- if (adj == "none") stats::qt(0.975, fit$df.residual) else
          stats::qtukey(0.95, 3, fit$df.residual) / sqrt(2)
        p <- if (adj == "none") 2 * stats::pt(-abs(tt), fit$df.residual) else
          stats::ptukey(sqrt(2) * abs(tt), 3, fit$df.residual, lower.tail = FALSE)
        info <- paste(used, adj, arm)
        expect_equal(r$k$estimate[i], e, info = info)
        expect_equal(r$k$se[i], se, info = info)
        expect_equal(r$k$conf.low[i], e - q * se, info = info)
        expect_equal(r$k$conf.high[i], e + q * se, info = info)
        expect_equal(r$k$p.value[i], p, info = info)
      }
    }
  }
  # the arm minus the reference, whatever the reference: Drug A - Placebo is
  # the negative of Placebo - Drug A
  a <- rf_run("Placebo")$k
  b <- rf_run("Drug A")$k
  expect_equal(a$estimate[a$contrast == "Drug A - Placebo"],
               -b$estimate[b$contrast == "Placebo - Drug A"])
})

test_that("reference: the table says `vs Placebo`, and its columns show the contrast's sign", {
  an_skip()
  d <- rf_data()
  sl <- d[!duplicated(d$USUBJID), ]
  for (ref in c("Placebo", "Drug B")) {
    r <- rf_run(ref)
    a <- c(an_args, list(ref = ref))
    lab <- Filter(function(it) identical(it$fn, "plan_labels"), r$x$content)
    expect_length(lab, 1L)
    expect_match(paste(unlist(lab[[1L]]$args), collapse = "|"),
                 paste0("Difference in LS means (vs ", ref, ")"), fixed = TRUE)
    k <- eval(parse(text = r$x$ard$analyses[[3L]]$code), list(data = d))
    k <- suppressMessages(cards::apply_fmt_fun(
      cards::update_ard_fmt_fun(k, stat_names = "p.value", fmt_fun = an_pvalue)))
    m <- cardx::ard_emmeans_emmeans(d, formula = CHG ~ TRT01A + BASE, method = "lm")
    as_arm <- function(z, what, level) {
      z$group1_level <- level
      z$variable <- what
      z$variable_level <- list(NULL)
      z
    }
    ard <- cards::bind_ard(
      cards::ard_tabulate(sl, variables = TRT01A),
      as_arm(m, "LSMEAN", m$variable_level),
      as_arm(k, "DIFF", lapply(k$variable_level, sub, pattern = " - .*", replacement = "")))
    t <- an_tab(do.call(tpl_ancova, c(list(ard), a)))
    expect_true(any(grepl(paste0("vs ", ref, ")"), t$group, fixed = TRUE) |
                  grepl(paste0("vs ", ref, ")"), t$label, fixed = TRUE)), info = ref)
    # the reference arm's column has no difference; each other arm's is its estimate
    diff <- t$label == "Difference (SE)"
    expect_true(is.na(t[[ref]][diff]), info = ref)
    for (arm in setdiff(rf_arms, ref)) {
      i <- r$k$contrast == paste(arm, "-", ref)
      expect_identical(t[[arm]][diff], sprintf("%.2f (%.2f)", r$k$estimate[i], r$k$se[i]),
                       info = paste(ref, arm))
    }
  }
})
