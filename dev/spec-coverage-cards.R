# How much of cards and cardx the table / listing SPEC (the ARD definition's
# analysis rows) can write: one row a function's argument, its status, where
# tflspec says it.  Writes inst/dev/spec-coverage-cards.csv and prints the
# weighted percentages for inst/dev/spec-coverage.md.
#
#   Rscript dev/spec-coverage-cards.R [out.csv]
#
# Run with the tflspec to measure installed, and cards / cardx from CRAN.
#
# Scope: every ard_*() cards and cardx export (their data.frame method's
# arguments, as tfl_ard_args() reads them, and the arguments they pass on
# through `...` to the function doing the work: inst/ard/passed_args.csv),
# cards' helpers an analysis row writes in its `post` column
# (inst/ard/utilities.csv, use "post") and, since 2026-11-01, the helpers
# the generated code writes from a row's columns (cards'
# continuous_summary_fns(), label_round(), alias_as_fmt_fun(),
# apply_fmt_fun(): the statistics and formats columns) and cardx's
# proportion_ci_*() (ard_categorical_ci(method =)'s choices, their
# arguments ard_categorical_ci()'s), and since 2026-11-06 the arguments of
# the model cardx fits, written in `method.args` (survival::survfit(),
# survival::coxph(), stats::glm(), stats::lm(), mmrm::mmrm(): the
# Kaplan-Meier, Cox, logistic, ANCOVA and MMRM models; inst/ard/model_args.csv).
#
# Status of an argument:
#   yes      an analysis row's own column writes it (by, variables, strata,
#            denominator, statistics, the data), or the catalog
#            (inst/ard/args.csv) describes it -- its kind and a hint: the
#            GUI shows it -- and the check judges the value written in
#            `args` (`post`) by that kind (TRUE / FALSE, a number, one of
#            the choices, text, a formula, a column, a list ...)
#   partial  written as R in `args` (or `post`), its name checked against
#            the installed function, but not described, or its value not
#            judged (R taken as it is)
#   no       a SPEC cannot write it
# Not offered by design (old names, survey designs, ard_formals(): see
# ?tfl_ard_functions) and the helpers tflspec writes itself are noted
# "[by design]" and left out of the percentages.
#
# A function's coverage is its arguments' mean (yes 1, partial 0.5, no 0);
# the percentage is the functions' mean weighted by their use in clinical
# tables: 3 what demographics, disposition, AE by SOC / PT and severity,
# lab summary and shift, vital signs and model-based efficacy (ANCOVA /
# MMRM estimates, CMH, KM, log-rank, Cox) use; 2 sometimes; 1 rarely.

suppressPackageStartupMessages(library(tflspec))
stopifnot(requireNamespace("cards"), requireNamespace("cardx"))
ns <- asNamespace("tflspec")

# Does the check judge the value of this kind of argument?  (A tflspec
# before the check of values: none.)
judged <- function(kind, arg, call) {
  if (!exists(".arg_value_problem", ns)) return(FALSE)
  if (kind == "code") {
    shape <- get0(".ard_code_shape", ns, ifnotfound = function(...) NA)
    return(!is.na(shape(sub("^.*::", "", call), arg)))
  }
  kind %in% c("logical", "number", "choice", "text", "formula", "column",
              "columns", "levels")
}
`%||%` <- function(a, b) if (is.null(a)) b else a

w3 <- c("cards::ard_summary", "cards::ard_tabulate", "cards::ard_tabulate_value",
        "cards::ard_missing", "cards::ard_total_n", "cards::ard_stack",
        "cards::ard_stack_hierarchical", "cards::ard_stack_hierarchical_count",
        "cards::ard_hierarchical", "cards::ard_hierarchical_count",
        "cardx::ard_tabulate_max", "cardx::ard_tabulate_abnormal",
        "cardx::ard_categorical_ci", "cardx::ard_continuous_ci",
        "cardx::ard_stats_t_test", "cardx::ard_stats_wilcox_test",
        "cardx::ard_stats_chisq_test", "cardx::ard_stats_fisher_test",
        "cardx::ard_stats_mantelhaen_test", "cardx::ard_stats_prop_test",
        "cardx::ard_regression", "cardx::ard_emmeans_emmeans",
        "cardx::ard_emmeans_contrast", "cardx::ard_car_anova",
        "cardx::ard_survival_survfit", "cardx::ard_survival_survdiff",
        "cardx::ard_incidence_rate", "cards::add_calculated_row",
        "cards::filter_ard_hierarchical", "cards::sort_ard_hierarchical")
w2 <- c("cards::ard_strata", "cards::ard_pairwise", "cards::ard_mvsummary",
        "cards::ard_tabulate_rows", "cards::ard_attributes",
        "cardx::ard_stats_aov", "cardx::ard_stats_anova",
        "cardx::ard_stats_kruskal_test", "cardx::ard_stats_paired_t_test",
        "cardx::ard_stats_paired_wilcox_test", "cardx::ard_stats_mcnemar_test",
        "cardx::ard_stats_mcnemar_test_long", "cardx::ard_stats_poisson_test",
        "cardx::ard_regression_basic", "cardx::ard_survival_survfit_diff",
        "cardx::ard_stats_oneway_test", "cardx::ard_stats_t_test_onesample",
        "cardx::ard_stats_wilcox_test_onesample", "cardx::ard_smd_smd",
        "cardx::ard_effectsize_cohens_d", "cardx::ard_effectsize_paired_cohens_d",
        "cardx::ard_effectsize_hedges_g", "cardx::ard_effectsize_paired_hedges_g",
        "cardx::ard_stats_mantelhaen_test", "cards::diff_ard_hierarchical",
        "cards::update_ard_stat_label", "cards::replace_null_statistic",
        "cards::update_ard_fmt_fun")
w3 <- c(w3, "cards::continuous_summary_fns", "cards::label_round",
        "cardx::proportion_ci_wilson", "cardx::proportion_ci_clopper_pearson")
w2 <- c(w2, "cards::alias_as_fmt_fun", "cards::apply_fmt_fun",
        "cardx::proportion_ci_wald", "cardx::proportion_ci_strat_wilson")
weight <- function(call) if (call %in% w3) 3 else if (call %in% w2) 2 else 1

rows <- list()
add <- function(group, fn, arg, s, where = "", note = "") {
  rows[[length(rows) + 1L]] <<- data.frame(
    package = sub("::.*$", "", fn), `function` = sub("^.*::", "", fn),
    argument = arg, covered = s, where = where, note = note, group = group,
    weight = weight(fn), check.names = FALSE, stringsAsFactors = FALSE)
}

# ---- the ard_*() functions ----------------------------------------------
# (and the arguments each passes on through `...`, the scope's: this
# checkout's catalog, so a tflspec before it is measured on the same scope)
scope_passed <- utils::read.csv("inst/ard/passed_args.csv", stringsAsFactors = FALSE)
cat_file <- function(to) if (nzchar(to)) "inst/ard/passed_args.csv" else "inst/ard/args.csv"
f <- tfl_ard_functions()
keys <- tfl_ard_methods()
for (i in seq_len(nrow(f))) {
  call <- f$call[i]
  grp <- if (startsWith(call, "cardx")) "cardx ard_*()" else "cards ard_*()"
  a <- tfl_ard_args(call)
  if (is.null(a)) next
  kw <- keys$method[keys$call == call]
  via <- if (length(kw)) sprintf("method: %s (or %s)", kw[1L], call) else
    paste("method:", call)
  # the arguments it passes on through `...` (inst/ard/passed_args.csv):
  # in scope whichever tflspec is measured; one before tfl_ard_args() gave
  # them is written as R in args, passed on, not described
  passed <- if ("passed_to" %in% names(a)) a$passed_to else rep("", nrow(a))
  extra <- scope_passed[scope_passed$call == call & !scope_passed$arg %in% a$arg, ]
  for (k in seq_len(nrow(extra))) {
    add(grp, call, extra$arg[k], if (f$offered[i]) "partial" else "no", paste0(via, "; args"),
        paste0("passed on to ", extra$passed_to[k], "; R in args, not described"))
  }
  for (j in seq_len(nrow(a))) {
    arg <- a$arg[j]
    to <- if (nzchar(passed[j])) paste0("passed on to ", passed[j], "; ") else ""
    if (!f$offered[i]) {
      add(grp, call, arg, "no", via, "[by design] not offered for a new analysis")
    } else if (a$column[j] != "args") {
      add(grp, call, arg, "yes", paste0(via, "; column `", a$column[j], "`"),
          "the analysis row's column")
    } else if (nzchar(a$hint[j]) && judged(a$kind[j], arg, call)) {
      add(grp, call, arg, "yes", paste0(via, "; args"),
          paste0(to, cat_file(passed[j]), ": ", a$kind[j], "; its value judged"))
    } else if (nzchar(a$hint[j])) {
      add(grp, call, arg, "partial", paste0(via, "; args"),
          paste0(to, cat_file(passed[j]), ": ", a$kind[j], "; R taken as it is"))
    } else {
      add(grp, call, arg, "partial", paste0(via, "; args"),
          paste0("R in args; kind from its default (", a$kind[j], "), no hint"))
    }
  }
}

# ---- the helpers a row writes in `post` ----------------------------------
u <- utils::read.csv(system.file("ard", "utilities.csv", package = "tflspec"),
                     stringsAsFactors = FALSE)
for (k in which(u$use == "post")) {
  call <- paste0(u$package[k], "::", u$`function`[k])
  a <- tfl_ard_args(call)
  if (is.null(a)) next
  for (j in seq_len(nrow(a))) {
    arg <- a$arg[j]
    if (j == 1L) {
      add("cards post helpers", call, arg, "yes", "post", "the analysis's ARD (left out)")
    } else if (nzchar(a$hint[j]) && judged(a$kind[j], arg, call) &&
               exists(".post_args_unknown", ns)) {
      add("cards post helpers", call, arg, "yes", "post",
          paste0("inst/ard/args.csv: ", a$kind[j], "; its name and value judged"))
    } else if (nzchar(a$hint[j])) {
      add("cards post helpers", call, arg, "partial", "post",
          paste0("inst/ard/args.csv: ", a$kind[j], "; R taken as it is"))
    } else {
      add("cards post helpers", call, arg, "partial", "post", "R in post; no hint")
    }
  }
}

# ---- the helpers the generated code writes from a row's columns ----------
# (the statistics and formats columns; tflspec writes the call itself).
# A tflspec before the formats column wrote fmt_fun (.fmt_r) or the
# statistics column continuous_summary_fns() (.stat_arg): no.
has <- function(f) exists(f, ns)
helper_rows <- list(
  list("cards::continuous_summary_fns", "summaries", has(".stat_arg"),
       "column `statistics` (a continuous analysis)", "cards' own statistics, in the order asked"),
  list("cards::continuous_summary_fns", "other_stats", has(".stat_arg"),
       "column `statistics`; tfl_ard_statistics(fun =)",
       "a statistic of the statistics catalog with its own function (tfl_stats[...])"),
  list("cards::label_round", "digits", has(".fmt_r"), "column `formats` (xx.x, 2)",
       "the number of decimals"),
  # (since 2026-11-03 the formats column takes xx.x*1000: any scale)
  if (isTRUE(tryCatch(ns$.fmt_ok("xx.x*1000"), error = function(e) FALSE))) {
    list("cards::label_round", "scale", TRUE, "column `formats` (xx.x%, xx.x*1000)",
         "100 (a proportion as a percent) or any whole number (a rate per 1,000)")
  } else {
    list("cards::label_round", "scale", "partial", "column `formats` (xx.x%)",
         "100 only (a proportion as a percent); another scale is R in args / post")
  },
  list("cards::label_round", "width", "by design", "",
       "[by design] the cell's width and alignment are rtfreporter's (decimal alignment), not padded text"),
  list("cards::alias_as_fmt_fun", "x", has(".fmt_r"), "column `formats`",
       "a number of decimals, xx.x, xx.x%, xx.x*1000, pvalue"),
  list("cards::alias_as_fmt_fun", "variable", has(".fmt_fun_arg"), "column `formats` (AGE:sd=xx.xx)",
       "a variable's own format"),
  list("cards::alias_as_fmt_fun", "stat_name", has(".fmt_r"), "column `formats` (sd=xx.xx)",
       "a statistic's format"),
  list("cards::apply_fmt_fun", "x", TRUE, "the pipe", "the analysis's ARD"),
  list("cards::apply_fmt_fun", "replace", has(".post_fmt_split"), "tflspec fills it",
       "TRUE after a post step that sets formats"))
ci_map <- c(x = "column `variables` (and `by`)", conf.level = "args: conf.level",
            correct = "args: method = \"waldcc\" / \"wilsoncc\" / \"strat_wilsoncc\"",
            strata = "args: strata", weights = "args: weights",
            max.iterations = "args: max.iterations")
for (fn in grep("^proportion_ci_", getNamespaceExports("cardx"), value = TRUE)) {
  for (arg in names(formals(getExportedValue("cardx", fn)))) {
    helper_rows[[length(helper_rows) + 1L]] <- list(
      paste0("cardx::", fn), arg, arg %in% names(ci_map),
      paste0("method: proportion_ci (cardx::ard_categorical_ci); ", ci_map[arg] %||% ""),
      "ard_categorical_ci()'s argument, its value judged there")
  }
}
for (h in helper_rows) {
  st <- if (isTRUE(h[[3L]])) "yes" else if (identical(h[[3L]], FALSE)) "no" else
    if (identical(h[[3L]], "by design")) "no" else h[[3L]]
  grp <- if (startsWith(h[[1L]], "cardx")) "cardx CI helpers" else "cards code helpers"
  add(grp, h[[1L]], h[[2L]], st, h[[4L]], h[[5L]])
}

# ---- the arguments of the model cardx fits (since 2026-11-06) -------------
# Written in `method.args = list(...)` of ard_survival_survfit() (to
# survival::survfit()), ard_regression(), ard_emmeans_*() and
# ard_stats_anova() (to `package::method()`: a Cox, logistic, ANCOVA or
# MMRM model), or in a model's call in `x` (ard_survival_survfit_diff(x =
# survival::survfit(...))).  The model function's own arguments and those
# its `...` reach (survfitKM(), glm.control(), coxph.control(),
# mmrm_control()); formula and data are cardx's, x and y the fit's own.
# A tflspec before inst/ard/model_args.csv takes them as R, its names not
# checked: partial.
model_fns <- list(
  `survival::survfit` = c("survival:::survfit.formula", "survival:::survfitKM"),
  `survival::coxph` = c("survival::coxph", "survival::coxph.control"),
  `stats::glm` = c("stats::glm", "stats::glm.control"),
  `stats::lm` = "stats::lm",
  `mmrm::mmrm` = c("mmrm::mmrm", "mmrm::mmrm_control", "mmrm:::h_get_optimizers"))
model_by_design <- c(
  subset = "the population's and the analysis's where",
  na.action = "the analysis data's missing values are the derivation's",
  model = "a part of the fitted object", qr = "a part of the fitted object",
  method = "the fitting routine (glm.fit, qr)", control = "written as its parts",
  init = "starting values", start = "starting values", etastart = "starting values",
  mustart = "starting values", tt = "time-dependent terms", statedata = "multi-state",
  istate = "multi-state", etype = "multi-state", error = "multi-state",
  influence = "a part of the fitted object", type = "survfitKM()'s internal",
  entry = "a part of the fitted object", time0 = "multi-state",
  nocenter = "centring", n_cores = "computing", optimizer = "computing",
  optimizer_fun = "computing", optimizer_args = "computing",
  optimizer_control = "computing", disable_theta_vcov = "computing",
  emmeans_gcomp_vars = "emmeans' own", drop_visit_levels = "computing",
  optimizers = "computing", toler.chol = "numerical tolerance",
  toler.inf = "numerical tolerance", survcheckallow = "data checks")
mfun <- function(k) {
  pkg <- sub(":::?.*$", "", k)
  if (!requireNamespace(pkg, quietly = TRUE)) return(NULL)
  get0(sub("^.*:::?", "", k), envir = asNamespace(pkg), mode = "function")
}
mcat <- utils::read.csv("inst/ard/model_args.csv", stringsAsFactors = FALSE,
                        na.strings = "")
has_model_check <- exists(".model_args_problems", ns)
model_judged <- c("logical", "number", "choice", "text", "variable", "family", "list")
for (key in names(model_fns)) {
  fs <- lapply(model_fns[[key]], mfun)
  if (any(vapply(fs, is.null, NA))) next
  args <- setdiff(unique(unlist(lapply(fs, function(f) names(formals(f))))),
                  c("formula", "data", "...", "x", "y"))
  # (the arguments a method keyword writes them for)
  via <- switch(key,
    `survival::survfit` = "method: km / km_diff; args: method.args = list(...) or x = survival::survfit(...)",
    `mmrm::mmrm` = "method: lsmeans / lsmeans_diff (method = \"mmrm\", package = \"mmrm\"); args: method.args",
    sprintf("method: regression / lsmeans / nested_models (method = \"%s\"); args: method.args or x = %s(...)",
            sub("^.*::", "", key), key))
  w3 <- c(w3, key)
  for (a in args) {
    r <- mcat[mcat$fn == key & mcat$arg == a, , drop = FALSE]
    if (a %in% names(model_by_design) && !nrow(r)) {
      add("cardx model arguments", key, a, "no", via,
          paste0("[by design] ", model_by_design[[a]]))
    } else if (!has_model_check) {
      add("cardx model arguments", key, a, "partial", via,
          "R in method.args / the model's call; not described, its name not checked")
    } else if (nrow(r) && r$kind[1L] %in% model_judged) {
      add("cardx model arguments", key, a, "yes", via,
          paste0("inst/ard/model_args.csv: ", r$kind[1L], "; its name and value judged"))
    } else if (nrow(r)) {
      add("cardx model arguments", key, a, "partial", via,
          paste0("inst/ard/model_args.csv: ", r$kind[1L], "; its name checked, R taken as it is"))
    } else {
      add("cardx model arguments", key, a, "partial", via,
          "R in method.args; its name checked against the installed function, not described")
    }
  }
}

cov <- do.call(rbind, rows)
out <- commandArgs(TRUE)[1L]
if (is.na(out)) out <- "inst/dev/spec-coverage-cards.csv"
utils::write.csv(cov[c("package", "function", "argument", "covered", "where",
                       "note", "group", "weight")],
                 out, row.names = FALSE, na = "")

score <- c(yes = 1, partial = 0.5, no = 0)
pct <- function(d) {
  d <- d[!startsWith(d$note, "[by design]"), , drop = FALSE]
  d$score <- score[d$covered]
  fn <- stats::aggregate(score ~ package + `function` + weight, data = d, FUN = mean)
  sprintf("%.1f%% (%d functions, %d arguments: yes %d / partial %d / no %d)",
          100 * sum(fn$score * fn$weight) / sum(fn$weight), nrow(fn), nrow(d),
          sum(d$covered == "yes"), sum(d$covered == "partial"),
          sum(d$covered == "no"))
}
cat(sprintf("cards %s, cardx %s\n", utils::packageVersion("cards"),
            utils::packageVersion("cardx")))
cat("all:               ", pct(cov), "\n")
for (g in unique(cov$group)) cat(sprintf("%-19s %s\n", g, pct(cov[cov$group == g, ])))
cat("weight-3 functions:", pct(cov[cov$weight == 3, ]), "\n")

# Since 2026-11-02: the ard_*() functions a method keyword names (the name a
# GUI offers and a person reads: "Kruskal-Wallis test", not
# cardx::ard_stats_kruskal_test), by weight.  A keyword writes the same code
# as its function: this measures how a SPEC is written, not what it can say.
kw_calls <- tfl_ard_methods()$call
fa <- unique(cov[cov$group %in% c("cards ard_*()", "cardx ard_*()") &
                   !startsWith(cov$note, "[by design]"),
                 c("package", "function", "weight")])
fa$keyword <- paste0(fa$package, "::", fa$`function`) %in% kw_calls
for (w in 3:1) {
  d <- fa[fa$weight == w, ]
  cat(sprintf("weight-%d ard_*() with a keyword: %d of %d%s\n", w, sum(d$keyword),
              nrow(d), if (any(!d$keyword)) paste0(" (not: ", paste(
                d$`function`[!d$keyword], collapse = ", "), ")") else ""))
}
