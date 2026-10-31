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
# and cards' helpers an analysis row writes in its `post` column
# (inst/ard/utilities.csv, use "post").
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
        "cardx::ard_stats_cohens_d", "cardx::ard_stats_paired_cohens_d",
        "cardx::ard_stats_mantelhaen_test", "cards::diff_ard_hierarchical",
        "cards::update_ard_stat_label", "cards::replace_null_statistic",
        "cards::update_ard_fmt_fun")
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
