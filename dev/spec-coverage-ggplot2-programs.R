# The clinical figures as whole programs, the way a statistical programmer
# writes them with the tidyverse: the data piped into ggplot() (`%>%` and
# `|>`), a summary made first, layers kept in a list, a layer added on a
# condition, theme_set() / theme_update(), `ggplot2::` before every call,
# a layer's data a part of the plot's.  Each is read by
# tfl_fig_content_from_r(), written back (the figure's code from its
# content) and drawn, against the program drawn as it is:
#
#   Rscript dev/spec-coverage-ggplot2-program-measure.R [out.csv]
#
# Written for this measure (not copied from anywhere); the data are made
# up here.  The lines before the first `#### program:` make the data (the
# ADaM-like datasets a program reads); each program after its marker is
# what a programmer writes from there, ending with the figure printed or
# saved.

# (magrittr's pipe is what these programs are written in: the linter's
# preference for |> is not theirs)
# nolint start: pipe_consistency_linter, pipe_continuation_linter.
set.seed(20261115)
arm <- c("Placebo", "Drug 10 mg", "Drug 20 mg")
adsl <- data.frame(USUBJID = sprintf("S-%03d", 1:90), TRT01A = rep(arm, 30),
                   AGE = round(stats::rnorm(90, 58, 9)), SEX = rep(c("F", "M"), 45),
                   WEIGHTBL = round(stats::rnorm(90, 75, 12), 1))
adlb <- merge(adsl[c("USUBJID", "TRT01A")],
              expand.grid(USUBJID = adsl$USUBJID, PARAMCD = c("ALT", "AST"),
                          AVISITN = c(0, 2, 4, 8, 12), stringsAsFactors = FALSE))
adlb$TRTA <- factor(adlb$TRT01A, levels = arm)
adlb$AVISIT <- ifelse(adlb$AVISITN == 0, "Baseline", paste("Week", adlb$AVISITN))
adlb$AVAL <- round(stats::rlnorm(nrow(adlb), log(30), 0.35), 1)
adlb$BASE <- ave(adlb$AVAL, adlb$USUBJID, adlb$PARAMCD, FUN = function(v) v[1])
adlb$CHG <- adlb$AVAL - adlb$BASE
adlb$ANRHI <- 40
adtr <- data.frame(USUBJID = adsl$USUBJID, TRTA = adsl$TRT01A,
                   PCHG = round(stats::runif(90, -100, 60), 1))
adtr$BOR <- factor(cut(adtr$PCHG, c(-Inf, -99, -30, 20, Inf), labels = c("CR", "PR", "SD", "PD")),
                   levels = c("CR", "PR", "SD", "PD"))
km <- do.call(rbind, lapply(seq_along(arm), function(i) {
  t <- sort(round(stats::rexp(30, 0.08 / i), 1))
  s <- cumprod(1 - stats::rbinom(30, 1, 0.7) / (30:1))
  data.frame(TRTA = arm[i], time = c(0, t), surv = c(1, s),
             censored = c(FALSE, stats::runif(30) < 0.25))
}))
adae <- data.frame(USUBJID = sample(adsl$USUBJID, 200, replace = TRUE),
                   AEDECOD = sample(c("Headache", "Nausea", "Fatigue", "Rash", "Diarrhoea"),
                                    200, replace = TRUE))
adae$TRTA <- adsl$TRT01A[match(adae$USUBJID, adsl$USUBJID)]
adex <- data.frame(USUBJID = adsl$USUBJID[1:15], TRTA = adsl$TRT01A[1:15],
                   TRTDURM = round(stats::runif(15, 2, 24), 1))
adrs <- data.frame(USUBJID = rep(adex$USUBJID, 2), AVALC = rep(c("PR", "PD"), each = 15),
                   ADY = c(adex$TRTDURM * 0.3, adex$TRTDURM * 0.9))
adtte <- data.frame(USUBJID = adsl$USUBJID, TRTA = factor(adsl$TRT01A, levels = arm),
                    PARAMCD = "OS", AVAL = round(stats::rexp(90, 0.05), 1),
                    CNSR = stats::rbinom(90, 1, 0.3))
fst <- data.frame(subgroup = c("All", "Age < 65", "Age >= 65", "Female", "Male"),
                  hr = c(0.74, 0.70, 0.80, 0.69, 0.79))
fst$lcl <- fst$hr * exp(-0.25)
fst$ucl <- fst$hr * exp(0.25)

#### program: lab_spaghetti
# individual profiles and the arm's mean, the data piped in (magrittr)
adlb %>%
  filter(PARAMCD == "ALT") %>%
  ggplot(aes(x = AVISITN, y = AVAL)) +
  geom_line(aes(group = USUBJID), colour = "grey70", alpha = 0.5) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, colour = "#1F78B4") +
  facet_wrap(~TRTA) +
  labs(x = "Week", y = "ALT (U/L)") +
  theme_bw()

#### program: mean_se_native
# mean +/- SE by arm over time, the native pipe, one dodge
adlb |>
  dplyr::filter(PARAMCD == "ALT", AVISITN > 0) |>
  ggplot(aes(AVISITN, CHG, colour = TRTA, group = TRTA)) +
  stat_summary(fun.data = mean_se, geom = "pointrange", position = position_dodge(width = 0.6)) +
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = c(2, 4, 8, 12)) +
  labs(x = "Week", y = "Mean (SE) change from baseline", colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

#### program: waterfall_pipe
# a waterfall plot: the order made in the pipe, the plot saved
p <- adtr %>%
  arrange(desc(PCHG)) %>%
  mutate(ord = row_number()) %>%
  ggplot(aes(x = ord, y = PCHG, fill = BOR)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = c(-30, 20), linetype = "dashed") +
  scale_fill_manual(values = c(CR = "#1A9850", PR = "#91CF60", SD = "#FEE08B", PD = "#D73027"),
                    drop = FALSE) +
  labs(x = NULL, y = "Best change from baseline (%)") +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave("f_waterfall.png", p, width = 8, height = 5, dpi = 300)

#### program: summary_prefixed
# a summary made first, every call by ggplot2::
plot_sum <- adlb %>%
  filter(PARAMCD == "AST") %>%
  group_by(TRTA, AVISITN) %>%
  summarise(m = mean(AVAL), se = sd(AVAL) / sqrt(n()), .groups = "drop")
ggplot2::ggplot(plot_sum, ggplot2::aes(x = AVISITN, y = m, colour = TRTA)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = m - se, ymax = m + se), width = 0.3) +
  ggplot2::labs(x = "Week", y = "Mean (SE) AST (U/L)", colour = NULL) +
  ggplot2::theme_minimal()

#### program: layers_list
# reference lines and the study's look kept in lists, + NULL to end
ref_lines <- list(
  geom_hline(yintercept = 40, linetype = "dashed", colour = "red"),
  geom_vline(xintercept = 40, linetype = "dashed", colour = "red"))
study_look <- list(theme_bw(base_size = 9), theme(legend.position = "bottom"))
adlb %>%
  filter(AVISITN == 12) %>%
  select(USUBJID, TRTA, PARAMCD, AVAL) %>%
  tidyr::pivot_wider(names_from = PARAMCD, values_from = AVAL) %>%
  ggplot(aes(ALT, AST, colour = TRTA)) +
  geom_point() +
  ref_lines +
  scale_x_log10() +
  scale_y_log10() +
  list(labs(x = "ALT (U/L)", y = "AST (U/L)", colour = NULL)) +
  study_look +
  NULL

#### program: layer_if
# a layer added on a condition the program sets
show_normal <- TRUE
show_n <- FALSE
ggplot(adlb[adlb$PARAMCD == "ALT", ], aes(x = AVISIT, y = AVAL, fill = TRTA)) +
  geom_boxplot(outlier.shape = 1, position = position_dodge(width = 0.8)) +
  {if (show_normal) geom_hline(yintercept = 40, linetype = "dotted")} +
  if (show_n) geom_text(aes(label = after_stat(count)), stat = "count", y = 0) +
  labs(x = NULL, y = "ALT (U/L)", fill = NULL) +
  theme_bw()

#### program: theme_global
# the study's theme set once for the session, then updated
theme_set(theme_bw(base_size = 10))
theme_update(legend.position = "top", panel.grid.minor = element_blank())
p_cdf <- adlb %>%
  filter(PARAMCD == "ALT", AVISITN == 12) %>%
  ggplot(aes(x = CHG, colour = TRTA)) +
  stat_ecdf(geom = "step", pad = FALSE) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Change from baseline in ALT (U/L)", y = "Cumulative percent", colour = NULL)
print(p_cdf)

#### program: km_censor_subset
# KM curves, the censored times a part of the plot's data three ways
ggplot(km, aes(time, surv, colour = TRTA)) +
  geom_step() +
  geom_point(data = km[km$censored, ], shape = 3) +
  geom_point(data = subset(km, censored), shape = 3) +
  geom_point(data = km %>% filter(censored), shape = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Months", y = "Survival probability", colour = NULL) +
  theme_classic()

#### program: ae_heat_count
# AE counts by arm counted first, then a heat map with the numbers
ae_n <- adae %>% count(TRTA, AEDECOD)
ggplot(ae_n, aes(x = TRTA, y = reorder(AEDECOD, n), fill = n)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = n), size = 3) +
  scale_fill_gradient(low = "white", high = "#B2182B") +
  labs(x = NULL, y = NULL, fill = "Events") +
  theme_minimal()

#### program: swimmer_layers
# a swimmer plot of two datasets, no data of the plot's own
ggplot() +
  geom_col(data = adex, aes(x = TRTDURM, y = reorder(USUBJID, TRTDURM), fill = TRTA), width = 0.6) +
  geom_point(data = adrs, aes(x = ADY, y = USUBJID, shape = AVALC), size = 2) +
  scale_shape_manual(values = c(PR = 17, PD = 4)) +
  labs(x = "Months on treatment", y = NULL, fill = NULL, shape = "Response") +
  theme_bw()

#### program: age_hist_mutate
# a histogram of a variable derived in the pipe
adsl %>%
  mutate(AGEGR = if_else(AGE >= 65, ">= 65", "< 65")) %>%
  ggplot(aes(x = AGE, fill = AGEGR)) +
  geom_histogram(binwidth = 5, boundary = 0, colour = "white") +
  facet_grid(rows = vars(TRT01A)) +
  labs(x = "Age (years)", y = "Patients", fill = NULL) +
  theme_bw()

#### program: box_layer_pipe
# a box plot with the points above the range from a pipe of the plot's data
alt <- adlb %>% filter(PARAMCD == "ALT")
ggplot(alt, aes(x = TRTA, y = AVAL)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(data = alt %>% filter(AVAL > ANRHI), colour = "red",
             position = position_jitter(width = 0.15, seed = 1)) +
  labs(x = NULL, y = "ALT (U/L)") +
  theme_bw()

#### program: km_ggsurvfit_pipe
# a KM plot by ggsurvfit, the fit piped into it
adtte %>%
  filter(PARAMCD == "OS") %>%
  ggsurvfit::survfit2(survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = .) %>%
  ggsurvfit(linewidth = 0.8) +
  add_confidence_interval() +
  add_censor_mark(shape = 3) +
  scale_ggsurvfit(x_scales = list(breaks = seq(0, 60, by = 12))) +
  labs(x = "Months", y = "Overall survival")

#### program: forest_data_arg
# a forest plot, the data computed in ggplot()'s own argument
ggplot(data = fst %>% mutate(subgroup = factor(subgroup, levels = rev(subgroup))),
       aes(x = hr, y = subgroup)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_pointrange(aes(xmin = lcl, xmax = ucl), shape = 15) +
  scale_x_log10(breaks = c(0.5, 0.75, 1, 1.5)) +
  labs(x = "Hazard ratio (95% CI)", y = NULL) +
  theme_classic()

#### program: dot_placeholder
# the data piped into ggplot()'s data by a dot, a layer's data a function
# of the plot's by magrittr's dot
adlb %>%
  filter(PARAMCD == "ALT", AVISITN == 12) %>%
  ggplot(data = ., aes(x = BASE, y = AVAL)) +
  geom_point(colour = "grey50") +
  geom_point(data = . %>% filter(AVAL > ANRHI), colour = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_equal() +
  labs(x = "Baseline ALT (U/L)", y = "Week 12 ALT (U/L)") +
  theme_bw()

#### program: patchwork_pipes
# two plots of pipes, stacked by patchwork
p_top <- adlb %>%
  filter(PARAMCD == "ALT") %>%
  ggplot(aes(AVISITN, AVAL, colour = TRTA)) +
  stat_summary(fun = mean, geom = "line") +
  labs(x = NULL, y = "Mean ALT (U/L)", colour = NULL)
p_bottom <- adlb %>%
  filter(PARAMCD == "ALT") %>%
  count(TRTA, AVISITN) %>%
  ggplot(aes(AVISITN, TRTA, label = n)) +
  geom_text(size = 3) +
  labs(x = "Week", y = NULL) +
  theme_minimal()
p_top / p_bottom + plot_layout(heights = c(3, 1))

#### program: step_if
# a plot built in steps, a step added on the program's flag
show_ci <- TRUE
log_axis <- FALSE
sm <- adlb %>%
  filter(PARAMCD == "AST") %>%
  group_by(TRTA, AVISITN) %>%
  summarise(m = mean(AVAL), lo = m - sd(AVAL), hi = m + sd(AVAL), .groups = "drop")
p <- ggplot(sm, aes(AVISITN, m, colour = TRTA, fill = TRTA)) +
  geom_line()
if (show_ci) {
  p <- p + geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA)
}
if (log_axis) p <- p + scale_y_log10()
p <- p + labs(x = "Week", y = "AST (U/L), mean +/- SD", colour = NULL, fill = NULL)
p

#### program: bar_reorder_flip
# a bar chart of AE counts, ordered in the pipe, bars across
adae %>%
  count(AEDECOD, TRTA) %>%
  mutate(AEDECOD = forcats::fct_reorder(AEDECOD, n, .fun = sum)) %>%
  ggplot(aes(x = n, y = AEDECOD, fill = TRTA)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Events", y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

#### program: spaghetti_join
# patients' profiles, the arm joined in the pipe, the end of each line
# labelled from a layer's own summary
adlb %>%
  filter(PARAMCD == "ALT") %>%
  select(-TRTA, -TRT01A) %>%
  left_join(adsl %>% select(USUBJID, TRT01A), by = "USUBJID") %>%
  ggplot(aes(AVISITN, CHG, group = USUBJID)) +
  geom_line(alpha = 0.3) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(TRT01A), ncol = 3, labeller = label_wrap_gen(width = 12)) +
  scale_x_continuous(breaks = c(0, 2, 4, 8, 12)) +
  labs(x = "Week", y = "Change from baseline in ALT (U/L)")

#### program: plot_fn_params
# one plot function for every parameter, two of its plots stacked
lab_mean <- function(data, param, ylab = paste(param, "(U/L)")) {
  d <- data %>%
    filter(PARAMCD == param) %>%
    group_by(TRTA, AVISITN) %>%
    summarise(m = mean(AVAL), se = sd(AVAL) / sqrt(n()), .groups = "drop")
  ggplot(d, aes(AVISITN, m, colour = TRTA)) +
    geom_line() +
    geom_errorbar(aes(ymin = m - se, ymax = m + se), width = 0.3) +
    labs(x = "Week", y = ylab, colour = NULL) +
    theme_bw()
}
lab_mean(adlb, "ALT") / lab_mean(adlb, "AST", ylab = "AST (U/L), mean +/- SE") +
  plot_layout(guides = "collect")

#### program: plot_fn_columns
# a plot function given its columns by name, .data[[ ]] and !!sym()
profile_plot <- function(d, yvar, colvar, title) {
  ggplot(d, aes(AVISITN, .data[[yvar]], group = USUBJID)) +
    geom_line(aes(colour = !!sym(colvar)), alpha = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ggtitle(title) +
    theme_minimal()
}
p <- profile_plot(adlb %>% filter(PARAMCD == "ALT"), "CHG", "TRTA",
                  title = "Change from baseline in ALT")
print(p)

#### program: plot_fn_embrace
# a plot function given a column bare ({{ }}) and a layer's settings (...)
box_by_arm <- function(data, y, ...) {
  data %>%
    ggplot(aes(TRTA, {{ y }}, fill = TRTA)) +
    geom_boxplot(...) +
    labs(x = NULL) +
    theme_classic() +
    theme(legend.position = "none")
}
box_by_arm(adtr, PCHG, width = 0.5, outlier.shape = 1)

#### program: same_plot_other_data
# the plot made once, drawn again on other data (%+%), finished on the last plot
p_alt <- adlb %>%
  filter(PARAMCD == "ALT") %>%
  ggplot(aes(AVISITN, AVAL, group = USUBJID)) +
  geom_line(alpha = 0.3) +
  labs(x = "Week", y = "ALT (U/L)")
ast <- adlb %>% filter(PARAMCD == "AST")
p_alt %+% ast + labs(y = "AST (U/L)")
last_plot() + theme_bw()
# nolint end
