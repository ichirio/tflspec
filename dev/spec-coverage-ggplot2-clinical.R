# The clinical figures, written by a SPEC: the ggplot2 code a statistical
# programmer writes for the figures the measure weights (KM, forest,
# waterfall, spider, swimmer, box / violin, mean +/- SE over time, bar,
# scatter, heat map, CDF, eDISH, lab by visit), read, checked, written
# back and drawn as dev/spec-coverage-ggplot2-examples.R does with
# ggplot2's own examples:
#
#   Rscript dev/spec-coverage-ggplot2-examples.R out.csv dev/spec-coverage-ggplot2-clinical.R
#
# Written for this measure (not copied from anywhere); the data are made
# up here.  Each figure is one assignment `fig_<name> <- ggplot(...) + ...`;
# the measure names its rows by the figure.

set.seed(20261107)
arm <- c("Placebo", "Drug 10 mg", "Drug 20 mg")

# KM estimates as a data frame (a survfit's times, survival, CI, censored)
km <- do.call(rbind, lapply(seq_along(arm), function(i) {
  t <- sort(round(stats::rexp(40, 0.08 / i), 1))
  s <- cumprod(1 - stats::rbinom(40, 1, 0.7) / (40:1))
  data.frame(TRTA = arm[i], time = c(0, t), surv = c(1, s),
             lower = pmax(0, c(1, s) - 0.08), upper = pmin(1, c(1, s) + 0.08),
             censored = c(FALSE, stats::runif(40) < 0.2))
}))
km$TRTA <- factor(km$TRTA, levels = arm)
fig_km <- ggplot(km, aes(x = time, y = surv, colour = TRTA, fill = TRTA)) +
  geom_step(linewidth = 0.7) +
  geom_ribbon(aes(ymin = lower, ymax = upper), stat = "identity", alpha = 0.15,
              colour = NA, show.legend = FALSE, outline.type = "both") +
  geom_point(data = km[km$censored, ], shape = 3, size = 1.5, show.legend = FALSE) +
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "grey50") +
  annotate("text", x = 1, y = 0.1, label = "HR 0.72 (95% CI 0.55, 0.94)\nLog-rank p = 0.016",
           hjust = 0, vjust = 0, size = 3) +
  scale_x_continuous(breaks = seq(0, 48, by = 6), limits = c(0, 48),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +
  scale_colour_manual(values = c(Placebo = "grey40", "Drug 10 mg" = "#1F78B4",
                                 "Drug 20 mg" = "#E31A1C"), name = NULL) +
  scale_fill_manual(values = c(Placebo = "grey40", "Drug 10 mg" = "#1F78B4",
                               "Drug 20 mg" = "#E31A1C"), guide = "none") +
  coord_cartesian(xlim = c(0, 48), clip = "off") +
  labs(x = "Time from randomisation (months)", y = "Overall survival (%)",
       title = "Kaplan-Meier plot of overall survival", caption = "+ Censored") +
  theme_bw(base_size = 10) +
  theme(legend.position = "inside", legend.position.inside = c(0.98, 0.98),
        legend.justification = c(1, 1), legend.background = element_rect(fill = "white", colour = "grey70"),
        legend.key.width = unit(1.2, "cm"), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 8), plot.title.position = "plot")

# a forest plot of a hazard ratio by subgroup
fo <- data.frame(
  group = rep(c("Age", "Sex", "Region"), c(2, 2, 3)),
  level = c("< 65", ">= 65", "Male", "Female", "Europe", "Americas", "Asia"),
  n = c(210, 190, 220, 180, 150, 140, 110),
  hr = c(0.70, 0.78, 0.69, 0.81, 0.75, 0.66, 0.90))
fo$lcl <- fo$hr * exp(-0.3)
fo$ucl <- fo$hr * exp(0.3)
fo$label <- sprintf("%.2f (%.2f, %.2f)", fo$hr, fo$lcl, fo$ucl)
fig_forest <- ggplot(fo, aes(x = hr, y = level)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_errorbar(aes(xmin = lcl, xmax = ucl), width = 0.25, orientation = "y") +
  geom_point(aes(size = n), shape = 15, colour = "#08519C") +
  geom_text(aes(x = 3.5, label = label), hjust = 0, size = 3) +
  scale_x_log10(breaks = c(0.25, 0.5, 1, 2), labels = c("0.25", "0.5", "1", "2"),
                limits = c(0.25, 6)) +
  scale_y_discrete(limits = rev) +
  scale_size_area(max_size = 4, guide = "none") +
  facet_grid(rows = vars(group), scales = "free_y", space = "free_y", switch = "y") +
  guides(x = guide_axis_logticks(long = 2, mid = 1, short = 0.5)) +
  labs(x = "Hazard ratio (95% CI)", y = NULL) +
  theme_classic(base_size = 10) +
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text.y.left = element_text(angle = 0, hjust = 0, face = "bold"),
        panel.spacing.y = unit(0.2, "lines"), axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

# a waterfall plot of the best percent change in the sum of diameters
wf <- data.frame(USUBJID = sprintf("S-%03d", 1:40), PCHG = sort(stats::runif(40, -100, 60), decreasing = TRUE))
wf$BOR <- factor(ifelse(wf$PCHG <= -30, ifelse(wf$PCHG <= -95, "CR", "PR"), ifelse(wf$PCHG >= 20, "PD", "SD")),
                 levels = c("CR", "PR", "SD", "PD"))
fig_waterfall <- ggplot(wf, aes(x = reorder(USUBJID, -PCHG), y = PCHG, fill = BOR)) +
  geom_col(width = 0.8, colour = "grey20", linewidth = 0.2) +
  geom_hline(yintercept = c(-30, 20), linetype = "dashed", colour = "grey30") +
  scale_fill_manual(values = c(CR = "#2166AC", PR = "#67A9CF", SD = "#BDBDBD", PD = "#EF8A62"),
                    breaks = c("CR", "PR", "SD", "PD"), drop = FALSE, name = "Best overall response") +
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by = 20),
                     labels = scales::label_number(suffix = "%")) +
  labs(x = "Subject", y = "Best % change from baseline in target lesions") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(), legend.position = "bottom",
        legend.direction = "horizontal")

# a spider plot of the percent change over time by subject
sp <- do.call(rbind, lapply(1:12, function(i) {
  w <- seq(0, 48, by = 6)[seq_len(sample(4:9, 1))]
  data.frame(USUBJID = sprintf("S-%03d", i), TRTA = arm[1 + i %% 3], ADY = w,
             PCHG = c(0, cumsum(stats::rnorm(length(w) - 1, -5, 12))))
}))
fig_spider <- ggplot(sp, aes(x = ADY, y = PCHG, group = USUBJID, colour = TRTA)) +
  geom_line(alpha = 0.8) +
  geom_point(size = 1.2) +
  geom_hline(yintercept = c(-30, 0, 20), linetype = c("dashed", "solid", "dashed"), colour = "grey40") +
  scale_colour_brewer(palette = "Dark2", name = "Treatment") +
  scale_x_continuous(breaks = seq(0, 48, by = 6), name = "Week") +
  scale_y_continuous(name = "% change from baseline", n.breaks = 8) +
  theme_light() +
  theme(legend.position = "top")

# a swimmer plot of the time on treatment, with events
sw <- data.frame(USUBJID = sprintf("S-%03d", 1:15), dur = round(stats::runif(15, 3, 30), 1),
                 TRTA = rep(arm, 5), ongoing = stats::runif(15) < 0.4)
sw_ev <- data.frame(USUBJID = rep(sw$USUBJID, 2), time = c(sw$dur * 0.3, sw$dur * 0.8),
                    event = rep(c("Response", "Progression"), each = 15))
fig_swimmer <- ggplot(sw, aes(y = reorder(USUBJID, dur))) +
  geom_col(aes(x = dur, fill = TRTA), width = 0.6, orientation = "y") +
  geom_point(data = sw_ev, aes(x = time, y = USUBJID, shape = event), size = 2.5) +
  geom_segment(data = sw[sw$ongoing, ], aes(x = dur, xend = dur + 2, yend = USUBJID),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  scale_shape_manual(values = c(Response = 17, Progression = 4), name = "Event") +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8, name = "Treatment") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 36, by = 3)) +
  labs(x = "Months on treatment", y = "Subject") +
  theme_bw() +
  theme(legend.position = "right", legend.box = "vertical", panel.grid.major.y = element_blank())

# a box plot and a violin of a lab value by treatment and visit
lb <- expand.grid(USUBJID = 1:30, TRTA = arm, AVISIT = c("Baseline", "Week 4", "Week 8"))
lb$AVAL <- stats::rnorm(nrow(lb), 5 + as.integer(lb$AVISIT) * 0.3, 1)
fig_box <- ggplot(lb, aes(x = AVISIT, y = AVAL, fill = TRTA)) +
  geom_violin(trim = FALSE, alpha = 0.3, scale = "width", position = position_dodge(width = 0.9)) +
  geom_boxplot(width = 0.15, outlier.shape = NA, position = position_dodge(width = 0.9),
               staplewidth = 0.5) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9, seed = 1),
             size = 0.6, alpha = 0.4) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5, fill = "white",
               position = position_dodge(width = 0.9)) +
  scale_fill_grey(start = 0.4, end = 0.9, name = "Treatment") +
  labs(x = NULL, y = "ALT (U/L)") +
  theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# the mean +/- SE over time by treatment
mt <- expand.grid(USUBJID = 1:25, TRTA = arm, AVISITN = c(0, 2, 4, 8, 12))
mt$CHG <- stats::rnorm(nrow(mt), -0.4 * mt$AVISITN * as.integer(mt$TRTA) / 3, 2)
fig_mean <- ggplot(mt, aes(x = AVISITN, y = CHG, colour = TRTA, shape = TRTA, linetype = TRTA)) +
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.6)) +
  stat_summary(fun = mean, geom = "point", size = 2, position = position_dodge(width = 0.6)) +
  stat_summary(fun.data = mean_se, fun.args = list(mult = 1), geom = "errorbar", width = 0.4,
               linetype = "solid", position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  scale_x_continuous(breaks = c(0, 2, 4, 8, 12), labels = c("BL", "W2", "W4", "W8", "W12"),
                     minor_breaks = NULL) +
  scale_colour_manual(values = c("black", "#1F78B4", "#E31A1C")) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_linetype_manual(values = c("dashed", "solid", "solid")) +
  labs(x = "Visit", y = "Mean (+/- SE) change from baseline", colour = "Treatment",
       shape = "Treatment", linetype = "Treatment") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm"), text = element_text(size = 9))

# the same, the geom with the stat's form (what the GUI writes), CI by the normal
fig_mean_geom <- ggplot(mt, aes(x = AVISITN, y = CHG, colour = TRTA)) +
  geom_line(stat = "summary", fun = mean) +
  geom_pointrange(stat = "summary", fun.data = mean_se, fun.args = list(mult = 1.96),
                  position = position_dodge(width = 0.5), fatten = 2) +
  scale_x_continuous(breaks = c(0, 2, 4, 8, 12)) +
  scale_colour_discrete(type = c("black", "#1F78B4", "#E31A1C"), name = "Treatment") +
  ylab("Mean change (95% CI)") +
  xlab("Week")

# a bar chart of the AE counts by SOC and severity, percentages on top
ae <- data.frame(AESOC = rep(c("Gastrointestinal", "Nervous system", "Skin", "Infections"), each = 6),
                 TRTA = rep(arm[2:3], each = 3, times = 4),
                 AESEV = rep(c("MILD", "MODERATE", "SEVERE"), 8), n = stats::rpois(24, 12))
fig_bar <- ggplot(ae, aes(x = AESOC, y = n, fill = AESEV)) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0.1), width = 0.8) +
  geom_text(aes(label = n), position = position_dodge2(width = 0.8, preserve = "single"),
            vjust = -0.4, size = 2.5) +
  facet_wrap(~TRTA, ncol = 1, strip.position = "top") +
  scale_fill_brewer(palette = "Reds", direction = 1, name = "Severity",
                    labels = c(MILD = "Mild", MODERATE = "Moderate", SEVERE = "Severe")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  labs(x = "System organ class", y = "Number of subjects") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "grey90"), strip.text = element_text(face = "bold"),
        legend.position = "bottom")

# a stacked bar of the proportions, flipped
fig_stack <- ggplot(ae, aes(y = AESOC, x = n, fill = AESEV)) +
  geom_col(position = position_fill(reverse = TRUE)) +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = c(MILD = "#FEE0D2", MODERATE = "#FC9272", SEVERE = "#DE2D26")) +
  guides(fill = guide_legend(title = "Severity", nrow = 1, reverse = TRUE, title.position = "top")) +
  labs(x = "Percent of events", y = NULL) +
  theme_minimal() +
  theme(legend.position = "top", legend.justification = "left")

# a scatter of the baseline against the week 12 value, by parameter
sc <- data.frame(PARAMCD = rep(c("ALT", "AST"), each = 60), TRTA = rep(arm, 40),
                 BASE = stats::rlnorm(120, 3, 0.4))
sc$AVAL <- sc$BASE * stats::rlnorm(120, 0, 0.2)
fig_scatter <- ggplot(sc, aes(x = BASE, y = AVAL, colour = TRTA)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.6, level = 0.95) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(vars(PARAMCD), scales = "free", ncol = 2, labeller = label_both) +
  scale_x_log10() +
  scale_y_log10() +
  scale_colour_viridis_d(end = 0.85) +
  labs(x = "Baseline (U/L)", y = "Week 12 (U/L)", colour = "Treatment") +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = "bottom")

# an eDISH plot: peak ALT against peak bilirubin, x ULN
ed <- data.frame(USUBJID = 1:150, TRTA = rep(arm, 50), ALT = stats::rlnorm(150, 0, 0.8),
                 BILI = stats::rlnorm(150, -0.5, 0.6))
fig_edish <- ggplot(ed, aes(x = ALT, y = BILI)) +
  annotate("rect", xmin = 3, xmax = Inf, ymin = 2, ymax = Inf, fill = "red", alpha = 0.1) +
  geom_point(aes(shape = TRTA, colour = TRTA), size = 1.8) +
  geom_vline(xintercept = 3, linetype = "dashed") +
  geom_hline(yintercept = 2, linetype = "dashed") +
  annotate("text", x = 20, y = 8, label = "Potential Hy's law", hjust = 1, fontface = "italic") +
  scale_x_log10(limits = c(0.1, 30), breaks = c(0.1, 1, 3, 10, 30), labels = scales::label_number()) +
  scale_y_log10(limits = c(0.1, 10), breaks = c(0.1, 1, 2, 10)) +
  scale_shape_manual(values = c(1, 2, 0)) +
  annotation_logticks(sides = "bl") +
  labs(x = "Peak ALT (x ULN)", y = "Peak total bilirubin (x ULN)", colour = "Treatment", shape = "Treatment") +
  theme_bw()

# a heat map of a correlation matrix
hm <- expand.grid(x = c("ALT", "AST", "BILI", "ALP"), y = c("ALT", "AST", "BILI", "ALP"))
hm$r <- round(stats::runif(nrow(hm), -1, 1), 2)
fig_heat <- ggplot(hm, aes(x = x, y = y, fill = r)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       limits = c(-1, 1), name = "Correlation") +
  scale_x_discrete(position = "top", guide = guide_axis(angle = 45)) +
  scale_y_discrete(limits = rev) +
  coord_equal() +
  guides(fill = guide_colourbar(title.position = "top", direction = "horizontal",
                                theme = theme(legend.key.width = unit(3, "cm")))) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid = element_blank())

# a CDF plot of the change from baseline in a PRO score
cd <- data.frame(TRTA = rep(arm, each = 80), CHG = stats::rnorm(240, rep(c(0, -3, -5), each = 80), 6))
fig_cdf <- ggplot(cd, aes(x = CHG, colour = TRTA)) +
  stat_ecdf(geom = "step", pad = FALSE, linewidth = 0.7) +
  geom_vline(xintercept = c(-10, 0), linetype = c("dotted", "solid"), colour = "grey50") +
  scale_y_continuous(labels = scales::label_percent(), breaks = seq(0, 1, by = 0.25)) +
  scale_x_continuous(limits = c(-25, 20), breaks = seq(-25, 20, by = 5), oob = scales::oob_keep) +
  scale_colour_manual(values = c("grey30", "#33A02C", "#6A3D9A"), name = NULL) +
  labs(x = "Change from baseline in score", y = "Cumulative percent of subjects") +
  theme_bw() +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.8))

# the same, the geom with the stat's form
fig_cdf_geom <- ggplot(cd, aes(x = CHG, colour = TRTA)) +
  geom_step(stat = "ecdf", pad = TRUE, direction = "hv") +
  coord_cartesian(xlim = c(-25, 20), expand = FALSE) +
  ylab("Cumulative proportion")

# a lab parameter by visit, faceted by parameter, a density of the baseline
lv <- expand.grid(USUBJID = 1:20, TRTA = arm[1:2], AVISITN = c(0, 4, 8), PARAMCD = c("ALT", "AST"))
lv$AVAL <- stats::rlnorm(nrow(lv), 3, 0.3)
fig_labvisit <- ggplot(lv, aes(x = factor(AVISITN), y = AVAL, colour = TRTA)) +
  geom_boxplot(position = position_dodge(preserve = "single"), outlier.size = 0.8, varwidth = FALSE,
               notch = FALSE) +
  facet_grid(PARAMCD ~ ., scales = "free_y", labeller = labeller(PARAMCD = label_wrap_gen(10))) +
  scale_x_discrete(labels = c("0" = "Baseline", "4" = "Week 4", "8" = "Week 8")) +
  scale_colour_hue(l = 40, name = "Treatment") +
  labs(x = "Visit", y = "Value", tag = "A") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0), legend.position = "bottom")

fig_density <- ggplot(lv[lv$AVISITN == 0, ], aes(x = AVAL, fill = TRTA)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, alpha = 0.4, position = "identity") +
  geom_density(alpha = 0.2, adjust = 1.2, bounds = c(0, Inf)) +
  geom_rug(sides = "b", alpha = 0.3) +
  stat_function(fun = stats::dlnorm, args = list(meanlog = 3, sdlog = 0.3), colour = "black",
                inherit.aes = FALSE) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02"), name = NULL) +
  scale_x_continuous(transform = "log10") +
  xlab("Baseline value") +
  theme_classic()

# a mean with a ribbon of the 95% CI, the stat as stat_summary(geom = "ribbon")
fig_ribbon <- ggplot(mt, aes(x = AVISITN, y = CHG, colour = TRTA, fill = TRTA)) +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", alpha = 0.2, colour = NA) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary_bin(fun = median, geom = "point", bins = 5) +
  stat_ellipse(aes(group = TRTA), type = "norm", level = 0.95, linetype = "dotted") +
  scale_fill_manual(values = c("grey40", "#1F78B4", "#E31A1C"), aesthetics = c("colour", "fill")) +
  ylab("Mean change (95% CI)")

# the mean +/- SD and the median with its quartiles, the summaries written
# out as functions (the GUI's geom_*(stat = "summary") form for one)
fig_mean_sd <- ggplot(mt, aes(x = AVISITN, y = CHG, colour = TRTA)) +
  stat_summary(fun = mean, fun.min = function(y) mean(y) - sd(y),
               fun.max = function(y) mean(y) + sd(y), geom = "pointrange",
               position = position_dodge(width = 0.6)) +
  geom_errorbar(stat = "summary",
                fun.data = \(y) data.frame(y = median(y), ymin = quantile(y, 0.25), ymax = quantile(y, 0.75)),
                width = 0.3, linetype = "dotted", position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = c(0, 2, 4, 8, 12), labels = ~ ifelse(.x == 0, "Baseline", paste("Week", .x))) +
  scale_y_continuous(labels = function(x) sprintf("%+.1f", x)) +
  labs(x = NULL, y = "Mean (+/- SD) change from baseline", colour = NULL)

# a forest plot on a log2 axis: the breaks as powers of 2, labels by a function
fig_forest_log2 <- ggplot(fo, aes(x = hr, y = level, xmin = lcl, xmax = ucl)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_pointrange(shape = 15) +
  scale_x_continuous(transform = "log2", breaks = 2^(-2:2), limits = c(0.25, 4),
                     labels = function(x) format(x, drop0trailing = TRUE)) +
  scale_y_discrete(limits = rev) +
  labs(x = "Hazard ratio (log2 scale)", y = NULL)

# a lab value's distribution, the bin width by the Freedman-Diaconis rule
fig_hist_fd <- ggplot(lv, aes(x = AVAL)) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / length(x)^(1 / 3), fill = "grey70", colour = "white") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~ TRTA, ncol = 1) +
  xlab("Value")

# Round 11: what the figures above leave out and a statistical programmer
# writes every day -- calendar dates, a statistic's own labels
# (after_stat()), a strip's labels by a lookup, a part of the data by a
# function, the scales' helpers for breaks, a second axis, a legend's keys
# redrawn, colour steps of a heat map.

# subjects' time on study by calendar date (enrolment, data cut-off)
dt <- data.frame(USUBJID = sprintf("S-%03d", 1:12), TRTA = rep(arm, 4),
                 start = as.Date("2024-01-15") + sort(sample(0:240, 12)))
dt$end <- dt$start + sample(60:400, 12)
fig_dates <- ggplot(dt, aes(y = reorder(USUBJID, start), colour = TRTA)) +
  geom_linerange(aes(xmin = start, xmax = end), linewidth = 2.5) +
  geom_vline(xintercept = as.Date("2025-01-31"), linetype = "dashed") +
  annotate("text", x = as.Date("2025-01-31"), y = 12.5, label = "Data cut-off", hjust = -0.05, size = 3) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", date_minor_breaks = "1 month",
               limits = as.Date(c("2024-01-01", "2025-06-30")), expand = expansion(add = c(0, 10))) +
  scale_colour_brewer(palette = "Set1", name = NULL) +
  labs(x = NULL, y = "Subject") +
  theme_bw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# the subjects with an AE by grade: counts and percentages the stat computes
ag <- data.frame(TRTA = sample(arm, 200, replace = TRUE),
                 AETOXGR = factor(sample(1:4, 200, replace = TRUE, prob = c(4, 3, 2, 1))))
fig_ae_pct <- ggplot(ag, aes(x = TRTA, fill = AETOXGR)) +
  geom_bar(aes(y = after_stat(count / sum(count))), position = position_dodge(preserve = "single")) +
  geom_text(aes(label = after_stat(count), y = after_stat(count / sum(count))), stat = "count",
            position = position_dodge(width = 0.9), vjust = -0.3, size = 2.5) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), breaks = scales::breaks_width(0.05),
                     expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(values = c("1" = "#FEE5D9", "2" = "#FCAE91", "3" = "#FB6A4A", "4" = "#CB181D"),
                    labels = c("Grade 1", "Grade 2", "Grade 3", "Grade 4"), name = "Worst grade") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(colour = "grey30"))) +
  labs(x = NULL, y = "Subjects (%)") +
  theme_classic() +
  theme(legend.position = "bottom", plot.margin = margin(5.5, 20, 5.5, 5.5),
        axis.title.y = element_text(margin = margin(r = 8)))

# lab values by visit against the normal range, the strips by a lookup and a
# part of the data by a function
lr <- expand.grid(USUBJID = 1:15, AVISITN = c(0, 2, 4, 8, 12), PARAMCD = c("ALT", "CREAT"))
lr$AVAL <- stats::rlnorm(nrow(lr), ifelse(lr$PARAMCD == "ALT", 3.2, 4.4), 0.35)
lr$ANRHI <- ifelse(lr$PARAMCD == "ALT", 40, 110)
lr$HIGH <- lr$AVAL > lr$ANRHI
fig_lab_range <- ggplot(lr, aes(x = AVISITN, y = AVAL, group = USUBJID)) +
  geom_line(colour = "grey70", linewidth = 0.3) +
  geom_point(data = function(d) subset(d, HIGH), colour = "#E31A1C", size = 1.5) +
  geom_hline(aes(yintercept = ANRHI), linetype = "dashed", colour = "#E31A1C") +
  stat_summary(aes(group = 1), fun = median, geom = "line", linewidth = 1) +
  facet_wrap(~ PARAMCD, scales = "free_y", nrow = 1,
             labeller = as_labeller(c(ALT = "Alanine aminotransferase (U/L)", CREAT = "Creatinine (umol/L)"))) +
  scale_x_continuous(breaks = c(0, 2, 4, 8, 12), labels = c("BL", paste0("W", c(2, 4, 8, 12)))) +
  scale_y_continuous(limits = c(0, NA), breaks = scales::breaks_pretty(n = 6),
                     sec.axis = dup_axis(name = NULL, labels = NULL)) +
  labs(x = "Visit", y = NULL) +
  theme_bw() +
  theme(strip.text = element_text(hjust = 0), panel.border = element_rect(colour = "black", fill = NA))

# the mean change by visit and treatment, the groups an interaction, the
# CI by bootstrap, the legend's keys redrawn
fig_mean_boot <- ggplot(mt, aes(x = factor(AVISITN), y = CHG, colour = TRTA,
                                group = interaction(TRTA, AVISITN > 0))) +
  stat_summary(fun.data = mean_cl_boot, fun.args = list(conf.int = 0.95, B = 200), geom = "errorbar",
               width = 0.2, position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "point", size = 2, position = position_dodge(width = 0.5)) +
  stat_summary(fun = mean, geom = "text", aes(label = after_stat(sprintf("%.1f", y))),
               vjust = -1.2, size = 2.5, position = position_dodge(width = 0.5), show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_discrete(labels = c("0" = "Baseline", "2" = "Week 2", "4" = "Week 4", "8" = "Week 8", "12" = "Week 12")) +
  scale_colour_manual(values = c("grey40", "#1F78B4", "#E31A1C"), name = NULL) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, size = 3))) +
  labs(x = NULL, y = "Mean change (95% bootstrap CI)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.key.size = unit(0.4, "cm"),
        legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-8, 0, 0, 0))

# a heat map of the change by subject and visit, the colour in steps
hs <- expand.grid(USUBJID = sprintf("S-%02d", 1:20), AVISITN = c(2, 4, 8, 12))
hs$PCHG <- round(stats::rnorm(nrow(hs), -10, 25))
fig_heat_steps <- ggplot(hs, aes(x = factor(AVISITN), y = USUBJID, fill = PCHG)) +
  geom_tile(colour = "grey90") +
  scale_fill_steps2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                    breaks = c(-50, -30, 0, 20, 50), limits = c(-100, 100), oob = scales::squish,
                    name = "% change") +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  guides(fill = guide_coloursteps(show.limits = TRUE, even.steps = FALSE)) +
  labs(x = "Week", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(), legend.key.height = unit(1.5, "cm"))

# a KM-like step with the numbers at risk under it, a second axis in weeks
fig_km_axis <- ggplot(km, aes(x = time, y = surv, colour = TRTA, linetype = TRTA)) +
  geom_step() +
  geom_point(data = ~ subset(.x, censored), shape = "|", size = 3, show.legend = FALSE) +
  scale_x_continuous(breaks = scales::breaks_width(6), sec.axis = sec_axis(~ . * 52 / 12, name = "Weeks",
                                                                           breaks = seq(0, 208, by = 26))) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +
  coord_cartesian(xlim = c(0, 48), ylim = c(0, NA)) +
  labs(x = "Months", y = "Event-free (%)", colour = NULL, linetype = NULL) +
  theme_classic() +
  theme(legend.position = "inside", legend.position.inside = c(1, 1), legend.justification = c("right", "top"))

# round 12 (2026-11-09): a log axis and scales' helpers with arguments

# PK: the mean (+/- SE) concentration over time on a log axis, the LLOQ
pk <- expand.grid(USUBJID = 1:12, TRTA = arm[2:3], NTPT = c(0.5, 1, 2, 4, 8, 12, 24))
pk$AVAL <- exp(stats::rnorm(nrow(pk), log(80 * as.integer(pk$TRTA) / 2) - 0.12 * pk$NTPT, 0.3))
fig_pk_log <- ggplot(pk, aes(x = NTPT, y = AVAL, colour = TRTA, shape = TRTA)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.6) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey40") +
  annotation_logticks(sides = "l") +
  scale_x_continuous(breaks = c(0, 2, 4, 8, 12, 24), labels = scales::label_number(accuracy = 1)) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "Nominal time after dose (h)", y = "Concentration (ng/mL)", colour = NULL, shape = NULL) +
  theme_bw()

# PK: each subject's profile, log axis labelled by its numbers, by arm
fig_pk_ind <- ggplot(pk, aes(x = NTPT, y = AVAL, group = USUBJID)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~TRTA, nrow = 1) +
  scale_x_continuous(breaks = scales::breaks_width(4, offset = 0)) +
  scale_y_log10(breaks = scales::breaks_log(n = 6, base = 10),
                labels = scales::label_number(accuracy = 0.1, big.mark = ","),
                sec.axis = dup_axis(name = derive(), labels = scales::label_log())) +
  labs(x = "Time (h)", y = "Concentration (ng/mL)") +
  theme_bw()

# the mean percent change with signed labels, a ratio axis by label_math
fig_pct_signed <- ggplot(mt, aes(x = AVISITN, y = CHG * 10, colour = TRTA)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "pointrange") +
  scale_x_continuous(breaks = scales::breaks_width(2),
                     labels = scales::label_number(prefix = "W", accuracy = 1)) +
  scale_y_continuous(labels = scales::label_number(style_positive = "plus", suffix = "%"),
                     breaks = scales::breaks_pretty(n = 6)) +
  scale_colour_brewer(palette = "Dark2", labels = scales::label_wrap(10)) +
  labs(x = "Week", y = "Mean % change (+/- SE)", colour = NULL) +
  theme_minimal()

# a forest plot of the odds ratio on a log2 axis labelled 2^x
fig_forest_math <- ggplot(fo, aes(x = hr, y = level, xmin = lcl, xmax = ucl)) +
  geom_pointrange() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(transform = scales::transform_log(base = 2),
                     breaks = scales::breaks_log(n = 5, base = 2),
                     labels = scales::label_log(base = 2)) +
  scale_y_discrete(labels = scales::label_wrap(12)) +
  labs(x = "Odds ratio (log scale)", y = NULL) +
  theme_classic()

# round 13: more everyday figures

# a butterfly (tornado) chart of the AE incidence, one arm to each side
bf <- data.frame(AEDECOD = rep(c("Nausea", "Headache", "Fatigue", "Rash", "Diarrhoea"), 2),
                 TRTA = rep(c("Placebo", "Drug 20 mg"), each = 5), pct = stats::runif(10, 2, 25))
fig_butterfly <- ggplot(bf, aes(x = reorder(AEDECOD, pct),
                                y = ifelse(TRTA == "Placebo", -pct, pct), fill = TRTA)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, colour = "grey20") +
  scale_y_continuous(labels = abs, breaks = seq(-30, 30, by = 10), limits = c(-30, 30)) +
  scale_fill_manual(values = c("grey60", "#B2182B"), breaks = c("Placebo", "Drug 20 mg")) +
  coord_flip() +
  labs(x = NULL, y = "Subjects with the event (%)", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top", panel.grid.minor = element_blank())

# a normal Q-Q plot of the change by treatment
fig_qq <- ggplot(mt, aes(sample = CHG, colour = TRTA)) +
  stat_qq(distribution = stats::qnorm, size = 1, alpha = 0.6) +
  stat_qq_line(distribution = stats::qnorm, line.p = c(0.25, 0.75), fullrange = FALSE) +
  facet_wrap(vars(TRTA), nrow = 1) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_bw() +
  theme(legend.position = "none", aspect.ratio = 1)

# a Bland-Altman plot: the difference of two methods against their mean
ba <- data.frame(m1 = stats::rnorm(60, 50, 10))
ba$m2 <- ba$m1 + stats::rnorm(60, 1, 3)
fig_bland <- ggplot(ba, aes(x = (m1 + m2) / 2, y = m2 - m1)) +
  geom_point(shape = 21, fill = "grey80") +
  geom_hline(aes(yintercept = mean(m2 - m1)), colour = "#2166AC") +
  geom_hline(aes(yintercept = mean(m2 - m1) + 1.96 * sd(m2 - m1)), linetype = "dashed") +
  geom_hline(aes(yintercept = mean(m2 - m1) - 1.96 * sd(m2 - m1)), linetype = "dashed") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "grey50", linewidth = 0.5) +
  annotate("label", x = Inf, y = Inf, label = "Mean +/- 1.96 SD", hjust = 1.1, vjust = 1.5,
           size = 3, label.size = 0) +
  labs(x = "Mean of the two methods", y = "Difference (method 2 - method 1)") +
  theme_classic()

# a 100% stacked bar of the response by treatment, counts inside
rs <- data.frame(TRTA = sample(arm, 150, replace = TRUE),
                 AVALC = sample(c("CR", "PR", "SD", "PD"), 150, replace = TRUE))
rs$AVALC <- factor(rs$AVALC, levels = c("CR", "PR", "SD", "PD"))
fig_fill <- ggplot(rs, aes(x = TRTA, fill = AVALC)) +
  geom_bar(position = "fill", width = 0.6) +
  geom_text(aes(label = after_stat(count)), stat = "count",
            position = position_fill(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_fill_grey(start = 0.9, end = 0.4, name = "Best response") +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  labs(x = NULL, y = "Subjects (%)") +
  theme_classic()

# the mean over time with week labels built by R and the means as text
fig_mean_text <- ggplot(mt, aes(x = AVISITN, y = CHG, colour = TRTA, group = TRTA)) +
  stat_summary(fun = mean, geom = "line") +
  geom_text(stat = "summary", fun = mean, aes(label = after_stat(round(y, 1))),
            vjust = -0.8, size = 2.5, show.legend = FALSE) +
  stat_summary_bin(fun = median, geom = "point", breaks = c(-1, 1, 3, 6, 10, 14), shape = 4) +
  scale_x_continuous(breaks = c(0, 2, 4, 8, 12), labels = c("BL", paste0("W", c(2, 4, 8, 12)))) +
  scale_colour_hue(l = 45, c = 80, h.start = 30) +
  labs(x = NULL, y = "Mean change", colour = "Treatment") +
  theme_bw() +
  theme(legend.position = "bottom")

# a histogram with the density and the normal curve, a margin facet
fig_hist_density <- ggplot(lv, aes(x = AVAL)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, boundary = 0, closed = "left",
                 fill = "grey85", colour = "grey40") +
  geom_density(colour = "#B2182B", adjust = 1.5, bounds = c(0, Inf)) +
  stat_function(fun = stats::dnorm, args = list(mean = 5, sd = 1), linetype = "dashed", n = 201) +
  facet_grid(rows = vars(TRTA), margins = TRUE) +
  labs(x = "Value", y = "Density") +
  theme_bw()

# the scatter with 95% ellipses and the unique points
fig_ellipse <- ggplot(sc, aes(x = BASE, y = AVAL, colour = TRTA)) +
  stat_unique(alpha = 0.5) +
  stat_ellipse(type = "norm", level = 0.95, geom = "polygon", alpha = 0.08,
               aes(fill = TRTA), segments = 101) +
  stat_ellipse(type = "t", level = 0.68, linetype = "dotted") +
  coord_fixed(ratio = 1, xlim = c(0, 60), ylim = c(0, 60), expand = FALSE) +
  theme_bw()

# a CDF with no padding, a responder threshold and an arrow to it
fig_cdf_pad <- ggplot(cd, aes(x = CHG, colour = TRTA, linetype = TRTA)) +
  stat_ecdf(geom = "step", pad = FALSE, linewidth = 0.6) +
  geom_vline(xintercept = -10, colour = "grey40") +
  annotate("segment", x = -20, xend = -10.5, y = 0.85, yend = 0.85,
           arrow = arrow(length = unit(2, "mm"), ends = "last", type = "open")) +
  annotate("text", x = -20, y = 0.85, label = "Responder threshold", hjust = 1.05, size = 3) +
  scale_y_continuous(labels = scales::label_percent(), sec.axis = dup_axis(name = NULL)) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +
  labs(x = "Change from baseline", y = "Cumulative percent of subjects") +
  theme_bw() +
  theme(legend.position = "inside", legend.position.inside = c(0.02, 0.98),
        legend.justification = c("left", "top"), legend.title = element_blank())

# dose proportionality of the exposure, titles in plotmath
dp <- data.frame(DOSE = rep(c(10, 30, 100, 300), each = 8))
dp$AUC <- dp$DOSE * 12 * stats::rlnorm(nrow(dp), 0, 0.3)
fig_dose_prop <- ggplot(dp, aes(x = DOSE, y = AUC)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black", linewidth = 0.5) +
  scale_x_log10(breaks = c(10, 30, 100, 300)) +
  scale_y_log10(name = expression(AUC[0 - infinity] ~ (ng %.% h / mL))) +
  labs(x = expression(paste("Dose (", mu, "g/kg)")),
       title = quote(log(AUC) == beta[0] + beta[1] %.% log(Dose))) +
  theme_bw()

# ---- round 14 (2026-11-11) ----

# a pie of the best overall response, one per treatment
rp <- data.frame(TRTA = rep(arm, each = 4), AVALC = rep(c("CR", "PR", "SD", "PD"), 3),
                 n = c(2, 8, 10, 10, 5, 12, 8, 5, 8, 14, 6, 2))
rp$AVALC <- factor(rp$AVALC, levels = c("CR", "PR", "SD", "PD"))
fig_pie <- ggplot(rp, aes(x = "", y = n, fill = AVALC)) +
  geom_col(width = 1, colour = "white") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), colour = "white", size = 3) +
  coord_polar(theta = "y", start = 0, direction = -1) +
  facet_wrap(vars(TRTA), nrow = 1) +
  scale_fill_viridis_d(option = "D", end = 0.85, direction = -1, name = "BOR") +
  theme_void() +
  theme(legend.position = "bottom", strip.text = element_text(size = 9, margin = margin(b = 4)))

# lab values over time, one panel per parameter, labels of the visits by paste0()
lp <- expand.grid(USUBJID = 1:15, PARAM = c("Alanine Aminotransferase (U/L)", "Creatinine (umol/L)"),
                  AVISITN = seq(0, 24, by = 4))
lp$AVAL <- stats::rlnorm(nrow(lp), 3, 0.3)
fig_lab_panels <- ggplot(lp, aes(x = AVISITN, y = AVAL, group = USUBJID)) +
  geom_line(colour = "grey70", linewidth = 0.3) +
  stat_summary(aes(group = 1), fun = median, geom = "line", linewidth = 1, colour = "black") +
  facet_wrap(~PARAM, scales = "free_y", ncol = 2, strip.position = "left",
             labeller = labeller(PARAM = label_wrap_gen(width = 20))) +
  scale_x_continuous(breaks = seq(0, 24, by = 4), labels = paste0("W", seq(0, 24, by = 4))) +
  scale_y_continuous(n.breaks = 6) +
  labs(x = NULL, y = NULL, tag = "A") +
  theme_bw() +
  theme(strip.placement = "outside", strip.background = element_blank(),
        panel.spacing.x = unit(1, "lines"), axis.ticks.length = unit(-0.1, "cm"),
        plot.tag.position = "topleft")

# a dot plot of the individual values with the median
fig_dot <- ggplot(lb[lb$AVISIT == "Week 8", ], aes(x = TRTA, y = AVAL)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5, binwidth = 0.2, fill = "grey60") +
  stat_summary(fun = median, fun.min = median, fun.max = median, geom = "crossbar",
               width = 0.5, linewidth = 0.3) +
  scale_x_discrete(labels = c(Placebo = "PBO", "Drug 10 mg" = "10 mg", "Drug 20 mg" = "20 mg"),
                   guide = guide_axis(n.dodge = 1, angle = 0)) +
  labs(x = NULL, y = "ALT (U/L) at week 8") +
  theme_classic(base_size = 11, base_family = "sans")

# a calendar of the dosing over months
ds <- data.frame(USUBJID = rep(sprintf("S%02d", 1:8), each = 2),
                 date = as.Date("2025-01-15") + c(0, 120, 10, 200, 20, 90, 30, 250,
                                                  40, 180, 50, 160, 60, 300, 70, 140))
ds$what <- rep(c("First dose", "Last dose"), 8)
fig_calendar <- ggplot(ds, aes(x = date, y = USUBJID, shape = what)) +
  geom_line(aes(group = USUBJID), colour = "grey50") +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", date_minor_breaks = "1 month",
               expand = expansion(add = 15)) +
  scale_shape_manual(values = c("First dose" = 16, "Last dose" = 4), name = NULL) +
  scale_y_discrete(limits = rev) +
  labs(x = NULL, y = "Subject") +
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "horizontal",
        panel.grid.major.y = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1))

# the correlation of the PK and the response, a bubble by dose
pr <- data.frame(CMAX = stats::rlnorm(60, 4, 0.5), DOSE = rep(c(10, 30, 100), 20))
pr$RESP <- 20 + 10 * log(pr$CMAX) + stats::rnorm(60, 0, 5)
fig_bubble <- ggplot(pr, aes(x = CMAX, y = RESP, colour = factor(DOSE))) +
  geom_point(aes(size = DOSE), alpha = 0.6) +
  geom_smooth(aes(group = 1), method = "loess", formula = y ~ x, span = 0.9, se = FALSE,
              colour = "black", linewidth = 0.5, show.legend = FALSE) +
  scale_size_area(max_size = 6, breaks = c(10, 30, 100)) +
  scale_colour_brewer(palette = "Set1", name = "Dose (mg)") +
  scale_x_continuous(transform = "log10", labels = scales::label_comma()) +
  guides(size = "none", colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  labs(x = "Cmax (ng/mL)", y = "Response") +
  theme_light() +
  theme(legend.position = "right", legend.key = element_blank())

# a heat map of the AE incidence by preferred term and arm, text in the tiles
hi <- expand.grid(AEDECOD = c("Nausea", "Headache", "Fatigue", "Rash", "Diarrhoea"), TRTA = arm)
hi$pct <- round(stats::runif(nrow(hi), 0, 30), 1)
fig_heat_text <- ggplot(hi, aes(x = TRTA, y = AEDECOD, fill = pct)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f", pct), colour = pct > 20), size = 3, show.legend = FALSE) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, limits = c(0, 30), name = "%") +
  scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "white")) +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  guides(fill = guide_colourbar(direction = "horizontal", position = "bottom")) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.key.width = unit(2, "cm"), legend.key.height = unit(0.3, "cm"),
        panel.grid = element_blank(), axis.ticks = element_blank())

# subject profiles: change by visit, one panel per subject in a grid
fig_profile <- ggplot(sp[sp$USUBJID %in% unique(sp$USUBJID)[1:6], ], aes(x = ADY, y = PCHG)) +
  geom_area(fill = "grey85", alpha = 0.6) +
  geom_line() +
  geom_point(aes(shape = PCHG <= -30), size = 1.5) +
  geom_hline(yintercept = c(-30, 20), linetype = "dashed") +
  facet_grid(rows = vars(USUBJID), switch = "y", space = "fixed", scales = "fixed") +
  scale_shape_manual(values = c(1, 16), labels = c("No response", "Response"), name = NULL) +
  scale_y_continuous(position = "right", breaks = c(-60, -30, 0, 20, 60)) +
  coord_cartesian(ylim = c(-80, 80), expand = TRUE) +
  labs(x = "Week", y = "% change") +
  theme_bw(base_size = 9) +
  theme(strip.text.y.left = element_text(angle = 0), legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0), panel.border = element_rect(colour = "grey50"))

# a stacked proportion of the visits by category, a dodge in the text
fig_area <- ggplot(rs, aes(x = TRTA, fill = AVALC)) +
  geom_bar(position = position_stack(reverse = TRUE), width = 0.6, colour = "black", linewidth = 0.2) +
  geom_text(aes(label = after_stat(count)), stat = "count",
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 3) +
  scale_fill_grey(start = 0.9, end = 0.4) +
  coord_flip() +
  labs(x = NULL, y = "Number of subjects", fill = "Response") +
  theme_bw() +
  theme(legend.position = "top", legend.title.position = "left", legend.byrow = TRUE,
        legend.text = element_text(size = 8), aspect.ratio = 0.4)

# exposure-response: the probability of a response by a logistic fit
pr$RESPFL <- as.integer(pr$RESP > 60)
fig_er_logit <- ggplot(pr, aes(x = CMAX, y = RESPFL, colour = factor(DOSE))) +
  geom_jitter(width = 0, height = 0.03, alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "glm", method.args = list(family = binomial),
              formula = y ~ x, colour = "black", fill = "grey80") +
  xlim(c(0, 250)) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("No response", "50%", "Response")) +
  scale_colour_manual(values = RColorBrewer::brewer.pal(3, "Dark2"), name = "Dose (mg)") +
  labs(x = "Cmax (ng/mL)", y = "Probability of response") +
  theme_bw()

# a spider plot whose reference lines have a legend, the CI band kept above -100
sp2 <- sp[sp$USUBJID %in% unique(sp$USUBJID)[1:8], ]
fig_spider_ref <- ggplot(sp2, aes(x = ADY, y = PCHG, group = USUBJID)) +
  geom_ribbon(aes(ymin = pmax(PCHG - 15, -100), ymax = PCHG + 15), fill = "grey90", colour = NA) +
  geom_line(aes(colour = TRTA)) +
  geom_hline(aes(yintercept = y, linetype = what),
             data = data.frame(y = c(20, -30), what = c("PD (+20%)", "PR (-30%)"))) +
  scale_colour_manual(values = setNames(c("grey40", "#1F78B4", "#E31A1C"), arm), name = NULL) +
  scale_linetype_manual(values = c("dashed", "dotted"), name = NULL) +
  labs(x = "Week", y = "Change in tumour size (%)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "vertical")

# ---- Round 15: the same figures as programs written for ggplot2 before 3.4 / 3.5 write them
# (size for a line's width, fun.y, ..count.., guide_legend(title.position =),
# legend.position = c(x, y), geom_errorbarh(), guide = FALSE, expand_scale())

# a KM plot of an older program
fig_old_km <- ggplot(km, aes(x = time, y = surv, colour = TRTA)) +
  geom_step(size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dotted", size = 0.4) +
  scale_y_continuous(limits = c(0, 1), expand = expand_scale(mult = c(0, 0.02))) +
  scale_colour_manual(values = c("grey40", "#1F78B4", "#E31A1C")) +
  guides(colour = guide_legend(title = NULL, keywidth = 2.5, label.hjust = 0)) +
  labs(x = "Months", y = "Survival probability") +
  theme_bw() +
  theme(legend.position = c(0.85, 0.85), legend.background = element_rect(colour = "grey70", size = 0.3),
        panel.grid.minor = element_line(size = 0.2))

# a forest plot of an older program: the horizontal error bars
fig_old_forest <- ggplot(fo, aes(x = hr, y = level)) +
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.5) +
  geom_errorbarh(aes(xmin = lcl, xmax = ucl), height = 0.2, size = 0.6) +
  geom_point(shape = 15, size = 3) +
  scale_x_log10() +
  labs(x = "Hazard ratio (95% CI)", y = NULL) +
  theme_classic() +
  theme(axis.line = element_line(size = 0.4))

# the mean +/- SE over time of an older program: fun.y, a legend inside
fig_old_mean <- ggplot(mt, aes(x = AVISITN, y = CHG, colour = TRTA)) +
  stat_summary(fun.y = mean, geom = "line", size = 0.8, position = position_dodge(width = 0.5)) +
  stat_summary(fun.y = mean, geom = "point", size = 2, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3, size = 0.5,
               position = position_dodge(width = 0.5)) +
  stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, geom = "linerange", alpha = 0.2,
               size = 2, position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = c(0, 2, 4, 8, 12)) +
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) +
  labs(x = "Week", y = "Mean (SE) change from baseline", colour = "Treatment") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title.align = 0.5)

# a bar chart of percentages by the stat's own counts, the old dotted form
fig_old_pct <- ggplot(ae, aes(x = AESOC, fill = AESEV)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), position = "dodge") +
  geom_text(aes(y = ..count.. / sum(..count..), label = ..count..), stat = "count",
            position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  scale_y_continuous(labels = scales::percent, expand = expand_scale(mult = c(0, 0.1))) +
  scale_fill_brewer(palette = "Blues", guide = guide_legend(reverse = TRUE, label.position = "left")) +
  labs(x = NULL, y = "Percent of events") +
  theme_minimal()

# a CDF plot of an older program
fig_old_cdf <- ggplot(cd, aes(x = CHG, colour = TRTA)) +
  stat_ecdf(geom = "step", size = 0.7) +
  geom_vline(xintercept = 0, colour = "grey50", size = 0.3) +
  scale_colour_manual(values = c("grey30", "#33A02C", "#6A3D9A"), guide = guide_legend(byrow = TRUE, ncol = 2)) +
  labs(x = "Change from baseline", y = "Cumulative proportion") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.8), legend.title = element_blank())

# a box plot of an older program: stat(), a legend turned off with FALSE
fig_old_box <- ggplot(lb, aes(x = AVISIT, y = AVAL, fill = TRTA)) +
  geom_boxplot(size = 0.4, outlier.size = 0.8) +
  stat_summary(aes(group = TRTA), fun.y = mean, geom = "point", shape = 23, size = 2,
               position = position_dodge(width = 0.75)) +
  scale_fill_grey(start = 0.5, end = 0.9) +
  guides(fill = FALSE) +
  labs(x = NULL, y = "ALT (U/L)") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, size = 0.5))

# a heat map of an older program: the colour bar's size and title
fig_old_heat <- ggplot(hm, aes(x = x, y = y, fill = r)) +
  geom_tile(colour = "white", size = 0.5) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", limits = c(-1, 1)) +
  guides(fill = guide_colourbar(barwidth = 15, barheight = 0.8, title.position = "top",
                                title.hjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Correlation") +
  theme_minimal() +
  theme(legend.position = "bottom")

# a waterfall plot of an older program: aes_string(), the bars' outline
fig_old_waterfall <- ggplot(wf, aes_string(x = "reorder(USUBJID, -PCHG)", y = "PCHG", fill = "BOR")) +
  geom_col(width = 0.8, colour = "grey30", linewidth = 0.2) +
  geom_hline(yintercept = c(20, -30), linetype = "dashed", size = 0.4) +
  scale_fill_manual(values = c(CR = "#1A9850", PR = "#91CF60", SD = "#FEE08B", PD = "#D73027")) +
  labs(x = NULL, y = "Best change from baseline (%)") +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.position = c(0.9, 0.8), legend.text.align = 0)
