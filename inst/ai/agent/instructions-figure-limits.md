## Known limits and what to write instead

The template or step has no argument for these; write the item shown.

1. A KM median (95% CI) box: one `layer_code` on the `km` template (`plot: add:` with `annotate` works too).
   `{layer: layer_code, code: 'p <- p + ggplot2::annotate("label", x = -Inf, y = -Inf, hjust = -0.05, vjust = -0.1, size = 3, label = <R text>)'}`
   The text is built from the fit: `summary(survival::survfit(survival::Surv(AVAL, CNSR == 0) ~ TRT01P, data = <the figure's data>))$table`
   gives `median`, `0.95LCL`, `0.95UCL` a curve; `sprintf("%.1f (%.1f, %.1f)", ...)`, one line an arm.
2. A long y-axis title is cut: break it with `
` in a double-quoted YAML string (not a real line break).
   `plot: {y_label: "Mean Change from Baseline (95% CI)
in Diastolic BP (mmHg)"}`
3. A `rate` step's label prints on two lines: a `code` step after it (`rt` is the rate step's `name`).
   `- {step: code, code: 'rt$label <- sprintf("%.1f%% (%d/%d)", rt$rate, rt$x, rt$n)', label: "One-line label rate (n/N)"}`
4. Arms come in alphabetical order (Control first): a `levels` step on the arm variable, in the data steps before the plot. The palette follows the order.
   `- {step: levels, variable: TRT01P, levels: Drug X | Control}` (the arms in the shell's order)
5. `forest_hr` subgroup headings: the argument `subgroup_labels` (`{AGEGR1: Age group, SEX: Sex}`), not a layer; the common ADaM variables read their label without it.
