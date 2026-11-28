## Known limits and what to write instead

The template or step has no argument for these; write the item shown.

1. A KM median (95% CI) box: one `layer_code` on the `km` template (`plot: add:` with `annotate` works too).
   `{layer: layer_code, code: 'p <- p + ggplot2::annotate("label", x = -Inf, y = -Inf, hjust = -0.05, vjust = -0.1, size = 3, label = <R text>)'}`
   The text is built from the fit: `summary(survival::survfit(survival::Surv(AVAL, CNSR == 0) ~ TRT01P, data = <the figure's data>))$table`
   gives `median`, `0.95LCL`, `0.95UCL` a curve (`m` below); one line an arm:
   `paste0(fmt_num(m[, "median"], 1), " (", fmt_num(m[, "0.95LCL"], 1), ", ", fmt_num(m[, "0.95UCL"], 1), ")")`.
2. `forest_hr` subgroup headings: the argument `subgroup_labels` (`{AGEGR1: Age group, SEX: Sex}`), not a layer; the common ADaM variables read their label without it.

A number the figure prints itself (a label, an annotation, a text column) is `fmt_num(x, digits)`, not
`sprintf("%.1f", x)`, `formatC()` or `round()`: it rounds by the study's rule, so the figure prints what the table prints.
A percent is scaled first (`fmt_num(100 * n / N, 1)`); a missing value prints `NE` (`na =` another text); a p-value is `fmt_pvalue(p)`.
