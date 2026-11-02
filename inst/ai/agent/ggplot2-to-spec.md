# From ggplot2 code to a figure SPEC

A figure draft is a SPEC (`instructions-figure.md`: `data`, `plot`,
`layers`). Write the SPEC directly when you can see it. When you are unsure
how the shell's figure is said in the SPEC, write it first as ggplot2 code,
the way you would for a program, then convert that code to the SPEC with
the table below. The SPEC is the answer; the code is your working.

## 1. Write the code in the form that converts

Write one chain by default: the data, then the plot.

```r
df <- adlb |>
  filter(PARAMCD == "ALT", SAFFL == "Y", AVISIT %in% vis) |>
  semi_join(adsl |> filter(SAFFL == "Y"), by = "USUBJID") |>
  mutate(AVISIT = reorder(AVISIT, AVISITN))
sm <- df |> group_by(TRT01A, AVISIT) |>
  summarise(n = n(), mean = mean(AVAL), se = sd(AVAL) / sqrt(n()), .groups = "drop")
p <- ggplot(sm, aes(AVISIT, mean, colour = TRT01A, group = TRT01A)) +
  geom_line(position = position_dodge(0.3)) +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2,
                position = position_dodge(0.3)) +
  geom_hline(yintercept = 40, linetype = "dashed") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(x = "Visit", y = "ALT (U/L)", colour = "Treatment") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
```

- One `|>` pipe a data frame, one `+` chain for the plot, in this order:
  the layers, then the scales, `labs()`, `facet_*()`, `coord_*()`,
  `theme()`.
- If one chain is too hard, cut the code into a few named chunks: the data
  (`df`), each object made from it (`sm`, `fit`, `est`), the plot (`p`), a
  panel under or beside it (the number at risk, a text column). Each chunk
  converts to its part of the SPEC.
- Use plain dplyr verbs and ggplot2 functions. A helper function, a loop,
  or `p$data <- ...` has no SPEC form.
- Keep the code in `ai/code/<output id>.R`. It is not run; it is the record
  of how you got to the SPEC.

## 2. Convert, line by line

### The data (`data:` steps, in the code's order)

| ggplot2 / dplyr | SPEC |
|---|---|
| `df <- adlb` | `{step: read, dataset: ADLB}` |
| `filter(PARAMCD == "ALT")` | `{step: param, value: ALT}` |
| `filter(SAFFL == "Y")` | `{step: flag, variable: SAFFL}` |
| `semi_join(adsl \|> filter(EFFFL == "Y"), by = "USUBJID")` | `{step: join, dataset: ADSL, where: 'EFFFL == "Y"', how: semi}` |
| `left_join(adrs \|> filter(PARAMCD == "BOR") \|> select(USUBJID, BOR = AVALC))` | `{step: join, dataset: ADRS, where: 'PARAMCD == "BOR"', vars: BOR = AVALC}` |
| any other `filter(...)` | `{step: filter, expr: ...}` |
| `mutate(X = ...)` | `{step: derive, variable: X, expr: ...}` |
| `mutate(AVISIT = reorder(AVISIT, AVISITN))`, `factor(x, levels = )` | `{step: levels, ...}` |
| `arrange(desc(v)) \|> mutate(i = row_number())` | `{step: rank, ...}` |
| `group_by(...) \|> summarise(n, mean, sd, se ...)` | `{step: summary, name: sm, ...}` (`summary_by`: by group only) |
| responders / n with a binomial CI | `{step: rate, ...}` |
| `count(group, category)` with % | `{step: count, ...}` |
| `survfit(Surv(AVAL, 1 - CNSR) ~ TRT01P)` | `{step: survfit, name: fit, time: AVAL, censor: CNSR, by: TRT01P}` |
| AVAL in days shown in months / weeks | `{step: time_unit, variable: AVAL, unit: months}` |
| anything else that makes a data frame | `{step: code, code: 'df <- ...'}` (the datasets by their lower-case names) |

### The layers (`layers:`, in the chain's order)

| ggplot2 | SPEC |
|---|---|
| `geom_line()`, `geom_point()`, `geom_col()`, `geom_boxplot()`, `geom_errorbar()`, `geom_text()`, `geom_segment()`, `geom_ribbon()`, `geom_step()`, `geom_area()`, `geom_tile()`, `geom_histogram()`, `geom_density()`, `geom_jitter()`, `geom_smooth()` | the layer of the same short name: `{layer: line, data: sm, x: AVISIT, y: mean, colour: TRT01A}`. Each `aes()` mapping is a field; each constant (`width = 0.2`, `linetype = "dashed"`) is a field too |
| `geom_errorbarh()` / `geom_errorbar(orientation = "y")` | `{layer: errorbar_h, ...}` |
| `geom_hline(yintercept = 40)`, `geom_vline()`, `geom_abline()` | `{layer: hline, yintercept: 40, linetype: dashed}`; several values: `yintercept: 20, -30` |
| `position = position_dodge(0.3)` | `dodge: true` on the layer, `dodge: 0.3` in `plot` |
| `ggsurvfit()` + `add_censor_mark()` + `add_confidence_interval()` + `add_risktable()` | `km_curve` (`type: risk` for 1 - S(t)), `censor_mark`, `km_ci`, `risk_table` |
| the n of each group at each x, under the plot | `{layer: n_table, ...}` |
| text columns at the right (a forest's N and HR (95% CI)) | `{layer: text_column, ...}` |
| labels at reference lines, right of the panel | `{layer: ref_label, y: 20, -30, label: "{y}%"}` |
| any other geom or stat | `{layer: geom, geom: geom_xxx, data: df, aes: x = A \| y = B, params: alpha = 0.3}` |
| a ggplot2 or extension function with exact arguments | `{layer: call, fn: ..., aes: ..., args: ...}` |
| `annotate(...)`, `guides(...)`, a `theme()` that `plot` cannot say | `{layer: layer_code, code: 'p <- p + annotate(...)'}`. It runs after the settings, so its `theme()` and `labs()` have the last word |

### The settings (`plot:`)

| ggplot2 | SPEC |
|---|---|
| `labs(x = "Visit", y = "ALT (U/L)", title = )` | `x_label`, `y_label`, `title` |
| `labs(x = NULL)` (no axis title) | `x_label: ""` |
| `labs(colour = "Treatment")` (the legend's title) | `legend_title: Treatment` |
| `aes(colour = TRT01A)` / `aes(fill = BOR)` with a manual scale | `colour_by: TRT01A` |
| `scale_fill_manual(values = c(CR = "darkgreen", PD = "red"))` | `colours: {CR: darkgreen, PD: red}` (over the study palette `palette`) |
| `scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))` | `y_min: 0`, `y_max: 100`, `y_by: 20` (x likewise) |
| `scale_x_continuous(breaks = seq(0, 36, 6))` | `x_by: 6` (`x_max: 36` for the end) |
| `coord_cartesian(ylim = c(-100, 100))` | `y_min: -100`, `y_max: 100` |
| `scale_x_log10()` / `scale_y_log10()` | `x_log: true` / `y_log: true` |
| `coord_equal()` (with the same limits) | `equal: true` with `x_min` ... `y_max` |
| `facet_wrap(~ TRT01P)` | `facet_by: TRT01P` |
| `theme(legend.position = "bottom")`, `"none"`, inside top right | `legend: bottom`, `none`, `inside` |
| `theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())` | `x_text: false` |
| `theme_bw()`, `theme_minimal()` | leave out: the study's theme (`theme`) is the figure style |
| any other `scale_*()`, `theme(...)`, `guides()`, `coord_*()` | `add: [{fn: theme, args: 'axis.text.x = element_text(angle = 45, hjust = 1)'}]`, or a `layer_code` |

### Titles

The shell's title lines and population go to `page.titles`, not to the
plot (the report prints them above the figure).

## 3. What has no SPEC form

- First try `layer_code` (`p <- p + ...`, after the settings) or
  `plot.add`.
- A figure of two plots side by side or one under the other: use the
  panels the SPEC has (`n_table`, `text_column`, `risk_table`). Anything
  else needs a `figure` type, or an `ai: confirm` that names what is lost.
- Never drop a shell item silently: what the SPEC does not draw goes to
  `ai: confirm` (`where`, `item`, `why`).

## 4. Say which way you went

For each figure, add one assumption:

```yaml
ai:
  assumptions:
    - {where: figure, item: route, value: direct, assumption: "the SPEC written directly"}
    # or
    - {where: figure, item: route, value: code, assumption: "ggplot2 first (ai/code/F-14-2-4.R), then converted"}
```

Then check and build the draft as usual (`instructions-figure.md`).
