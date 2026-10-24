## Templates first

The study has figure templates: a figure kind written from a few arguments (listed below). When one fits the figure, answer with it instead of a design:

- After the header: `template: {name: <template>, args: {<argument>: <value>, ...}}` -- a mapping, unlike a design's `template` (a preset's name). Give only the arguments the shell asks to differ from the default.
- `data`: the data stage, when the template's arguments do not hold it (the Kaplan-Meier types): a design's data steps -- `read`, the analysis set (`join`, `how: semi`, on ADSL), `param`, `filter`, `derive` -- before the template's call, which makes the rest (the time's unit, the fit).
- `layers`: only the figure's own items, for what the template does not draw (a reference line, a label), each as a design's layer.
- Then `assumptions`, as always.
- When no template fits, answer with the design, as the schema above says.
- An argument a template does not list is an error. Name only the templates and arguments below.
- Kaplan-Meier figures: `km_survival` (the survival S(t): curves, median line, number at risk, p-value) or `km_cumulative` (the cumulative percentage 1 - S(t): CI bars, number at risk, p-value; FDA STF Figures 1, 4 and 13). `by` (the curves' variable) is required; the dataset, the analysis set and the parameter are `data` steps, not arguments. The look (censor marks' shape, line widths, palette, theme) is the study's figure style, not an argument.
- FDA STF Figures 1, 4 and 13 are written as `km_cumulative` with `data` steps (the parameter), `time_unit: weeks` and the axes' labels: see `system.file("extdata", "report-definitions", package = "tflspec")` (`stf_f01_km_disc.yml`, `stf_f04_km_ae_onset.yml`, `stf_f13_km_lfu.yml`).

An example: Kaplan-Meier curves of progression-free survival by arm, with a dashed line at the median.

```yaml
tflspec_ai: {task: figure, version: 1, output_id: F-14-2-1}
data:
  - {step: read, dataset: ADTTE}
  - {step: join, dataset: ADSL, where: SAFFL == "Y", by: USUBJID, how: semi}
  - {step: param, value: PFS}
template:
  name: km_survival
  args: {by: TRT01A, time_unit: months, median_line: false}
layers:
  - {layer: geom_hline, yintercept: 0.5, linetype: dashed}
assumptions:
  - {output_id: F-14-2-1, where: template, item: km_survival, assumption: the shell is the standard KM plot (number at risk and p-value too), question: ~}
```
