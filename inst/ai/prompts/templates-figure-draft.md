An example: Kaplan-Meier curves of progression-free survival by arm, with a dashed line at the median (`spec/outputs/F-14-2-1.yml`).  The `data` steps are the data stage only -- `read`, the analysis set, `param`; the template makes the time unit and the fit (`survfit`): never write those, and never leave `data.steps` out.  `plot:` changes the template's settings as a designed figure's `plot:` does (axis titles, limits and steps, the legend); `layers` adds to its layers.

```yaml
id: F-14-2-1
kind: figure
label: "Kaplan-Meier Plot of Progression-Free Survival"
population: SAF
extends: [defaults]
template:
  name: km_survival
  args: {by: TRT01A, time_unit: months, median_line: false}
data:
  source: code
  steps:
    - {step: read, dataset: ADTTE}
    - {step: flag, variable: SAFFL}
    - {step: param, value: PFS}
plot: {legend: bottom}
layers:
  - {layer: geom_hline, yintercept: 0.5, linetype: dashed}
page:
  titles:
    "1": {center: "Figure 14.2.1"}
ai:
  assumptions:
    - {where: template, item: km_survival, assumption: "the standard KM plot: the shell has the number at risk and the p-value too", value: km_survival}
```
