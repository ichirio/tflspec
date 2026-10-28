Here is the design of F-14-2-2.

```yaml
tflspec_ai: {task: figure, version: 1, output_id: F-14-2-2}
template: km_simple
data:
- {step: read, dataset: ADTTE}
- {step: param, value: OS}
- {step: flag, variable: ITTFL}
- {step: survfit, name: fit, time: AVAL, censor: CNSR, by: TRT01P}
plot:
  x_label: Time (Months)
  y_label: Survival Probability
  colour_by: TRT01P
  palette: treatment
  legend: inside
  x_min: 0
  y_min: 0
  y_max: 1
  y_by: 0.2
  dpi: 300
  base_size: 10
  theme: boxed
layers:
- {layer: km_curve, linewidth: 0.3}
- {layer: censor_mark, shape: plus, size: 3, stroke: 0.6}
assumptions:
- {output_id: F-14-2-2, where: data, item: arm, assumption: "Treatment variable for the curves: TRT01P (planned), because the analysis set is the Intent-To-Treat population", value: TRT01P, question: "Planned (TRT01P) or actual (TRT01A) treatment for the curves?"}
- {output_id: F-14-2-2, where: data, item: time, assumption: "AVAL is already in months (AVALU is MONTHS in the ADaM summary): no conversion", question: "Is OS's AVAL in months?"}
- {output_id: F-14-2-2, where: plot, item: x axis, assumption: "the x axis' breaks are the default; the shell gives none", question: "Ticks every 6 months?"}
```
