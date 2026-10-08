Here is the design, from the template km_risk_table.

```yaml
tflspec_ai: {task: figure, version: 1, output_id: F-14-2-1}
template: km_risk_table
data:
- {step: read, dataset: ADTTE}
- {step: param, value: OS}
- {step: flag, variable: FASFL}
- {step: time_unit, variable: AVAL, unit: months}
stats:
- {step: survfit, name: fit, time: AVAL, censor: CNSR, by: TRT01P}
plot:
  title: "Kaplan-Meier Plot of Overall Survival"
  x_label: "Time (Months)"
  y_label: Survival Probability
  colour_by: TRT01P
  legend: inside
layers:
- {layer: km_curve}
- {layer: censor_mark, shape: x}
- {layer: risk_table}
assumptions:
- "The SAP does not say where the legend goes; inside the plot as in the shell."
```
