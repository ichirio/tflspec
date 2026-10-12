An answer in the design's shape of before 0.0.24.9074: a `stats` list.

```yaml
tflspec_ai: {task: figure, version: 1, output_id: F-14-2-1}
template: km_simple
data:
- {step: read, dataset: ADTTE}
- {step: param, value: OS}
stats:
- {step: survfit, name: fit, time: AVAL, censor: CNSR, by: TRT01P}
layers:
- {layer: km_curve}
assumptions: []
```
