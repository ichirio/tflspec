```yaml
tflspec_ai: {task: figure, version: 1}
template: km_simple
data:
- {step: read, dataset: ADTTE}
stats:
- {step: survfit, name: fit, time: AVAL, censor: CNSR}
layers:
- {layer: km_curve}
assumptions: []
```
