```json
{
  "tflspec_ai": {"task": "figure", "version": 1, "output_id": "F-14-2-1"},
  "template": "km_simple",
  "data": [
    {"step": "read", "dataset": "ADTTE"},
    {"step": "param", "value": "OS"}
  ],
  "stats": [{"step": "survfit", "name": "fit", "time": "AVAL", "censor": "CNSR", "by": "TRT01P"}],
  "plot": {"x_label": "Time (Days)", "colour_by": "TRT01P", "y_min": 0, "y_max": 1},
  "layers": [{"layer": "km_curve"}],
  "assumptions": []
}
```
