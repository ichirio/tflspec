## Templates first

The study has figure templates: a figure kind written from a few arguments (listed below). When one fits the figure, answer with it instead of a design:

- After the header: `template: {name: <template>, args: {<argument>: <value>, ...}}` -- a mapping, unlike a design's `template` (a preset's name). Give only the arguments the shell asks to differ from the default.
- `layers`: only the figure's own items, for what the template does not draw (a reference line, a label), each as a design's layer.
- Then `assumptions`, as always.
- When no template fits, answer with the design, as the schema above says.
- An argument a template does not list is an error. Name only the templates and arguments below.

An example: Kaplan-Meier curves of progression-free survival by arm, with a dashed line at the median.

```yaml
tflspec_ai: {task: figure, version: 1, output_id: F-14-2-1}
template:
  name: km_simple
  args: {param: PFS, group: TRT01A, time_unit: months}
layers:
  - {layer: geom_hline, yintercept: 0.5, linetype: dashed}
assumptions:
  - {output_id: F-14-2-1, where: template, item: km_simple, assumption: the shell is the standard KM plot, question: ~}
```
