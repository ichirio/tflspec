## Templates first

The study has templates: a report kind written from a few arguments (the templates are listed below). When one fits a report, write that report as the template, not as its analyses and items:

- `template: {name: <template>, args: {<argument>: <value>, ...}}` in place of `ard`, `data` and the template's `content`. Give only the arguments the shell asks to differ from the default; the analysis set goes in the report's `population` (or the `population` argument).
- Keep `id`, `kind`, `label`, `population`, `extends`, `codelists` and `page` as for any report.
- `content`: only the report's own items, for what the template does not do (an item of the same function changes the template's: `plan_labels`, `plan_cells` ... merged by key).
- When no template fits, or the shell needs analyses no template has, write the report as the schema above says (a code SPEC). Never mix: a report has `template` or `ard`, not both.
- An argument a template does not list is an error. Name only the templates and arguments below.

An example: a demographics table as its template, and the adverse events by SOC / PT with one item of its own.

```yaml
tflspec_ai: {task: table, version: 1, outputs: [T-14-1-1, T-14-3-1]}
outputs:
  - id: T-14-1-1
    kind: table
    label: Demographic and Baseline Characteristics
    population: SAF
    extends: [defaults]
    template:
      name: demog
      args: {categorical: [AGEGR1, SEX, RACE]}
    page:
      titles:
        "1": {center: Table 14.1.1}
        "2": {center: Demographic and Baseline Characteristics}
  - id: T-14-3-1
    kind: table
    label: Treatment-Emergent Adverse Events by System Organ Class and Preferred Term
    population: SAF
    extends: [defaults]
    template:
      name: ae_soc_pt
      args: {overall: Subjects with at least one TEAE}
    content:
      - plan_paginate_rows: {max_rows: 18}
    page:
      titles:
        "1": {center: Table 14.3.1}
assumptions:
  - {output_id: T-14-3-1, where: template, item: ae_soc_pt, assumption: "Template: the standard SOC / PT table, because the shell has the same rows and columns", value: ae_soc_pt, question: ~}
```
