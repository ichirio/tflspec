Here is the report file of T-14-1-1.

```yaml
tflspec_ai: {task: table, version: 1, outputs: [T-14-1-1]}
outputs:
  - id: T-14-1-1
    kind: table
    label: Demographic and Baseline Characteristics
    population: SAF
    extends: [defaults]
    ard:
      analyses:
        - {id: N, method: total_n, dataset: ADSL, population: SAF}
        - {id: AGE, method: continuous, dataset: ADSL, population: SAF, by: [TRT01A], overall: true, variables: [AGE]}
        - {id: CAT, method: categorical, dataset: ADSL, population: SAF, by: [TRT01A], overall: true, variables: [SEX, AGEGR1, RACE]}
    data: {source: ard}
    codelists:
      SEX:
        - {value: M, label: Male}
        - {value: F, label: Female}
    content:
      - table_plan: {cols: TRT01A, rows: {group: variable}}
      - plan_cells:
          continuous: {n: "{N:.0f}", Mean (SD): "{mean:.1f} ({sd:.2f})", Median: "{median:.1f}", "Min, Max": "{min:.0f}, {max:.0f}"}
      - plan_total: {label: Total}
      - plan_labels: {AGE: Age (years), SEX: "Sex, n (%)", AGEGR1: "Age group, n (%)", RACE: "Race, n (%)"}
      - plan_levels: {variable: [AGE, SEX, AGEGR1, RACE], TRT01A: [Drug X, Control]}
      - plan_col_header:
          header:
            - {line: "1", cols: .values, span: each, text: "{col}"}
            - {line: "2", cols: row_label, text: Characteristic}
            - {line: "2", cols: .values, span: each, text: "(N={n})"}
    page:
      titles:
        "1": {center: Table 14.1.1}
        "2": {center: Demographic and Baseline Characteristics}
        "3": {center: Safety Population}
assumptions:
  - {output_id: T-14-1-1, where: ard.analyses, item: arm, assumption: "the columns are the actual treatment, TRT01A", question: "Actual (TRT01A) or planned (TRT01P) treatment?"}
  - {output_id: T-14-1-1, where: codelists, item: SEX, assumption: "the data code sex as M / F; printed Male, Female", question: "Is this the order the shell wants?"}
```

Please check the assumptions before you use the file.
