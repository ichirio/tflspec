Here is the report file of T-14-3-1.

```yaml
tflspec_ai: {task: table, version: 1, outputs: [T-14-3-1]}
outputs:
  - id: T-14-3-1
    kind: table
    label: Treatment-Emergent Adverse Events by System Organ Class and Preferred Term
    population: SAF
    extends: [defaults]
    ard:
      analyses:
        - {id: AE, method: hierarchical, dataset: ADAE, population: SAF, where: !r TRTEMFL == "Y", by: [TRT01A], variables: [AEBODSYS, AEDECOD], args: !r over_variables = TRUE}
    data: {source: ard}
    content:
      - normalize_ard: {hierarchy: [AEBODSYS, AEDECOD], overall: Subjects with at least one TEAE}
      - table_plan: {cols: TRT01A, rows: {group1: AEBODSYS}}
      - plan_levels: {TRT01A: [Drug X, Control]}
      - plan_sort: {keys: [.overall, group1, .depth, -n, label]}
      - plan_col_header:
          header:
            - {line: "1", cols: row_label, text: System Organ Class}
            - {line: "1", cols: .values, span: each, text: "{col}"}
            - {line: "2", cols: row_label, text: "  Preferred Term"}
            - {line: "2", cols: .values, span: each, text: "(N={n})"}
    page:
      titles:
        "1": {center: Table 14.3.1}
        "2": {center: Treatment-Emergent Adverse Events by System Organ Class and Preferred Term}
        "3": {center: Safety Population}
      footnotes:
        "1": {left: A subject is counted once in a system organ class and once in a preferred term.}
assumptions:
  - {output_id: T-14-3-1, where: ard.analyses, item: where, assumption: "treatment-emergent is TRTEMFL == \"Y\"", question: "Is TRTEMFL the treatment-emergent flag?"}
```

Please check the assumptions before you use the file.
