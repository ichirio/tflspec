Here are the report files of the sheet: T-14-3-1 and T-14-3-5.

```yaml
tflspec_ai: {task: table, version: 1, outputs: [T-14-3-1, T-14-3-5]}
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
  - id: T-14-3-5
    kind: table
    label: Shift from Baseline in Laboratory Normal Range Indicator at Cycle 2 Day 1
    population: SAF
    extends: [defaults]
    ard:
      analysis_data:
        - {id: adlb_c2, from: ADLB, population: SAF, where: !r 'AVISIT == "CYCLE 2 DAY 1" & PARAMCD %in% c("ALT", "AST")'}
      analyses:
        - {id: SHIFT, method: categorical, data: adlb_c2, by: [TRT01A, PARAMCD, BNRIND], variables: [ANRIND]}
    data: {source: ard}
    content:
      - table_plan: {cols: [TRT01A, .label], rows: {group: PARAMCD}, label: BNRIND}
      - plan_cells: {categorical: "{n}"}
      - plan_levels: {TRT01A: [Drug X, Control], BNRIND: [LOW, NORMAL, HIGH], .label: [LOW, NORMAL, HIGH]}
      - plan_labels: {PARAMCD: {ALT: Alanine Aminotransferase (U/L), AST: Aspartate Aminotransferase (U/L)}}
      - plan_stub: {vars: [group, BNRIND], name: row_label, before: true}
      - plan_col_header:
          header:
            - {line: "1", cols: .values, span: TRT01A, text: "{col1}"}
            - {line: "2", cols: row_label, text: "Parameter / Baseline"}
            - {line: "2", cols: .values, span: each, text: "{col}"}
    page:
      titles:
        "1": {center: Table 14.3.5}
        "2": {center: Shift from Baseline in Laboratory Normal Range Indicator at Cycle 2 Day 1}
        "3": {center: Safety Population}
assumptions:
  - {output_id: T-14-3-1, where: ard.analyses, item: where, assumption: "treatment-emergent is TRTEMFL == \"Y\"", question: "Is TRTEMFL the treatment-emergent flag?"}
  - {output_id: T-14-3-5, where: ard.analysis_data, item: visit, assumption: "the shell's on-treatment visit is CYCLE 2 DAY 1; ALT and AST only", question: "Which visit, or the worst post-baseline value?"}
```

Please check the assumptions before you use the file.
