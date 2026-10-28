Here are the report files of the four tables.

```yaml
tflspec_ai: {task: table, version: 1, outputs: [T-14-1-1, T-14-2-1, T-14-3-1, T-14-3-5]}
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
  - id: T-14-2-1
    kind: table
    label: Best Overall Response and Objective Response Rate
    population: ITT
    extends: [defaults]
    ard:
      analysis_data:
        - {id: adrs_bor, from: ADRS, population: ITT, where: !r PARAMCD == "BOR", derive: !r 'RESP = AVALC %in% c("CR", "PR")'}
      analyses:
        - {id: BOR, method: categorical, data: adrs_bor, by: [TRT01P], variables: [AVALC]}
        - {id: ORR, method: proportion_ci, data: adrs_bor, by: [TRT01P], variables: [RESP], args: !r method = "clopper-pearson"}
    data: {source: ard}
    codelists:
      AVALC:
        - {value: CR, label: Complete response (CR)}
        - {value: PR, label: Partial response (PR)}
        - {value: SD, label: Stable disease (SD)}
        - {value: PD, label: Progressive disease (PD)}
        - {value: NE, label: Not evaluable (NE)}
    content:
      - table_plan: {cols: TRT01P, rows: {group: variable}}
      - plan_cells:
          proportion_ci: {"n (%)": "{n} ({estimate:.1f%})", 95% CI: "[{conf.low:.1f%}, {conf.high:.1f%}]"}
      - plan_labels: {AVALC: "Best overall response, n (%)", RESP: Objective response rate (CR + PR)}
      - plan_levels: {variable: [AVALC, RESP], TRT01P: [Drug X, Control]}
      - plan_col_header:
          header:
            - {line: "1", cols: .values, span: each, text: "{col}"}
            - {line: "2", cols: row_label, text: Response}
            - {line: "2", cols: .values, span: each, text: "(N={n})"}
    page:
      titles:
        "1": {center: Table 14.2.1}
        "2": {center: Best Overall Response and Objective Response Rate}
        "3": {center: Intent-To-Treat Population}
      footnotes:
        "1": {left: "95% CI: Clopper-Pearson exact confidence interval."}
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
  - {output_id: T-14-1-1, where: ard.analyses, item: arm, assumption: "Treatment variable for the columns: TRT01A (actual), because the analysis set is the safety set (SAF)", value: TRT01A, question: "Actual (TRT01A) or planned (TRT01P) treatment?"}
  - {output_id: T-14-1-1, where: codelists, item: SEX, assumption: "the data code sex as M / F; printed Male, Female", question: "Is this the order the shell wants?"}
  - {output_id: T-14-2-1, where: ard.analysis_data, item: RESP, assumption: "a responder has a best overall response of CR or PR (PARAMCD BOR)", question: "Confirmed responses only?"}
  - {output_id: T-14-3-1, where: ard.analyses, item: where, assumption: "treatment-emergent is TRTEMFL == \"Y\"", question: "Is TRTEMFL the treatment-emergent flag?"}
  - {output_id: T-14-3-5, where: ard.analysis_data, item: visit, assumption: "the shell's on-treatment visit is CYCLE 2 DAY 1; ALT and AST only", question: "Which visit, or the worst post-baseline value?"}
```

Please check the assumptions before you use the files.
