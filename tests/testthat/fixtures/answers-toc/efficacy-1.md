Here is the report file of T-14-2-1.

```yaml
tflspec_ai: {task: table, version: 1, outputs: [T-14-2-1]}
outputs:
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
assumptions:
  - {output_id: T-14-2-1, where: ard.analysis_data, item: RESP, assumption: "a responder has a best overall response of CR or PR (PARAMCD BOR)", question: "Confirmed responses only?"}
```

Please check the assumptions before you use the file.
