# The ARS guide: a CDISC ARS reporting event beside the drafts

Read this only when the person asks for an ARS reporting event (CDISC
Analysis Results Standard v1.0, JSON). The report drafts do not need one.

`ai/ars/example-reporting-event.json` is a whole reporting event that
passes the check: one table, one analysis set, two simple data subsets,
one compound data subset, one method, two analyses. Its study, ids and
names are made up. Start from its shape; take the study's own datasets,
variables, values and ids from the request and the files it names.

## The check

Every reporting event written is checked, in R:

```r
tfl_check_ars("ai/ars/<name>.json")
```

Zero rows: it passes. Each row is a problem: `part` (the class and id),
`field` and `problem`. Fix every row and check again. The check reads the
JSON Schema of ARS v1.0 when the jsonvalidate package is installed; the
rows whose `part` starts with `schema` come from it.

## What every object carries

| Object | Required |
|---|---|
| reporting event | `id`, `name`, `mainListOfContents` |
| `analysisSets[]`, `dataSubsets[]` | `id`, `name`, `level`, `order`, and a `condition` or a `compoundExpression` |
| a `whereClauses[]` item of a `compoundExpression` | `level`, `order`, and `subClauseId` **or** its own `condition` / `compoundExpression` |
| `analysisGroupings[]` | `id`, `name`, `dataDriven` |
| `methods[]` | `id`, `name`, `operations` (each `id`, `name`, `order`) |
| `analyses[]` | `id`, `name`, `methodId`, `purpose`, `reason` |
| `outputs[]` | `id`, `name`, `displays` (each `order` and `display` with `id`, `name`) |
| a list item of `mainListOfContents` | `name`, `level`, `order` |

`level` and `order` are integers, not strings. Every id is used once in
its class, and every `...Id` names an object that is there.

## A simple data subset

One condition: `dataset`, `variable`, `comparator` (`EQ`, `NE`, `GT`,
`GE`, `LT`, `LE`, `IN`, `NOTIN`) and `value`, always a list of strings,
even for one value or a number.

```json
{
  "id": "Dss_TEAE",
  "name": "Treatment-emergent adverse events",
  "level": 1,
  "order": 1,
  "condition": {
    "dataset": "ADAE",
    "variable": "TRTEMFL",
    "comparator": "EQ",
    "value": ["Y"]
  }
}
```

## A compound data subset

Two or more conditions joined with `AND` or `OR` (or one negated with
`NOT`) are a `compoundExpression`. Each item of its `whereClauses` is a
sub-clause and carries its own **`level`** and **`order`**: `level` one
deeper than the subset that holds it (the subset is 1, its sub-clauses 2),
`order` 1, 2, ... within the expression. A sub-clause that is another data
subset names it by **`subClauseId`** (that subset must be in
`dataSubsets` too):

```json
{
  "id": "Dss_TEAE_SER",
  "name": "Serious treatment-emergent adverse events",
  "level": 1,
  "order": 3,
  "compoundExpression": {
    "logicalOperator": "AND",
    "whereClauses": [
      {
        "subClauseId": "Dss_TEAE",
        "level": 2,
        "order": 1
      },
      {
        "subClauseId": "Dss_SER",
        "level": 2,
        "order": 2
      }
    ]
  }
}
```

A sub-clause may instead hold its condition itself, still with `level` and
`order`: `{"level": 2, "order": 1, "condition": {...}}`. Never leave a
sub-clause without `level` and `order`, and never write a bare condition
as a sub-clause -- the check reports each one. This is wrong:

```text
"whereClauses": [
  {"dataset": "ADAE", "variable": "TRTEMFL", "comparator": "EQ", "value": ["Y"]},
  {"subClauseId": "Dss_SER"}
]
```

## An analysis using a subset

The analysis names its subset by `dataSubsetId`, its analysis set by
`analysisSetId`, its method by `methodId`. `purpose` and `reason` are CDISC
terms (`PRIMARY OUTCOME MEASURE`, `SECONDARY OUTCOME MEASURE`,
`EXPLORATORY OUTCOME MEASURE`; `SPECIFIED IN PROTOCOL`, `SPECIFIED IN SAP`,
`DATA DRIVEN`, `REQUESTED BY REGULATORY AGENCY`). They are the SAP's
decision: when the request and `ai/decisions.md` do not say them, ask the
person, or mark them to confirm.

```json
{
  "id": "An_TEAE_SER",
  "name": "Subjects with a serious treatment-emergent adverse event",
  "purpose": {"controlledTerm": "PRIMARY OUTCOME MEASURE"},
  "reason": {"controlledTerm": "SPECIFIED IN SAP"},
  "dataset": "ADAE",
  "variable": "USUBJID",
  "analysisSetId": "AnalysisSet_SAF",
  "dataSubsetId": "Dss_TEAE_SER",
  "orderedGroupings": [
    {"order": 1, "groupingId": "AnlsGrouping_TRT01A", "resultsByGroup": true}
  ],
  "methodId": "Mth_CountSubj"
}
```

## From the specs instead

When the study's ARD spec is there, R writes the reporting event from it,
with the same check: `tfl_ars(ard_spec, table_spec, report_spec)`, then
`tfl_write_ars_json(ars, path)` and `tfl_check_ars(path)`.
