An example: a demographics table as its template (`spec/outputs/T-14-1-1.yml`), and the adverse events by SOC / PT with one argument that differs (`spec/outputs/T-14-3-1.yml`) -- one file a report.

```yaml
id: T-14-1-1
kind: table
label: "Demographic and Baseline Characteristics"
population: SAF
extends: [defaults]
template:
  name: demog
  args: {categorical: [AGEGR1, SEX, RACE]}
page:
  titles:
    "1": {center: "Table 14.1.1"}
    "2": {center: "Demographic and Baseline Characteristics"}
```

```yaml
id: T-14-3-1
kind: table
label: "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term"
population: SAF
extends: [defaults]
template:
  name: ae_soc_pt
  args: {any_row_label: "Subjects with at least one TEAE"}
page:
  titles:
    "1": {center: "Table 14.3.1"}
ai:
  assumptions:
    - {where: template, item: ae_soc_pt, assumption: "the standard SOC / PT table: the shell has the same rows and columns", value: ae_soc_pt}
```
