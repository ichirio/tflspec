# Study XYZ-ONC-01: an example study folder

The tables and figures of tflspec's oncology sample study (Drug X vs
Control; the datasets `adsl` ... `adtte`, see `?adsl`, and the study's SAP
in `system.file("sample", "onco", package = "tflspec")`), made the way a
study makes them: specs in Excel, one R program written from each, the
programs run.

| File | What it is |
|---|---|
| `prepare_data.R` | the analysis data in `data/adam/`, with the few figure variables the ADaM do not carry |
| `spec/ard_spec.xlsx` | the analyses: populations, analysis data, one cards / cardx call a row |
| `spec/tables.xlsx` | how each table is laid out (rtfreporter plan) |
| `spec/report.xlsx` | the page around it: header, footer, titles, footnotes |
| `spec/plot_list.xlsx` | the figures, one row each |
| `run_all.R` | writes the programs into `programs/` and runs them into `output/` |
| `make_specs.R` | how the workbooks were first written (run once; edit the workbooks after) |

The outputs:

| Output | Title | Population |
|---|---|---|
| T-14-1-1 | Demographic and Baseline Characteristics | Safety |
| T-14-2-1 | Best Overall Response, Objective Response Rate and Disease Control Rate | ITT |
| T-14-3-1 | Treatment-Emergent Adverse Events by System Organ Class and Preferred Term | Safety |
| F-14-2-1 | Kaplan-Meier Plot of Progression-Free Survival | ITT |
| F-14-2-2 | Kaplan-Meier Plot of Overall Survival | ITT |
| F-14-2-3 | Progression-Free Survival: Hazard Ratio by Subgroup | ITT |
| F-14-2-4 | Best Percent Change from Baseline in Sum of Diameters (waterfall) | Efficacy evaluable |
| F-14-2-5 | Time on Treatment and Response, Confirmed Responders (swimmer) | Responders |
| F-14-2-6 | Percent Change from Baseline in Sum of Diameters over Time (spider) | Efficacy evaluable |

To run it, copy the folder and run `run_all.R` from it:

```r
file.copy(system.file("examples", "onco", package = "tflspec"), ".", recursive = TRUE)
setwd("onco")
source("run_all.R")
```

The programs in `programs/` need cards, cardx, rtfreporter, dplyr,
ggplot2, ggsurvfit, patchwork and survival, not tflspec.
