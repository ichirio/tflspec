# Start a study folder for drafted report files

Writes what every drafted report file stands on, so that the AI writes
only the reports: `study.yml` (the study's id and settings: the RTF
files to `output/tfl`, the programs to `programs/tfl`, as tflplanner
keeps them, so the folder opens there as a study), `spec/data.yml` (each
ADaM file as a dataset, and an analysis set for each population flag of
ADSL – `SAFFL`, `FASFL`, `ITTFL`, `PPROTFL` ... – as `<flag> == "Y"`)
and `spec/defaults.yml` (the page every report starts from: the running
header and footer, and the cell of a count). Each report file then says
`extends: [defaults]` and only what differs. Check the analysis sets'
ids and labels: the table prompt gives them to the AI.

## Usage

``` r
tfl_ai_study_init(
  dir,
  adam = "adam",
  study_id = "STUDY-01",
  title = NULL,
  overwrite = FALSE
)
```

## Arguments

- dir:

  The study folder (made if need be).

- adam:

  The ADaM folder, as the study's programs reach it from `dir`
  (relative, such as `"adam"`, or absolute); its `.xpt` (or `.sas7bdat`,
  `.rds`) files become the datasets.

- study_id, title:

  The study's id and title.

- overwrite:

  Write over files already there?

## Value

The paths written, invisibly.

## Examples

``` r
d <- file.path(tempdir(), "study-init")
dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
saveRDS(tflspec::adsl, file.path(d, "adam", "adsl.rds"))
tfl_ai_study_init(d, adam = "adam", study_id = "ONCO-01", overwrite = TRUE)
cat(readLines(file.path(d, "spec", "data.yml")), sep = "\n")
#> datasets:
#>   - dataset: ADSL
#>     level: ADaM
#>     path: adam/adsl.rds
#> populations:
#>   - id: ENRL
#>     dataset: ADSL
#>     where: !r ENRLFL == "Y"
#>     label: Enrolled Population
#>   - id: ITT
#>     dataset: ADSL
#>     where: !r ITTFL == "Y"
#>     label: Intent-To-Treat Population
#>   - id: SAF
#>     dataset: ADSL
#>     where: !r SAFFL == "Y"
#>     label: Safety Population
#>   - id: EFF
#>     dataset: ADSL
#>     where: !r EFFFL == "Y"
#>     label: Efficacy Evaluable Population
```
