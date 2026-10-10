# How the generated programs name folders and packages

Two options make the code tflspec writes fit a study's setup file (such
as tflplanner's `programs/study_setup.R`), which defines the study's
folders as variables and attaches the packages every program uses.

## Details

- `tflspec.paths`: a named character vector, a variable for a folder,
  relative to the study folder –
  `c(path_adam = "data/adam", path_ard = "output/ard")`. A file under
  one of them is written through the variable:
  `readRDS(file.path(path_adam, "adsl.rds"))` instead of
  `readRDS("data/adam/adsl.rds")`. The folder that holds it most closely
  is the one used.

- `tflspec.attached`: the packages the setup attaches with
  [`library()`](https://rdrr.io/r/base/library.html) –
  `c("cards", "dplyr")`. Their functions are called without `pkg::` in
  the program, the code of the spec it carries included (only the code:
  a string or a comment keeps what it says), and an ARD program does not
  write [`library(cards)`](https://github.com/pharmaverse/cards) itself.
  A package not listed keeps its prefix (`cardx::`).

Unset (the default), the code is as it has always been: literal paths
and `pkg::` on every call. The functions that write code read them:
[`tfl_ard_code()`](https://ichirio.github.io/tflspec/reference/tfl_ard_code.md),
[`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md),
[`tfl_table_code()`](https://ichirio.github.io/tflspec/reference/tfl_table_code.md),
[`tfl_report_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_code.md),
[`tfl_report_setup_code()`](https://ichirio.github.io/tflspec/reference/tfl_report_setup_code.md),
[`tfl_listing_code()`](https://ichirio.github.io/tflspec/reference/tfl_listing_code.md),
[`tfl_read_data_code()`](https://ichirio.github.io/tflspec/reference/tfl_read_data_code.md),
[`tfl_fig_design_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_design.md)
and
[`tfl_fig_setup_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_setup_code.md).

## Examples

``` r
sp <- tfl_ard_spec(list(
  study = data.frame(key = "id", value = "USUBJID"),
  datasets = data.frame(dataset = "ADSL", path = "data/adam/adsl.rds"),
  populations = data.frame(population_id = "SAF", dataset = "ADSL",
                           where = "SAFFL == \"Y\""),
  analyses = data.frame(output_id = "T1", analysis_id = "AGE",
                        method = "continuous", population_id = "SAF",
                        by = "TRT01A", variables = "AGE")))
old <- options(tflspec.paths = c(path_adam = "data/adam"),
               tflspec.attached = "cards")
cat(tfl_ard_code(sp, part = "body"), sep = "\n")
#> report_id <- "T1"
#> 
#> # ---- data ----
#> adsl <- readRDS(file.path(path_adam, "adsl.rds"))
#> 
#> # ---- populations ----
#> pop_saf <- filter(adsl, SAFFL == "Y")
#> 
#> # ---- analyses ----
#> ard_age <- pop_saf |>
#>   ard_summary(
#>     by = TRT01A,
#>     variables = AGE,
#>     fmt_fun = everything() ~ fmt_default
#>   ) |>
#>   apply_fmt_fun() |>
#>   tag_ard(report_id, "AGE", population = "SAF")
#> 
#> # the report's rows of the study ARD, with the fingerprint of its
#> # definition (tfl_ard_spec_hash())
#> ard_age |>
#>   save_ard(report_id, definition = "4313d43839155da7a4859d80620217ee")
options(old)
```
