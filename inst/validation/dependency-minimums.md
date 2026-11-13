# tflspec: the minimum versions of its dependencies

Each minimum in `DESCRIPTION` is the first version of that package that has
every function and argument tflspec uses: in `R/`, in the tests that always
run, and in the code its SPECs and templates write (the ARD, table and
figure programs). For each package the table names what was found, the
version that first has it, and where that is recorded. The minimum is the
latest of those versions, raised to what a dependency needs in turn
(testthat needs ps and ggplot2 needs withr of a version).

**The two ends.** *The minimum environment* is R at the minimum
`DESCRIPTION` names (`Depends: R (>= 4.2)`) with every direct dependency at
exactly its minimum (the R-CMD-check job *minimum environment*). The
suggested packages with no minimum, and every indirect dependency, come
from CRAN as it was on **2026-06-18**: the Posit Package Manager snapshot of
that date (`https://packagemanager.posit.co/cran/__linux__/noble/2026-06-18`,
for Ubuntu 24.04). That date is the first snapshot that has the newest of
the minimums, siera 0.5.6 (on CRAN on 2026-06-17; the snapshot of
2026-06-17 still has 0.5.5). rtfreporter is installed from its `next`,
where its minimum is what `next` has. *The recommended environment* is the
latest R with the latest packages: the release jobs of the same workflow.
When a minimum is raised to a version released later, the date moves to the
first snapshot that has it.

**Required and optional.** rtfreporter and ggplot2 are in `Imports`; cards
and cardx stay in `Suggests` (a study that writes no ARD does not need
them), with the minimums below. A package only some SPECs write (the
survival, mixed-model and effect-size ones) stays in `Suggests` with no
minimum.

**The policy for a dependency that raises its own version.** When a
dependency raises its version and tflspec follows without breaking anyone
who has the older one, that is a minor version of ours; when the follow
cannot be backward compatible, it is a major version.

The rule for what sets a minimum: a use in `R/`, in a test that always runs,
or in the code the SPECs write does. These do not: a use only in a test
that skips when the version is older, and a catalogue entry the user may
pick (a function of a later version is listed, not called; the catalogue
`inst/ard/functions.csv` names the version).

| Package | Minimum | What needs it (function or argument -> version) | Source |
|---|---|---|---|
| R (`Depends`) | 4.2 | cardx 0.3.1 (below) depends on R (>= 4.2), and the minimum environment installs the suggested packages. What tflspec itself uses needs less: the native pipe and `\(x)` -> 4.1.0; cards 0.8.0 needs R (>= 4.1). `car`, whose dependencies (quantreg, MatrixModels, Matrix) need a newer R, is left out of that job, as it is from the R 4.2 row. | cardx 0.3.1 `DESCRIPTION`; R NEWS 4.1.0 |
| grDevices, stats, utils (`Imports`) | none | They ship with R, so their version is R's. | -- |
| ggplot2 | 3.5.0 | `coord_radial()` (the polar figures) -> 3.5.0, the first of the two ggplot2 lines the figure designs are written for (3.5.x and 4.0.x, `inst/fig/ggplot2_compat.csv`); `standardise_aes_names()`, `get_element_tree()` -> 3.3.0; `geom_bin_2d()` -> 3.3.4; `expansion()` -> 3.3.0; `geom_col()` -> 2.2.0. Not counted: `coord_transform()` -> 4.0.0 (the compat test's row for it holds on either line). | ggplot2 NEWS 3.5.0; `NAMESPACE` of each release |
| jsonlite | 1.0 | `toJSON(auto_unbox = , null = , na = , digits = , pretty = )`, `fromJSON(simplifyVector = , simplifyDataFrame = , simplifyMatrix = )` -> 1.0, the first release with all of them. | `NAMESPACE` of each release (the arguments are in the function's help of 1.0) |
| openxlsx | 4.0.0 | `createNamedRegion()`, `dataValidation()` (the workbooks tflspec writes) -> 4.0.0; `writeData(keepNA = , headerStyle = )`, `writeComment()`, `createComment()`, `getSheetNames()`, `int2col()` -> 3.0.0; `addFilter()` -> 2.2.1. | `NAMESPACE` of each release; openxlsx NEWS 3.0.1 (`keepNA`) |
| readxl | 1.2.0 | `read_excel(.name_repair = )` -> 1.2.0, with a current tibble (1.0.0 calls `tibble::as_tibble(validate = )`, which tibble has made defunct; the same as rtfreporter's minimum); `read_excel(range = , trim_ws = , skip = )` -> 1.0.0; `excel_sheets()` -> 0.1.0. | readxl NEWS 1.2.0; rtfreporter `dependency-minimums.md` |
| rlang | none | `hash()`, `enquo()`, `enquos()`, `parse_expr()`, `eval_tidy()`, `quo_is_null()`, `as_name()` -> 0.4.10 or earlier. ggplot2 3.5.0 needs rlang (>= 1.1.0), which is what is installed. | rlang NEWS 0.4.10; ggplot2 3.5.0 `DESCRIPTION` |
| rtfreporter | 0.8.2.9100 | What its `next` has: tflspec and rtfreporter are released together, so the minimum is the version of rtfreporter's `next` when this table was made. In the minimum environment it is installed from `next` (`Remotes`). | rtfreporter `next`, `DESCRIPTION` |
| yaml | 2.1.15 | `read_yaml()`, `write_yaml()` -> 2.1.15; `as.yaml(handlers = , precision = , indent.mapping.sequence = , column.major = , line.sep = )`, `yaml.load(handlers = )` -> before 2.1.15. | yaml NEWS 2.1.15 ("Added `read_yaml` and `write_yaml`") |
| cards | 0.8.0 | `ard_tabulate_rows()` (in `R/` and in the code the templates write) -> 0.8.0; `ard_tabulate()`, `ard_summary()`, `ard_tabulate_value()`, `ard_mvsummary()` and the `ADLB` data -> 0.7.0; the others -> 0.1.0 to 0.6.1. Not counted: `diff_ard_hierarchical()` -> 0.9.0, which the catalogue lists for the user to pick (the tests that need it check for it). Unchanged. | cards NEWS 0.7.0, 0.8.0; `NAMESPACE` of each release |
| cardx | 0.3.1 | `ard_emmeans_emmeans()`, `ard_emmeans_contrast()` (in `R/` and in the templates' model rows) -> 0.3.1; `ard_tabulate_max()`, `ard_tabulate_abnormal()` -> 0.3.0; `ard_incidence_rate()` -> 0.2.5; `ard_stats_mantelhaen_test()` -> 0.2.4; `ard_stats_poisson_test()` -> 0.2.1; `ard_regression()`, `ard_categorical_ci()`, `ard_survival_survfit()` and the other `ard_stats_*()` -> 0.2.0 or earlier. Unchanged. | cardx NEWS 0.3.1, 0.3.0, 0.2.5; `NAMESPACE` of each release |
| siera | 0.5.6 | `tfl_ars_ard()` (`R/ars_run.R`) runs siera's programmes against the ADaM written as CSV, and what it relies on is siera 0.5.6's (the minimum set when it was written, NEWS #55); `readARS()` alone -> 0.1.0. The round-trip tests also skip below 0.5.6. Unchanged. | tflspec NEWS (the siera profile, #55); `R/ars_run.R` |
| testthat | 3.1.7 | `local_mocked_bindings()` -> 3.1.7; `expect_no_error()`, `expect_no_warning()`, `expect_no_message()` -> 3.1.5; `expect_no_match()` -> 3.0.3; `expect_snapshot()` and the third edition -> 3.0.0. | testthat NEWS 3.1.7, 3.1.5, 3.0.3, 3.0.0 |
| withr | 2.5.0 | `local_tempdir()` (92 uses in the tests) -> 2.3.0; `local_tempfile()`, `local_pdf()` -> 2.1.0; `local_options()`, `local_envvar()`, `local_dir()`, `defer()` -> 2.0.0. ggplot2 3.5.0 needs withr (>= 2.5.0). | `NAMESPACE` of each release; ggplot2 3.5.0 `DESCRIPTION` |
| dplyr | none | `cross_join()`, `where()` -> 1.1.0; `across()`, `any_of()`, `slice_max()` -> 1.0.0. The minimum cards (0.8.0) needs dplyr (>= 1.1.4), which is what is installed. | `NAMESPACE` of each release; cards 0.8.0 `DESCRIPTION` |
| tibble, tidyr | none | `tribble()` -> 1.2; `tibble()` -> 1.0; cards 0.8.0 needs tidyr (>= 1.3.1), and dplyr 1.1.4 needs tibble (>= 3.2.0). | cards 0.8.0 and dplyr 1.1.4 `DESCRIPTION` |
| ggsurvfit | none, 1.1.0 in the minimum environment | The KM figure (`ggsurvfit::ggsurvfit()`, `survfit2()`) -> 0.1.0. ggsurvfit 1.2.0 needs ggplot2 >= 4.0.0, so the minimum environment installs 1.1.0, as the ggplot2 3.5.x row does. | ggsurvfit `DESCRIPTION` |
| ggh4x, ggrepel | none, left out of the minimum environment | The facet and label geoms of some figure designs (`help_secondary()`, `facetted_pos_scales()`, `geom_text_repel()` -> 0.2.0 to 0.3). The newest of each at the snapshot needs ggplot2 >= 3.5.2 (ggrepel also withr >= 3.0.2), more than the minimum, so they cannot be installed with it, and the tests that need them skip. | ggh4x and ggrepel `DESCRIPTION` |
| the other suggested packages (arrow, broom, broom.helpers, car, effectsize, emmeans, ggnewscale, ggtext, haven, jsonvalidate, magrittr, mmrm, officer, parameters, patchwork, pdftools, rstudioapi, scales, smd, survival, writexl, xml2) | none | Used by the SPECs that name them (a model, a figure geom, a file kind) and by tests that skip when one is missing; each comes from the snapshot, as a version old enough to be a minimum could not be built from source without a system library (arrow, pdftools) or would need a long chain of older packages. `car` is left out, as it is from the R 4.2 row: its dependencies (quantreg, MatrixModels, Matrix) need a newer R. | -- |

## Changes

The minimums before this table was made, and why each changed: R 4.1 ->
4.2 (cardx 0.3.1, which the minimum environment installs); testthat 3.0.0
-> 3.1.7 (`local_mocked_bindings()`); withr (none -> 2.5.0) follows
ggplot2 3.5.0; jsonlite, openxlsx, readxl and yaml had no minimum and now
have the one in the table; ggplot2, cards, cardx, siera and rtfreporter are
unchanged.
