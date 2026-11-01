# ae_overview: the template ae_overview

The table template `ae_overview`, layout `summary`. Its program is one
call, `tfl_tpl(ard, "ae_overview", ...)`; a parameter a report leaves
out takes the study standard's value, else the variant's default, else
the layout's.

## Parameters

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Parameter | Tier | Stage | Default | Maps to | What |
| `population` | other | call | SAF | output.population | the analysis set's id |
| `subset` | other | call |  |  | which subjects (R, on the analysis set's ADSL): PART == "Part 1" |
| `by` | other | call | TRT01A | settings.treatment | the column variable |
| `data` | other | call | ADAE |  | the dataset of events |
| `where` | other | call | TRTEMFL == "Y" |  | the events kept (R): the treatment-emergent |
| `data_id` | other | call |  |  | the events' analysis data's id; NULL: \_ |
| `flags` | other | call | (id = ANY; label = Any TEAE); (id = REL; label = Any trea... |  | the rows, in order: each 'id' (the flag's name), 'label' (the row's text) and 'where' (R: the events it counts; none: every event kept) |
| `heading` | other | call | Subjects with |  | the label column's heading |

## Aliases

The same template by the names of other collections' items: each runs it
as itself (`tfl_tpl(x, "stf_t04_populations", ...)`).

|                       |               |          |                      |
|-----------------------|---------------|----------|----------------------|
| Alias                 | Collection    | Item     | Title                |
| `stf_t04_populations` | FDA STF IG v2 | T04      | Analysis populations |
| `ars_ae_summary`      | CDISC ARS     | 14-3-1-1 | TEAE overview        |

Sources:

- FDA STF IG v2: U.S. Food and Drug Administration, CDER. Standard
  Safety Tables and Figures: Integrated Guide, Version 2.0, April 2025.
  <https://www.fda.gov/media/187065/download> (accessed 2026-10-28). A
  U.S. Government work; FDA's website content is in the public domain
  unless noted
  (https://www.fda.gov/about-fda/about-website/website-policies).

- CDISC ARS: Clinical Data Interchange Standards Consortium (CDISC).
  Analysis Results Standard, v1.0, 19 April 2024.
  <https://www.cdisc.org/standards/foundational/analysis-results-standard>
  (accessed 2026-10-28). Copyright CDISC. Its examples:
  github.com/cdisc-org/analysis-results-standard (content CC BY 4.0,
  code MIT); cited by number only. CDISC is a trademark of the Clinical
  Data Interchange Standards Consortium.

The aliases follow the numbering of these publications, and the titles
are short descriptions of our own, not their text. The templates are
tflspec's, not endorsed or recommended by FDA or CDISC.

## Example

    plan <- tfl_tpl(ard, "ae_overview", population = "SAF")

## See also

[`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md),
[`tfl_tpl_help()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_help.md),
[`tfl_tpl_args()`](https://ichirio.github.io/tflspec/reference/tfl_tpl_args.md),
[`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)

## Examples

``` r
tfl_tpl_help("ae_overview", tier = c("required", "usual"))
#> ae_overview (table, layout summary; tflspec)
#> Call: tfl_tpl(ard, "ae_overview", ...)
```
