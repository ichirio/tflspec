# A template with other defaults

A company's or a study's template is mostly its defaults: the variables
of its demographics table, the severity's levels, the records kept. This
registers one by them alone, no R body: `from`'s parameters, types and
body, with `defaults` in place of `from`'s. It is looked up as `from` is
when it runs, so a fix to `from` reaches it.

## Usage

``` r
tfl_template_variant(
  name,
  from,
  defaults = list(),
  label = NULL,
  example = NULL,
  ai = NULL,
  hide = NULL
)
```

## Arguments

- name:

  The variant's name. `from`'s own name replaces `from` (with a
  message).

- from:

  The template it is a variant of
  ([`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)).

- defaults:

  Its parameters' defaults, by name:
  `list(variables = c("AGE", "SEX"))`. Each is one of `from`'s
  parameters.

- label:

  Its label; `NULL`: `from`'s.

- example:

  The parameters
  [`tfl_test_templates()`](https://ichirio.github.io/tflspec/reference/tfl_test_templates.md)
  and its help page's example run it with; `NULL`: `from`'s.

- ai:

  `TRUE` to list it in the AI requests that answer with templates
  ([`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)`(templates = TRUE)`);
  `NULL`: `from`'s.

- hide:

  Parameters of `from` that are not the variant's arguments: left out of
  its help,
  [`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)
  and the AI's list, and a report's `template: args:` may not give them.
  They keep their defaults and the study standard's values
  (`tfl_param(study = )`), as the look of a figure's report type takes
  the figure style standard's; a report changes that with its own items
  (`+`) or `plot:`.

## Value

`name`, invisibly.

## Details

A report's YAML names it (`template: {name: demog_jp}`); its program
(the A use,
[`tfl_content_code()`](https://ichirio.github.io/tflspec/reference/tfl_content_code.md))
calls `from`'s function with the variant's defaults written out, so the
package that registers it exports nothing. A value comes from, first to
last: the report's arguments, the study standard (`study.yml`,
`defaults.yml`: `tfl_param(study = )`), the variant's `defaults`,
`from`'s.

Register it where it is used: a company's package's `.onLoad()`
([`tfl_use_company_templates()`](https://ichirio.github.io/tflspec/reference/tfl_use_company_templates.md)),
or a study's `spec/templates/*.R`.

## Examples

``` r
tfl_template_variant("demog_age_sex", from = "demog",
                     defaults = list(continuous = "AGE", categorical = "SEX"))
tfl_template_params("demog_age_sex")$categorical$default
#> [1] "SEX"
```
