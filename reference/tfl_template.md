# Register a template

A template's definition: its kind, the layout it is built on, its few
parameters
([`tfl_param()`](https://ichirio.github.io/tflspec/reference/tfl_param.md))
and its body, a function of the parameters' values and the report's
context that returns the report's items. A company's package calls it
when it is loaded (`.onLoad()`), and so does a study's
`spec/templates/<name>.R`. The usual body is a tflspec template's with
the company's defaults
([`tfl_template_params()`](https://ichirio.github.io/tflspec/reference/tfl_template_params.md)
to start from,
[`tfl_template_expand()`](https://ichirio.github.io/tflspec/reference/tfl_template_expand.md)
in the body); see
[`tfl_use_company_templates()`](https://ichirio.github.io/tflspec/reference/tfl_use_company_templates.md).

## Usage

``` r
tfl_template(
  name,
  kind = c("table", "figure", "listing"),
  layout,
  params,
  body,
  label = NULL,
  call = NULL,
  example = NULL,
  ai = FALSE
)
```

## Arguments

- name:

  The template's name (`"acme_demog"`).

- kind:

  `"table"`, `"figure"` or `"listing"`.

- layout:

  The layout it is built on (`"summary"`, `"hierarchical"`, ...), or its
  own name for a layout of its own.

- params:

  Its parameters, a list of
  [`tfl_param()`](https://ichirio.github.io/tflspec/reference/tfl_param.md).

- body:

  `function(a, ctx)`: `a` the parameters' values by name (the report's,
  else their defaults), `ctx` the report's `codelists` and the study's
  `population` (its analysis sets); returns
  `list(ard = , data = , content = )`, `content` a list of
  [`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md).
  A figure's body returns its design (`data` its steps, `content` its
  layers, `plot` its settings) and `call_steps`: how many of the data's
  last steps (the statistics, the fit) its call makes from the data the
  steps before them make.

- label:

  What it makes, in a few words; `NULL` none.

- call:

  The function a report's program calls for it (the A use); `NULL`:
  [`tfl_tpl()`](https://ichirio.github.io/tflspec/reference/tfl_tpl.md)
  for a table or a listing (`tfl_tpl(ard, "<name>", ...)`), `tpl_<name>`
  for a figure.

- example:

  Its parameters for a try (a named list): what
  [`tfl_test_templates()`](https://ichirio.github.io/tflspec/reference/tfl_test_templates.md)
  runs it with, beside the defaults; `NULL` none.

- ai:

  `TRUE` to list it in the AI requests that answer with templates
  ([`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)`(templates = TRUE)`).

## Value

`name`, invisibly.

## Details

A name already registered is replaced, with a message (a company's
`demog` over tflspec's: the package loaded later wins). A parameter that
ends in the look – the rows a page, column widths, the digits, the blank
rows, the style – is warned about: the study's styles and defaults say
those, and a template parameter would beat them
([`tfl_test_templates()`](https://ichirio.github.io/tflspec/reference/tfl_test_templates.md)
finds them).

## Examples

``` r
tfl_template("my_ages", kind = "table", layout = "summary",
  params = list(tfl_param("population", "SAF", doc = "the analysis set's id")),
  body = function(a, ctx) tfl_template_expand("summary",
    list(population = a$population, continuous = "AGE"),
    codelists = ctx$codelists, population = ctx$population))
tfl_templates()[, c("name", "package")]
#>              name   package
#> 1  ae_disc_soc_pt   tflspec
#> 2         ae_eair   tflspec
#> 3    ae_max_grade   tflspec
#> 4     ae_overview   tflspec
#> 5      ae_pt_freq   tflspec
#> 6   ae_ser_soc_pt   tflspec
#> 7       ae_soc_pt   tflspec
#> 8          ancova   tflspec
#> 9           demog   tflspec
#> 10    disposition   tflspec
#> 11      forest_hr   tflspec
#> 12   hierarchical   tflspec
#> 13             km   tflspec
#> 14  km_cumulative   tflspec
#> 15    km_survival   tflspec
#> 16       km_table   tflspec
#> 17   lab_abnormal   tflspec
#> 18      lab_shift   tflspec
#> 19        listing   tflspec
#> 20        mean_ci   tflspec
#> 21        mean_se   tflspec
#> 22        my_ages (session)
#> 23            orr   tflspec
#> 24   param_change   tflspec
#> 25          shift   tflspec
#> 26     stat_block   tflspec
#> 27  stf_t02_demog   tflspec
#> 28        summary   tflspec
#> 29      waterfall   tflspec
```
