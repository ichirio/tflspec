# A template parameter

One parameter of a template
([`tfl_template()`](https://ichirio.github.io/tflspec/reference/tfl_template.md)):
its name, default and what it is. A template has few, and none of them
is the look (a font, a width, the rows a page): the study's styles and
defaults say that.

## Usage

``` r
tfl_param(
  name,
  default = NULL,
  required = FALSE,
  doc = NULL,
  study = NULL,
  type = NULL,
  multiple = NULL,
  of = NULL,
  choices = NULL,
  fields = NULL,
  group = NULL,
  tier = NULL,
  stage = "call"
)
```

## Arguments

- name:

  The parameter's name.

- default:

  Its default value; `NULL` for none.

- required:

  `TRUE` when a report must give it.

- doc:

  One line saying what it is.

- study:

  Where the study standard's value of it is, taken when the report's
  template SPEC does not give it: `"output.population"` (the report's
  `population:`), `"settings.treatment"` (`study.yml`'s
  `settings: treatment:`), or `"<verb>.<argument>"`
  (`"plan_cells.continuous"`: that argument of the study's defaults (and
  pattern) item). `NULL`: tflspec's for its name (`population`, `by`,
  `group`), else the template's own default only.

- type:

  What its value is, for an input form: `"vars"` (variable names),
  `"levels"` (values of a variable), `"flag"` (`TRUE` / `FALSE`),
  `"number"`, `"text"` (also an R condition, an id, a label), `"choice"`
  (one of `choices`) or `"list"` (a named vector or a list of records,
  their `fields`). `NULL`: tflspec's type for the name (`by` is
  `"vars"`, `where` `"text"` ...), else one read from the default.

- multiple:

  `TRUE` when it takes several values (`hierarchy`), `FALSE` one (`by`);
  `NULL`: as `type`.

- of:

  For `type = "levels"`: whose values they are. A name of the template's
  parameters first (the variable it names: `by_levels`, `of = "by"`),
  else a variable's name (`params`, `of = "PARAMCD"`).

- choices:

  For `type = "choice"`: the values it may take.

- fields:

  For `type = "list"`: a record's fields, `c("vars", "label", "width")`;
  a named vector's are `c("name", "text")`.

- group:

  The input form's group it is shown in: `"target"` (shown first) for
  what the report is of – `population`, `subset` (which subjects: a
  condition on the analysis set's ADSL, narrowing the denominators, the
  header N and, through them, the records) and `where` (which records of
  the records data); `NULL`: tflspec's for its name, else none.

- tier:

  How often it is set, for a help page and an input form: `"required"`,
  `"usual"` (shown with its default) or `"other"` (folded away); `NULL`:
  `"required"` when `required`, else `"other"`.

- stage:

  Where its value is used: `"call"` (the default), an argument of the
  template's call in a report's program, or `"data"`, a step that
  prepares the analysis data (a figure's records, its analysis set),
  written in the program's data steps, not in the call.

## Value

A `tfl_param`.

## Examples

``` r
tfl_param("population", "SAF", doc = "the analysis set's id")
#> $name
#> [1] "population"
#> 
#> $default
#> [1] "SAF"
#> 
#> $required
#> [1] FALSE
#> 
#> $doc
#> [1] "the analysis set's id"
#> 
#> $study
#> NULL
#> 
#> $type
#> NULL
#> 
#> $multiple
#> NULL
#> 
#> $of
#> NULL
#> 
#> $choices
#> NULL
#> 
#> $fields
#> NULL
#> 
#> $group
#> NULL
#> 
#> $tier
#> NULL
#> 
#> $stage
#> [1] "call"
#> 
#> attr(,"class")
#> [1] "tfl_param"
tfl_param("layout", "cols", type = "choice", choices = c("cols", "rows"))
#> $name
#> [1] "layout"
#> 
#> $default
#> [1] "cols"
#> 
#> $required
#> [1] FALSE
#> 
#> $doc
#> NULL
#> 
#> $study
#> NULL
#> 
#> $type
#> [1] "choice"
#> 
#> $multiple
#> NULL
#> 
#> $of
#> NULL
#> 
#> $choices
#> [1] "cols" "rows"
#> 
#> $fields
#> NULL
#> 
#> $group
#> NULL
#> 
#> $tier
#> NULL
#> 
#> $stage
#> [1] "call"
#> 
#> attr(,"class")
#> [1] "tfl_param"
```
