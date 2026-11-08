# One content item: its code, its written arguments, its own part

- `tfl_item_code()`: the item's one call, as
  [`tfl_content_code()`](https://ichirio.github.io/tflspec/reference/tfl_content_code.md)
  writes it (no `plan <-`, no pipe).

- `tfl_item_written()`: the names of the arguments its code writes – the
  arguments given, an `!unset` or `NULL` one left out; a verb's
  positional `...` (`plan_titles("A", "B")`) is `"..."`.

- `tfl_item_own()`: what the report's own layer holds of the item, given
  the item `below` it (the layers' without the report's own,
  [`tfl_resolve_output()`](https://ichirio.github.io/tflspec/reference/tfl_resolve_output.md)'s
  `provenance = TRUE`). A verb whose `...` is named (`plan_cells()` ...)
  keeps only the keys whose value differs from below's, and a key below
  has that the item has not is `!unset`; any other verb is the item
  whole, or `NULL` when it is the same as below's (the own item can go).
  Values are compared as their R text, so `30` and `30L` are the same.

## Usage

``` r
tfl_item_code(item, pipe = NULL)

tfl_item_written(item)

tfl_item_own(item, below = NULL)
```

## Arguments

- item:

  A
  [`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md).

- pipe:

  Unused: the call alone (kept for the code's form to come).

- below:

  The item below it, or `NULL` (no layer has the verb: the item is all
  the report's own).

## Value

`tfl_item_code()`: the code, one element a line. `tfl_item_written()`:
the argument names. `tfl_item_own()`: a
[`tfl_item()`](https://ichirio.github.io/tflspec/reference/tfl_r.md) or
`NULL`.

## Examples

``` r
it <- tfl_item("plan_cells", list(continuous = list(n = "{N}"), default = "{n} ({p}%)"))
tfl_item_code(it)
#> [1] "plan_cells(continuous = list(n = \"{N}\"), default = \"{n} ({p}%)\")"
tfl_item_written(it)
#> [1] "continuous" "default"   
below <- tfl_item("plan_cells", list(default = "{n} ({p}%)"), from = "defaults")
tfl_item_own(it, below)$args
#> $continuous
#> $continuous$n
#> [1] "{N}"
#> 
#> 
```
