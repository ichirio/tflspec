# Write a company's template package to start from

A company's templates live in an R package of its own (decided
2026-10-17). This writes one to start from, every file meant to be read
and changed: for each template chosen, one
[`tfl_template_variant()`](https://ichirio.github.io/tflspec/reference/tfl_template_variant.md)
call – tflspec's template with the company's defaults, so tflspec's
fixes reach it – registered when the package is loaded. It exports
nothing: a report's program calls tflspec's `tpl_<template>()` with the
company's defaults written out.

## Usage

``` r
tfl_use_company_templates(
  path,
  package = basename(path),
  templates = c("demog", "ae_soc_pt"),
  prefix = NULL
)
```

## Arguments

- path:

  The package's folder (made; it must be empty or new).

- package:

  The package's name; the folder's name by default.

- templates:

  tflspec templates to wrap
  ([`tfl_templates()`](https://ichirio.github.io/tflspec/reference/tfl_templates.md)).

- prefix:

  The wrapped templates' names are `<prefix>_<template>` (`acme_demog`);
  `NULL`: `package`. `""`: the same names, replacing tflspec's when the
  package is loaded.

## Value

The files written, invisibly.

## Details

The files: `DESCRIPTION`, `NAMESPACE`, `LICENSE`, `R/templates.R` (the
templates), `R/zzz.R` (their registration), `tests/` (that each is
registered, takes no look as a parameter and expands),
`inst/standards/defaults.yml` (the company standard a new study's
standard is copied from, the form of a study's `spec/defaults.yml`) and
`README.md`.

## Examples

``` r
d <- file.path(tempdir(), "acmetpl")
f <- tfl_use_company_templates(d, templates = c("demog", "ae_soc_pt"))
basename(f)
#> [1] "DESCRIPTION"      "LICENSE"          "NAMESPACE"        "templates.R"     
#> [5] "zzz.R"            "testthat.R"       "test-templates.R" "defaults.yml"    
#> [9] "README.md"       
unlink(d, recursive = TRUE)
```
