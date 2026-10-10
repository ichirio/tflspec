# One output's part of the study ARD

One output's part of the study ARD

## Usage

``` r
tfl_ard_for(ard, output_id)
```

## Arguments

- ard:

  The study ARD
  ([`tfl_build_ard()`](https://ichirio.github.io/tflspec/reference/tfl_build_ard.md)).

- output_id:

  The output.

## Value

Its rows, without the id columns: what
[`rtfreporter::normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.html)
takes.
