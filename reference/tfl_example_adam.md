# Example ADaM-like data for trying tflspec

Synthetic oncology data (no real subjects): `ADSL`, `ADTTE` (OS / PFS /
DOR, days), `ADRS` (OVR per visit + BOR) and `ADTR` (best percent change
in sum of diameters; `SDIAM` over time), `ADLOT` (lines of therapy),
`ADAE`, `ADLB` (ALT / AST / BILI) and `ADPC` (concentrations).

## Usage

``` r
tfl_example_adam(n = 40, seed = 1)
```

## Arguments

- n:

  Number of subjects.

- seed:

  Random seed.

## Value

A named list of data frames.
