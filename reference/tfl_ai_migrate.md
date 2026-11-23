# Put a study's AI drafts apart from its own report files

Makes `ai/spec/outputs/`, the AI's drafts' folder (see
[`tfl_ai_adopt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adopt.md)),
and copies there the report files the AI wrote – those its answers name
(`ai/manifest.csv`, `spec/_assumptions.yml`) – so that later rounds
redraft them there. The study's own `spec/outputs/` keep them, recorded
as adopted (they were the study's), so nothing a person changed is lost.
From then on the collection and `run.R` write the drafts apart, and
`run.R` builds them into `ai/output/`. A study already apart is left as
it is.

## Usage

``` r
tfl_ai_migrate(study_dir)
```

## Arguments

- study_dir:

  The study folder.

## Value

The ids of the drafts copied apart, invisibly.

## See also

[`tfl_ai_adopt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adopt.md)
