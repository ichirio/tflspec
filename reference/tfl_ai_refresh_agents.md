# Write the agent's instructions again from the study's settings

After study.yml's `ai:` keys change – review, language, ... – writes the
agent's instructions again from the study's settings and the inputs
[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)
recorded (`ai/agent_inputs.yml`); the drafts, requests and decisions are
not touched.

## Usage

``` r
tfl_ai_refresh_agents(study_dir = ".")
```

## Arguments

- study_dir:

  The study folder (default: the working directory).

## Value

The files written, invisibly.

## See also

[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)
