# A study's requests, read again

The requests
[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)
/
[`tfl_ai_write_requests()`](https://ichirio.github.io/tflspec/reference/tfl_ai_write_requests.md)
wrote to `ai/requests`, as they returned them – for a screen opened
again, without preparing them anew.

## Usage

``` r
tfl_ai_requests(study_dir, requests = file.path("ai", "requests"))
```

## Arguments

- study_dir:

  The study folder.

- requests:

  The requests' folder, under `study_dir`.

## Value

A data frame, one row a request: `request`, `family`, `outputs` (the
report ids, comma-separated), `n`, `chars`, `file` (relative to
`study_dir`); no row when none were written.

## See also

[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)
for their answers.
