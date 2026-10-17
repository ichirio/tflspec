# Read the answers of a study's requests, check them, write the report files

Reads each `<request>.md` in `answers` (the AI's whole answer to the
request of the same name,
[`tfl_ai_write_requests()`](https://ichirio.github.io/tflspec/reference/tfl_ai_write_requests.md)),
checks it against its request
([`tfl_ai_parse()`](https://ichirio.github.io/tflspec/reference/tfl_ai_parse.md),
[`tfl_ai_check()`](https://ichirio.github.io/tflspec/reference/tfl_ai_check.md))
and, when it has no error, writes its report files
([`tfl_ai_unpack()`](https://ichirio.github.io/tflspec/reference/tfl_ai_unpack.md)).
An answer with errors writes no file: its repair prompt
([`tfl_ai_repair_prompt()`](https://ichirio.github.io/tflspec/reference/tfl_ai_repair_prompt.md))
goes to `repairs`, to paste into the same chat; save the new answer over
the old and collect again.

## Usage

``` r
tfl_ai_collect_answers(
  study_dir,
  requests = file.path("ai", "requests"),
  answers = file.path("ai", "answers"),
  repairs = file.path("ai", "repairs"),
  overwrite = TRUE
)
```

## Arguments

- study_dir:

  The study folder.

- requests, answers, repairs:

  The folders, under `study_dir`.

- overwrite:

  `TRUE` (default) to write over report files already there: an answer
  collected again replaces its reports.

## Value

A data frame, one row a report: `request`, `output_id`, `status` (`ok`,
`warning`, `error`, `no answer`), `problems` (how many), `first` (the
first problem). Its print is a summary.

## See also

[`tfl_ai_write_requests()`](https://ichirio.github.io/tflspec/reference/tfl_ai_write_requests.md)
