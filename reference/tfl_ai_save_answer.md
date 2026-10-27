# Save a chat's answer to a request where tfl_ai_collect_answers() reads it

Writes the answer (the chat's whole reply, as copied) to
`ai/answers/<request>.md` in UTF-8 – the name and the encoding
[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md)
reads – for a program or a screen that takes the pasted text, rather
than the person saving a file.

## Usage

``` r
tfl_ai_save_answer(
  study_dir,
  request,
  text,
  overwrite = TRUE,
  requests = file.path("ai", "requests"),
  answers = file.path("ai", "answers")
)
```

## Arguments

- study_dir:

  The study folder.

- request:

  The request's name (`ae-1`: a row of the requests' `request`,
  `ai/requests/index.md`).

- text:

  The answer: one string, or lines.

- overwrite:

  `TRUE` (default) to write over an answer saved before (a repaired
  answer replaces the first).

- requests, answers:

  The folders, under `study_dir`.

## Value

The file's path, invisibly.

## See also

[`tfl_ai_collect_answers()`](https://ichirio.github.io/tflspec/reference/tfl_ai_collect_answers.md),
[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md).
