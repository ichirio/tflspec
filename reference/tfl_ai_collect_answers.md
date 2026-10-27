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

A `tfl_ai_collected`: a list with `reports` (a data frame, one row a
report: `request`, `output_id`, `status` – `ok`, `warning`, `error`,
`no answer` –, `problems` (how many), `first` (the first problem),
`problems_text` (all of them, one a line), `file` (the report's
`spec/outputs/<id>.yml` when written, else `NA`), `repair` (the
request's repair prompt to paste into the same chat, else `NA`)),
`next_steps` (a request's next step, named by the request),
`assumptions` (the study's `spec/_assumptions.yml` for these reports:
`output_id`, `where`, `item`, `assumption`, `question`, `id` – the
assumption's `A<n>` – and `kind` – its `K<n>`,
[`tfl_ai_assumptions()`](https://ichirio.github.io/tflspec/reference/tfl_ai_assumptions.md)),
`stray` (answer files named after no request), `requests` (the requests'
folder), `templates` (each request's setting,
[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)'s
`templates`: a logical named by the request), `manifest` and
`manifest_file` (below).

The manifest is every report the answers say they make, for the person
to check the count against the TOC (one TOC row can be several reports):
each answer lists its reports first (`reports:`, one line a report:
`id`, `title`, `split` – how its TOC row is split for it –, `toc` – the
TOC row's number). A data frame, one row a report: `request`, `toc`,
`output_id`, `title`, `split`, `listed` (`FALSE`: a report the answer
writes but does not list; `NA`: a request not answered or not read, its
reports as asked) and `status` (the report's, as in `reports`, or
`"not answered"`: listed, but no report file in the answer). It is
written to `ai/manifest.csv` (UTF-8) beside the requests' folder, anew
at each collection. The paths are relative to `study_dir`. Its print is
a summary: each request's status, its reports and the next step.

## Details

An answer is read as UTF-8, with or without the byte-order mark Windows
Notepad writes, and with any line ends; an answer saved in another
encoding (Shift-JIS / CP932, "ANSI") is an `error` that says how to save
it again. An answer saved as `<request>.md.txt` or `<request>.txt`
(Notepad adds `.txt`) is read too.

## See also

[`tfl_ai_write_requests()`](https://ichirio.github.io/tflspec/reference/tfl_ai_write_requests.md)
