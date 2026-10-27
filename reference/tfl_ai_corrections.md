# Write the person's corrections for the agent's next round

Gathers what the person wants changed into one file,
`ai/corrections.md`, which the agent works through in its next round
(round 2 of its instructions, `AGENTS.md`): from the person's list of
reports
([`tfl_ai_expected_write()`](https://ichirio.github.io/tflspec/reference/tfl_ai_expected_write.md))
each report to `add`, `drop` or `change` and each `note` on what a
report should show, and each question of `ai/questions.md` the person
has answered (still open). One section an item – `## C<n>. add ...`,
`## Q<n> (answered). ...` – with `- Status: open`, which the agent sets
to `done` once it has done it.

## Usage

``` r
tfl_ai_corrections(study_dir = ".")
```

## Arguments

- study_dir:

  The study folder.

## Value

The sections written this time (their headings), invisibly; the file is
`ai/corrections.md`.

## Details

Each assumption of the AI the person answered `wrong`
([`tfl_ai_assumption_answers_write()`](https://ichirio.github.io/tflspec/reference/tfl_ai_assumption_answers_read.md))
is a section too: one report's
(`## A12. assumption of T-14-1-1: <item>`, with `- Was:` and
`- Correct:`) or every report of its kind's
(`## K3. assumption of every report: <item>`, with `- Reports:` too). An
answer `ok` needs no section. An answer changed while its section is
open changes its `- Correct:`; an answer removed (or no longer `wrong`)
while its section is open sets it `- Status: withdrawn`. A question of
`ai/questions.md` whose assumptions are all answered is answered first
from them (unless the person has written its answer).

Written again (after more corrections), the sections there are kept as
they are – their number, their status, what the agent wrote – and only
new ones are added: the same correction is never written twice. The
person then tells the agent "the answers and corrections are written".

## See also

[`tfl_ai_outputs()`](https://ichirio.github.io/tflspec/reference/tfl_ai_outputs.md),
[`tfl_ai_questions()`](https://ichirio.github.io/tflspec/reference/tfl_ai_questions.md)
