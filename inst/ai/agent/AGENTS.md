# tflspec: this study's report specifications, by an agent

<!-- tflspec route: spec -->

Your one job: write each report's **specification file** -- the skeleton of
the report, as YAML -- into `ai/spec/outputs/<id>.yml`, one file a report.
Those are drafts: the person checks them in tflplanner ("Check"), finishes
them in the builder and adopts them into `spec/outputs/`; you do not.

What you do not know and cannot settle from the files you read, you **ask
the person in the chat**, in {{language}}, before you write -- and stop
until they answer.  Their answers go into `ai/decisions.md`, which you read
first and follow for every report after.

A draft must be a valid specification: the YAML well formed, every key
and value as the schema in the instructions says.  You cannot run the
check yourself, so write exactly the forms the instructions and their
examples show, and nothing else.  A draft that lacks something (an
analysis set you were not told) is fine -- the person's check says what
is missing; a draft that breaks the form is not.

The paths below are from this folder: the one holding this file and `ai/`.
If the workspace is a folder above it, use the paths from here.

## What you read

Only these:

- `ai/decisions.md` first: the decisions the person made; they override
  your own choices (see *Decisions* below);
- the request you are working on, `ai/requests/<request>.md`, and the
  files it names: `ai/instructions-table.md` or
  `ai/instructions-figure.md` (the instructions and the schema),
  `ai/templates.md`, `ai/adam_summary.md`, `spec/data.yml`,
  `spec/defaults.yml` -- read each once and follow it for every request.{{reads}}

Do not look for other files: do not list or search the folders, and do not
open a file not listed here (programs, other studies, anything else). The
request has what you need: the shell's text and the variables
(`ai/adam_summary.md`).

## A request: ask, then write the skeletons

For each request you are given, in order:

1. Read `ai/decisions.md`, then the request.  The request lists the
   decisions that apply to its reports (*Decisions that apply*); apply
   them without asking (see *Decisions* for when to ask instead).
2. **Find what defines each report and is not settled** by the request,
   its shell text, `ai/adam_summary.md`, `spec/data.yml` or a decision:
   the analysis set (population); for a dataset of parameters (PARAMCD:
   ADLB, ADVS ...) the parameter; the treatment variable when neither the
   TOC, the shell nor a decision says it; the denominator of a percent;
   what is counted (subjects or events); missing values; a subgroup's or
   a filter's condition; what a code stands for when the summary does not
   show it.  These change the numbers or their meaning.  Never take a
   likely one.
3. {{ask_when}} **Ask them in the chat**, in {{language}}, numbered.  Each
   question says **which report it is about -- its id and its title --**
   and the item, the choices you see, and the one you would take if the
   person says "default".  A question that holds for every report of the
   request says so.  Then **stop and wait**: do not write the report until
   the person answers.  There is no limit to the questions; ask what you
   need, each once.  When an answer does not settle the item, ask again,
   saying what was unclear: **three times at most** for one item; then
   take the likeliest choice and mark it *to confirm* (step 6).
4. **Record the answers**: for each one, add one line to
   `ai/decisions.md` (the form is at the top of that file: its id, scope,
   topic, the decision, where it came from).  Change nothing already
   there.
5. **Write the skeleton** of each report as `ai/spec/outputs/<id>.yml`
   (UTF-8), the forms of the instructions exactly:
   - `id`, `kind`, `label`, `population`, `extends: [defaults]`;
   - `data`, and `ard` (the analysis data and the analyses: method, by,
     variables, the denominator);
   - the content: a template with its arguments when one fits
     (`template: {name: ..., args: {...}}`, only the arguments that differ
     from the default; see *Templates first*), else the items that make
     the rows -- `table_plan` (cols, rows, label) and `plan_cells`;
   - the **digits** when the shell shows them (`xx.x`, `xx.xx`): in the
     cell templates (`{mean:.1f}`) or `plan_digits`; when it does not,
     write none (the study's default applies; do not ask);
   - `page: titles`, from the shell;
   - the **footnotes**, from the shell: {{footnotes_at}}
   Nothing else of the look: no column widths, no header rows, no page
   settings, no blank rows, no borders, no fonts.  The study's defaults and
   the person's builder do those.
6. In the same file, under a top-level `ai:` key, write:
   - `assumptions`: one item for each choice that changes the numbers or
     their meaning that you took from a decision (`from: D<n>`) or, after
     three rounds, on your own: `where`, `item`, `assumption` (one
     sentence: what you decided and how), `value`;
   - `confirm`: what you could not settle and the person should decide in
     the builder: `where`, `item`, `why` (one sentence).
   ```yaml
   ai:
     assumptions:
       - {where: ard.analyses, item: where, assumption: "treatment-emergent is TRTEMFL == \"Y\"", value: "TRTEMFL == \"Y\"", from: D3}
     confirm:
       - {where: content, item: plan_cells, why: "the shell shows n (%) but not whether the % is of the arm or of all"}
   ```
   How the output looks is never an assumption and never a question:
   decide nothing about it, write nothing of it.
7. Go on to the next request.  A question about the work rather than a
   report (a file the request names is missing, the request is unclear):
   ask it in the chat too, and add it to `ai/questions.md` (`## Q<n>. ...`,
   `- Reports:`, `- Where:`, `- Status: open`, `- Answer:`).

When the requests you were given are done, **stop** and tell the person:
the report files written (their ids), the decisions added, and what is
marked to confirm.  Do not go on to other requests: the person asks for
them.

At most {{max_reads}} times for a request, open a file or run a command
beyond the request and the files it names.  When you reach it, ask or
mark to confirm, and go on.

If something does not work -- a file cannot be saved, a request cannot be
read -- try once more.  If it fails again, say so in the chat, write it in
`ai/questions.md`, and stop.

## When the check refuses a draft

The person's check reads your drafts.  One that breaks the form (a key the
schema does not have, a value of the wrong type, YAML that does not parse)
is refused and stays a draft; the person pastes the check's note into the
chat.  Fix the file as the note says -- the form only, the content as it
was -- save it over `ai/spec/outputs/<id>.yml`, and tell the person.

## Decisions

`ai/decisions.md` holds one decision a line:

```
- D<n> [<scope>] <topic>: <the decision> (<source>)
```

`scope` is `all` (every report), a dataset (`ADAE`), a TOC section, or
report ids separated by commas; `topic` names the item decided
(`population.default`, `treatment_var.SAF`, `denominator`, `missing`,
`subjects_or_events`, `codes.RACE`, `teae.where` ...); `source` is where
it came from (`chat, T-14-3-1`, or the person's own line).  A line of the
person's own without `[scope]` is `all`.

Before asking, look for the topic there -- and be **careful**: apply a
decision without asking only when you are sure it holds for this report
(its scope is `all`, or the report's dataset{{carry_section}}, and nothing
in the shell says otherwise); write an assumption line with `from: D<n>`.
Whenever you are not sure -- a decision made for other reports, a shell
that reads differently, two decisions of one topic that disagree --
**ask**: "T-14-1-1 took X for this (D2); the same here?", with "yes" as
what you would take.  When the person says yes, widen the decision's
scope (add the report's id, its dataset or its section) instead of adding
a line.

## The person's corrections

When the person says corrections are written (`ai/corrections.md`): for
each section with `- Status: open`, change the report file it names as it
says, set the section to `- Status: done` (one you cannot do: leave it
open and add a line `- Agent:` saying why), and stop.  A question answered
there is a decision: add its line to `ai/decisions.md` first.

## Never

- Do not install packages or go online. Do not run `run.R`, and do not
  write programs, YAML under `spec/` or RTF files: the person's check does
  that. {{run_r}}
- Do not open a file not listed under "What you read", and do not search
  the folders.
- Do not change the TOC / SHELL workbook, the ADaM, `study.yml`,
  `spec/data.yml`, `spec/defaults.yml`, `run.R`, `AGENTS.md` or
  `.github/copilot-instructions.md`.
- Write only `ai/spec/outputs/`, `ai/questions.md`, `ai/decisions.md`
  (add lines, change nothing a person wrote) and the `- Status:` /
  `- Agent:` lines of `ai/corrections.md`.
- Do not guess what defines a report: ask.  Do not write the look.
- Do not try the same thing more than twice: say so in the chat, write it
  in `ai/questions.md`, and stop.
