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

The paths below are from the study folder: the one holding `ai/` (this
file is `ai/AGENTS.md`).  If the workspace is a folder above it, use the
paths from there.

## What you read

Only these:

- this file, once;
- `ai/decisions.md` first: the decisions the person made; they override
  your own choices (see *Decisions* below);
- `ai/requests/index.md`: the requests in the order you do them;
- the request you are working on, `ai/requests/<request>.md`, and the
  files it names: `ai/instructions-table.md` or
  `ai/instructions-figure.md` (the instructions, the schema, what a draft
  holds), `ai/templates.md`, `ai/adam_summary.md`, `spec/data.yml`,
  `spec/defaults.yml` -- read each once and follow it for every request.{{reads}}

Do not look for other files: do not list or search the folders, and do not
open a file not listed here (programs, other studies, anything else). The
request has what you need: the shell's text and the variables
(`ai/adam_summary.md`).

## The requests: ask, then write the skeletons

Do every request of `ai/requests/index.md`, top to bottom.  A request
whose reports all have their draft (`ai/spec/outputs/<id>.yml`) is done:
skip it.  For each of the others, in order:

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
   likely one.  When a footnote, the SAP or a document the person gave
   **defines a derived timepoint or population** ("End of Treatment is
   the last on-treatment assessment", "the per-protocol set excludes
   ..."), apply that rule (an analysis data with `code`, an assumption
   that cites it) or ask; do not take a visit name or a flag from the
   data in its place.
   **What to ask, and what not to ask:**
   - Do not ask what the documents or the data settle: the shell, the
     TOC, the SAP, `ai/adam_summary.md`, `spec/data.yml` or a decision.
     Decide, and write the source in the assumption (`from:`).
   - Do not ask about a case the data shows cannot happen: what to show
     when there are 0 events while the summary shows events; how to
     treat missing values when the summary says `[missing: none]` for
     the variable.  Write the usual default and list it under
     *to confirm* (step 6) -- no question.
   - Ask only when the sources disagree, or when a choice changes a
     report's numbers and nothing -- no document, no data, no decision
     -- defines it.
3. {{ask_when}} **Ask them in the chat**, in {{language}}, numbered.
   **Collect every question first** -- every item of step 2, for every
   report of the request -- and ask them together in **one message**;
   never one at a time, waiting for an answer between them.  A follow-up
   question only when an answer raises a new point.  Each
   question says **which report it is about -- its id and its title --**
   and the item, the choices you see, and the one you would take if the
   person says "default".  **End every question with an example answer**,
   one line, `例: ...` in Japanese (`e.g. ...` in English): the answer as
   the programmer would type it, holding every piece the draft needs --
   the title's label, the condition, the variable, which source is wrong
   -- so that the person sees what to answer, and can paste it as it is
   or edit it.  The example follows your default choice.  A choice alone
   ("SAFFL") is not an answer.
   - a population conflict:
     `例: シェルのとおり Safety Analysis Set（SAFFL = 'Y'）、実治療 TRT01A。TOC の Full Analysis Set は誤り。`
   - a denominator:
     `例: 分母は Total 行（post-baseline 値が 1 つ以上ある被験者）。食い違う脚注は削除。`
   A question that holds for every report of the
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
7. **Read the draft once more before you save it**, against the list
   *Before you save a draft* at the end of the instructions
   (`ai/instructions-table.md`): the keys, the indentation, the quotes,
   the forms of `data` and `ard`.  A draft the check refuses comes back
   to you with its problems; one you checked yourself seldom does.
8. Go on to the next request.  A question about the work rather than a
   report (a file the request names is missing, the request is unclear):
   ask it in the chat too, and wait for the answer.

When every request is done, **review the drafts** (*Before you stop*,
below), then **stop** and tell the person: the report files written
(their ids), the decisions added, what is marked to confirm, and what
the review changed.

## Before you stop

Review each draft against this list and fix what fails:

- every row, column, statistic and footnote of the shell is in the draft
  or marked to confirm;
- the analysis set, the treatment variable, the denominator and the
  filters agree with the shell, the TOC and the documents (or the
  disagreement was asked about or is marked);
- the categories' order and labels agree with the shell and the code
  lists;
- the digits follow the shell, or the documents' rule;
- a derived timepoint or population a footnote or a document defines is
  made by its rule, not taken from the data;
- nothing of the look is written;
- the forms are the instructions' (*Before you save a draft*).

Then tell the person what the review changed, draft by draft (or that it
changed nothing).

## Stopping and going on

The person may stop you between requests ("stop", a question of their
own) and later say **"continue"**: go on from the first request of
`ai/requests/index.md` that has a report with no draft, or whose draft the
person says the check refused -- the decisions in `ai/decisions.md` hold
as before.  Never write a draft that is there again unless the person asks
for it (the check's note, a correction in the chat).

At most {{max_reads}} times for a request, open a file or run a command
beyond the request and the files it names.  When you reach it, ask or
mark to confirm, and go on.

If something does not work -- a file cannot be saved, a request cannot be
read -- try once more.  If it fails again, say so in the chat and stop.

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

## Never

- Do not install packages or go online. Do not run `run.R`, and do not
  write programs, YAML under `spec/` or RTF files: the person's check does
  that. {{run_r}}
- Do not open a file not listed under "What you read", and do not search
  the folders.
- Do not change the TOC / SHELL workbook, the ADaM, `study.yml`,
  `spec/data.yml`, `spec/defaults.yml`, `run.R`, `AGENTS.md` or
  `.github/copilot-instructions.md`.
- Write only `ai/spec/outputs/` and `ai/decisions.md` (add lines, change
  nothing a person wrote).
- Do not guess what defines a report: ask -- but do not ask what the
  documents or the data settle, nor about a case the data rules out.
  Do not write the look.
- Do not try the same thing more than twice: say so in the chat and stop.
