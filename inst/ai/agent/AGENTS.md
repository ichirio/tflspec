# tflspec: this study's report specifications, by an agent

Your one job: write the **answer** to each request you are given, as
`ai/answers/<request>.md`. An answer is the specification (YAML) of the
request's reports. The person checks every answer in tflplanner ("Check all
answers"), and that writes the YAML drafts and the programs; you do not.

The paths below are from this folder: the one holding this file and `ai/`.
If the workspace is a folder above it, use the paths from here.

## What you read

Only these:

- the request you are answering, `ai/requests/<request>.md`;
- the files it names: `ai/instructions-table.md` or
  `ai/instructions-figure.md` (the instructions and the schema),
  `ai/templates.md`, `ai/adam_summary.md`, `spec/data.yml`,
  `spec/defaults.yml` -- read each once and follow it for every request;
- `ai/decisions.md`: it overrides your own choices.{{reads}}

Do not look for other files: do not list or search the folders, and do not
open a file not listed here (programs, other studies, anything else). The
request has what you need: the shell's text and the variables
(`ai/adam_summary.md`).

## Round 1: the requests you are given

For each request you are given, in order:

1. Read it and write the answer it asks for: one ```` ```yaml ```` block
   (the header, `reports`, `outputs`, `assumptions`).
2. Save the whole answer as `ai/answers/<request>.md` (UTF-8). That is the
   request done: go on to the next one. One answer a request.
3. What the request, its shell text and `ai/adam_summary.md` do not
   settle: **do not look for it**. Above all, what a code stands for (a
   numeric code and its text, `RACEN` and `RACE`): when the summary does
   not show it (`[codes: ...]`), do not read the data for it; take it as
   an assumption with the question "the codes' texts are not known". Take the likeliest choice and write it
   as an assumption -- `assumption` one sentence saying what you decided
   and how, `value` the value you used -- with its `question` (the
   request's answer format says how), and go on.  The treatment variable
   follows the analysis set: the safety set (SAFFL) takes TRT01A (actual),
   any other set (ITT, FAS, PP; demographics, disposition) TRT01P
   (planned), unless the shell or the TOC says otherwise. Not so for the analysis set and, for a dataset of
   parameters (PARAMCD), the parameter: they define the report. When the
   request does not say them, never take a likely one: write the question,
   and leave them out of the report.
4. A question about the work rather than a report (a file the request
   names is missing, the request is unclear): add it to `ai/questions.md`
   (`## Q<n>. ...`, `- Reports:`, `- Where:`, `- The agent took, for now:`,
   `- Status: open`, `- Answer:`).

When the requests you were given are done, **stop** and tell the person:
the answers written, and the questions asked. Do not go on to other
requests: the person asks for them.

At most {{max_reads}} times for a request, open a file or run a command
beyond the request and the files it names. When you reach it, write what
is still unclear as assumptions and questions, write the request's
answer, and go on to the next.

If something does not work -- a file cannot be saved, a request cannot be
read -- try once more. If it fails again, write what happened in
`ai/questions.md` and stop.

## Round 2: the person's answers and corrections

When the person says the answers and corrections are written:

1. Read `ai/corrections.md` (when there is none, read `ai/questions.md` for
   the questions answered). For each section with `- Status: open`:
   - `add <id>`: write the report from its request, as round 1 does, and
     list it in the request's answer (`reports:` and `outputs`).
   - `drop <id>`: take the report out of the answer (both lists).
   - `change <id>: ...`: change what it says in the report (a new id:
     rename the report and every mention of it).
   - `note <id>`: change the report as the note says.
   - `Q<n> (answered)`: add one line to `ai/decisions.md`,
     `- Q<n> (<the reports>): <the decision, in a sentence>`; set the
     question's `- Status: open` to `- Status: decided` in
     `ai/questions.md`; then change every report it names.
   - `A<n>. assumption of <id>: <item>` / `K<n>. assumption of every
     report: <item>`: the person says your assumption (`- Was:`) is wrong;
     change the report(s) so they do what `- Correct:` says.
2. Save each changed answer whole over `ai/answers/<request>.md`, set the
   section's `- Status: open` to `- Status: done` (a section you cannot do:
   leave it open and add a line `- Agent:` saying why), and stop.
3. Tell the person what changed and the sections left.

## Never

- Do not install packages or go online. Do not run `run.R`, and do not
  write programs, YAML under `spec/` or RTF files: the person's check does
  that. {{run_r}}
- Do not open a file not listed under "What you read", and do not search
  the folders.
- Do not change the TOC / SHELL workbook, the ADaM, `study.yml`,
  `spec/data.yml`, `spec/defaults.yml`, `run.R`, `AGENTS.md` or
  `.github/copilot-instructions.md`.
- Write only `ai/answers/`, `ai/questions.md`, `ai/decisions.md` and the
  `- Status:` / `- Agent:` lines of `ai/corrections.md` (add lines; change
  nothing a person wrote).
- Do not try the same thing more than twice: write it in
  `ai/questions.md` and stop.
