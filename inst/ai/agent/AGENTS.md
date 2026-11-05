# tflspec: this study's report specifications, by an agent

<!-- tflspec route: spec -->

Your one job: write each report's **specification file** -- the skeleton of
the report, as YAML -- into `ai/spec/outputs/<id>.yml`, one file a report.
Those are drafts: the person checks them in tflplanner ("Check"), finishes
them in the builder and adopts them into `spec/outputs/`; you do not.

Every open point of a report you settle in **three steps, in this order**
(*Every open point: three steps*, below): 1) from the documents -- the
shell, the TOC, the SAP, the ADaM and its specification, the company's
and the study's standard, a decision; 2) when they do not settle it, as
an experienced statistical programmer would -- the usual convention for
that kind of table -- written as an assumption with
`from: "programmer convention"`, so that QC sees it; 3) only what neither
settles **and** would change the numbers or the structure, you ask the person (in the chat when you run one request; in a batch you
take your default and note the question: *Running the requests*, below).
Their answers go into `ai/decisions.md`, which you read
first and follow for every report after.

A draft must be a valid specification: the YAML well formed, every key
and value as the schema in the instructions says.  {{check_rule}}
Write exactly the forms the instructions and their examples show, and
nothing else.  Quote every free-text value -- `label`, titles, footnotes, an assumption's text -- in double quotes.  Always quote a value that holds `: ` or `#`, or starts with `[ { * & ! | > ' " % @`, or is yes / no / on / off or a number you mean as text.
A draft that lacks something (an
analysis set you were not told) is fine -- the person's check says what
is missing; a draft that breaks the form is not.

This folder -- the study's `ai/` -- is your workspace: every path below
is from it, and every file you read or write is in it.  Never open a
path outside it (`../`), except to run `../run.R` as *Build and check* says.

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

## Every open point: three steps

1. **The documents settle it.**  The request's shell text and TOC row,
   the SAP and the documents the person gave, `ai/adam_summary.md` (the
   ADaM and its specification: the variables, their values, the define's
   derivations), `study_facts.md` (the study's standard), a decision in
   `ai/decisions.md`.  Take it, and write the source in the assumption
   (`from: shell`, `from: SAP`, `from: define`, `from: D<n>`).
2. **Else the usual convention settles it.**  Decide it as an experienced
   statistical programmer would for that kind of table, and write it as
   an assumption with `from: "programmer convention"` -- one sentence of
   what you took and why it is the usual one.  For example, unless the
   documents say otherwise:
   - a percent's denominator is the column's N: the subjects of the
     analysis set in that arm (the N under the arm);
   - an AE table counts subjects, each once in a SOC and once in a PT; a
     table by severity or grade counts a subject once, at the worst;
     treatment-emergent is `TRTEMFL == "Y"` when ADAE has it;
   - a safety table (AE, labs, vital signs, exposure) is by the actual
     treatment (TRT01A), an efficacy table by the planned or randomized
     one (TRT01P);
   - a summary by visit takes the dataset's analysis records
     (`ANL01FL == "Y"` when the summary lists it), the baseline the
     record flagged `ABLFL == "Y"` and the change `CHG`, the visits in
     `AVISITN` order;
   - a subject with no value is left out of the statistics (n shows
     how many), and a Missing row only when the shell shows one.
   A convention holds only when it is *the* usual one for this kind of
   table: when two are usual (LOCF or observed cases, a percent of the
   arm or of those with a value), it is not settled -- step 3.
3. **Else ask** -- only what neither the documents nor a convention
   settles **and** what changes the report's numbers or its structure
   (its rows, its columns, what is counted).  A point that changes
   neither is not a question: take the usual choice, say nothing of the
   look, and list the rest under *to confirm* (step 6 below).

The aim is **few questions**, and a draft that needs no rework when the
person says "yes".  Every open point is in one of three tiers:

**(a) Decide it yourself, silently** -- an assumption (`from: shell`,
`from: SAP 9.2` or `from: "programmer convention"`), no question --
whatever an experienced programmer decides without asking, even when the
shell and the SAP differ:
- the layout when the shell and the SAP differ: the shell's columns,
  rows, sort order, overview rows and labels (the shell is the agreed
  layout: the SAP's extra "All Drug A" column, its extra overview rows,
  its sort by another column are not used);
- which visits or rows a table shows when the shell lists them and the
  SAP adds a display rule (a minimum n a visit, a baseline row only for
  those with post-baseline values): the shell's, the SAP's rule to
  confirm;
- a presentation rule the SAP or the standard gives and the shell does
  not contradict (a zero count printed `0`, `<0.1`, the digits by the
  data's precision): write it, `from: SAP 5.5`;
- the column widths (step 5); a footnote of a statistic the table does
  not show (dropped); a label's case as the shell writes it (the visits
  `Cycle 2 Day 1`, not the data's `CYCLE 2 DAY 1`: a code list or
  labels).

**(b) Decide a default, and ask only to confirm it** -- for what changes
the numbers when the documents disagree or say nothing: the analysis
set, the treatment variable, a subset, a flag or a derivation
(treatment-emergent, on-treatment, the worst value), an imputation, a
denominator, what is counted.  Take the default an experienced
programmer takes, and ask a yes / no question that says it; the draft
you write after the answer takes it with an assumption
(`from: "programmer convention"`, or the decision), so that "yes" needs
no rework:
- an AE table whose population nothing states: "14.3.1.1: I use the
  Safety Analysis Set (SAFFL = 'Y') by actual treatment (TRT01A), as
  usual for AE tables -- OK?";
- a title that says "Part 1": "14.2.3: Part 1 subjects only
  (`PARTFL == "Y"` / `APERIOD == 1`), from the title -- OK?";
- a TOC and a shell that name different sets, the SAP siding with one:
  "16.2.7.1: Safety Analysis Set, as the shell and SAP 10 (the TOC says
  Full Analysis Set) -- OK?";
- two derivations, one per document, that would change the numbers: ask.
  When one is the shell's note and the other the SAP's rule, the default
  is the shell's (an SAP often gives a derivation in prose, a shell is
  written per output).  Otherwise the default is the one the specific
  note or the newer SAP gives.  Say so in the question.

**(c) Ask an open question** only when no sensible default exists: a
variable the shell names that the data does not have (BMIBL), a
parameter nothing names, two equally usual choices that change the
numbers (LOCF or observed cases).  Give the choices and the one you
would take, as step 3 says.

When the documents agree on a point, it is settled: take it, no question.

## The requests: ask, then write the skeletons

The requests are those of `ai/requests/index.md`, in the order of
`ai/progress.md`.  A request whose reports all have their draft
(`ai/spec/outputs/<id>.yml`) is done: skip it.  How many you do before you
stop is *Running the requests*, below: one, or a batch.  For each request:

1. Read `ai/progress.md` and the request, and `ai/decisions.md` (all of it:
   it is the questions and answers so far).  The request lists the
   decisions that may apply to its reports (*Decisions that may apply*);
   judge, for each report, which entries hold for it (see *Decisions*).
2. **Find what defines each report and is not settled** by the request,
   its shell text, `ai/adam_summary.md`, `spec/data.yml` or a decision:
   the analysis set (population); for a dataset of parameters (PARAMCD:
   ADLB, ADVS ...) the parameter; the treatment variable when neither the
   TOC, the shell nor a decision says it; the denominator of a percent;
   what is counted (subjects or events); missing values; a subgroup's or
   a filter's condition; what a code stands for when the summary does not
   show it.  These change the numbers or their meaning.  Settle each by
   the three steps (*Every open point*, above): the documents, else the
   usual convention, else a question -- never a choice that is only
   likely.  When a footnote, the SAP or a document the person gave
   **defines a derived timepoint or population** ("End of Treatment is
   the last on-treatment assessment", "the per-protocol set excludes
   ..."), apply that rule (an analysis data with `code`, an assumption
   that cites it) or ask; do not take a visit name or a flag from the
   data in its place.
   **What to ask, and what not to ask:**
   - Do not ask what the documents or the data settle: the shell, the
     TOC, the SAP, `ai/adam_summary.md`, `spec/data.yml` or a decision.
     Decide, and write the source in the assumption (`from:`).
   - Do not ask what the usual convention for this kind of table
     settles (*Every open point*, step 2): decide it, and write it as an
     assumption with `from: "programmer convention"`.
   - Do not ask about a case the data shows cannot happen: what to show
     when there are 0 events while the summary shows events; how to
     treat missing values when the summary says `[missing: none]` for
     the variable.  Write the usual default and list it under
     *to confirm* (step 6) -- no question.
   - Ask only what changes the numbers and the documents disagree on or
     do not settle -- with your default, to confirm (tier (b), above) --
     or what has no sensible default (tier (c)).  A layout the shell and
     the SAP show differently is not a question: the shell's (tier (a)).
   - Ask for the digits of a parameter that has none at the define's
     value level while others of the variable have: the variable-level
     digits are not known to be the collected precision.  Offer them as
     the default.
   **Your default choice** (the one you take when the person says
   "default"):
   - never a variable or a value that `ai/adam_summary.md` does not list
     (no `BMIBL` when ADSL has no BMIBL): then the default is to leave the
     row out, and the question says the variable is not in the data;
   - the shell's layout (its columns, rows and items) over the SAP's when
     the two differ: the shell is the agreed layout, the SAP is amended
     later;
   - the shell's note over an SAP derivation rule when the two would
     change the numbers: the SAP gives derivations in prose, the shell is
     written per output; the question says so;
   - a specific programming note or derivation rule over a generic
     footnote (footnotes are often stale), and the question says so.
3. **Ask, or take your default**, as you are run: one request, you ask in
   the chat; in a batch, you take your default and note the question
   (*Running the requests*, below).
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
   - the **cell form and the N** when the shell's differ from the study's
     (`study_facts.md`): `xx (xx.x%)` is `"{n} ({p:.1f%}%)"` (`.1f%` prints
     no `%` sign), `xx (xx.x)` is `"{n} ({p:.1f%})"` -- a template's
     `count_cell` or `plan_cells`; `(N=xx)` under the arm is one
     `plan_col_header` line, `{col}\n(N={n})` (the instructions say how) --
     only for a table whose arm heads one value column and whose header
     has no N yet.  A template whose header already shows the N
     (`param_change`, `shift`), or arms over several columns (Value and
     Change from Baseline): no header line -- it would replace the arm's
     name with `(N=NA)`;
   - a **zero count** as the SAP or the standard says (`0`, no percent):
     a guarded cell -- a template's `count_cell: [!r 'n == 0 ~ "0"',
     "{n} ({p:.1f%}%)"]`, a code SPEC's `plan_cells: {categorical: [...]}`
     with the same list;
   - the **column widths**, always, as a programmer sets them: a table's
     `plan_columns: {widths: {row_label: 3, .values: 1}}`
     (relative; `row_label` is the stub, `.values` every value column), a
     listing column's `width:` (characters).  From the
     shell's proportions when it shows them; else from the kind of table
     and its content -- the longest row label against a value cell
     (`xx (xx.x%)` is about 10 characters), the number of value columns
     (a demographics or AE table: the stub about 2.5 to 3 times a value
     column; a table of many columns, narrower); a listing column wide
     enough for its longest value (a subject id on one line), a short
     column too (`Age/Sex`, `64/M`: about 10).  The widths are scaled to
     the page, so their sum fits it (a landscape page about 120
     characters): take the room from the long text column (a term), not
     the short ones.  An
     assumption, `item: widths`, `from: "programmer convention"` (or
     `from: shell`);
   - the **stub heading** as the shell's (`System Organ Class / Preferred
     Term`, `Visit / Statistic`): a template's `heading` argument where
     it has one, else `plan_stub: {heading: "..."}`; a shell with none
     leaves the study's blank.  An assumption, `item: stub heading`,
     `from: shell`;
   - {{titles_rule}}
   - the **footnotes**, from the shell: {{footnotes_at}}
     A footnote that states what a decision replaced (the shell's old
     rule, after the person chose the SAP's) is written to the decision,
     with an assumption `from: D<n>`; never left saying the old rule;
   Nothing else of the look: no other header rows, no page settings, no
   blank rows, no borders, no fonts.  The study's defaults and the
   person's builder do those.
6. In the same file, under a top-level `ai:` key, write:
   - `assumptions`: one item for each choice that changes the numbers or
     their meaning that you took from a document (`from: shell`,
     `from: SAP`, `from: define`), from a decision (`from: D<n>`), from
     the usual convention (`from: "programmer convention"`) or, after
     three rounds, on your own: `where`, `item`, `assumption` (one
     sentence: what you decided and how), `value`, `from`;
   - `confirm`: what you could not settle and the person should decide in
     the builder: `where`, `item`, `why` (one sentence).
   ```yaml
   ai:
     assumptions:
       - {where: ard.analyses, item: where, assumption: "treatment-emergent is TRTEMFL == \"Y\"", value: "TRTEMFL == \"Y\"", from: D3}
       - {where: ard.analyses, item: denominator, assumption: "the percent is of the arm's subjects in the safety set, the usual denominator of an AE table", value: "SAF by TRT01A", from: "programmer convention"}
     confirm:
       - {where: content, item: plan_cells, why: "the shell shows n (%) but not whether the % is of the arm or of all"}
   ```
   Write every assumption, confirm item and question in the chat's language
   (`ai: language`: {{language}}); keys and values stay as they are.
   How the output looks is never a question, and nothing of it is written
   but the column widths (step 5, an assumption `item: widths`).
7. **Read the draft once more before you save it**, against the list
   *Before you save a draft* at the end of the instructions
   (`ai/instructions-table.md`): the keys, the indentation, the quotes,
   the forms of `data` and `ard`.  A draft the check refuses comes back
   to you with its problems; one you checked yourself seldom does.
8. Set the request's row in `ai/progress.md`, and go on as *Running the
   requests* says.  A question about the work rather than a report (a file
   the request names is missing, the request is unclear): ask it in the chat
   when you run one request.  In a batch, do not stop: note it in
   `ai/questions-end.md` as a `BLOCKER` entry, go on with the other requests,
   and ask it at the end of the batch.

When every request is done, **review the drafts** (*Before you stop*,
below), then **stop** and tell the person: the report files written
(their ids), the decisions added, what is marked to confirm, and what
the review changed.

## Running the requests: one request, or a batch

`ai/agent_prompt.md` says how you start: **one request**, then the menu, or
**a batch** of all the requests that are left. After that the person chooses
at the menu. Work in one of two ways, and say nothing of the choice but the
menu:

{{run_one}}

{{run_batch}}

### The menu

After one request, and after a batch while requests remain, **stop and post
this in {{language}}**, with the numbers of `ai/progress.md`:

```
[3/17] Done: T-AE (14.3.1.1, 14.3.1.2). What next?
c = the next request (T-LB-SUM)    <number> = the next N together (e.g. 5)    a = all the rest together (14)
```

In Japanese:

```
[3/17] 完了: T-AE（14.3.1.1, 14.3.1.2）。次は？
c = 次の依頼（T-LB-SUM）    数字 = 次の N 依頼をまとめて（例: 5）    a = 残りを全部まとめて（14）
```

Accept the reply as:
- `c`, `続ける`, `次`, `continue`: the next request, in the one-request way;
- a number N (`5`): the next N requests, as a batch;
- `a`, `全部`, `all`: all the rest, as a batch;
- any free text you can map to these clearly ("do the next five");
- anything unclear: show the menu again; do not guess.

When no request is left, do not show the menu: finish (*Before you stop*).

## Many requests: one at a time

Each request is processed on its own: the common files plus ONE request.
- Read the common files once: this file, the instructions, `ai/templates.md`,
  `ai/adam_summary.md`, `study_facts.md`, `ai/reference/` and
  `ai/decisions.md`. Then, for each request, read only that request and the
  files it names.
- Do not reopen an earlier request, its shell, or its drafts. Only when
  `ai/decisions.md` points to one, read that one draft.
- Keep nothing of an earlier report in mind beyond `ai/decisions.md` and
  `ai/progress.md`. They are your memory.
- **Start every request by reading `ai/progress.md` and that request
  again**, even when you think you remember them: your editor may have
  summarized the chat, and these files are the truth.
- **File first.** Every request writes, as it goes and even when incomplete,
  its drafts, its decisions (`ai/decisions.md`), its questions
  (`ai/questions-end.md` in a batch) and its row in `ai/progress.md`. A failed
  or summarized session resumes from the files and fixes things there.
- Your editor may summarize (compact) a long chat on its own. After that, go
  on from `ai/progress.md` as usual; nothing is lost, because every draft and
  decision is in a file.{{per_chat}}

## Progress

Say where you are, one line in {{language}}, so that the person sees how
far you have got:

- at the start of each request: `[3/6] T-LB-SUM（14.3.4.1）を開始。残り 3 件`
  (`[3/6] Starting T-LB-SUM (14.3.4.1); 3 left`);
- when a request is done: `[3/6] 完了: 14.3.4.1。次: T-LB-SHIFT`
  (`[3/6] Done: 14.3.4.1; next: T-LB-SHIFT`) -- in a batch, **go straight on
  to the next request**; in the one-request way, show the menu.  You stop
  only at the menu, to ask questions (then wait for the answers), when you are
  stuck (*When you are stuck*), or when every request is done;
- a question message starts with the same tag (step 3).

`n` is the request's number of `N` in `ai/requests/index.md`.

`ai/progress.md` is the work queue, in this form (R wrote it with every
request `todo`; if it is missing, write it at your first start from
`ai/requests/index.md`):

```
| request | reports | status | batch | questions |
|---|---|---|---|---|
| T-DM | 14.1.1, 14.1.2 | built | 1 | Q1 |
| T-AE | 14.3.1.1 | todo | | |
```

`status` is one of `todo`, `asked` (one request: questions asked, waiting),
`drafted`, `built`, `checked`, `blocked`, `reworked`, `failed` (`run.R` sets
`built` or `failed` after a build; a `checked` row stays unless the build
failed). `batch` is the batch's
number (empty in the one-request way). Update one row as you go. A new chat
starts from the first row that is not `checked` (or not `built`, when the
review is none). (`ai/status.md` is `run.R`'s: do not write it.)

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
- every choice the documents did not settle is an assumption with its
  `from:` (`"programmer convention"` for a usual convention), and every
  question asked was one neither settled;
- a derived timepoint or population a footnote or a document defines is
  made by its rule, not taken from the data;
- nothing of the look is written but the column widths, which every
  draft has;
- every free-text value (`label`, titles, footnotes, assumption text) is
  in double quotes -- always one with `: ` or `#` in it;
- every assumption, confirm item and question is in the chat's language
  (`ai: language`: {{language}}); keys and values stay as they are;
- a template with its own data (`km_survival`, `km_cumulative`) has
  `data.steps` of `read`, the analysis set and `param`, and none of the
  template's own steps (`survfit`, `time_unit`);
- the forms are the instructions' (*Before you save a draft*).

Then tell the person what the review changed, draft by draft (or that it
changed nothing).

## Stopping and going on

The person may stop you at any time -- "止まって", "stop", "pause", or a
question of their own.  Then **finish the step in hand cleanly**: save the
draft you are writing, write `ai/progress.md` (what is done, what is
next), and stop, saying where you stopped (`[3/6] 停止: 14.3.4.1 まで。次:
T-LB-SHIFT` / `[3/6] Stopped after 14.3.4.1; next: T-LB-SHIFT`).  Later
the person says **"続けて" / "continue" / `c`**: go on from `ai/progress.md` --
the first request of `ai/progress.md` that is not `checked` (or not
`built`, when the review is none), or whose draft the person says the check
refused -- the decisions
in `ai/decisions.md` hold as before.  Never write a draft that is there
again unless the person asks for it (the check's note, a correction in
the chat).

At most {{max_reads}} times for a request, open a file or run a command
beyond the request and the files it names.  When you reach it, ask or
mark to confirm, and go on.

If something does not work -- a file cannot be saved, a request cannot be
read -- try again, up to three times in all.  If it still fails, *When you
are stuck*.

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
a line.  In a batch the question is not asked: take "yes" and note it in
`ai/questions-end.md`.

A decision is never applied to other reports at the moment it is made.
Each report judges it when it is processed.

## When you are stuck

Your editor summarizes a long chat by itself; that comes first. This is for
when it did not help, and you cannot go on safely:
- your context is full and a summary did not help (you lose track of the
  request, or of what you read);
- the same tool call fails three times;
- `Rscript` is refused or not found.

Then, in this order:
1. Save `ai/progress.md`, with the current row's status and a note of where
   you are, and the current draft, even when it is incomplete.
2. Stop.
3. Post one message, in {{language}}: what happened, exactly what the person
   should do, and where the run will resume (the request). The person can
   choose:
   - "type `/compact` in this chat, then `c`";
   - "open a new chat, give it `ai/agent_prompt.md`, then type `c`";
   - "allow `Rscript` (or run `Rscript run.R <request>` yourself, from the study folder), then type
     `c`".

Never loop: never retry the same failing step more than three times.

## Never

- Do not install packages or go online. {{run_rule}} Do not
  write programs, the study's own specification (outside this folder) or
  RTF files: the person's check does that. {{run_r}}
- Do not open a file not listed under "What you read", and do not search
  the folders.
- Do not change the TOC / SHELL workbook, the ADaM, `study.yml`,
  `spec/data.yml`, `spec/defaults.yml`, `run.R`, `AGENTS.md` or
  `.github/copilot-instructions.md`.
- Write only `ai/spec/outputs/`, `ai/decisions.md` (add lines, change
  nothing a person wrote), `ai/progress.md`, `ai/questions-end.md`.
- Do not guess what defines a report: take it from the documents, else
  from the usual convention (an assumption, `from: "programmer
  convention"`), else ask -- and do not ask what those settle, nor about a
  case the data rules out.
  Do not write the look (the column widths are not the look: write
  them).
- Do not try the same thing more than three times: *When you are stuck*.
