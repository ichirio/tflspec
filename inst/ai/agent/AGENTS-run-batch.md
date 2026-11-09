### A batch

A batch is the requests you were told to do together (all the rest, or the
next N). **Do not ask now: take your default, note the question, go on.**
The batch's questions are asked once, at its end, so that the person need not
wait at the chat. For each request of the batch, in the order of
`ai/progress.md`, for each question step 2 leaves (what you would ask), do
this instead of asking:
- Take the choice you would give as the default (*Your default choice*,
  above), as if the person had answered "default".
- Add the question to `ai/questions-end.md` (create it the first time), under
  a heading for this batch: `## Batch <k>: <requests>` (a new section for
  each batch; `k` counts the batches of the run). Use one numbered entry,
  `Q<n>`, numbered across the whole run:
  `Q<n> [<request>] <report ids and titles>: <the question, its choices> --
  default taken: <your choice>` followed by its `例:` line or lines, as in
  the one-request form above.
- Record the default in `ai/decisions.md` as a decision whose source is
  `(default, Q<n>)`, and write the assumption in the draft with
  `from: D<n>` and `question: Q<n>`.
- A "the same here?" question (*Decisions*) is not asked either: take "yes"
  as the default, and note it in `ai/questions-end.md` like the others.
- **A point with no sensible default** is one where a report cannot be
  written without it: the analysis set, the parameter, or what is counted
  is unknown, or the documents contradict each other with nothing to break
  the tie. Do not write that report. Add the entry as
  `Q<n> BLOCKER [<request>] <report id>: ...`, set its row `blocked`, say it
  in the progress line, and go on with the other reports.
- **Never stop and wait mid-batch.** Go straight on, request after request:
  write the drafts{{batch_build}}, and fill
  the row of `ai/progress.md` (with the `batch` number) as you go.

When every request of the batch is {{batch_done}}, **ask everything at
once**. Post this as one message, in {{language}}:
- the batch's questions from `ai/questions-end.md` (this batch's section
  only): the blockers first, then the rest by request;
- each entry with the default you took and its `例:` lines;
- {{batch_defaults}};
- the defaults listed once more, one line each, so that a default is
  never kept by silence alone;
- at the end: "Answer only what you want changed (Q<n>: ...); the others
  keep their default.  Change a default the same way: `D<n>: ...` or
  `<report> <item>: ...`.  OK to keep all the other defaults?"

Then **stop and wait**. When the person answers:
- For each changed answer, change that decision's line in
  `ai/decisions.md`: the new decision, from `chat`.
- **Rework by each request**: for each request of the batch, one at a time,
  in the order of `ai/progress.md`, read its drafts' assumptions and the
  changed decisions, and judge whether the draft changes. Change it only if
  so. A draft that cites a changed decision (`from: D<n>`, `question: Q<n>`)
  is surely affected; others may be too, when the same topic holds for them.
  The blockers answered are written now.
- Mark the rows `reworked` (changed) or `checked` (not changed) in
  `ai/progress.md`.
- {{rework_build}}
- Then say what changed, report by report, and **show the menu again** when
  requests remain (*The menu*), else finish (*Before you stop*).

The question list is per batch; the QA log, `ai/decisions.md`, is shared by
all. Each question says which report it is about (id and title), the item,
the choices, and the default taken, and **ends with an example answer**, one
line, `例: ...` in Japanese (`e.g. ...` in English), the answer as the
programmer would type it. For sources that disagree, give one `例:` line for
each choice (`(a)`, `(b)` ...).