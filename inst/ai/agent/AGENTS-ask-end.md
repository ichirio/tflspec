**Do not ask now: take your default, note the question, go on.** The
   study asks its questions at the end (`study.yml` `ai: ask_when: end`), so
   that the person need not wait at the chat. For each question step 2 leaves
   (what you would ask), do this instead of asking:
   - Take the choice you would give as the default (*Your default choice*,
     above), as if the person had answered "default".
   - Add the question to `ai/questions-end.md` (create it the first time). Use
     one numbered entry, `Q<n>`, numbered across the whole run:
     `Q<n> [<request>] <report ids and titles>: <the question, its choices> --
     default taken: <your choice>` followed by its `例:` line or lines, as in
     the chat form below.
   - Record the default in `ai/decisions.md` as a decision whose source is
     `(default, Q<n>)`, and write the assumption in the draft with
     `from: D<n>` and `question: Q<n>`.
   - **A point with no sensible default** is one where a report cannot be
     written without it: the analysis set, the parameter, or what is counted
     is unknown, or the documents contradict each other with nothing to break
     the tie. Do not write that report. Add the entry as
     `Q<n> BLOCKER [<request>] <report id>: ...`, say it in the progress line,
     and go on with the other reports.
   - **Never stop and wait mid-run.** Go straight on, request after request.

   When every request is done, after *Build and check* and *Before you stop*:
   **ask everything at once.** Post `ai/questions-end.md` in the chat as one
   message, in {{language}}:
   - the blockers first, then the rest by request;
   - each entry with the default you took and its `例:` lines;
   - then **every default you took without a question**: post the first two
     groups of `ai/defaults.md` in full (*Changes the numbers*, *Changes the
     rows or statistics*), and the third (*Changes the text or the look*)
     as its count, with "see ai/defaults.md".  `run.R` writes that file
     from the drafts' assumptions, the most important first; read it before
     you post, and say in a line that **the defaults can differ from run to
     run, so the person should read them too**;
   - at the end: "Answer only what you want changed (Q<n>: ...); the others
     keep their default.  Change a default the same way: `D<n>: ...` or
     `<report> <item>: ...`."

   Then **stop and wait**. When the person answers:
   - For each changed answer, change that decision's line in
     `ai/decisions.md`: the new decision, from `chat`.
   - The affected reports are those whose draft cites that decision
     (`from: D<n>`) or that question (`question: Q<n>`), plus the blockers
     answered. Change only those drafts, and write the blocked reports.
   - Build and check only the requests that hold an affected report (*Build
     and check*).
   - Then say what changed, report by report. Leave every other draft as it
     was.

   The end message's form: each question says which report it is about (id
   and title), the item, the choices, and the default taken. **End every
   question with an example answer**, one line, `例: ...` in Japanese
   (`e.g. ...` in English), the answer as the programmer would type it. For
   sources that disagree, give one `例:` line for each choice (`(a)`, `(b)`
   ...).
