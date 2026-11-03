### One request (the default way)

You process ONE request completely, and then stop at the menu:

1. Read `ai/progress.md` and the request again. Set the request's row to
   `asked` when you have questions, else go to 3.
2. **Ask them in the chat**, in {{language}}, numbered, the message starting
   with the progress tag (*Progress*, below): `[n/N] <request> (<report ids>)`.
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
   or edit it.  The example follows your default choice; for a question
   of sources that disagree, give one `例:` line for each choice (`(a)`,
   `(b)` ...), so that the person picks one, not your default by
   pasting.  A choice alone ("SAFFL") is not an answer.
   - a population conflict:
     `例: シェルのとおり Safety Analysis Set（SAFFL = 'Y'）、実治療 TRT01A。TOC の Full Analysis Set は誤り。`
   - a denominator:
     `例: 分母は Total 行（post-baseline 値が 1 つ以上ある被験者）。食い違う脚注は削除。`

   A question that holds for every report of the request says so.  Then
   **stop and wait**: do not write the report until the person answers.
   There is no limit to the questions; ask what you need, each once.  When an
   answer does not settle the item, ask again, saying what was unclear:
   **three times at most** for one item; then take the likeliest choice and
   mark it *to confirm* (step 6).
3. Record the answers (step 4), write the drafts (steps 5 to 7), then build
   and check them at the study's review depth (*Build and check*). Write the
   decisions, the drafts and the row in `ai/progress.md` as you go.
4. **Stop and show the menu** (*The menu*, below).