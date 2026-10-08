# The fixed parts of the drafting prompts

`tfl_ai_prompt()` and `tfl_ai_repair_prompt()` assemble their text from
these files at call time.  Edit the wording here; the code fills the
`{{name}}` placeholders (the schema, the example, the study's context).

| file | what |
|---|---|
| `system.md` | the frame every task shares: the ground rules and the answer's shape |
| `format-yaml.md`, `format-json.md` | the rules of the answer's format |
| `toc.md`, `figure.md` | one task each: the goal and the instructions (`<!-- system -->`), the user's message in chat mode (`<!-- chat -->`, the user attaches the documents) and in API mode (`<!-- api -->`, the documents' text is in the message) |
| `repair.md` | the turn that sends an answer's problems back |

A change here changes the golden prompts
(`tests/testthat/_snaps/ai-prompt.md`): review it as a snapshot change.
Prompts are English; the model is asked to write its assumptions in the
user's language.
