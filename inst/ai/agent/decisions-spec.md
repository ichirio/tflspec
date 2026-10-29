# Decisions

What this study's reports follow, over the agent's own choices. The agent
reads it before every request, applies what fits, and adds the person's
answers from the chat. One line a decision:

```
- D<n> [<scope>] <topic>: <the decision> (<source>)
```

`scope`: `all`, a dataset (`ADAE`), a TOC section, or report ids separated
by commas. `topic`: the item decided (`population.default`,
`treatment_var.SAF`, `denominator`, `missing`, `subjects_or_events`,
`codes.RACE`, `teae.where` ...). `source`: `chat, <report>`, or `person`.
A line of your own without `[scope]` is `all`; write what the reports
follow (the arms' order, a footnote's wording, a rule for every report).
