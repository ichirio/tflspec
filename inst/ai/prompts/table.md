<!-- task -->
## Your task

Write the report files of some tables of a clinical study: the specification that tflspec turns into the program of each table (an ARD made with cards / cardx, laid out with rtfreporter's plan verbs, written to RTF). The user's message lists the reports, gives the study's files already written (the analysis sets, the defaults every report starts from), the ADaM datasets' variables, and the TOC and the shells -- the mock-up of each table.

For each report, read its shell and write one report file:

- `ard`: the analyses that compute every number of the shell. One analysis for a block of rows that one method makes (the continuous summary of AGE; the counts of SEX, AGEGR1 and RACE; the AEs by SOC and PT). Name the dataset and the analysis set; restrict the records with `where`; make a variable the data do not have with an `analysis_data` and its `derive`. The by variables are the shell's columns (the arms); `overall: true` adds a Total column.
- `codelists`: a variable whose values the shell prints in another wording or order (M -> Male, then F -> Female).
- `content`: the table, one item a call. `table_plan` says what goes across (`cols`: the arm) and down (`rows`); `plan_cells` turns statistics into the shell's cell text with the shell's decimals (`xx.x` is one decimal: `{mean:.1f}`); `plan_labels` writes the row headings as the shell does; `plan_levels` orders the arms and the variables; `plan_col_header` writes the column header lines; `plan_sort`, `plan_total`, `plan_stub`, `plan_blanks` as the shell needs. Add only what the defaults do not already give.
- A table always has its column key: with one arm too, the analyses have `by: [<the arm>]` and `table_plan` has `cols: <the arm>`. An analysis without `by` in such a table is an error (but the arm's own count, the N); write `by: []` for one that has no columns. An overall row of a hierarchical table (`normalize_ard: {overall: Any TEAE}`) needs `args: over_variables = TRUE` on its hierarchical analysis; without it the row is not there.
- `page`: the titles and the footnotes, copied exactly from the TOC and the shell.
- The analysis set and, for a dataset of parameters (PARAMCD), the parameter define the report: take them from the shell (`population`; `where: PARAMCD == ...`, or PARAM in the rows for a table of them all). Never guess them: when the shell does not say them, ask in a question; a report without them is an error.

One TOC row can be several reports, and one shell the layout of several. How a row splits differs by study: parts or phases, subgroups, a parameter each (a lab test, a vital sign), a visit each ... Read it from the TOC row's columns (a flag of the split, the population, the parameter) and from the shell's text (a part's heading, "repeat for each ...", "by <variable>"); a sheet can also hold the parts one under the other, a later one often writing only what differs (the subset, a column). Write one report file for each report the row makes. Its id: the number the shell or the TOC writes for it (the TOC's Table 15.1.5.2 and the shell's part 15.1.5.1.a: Table 15.1.5.2.a -- the TOC's number, the shell's letter); with none, the TOC's number with `.a`, `.b` ... in order. Its title: as the shell or the TOC writes it. Take the layout from the shell (the first part) and apply what each report changes. Write how you split the row as an assumption (`assumption` and `question`).

When the user's message shows reviewed reports of this study, follow them: write a report like them in the same way, item by item.
<!-- values -->
Write each value as YAML: a number as a number, `true` / `false` for a logical, a list as a list, a mapping as a mapping; R code (a condition, a derivation, the arguments of a method) only where the schema says `r`, tagged `!r`. Every name is one the user's message lists: a dataset, a variable, a value of a variable as the data write it, an analysis set's id.
<!-- content -->
`reports`: the reports you make, one line each: `- {id: <id>, title: <title>, split: <how its TOC row is split, or ~>, toc: <the TOC row's number>}`. Then `outputs`: one report file a report (a mapping with the keys of the schema), in that order.
<!-- whole -->
report file of every report asked for
<!-- chat -->
## What to do

Read the TOC and the shell of each report above (or in the attached workbook), and write the answer block as the instructions above describe: one report file a report. Write the assumptions as mappings: `- {output_id: <id>, where: <key>, item: <what>, assumption: <what you took>, question: <what the reviewer should confirm>}`.
<!-- api -->
## What to do

Read the TOC and the shell of each report above and in the documents below, and write the answer block as the instructions above describe: one report file a report. Write the assumptions as mappings: `- {output_id: <id>, where: <key>, item: <what>, assumption: <what you took>, question: <what the reviewer should confirm>}`.
