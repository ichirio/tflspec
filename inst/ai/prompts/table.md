<!-- task -->
## Your task

Write the report files of some tables of a clinical study: the specification that tflspec turns into the program of each table (an ARD made with cards / cardx, laid out with rtfreporter's plan verbs, written to RTF). The user's message lists the reports, gives the study's files already written (the analysis sets, the defaults every report starts from), the ADaM datasets' variables, and the TOC and the shells -- the mock-up of each table.

For each report, read its shell and write one report file:

- `ard`: the analyses that compute every number of the shell. One analysis for a block of rows that one method makes (the continuous summary of AGE; the counts of SEX, AGEGR1 and RACE; the AEs by SOC and PT). Name the dataset and the analysis set; restrict the records with `where`; make a variable the data do not have with an `analysis_data` and its `derive`. The by variables are the shell's columns (the arms); `overall: true` adds a Total column.
- `codelists`: a variable whose values the shell prints in another wording or order (M -> Male, then F -> Female).
- `content`: the table, one item a call. `table_plan` says what goes across (`cols`: the arm) and down (`rows`); `plan_cells` turns statistics into the shell's cell text with the shell's decimals (`xx.x` is one decimal: `{mean:.1f}`); `plan_labels` writes the row headings as the shell does; `plan_levels` orders the arms and the variables; `plan_col_header` writes the column header lines; `plan_sort`, `plan_total`, `plan_stub`, `plan_blanks` as the shell needs. Add only what the defaults do not already give.
- A table always has its column key: with one arm too, the analyses have `by: [<the arm>]` and `table_plan` has `cols: <the arm>`. An overall row of a hierarchical table (`normalize_ard: {overall: Any TEAE}`) needs `args: over_variables = TRUE` on its hierarchical analysis; without it the row is not there.
- `page`: the titles and the footnotes, copied exactly from the TOC and the shell.

One sheet can hold several reports of the same shape (Part 1 / Part 2, Phase 1 / Phase 2 ...; a later one often writes only what differs: the subset, a column). When you find them, write one report file a part. Its id is the report's number as the TOC writes it with the part's letter as the shell writes it (the TOC's Table 15.1.5.2, the shell's 15.1.5.1.a: Table 15.1.5.2.a); with no letter in the shell, `.a`, `.b` in the parts' order. Its title is the part's title as the shell writes it. Take the layout from the first part and apply what a later part changes. Write how you split the sheet as an assumption (`assumption` and `question`).

When the user's message shows reviewed reports of this study, follow them: write a report like them in the same way, item by item.
<!-- values -->
Write each value as YAML: a number as a number, `true` / `false` for a logical, a list as a list, a mapping as a mapping; R code (a condition, a derivation, the arguments of a method) only where the schema says `r`, tagged `!r`. Every name is one the user's message lists: a dataset, a variable, a value of a variable as the data write it, an analysis set's id.
<!-- content -->
`outputs`: the reports, one report file each (a mapping with the keys of the schema), in the order of the user's message.
<!-- whole -->
report file of every report asked for
<!-- chat -->
## What to do

Read the TOC and the shell of each report above (or in the attached workbook), and write the answer block as the instructions above describe: one report file a report. Write the assumptions as mappings: `- {output_id: <id>, where: <key>, item: <what>, assumption: <what you took>, question: <what the reviewer should confirm>}`.
<!-- api -->
## What to do

Read the TOC and the shell of each report above and in the documents below, and write the answer block as the instructions above describe: one report file a report. Write the assumptions as mappings: `- {output_id: <id>, where: <key>, item: <what>, assumption: <what you took>, question: <what the reviewer should confirm>}`.
