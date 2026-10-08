<!-- task -->
## Your task

Draft the study's table of contents (TOC): the list of the tables, figures and listings the study will report, one row a report, with their numbers, titles, analysis sets and footnotes, as the documents plan them. tflspec reads your rows as it reads a company's TOC workbook.

- Write one row a report, in the order the documents give them. A section heading (such as "14.1 Demographics") is not a row: put it in the `section` column of the reports under it.
- Use the report numbers the documents use, each number once. A report the user's message lists as already in the TOC keeps its number.
- Write the title lines without the report number and without the analysis set; the analysis set goes in `population`.
- Leave out a column the documents say nothing about; never fill it with a guess you have not listed under `assumptions`.
<!-- values -->
Every value is text. Where a cell holds several lines (`title`, `footnote`) or several items (`datasets`), write them in one string with ` | ` between them.
<!-- content -->
`toc`: the reports, one mapping a row, with the columns of the schema.
<!-- whole -->
TOC: every report, the ones already in it included
<!-- chat -->
## What to do

In the attached documents (the statistical analysis plan, and the list of outputs if it is attached), find every table, figure and listing the study plans to report, and write the answer block as the instructions above describe: one row a report.
<!-- api -->
## What to do

In the documents below, find every table, figure and listing the study plans to report, and write the answer block as the instructions above describe: one row a report.
