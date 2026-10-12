# golden prompts: toc, chat and api

    Code
      cat(format(tfl_ai_prompt("toc", ai_toc_context())))
    Output
      You are helping a statistical programmer draft the specification of a clinical study's tables, listings and figures. The specification is read by tflspec, an R package that turns it into report programs. tflspec is new and is probably not in your training data: rely on this message, not on memory.
      
      ## Your task
      
      Draft the study's table of contents (TOC): the list of the tables, figures and listings the study will report, one row a report, with their numbers, titles, analysis sets and footnotes, as the documents plan them. tflspec reads your rows as it reads a company's TOC workbook.
      
      - Write one row a report, in the order the documents give them. A section heading (such as "14.1 Demographics") is not a row: put it in the `section` column of the reports under it.
      - Use the report numbers the documents use, each number once. A report the user's message lists as already in the TOC keeps its number.
      - Write the title lines without the report number and without the analysis set; the analysis set goes in `population`.
      - Leave out a column the documents say nothing about; never fill it with a guess you have not listed under `assumptions`.
      
      ## Ground rules
      
      1. Use only the names this message gives: the columns and fields of the schema below, and the reports, datasets and variables listed in the user's message. A column or field the schema does not list is an error; it is never ignored.
      2. Do not invent. When a document asks for something the context does not list, or two documents disagree, take the closest thing that is listed or leave the item out, and say so under `assumptions`. Never make up a name.
      3. Every value is text. Where a cell holds several lines (`title`, `footnote`) or several items (`datasets`), write them in one string with ` | ` between them.
      4. Your answer is a draft. A person reviews it item by item before anything is applied, so state every guess rather than hiding it.
      
      ## Your answer
      
      Return exactly one fenced code block (```yaml). Anything outside the block is not read; keep any remarks short.
      
      - The block starts with the header `tflspec_ai: {task: toc, version: 1}`.
      - Then `toc`: the reports, one mapping a row, with the columns of the schema.
      - Return the whole TOC: every report, the ones already in it included, not only what you added or changed.
      - End with `assumptions`: one short line for each guess you made and each conflict between documents, or an empty list when there are none. Write the assumptions in the language of the user's message.
      
      Write the block in YAML:
      
      - Write a list of rows or pieces one item a line, as a flow mapping: `- {name: value, name: value}`.
      - Put every text in double quotes when it contains any of `{ } [ ] : , # & * ! | > ' " %` or starts or ends with a space. A template such as `"{n} ({p}%)"` must be quoted, or YAML reads it as a mapping.
      - Inside double quotes, write a line break as `\n` and a double quote as `\"`.
      - Use spaces, never tabs.
      
      ## Schema
      
      `toc` is a list of rows, one a report. Its columns:
      
      - `output_id`: The report's number as the documents give it, unique in the TOC (required). Example: `T-14-1-1`
      - `type`: The kind of report: table, figure or listing. Example: `table`
      - `title`: The title lines in order, without the report number and the analysis set; ` | ` between lines. Example: `Summary of Demographic and Baseline Characteristics`
      - `population`: The analysis set the report is on, as the documents name it; it becomes the last title line. Example: `Safety Population`
      - `footnote`: The footnote lines in order; ` | ` between lines. Example: `Percentages are based on the number of subjects in the analysis set.`
      - `program`: The name of the program that makes the report. Example: `t_14_1_1.R`
      - `file`: The name of the report's output file. Example: `t_14_1_1.rtf`
      - `note`: A remark for people; never read by tflspec.
      - `section`: The heading the report is listed under in the documents. Example: `14.1 Demographic and Baseline Data`
      - `datasets`: The ADaM datasets the report reads; ` | ` between them. Example: `ADSL`
      - `label`: The report's number as printed in its title. Example: `Table 14.1.1`
      
      ## Example
      
      An answer from a sample study, not yours: it shows the shape, not the content.
      
      ```yaml
      tflspec_ai: {task: toc, version: 1}
      toc:
      - {output_id: T-14-1-1, type: table, title: Summary of Demographic and Baseline Characteristics, population: Safety Population, footnote: Percentages are based on the number of subjects in each group., section: 14.1 Demographic and Baseline Data, datasets: ADSL, label: Table 14.1.1}
      - {output_id: T-14-3-1, type: table, title: Overview of Treatment-Emergent Adverse Events, population: Safety Population, footnote: "TEAE: treatment-emergent adverse event. | A subject is counted once per row.", section: 14.3 Safety Data, datasets: "ADSL | ADAE", label: Table 14.3.1}
      - {output_id: F-14-2-1, type: figure, title: Kaplan-Meier Plot of Overall Survival, population: Full Analysis Set, section: 14.2 Efficacy Data, datasets: ADTTE, label: Figure 14.2.1}
      assumptions:
      - The SAP names no footnotes for Figure 14.2.1; left blank.
      - Table 14.3.1 is listed in SAP section 10.2 but not in the TOC appendix; kept.
      ```
      
      ---
      
      ## The study
      
      CDISCPILOT01: Safety and Efficacy of the Xanomeline Transdermal Therapeutic System
      
      ## The TOC's columns
      
      Fill these columns (the schema describes them): `output_id`, `type`, `title`, `population`, `footnote`, `section`, `datasets`, `label`.
      
      ## Reports already in the TOC
      
      Keep their numbers; add the reports the documents plan that are not here.
      
      | output_id | title |
      |---|---|
      | AE | Report AE |
      | DM | Report DM |
      | LB | Report LB |
      | ORR | Report ORR |
      | PK | Report PK |
      
      ## What to do
      
      In the attached documents (the statistical analysis plan, and the list of outputs if it is attached), find every table, figure and listing the study plans to report, and write the answer block as the instructions above describe: one row a report.
      
      Attached: the statistical analysis plan (SAP); the study's list of planned outputs, if it is a separate document.

---

    Code
      cat(format(tfl_ai_prompt("toc", ai_toc_context(), mode = "api", format = "json",
      documents = list(list(name = "SAP", kind = "sap", section = "14", text = "14.1.1 Demographics (Safety Population)\n14.2.1 Overall survival (FAS)")))))
    Output
      You are helping a statistical programmer draft the specification of a clinical study's tables, listings and figures. The specification is read by tflspec, an R package that turns it into report programs. tflspec is new and is probably not in your training data: rely on this message, not on memory.
      
      ## Your task
      
      Draft the study's table of contents (TOC): the list of the tables, figures and listings the study will report, one row a report, with their numbers, titles, analysis sets and footnotes, as the documents plan them. tflspec reads your rows as it reads a company's TOC workbook.
      
      - Write one row a report, in the order the documents give them. A section heading (such as "14.1 Demographics") is not a row: put it in the `section` column of the reports under it.
      - Use the report numbers the documents use, each number once. A report the user's message lists as already in the TOC keeps its number.
      - Write the title lines without the report number and without the analysis set; the analysis set goes in `population`.
      - Leave out a column the documents say nothing about; never fill it with a guess you have not listed under `assumptions`.
      
      ## Ground rules
      
      1. Use only the names this message gives: the columns and fields of the schema below, and the reports, datasets and variables listed in the user's message. A column or field the schema does not list is an error; it is never ignored.
      2. Do not invent. When a document asks for something the context does not list, or two documents disagree, take the closest thing that is listed or leave the item out, and say so under `assumptions`. Never make up a name.
      3. Every value is text. Where a cell holds several lines (`title`, `footnote`) or several items (`datasets`), write them in one string with ` | ` between them.
      4. Your answer is a draft. A person reviews it item by item before anything is applied, so state every guess rather than hiding it.
      
      ## Your answer
      
      Return exactly one fenced code block (```json). Anything outside the block is not read; keep any remarks short.
      
      - The block starts with the header `"tflspec_ai": {"task":"toc","version":1}`.
      - Then `toc`: the reports, one mapping a row, with the columns of the schema.
      - Return the whole TOC: every report, the ones already in it included, not only what you added or changed.
      - End with `assumptions`: one short line for each guess you made and each conflict between documents, or an empty list when there are none. Write the assumptions in the language of the user's message.
      
      Write the block in JSON: one object holding the header, the content and `assumptions`. Write text as strings, numbers and `true` / `false` as they are, a list as an array, and nothing JSON does not allow (no comments, no trailing commas).
      
      ## Schema
      
      `toc` is a list of rows, one a report. Its columns:
      
      - `output_id`: The report's number as the documents give it, unique in the TOC (required). Example: `T-14-1-1`
      - `type`: The kind of report: table, figure or listing. Example: `table`
      - `title`: The title lines in order, without the report number and the analysis set; ` | ` between lines. Example: `Summary of Demographic and Baseline Characteristics`
      - `population`: The analysis set the report is on, as the documents name it; it becomes the last title line. Example: `Safety Population`
      - `footnote`: The footnote lines in order; ` | ` between lines. Example: `Percentages are based on the number of subjects in the analysis set.`
      - `program`: The name of the program that makes the report. Example: `t_14_1_1.R`
      - `file`: The name of the report's output file. Example: `t_14_1_1.rtf`
      - `note`: A remark for people; never read by tflspec.
      - `section`: The heading the report is listed under in the documents. Example: `14.1 Demographic and Baseline Data`
      - `datasets`: The ADaM datasets the report reads; ` | ` between them. Example: `ADSL`
      - `label`: The report's number as printed in its title. Example: `Table 14.1.1`
      
      ## Example
      
      An answer from a sample study, not yours: it shows the shape, not the content.
      
      ```json
      {
        "tflspec_ai": {
          "task": "toc",
          "version": 1
        },
        "toc": [
          {
            "output_id": "T-14-1-1",
            "type": "table",
            "title": "Summary of Demographic and Baseline Characteristics",
            "population": "Safety Population",
            "footnote": "Percentages are based on the number of subjects in each group.",
            "section": "14.1 Demographic and Baseline Data",
            "datasets": "ADSL",
            "label": "Table 14.1.1"
          },
          {
            "output_id": "T-14-3-1",
            "type": "table",
            "title": "Overview of Treatment-Emergent Adverse Events",
            "population": "Safety Population",
            "footnote": "TEAE: treatment-emergent adverse event. | A subject is counted once per row.",
            "section": "14.3 Safety Data",
            "datasets": "ADSL | ADAE",
            "label": "Table 14.3.1"
          },
          {
            "output_id": "F-14-2-1",
            "type": "figure",
            "title": "Kaplan-Meier Plot of Overall Survival",
            "population": "Full Analysis Set",
            "section": "14.2 Efficacy Data",
            "datasets": "ADTTE",
            "label": "Figure 14.2.1"
          }
        ],
        "assumptions": ["The SAP names no footnotes for Figure 14.2.1; left blank.", "Table 14.3.1 is listed in SAP section 10.2 but not in the TOC appendix; kept."]
      }
      ```
      
      ---
      
      ## The study
      
      CDISCPILOT01: Safety and Efficacy of the Xanomeline Transdermal Therapeutic System
      
      ## The TOC's columns
      
      Fill these columns (the schema describes them): `output_id`, `type`, `title`, `population`, `footnote`, `section`, `datasets`, `label`.
      
      ## Reports already in the TOC
      
      Keep their numbers; add the reports the documents plan that are not here.
      
      | output_id | title |
      |---|---|
      | AE | Report AE |
      | DM | Report DM |
      | LB | Report LB |
      | ORR | Report ORR |
      | PK | Report PK |
      
      ## What to do
      
      In the documents below, find every table, figure and listing the study plans to report, and write the answer block as the instructions above describe: one row a report.
      
      <document name="SAP" kind="sap" section="14">
      14.1.1 Demographics (Safety Population)
      14.2.1 Overall survival (FAS)
      </document>

