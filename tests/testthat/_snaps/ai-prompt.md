# golden prompts: toc and figure, chat and api

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

---

    Code
      cat(format(tfl_ai_prompt("figure", ai_figure_context())))
    Output
      You are helping a statistical programmer draft the specification of a clinical study's tables, listings and figures. The specification is read by tflspec, an R package that turns it into report programs. tflspec is new and is probably not in your training data: rely on this message, not on memory.
      
      ## Your task
      
      Draft the design of one figure: the description that tflspec turns into a ggplot2 script. A design has four parts:
      
      - `data`: the steps from the ADaM datasets to the figure's data `df` (read a dataset, keep a parameter and an analysis set, join variables, derive one ...);
      - `stats`: what is computed from `df` (a Kaplan-Meier fit, summary statistics by visit ...), each with a `name` the layers refer to;
      - `plot`: the figure-wide settings (title, axes, colours, legend, size);
      - `layers`: what is drawn, in the order it is drawn.
      
      Start from the template that fits the figure best (name it in `template`; its pieces are good defaults), then change what the documents ask for. When the user's message shows the current design, change that one and keep what the documents do not contradict. Write R code (`data_code`, `stats_code`, `layer_code`) only where no piece does the job.
      
      ## Ground rules
      
      1. Use only the names this message gives: the columns and fields of the schema below, and the reports, datasets and variables listed in the user's message. A column or field the schema does not list is an error; it is never ignored.
      2. Do not invent. When a document asks for something the context does not list, or two documents disagree, take the closest thing that is listed or leave the item out, and say so under `assumptions`. Never make up a name.
      3. Write each value in its field's kind: a number as a number, `true` / `false` for a logical, anything else as text. A field of kind `variables` or `values` takes several items in one string with ` | ` between them. An R expression is text.
      4. Your answer is a draft. A person reviews it item by item before anything is applied, so state every guess rather than hiding it.
      
      ## Your answer
      
      Return exactly one fenced code block (```yaml). Anything outside the block is not read; keep any remarks short.
      
      - The block starts with the header `tflspec_ai: {task: figure, version: 1, output_id: F-14-2-1}`.
      - Then the design: `template`, then `data`, `stats` and `layers` as lists of pieces (each piece a mapping that starts with its `step` or `layer`) and `plot` as one mapping.
      - Return the whole design, not only what you added or changed.
      - End with `assumptions`: one short line for each guess you made and each conflict between documents, or an empty list when there are none. Write the assumptions in the language of the user's message.
      
      Write the block in YAML:
      
      - Write a list of rows or pieces one item a line, as a flow mapping: `- {name: value, name: value}`.
      - Put every text in double quotes when it contains any of `{ } [ ] : , # & * ! | > ' " %` or starts or ends with a space. A template such as `"{n} ({p}%)"` must be quoted, or YAML reads it as a mapping.
      - Inside double quotes, write a line break as `\n` and a double quote as `\"`.
      - Use spaces, never tabs.
      
      ## Schema
      
      A field marked * is required. Leave out a field to take its default.
      
      ### template
      
      The template the design starts from (a note: the pieces are what count). One of:
      
      - `km_risk_table`: KM curves + number at risk (reads ADTTE)
      - `km_simple`: KM curves (reads ADTTE)
      - `km_ci`: KM curves + confidence bands + number at risk (reads ADTTE)
      - `km_single_arm`: One KM curve + number at risk (reads ADTTE)
      - `waterfall_response`: Waterfall, bars by best response (reads ADTR + ADRS)
      - `waterfall_plain`: Waterfall (reads ADTR)
      - `swimmer_bar`: Swimmer: bars + ongoing arrows (reads ADSL)
      - `swimmer_response`: Swimmer: bars by best response + ongoing arrows (reads ADSL + ADRS)
      - `swimmer_assessment`: Swimmer: bars by best response + response at each assessment (reads ADSL + ADRS)
      - `swimmer_full`: Swimmer: bars, assessments, event markers, ongoing arrows (reads ADSL + ADRS)
      - `individual_spider`: Spider: % change in tumour size per subject, by best response (reads ADTR + ADRS)
      - `bar_rate_ci`: Response rate by group with 95% CI (reads ADRS + ADSL)
      - `bar_stacked`: 100% stacked bars of a category by group (reads ADRS + ADSL)
      - `bar_dodged`: Percent per category, groups side by side (reads ADRS + ADSL)
      - `mean_se`: Mean +/- SE by visit (reads ADLB / ADVS + ADSL)
      - `mean_sd`: Mean +/- SD by visit (reads ADLB / ADVS + ADSL)
      - `mean_ci`: Mean (95% CI) by visit (reads ADLB / ADVS + ADSL)
      - `mean_se_n`: Mean +/- SE by visit + n (reads ADLB / ADVS + ADSL)
      - `individual_spaghetti`: Spaghetti: one line per subject + group means (reads ADLB / ADVS + ADSL)
      - `box_by_visit`: Box plots by visit and group + mean marker (reads ADLB / ADVS + ADSL)
      - `box_by_group`: Box plot per group at one visit + points + mean marker (reads ADLB / ADVS + ADSL)
      - `box_change`: Box plots of change from baseline by visit + zero line (reads ADLB / ADVS + ADSL)
      - `scatter_shift`: Baseline vs post-baseline at one visit + identity line (reads ADLB / ADVS + ADSL)
      - `scatter_xy`: Two variables with a linear fit per group (reads ADLB / ADVS + ADSL)
      - `pk_mean`: PK: mean +/- SD concentration by nominal time (reads ADPC + ADSL)
      - `pk_mean_log`: PK: mean +/- SD concentration, log axis (reads ADPC + ADSL)
      - `pk_individual`: PK: individual profiles, log axis, one panel per group (reads ADPC + ADSL)
      
      ### data
      
      The steps from the ADaM datasets to the figure's data `df`, in order; each piece starts with `step`.
      
      - `read`: Read a dataset. The dataset the figure starts from: `df`. Fields:
        - `dataset`* (dataset; default `ADSL`)
      - `join`: Join variables. Variables of another dataset (e.g. ADSL's treatment, ADRS's best response), one row a subject. Fields:
        - `dataset`* (dataset; default `ADSL`)
        - `where` (expr): e.g. PARAMCD == "BOR".
        - `vars`* (variables): NAME = VAR renames, e.g. BOR = AVALC.
        - `by` (variable; default `USUBJID`)
      - `param`: Keep a parameter. The rows of one (or more) PARAMCD. Fields:
        - `value`* (param)
        - `variable` (variable; default `PARAMCD`)
      - `flag`: Keep an analysis set. The rows whose flag is "Y" (FASFL, SAFFL, ANL01FL ...). Fields:
        - `variable`* (flag; default `SAFFL`)
        - `value` (text; default `Y`)
      - `filter`: Keep rows (condition). Any condition, in R. Fields:
        - `expr`* (expr): e.g. AVISITN > 0 & !is.na(AVAL)
      - `derive`: Derive a variable. A new (or changed) variable, in R. Fields:
        - `variable`* (text)
        - `expr`* (expr): e.g. AVAL / 7.
      - `time_unit`: Change a time's unit. A time in days, shown in days (as it is), weeks, months or years. Fields:
        - `variable`* (variable; default `AVAL`)
        - `unit` (choice; one of days | weeks | months | years; default `months`)
      - `levels`: Order a variable's values. The order of groups or visits on the axis and in the legend: by another variable (AVISIT by AVISITN), or listed. Fields:
        - `variable`* (variable)
        - `order_by` (variable): e.g. AVISITN.
        - `levels` (text): | between them.
        - `labels` (text): | between them.
      - `rank`: Rank rows. A row number after sorting, e.g. the bars of a waterfall. Fields:
        - `by`* (variable; default `AVAL`)
        - `descending` (logical; default `TRUE`)
        - `variable` (text; default `INDEX`)
      - `data_code`: R code. What no step does: code that changes `df` (the datasets are there by their lower-case names). Fields:
        - `code`* (code)
      
      ### stats
      
      What is computed from `df`, each with the `name` the layers refer to; each piece starts with `step`.
      
      - `survfit`: Kaplan-Meier fit. survfit2(Surv(time, censor == 0) ~ group). Fields:
        - `name` (text; default `fit`)
        - `time`* (variable; default `AVAL`)
        - `censor`* (variable; default `CNSR`)
        - `by` (variable): Empty = one curve.
        - `conf_type` (choice; one of log | log-log | plain; default `log`)
      - `summary`: Summary statistics. n, mean, SD, SE and an interval (lo, hi) of a value, by group and visit. Fields:
        - `name` (text; default `sm`)
        - `value`* (variable; default `AVAL`)
        - `by`* (variables)
        - `interval` (choice; one of se | sd | ci; default `se`)
        - `positive` (logical; default `FALSE`): For a log axis: lo is left blank where it would be <= 0.
      - `summary_by`: Summary by group. n, mean, SD, SE and an interval of a value by group only (one row a group): a mean marker per box ... Fields:
        - `name` (text; default `sg`)
        - `value`* (variable; default `AVAL`)
        - `by`* (variables)
        - `interval` (choice; one of se | sd | ci; default `se`)
      - `rate`: Rate with 95% CI. Responders / n by group with the exact binomial interval: rate, lcl, ucl (%), and a label 'rate (x/n)'. Fields:
        - `name` (text; default `rt`)
        - `category`* (variable; default `AVALC`)
        - `responders`* (text; default `CR, PR`)
        - `by`* (variables)
      - `count`: Counts and percents. n and % of each category within each group: n, pct, and a label 'pct%'. Fields:
        - `name` (text; default `ct`)
        - `category`* (variable; default `AVALC`)
        - `by`* (variables)
        - `levels` (text): | between them; others follow.
      - `subset`: Another dataset (as an object). Rows of another dataset (or of `df`), by name, for layers that draw them: the assessments of a swimmer plot, the ongoing subjects ... Fields:
        - `name`* (text)
        - `dataset`* (dataset): or df.
        - `where` (expr)
        - `from_df` (variables): Joined by the key: the y position of its subject, a colour ...
        - `by` (variable; default `USUBJID`)
      - `stats_code`: R code. Code that computes what the layers draw, from `df`. Fields:
        - `code`* (code)
      
      ### plot
      
      One mapping of the figure-wide settings; `add` is a list of calls written after them.
      
      - Fields:
        - `title` (text)
        - `x_label` (text)
        - `y_label` (text)
        - `x_min` (number)
        - `x_max` (number)
        - `x_by` (number)
        - `x_text` (logical; default `TRUE`)
        - `y_min` (number)
        - `y_max` (number)
        - `y_by` (number)
        - `x_log` (logical; default `FALSE`)
        - `y_log` (logical; default `FALSE`)
        - `equal` (logical; default `FALSE`): The same range on both axes, e.g. baseline vs post-baseline; needs X and Y min / max.
        - `facet_by` (variable): A variable: one panel for each of its values.
        - `colour_by` (variable): The variable the palette's colours go to (groups, responses).
        - `palette` (choice; one of treatment | response | response_assessment | response_light | okabe_ito | grey; default `treatment`)
        - `dodge` (number; default `0.3`)
        - `theme` (choice; one of boxed | L_axis | minimal | classic; default `boxed`)
        - `base_size` (number; default `10`)
        - `legend` (choice; one of none | right | bottom | top | inside | inside_tl | inside_br | inside_bl; default `bottom`)
        - `width` (number; default `7.5`)
        - `height` (number; default `4.5`)
        - `units` (choice; one of in | cm | px; default `in`)
        - `dpi` (number; default `300`)
        - `add` (pieces): Any ggplot2/extension call, after the figure's settings and before any panels.
      - Each call of `add`: Fields:
        - `fn`* (text): e.g. theme, facet_grid, scale_y_log10, or pkg::fn.
        - `package` (text): Defaults to the search order ggplot2 -> ggsurvfit -> patchwork.
        - `data` (object)
        - `aes` (raw)
        - `pos` (raw)
        - `args` (raw)
      
      ### layers
      
      What is drawn, in order; each piece starts with `layer`.
      
      - `km_curve`: KM curves. The Kaplan-Meier curves (ggsurvfit); the first layer. Fields:
        - `fit` (object; default `fit`)
        - `linewidth` (number; default `0.3`)
      - `km_ci`: KM confidence bands. The curves' confidence intervals. Fields:
        - `alpha` (number; default `0.2`)
      - `censor_mark`: Censor marks. A mark where a subject is censored. Fields:
        - `shape` (choice; one of circle | square | diamond | triangle | triangle_down | x | plus | dot | solid_square | solid_triangle | star | open_circle; default `x`)
        - `size` (number; default `3`)
        - `stroke` (number; default `0.6`)
      - `ref_label`: Reference line labels. Labels right of the panel at reference lines, e.g. 20% and -30%. Fields:
        - `y`* (values): Several: 20, -30.
        - `label` (text; default `{y}`): {y} = the value, e.g. {y}%.
        - `size` (number; default `3.5`)
      - `risk_table`: Number at risk (panel). The number at risk below the curves, at the x axis's breaks. Fields:
        - `fit` (object; default `fit`)
        - `title` (text; default `Number of Patients at Risk`)
        - `size` (number; default `3`)
        - `height` (number; default `0.167`)
      - `n_table`: n by visit (panel). The n of each group at each x, below the plot. Fields:
        - `data` (object; default `sm`)
        - `x`* (variable)
        - `group`* (variable)
        - `label` (variable; default `n`)
        - `title` (text; default `n`)
        - `height` (number; default `0.18`)
      - `geom`: Any ggplot2 layer. Any geom or stat by name, with its aesthetics and settings. Fields:
        - `geom`* (text; default `geom_point`): e.g. geom_area, stat_ecdf, ggrepel::geom_text_repel.
        - `data` (object; default `df`)
        - `aes` (named): x = AVAL | y = CHG | colour = TRT01A.
        - `params` (named): alpha = 0.3 | size = 2.
      - `layer_code`: R code. Code that adds to the plot `p` (e.g. p <- p + annotate(...)). Fields:
        - `code`* (code)
      - `call`: Any function (call). p <- p + fn(data, aes(...), args...): any ggplot2 or extension function, checked against its own arguments. Fields:
        - `fn`* (text): e.g. geom_label, add_quantile, or pkg::fn.
        - `package` (text): Defaults to the search order ggplot2 -> ggsurvfit -> patchwork.
        - `data` (object; default `df`)
        - `aes` (raw): x: AVAL | label: n -- values are expressions.
        - `pos` (raw)
        - `args` (raw)
        - `base` (logical; default `FALSE`)
      - `figure`: Whole figure (type). A figure type not yet in parts: its script as tfl_fig_<type>() writes it; the other parts are then unused. Fields:
        - `type`* (choice; one of km | waterfall | swimmer | individual | forest | bar | mean | box | ae_dot | butterfly | edish | pk | scatter | sankey | sunburst)
        - `style` (text)
        - `args` (named)
      - `line`: Lines. Lines through the points of each group. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `colour` (variable)
        - `group` (variable): Empty = the colour.
        - `linetype` (variable)
        - `linewidth` (number; default `0.5`)
        - `alpha` (number)
        - `dodge` (logical; default `FALSE`)
      - `point`: Points. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `colour` (variable)
        - `shape` (shape; default `dot`)
        - `size` (number; default `2`)
        - `alpha` (number)
        - `na.rm` (logical)
        - `group` (variable): For dodging without a colour, e.g. a mean marker per group.
        - `dodge` (logical; default `FALSE`)
      - `errorbar`: Error bars. From ymin to ymax, e.g. the summary's lo and hi. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `ymin`* (variable; default `lo`)
        - `ymax`* (variable; default `hi`)
        - `colour` (variable)
        - `width` (number; default `0.2`)
        - `dodge` (logical; default `FALSE`)
      - `pointrange`: Point and range. A point with its interval. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable; default `mean`)
        - `ymin`* (variable; default `lo`)
        - `ymax`* (variable; default `hi`)
        - `colour` (variable)
        - `dodge` (logical; default `FALSE`)
      - `col`: Bars. Bars of height y. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `fill` (variable)
        - `width` (number; default `0.8`)
        - `position` (expr): stack / fill / position_dodge(width = 0.75); empty = as is.
        - `colour` (text)
        - `dodge` (logical; default `FALSE`)
      - `text`: Text. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `label`* (variable)
        - `colour` (variable)
        - `size` (number; default `3`)
        - `position` (expr): e.g. position_stack(vjust = 0.5), position_dodge(width = 0.75)
        - `vjust` (number): e.g. -0.3 = just above.
        - `dodge` (logical; default `FALSE`)
      - `label`: Text in boxes. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `label`* (variable)
        - `size` (number; default `3`)
        - `dodge` (logical; default `FALSE`)
      - `step`: Steps. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `colour` (variable)
      - `area`: Area. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `fill` (variable)
        - `alpha` (number; default `0.5`)
      - `ribbon`: Ribbon. A band from ymin to ymax. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `ymin`* (variable; default `lo`)
        - `ymax`* (variable; default `hi`)
        - `fill` (variable)
        - `alpha` (number; default `0.2`)
      - `segment`: Segments. Lines from (x, y) to (xend, yend), e.g. the bars of a swimmer plot. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `xend`* (variable)
        - `yend`* (variable)
        - `colour` (variable)
        - `linewidth` (number; default `0.5`)
        - `arrow` (expr): e.g. arrow(length = unit(2, 'mm'))
        - `alpha` (number)
      - `tile`: Tiles. A heat map. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `fill`* (variable)
      - `boxplot`: Box plots. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `fill` (variable)
        - `width` (number; default `0.6`)
        - `outlier.shape` (shape; default `dot`)
        - `alpha` (number)
        - `position` (expr): e.g. position_dodge(width = 0.8)
        - `dodge` (logical; default `FALSE`)
      - `violin`: Violins. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `fill` (variable)
        - `dodge` (logical; default `FALSE`)
      - `histogram`: Histogram. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `fill` (variable)
        - `bins` (number; default `30`)
      - `density`: Density. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `colour` (variable)
      - `jitter`: Jittered points. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `colour` (variable)
        - `width` (number; default `0.2`)
        - `height` (number; default `0`)
        - `alpha` (number)
        - `size` (number; default `1.2`)
      - `smooth`: Smoothed line. A fitted line and its band. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `colour` (variable)
        - `method` (choice; one of lm | loess | glm | gam; default `lm`)
        - `se` (logical; default `TRUE`)
      - `hline`: Horizontal lines. Reference lines, e.g. y = 0.5 (median), 0, or 20 and -30. Fields:
        - `yintercept`* (values; default `0`): Several: 20, -30.
        - `linetype` (choice; one of solid | dashed | dotted | dotdash | longdash | twodash; default `dashed`)
        - `colour` (text; default `grey50`)
        - `linewidth` (number; default `0.3`)
      - `vline`: Vertical lines. Fields:
        - `xintercept`* (values; default `0`): Several: 1, 2.
        - `linetype` (choice; one of solid | dashed | dotted | dotdash | longdash | twodash; default `dashed`)
        - `colour` (text; default `grey50`)
        - `linewidth` (number; default `0.3`)
      - `abline`: Diagonal line. Fields:
        - `intercept` (number; default `0`)
        - `slope` (number; default `1`)
        - `linetype` (choice; one of solid | dashed | dotted | dotdash | longdash | twodash; default `dashed`)
        - `colour` (text; default `grey50`)
      - `text_repel`: Text that avoids overlaps. Labels pushed apart (ggrepel). Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `label`* (variable)
        - `colour` (variable)
        - `size` (number; default `3`)
      - `sina`: Sina plot. Points spread by their density, e.g. a distribution per group (instead of box plot + jitter). Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `colour` (variable)
        - `size` (number; default `1.2`)
        - `alpha` (number)
      
      ### The kinds of value
      
      - dataset: a dataset's name
      - variable: a variable's name
      - variables: variable names, ` | ` between them
      - flag: a flag variable's name (FASFL, SAFFL ...)
      - param: a PARAMCD value
      - object: `df` or the `name` of a statistics piece
      - choice: one of the values listed
      - number: a number
      - logical: true or false
      - values: numbers, ` | ` between them
      - text: text
      - expr: an R expression, as text
      - code: R code, as text
      - named: `name = value` pairs, ` | ` between them
      - raw: a YAML mapping or list, as the help says
      - pieces: a list of calls (`fn`, `args` ...)
      - shape: a point shape, one of circle | square | diamond | triangle | triangle_down | x | plus | dot | solid_square | solid_triangle | star | open_circle
      
      ## Example
      
      An answer from a sample study, not yours: it shows the shape, not the content.
      
      ```yaml
      tflspec_ai: {task: figure, version: 1, output_id: F-14-2-3}
      template: km_risk_table
      data:
      - {step: read, dataset: ADTTE}
      - {step: param, value: OS}
      - {step: flag, variable: FASFL}
      - {step: time_unit, variable: AVAL, unit: months}
      stats:
      - {step: survfit, name: fit, time: AVAL, censor: CNSR, by: TRT01P}
      plot:
        title: Kaplan-Meier Plot of Overall Survival
        x_label: Time (Months)
        y_label: Survival Probability
        colour_by: TRT01P
        palette: treatment
        legend: inside
        x_min: 0
        y_min: 0
        y_max: 1
        y_by: 0.2
        width: 8.33
        height: 4.79
        dpi: 300
        units: in
        base_size: 10
        theme: boxed
      layers:
      - {layer: km_curve, linewidth: 0.3}
      - {layer: censor_mark, shape: x, size: 3, stroke: 0.6}
      - {layer: hline, yintercept: 0.5, linetype: twodash, colour: grey50, linewidth: 0.3}
      - {layer: risk_table, title: Number of Patients at Risk, size: 3, height: 0.167}
      assumptions:
      - "The SAP gives the time axis in months; the shell's ticks every 6 months are not set (the default breaks are used)."
      - "The parameter is OS as in the SAP's section 9.4."
      ```
      
      ---
      
      ## The study
      
      CDISCPILOT01
      
      ## The report
      
      - Report: F-14-2-1
      - Titles: Figure 14.2.1 | Kaplan-Meier Plot of Overall Survival
      - Analysis set: FAS, Full Analysis Set (`FASFL == "Y"`)
      
      ## The datasets
      
      Name only these datasets and variables.
      
      ### ADSL
      
      | variable | label | kind |
      |---|---|---|
      | STUDYID |  | character |
      | USUBJID | Unique Subject Identifier | character |
      | SUBJID | Subject Identifier for the Study | character |
      | TRT01P | Planned Treatment for Period 01 | character |
      | TRT01PN | Planned Treatment for Period 01 (N) | numeric |
      | FASFL | Full Analysis Set Population Flag | character |
      | SAFFL | Safety Population Flag | character |
      | TRTDURD | Total Treatment Duration (Days) | numeric |
      | EOSSTT | End of Study Status | character |
      | DTHADY | Relative Day of Death | numeric |
      | NACTDY | Study Day of Subsequent Anti-Cancer Therapy | numeric |
      | EOSDY | Study Day of End of Study | numeric |
      | SEX | Sex | character |
      | AGEGR1 | Pooled Age Group 1 | character |
      | TRT01A | Actual Treatment for Period 01 | character |
      | RANDDY |  | numeric |
      | TRTSDY | Treatment Start Day (from Randomization) | numeric |
      | TRTEDY | Treatment End Day (from Randomization) | numeric |
      
      ### ADTTE
      
      | variable | label | kind |
      |---|---|---|
      | USUBJID |  | character |
      | TRT01P |  | character |
      | TRT01PN |  | numeric |
      | FASFL | Full Analysis Set Population Flag | character |
      | PARAMCD |  | character |
      | PARAM |  | character |
      | AVAL | Analysis Value | numeric |
      | CNSR | Censor | numeric |
      | SEX |  | character |
      | AGEGR1 |  | character |
      
      ## The current design
      
      There is none yet: start from the template that fits best.
      
      ## What to do
      
      In the attached statistical analysis plan, find what it says about this figure: the analysis set, the parameter, the groups, the time axis and what is estimated. In the attached shell (mock-up) of the figure, if there is one, read the axes, their labels and ranges, the legend and any panels below the plot. Then write the answer block as the instructions above describe.
      
      Attached: the statistical analysis plan (SAP); the shell (mock-up) of F-14-2-1, if there is one.

---

    Code
      cat(format(tfl_ai_prompt("figure", ai_figure_context(design), mode = "api",
      example = FALSE, documents = list(name = "SAP", section = "9.4", text = "Overall survival is shown by Kaplan-Meier curves by arm."))))
    Output
      You are helping a statistical programmer draft the specification of a clinical study's tables, listings and figures. The specification is read by tflspec, an R package that turns it into report programs. tflspec is new and is probably not in your training data: rely on this message, not on memory.
      
      ## Your task
      
      Draft the design of one figure: the description that tflspec turns into a ggplot2 script. A design has four parts:
      
      - `data`: the steps from the ADaM datasets to the figure's data `df` (read a dataset, keep a parameter and an analysis set, join variables, derive one ...);
      - `stats`: what is computed from `df` (a Kaplan-Meier fit, summary statistics by visit ...), each with a `name` the layers refer to;
      - `plot`: the figure-wide settings (title, axes, colours, legend, size);
      - `layers`: what is drawn, in the order it is drawn.
      
      Start from the template that fits the figure best (name it in `template`; its pieces are good defaults), then change what the documents ask for. When the user's message shows the current design, change that one and keep what the documents do not contradict. Write R code (`data_code`, `stats_code`, `layer_code`) only where no piece does the job.
      
      ## Ground rules
      
      1. Use only the names this message gives: the columns and fields of the schema below, and the reports, datasets and variables listed in the user's message. A column or field the schema does not list is an error; it is never ignored.
      2. Do not invent. When a document asks for something the context does not list, or two documents disagree, take the closest thing that is listed or leave the item out, and say so under `assumptions`. Never make up a name.
      3. Write each value in its field's kind: a number as a number, `true` / `false` for a logical, anything else as text. A field of kind `variables` or `values` takes several items in one string with ` | ` between them. An R expression is text.
      4. Your answer is a draft. A person reviews it item by item before anything is applied, so state every guess rather than hiding it.
      
      ## Your answer
      
      Return exactly one fenced code block (```yaml). Anything outside the block is not read; keep any remarks short.
      
      - The block starts with the header `tflspec_ai: {task: figure, version: 1, output_id: F-14-2-1}`.
      - Then the design: `template`, then `data`, `stats` and `layers` as lists of pieces (each piece a mapping that starts with its `step` or `layer`) and `plot` as one mapping.
      - Return the whole design, not only what you added or changed.
      - End with `assumptions`: one short line for each guess you made and each conflict between documents, or an empty list when there are none. Write the assumptions in the language of the user's message.
      
      Write the block in YAML:
      
      - Write a list of rows or pieces one item a line, as a flow mapping: `- {name: value, name: value}`.
      - Put every text in double quotes when it contains any of `{ } [ ] : , # & * ! | > ' " %` or starts or ends with a space. A template such as `"{n} ({p}%)"` must be quoted, or YAML reads it as a mapping.
      - Inside double quotes, write a line break as `\n` and a double quote as `\"`.
      - Use spaces, never tabs.
      
      ## Schema
      
      A field marked * is required. Leave out a field to take its default.
      
      ### template
      
      The template the design starts from (a note: the pieces are what count). One of:
      
      - `km_risk_table`: KM curves + number at risk (reads ADTTE)
      - `km_simple`: KM curves (reads ADTTE)
      - `km_ci`: KM curves + confidence bands + number at risk (reads ADTTE)
      - `km_single_arm`: One KM curve + number at risk (reads ADTTE)
      - `waterfall_response`: Waterfall, bars by best response (reads ADTR + ADRS)
      - `waterfall_plain`: Waterfall (reads ADTR)
      - `swimmer_bar`: Swimmer: bars + ongoing arrows (reads ADSL)
      - `swimmer_response`: Swimmer: bars by best response + ongoing arrows (reads ADSL + ADRS)
      - `swimmer_assessment`: Swimmer: bars by best response + response at each assessment (reads ADSL + ADRS)
      - `swimmer_full`: Swimmer: bars, assessments, event markers, ongoing arrows (reads ADSL + ADRS)
      - `individual_spider`: Spider: % change in tumour size per subject, by best response (reads ADTR + ADRS)
      - `bar_rate_ci`: Response rate by group with 95% CI (reads ADRS + ADSL)
      - `bar_stacked`: 100% stacked bars of a category by group (reads ADRS + ADSL)
      - `bar_dodged`: Percent per category, groups side by side (reads ADRS + ADSL)
      - `mean_se`: Mean +/- SE by visit (reads ADLB / ADVS + ADSL)
      - `mean_sd`: Mean +/- SD by visit (reads ADLB / ADVS + ADSL)
      - `mean_ci`: Mean (95% CI) by visit (reads ADLB / ADVS + ADSL)
      - `mean_se_n`: Mean +/- SE by visit + n (reads ADLB / ADVS + ADSL)
      - `individual_spaghetti`: Spaghetti: one line per subject + group means (reads ADLB / ADVS + ADSL)
      - `box_by_visit`: Box plots by visit and group + mean marker (reads ADLB / ADVS + ADSL)
      - `box_by_group`: Box plot per group at one visit + points + mean marker (reads ADLB / ADVS + ADSL)
      - `box_change`: Box plots of change from baseline by visit + zero line (reads ADLB / ADVS + ADSL)
      - `scatter_shift`: Baseline vs post-baseline at one visit + identity line (reads ADLB / ADVS + ADSL)
      - `scatter_xy`: Two variables with a linear fit per group (reads ADLB / ADVS + ADSL)
      - `pk_mean`: PK: mean +/- SD concentration by nominal time (reads ADPC + ADSL)
      - `pk_mean_log`: PK: mean +/- SD concentration, log axis (reads ADPC + ADSL)
      - `pk_individual`: PK: individual profiles, log axis, one panel per group (reads ADPC + ADSL)
      
      ### data
      
      The steps from the ADaM datasets to the figure's data `df`, in order; each piece starts with `step`.
      
      - `read`: Read a dataset. The dataset the figure starts from: `df`. Fields:
        - `dataset`* (dataset; default `ADSL`)
      - `join`: Join variables. Variables of another dataset (e.g. ADSL's treatment, ADRS's best response), one row a subject. Fields:
        - `dataset`* (dataset; default `ADSL`)
        - `where` (expr): e.g. PARAMCD == "BOR".
        - `vars`* (variables): NAME = VAR renames, e.g. BOR = AVALC.
        - `by` (variable; default `USUBJID`)
      - `param`: Keep a parameter. The rows of one (or more) PARAMCD. Fields:
        - `value`* (param)
        - `variable` (variable; default `PARAMCD`)
      - `flag`: Keep an analysis set. The rows whose flag is "Y" (FASFL, SAFFL, ANL01FL ...). Fields:
        - `variable`* (flag; default `SAFFL`)
        - `value` (text; default `Y`)
      - `filter`: Keep rows (condition). Any condition, in R. Fields:
        - `expr`* (expr): e.g. AVISITN > 0 & !is.na(AVAL)
      - `derive`: Derive a variable. A new (or changed) variable, in R. Fields:
        - `variable`* (text)
        - `expr`* (expr): e.g. AVAL / 7.
      - `time_unit`: Change a time's unit. A time in days, shown in days (as it is), weeks, months or years. Fields:
        - `variable`* (variable; default `AVAL`)
        - `unit` (choice; one of days | weeks | months | years; default `months`)
      - `levels`: Order a variable's values. The order of groups or visits on the axis and in the legend: by another variable (AVISIT by AVISITN), or listed. Fields:
        - `variable`* (variable)
        - `order_by` (variable): e.g. AVISITN.
        - `levels` (text): | between them.
        - `labels` (text): | between them.
      - `rank`: Rank rows. A row number after sorting, e.g. the bars of a waterfall. Fields:
        - `by`* (variable; default `AVAL`)
        - `descending` (logical; default `TRUE`)
        - `variable` (text; default `INDEX`)
      - `data_code`: R code. What no step does: code that changes `df` (the datasets are there by their lower-case names). Fields:
        - `code`* (code)
      
      ### stats
      
      What is computed from `df`, each with the `name` the layers refer to; each piece starts with `step`.
      
      - `survfit`: Kaplan-Meier fit. survfit2(Surv(time, censor == 0) ~ group). Fields:
        - `name` (text; default `fit`)
        - `time`* (variable; default `AVAL`)
        - `censor`* (variable; default `CNSR`)
        - `by` (variable): Empty = one curve.
        - `conf_type` (choice; one of log | log-log | plain; default `log`)
      - `summary`: Summary statistics. n, mean, SD, SE and an interval (lo, hi) of a value, by group and visit. Fields:
        - `name` (text; default `sm`)
        - `value`* (variable; default `AVAL`)
        - `by`* (variables)
        - `interval` (choice; one of se | sd | ci; default `se`)
        - `positive` (logical; default `FALSE`): For a log axis: lo is left blank where it would be <= 0.
      - `summary_by`: Summary by group. n, mean, SD, SE and an interval of a value by group only (one row a group): a mean marker per box ... Fields:
        - `name` (text; default `sg`)
        - `value`* (variable; default `AVAL`)
        - `by`* (variables)
        - `interval` (choice; one of se | sd | ci; default `se`)
      - `rate`: Rate with 95% CI. Responders / n by group with the exact binomial interval: rate, lcl, ucl (%), and a label 'rate (x/n)'. Fields:
        - `name` (text; default `rt`)
        - `category`* (variable; default `AVALC`)
        - `responders`* (text; default `CR, PR`)
        - `by`* (variables)
      - `count`: Counts and percents. n and % of each category within each group: n, pct, and a label 'pct%'. Fields:
        - `name` (text; default `ct`)
        - `category`* (variable; default `AVALC`)
        - `by`* (variables)
        - `levels` (text): | between them; others follow.
      - `subset`: Another dataset (as an object). Rows of another dataset (or of `df`), by name, for layers that draw them: the assessments of a swimmer plot, the ongoing subjects ... Fields:
        - `name`* (text)
        - `dataset`* (dataset): or df.
        - `where` (expr)
        - `from_df` (variables): Joined by the key: the y position of its subject, a colour ...
        - `by` (variable; default `USUBJID`)
      - `stats_code`: R code. Code that computes what the layers draw, from `df`. Fields:
        - `code`* (code)
      
      ### plot
      
      One mapping of the figure-wide settings; `add` is a list of calls written after them.
      
      - Fields:
        - `title` (text)
        - `x_label` (text)
        - `y_label` (text)
        - `x_min` (number)
        - `x_max` (number)
        - `x_by` (number)
        - `x_text` (logical; default `TRUE`)
        - `y_min` (number)
        - `y_max` (number)
        - `y_by` (number)
        - `x_log` (logical; default `FALSE`)
        - `y_log` (logical; default `FALSE`)
        - `equal` (logical; default `FALSE`): The same range on both axes, e.g. baseline vs post-baseline; needs X and Y min / max.
        - `facet_by` (variable): A variable: one panel for each of its values.
        - `colour_by` (variable): The variable the palette's colours go to (groups, responses).
        - `palette` (choice; one of treatment | response | response_assessment | response_light | okabe_ito | grey; default `treatment`)
        - `dodge` (number; default `0.3`)
        - `theme` (choice; one of boxed | L_axis | minimal | classic; default `boxed`)
        - `base_size` (number; default `10`)
        - `legend` (choice; one of none | right | bottom | top | inside | inside_tl | inside_br | inside_bl; default `bottom`)
        - `width` (number; default `7.5`)
        - `height` (number; default `4.5`)
        - `units` (choice; one of in | cm | px; default `in`)
        - `dpi` (number; default `300`)
        - `add` (pieces): Any ggplot2/extension call, after the figure's settings and before any panels.
      - Each call of `add`: Fields:
        - `fn`* (text): e.g. theme, facet_grid, scale_y_log10, or pkg::fn.
        - `package` (text): Defaults to the search order ggplot2 -> ggsurvfit -> patchwork.
        - `data` (object)
        - `aes` (raw)
        - `pos` (raw)
        - `args` (raw)
      
      ### layers
      
      What is drawn, in order; each piece starts with `layer`.
      
      - `km_curve`: KM curves. The Kaplan-Meier curves (ggsurvfit); the first layer. Fields:
        - `fit` (object; default `fit`)
        - `linewidth` (number; default `0.3`)
      - `km_ci`: KM confidence bands. The curves' confidence intervals. Fields:
        - `alpha` (number; default `0.2`)
      - `censor_mark`: Censor marks. A mark where a subject is censored. Fields:
        - `shape` (choice; one of circle | square | diamond | triangle | triangle_down | x | plus | dot | solid_square | solid_triangle | star | open_circle; default `x`)
        - `size` (number; default `3`)
        - `stroke` (number; default `0.6`)
      - `ref_label`: Reference line labels. Labels right of the panel at reference lines, e.g. 20% and -30%. Fields:
        - `y`* (values): Several: 20, -30.
        - `label` (text; default `{y}`): {y} = the value, e.g. {y}%.
        - `size` (number; default `3.5`)
      - `risk_table`: Number at risk (panel). The number at risk below the curves, at the x axis's breaks. Fields:
        - `fit` (object; default `fit`)
        - `title` (text; default `Number of Patients at Risk`)
        - `size` (number; default `3`)
        - `height` (number; default `0.167`)
      - `n_table`: n by visit (panel). The n of each group at each x, below the plot. Fields:
        - `data` (object; default `sm`)
        - `x`* (variable)
        - `group`* (variable)
        - `label` (variable; default `n`)
        - `title` (text; default `n`)
        - `height` (number; default `0.18`)
      - `geom`: Any ggplot2 layer. Any geom or stat by name, with its aesthetics and settings. Fields:
        - `geom`* (text; default `geom_point`): e.g. geom_area, stat_ecdf, ggrepel::geom_text_repel.
        - `data` (object; default `df`)
        - `aes` (named): x = AVAL | y = CHG | colour = TRT01A.
        - `params` (named): alpha = 0.3 | size = 2.
      - `layer_code`: R code. Code that adds to the plot `p` (e.g. p <- p + annotate(...)). Fields:
        - `code`* (code)
      - `call`: Any function (call). p <- p + fn(data, aes(...), args...): any ggplot2 or extension function, checked against its own arguments. Fields:
        - `fn`* (text): e.g. geom_label, add_quantile, or pkg::fn.
        - `package` (text): Defaults to the search order ggplot2 -> ggsurvfit -> patchwork.
        - `data` (object; default `df`)
        - `aes` (raw): x: AVAL | label: n -- values are expressions.
        - `pos` (raw)
        - `args` (raw)
        - `base` (logical; default `FALSE`)
      - `figure`: Whole figure (type). A figure type not yet in parts: its script as tfl_fig_<type>() writes it; the other parts are then unused. Fields:
        - `type`* (choice; one of km | waterfall | swimmer | individual | forest | bar | mean | box | ae_dot | butterfly | edish | pk | scatter | sankey | sunburst)
        - `style` (text)
        - `args` (named)
      - `line`: Lines. Lines through the points of each group. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `colour` (variable)
        - `group` (variable): Empty = the colour.
        - `linetype` (variable)
        - `linewidth` (number; default `0.5`)
        - `alpha` (number)
        - `dodge` (logical; default `FALSE`)
      - `point`: Points. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `colour` (variable)
        - `shape` (shape; default `dot`)
        - `size` (number; default `2`)
        - `alpha` (number)
        - `na.rm` (logical)
        - `group` (variable): For dodging without a colour, e.g. a mean marker per group.
        - `dodge` (logical; default `FALSE`)
      - `errorbar`: Error bars. From ymin to ymax, e.g. the summary's lo and hi. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `ymin`* (variable; default `lo`)
        - `ymax`* (variable; default `hi`)
        - `colour` (variable)
        - `width` (number; default `0.2`)
        - `dodge` (logical; default `FALSE`)
      - `pointrange`: Point and range. A point with its interval. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable; default `mean`)
        - `ymin`* (variable; default `lo`)
        - `ymax`* (variable; default `hi`)
        - `colour` (variable)
        - `dodge` (logical; default `FALSE`)
      - `col`: Bars. Bars of height y. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `fill` (variable)
        - `width` (number; default `0.8`)
        - `position` (expr): stack / fill / position_dodge(width = 0.75); empty = as is.
        - `colour` (text)
        - `dodge` (logical; default `FALSE`)
      - `text`: Text. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `label`* (variable)
        - `colour` (variable)
        - `size` (number; default `3`)
        - `position` (expr): e.g. position_stack(vjust = 0.5), position_dodge(width = 0.75)
        - `vjust` (number): e.g. -0.3 = just above.
        - `dodge` (logical; default `FALSE`)
      - `label`: Text in boxes. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `label`* (variable)
        - `size` (number; default `3`)
        - `dodge` (logical; default `FALSE`)
      - `step`: Steps. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `colour` (variable)
      - `area`: Area. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `fill` (variable)
        - `alpha` (number; default `0.5`)
      - `ribbon`: Ribbon. A band from ymin to ymax. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `ymin`* (variable; default `lo`)
        - `ymax`* (variable; default `hi`)
        - `fill` (variable)
        - `alpha` (number; default `0.2`)
      - `segment`: Segments. Lines from (x, y) to (xend, yend), e.g. the bars of a swimmer plot. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `xend`* (variable)
        - `yend`* (variable)
        - `colour` (variable)
        - `linewidth` (number; default `0.5`)
        - `arrow` (expr): e.g. arrow(length = unit(2, 'mm'))
        - `alpha` (number)
      - `tile`: Tiles. A heat map. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `fill`* (variable)
      - `boxplot`: Box plots. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `fill` (variable)
        - `width` (number; default `0.6`)
        - `outlier.shape` (shape; default `dot`)
        - `alpha` (number)
        - `position` (expr): e.g. position_dodge(width = 0.8)
        - `dodge` (logical; default `FALSE`)
      - `violin`: Violins. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `fill` (variable)
        - `dodge` (logical; default `FALSE`)
      - `histogram`: Histogram. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `fill` (variable)
        - `bins` (number; default `30`)
      - `density`: Density. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `colour` (variable)
      - `jitter`: Jittered points. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `colour` (variable)
        - `width` (number; default `0.2`)
        - `height` (number; default `0`)
        - `alpha` (number)
        - `size` (number; default `1.2`)
      - `smooth`: Smoothed line. A fitted line and its band. Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `colour` (variable)
        - `method` (choice; one of lm | loess | glm | gam; default `lm`)
        - `se` (logical; default `TRUE`)
      - `hline`: Horizontal lines. Reference lines, e.g. y = 0.5 (median), 0, or 20 and -30. Fields:
        - `yintercept`* (values; default `0`): Several: 20, -30.
        - `linetype` (choice; one of solid | dashed | dotted | dotdash | longdash | twodash; default `dashed`)
        - `colour` (text; default `grey50`)
        - `linewidth` (number; default `0.3`)
      - `vline`: Vertical lines. Fields:
        - `xintercept`* (values; default `0`): Several: 1, 2.
        - `linetype` (choice; one of solid | dashed | dotted | dotdash | longdash | twodash; default `dashed`)
        - `colour` (text; default `grey50`)
        - `linewidth` (number; default `0.3`)
      - `abline`: Diagonal line. Fields:
        - `intercept` (number; default `0`)
        - `slope` (number; default `1`)
        - `linetype` (choice; one of solid | dashed | dotted | dotdash | longdash | twodash; default `dashed`)
        - `colour` (text; default `grey50`)
      - `text_repel`: Text that avoids overlaps. Labels pushed apart (ggrepel). Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `label`* (variable)
        - `colour` (variable)
        - `size` (number; default `3`)
      - `sina`: Sina plot. Points spread by their density, e.g. a distribution per group (instead of box plot + jitter). Fields:
        - `data` (object; default `df`)
        - `x`* (variable)
        - `y`* (variable)
        - `colour` (variable)
        - `size` (number; default `1.2`)
        - `alpha` (number)
      
      ### The kinds of value
      
      - dataset: a dataset's name
      - variable: a variable's name
      - variables: variable names, ` | ` between them
      - flag: a flag variable's name (FASFL, SAFFL ...)
      - param: a PARAMCD value
      - object: `df` or the `name` of a statistics piece
      - choice: one of the values listed
      - number: a number
      - logical: true or false
      - values: numbers, ` | ` between them
      - text: text
      - expr: an R expression, as text
      - code: R code, as text
      - named: `name = value` pairs, ` | ` between them
      - raw: a YAML mapping or list, as the help says
      - pieces: a list of calls (`fn`, `args` ...)
      - shape: a point shape, one of circle | square | diamond | triangle | triangle_down | x | plus | dot | solid_square | solid_triangle | star | open_circle
      
      
      
      ---
      
      ## The study
      
      CDISCPILOT01
      
      ## The report
      
      - Report: F-14-2-1
      - Titles: Figure 14.2.1 | Kaplan-Meier Plot of Overall Survival
      - Analysis set: FAS, Full Analysis Set (`FASFL == "Y"`)
      
      ## The datasets
      
      Name only these datasets and variables.
      
      ### ADSL
      
      | variable | label | kind |
      |---|---|---|
      | STUDYID |  | character |
      | USUBJID | Unique Subject Identifier | character |
      | SUBJID | Subject Identifier for the Study | character |
      | TRT01P | Planned Treatment for Period 01 | character |
      | TRT01PN | Planned Treatment for Period 01 (N) | numeric |
      | FASFL | Full Analysis Set Population Flag | character |
      | SAFFL | Safety Population Flag | character |
      | TRTDURD | Total Treatment Duration (Days) | numeric |
      | EOSSTT | End of Study Status | character |
      | DTHADY | Relative Day of Death | numeric |
      | NACTDY | Study Day of Subsequent Anti-Cancer Therapy | numeric |
      | EOSDY | Study Day of End of Study | numeric |
      | SEX | Sex | character |
      | AGEGR1 | Pooled Age Group 1 | character |
      | TRT01A | Actual Treatment for Period 01 | character |
      | RANDDY |  | numeric |
      | TRTSDY | Treatment Start Day (from Randomization) | numeric |
      | TRTEDY | Treatment End Day (from Randomization) | numeric |
      
      ### ADTTE
      
      | variable | label | kind |
      |---|---|---|
      | USUBJID |  | character |
      | TRT01P |  | character |
      | TRT01PN |  | numeric |
      | FASFL | Full Analysis Set Population Flag | character |
      | PARAMCD |  | character |
      | PARAM |  | character |
      | AVAL | Analysis Value | numeric |
      | CNSR | Censor | numeric |
      | SEX |  | character |
      | AGEGR1 |  | character |
      
      ## The current design
      
      Change this design and return it whole:
      
      ```yaml
      template: km_simple
      data:
      - {step: read, dataset: ADTTE}
      - {step: param, value: OS}
      - {step: flag, variable: FASFL}
      - {step: time_unit, variable: AVAL, unit: months}
      stats:
      - {step: survfit, name: fit, time: AVAL, censor: CNSR, by: TRT01P}
      plot:
        x_label: Time (Months)
        y_label: Survival Probability
        colour_by: TRT01P
        palette: treatment
        legend: inside
        x_min: 0
        y_min: 0
        y_max: 1
        y_by: 0.2
        width: 8.33
        height: 4.79
        dpi: 300
        units: in
        base_size: 10
        theme: boxed
      layers:
      - {layer: km_curve, linewidth: 0.3}
      - {layer: censor_mark, shape: x, size: 3, stroke: 0.6}
      ```
      
      ## What to do
      
      In the documents below, find what they say about this figure: the analysis set, the parameter, the groups, the time axis and what is estimated; in a shell (mock-up), the axes, their labels and ranges, the legend and any panels below the plot. Then write the answer block as the instructions above describe.
      
      <document name="SAP" section="9.4">
      Overall survival is shown by Kaplan-Meier curves by arm.
      </document>

