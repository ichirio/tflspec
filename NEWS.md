# tflspec (development version)

* **Fonts per script and their languages** (#449, rtfreporter#715): a font
  table entry's `name` is one family or a family per script slot (`latin`,
  `east_asian`, `complex`, `hansi`), the titles' and footnotes' `font` too;
  `page.rtf_document.font_lang` gives a slot's language (`east_asian: ja`).
  The report's `rtf_document()` call writes them as rtfreporter takes them
  (named vectors), the study setup as `options(rtfreporter.font =,
  rtfreporter.font_lang =)`, and the page sheet's `font` / new `font_lang`
  columns say them as `latin = X | east_asian = Y` -- before, a slotted
  family was lost on the sheets' path.  rtfreporter (>= 0.8.2.9094)
  (0.0.24.9223).

* **The running header's and footer's look** (#450): `page.rtf_header` and
  `page.rtf_footer` take rtfreporter's `rtf_header()` / `rtf_footer()`
  arguments (`border`, `width`, `width_twips`, `row_height_twips`,
  `font_size_half_points`, `font`, `markup`, `cell_padding_*`,
  `drop_empty_rows`; the lines stay `page.header` / `page.footer`), and
  `page.rtf_footnotes` a `border`.  The report's call and the study setup's
  `study_header` / `study_footer` write them; a report whose look differs
  from the study's writes its own call.

* **Page sheets keep the page arguments no column says** (#451): YAML ->
  sheets -> YAML no longer drops a page's other rtfreporter arguments (the
  titles' row height, the running header's look, the figure's align, a
  second font ...): the page sheet's new column `page_more` holds them as
  YAML and they are read back as they were.

* **rtf_figures()'s drawing, the study's rtfreporter options, overwrite**
  (#452): `page.rtf_figures` takes `render_width`, `render_height`,
  `render_dpi` and `png_device` (rtfreporter#719; a drawn figure's
  `rtfplot()` gets the device too); `defaults.yml`'s `options:` names
  rtfreporter's options (`rtfreporter.png_device`,
  `rtfreporter.figure.default_dpi`, `rtfreporter.ard_pipe` ...), which the
  study setup writes as `options()`; `page.generate_rtfreport.overwrite`
  says whether a report's file may be written over, read by the new
  `tfl_report_overwrite()` (and `tfl_ai_run()`).  rtfreporter (>=
  0.8.2.9096).

* **No page argument is dropped any more** (#444): `page.rtf_titles` and
  `page.rtf_footnotes` write `row_height_twips`, `markup`, `font` and
  `align` (not only `font_size_half_points`), `page.rtf_figures` its
  `align`, and `page.rtf_tables` a `font_size_half_points` left on the page
  (a content of R code) -- the schema took them, the program left them out.
  `markup` takes rtfreporter's words (`"script"`, `"relational"`, `"all"`,
  `"none"`), no longer `TRUE` / `FALSE` (0.0.24.9218).

* **A report on the page of the content before it** (#443): a report's
  `page.rtf_tables` says `page_break`, `space_before`, `first_rows` and
  `cont_title`, and `page.rtf_figures` `page_break` and `space_before`, as
  rtfreporter's `rtf_tables()` / `rtf_figures()` take them; the report
  sheet has the same columns.  The generated call passes them only when the
  report is not on a page of its own (`page_break: false`).  They take effect
  in a document that holds content before the report's (0.0.24.9216).

* `tfl_ai_prepare()`'s `toc` (a dry run's too) says each report's `parts`,
  `parts_row` and `same_number`, as `tfl_ai_read_shell()$toc` does: a
  caller need not read the workbook again (0.0.24.9213).

* **The AI requests' defaults** (0.0.24.9212): `size = NULL` holds one
  report a request with `mode = "agent"` (each answer checked and built
  before the next) and, in a chat, 3 (`tfl_ai_prepare()`) or 8
  (`tfl_ai_write_requests()`) as before; `instructions = "once"` is the
  default (a chat's instructions in `00-instructions.md`, its requests
  without them).  With `mode = "agent"` the instructions are always
  written once, `ai/instructions-table.md` / `-figure.md`, whichever
  `instructions` says.

* **A report's parts, and requests written fast** (0.0.24.9211).
  - A report whose shell sheet holds it in parts (`Table 15.1.5.1.a` above,
    `.b` below, the later often only what differs) is found:
    `tfl_ai_read_shell()$toc` says `parts` (the parts' ids, the TOC's number
    with the letter: `t.15.1.5.1.a | t.15.1.5.1.b`) and `parts_row` (the row
    each starts on), and its request asks for a report file a part (the
    layout the first part's).  Answers with the parts' ids are the report's
    (not reports too many), and `spec/toc.yml` lists the parts in its place.
  - A number's part letter after a dot, a dash or an underscore (`.a`, `-a`,
    `_a`) is the number's, as one written straight on (`T15.1.5.1A`); a TOC
    row of a part with no sheet of its own reads its parent's sheet whole.
    A number the TOC names twice warns (`same_number`; its first row kept).
  - The table instructions (`ai/instructions-table.md`, a chat's prompt)
    always say that a sheet can hold a report's parts (Part 1 / Part 2,
    Phase 1 / Phase 2): a report file a part, its id the shell's number or
    the TOC's with `.a`, `.b`, the layout the first part's, the split an
    assumption.  The request adds the parts' rows when they are found.
  - `size = 1`: a request is named after its report's number
    (`ai/requests/t.15.1.5.1.md`).
  - A request's shell is cut from the workbook as it was read, not read
    again from the file a request at a time: 46 reports' requests in about
    30 seconds, where a workbook of many sheets took half an hour.  Each
    request is written as it is made (`index.md` at the end), and
    `progress = function(step, i, n)` on `tfl_ai_prepare()` /
    `tfl_ai_write_requests()` says where the work is (`"adam"`,
    `"workbook"`, `"request"` i of n).

* **The layouts' parameters: what a report changes** (0.0.24.9208).
  `summary` (42 parameters, now 15) and `hierarchical` (33, now 15) keep
  only what one report's table differs from another's in; the look (the
  stub, blank rows, widths, rounding, the column header) and the page are
  the study standard's -- change them after the template, `tfl_tpl(...) |>
  plan_stub(...)` -- and the ARD's ids and labels are the layout's own (the
  template's label).  The layout's page (`summary`: 30 rows, group_safe;
  `hierarchical`: 22, group_force) gives way to a study standard's
  `plan_paginate_rows()`; `hierarchical`'s column header (its n (%) line)
  stays the table's.  Renamed after the parameter naming rules:
  `cat_cell` / `cell` -> `count_cell`, `statistics` ->
  `continuous_statistics`, `cat_statistics` -> `categorical_statistics`,
  `overall` -> `any_row` (TRUE / FALSE) and `any_row_label`.  The study's
  tables are as before.
* **`stf_t02_demog`**: `demog` in the shape of FDA STF IG v2 Table 2 (Sex,
  Age as Mean (SD) and Median (min, max), age groups, race, ethnicity,
  country; a Total column).  It is not endorsed or recommended by FDA.
  `demog`'s label is now "Demographics".

* **`orr(stat_layout = "cols")`: each arm's N over its two columns**
  (0.0.24.9207), as the default layout has it under the arm: rtfreporter
  (>= 0.8.2.9090) reads it over split columns (ichirio/rtfreporter#709).

* **`run.R`'s status of a request not run this time** (0.0.24.9205):
  `Rscript run.R <request>` built that request's reports alone, and
  `ai/status.md` said of another, never built, "the report files are
  written", and of one whose last build failed, nothing.  A report not
  built yet now says so, with its `Rscript run.R` line; a failed build is
  kept in `ai/built.csv` and said again.  A report that could not be built
  has no RTF (or PNG) left of an earlier run: they are removed.

* **The figure templates are called as the tables': `tfl_tpl(<data>,
  "<name>", ...)`** (0.0.24.9204, #430), and a report's program calls it:
  `content <- tfl_tpl(adtte_ttdisc, "km_disc", group = TRT01A)`.  The
  arguments of the data steps (`population`, `param`, `data`, `where`,
  `event`: `tfl_param(stage = "data")`) are the program's, not the call's.
  The figures' own functions (`tpl_km_simple()` ...) stay and draw as before.
  - One Kaplan-Meier layout, `km`, its look as arguments: `y_type`
    (survival or risk, 1 - S(t)), `y_percent`, `ci` (`"band"`, or `"bars"`:
    the CI at the axis's times, `geom_errorbar()`), `censor_marks`,
    `median_line`, `risk_table` (`risk_title`, `risk_stats` ...), `pvalue`,
    `x_max` / `x_by`, the labels, the legend, `event` (CNSR made from a
    condition: ADSL's `EOTSTT == "DISCONTINUED"`), `time_unit` and `plot`
    (any other setting).  The figure style's km values a study changes
    (`censor_shape`, `risk_title` ...) are the study's values of their
    parameters (`tfl_param(study = "km.<key>")`).
  - `km_disc` (FDA STF Figure 1: time to permanent discontinuation, the
    cumulative % in weeks, the CI as bars, the log-rank p-value),
    `km_ae_onset` (Figure 4) and `km_lfu` (Figure 13) are its parameter
    sets; `km_simple`, `km_risk_table` and `km_ci` too, their designs as
    before.
  - The generated code of `.content_fig_tpl_code()` lives in
    `R/tpl_fig_code.R`.


* **A template's help** (#429, 0.0.24.9203): `tfl_tpl_args(name, study,
  population)` is a template's parameters as a table -- tier, the layout's
  default, the default a variant changes, the study standard's value, the
  value in effect and its layer (`"study"`, `"variant:<name>"`,
  `"template"`) -- and `tfl_tpl_help()` prints it by tier.  Each table and
  listing template has its page, `?tfl_tpl_<name>` (`?tfl_tpl_demog`),
  made from the registry when the documentation is built (roxygen
  `@eval`); a test says when one is missing or out of date.

* **`tfl_tpl(x, name, ...)`: one call for every table and listing template**
  (0.0.24.9202), and a report's program calls it: `tfl_tpl(ard, "demog",
  population = "SAF")`.  The template's own functions (`tpl_demog()` ...)
  and `tfl_template_run()` stay.
  - The layouts `summary` and `hierarchical` take a table's whole look as
    parameters (`order`, `total`, `heading`, `header_n`, `rounding`,
    `stub_*`, `blanks*`, `widths`, `max_rows`, `split` ...), each in a tier
    (`tfl_param(tier = "required" / "usual" / "other")`); those whose
    `study` names a study standard's item take its value, and the
    program's call says them, with no `# (study default)` lines after it.
  - `demog` is `summary` with its own defaults (summary's `variables` is
    now `continuous`); `ae_soc_pt` is `hierarchical`'s, and
    `ae_ser_soc_pt` (serious TEAEs, "Any SAE") and `ae_disc_soc_pt` (TEAEs
    leading to discontinuation) are `ae_soc_pt`'s.  `min_pct` on
    `hierarchical`; `stack = TRUE` on `summary`.
  - `tfl_templates()` says each one's `label` and whether the AI kit uses
    it (`ai`; `tfl_template(ai = )`, `tfl_template_variant(ai = , example = )`).

* **`orr(stat_layout = "cols")`: each arm's rate and its CI side by side**
  (0.0.24.9201), on the layout `stat_block`; the best overall response's
  n (%) in the first of the two columns.  The arms' N is not in that
  header yet (rtfreporter does not read it over split columns,
  ichirio/rtfreporter#709).  The
  default, `"rows"`, is the table as before.

* **A study copied elsewhere checks its answers against its own files**
  (0.0.24.9200).  The requests' contexts (`ai/requests/_contexts.rds`)
  kept the study folder's path of the PC that wrote them, and an answer's
  check read the study's files there; they now keep none, and
  `tfl_ai_collect_answers()` (and `run.R`) use the folder they are run in
  -- requests written before included.
* **An agent's requests say an agent's next steps**: `ai/requests/index.md`
  and `ai/status.md`'s next steps name `ai/agent_prompt.md`, the answer to
  save and `Rscript run.R <request>`, not a chat to paste into.

* **A report that could not be built: its repair is a file of its own**
  (0.0.24.9199), `ai/repairs/<request>-build.md`, which stays until the
  request's reports are built: `tfl_ai_collect_answers()` -- run again
  between, by a screen's reload -- removes only the checks' repair
  (`<request>.md`), and took the build's with it.  `ai/status.md` and the
  agent's instructions name the file.

* **`demog` and `ae_pt_freq` are built on their layouts** (0.0.24.9198):
  `demog`'s usual table is `summary`'s body (its own code stays for
  `stack = TRUE` and a dataset other than ADSL), `ae_pt_freq` is
  `hierarchical`'s of one level with its own order, `min_pct` filter and
  page.  Their ARDs, items and programs are as before.  `hierarchical`
  with one level (`hierarchy = "AEDECOD"`) no longer stops: it has no row
  keys above the term.

* **`tfl_ai_run()` (`run.R`) builds only what changed** (0.0.24.9197).  A
  report's ARD and RTF are made again only when its report file, the
  study's files (`study.yml`, `spec/data.yml`, `spec/defaults.yml`, its
  templates), the ADaM it reads or the packages' versions changed since
  the last run (`ai/built.csv`); else its RTF is kept.  `requests =`
  (`Rscript run.R ae-1`) builds one request's reports, every answer still
  read and checked; `rebuild = TRUE` builds them all.  A report that
  passed its checks but could not be built gets a repair,
  `ai/repairs/<request>.md`, with why.  The agent's instructions and each
  request name `Rscript run.R <request>`.  In a study of 100 reports a
  run after one answer goes from minutes to seconds.

* **A study's AI files name its paths from the study folder**
  (0.0.24.9196).  `tfl_ai_prepare(mode = "agent")`'s first message for the
  agent (`ai/agent_prompt.md`, `$agent_prompt`) names `AGENTS.md`,
  `ai/requests/index.md`, `run.R` ... from the study folder, not with this
  PC's path; the requests name a TOC / SHELL workbook in the study folder
  from it, however it was given (relative, with backslashes, another
  case); `tfl_ai_study_init()` writes an ADaM folder in the study, given
  whole, from it in `spec/data.yml`.  A study copied to another PC then
  says the same.  A workbook or ADaM folder outside the study keeps its
  whole path.

* **The layout template `stat_block`: one record's statistics over several
  rows and columns** (0.0.24.9195).  Each arm's block holds one analysis
  record's statistics -- its N in one column, its rate and interval in the
  next, or under each other -- as `cells` says: a named list, heading =
  template, one element a column (`direction = "cols"`) or a row
  (`"rows"`), a vector its lines (`cells = list(N = "{N}", "ORR(%) 90%CI" =
  c("{n} ({estimate:.1f%})", "{conf.low:.1f%}, {conf.high:.1f%}"))`; in
  YAML, records `{label, template}`).  The statistics each group shows are
  read from its templates; the ARD is the report's own (so, as `km_table`,
  it takes no `subset` or `where`).  `tpl_stat_block()` is the call.  Needs rtfreporter 0.8.2.9086 (a split record takes its
  group's template, whatever the study's `categorical`).
* **`orr`: `conf_level`** (0.95): the rate's CI's level, and its row's
  label (`90% CI`).  The template is now tagged as the `stat_block`
  layout; with the defaults it writes the same table as before.


* **The template `vs_change` and `tpl_vs_change()` are removed**
  (0.0.24.9194).  Its table is `param_change` with `baseline = TRUE` and
  `layout = "rows"`: one parameter (`params`), one visit (`visits`),
  vs_change's statistics, cells and labels (`baseline_label`,
  `value_label`, `change_label`) give the same rows and cells.  The one
  difference is param_change's blocks of the parameter and the visit; a
  report that wants them in the title removes them with its own `plan_*`
  items.  A report file with `template: {name: vs_change}` is now an
  error naming the templates there are.

* **Templates: the standard filter pair, `subset` (which subjects) and
  `where` (which records)** (0.0.24.9193).  Every table and figure
  template takes `subset`, an R condition on the analysis set's ADSL --
  the denominator, the header N and, through the three stages, the
  records -- and every template of records data `where`.  `tfl_param()`
  gets `group` (`"target"`: `population`, `subset`, `where`, listed
  first); `tfl_test_templates()` lists a template without them.  A
  listing keeps ADSL's subjects by a semi join; `km_table` has no data of
  its own and takes neither.  An analysis set narrowed by a report's own
  condition is one statement when the set is used for nothing else
  (`adsl_saf <- filter(adsl, SAFFL == "Y", PART == "Part 1")`).

* **`param_change`: a baseline block** (`baseline = TRUE`,
  `baseline_label`; 0.0.24.9193).  The baseline's rows (`BASE`) come
  before the value's.  With vs_change's statistics, cells and labels,
  param_change gives vs_change's rows and cells; its blocks of the
  parameter and the visit are the one difference.  `vs_change` stays for
  now.

* **Generated ARD programs in three stages: the denominator, the target
  subjects, the target records** (0.0.24.9192).  A records dataset is
  joined to its subjects: `inner_join()` when it takes ADSL columns (an
  explicit `add`, and the `by` / `strata` of analyses with a denominator;
  never record-level columns the population's derive makes, such as
  `TRTA`), the records' own columns of those names dropped first;
  `semi_join()` otherwise.  The report's `where` is its own `filter()`
  after the join, and a dataset as a denominator is a `semi_join()` too.
  Analyses on the same dataset, analysis set and `where` share one object,
  with the union of the ADSL columns they need.  See `tfl_ard_code()`'s
  details.

* **Generated code pipes with `|>`** (0.0.24.9192).  The report
  programs' pipe was RStudio's Insert Pipe Operator setting, and `%>%`
  outside RStudio, while the ARD programs wrote `|>`; now every program
  writes `|>` unless asked (`pipe = "%>%"` / `"rstudio"`, or the option
  `rtfreporter.ard_pipe`).

* **The AI flow with templates: `tfl_ai_prepare(templates = TRUE)`**
  (0.0.24.9191).  The requests ask the AI to answer a report with one of
  the study's templates where one fits (`template: {name, args}`, only the
  arguments that differ, the report's own content items after it), else as
  a code SPEC; the templates are listed from the registry (tflspec's, a
  company's, a study's variants, the target arguments marked) when the
  requests are written, never by hand.  An answer's template is checked
  (its name, its arguments' names and types) and expanded, so the code
  SPEC's checks run on what it stands for; the report file keeps the
  template, and its RTF or PNG is built as any other.  Each request's
  first line and `ai/status.md` say the setting.  `templates = FALSE` asks
  for the code SPEC only: a chat's requests are byte for byte as before.
  `tfl_ai_compare()` (new) sets the two side by side: one row a report --
  answered with a template or code, its report file's values, its check,
  its repairs -- in `ai/compare.csv`.
  - `mode = "agent"`: what every request repeated is written once in
    `ai/` -- `instructions-table.md`, `instructions-figure.md` (the
    instructions and the schema), `templates.md` -- and the ADaM summary
    and `spec/` files are named, not copied; a request names them, its own
    answer header and the templates likely for each report (from its
    title).  A request drops from about 53,000 to 4,000 characters; with
    `templates = FALSE` the agent's requests take the same form.
    `tfl_ai_write_requests()` has a `mode` argument for this.
  - A chat's request over `max_chars` with templates first lists their
    arguments by name only, then is halved as before.

* **Figure templates: the design form, and `tpl_` functions that draw
  only** (#403, 0.0.24.9190).  A template SPEC's figure expands into the
  design form (data steps, plot, layers), as the figure designer and the
  AI drafts write it; a report's own items after `template:` go onto it
  (a dashed line, the numbers from an ARD, labs).  As for tables, the
  program writes the data steps (the analysis set's semi join, the
  filters) and a `tpl_` function draws: `tpl_km_simple(adtte_pfs, group =
  TRT01A) + geom_hline(...)`, the `+` on the main plot.  New templates:
  `mean_se` / `mean_ci`, `waterfall`, `km_risk_table` / `km_ci` (with
  ggsurvfit's `add_risktable()`) and `forest_hr` (its own ARD, built by
  `tfl_build_ard()`; `tpl_forest_hr(ard, ...)` and `tpl_forest_layout()`,
  the assembly a line of its own).  Each figure template declares the
  statistics its function makes (`call_steps`); the functions inside
  tflspec attach no package.  The template SPEC, its program and its
  items written out give the same PNG, and the program written out in
  full calls no tflspec function.

* **ae_pt_freq and ae_overview draw in a study of the default standard**
  (#402, 0.0.24.9189).  A study whose standard folds the row headings
  (`plan_stub(name = "row_label", before = TRUE)`, as `tfl_ai_study_init()`
  writes) stopped on these tables of one row-heading column; rtfreporter
  0.8.2.9085 (#701) takes such a column as the stub, and a header cell
  naming it as the stub.  Needs rtfreporter 0.8.2.9085.

* **Template `lab_abnormal`: subjects with laboratory values in abnormal
  categories** (#400, 0.0.24.9188).  n/N (%), a block a parameter, in
  `PARAMN`'s order (`param_order`).  The category is an ADaM variable
  (`ANRIND`, or criteria levels such as `MCRIT1ML`), not thresholds;
  `cumulative = TRUE` counts a level or worse, `baseline_normal = TRUE`
  only subjects normal at baseline.  Every parameter analysed keeps its
  rows, 0/N when no subject is in a category.

* **Template `ae_eair`: exposure-adjusted incidence rates by SOC and PT**
  (#400, 0.0.24.9188).  Each arm's subjects with the event over their
  time at risk (to the first event, else the whole exposure, `exposure =
  "TRTDURD"`), per `per` person-years, with `cardx::ard_incidence_rate()`.

* **Template `ancova`: LS means of the change from baseline and their
  differences** (#400, 0.0.24.9188).  Each arm's LS mean at one visit
  (`cardx::ard_emmeans_emmeans()`, the model `CHG ~ arm + BASE`,
  `covariates`) and each other arm's difference against the reference arm
  (`ref`; NULL: the last in the code list, said) with its 95% CI and
  p-value (`cardx::ard_emmeans_contrast()`, written by `fmt_pvalue()`).

* **Template `param_change`: parameters' value and change from baseline
  at each visit** (#386, 0.0.24.9187).  Laboratory tests, vital signs or
  ECG parameters (`data`: ADLB, ADVS, ADEG) -- n, mean (SD), median, min,
  max of `AVAL` and `CHG`, a block a parameter (`PARAM`, its unit in its
  text) and a visit, in the order of `PARAMN` and `AVISITN`.  Each arm's
  value and change side by side under the arm and its N (`layout =
  "cols"`), or the change's rows under the value's (`"rows"`).  The
  change is read from the records that have one, so the baseline visit's
  change cells are empty.  `params`, `visits` and `where` are all records
  unless given (`where = 'ANL01FL == "Y"'` in the examples).  Needs
  rtfreporter 0.8.2.9084 (the arm's N over its two columns).

* **Template headings given by name merge with the defaults'; a
  template's default variables are those the data has** (#393,
  0.0.24.9186).  `labels: {RACE: "Race, n (%) [a]"}` in a template SPEC
  changed every other heading to NA (the whole default replaced): now
  `labels` and `param_labels` given by name change those names only, and
  `RACE: ~` drops one (`cells` are the rows given).  An ARD analysis may
  say `optional: true` (the analyses sheet's new column): its variables
  are written `any_of(c(...))`, the ones the data has, and
  `tfl_build_ard()` says which it left out.  demog and disposition set it
  for the variables a report does not name, so demog runs on an ADSL with
  no ETHNIC; a report's own variables are as written (a misspelling
  stops).  ARS export leaves the flag out (tflspec's own).

* **Template parameters from the study standard** (#392, 0.0.24.9185).
  `tfl_param(study = )` also takes `"output.population"` (the report's
  `population:`) and `"settings.treatment"` (`study.yml`'s new `settings:
  treatment:`); `population`, `by` and `group` take them by their names,
  so a report with `population: SAF` need not write it again.  A value
  comes from the report's arguments, the study standard, a variant's
  defaults, the template's.  `tfl_test_templates()` lists an analysis set
  or treatment parameter with no study source.  **km_simple takes
  `population`** (the analysis set's id) in place of the flag `pop`: its
  subjects are kept by `data.yml`'s definition, a semi join of the
  figure's data (the join step's new `how = "semi"`:
  `semi_join(adsl |> filter(SAFFL == "Y") |> select(USUBJID), by =
  "USUBJID")`), whatever the figure's data has.  `tfl_fig_template()`'s
  `pop` stays (the figure designer's presets).

* **ae_max_grade's ARD builds in a study** (#394, 0.0.24.9184).  Its
  analysis data made by code (each subject's highest record) had no
  `from`, and the ARD definition's check stopped ("`from` is blank"); each
  says the records it is made from now.

* **`tfl_template_variant()`: a template by its defaults alone** (#388,
  0.0.24.9183).  `tfl_template_variant("demog_jp", from = "demog",
  defaults = list(...))` registers `from`'s parameters, types and body with
  other defaults, no R body; it reads `from` as it is when it runs, so a
  fix to it reaches the variant.  The same name replaces `from` (with a
  message).  A value comes from the report's arguments, then the study
  standard, the variant's defaults, `from`'s.  A report's program calls `from`'s
  `tpl_*()` with the variant's defaults written out, under `# template
  <name>: a variant of <from> (<package> <version>)`.  `tfl_templates()`
  has a `from` column.  A study's `spec/templates/*.R` is run when the
  study is read (`tfl_read_study_spec()`), and `tfl_use_company_templates()`
  writes one variant a template, its package exporting nothing.

* **`tfl_param(type = )`: each template parameter's input type** (#387,
  0.0.24.9182).  `"vars"`, `"levels"`, `"flag"`, `"number"`, `"text"`,
  `"choice"` (with `choices`) or `"list"` (with `fields`), and `multiple`,
  in `tfl_template_params()` -- what tflplanner's template SPEC form reads
  in place of guessing from the default.  Levels say whose values they
  are (`of`: `by_levels` of `by`, `params` of `PARAMCD`).  Every tflspec
  template's parameters have theirs; a company's without one takes
  tflspec's for its name, else one read from its default, which
  `tfl_test_templates()` lists (`"no type"`).  The schema says each plan
  function's `tier` (`required`, `usual`, `other`): which cards
  tflplanner's builder shows from the start.

* **Template `ae_max_grade`: adverse events by SOC and PT at the maximum
  severity or grade** (#385, 0.0.24.9181).  Each subject once, at the
  highest severity (`AESEV`) or grade (`AETOXGR`) of their events --
  overall, in each SOC, in each PT (`dplyr::slice_max()` in the ARD
  program).  The grades are rows under each term (`grade_in = "rows"`, as
  FDA's tables), or columns in each arm with its "Any grade" column first
  (`"cols"`).  The overall block is first; the SOCs and PTs by their count
  (`sort = "n"`) or by name (`"name"`).  Needs rtfreporter 0.8.2.9083.

* **Template `ae_pt_freq`: adverse events by preferred term, most frequent
  first** (#383, 0.0.24.9180), the first of the safety templates.  The
  treatment-emergent events' preferred terms, the subjects with any event
  first, then the terms by decreasing frequency in the first group of the
  code list of `by` (`sort_by` names another column).  `min_pct` keeps the
  terms of that percentage of subjects or more, in any column (as FDA's
  tables of common adverse events), in all (`min_pct_in = "all"`) or in
  the Total column (`"total"`).  `tpl_ae_pt_freq()` is its call.

* **shift / lab_shift as a template SPEC** (#382, 0.0.24.9179).  Written
  as `template:` in a report file, they stopped with "`vars` needs at
  least two columns" in a study whose defaults say
  `plan_stub: {name, before}` (as SAMPLE-01's do): the template's
  `plan_stub(vars = )` was taken for the look and the study's replaced it.
  An item is the look now only when all its arguments are: the stub's
  columns stay.  Every template's `template:` form is now tested against
  its written-out items.

* **The AI steps over several workbooks** (#378, 0.0.24.9178).
  `tfl_ai_read_shell(path, shells = )` reads a TOC in one workbook and
  its shells in others: each report's shell is looked for in all of them
  (a sheet named as one already there is `"<sheet> [<workbook>]"`, its own
  name kept), the text has a part a workbook, and each report's place names
  its workbook.  `tfl_ai_prepare(toc, shells = , add = TRUE)` (and
  `tfl_ai_write_requests(add = TRUE)`) adds a workbook's reports to the
  study's requests and answers already there -- tables and figures, then
  listings, then a book of PK figures, one at a time: the new requests are
  named after those there (`ae-2`), a report asked for already stops the
  call (`overwrite = TRUE` asks for it again), `spec/toc.yml` gets the new
  reports at its end.  The requests (`tfl_ai_requests()` too) and
  `tfl_ai_prepare()`'s `toc` have a `book` column.

* **Figures in the AI steps** (#376, 0.0.24.9177).  A figure of the TOC
  is a request of its own (`figure-1` ...: its shell from the workbook,
  its analysis set, the ADaM summary, the figure templates); its answer, a
  figure design, is checked and written as the figure's report file in the
  design's form -- the same design back for tflplanner's figure designer
  -- and `tfl_ai_run()` draws it as tflplanner does
  (`tfl_fig_design_code()`): its PNG and its RTF.  The requests and the
  reports have a `type` column (`table`, `figure`), the run's reports a
  `png`.  A figure's assumptions with their questions join
  `ai/questions.md`.  `tfl_ai_write_requests(types = )` now defaults to
  tables and figures.  New `tfl_ai_questions()` and
  `tfl_ai_answer_questions()` read the questions and write the person's
  answers (each into its `- Answer:`, the rest as it is), and
  `tfl_ai_prepare(mode = "agent")` returns `agent_prompt`, the agent's
  first message, also written to `ai/agent_prompt.md`.  The example workbook has a Kaplan-Meier figure, with its
  stored answer.

* **`tfl_ai_prepare(mode = "agent")`: the study's requests for a coding
  agent** (#374, 0.0.24.9176), GitHub Copilot's agent in VS Code and the
  like.  The agent works through every request without stopping: it saves
  an answer, runs `run.R`, reads `ai/status.md` and repairs.  What it
  cannot settle it takes a guess on and asks as an assumption's
  `question`.  The new `tfl_ai_run()` (what `run.R` calls) collects the
  answers, builds each report's ARD and RTF -- one report's failure not
  stopping the others -- and writes `ai/status.md`.  It also gathers the
  questions into `ai/questions.md`: one question asked by several reports
  once, the reports listed, an answer there kept.  The person answers it;
  the agent moves the answers to `ai/decisions.md` and redoes the reports
  they touch.  The instructions (`AGENTS.md`, `.github/copilot-instructions.md`)
  tell the agent to work from the ADaM summary and read the ADaM with R
  only to settle a question the summary does not (with the company's
  approved AI; a chat is sent the summary only).  Each request gives the shells'
  text and where each report's shell is in the TOC / SHELL workbook file
  (`tfl_ai_write_requests(workbook = "file")`: the file, the sheet, the
  cells, the TOC row); the agent reads that place alone with R when the
  text looks cut, never another sheet on a guess.  For tflplanner: a new study's
  RTF files go to `output/tfl`, its programs to `programs/tfl`, and
  `tfl_ai_prepare()` writes `spec/toc.yml` (the TOC's reports in order,
  with their section and analysis set) when the study has none.  The
  guide's step 6 is now `tfl_ai_run(".")`.

* **The AI steps return what a screen needs** (#371, 0.0.24.9175), for
  tflplanner's AI page.  `tfl_ai_prepare()` returns a `tfl_ai_prepared`
  list: `requests`, `toc` (each report of the TOC, its shell found or not,
  the request asking for it), `sheets`, `adam` (the ADaM summary),
  `files` (written, relative to the study) and `next_steps`.
  `dry_run = TRUE` reads the workbook and the ADaM and writes nothing (to
  look before the requests are written); `adam_summary =` reuses a call's
  `$adam`.  `tfl_ai_collect_answers()` returns a `tfl_ai_collected` list:
  `reports` (the data frame it returned, with `problems_text` -- every
  problem --, `file` and `repair`), `next_steps`, `assumptions`, `stray`;
  its print is as before.  `res$status` is now `res$reports$status`.  New
  `tfl_ai_save_answer(study_dir, request, text)` writes a pasted answer
  where the collect reads it (`ai/answers/<request>.md`, UTF-8), and
  `tfl_ai_requests(study_dir)` reads the requests written again.
  `tfl_ai_prepare()` also takes `datasets` and `max_levels` (the ADaM
  summary's) and `quiet`, and returns the workbook reading's notes as
  `hint`.  A request no longer shows where the ADaM is: its copy of
  `spec/data.yml` names each dataset's file alone (`path: adsl.xpt`), the
  study's own file keeping the full path.

* **A line break inside a cell no longer shifts the shells' text**
  (#370, 0.0.24.9174).  `tfl_ai_read_shell()` missed a cell whose text
  holds a line break (Alt+Enter in Excel: a column header like
  "Placebo" over "(N=xx)"), and every later cell of the workbook took the
  text of the one before it -- the shells and the TOC titles the AI read
  were wrong.  The workbook reader now reads such a cell as one (`\n` in
  the text), and the cells after it keep their own text.

* **A nested count with no records: the overall row 0, not a stop**
  (#367, 0.0.24.9173).  `cards::ard_stack_hierarchical()` stops on data
  with no record ("all missing"), so a table of zero events -- common in a
  study -- could not be made.  The ARD program now calls the study helper
  `ard_hierarchical_or_none()` with the same arguments: cards' ARD with
  records; with none, the overall row (with `over_variables = TRUE`) n = 0
  and p = 0 with each group's N, and the groups' counts.  The table is
  "Any ... 0 (0.0)" alone.  `complete_hierarchy()` adds nothing under an
  overall row of 0 unless `when_none = TRUE`.  ARS still shows cards'
  function.

* **An AI answer's overall row made** (#366, 0.0.24.9172).  An answer that
  names an overall row (`normalize_ard: {overall: ...}`) without
  `over_variables = TRUE` on its hierarchical analysis had no such row;
  `tfl_ai_collect_answers()` now writes `over_variables = TRUE` into that
  analysis in the report file and adds an assumption saying tflspec added
  it.  A spec written by hand keeps the check's warning.

* **AI requests with the TOC / SHELL workbook attached, and
  `tfl_ai_prepare()`** (#364, 0.0.24.9171).
  `tfl_ai_write_requests(workbook = )`: `"text"` (default) as before;
  `"attach"` -- each request says where its reports' shells are in the
  attached workbook (sheet, cells, TOC row) instead of their text; `"both"`
  -- the places and the text, the attachment right where they differ.  For
  a workbook whose text is not read right, when the chat takes an Excel
  file.  `tfl_ai_prepare(dir, toc, ...)` runs the guide's step 3 in one
  call (`tfl_ai_study_init()` only when there is no `study.yml`, the ADaM
  summary, the workbook read, the requests).  The AI table guide (English
  and Japanese) shows both.
* **AI answers as a tester's chat writes them** (#365, 0.0.24.9170).  A
  report number as the TOC writes it (`Table 15.3.1.3.13.a`, as the prompt
  asks) is the report file's id with letters, digits, `.`, `_` and `-`
  only (`Table-15.3.1.3.13.a`), its assumptions too; it could not be read
  before.  `tfl_ai_collect_answers()` takes an answer's report named
  another way (`T-15.3.1.3.13.a`) as the one whose number it is.  The
  check now says what stopped later: a table with no column key (an
  error: one arm too, the arm as `by` and `cols`) and an overall row no
  hierarchical analysis makes (a warning: `over_variables = TRUE`); the
  table prompt says both.  An answer with none of its request's reports no
  longer says "done".

* **An ARD that is not cards' in the code lists' order: `relevel_ard()`**
  (#362, 0.0.24.9169).  cardx's survfit keeps its rows in the arms' order
  but makes their factor again, in alphabetical order, so a survfit
  table's arms came out alphabetical unless the report said
  `plan_levels(TRT01A = )`.  The ARD program now writes
  `relevel_ard(TRT01A = cl_trt01a)` after an analysis that is not cards'
  (custom code, cardx, a study's own function) reading a column with a
  code list: the column's levels in the list's order, its values and rows
  as they were (a study helper, `tfl_helpers_code()`).  So #352 leaves out
  a `plan_levels()` entry equal to the code list whatever analysis reads
  it, and the km_table and orr templates write none for the arms (an order
  given, `by_levels`, stays).  The tables are the same; SAMPLE-01's
  T-14-2-2 gives the same RTF without its `plan_levels(TRT01A = )`.  Such a
  report's ARD fingerprint changes, so an ARD made before is made again.

* **A nested count's code-list values counted 0: `complete_hierarchy()`**
  (#357, 0.0.24.9168).  cards' `ard_stack_hierarchical()` counts only the
  values some group has, so a fixed SOC / PT no subject has could not be
  shown.  `complete_hierarchy()` in an analysis's `post` adds them, n = 0
  and p = 0 in every group (the Total column too), N the group's: the
  outermost variable's from its code list, an inner one's where the
  `codelists` sheet's new column `under` names its parent (a value
  without one is not added, and the program says so).  The rows keep the
  code lists' order.  The generated code writes it in full (`variables =`
  the row's, `under =` the lists', as they are in the ARD); it is one of
  the study helpers (`tfl_helpers_code()`).  Without it nothing changes,
  and a study whose code lists have no `under` keeps its fingerprints.

* **A named order, and the variables' headings under `variable =`** (#356,
  0.0.24.9167; rtfreporter #679, 0.8.2.9075).  `plan_levels()` may name its
  values' text, as `factor(levels, labels)` (`plan_levels(SEX = c(F =
  "Female", M = "Male"))`): the schema takes a map (value: text), the
  sheets' `levels` cell `F = Female | M = Male`, both ways, and a named
  order is never left out as the ARD's own.  The variables' headings are
  written `plan_labels(variable = c(AGE = "Age (years)", ...))` (the sheets'
  bridge and the templates), so they never meet a variable's value labels;
  a report that says `plan_labels(AGE = )` reads as before, and a named
  `plan_levels(variable = )` comes back as the headings.  The tables are
  the same.

* **`plan_levels()` the ARD already says is not written** (#352,
  0.0.24.9166).  A table's program (`tfl_content_code()`) and its run
  (`tfl_content_run()`) leave out a column's order that is the report's
  code list -- applied when the ARD is made, a factor cards keeps in its
  order -- when every analysis reading the column is cards', and
  `variable =` when it is the order of the ARD's analyses.  T-14-1-1's
  program loses its `plan_levels()` and the code lists at its head.  The
  templates of cards' analyses (demog, summary and those on it,
  hierarchical, ae_overview; shift's columns) write none, so their call
  has no `codelists =` either.  Kept: an order of the report's own, a
  column no analysis reads, one a cardx analysis reads (survfit names the
  arms from the strata: T-14-2-2) and a report whose data steps make its
  data.  A workbook's table (`tfl_table_code(ard_spec = )`) leaves out the
  same, so it and its YAML file write one program.  The RTF is the same.

* **The AI table guide's install lines** (#360, 0.0.24.9165).  A new R has
  no `remotes`, and the sample's efficacy table needs `broom` (cardx's
  confidence intervals): the guide (English and Japanese) now installs
  `remotes`, `cards`, `cardx`, `haven` and `broom` first, then the two
  packages from GitHub.  It also says that the `widen_ard()` notes printed
  while making the RTF are information, and what to do when
  `tfl_build_ard()` asks for a package.

* **Fix: the study's ARD program puts a report's code lists on its data**
  (#353, 0.0.24.9164).  When a report's analysis data was its population
  under a name (`adsl_saf <- pop_saf`), the study's program wrote the code
  lists but did not apply them (`set_levels()` was taken off `pop_saf`, as
  a data the analyses do not read), so the ARD kept the raw codes:
  SAMPLE-01's T-14-1-1 printed "F" / "M" where its code list says
  "Female" / "Male".  One report's program was right.  Now each report's
  ARD is the same from the study's program as from its own; the tables
  show the code lists' labels, order and 0-count levels.

* **`plan_levels()` before `plan_labels()`** (#349, 0.0.24.9163): the
  values' order first, then their names and the variables'.  The schema's
  ranks swap (`plan_levels` 80, `plan_labels` 90), the cards' and the
  generated code's order, and the templates write them so.  The table is
  the same: rtfreporter applies the two together, whatever their order
  (checked for the variables' order and headings, value labels, and a
  column the plan makes).  A file that writes `plan_labels()` first reads
  as before.

* **The rounding in either case** (#347, 0.0.24.9162; rtfreporter #676).
  The `study` sheet's `rounding` and a YAML file's `plan_digits: {rounding:
  }` (a report, defaults.yml, a pattern, a style) are read in lower case:
  "SAS" is "sas".  The generated code writes "sas" / "r" as before, so it
  needs no newer rtfreporter.

* **Template SPEC: the study's styles, a template's look under them, and
  where titles and footnotes print** (#344, 0.0.24.9160).
  - `defaults.yml` may hold named styles, `styles: {compact: {content:,
    page:}}`: the look a report picks with `style: compact` (none named:
    `standard`).  The style is the nearest layer below the report's own;
    its items are marked `style:<name>` and the program says `# (study
    style: compact)`.  `tfl_write_study_part("defaults")` writes them.
    SAMPLE-01's `patterns/Compact` moved to `styles: compact` gives the same
    RTF for all its tables.
  - A template's look (plan_style, plan_blanks, plan_columns, plan_digits,
    the rows a page ...) is under the study's layers, which say the look
    and win (decided 10/17); its shape stays over them.  The template's
    call (A) has the study's lines after it, and `tfl_template_to_spec()`
    (B) leaves the study's look to the layers: the same table.
  - study.yml's `settings: titles_at: header | titles` and `footnotes_at:
    footer | footnotes` (decided 10/17): `header` puts a report's titles
    (`page.titles`) in the running header after the study's lines,
    `footer` its footnotes in the running footer before them; `titles` /
    `footnotes`, or no setting, keep them above and below the table.

* **Template SPEC: a company's templates** (#343, 0.0.24.9159).  A
  company's templates are an R package of its own (decided 2026-10-17).
  `tfl_template()` registers a template from outside (its kind, layout,
  parameters, body, the function its program calls and an example); a name
  already registered is replaced with a message, and a parameter that
  changes the look (the rows a page, widths, digits, blank rows, the style)
  is warned about.  `tfl_param()` is exported; `tfl_template_params()`
  gives a template's parameters to start from; `tfl_template_run()` is the
  A use of any template, what a company's exported functions call.
  `tfl_test_templates()` runs each template with one parameter at a time
  changed and lists those that change the look -- tflspec's own have none.
  `tfl_use_company_templates()` writes a company's package to start from:
  a thin wrapper of each tflspec template chosen (the same parameters, the
  company's defaults, tflspec's body), registered on load, its function,
  tests, the company's study defaults and a README; it installs and its
  tests pass.  `tfl_templates()` gains `package` and `call`.

* **Template SPEC in a report's YAML** (#341, 0.0.24.9158).  A report's file
  may be `template: {name, args}`, its `ard:` / `data:` / `content:`
  holding only what it adds or changes (`tfl_output(template = )`; read and
  written, a named vector in `args` as a map; a figure designer's
  `template: km_simple`, a name, is not one).  `tfl_resolve_output()` puts
  the template's items between the study's layers and the report's own
  (each marked `template:<name>`), and its `ard:` under the report's; a
  parameter that says where its study default is
  (`plan_cells.continuous` for `summary` / `demog`'s `cells`) takes it, and
  a SPEC value over a different one says so.  The program
  (`tfl_content_code()`) is the template's call, `plan <- tpl_x(ard, ...)`,
  with the study's and the report's own lines after it, each marked (A);
  `template = "items"` writes the items instead, as it does when the call
  cannot say the report's own table roles, data or normalization.
  `tfl_template_to_spec()` writes a template report out as its items (B).
  The sheets, and so the study's ARD program, take a template report's
  items and analyses.  SAMPLE-01's nine template reports, written as
  template SPECs, give the same ARD and the same RTF now, by B and by A.

* **AI kit: the first try on a Japanese Windows PC** (#338, 0.0.24.9157).
  `tfl_ai_collect_answers()` and `tfl_ai_unpack()` read an answer saved as
  UTF-8 with or without a BOM, with CRLF line ends, or as UTF-16 (Notepad's
  "Unicode"). They also read an answer Notepad saved as `<request>.md.txt`.
  An answer saved in Shift-JIS / CP932 is an `error` that says how to save
  it again as UTF-8. Every file the kit writes (requests, index, ADaM
  summary, TOC/shell text, repairs, the study's YAML, the report files) is
  UTF-8 whatever the session's locale, and the folder of
  `file = "ai/..."` is made when it is missing. `tfl_ai_write_requests()`
  makes `ai/answers` and says what to paste first. The print of
  `tfl_ai_collect_answers()` gives each request's status, its reports and
  the next step (paste the repair into the same chat, the request into a
  new one ...). It also names answers whose file name matches no request.
  An error when writing one request's reports no longer stops the other
  requests. `tfl_ai_read_shell()` on a company's own workbook: a sheet
  named like a TOC must list report numbers and must not be named by one
  (a shell sheet named `L-16-2-1 <list>` is not taken for the TOC). A
  header cell saying title (Title, or the Japanese words for it) gives the
  title column, so a longer analysis-set column is not taken for the
  title. Full-width digits in a report number are read. The print says
  what to pass when no TOC or a report's shell was found (`toc_sheet =`,
  `sheets =`). A request's own read of the workbook keeps the `toc_sheet`
  and `sheets` given. The table guides' sample block uses one stored answer
  per request (`inst/ai/example/answers/`), so all four reports print `ok`.
  The block and the guide's steps 3 and 6 are run as tests.
* **Where a table's page names print, said in its content** (#337,
  0.0.24.9156; rtfreporter #672).  `plan_paginate_group(heading_at = )` and
  `plan_paginate_rows(heading_at = )` (`section`, `title`, `none`) are in
  the schema, and the sheets' `layout` gains `group_heading_at` and
  `pages_heading_at`, both ways.  It is the default of the report's
  `rtf_tables(auto_section = , auto_title = )`: a report's own setting
  (the page's `rtf_tables: {auto_section: }`, the report sheet's
  `auto_section`) wins, and a blank one does not undo it.  Needs
  rtfreporter 0.8.2.9072.

* **Template SPEC: `ae_overview`** (#335, 0.0.24.9155).  A display template
  on `summary` (`tpl_ae_overview()`): the subjects with a treatment-emergent
  adverse event of each kind -- any, related, grade 3 or higher, serious,
  leading to discontinuation, leading to death -- n (%) by arm, a row a
  kind.  Each row is a `subjects` analysis of the events its `where` keeps;
  `flags` changes the rows.  The onco example study gains the report it is
  checked against (T-14-3-2, Overview of Treatment-Emergent Adverse
  Events); the template gives the same items and RTF by A, by B and by
  its spec.

* **Template SPEC: four more** (#333, 0.0.24.9154).  `summary` takes an
  optional mean (95% CI) analysis of its continuous variables (`ci_label`;
  T-14-2-3).  `km_table` (`tpl_km_table()`): the Kaplan-Meier median and
  event-free rates, from the ARD -- its survival analyses stay the report's
  own, as decided (T-14-2-2).  `vs_change` on `summary` (`tpl_vs_change()`):
  a vital sign's baseline, value and change at one visit (T-14-2-1).  `orr`
  (`tpl_orr()`): the best overall response and the objective response rate
  with its CI (the AI example's T-14-2-1).  Each gives the same items and
  RTF by A, by B and by its spec file.

* **Template SPEC: `summary` takes categorical variables, and `disposition`**
  (#331, 0.0.24.9153).  The layout `summary` takes `categorical` beside the
  continuous `variables` (either or both), the subjects-per-group and
  categorical analyses' labels and the categorical statistics (`NULL`:
  cards' own); a parameter given as `NULL` is "none", not the template's
  default.  The look (the rows a page) is no parameter: a report changes it
  with an item of its own (T-14-1-3's `plan_paginate_rows(max_rows = 24)`).  The display template
  `disposition` (`tpl_disposition()`) on it: the status at the end of the
  study.  On the sample study T-14-1-2 (`disposition`), T-14-1-3 and
  T-14-1-4 (`summary`) give the same items and RTF by A, by B and by their
  spec files, and T-14-2-1 still does.  The layout `shift` (a category at
  baseline by the same after it, a block a parameter) and the display
  template `lab_shift` on it (`tpl_shift()`, `tpl_lab_shift()`): the AI
  example's T-14-3-5 gives the same items and RTF by both.  The layout
  `listing` (`tpl_listing()`): a dataset's records, their order and the
  columns -- a column's width is part of its definition, the one look a
  template takes; the sample study's L-16-2-7 gives the same data, items
  and RTF by A and by B.

* **For a builder of the YAML's items** (#329, 0.0.24.9152; tflplanner B3).
  `tfl_resolve_output(provenance = TRUE)` says where each item's arguments
  come from (`arg_from`: `own`, `defaults`, `pattern:<name>`, by argument --
  a verb whose `...` is named by top-level key) and gives the item `below`
  it (without the report's own layer: what a revert goes back to);
  `provenance = FALSE` is as before.  One item: `tfl_item_code()` (its call),
  `tfl_item_written()` (the arguments its code writes), `tfl_item_own()`
  (what the report's own layer holds of it over `below`).
  `tfl_schema(fn)` has the function's `formals` (the data's arguments left
  out) and the `required` ones.  `tfl_write_study_part()` writes the
  study's defaults, a pattern, its code lists, data or TOC back to its file
  (the sample study's files written back are byte-identical).

* **A report file reads and writes back as the same text** (#327, 0.0.24.9151).
  `tfl_write_output()` writes the list-valued keys of the `ard:` part
  (`analysis_data` add/keep/distinct; `analyses` by/strata/variables/
  statistics/formats) and listing_col `vars` as a sequence even when they
  hold one name, as the sheets bridge does; they were written as a scalar.

* **Template SPEC, a first prototype** (#325, 0.0.24.9150).  A report kind from
  a few parameters, one definition for two uses: `tpl_demog()` /
  `tpl_summary()` give the table plan (A: the program's one call, the
  study's defaults written after it), `tfl_template_expand()` the report's
  items (B: saved into its YAML, marked `template:<name>`).  The layout
  template `summary` (continuous variables by a column key) and the display
  template `demog` on it; the layout `hierarchical` and `ae_soc_pt` on it
  (`tpl_ae_soc_pt()`, `tpl_hierarchical()`); `tfl_templates()` lists them.
  On the sample study T-14-1-1, T-14-1-1S (`demog(stack = TRUE)`), T-14-2-1
  (`summary`) and T-14-3-1 (`ae_soc_pt`) give the same RTF by A, by B and
  by their spec files.  `km_simple` (the figure template) is on the
  registry too: the sample study's F-14-2-3, written as the template and its
  own items after it (a median line, three `tfl_ard_number()`, the y axis'
  title), draws the same PNG as its design-form file.  Many reports at once in Excel: `tfl_read_template_sheets()`
  reads a workbook of two sheets (`templates`, `template_rows`) and
  `tfl_write_template_yaml()` writes each report's file; the example
  (`inst/extdata/template-spec/`, DM and AE, the workbook and the same as
  YAML) gives the sample study's items exactly.

* **A table whose data steps normalize the ARD: the sheets' code** (#323,
  0.0.24.9149).  `tfl_content_code()` and `tfl_content_run()` no longer
  write or run the study defaults' `normalize_ard()` after a report's
  `data.steps` (the steps make the data; normalizing it again dropped what
  they did, such as their row labels).  With no data steps, `normalize_ard()`
  stays first in the one pipe (#303).  `plan_cells()`'s entries come in the
  sheets' order: the table's variables first, `default` after.

* **The converter reads a page cell as the sheets do** (#321, 0.0.24.9148).
  `tfl_yaml_from_sheets()` writes a header, footer, title or footnote cell
  that is quoted as its text (`"  Indented"`: the spaces kept, the quotes
  not), and a written `\n` in those cells and in a token's value as a line
  break -- what the sheets path reads them as.  A converted report's
  program is the sheets'.

* **The study's report setup and a report's tokens from the YAML spec**
  (#317, 0.0.24.9147).  `tfl_report_setup_code()` also takes a study's
  spec (`tfl_read_study_spec()`): its defaults' `page:` -- the first font of
  `rtf_document: font_table`, `rtf_default_format: font_size_half_points`,
  `tokens`, `header`, `footer` -- written as the sheets saying the same
  write them, with the names `tfl_report_code(output, setup = TRUE, study
  = )` refers to (`study_header`, `study_header_lines` ...).
  `tfl_report_tokens()` takes a report of the YAML spec and its `study`.
  A YAML report's program (`rtf_document(program = )`): a study with no
  `settings: program` no longer takes its `program_dir` for it (partial
  matching), and the report's id as `program_fallback` is in the study's
  `program_dir`, as the sheets' path writes it.

* Docs: the Pages' top bar is the package icon's colour, green `#1d5e47` (#313, 0.0.24.9146),
  as rtfreporter's bar is its icon's navy.  As rtfreporter's, the menu puts
  Reference before Articles, and lists the English articles only: the
  Japanese one opens from the "In Japanese" toggle at the top of its English
  page (and back with "In English").

* Docs: the AI table-drafting guide says plainly what to try first: one
  request as it is; when the chat will not take it, the requests again with
  `instructions = "once"` (`00-instructions.md` first in a new chat, then the
  request) (#318, 0.0.24.9145).

* **A whole study's AI requests as files** (#315, 0.0.24.9142).
  `tfl_ai_write_requests(study_dir, shell, adam, size = )` writes one prompt
  a request to `ai/requests/` -- one paste each, no attachment -- with an
  `index.md` of their reports and lengths; `max_chars` splits a request too
  long, `instructions = "once"` writes the instructions (about 23,000
  characters) once to `00-instructions.md`.  The answers saved as
  `ai/answers/<request>.md` are read by `tfl_ai_collect_answers(study_dir)`:
  each checked against its own request and, with no error, written as report
  files (only the reports it asked for); one row a report, `ok` /
  `warning` / `error` / `no answer`, and the repair prompt of an answer with
  errors in `ai/repairs/`.
* Docs: the AI table-drafting guide (English and Japanese, the article and
  `inst/ai/tflspec-ai-table-guide*.md`) rewritten short and practical: what
  it does, what goes where, what to run, what to give the AI, where its
  answer goes, and the functions that check it, write the YAML and make the
  RTF; a whole study with the two functions above; the sample run short.

* **AI drafting of tables' code spec from a company's TOC / SHELL
  workbook** (#307, 0.0.24.9141).  A new drafting task, `table`: a batch of
  5 to 10 tables becomes report files of the YAML spec
  (`spec/outputs/<id>.yml`: `ard:` analyses, `data:`, `codelists:`,
  `content:` items, `page:`) over the study's defaults.
  `tfl_ai_context("table", outputs, shell, datasets, study_dir, examples)`,
  `tfl_ai_prompt("table")` (the schema from `tfl_schema()`,
  `tfl_spec_columns()` and `tfl_ard_methods()`; a reviewed report of the
  family as the model with `examples =`), `tfl_ai_parse()` (one YAML block,
  `outputs:` read as `tfl_read_output()` reads a file),
  `tfl_ai_check()` (the schema: unknown functions and arguments, types,
  required arguments; the ARD's fields and methods; the analysis sets; the
  datasets and variables of the ADaM summary; the ARD program and the
  table code written from a copy of the study folder, nothing run) and
  `tfl_ai_unpack()` (one file a report; the assumptions join
  `spec/_assumptions.yml`, batch by batch).  New helpers:
  `tfl_ai_read_shell()` (any workbook layout as compact text: every sheet,
  non-empty cells in reading order, merged cells once, indents; pick reports
  by number or sheets; a size cap), `tfl_ai_adam_summary()` (a folder of
  XPT: variables, labels, types, short value lists, no records),
  `tfl_ai_batches()` (the TOC's tables by family) and
  `tfl_ai_study_init()` (study.yml, spec/data.yml with ADSL's analysis
  sets, spec/defaults.yml).  The AI user manual takes the code spec as its
  main path (the sheet spec is Appendix A), and a tester guide in English
  and Japanese ships in `inst/ai/` (articles "Drafting tables' code spec
  with an AI chat").  A sample TOC / SHELL workbook and a stored answer
  are in `inst/ai/example/`.

- **`tfl_as_table_spec()` takes the variables' order from
  `plan_levels(variable = )`** (#311, 0.0.24.9140), not from the order of
  `plan_labels()`: a label sets only the text (rtfreporter #666).  A plan's
  `plan_levels(variable = )` no longer becomes a variables row named
  `variable`.

* **The sheets from a spec in memory: `tfl_sheets_from_spec(study,
  outputs)`** (#304, 0.0.24.9139).  What `tfl_sheets_from_yaml(dir)` does,
  from a study and its reports already read (`tfl_read_study_spec()`,
  `tfl_read_outputs()`) or changed in memory and not written yet;
  `tfl_sheets_from_yaml(dir)` is now `tfl_sheets_from_spec()` of what it
  reads, its result unchanged.
* A test compares a listing's content code (`tfl_content_code()`) with
  the sheets' `tfl_listing_code()`: the same pages (RTF) on the data the
  sheets' program makes.  `tfl_content_code()` does not write the
  listing's data part (dataset, `where`, `sort`, dates), and writes a
  label's `\n` as it is where the sheets make it a line break (a skipped
  test until it does).

- **`tfl_args_from_r()` keeps a `NULL` argument on R < 4.4** (#305,
  0.0.24.9138).  `label = NULL` read as no argument on R 4.2 (`is.atomic(NULL)`
  is TRUE there); it is `tfl_r("NULL")` on every R now.

* **The table's code is one pipe from the ARD** (#302, 0.0.24.9137).
  `tfl_table_code()` writes `plan <- ard |> normalize_ard(...) |>
  table_plan(...) |> plan_*()`: `normalize_ard()`'s `hierarchy`,
  `overall` and `stack` are the ones `tfl_normalize_code()` writes (new
  `ard_spec`; with no ARD definition, the hierarchy the rows and label
  say, as the spec files take it).  `data` is now the ARD's name
  (`"ard"`); `normalize = FALSE` writes `plan <- table_plan(data, ...)` as
  before, for a program that normalizes the ARD itself.
* `plan_levels()`'s names come in one order, whatever the sheets' rows:
  `variable`, the column keys, the row and stack keys, then the
  categorical variables.
* The `digits` sheet takes significant digits: `3s` is `{mean:.3s}`.

* **The variables' order is `plan_levels(variable = )`** (#300,
  0.0.24.9136).  The `variables` sheet's `order` is written as
  `plan_levels(variable = c("AGE", "SEX", ...))` (the values of the ARD's
  `variable` column; a variable nested with `under` has its place from
  `plan_nest()`), and read back from it.  It used to reach the table only
  through the order of the names in `plan_labels()`, and a variable with an
  order but no label was not ordered.  `plan_labels()` sets no order now
  and comes back in the order it was written: SAMPLE-01's tables are
  byte-identical, from the YAML and through the sheets.  The sheets read
  back from content keep the variables' rows in their place:
  `plan_levels()` is read before `plan_labels()` wherever it is written
  (the recommended order of #290 puts the labels first, which moved a
  variable with levels but no label, e.g. `TRT01A`, to the end -- and its
  code list with it in tflplanner's programs).

* **The onco sample study: screen failures, ethnicity, study disposition,
  an AE time-to-event endpoint** (#298, 0.0.24.9135).  `adsl` holds every
  screened subject: the 160 randomised and 40 screen failures, who have no
  arm (`ARM`, `TRT01P` ... `NA`, `ARMNRS` "SCREEN FAILURE", as SDTMIG 3.3
  has it; the factors keep their two levels) and no records in the other
  datasets.  New ADSL variables: `ETHNIC`, `RFICDT` (informed consent),
  `ENRLFL` (enrolled: randomised), `ARMNRS`, and the study disposition
  `EOSSTT`, `EOSDT`, `DCSREAS`, `DCSREASP` (a randomised subject is in
  follow-up until death; a screen failure ends at screening).  `adtte` has
  a safety endpoint, `TTSKIN`: the time to the first treatment-emergent
  skin and subcutaneous tissue disorder of the safety set, in days from
  the first dose, censored at the end of the treatment-emergent period.
  The new values are drawn after the old ones (same seed): every existing
  record keeps its values, and the other six datasets are unchanged.  An
  analysis of the randomised subjects selects them (`ITTFL`, `SAFFL`,
  `EFFFL`): `adsl` now has 200 rows.  The SAP and CRF outline in
  `system.file("sample", "onco", package = "tflspec")` say the same.

* **The samples run on the oncology sample study** (#295, 0.0.24.9134).
  tflspec's examples, articles, README and AI kit use `adsl`, `adae`,
  `adrs` ... (study XYZ-ONC-01, Drug X vs Control) instead of the CDISC
  pilot data (`cards::ADSL`, pharmaverseadam), which is dropped from
  Suggests.  `inst/examples/onco/` replaces `inst/examples/pharmaverseadam/`:
  a study folder whose specs (ARD spec, table and report specs, plot list)
  are written into programs and run -- demographics, best overall response
  with ORR and DCR, TEAEs by SOC and PT, PFS and OS Kaplan-Meier plots, a
  PFS forest plot, waterfall, swimmer and spider plots.  The example
  workbooks of `inst/extdata/ard-spec` (made by
  `data-raw/ard-spec-examples/make-examples.R`) are built from the sample
  study: the response rate from its best overall response, the
  laboratory shift from its CTCAE grades.  `inst/extdata/spec-yaml` and
  the define's code lists use its arms and age groups.  The AI kit's
  made-up study XYZ-001 reads the same data.  The datasets' help pages
  link to each other (they linked to pharmaverseadam's and ggsurvfit's
  topics of the same names).  No function changes.
* **A value's labels in the report's code list** (#294, 0.0.24.9133; the
  user's decision of 2026-10-16).  The key columns a plan makes itself --
  a stack's pair (`tables$stack`: `subgroup`, `subgroup_level`) and the
  groups of statistics (`.stat_group`) -- are not in the data, so the ARD
  program cannot put a code list on them: the bridge writes their code
  list rows as `plan_labels(subgroup = c(ALL = "All Subjects"))` and
  reads that call back as code list rows (`tfl_sheets_from_yaml()` passes
  it the report's `normalize_ard()`).  Their order stays the plan's (no
  `plan_levels()` from the list, so a list naming some values drops
  none).  A data variable's value labels stay its code list's, which the
  ARD program puts on the data; `plan_labels()` of one is listed in
  `not_converted` with where they go.  `tfl_args_from_r()` reads
  `plan_labels(c(AGE = "Age"))` and `plan_levels()` given one named
  vector, as rtfreporter does (the schema's `map: true`).

* **An oncology sample study: `adsl`, `adex`, `adae`, `adlb`, `advs`,
  `adrs`, `adtr`, `adtte`** (#291, 0.0.24.9132).  Simulated ADaM datasets
  of study XYZ-ONC-01 (Drug X vs Control, 1:1, RECIST 1.1 every 6 weeks,
  cut-off 30 June 2026; 160 subjects; not real patient data), with CDISC
  names and labels; `TRT01P`, `TRT01A` and `ARM` are factors with Drug X
  first.  They agree with one another, being made from one set of
  simulated subject histories: a confirmed CR/PR has its tumour sizes 30%
  (CR: 0 mm) below baseline, nothing is assessed after progression, the
  PFS and OS events are the progressions and the deaths (`DTHFL`,
  `DTHDT`), doses stop at progression, toxicity, death or the cut-off, and
  a reduced dose follows a grade >= 3 adverse event.  The recipe is
  `data-raw/gen_onco.R` (seeded, base R); the study's mini SAP (populations,
  endpoints, a display list) and CRF outline are installed in
  `system.file("sample", "onco", package = "tflspec")`.  `LazyData: true`.

* **`tfl_args_from_r()` round trips** (#288, 0.0.24.9131; found by S2):
  an argument the schema takes as an integer comes back an integer (`26`,
  not `26.0`), and a figure function's arguments keep the order they were
  written in (the positional ones named in their places).

* **The plan verbs in one recommended order** (#289, 0.0.24.9130; the
  user's decision of 2026-10-16): the order a person designs a table in --
  `normalize_ard`, then the mapping (`table_plan`, `plan_cells`,
  `plan_stat_groups`, `plan_total`, `plan_compare`, `plan_filter`), then
  the finishing (`plan_labels`, `plan_levels`, `plan_nest`,
  `plan_digits`, `plan_sort`, `plan_col_header`, `plan_stub`,
  `plan_blanks`, the pages, `plan_row_group`, `plan_hide`,
  `plan_columns`, `plan_style`, `plan_cell_style`).  The schema's `rank`
  follows it, and every plan verb and figure function says its `stage`
  (`mapping`, `finishing`, or a figure's `output`: `tfl_schema(fn)$stage`);
  the code a table workbook stands for (`tfl_table_code()`,
  `tfl_content_code()`, the converter) writes its verbs in that order.
  The tables are the same: SAMPLE-01's 12 tables give the same RTF byte
  for byte, and every table case through its workbook and its code the
  same RTF as its plan.

* **Every design converts; four extension packages in the figure catalog**
  (#286, 0.0.24.9128).  `tfl_fig_content_from_design()` converts a figure
  type of its own (ae_dot, butterfly, edish, sankey, sunburst, forest_or)
  to its R (`content: !r`), which `tfl_content_run()` runs with the
  datasets by their names after the figure setup -- the same figure; and a
  composed design (`plots:` + `compose:`) to named plots sharing one data
  part (each plot's objects renamed after it, `df_km`, `fit_km`),
  `compose.layout` to `wrap_plots()` (one row or one column),
  `compose.add` to items (`&` kept as the item's `op`), `plot.dpi` to
  `tfl_image`.  The figure catalog adds ggrepel's `geom_label_repel`,
  ggtext's `geom_richtext`, `element_markdown`, `element_textbox_simple`,
  ggnewscale's `new_scale_fill` and ggh4x's `facet_nested`,
  `facet_nested_wrap`, `facetted_pos_scales` (in Suggests); a function of
  a package the program does not attach is written `pkg::fn`.

* **`tfl_fig_template(form = "items")`** (#284, 0.0.24.9127): a template
  as a figure report's content of function calls -- `template`, `args`
  (what it was given), `data` (the report's `data:`), `content` (each
  item, a named plot's too, `from = "template:<name>"`, the builder's
  mark) and, for a template of its own ARD (`forest_hr`), `analyses`.
  The designer's design stays the default.

* **`tfl_image`: a figure drawn at its size on the page** (#282,
  0.0.24.9126).  A figure of items' `tfl_image` gives its size on the page
  (`width_in` / `height_in`: inches, or `fit`, the page's body) and its
  resolution (`dpi`, 300 when not said): the report draws the figure at
  that size and resolution -- `rtf_figures(doc, rtfplot(content,
  render_width =, render_height =, render_dpi =))` -- where it drew it at
  6.5 x 4.5 in and scaled it into its box (a box of another shape
  stretched it).  With `file` (`{output_id}` filled) the figure's code
  also saves the image (`ggsave()`, the same size, in `format`).  A figure
  of the designer's pieces is placed as before.

* **A figure's content as function calls, plot by plot** (#280,
  0.0.24.9125; the figure designer's redesign, stage F1).  A figure
  report's `content:` is its plot's items, one a call: the function that
  makes the plot first (`ggplot`, `ggsurvfit`, `ggcuminc`, or tflspec's
  own `tfl_risk_panel`, `tfl_text_column`, `tfl_n_panel`), then its layers
  (`geom_*`, `annotate`, ggsurvfit's `add_*`, `tfl_ard_number`) and its
  settings (`scale_*`, `coord_*`, `facet_*`, `labs`, `theme_tfl` /
  `theme`, `guides`); several plots are named (`- km: [items]`,
  `tfl_plot()`) and arranged by patchwork's items (`wrap_plots`,
  `plot_layout`, `plot_annotation`); `tfl_image` is the last item.
  `tfl_content_code()` writes it (`p_<name>` a plot, `content <-
  wrap_plots(...)`, a `position_dodge()` said twice as `pd`) and
  `tfl_content_run()` runs it, after the study's figure setup.  The
  schema has a figure catalog (61 functions: `figure:`, `rank`, `label` in
  English and Japanese, `visible`, `aes`; a catalog geom takes its
  parameters from the installed layer).  `tfl_fig_content_from_design()`
  converts a designer's design once -- the 28 templates of ggplot2 and
  tflplanner's two designed sample figures draw the same figure (layer
  data, labels, axis ranges); the 10 figure types of their own become R.
  `tfl_yaml_from_sheets()` / `tfl_convert_sheets_to_yaml()` gain `figures
  = c("design", "items")` (the designer's form stays the default until
  the designer works in items).  `tfl_args_to_r()` / `tfl_args_from_r()`
  write an item's arguments as R text and read them back (the builder's
  R form).  The AI kit's blank and example figure, the manual's §10.1 and
  the guide's figure section are in the new form.

* **A table's font size is its plan's** (#279, 0.0.24.9124; a user's PK
  table warned `rtf_tables(font_size_half_points = ) is deprecated`).  The
  report sheet's `table_font_size_half_points` is the table's
  `plan_style(font_size_half_points = )` (the sheets' table code, the
  converter's content via the style sheet), no longer `rtf_tables()`'s (a
  listing's pages, made apart by the sheets, still pass it there).  A YAML
  written before, with `page: rtf_tables: font_size_half_points`, has it moved
  to the content's `plan_style()` by `tfl_resolve_output()` (the report's own
  `plan_style` key wins): the table keeps its size.

* **An AE table with no ARD definition gets its hierarchy** (#278,
  0.0.24.9124; a user's AE table stopped at `table_plan(label = ): no
  column 'AEDECOD'`).  The sheets knew a table's hierarchy only from the
  ARD definition's hierarchical analysis; a study whose ARD is made by its
  own program has none.  For a report with no ARD definition, the
  converter takes `normalize_ard()`'s hierarchy from the table's rows and
  label (`group1 = AEBODSYS`, `label = AEDECOD`: `hierarchy = c("AEBODSYS",
  "AEDECOD")`), its `overall` from `overall_label`, and lists it in
  `_not_converted.yml` to check.  A cell's line breaks (Excel keeps them
  as CRLF) are LF in the YAML.

* **An NA argument stays NA; a default template and a report's rows by
  name are both kept** (#276, 0.0.24.9124; a user's study converted from
  the workbooks stopped at `table_plan(label = ): no column 'NA'`).  A
  logical NA (`table_plan(label = NA)`) is written `.na` and read back NA,
  not the text "NA".  The layering of a named-dots verb keeps a key that is
  one template in the defaults and rows by name in the report (or the
  other way) as both, the template first: `plan_cells(default = list("{n}
  ({p}%)", n = ..., orr_ci = ...))`, as the sheets make it.

* **A figure with no design gets nothing of a table's** (#274, 0.0.24.9124).
  The converter gave a figure with no design (and no code of its own),
  and a listing with no rows, a table's data and content (`table_plan: {}`):
  its data is its own code now (`source: code`), a listing's a dataset.

* **A nested variable keeps its place in the round trip** (#273,
  0.0.24.9123; found by S2 with tflplanner's SAMPLE-01).  A variable under
  another's level (`under`, `plan_nest()`) with no heading of its own is
  not in `plan_labels()`, whose order is the variables' order: read back
  from the content (a YAML report to the sheets), it now goes right after
  the variable it is under, so the variables keep the order they were
  written in.

* **The converter keeps what tflplanner's study has** (#270, 0.0.24.9122; for
  tflplanner reading and writing the YAML spec).  The listing / figure
  workbook's `figures` sheet (the datasets a figure or report written by
  hand reads, and whether it reads its ARD) is the report's
  `data.datasets` / `data.ard`; a row's `note` stays (`note:`), a table's
  note is its report's; R text keeps `\n` line ends.  The sheets back
  (`tfl_sheets_from_yaml()`) give them, a blank field as a blank cell (not
  `"NA"`), and no figure design for a figure that is its own code.

* **A report's document says its RTF file** (#268, 0.0.24.9121; rtfreporter's
  `rtf_document(file = )`, >= 0.8.2.9063).  `tfl_report()` and
  `tfl_report_code()` give the document the path `tfl_report_path()` says
  (a YAML report's with its `study`), so `generate_rtfreport(doc)` writes
  it and the program says its output once, beside its program.  A path
  given to `generate_rtfreport()` still wins.

* **`tfl_report_path()` takes a YAML report** (#264, 0.0.24.9120).  With
  `study =` (as `tfl_report()`): its TOC entry's `file`, else
  `study.yml`'s `settings: file` (`"{output_id}.rtf"` when neither says
  it), in `settings: output_path`.

* **The AI kit asks for the study's YAML files, in one answer** (#265,
  0.0.24.9119).  `tflspec_ai_manual(dir)` writes seven files (eight with
  the ADaM metadata, now `07_adam_metadata.csv`): the request, the
  manual, the guide, the schema (`03_spec_schema.yml`, the installed
  `inst/spec/schema.yml`), every file of a study blank
  (`04_blank_spec.md`), the example TOC and shells and the answer written
  from them (`06_example_spec.md`, made through `tfl_yaml_from_sheets()`).
  The chat answers with one Markdown text, a `### <path>` heading and a
  `yaml` block a file; the new `tfl_ai_unpack(answer, dir)` writes it
  into a study folder (only `study.yml` and `spec/**.yml`; nothing is
  written when a path, a duplicate or a block is wrong).
  `tfl_ai_review(dir)` reviews a study folder: each report file against
  the schema (K16-K18), the definition through the workbook's checks with
  each finding named by its file and key, `spec/_assumptions.yml` (K08,
  K09), the figures; the two prompts go into the folder.  A workbook or
  one report file is reviewed as before.  `tflspec_ai_manual()` loses
  `split` (there are no blank workbooks).  The manual gains §5, the study
  folder; the guide is rewritten for the YAML files.

* **The defaults' content is a table's** (#263, 0.0.24.9117; found by the
  AI kit).  `tfl_resolve_output()` layers `defaults.yml`'s `content`
  (normalize_ard, the digits ...) onto a table only: a figure or a
  listing takes the defaults' page and code lists, not those layers
  (a figure's code stopped at "Unknown layer: normalize_ard").

* **A report's ARD analyses are its `ard:` section** (#261, 0.0.24.9116;
  the user's decision of 2026-10-15: the ARD is made in a stage of its
  own, before the reports, and every report has the same three steps --
  1 data, 2 content, 3 page and output).  A report file's
  `data.analysis_data` / `data.analyses` move to `ard: {analysis_data,
  analyses}` (in the report's own file), and `data:` keeps what the
  report reads (`source: ard | code | dataset`).  `tfl_output(ard = )`;
  the reader, the writer, the converter, `tfl_sheets_from_yaml()`, the
  flat xlsx and its import follow.  The generated code is unchanged.

* **The YAML spec, stage 0** (0.0.24.9112; the user's decision of
  2026-10-14 to make YAML the source of a study's definition, one file a
  report): the R objects `tfl_r()` (the `!r` tag), `tfl_unset()` (the
  `!unset` tag), `tfl_call()` (a nested `{fn:, args:}` call), `tfl_item()`
  (one layer of `content:`, a function and its arguments by name) and
  `tfl_output()` (one report), with `tfl_read_output()` /
  `tfl_write_output()` for `spec/outputs/<id>.yml` and the fixtures
  `inst/extdata/spec-yaml/`.  Stage 1 (part): the study's files
  (`tfl_read_study_spec()`, `tfl_read_outputs()`), the layering of a
  report over `defaults` and `patterns/<name>` (`tfl_resolve_output()`:
  the later wins: maps by key and `!unset` removes a key; a verb whose
  `...` is named (plan_cells, plan_labels, plan_levels, plan_digits,
  plan_stat_groups, plan_compare) merges by key, any other verb's items
  replace the layers' items of it, a verb only the report has goes where
  the generators' order puts it (the schema's `rank`); a repeated item is
  written as it is), the one-time conversion of a study's sheets
  (`tfl_convert_sheets_to_yaml()`, `tfl_yaml_from_sheets()`, with
  `_not_converted.yml`; a table's `content:` through the bridge), the sheets
  back from the YAML (`tfl_sheets_from_yaml()`), and the flat view as a
  workbook (`tfl_write_spec_xlsx()`, `tfl_import_spec_xlsx()`, best-effort).
  study.yml carries `spec_version` and the packages' versions it was
  written with (a newer `spec_version` is refused).
  `tfl_fig_r()` is `tfl_r()` now, for every kind (no alias).  The
  generators follow (design: `_archive/spec-format-redesign-2026-10-14.md`).

* **The YAML spec, stage 1: the schema and the content's code.**
  `inst/spec/schema.yml` types each argument a spec item gives -- a YAML
  map for a named vector, rows for a data frame, `!r` for an expression, a
  formula or a guard, `{fn:, args:}` for a nested call -- and the grammar of
  `...` for the 13 plan verbs whose body it is (`plan_cells()`,
  `plan_digits()`, `plan_sort()`, ...); `tfl_schema()` returns it and
  `tfl_schema_check()` checks one item (an unknown argument warns, the
  installed package's formals have the last word).  `tfl_content_code()`
  writes a table's `content:` as its code, one line an item in the items'
  order (`ard |> normalize_ard() |> table_plan() |> plan_*()`), an item from
  the study's defaults or a pattern marked `# (study default)`; and
  `tfl_content_run()` makes the same plan from the same calls without the
  code (the written code, run, gives the same table).  `tfl_table_code()`
  and `tfl_table_plan()` now go through the same items, from a workbook's
  sheets by a bridge that also goes back (content to sheets), so a workbook
  and a spec file write the same code: byte for byte the same as before for
  every bundled workbook, the tflplanner sample and the table cases.

* **The YAML spec, stage 2: every kind's code from its spec file**
  (0.0.24.9113).  `tfl_content_code()` / `tfl_content_run()` write and run
  a listing (`plan_listing()` with `listing_col()`, the same RTF as
  `listing_spec()`), a figure (its layers as the figure designer has
  them: `tfl_fig_design_code()` of the design the report stands for) and
  a report whose content is its own code (`content: !r`, the old
  `content_code`; `data: {source: code}` for the old `data_code`); a
  table's or a listing's `data.steps` (the old `process_code`) come before
  the plan.  `tfl_report()` and `tfl_report_code()` take a report of the
  YAML spec: its `page:` is the document's calls by name (`rtf_page`,
  `rtf_default_format`, `rtf_document`, `rtf_tables` / `rtf_figures`,
  `rtf_titles`, `rtf_footnotes`) with the lines of `header`, `footer`,
  `titles` and `footnotes` and the `tokens`; `setup = TRUE` leaves the
  study's (its defaults) to the setup code.  An ARD analysis is its calls
  (`cards::ard_summary()` and the pipe after it), written and run from the
  same list.  The schema covers the cards and cardx analyses, `page:`'s
  calls and the figure designer's layers (made from `tfl_fig_parts()`); a
  call of another package is written `pkg::fn`.  The conversion puts
  `normalize_ard` in the defaults' content, the report's file and program
  in `settings:`, `page:` in the new shape, and reads an old figure design
  as the designer did (YAML 1.1: `yes` is TRUE).  On the tflplanner sample,
  the code is byte for byte as before: 12 tables, 17 report documents
  (with and without `setup`), 27 analyses, the designer's figures.

* **The YAML spec, stage 3: one schema for the help, the AI and the
  figures** (0.0.24.9115).  `inst/spec/columns.csv` is gone: the spec
  sheets' columns are the schema file's (`.sheets` of `inst/spec/schema.yml`),
  and `tfl_spec_columns()` returns them as before, every text and order the
  same (the xlsx header comments, the GUI's `?`, the AI kit).  A column that
  is a function's argument says which (`at: rtf_page.paper_size`), and the
  new `tfl_schema_help(fn, arg)` reads that one text for the YAML spec's
  item.  A figure's own ggplot2 layer (`geom: geom_area`, `aes:`, `params:`)
  is checked against the installed package's layer function -- its
  aesthetics and parameters, an unknown one a warning -- and
  `tfl_schema("geom_point")` (or `"ggrepel::geom_text_repel"`) describes it.
  `tfl_ai_review()` reviews a report of the YAML spec
  (`spec/outputs/<id>.yml`): each item against the schema (K16, K17) and
  each `!r` where the schema has a form of its own for the value (K18).

* **The YAML spec: the ARD program from a study's spec files**
  (0.0.24.9114).  `tfl_ard_code()` and `tfl_build_ard()` take a YAML study
  (`tfl_read_study_spec()`): its data catalog and each report's
  `data.analysis_data` / `data.analyses`, whatever the report's kind (a
  figure's hazard ratios too), the reports' own code lists, the reports in
  the TOC's order.  Each analysis that is a call is written from its calls
  (`.ard_analysis_call()`), the list the run reads.  On the tflplanner
  sample the program is byte for byte the definition's (in the TOC's order)
  and the ARD it builds the same, row for row.

* **`populations` has a `label` column**: the text a header or footnote
  prints for the analysis set (`Safety Analysis Set`), also its name in CDISC
  ARS (`tfl_ars()` writes it as the analysis set's `name`, `tfl_ars_to_specs()`
  reads a name that differs from the id back into it).  Blank: the id, as
  before.  A workbook with no `label` column still reads; one with it does
  not read on an earlier tflspec (an unknown column), so tflplanner asks for
  this version.  (#252; part 3 of tflplanner#388.)

* **The AI kit takes the study's ADaM metadata**: `tflspec_ai_manual(dir,
  define = , adam_spec = , datasets = )` writes `08_adam_metadata.csv`, so
  the assistant names the study's own datasets, variables and levels instead
  of guessing them from the ADaM IG; the request and the guide tell it to
  take them only from there and to list in `_assumptions` any it could not.
  New `tfl_adam_metadata()` builds it, one row a variable, from one source
  in this order: the define.xml, the ADaM spec, the datasets' column
  information (names, labels, kinds, short code lists; never a subject's
  values).  `00_START_HERE.md` says what to give the AI (required, strongly
  recommended, nice to have), what never, and what happens when something
  is missing.  `tfl_ai_context("toc", datasets = )` takes the same frame, as
  the figure task did, and `tfl_ai_review(metadata = )` checks a workbook's
  names against it: an unknown dataset or variable (K14, with the nearest
  names) or a code list value not among a variable's levels (K15) is the
  AI's to fix.

* **`tables$keep_where`, `keep_any`, `keep_parents`**: rows kept by a
  condition, as rtfreporter's `plan_filter()` (rtfreporter #620, needs
  0.8.2.9050): `p >= 0.05` keeps the PTs with 5% or more in any arm. The
  condition is checked for being R at the sheet; `tfl_as_table_spec()`
  writes it back.

* **`tables$show_keys`, and `.variables` / `.others` in `tables$stat_groups`**
  (rtfreporter #621, needs 0.8.2.9049): a subgroup's own n (%) beside its
  ORR. `SEX | AGEGR1` becomes `table_plan(show_keys = )`; `n = N | orr_ci =
  n, estimate | .variables = RESP | .others = n` becomes
  `plan_stat_groups(..., .variables = "RESP", .others = "n")`.

* **`tables$stack_nest`**: `TRUE` writes `normalize_ard(stack_nest = TRUE)`
  (rtfreporter #623, needs 0.8.2.9048), a crossed layer's outer level as a
  heading row.

* **`tables$compare`, `compare_by`, `compare_position`: comparison columns
  from the sheets** (rtfreporter #625, needs rtfreporter 0.8.2.9047).  `p |
  diff` becomes `plan_compare(p = ..., diff = ...)`, each column's templates
  the `cells` rows whose `variable` is its name; a test's p-value, a
  difference and its CI go on their variable's row, from the ARD.
  `tfl_as_table_spec()` writes them back.

* **`layout$group_heading`: each page's name from a template**, as
  rtfreporter's `plan_paginate_group(heading = )` (rtfreporter #619):
  `Treatment: {value} (N={n})` gives a section per arm that says the arm and
  its N from the ARD.  `tfl_as_table_spec()` writes it back.
  `layout$pages_heading` does the same for BY pages (`pages_page_by`), a
  page per arm with a row budget inside.

* **`tables$stack`: parallel subgroups as one pair of keys** (rtfreporter
  #619).  `subgroup = ALL, SEX, AGEGR1` makes `tfl_normalize_code()` write
  `normalize_ard(stack = list(subgroup = c("ALL", "SEX", "AGEGR1")))`, so
  `rows` names `subgroup` / `subgroup_level` whatever group slot each
  variable came in, instead of `group2` / `group2_level`.  A layer may be
  crossed, `AGEGR1 * SEX` (rows `AGEGR1 / SEX` | `65-80 / F`, joined by
  `tables$stack_sep`), and the strata slots stand for what the ARD has
  there: `subgroup = group2, group3`.

* **`tables$stat_groups`: one record's statistics in two columns from the
  sheet** (rtfreporter #619, needs rtfreporter 0.8.2.9046).  `n = N |
  orr_ci = n, estimate, conf.low, conf.high` becomes
  `plan_stat_groups(n = "N", orr_ci = c(...))`, its key `.stat_group`
  named in `cols` (`TRT01A | .stat_group`), a `cells` row's `variable` a
  group's name; `tfl_as_table_spec()` writes it back.

* **An AE table's "Subjects with at least one TEAE" row from the sheets**
  (#236).  The `tables` sheet's new column `overall_label` is that row's
  text; its `hierarchical` analysis computes it (`args` `over_variables =
  TRUE`).  The row is the normalization's, so the new
  `tfl_normalize_code(spec, ard_spec, output_id)` writes the report's line
  -- `data <- normalize_ard(ard, hierarchy = c("AEBODSYS", "AEDECOD"),
  overall = "Subjects with at least one TEAE")` -- with the hierarchy of
  the report's hierarchical analysis, so each SOC row has its counts too;
  a report with neither stays `data <- normalize_ard(ard)`.  The AI kit's
  example T-2 and the TOC / shell guide now show the row.  The shipped AE
  example (`extdata/ard-spec/AE.xlsx`, `label = AEDECOD`) needs that
  hierarchy too: without it the PT has no column of its own; a test now
  runs it on an ARD normalized as `tfl_normalize_code()` writes (#237).

* **The siera profile's file runs in siera** (#239).  siera stops on the
  first output it cannot handle, so the sample's siera file gave no
  programme at all.  `tfl_ars(profile = "siera")` now leaves out the
  outputs siera cannot run -- fewer than three analyses (it applies the
  analysis set from an output's third), an analysis without a grouping (a
  Total column, a study total), a grouping on a column its dataset does not
  have (one the spec derives; needs the new `columns =`, the datasets'
  column names) -- and lists each in `tfl_ars_unmapped()` (item `siera`,
  with why).  Checked on tflplanner's sample: siera reads the whole file and
  runs the other five outputs, whose 1,721 numbers equal the study ARD's.

* **A figure's size is its report's: its own size, fit to the page, or
  inches** (0.0.24.9102, the user's request on tflplanner's step 3).  The
  report sheet's `figure_width_in` / `figure_height_in` take a number of
  inches, `fit` (the page's body: the page or paper in its orientation
  less its margins, its height less the report's title and footnote
  lines) or blank -- the figure's own size, as rtfreporter places an
  image: `tfl_fig_page_size(native = )` says each.  The report step sizes
  only what is said (`rtf_figures(width_twips = , height_twips = )`), and a
  report program draws the figure at that size.  A design's `plot` keeps
  only `dpi`, the resolution (pixels = inches x dpi; it does not change
  the size); its `width`, `height` and `units` go (an old design's are
  read as nothing).  A script of its own (`tfl_fig_design_code(save = TRUE)`)
  saves its PNG at `width_in` x `height_in` (7.5 x 4.5 by default; the KM
  templates' 8.33 x 4.79 of the style options no longer): the templates'
  snapshots change by those lines.

* **A report is a table, a listing or a figure; any of its steps may be
  its own code** (0.0.24.9102, tflplanner's user-program design, the user's direction:
  "user code" was a fourth type forced into line).  The report sheet's
  `data_code` and `content_code` hold the report's own R for its data
  step and its content step: a step whose code is not blank is that code,
  instead of the definition.  The data step leaves `data` (a figure:
  `df`), the content step `content`; the report's datasets (by their
  names in lower case) and, when it reads its ARD, `ard` are in reach.
  `type` takes `table`, `listing` and `figure` only: `user` is an error
  that says where its code goes.  `tfl_check_code()` checks a step's code
  against the rules, reading it (rules U01-U05: R that does not parse, a
  dataset the report does not register, nothing left for the next step,
  files read or packages attached by the code, `ard` without the report
  reading its ARD), and says the catalog's datasets it uses.  A report
  whose content is its own code and has no analyses is not an ARS output
  (said in `tfl_ars_unmapped()`), as a `user` report was.
  `tfl_listing_code(part = )`: `"data"` (the data up to its order, for a
  content step of the report's own code) or `"content"` (the listing laid
  out from a `data` a data step of its own code made); `"all"` as before.

* **A figure's data steps: each reads an object and leaves one**
  (0.0.24.9102, tflplanner's generalised data steps, the user's
  questions Q1-Q3).  Any data step may say the object it reads (`in`) and
  the one it leaves (`out`): `{step: filter, expr: AVAL > 0, out: df_ev}`
  writes `df_ev <- df |> filter(AVAL > 0)` and keeps `df`;
  `{step: survfit, in: df_ev}` fits it.  Unset, everything is as before:
  a read makes `df`, a step on rows changes the object above it, a named
  step reads `df` -- except that a second read now makes `df2` (then
  `df3` ...) where it made `df` again.  A dataset by its name
  (`in: adsl`) is read.  `tfl_fig_objects()` lists each step's `in` and
  `out`, for a GUI to offer.
* **Two data steps**: `function`, any function's result
  (`fn: survival::survfit`, `args` in R, the object it reads given to its
  `data_arg`, `data` by default), and `ard`, an analysis of the figure's
  ARD (`analysis_id`, `fn`, `in` an analysis data, `args`): a row of the
  study's ARD definition the GUI writes, no line of the figure's program
  (the program reads the ARD).  The specific steps (`survfit`,
  `summary`, `rate`, `count`) stay.
* `tfl_check_fig_design()`: an `in` no step made before (and not a
  dataset) is a problem; the columns follow the object a step reads.

* **The study's ARD program puts a code list on a data once** (#235).
  With code lists that relabel (`M` = Male), `tfl_ard_code()` for the
  whole study (no `output_id`) put them on the dataset as it was read and
  again on the analysis set made from it; the second found "Male" where
  it expected `M` and stopped the program.  Each report's part now puts
  them, as one report's program always did, only on the data its
  analyses read: the study's ARD is the reports' ARDs bound together.

* **An AI kit to draft a study's specs from its TOC and shells**
  (`tflspec_ai_manual(dir)`).  A chat cannot read the disk, and most take
  ten files at most: `tflspec_ai_manual(dir)` writes one flat folder of
  eight -- the request to paste (`00_START_HERE.md`), the manual, a new
  guide for the assistant (`02_tflspec-ai-toc-shell-guide.md`: reading a
  company's TOC and shells -- header rows, Title1-3, populations,
  footnotes, column headers and `(N=xx)`, decimals, indentation, pages --
  into the ARD, table, report and listing specs and a figure design; a
  checklist; an `_assumptions` sheet for every guess), every spec sheet
  blank with a `_columns` sheet (written now from the column catalog;
  `split = TRUE`: one workbook a spec), a blank figure design, and a
  made-up study end to end (its TOC and four shells -- demographics, AEs by
  SOC and PT, a listing of serious AEs, mean blood pressure by visit --
  and the specs written from them, which run on the pilot data in the
  tests).  With no `dir` it returns the manual's path, as before; the
  former `file =` (one copy of the manual) is gone.
* **`tfl_ai_review(file, toc, adam, others)`**: one file an assistant
  returned, checked and split by who acts on it.  For the AI: a sheet or
  column that is not the spec's, an output or title that is not the TOC's,
  `(N=86)` where `(N={n})` belongs, analyses no table uses, a missing
  `_assumptions` sheet, and the review's errors and checks
  (`tfl_review_spec()`, `tfl_check_fig_design()`).  For the person: each
  assumption, each analysis set's flag, each denominator, each test or
  model, each fixed order of levels.  It writes `<file>_repair.md` (paste
  into the chat) and `<file>_questions.md` (answer and paste back).  The
  kit's rules K01-K13 and who acts on each review rule are
  `inst/review/ai_rules.csv`.

* **The treatment colours** (#233).
  - The order of the `treatment` palette's rows in the figure style standard
    is the arms' colour order (now said in `?tfl_fig_style`).  Its first
    colour is Okabe-Ito blue `#0072B2` (was `"blue"`), so the six built-in
    colours are all Okabe-Ito, in a colour-blind-safe order.
  - More arms than colours: the setup's `tfl_colours()` repeats them with a
    warning naming the arms, and `tfl_check()` / `tfl_check_fig()` say which
    arms of a legend share a colour.  A program written without the setup,
    and the quick figures' (`tfl_fig_*()`), repeat them too (they drew the
    extra arms grey).
  - Optional `treatment_shapes` / `treatment_linetypes` settings (blank by
    default: no figure changes) give the arms point shapes and line types in
    the same order, for print in black and white.  A designed figure that
    colours by the arms maps them too -- a line's `linetype`, a point's
    `shape`, the KM curves' `linetype_aes = TRUE` -- with
    `scale_linetype_manual()` / `scale_shape_manual()` and legend keys long
    enough to show the line types; the setup's `tfl_shapes()` /
    `tfl_linetypes()` give them.

* **The generated programs, reviewed again** (tflplanner #347).  What the
  programs say is the same -- the sample study's tables and listing are the
  same RTF byte for byte -- and they read as a person writes them:
  - *ARD programs*: an ARD object never takes a function's name
    (`ard_stack_1`, not `ard_stack <- ... ard_stack(`); an analysis set that is
    its dataset as it is (`pop_enr <- adsl`) is the dataset; a `filter()`'s
    conditions are its arguments (`filter(A, B)`, not `A & B`); a long step
    (a `derive`'s `case_when()`, a `filter()`) is laid out an argument a line,
    and a pipe step leaves room for its ` |>` in 80 columns; every analysis
    says its id above it, and an analysis data its label; a custom analysis
    over several lines ends its call before the pipe goes on; a Total column's
    two calls are piped from their data, with a comment saying why there are
    two; `ard_setup.R` writes only the computed statistics the analyses use
    (`var = stats::var`, `sum`; a long one as a block).
  - *Table and report programs*: the report is one pipe (`rtf_document() |>
    rtf_section() |> rtf_tables()`), the section as `rtf_section(header = ,
    footer = )` (needs rtfreporter >= 0.8.2.9040); a report's footer that is
    its own lines and the study's names the study's (`c(list(...),
    study_footer_lines)`, which the setup now defines with
    `study_header_lines`); the program's path through the study's path
    variables; the levels as code lists at the head (`cl_sex <- c(...)`,
    `plan_levels(SEX = cl_sex)`), as the ARD programs have them; a header grid
    as `tribble()` rows (when the program attaches dplyr); whole numbers
    without `L`.
  - *Listings*: the records, order and dates in one pipe
    (`mutate(across(where(...), format))`, not `data[] <- lapply(...)`), and
    `listing_spec()` laid out as the other calls.
  - *Figures*: the data under a name that says what it is (`adtte_ttde`, not
    `df`; `tfl_fig_data_name()` gives it, for an app showing the objects);
    with the study's setup, `theme_tfl()` instead of the theme's
    numbers (boxed and L_axis); the legend in one `theme()`; the numbers an
    `ard_number` prints named in the data part (`ard_label_1 <- ...`) and the
    plot naming them; a palette no layer uses is not written; panels named
    for what they are (`p_text`, not `p_txt4`); long terms wrapped; a code
    step says its `label` (a new field of `code` and `layer_code`) instead of
    "your code".  The forest template: no title on its subgroup axis
    (`labs(y = NULL)` survives YAML), its rows made with dplyr (no
    `split() |> lapply()`), its Cox model named and its event written
    `CNSR == 0`, as the KM figures write it.
* **Figures checked and written faster** (0.0.24.9096, #229; tflplanner's
  study opening).  The ggplot2 compatibility walk works out once per
  target which versions it reaches and compiles each name pattern once,
  instead of for every piece: `tfl_check_fig_design()` about 2.5 times
  faster, `tfl_fig_design_code()` 1.5 times, the same results.
  `tfl_fig_uses_ard()` says whether a design reads an ARD without writing
  its script (what the script's `attr(, "ard")` says).  A report's ARD
  fingerprint (`tfl_ard_spec_hash()`, asked for every report as a study
  opens) writes one file of the session's over instead of making and
  deleting one each time, the analysis data sheet is normalized once
  while it is the same, and the code lists are checked once while they
  are the same (bad ones fail every call): the same fingerprints.

* **A time's unit is a named constant, not a number in each program**
  (tflplanner, the user's request).  A figure's time-unit step (and the
  swimmer templates' assessment days) divides by `days_per_week`,
  `days_per_month` or `days_per_year` -- written
  `mutate(AVAL = AVAL / days_per_month)` (no data column shares a program
  constant's name: tflplanner's review checks it) -- instead of `7`,
  `30.4375`, `365.25`.  A study's setup sets them (tflplanner: from the
  company standards); the figure setup (`tfl_fig_setup_code()`) defines
  each only when it is not defined yet, with the values of the new
  `tfl_time_unit_days()`, and a figure script of its own defines the ones
  it uses the same way.  The templates' snapshots and the program fixtures
  change by that line.  The review's catalog has the rule tflplanner
  checks it with: **D03**, a dataset column named like a program constant
  (an error: rename the column).

* **Page patterns** (tflplanner's page-patterns design).  The report
  half -- `report`, `page`, `header`, `footer`, `titles`, `footnotes`,
  `tokens` -- holds named page patterns besides the blank rows: rows whose
  `output_id` is `@` and a name (`@Compact`), saying only what differs
  from Standard (the blank rows).  A report chooses one with the new
  `report$pattern` column (blank: Standard).  A report is resolved in two
  stages -- its own rows over its pattern's, then over Standard's -- by
  the rules each sheet already has (cell by cell, by line, by name), and
  `(none)` takes a line out at either level.  Standard's header, footer,
  tokens and font stay the study's setup (`tfl_report_setup_code()`);
  what a pattern changes is written in its reports' programs.  The
  checks: an `@` row only on those sheets, a pattern's name (a letter,
  then letters, digits or `_`), a pattern no row defines, a pattern
  chosen on the blank row or by a pattern.  A workbook without patterns
  reads and writes as before, with the same programs.
* **`tfl_read_toc()` reads a data frame** (tflplanner #299, D10).  `path`
  may be a data frame of the TOC's rows (its names the column names) in
  place of a file; `sheet` and `skip` do not apply, and a row with no
  output id is named by its row in the data frame.  The rows go through
  the one reader as a file's do -- tflplanner's import by company rules
  hands it the rows it has read, one report a phase, without a temporary
  file.
* **The forest plot's analysis data is named by tflplanner's rule**,
  `<data>_<population>_<param>` (`adtte_fas_os`; was `adtte_os`), the
  population without its flag's `FL` -- the same rule tflplanner names
  every analysis data it suggests by (tflplanner #293, its figure work).

* **The forest plot from the figure's own ARD** (tflplanner #293 phase
  6).  `forest_hr` is a parts template now: its hazard ratios, intervals
  and N are statistics of an ARD made by cards / cardx -- one Cox model
  of all subjects (`cardx::ard_regression()`) and one within each subgroup
  variable (`cards::ard_strata()`), each a `custom` analysis -- read with
  `ard_stats()`, not fitted inside the figure program.  The analyses the
  plot needs come with the design, `attr(design, "analyses")`, from the
  new `tfl_fig_forest_analyses()`; `tfl_fig_template()` takes `subgroups`
  and `comparison` (the arm against the reference).  Two new pieces
  serve it: the layer `text_column` (columns of text at the right of the
  plot, a panel of its own: N, the estimate and its interval) -- panels
  now go below the plot or at its right -- and the catalog layer
  `errorbar_h` (a horizontal error bar).  A subgroup whose model did not
  converge has no point and prints NE.  The template snapshot of
  `forest_hr` changes from the whole script to the parts.

* **Review rule A01 checks an analysis data's `add` columns in the
  population's data** (ADSL), where `add` takes them from by the
  subject, not in the analysis data's own dataset: an ADTTE data adding
  SEX was said to read a column ADTTE has not got.  And a figure with
  analyses of its own (`figures` given) is not reviewed as a table: no
  "no column key" (T09) for a forest plot.

* **AI-assisted drafting, the network-free core** (#182, part 1: the `toc`
  and `figure` tasks).  tflspec writes the prompt and reads the answer; it
  sends nothing (no HTTP client, no key):
  - `tfl_ai_context()`: the study facts a prompt carries, checked (the
    study, the report, its titles and analysis set, the datasets' names,
    labels and kinds -- never their values -- the current design, the
    TOC's columns and reports);
  - `tfl_ai_prompt()`: the prompt, `system` + `user` (+ `attach` in chat
    mode, the documents' text between `<document>` fences in API mode,
    `messages` for a client), one text to copy with `format()`.  English,
    with the assumptions asked in the user's language; the fixed wording
    is in `inst/ai/prompts/*.md`;
  - `tfl_ai_schema()`: the columns or pieces the answer may write,
    generated from what tflspec reads (`tfl_read_toc()`'s fields;
    `tfl_fig_parts()`, `tfl_fig_templates()`), so the prompt cannot drift;
  - `tfl_ai_parse()`: the answer's block, YAML by default and JSON too,
    with the header `tflspec_ai: {task, version, output_id}`; nothing is
    repaired silently: a block that does not read, an unknown column or
    key, a header of another task or version, a block cut off are problems
    with their place;
  - `tfl_ai_check()`: the answer through `tfl_read_toc()` /
    `tfl_check_fig_design()` in its context (`where`, `problem`,
    `severity`); `tfl_ai_repair_prompt()`: the problems sent back with the
    schema of the offending parts only;
  - `tfl_ai_diff()`: what the answer changes, a TOC's rows by
    `output_id`, a figure's pieces by name and position (`added`,
    `changed` field by field, `removed`, `moved`, `same`);
  - `tfl_ai_tasks()` lists the tasks; `table` and `ard` come later.

* **Document import, level 1: a define read by rule**
  (ichirio/tflplanner#283).  No AI: a define is standard CDISC XML.
  - `tfl_read_define()` reads a Define-XML 2.0 or 2.1 (`xml2`, Suggests)
    into tidy data frames: `datasets` (`ItemGroupDef`), `variables`
    (`ItemRef` + `ItemDef`: label, type, length, code list ...),
    `codelists` (`CodeListItem` / `EnumeratedItem`: coded value, decode,
    order, rank, extended, NCI code), `external` (MedDRA ...) and `study`.
    Namespaces are found by URI, texts in the language asked for.
  - `tfl_read_define_spec()` reads the workbook a define is made from
    through an import profile, `tfl_define_profile()`: the Pinnacle 21
    layout by default, any sheet or column changed by `map` for a
    company's own; the same `tfl_define`.
  - `tfl_define_codelists(define, variables, output_id)` gives a report's
    `codelists` rows (value = coded value, label = decode, order) and its
    variables' labels (rows of the `variables` sheet, in
    `attr(, "variables")`), a `note` saying where each came from; `output_id` is required.  What the define cannot give
    (no code list, an external dictionary, a variable it lacks) is listed
    in `attr(, "set_by_hand")`, not guessed.

* **The pkgdown site, tidied** (#162): a "Get started" article that walks
  the whole flow (an ARD spec -> `tfl_ard_code()` / the ARD -> the bundled
  `DM.xlsx` table -> the bundled `report.xlsx` report), articles on the
  ARD spec, the table spec, reports, listings, figures and ARS, and a
  reference index grouped by the workflow.  The articles follow this
  version's spec: a report's analysis data and code lists, the variables'
  labels, the `digits` sheet, `tables$value`, the page sheet's font, the
  study's setup code (`tfl_report_setup_code()`, `setup = TRUE`), figure
  code piped with `|>`, and a report's analysis data in ARS.  The README
  says the same of the font, the setup code and the code lists.

* **A Total column, read from cards' overall rows** (#212, rtfreporter
  `plan_total()`).  The `analyses` sheet's new column `overall` (`TRUE`)
  runs the analysis again without its `by` and binds it under the grouped
  call (`bind_rows(ard_tabulate(adsl, by = ARM, ...), ard_tabulate(adsl,
  ...))`; on a `cards::ard_stack` row, `.overall = TRUE`): cards' own
  overall rows, with no group.  The `tables` sheet's new `total` (the
  heading, `Total`) and `total_position` (`last` / `first`) are written as
  `plan_total(label = "Total")`, whose `{n}` is the study total.
  `tfl_ars()` writes each such analysis twice, by its groups and over all
  subjects without the grouping (`An_<output>_<analysis>_TOTAL`), so the
  ARS names no `ARM = "Total"` the data does not have.  The definition
  check refuses `overall` without a `by`, on a row inside a parent (it is
  the parent's), on `custom` / `subjects` / `ard_strata` / `ard_pairwise`,
  and together with `.overall` in `args`.  A definition without the
  columns writes the same programs, with the same fingerprints.  Needs
  rtfreporter >= 0.8.2.9034.

* **A variable's rows under one level of another** (tflplanner's RACE
  sample, rtfreporter #598).  The `variables` sheet's new column `under`
  (`RACE: Asian`: that variable, a colon, the level as the table shows
  it) is written as `plan_nest(RSUB = c(RACE = "Asian"))`: the variable's
  rows right after that level's row, one indent deeper, its own heading
  dropped -- the sub-categories of a race from a second analysis on the
  same data.  The definition check names a value that is not
  `variable: level`; `tfl_as_table_spec()` reads the column back from a
  plan.  Needs rtfreporter >= 0.8.2.9032.

* **An output that prints another's analyses is in the ARS** (#293
  phase 5, tflplanner #293).  `tfl_ars(references =)` takes the outputs
  that print another output's analyses -- a figure printing a table's
  median and hazard ratio -- as `output_id`, `source`, `analysis_id`.
  Each is an ARS `Output` (its displays and file, from the report spec)
  whose list-of-contents item names the source's analyses, which are not
  written twice; one whose analyses the ARS has none of is listed by
  `tfl_ars_unmapped()` with what it prints.  Before, such a figure was
  unmapped ("no analyses in the ARD definition").

## Upgrading from an earlier version

Update tflspec and tflplanner together, open each study in tflplanner and
save it, then look at its Review tab.

* **Six exports are gone; a study keeps them in
  `programs/study_helpers.R`** (#187).  `set_levels()`, `tag_ard()`,
  `fmt_ard()`, `keep_stats()`, `fmt_pvalue()` and `save_ard()` were
  tflspec's up to 0.0.24.9066.  The generated programs still call them,
  from the study's own code: `tfl_helpers_code()`, which tflplanner writes
  to `programs/study_helpers.R`; the study's setup sources it, so the
  programs run without tflspec.  A program that calls
  `tflspec::set_levels()`, or attaches tflspec for these, stops: write it
  again (open the study and save) or source `programs/study_helpers.R`.
  tflplanner's review lists such programs (rule P02).
* **A figure design written before reads as it is** (#201): its `stats:`
  steps and its `data_code` / `stats_code` read as the one `data:` list,
  and a save writes the new form.
* **Figure programs have a new shape, and draw the same figures** (#201):
  a `# ---- data ----` and a `# ---- plot ----` section, one `+` chain.
  Saving the study writes them again.
* **tflplanner 0.0.2.9157 and later need tflspec 0.0.24.9077 or later**:
  update both together.
* **The steps:** update both packages; open each study and save it (the
  programs and `study_helpers.R` are written again; a program edited by
  hand is copied to `programs/.edited/` first); check the Review tab
  (P02 names a program that still calls a removed function).

## Changes

* **Ready for CRAN's review, the parts that are not the submission itself**
  (#218).  `readxl` moves from Suggests to Imports: reading the Excel
  specifications is the package's job, so it was never optional
  (`rtfreporter` follows at the submission, once it is on CRAN; in Imports
  now the check warns that it is not).  `haven` and `writexl` stay in
  Suggests; every function that uses them now says what to install
  when they are missing (`tfl_fig_style_template()`, `tfl_read_ard()` of an
  .xpt file, reading .xpt / .sas7bdat data).  The 14 quick-API figure
  functions and `print.tfl_code()` document what they return; the examples
  of `tfl_table_plan()` and `tfl_as_table_spec()` run (on a bundled
  specification and a small cards ARD) instead of `\dontrun{}`.  The five
  tests that draw every figure type skip on CRAN (CI still runs them).  The
  15 README-unused figures stay out of the built package; the Title quotes
  'Excel' and the Description spells out TFL; the WORDLIST knows the
  package's terms.

* **D02 in plain words** (tflplanner): "%s: the checks against the data
  have not run yet." and "Press [Review with the data]: it reads the
  datasets once (not again until a file changes)." -- "the data facts"
  was a word of the code.
* **The review's catalog has P02** (tflplanner's): a study program that
  calls one of the six removed functions as `tflspec::` or after
  `library(tflspec)`, without `programs/study_helpers.R` sourced.
* **The review's sentences can be translated** (tflplanner #288).  Each
  row of `tfl_review_spec()` gains `template`: the sentence its `args`
  fill (`message` is the two put together, unchanged).  It is the rule's
  message, or, for the rules whose message was the whole sentence, the
  check's own words: a table against its ARD (T06, T07: a key that is
  neither a group nor a variable, cells written for a variable the ARD
  does not analyse, a statistic a template reads), a listing's columns
  (L01, L02), a figure against the data (F03: no dataset, no variable),
  and a figure's advice (F02: `tfl_fig_advice()`'s template and values).
  The new `tfl_review_templates()` lists the sentences beyond the rules',
  so an app translates each once.  `tfl_check_ard()` says what it said.
* **An `ard_number`'s label prints several statistics of its address**
  (tflplanner #311).  `{value}` is its `stat`, as before; `{conf.low}`,
  `{conf.high}` ... (any `stat_name`) are those statistics of the same
  analysis, variable, level and group: `"HR {estimate} (95% CI
  {conf.low}, {conf.high})"` writes one `paste0()`, a number a line.
  `tfl_check_fig_design(ard =)` looks for each statistic the label names.

* **The review: a figure's dataset not read yet is not missing; a review
  of many reports in half the time** (#204).  With the ARD definition
  given, a dataset of its catalog whose facts were not made (a cache that
  has not read every file yet) is no longer said to be missing by F03.
  tflspec's own figure style and the installed ggplot2's version are
  worked out once a session (a review asked for them once a figure), the
  same `|` cells are split once, and the table rules split each
  analysis's variables once a report: 200 reports reviewed in about 1 s
  instead of 2.2 s (installed; the figures' part 0.7 s to 0.1 s).
  The catalog gains the rules of a figure that prints a table's numbers
  (tflplanner's: its ARD source is not a table of the study, F04; the
  design reads an ARD but has no source, F05; an analysis the piece
  names was dropped, F06; the ARD is not made yet, F07; a piece the ARD
  cannot answer, F08), so a study review lists them.
* **A figure can read an ARD** (#293 phase 3, tflplanner #293).  The
  study helpers gain `ard_value()` (one statistic of an ARD, as the ARD
  formatted it or with `digits`; exactly one row or the program stops),
  `ard_stats()` (statistics as a data frame: a row per group and level, a
  column per statistic) and `ard_fingerprint()` (the definition
  `ard_status.csv` recorded for a report).  Two pieces use them: the data
  step `ard_stats` (an object of its own) and the layer `ard_number`
  (prints one statistic -- a median, a hazard ratio -- as an
  `annotate("text")` term of the chain).  The program puts the ARD in
  `ard`; `tfl_fig_design_code()`'s attribute `ard` says the code reads
  it, and `tfl_check_fig_design(ard =)` looks for the pieces' analyses,
  variables, levels, statistics and groups in it.  The report sheet's
  `ard_source` takes `own` and `table:<output_id>` (a figure's: its own
  analyses, or a table's ARD) besides blank and `import:<file>`.

* **A figure's steps are one list; its script two sections and one chain**
  (#201, tflplanner #293 phase 1).  A figure design's `data:` is one
  ordered list: the steps that make an object of their own (`survfit`,
  `summary`, `summary_by`, `rate`, `count`, `subset`), which were `stats:`,
  are in it, and a step after one changes that object, in its pipe;
  `data_code` and `stats_code` are `code`.  A design written before reads
  as the one list (and is written back without `stats:`).
  `tfl_fig_design_code()` writes `# ---- data ----` (one pipe an object,
  the code lists on `df`, the axis breaks, a panel's data) and
  `# ---- plot ----` (the palette first, then one `+` chain into the
  figure; a panel below it is a chain of its own, joined on the last
  line); with `setup = TRUE` there is no header and no `library()`, and
  the attributes `reads`, `libs`, `needs` say what it needs.  The number
  at risk is by default a panel from the figure's own fit; the
  `risk_table` layer's `method: add_risktable` draws ggsurvfit's table
  instead.  The swimmer template's months for the assessments are a step
  of `assess`.  The drawn figures are the same (test-fig-exact); the
  template snapshots change once; the generated programs are fixed byte
  for byte (`tests/testthat/fixtures/fig-programs/`).
* **A review of the whole definition** (#199, tflplanner#288, phase 0).
  `tfl_review_spec(spec, ard, listings, figures)` lists, for every report,
  what cannot be used (`error`), what is valid but probably wrong
  (`check`) and what is missing and has to be set by hand (`hand`), one
  row each: the report, the sheet, the row's key (the variable, the
  analysis_id, `variable / context / row` on `cells`), the column, the
  message, a hint and the rule.  Nothing stops: an ARD definition that
  cannot be built is reviewed as its sheets.  The rules are a catalog,
  `tfl_review_rules()` (`inst/review/rules.csv`): what the constructors
  find (S01); a grouping variable also analysed, statistics of the wrong
  kind, a name the ARD uses itself (A06, A08, A15); a column or row key no
  analysis groups by, a variables or cells row for a variable not
  analysed, a template statistic of the other kind, digits no template
  reads, no column key, no cells (T01-T05, T09, T10); a listing without
  columns (L01); a figure's required field blank and its advice (F01,
  F02); a code list of a variable the report does not read (C01).  With
  the facts of the data, `tfl_data_facts(data, populations = ard)` --
  each column's class and values, each analysis set's subjects, each
  condition's rows, small and with no records, so they can be kept --
  also a column the data do not have, a condition that cannot be
  evaluated or keeps nothing, a flag that is not Y / N, a mean of a
  character column, a derived column that replaces one (A01-A05, A16), a
  code list value no record has (C02) and a data value the code list does
  not have (C03, an error: the program stops on it), a listing's columns
  (L02), a figure's dataset and variables (F03).  With what each report's
  ARD holds (`tfl_ard_facts()`, in `facts$ard`): the statistics, keys and
  levels the table reads (T06-T08) and what went wrong while it was made
  (A17).
* **The constructors collect their problems** (tflplanner#274 D11).
  `tfl_table_spec()`, `tfl_ard_spec()` and `tfl_listing_spec()` find every
  problem, as rows, and stop as before -- the table definition on its
  first, the others with all of them, the messages unchanged -- with a
  condition of class `tflspec_spec_error` that carries the rows as
  `problems` (`output_id`, `sheet`, `row`, `field`, `message`), so a GUI
  reads where a problem is instead of parsing the message.
  `tfl_check_ard()` and the review share its three rules.

* **"Population" everywhere the apps show it** (#197).  The figure
  designer ("Population flag", "Keep a population"), the figure advice,
  five column descriptions of `inst/spec/columns.csv` and the `header_n`
  message no longer say "analysis set".  tflplanner's Japanese for them
  is keyed on these texts: its matching change goes in right after.

* **The example workbooks are made by code of their own** (#195).  The five
  example reports (`inst/extdata/ard-spec/`: DM, AE, ORR, LB, PK) are
  built from new example ARDs and plan code in
  `data-raw/ard-spec-examples/make-examples.R` -- the cards example data
  and made-up values, the same features as before (two column keys, rows
  of conditions, a shift table a page per parameter, columns over several
  pages, significant digits), checked as before against the code and the
  RTF.  Their contents differ: the reports' variables, labels and titles
  are new.  The docs, tests and NEWS no longer point to discussions by
  number, and their examples use neutral variable names.

* **A `.lintr`, the same as tflplanner's and rtfreporter's** (#193).
  `lintr::lint_package()` finds nothing: the example ADaM script uses
  `|>`, three code templates are in double quotes, `d$variable %in% NA` is
  `is.na()`, and a few spaces.  No change in what the package does.

- R-CMD-check now also runs on R 4.2, the oldest R tested (`Depends: R (>= 4.1)` is kept; cardx, used by the generated ARD code, needs R >= 4.2). That job leaves car out (it needs packages that want R >= 4.4) and lets a suggested package be missing; the tests skip what needs it.

* **A population is an analysis set** (#191).  The README and the AI user
  manual say it once: a population (the `populations` sheet,
  `population_id`) is what ICH E9 and CDISC ARS call an analysis set;
  `tfl_ars()` writes it as an `AnalysisSet` and an analysis's
  `population_id` as `analysisSetId`.  The argument hints
  (`inst/ard/args.csv`) say "population" throughout.

* **A report's code lists give the data its labels; the ARD holds them**
  (#187).  `tfl_ard_code(codelists = )` writes each list at the program's
  head, `cl_sex <- c(F = "Female", M = "Male")` (a value named by its
  data value, its label the value it becomes; a list without labels, its
  values), and puts them on the data the analyses read:
  `set_levels(SEX = cl_sex, RACE = cl_race)`.  Each listed column is a
  factor in the list's order with the labels as its values, so the ARD
  holds `"Female"`.
  - A value a list does not have (not NA) now stops the program, naming
    the column and the value -- it was added after the listed ones.
  - The table follows: `plan_levels()` orders the ARD's values (the
    labels), and `plan_labels()` gives the variables' headings only (the
    `variables` sheet's `label`).  The code list of `variable` (an earlier
    form of the headings) moves to the `variables` sheet's `label` when a
    table spec is read, with a message.
  - An analysis's `where` on a listed column it reads is written in its
    labels: `tfl_ard_code()` stops on `SEX == "F"` and says to write
    `"Female"`.  `tfl_ars()` writes the data's values back, in a
    condition and in a grouping's groups (their names the labels).
  - Listings and designed figures take them too:
    `tfl_listing_code(codelists = )` (the columns a listing shows or sorts
    by, after its condition; it sorts in the lists' order) and
    `tfl_fig_design_code(codelists = )` (the last of the figure's data
    steps).  A listing's data is now one dplyr pipe: `filter()`,
    `set_levels()`, `arrange()`.

* **The generated programs run without tflspec** (#187).  The functions
  they call -- `set_levels()`, `tag_ard()`, `fmt_ard()`, `keep_stats()`,
  `fmt_pvalue()`, `save_ard()` -- are code a study keeps in a file of its
  own: `tfl_helpers_code()` gives it, and the whole program
  (`tfl_ard_code(part = "all")`) carries it.  The six exports of 0.0.24.9066
  are removed (no aliases); the setup attaches cards and dplyr only.

* **An analysis data that keeps columns (`keep`) keeps the ones its
  analyses read** (#187): their `by`, `strata` and `variables`, and a
  denominator's `by` -- they were dropped unless listed, and the analysis
  stopped.

* **A program's names are never a column's** (#187): the report's id is
  `report_id` (was `output_id <- "..."`), a code list `cl_<variable>`, a
  folder `path_<folder>`, an analysis's ARD `ard_<id>`.  A test checks
  them against the ADaM's and the ARD's columns.

* **A table's blank rows at positions** (#185).  `layout`'s `blank_where`
  takes row positions as well as a named rule: `0 | 5 | -1` (a blank after
  each; 0 before the first row, -1 after the last) is written
  `plan_blanks(where = c(0, -1))` -- it was the text `"0 | -1"`, which
  stopped the report program -- and `tfl_as_table_spec()` takes a plan's
  positions back.  Whole numbers in the written code read as a person
  writes them (`c(0, -1)`, not `0:-1`).

* **The ARD program reads as a person writes it** (#181).  Same ARDs:
  - no function or loop of its own: it calls `set_levels()`, `tag_ard()`,
    `fmt_ard()`, `keep_stats()`, `fmt_pvalue()` and `save_ard()`, new
    exports named as a program calls them (not `tfl_`); its setup is
    `library(cards)`, `library(dplyr)`, `library(tflspec)` and the
    company standards' values (`tfl_stats`, `fmt_default`, which replace
    `.tfl_stats`, `.fmt_default`; `.fmts(mean = 2L)` is now
    `modifyList(fmt_default, list(mean = 2L))`);
  - one pipe an analysis, from its data: `ard_cont <- adsl_saf |>
    ard_summary(...) |> apply_fmt_fun() |> tag_ard(output_id, "CONT",
    population = "SAF")`; one report's ends in `bind_rows(...) |>
    save_ard(output_id, definition = "...")` (`tfl_ard_code(part = "body",
    save = FALSE)`: in `ard`), with `output_id <- "..."` once at its top --
    no `ards[[i]]`; in the study's program, a part a report into `ards`;
  - the tidyverse layout: a call on one line when it fits in 80
    characters, else an argument a line two spaces in and the closing
    bracket on its own line, a long vector a value a line;
  - dplyr's verbs for the data (`filter()`, `mutate()`, `select()`,
    `left_join()`).  A data frame's column keeps its label through
    `filter()` (base `subset()` dropped it), so a model's `var_label` in
    the ARD is the column's label where the data are plain data frames
    with labels (read as tibbles, as before);
  - a report's code lists on the data its analyses read, as
    `set_levels()`'s arguments (`codelists <- list(...)` once when more
    data take them); an analysis set the data is made from is no longer
    made factors again, and `adsl_saf` is made in one statement again
    (#171 left `pop_saf` and `adsl_saf <- .levels(pop_saf)`);
  - a `custom` analysis's code with the data's names in place of `data`
    and `population` and its last expression the analysis's ARD; in
    `local()` only when it defines a function or assigns a name the
    program has.  An analysis data's `code` of one expression likewise.
    A function's own `data` argument in `args` stays as it is;
  - a comment of the analysis's label only; `# ---- data ----` headings.

* **The group count's analysis is GROUPN** (#181): the subjects per group,
  which clinical reporting calls big N, written by `tfl_ars()` for a
  percentage's denominator (`An_<output>_GROUPN_<by>`, was `BIGN`).  No
  alias.

* **The generated programs name a study's folders and its attached
  packages as its setup does** (#178).  Two options, unset by default
  (the code as before):
  - `tflspec.paths`, a variable for a folder (`c(path_adam = "data/adam")`):
    a dataset, the study ARD and a `source` file under one of them are
    written through it, `readRDS(file.path(path_adam, "adsl.rds"))`;
  - `tflspec.attached`, the packages the setup attaches (`c("cards",
    "dplyr")`): their calls are written without `pkg::` (in the parsed
    code: a string or a comment keeps it), and an ARD program does not
    write `library(cards)` itself.

  `tfl_ard_code()`, `tfl_table_code()`, `tfl_report_code()`,
  `tfl_report_setup_code()`, `tfl_listing_code()`, `tfl_read_data_code()`,
  `tfl_fig_design_code()` and `tfl_fig_setup_code()` read them;
  `tfl_build_ard()` runs its program by itself and ignores them.  See
  `?tflspec_code_options`.

* **The formats in the cards call** (#179).  An ARD program says each
  analysis's formats where its statistics are computed, in the cards
  call's own `fmt_fun`: `fmt_fun = everything() ~ .fmts(mean = 2L)`, a
  variable's own as `BMIBL ~ .fmts(mean = 2L, sd = 3L)`, over the catalog's
  defaults (`.fmt_default`, now a list as `fmt_fun` takes it: `1L`,
  `cards::label_round(1, scale = 100)`, `.pvalue`); the line that tags it
  is `.tag(cards::apply_fmt_fun(ard), ...)`.  So for `ard_summary()`,
  `ard_tabulate()`, `ard_tabulate_value()` (and a subject flag),
  `ard_missing()`, `ard_hierarchical()` / `_count()`, `ard_mvsummary()`,
  `ard_tabulate_rows()`, `cardx::ard_tabulate_max()`, and each of them
  inside `ard_stack()` and `ard_strata()`.  What takes no `fmt_fun` keeps
  `.fmt()` after the call: cardx's tests, CIs and models,
  `ard_stack_hierarchical()`, a study's own function, `custom` code,
  `ard_pairwise()`, `ard_stack()`'s own rows (the by counts, the total N),
  an analysis whose `args` gives `fmt_fun` or whose `post` changes the ARD,
  a variable's own format for `ard_hierarchical()` (its `fmt_fun` formats
  a column of its own) or `ard_tabulate_rows()`.
  `stat_fmt` is the same as before, row by row (a test builds both ways).
  A stack's analysis whose formats were all of its own variables
  (`SEX:p=xx.xx%`) no longer stops the program being written.

* **A report's own font** (#176).  A report's row of `page` that says its
  `font` (or size) wins over the company's: its program says it, with the
  setup and alone, and the file is in it.  A test now holds it; nothing
  else changes.

* **The company's font, said once** (#174).  The `page` sheet gains
  `font`, the document's font (`rtf_document(font_table = )`).  The study's
  row (blank `output_id`) of `font` and `font_size_half_points` is the
  company's: `tfl_report_setup_code()` writes it once as
  `options(rtfreporter.font = , rtfreporter.font_size_half_points = )`, and
  a report's program written with `setup = TRUE` leaves them to it (a
  report's own it still says).  Blank: no line, rtfreporter's own
  (Courier, 9 pt).  A program standing alone says them itself; the file
  is the same either way.
* **Figure code in the style of a study's other programs** (#164).  Every
  generated figure script (the designer's templates, the `pp_*` types)
  pipes with `|>` (the dplyr verbs stay); its parts are one line each
  (`# ---- data ----`, `# ---- the figure ----`, `# ---- saving the figure
  ----`) instead of `#####` Step banners; `filter(a) |> filter(b)` is
  `filter(a, b)`; the palette is two short lines.
  `tfl_fig_design_code()` gains `setup` (the palette from the study's
  figure setup, `tfl_colours()`), `save` (`FALSE`: no PNG) and `name`
  (what the figure is called): a report program gets its figure as
  `plot` directly.  A composed figure's panels are made as `fig_<name>`.
  The figures drawn are the same (all 38 templates).
* **A code list of `variable`: the variables' labels** (#172).  Its
  values are the variables' names (the ARD's `variable` column) and its
  labels what they print as: `variable / AGE / Age (years)`.  The variables
  sheet's `label`, when given, wins; it is not an order (the rows follow
  the variables sheet).  A label is a report's, like its code lists.

* **A code list is a report's** (#170).  Every `codelists` row names its
  report: a blank `output_id` is an error, in a table definition and in
  `tfl_ard_code(codelists = )` (no study-wide rows; a sheet made before
  this version gives its rows their report).  A report's ARD program, and
  its fingerprint, take only its code lists of the variables its analyses
  read (`by`, `strata`, `variables`, the names in `args`, `code` and
  `post`): a variable its tables only show is not made a factor.  The
  study's program, when a report has code lists, gives each report's part
  its own `.codelists` and reads the data again with them, so a report's
  factors are not the next report's; without code lists it reads the data
  once, as before.  A data made from another (a population, an analysis
  data, an analysis's own subset) is made factors again as its last step
  (`|> .levels()`): a value the lists do not have counts only where the
  data analysed have it (a table of the safety population has no column
  for the screen failures' arm).
* **Decimals written once: the `digits` sheet** (#168).  Each statistic's
  decimals (`statistic`, `digits`), for every analysis variable (`variable`
  blank) or a variable's exception (`variable` = its name); a report's rows
  replace the defaults.  A template's tokens that say no format take them
  (`{mean} ({sd})` with mean 1, sd 2 is `{mean:.1f} ({sd:.2f})`; `p`, a
  percent, `{p:.1f%}`); a variable with an exception and no rows of its
  own gets the kind's rows with its decimals.  A template's own format
  and a `cells` row's `digits` win.  `tables$value` now says which of the
  ARD's values a table prints for its cells too: `stat` (rounded here) or
  `stat_fmt` (the ARD's own text: the `digits` sheet does not apply).  The
  generated code is `plan_cells()` as before.

* **An analysis data is a report's** (#166).  `analysis_data` has an
  `output_id` column, first: the key is the report and the name, so the
  same `data_id` may mean something else in another report (a Phase I
  table's adsl_saf and a Phase II table's).  A row's `from` and `subjects`
  name its own report's rows above it, an analysis's `data` and
  `denominator` its own report's; the checks say so, and a blank
  `output_id` is an error (a sheet made before this version is the study's:
  give its rows their report).  A report's program is as it was; the
  study's is one part a report after the datasets and populations, each
  part making its own analysis data before its analyses.  The fingerprint,
  ARS and `tfl_ard_as_custom()` read the report's rows.

* **A report's own tokens, and one header for every report** (#148).
  - A report's header, footer, titles and footnotes may say
    `{OUTPUT_ID}`, `{OUTPUT_LABEL}`, `{OUTPUT_TITLE}`,
    `{OUTPUT_POPULATION}`, `{OUTPUT_SECTION}` and `{STUDY_ID}`.
    `{OUTPUT_LABEL}` is made from the ID ("T-14-1-1" -> "Table 14.1.1":
    the kind from its first letter T / L / F, else the report's type; the
    number from its first digit, separators made dots); tokens rows
    `OUTPUT_KIND_TABLE` / `_LISTING` / `_FIGURE` give other words.  The
    others are blank unless the tokens sheet gives them (a report list
    writes them from a TOC).  A row of the tokens sheet always wins.
  - They go to `rtf_document(tokens = )` only when the report says one: a
    report that says none is written as before.
  - A band line left with nothing but empty tokens (and brackets:
    `<{OUTPUT_POPULATION}>`) is not printed.
  - `tfl_report_setup_code()` writes what a study's reports share, once:
    its tokens (the tokens sheet's default rows) as
    `options(rtfreporter.tokens = )`, its header and footer as
    `study_header` / `study_footer` (with `drop_empty_rows = TRUE`, so a
    line a report's tokens leave empty is not printed).
    `tfl_report_code(setup = TRUE)` then uses those by name and gives the
    document only the report's own tokens; a report with its own header
    writes it as before.  The file is the same as the program alone.
    Needs rtfreporter 0.8.2.9025.
  - `tfl_report_tokens()`: a report's tokens as its program gives them
    (for a preview of its page).
  - `tfl_read_toc()`: a `label` field in the map, and the attributes
    `labels`, `first_titles` and `populations` for a report list's tokens.
  - A spec of study defaults only, scoped to a report, is that report's
    (its `{output_id}` file name and its own tokens were blank).

* **The arguments' help says when to use them** (#160).  For the
  functions used most -- the common `id`, `denominator`, `include`,
  `strata`; `ard_stack()`'s `.overall`, `.missing`, `.attributes`,
  `.total_n`; `ard_stack_hierarchical()`'s `variables`, `over_variables`,
  `overall`, `include`, `attributes`, `total_n`, `by_stats`;
  `ard_tabulate_value()`'s and `ard_categorical_ci()`'s `value` -- the hint
  of `tfl_ard_args()` gains a sentence of use ("Use it for a Total
  column"), and `attributes` says where the label goes.  The
  statistic N is labelled "Number of non-missing values" (it read "n (...)",
  next to the categorical n).

* **The design concept, written down** (#154).  The README has a
  "Concept" section: readable code from a spec, the typical cases kept
  simple, R code inside the spec, shared setup code and per-report
  values, spec -> code one way.  Docs only.

* **A hex logo, shared with rtfreporter and tflplanner** (#158), made with
  the site's favicons by `data-raw/logo.R`.

* Added a root `CITATION.cff` so GitHub's "Cite this repository" button
  works (#156).

* **`tfl_read_toc()`: each report's datasets** (#149).  A new field of the
  map, `datasets`: each report's datasets are `attr(, "datasets")` (named
  by output id), "ADSL, ADAE", "ADSL / ADAE" or one a line as
  `"ADSL | ADAE"`.  Not part of the spec, as the sections.

* **The ARD program as a person writes it** (#150).  A data is made in one
  statement -- its condition, derive, code lists, the columns kept
  (`subset(select = )`) and one row per (`dplyr::distinct()`) as a pipe --
  instead of a copy changed line by line; one condition after the
  subjects' is not put in brackets; an analysis data that is an analysis
  set and nothing else is made under its own name
  (`adsl_saf <- adsl |> subset(SAFFL == "Y") |> transform(TRTA = TRT01A)`)
  when the program uses the set for nothing else.  The data are the same.

* **`tfl_read_toc()`: each report's section** (#145).  The TOC's heading
  rows ("14.1 Demographics") were passed over; each report's section is
  now `attr(, "sections")` (named by output id): the last heading row
  above it, or the TOC's `section` column when the map names one (a new
  field of the map).  It is not part of the spec (the report sheet is the
  same): a report list may keep it.

* **`tfl_ard_as_custom()`: an analysis as R** (#143).  The call an analysis
  row stands for, written as the code of a `custom` analysis (`data` and
  `population` bound), with its method's default formats written out: the
  custom analysis gives the same ARD, and its code can then be changed for
  what the columns cannot say.  The definition keeps the code; the program
  is never edited.

* **Analysis data written as R: `analysis_data$code`** (#141).  When the
  columns cannot say how a data is made, `code` is R whose value is the
  data, the program's objects in reach (the datasets, `pop_<population>`,
  the analysis data above).  `from` stays; the other columns that make a
  data are left blank with it (checked).  The program has it as
  `<data_id> <- local({ ... })`, in the sheet's order.  A `code` column
  blank in every row leaves the fingerprints as they were; ARS notes it as
  what it has no place for.

* **The hints of `by` and `strata` say what differs** (#139).  In cards'
  summaries `by` analyses every combination of its levels, one the data do
  not have too (0), and `strata` only the combinations the data have; in a
  test `by` is the groups compared.  `strata` of `ard_categorical_ci()`
  (the stratified Wilson interval) and of `ard_stats_mantelhaen_test()`
  (the CMH strata) has a hint of its own.  `tfl_ard_args()`.

* **Analysis data: a report's own subjects, the columns kept; code lists
  per report** (#137).  `analysis_data$subjects` keeps the subjects of an
  analysis data above (the safety set in phase 1 ...): the numerator kept
  to the denominator's subjects, instead of `population_id`; `add` then
  takes its columns from that data.  `analysis_data$keep` keeps those
  columns (the subject key always): with `derive`, transmute().  The code
  lists make factors of derived columns too (analysis data, populations),
  and a report's program (`tfl_ard_code(output_id = )` one report) uses
  its own code list rows as well, which replace the study's of the same
  variable and value -- as its tables do.  Blank columns and no report
  rows: the same code and fingerprints.

* **Analysis data: named data the analyses read** (#135).  A sheet
  `analysis_data` (the study's): each row makes one data, its `data_id` the
  object's name in the ARD program -- `from` a dataset or an analysis data
  above it, kept to a population's subjects (`population_id`) and the
  records `where` keeps, with columns of the population's data added by
  the subject key (`add`: `TRT01A | AGEGR1`), columns derived (`derive`)
  and one row per set of values (`distinct`: `USUBJID | APHASE`, a
  denominator per subject and phase).  An analysis names one in its new
  column `data` instead of `dataset` / `population_id`; its `where` still
  applies, on top; `denominator` may name one too.  The program makes
  the named data a report reads once, in the sheet's order.  `tfl_ars()`
  gives such an analysis its population, first dataset and the
  conditions together; `add` / `derive` / `distinct` are unmapped.  A
  spec without the sheet reads, writes and runs as before: the same code
  and the same fingerprints.

* **A keyword's function name is the keyword's analysis** (#133).
  `method` written as the function a keyword calls -- `cards::ard_summary`,
  `cards::ard_stack_hierarchical`, `cardx::ard_continuous_ci` ... -- gets
  the keyword's `statistic =` from `statistics`, its defaults
  (`denominator`, `id`) and its formats, so the code is the same either
  way; `tfl_ars()` gives it the keyword's method (`Mth_categorical` ...).
  A function the catalog does not know stays as written.
* **A blank `program` is the file name of the program that runs** (#131).
  The report sheet's `program` is said to rtfreporter only when it is
  written; blank, `tfl_report()` / `tfl_report_code()` pass
  `rtf_document(program_fallback = <program_dir>/<output_id>)` instead, so
  `{PROGRAM}` names the program that runs (sourced, run by Rscript ...) and
  the report's ID only when no file name is found.  Needs rtfreporter
  0.8.2.9024.
* **A `tokens` sheet** (#129): tokens of one's own for a report's
  header, footer, titles and footnotes -- `name` `STUDY`, `value`
  `ABC-123`, written `{STUDY}` -- like the `titles` sheet: a blank
  `output_id` is the study's default, a report's row of the same name
  replaces it, `(none)` takes it out.  `tfl_report()` and
  `tfl_report_code()` hand them to `rtf_document(tokens = )`.  Names follow
  rtfreporter's rule (upper case, not one of its own tokens).  Needs
  rtfreporter 0.8.2.9023.  With rtfreporter 0.8.2.9022 a program name with
  no extension -- the default `{output_id}` -- shows as `{output_id}.R` in
  `{PROGRAM}`.
* The workbook's tokens help names **`{PROGRAM_FULL}`**, the program's
  absolute path (rtfreporter #560; #127).  rtfreporter fills it, so it
  needs rtfreporter 0.8.2.9021: the floor is raised.
* **`tfl_ard_function_info(files)`** (#125): what the ARD functions of one's own in
  some R files are, read without running them -- one row per function an
  analysis could name (`name <- function(...)`, or `name <-
  cards::as_cards_fn(function(...), stat_names = )` as the templates write
  it) with its file, the title and description of the roxygen block above
  it, the statistics it declares and its arguments with their `@param`
  hints.  `test-*.R` files are skipped.  One way of finding them, for a
  screen's list, an analysis form's arguments and the ARS (which now uses
  it).
* The templates of `tfl_ard_function_template()` start with a roxygen block
  (a title, a description, `@param` for each argument), so a screen shows
  a function made from one with its title and argument hints.

* **ARS: a report with no analyses is listed, not dropped** (#123): a report
  of user code that does not read the ARD, a listing, or a table not defined
  yet has no analyses, so it is not an ARS output; `tfl_ars_unmapped()` now
  says so (item `output`), with the reason by its type.  A user-code report
  that reads the ARD is an output with its analyses, as a table is.

* **Every function of cards / cardx has a decided use** (#121): besides the
  ard_*() of the catalog, `inst/ard/utilities.csv` says what each other
  exported function of cards and cardx is to tflspec -- a post step, written
  by the generated code, a screen's tool, internal, for writing one's own ARD
  function, a selector, a choice of an argument, not used (`bind_ard()`:
  tflspec #118) -- and a test names any function a new cards / cardx exports
  without a row (the weekly CRAN-latest CI).

* **An ARS method of a function says what it runs** (#119): an analysis
  whose method is a function (`cards::ard_summary`, a study's own
  `ard_riskdiff`) gets, as its method's `codeTemplate`, the call the ARD
  program makes (`ard_riskdiff(data, by = TRT01A, variables = AEFL, ...)`)
  instead of `ard_riskdiff(...)`; an own function's file is named above it,
  and the statistics it declares (`cards::as_cards_fn(stat_names = )`) are
  its operations when the row names none.  `tfl_ars(dir = )` is the study
  folder the `source` files are read from (read, not run).

* **A parent row's arguments say the columns they are written in** (#113):
  `tfl_ard_args()` gave `ard_stack(.by =)`, `ard_strata(.by =, .strata =)`
  and `ard_pairwise(variable =)` the column `args`, though the generated code
  writes them from the analysis row's `by`, `strata` and `variables`.  They
  now say so, so a screen that builds a parent row from the catalog writes
  them where the code reads them.

* **An ARD function of one's own, from a template** (#115):
  `tfl_ard_function_template(name, type, file, test = TRUE)` writes an
  `ard_*()` an analysis row can name as its method, in the three shapes
  cards and cardx write theirs -- `"summary"` (statistics of one's own,
  `ard_summary(statistic = )`), `"test"` (a test across groups,
  `tidy_as_ard()`, its errors kept in the ARD) and `"free"`
  (`ard_strata()` + `ard_identity()`) -- and, with `test = TRUE`, a
  testthat file next to it.  Each says which statistics it gives
  (`cards::as_cards_fn(stat_names = )`).
* `tfl_check_ard_function()` checks the statistics a function says it gives
  (`attr(fun, "stat_names")`, as `cards::as_cards_fn()` sets it) without
  `stat_names =`; one that says nothing gets a note.
* `tfl_check_ard()` no longer calls a `groupN` without `groupN_level` an
  error: a test across groups has that shape (cardx's own `ard_stats_*()`
  give it), so an ARD of tests taken in, or an own function wrapping a test,
  was refused.  cards' note that the ARD has no `method` rows is left out
  (no report reads them; an ARD written and read back has none).

* **`tfl_ard_conditions()`**: what went wrong while an ARD was made, as a
  table.  cards does not stop when one analysis fails (a test given three
  groups, a statistic whose function stops or warns): it leaves the message
  in the ARD's `error` / `warning` column and carries on, so a study ARD can
  look complete with an analysis missing.  One row per analysis, variable,
  groups and message, with the statistics it is about -- what
  `cards::print_ard_conditions()` prints, with `output_id` / `analysis_id`.
  For the ARD tab's list of problems (tflplanner).

* **The report spec's `type` names `user`** (#108): a report whose own code
  leaves `content` (a data frame, rtftable pages, rtfplot figures or a
  list of them), dressed by the report spec as a table is (`rtf_tables()`
  takes all of these).  Only the column help and the AI manual change.
* **The study's code lists, before the ARD** (#105): `tfl_ard_code()`,
  `tfl_build_ard()` and `tfl_ard_spec_hash()` take `codelists` (a table
  definition's `codelists` sheet, its study rows).  Each listed character
  column of the data read becomes a factor in the code list's order, so the
  ARD keeps the order and **counts a level no record has** (`n = 0`): a
  table made from it shows that level's row with 0.  Without `codelists`, as
  before.  To leave those rows out of one table, the `variables` sheet has a
  new column **`empty_levels`** (`show`, the default, or `hide`): `hide`
  writes `plan_levels(.drop_empty = )` (rtfreporter >= 0.8.2.9015, now
  required), which drops a value counted 0 in every column.  The ARD keeps
  every level either way; `tfl_as_table_spec()` gives the column back.

* **The ARD keywords call cards' current names** (#104): `continuous`,
  `categorical` and `dichotomous` (and the `subjects` flag) write
  `cards::ard_summary()`, `ard_tabulate()` and `ard_tabulate_value()`
  instead of the names cards 0.7.0 deprecated.  **The ARD's `context`
  changes**: `continuous` -> `summary`, `categorical` -> `tabulate`,
  `dichotomous` -> `tabulate_value` (the values are the same; rtfreporter
  reads either as the same kind of row, and the tables do not change --
  SAMPLE-01's reports are byte for byte the same).  A Table Spec whose
  `cells$context` names the old context of an ARD made before should be
  made again with the study ARD.

* **The ARD catalog, refined** (#103): `tfl_ard_functions()` returns
  `offered` (`FALSE` for the old names and what is not offered by design);
  the arguments a screen cannot write as a column or a level are `code` or
  `text` (`abnormal`, `include` of `ard_pairwise()`, `primary_covariate`,
  `stats_to_remove`); every argument of the offered functions has a hint.

* **Try an ARD function of one's own** (#102): `tfl_check_ard_function(fun,
  data, by = , variables = , stat_names = )` calls it as an analysis row
  would and reports what is wrong -- an error or warning in the call, a
  result that is not a cards ARD, the statistics it should give.

* **An ARD made elsewhere** (#101): the `report` sheet's `ard_source` says
  where a report's ARD comes from -- blank, its own ARD definition;
  `import:<file>`, an ARD taken into the study's `input/ard/`.
  `tfl_check_ard(ard, spec)` checks one: its shape as a cards ARD, and
  whether it has what the report's table reads (the column and row keys,
  the variables, the statistics the templates name).

* **The study ARD as JSON, YAML or XPT** (#99): `tfl_write_ard()` writes a
  copy -- JSON / YAML as one record per statistic with each variable's levels
  in their order and the column types, which `tfl_read_ard()` reads back
  (all but the formatting functions); or cards' nested shape; or XPT
  (version 8, or 5 with 8-character names).  Each format says in a warning
  what it does not keep.  The rds stays the record.

* **cards >= 0.8.0 and cardx >= 0.3.1** (#98), stated in DESCRIPTION and
  tested at both ends: CI has a row with exactly those versions, and runs
  every week on what CRAN has now, where a new `ard_*()` the catalog does not
  describe fails a test.  `?tfl_ard_functions` names what is not offered by
  design: the old names, the survey-design functions (their input is not a
  data frame) and `ard_formals()`.

* **Steps on the ARD after the call: the `post` column** (#97).  Calls
  with the ARD left out, `|` between them, piped after the analysis:
  `cards::add_calculated_row(expr = sd / sqrt(N), stat_name = "se")` (a
  statistic computed from others), `cards::filter_ard_hierarchical(p > 0.05)`,
  `cards::sort_ard_hierarchical()`, `cards::diff_ard_hierarchical()`, or a
  function of one's own.  `tfl_ars()` lists it as unmapped.  Blank: as
  before.

* **Analyses run inside another: the `parent` column** (#96).  A row of
  the `analyses` sheet whose `parent` names an analysis with method
  `cards::ard_stack`, `cards::ard_strata` or `cards::ard_pairwise` is run
  inside it, on its data, analysis set and condition (and in a stack its
  `by`).  The program writes the one call a person would --
  `cards::ard_stack(pop_saf, .by = ARM, cards::ard_continuous(variables =
  c(AGE, BMIBL)), cards::ard_categorical(variables = SEX))` -- and tags each
  variable's rows with its own `analysis_id` (the stack's own rows, the by
  counts and the total N, with the parent's).  `tfl_ars()` writes each row
  inside as an analysis of its own.  What a row inside may not say (the
  parent's data, a second analysis of one variable in a stack, `custom`) is
  refused by name.  A blank `parent` is as before.

* **The ARD functions as a catalog** (#93): `tfl_ard_functions()` lists
  every installed cards / cardx `ard_*()` by category, with a heading, a
  sentence and its shape (columns, a formula, a fitted model, other
  analyses); `tfl_ard_args("cardx::ard_categorical_ci")` gives one
  function's arguments read from its own `formals()` -- default, required,
  what fills each (columns, levels, a number, a choice ...), a hint, and the
  analysis row's column it goes in.  Rows in `inst/ard/functions.csv` and
  `args.csv`; a company adds its own with `options(tflspec.ard_functions =,
  tflspec.ard_args =)`.  A test compares the catalog with the installed
  cards / cardx, so a version that adds or renames a function is noticed.
  The groundwork for choosing any ARD function in tflplanner; analysis rows
  and the generated code are unchanged.

* **The study ARD stays a cards ARD** (#92): the ARD program's `.tag()`
  keeps a result's class `card` (the ids go in front), so the study ARD is
  one and cards' own tools take it (`as_nested_list()`, `compare_ard()`).
  The values and the rows are unchanged; a result that is not a card
  (custom code) is tagged as before.

* **A report's line that says `(none)`** (#91) in the `header`, `footer`,
  `titles` or `footnotes` sheet takes the study's line of that number out,
  instead of replacing it (a line with no text is still a blank line).  The
  run-information line 99 a report does without, for example.
* **tflspec is now licensed under the Apache License 2.0** (#90), like
  rtfreporter and tflplanner, instead of MIT.  `LICENSE` (the MIT template)
  is replaced by `LICENSE.md` (the licence text); the README has the licence
  badge and section.  The CDISC ARS JSON Schema in `inst/ars/` keeps its own
  MIT licence.

* **A manual legend of a KM or waterfall figure was drawn empty** (#88).  The
  style catalog gave the legend panel's columns and height
  (`legend_ncol`, `legend_height`) to swimmer figures only, so the generated
  code of any other figure with `legend_type = "manual"` called
  `legend_panel(ncol = NA)`: every item was dropped, with ggplot's
  "Removed N rows containing missing values" warnings (and "Killing locked
  device" inside the plot).  Both now have a value for every figure type --
  swimmer's, 3 columns and 0.2 of the plot height -- so the legend is drawn
  and the generated script runs without warnings.  A manual legend beside
  the plot (`right` / `left`) now stacks its items in one column, as one
  inside the plot does, and `legend_panel()` takes `n_rows` so the items
  keep their size at the top of a panel as tall as the plot instead of
  being stretched over it.  Mapped legends are unchanged; a swimmer
  figure's manual legend changes only beside the plot.

* **Authors, copyright and citation** (#86).  `Authors@R` names the author
  in full as the copyright holder, and CDISC as the copyright holder of the
  ARS JSON Schema in `inst/ars/` (MIT, included unchanged; listed in
  `inst/COPYRIGHTS`).  `citation("tflspec")` has a CITATION file, and the
  README says how to cite tflspec (and cards / cardx) and acknowledges the
  packages and standards it builds on.

* **A follow-up `R CMD check --as-cran` on R 4.6.1 with every Suggests**
  (#84).  The check is clean but for the CRAN-incoming NOTE.  `Language:
  en-GB` is declared (the text is British) and `inst/WORDLIST` lists the
  technical words, so `spelling::spell_check_package()` finds nothing; the one
  American `behavior` in the `scale_mode` help is `behaviour`.  The
  "cell styles are a sheet" test no longer prints sixteen
  `style_header()` warnings that are rtfreporter's, not its subject.

* The `style` sheet's `align` help no longer warns that any `border_*` or
  look column left-aligns every column: from rtfreporter 0.8.2.9008
  (rtfreporter #522) a blank `align` keeps each column's default.  Needs
  rtfreporter >= 0.8.2.9008.

* **The table definition says what the plan learned in rtfreporter
  0.8.2.9006** (#81; rtfreporter #517 / #519).  New columns, blank = as before:
  `layout$pages_page_by` (BY pages with a row budget inside,
  `plan_paginate_rows(page_by = )`); `style$header_align`, `header_bold`,
  `header_italic`, `align`, `bold`, `italic`, `underline` (the table's
  default look, one `rtf_table_style()` with the `border_*` columns),
  `table_width_twips`, `table_width_pct`, `table_width_pct_of_writable`,
  and `col_header_align` (`plan_col_header()`); `cell_styles$underline` and
  `indent_twips`.  `tfl_table_plan()`, `tfl_table_code()` and
  `tfl_as_table_spec()` carry them both ways, byte for byte.  What a sheet
  cannot hold is listed by `tfl_as_table_spec()` (`not_converted`) instead
  of being dropped: `plan_columns(cell_format = )`, `column_widths_twips`,
  `plan_col_header(header_sep = )`.  Needs rtfreporter >= 0.8.2.9006.

* **`tfl_read_toc()`: a company's TOC as report specs** (#78).  A study's
  list of outputs in the company's own workbook or `.csv` is read through a
  map of its columns (`output_id`, `type`, `title` -- one column or several
  --, `population`, `footnote`, `program`, `file`, `note`) into the
  `report`, `titles` and `footnotes` sheets.  Kinds are read loosely, or
  guessed from the id or the first title (`attr(, "guessed")`); a cell's
  line breaks or `" | "` make lines; section headings are skipped
  (`attr(, "skipped")`).  A column the map names and the TOC has not, a row
  with no id but content, and an id given twice stop with what to do.  The
  spec is unchanged.

* `tfl_ard_code()` writes the code of an analysis with no dataset and no
  population again (#76): #74 left its data unnamed and stopped with
  "argument is of length zero", before tflplanner could say what the
  definition lacks.  Its data is `NULL`, as before #74.

* **The ARD program reads like code written by hand** (#74).  Each
  analysis's data -- the dataset, restricted to the population's subjects
  and to the analysis's `where` -- is made once, under a name
  (`adae_saf <- subset(adae, USUBJID %in% pop_saf$USUBJID & (TRTEMFL == "Y"))`),
  and each analysis is one call on it (`ard <- cards::ard_stack_hierarchical(adae_saf,
  ..., denominator = pop_saf, ...)`) instead of a `local({ data <- ...;
  population <- ... })` block.  A row whose own R (`args`, `custom` code)
  names `data` or `population`, and a subject flag, still bind the two
  names locally.  The ARD spec is unchanged and the ARD made is the same:
  the 64 exact-test cases are identical, and the sample study's ARD and
  RTFs (tflplanner's SAMPLE-01) are unchanged.

* **A code list in the table spec** (#72): an optional `codelists` sheet
  (`variable`, `value`, `label`, `order`) gives a value's printed text and
  its place.  It becomes `plan_labels(SEX = c(SEX = "Sex", F = "Female"))`
  (the variable's label from `variables$label` under its own name) and
  `plan_levels(SEX = c("M", "F"))`.  A report's own rows replace the
  defaults of the same variable and value.  **Precedence:** a variable's
  `levels` on the `variables` sheet, when given, is its order; the code
  list's order applies otherwise; its labels apply either way.  Needs
  rtfreporter 0.8.2.9004 (rtfreporter#514) for the text of an analysis
  variable's levels.

* **`.rda` / `.RData` data files** (one dataset a file) are read: the
  written programs (`tfl_ard_code()`, `tfl_read_data_code()`) load them,
  and `tfl_read_adam()` takes them from a folder.

* **`tfl_listing(data, spec)`**: the data comes first, as in
  `tfl_table_plan(data, spec)`, so a listing can be piped from its data
  (#68).  No alias: a call in the old order (`tfl_listing(spec, data)`)
  stops with "the order of the arguments changed: tfl_listing(data, spec)".

* **Docs** (#64, brush-up).
  - A pkgdown site (https://ichirio.github.io/tflspec/), its reference in
    the AI manual's chapters; `URL` and `BugReports` in DESCRIPTION.
  - The README follows the one workflow: ARD spec, table spec, report,
    listing, figure design, ARS (the figure sections folded into one).
  - The AI manual says how to write an analysis that takes a formula
    (survival, models), what `statistics` means for CIs, tests and models,
    the keywords' own arguments, and what `tables$value` is (a column, not
    a template); its two examples are run by the tests as written.

* **Round trips checked by what a reader gets** (#64, brush-up).
  - `tfl_as_table_spec()` compares the RTF, byte by byte, not the page
    objects (two page objects may hold the same output differently); its
    `check` argument is `compare` now.  A test takes 35 plans through the
    spec, a workbook and the written code back to the same RTF
    (`tests/testthat/fixtures/table-cases.R`, shared with the coverage
    script).
  - Five figure templates (KM with the number at risk, waterfall, forest,
    mean by visit, box plots) are compared with the same figures written by
    hand in ggplot2: every layer's data, the labels, the axes.
  - `tfl_as_listing_spec()`: a `listing_spec()` or a `plan_listing()` plan
    as a listing spec, saying what the sheets cannot carry; from a plan it
    compares the RTF.  New columns `listings$blank_row`, `listings$wrap`
    (the name of a function), `listing_cols$sep` (quoted to keep spaces) and
    `listing_cols$align`.
  - The report sheet gains `watermark` and, for a figure,
    `figure_width_in` / `figure_height_in`.
  - The AI manual says there is no ARD-to-ARD-spec function: an existing
    analysis comes in through ARS (`tfl_ars_to_specs()`).

* **Functions say what they want and what they got** (#64, brush-up).
  - A function that takes a spec (`tfl_ard_code()`, `tfl_build_ard()`,
    `tfl_table_code()`, `tfl_table_plan()`, `tfl_report()`,
    `tfl_report_code()`, `tfl_report_path()`, `tfl_listing_code()`,
    `tfl_listing()`) refuses anything else with "`spec` must be ...; got a
    data.frame (2 x 1)" -- a list or a data frame gave R's own warnings.
  - `tfl_ard_for()` refuses an output the ARD has not, naming those it has
    (it gave 0 rows).
  - A file that is not there is said one way: `<function>(): no file
    '<path>'`.
  - Argument order: `tfl_report(spec, output_id, content)` (output_id
    second, as in every other function; a content given second is told
    to be named) and `tfl_fig_list_template(adam, path)` (the material
    first, as the other templates).  No alias.
  - The AI manual says how the functions are named (12.1).

* **Column names and their help** (#64, brush-up before CRAN).
  - Renamed, as the names said no unit: the table spec's `columns$width`
    is `rel_width` (a relative width), and the report's
    `table_font_size`, `title_font_size`, `footnote_font_size` are
    `*_font_size_half_points`.  A workbook with a former name is told the
    new one; no alias is kept.  The listing's `width` stays (it is
    `listing_col(width = )`, characters a line).
  - The rule the names follow is written down (README, AI manual 4.1):
    table / listing / report columns are rtfreporter's argument names, ARD
    columns cards' (`statistics` and `formats` excepted).
  - One argument, one place: a column (`by`, `variables`, `strata`,
    `denominator`, or `statistics` where it is the call's `statistic`) and
    `args` may not both give it -- the column was dropped unseen.
  - `tfl_spec_columns()` describes every column of every sheet (ARD,
    table, report, listing), one row a column, in English: the form of
    the value, its unit and what a blank means.  Header-cell comments
    follow it.  A test keeps it complete.
  - `listings` / `listing_cols` are no longer listed as reserved sheets.
  - `tfl_ard_spec_hash()` (and the fingerprints `tfl_ard_code()` saves)
    take the content of the study's own function files (key `source`;
    a missing one counts) and leave out columns blank in every row, so
    a column added later changes no fingerprint.  New `dir` argument.
  - The bundled example workbooks are written again with the new names.

* **The ARD spec says more, exactly** (#64, brush-up before CRAN).
  - `args` is read as the arguments of a call: a keyword's own argument
    given after another (`over_variables = TRUE, denominator = population`)
    is no longer missed, which named it twice in the generated call; `args`
    that are not R are refused by `tfl_ard_spec()`.
  - A study's own analysis function is a `method`: its plain name, loaded by
    the new study key `source` (R files the ARD program sources).  For what
    cards / cardx have no function for -- risk differences (Newcombe,
    Miettinen-Nurminen), competing risks, multiple imputation.
  - New `analyses` columns `strata` (the analysis repeated within them) and
    `denominator` (`population`, `row` / `column` / `cell`, a population or
    a dataset).  A definition without them reads as before.  `tfl_ars()`
    writes strata as groupings and says a denominator ARS has no place for.
  - A method that gives several ARDs (`cards::ard_pairwise()`) keeps which
    is which; a fitted model given first in `args` takes no data.
  - Checked against cards / cardx written by hand: 64 cases, every value
    the same (`tests/testthat/test-ard-exact.R`).

* **The table spec says more of a plan** (#64).
  - A `cell_styles` sheet: one `plan_cell_style()` a row -- the cells
    chosen by `cols`, `header` and `where` (an R condition over the
    table's columns), and `bold`, `italic`, `align`, `color`,
    `background`.  Two `where` rows may not set the same look (a plan
    keeps one conditional rule per look).
  - `layout` keys `pages_break_before`, `colpages_cut_by`, `colpages_fit`
    and `colpages_allow_span_break`.
  - `tfl_as_table_spec()` writes all of these (they were "stays in code"),
    and a `plan_style()` value that is not one value is said instead of
    written as two rows.  A plan with cell styles and no `plan_style()`
    no longer reads its styles as plan_style() arguments.
  - A plan's titles and footnotes go to the `titles` / `footnotes` sheets
    (they were dropped without a word).

* **ARS: an added subject count keeps an id of its own** (#61).  The
  subject count `tfl_ars()` adds for a percentage's denominator is
  `An_<output>_BIGN_<by>` (`_ALL` without a grouping) and never takes an
  id the output already has (an output with a `BIGN` row and no `by`
  elsewhere wrote the same id twice).  `tfl_check_ars()` names a blank
  purpose once, not again from the schema.

* **CDISC's Excel template for ARS** (#59, stage 4 of the ARS export).
  `tfl_write_ars_xlsx()` writes a reporting event as the Excel template of
  CDISC's ARS repository: its 25 sheets in their order with their columns
  (one row per list item, group, operation, display sub-section; a
  WhereClause as `level` / `order` rows; document references and nested
  categories kept).  CDISC's converter (`excel2ars.py`) reads it back to
  the same JSON: checked on tflspec's own events and on CDISC's Common
  Safety Displays.  Written with writexl, so any reader opens it.
  - The AI manual gains §5.1: writing ARS, running it with siera, reading
    anyone's ARS back into specs.

* **Reading ARS back into specs** (#57, stage 3 of the ARS export).
  `tfl_read_ars_json()` reads an ARS JSON -- tflspec's or anyone's -- as
  the model, refusing one whose references name nothing.
  `tfl_ars_to_specs()` turns it into specs: the ARD spec (analysis sets as
  populations, WhereClauses as R, analyses with their method, `by`,
  variables, statistics, purpose and reason) and the report spec (outputs
  in the list's order, file, titles, footnotes, header, footer, global
  sections included); with `table = TRUE` a table spec with the
  groupings' listed groups as `levels`.
  - tfl_ars()'s own layout comes back as written (several variables in a
    row, a hierarchy in one row, the added subject counts left out): the
    ARD of the spec read back is the ARD of the spec.
  - Anyone's analysis is one row, its method read from what its
    operations compute; a count of subjects in record-level data is
    `subjects` (by the arm) or `hierarchical` (SOC / PT).  CDISC's Common
    Safety Displays reads with only its ANOVA left out, and said.
  - ARS does not say where the ADaM is: `datasets$path` is blank.

* **ARS siera can run, and the round trip** (#55, stage 2 of the ARS
  export).  `tfl_ars(profile = "siera")` writes the reporting event so
  siera's `readARS()` runs it, still valid CDISC ARS: each method carries
  an R code template (written for tflspec against siera's contract), a
  proportion with its CI is an analysis of the variable, each output's
  subject count comes first, ids keep only letters, digits and `_`, the
  list of outputs is there.  A hierarchy keeps its nesting: a level below
  the first counts only the pairs of it and the level above that are in
  the data.  What siera has no template for is left out and listed by
  `tfl_ars_unmapped()`; `tfl_check_ars()` checks what siera needs beyond
  the model (three analyses an output, one analysis set of one condition,
  a code template for each method).
  - `tfl_ars_ard(ars, adam)` has siera make the ARD: the ADaM written as
    CSV (siera's contract), siera's programmes run each in an environment
    of its own, `OutputId` given back as the spec's output id.
  - The tests run the round trip on the CDISC pilot data: every number
    siera makes from the ARS is tflspec's from the spec (demographics,
    TEAEs by SOC and PT, proportions with Wilson CIs).
  - siera (>= 0.5.6) is suggested.

* **The specs as a CDISC ARS reporting event** (#53, stage 1 of the ARS
  export).  `tfl_ars()` writes what the ARD spec analyses -- and, when
  given, the table / report specs' levels, titles, footnotes, header,
  footer and file -- as the CDISC Analysis Results Standard v1.0 model;
  `tfl_write_ars_json()` writes it as the ARS JSON (the exchange form; the
  same specs always give the same file); `tfl_check_ars()` checks required
  fields and every reference, and with jsonvalidate the JSON against
  CDISC's JSON Schema for ARS v1.0 (shipped in `inst/ars/`, MIT).  The
  specs stay the source.
  - The layout is CDISC's own example's (Common Safety Displays): a
    categorical variable is a count of the subject key grouped by the
    variable, its percentage pointing at the output's subject count by
    group (added, and said, when the output has none); a hierarchy is one
    analysis per depth; a population is an analysis set, `where` a data
    subset (`&`, `|`, `!`, `%in%` become WhereClauses).
  - What ARS cannot say (a derivation, display formats, `args`, the
    table's look, a condition with no WhereClause) is listed by
    `tfl_ars_unmapped()` with the reason, never dropped silently.
  - The ARD spec's `analyses` gain two optional columns, `purpose` and
    `reason` (the CDISC terms).  A purpose is the SAP's decision, so it is
    never guessed: blank, `tfl_check_ars()` names the analysis.
  - jsonlite joins Imports; jsonvalidate is suggested.

* **Each spec is written with only the sheets it needs** (#51).
  `tfl_write_table_spec()` writes `study` (its key: `rounding`), the table
  sheets and `about`; the new `tfl_write_report_spec()` writes `study`
  (`output_path`, `program_dir`), the report sheets and `about`.  A sheet
  of the other half is written only when the spec has rows in it, so
  nothing is dropped.  `tfl_write_ard_spec()` writes its four sheets; the
  catalogs `_methods` / `_statistics` only with `catalogs = TRUE`.
  `tfl_write_listing_spec()` is unchanged in its sheets.
  - No `_README` sheet: what each column means is a comment on its header
    cell, from one table, `tfl_spec_columns()` (also what a GUI shows as
    column help).
  - `tfl_write_specs()` writes several specs into one workbook (one `study`
    sheet with every kind's keys).  The table / report reader now passes
    over an ARD or listing spec's sheets and the ARD spec's `study` keys,
    so each reader takes its own sheets from such a workbook.
  - The bundled workbooks (`inst/extdata/ard-spec/`) are in the new shape:
    `DM`, `AE`, `ORR`, `LB`, `PK`, `study` are table specs, `report` is a
    report spec.
  - A line break in a cell reads back as `"\n"`: openxlsx on Windows
    writes it as `"\r\n"`, and the readers now take that back to `"\n"`,
    so writing a spec again does not add a `"\r"` each time.

* **Each figure template says its category and the data it reads**:
  `tfl_fig_templates()` gains `category` (the clinical category of
  `tfl_fig_catalog()`: Efficacy: time to event, Efficacy: tumour response,
  Longitudinal, Safety, PK / PD ...) and `data` (the datasets it reads, as
  the catalog writes them: `ADTR + ADRS`, `ADLB / ADVS + ADSL`), from the
  catalog row of the template's type and style -- one axis for both.  For a
  GUI's headings, and to grey the templates a study's data cannot draw
  (tflplanner GUI review iter06).  Added columns only.

* **A figure's time can stay in days**: the data step `time_unit` takes
  `unit: days` (no conversion) besides weeks, months and years --
  ADTTE's AVAL is usually in days.  It was offered by tflplanner but
  refused by the check (tflplanner GUI review iter02).

* **Each ARD method has a label**: `tfl_ard_methods()` gains a `label`
  column -- the name a person reads ("Summary statistics", "Counts and
  percents", "Nested counts (e.g. SOC / PT)", "Proportion with CI" ...) --
  beside its one-line `note`, for a GUI's choices and headings (tflplanner).
  A catalog without the column (a company's own, written before) reads
  its method names as labels.

* Development reopens at 0.0.24.9000, after the 0.0.24 release.

# tflspec 0.0.24

* **The table engine is rtfreporter's, adopted.**  The move of the ARD
  functions and the plan to rtfreporter (plan E) was adopted in
  rtfreporter's pre-CRAN API review, and rtfreporter 0.8.2 is its first
  release; tflspec 0.0.24 needs rtfreporter 0.8.2 (a suggestion: only the
  table half uses it).  The rest of this entry is what changed since 0.0.23.

* **An AI assistant manual ships with the package**:
  `tflspec_ai_manual()` returns (or copies out) `inst/ai/tflspec-ai-user-manual.md`,
  the briefing to attach to a chat session -- the specification formats,
  the functions that write code from them, what that code looks like, the
  former names that are refused, and the complete export list (checked
  against the exports by a test).  rtfreporter's own manual covers the
  rendering side.

* **Table definitions follow rtfreporter's redesigned plan verbs**
  (rtfreporter >= 0.8.1.9003, ichirio/rtfreporter#498).  What
  `tfl_table_plan()` builds, `tfl_table_code()` writes and
  `tfl_as_table_spec()` reads back:
  - `table_plan()` takes only the roles (`cols`, `rows`, `label`, `stat`),
    and so does `tfl_table_plan()`'s `...`: `stats`, `value`, `na` (and
    `notes`) are `plan_cells()`'s, `sort_stat` is `plan_sort(stat = )`,
    `sep` is `plan_columns(sep = )`, `header_n` is
    `plan_col_header(values = )`.
  - A `cells` row with no template (a `stats = rows` table's digits) is
    `plan_digits(.rows = )`; `plan_fmt()` is gone.
  - `columns$row_title` and `style$auto_width` go to `plan_columns()`.
  - The `style` sheet is `plan_style()`'s closed list of arguments; it
    gains `row_height_exact`, the cell paddings, `markup`,
    `blank_row_normalize` and the rules of one kind of row,
    `border_header` ... `border_last_row`, written `top | bottom` or
    `none`.
  - **Workbook columns renamed** to the verbs' arguments:
    `layout$stub_into` is `stub_name`, `group_show` is `group_keep`,
    `colpages_carry` is `colpages_keep`; `layout$pages_by` is removed
    (one page per value is `group_page = TRUE` with `group_col`), and
    `group_col` belongs to the page group only.  A workbook with a former
    column is refused with what to write instead; nothing is read under
    its old name.  The example workbooks are rebuilt, and the five
    example reports are byte-identical to 0.0.23.9000's.
  - The ORR example's derived key is `orr` (it was `orr_ci`): with
    `sep = "_"` a value containing the separator cannot be split back.

* **tflspec is the spec side only** (#29; step 4 of plan E).
  The table engine -- the ARD functions and the plan -- now lives in
  rtfreporter (>= 0.8.1.9001) under its names: `normalize_ard()`,
  `spread_ard()`, `pull_ard()`, `list_ard_keys()`, `cell_rows()`,
  `overall_row()`, `table_plan()`, the `plan_*()` verbs, `plan_apply()`,
  `plan_layers()`, `plan_template()`.  tflspec keeps every spec: ARD
  (`tfl_ard_spec()` ...), table (`tfl_table_spec()`, `tfl_table_plan()`,
  `tfl_table_code()`, `tfl_as_table_spec()`), report (`tfl_report()`,
  `tfl_report_code()`), listing, figures.
  - rtfreporter is no longer a `Depends`: a program that runs a spec or
    the code tflspec writes starts with `library(rtfreporter)` (the code
    is written unqualified, as before).
  - `tfl_table_code()` writes `table_plan() |> plan_*()`; widths go to
    `plan_columns(widths = )` (rtfreporter dropped `plan_style(widths = )`).
  - The former names (`tfl_ard_normalize()`, `tfl_plan()`, `tfl_plan_*()`,
    `tfl_apply_plan()`, `tfl_plan_layers()`, `tfl_plan_template()`,
    `tfl_ard_template()`) are gone from tflspec; no aliases.
  The five example reports and the users' sample reports are byte-identical
  across the two packages.

  The move was adopted (rtfreporter's pre-CRAN API review): the engine is rtfreporter's from its 0.8.2 release on.

* tflspec now follows rtfreporter's version scheme: a release is `X.Y.Z`
  (tagged, with a GitHub Release -- v0.0.23 is the first), development is
  `X.Y.Z.9000`.

# tflspec 0.0.23

* **Composed figures** (#39): a figure design may have `plots:` (name ->
  a whole figure design, each reading its own data) and `compose:`
  (`layout`, e.g. `km | box` or `(a | b) / c`; `add`, calls after it with
  `op: "+"` or `"&"`: `plot_layout`, `plot_annotation`, `theme` ...). The
  script writes each figure as it would alone, keeps it as `fig_<name>`,
  lays them out with patchwork and saves once at the design's `plot`
  size. A figure with panels below it (number at risk, n) or with
  ggsurvfit's `add_risktable()` is wrapped with `wrap_elements()` (the
  latter built with `ggsurvfit_build()`, without which patchwork drops
  the table), so it stays one figure with one tag.
  `tfl_check_fig_design()` checks each plot (parts `plots$<name> ...`),
  the layout's names, and the compose calls; `tfl_fig_advice()` gives
  each plot's advice with fixes aimed at it (`fix$plot`).
* ggsurvfit's `add_*` as `call` layers: the check requires the KM curves
  layer, and reports `add_risktable()` together with the `risk_table`
  layer (the number at risk twice) or with an `n_table` panel. Alone,
  `add_risktable()` needs nothing more: `ggsave()` keeps its table.
* **Extension functions by name**: `tfl_fig_calls()` lists ggh4x
  (`facet_nested`, `facet_nested_wrap`, `facetted_pos_scales`), ggtext
  (`element_markdown`, `element_textbox_simple`), ggforce (`facet_zoom`)
  and ggnewscale (`new_scale_colour`, `new_scale_fill`, `new_scale`); a
  `call` of one needs no `package:`. The geom catalog gains `sina`
  (ggforce's `geom_sina`). cowplot, ggpubr, ggbreak ... are reached with
  `package:`.
* ggplot2 4.0's label-attribute advice also looks at joined datasets and
  counts an empty axis label as none.

# tflspec 0.0.22

* **Figure designs are written for ggplot2 3.5 or 4.0** (#37).
  `inst/fig/ggplot2_compat.csv` lists what differs between the two for a
  design's `call` pieces (functions and arguments added, renamed,
  removed, deprecated); `tfl_fig_compat()` returns it (with
  `ggplot2_version =`, what each row means for that version).
  The target is the `ggplot2_version` argument of
  `tfl_fig_design_code()` / `tfl_check_fig_design()` /
  `tfl_fig_advice()`, else the design's top-level `ggplot2_version:`,
  else `getOption("tflspec.ggplot2_version")`, else the installed
  ggplot2's; `"3.5"` and `"4.0"` are allowed.
  * The script writes renamed functions and arguments under the target's
    names (`geom_label(label.size)` <-> `linewidth`, `coord_trans` <->
    `coord_transform`, `layer_scales` -> `get_panel_scales` ...), with a
    `# ggplot2 4.0: a -> b` comment; a version guard
    (`stopifnot(utils::packageVersion("ggplot2") >= "4.0.0")`) only when
    it uses a 4.0-only feature; `# Written for ggplot2 X` in its header
    only when the target was set. The output of designs without `call`
    pieces (every template) is unchanged.
  * `tfl_check_fig_design()` reports what the target does not have
    (`coord_cartesian(reverse =)` for 3.5, `element_geom()` ...) or drops
    (`geom_bar()` / `geom_col()` `size =` in 4.0: not translated to
    `linewidth`). Names of the other version are no longer reported as
    unknown by the installed-version check.
  * `tfl_fig_advice()` gives what the target deprecates (`size =` for
    lines, a numeric `legend.position`, `geom_errorbarh()` ...) with a
    fix, `op = "compat"`, that rewrites the call for it; and notes that
    ggplot2 4.0 titles an untitled axis with its column's `label`
    attribute (an ADaM variable label).
* The CI checks the package with ggplot2 3.5.2 and the latest 4.0.x too;
  a test checks the compat table's rows against the installed ggplot2.
* `tfl_check_fig_design()` without data no longer reports "no data named
  df" for a layer that reads `df`.

# tflspec 0.0.21

* **A generic `call` piece for figure designs** (#34): a `layers:` element
  `{layer: call, fn, package, data, aes, pos, args, base}` writes
  `p <- p + fn(data = ..., aes(...), pos..., args...)` for *any* ggplot2
  or extension function (`add_quantile`, `add_risktable`, `geom_label` with
  settings the catalog does not have ...), in its place among the other
  layers. `plot: {add: [...]}` writes the same shape *after* the figure's
  settings (scales, axes, labs, theme preset, legend) and before any
  panels -- for `theme()`, `scale_*`, `coord_*`, `facet_*`, `labs`,
  `guides` and the like, which would otherwise be overridden by (or
  override, in a confusing order) the design's own settings.
  `tfl_fig_parts()` gains the `call` and `plot_add` pieces (and `plot`
  gains the `add` field) without changing any existing piece or field.
* Argument values follow a small YAML -> R table: numbers, logicals, `~`
  (`NULL`) and `.inf` write as R literals; a sequence (even of length 1)
  writes as `c()`; a map without `fn` writes as a named vector (quoting a
  non-syntactic name); a map with `fn` writes as a nested call
  (`element_text(...)`, `unit(...)`, `arrow(...)` ...); strings inside
  `aes:`/`vars()` are written as expressions, not quoted. The YAML tag
  `!r` (e.g. `theme: !r theme_risktable_default(axis.text.y.size = 9)`)
  is raw R, kept verbatim; `tfl_fig_r()` makes one in R, and
  `tfl_read_fig_design()`/`tfl_write_fig_design()` round-trip it. Every
  generated call is checked with `parse()` before being handed back.
* `tfl_check_fig_design()` validates a `call`/`plot.add`: an unresolved
  package is a warning (code generation still qualifies it as `pkg::fn`);
  an unknown argument to a function without `...` is an error, with an
  `agrep()` suggestion; an unknown `geom_*`/`stat_*` parameter or
  aesthetic is a warning (checked against that geom/stat's own
  aesthetics and parameters); an unknown `theme()` element is an error
  (checked against `ggplot2::get_element_tree()`); other `...` functions
  (`labs`, `aes`, `vars`, ggsurvfit's `add_*` ...) are not checked. A
  figure-wide function (`theme*`, `scale_*`, `coord_*`, `facet_*`,
  `labs`, `guides`, axis titles) used in `layers` warns to use `plot.add`
  instead; a `plot.add` call that overrides `facet_by`, `colour_by`,
  `x_min`/`x_max`/`y_min`/`y_max` or `x_log` warns.
* Out of scope for this release (later): a ggplot2 3.5/4.0 compatibility
  table and a two-version CI matrix; `compose:`/`plots:` for multiple
  (patchwork) figures; ggsurvfit `add_*`-specific integration (risk-table
  de-duplication, `ggsurvfit_build()` assembly).

# tflspec 0.0.20

* **The plan no longer reads the spec** (#29; step 1 of plan E).  Inside
  tflspec the ARD / plan code (`R/ard.R`, `R/plan.R`,
  `R/rtfreporter-glue.R`) references no spec function any more; the spec
  code has its own file, `R/table_spec.R`.  Behaviour is unchanged: the
  ten example RTFs and the five users' sample reports are byte-identical.
  - `tfl_plan()` loses `spec =`.  New **`tfl_table_plan(data, spec,
    output_id)`** builds the plan from a workbook through the public verbs
    (the steps `tfl_table_code()` writes); a role given in the call still
    wins.  Data first, like `tfl_plan()`, so it pipes:
    `ard |> tfl_ard_normalize() |> tfl_table_plan(spec)`.
  - `tfl_plan_col_header(header = <data frame of cells>)` is the plan's
    own: the cell rows (`line`, `cols` -- `.values`, `KEY = value` --,
    `span`, `text`, borders) are typed and placed by the plan.
  - New **`tfl_plan_layers(plan)`**: what a plan declares and what it
    resolved to (roles, layers by kind, cells, columns, the header as cell
    rows, pages).  `tfl_as_table_spec()` reads only that and
    `tfl_apply_plan()`.
  - `tfl_plan_template()` loses `spec =`.

# tflspec 0.0.19

* **Every figure type has a template** (#32): 27 templates in parts --
  KM (4), waterfall (2), swimmer (4: bars, by best response, with the
  response at each assessment, with event markers), spider, bar (rate with
  95% CI, 100% stacked, dodged), mean over time (4), spaghetti, box (3),
  scatter (shift, xy), PK (mean, mean on a log axis, individual per
  group) -- and 9 whole-script templates for the types not yet in parts
  (forest hr / or, AE dot, butterfly, eDISH, sankey, sunburst): one
  `figure` layer with the type's arguments.  `tfl_fig_templates()` gains
  `parts`; `tfl_fig_template()` gains `x`, `y`, `at_visit`, `category`,
  `responders`, `id`, `duration` and `...`.
* New pieces for them: statistics `summary_by` (by group only), `rate`
  (exact binomial CI, with a label), `count` (n and % of a category, with
  a label) and `subset` (another dataset, or rows of `df`, as an object
  the layers draw: assessments, ongoing subjects); `summary` gains
  `positive` (no lower bar at or below 0, for a log axis).  Figure
  settings gain `x_log`, `y_log`, `equal` and `facet_by` (one panel per
  value).  Catalog layers gain `alpha`, `position` (R), `vjust`, an
  `arrow` for segments, `na.rm` and a `group` for points.
* Advice: a whole-script design gets none; no legend is asked for when
  the groups are on an axis or in panels; the n panel is suggested only
  for figures by visit.

# tflspec 0.0.18

* **Advice on figure designs** (#28): `tfl_fig_advice(design, adam)` says
  what is usually wanted and is missing or unusual -- a KM figure without
  the number at risk or censor marks, a time axis still in days, a legend
  inside the panel with many groups, more groups than the palette has
  colours, a group colouring with no legend, text visits with no order, a
  waterfall without its +20% / -30% marks, no analysis set kept, nothing
  drawn ...  Each line names its part (`data`, `stats`, `plot`, `layers`)
  and its level (`info`, `warning`), and where one change would do it
  carries a fix that `tfl_fig_apply_fix()` makes (a layer or step added, a
  setting changed).  The checks stay [tfl_check_fig_design()]: errors
  there, advice here.

# tflspec 0.0.17

* **Styling verbs** (#26): `tfl_plan_header_style()`, `tfl_plan_col_style()`
  and `tfl_plan_zone_style()` declare rtfreporter's `style_header()`,
  `style_cols()` and `style_zone()` with their own arguments, so header
  bold / alignment / borders, column styles and zone borders no longer need
  a `tfl_plan_after()` step.  They run on the pages in the order written
  (after the header and the decimal alignment, before any after() step);
  `cols` may be column names and `.values`, so a reordered table keeps
  them.  A workbook cannot carry them: `tfl_as_table_spec()` lists them.
  A plan with only `tfl_plan_columns()` now also goes to pages.

# tflspec 0.0.16

* **`tfl_plan_after()` is the way out, not the way in** (#24).  The usual reasons to reach for it have declarations, which name
  columns and go into a workbook: `set_decimal_split()` ->
  `tfl_plan_columns(decimal = ".values")`, `paginate_cols()` ->
  `tfl_plan_paginate_cols()`, widths by position ->
  `tfl_plan_columns(widths = c(<column> = , .values = ))`.  `?plan_verbs`
  has the table; the error of `tfl_plan_after()` no longer suggests
  `set_decimal_split()`.  The PK example is written with declarations only
  (its RTF is byte-identical).  A test checks that `tfl_plan_columns(decimal
  = )` gives exactly the pages a `set_decimal_split()` step gives.

# tflspec 0.0.15

* **Figure designs are now parts and layers** (#20): a design is four
  lists of small pieces a GUI lists, adds and edits one by one --
  - `data`: the steps from ADaM to the plot's data (`read`, `join`, `param`,
    `flag`, `filter`, `derive`, `time_unit`, `levels`, `rank`, and
    `data_code`: your own code for what no step does);
  - `stats`: `survfit` (survfit2), `summary` (n, mean, SD, SE, interval by
    group and visit), `stats_code`;
  - `plot`: title, axes, colours, theme, legend, size;
  - `layers`, in order: KM curves / bands / censor marks, the number at risk
    and n panels, reference line labels, **any layer of the geom catalog**,
    `geom` (any function by name), `layer_code`, or `figure` (a figure
    type's whole script, for the types not yet in parts).
* **The geom catalog** (`inst/fig/geoms.csv`, `inst/fig/geom_fields.csv`):
  ggplot2's common geoms and ggrepel's text, each with the aesthetics it
  maps and the settings it takes; one generic writer makes the code, so a
  geom is added by adding rows.  `tfl_fig_add_layer()` adds one for the
  session (another package's geom, a company's own layer).
* **Templates**: `tfl_fig_template()` fills the four parts at once --
  `km_risk_table`, `km_simple`, `km_ci`, `km_single_arm`, `mean_se`,
  `mean_sd`, `mean_ci`, `mean_se_n`, `waterfall_response`,
  `waterfall_plain` (`tfl_fig_templates()`); sizes and line widths from the
  figure style standard.
* `tfl_fig_parts()` describes every piece and its fields;
  `tfl_check_fig_design()` checks a design against them and the data
  (variables through the steps, PARAMCDs, the layers' data, the order of
  layers).  `tfl_fig_design()` now takes the four parts; a 0.0.12 design
  file (`type` / `style` / `args`) reads as a `figure` layer.

# tflspec 0.0.14

* **Table and report definitions write code too** (#19), as ARD and
  listing definitions do, so a generator (tflplanner) can write the whole
  report program:
  - `tfl_table_code(spec, output_id)` writes the `tfl_plan()` pipeline the
    workbook stands for: the roles of its `tables` sheet in `tfl_plan()`,
    and one `tfl_plan_*()` verb for each thing the other sheets say.  The
    reverse of `tfl_as_table_spec()`; what a workbook cannot say is written
    under it by hand.
  - `tfl_report_code(spec, output_id, content)` writes the rtfreporter calls
    of the report: `rtf_document()`, `rtf_section()` (running header and
    footer), `rtf_tables()` / `rtf_figures()`, `rtf_titles()`,
    `rtf_footnotes()` -- a program that needs only rtfreporter.
  - Both come from the same list of steps that `tfl_plan(spec = )` and
    `tfl_report()` now run, so the object and the program cannot disagree;
    on the five example reports the written program gives byte-identical
    RTF (checked by `data-raw/ard-spec-examples/make-examples.R`), and the
    objects are unchanged.
* What only a workbook could say now has a public form, so the code can say
  it: `tfl_plan(sort_stat = )`, `tfl_plan_col_header(header = <data frame of
  cells>)` (the `col_header` sheet's rows), and a new verb
  `tfl_plan_columns(widths = , decimal = )` (widths by column name and
  decimal alignment: the `columns` sheet).  `tfl_plan_digits(rounding = )`
  on its own no longer also declares an empty set of digits.

# tflspec 0.0.13

* `tfl_read_data_code()`: a dataset the data catalog has not gives
  `stop("tflspec: dataset X is not in the data catalog.")` (it said
  `tflplanner:`, though the code is tflspec's) (#17).

# tflspec 0.0.12

* **Figure designs** (#14): a figure as data -- its type, its style and the
  arguments of its `tfl_fig_<type>()` -- kept as one YAML file per figure.
  - `tfl_fig_schema()` describes every argument of every figure type: its
    section (data / mapping / style / axes / legend / output), its kind (a
    dataset, a PARAMCD, a variable, one of a set of values, a number ...),
    basic or advanced, its default and its choices.  For `km`, `waterfall`
    and `swimmer` it includes the axis and look options they take through
    `...`.  A GUI draws its form from it (tflplanner's Plot Designer).
  - `tfl_fig_design()`, `tfl_write_fig_design()` / `tfl_read_fig_design()`
    (YAML), `tfl_fig_design_code()` (the script, by the type's
    `tfl_fig_<type>()`) and `tfl_check_fig_design()` (the arguments against
    the schema, and the variables and PARAMCDs against the data).
  - 'yaml' is a new import.

# tflspec 0.0.11

* **Listing specs are read in tflspec** (#13), as ARD and table specs are:
  the definition is two sheets keyed by `output_id`, `listings` (dataset,
  where, sort, max_rows, type) and `listing_cols` (vars, label, width,
  collapse_repeats) -- the sheets of tflplanner's
  `listing_figure_spec.xlsx`, which reads unchanged.
  - `tfl_listing_spec()` makes the definition (class `tfl_listing_spec`)
    and checks it: a listing with no dataset or no columns, a `where` that is
    not R, a `sort` that is not variable names, a `max_rows` that is not a
    whole number, a `width` that is not a positive number, a
    `collapse_repeats` that is not TRUE / FALSE, columns of an unknown
    listing.  `check = FALSE` keeps a draft.
  - `tfl_read_listing_spec(path, output_id)` / `tfl_write_listing_spec()`:
    the workbook (other sheets, and a `note` column, are ignored).
  - `tfl_listing_code(spec, output_id, datasets)` now takes the definition
    (or its workbook's path) instead of loose data-frame rows.
  - New `tfl_listing(spec, data, output_id)`: the pages themselves from data
    in hand -- the same pages the code makes (tested).

# tflspec 0.0.10

* **The former names are gone** (#11): `ard_normalize()`, `rtf_plan()`,
  `table_plan()`, `plan_cells()`, `rtf_report()`, `pp_km()`, `plot_code()`
  and the rest now stop with "could not find function"; call the `tfl_`
  name (the table is in 0.0.9 below).  tflspec keeps only the `tfl_`
  functions, before its API grows around two sets of names.

# tflspec 0.0.9

* **Every function now starts with `tfl_`** (#9), so tflspec's names do not
  collide with cards / cardx (`ard_*`) or rtfreporter (`rtf_*`) when they
  are attached together, and say whose function it is -- as `rtf_` does
  for rtfreporter.  The scheme is `tfl_` + area + action:
  - ARD: `tfl_ard_spec()`, `tfl_read_ard_spec()`, `tfl_ard_code()` (was
    `ard_spec_code()`), `tfl_build_ard()`, `tfl_ard_normalize()`,
    `tfl_ard_spread()`, ...
  - Tables: `tfl_table_spec()`, `tfl_read_table_spec()`, `tfl_plan()` (was
    `rtf_plan()` / `table_plan()`) and the verbs `tfl_plan_cells()`,
    `tfl_plan_digits()`, ..., `tfl_apply_plan()`, `tfl_plan_template()`.
  - Reports: `tfl_report()` (was `rtf_report()`), `tfl_read_report_spec()`,
    `tfl_report_path()`.
  - Listings: `tfl_listing_code()` (was `listing_spec_code()`),
    `tfl_read_data_code()`.
  - Figures: `tfl_fig_km()`, `tfl_fig_waterfall()`, ... (were `pp_*()`),
    `tfl_fig_spec()` / `tfl_read_fig_spec()` / `tfl_fig_code()` /
    `tfl_fig_list_code()` (were `plot_spec()`, `read_plot_spec()`,
    `plot_code()`, `plot_list_code()`), `tfl_fig_style()`,
    `tfl_check_fig()` (was `check_figure()`), `tfl_fig_types()` (was
    `pp_styles()`); figures drawn directly: `tfl_plot_sankey()`,
    `tfl_plot_sankey_batch()`, `tfl_plot_sunburst()`.
  S3 classes follow: `tfl_plan`, `tfl_table_spec`, `tfl_ard_spec`,
  `tfl_ard_cells`, `tfl_ard_overall`, `tfl_fig_spec` (was `pp_spec`),
  `tfl_code` (was `pp_code`).
* **The former names still work**: each is the same function as its new
  name (see `?tflspec-superseded` for the table).  They are superseded --
  no warning -- and will be removed before the first CRAN release.
  Generated code (`tfl_plan_template()`, figure scripts) writes the new
  names.  The five example reports' RTF are byte-identical.

# tflspec 0.0.8

* **pharmaverseadam example** (`inst/examples/pharmaverseadam/`): a plot
  list with 19 clinical figures (`plot_list.xlsx`), the data preparation
  tflspec does not do (`prepare_adam.R`) and `run_all.R` (spec -> programs ->
  figures). All 19 programs run without warnings.
* Template figure types take `where =`, an extra record condition in R code
  (e.g. only scheduled visits).
* Groups follow the paired numeric code of ADSL when there is one (`TRT01AN`
  for `TRT01A`) instead of alphabetical order.
* Generated programs cope with real data: swimmer event markers are skipped
  when no subject has an event; a forest model that does not converge is
  shown as NE; eDISH drops records without a ULN.
* `write_plot_code()` / `plot_list_code(dir =)` keep plot ids in file names
  (`F-01.R`, not `F.01.R`).
* **Catalogue of clinical figure types**: `pp_catalog()` classifies every type
  and style (subtype) by category (efficacy: time to event / tumour response /
  subgroups and rates, longitudinal, safety, PK / PD, distribution, treatment
  patterns), with planned types recorded for later. `pp_styles()` and the
  Excel plot list are derived from it.
* **Ten new figure types**, each with styles: `pp_forest()` (hr, or,
  estimates), `pp_bar()` (rate_ci, stacked, dodged), `pp_mean()` (se, sd, ci,
  se_n), `pp_individual()` (spaghetti, spider), `pp_box()` (by_visit,
  by_group, change), `pp_ae_dot()` (risk_diff, incidence), `pp_butterfly()`
  (soc, pt), `pp_edish()` (alt, alt_ast), `pp_scatter()` (shift, xy) and
  `pp_pk()` (mean, mean_log, individual). Their scripts depend only on
  dplyr / ggplot2 (+ survival, patchwork).
* Swimmer subtype: `pp_swimmer(start =, end =)` draws bars from a start day
  instead of 0.
* `pp_example_adam()` gains `ADAE`, `ADLB`, `ADPC`, tumour size over time in
  `ADTR`, and `SEX` / `AGEGR1` / `TRT01A` / start and end days in `ADSL`
  (drawn from a separate random stream: existing values are unchanged).
* **Treatment-sequence figures moved here from ydisctools**: `plot_sankey()`
  (nodes laid out from the node table, baseline-aware ribbon stacking,
  shared / adaptive scales), `plot_sankey_subgroups_batch()` and
  `plot_sunburst()`, with their tests. `plot_sankey_polygon()` (deprecated
  alias) was not carried over.
* `sankey_data()` / `sunburst_data()` build their inputs from one row per
  subject and line (e.g. lines of therapy), optionally by subgroup.
* Quick API `pp_sankey()` (styles `grey_links`, `colored_links`,
  `subgroups`) and `pp_sunburst()` (style `rings`); both are also available
  as `type` in the Excel plot list (`group` = subgroup variable of sankey).
  Unlike the other figure types, these scripts call tflspec.
* `pp_example_adam()` gains `ADLOT` (lines of therapy); existing datasets
  are unchanged.
* ggplot2 and rlang are now imported.

# tflspec 0.0.7

* **`rtf_plan()` is now `table_plan()`**, and the object it returns is of
  class `table_plan` (was `rtf_plan`), the plan paired with `table_spec`.
  The plan lives in tflspec, not rtfreporter, so it no longer carries
  rtfreporter's `rtf_` prefix.  `rtf_plan()` still works: it is the same
  function under its former name, **superseded** -- write `table_plan()` in
  new code.  `plan_template()` now writes `table_plan()` (#7).

# tflspec 0.0.6

* **Figure style standard** (`fig_style()`, `fig_style_template()`,
  `read_fig_style()`): every look-and-feel value of a figure -- font sizes,
  line widths, ticks, reference lines, the censor mark, palettes, event
  markers, the output size -- in one catalog of three sheets (`settings`,
  `colors`, `markers`).  The built-in one is taken from the KM / waterfall /
  swimmer sample programs; a company sets its own with
  `options(tflspec.fig_style = read_fig_style(path))`.  `pp_km()`,
  `pp_waterfall()` and `pp_swimmer()` take their defaults from it (so the
  generated scripts change: e.g. the KM line is 0.3, the censor mark size 3
  / stroke 0.6, the 50% line `twodash`; a swimmer event named like a marker
  of the style -- `Death`, `Discontinued` -- takes its shape and colour).
* **Figure checks** (`fig_setup_code()`, `check_figure()`): the style as
  data plus `theme_tfl()`, `scale_colour_tfl()` / `scale_fill_tfl()`,
  `tfl_marker()`, `tfl_save()` and `tfl_check()` as plain ggplot2 code, for
  figure programs written by hand or by an AI.  `tfl_check()` warns
  ("Figure check: ...") about rows ggplot2 dropped, legend colours that are
  not the standard's and a legend not in the expected order, notes data
  beyond the visible axes, and returns a fingerprint of the drawn data.
* **KM number at risk from the table's ARD**: `pp_km(ard = )` reads the
  number at risk from the KM table's `cardx::ard_survival_survfit()` ARD, so
  the figure and the table agree, and warns when it differs from the
  curve's own count.

# tflspec 0.0.5

* **`library(tflspec)` is enough**: rtfreporter moved from `Imports` to
  `Depends`, so attaching tflspec attaches rtfreporter too.  tflspec
  extends rtfreporter (a plan is one of its table sources), which is the
  case `Depends` is for.  Code written for the rtfreporter branch needs
  `library(tflspec)` and nothing else, unless it calls the moved functions
  as `rtfreporter::` (now `tflspec::`).
* R CMD check (`--as-cran`) runs on GitHub Actions: ubuntu devel / release
  / oldrel-1, macOS, Windows.

# tflspec 0.0.4

* **The ARD spec engine moved here from tflplanner**: an Excel definition of
  the study's analyses (`study`, `datasets`, `populations`, `analyses`) ->
  cards / cardx code -> the study ARD.  `ard_spec()`, `read_ard_spec()`,
  `write_ard_spec()`, `ard_spec_code()`, `build_ard()`, `ard_for()`, and the
  catalogs `ard_methods()` / `ard_statistics()`.  New: `ard_spec_template()`,
  `ard_spec_hash()` (was internal), and `ard_spec_code(part = "setup" /
  "body")` for a program layout of one's own.
* The catalogs have built-in defaults here; a company's own are passed as
  `statistics =` / `methods =` to the functions that use them (tflplanner
  passes its company standards), or set with
  `options(tflspec.ard_statistics =, tflspec.ard_methods =)`.
* **Listing code** moved from tflplanner too: `listing_spec_code()` writes a
  listing's program from its definition rows, and `read_data_code()` the
  lines that read a dataset of the data catalog.  The layout itself stays
  rtfreporter's (`listing_spec()`, `as_rtftables(listing = )`).
* Verified: tflplanner's generated ARD programs, report programs, listing
  code and ARD status for two studies are identical before and after the
  move (104 / 104).

# tflspec 0.0.3

* **plotplanner is now tflspec.** The repository and package were renamed
  when the table half joined, so one package holds the Excel specifications
  for tables, listings and figures.  Options `plotplanner.*` are now
  `tflspec.*`.
* **The ARD / plan / spec family moved here from rtfreporter**
  (ichirio/rtfreporter#474, branch `feat/474-ard-experimental` at 2d608cc):
  `ard_*()`, `rtf_plan()` and the `plan_*()` verbs, `apply_plan()`,
  `table_spec()` / `read_table_spec()` / `write_table_spec()` /
  `as_table_spec()`, and `read_report_spec()` / `rtf_report()` /
  `report_path()`, together with their tests, the example workbooks
  (`inst/extdata/ard-spec/`) and their builders (`data-raw/`).  The code is
  unchanged apart from names: generated scripts now say `tflspec::`, and the
  pipe option is `tflspec.ard_pipe`.  Rounding still follows rtfreporter's
  `round_num()` and `rtfreporter.rounding`.
* A plan is a table source for rtfreporter: tflspec registers
  `as_rtftables.rtf_plan()` on rtfreporter's `as_rtftables()` generic
  (rtfreporter >= 0.8.0.9085), so `rtf_tables(doc, plan)` works as it did on
  the branch.
* Verified against the branch: the same 735 tests pass, and the five example
  reports (DM, AE, ORR, LB, PK) give byte-identical RTF, from both the plan
  code and the Excel spec.
