# tflspec (development version)

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

* **tflspec is the spec side only** (#29; step 4 of plan E, Discussion #23).
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
  The five example reports and the Discussion #3 samples are byte-identical
  across the two packages.

  The move was adopted (rtfreporter's pre-CRAN API review, Discussion
  #316): the engine is rtfreporter's from its 0.8.2 release on.

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

* **The plan no longer reads the spec** (#29; step 1 of plan E, Discussion
  #23).  Inside tflspec the ARD / plan code (`R/ard.R`, `R/plan.R`,
  `R/rtfreporter-glue.R`) references no spec function any more; the spec
  code has its own file, `R/table_spec.R`.  Behaviour is unchanged: the
  ten example RTFs and the five Discussion #3 samples are byte-identical.
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

* **`tfl_plan_after()` is the way out, not the way in** (#24, Discussion
  #23).  The usual reasons to reach for it have declarations, which name
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
