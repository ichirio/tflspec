# programs-family: the study's report and ARD programs, frozen

Golden fixtures of `test-report-programs.R` (tflspec#910, step G5-1c): the
report programs and ARD programs tflplanner wrote before they moved to
tflspec (`tfl_report_program()`, `tfl_ard_program()`).  No program's text may
change in the move; these files are the text it had.

## How they were frozen

1. Baseline: tflplanner and tflspec as on their branches
   `s1/g5-1b-setup` (tflplanner 2ed24898, tflspec 13372fd4), rtfreporter
   d305c19, installed into a library of their own.
2. Phase A (before any change): for each study below, tflplanner wrote
   - `out/<study>/tfl/<id>.R`: each report's program, `program_code()` in
     the study's code context, as a save writes it;
   - `out/<study>/data/<id>.R`: its data part as a preview makes it
     (`data_lines(todo = FALSE)`, the app's session: no study context);
   - `out/<study>/ard/ard_setup.R` and `out/<study>/ard/<id>.R`: the ARD
     setup and each output's ARD program.

   A program that gave an error holds `ERROR: <its message>`.  The date
   is 2026-11-20 and the `#  Generated  :` line is masked
   (`<masked>`), as it names the generator's version.  A file is the
   program's lines joined by `\n`, with a last `\n`.
3. Phase B (after the move): the moved tflplanner gave the same text for
   every file above, and wrote `in/<study>.rds`: the values tflplanner
   gives tflspec (`.report_values()` of each report, `.report_shared()`,
   the company's inputs `.report_standards()`, the code context's attached
   packages, the layout, the date, the ARD spec and codelists).  Values
   tflplanner gives as functions are worked out; an error a value gave is
   kept as its condition.  A study with a YAML spec has a copy of its
   `study.yml` and `spec/` in `study/<study>/`: its spec reads its series
   copies from there.  A study with ARD programs also has a byte copy of
   the study's own function files (the study key `source`, e.g.
   `programs/ard/functions/ard_cv_geomean.R`) in `study/<study>/`: an ARD
   program's fingerprint (`definition = "<md5>"`) reads their md5, so the
   test passes that folder as `dir`.  The repository's `.gitattributes`
   keeps every file here as it was made (`-text`: no line-end change on
   checkout).

## Studies

- `sample`: SAMPLE-01 (`create_sample_study()`), the default company home.
- `sample-company`: SAMPLE-01 under a company home where every input
  differs from tflplanner's own: its code templates, its settings
  (`listing_type: compact`), its figure sizes, the labels of its ARD
  statistics and methods.
- `edited`: SAMPLE-02 edited in every screen kind (as G5-0 did): a
  figure with a content key, a code SPEC report (its own R), template
  reports of each kind (`T-TPL`, `L-TPL`, `F-TPL`), figures of their own
  code, a Rework (one and two R steps), a Plot Designer figure, steps of a
  report's own code, figures
  whose ARD is their own, a table's or taken in, an ARD taken in by a
  table.
- `sheets`: a planner with no YAML spec and no study: tables, listings
  and figures from the sheets.

Two of G5-0's screen edits of `edited` did not apply when frozen: the
first listing from the data (`L-99-1`: tflplanner's `first_listing()`
is gone) and the titles with quotes (`set_page_line()` is gone).  The
listing kind is still covered by `L-16-2-7`, `L-91`, `L-92` and `L-TPL`.

The AI sample study was not frozen: it has no reports.

## spec_version

The study.yml of `sample` and `sample-company` (SAMPLE-01, as tflplanner's
`create_sample_study()` wrote it) and the setup-family's study.yml
(`tests/testthat/fixtures/setup-family/study/`, written by hand when
frozen) say `spec_version: 2` since the old forms stop (canon 11-10
:377): edited by hand, as no program's text reads the version.  SAMPLE-01
is tflplanner's one source (#891): a change to it is a PR there, not a
data-raw run.
