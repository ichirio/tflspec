# Adding a template

This guide takes you from nothing to a template in a pull request. A person
or an agent can follow it step by step. Read `CONTRIBUTING.md` first for the
workflow and the rules (branch from `next`, no `Version:` or `NEWS.md`
change, fictional data only, ASCII-only R source).

Commands run from the package root. Where a step names a function, find it
by name: line numbers move.

## 0. Set up once

```r
install.packages(".", repos = NULL, type = "source")   # or devtools::install()
library(tflspec)
Sys.setenv(NOT_CRAN = "true")   # run the tests CRAN would skip
```

Install again after you change code in `R/`: the tests run against the
installed package.

## 1. Choose: a variant or a new template

- A **variant** is an existing template with other defaults: another filter,
  other labels, a different set of variables. It is one call to
  `tfl_template_variant()` in `R/tpl_registry.R`. Use it when the layout
  (summary, hierarchical, listing ...) already does what you need.
  Examples: `ae_ser_soc_pt` (serious events: a `where` and a label) and
  `stf_t02_demog` (demographics with a Total column and its own row order).
- A **new template** has its own program: parameters and a body that builds
  the items. Use it when no existing layout can produce the table by
  changing defaults. Example: `lab_abnormal` in `R/tpl_lab_abnormal.R`.

Start with a variant if you can. The signature is

```r
tfl_template_variant(name, from, defaults = list(), label = NULL,
                     example = NULL, ai = NULL, hide = NULL)
```

`from` is a template in `tfl_templates()$name`. Each name in `defaults` must
be one of `from`'s parameters: `names(tfl_template_params("ae_soc_pt"))`.

## 2. The variant path

The example below makes a fictional variant, `ae_rel_soc_pt`: treatment-
emergent adverse events judged related to the study drug. Replace the names
with yours.

1. **Define it** in `R/tpl_registry.R`, next to the variants of the same
   `from`. For `ae_soc_pt` that is below the block that defines `ae_soc_pt`,
   `ae_ser_soc_pt` and `ae_disc_soc_pt`. For a variant of `summary`, copy the
   shape of `stf_t02_demog`, below `demog`.

   ```r
   tfl_template_variant("ae_rel_soc_pt", from = "ae_soc_pt",
     label = "Related adverse events by SOC and PT",
     example = list(population = "SAF"), ai = FALSE,
     defaults = list(where = "TRTEMFL == \"Y\" & AEREL == \"RELATED\"",
                     any_row_label = "Any related TEAE"))
   ```

   Keep every string ASCII. Check the names and values you filter on against
   the bundled data, for example `table(adae$AEREL)`.

2. **Set `ai`.** See "The `ai` trap" below. A first contribution sets
   `ai = FALSE`.

3. **Add its example** to `.tpl_examples` in `R/tpl_company.R`: one line,
   the parameters a test run uses. `test-template-company.R` fails if a
   registered template has no entry (it compares `tfl_templates()$name` with
   `names(.tpl_examples)`).

   ```r
   ae_rel_soc_pt = list(population = "SAF"),
   ```

   Put it next to the template it comes from. Mind the comma at the end of
   the list when the entry is last.

4. **Add its help page block** to `R/tpl_docs.R`, in alphabetical order:

   ```r
   #' @eval .tpl_rd("ae_rel_soc_pt")
   NULL
   ```

5. **Make the help page.** Run `devtools::document()`. It writes
   `man/tfl_tpl_ae_rel_soc_pt.Rd`, and `test-tpl-docs.R` checks that it
   exists and has a row for each parameter. If `document()` also changes Rd
   files that have nothing to do with your template, revert those
   (`git checkout -- man/<file>.Rd`); stage only your own.

6. **Write its tests** in `tests/testthat/test-tpl-<name>.R`. Model the file
   on `tests/testthat/test-tpl-ae-pt-freq.R`: a test of the expanded items
   (`tfl_template_expand()`), then a test of the table against the key
   (step 4 of "Computing the key"). A variant has no `tpl_<name>()` function;
   call it with `tfl_tpl(ard, "<name>", population = "SAF")`.

7. **Optional: a catalog row** in `inst/templates/catalog.csv`, only when the
   template writes an item of a published collection (FDA STF IG v2, PhUSE
   CSS, CDISC ARS). The columns are `source,item,title,alias,template,status,note`;
   `status` is one of `same`, `close`, `missing`, `not_output`. A row
   makes the item's `alias` run your template. Skip it for a template of
   your own.

`test-template-to-spec-run.R` and `test-template-spec.R` take your example
from `.tpl_examples` by themselves and check that the template runs as a
template report and as the code SPEC. You write nothing for them. If your
example cannot be built from the bundled studies, tell the maintainers in the
pull request.

## 3. The `ai` trap

A variant takes `ai` from the template it comes from when you leave it
`NULL`. `ae_soc_pt` has `ai = TRUE`, so `ae_ser_soc_pt` and `ae_disc_soc_pt`
are listed in the AI requests that answer with templates
(`tfl_ai_prepare(templates = TRUE)`). If you do not set `ai`, your variant
joins that list without anyone choosing it, and `tests/testthat/test-tfl-tpl.R`
fails: it compares `sort(t$name[t$ai])` with a fixed list of names.

You have two ways out:

- Set `ai = FALSE` in the definition. Nothing else changes. **A first
  contribution should do this.** What the AI is offered is the maintainers'
  decision, and they can switch it on in a later change.
- Set `ai = TRUE` (or leave it inherited) and add your name to the sorted
  list in `test-tfl-tpl.R`, in the test "tfl_tpl(): the same plan as the
  template's own function; its errors". Do this only if the maintainers
  asked for it.

`stf_t02_demog` is the example of the first way.

## 4. The new-template path

1. **Write `R/tpl_<name>.R`.** Model it on `R/tpl_lab_abnormal.R`. It has
   four parts: the parameters (a list of `tfl_param()`), the body
   `function(args, ctx)` that returns the ARD analyses, the data and the
   content items, a `.tpl_<x>_register()` function that calls
   `.tfl_template()` (and, if the template needs an example or parameter
   kinds of its own, `.tpl_extend(examples = , param_kinds = )`), and an
   exported `tpl_<name>()` wrapper with its roxygen text.
2. **Register it.** R reads `R/` in file-name order and the registry file is
   read after your file, so the template is registered at the end of
   `R/tpl_registry.R`, in the block of `.tpl_*_register()` calls under the
   comment "the templates in files of their own". Add your call there. Name
   your file so that it sorts before `tpl_registry.R`.
3. **Its example.** Either put it in `.tpl_extend(examples = list(<name> =
   ...))` in your register function (as `R/tpl_pk.R` does), or add a line
   to `.tpl_examples` in `R/tpl_company.R` (as `lab_abnormal` does).
4. **Its help page.** Add the `@eval .tpl_rd("<name>")` block to
   `R/tpl_docs.R` as in step 4 of the variant path. A table or listing
   template gets `man/tfl_tpl_<name>.Rd` from it. `devtools::document()`
   also exports `tpl_<name>()` (the `NAMESPACE` is generated) and writes its
   page. List `tpl_<name>` in `_pkgdown.yml`, in the section with
   `tpl_lab_abnormal`, so the reference index stays complete.
5. **Its tests.** Model `tests/testthat/test-tpl-lab-abnormal.R`: the
   expanded items, the table against a key, the call equal to the report's
   items, the RTF.
6. **Set `ai`.** `.tfl_template()` defaults to `ai = FALSE`. Leave it.

## 5. Computing the key

A test of a table must not use the template to get its expected numbers.
Count them from the bundled data in plain R, then expect those numbers.

```r
test_that("the table: the any-event row is the key", {
  skip_if_not_installed("cards")
  saf <- adsl[adsl$SAFFL == "Y", ]
  ae <- adae[adae$SAFFL == "Y" & adae$TRTEMFL == "Y" & adae$AEREL == "RELATED", ]
  # the key: the subjects with at least one such event, by arm
  key <- tapply(ae$USUBJID, ae$TRT01A, function(u) length(unique(u)))
  ard <- cards::ard_stack_hierarchical(ae, variables = c(AEBODSYS, AEDECOD),
                                       by = TRT01A, denominator = saf,
                                       id = USUBJID, over_variables = TRUE)
  p <- tfl_tpl(ard, "ae_rel_soc_pt", population = "SAF")
  tab <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  expect_identical(tab$group1[1L], "Any related TEAE")
  expect_identical(as.integer(sub(" .*", "", tab[["Drug X"]][1L])),
                   as.integer(key[["Drug X"]]))
})
```

Do not copy the numbers out of the table you are testing. Compute them from
`adsl`, `adae` and the other bundled datasets with the filter you wrote in
the definition, and give the key a comment that says what it counts.

## 6. Run the tests

Install again, then run these (each is a `filter` of
`testthat::test_dir("tests/testthat", filter = ...)`):

| Filter | What it checks |
|--------|----------------|
| `tpl-<name>` | your own test file |
| `template-company` | the example exists and expands |
| `tfl-tpl` | the registry and the `ai` list |
| `tpl-docs` | the help page exists, a row for each parameter |
| `tpl-help` | the help text of the templates |
| `tpl-catalog` | the catalog, if you added a row |
| `template-spec` | every template as a template SPEC |
| `template-to-spec-run` | every template built to an RTF (needs `haven`) |

```r
for (f in c("tpl-<name>", "template-company", "tfl-tpl", "tpl-docs", "tpl-help",
            "template-spec", "template-to-spec-run")) {
  testthat::test_dir("tests/testthat", filter = f)
}
lintr::lint_package()
```

Then open the pull request as `CONTRIBUTING.md` describes. The continuous
integration runs everything else.

## 7. What the Shiny app needs

Nothing. tflplanner reads the registry live (`tflspec::tfl_templates()`),
so your template shows up as soon as the package with it is installed.
tflplanner has an optional map of one-line descriptions, `.tpl_desc` in its
`R/template_report.R`. A template missing from it is listed without a
description. You do not need to change tflplanner for your pull request.

## Checklist

- The definition, with `ai` set.
- The example in `.tpl_examples` (or `.tpl_extend(examples = )`).
- The `@eval .tpl_rd(...)` block in `R/tpl_docs.R`.
- `devtools::document()` run; only your Rd files staged.
- A test file with a key counted in plain R.
- The tests of step 6 pass; `lintr::lint_package()` is clean.
- No change to `Version:`, `NEWS.md` or the AI manuals' version line.
