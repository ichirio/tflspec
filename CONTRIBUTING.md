# Contributing to tflspec

Issues and pull requests are welcome at <https://github.com/ichirio/tflspec>.
For anything non-trivial, open an issue first so we agree on the approach.
A good first contribution is a template: see `dev/adding-a-template.md`.

## Workflow

1. Fork the repository and clone your fork.
2. Add the original repository as `upstream`:
   `git remote add upstream https://github.com/ichirio/tflspec.git`.
3. Start from the current `main`:
   `git fetch upstream`, then `git checkout -b <your-branch> upstream/main`.
4. Make the change, commit, and push the branch to your fork.
5. Open a pull request against `main` of `ichirio/tflspec`.

Branch from `main` and open the pull request against `main`. (The maintainers
work from `next`; yours does not.) The maintainers add labels and merge.
If your branch falls behind, rebase it on `upstream/main`: the section "Rebase
a topic branch on upstream/main" in `AGENTS.md` has the steps. Keep your
fork's `main` up to date the same way ("Update the fork's main" there).

## Rules

- Do not change `Version:` in `DESCRIPTION`, `NEWS.md` or the version line of
  the AI manuals. The maintainers set the version.
- One change per pull request. Do not stack pull requests.
- When sheet columns, template arguments or the generated code change,
  tflplanner may need a follow-up (a new column needs a translation there).
  Say so in the pull request; the maintainers run tflplanner's tests.
- Use fictional data only: Example Pharma and the bundled sample studies
  (`adsl`, `adae`, `adlb` ...). Never put a company's specifications or data
  in code, tests, issues or pull requests.
- R source is ASCII-only. Write other characters as `\uXXXX` escapes.
  Non-ASCII is fine in comments and roxygen.
- Change the code, its tests and its roxygen text together.
- Keep the line endings of the files you edit.

## The change record

**Record the change in the PR description.**  A PR never edits `NEWS.md`,
`CHANGELOG.md` or `Version:`, and there is no separate memo file.  Fill the
template's *User-visible change (for NEWS)* field without being asked: one or
two bullets in NEWS style (what a user can now do, or what is fixed, named by
its function or argument), or "none" for an internal change; then "Breaking:
no", or "Breaking: yes" with the migration.  The maintainers write `NEWS.md`
from the merged PRs at release.  For example:

```markdown
## User-visible change (for NEWS)

- `tfl_tpl()`'s `ae_pt_freq` takes `min_pct`: a preferred term with less than
  that percent of subjects in every arm is left out (`min_pct_in` says where
  the percent must be met).

Breaking: no
```

## Checks before a push

Install the package from your branch, then run the tests of the area you
changed. `<area>` is part of a test file's name: `ae-pt-freq` runs
`tests/testthat/test-tpl-ae-pt-freq.R`. A name's underscores become hyphens
there.

```r
install.packages(".", repos = NULL, type = "source")   # or devtools::install()
Sys.setenv(NOT_CRAN = "true")                           # run the tests CRAN would skip
testthat::test_dir("tests/testthat", filter = "<area>",
                   package = "tflspec", load_package = "installed")
```

`package` and `load_package` run the tests against the installed package.
Without them, the tests of the help pages (`tpl-docs`, `tpl-help`) fail with
`could not find function ".tpl_rd"`.

Run the lint check, and `devtools::document()` after any roxygen change.
Commit the `man/*.Rd` files it changes for your work, and revert the ones it
changes for nothing you touched.

```r
lintr::lint_package()      # should be clean
devtools::document()
```

A pull request against `main` runs the full R-CMD-check: R CMD check on
ubuntu (R devel, release, oldrel and 4.2), macOS and Windows, with ggplot2 3.5
and 4.0 and the oldest cards and cardx. It also builds the pkgdown site. A lint
job runs on every pull request; run `lintr::lint_package()` before a push too,
so that it does not fail there. (The maintainers' pull requests into `next` run
the lint job and a quick test job.)
