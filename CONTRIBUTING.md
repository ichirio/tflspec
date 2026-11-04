# Contributing to tflspec

Issues and pull requests are welcome at <https://github.com/ichirio/tflspec>.
For anything non-trivial, open an issue first so we agree on the approach.
A good first contribution is a template: see `dev/adding-a-template.md`.

## Workflow

1. Fork the repository and clone your fork.
2. Add the original repository as `upstream`:
   `git remote add upstream https://github.com/ichirio/tflspec.git`.
3. Start from the current `next`:
   `git fetch upstream`, then `git checkout -b <your-branch> upstream/next`.
4. Make the change, commit, and push the branch to your fork.
5. Open a pull request against `next` of `ichirio/tflspec`.

Branch from `next`, not from `main`. The maintainers add labels and merge.
If your branch falls behind, merge `upstream/next` into it.

## Rules

- Do not change `Version:` in `DESCRIPTION`, `NEWS.md` or the version line of
  the AI manuals. Describe the user-visible change in the pull request
  description. The maintainers write the NEWS entry and set the version.
- Use fictional data only: Example Pharma and the bundled sample studies
  (`adsl`, `adae`, `adlb` ...). Never put a company's specifications or data
  in code, tests, issues or pull requests.
- R source is ASCII-only. Write other characters as `\uXXXX` escapes.
  Non-ASCII is fine in comments and roxygen.
- Change the code, its tests and its roxygen text together.
- Keep the line endings of the files you edit.

## Checks before a push

Install the package from your branch, then run the tests of the area you
changed. `<area>` is part of a test file's name: `ae-pt-freq` runs
`tests/testthat/test-tpl-ae-pt-freq.R`.

```r
install.packages(".", repos = NULL, type = "source")   # or devtools::install()
library(tflspec)
Sys.setenv(NOT_CRAN = "true")                           # run the tests CRAN would skip
testthat::test_dir("tests/testthat", filter = "<area>")
```

Run the lint check, and `devtools::document()` after any roxygen change.
Commit the `man/*.Rd` files it changes for your work, and revert the ones it
changes for nothing you touched.

```r
lintr::lint_package()      # should be clean
devtools::document()
```

The continuous integration runs the full test suite and `R CMD check` on
your pull request.
