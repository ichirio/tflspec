# Agent and contributor instructions for tflspec

This repository is the R package tflspec (`DESCRIPTION` is at the root).
tflspec turns Excel and YAML specifications of tables, listings and figures
into R programs that use rtfreporter. Its exports start with `tfl_`.

## Where things are

| What | Where |
|------|-------|
| The code | `R/` (templates: `R/tpl_*.R`; the registry: `R/tpl_registry.R`) |
| Tests | `tests/testthat/` |
| Help pages | `man/` (made by `devtools::document()`; commit them) |
| Sample data | `data/` (`adsl`, `adae`, `adlb` ...: fictional) |
| Vignettes and articles | `vignettes/` |
| Developer scripts and guides | `dev/` (not shipped) |

## Read these first

- Workflow, checks and rules for a pull request: `CONTRIBUTING.md`.
- Adding a template, step by step: `dev/adding-a-template.md`.

## Rules to keep

- Branch from `next` and open the pull request against `next`.
- Do not change `Version:` in `DESCRIPTION`, `NEWS.md` or the version line of
  the AI manuals. Put the NEWS point in the pull request description.
- Use fictional data only (Example Pharma, the bundled sample studies).
  Never put a company's specifications or data in code, tests, issues or
  pull requests.
- R source is ASCII-only. Write other characters as `\uXXXX` escapes.
  Non-ASCII is fine in comments and roxygen.
- Change the code, its tests and its roxygen text together. Run
  `devtools::document()` after a roxygen change.
- Before a push, run the tests of what you changed and `lintr::lint_package()`.
  `CONTRIBUTING.md` has the commands.
- Keep the line endings of the files you edit.
