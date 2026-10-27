# Notes for Claude Code

The R package tflspec: Excel / YAML specifications of tables, listings
and figures, turned into R programs that use rtfreporter. Exports start
with `tfl_`.

## The three packages

- Dependency direction: rtfreporter \<- tflspec \<- tflplanner. The SPEC
  mirrors rtfreporter’s API: no shapes of its own; a change of shape
  starts in rtfreporter. tflspec can write every rtfreporter feature.

## Pull requests

- Do not change `Version`, `NEWS.md` or the AI manuals’ version line in
  a PR. Put the NEWS point in the PR description; the version is bumped
  only for a tagged snapshot or a CRAN submission.
- Branch from `main`; do not stack PRs. One open PR at a time per group
  of files.
- When a PR is ready, add the `automerge` label. If it must wait for
  another PR (a new sheet column needs tflplanner’s translation first),
  write one line in its description: `merge-after: tflplanner#123`.
- Before a PR: the tests of what changed and lint. When sheet columns or
  the generated code change, also tflplanner’s related tests
  (test-app-robust, test-spec-yaml …) with this branch in a separate
  library. CI runs the rest.

## Tests and code

- Never copy tests into `$TEMP`; run them where they are.
- Point `R_USER_CONFIG_DIR`, `R_USER_DATA_DIR` and `R_USER_CACHE_DIR`
  (and `options(tflplanner.home)`) at a work folder, never at the user’s
  own.
- Run the tests in one R session too
  ([`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html)
  on the installed package), not only one file at a time.
- R 4.6.1 (`C:/Program Files/R/R-4.6.1/bin/Rscript.exe` on Windows).
  ASCII-only R code (`\uXXXX` escapes). Split a file before it passes
  2,000 lines.
