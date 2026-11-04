<!--
Thanks for contributing to tflspec!
Please read CONTRIBUTING.md first, and fill in the sections below.
Keep the PR focused on one logical change.
-->

## What and why

<!-- What does this PR do, and why?  One or two sentences. Link the issue,
     e.g. "Closes #12".  Open an issue first for anything non-trivial. -->

Closes #

## User-visible change (for NEWS)

<!-- This is the record of the change: a PR never edits NEWS.md, CHANGELOG.md
     or Version, and the maintainers write NEWS from the merged PRs at release.
     One or two bullets in NEWS style, or "none" for an internal change. -->

-

Breaking: no  <!-- or "yes", and the migration: what old code must change to -->

## Checks

- [ ] Branched off the latest `main`; the PR targets `main`. One change; not
      stacked on another PR.
- [ ] Tests added or updated under `tests/testthat/`; the tests of the area pass
      (the commands are in `CONTRIBUTING.md`), also in one R session on the
      installed package.
- [ ] Roxygen updated and `devtools::document()` run; only my own `man/*.Rd`
      committed.
- [ ] `lintr::lint_package()` is clean.
- [ ] I did not edit `NEWS.md`, `CHANGELOG.md`, `Version:` or the AI manuals'
      version line.
- [ ] R code is ASCII-only; fictional data only.
- [ ] If sheet columns, template arguments or the generated code changed: the
      possible tflplanner follow-up is described here.
