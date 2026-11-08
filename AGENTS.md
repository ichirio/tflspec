# Agent and contributor instructions for tflspec

This repository is the R package tflspec (`DESCRIPTION` is at the root).
tflspec turns Excel and YAML specifications of tables, listings and
figures into R programs that use rtfreporter. Its exports start with
`tfl_`.

## Where things are

| What | Where |
|----|----|
| The code | `R/` (templates: `R/tpl_*.R`; the registry: `R/tpl_registry.R`) |
| Tests | `tests/testthat/` |
| Help pages | `man/` (made by `devtools::document()`; commit them) |
| Sample data | `data/` (`adsl`, `adae`, `adlb` …: fictional) |
| Vignettes and articles | `vignettes/` |
| Developer scripts and guides | `dev/` (not shipped) |

## Read these first

- Workflow, checks and rules for a pull request: `CONTRIBUTING.md`.
- Adding a template, step by step: `dev/adding-a-template.md`.
- The git steps (fork, update `main`, rebase): the last section of this
  file.

## The three packages

- Dependencies point one way: rtfreporter \<- tflspec \<- tflplanner.
  tflspec knows nothing of tflplanner.
- The SPEC mirrors rtfreporter’s API and has no shapes of its own. A
  change of shape starts in rtfreporter.

## Pull requests

- Branch from `main` and open the pull request against `main`. (The
  maintainers work from [`next`](https://rdrr.io/r/base/Control.html); a
  contributor’s pull request does not.)
- One change per pull request. Do not stack pull requests: do not branch
  from an open pull request’s branch.
- Do not change `Version:` in `DESCRIPTION`, `NEWS.md` or the version
  line of the AI manuals.
- When sheet columns, template arguments or the generated code change,
  tflplanner may need a follow-up (a new column needs a translation
  there). Say so in the pull request. The maintainers run tflplanner’s
  tests.
- Fill in the pull request template. The maintainers add labels and
  merge.

**Record the change in the PR description.** A PR never edits `NEWS.md`,
`CHANGELOG.md` or `Version:`, and there is no separate memo file. Fill
the template’s *User-visible change (for NEWS)* field without being
asked: one or two bullets in NEWS style (what a user can now do, or what
is fixed, named by its function or argument), or “none” for an internal
change; then “Breaking: no”, or “Breaking: yes” with the migration. The
maintainers write `NEWS.md` from the merged PRs at release. For example:

``` markdown
## User-visible change (for NEWS)

- `tfl_tpl()`'s `ae_pt_freq` takes `min_pct`: a preferred term with less than
  that percent of subjects in every arm is left out (`min_pct_in` says where
  the percent must be met).

Breaking: no
```

## Rules to keep

- Use fictional data only (Example Pharma, the bundled sample studies).
  Never put a company’s specifications or data in code, tests, issues or
  pull requests.
- R source is ASCII-only. Write other characters as `\uXXXX` escapes.
  Non-ASCII is fine in comments and roxygen.
- Change the code, its tests and its roxygen text together. Run
  `devtools::document()` after a roxygen change.
- Keep the line endings of the files you edit.
- Split a source file before it passes 2,000 lines.
- Explain a feature by what it does, not by comparison with another
  package; name another package only when it is the actual subject (an
  engine, a dependency, an adapter).

## Tests

- Before a push, run the tests of what you changed and
  `lintr::lint_package()`. `CONTRIBUTING.md` has the commands.
- Run tests where they are. Never copy them into a temporary folder.
- Run the tests in one R session too
  ([`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html)
  on the installed package, as `CONTRIBUTING.md` shows), not only one
  file at a time: per-file runs miss interference between tests.
- Point `R_USER_CONFIG_DIR`, `R_USER_DATA_DIR` and `R_USER_CACHE_DIR` at
  a work folder, never at the user’s own.
- Use R 4.6.1 or later to develop. The package itself says `R (>= 4.1)`
  in `DESCRIPTION`, so write code that runs on R 4.1.

## Git work for a contributor

If you are an agent working for a contributor, you do the git work as
well. Their fork is `origin`; `https://github.com/ichirio/tflspec.git`
is `upstream` (`git remote add upstream <url>`, one time only). Check
with `git remote -v` before you start. Run `git status` first; if the
working tree has uncommitted changes, commit them or ask, and never
discard them.

### Update the fork’s main

Do this before every new piece of work. A fork does not update itself.
Keep `main` pristine: never commit to it.

``` bash
git checkout main
git fetch upstream
git merge --ff-only upstream/main
git push origin main
```

Then cut the topic branch: `git checkout -b <type>/<issue>-<slug>`.

### If the fork’s main has gone stale or has its own commits

If `git merge --ff-only` fails, `main` has commits upstream does not
have. Stop and tell the contributor which commits
(`git log upstream/main..main`). The reset below discards them, so run
it only after they agree, and keep the work on a branch first:

``` bash
git branch my-old-work main
git fetch upstream
git checkout main
git reset --hard upstream/main
git push origin main --force-with-lease
```

This is the only time `main` is ever force-pushed.

### Rebase a topic branch on upstream/main

Do this when the branch has fallen behind, or when CI or a reviewer asks
for it.

``` bash
git fetch upstream
git checkout <topic-branch>
git rebase upstream/main
```

If there are conflicts, git stops at each one:

1.  `git status` lists the conflicted files. Open each one and keep both
    sides’ intent: upstream’s change and the contributor’s.
2.  If you cannot tell what a side intended, read its commit
    (`git log -p upstream/main -- <file>`). If you still cannot tell,
    run `git rebase --abort` and ask. Don’t guess.
3.  `git add <file>`, then `git rebase --continue`. Repeat until it
    finishes. Never edit `Version:` or `NEWS.md` to settle a conflict;
    take upstream’s.

Then run the tests of the area you changed again, because a rebase
changes the code under them. Only when they pass:

``` bash
git push --force-with-lease origin <topic-branch>
```

- `--force-with-lease` on the topic branch only: a rebase rewrites its
  history, so a plain push is refused.
- Never use plain `--force`. If `--force-with-lease` is refused, someone
  else pushed to the branch: fetch and look at what they pushed before
  going on.
- Never force-push `main`, except for the stale-fork reset above.

### When CI or a review asks for a rebase

- “This branch is out of date” or “has conflicts”: rebase as above and
  push. The pull request updates by itself; don’t open a new one, and
  don’t close it.
- CI failed: read the failed step’s log first. If the failure comes from
  your change, fix it in a new commit on the branch. Rebase only if
  upstream has moved past a fix you need.
- A review asked for changes: add commits on the branch and reply on the
  pull request. Don’t rewrite history that the reviewer has already
  read, unless they asked for a rebase or squash.
- Stop and ask the contributor if a rebase would drop commits you didn’t
  write, or if conflicts reach beyond the files they changed.
