## Build and check: each report, yourself

A report is not done when its draft is written: build it and check what
R made against the shell yourself -- no person needs to ask.  How deep
you check is the study's, by the report's kind:

{{levels}}

Each level does what the ones below it do.  After you write a request's
drafts, for the reports of a kind built here:

1. **Build** that request alone, in the terminal:

   {{build_cmd}}

   R checks the drafts, builds each report's ARD and RTF and draws its
   figures (R reads the ADaM; you do not).  It takes a minute or so.
   Beside each report's RTF it writes `<id>.R` (the report's program) and
   `<id>-facts.txt` (what it made, in words).  Do not open the RTF.
2. **Read** `ai/status.md` (level 0): each report's status and its files.
   When the check or the build failed, read `ai/repairs/<request>.md` or
   `ai/repairs/<request>-build.md` (the first error, and where), fix the
   draft and build again.
3. **Check** against the shell, point by point, as deep as the report's
   level:
   - level 1, **the program** `<id>.R` (its data and analyses steps): the
     analysis set and the filters, the variables, the denominator, the
     statistics.  For a listing, `<id>.R` is its records' steps (the
     dataset, the filter, the sort) and its layout: read its facts' first
     lines (the rows and the columns) too, to check the filter and the
     sort.
   - level 2, **the facts** `<id>-facts.txt`: a table's header rows (each
     arm, its N), its rows in their order, its empty cells and the
     missing values printed; a figure's labels, axes, legends and layers
     (how many rows each draws).  A missing arm, N or row, a wrong order,
     an empty column, a layer of 0 rows or one group where the shell has
     two is a data error: fix the data steps or the analysis, not the
     look.
   - level 3, **the output**: a figure's PNG beside the shell (when you
     cannot see images, its facts) -- the axes' labels, ranges, ticks and
     time unit; the groups, their order, their colours or line types and
     the legend; what is drawn (lines, points, bars, error bars,
     reference lines, censoring marks); a panel under or beside it (the
     number at risk, a text column) and its numbers.  A table's or a
     listing's pages, the facts' *The pages*: a page over full, a group
     split over two pages, the titles and footnotes as printed.  At level
     2, read a table's *The pages* too when the facts' second line says
     it is more than one page.  The pages are not the draft's (the
     builder sets them): a page over full or a group split is an `ai:
     confirm` that says so (where the shell breaks the pages, if it
     shows), not a fix.
4. **Fix** what differs and build again (1).  At most **two** fixes a
   report.  A build that fails is not a fix: fix what its repair names
   and build again.  When a fix breaks something else, stop: keep the
   draft and mark what differs to confirm.  Where it still fails or still differs after the second:
   - a figure: draw it with your own ggplot2 code -- keep the data steps
     (a `code` step where a piece will not do) and make `layers:` one
     layer that draws the whole plot, `{layer: layer_code, code: 'p <-
     ggplot(df, aes(...)) + ...'}`.  Add the assumption `{where: figure,
     item: route, value: own code, assumption: "two fixes did not draw
     it: <why>"}` and an `ai: confirm` that says the builder cannot edit
     its layers.  Build it once more.
   - a table or a listing: keep the draft, and add an `ai: confirm` that
     says what still differs from the shell.
   Then go on.
5. Say each build in the progress line: `[2/5] T-14-3-1: built, level 2,
   1 fix (the arms' order)`.

When R cannot be run here (the terminal is refused, or R is not found):
say so once, stop building, and ask the person to run `run.R` (it builds
the reports; a check that only reads the drafts does not) and to tell you
when it is done; then do steps 2 to 4 from the files it wrote.
