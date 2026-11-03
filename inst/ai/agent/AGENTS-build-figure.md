## Build and look: each figure, yourself

A figure is not done when its draft is written: build it, look at it and
fix it yourself -- no person needs to ask.  After you write a figure
request's draft:

1. **Build** that request alone, in the terminal:

   {{build_cmd}}

   R checks the draft, builds its ARD and draws the figure (R reads the
   ADaM; you do not).  It takes a minute or so.
2. **Read** `ai/status.md`: the report's status and its PNG.  When the
   check or the build failed, read `ai/repairs/<request>.md` or
   `ai/repairs/<request>-build.md` (the first error, and where), fix the
   draft (`ai/instructions-figure.md`, `ai/ggplot2-to-spec.md`) and build
   again.
3. **Look**: open the PNG beside the shell.  When you cannot see images,
   read `<id>-facts.txt` beside the PNG (the labels, the axes' ranges and
   ticks, the legends, each layer and how many rows it draws).  Check it
   against the shell, point by point: the axes' labels, ranges, ticks and
   time unit; the groups, their order, their colours or line types and
   the legend; what is drawn (lines, points, bars, error bars, reference
   lines, censoring marks); a panel under or beside it (the number at
   risk, a text column) and its numbers.  An empty plot, a layer of 0
   rows, or one group where the shell has two is a data error: fix the
   data steps, not the look.
4. **Fix** what differs and build again (1).  At most **two** fixes a
   figure.  Where it still fails or still differs after the second, draw
   that figure with your own ggplot2 code: keep the data steps (a `code`
   step where a piece will not do) and make `layers:` one layer that
   draws the whole plot, `{layer: layer_code, code: 'p <- ggplot(df,
   aes(...)) + ...'}`.  Add the assumption `{where: figure, item: route,
   value: own code, assumption: "two fixes did not draw it: <why>"}` and
   an `ai: confirm` that says the builder cannot edit its layers.  Build
   it once more, and go on.
5. Say each build in the progress line: `[2/5] F-14-2-1: built, 1 fix
   (the x ticks)`.

When R cannot be run here (the terminal is refused, or R is not found):
say so once, stop building, and ask the person to run the check
(`run.R`, or tflplanner's AI tab) and to tell you when it is done; then
do steps 2 to 4 from the files it wrote.
