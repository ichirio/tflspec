<!-- task -->
## Your task

Draft the design of one figure: the description that tflspec turns into a ggplot2 script. A design has four parts:

- `data`: the steps from the ADaM datasets to the figure's data `df` (read a dataset, keep a parameter and an analysis set, join variables, derive one ...);
- `stats`: what is computed from `df` (a Kaplan-Meier fit, summary statistics by visit ...), each with a `name` the layers refer to;
- `plot`: the figure-wide settings (title, axes, colours, legend, size);
- `layers`: what is drawn, in the order it is drawn.

Start from the template that fits the figure best (name it in `template`; its pieces are good defaults), then change what the documents ask for. When the user's message shows the current design, change that one and keep what the documents do not contradict. Write R code (`data_code`, `stats_code`, `layer_code`) only where no piece does the job.
<!-- values -->
Write each value in its field's kind: a number as a number, `true` / `false` for a logical, anything else as text. A field of kind `variables` or `values` takes several items in one string with ` | ` between them. An R expression is text.
<!-- content -->
the design: `template`, then `data`, `stats` and `layers` as lists of pieces (each piece a mapping that starts with its `step` or `layer`) and `plot` as one mapping.
<!-- whole -->
design
<!-- chat -->
## What to do

In the attached statistical analysis plan, find what it says about this figure: the analysis set, the parameter, the groups, the time axis and what is estimated. In the attached shell (mock-up) of the figure, if there is one, read the axes, their labels and ranges, the legend and any panels below the plot. Then write the answer block as the instructions above describe.
<!-- api -->
## What to do

In the documents below, find what they say about this figure: the analysis set, the parameter, the groups, the time axis and what is estimated; in a shell (mock-up), the axes, their labels and ranges, the legend and any panels below the plot. Then write the answer block as the instructions above describe.
