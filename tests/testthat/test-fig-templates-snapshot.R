# Freezes tfl_fig_design_code() for every template (tfl_fig_templates()) as
# it stood before the generic `call` piece / `plot.add` were added (issue
# #34): the figure-spec extension adds pieces, it must not change what an
# existing design (with none of them) generates.

.fig_tmpl_snapshot_design <- function(t, kind, prm) {
  args <- switch(kind, km = list(param = prm), forest = list(param = prm), list())
  do.call(tfl_fig_template, c(list(t), args, list(group = fig_group_of(t))))
}

fig_tp <- tfl_fig_templates()
fig_adam <- tfl_example_adam()
fig_prm <- unique(fig_adam$ADTTE$PARAMCD)[1]

for (.i in seq_len(nrow(fig_tp))) {
  local({
    i <- .i
    t <- fig_tp$template[i]
    kind <- fig_tp$kind[i]
    test_that(paste("template code is unchanged:", t), {
      d <- .fig_tmpl_snapshot_design(t, kind, fig_prm)
      code <- paste(tfl_fig_design_code(d, plot_id = t), collapse = "\n")
      # the package version in the header changes with every release
      code <- gsub("tflspec [0-9][0-9.]*", "tflspec <version>", code)
      expect_snapshot(cat(code))
    })
  })
}
