# ============================================================================
#  A built figure, said in words: what the agent checks against the shell
# ----------------------------------------------------------------------------
#  tfl_ai_run() writes, beside a figure's PNG, <id>-facts.txt: its labels,
#  its axes (range and tick labels), its legends, its layers (the geom and
#  how many rows each draws), its facets and its panels.  An agent that can
#  open the PNG looks at it; one that cannot (an editor's agent that reads
#  text only) reads this file -- and both compare it with the shell.
# ============================================================================

# The facts' lines of a ggplot (or of each plot of a patchwork)
.ai_fig_facts <- function(p) {
  plots <- if (inherits(p, "patchwork") && length(p$patches$plots)) {
    c(p$patches$plots, list(p))
  } else list(p)
  out <- character()
  for (k in seq_along(plots)) {
    if (length(plots) > 1L) out <- c(out, sprintf("## panel %d of %d", k, length(plots)))
    out <- c(out, tryCatch(.ai_one_plot_facts(plots[[k]]), error = function(e) {
      sprintf("(not read: %s)", .ai_one_line(conditionMessage(e)))
    }), "")
  }
  out
}

.ai_one_plot_facts <- function(p) {
  if (!inherits(p, "ggplot")) return("(not a ggplot)")
  b <- suppressMessages(suppressWarnings(ggplot2::ggplot_build(p)))
  lab <- b$plot$labels
  one <- function(x) if (is.null(x) || !length(x)) NA_character_ else
    gsub("\\s+", " ", paste(as.character(unlist(x)), collapse = " "))
  labels <- unlist(lapply(c("title", "subtitle", "x", "y", "colour", "fill", "linetype", "shape",
                            "caption"), function(k) {
    v <- one(lab[[k]])
    if (!is.na(v) && nzchar(v)) sprintf("- %s: %s", k, v)
  }))
  pp <- b$layout$panel_params[[1L]]
  axis <- function(a) {
    s <- pp[[a]]
    if (is.null(s)) return(NULL)
    rng <- tryCatch(s$continuous_range %||% s$dimension(), error = function(e) NULL)
    tl <- tryCatch(stats::na.omit(s$get_labels()), error = function(e) character())
    sprintf("- %s axis: %s%s", a,
            if (is.numeric(rng)) sprintf("from %s to %s", signif(rng[1L], 4), signif(rng[2L], 4)) else "",
            if (length(tl)) sprintf("; ticks: %s", paste(as.character(tl), collapse = ", ")) else "")
  }
  legends <- unlist(lapply(c("colour", "fill", "linetype", "shape", "size"), function(a) {
    s <- b$plot$scales$get_scales(a)
    if (is.null(s)) return(NULL)
    v <- tryCatch(stats::na.omit(s$get_labels()), error = function(e) character())
    if (length(v)) sprintf("- %s: %s", a, paste(as.character(v), collapse = ", "))
  }))
  layers <- vapply(seq_along(b$plot$layers), function(i) {
    l <- b$plot$layers[[i]]
    geom <- paste0("geom_", tolower(sub("^Geom", "", class(l$geom)[1L])))
    sprintf("- %d. %s: %d row(s)", i, geom, nrow(b$data[[i]]))
  }, "")
  facet <- class(b$plot$facet)[1L]
  c(if (length(labels)) c("Labels:", labels),
    "Axes:", axis("x"), axis("y"),
    if (length(legends)) c("Legends (their entries, in order):", legends),
    "Layers (in the order drawn):", if (length(layers)) layers else "- none",
    if (!identical(facet, "FacetNull")) sprintf("Facets: %s, %d panel(s)", facet,
                                                 nrow(b$layout$layout)))
}

# Writes the facts beside the PNG (<id>-facts.txt); the file's path, or
# NULL when there is no plot
.ai_fig_facts_write <- function(p, png) {
  if (is.null(p) || is.null(png) || is.na(png)) return(NULL)
  f <- sub("[.]png$", "-facts.txt", png)
  .ai_write_text(c(sprintf("# %s, as built", sub("[.]png$", "", basename(png))), "",
                   .ai_fig_facts(p)), f)
  f
}
