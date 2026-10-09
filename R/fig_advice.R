# Advice on a figure design: what would usually be done, and one-step
# fixes for it.
#
# The checks (tfl_check_fig_design()) say what is wrong -- a variable not
# in the data, a layer with no data.  The advice says what is usually
# wanted and is missing or unusual: a KM figure without the number at
# risk, a legend inside the panel with many groups, more groups than the
# palette has colours, a waterfall with the patients' numbers on the x
# axis ...  Each line names the piece it is about and, where one change
# would do it, carries a fix: a small edit of the design that
# tfl_fig_apply_fix() makes -- a layer or step added, a setting changed.
# A GUI shows the advice by the preview and offers the fix as one click.
#
# Rules are functions of (design, ctx) that return zero or more lines
# (.adv()); ctx holds what the rules read from the data: the levels of the
# colour variable, the dataset's variables, the PARAMCDs.

# message = the line, or a template for sprintf() with `args` -- kept apart
# so a GUI can translate the template and format it itself
.adv <- function(rule, level, part, message, fix = NULL, args = list()) {
  structure(list(rule = rule, level = level, part = part,
                 message = if (length(args)) do.call(sprintf, c(list(message), args)) else message,
                 template = message, args = args, fix = fix),
            class = "tfl_fig_advice_line")
}

# a fix: op = add_layer | add_step | set_plot | set_piece | remove_layer |
# compat (a call piece rewritten for a ggplot2 version, fig_compat.R)
.fix <- function(op, ...) list(op = op, ...)

.layer_kinds <- function(d) vapply(d$layers, function(l) l$layer %||% "", "")
.step_kinds <- function(d) vapply(d$data, function(s) s$step %||% "", "")
.has_layer <- function(d, k) any(.layer_kinds(d) == k)
.has_step <- function(d, k) any(.step_kinds(d) == k)
.layer_i <- function(d, k) which(.layer_kinds(d) == k)
.step_i <- function(d, k) which(.step_kinds(d) == k)

# what the rules read from the data (NULL parts when there is no data)
.fig_advice_ctx <- function(design, adam) {
  adam <- if (!is.null(adam)) pp_prep_adam(adam)
  ctx <- list(adam = adam, levels = NULL, palette_n = NA_integer_, vars = NULL)
  reads <- Filter(function(s) identical(s$step, "read"), design$data)
  if (!is.null(adam) && length(reads)) {
    ctx$vars <- names(adam[[toupper(reads[[1L]]$dataset %||% "")]])
  }
  pal <- tfl_fig_palettes()[[.pv(design$plot, "palette") %||% "treatment"]]
  ctx$palette <- pal
  ctx$palette_n <- length(pal)
  by <- design$plot$colour_by
  if (!is.null(adam) && !is.null(by)) {
    # the variable in the dataset read, else in ADSL (joined)
    ds <- if (length(reads)) adam[[toupper(reads[[1L]]$dataset %||% "")]]
    x <- if (!is.null(ds) && by %in% names(ds)) ds[[by]] else adam$ADSL[[by]]
    if (!is.null(x)) {
      ctx$levels <- if (is.factor(x)) levels(droplevels(x)) else sort(unique(as.character(x[!is.na(x)])))
    }
  }
  ctx
}

.fig_advice_rules <- function() list(
  # ---- KM
  km_risk = function(d, ctx) {
    if (.has_layer(d, "km_curve") && !.has_layer(d, "risk_table")) .adv(
      "km_risk", "info", "layers",
      "A KM figure usually shows the number at risk below the curves.",
      .fix("add_layer", layer = list(layer = "risk_table")))
  },
  km_censor = function(d, ctx) {
    if (.has_layer(d, "km_curve") && !.has_layer(d, "censor_mark")) .adv(
      "km_censor", "info", "layers",
      "KM curves usually mark where subjects are censored.",
      .fix("add_layer", layer = list(layer = "censor_mark"), after = "km_curve"))
  },
  km_axis = function(d, ctx) {
    if (.has_layer(d, "km_curve") && is.null(d$plot$x_max)) .adv(
      "km_axis", "info", "plot",
      "Set X max and X step, so the breaks (and the number at risk's columns) fall on round times.")
  },
  km_unit = function(d, ctx) {
    sf <- Filter(function(s) identical(s$step, "survfit"), d$stats)
    if (length(sf) && !.has_step(d, "time_unit")) .adv(
      "km_unit", "info", "data",
      "The time is in days; a KM axis is usually in months (or weeks).",
      .fix("add_step", step = list(step = "time_unit", variable = sf[[1L]]$time %||% "AVAL",
                                   unit = "months")))
  },
  # ---- groups, colours, legend
  palette_short = function(d, ctx) {
    n <- length(ctx$levels)
    if (n && is.null(names(ctx$palette)) && n > ctx$palette_n) .adv(
      "palette_short", "warning", "plot",
      "%d groups, but the palette has %d colours: some groups get none.", args = list(n, ctx$palette_n))
  },
  palette_names = function(d, ctx) {
    n <- length(ctx$levels)
    if (n && !is.null(names(ctx$palette))) {
      miss <- setdiff(ctx$levels, names(ctx$palette))
      if (length(miss)) .adv(
        "palette_names", "warning", "plot",
        "The palette has no colour for %s (drawn grey).", args = list(paste(miss, collapse = ", ")))
    }
  },
  legend_inside = function(d, ctx) {
    n <- length(ctx$levels)
    if (n > 4L && grepl("^inside", .pv(d$plot, "legend") %||% "")) .adv(
      "legend_inside", "info", "plot",
      "%d groups: a legend inside the panel may cover the data; below is usual.",
      .fix("set_plot", legend = "bottom"), args = list(n))
  },
  legend_none = function(d, ctx) {
    n <- length(ctx$levels)
    by <- d$plot$colour_by
    # no legend needed when the groups are on an axis or in panels
    on_axis <- !is.null(by) && (identical(d$plot$facet_by, by) ||
      any(vapply(d$layers, function(l) identical(l$x, by) || identical(l$y, by), logical(1))))
    if (n > 1L && !on_axis && identical(.pv(d$plot, "legend"), "none")) .adv(
      "legend_none", "warning", "plot",
      "%d groups are coloured, but there is no legend.",
      .fix("set_plot", legend = "bottom"), args = list(n))
  },
  no_colour = function(d, ctx) {
    pieces <- .fig_pieces()
    maps <- unlist(lapply(d$layers, function(l) {
      g <- pieces[[l$layer %||% ""]]$geom
      if (is.null(g)) return(NULL)
      c(if ("colour" %in% g$aes) l$colour, if ("fill" %in% g$aes) l$fill)
    }))
    if (is.null(d$plot$colour_by) && length(maps)) .adv(
      "no_colour", "warning", "plot",
      "Layers colour by %s, but the figure's 'Colours by' is empty: the palette is not applied.",
      .fix("set_plot", colour_by = unique(maps)[1L]), args = list(paste(unique(maps), collapse = ", ")))
  },
  # ---- mean over time
  mean_n = function(d, ctx) {
    sm <- Filter(function(s) identical(s$step, "summary") && grepl("VISIT", toupper(s$by %||% "")), d$stats)
    if (length(sm) && .has_layer(d, "errorbar") && !.has_layer(d, "n_table")) {
      by <- .split_vals(sm[[1L]]$by)
      eb <- d$layers[[.layer_i(d, "errorbar")[1L]]]
      .adv("mean_n", "info", "layers",
           "A mean-over-time figure usually shows the n of each group at each visit below it.",
           .fix("add_layer", layer = list(layer = "n_table", data = sm[[1L]]$name %||% "sm",
                                          x = eb$x, group = eb$colour %||% by[1L])))
    }
  },
  visit_order = function(d, ctx) {
    xs <- unique(unlist(lapply(d$layers, function(l) l$x)))
    xs <- xs[grepl("^AVISIT$|VISIT$", xs)]
    ordered <- unlist(lapply(d$data, function(s) if (identical(s$step, "levels")) s$variable))
    for (x in setdiff(xs, ordered)) {
      return(.adv("visit_order", "warning", "data",
        "%s is text: without an order its visits sort alphabetically. Order it by its number.",
        .fix("add_step", step = list(step = "levels", variable = x, order_by = paste0(x, "N"))),
        args = list(x)))
    }
  },
  # ---- waterfall
  waterfall_ref = function(d, ctx) {
    if (.has_step(d, "rank") && .has_layer(d, "col")) {
      ys <- unlist(lapply(d$layers, function(l) if (identical(l$layer, "hline")) .split_vals(l$yintercept)))
      if (!all(c("20", "-30") %in% ys)) .adv(
        "waterfall_ref", "info", "layers",
        "A waterfall usually marks +20% (progression) and -30% (response).",
        .fix("add_layer", layer = list(layer = "hline", yintercept = "20, -30", linetype = "dashed",
                                       colour = "grey", linewidth = 0.5)))
    }
  },
  waterfall_x = function(d, ctx) {
    if (.has_step(d, "rank") && .has_layer(d, "col") && isTRUE(as.logical(.pv(d$plot, "x_text")))) .adv(
      "waterfall_x", "info", "plot",
      "The bars' numbers on the x axis say nothing; they are usually hidden.",
      .fix("set_plot", x_text = FALSE))
  },
  # ---- general
  nothing = function(d, ctx) {
    if (!length(d$layers)) .adv("nothing", "warning", "layers", "No layer: nothing is drawn.")
  },
  no_read = function(d, ctx) {
    if (!.has_step(d, "read")) .adv("no_read", "warning", "data", "No dataset is read.",
                                    .fix("add_step", step = list(step = "read", dataset = "ADSL"), first = TRUE))
  },
  pop = function(d, ctx) {
    if (.has_step(d, "read") && !.has_step(d, "flag") && !.has_step(d, "data_code")) {
      # the flag the data has: the usual ones first, else any *FL
      have <- grep("FL$", ctx$vars %||% character(), value = TRUE)
      flag <- c(intersect(c("FASFL", "SAFFL", "ITTFL", "PPROTFL"), have), have, "SAFFL")[1L]
      .adv("pop", "info", "data",
           "No population: every row of the dataset is used. Keep the population's flag (FASFL, SAFFL ...).",
           .fix("add_step", step = list(step = "flag", variable = flag)))
    }
  },
  # ---- ggplot2 3.5 / 4.0 (fig_compat.R)
  gg_compat = function(d, ctx) {
    unlist(lapply(.fig_call_pieces(d), function(p) {
      w <- .fig_compat_walk(p$spec, ctx$gg$version, p$part)$warns
      lapply(unique(w), function(m) .adv(
        "gg_compat", "warning", p$sec, "%s: %s",
        .fix("compat", sec = p$sec, i = p$i, ggplot2_version = ctx$gg$version),
        args = list(p$part, m)))
    }), recursive = FALSE)
  },
  gg_label_attr = function(d, ctx) {
    # an axis the design leaves untitled, mapped to a column with a label
    if (!identical(ctx$gg$version, "4.0") || is.null(ctx$adam)) return(NULL)
    # the columns of the datasets read and joined, with their labels
    dss <- unlist(lapply(d$data, function(s) if (s$step %in% c("read", "join")) toupper(s$dataset %||% "")))
    labs <- list()
    for (nm in rev(dss)) {
      ds <- ctx$adam[[nm]]
      for (v in names(ds)) if (!is.null(attr(ds[[v]], "label"))) labs[[v]] <- attr(ds[[v]], "label")
    }
    labelled <- names(labs)
    mapped <- function(ax) unique(as.character(unlist(lapply(d$layers, function(l)
      c(l[[ax]], if (identical(l$layer, "call")) l$aes[[ax]])))))
    hit <- c(if (is.null(.pv(d$plot, "x_label"))) intersect(mapped("x"), labelled),
             if (is.null(.pv(d$plot, "y_label"))) intersect(mapped("y"), labelled))
    if (length(hit)) .adv(
      "gg_label_attr", "info", "plot",
      "ggplot2 4.0 titles an axis with its column's label attribute when the design gives no title: %s would be titled '%s'. Set the X / Y label to choose it.",
      args = list(hit[1L], labs[[hit[1L]]]))
  },
  gg_installed = function(d, ctx) {
    inst <- .fig_installed_gg()
    if (!identical(inst, ctx$gg$version) && length(.fig_call_pieces(d))) .adv(
      "gg_installed", "info", "design",
      "The calls' arguments are checked against the installed ggplot2 %s; the script is for %s, by the compat table (tfl_fig_compat()).",
      args = list(as.character(utils::packageVersion("ggplot2")), ctx$gg$version))
  },
  size = function(d, ctx) {
    w <- as.numeric(.pv(d$plot, "width")); h <- as.numeric(.pv(d$plot, "height"))
    if (!is.na(w) && !is.na(h) && w < h) .adv(
      "size", "info", "plot", "The figure is taller than wide; a landscape page usually wants the reverse.")
  }
)

#' Advice on a figure design
#'
#' What is usually wanted and is missing or unusual in a design -- a KM
#' figure without the number at risk, a legend inside the panel with many
#' groups, more groups than the palette has colours, text visits with no
#' order ...  Not errors (those are [tfl_check_fig_design()]): the figure
#' draws either way.  Where one change would do it, the line carries a
#' fix that `tfl_fig_apply_fix()` makes: a layer or a step added, a
#' setting changed.
#'
#' @param design A `tfl_fig_design`.
#' @param adam The data ([tfl_read_adam()]); with it the groups' levels
#'   are counted against the palette and the legend.
#' @param ggplot2_version The ggplot2 the design is for (see
#'   [tfl_fig_compat()]): what that version deprecates in a `call` is
#'   advice, with a fix that rewrites the call for it.
#' @param fix One row's `fix` (a list: `op` and its fields; `plot` names
#'   the plot of a composed design it is for).
#' @return `tfl_fig_advice()`: a data frame with `rule`, `level` (`info`,
#'   `warning`), `part` (`data`, `stats`, `plot`, `layers`, `add`, `design`), `message`,
#'   `template` and `args` (the message before `sprintf()` and its values,
#'   for a GUI that translates it) and `fix` (a list column; `NULL` where
#'   there is no one-step fix).
#'   `tfl_fig_apply_fix()`: the design, changed.
#' @export
tfl_fig_advice <- function(design, adam = NULL, ggplot2_version = NULL) {
  if (length(design$plots)) return(.fig_advice_compose(design, adam, ggplot2_version))
  ctx <- .fig_advice_ctx(design, adam)
  ctx$gg <- .fig_target_version(ggplot2_version, design)
  lines <- list()
  whole <- any(.layer_kinds(design) == "figure")
  for (r in if (whole) list() else .fig_advice_rules()) {
    out <- tryCatch(r(design, ctx), error = function(e) NULL)
    if (inherits(out, "tfl_fig_advice_line")) out <- list(out)
    lines <- c(lines, Filter(function(x) inherits(x, "tfl_fig_advice_line"), out))
  }
  .fig_advice_df(lines)
}

.fig_advice_df <- function(lines) {
  data.frame(
    rule = vapply(lines, `[[`, "", "rule"),
    level = vapply(lines, `[[`, "", "level"),
    part = vapply(lines, `[[`, "", "part"),
    message = vapply(lines, `[[`, "", "message"),
    template = vapply(lines, `[[`, "", "template"),
    args = I(lapply(lines, `[[`, "args")),
    fix = I(lapply(lines, `[[`, "fix")),
    stringsAsFactors = FALSE)
}

#' @rdname tfl_fig_advice
#' @export
tfl_fig_apply_fix <- function(design, fix) {
  if (is.null(fix)) return(design)
  if (!is.null(fix$plot)) {
    # a fix for one plot of a composed design
    design$plots[[fix$plot]] <- tfl_fig_apply_fix(.fig_as_design(design$plots[[fix$plot]]),
                                                  fix[setdiff(names(fix), "plot")])
    return(design)
  }
  switch(fix$op,
    add_layer = {
      k <- .layer_kinds(design)
      at <- if (!is.null(fix$after) && any(k == fix$after)) max(which(k == fix$after)) else length(k)
      design$layers <- append(design$layers, list(fix$layer), after = at)
    },
    add_step = {
      if (isTRUE(fix$first)) {
        design$data <- c(list(fix$step), design$data)
      } else if (!is.null(fix$before) && fix$before %in% .step_kinds(design)) {
        design$data <- append(design$data, list(fix$step),
                              after = min(which(.step_kinds(design) == fix$before)) - 1L)
      } else {
        # after the last keep-rows step, before any derive of the same variable
        design$data <- c(design$data, list(fix$step))
      }
    },
    set_plot = {
      for (f in setdiff(names(fix), "op")) design$plot[[f]] <- fix[[f]]
    },
    set_piece = {
      x <- design[[fix$sec]][[fix$i]]
      for (f in setdiff(names(fix), c("op", "sec", "i"))) x[[f]] <- fix[[f]]
      design[[fix$sec]][[fix$i]] <- x
    },
    remove_layer = design$layers <- design$layers[-fix$i],
    compat = design <- .fig_compat_fix_piece(design, fix$sec, fix$i, fix$ggplot2_version),
    stop("Unknown fix: ", fix$op, call. = FALSE))
  design
}
