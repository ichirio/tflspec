# The figure model: a figure as four parts, each a list of small, named
# pieces a GUI can list, add, remove and edit one by one.
#
#   data    the steps from ADaM to the plot's data `df`: read a dataset,
#           join ADSL, keep a PARAMCD or an analysis set, derive a
#           variable, change a time's unit, order a variable's values,
#           rank rows ... and, for what has no step, the user's own code;
#   stats   what is computed from `df`: a Kaplan-Meier fit (survfit2), the
#           summary statistics by group and visit ... or code;
#   plot    the figure-wide settings: title, axes, colours, theme, legend,
#           size;
#   layers  what is drawn, in order: a KM curve, censor marks, lines,
#           points, error bars, bars, reference lines, text; panels below
#           (number at risk, n); any ggplot2 geom by name; or code.
#
# A template (tfl_fig_template()) fills the four parts at once for a kind
# of figure (KM with the number at risk, mean over time, waterfall ...),
# after which each piece is edited on its own.  tfl_fig_parts() describes
# every piece and its fields; tfl_fig_design_code() writes the script.

# ---- the pieces ------------------------------------------------------------

# a field of a piece
.ff <- function(field, kind, label, default = NA, choices = NA, help = "",
                required = FALSE, of = NA) {
  data.frame(field = field, kind = kind, label = label,
             default = as.character(default),
             choices = if (length(choices) > 1L || !is.na(choices[1L]))
               paste(choices, collapse = " | ") else NA_character_,
             help = help, required = required, of = of,
             stringsAsFactors = FALSE)
}

.fig_units <- c(days = 1, weeks = 7, months = 30.4375, years = 365.25)
.fig_legends <- c("none", "right", "bottom", "top", "inside", "inside_tl",
                  "inside_br", "inside_bl")

# every piece: its section, label, help and fields
# every piece: built once, then kept (see .fig_cached()); a field lookup
# asks for it many times a design
.fig_pieces <- function() .fig_cached("pieces", .fig_pieces_build)

.fig_pieces_build <- function() {
  pal <- names(tfl_fig_palettes())
  shapes <- names(pp_shape_names)
  lty <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  c(list(
    # ---- data
    read = list(section = "data", label = "Read a dataset",
      help = "The dataset the figure starts from: `df`.",
      fields = rbind(.ff("dataset", "dataset", "Dataset", "ADSL", required = TRUE))),
    join = list(section = "data", label = "Join variables",
      help = "Variables of another dataset (e.g. ADSL's treatment, ADRS's best response), one row a subject.",
      fields = rbind(
        .ff("dataset", "dataset", "Dataset", "ADSL", required = TRUE),
        .ff("where", "expr", "Its rows (R)", help = "e.g. PARAMCD == \"BOR\""),
        .ff("vars", "variables", "Variables", required = TRUE, of = "dataset",
            help = "NAME = VAR renames, e.g. BOR = AVALC."),
        .ff("by", "variable", "By", "USUBJID"))),
    param = list(section = "data", label = "Keep a parameter",
      help = "The rows of one (or more) PARAMCD.",
      fields = rbind(
        .ff("value", "param", "PARAMCD", required = TRUE),
        .ff("variable", "variable", "Variable", "PARAMCD"))),
    flag = list(section = "data", label = "Keep an analysis set",
      help = "The rows whose flag is \"Y\" (FASFL, SAFFL, ANL01FL ...).",
      fields = rbind(
        .ff("variable", "flag", "Flag", "SAFFL", required = TRUE),
        .ff("value", "text", "Value", "Y"))),
    filter = list(section = "data", label = "Keep rows (condition)",
      help = "Any condition, in R.",
      fields = rbind(.ff("expr", "expr", "Condition (R)", required = TRUE,
                         help = "e.g. AVISITN > 0 & !is.na(AVAL)"))),
    derive = list(section = "data", label = "Derive a variable",
      help = "A new (or changed) variable, in R.",
      fields = rbind(
        .ff("variable", "text", "Variable", required = TRUE),
        .ff("expr", "expr", "Value (R)", required = TRUE, help = "e.g. AVAL / 7"))),
    time_unit = list(section = "data", label = "Change a time's unit",
      help = "A time in days, shown in days (as it is), weeks, months or years.",
      fields = rbind(
        .ff("variable", "variable", "Time variable", "AVAL", required = TRUE),
        .ff("unit", "choice", "Unit", "months", names(.fig_units)))),
    levels = list(section = "data", label = "Order a variable's values",
      help = "The order of groups or visits on the axis and in the legend: by another variable (AVISIT by AVISITN), or listed.",
      fields = rbind(
        .ff("variable", "variable", "Variable", required = TRUE),
        .ff("order_by", "variable", "Order by", help = "e.g. AVISITN"),
        .ff("levels", "text", "Values, in order", help = "| between them"),
        .ff("labels", "text", "Their labels", help = "| between them"))),
    rank = list(section = "data", label = "Rank rows",
      help = "A row number after sorting, e.g. the bars of a waterfall.",
      fields = rbind(
        .ff("by", "variable", "Sort by", "AVAL", required = TRUE),
        .ff("descending", "logical", "Descending", "TRUE"),
        .ff("variable", "text", "New variable", "INDEX"))),
    data_code = list(section = "data", label = "R code",
      help = "What no step does: code that changes `df` (the datasets are there by their lower-case names).",
      fields = rbind(.ff("code", "code", "Code", required = TRUE))),
    # ---- stats
    survfit = list(section = "stats", label = "Kaplan-Meier fit",
      help = "survfit2(Surv(time, censor == 0) ~ group).",
      fields = rbind(
        .ff("name", "text", "Name", "fit"),
        .ff("time", "variable", "Time", "AVAL", required = TRUE),
        .ff("censor", "variable", "Censor (1 = censored)", "CNSR", required = TRUE),
        .ff("by", "variable", "Group", help = "Empty = one curve"),
        .ff("conf_type", "choice", "Confidence interval", "log",
            c("log", "log-log", "plain")))),
    summary = list(section = "stats", label = "Summary statistics",
      help = "n, mean, SD, SE and an interval (lo, hi) of a value, by group and visit.",
      fields = rbind(
        .ff("name", "text", "Name", "sm"),
        .ff("value", "variable", "Value", "AVAL", required = TRUE),
        .ff("by", "variables", "By", required = TRUE),
        .ff("interval", "choice", "Interval (lo, hi)", "se", c("se", "sd", "ci")),
        .ff("positive", "logical", "Lower bound only above 0", "FALSE",
            help = "For a log axis: lo is left blank where it would be <= 0."))),
    summary_by = list(section = "stats", label = "Summary by group",
      help = "n, mean, SD, SE and an interval of a value by group only (one row a group): a mean marker per box ...",
      fields = rbind(
        .ff("name", "text", "Name", "sg"),
        .ff("value", "variable", "Value", "AVAL", required = TRUE),
        .ff("by", "variables", "By", required = TRUE),
        .ff("interval", "choice", "Interval (lo, hi)", "se", c("se", "sd", "ci")))),
    rate = list(section = "stats", label = "Rate with 95% CI",
      help = "Responders / n by group with the exact binomial interval: rate, lcl, ucl (%), and a label 'rate (x/n)'.",
      fields = rbind(
        .ff("name", "text", "Name", "rt"),
        .ff("category", "variable", "Category", "AVALC", required = TRUE),
        .ff("responders", "text", "Counted as response", "CR, PR", required = TRUE),
        .ff("by", "variables", "By", required = TRUE))),
    count = list(section = "stats", label = "Counts and percents",
      help = "n and % of each category within each group: n, pct, and a label 'pct%'.",
      fields = rbind(
        .ff("name", "text", "Name", "ct"),
        .ff("category", "variable", "Category", "AVALC", required = TRUE),
        .ff("by", "variables", "By", required = TRUE),
        .ff("levels", "text", "Categories, in order", help = "| between them; others follow"))),
    subset = list(section = "stats", label = "Another dataset (as an object)",
      help = "Rows of another dataset (or of `df`), by name, for layers that draw them: the assessments of a swimmer plot, the ongoing subjects ...",
      fields = rbind(
        .ff("name", "text", "Name", required = TRUE),
        .ff("dataset", "dataset", "Dataset", required = TRUE, help = "or df"),
        .ff("where", "expr", "Its rows (R)"),
        .ff("from_df", "variables", "Variables taken from df", help = "Joined by the key: the y position of its subject, a colour ..."),
        .ff("by", "variable", "Key", "USUBJID"))),
    stats_code = list(section = "stats", label = "R code",
      help = "Code that computes what the layers draw, from `df`.",
      fields = rbind(.ff("code", "code", "Code", required = TRUE))),
    # ---- layers
    km_curve = list(section = "layers", label = "KM curves", base = TRUE,
      help = "The Kaplan-Meier curves (ggsurvfit); the first layer.",
      fields = rbind(
        .ff("fit", "object", "Fit", "fit"),
        .ff("linewidth", "number", "Line width", 0.3))),
    km_ci = list(section = "layers", label = "KM confidence bands",
      help = "The curves' confidence intervals.",
      fields = rbind(.ff("alpha", "number", "Transparency", 0.2))),
    censor_mark = list(section = "layers", label = "Censor marks",
      help = "A mark where a subject is censored.",
      fields = rbind(
        .ff("shape", "choice", "Shape", "x", shapes),
        .ff("size", "number", "Size", 3),
        .ff("stroke", "number", "Stroke", 0.6))),
    ref_label = list(section = "layers", label = "Reference line labels",
      help = "Labels right of the panel at reference lines, e.g. 20% and -30%.",
      fields = rbind(
        .ff("y", "values", "At y", required = TRUE, help = "Several: 20, -30"),
        .ff("label", "text", "Label", "{y}", help = "{y} = the value, e.g. {y}%"),
        .ff("size", "number", "Size", 3.5))),
    risk_table = list(section = "layers", label = "Number at risk (panel)",
      panel = TRUE,
      help = "The number at risk below the curves, at the x axis's breaks.",
      fields = rbind(
        .ff("fit", "object", "Fit", "fit"),
        .ff("title", "text", "Title", "Number of Patients at Risk"),
        .ff("size", "number", "Text size", 3),
        .ff("height", "number", "Height (share)", 0.167))),
    n_table = list(section = "layers", label = "n by visit (panel)",
      panel = TRUE, help = "The n of each group at each x, below the plot.",
      fields = rbind(
        .ff("data", "object", "Data", "sm"),
        .ff("x", "variable", "X", required = TRUE, of = "data"),
        .ff("group", "variable", "Group", required = TRUE, of = "data"),
        .ff("label", "variable", "Value", "n", of = "data"),
        .ff("title", "text", "Title", "n"),
        .ff("height", "number", "Height (share)", 0.18))),
    geom = list(section = "layers", label = "Any ggplot2 layer",
      help = "Any geom or stat by name, with its aesthetics and settings.",
      fields = rbind(
        .ff("geom", "text", "Function", "geom_point", required = TRUE,
            help = "e.g. geom_area, stat_ecdf, ggrepel::geom_text_repel"),
        .ff("data", "object", "Data", "df"),
        .ff("aes", "named", "Aesthetics", help = "x = AVAL | y = CHG | colour = TRT01A"),
        .ff("params", "named", "Settings (R)", help = "alpha = 0.3 | size = 2"))),
    layer_code = list(section = "layers", label = "R code",
      help = "Code that adds to the plot `p` (e.g. p <- p + annotate(...)).",
      fields = rbind(.ff("code", "code", "Code", required = TRUE))),
    call = list(section = "layers", label = "Any function (call)",
      help = "p <- p + fn(data, aes(...), args...): any ggplot2 or extension function, checked against its own arguments.",
      fields = rbind(
        .ff("fn", "text", "Function", required = TRUE, help = "e.g. geom_label, add_quantile, or pkg::fn"),
        .ff("package", "text", "Package", help = "Defaults to the search order ggplot2 -> ggsurvfit -> patchwork."),
        .ff("data", "object", "Data", "df"),
        .ff("aes", "raw", "Aesthetics (map)", help = "x: AVAL | label: n -- values are expressions."),
        .ff("pos", "raw", "Positional arguments (sequence)"),
        .ff("args", "raw", "Named arguments (map)"),
        .ff("base", "logical", "First layer (p <- fn(...))", "FALSE"))),
    figure = list(section = "layers", label = "Whole figure (type)",
      help = "A figure type not yet in parts: its script as tfl_fig_<type>() writes it; the other parts are then unused.",
      fields = rbind(
        .ff("type", "choice", "Type", required = TRUE),
        .ff("style", "text", "Style"),
        .ff("args", "named", "Arguments"))),
    # ---- plot.add (a call, without `layer`/`base`; see `call` above)
    plot_add = list(section = "plot", label = "Add a call (figure-wide)",
      help = "p <- p + fn(...), after the figure's settings (scales, axes, labs, theme, legend) and before any panels: theme(), scale_*, coord_*, facet_*, guides, labs ...",
      fields = rbind(
        .ff("fn", "text", "Function", required = TRUE, help = "e.g. theme, facet_grid, scale_y_log10, or pkg::fn"),
        .ff("package", "text", "Package", help = "Defaults to the search order ggplot2 -> ggsurvfit -> patchwork."),
        .ff("data", "object", "Data"),
        .ff("aes", "raw", "Aesthetics (map)"),
        .ff("pos", "raw", "Positional arguments (sequence)"),
        .ff("args", "raw", "Named arguments (map)")))
  ), .fig_geom_pieces())
}

# the figure-wide settings
.fig_plot_fields <- function() .fig_cached("plot_fields", .fig_plot_fields_build)

.fig_plot_fields_build <- function() {
  rbind(
    .ff("title", "text", "Figure title"),
    .ff("x_label", "text", "X label"),
    .ff("y_label", "text", "Y label"),
    .ff("x_min", "number", "X min"),
    .ff("x_max", "number", "X max"),
    .ff("x_by", "number", "X step"),
    .ff("x_text", "logical", "X axis text", "TRUE"),
    .ff("y_min", "number", "Y min"),
    .ff("y_max", "number", "Y max"),
    .ff("y_by", "number", "Y step"),
    .ff("x_log", "logical", "Log X axis", "FALSE"),
    .ff("y_log", "logical", "Log Y axis", "FALSE"),
    .ff("equal", "logical", "Equal X and Y scales", "FALSE",
        help = "The same range on both axes, e.g. baseline vs post-baseline; needs X and Y min / max."),
    .ff("facet_by", "variable", "One panel per", help = "A variable: one panel for each of its values."),
    .ff("colour_by", "variable", "Colours by", help = "The variable the palette's colours go to (groups, responses)."),
    .ff("palette", "choice", "Palette", "treatment", names(tfl_fig_palettes())),
    .ff("dodge", "number", "Dodge width", 0.3),
    .ff("theme", "choice", "Theme", "boxed", pp_themes),
    .ff("base_size", "number", "Base font size", 10),
    .ff("legend", "choice", "Legend", "bottom", .fig_legends),
    .ff("width", "number", "Width", 7.5),
    .ff("height", "number", "Height", 4.5),
    .ff("units", "choice", "Units", "in", c("in", "cm", "px")),
    .ff("dpi", "number", "DPI", 300),
    .ff("add", "pieces", "Additional calls (plot.add)", of = "plot_add",
        help = "Any ggplot2/extension call, after the figure's settings and before any panels."))
}

#' The pieces of a figure design
#'
#' Every piece a figure design ([tfl_fig_design()]) is made of -- the data
#' steps, the statistics, the figure-wide settings and the layers -- with
#' its fields: what a GUI lists, adds and edits one by one.
#'
#' @return A data frame: `section` (`data`, `stats`, `plot`, `layers`),
#'   `piece`, `piece_label`, `piece_help`, `field`, `kind` (`dataset`,
#'   `variable`, `variables`, `flag`, `param`, `object` (the data or a
#'   statistic by name), `choice`, `number`, `logical`, `text`, `expr` (R),
#'   `code`, `named` (`name = value | ...`)), `label`, `default`,
#'   `choices` (`|` between them), `help`, `required`, `of`.
#' @export
tfl_fig_parts <- function() .fig_cached("parts", .fig_parts_build)

.fig_parts_build <- function() {
  p <- .fig_pieces()
  rows <- lapply(names(p), function(k) {
    f <- p[[k]]$fields
    if (k == "figure") {
      f$choices[f$field == "type"] <- paste(tfl_fig_types_implemented(), collapse = " | ")
    }
    cbind(section = p[[k]]$section, piece = k, piece_label = p[[k]]$label,
          piece_help = p[[k]]$help %||% "", f, stringsAsFactors = FALSE)
  })
  pl <- cbind(section = "plot", piece = "plot", piece_label = "Figure",
              piece_help = "The figure-wide settings.", .fig_plot_fields(),
              stringsAsFactors = FALSE)
  out <- do.call(rbind, c(rows, list(pl)))
  rownames(out) <- NULL
  out
}

tfl_fig_types_implemented <- function() {
  cat <- tfl_fig_catalog()
  unique(cat$type[cat$status == "implemented"])
}

# ---- the design -------------------------------------------------------------

#' A figure design
#'
#' A figure as four parts (see [tfl_fig_parts()] for every piece):
#'
#' * `data`: the steps from ADaM to the plot's data `df` -- each a list with
#'   `step` (`read`, `join`, `param`, `flag`, `filter`, `derive`,
#'   `time_unit`, `levels`, `rank`, `data_code`) and its fields;
#' * `stats`: what is computed from `df` (`survfit`, `summary`,
#'   `stats_code`), each with its `name`;
#' * `plot`: the figure-wide settings (title, axes, colours, theme, legend,
#'   size), plus `add`: a list of `call`s (below) written after the
#'   figure's settings and before any panels -- for `theme()`, `scale_*`,
#'   `coord_*`, `facet_*`, `labs`, `guides` and the like;
#' * `layers`: what is drawn, in order -- each a list with `layer`
#'   (`km_curve`, `km_ci`, `censor_mark`, `risk_table`, `n_table`,
#'   `ref_label`; any layer of the geom catalog -- `line`, `point`,
#'   `errorbar`, `col`, `text`, `hline`, `ribbon`, `boxplot` ... see
#'   [tfl_fig_add_layer()]; `geom` (any function by name), `call` (any
#'   function, by `fn`, `package`, `data`, `aes`, `pos`, `args` -- checked
#'   against its own arguments; see [tfl_fig_r()] for raw R in `aes`/`args`),
#'   `layer_code`, or `figure`: a figure type's whole script) and its fields.
#'
#' [tfl_fig_template()] makes one for a kind of figure; it is kept as one
#' YAML file per figure (`tfl_write_fig_design()` / `tfl_read_fig_design()`)
#' and `tfl_fig_design_code()` writes its script.
#'
#' ```yaml
#' template: km_risk_table
#' data:
#' - {step: read, dataset: ADTTE}
#' - {step: param, value: OS}
#' - {step: flag, variable: FASFL}
#' - {step: time_unit, variable: AVAL, unit: months}
#' stats:
#' - {step: survfit, name: fit, time: AVAL, censor: CNSR, by: TRT01A}
#' plot: {x_label: Time (Months), y_label: Survival Probability, colour_by: TRT01A,
#'   legend: inside, x_min: 0, y_min: 0, y_max: 1, y_by: 0.2}
#' layers:
#' - {layer: km_curve}
#' - {layer: censor_mark, shape: x}
#' - {layer: hline, yintercept: 0.5, linetype: twodash}
#' - {layer: risk_table}
#' ```
#'
#' A composed figure (two figures, each with its own data):
#'
#' ```yaml
#' plot: {width: 10, height: 4.5}
#' plots:
#'   km:   {data: [...], stats: [...], plot: {...}, layers: [...]}
#'   box:  {data: [...], plot: {...}, layers: [...]}
#' compose:
#'   layout: km | box
#'   add:
#'   - {fn: plot_layout, args: {widths: [3, 2]}}
#'   - {fn: plot_annotation, args: {tag_levels: A}}
#'   - {op: "&", fn: theme, args: {legend.position: bottom}}
#' ```
#'
#' @param data,stats,layers Lists of pieces (each a named list).
#' @param plot A named list of the figure-wide settings.
#' @param template The template it was made from (a note).
#' @param plots A composed figure: a named list of figure designs (each
#'   with its own `data`, `stats`, `plot`, `layers`), put together by
#'   patchwork as `compose` says. The design's own `plot` then holds only
#'   the saved size (`width`, `height`, `dpi`, `units`), and it has no
#'   `data`, `stats` or `layers` of its own.
#' @param compose With `plots`: `layout`, an expression of the plots' names
#'   with `|` (side by side), `/` (stacked), `+`, `-` and brackets (default:
#'   all side by side); `add`, a list of calls written after it, each with
#'   `op` `"+"` (the default) or `"&"` (every figure) -- `plot_layout`,
#'   `plot_annotation`, `theme` ... A figure with panels below it (the
#'   number at risk, n) or with ggsurvfit's `add_risktable` is kept as one
#'   figure (`wrap_elements()`).
#' @param ggplot2_version The ggplot2 the script is written for, `"3.5"` or
#'   `"4.0"` (see [tfl_fig_compat()]); `NULL`: the design's
#'   `ggplot2_version`, else the option `tflspec.ggplot2_version`, else the
#'   installed ggplot2's. Set (anywhere but the installed version), the
#'   script's header says `# Written for ggplot2 X`.
#' @param design A `tfl_fig_design`.
#' @param path A `.yml` file.
#' @param plot_id The figure's ID: the PNG's name.
#' @return `tfl_fig_design()` and `tfl_read_fig_design()`: a
#'   `tfl_fig_design`; `tfl_write_fig_design()`: `path`, invisibly;
#'   `tfl_fig_design_code()`: the script (a `tfl_code`).
#' @export
tfl_fig_design <- function(data = list(), stats = list(), plot = list(),
                           layers = list(), template = NULL, ggplot2_version = NULL,
                           plots = NULL, compose = NULL) {
  clean <- function(x) lapply(x, function(p) as.list(p)[!vapply(p, is.null, logical(1))])
  structure(list(template = template, data = clean(data),
                 stats = clean(stats),
                 plot = as.list(plot)[!vapply(plot, is.null, logical(1))],
                 layers = clean(layers),
                 ggplot2_version = if (!is.null(ggplot2_version)) .fig_norm_version(ggplot2_version),
                 plots = if (length(plots)) lapply(plots, .fig_as_design),
                 compose = if (length(compose)) as.list(compose)),
            class = "tfl_fig_design")
}

#' @export
print.tfl_fig_design <- function(x, ...) {
  cat(yaml::as.yaml(.fig_design_list(x)))
  invisible(x)
}

.fig_design_list <- function(design) {
  x <- unclass(design)
  x <- x[intersect(c("template", "ggplot2_version", "data", "stats", "plot", "layers",
                     "plots", "compose"), names(x))]
  if (length(x$plots)) x$plots <- lapply(x$plots, .fig_design_list)
  x[!vapply(x, function(v) is.null(v) || !length(v), logical(1))]
}

#' @rdname tfl_fig_design
#' @export
tfl_write_fig_design <- function(design, path) {
  writeLines(enc2utf8(yaml::as.yaml(.fig_design_list(design))), path,
             useBytes = TRUE)
  invisible(path)
}

#' @rdname tfl_fig_design
#' @export
tfl_read_fig_design <- function(path) {
  x <- yaml::read_yaml(path, handlers = list(r = tfl_fig_r))
  .fig_design_from_list(x)
}

.fig_design_from_list <- function(x) {
  # a design of type / style / args (tflspec 0.0.12): one whole-figure layer
  if (!is.null(x$type)) {
    return(tfl_fig_design(layers = list(list(
      layer = "figure", type = x$type, style = x$style, args = x$args %||% list()))))
  }
  tfl_fig_design(x$data %||% list(), x$stats %||% list(), x$plot %||% list(),
                 x$layers %||% list(), x$template, x$ggplot2_version,
                 x$plots, x$compose)
}

# ---- the code ---------------------------------------------------------------

# a piece's field, else its default ("" / NA = not set)
.fv <- function(piece, field, kind) {
  v <- piece[[field]]
  if (!is.null(v) && length(v) && !(length(v) == 1L && (is.na(v) || identical(v, "")))) {
    return(v)
  }
  defs <- .fig_pieces()[[kind]]$fields
  d <- defs$default[defs$field == field]
  if (!length(d) || is.na(d)) NULL else d
}
.pv <- function(plot, field) {
  v <- plot[[field]]
  if (!is.null(v) && length(v) && !(length(v) == 1L && (is.na(v) || identical(v, "")))) {
    return(v)
  }
  f <- .fig_plot_fields()
  d <- f$default[f$field == field]
  if (!length(d) || is.na(d)) NULL else d
}
.lgl <- function(v) isTRUE(as.logical(v))
.split_vals <- function(v) {
  if (is.null(v)) return(character())
  v <- unlist(strsplit(as.character(v), "\\s*[|,]\\s*"))
  trimws(v[nzchar(trimws(v))])
}
# "a = 1 | b = x" or a named list -> c(a = "1", b = "x")
.named <- function(v) {
  if (is.null(v)) return(character())
  if (is.list(v) || !is.null(names(v))) {
    v <- unlist(v)
    return(stats::setNames(as.character(v), names(v)))
  }
  parts <- .split_vals(gsub(",", "\u0001", v))
  parts <- gsub("\u0001", ",", parts)
  parts <- parts[grepl("=", parts, fixed = TRUE)]
  stats::setNames(trimws(sub("^[^=]*=", "", parts)), trimws(sub("=.*$", "", parts)))
}
.args_code <- function(x) {
  x <- x[!vapply(x, is.null, logical(1))]
  if (!length(x)) return("")
  paste(paste(names(x), "=", unlist(x)), collapse = ", ")
}

# the data part: `df`, and the datasets it reads
.fig_data_code <- function(steps) {
  out <- character()
  pipe <- character()
  reads <- character()
  flush <- function() {
    if (length(pipe)) {
      out <<- c(out, paste0("df <- ", paste(pipe, collapse = " |>\n  ")), "")
      pipe <<- character()
    }
  }
  for (s in steps) {
    k <- s$step
    v <- function(f) .fv(s, f, k)
    add <- function(x) {
      # filter(a) |> filter(b) is filter(a, b)
      last <- if (length(pipe)) pipe[[length(pipe)]] else ""
      if (length(x) == 1L && startsWith(x, "filter(") && startsWith(last, "filter(") &&
          !grepl("\n", last, fixed = TRUE) && !grepl("\n", x, fixed = TRUE)) {
        pipe[[length(pipe)]] <<- paste0(substr(last, 1L, nchar(last) - 1L), ", ",
                                        substring(x, 8L))
        return(invisible())
      }
      pipe <<- c(if (!length(pipe)) "df" else pipe, x)
    }
    switch(k,
      read = {
        flush()
        ds <- toupper(v("dataset"))
        reads <- c(reads, ds)
        pipe <- pp_ds_name(ds)
      },
      join = {
        ds <- toupper(v("dataset"))
        reads <- c(reads, ds)
        vars <- .split_vals(v("vars"))
        by <- v("by")
        w <- v("where")
        sel <- paste(c(by, vars), collapse = ", ")
        add(paste0("left_join(\n    ", pp_ds_name(ds),
                   if (!is.null(w)) paste0(" |> filter(", w, ")"),
                   " |> select(", sel, "),\n    by = ", q(by), "\n  )"))
      },
      param = {
        vals <- .split_vals(v("value"))
        add(if (length(vals) == 1L) sprintf("filter(%s == %s)", v("variable"), q(vals))
            else sprintf("filter(%s %%in%% %s)", v("variable"), vec_code(vals)))
      },
      flag = add(sprintf("filter(%s == %s)", v("variable"), q(v("value")))),
      filter = add(sprintf("filter(%s)", v("expr"))),
      derive = add(sprintf("mutate(%s = %s)", v("variable"), v("expr"))),
      # days: the time stays as it is (ADTTE's AVAL is in days)
      time_unit = if (!identical(v("unit"), "days")) add(sprintf("# days -> %s
  mutate(%s = %s / %s)", v("unit"), v("variable"),
                              v("variable"), format(.fig_units[[v("unit")]]))),
      levels = {
        var <- v("variable")
        lv <- .split_vals(v("levels"))
        lb <- .split_vals(v("labels"))
        ob <- v("order_by")
        add(if (length(lv)) {
          sprintf("mutate(%s = factor(%s, levels = %s%s))", var, var, vec_code(lv),
                  if (length(lb) == length(lv)) paste0(", labels = ", vec_code(lb)) else "")
        } else if (!is.null(ob)) {
          sprintf("mutate(%s = reorder(factor(%s), %s))", var, var, ob)
        } else sprintf("mutate(%s = factor(%s))", var, var))
      },
      rank = {
        by <- v("by")
        add(c(sprintf("arrange(%s)", if (.lgl(v("descending"))) sprintf("desc(%s)", by) else by),
              sprintf("mutate(%s = row_number())", v("variable"))))
      },
      data_code = {
        flush()
        out <- c(out, "# your code", s$code, "")
      },
      stop("Unknown data step: ", k, call. = FALSE))
  }
  flush()
  list(code = out, reads = unique(reads))
}

.fig_stats_code <- function(steps) {
  out <- character()
  libs <- character()
  for (s in steps) {
    k <- s$step
    v <- function(f) .fv(s, f, k)
    switch(k,
      survfit = {
        libs <- c(libs, "ggsurvfit")
        ct <- v("conf_type")
        out <- c(out, sprintf("%s <- survfit2(Surv(%s, %s == 0) ~ %s, data = df%s)",
                              v("name"), v("time"), v("censor"), v("by") %||% "1",
                              if (!identical(ct, "log")) paste0(", conf.type = ", q(ct)) else ""),
                 "")
      },
      summary = {
        by <- .split_vals(v("by"))
        val <- v("value")
        iv <- v("interval")
        lohi <- switch(iv,
          se = c("mean - se", "mean + se"),
          sd = c("mean - sd", "mean + sd"),
          ci = c("mean - qt(0.975, n - 1) * se", "mean + qt(0.975, n - 1) * se"))
        out <- c(out, paste0(v("name"), " <- df |>\n",
          sprintf("  filter(!is.na(%s)) |>\n", val),
          sprintf("  group_by(%s) |>\n", paste(by, collapse = ", ")),
          sprintf("  summarise(n = n(), mean = mean(%s), sd = sd(%s), .groups = \"drop\") |>\n", val, val),
          sprintf("  mutate(se = sd / sqrt(n), lo = %s, hi = %s)", lohi[1], lohi[2]),
          if (.lgl(v("positive"))) " |>\n  mutate(lo = ifelse(lo > 0, lo, NA))   # log axis: no lower bar at or below 0"), "")
      },
      summary_by = {
        by <- .split_vals(v("by"))
        val <- v("value")
        lohi <- switch(v("interval"),
          se = c("mean - se", "mean + se"),
          sd = c("mean - sd", "mean + sd"),
          ci = c("mean - qt(0.975, n - 1) * se", "mean + qt(0.975, n - 1) * se"))
        out <- c(out, paste0(v("name"), " <- df |>\n",
          sprintf("  filter(!is.na(%s)) |>\n", val),
          sprintf("  group_by(%s) |>\n", paste(by, collapse = ", ")),
          sprintf("  summarise(n = n(), mean = mean(%s), sd = sd(%s), .groups = \"drop\") |>\n", val, val),
          sprintf("  mutate(se = sd / sqrt(n), lo = %s, hi = %s)", lohi[1], lohi[2])), "")
      },
      rate = {
        by <- .split_vals(v("by"))
        resp <- .split_vals(v("responders"))
        out <- c(out, paste0(v("name"), " <- df |>\n",
          sprintf("  group_by(%s) |>\n", paste(by, collapse = ", ")),
          sprintf("  summarise(n = n(), x = sum(%s %%in%% %s), .groups = \"drop\") |>\n", v("category"), vec_code(resp)),
          "  mutate(\n",
          "    rate  = 100 * x / n,\n",
          "    lcl   = 100 * mapply(function(x, n) binom.test(x, n)$conf.int[1], x, n),\n",
          "    ucl   = 100 * mapply(function(x, n) binom.test(x, n)$conf.int[2], x, n),\n",
          "    label = sprintf(\"%.1f%%\\n(%d/%d)\", rate, x, n)\n",
          "  )"), "")
      },
      count = {
        by <- .split_vals(v("by"))
        cat <- v("category")
        lv <- .split_vals(v("levels"))
        out <- c(out, paste0(v("name"), " <- df |>\n",
          sprintf("  count(%s, %s) |>\n", paste(by, collapse = ", "), cat),
          sprintf("  group_by(%s) |>\n", paste(by, collapse = ", ")),
          "  mutate(pct = 100 * n / sum(n), label = sprintf(\"%.0f%%\", pct)) |>\n",
          "  ungroup()",
          if (length(lv)) sprintf(" |>\n  mutate(%s = factor(%s, levels = unique(c(intersect(%s, %s), sort(%s)))))",
                                  cat, cat, vec_code(lv), cat, cat)), "")
      },
      subset = {
        ds <- v("dataset")
        src <- if (toupper(ds) == "DF") "df" else pp_ds_name(toupper(ds))
        from <- .split_vals(v("from_df"))
        w <- v("where")
        out <- c(out, paste0(v("name"), " <- ", src,
          if (!is.null(w)) sprintf(" |>\n  filter(%s)", w),
          if (length(from)) sprintf(" |>\n  inner_join(df |> select(%s), by = %s)",
                                    paste(c(v("by"), from), collapse = ", "), q(v("by")))), "")
      },
      stats_code = out <- c(out, "# your code", s$code, ""),
      stop("Unknown statistics step: ", k, call. = FALSE))
  }
  list(code = out, libs = libs)
}

# aes(...) of a layer: its variable fields (NULL ones left out)
.aes <- function(...) {
  a <- list(...)
  a <- a[!vapply(a, is.null, logical(1))]
  if (!length(a)) return(NULL)
  sprintf("aes(%s)", paste(names(a), "=", unlist(a), collapse = ", "))
}

.fig_layer_code <- function(l, i, plot, ggplot2_version = NULL) {
  k <- l$layer
  v <- function(f) .fv(l, f, k)
  dodge <- if (.lgl(v("dodge"))) "position = pd"
  num <- function(f) v(f)
  term <- function(fn, ...) {
    a <- c(...)
    a <- a[!vapply(a, is.null, logical(1)) & nzchar(a)]
    sprintf("%s(%s)", fn, paste(a, collapse = ", "))
  }
  kv <- function(name, val) if (!is.null(val)) paste(name, "=", val)
  lbl <- function(title) paste0("# ---- layer ", i, ": ", title, " ----")
  switch(k,
    km_curve = list(base = sprintf("p <- ggsurvfit(%s, linewidth = %s%s)", v("fit"), num("linewidth"),
                                   if (is.null(plot$colour_by)) ", colour = pal[[1]]" else "")),
    km_ci = list(code = c(lbl("KM confidence bands"),
      sprintf("p <- p + add_confidence_interval(alpha = %s%s)", num("alpha"),
              if (is.null(plot$colour_by)) ", fill = pal[[1]]" else ""))),
    censor_mark = list(code = c(lbl("censor marks"),
      sprintf("p <- p + add_censor_mark(shape = %s, size = %s, stroke = %s%s)",
              pp_shape_code(v("shape"), "x"), num("size"), num("stroke"),
              if (is.null(plot$colour_by)) ", colour = pal[[1]]" else ""))),
    ref_label = {
      ys <- .split_vals(v("y"))
      yc <- if (length(ys) == 1L) ys else sprintf("c(%s)", paste(ys, collapse = ", "))
      parts <- strsplit(v("label"), "{y}", fixed = TRUE)[[1L]]
      if (endsWith(v("label"), "{y}")) parts <- c(parts, "")
      lab <- if (length(parts) > 1L) {
        sprintf("paste0(%s)", paste(q(parts), collapse = sprintf(", %s, ", yc)))
      } else q(v("label"))
      list(code = c(lbl("reference line labels"),
        sprintf("p <- p + annotate(\"text\", x = Inf, y = %s, label = %s, hjust = -0.3, size = %s)",
                yc, lab, num("size"))),
        # room right of the panel, after the theme (which would reset it)
        after = "p <- p + theme(plot.margin = margin(5.5, 50, 5.5, 5.5))   # room for the labels",
        clip_off = TRUE)
    },
    risk_table = {
      fit <- v("fit")
      single <- is.null(plot$colour_by)
      list(panel = list(name = "p_risk", height = as.numeric(v("height"))), libs = "patchwork",
        code = c(lbl("number at risk (a panel below)"),
          sprintf("sr <- summary(%s, times = x_breaks, extend = TRUE)", fit),
          paste0("risk_df <- data.frame(\n",
                 "  time   = sr$time,\n",
                 if (single) "  strata = names(pal)[1],\n"
                 else "  strata = sub(\"^[^=]*=\", \"\", as.character(sr$strata)),\n",
                 "  n_risk = sr$n.risk\n)"),
          "risk_df$strata <- factor(risk_df$strata, levels = rev(names(pal)))",
          plus_code("p_risk", list(
            "ggplot(risk_df, aes(x = time, y = strata, label = n_risk, colour = strata))",
            sprintf("geom_text(size = %s)", num("size")),
            'scale_colour_manual(values = pal, guide = "none")',
            "scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0.02, 0.02)))",
            'coord_cartesian(xlim = range(x_breaks), clip = "off")',
            sprintf("labs(title = %s, x = NULL, y = NULL)", q(v("title"))),
            sprintf("theme_void(base_size = %s)", .pv(plot, "base_size")),
            paste0("theme(\n",
                   "  plot.title          = element_text(hjust = 0, size = rel(0.9)),\n",
                   "  plot.title.position = \"plot\",\n",
                   "  axis.text.y         = element_text(hjust = 1, margin = margin(r = 5))\n)")))))
    },
    n_table = list(panel = list(name = paste0("p_n", i), height = as.numeric(v("height"))), libs = "patchwork",
      code = c(lbl("n (a panel below)"),
        plus_code(paste0("p_n", i), list(
          sprintf("ggplot(%s, aes(x = %s, y = factor(%s, levels = rev(names(pal))), label = %s, colour = %s))",
                  v("data"), v("x"), v("group"), v("label"), v("group")),
          "geom_text(size = 3)",
          'scale_colour_manual(values = pal, guide = "none")',
          sprintf("labs(title = %s, x = NULL, y = NULL)", q(v("title"))),
          sprintf("theme_void(base_size = %s)", .pv(plot, "base_size")),
          "theme(axis.text.y = element_text(hjust = 1, margin = margin(r = 5)), plot.title = element_text(size = rel(0.9)))")))),
    geom = {
      a <- .named(v("aes"))
      pr <- .named(v("params"))
      list(code = c(lbl(v("geom")), paste0("p <- p + ", term(v("geom"),
        kv("data", v("data")),
        if (length(a)) sprintf("aes(%s)", paste(names(a), "=", a, collapse = ", ")),
        if (length(pr)) paste(names(pr), "=", pr, collapse = ", ")))),
        libs = if (grepl("::", v("geom"), fixed = TRUE)) character() else NULL)
    },
    layer_code = list(code = c(lbl("your code"), l$code)),
    call = {
      res <- .fig_call_code(l, target = "p", plus = !isTRUE(l$base), ggplot2_version = ggplot2_version)
      out <- list(libs = res$libs, guard = res$guard, features = res$features)
      if (length(res$pkgs)) out$package <- res$pkgs
      if (isTRUE(l$base)) out$base <- res$line else out$code <- c(lbl(paste0("call: ", l$fn)), res$line)
      out
    },
    {
      g <- .fig_pieces()[[k]]$geom
      if (is.null(g)) stop("Unknown layer: ", k, call. = FALSE)
      .fig_geom_code(l, k, g, lbl(.fig_pieces()[[k]]$label))
    })
}

.fig_axis_code <- function(plot, has_risk, fit, clip = FALSE) {
  n <- function(f) { x <- .pv(plot, f); if (is.null(x)) NULL else as.numeric(x) }
  x_min <- n("x_min"); x_max <- n("x_max"); x_by <- n("x_by")
  y_min <- n("y_min"); y_max <- n("y_max"); y_by <- n("y_by")
  pre <- character()
  terms <- list()
  x_breaks <- FALSE
  if (!is.null(x_max) && !is.null(x_by)) {
    pre <- sprintf("x_breaks <- seq(%s, %s, by = %s)", x_min %||% 0, x_max, x_by)
    x_breaks <- TRUE
  } else if (has_risk) {
    pre <- if (!is.null(x_by)) {
      sprintf("x_breaks <- seq(%s, max(%s$time), by = %s)", x_min %||% 0, fit, x_by)
    } else sprintf("x_breaks <- pretty(c(%s, %s))", x_min %||% 0,
                   if (!is.null(x_max)) x_max else sprintf("max(%s$time)", fit))
    x_breaks <- TRUE
  }
  if (x_breaks) {
    terms <- c(terms, "scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0.02, 0.02)))")
  } else if (!is.null(x_by)) {
    terms <- c(terms, sprintf("scale_x_continuous(breaks = scales::breaks_width(%s))", x_by))
  }
  if (!is.null(y_by) && !is.null(y_min) && !is.null(y_max)) {
    terms <- c(terms, sprintf("scale_y_continuous(breaks = seq(%s, %s, by = %s))", y_min, y_max, y_by))
  } else if (!is.null(y_by)) {
    terms <- c(terms, sprintf("scale_y_continuous(breaks = scales::breaks_width(%s))", y_by))
  }
  xl <- if (x_breaks) "range(x_breaks)" else if (!is.null(x_min) && !is.null(x_max)) sprintf("c(%s, %s)", x_min, x_max)
  yl <- if (!is.null(y_min) && !is.null(y_max)) sprintf("c(%s, %s)", y_min, y_max)
  if (.lgl(.pv(plot, "x_log"))) terms <- c(terms, "scale_x_log10()")
  if (.lgl(.pv(plot, "y_log"))) terms <- c(terms, "scale_y_log10()")
  if (.lgl(.pv(plot, "equal")) && !is.null(xl)) {
    terms <- c(terms, sprintf("coord_equal(xlim = %s, ylim = %s)", xl, yl %||% xl))
  } else if (!is.null(xl) || !is.null(yl) || clip) {
    terms <- c(terms, sprintf("coord_cartesian(%s)", paste(c(
      if (!is.null(xl)) paste("xlim =", xl), if (!is.null(yl)) paste("ylim =", yl),
      if (clip) "clip = \"off\""), collapse = ", ")))
  }
  fb <- .pv(plot, "facet_by")
  if (!is.null(fb)) terms <- c(terms, sprintf("facet_wrap(vars(%s))", fb))
  list(pre = pre, terms = terms)
}

# `setup`: the program sources the study's figure setup
# (tfl_fig_setup_code()), whose tfl_colours() gives the palette
.fig_palette_code <- function(plot, setup = FALSE) {
  pname <- .pv(plot, "palette")
  pal <- tfl_fig_palettes()[[pname]]
  if (is.null(pal)) stop("Unknown palette: ", pname, call. = FALSE)
  by <- plot$colour_by
  if (!is.null(names(pal))) {
    return(c(sprintf("# the %s palette: a colour for each value", pname),
             if (setup) sprintf("pal <- tfl_colours(%s)", q(pname))
             else sprintf("pal <- %s", vec_code(pal))))
  }
  if (is.null(by)) {
    return(if (setup) sprintf("pal <- c(All = tfl_colours(%s)[[1]])", q(pname))
           else sprintf("pal <- c(All = %s)", q(pal[[1]])))
  }
  lv <- sprintf("levels(droplevels(factor(df$%s)))", by)
  c(sprintf("# the %s palette, a colour for each %s", pname, by),
    if (setup) sprintf("pal <- tfl_colours(%s, %s)", q(pname), lv) else c(
      sprintf("lv <- %s", lv),
      sprintf("pal <- setNames(%s[seq_along(lv)], lv)", vec_code(unname(pal)))))
}

# One figure's code, in parts: what to library(), the guard, the data
# (the data, statistics, palette, axes) and the figure (assembled as
# `name`); `patch`: the figure is itself a patchwork (panels below it);
# `risktable`: a ggsurvfit with add_risktable (a patchwork once built).
.fig_design_body <- function(design, gg, setup = FALSE, name = "fig") {
  plot <- design$plot
  d <- .fig_data_code(design$data)
  s <- .fig_stats_code(design$stats)
  layers <- design$layers
  kinds <- vapply(layers, function(l) l$layer %||% "", "")
  fit_of <- function() {
    r <- layers[kinds == "risk_table"]
    if (length(r)) .fv(r[[1L]], "fit", "risk_table") else "fit"
  }
  lc <- lapply(seq_along(layers), function(i) .fig_layer_code(layers[[i]], i, plot, gg$version))
  clip <- any(vapply(lc, function(x) isTRUE(x$clip_off), logical(1)))
  ax <- .fig_axis_code(plot, any(kinds == "risk_table"), fit_of(), clip)
  base <- if (length(lc) && !is.null(lc[[1L]]$base)) lc[[1L]]$base else "p <- ggplot()"
  panels <- Filter(Negate(is.null), lapply(lc, `[[`, "panel"))
  add_res <- .fig_plot_add_code(plot$add %||% list(), gg$version)
  guard_v <- c(unlist(lapply(lc, `[[`, "guard")), add_res$guard)
  features <- c(unlist(lapply(lc, `[[`, "features")), add_res$features)
  guard <- .fig_guard_code(guard_v, features)
  libs <- unique(c("dplyr", "ggplot2", s$libs, unlist(lapply(lc, `[[`, "libs")), add_res$libs))
  # a catalog layer of another package is called as pkg::fn; it must be there
  needs <- setdiff(unique(c(unlist(lapply(lc, `[[`, "package")), add_res$pkgs)), "ggplot2")
  if (any(kinds %in% c("km_curve", "km_ci", "censor_mark"))) libs <- unique(c(libs, "ggsurvfit"))
  uses_pd <- any(vapply(layers, function(l) .lgl(l$dodge), logical(1)))
  lab <- function(f) { x <- .pv(plot, f); if (!is.null(x)) q(x) }
  labs_args <- c(if (!is.null(lab("x_label"))) paste("x =", lab("x_label")),
                 if (!is.null(lab("y_label"))) paste("y =", lab("y_label")),
                 if (!is.null(lab("title"))) paste("title =", lab("title")))
  # the palette's scales, for the aesthetics a layer maps
  pieces <- .fig_pieces()
  maps <- function(aes) any(vapply(layers, function(l) {
    g <- pieces[[l$layer %||% ""]]$geom
    if (!is.null(g)) return(aes %in% g$aes && !is.null(l[[aes]]))
    if (identical(l$layer, "geom")) return(aes %in% names(.named(l$aes)))
    identical(l$layer, "n_table") && aes == "colour"
  }, logical(1)))
  scales <- if (!is.null(plot$colour_by)) list(
    if (maps("colour") || any(kinds == "km_curve"))
      "scale_colour_manual(values = pal, breaks = names(pal))",
    if (maps("fill") || any(kinds == "km_ci"))
      "scale_fill_manual(values = pal, breaks = names(pal), na.value = \"grey80\")")
  finish <- c(scales, ax$terms,
    if (length(labs_args)) sprintf("labs(%s)", paste(labs_args, collapse = ", ")),
    as.list(pp_theme_lines(.pv(plot, "theme"), .pv(plot, "base_size"))),
    if (!.lgl(.pv(plot, "x_text"))) "theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())",
    as.list(pp_q_legend(.pv(plot, "legend"))))
  body_layers <- unlist(lapply(lc, function(x) if (is.null(x$panel)) x$code))
  panel_code <- unlist(lapply(lc, function(x) if (!is.null(x$panel)) x$code))
  assemble <- if (length(panels)) {
    h <- vapply(panels, `[[`, numeric(1), "height")
    sprintf("%s <- %s + plot_layout(heights = %s)", name,
            paste(c("p", vapply(panels, `[[`, "", "name")), collapse = " / "),
            vec_code(round(c(1 - sum(h), h), 3)))
  } else sprintf("%s <- p", name)
  n <- function(f) .pv(plot, f)
  fnames <- vapply(layers, function(l) if (identical(l$layer, "call")) .fig_bare_fn_name(l$fn) else "", "")
  list(
    libs = libs, needs = needs, guard = guard, guard_v = guard_v, features = features,
    step1 = c(
      sprintf("# Input data frames: %s", paste(pp_ds_name(d$reads), collapse = ", ")),
      "",
      d$code,
      s$code,
      .fig_palette_code(plot, setup),
      "",
      ax$pre,
      if (uses_pd) sprintf("pd <- position_dodge(width = %s)", n("dodge")),
      ""),
    step2 = c(
      base,
      body_layers,
      "# ---- the figure's settings ----",
      plus_code("p", finish, append = TRUE),
      unlist(lapply(lc, `[[`, "after")),
      add_res$lines,
      panel_code,
      "",
      assemble),
    patch = length(panels) > 0L, risktable = any(fnames == "add_risktable"))
}

# a script's lines, one an element, no two blank lines in a row
.code_lines <- function(code) {
  code <- unlist(strsplit(paste(code, collapse = "\n"), "\n", fixed = TRUE))
  code[!(code == "" & c(FALSE, utils::head(code, -1L) == ""))]
}

# saving the PNG, at the size in `plot`
.fig_save_code <- function(plot, plot_id, name = "fig") {
  n <- function(f) .pv(plot, f)
  c(section("saving the figure"),
    sprintf("fig_path   <- file.path(\"output\", %s)", q(paste0(pp_file_name(plot_id), ".png"))),
    sprintf("fig_width  <- %s", n("width")),
    sprintf("fig_height <- %s", n("height")),
    sprintf("fig_dpi    <- %s", n("dpi")),
    sprintf("fig_units  <- %s", q(n("units"))),
    "",
    "dir.create(dirname(fig_path), showWarnings = FALSE, recursive = TRUE)",
    "ggsave(",
    "  filename = fig_path,",
    sprintf("  plot     = %s,", name),
    "  width    = fig_width,",
    "  height   = fig_height,",
    "  dpi      = fig_dpi,",
    "  units    = fig_units",
    ")")
}

#' @rdname tfl_fig_design
#' @param setup `TRUE`: the code runs after the study's figure setup
#'   ([tfl_fig_setup_code()]), so the palette is its `tfl_colours()`.
#' @param save `FALSE` leaves out saving the PNG: the code makes the
#'   figure only (a report program writes it into its RTF).
#' @param name The name the figure is given.
#' @export
tfl_fig_design_code <- function(design, plot_id = "fig", ggplot2_version = NULL,
                                setup = FALSE, save = TRUE, name = "fig") {
  design <- if (inherits(design, "tfl_fig_design")) design else .fig_design_from_list(design)
  gg <- .fig_target_version(ggplot2_version, design)
  if (length(design$plots)) {
    return(.drop_attached_ns(.fig_compose_code(design, plot_id, gg, setup = setup,
                                               save = save, name = name)))
  }
  whole <- Filter(function(l) identical(l$layer, "figure"), design$layers)
  if (length(whole)) {
    w <- whole[[1L]]
    fn <- getExportedValue("tflspec", .fig_fun(w$type))
    args <- lapply(w$args %||% list(), function(v) if (is.list(v)) unlist(v) else v)
    return(.drop_attached_ns(do.call(fn, c(list(style = w$style %||% NULL,
                                                plot_id = plot_id), args))))
  }
  b <- .fig_design_body(design, gg, setup = setup, name = name)
  code <- c(
    sprintf("# %s: %s", plot_id, design$template %||% "figure design"),
    sprintf("# Generated by tflspec %s from the figure's design.",
            utils::packageVersion("tflspec")),
    if (gg$explicit) sprintf("# Written for ggplot2 %s", gg$version),
    "",
    paste0("library(", b$libs, ")"),
    if (length(b$needs)) sprintf("# also needs: %s (called as pkg::fn)", paste(b$needs, collapse = ", ")),
    if (length(b$guard)) c("", b$guard),
    "",
    section("data"),
    b$step1,
    section("the figure"),
    b$step2,
    if (save) c(name, "", .fig_save_code(design$plot, plot_id, name)))
  structure(.drop_attached_ns(.code_lines(code)), class = "tfl_code")
}

# ---- the checks -------------------------------------------------------------

#' @rdname tfl_fig_design
#' @param adam The data ([tfl_read_adam()]): the variables and PARAMCDs the
#'   design names are looked for in it.
#' @return `tfl_check_fig_design()`: a data frame of the problems
#'   (`part`, `field`, `problem`; `part` is e.g. `data[2] join`); no rows
#'   when there are none. With the target ggplot2 (`ggplot2_version`), a
#'   function or argument of a `call` the target does not have, or drops,
#'   is one; what it only deprecates is advice ([tfl_fig_advice()]).
#' @export
tfl_check_fig_design <- function(design, adam = NULL, ggplot2_version = NULL) {
  out <- data.frame(part = character(), field = character(), problem = character(),
                    stringsAsFactors = FALSE)
  add <- function(p, f, x) out[nrow(out) + 1L, ] <<- list(p, f, x)
  gg <- tryCatch(.fig_target_version(ggplot2_version, design), error = function(e) {
    add("design", "ggplot2_version", conditionMessage(e))
    list(version = .fig_installed_gg())
  })
  compat_errors <- function(spec, part) {
    e <- .fig_compat_walk(spec, gg$version, part)$errors
    for (r in seq_len(nrow(e))) add(e$part[r], e$field[r], e$problem[r])
  }
  adam <- if (!is.null(adam)) pp_prep_adam(adam)
  if (length(design$plots)) return(rbind(out, .fig_check_compose(design, adam, gg)))
  pieces <- .fig_pieces()
  ds <- function(nm) if (!is.null(adam) && !is.null(nm)) adam[[toupper(nm)]]
  cols <- NULL          # the columns of df, as far as known
  objects <- list()     # stats objects: their columns (NULL = unknown)
  check_fields <- function(p, part, kind) {
    f <- pieces[[kind]]$fields
    for (i in seq_len(nrow(f))) {
      v <- p[[f$field[i]]]
      set <- !is.null(v) && length(v) && !all(is.na(v)) && !identical(v, "")
      if (f$required[i] && !set && is.na(f$default[i])) add(part, f$field[i], "is required")
      if (!set) next
      if (f$kind[i] == "choice" && !is.na(f$choices[i]) && kind != "figure") {
        ch <- strsplit(f$choices[i], " | ", fixed = TRUE)[[1L]]
        if (!all(as.character(v) %in% ch)) add(part, f$field[i], paste0("'", v, "' is not one of ", paste(ch, collapse = ", ")))
      }
      if (f$kind[i] == "number" && suppressWarnings(anyNA(as.numeric(v)))) add(part, f$field[i], "is not a number")
    }
    unknown <- setdiff(names(p), c(f$field, "step", "layer"))
    for (u in unknown) add(part, u, paste0("is not a field of ", kind))
  }
  need_var <- function(part, field, vars, where = cols, what = "df") {
    if (is.null(where)) return()
    miss <- setdiff(vars, where)
    if (length(miss)) add(part, field, paste0("no variable ", paste(miss, collapse = ", "), " in ", what))
  }
  for (i in seq_along(design$data)) {
    s <- design$data[[i]]
    k <- s$step %||% ""
    part <- sprintf("data[%d] %s", i, k)
    if (!k %in% names(pieces) || pieces[[k]]$section != "data") {
      add(part, "step", "unknown data step")
      next
    }
    check_fields(s, part, k)
    v <- function(f) .fv(s, f, k)
    switch(k,
      read = {
        d <- ds(v("dataset"))
        if (!is.null(adam) && is.null(d)) add(part, "dataset", paste0("no dataset ", v("dataset")))
        cols <- if (!is.null(d)) names(d)
      },
      join = {
        d <- ds(v("dataset"))
        if (!is.null(adam) && is.null(d)) add(part, "dataset", paste0("no dataset ", v("dataset")))
        vars <- .split_vals(v("vars"))
        src <- sub("^.*=\\s*", "", vars)
        new <- sub("\\s*=.*$", "", vars)
        if (!is.null(d)) need_var(part, "vars", c(src, v("by")), names(d), toupper(v("dataset")))
        if (!is.null(cols)) cols <- union(cols, new)
      },
      param = {
        need_var(part, "variable", v("variable"))
        if (!is.null(cols) && !is.null(adam) && !is.null(s$value)) {
          d <- ds(Filter(function(x) identical(x$step, "read"), design$data)[[1L]]$dataset %||% "")
          if (!is.null(d) && v("variable") %in% names(d)) {
            miss <- setdiff(.split_vals(s$value), unique(d[[v("variable")]]))
            if (length(miss)) add(part, "value", paste0("no ", v("variable"), " ", paste(miss, collapse = ", ")))
          }
        }
      },
      flag = need_var(part, "variable", v("variable")),
      time_unit = need_var(part, "variable", v("variable")),
      levels = need_var(part, "variable", c(v("variable"), v("order_by"))),
      rank = {
        need_var(part, "by", v("by"))
        if (!is.null(cols)) cols <- union(cols, v("variable"))
      },
      derive = if (!is.null(cols)) cols <- union(cols, v("variable")),
      data_code = cols <- NULL)
  }
  objects["df"] <- list(cols)   # NULL (unknown columns) keeps the name
  for (i in seq_along(design$stats)) {
    s <- design$stats[[i]]
    k <- s$step %||% ""
    part <- sprintf("stats[%d] %s", i, k)
    if (!k %in% names(pieces) || pieces[[k]]$section != "stats") {
      add(part, "step", "unknown statistics step")
      next
    }
    check_fields(s, part, k)
    v <- function(f) .fv(s, f, k)
    switch(k,
      survfit = {
        need_var(part, "time", c(v("time"), v("censor"), v("by")))
        objects[[v("name")]] <- character()
      },
      summary = , summary_by = {
        by <- .split_vals(v("by"))
        need_var(part, "by", c(v("value"), by))
        objects[[v("name")]] <- c(by, "n", "mean", "sd", "se", "lo", "hi")
      },
      rate = {
        by <- .split_vals(v("by"))
        need_var(part, "by", c(v("category"), by))
        objects[[v("name")]] <- c(by, "n", "x", "rate", "lcl", "ucl", "label")
      },
      count = {
        by <- .split_vals(v("by"))
        need_var(part, "by", c(v("category"), by))
        objects[[v("name")]] <- c(by, v("category"), "n", "pct", "label")
      },
      subset = {
        ds <- v("dataset")
        d <- if (toupper(ds) == "DF") cols else if (!is.null(adam)) names(adam[[toupper(ds)]])
        if (!is.null(adam) && toupper(ds) != "DF" && is.null(adam[[toupper(ds)]])) {
          add(part, "dataset", paste0("no dataset ", ds))
        }
        from <- .split_vals(v("from_df"))
        need_var(part, "from_df", from)
        objects[[v("name")]] <- if (is.null(d)) NULL else union(d, from)
      },
      stats_code = objects["?"] <- list(NULL))
  }
  f <- .fig_plot_fields()
  for (nm in names(design$plot)) {
    r <- f[f$field == nm, , drop = FALSE]
    if (!nrow(r)) { add("plot", nm, "is not a figure setting"); next }
    v <- design$plot[[nm]]
    if (r$kind == "choice" && !is.na(r$choices) &&
        !as.character(v) %in% strsplit(r$choices, " | ", fixed = TRUE)[[1L]]) {
      add("plot", nm, paste0("'", v, "' is not one of ", r$choices))
    }
    if (r$kind == "number" && suppressWarnings(anyNA(as.numeric(v)))) add("plot", nm, "is not a number")
  }
  if (!is.null(design$plot$colour_by)) need_var("plot", "colour_by", design$plot$colour_by)
  kinds <- vapply(design$layers, function(l) l$layer %||% "", "")
  for (i in seq_along(design$layers)) {
    l <- design$layers[[i]]
    k <- kinds[i]
    part <- sprintf("layers[%d] %s", i, k)
    if (!k %in% names(pieces) || pieces[[k]]$section != "layers") {
      add(part, "layer", "unknown layer")
      next
    }
    check_fields(l, part, k)
    if (isTRUE(pieces[[k]]$base) && i != 1L) add(part, "layer", "must be the first layer")
    if (k %in% c("km_ci", "censor_mark") && !"km_curve" %in% kinds) add(part, "layer", "needs the KM curves layer")
    fl <- pieces[[k]]$fields
    obj <- NULL
    if ("data" %in% fl$field) {
      obj <- .fv(l, "data", k)
      if (!obj %in% names(objects) && !"?" %in% names(objects)) {
        add(part, "data", paste0("no data or statistics named ", obj))
      }
    }
    if ("fit" %in% fl$field) {
      fit <- .fv(l, "fit", k)
      if (!fit %in% names(objects) && !"?" %in% names(objects)) add(part, "fit", paste0("no fit named ", fit))
    }
    if (!is.null(obj) && obj %in% names(objects)) {
      for (vf in fl$field[fl$kind == "variable"]) {
        x <- .fv(l, vf, k)
        if (!is.null(x)) need_var(part, vf, x, objects[[obj]], obj)
      }
    }
    if (k == "call") {
      extra <- .fig_check_call(l, part)
      for (r in seq_len(nrow(extra))) add(extra$part[r], extra$field[r], extra$problem[r])
      compat_errors(l, part)
      # ggsurvfit's add_* go on a ggsurvfit(): the KM curves layer
      fnb <- .fig_bare_fn_name(l$fn %||% "")
      pkg <- .fig_resolve_fn(l$fn %||% "", l$package)$pkg %||% .fig_find_pkg(fnb)
      if (identical(pkg, "ggsurvfit") && startsWith(fnb, "add_") && !"km_curve" %in% kinds) {
        add(part, "fn", paste0(fnb, "() needs the KM curves layer (km_curve)"))
      }
      if (identical(fnb, "add_risktable")) {
        if ("risk_table" %in% kinds) {
          add(part, "fn", "the number at risk twice: add_risktable() and the risk_table layer; keep one")
        } else if (any(kinds == "n_table")) {
          add(part, "fn", "add_risktable() cannot be stacked with other panels (n_table); use the risk_table layer")
        }
      }
      if (grepl(.fig_plotwide_re, .fig_bare_fn_name(l$fn %||% ""))) {
        add(part, "fn", paste0("'", .fig_bare_fn_name(l$fn), "' is a figure-wide function; use plot.add instead of layers"))
      }
      if (!is.null(l$aes) && !is.null(obj) && obj %in% names(objects) && !is.null(objects[[obj]])) {
        for (nm in names(l$aes)) {
          val <- l$aes[[nm]]
          if (is.character(val) && length(val) == 1L && !.is_fig_r(val)) need_var(part, paste0("aes$", nm), val, objects[[obj]], obj)
        }
      }
    }
  }
  add_list <- design$plot$add %||% list()
  for (i in seq_along(add_list)) {
    a <- add_list[[i]]
    part <- sprintf("plot.add[%d]", i)
    extra <- .fig_check_call(a, part)
    for (r in seq_len(nrow(extra))) add(extra$part[r], extra$field[r], extra$problem[r])
    compat_errors(a, part)
    fnname <- .fig_bare_fn_name(a$fn %||% "")
    if (grepl("^facet_", fnname) && !is.null(design$plot$facet_by)) {
      add(part, "fn", "overrides plot$facet_by (a facet_* is also in plot.add)")
    }
    if (grepl("^scale_colou?r_", fnname) && !is.null(design$plot$colour_by)) {
      add(part, "fn", "overrides plot$colour_by (a scale_colour_*/scale_color_* is also in plot.add)")
    }
    if (grepl("^coord_", fnname) &&
        any(vapply(design$plot[c("x_min", "x_max", "y_min", "y_max")], Negate(is.null), logical(1)))) {
      add(part, "fn", "overrides plot$x_min/x_max/y_min/y_max (a coord_* is also in plot.add)")
    }
    if (grepl("^scale_x_", fnname) && .lgl(design$plot$x_log %||% FALSE)) {
      add(part, "fn", "overrides plot$x_log (a scale_x_* is also in plot.add)")
    }
  }
  out
}

# ---- templates --------------------------------------------------------------

.fig_templates <- function() {
  t <- function(template, kind, label, parts = TRUE) data.frame(
    template = template, kind = kind, label = label, parts = parts, stringsAsFactors = FALSE)
  cat <- tfl_fig_catalog()
  whole <- cat[cat$status == "implemented" & cat$type %in% c("forest", "ae_dot", "butterfly", "edish", "sankey", "sunburst") &
                 !cat$style %in% c("estimates", "subgroups"), ]
  tp <- rbind(
    t("km_risk_table", "km", "KM curves + number at risk"),
    t("km_simple", "km", "KM curves"),
    t("km_ci", "km", "KM curves + confidence bands + number at risk"),
    t("km_single_arm", "km", "One KM curve + number at risk"),
    t("waterfall_response", "waterfall", "Waterfall, bars by best response"),
    t("waterfall_plain", "waterfall", "Waterfall"),
    t("swimmer_bar", "swimmer", "Swimmer: bars + ongoing arrows"),
    t("swimmer_response", "swimmer", "Swimmer: bars by best response + ongoing arrows"),
    t("swimmer_assessment", "swimmer", "Swimmer: bars by best response + response at each assessment"),
    t("swimmer_full", "swimmer", "Swimmer: bars, assessments, event markers, ongoing arrows"),
    t("individual_spider", "individual", "Spider: % change in tumour size per subject, by best response"),
    t("bar_rate_ci", "bar", "Response rate by group with 95% CI"),
    t("bar_stacked", "bar", "100% stacked bars of a category by group"),
    t("bar_dodged", "bar", "Percent per category, groups side by side"),
    t("mean_se", "mean", "Mean +/- SE by visit"),
    t("mean_sd", "mean", "Mean +/- SD by visit"),
    t("mean_ci", "mean", "Mean (95% CI) by visit"),
    t("mean_se_n", "mean", "Mean +/- SE by visit + n"),
    t("individual_spaghetti", "individual", "Spaghetti: one line per subject + group means"),
    t("box_by_visit", "box", "Box plots by visit and group + mean marker"),
    t("box_by_group", "box", "Box plot per group at one visit + points + mean marker"),
    t("box_change", "box", "Box plots of change from baseline by visit + zero line"),
    t("scatter_shift", "scatter", "Baseline vs post-baseline at one visit + identity line"),
    t("scatter_xy", "scatter", "Two variables with a linear fit per group"),
    t("pk_mean", "pk", "PK: mean +/- SD concentration by nominal time"),
    t("pk_mean_log", "pk", "PK: mean +/- SD concentration, log axis"),
    t("pk_individual", "pk", "PK: individual profiles, log axis, one panel per group"),
    if (nrow(whole)) t(paste(whole$type, whole$style, sep = "_"), whole$type,
                       paste0(whole$description, " (whole script)"), parts = FALSE))
  # each template's clinical category and the data it reads, from the
  # catalog row of its type and style (one axis for both)
  style <- substring(tp$template, nchar(tp$kind) + 2L)
  at <- match(paste(tp$kind, style), paste(cat$type, cat$style))
  tp$category <- cat$category[at]
  tp$data <- cat$data[at]
  tp
}

#' Figure templates
#'
#' A template fills a figure design's four parts at once -- its data steps,
#' statistics, settings and layers -- for a kind of figure; each piece is
#' then edited on its own ([tfl_fig_design()]).  Sizes, line widths and
#' colours come from the figure style standard ([tfl_fig_style()]).  The
#' templates of the types not yet in parts (forest, AE dot, butterfly,
#' eDISH, sankey, sunburst) give a design of one `figure` layer: the
#' type's whole script, with its arguments (`parts = FALSE` in
#' `tfl_fig_templates()`; see [tfl_fig_schema()] for the arguments).
#'
#' @param template One of `tfl_fig_templates()$template`.
#' @param data,param,pop,group The dataset, its PARAMCD, the analysis set
#'   flag and the group variable (joined from ADSL when `join_adsl`).
#' @param time,censor,time_unit KM: the time, the censor variable, the axis's
#'   unit.  PK: `time` is the nominal time.
#' @param value,visit,visit_label The value and the visit (number and
#'   label): mean, box, spaghetti, scatter.
#' @param x,y Scatter: the two variables.
#' @param at_visit Box by group, shift: the visit kept (its label; empty =
#'   the last).
#' @param response_data,response Best response: its dataset and PARAMCD.
#' @param category,responders Bar: the category variable, and the values
#'   counted as response.
#' @param id,duration Swimmer: the subject and the bar's length (days).
#' @param join_adsl Join `group` (and `pop`) from ADSL (`TRUE` for the
#'   longitudinal kinds; KM, waterfall and swimmer read them from their own
#'   dataset).
#' @param title The figure's title.
#' @param ... For a whole-script template: the type's other arguments.
#' @return `tfl_fig_templates()`: a data frame (`template`, `kind`,
#'   `label`, `parts`, `category`, `data`): `category` is the clinical
#'   category of [tfl_fig_catalog()] (Efficacy: time to event, Safety,
#'   PK / PD ...), `data` the datasets the template reads, as the catalog
#'   writes them (`"ADTR + ADRS"`: both; `"ADLB / ADVS + ADSL"`: ADLB or
#'   ADVS, and ADSL) -- for a GUI's headings, and to say which templates a
#'   study's data can draw; `tfl_fig_template()`: a `tfl_fig_design`.
#' @export
tfl_fig_templates <- function() .fig_templates()

#' @rdname tfl_fig_templates
#' @export
tfl_fig_template <- function(template, data = NULL, param = NULL, pop = NULL,
                             group = NULL, time = NULL, censor = "CNSR",
                             time_unit = "months", value = NULL,
                             visit = "AVISITN", visit_label = "AVISIT",
                             x = NULL, y = NULL, at_visit = NULL,
                             response_data = "ADRS", response = "BOR",
                             category = "AVALC", responders = "CR, PR",
                             id = "USUBJID", duration = "TRTDURD",
                             join_adsl = NULL, title = NULL, ...) {
  tp <- .fig_templates()
  if (!template %in% tp$template) {
    stop("Unknown template '", template, "': one of ",
         paste(tp$template, collapse = ", "), call. = FALSE)
  }
  kind <- tp$kind[tp$template == template]
  o <- pp_default_options(kind)
  opt <- function(k, d) { x <- o[[k]]; if (is.null(x) || identical(x, "")) d else x }
  size <- list(width = as.numeric(opt("width", 7.5)), height = as.numeric(opt("height", 4.5)),
               dpi = as.numeric(opt("dpi", 300)), units = opt("units", "in"),
               base_size = as.numeric(opt("base_size", 10)), theme = opt("theme", "boxed"))
  # the design of a whole-script template: the type's function and its arguments
  if (!tp$parts[tp$template == template]) {
    style <- sub(paste0("^", kind, "_"), "", template)
    args <- c(list(data = data, param = param, pop = pop, group = group, title = title), list(...))
    args <- args[!vapply(args, is.null, logical(1))]
    return(tfl_fig_design(template = template, layers = list(c(
      list(layer = "figure", type = kind, style = style), if (length(args)) list(args = args)))))
  }
  read <- function(ds) list(step = "read", dataset = ds)
  keep_param <- function(p) list(step = "param", value = p)
  keep_pop <- function(p) if (!is.null(p)) list(step = "flag", variable = p)
  join_step <- function(vars, ds = "ADSL") if (length(vars)) list(step = "join", dataset = ds, vars = paste(vars, collapse = ", "))
  bor_join <- function() list(step = "join", dataset = response_data,
                              where = sprintf("PARAMCD == %s", q(response)), vars = "BOR = AVALC")
  bor_levels <- function() list(step = "levels", variable = "BOR", levels = "CR | PR | SD | PD | NE")
  drop <- function(x) x[!vapply(x, is.null, logical(1))]
  plot_of <- function(...) drop(c(list(title = title), list(...), size))
  switch(kind,
    km = {
      single <- template == "km_single_arm"
      data <- data %||% "ADTTE"; param <- param %||% "OS"; pop <- pop %||% "FASFL"
      time <- time %||% "AVAL"
      group <- if (single) NULL else group %||% "TRT01P"
      tfl_fig_design(
        template = template,
        data = drop(list(read(data), if (isTRUE(join_adsl)) join_step(c(group, pop)),
                         keep_param(param), keep_pop(pop),
                         list(step = "time_unit", variable = time, unit = time_unit))),
        stats = list(list(step = "survfit", name = "fit", time = time, censor = censor, by = group)),
        plot = plot_of(x_label = sprintf("Time (%s)", tools::toTitleCase(time_unit)),
                       y_label = "Survival Probability", colour_by = group, palette = "treatment",
                       legend = if (single) "none" else "inside",
                       x_min = 0, y_min = 0, y_max = 1, y_by = as.numeric(opt("y_by", 0.2))),
        layers = drop(list(
          list(layer = "km_curve", linewidth = as.numeric(opt("line_width", 0.3))),
          if (template == "km_ci") list(layer = "km_ci"),
          list(layer = "censor_mark", shape = opt("censor_shape", "x"),
               size = as.numeric(opt("censor_size", 3)), stroke = as.numeric(opt("censor_stroke", 0.6))),
          if (template != "km_simple") list(layer = "hline", yintercept = 0.5,
               linetype = opt("median_linetype", "twodash"), colour = opt("median_colour", "grey50"),
               linewidth = as.numeric(opt("median_line_width", 0.3))),
          if (template != "km_simple") list(layer = "risk_table",
               title = opt("risk_title", "Number of Patients at Risk"),
               size = as.numeric(opt("text_size", 3)), height = as.numeric(opt("risk_height", 0.167))))))
    },
    waterfall = {
      data <- data %||% "ADTR"; param <- param %||% "BPCHG"; pop <- pop %||% "FASFL"
      value <- value %||% "AVAL"
      resp <- template == "waterfall_response"
      tfl_fig_design(
        template = template,
        data = drop(list(read(data), keep_param(param), keep_pop(pop),
                         if (resp) bor_join(),
                         list(step = "filter", expr = sprintf("!is.na(%s)", value)),
                         list(step = "rank", by = value, descending = TRUE, variable = "INDEX"),
                         if (resp) bor_levels())),
        plot = plot_of(x_label = "Patients", y_label = "Best % Change in Sum of Target Lesion Diameters",
                       colour_by = if (resp) "BOR", palette = if (resp) "response" else "treatment",
                       legend = if (resp) "inside" else "none", x_text = FALSE,
                       y_min = -100, y_max = 100, y_by = 20),
        layers = list(
          list(layer = "col", data = "df", x = "INDEX", y = value, fill = if (resp) "BOR"),
          list(layer = "hline", yintercept = 0, linetype = "solid", colour = "black", linewidth = 0.5),
          list(layer = "hline", yintercept = "20, -30", linetype = "dashed", colour = "grey", linewidth = 0.5),
          list(layer = "ref_label", y = "20, -30", label = "{y}%")))
    },
    swimmer = {
      data <- data %||% "ADSL"; pop <- pop %||% "FASFL"
      resp <- template != "swimmer_bar"
      assess <- template %in% c("swimmer_assessment", "swimmer_full")
      full <- template == "swimmer_full"
      div <- .fig_units[[time_unit]]
      tfl_fig_design(
        template = template,
        data = drop(list(read(data), keep_pop(pop),
                         if (resp) bor_join(),
                         list(step = "filter", expr = sprintf("!is.na(%s)", duration)),
                         if (div != 1) list(step = "time_unit", variable = duration, unit = time_unit),
                         list(step = "derive", variable = "X0", expr = "0"),
                         list(step = "derive", variable = "Y_ID", expr = sprintf("reorder(%s, %s)", id, duration)),
                         list(step = "derive", variable = "X_ARROW", expr = sprintf("%s + max(%s) * 0.03", duration, duration)),
                         if (resp) bor_levels())),
        stats = drop(list(
          list(step = "subset", name = "ongoing", dataset = "df", where = 'EOSSTT == "ONGOING"'),
          if (assess) list(step = "subset", name = "assess", dataset = response_data,
                           where = sprintf("PARAMCD == %s & !is.na(ADY)", q("OVR")), from_df = "Y_ID"),
          if (assess && div != 1) list(step = "stats_code", code = sprintf("assess <- assess |> mutate(ADY = ADY / %s)", format(div))))),
        plot = plot_of(x_label = sprintf("Time (%s)", tools::toTitleCase(time_unit)), y_label = "Subject",
                       colour_by = if (resp) "BOR", palette = if (resp) "response_light" else "treatment",
                       legend = if (resp) "right" else "none", x_min = 0,
                       height = as.numeric(opt("height", 6))),
        layers = drop(list(
          list(layer = "segment", data = "df", x = "X0", y = "Y_ID", xend = duration, yend = "Y_ID",
               colour = if (resp) "BOR", linewidth = as.numeric(opt("bar_width", 4))),
          list(layer = "segment", data = "ongoing", x = duration, y = "Y_ID", xend = "X_ARROW", yend = "Y_ID",
               linewidth = 0.6, arrow = "arrow(length = unit(2, 'mm'), type = 'closed')"),
          if (assess) list(layer = "point", data = "assess", x = "ADY", y = "Y_ID", colour = "AVALC",
                           shape = "solid_square", size = as.numeric(opt("marker_size", 2))),
          if (full) list(layer = "point", data = "df", x = "DTHADY", y = "Y_ID", shape = "solid_triangle", size = 2.5, na.rm = TRUE))))
    },
    individual = {
      spider <- template == "individual_spider"
      data <- data %||% if (spider) "ADTR" else "ADLB"
      param <- param %||% if (spider) "SDIAM" else "ALT"
      value <- value %||% if (spider) "PCHG" else "AVAL"
      x <- x %||% if (spider) "ADY" else visit_label
      pop <- pop %||% if (spider) "FASFL" else "SAFFL"
      group <- if (spider) NULL else group %||% "TRT01A"
      join <- join_adsl %||% !spider
      div <- if (spider) .fig_units[[time_unit]] else 1
      tfl_fig_design(
        template = template,
        data = drop(list(read(data), keep_param(param), if (join) join_step(c(group, pop)), keep_pop(pop),
                         if (spider) bor_join(),
                         list(step = "filter", expr = sprintf("!is.na(%s) & !is.na(%s)", value, x)),
                         if (spider && div != 1) list(step = "time_unit", variable = x, unit = time_unit),
                         if (!spider) list(step = "levels", variable = visit_label, order_by = visit),
                         if (spider) bor_levels())),
        stats = if (!spider) list(list(step = "summary", name = "sm", value = value,
                                       by = paste(c(group, visit, visit_label), collapse = ", "))),
        plot = plot_of(x_label = if (spider) sprintf("Time (%s)", tools::toTitleCase(time_unit)) else "Visit",
                       y_label = if (spider) "Change from baseline in sum of diameters (%)" else value,
                       colour_by = if (spider) "BOR" else group,
                       palette = if (spider) "response" else "treatment", legend = "right"),
        layers = drop(list(
          if (spider) list(layer = "hline", yintercept = 0, linetype = "solid", colour = "grey40"),
          if (spider) list(layer = "hline", yintercept = "20, -30", linetype = "dashed", colour = "grey60"),
          list(layer = "line", data = "df", x = x, y = value, colour = if (spider) "BOR" else group,
               group = id, alpha = if (spider) 0.8 else 0.35),
          if (spider) list(layer = "point", data = "df", x = x, y = value, colour = "BOR", size = 1),
          if (!spider) list(layer = "line", data = "sm", x = visit_label, y = "mean", colour = group, linewidth = 1.2))))
    },
    bar = {
      data <- data %||% "ADRS"; param <- param %||% "BOR"; pop <- pop %||% "FASFL"; group <- group %||% "TRT01P"
      join <- join_adsl %||% TRUE
      style <- sub("^bar_", "", template)
      tfl_fig_design(
        template = template,
        data = drop(list(read(data), keep_param(param), if (join) join_step(c(group, pop)), keep_pop(pop))),
        stats = list(switch(style,
          rate_ci = list(step = "rate", name = "rt", category = category, responders = responders, by = group),
          list(step = "count", name = "ct", category = category, by = group,
               levels = if (style == "stacked") "CR | PR | SD | PD | NE"))),
        plot = plot_of(x_label = if (style == "dodged") param else "",
                       y_label = switch(style, rate_ci = sprintf("Response rate (%%) with 95%% CI [%s]", gsub(", ", "+", responders)),
                                        "Subjects (%)"),
                       colour_by = if (style == "stacked") category else group,
                       palette = if (style == "stacked") "response" else "treatment",
                       legend = switch(style, rate_ci = "none", "right"),
                       y_min = 0, y_max = if (style == "rate_ci") 110 else NULL),
        layers = switch(style,
          rate_ci = list(
            list(layer = "col", data = "rt", x = group, y = "rate", fill = group, width = 0.6),
            list(layer = "errorbar", data = "rt", x = group, ymin = "lcl", ymax = "ucl", width = 0.15),
            list(layer = "text", data = "rt", x = group, y = "ucl", label = "label", vjust = -0.3, size = 3)),
          stacked = list(
            list(layer = "col", data = "ct", x = group, y = "pct", fill = category, width = 0.6, colour = "white"),
            list(layer = "text", data = "ct", x = group, y = "pct", label = "label", size = 3,
                 position = "position_stack(vjust = 0.5)")),
          dodged = list(
            list(layer = "col", data = "ct", x = category, y = "pct", fill = group, width = 0.7,
                 position = "position_dodge(width = 0.75)"),
            list(layer = "text", data = "ct", x = category, y = "pct", label = "label", size = 2.8,
                 vjust = -0.3, position = "position_dodge(width = 0.75)"))))
    },
    mean = {
      data <- data %||% "ADLB"; param <- param %||% "ALT"; pop <- pop %||% "SAFFL"; group <- group %||% "TRT01A"
      value <- value %||% "AVAL"
      join <- join_adsl %||% TRUE
      iv <- switch(template, mean_sd = "sd", mean_ci = "ci", "se")
      ylab <- paste0(switch(iv, se = "Mean (+/- SE)", sd = "Mean (+/- SD)", ci = "Mean (95% CI)"),
                     " ", if (value == "AVAL") "" else paste0(value, " of "), param)
      tfl_fig_design(
        template = template,
        data = drop(list(read(data), keep_param(param), if (join) join_step(c(group, pop)), keep_pop(pop),
                         list(step = "filter", expr = sprintf("!is.na(%s) & !is.na(%s)", value, visit)),
                         list(step = "levels", variable = visit_label, order_by = visit))),
        stats = list(list(step = "summary", name = "sm", value = value,
                          by = paste(c(group, visit, visit_label), collapse = ", "), interval = iv)),
        plot = plot_of(x_label = "Visit", y_label = ylab, colour_by = group, palette = "treatment",
                       legend = "bottom", dodge = 0.3),
        layers = drop(list(
          if (value %in% c("CHG", "PCHG")) list(layer = "hline", yintercept = 0, linetype = "solid", colour = "grey60"),
          list(layer = "line", data = "sm", x = visit_label, y = "mean", colour = group, dodge = TRUE),
          list(layer = "point", data = "sm", x = visit_label, y = "mean", colour = group, dodge = TRUE),
          list(layer = "errorbar", data = "sm", x = visit_label, colour = group, dodge = TRUE),
          if (template == "mean_se_n") list(layer = "n_table", data = "sm", x = visit_label, group = group))))
    },
    box = {
      data <- data %||% "ADLB"; param <- param %||% "ALT"; pop <- pop %||% "SAFFL"; group <- group %||% "TRT01A"
      style <- sub("^box_", "", template)
      value <- value %||% if (style == "change") "CHG" else "AVAL"
      join <- join_adsl %||% TRUE
      one <- style == "by_group"
      tfl_fig_design(
        template = template,
        data = drop(list(read(data), keep_param(param), if (join) join_step(c(group, pop)), keep_pop(pop),
                         list(step = "filter", expr = sprintf("!is.na(%s)", value)),
                         if (style == "change") list(step = "filter", expr = sprintf("%s > 0", visit)),
                         if (one) list(step = "filter", expr = if (!is.null(at_visit)) sprintf("%s == %s", visit_label, q(at_visit))
                                                            else sprintf("%s == max(%s)", visit, visit)),
                         if (!one) list(step = "levels", variable = visit_label, order_by = visit))),
        stats = list(list(step = "summary_by", name = "sg", value = value,
                          by = paste(c(group, if (!one) c(visit, visit_label)), collapse = ", "))),
        plot = plot_of(x_label = if (one) "" else "Visit",
                       y_label = if (value == "AVAL") param else paste(value, "of", param),
                       colour_by = group, palette = "treatment", legend = if (one) "none" else "bottom",
                       dodge = 0.8),
        layers = drop(list(
          if (style == "change") list(layer = "hline", yintercept = 0, linetype = "solid", colour = "grey60"),
          if (one) list(layer = "boxplot", data = "df", x = group, y = value, fill = group, width = 0.5,
                        alpha = 0.6, outlier.shape = "open_circle")
          else list(layer = "boxplot", data = "df", x = visit_label, y = value, fill = group, width = 0.7,
                    dodge = TRUE),
          if (one) list(layer = "jitter", data = "df", x = group, y = value, width = 0.12, alpha = 0.6),
          list(layer = "point", data = "sg", x = if (one) group else visit_label, y = "mean",
               group = if (!one) group, shape = "plus", size = if (one) 3 else 2, dodge = !one))))
    },
    scatter = {
      data <- data %||% "ADLB"; param <- param %||% "ALT"; pop <- pop %||% "SAFFL"; group <- group %||% "TRT01A"
      shift <- template == "scatter_shift"
      x <- x %||% "BASE"; y <- y %||% if (shift) "AVAL" else "CHG"
      join <- join_adsl %||% TRUE
      tfl_fig_design(
        template = template,
        data = drop(list(read(data), keep_param(param), if (join) join_step(c(group, pop)), keep_pop(pop),
                         list(step = "filter", expr = sprintf("%s > 0 & !is.na(%s) & !is.na(%s)", visit, x, y)),
                         list(step = "filter", expr = if (!is.null(at_visit)) sprintf("%s == %s", visit_label, q(at_visit))
                                                      else sprintf("%s == max(%s)", visit, visit)))),
        plot = plot_of(x_label = if (shift) paste("Baseline", param) else paste(x, "of", param),
                       y_label = if (shift) paste("Post-baseline", param) else paste(y, "of", param),
                       colour_by = group, palette = "treatment", legend = "inside_tl",
                       width = as.numeric(opt("width", 6)), height = as.numeric(opt("height", 5.5))),
        layers = drop(list(
          if (shift) list(layer = "abline", intercept = 0, slope = 1, linetype = "dashed", colour = "grey50"),
          if (!shift && y %in% c("CHG", "PCHG")) list(layer = "hline", yintercept = 0, linetype = "solid", colour = "grey60"),
          list(layer = "point", data = "df", x = x, y = y, colour = group, size = 1.8, alpha = 0.8),
          if (!shift) list(layer = "smooth", data = "df", x = x, y = y, colour = group, method = "lm", se = FALSE))))
    },
    pk = {
      data <- data %||% "ADPC"; pop <- pop %||% "SAFFL"; group <- group %||% "TRT01A"
      value <- value %||% "AVAL"; time <- time %||% "NFRLT"
      join <- join_adsl %||% TRUE
      style <- sub("^pk_", "", template)
      log_y <- style != "mean"
      tfl_fig_design(
        template = template,
        data = drop(list(read(data), if (!is.null(param)) keep_param(param), if (join) join_step(c(group, pop)), keep_pop(pop),
                         list(step = "filter", expr = sprintf("!is.na(%s)", value)),
                         if (log_y) list(step = "filter", expr = sprintf("%s > 0", value)))),
        stats = if (style != "individual") list(list(step = "summary", name = "sm", value = value,
                                                     by = paste(c(group, time), collapse = ", "),
                                                     interval = "sd", positive = log_y)),
        plot = plot_of(x_label = "Nominal time (h)", y_label = if (style == "individual") "Concentration" else "Mean (SD) concentration",
                       colour_by = group, palette = "treatment", legend = if (style == "individual") "none" else "inside",
                       y_log = log_y, facet_by = if (style == "individual") group),
        layers = if (style == "individual") list(
          list(layer = "line", data = "df", x = time, y = value, colour = group, group = id, alpha = 0.5),
          list(layer = "point", data = "df", x = time, y = value, colour = group, size = 0.8, alpha = 0.5))
        else list(
          list(layer = "line", data = "sm", x = time, y = "mean", colour = group),
          list(layer = "point", data = "sm", x = time, y = "mean", colour = group, size = 1.8),
          list(layer = "errorbar", data = "sm", x = time, colour = group, width = 0.3)))
    })
}
