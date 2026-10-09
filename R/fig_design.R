# The arguments of each figure type's tfl_fig_<type>(): what the `figure`
# layer of a design (a figure type's whole script) takes.
#
# tfl_fig_schema() says, for every type, what each argument is: its section,
# its kind (a dataset, a PARAMCD, a variable, one of a set of values, a
# number ...), whether it is basic or advanced, its default and its
# choices.  A GUI draws the `figure` layer's form from it.

# How each argument name reads, wherever it appears.
# kind: dataset | param | variable | variables | flag | value | choice |
#       number | logical | text | expr | named | hidden
.fig_arg_info <- function() {
  a <- function(arg, section, kind, level, label, help, of = NA) {
    data.frame(arg = arg, section = section, kind = kind, level = level,
               label = label, help = help, of = of, stringsAsFactors = FALSE)
  }
  rbind(
    # ---- data
    a("data", "data", "dataset", "basic", "Dataset",
      "The analysis dataset the figure reads."),
    a("param", "data", "param", "basic", "Parameter (PARAMCD)",
      "The PARAMCD of the dataset to plot.", "data"),
    a("pop", "data", "flag", "basic", "Population flag",
      "A flag (== \"Y\") that selects the population, e.g. FASFL, SAFFL.",
      "data"),
    a("where", "data", "expr", "advanced", "Condition (R)",
      "A further condition on the data, e.g. AVISITN > 0."),
    a("response_data", "data", "dataset", "advanced", "Response dataset",
      "The dataset of the response (e.g. ADRS)."),
    a("response", "data", "param", "basic", "Response parameter",
      "The PARAMCD of the best overall response.", "response_data"),
    a("assessment", "data", "param", "advanced", "Assessment parameter",
      "The PARAMCD of the response at each assessment.", "response_data"),
    a("flag", "data", "flag", "advanced", "Record flag",
      "A flag (== \"Y\") selecting the records, e.g. ANL01FL.", "data"),
    a("tefl", "data", "flag", "advanced", "Treatment-emergent flag",
      "A flag (== \"Y\") selecting the events, e.g. TRTEMFL.", "data"),
    a("post_baseline", "data", "logical", "advanced", "Post-baseline only",
      "Only post-baseline records."),
    a("key", "data", "variable", "advanced", "Subject key",
      "The variable joining datasets (USUBJID).", "data"),
    # ---- mapping: the variables of the figure
    a("group", "mapping", "variable", "basic", "Group (treatment)",
      "The variable of the groups compared, e.g. TRT01A.", "data"),
    a("value", "mapping", "variable", "basic", "Value",
      "The analysis value, e.g. AVAL, CHG.", "data"),
    a("time", "mapping", "variable", "basic", "Time",
      "The time variable (AVAL of ADTTE; the time of a PK sample).", "data"),
    a("censor", "mapping", "variable", "basic", "Censor (1 = censored)",
      "CNSR: 1 = censored, 0 = event.", "data"),
    a("visit", "mapping", "variable", "basic", "Visit (numeric)",
      "The visit axis, e.g. AVISITN.", "data"),
    a("visit_label", "mapping", "variable", "advanced", "Visit label",
      "The visit's label, e.g. AVISIT.", "data"),
    a("at_visit", "mapping", "value", "advanced", "At visit",
      "The visit shown (a value of the visit variable).", "data"),
    a("x", "mapping", "variable", "basic", "X", "The x variable.", "data"),
    a("y", "mapping", "variable", "basic", "Y", "The y variable.", "data"),
    a("id", "mapping", "variable", "advanced", "Subject label",
      "The subject's label on the axis (SUBJID).", "data"),
    a("category", "mapping", "variable", "basic", "Category",
      "The categories counted.", "data"),
    a("stage", "mapping", "variable", "basic", "Stage",
      "The stage (line of therapy, period ...).", "data"),
    a("term", "mapping", "variable", "basic", "Term",
      "The event term, e.g. AEDECOD.", "data"),
    a("alt", "mapping", "param", "advanced", "ALT parameter", "PARAMCD of ALT.",
      "data"),
    a("ast", "mapping", "param", "advanced", "AST parameter", "PARAMCD of AST.",
      "data"),
    a("bili", "mapping", "param", "advanced", "Bilirubin parameter",
      "PARAMCD of total bilirubin.", "data"),
    a("duration", "mapping", "variable", "basic", "Duration",
      "The bar length (days), e.g. TRTDURD.", "data"),
    a("start", "mapping", "variable", "advanced", "Start",
      "The bar start (days), for bars not from 0.", "data"),
    a("end", "mapping", "variable", "advanced", "End",
      "The bar end (days), for bars not from 0.", "data"),
    a("day", "mapping", "variable", "advanced", "Assessment day",
      "The day of each assessment, e.g. ADY.", "response_data"),
    a("subgroups", "mapping", "variables", "basic", "Subgroups",
      "The subgroup variables, one block each.", "data"),
    a("by", "mapping", "variable", "advanced", "By", "Panels by this variable.",
      "data"),
    a("time_label", "mapping", "variable", "advanced", "Time label",
      "The nominal time's label.", "data"),
    a("x_var", "mapping", "variable", "advanced", "X variable", "", "data"),
    a("events", "mapping", "named", "advanced", "Events",
      "Event markers: label = day variable, e.g. Death = DTHADY.", "data"),
    a("ongoing", "mapping", "named", "advanced", "Ongoing",
      "Subjects ongoing: variable = value, e.g. EOSSTT = ONGOING.", "data"),
    a("responders", "mapping", "text", "advanced", "Responders",
      "The responses counted as response, e.g. CR, PR."),
    # ---- style
    a("style", "style", "choice", "basic", "Style",
      "The figure's style (what it draws)."),
    a("palette", "style", "choice", "advanced", "Palette",
      "The colours (the figure style standard's palettes)."),
    a("theme", "style", "choice", "advanced", "Theme",
      "boxed (panel border) or L_axis (left and bottom axes) ..."),
    # ---- axes
    a("time_unit", "axes", "choice", "basic", "Time unit",
      "The axis's unit; the data are in days."),
    a("visit_every", "axes", "number", "advanced", "Visit lines every",
      "Dotted visit lines every this many time units."),
    a("uln", "axes", "number", "advanced", "ULN multiple",
      "The multiple of the upper limit of normal drawn."),
    a("top", "axes", "number", "advanced", "Top N", "Show the N most frequent."),
    a("min_pct", "axes", "number", "advanced", "Minimum %",
      "Show only terms at least this frequent (%)."),
    # ---- legend and output
    a("legend", "legend", "choice", "basic", "Legend",
      "Where the legend goes (panel*: a legend drawn from an item table)."),
    a("title", "output", "text", "basic", "Figure title", "The figure's own title."),
    a("width", "output", "number", "advanced", "Width", ""),
    a("height", "output", "number", "advanced", "Height", ""),
    a("dpi", "output", "number", "advanced", "DPI", ""),
    a("units", "output", "choice", "advanced", "Units", ""),
    # ---- not in a design
    a("adam", "hidden", "hidden", "advanced", "", ""),
    a("file", "hidden", "hidden", "advanced", "", ""),
    a("plot_id", "hidden", "hidden", "advanced", "", ""),
    a("ard", "hidden", "expr", "advanced", "KM table's ARD (R)",
      "R code giving the KM table's ARD: its number at risk is used.")
  )
}

# The engine options a spec-engine type (km / waterfall / swimmer) takes
# through `...`: the axes and a few look settings.
.fig_engine_opts <- function() {
  o <- function(type, arg, section, kind, label, help) {
    data.frame(type = type, arg = arg, section = section, kind = kind,
               level = "advanced", label = label, help = help,
               stringsAsFactors = FALSE)
  }
  axes <- function(type) rbind(
    o(type, "x_max", "axes", "number", "X max", "The x axis ends here."),
    o(type, "x_by", "axes", "number", "X step", "The x axis breaks every ..."),
    o(type, "y_min", "axes", "number", "Y min", ""),
    o(type, "y_max", "axes", "number", "Y max", ""),
    o(type, "y_by", "axes", "number", "Y step", ""))
  rbind(
    axes("km"),
    o("km", "censor_shape", "style", "choice", "Censor mark", ""),
    o("km", "risk_title", "legend", "text", "Number at risk title", ""),
    axes("waterfall"),
    o("waterfall", "ref_lines", "axes", "text", "Reference lines",
      "Threshold lines, e.g. 20,-30."),
    axes("swimmer"),
    o("swimmer", "visit_label", "axes", "text", "Visit label", ""))
}

# The function of a figure type.
.fig_fun <- function(type) {
  cat <- tfl_fig_catalog()
  f <- unique(cat$fun[cat$type == type & cat$status == "implemented"])
  f <- f[!is.na(f) & nzchar(f)]
  if (!length(f)) stop("No figure function for type '", type, "'.", call. = FALSE)
  f[1L]
}

#' The arguments of every figure type, described
#'
#' One row per argument of each figure type's `tfl_fig_<type>()` (and, for
#' `km`, `waterfall` and `swimmer`, the axis and look options it takes
#' through `...`): what a figure design ([tfl_fig_design()]) may say, and
#' what a GUI draws its form from.
#'
#' * `section`: `data`, `mapping` (the variables), `style`, `axes`, `legend`
#'   or `output`;
#' * `kind`: `dataset`, `param` (a PARAMCD of the dataset named in `of`),
#'   `variable` / `variables` / `flag` (of `of`), `value`, `choice`,
#'   `number`, `logical`, `text`, `expr` (R code), `named`
#'   (`label = variable` pairs);
#' * `level`: `basic` (shown first) or `advanced`;
#' * `default`: the function's default, as text (`NA` = none / computed);
#' * `choices`: for `choice`, the values, `|` between them.
#'
#' @param type A figure type ([tfl_fig_types()]); `NULL` for all.
#' @return A data frame: `type`, `fun`, `arg`, `section`, `kind`, `level`,
#'   `label`, `help`, `of`, `default`, `choices`.
#' @export
tfl_fig_schema <- function(type = NULL) {
  cat <- tfl_fig_catalog()
  types <- unique(cat$type[cat$status == "implemented"])
  if (!is.null(type)) types <- intersect(types, type)
  info <- .fig_arg_info()
  styles_of <- function(t) cat$style[cat$type == t & cat$status == "implemented"]
  rows <- lapply(types, function(t) {
    fn <- .fig_fun(t)
    fm <- formals(getExportedValue("tflspec", fn))
    args <- setdiff(names(fm), "...")
    d <- lapply(args, function(ar) {
      i <- match(ar, info$arg)
      r <- if (is.na(i)) {
        data.frame(arg = ar, section = "mapping", kind = "text",
                   level = "advanced", label = ar, help = "", of = NA,
                   stringsAsFactors = FALSE)
      } else info[i, ]
      dv <- fm[[ar]]
      default <- NA_character_
      choices <- NA_character_
      if (is.call(dv) && identical(dv[[1L]], as.name("c"))) {
        v <- tryCatch(eval(dv), error = function(e) NULL)
        if (is.character(v) && is.null(names(v)) && ar %in% c("style", "legend", "time_unit",
                                                              "units", "theme", "palette")) {
          choices <- paste(v, collapse = " | ")
          default <- v[1L]
        } else if (!is.null(v)) {
          default <- if (is.null(names(v))) paste(v, collapse = " | ") else
            paste(paste(names(v), v, sep = " = "), collapse = " | ")
        }
      } else if (is.atomic(dv) && length(dv) == 1L && !is.na(dv)) {
        default <- as.character(dv)
      }
      r$default <- default
      r$choices <- choices
      r
    })
    d <- do.call(rbind, d)
    # the group, the analysis set and the subgroups: variables of the
    # figure's dataset for the spec engine (km, waterfall, swimmer); the
    # other types join them from ADSL
    if (!"..." %in% names(fm)) {
      d$of[d$arg %in% c("group", "pop", "subgroups")] <- "ADSL"
    }
    # the choices the standard or the engine knows
    set <- function(a, v) if (a %in% d$arg && is.na(d$choices[d$arg == a])) {
      d$choices[d$arg == a] <<- paste(v, collapse = " | ")
    }
    set("style", styles_of(t))
    if (is.na(d$default[d$arg == "style"])) d$default[d$arg == "style"] <- styles_of(t)[1L]
    set("palette", names(tfl_fig_palettes()))
    set("theme", pp_themes)
    set("units", c("in", "cm", "px"))
    set("time_unit", c("days", "weeks", "months", "years"))
    set("legend", c("none", "right", "bottom", "inside", "inside_bl",
                    "panel", "panel_right", "panel_inside"))
    eng <- .fig_engine_opts()
    eng <- eng[eng$type == t, setdiff(names(eng), "type"), drop = FALSE]
    if ("..." %in% names(fm) && nrow(eng)) {
      eng$of <- NA_character_
      eng$default <- vapply(eng$arg, function(k) {
        v <- pp_default_options(t)[[k]]
        if (is.null(v) || identical(v, "")) NA_character_ else v
      }, character(1))
      eng$choices <- ifelse(eng$arg == "censor_shape",
                            paste(names(pp_shape_names), collapse = " | "), NA)
      d <- rbind(d, eng)
    }
    d <- d[d$section != "hidden", , drop = FALSE]
    cbind(type = t, fun = fn, d, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
