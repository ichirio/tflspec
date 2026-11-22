# ---- figure content functions: the ggplot a figure template names ----------------
#
# A template in code-SPEC form (tflspec_v1/templates/*.yml) names its
# figure content in `figure:` (km_simple -> fig_km, forest_hr ->
# forest_hr).  The content is an R function of what the template's
# analysis step makes -- a model object (`type: model`) and the ARD -- and
# the appearance params; it returns a ggplot (fig_km(): the figure, built).
# tfl_figure_fn() finds it by
# that name (the registry below), as tfl_definition() finds a definition.
#
# The look reuses the figure style standard (tfl_fig_style()): the
# treatment palette, the theme's terms (pp_theme_lines()), the km censor
# mark, the common ADaM subgroup labels (.forest_sub_labels()).

# The registry, one table: the name a template's `figure:` gives -> its
# function, and what a report's program calls it with (tfl_content_code()
# writes that call).  A later figure adds its entry here.
#   fn      the exported function's name
#   model   the model object the function's first argument takes: its name in
#           the program and the call that makes it, with the template's
#           arguments in {braces} (NULL: the function takes the ARD only)
#   params  the template's arguments the function takes
#   direct  TRUE: they are the function's own arguments (fig_km(type = ));
#           FALSE: they go into its `params` list (forest_hr(params = ))
.fig_content_table <- function() {
  list(
    fig_km = list(
      fn = "fig_km",
      direct = TRUE,
      model = list(
        arg = "km_fit",
        call = paste0("ggsurvfit::survfit2(ggsurvfit::Surv_CNSR({time}, {censor}) ~ {by}, ",
                      "data = adtte_sub)")),
      params = c("time_unit", "x_label", "y_label", "legend")),
    forest_hr = list(
      fn = "forest_hr",
      direct = FALSE,
      model = NULL,
      params = c("comparison", "subgroup_labels", "favours", "x_breaks", "size_by_n",
                 "bold_headings", "n_by_arm")))
}

# The names a figure content's model call reads ({by}, {time}, {censor})
.fig_model_args <- function(e) {
  if (is.null(e$model)) return(character())
  m <- regmatches(e$model$call, gregexpr("[{][A-Za-z_.]+[}]", e$model$call))[[1L]]
  unique(gsub("[{}]", "", m))
}

# A figure report's content key, `figure: {name, args}`: the content
# function's name (a name of the table) and its arguments -- the model's
# ({by}, {time}, {censor}) then the function's `params`.  A report that has
# it as a list has it as written; a report resolved from a series template
# (`figure:` a name, the template's arguments in its `args` attribute) gets
# it from the arguments.  NULL for a report with neither.
.fig_content_key <- function(output) {
  fig <- output$figure
  if (is.null(fig)) return(NULL)
  tb <- .fig_content_table()
  if (is.list(fig)) {
    name <- fig$name
    args <- fig$args %||% list()
  } else {
    name <- fig
    args <- NULL
  }
  if (!is.character(name) || length(name) != 1L || is.na(name) || !name %in% names(tb)) {
    stop(sprintf("%s: the figure `%s` is not a figure content; one of %s.", output$id %||% "report",
                 paste(format(name), collapse = " "), paste(names(tb), collapse = ", ")), call. = FALSE)
  }
  if (is.null(args)) {
    given <- attr(output, "args") %||% list()
    keep <- c(.fig_model_args(tb[[name]]), tb[[name]]$params)
    args <- Filter(Negate(is.null), given[intersect(keep, names(given))])
  }
  list(name = name, args = args)
}

# The registry's functions by name
.fig_content_fns <- function() {
  tb <- .fig_content_table()
  stats::setNames(lapply(tb, function(e) get(e$fn, envir = asNamespace("tflspec"), mode = "function")),
                  names(tb))
}

#' The figure content function a template names
#'
#' A figure template in code-SPEC form names its content -- the ggplot
#' drawn from what its analysis step makes -- in its `figure:` key:
#' `km_simple` names `fig_km`, `forest_hr` names `forest_hr`.  This
#' returns the R function of that name, as `tfl_definition()` returns a
#' definition.
#'
#' @param name The name a template gives in `figure:`.
#' @return The function: [fig_km()] or [forest_hr()].
#' @seealso [fig_km()], [forest_hr()]
#' @examples
#' tfl_figure_fn("fig_km")
#' @export
tfl_figure_fn <- function(name) {
  fns <- .fig_content_fns()
  if (!is.character(name) || length(name) != 1L || is.na(name) || !name %in% names(fns)) {
    stop(sprintf("Unknown figure '%s': one of %s.", paste(format(name), collapse = " "),
                 paste(names(fns), collapse = ", ")), call. = FALSE)
  }
  fns[[name]]
}

# ---- helpers ------------------------------------------------------------------------

# The params over their defaults; a name the function does not take stops
.fig_fn_params <- function(params, defaults, fn) {
  params <- as.list(params %||% list())
  bad <- setdiff(names(params), names(defaults))
  if (length(params) && (is.null(names(params)) || any(!nzchar(names(params))))) {
    stop(sprintf("%s(): `params` is a named list.", fn), call. = FALSE)
  }
  if (length(bad)) {
    stop(sprintf("%s(): unknown params %s; it takes %s.", fn, paste(bad, collapse = ", "),
                 paste(names(defaults), collapse = ", ")), call. = FALSE)
  }
  for (k in names(params)) if (!is.null(params[[k]])) defaults[[k]] <- params[[k]]
  defaults
}

.fig_fn_need <- function(pkgs, fn) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    stop(sprintf("%s() needs the package(s) %s.", fn, paste(miss, collapse = ", ")), call. = FALSE)
  }
}

# A column of an ARD as character: a list column's first value, NA for NULL
.fig_fn_chr <- function(x) {
  if (!is.list(x)) return(as.character(x))
  vapply(x, function(v) if (length(v) && !is.null(v[[1L]])) as.character(v[[1L]]) else NA_character_, "")
}

.fig_fn_num <- function(x) {
  if (!is.list(x)) return(suppressWarnings(as.numeric(x)))
  vapply(x, function(v) {
    if (length(v) && !is.null(v[[1L]])) suppressWarnings(as.numeric(v[[1L]])) else NA_real_
  }, 0)
}

# An analysis's rows of the ARD (all rows when it has no analysis_id)
.fig_fn_rows <- function(ard, id) {
  ard <- as.data.frame(ard, stringsAsFactors = FALSE)
  if ("analysis_id" %in% names(ard)) ard <- ard[.fig_fn_chr(ard$analysis_id) %in% id, , drop = FALSE]
  ard
}

# The theme of the figure style standard (its terms as the generated code
# writes them), evaluated with ggplot2
.fig_fn_theme <- function(theme = "boxed", base_size = NULL) {
  terms <- lapply(pp_theme_lines(theme, base_size),
                  function(s) eval(str2lang(s), envir = asNamespace("ggplot2")))
  Reduce(`+`, terms[-1L], terms[[1L]])
}

# Each level's legend text: the level and its N from BIGN ("Drug X (N=20)")
# (the rows of the by variable, when the ARD names its variable)
.km_legend_labels <- function(lv, ard, by = NULL) {
  if (is.null(ard) || !NROW(ard)) return(lv)
  b <- .fig_fn_rows(ard, "BIGN")
  if (!all(c("variable_level", "stat_name", "stat") %in% names(b))) return(lv)
  keep <- .fig_fn_chr(b$stat_name) == "n"
  if (!is.null(by) && "variable" %in% names(b)) keep <- keep & .fig_fn_chr(b$variable) == by
  b <- b[keep, , drop = FALSE]
  n <- stats::setNames(.fig_fn_num(b$stat), .fig_fn_chr(b$variable_level))
  n <- n[!duplicated(names(n))]
  ifelse(lv %in% names(n), sprintf("%s (N=%s)", lv, format(n[lv], trim = TRUE, scientific = FALSE)), lv)
}

# ---- forest_hr ----------------------------------------------------------------------

#' Hazard ratio by subgroup: the figure content of forest_hr
#'
#' The ggplot of the template `forest_hr` (its `figure: forest_hr`): a
#' forest plot of the ARD's `HR` -- `cardx::ard_regression()` of a Cox
#' model, exponentiated, one per condition row (the rows tagged
#' `group1 = "subgroup"`, `group1_level = <subgroup>`, `group2 =
#' "subgroup_lvl"`, `group2_level = <level>`; the first, ALL/ALL, the
#' overall).  A row a condition, in the ARD's order: the hazard ratio of
#' the comparison arm and its interval, a dashed line at 1; a heading row
#' above each subgroup's levels.  At the right: N (the model's subjects)
#' and the hazard ratio (95% CI); an interval that cannot be estimated is
#' `NE`.
#'
#' @param ard The ARD: the rows of `analysis_id == "HR"` (all rows when
#'   the ARD has no `analysis_id`), stats `estimate`, `conf.low`,
#'   `conf.high`, `n_obs` (and `n_event` for `n_by_arm`).
#' @param params The appearance params, a named list; an unset one takes
#'   its default:
#'   - `comparison`: the arm shown (`NULL`: the first arm with an interval,
#'     not the reference).
#'   - `subgroup_labels`: the headings, named by the subgroup
#'     (`list(SEX = "Sex")`; `NULL`: the common ADaM variables' labels,
#'     else the subgroup's name).
#'   - `favours`: two labels under the axis, left and right of 1
#'     (`c("Favours Drug X", "Favours Control")`; `NULL`: none).
#'   - `x_breaks`: the x axis's breaks (`NULL`: ggplot2's).
#'   - `size_by_n`: `TRUE`: a point's size by its N (`FALSE`).
#'   - `bold_headings`: `TRUE`: the headings bold (`FALSE`).
#'   - `n_by_arm`: arms whose events / subjects are shown, a column each
#'     (`NULL`: none).
#' @return A ggplot.
#' @seealso [tfl_figure_fn()], [fig_km()]
#' @examplesIf requireNamespace("cardx", quietly = TRUE) && requireNamespace("survival", quietly = TRUE) && requireNamespace("broom.helpers", quietly = TRUE)
#' set.seed(1)
#' d <- data.frame(AVAL = rexp(60, 0.1), CNSR = rbinom(60, 1, 0.3),
#'                 ARM = rep(c("Control", "Drug X"), 30))
#' hr <- cardx::ard_regression(survival::coxph(survival::Surv(AVAL, 1 - CNSR) ~ ARM, data = d),
#'                             exponentiate = TRUE)
#' forest_hr(hr, list(favours = c("Favours Drug X", "Favours Control")))
#' @export
forest_hr <- function(ard, params = list()) {
  p <- .fig_fn_params(params, list(comparison = NULL, subgroup_labels = NULL, favours = NULL,
                                   x_breaks = NULL, size_by_n = FALSE, bold_headings = FALSE,
                                   n_by_arm = NULL), "forest_hr")
  if (!is.null(p$favours) && length(p$favours) != 2L) {
    stop("forest_hr(): `favours` is two labels, left and right of 1.", call. = FALSE)
  }
  est <- .forest_hr_rows(ard, p$comparison, p$n_by_arm)
  rows <- .forest_hr_layout(est, p$subgroup_labels, p$n_by_arm)
  pts <- rows[!rows$head, , drop = FALSE]
  # the axis's labels, a heading row bold (plotmath: bquote() keeps the
  # text; a phantom keeps a level's indent)
  y_labels <- if (isTRUE(p$bold_headings)) {
    as.expression(Map(function(l, h) {
      if (h) bquote(bold(.(l))) else if (startsWith(l, " ")) bquote(phantom("MM") * .(trimws(l))) else l
    }, rows$label, rows$head))
  } else rows$label
  # the text columns' heading: a row above the top row
  top <- max(rows$y) + 1
  y_labels <- c(y_labels, "")
  base_size <- as.numeric(pp_default_options("forest")$base_size %||% 10)
  g <- ggplot2::ggplot(pts, ggplot2::aes(x = .data$estimate, y = .data$y)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$conf.low, xmax = .data$conf.high),
                           orientation = "y", width = 0.25, na.rm = TRUE)
  g <- g + if (isTRUE(p$size_by_n)) {
    ggplot2::geom_point(ggplot2::aes(size = .data$n_obs), shape = 15, na.rm = TRUE)
  } else {
    ggplot2::geom_point(shape = 15, size = 2.5, na.rm = TRUE)
  }
  if (isTRUE(p$size_by_n)) g <- g + ggplot2::scale_size_area(max_size = 4, guide = "none")
  if (!is.null(p$favours)) {
    g <- g + ggplot2::annotate("text", x = c(1, 1), y = 0, label = paste0(c("", "  "), p$favours, c("  ", "")),
                               hjust = c(1, 0), size = base_size / ggplot2::.pt * 0.8)
  }
  # the x axis even about 1 (log): the favours' labels a half each
  m <- max(abs(log(c(pts$conf.low, pts$conf.high, p$x_breaks, 0.5, 2))), na.rm = TRUE) * 1.05
  g +
    ggplot2::scale_x_log10(breaks = p$x_breaks %||% ggplot2::waiver(), limits = exp(c(-m, m))) +
    ggplot2::scale_y_continuous(breaks = c(rows$y, top), labels = y_labels,
                                limits = c(if (is.null(p$favours)) 0.5 else -0.2, top + 0.3),
                                expand = ggplot2::expansion(add = 0.1),
                                sec.axis = ggplot2::dup_axis(breaks = c(rows$y, top),
                                                             labels = c(rows$txt, attr(rows, "heading")),
                                                             name = NULL)) +
    ggplot2::labs(x = "Hazard Ratio (95% CI)", y = NULL) +
    .fig_fn_theme("boxed", base_size) +
    ggplot2::theme(axis.text.y.left = ggplot2::element_text(hjust = 0),
                   axis.text.y.right = ggplot2::element_text(hjust = 0, family = "mono"),
                   legend.position = "none")
}

# The HR rows wide: a row a condition (ARD order), the comparison arm's
# estimate, interval, the model's N, and each n_by_arm arm's events/subjects
.forest_hr_rows <- function(ard, comparison = NULL, n_by_arm = NULL) {
  a <- .fig_fn_rows(ard, "HR")
  need <- c("variable_level", "stat_name", "stat")
  if (!NROW(a) || !all(need %in% names(a))) {
    stop("forest_hr(): the ARD has no HR rows (variable_level, stat_name, stat).", call. = FALSE)
  }
  sub <- if ("group1_level" %in% names(a)) .fig_fn_chr(a$group1_level) else rep(NA_character_, nrow(a))
  lvl <- if ("group2_level" %in% names(a)) .fig_fn_chr(a$group2_level) else rep(NA_character_, nrow(a))
  sub[is.na(sub)] <- "ALL"
  lvl[is.na(lvl)] <- "ALL"
  cond <- paste(sub, lvl, sep = "\r")
  conds <- unique(cond)
  arm <- .fig_fn_chr(a$variable_level)
  sn <- .fig_fn_chr(a$stat_name)
  val <- .fig_fn_num(a$stat)
  get <- function(cnd, a_lv, s) {
    v <- val[cond == cnd & arm %in% a_lv & sn == s]
    if (length(v)) v[[1L]] else NA_real_
  }
  # the arms compared with the reference: those with an interval
  arms <- unique(arm[!is.na(arm) & sn == "conf.low" & !is.na(val)])
  if (is.null(comparison)) {
    if (!length(arms)) stop("forest_hr(): the HR rows have no arm with an interval.", call. = FALSE)
    comparison <- arms[[1L]]
  } else if (!comparison %in% arms) {
    stop(sprintf("forest_hr(): `comparison` (%s) is the reference arm or not an arm of the HR rows; one of %s.",
                 comparison, paste(arms, collapse = ", ")), call. = FALSE)
  }
  out <- data.frame(
    subgroup = sub[match(conds, cond)], level = lvl[match(conds, cond)],
    estimate = vapply(conds, get, 0, a_lv = comparison, s = "estimate", USE.NAMES = FALSE),
    conf.low = vapply(conds, get, 0, a_lv = comparison, s = "conf.low", USE.NAMES = FALSE),
    conf.high = vapply(conds, get, 0, a_lv = comparison, s = "conf.high", USE.NAMES = FALSE),
    # the model's subjects: its arms' n_obs together
    n_obs = vapply(conds, function(cnd) {
      v <- val[cond == cnd & !is.na(arm) & sn == "n_obs"]
      if (length(v)) sum(v) else NA_real_
    }, 0, USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
  for (i in seq_along(n_by_arm)) {
    ev <- vapply(conds, get, 0, a_lv = n_by_arm[[i]], s = "n_event", USE.NAMES = FALSE)
    n <- vapply(conds, get, 0, a_lv = n_by_arm[[i]], s = "n_obs", USE.NAMES = FALSE)
    out[[paste0("n_arm", i)]] <- ifelse(is.na(n), "", sprintf("%s/%s", ev, n))
  }
  ok <- is.finite(out$estimate) & is.finite(out$conf.low) & is.finite(out$conf.high) & out$conf.high < 1000
  out$txt <- ifelse(ok, sprintf("%.2f (%.2f, %.2f)", out$estimate, out$conf.low, out$conf.high), "NE")
  out[!ok, c("estimate", "conf.low", "conf.high")] <- NA_real_
  attr(out, "comparison") <- comparison
  out
}

# The rows drawn: a heading above each subgroup's levels (the levels
# indented), the overall row "All subjects"; y from the top
.forest_hr_layout <- function(est, subgroup_labels = NULL, n_by_arm = NULL) {
  all <- est$subgroup == "ALL"
  subs <- unique(est$subgroup[!all])
  lab <- .forest_sub_labels(subs, subgroup_labels)
  heading <- function(s) if (s %in% names(lab)) lab[[s]] else s
  arm_cols <- if (length(n_by_arm)) paste0("n_arm", seq_along(n_by_arm))
  pieces <- list()
  for (i in seq_len(nrow(est))) {
    r <- est[i, , drop = FALSE]
    is_all <- r$subgroup == "ALL"
    first <- !is_all && !any(est$subgroup[seq_len(i - 1L)] == r$subgroup)
    if (first) {
      h <- r
      h[, c("estimate", "conf.low", "conf.high", "n_obs")] <- NA
      h$label <- heading(r$subgroup)
      h$txt <- ""
      h$head <- TRUE
      pieces[[length(pieces) + 1L]] <- h
    }
    r$label <- if (is_all) "All subjects" else paste0("    ", r$level)
    r$head <- FALSE
    pieces[[length(pieces) + 1L]] <- r
  }
  rows <- do.call(rbind, pieces)
  rows$y <- rev(seq_len(nrow(rows)))
  rownames(rows) <- NULL
  # the text columns at the right, each as wide as its widest (a mono font):
  # N, each n_by_arm arm's events/subjects, the hazard ratio (95% CI)
  cols <- c(list(N = ifelse(is.na(rows$n_obs), "", format(rows$n_obs, trim = TRUE))),
            stats::setNames(lapply(arm_cols, function(k) ifelse(rows$head, "", rows[[k]])),
                            if (length(n_by_arm)) paste(n_by_arm, "E/N") else character()),
            list(`HR (95% CI)` = rows$txt))
  pad <- function(x, w) formatC(x, width = -w)
  w <- vapply(names(cols), function(k) max(nchar(c(k, cols[[k]]))), 0)
  rows$txt <- do.call(paste, c(Map(pad, cols, w), sep = "  "))
  attr(rows, "heading") <- paste(unlist(Map(pad, names(cols), w)), collapse = "  ")
  rows
}
