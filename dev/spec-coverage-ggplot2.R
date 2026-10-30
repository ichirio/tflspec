# How much of ggplot2 the figure SPEC can write: one row a function's
# argument, its status, where tflspec says it.  Writes
# inst/dev/spec-coverage-ggplot2.csv and prints the weighted percentages
# for inst/dev/spec-coverage.md.
#
#   Rscript dev/spec-coverage-ggplot2.R [out.csv]
#
# Run with the tflspec to measure installed (library(tflspec)).
#
# Status of an argument:
#   yes      the function has a card in the catalog (inst/spec/schema.yml)
#            and the argument is typed and labelled there or by its kind
#            (inst/fig/gg_args.csv): the GUI shows it, the check types it
#   partial  written and accepted by the check, but untyped or the function
#            has no card (written from its formals)
#   no       the check does not know it (an error or "no argument")
# A function's coverage is its arguments' mean (yes 1, partial 0.5); the
# percentage is the functions' mean weighted by their use in clinical
# figures (3 KM / forest / waterfall / swimmer / box / mean +/- SE / bar /
# scatter / heat map / CDF, 2 sometimes, 1 rarely).  The extension
# packages tflspec suggests (ext_scope: ggsurvfit, ggrepel, ggh4x, ggtext,
# ggnewscale, patchwork) are measured the same way and printed apart; the
# csv's `package` says which.

suppressPackageStartupMessages({
  library(tflspec)
  library(ggplot2)
})
ns <- asNamespace("tflspec")
`%||%` <- function(a, b) if (is.null(a)) b else a

scope <- list(
  geom = c(geom_line = 3, geom_point = 3, geom_errorbar = 3, geom_pointrange = 3, geom_col = 3,
           geom_bar = 3, geom_step = 3, geom_boxplot = 3, geom_jitter = 3, geom_text = 3,
           geom_segment = 3, geom_hline = 3, geom_vline = 3, geom_ribbon = 3, geom_tile = 3,
           geom_violin = 2, geom_label = 2, geom_area = 2, geom_rect = 2, geom_linerange = 2,
           geom_crossbar = 2, geom_histogram = 2, geom_density = 2, geom_smooth = 2,
           geom_abline = 2, geom_path = 1, geom_rug = 1, geom_freqpoly = 1, geom_dotplot = 1,
           geom_raster = 1, geom_polygon = 1, geom_curve = 1, geom_count = 1, geom_qq = 1,
           geom_qq_line = 1, geom_blank = 1, geom_spoke = 1, geom_quantile = 1, geom_contour = 1,
           geom_density_2d = 1, geom_bin_2d = 1, geom_function = 1,
           geom_contour_filled = 1, geom_density_2d_filled = 1),
  # a geom drawn by another stat: the stat's parameters
  geom_stat = c("geom_step|ecdf" = 3, "geom_pointrange|summary" = 3, "geom_errorbar|summary" = 3,
                "geom_line|summary" = 3, "geom_point|summary" = 3, "geom_col|summary" = 2,
                "geom_ribbon|summary" = 2, "geom_path|ellipse" = 1, "geom_point|unique" = 1),
  stat = c(stat_ecdf = 3, stat_summary = 3, stat_summary_bin = 2, stat_ellipse = 2, stat_unique = 1,
           stat_smooth = 1, stat_bin = 1, stat_count = 1, stat_boxplot = 1, stat_density = 1,
           stat_ydensity = 1, stat_identity = 1, stat_function = 1, stat_qq = 1, stat_qq_line = 1,
           stat_sum = 1),
  scale = c(scale_x_continuous = 3, scale_y_continuous = 3, scale_x_discrete = 3, scale_y_discrete = 3,
            scale_x_log10 = 3, scale_y_log10 = 3, scale_colour_manual = 3, scale_fill_manual = 3,
            scale_shape_manual = 3, scale_linetype_manual = 3, scale_x_reverse = 2, scale_y_reverse = 2,
            scale_x_date = 2, scale_colour_brewer = 2, scale_fill_brewer = 2, scale_colour_gradient = 2,
            scale_fill_gradient = 2, scale_colour_gradient2 = 2, scale_fill_gradient2 = 2,
            scale_x_sqrt = 1, scale_y_sqrt = 1, scale_y_date = 1, scale_x_datetime = 1,
            scale_colour_distiller = 1, scale_fill_distiller = 1, scale_colour_gradientn = 1,
            scale_fill_gradientn = 1, scale_colour_viridis_d = 1, scale_fill_viridis_d = 1,
            scale_colour_viridis_c = 1, scale_fill_viridis_c = 1, scale_colour_grey = 1,
            scale_fill_grey = 1, scale_colour_hue = 1, scale_fill_hue = 1, scale_colour_identity = 1,
            scale_fill_identity = 1, scale_shape = 1, scale_linetype = 1, scale_size = 1,
            scale_size_manual = 1, scale_size_area = 1, scale_alpha = 1, scale_alpha_manual = 1,
            scale_linewidth = 2, scale_linewidth_manual = 2, scale_fill_steps2 = 2,
            scale_colour_discrete = 1, scale_fill_discrete = 1, scale_colour_continuous = 1,
            scale_fill_continuous = 1, scale_x_binned = 1, scale_colour_steps = 1,
            scale_fill_steps = 1, scale_colour_steps2 = 1, scale_colour_fermenter = 1,
            scale_fill_fermenter = 1, scale_x_time = 1,
            expansion = 3, sec_axis = 2, dup_axis = 1),
  coord = c(coord_cartesian = 3, coord_flip = 3, coord_trans = 2, coord_fixed = 1, coord_equal = 1,
            coord_polar = 1, coord_radial = 1),
  theme = c(theme = 3, theme_bw = 3, theme_classic = 3, theme_minimal = 2, theme_light = 1,
            theme_linedraw = 1, theme_gray = 1, theme_void = 1, theme_dark = 1, element_text = 3,
            element_line = 3, element_rect = 3, element_blank = 3, margin = 2),
  facet = c(facet_wrap = 3, facet_grid = 3),
  guide = c(guides = 3, guide_legend = 3, guide_colourbar = 2, guide_axis = 2, guide_none = 2,
            guide_bins = 1, guide_coloursteps = 1, guide_axis_logticks = 2,
            guide_axis_theta = 1, guide_axis_stack = 1),
  labs = c(labs = 3, xlab = 1, ylab = 1, ggtitle = 1, annotate = 3),
  position = c(position_dodge = 3, position_dodge2 = 2, position_jitter = 2, position_jitterdodge = 2,
               position_nudge = 2, position_stack = 2, position_fill = 2, position_identity = 1)
)

# the extension packages tflspec suggests: a function of a plot each, by
# the same weights (ggsurvfit's KM curves 3; patchwork's arrangement 3)
ext_scope <- list(
  ggsurvfit = c(ggsurvfit = 3, ggcuminc = 2, add_risktable = 3, add_censor_mark = 3,
                add_confidence_interval = 3, add_quantile = 2, add_pvalue = 3, add_legend_title = 2,
                add_risktable_strata_symbol = 2, scale_ggsurvfit = 3, theme_ggsurvfit_default = 2,
                theme_ggsurvfit_KMunicate = 2, theme_risktable_default = 2, theme_risktable_boxed = 2,
                stat_stepribbon = 1),
  ggrepel = c(geom_text_repel = 3, geom_label_repel = 2, position_nudge_repel = 1),
  ggh4x = c(facet_nested = 2, facet_nested_wrap = 2, facet_grid2 = 2, facet_wrap2 = 2,
            facetted_pos_scales = 2, force_panelsizes = 2, strip_nested = 2, strip_themed = 1,
            strip_vanilla = 1, elem_list_text = 1, elem_list_rect = 1),
  ggtext = c(element_markdown = 3, element_textbox_simple = 2, element_textbox = 1,
             geom_richtext = 2, geom_textbox = 1),
  ggnewscale = c(new_scale_colour = 2, new_scale_fill = 2, new_scale = 1),
  patchwork = c(wrap_plots = 3, plot_layout = 3, plot_annotation = 3, plot_spacer = 2,
                wrap_elements = 2, inset_element = 1, guide_area = 1, area = 1, free = 1)
)

# arguments no one writes in a SPEC: deprecated, internal
drop_args <- c("...", "fun.y", "fun.ymin", "fun.ymax", "flipped_aes", "super", "call", "scale_name",
               "trans", "size", "fatten", "draw_quantiles", "legend.text.align", "legend.title.align",
               "complete", "validate", "label.size",
               # a geom's parameters its constructor makes from the per-part
               # arguments (geom_boxplot(whisker.colour =) -> whisker_gp)
               "box_gp", "median_gp", "middle_gp", "whisker_gp", "staple_gp", "outlier_gp",
               "quantile_gp",
               # ggrepel's geom parameter its layer fills itself
               "panel_scales")

layer_args <- function(fn, pkg = "ggplot2") {
  f <- getExportedValue(pkg, fn)
  l <- tryCatch(suppressWarnings(suppressMessages(f())), error = function(e) NULL)
  a <- names(formals(f))
  if (inherits(l, "Layer")) a <- c(a, l$geom$parameters(TRUE), l$stat$parameters(TRUE))
  setdiff(unique(a), c(drop_args, "mapping", "data"))
}
fn_args <- function(fn, pkg = "ggplot2") {
  if (grepl("^(geom|stat)_", fn)) return(c("mapping", "data", layer_args(fn, pkg)))
  a <- names(formals(getExportedValue(pkg, fn)))
  # patchwork::free(x): x is the plot freed, as a geom's data
  if (identical(fn, "free")) a <- setdiff(a, "x")
  if (!identical(pkg, "ggplot2")) return(setdiff(unique(a), c(drop_args, "data", "mapping")))
  if (identical(fn, "labs")) a <- c(a, "x", "y", "colour", "fill", "shape", "linetype")
  if (startsWith(fn, "scale_") && "..." %in% a) {
    sc <- tryCatch(suppressWarnings(suppressMessages(getExportedValue("ggplot2", fn)())), error = function(e) NULL)
    ctor <- if (inherits(sc, "ScaleBinned")) "binned_scale" else
      if (inherits(sc, "ScaleContinuous") || grepl("gradientn", fn)) "continuous_scale" else "discrete_scale"
    a <- c(a, names(formals(getExportedValue("ggplot2", ctor))))
  }
  if (identical(fn, "coord_trans") && identical(a, "...")) a <- names(formals(ggplot2::coord_transform))
  if (identical(fn, "guides")) a <- c("colour", "fill", "shape", "linetype", "x", "y")
  setdiff(unique(a), drop_args)
}

file_entries <- get(".schema_all", ns)(figure = FALSE)
gg_args <- tryCatch(get(".fig_gg_args", ns)(), error = function(e) NULL)

status_of <- function(fn, arg, base_args = list(), card_name = fn) {
  args <- base_args
  if (nzchar(arg)) args[[arg]] <- tfl_r("NULL")
  p <- tryCatch(tfl_schema_check(tfl_item(fn, args)), error = function(e) {
    data.frame(arg = arg, message = conditionMessage(e), level = "error")
  })
  unknown <- grepl("no argument|neither the schema|no ggplot2 layer|no parameter", p$message)
  if (any(unknown)) return(list(status = "no", where = "", note = p$message[unknown][1L]))
  card <- file_entries[[card_name]]
  has_card <- !is.null(card$figure) && !is.null(card$rank)
  ent <- tryCatch(tfl_schema(fn), error = function(e) NULL)
  a <- if (nzchar(arg)) ent$args[[arg]]
  # a stat's parameter given to a geom: the stat's entry types it
  if (nzchar(arg) && is.null(a$label) && length(base_args) && exists(".schema_item_entry", ns)) {
    ent2 <- get(".schema_item_entry", ns)(ent, fn, args)
    a <- ent2$args[[arg]]
  }
  typed <- !nzchar(arg) || !is.null(a$label) || !is.null(card$args[[arg]]$type) ||
    (identical(arg, "mapping") && !is.null(card$aes))
  from_csv <- nzchar(arg) && !is.null(gg_args) && arg %in% gg_args$arg && is.null(card$args[[arg]])
  where <- if (has_card) paste0("schema.yml: ", card_name) else "formals (no card)"
  if (nzchar(arg) && typed && from_csv) where <- paste0(where, "; gg_args.csv")
  st <- if (has_card && typed) "yes" else "partial"
  list(status = st, where = where, note = "")
}

rows <- list()
add <- function(group, fn, arg, weight, s, note = "", package = "ggplot2") {
  rows[[length(rows) + 1L]] <<- data.frame(
    package = package, group = group, `function` = fn, argument = arg, weight = weight,
    covered = s$status, where = s$where, note = if (nzchar(s$note)) s$note else note,
    check.names = FALSE, stringsAsFactors = FALSE)
}
for (g in names(scope)) {
  w <- scope[[g]]
  for (key in names(w)) {
    if (identical(g, "geom_stat")) {
      fn <- sub("\\|.*$", "", key)
      st <- sub("^.*\\|", "", key)
      extra <- setdiff(layer_args(paste0("stat_", st)), c(layer_args(fn), "geom", "position",
                                                          "show.legend", "inherit.aes"))
      for (a in extra) {
        add(g, sprintf("%s(stat = \"%s\")", fn, st), a, w[[key]],
            status_of(fn, a, list(stat = st)), "the stat's parameter on the geom")
      }
      next
    }
    a <- fn_args(key)
    if (!length(a)) a <- ""
    for (x in a) add(g, key, x, w[[key]], status_of(key, x))
  }
}
for (pkg in names(ext_scope)) {
  if (!requireNamespace(pkg, quietly = TRUE)) next
  w <- ext_scope[[pkg]]
  for (key in names(w)) {
    # a function with a card is named bare (its card says the package),
    # any other pkg::fn
    item_fn <- if (!is.null(file_entries[[key]]$figure)) key else paste0(pkg, "::", key)
    a <- fn_args(key, pkg)
    if (!length(a)) a <- ""
    for (x in a) add(pkg, key, x, w[[key]], status_of(item_fn, x, card_name = key), package = pkg)
  }
}
cov <- do.call(rbind, rows)
cov$score <- c(yes = 1, partial = 0.5, no = 0)[cov$covered]

pct <- function(d) {
  f <- stats::aggregate(score ~ `function` + weight, data = d, FUN = mean)
  round(100 * sum(f$score * f$weight) / sum(f$weight), 1)
}
ext <- cov[cov$package != "ggplot2", , drop = FALSE]
cov_all <- cov
cov <- cov[cov$package == "ggplot2", , drop = FALSE]
main <- cov[cov$group != "stat", , drop = FALSE]
cat(sprintf("ggplot2 (geoms with their stats, scales, coords, themes, facets, guides, labs, positions): %.1f%%\n",
            pct(main)))
cat(sprintf("stat_*(): %.1f%%\n", pct(cov[cov$group == "stat", , drop = FALSE])))
for (g in names(scope)) cat(sprintf("  %-9s %5.1f%%\n", g, pct(cov[cov$group == g, , drop = FALSE])))
cat("arguments:", nrow(cov), " yes:", sum(cov$covered == "yes"), " partial:", sum(cov$covered == "partial"),
    " no:", sum(cov$covered == "no"), "\n")

if (nrow(ext)) {
  cat(sprintf("extensions: %.1f%%\n", pct(ext)))
  for (pkg in unique(ext$package)) cat(sprintf("  %-10s %5.1f%%\n", pkg, pct(ext[ext$package == pkg, ])))
  cat("arguments:", nrow(ext), " yes:", sum(ext$covered == "yes"), " partial:",
      sum(ext$covered == "partial"), " no:", sum(ext$covered == "no"), "\n")
}
cov <- cov_all

out <- commandArgs(TRUE)[1L]
if (!is.na(out)) {
  cov <- cov[, c("package", "function", "argument", "covered", "where", "note", "group", "weight")]
  utils::write.csv(cov, out, row.names = FALSE, fileEncoding = "UTF-8")
}
