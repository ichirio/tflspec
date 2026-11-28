# Registry of plot types, layer patterns and presets.
#
# Each plot type = one base pattern + optional layer patterns. `roles` lists the
# roles of each pattern; TRUE = required. Layers marked `multi` accept several
# role rows (one marker per row).

pp_types <- function() {
  list(
    km = list(
      title = "Kaplan-Meier curve (ggsurvfit)",
      packages = c("dplyr", "ggplot2", "ggsurvfit", "patchwork"),
      roles = list(
        base = c(time = TRUE, censor = TRUE, strata = FALSE)
      ),
      layers = list(
        censor_mark = list(roles = character()),
        ci          = list(roles = character()),
        median_line = list(roles = character()),
        n_at_risk   = list(roles = character())
      ),
      time_roles = list(base = "time")
    ),
    waterfall = list(
      title = "Waterfall plot",
      packages = c("dplyr", "ggplot2", "patchwork"),
      roles = list(
        base = c(id = TRUE, value = TRUE, fill = FALSE)
      ),
      layers = list(
        ref_lines  = list(roles = character()),
        ref_labels = list(roles = character())
      ),
      time_roles = list()
    ),
    swimmer = list(
      title = "Swimmer plot",
      packages = c("dplyr", "ggplot2", "patchwork"),
      roles = list(
        base = c(id = TRUE, end = TRUE, start = FALSE, colour = FALSE),
        assessment_marker = c(x = TRUE, fill = FALSE),
        event_marker = c(x = TRUE)
      ),
      layers = list(
        assessment_marker = list(roles = c("x", "fill")),
        event_marker      = list(roles = "x", multi = TRUE),
        ongoing_arrow     = list(roles = character()),
        visit_grid        = list(roles = character())
      ),
      time_roles = list(base = "end", assessment_marker = "x", event_marker = "x")
    )
  )
}

pp_legend_types <- c("mapped", "manual", "none")
pp_legend_positions <- c(
  "right", "bottom", "top", "left", "below",
  "inside_tr", "inside_tl", "inside_br", "inside_bl"
)
pp_themes <- c("boxed", "L_axis", "minimal", "classic")
pp_glyphs <- c("point", "line", "rect")

#' Colour palette presets
#'
#' Named palettes (e.g. `response`) are matched to data values by name;
#' unnamed palettes are assigned to values in order.
#' @return A named list of character vectors.
#' @export
tfl_fig_palettes <- function() .fs_palettes()

# Shape names accepted in the spec, mapped to ggplot2 point shapes.
pp_shape_names <- c(
  circle = 21, square = 22, diamond = 23, triangle = 24, triangle_down = 25,
  x = 4, plus = 3, dot = 16, solid_square = 15, solid_triangle = 17,
  star = 8, open_circle = 1
)

# Default option values: the generator's own, then the figure style
# standard's (tfl_fig_style(); a type's rows over the blank-type rows).
# `{plot_id}` / `{ds}` are substituted at generation.
pp_default_options <- function(type = NA) {
  base <- list(
    data_expr = "{ds}",
    id_var = "USUBJID",
    fig_path = 'file.path("output", "{plot_id}.png")',
    time_unit = "as_is",
    x_max = "", x_by = "", y_min = "", y_max = "", y_by = "",
    arm_label = "All",
    visit_every = "",
    risk_ard = "",
    legend_title = "",
    legend_width = "0.3",
    legend_hide = ""
  )
  st <- .fs_all(type)
  st <- lapply(st, function(v) if (is.na(v)) "" else v)
  for (k in names(st)) base[[k]] <- st[[k]]
  base
}

# "as_is": AVAL as it is (no conversion); "days": AVAL converted to days
# from its AVALU (tte_time()), the quick figures' time_unit = "days"; a time
# in days other than AVAL is as it is for both
pp_time_divisor <- c(as_is = NA, days = 1, days_to_weeks = 7, days_to_months = 30.4375, days_to_years = 365.25)
# the unit tte_time() is given for each option value
pp_time_unit_name <- c(as_is = "days", days = "days", days_to_weeks = "weeks",
                       days_to_months = "months", days_to_years = "years")
pp_time_axis_label <- c(
  as_is = "Time", days = "Time", days_to_weeks = "Time (Weeks)",
  days_to_months = "Time (Months)", days_to_years = "Time (Years)"
)
