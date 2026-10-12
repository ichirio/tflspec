# The layers a figure design can draw: a catalog, not code.
#
# Each ggplot2 geom (and each geom of an extension package) is a row of
# inst/fig/geoms.csv -- its function, package, label, whether it dodges --
# and its fields are rows of inst/fig/geom_fields.csv: the aesthetics it
# maps (`aes`) and the settings it takes (`param`), each with its kind,
# default and choices.  One generic writer turns any of them into
# `p <- p + fn(data = ..., aes(...), ...)`, so a geom is added by adding
# rows, and a GUI shows its form from the same rows.  tfl_fig_add_layer()
# adds one for the session (a company's own layer, a package not listed).
# Layers that are more than one call (the KM curves, the number at risk)
# stay pieces of their own (R/fig_model.R).

.fig_registry <- new.env(parent = emptyenv())

# A value built from the catalog and the figure style standard, kept until
# either changes: tfl_fig_add_layer() bumps the registry's version, and a
# new style (options(tflspec.fig_style =)) is a new key.
.fig_cached <- function(what, build) {
  key <- list(.fig_registry$version %||% 0L, getOption("tflspec.fig_style"))
  hit <- .fig_registry$cache[[what]]
  if (!is.null(hit) && identical(hit$key, key)) return(hit$value)
  value <- build()
  if (is.null(.fig_registry$cache)) .fig_registry$cache <- list()
  .fig_registry$cache[[what]] <- list(key = key, value = value)
  value
}

.fig_catalog <- function() {
  if (is.null(.fig_registry$geoms)) {
    rd <- function(f) utils::read.csv(
      system.file("fig", f, package = "tflspec", mustWork = TRUE),
      stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
    .fig_registry$geoms <- rd("geoms.csv")
    fl <- rd("geom_fields.csv")
    fl$required <- as.logical(fl$required)
    .fig_registry$fields <- fl
  }
  list(geoms = .fig_registry$geoms, fields = .fig_registry$fields)
}

# the catalog's layers as pieces (see .fig_pieces())
.fig_geom_pieces <- function() {
  cat <- .fig_catalog()
  out <- list()
  for (i in seq_len(nrow(cat$geoms))) {
    g <- cat$geoms[i, ]
    f <- cat$fields[cat$fields$layer == g$layer, , drop = FALSE]
    has_aes <- any(f$role == "aes")
    fields <- rbind(
      if (has_aes) .ff("data", "object", "Data", "df"),
      .ff(f$field, f$kind, f$label, f$default, NA, ifelse(is.na(f$help), "", f$help),
          f$required, ifelse(f$role == "aes", "data", NA)),
      if (identical(g$position, "dodge")) .ff("dodge", "logical", "Dodge", "FALSE"))
    fields$choices[match(f$field, fields$field)] <- f$choices
    out[[g$layer]] <- list(section = "layers", label = g$label,
                           help = if (is.na(g$help)) "" else g$help,
                           fields = fields,
                           geom = list(fn = g$fn, package = g$package,
                                       aes = f$field[f$role == "aes"],
                                       param = f$field[f$role == "param"]))
  }
  out
}

# a setting as R code, by its kind
.fig_param_code <- function(kind, v) {
  switch(kind,
    number = , expr = as.character(v),
    logical = if (isTRUE(as.logical(v))) "TRUE" else "FALSE",
    values = {
      x <- .split_vals(v)
      if (length(x) == 1L) x else sprintf("c(%s)", paste(x, collapse = ", "))
    },
    shape = format(pp_shape_code(v, "dot")),
    q(v))
}

.fig_geom_code <- function(l, k, g, title) {
  f <- .fig_pieces()[[k]]$fields
  v <- function(x) .fv(l, x, k)
  a <- stats::setNames(lapply(g$aes, v), g$aes)
  # lines group by their colour unless told otherwise
  if ("group" %in% g$aes && is.null(a$group) && !is.null(a$colour)) a$group <- a$colour
  # a point that colours by the arms takes their shapes (the standard's)
  if (!is.null(l$.shape_by)) a$shape <- l$.shape_by
  a <- a[!vapply(a, is.null, logical(1))]
  pr <- lapply(setdiff(g$param, if (!is.null(l$.shape_by)) "shape"), function(x) {
    val <- v(x)
    if (is.null(val)) return(NULL)
    paste(x, "=", .fig_param_code(f$kind[f$field == x], val))
  })
  fn <- if (identical(g$package, "ggplot2")) g$fn else paste0(g$package, "::", g$fn)
  args <- c(if ("data" %in% f$field) paste("data =", v("data")),
            if (length(a)) sprintf("aes(%s)", paste(names(a), "=", unlist(a), collapse = ", ")),
            unlist(pr),
            if (.lgl(v("dodge"))) "position = pd")
  list(code = c(title, sprintf("p <- p + %s(%s)", fn, paste(args, collapse = ", "))),
       package = g$package)
}

#' Add a layer to the figure designs' catalog
#'
#' The layers a figure design draws are a catalog: each geom's function and
#' package, the aesthetics it maps and the settings it takes.  tflspec
#' lists ggplot2's common geoms and some of extension packages; this adds
#' one more for the session -- another package's geom, or a company's own
#' layer function -- which [tfl_fig_parts()], [tfl_fig_design_code()] and
#' a GUI then offer like the others.
#'
#' @param layer The layer's name in a design (`layer: <name>`).
#' @param fn,package The function and its package.
#' @param label The name shown.
#' @param aes The aesthetics it maps: a character vector (names = the
#'   aesthetic, values = labels), or a data frame with `field`, `kind`,
#'   `label`, `default`, `choices`, `required`, `help`.
#' @param params The settings, as `aes`; `kind` is one of `number`, `text`,
#'   `choice`, `logical`, `shape`, `values` (several numbers), `expr` (R).
#' @param position `"dodge"` when it can be dodged.
#' @param help A line on what it draws.
#' @return The layer's name, invisibly.
#' @examples
#' tfl_fig_add_layer("beeswarm", "geom_beeswarm", "ggbeeswarm", "Beeswarm",
#'                   aes = c(x = "X", y = "Y", colour = "Colour by"),
#'                   params = data.frame(field = "size", kind = "number",
#'                                       label = "Size", default = "1.5"))
#' @export
tfl_fig_add_layer <- function(layer, fn, package, label = fn, aes = character(),
                              params = NULL, position = NA, help = NA) {
  cat <- .fig_catalog()
  as_fields <- function(x, role) {
    if (is.null(x) || !length(x)) return(NULL)
    if (!is.data.frame(x)) {
      x <- data.frame(field = names(x), kind = if (role == "aes") "variable" else "text",
                      label = unname(x), stringsAsFactors = FALSE)
    }
    for (col in c("default", "choices", "help")) if (is.null(x[[col]])) x[[col]] <- NA
    if (is.null(x$kind)) x$kind <- if (role == "aes") "variable" else "text"
    if (is.null(x$required)) x$required <- FALSE
    data.frame(layer = layer, field = x$field, role = role, kind = x$kind,
               label = x$label, default = as.character(x$default),
               choices = as.character(x$choices), required = as.logical(x$required),
               help = as.character(x$help), stringsAsFactors = FALSE)
  }
  g <- data.frame(layer = layer, fn = fn, package = package, label = label,
                  position = position, help = help, stringsAsFactors = FALSE)
  .fig_registry$version <- (.fig_registry$version %||% 0L) + 1L
  .fig_registry$geoms <- rbind(cat$geoms[cat$geoms$layer != layer, ], g)
  .fig_registry$fields <- rbind(cat$fields[cat$fields$layer != layer, ],
                                as_fields(aes, "aes"), as_fields(params, "param"))
  invisible(layer)
}
