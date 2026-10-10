# Spec sheets and their columns. Every sheet is optional except `plots`.
pp_sheet_cols <- list(
  plots = c(
    "plot_id", "plot_type", "dataset", "layers", "title", "x_label", "y_label",
    "theme", "palette", "legend_type", "legend_pos", "width", "height", "units", "dpi"
  ),
  roles   = c("plot_id", "layer", "role", "dataset", "variable", "label", "shape", "colour"),
  filters = c("plot_id", "layer", "dataset", "variable", "value"),
  levels  = c("plot_id", "variable", "value", "label", "order", "colour"),
  legend  = c("plot_id", "order", "label", "glyph", "shape", "colour", "fill", "linetype"),
  options = c("plot_id", "key", "value")
)

#' Read ADaM datasets
#'
#' @param x A folder containing `.xpt`, `.sas7bdat`, `.rds` or `.rda`
#'   (`.RData`, one dataset a file) files, or a named list of data frames.
#' @return A named list of data frames; names are upper-case dataset names.
#' @export
tfl_read_adam <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    if (is.null(names(x)) || any(names(x) == "")) stop("`x` must be a named list.", call. = FALSE)
    out <- lapply(x, as.data.frame)
    names(out) <- toupper(names(x))
    return(out)
  }
  if (!is.character(x) || length(x) != 1 || !dir.exists(x)) {
    stop("`x` must be a folder or a named list of data frames.", call. = FALSE)
  }
  files <- list.files(x, pattern = "\\.(xpt|sas7bdat|rds|rda|rdata)$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) stop("No .xpt / .sas7bdat / .rds / .rda files in ", x, call. = FALSE)
  out <- lapply(files, function(f) {
    ext <- tolower(tools::file_ext(f))
    switch(ext,
      rds = readRDS(f),
      xpt = {
        .ard_need("haven", "reading an .xpt file")
        haven::read_xpt(f)
      },
      sas7bdat = {
        .ard_need("haven", "reading a .sas7bdat file")
        haven::read_sas(f)
      },
      rda = , rdata = local({
        e <- new.env()
        load(f, envir = e)
        e[[ls(e)[1L]]]
      })
    )
  })
  names(out) <- toupper(tools::file_path_sans_ext(basename(files)))
  lapply(out, as.data.frame)
}

# Variables whose distinct values are offered as a codelist.
pp_codelist_vars <- function(df, max_levels) {
  keep <- vapply(df, function(v) {
    (is.character(v) || is.factor(v) || (is.numeric(v) && all(v == round(v), na.rm = TRUE))) &&
      length(unique(v[!is.na(v) & v != ""])) %in% seq_len(max_levels)
  }, logical(1))
  names(df)[keep]
}

pp_var_label <- function(v) {
  lab <- attr(v, "label", exact = TRUE)
  if (is.null(lab)) "" else as.character(lab)
}

# Distinct values in display order: factor levels, else paired numeric
# variable (e.g. TRT01P ordered by TRT01PN), else sorted.
pp_values <- function(df, var) {
  v <- df[[var]]
  if (is.factor(v)) return(levels(droplevels(v)))
  u <- unique(v[!is.na(v) & v != ""])
  n_var <- paste0(var, "N")
  if (n_var %in% names(df) && is.numeric(df[[n_var]])) {
    key <- unique(df[!is.na(v) & v != "", c(var, n_var)])
    key <- key[order(key[[n_var]]), ]
    return(as.character(unique(key[[var]])))
  }
  as.character(sort(u))
}

#' Write an Excel spec template with drop-down lists built from ADaM data
#'
#' Datasets, variables and codelist values offered in the drop-downs come from
#' `adam`. Variables that depend on a dataset pick the list of that dataset
#' (the row's `dataset`, or the plot's `dataset` when blank).
#'
#' @param adam Result of [tfl_read_adam()] (or a named list of data frames).
#' @param path Output `.xlsx` path.
#' @param spec Optional spec (list of data frames, e.g. from
#'   [tfl_read_fig_spec()]) whose rows are pre-filled.
#' @param max_levels Variables with more distinct values than this are not
#'   offered as codelists.
#' @param n_rows Rows prepared with drop-downs in each sheet.
#' @return `path`, invisibly.
#' @export
tfl_fig_spec_template <- function(adam, path, spec = NULL, max_levels = 50, n_rows = 300) {
  adam <- tfl_read_adam(adam)
  spec <- pp_normalize_spec(spec)
  ds_names <- names(adam)
  types <- pp_types()

  wb <- openxlsx::createWorkbook()
  hdr <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DDEBF7", border = "Bottom")

  for (sh in names(pp_sheet_cols)) {
    openxlsx::addWorksheet(wb, sh)
    openxlsx::writeData(wb, sh, spec[[sh]], headerStyle = hdr)
    openxlsx::freezePane(wb, sh, firstRow = TRUE)
    openxlsx::setColWidths(wb, sh, seq_along(pp_sheet_cols[[sh]]), widths = 14)
  }

  # Reference sheets (visible) ------------------------------------------------
  vars <- do.call(rbind, lapply(ds_names, function(d) {
    df <- adam[[d]]
    data.frame(
      dataset = d, variable = names(df),
      label = vapply(df, pp_var_label, character(1)),
      class = vapply(df, function(v) class(v)[1], character(1)),
      n_values = vapply(df, function(v) length(unique(v)), integer(1)),
      stringsAsFactors = FALSE
    )
  }))
  cl <- do.call(rbind, lapply(ds_names, function(d) {
    df <- adam[[d]]
    do.call(rbind, lapply(pp_codelist_vars(df, max_levels), function(v) {
      vals <- pp_values(df, v)
      data.frame(dataset = d, variable = v, value = vals,
                 n = as.integer(table(factor(as.character(df[[v]]), levels = vals))),
                 stringsAsFactors = FALSE)
    }))
  }))
  openxlsx::addWorksheet(wb, "adam_vars")
  openxlsx::writeData(wb, "adam_vars", vars, headerStyle = hdr)
  openxlsx::addWorksheet(wb, "adam_values")
  openxlsx::writeData(wb, "adam_values", cl, headerStyle = hdr)
  for (s in c("adam_vars", "adam_values")) {
    openxlsx::addFilter(wb, s, rows = 1, cols = 1:5)
    openxlsx::freezePane(wb, s, firstRow = TRUE)
  }

  # Hidden list sheet ---------------------------------------------------------
  openxlsx::addWorksheet(wb, "_lists", visible = FALSE)
  col <- 0L
  add_list <- function(name, values) {
    col <<- col + 1L
    values <- unique(as.character(values))
    if (!length(values)) values <- ""
    openxlsx::writeData(wb, "_lists", data.frame(x = values), startCol = col, colNames = FALSE)
    openxlsx::createNamedRegion(wb, "_lists", cols = col, rows = seq_along(values), name = name)
  }
  all_layers <- unique(unlist(lapply(types, function(t) names(t$layers))))
  all_roles <- unique(unlist(lapply(types, function(t) unlist(lapply(t$roles, names)))))
  add_list("PP_TYPES", names(types))
  add_list("PP_DATASETS", ds_names)
  add_list("PP_LAYERS", c("base", all_layers))
  add_list("PP_ROLES", all_roles)
  add_list("PP_THEMES", pp_themes)
  add_list("PP_PALETTES", names(tfl_fig_palettes()))
  add_list("PP_LEGEND_TYPES", pp_legend_types)
  add_list("PP_LEGEND_POS", pp_legend_positions)
  add_list("PP_GLYPHS", pp_glyphs)
  add_list("PP_SHAPES", names(pp_shape_names))
  add_list("PP_OPTIONS", names(pp_default_options()))
  add_list("PP_UNITS", c("in", "cm", "px"))
  for (d in ds_names) add_list(paste0("V_", d), names(adam[[d]]))
  all_cl_vars <- unique(cl$variable)
  add_list("PP_CL_VARS", all_cl_vars)
  for (i in seq_len(nrow(unique(cl[c("dataset", "variable")])))) {
    key <- unique(cl[c("dataset", "variable")])[i, ]
    add_list(paste0("L_", key$dataset, "_", key$variable),
             cl$value[cl$dataset == key$dataset & cl$variable == key$variable])
  }
  # Same variable across datasets (e.g. TRT01P): union of values, keyed by variable only.
  for (v in all_cl_vars) add_list(paste0("LV_", v), unique(cl$value[cl$variable == v]))

  # Drop-downs ----------------------------------------------------------------
  rows <- 2:(n_rows + 1)
  xml_esc <- function(x) gsub('"', "&quot;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE)
  dv <- function(sheet, colname, formula) {
    c_idx <- match(colname, pp_sheet_cols[[sheet]])
    openxlsx::dataValidation(wb, sheet, cols = c_idx, rows = rows, type = "list",
                             value = xml_esc(formula))
  }
  L <- function(sheet, colname) paste0("$", openxlsx::int2col(match(colname, pp_sheet_cols[[sheet]])), "2")
  # dataset of the row, falling back to the plot's dataset
  ds_of <- function(sheet) {
    sprintf('IF(%s="",VLOOKUP(%s,plots!$A:$C,3,FALSE),%s)', L(sheet, "dataset"), L(sheet, "plot_id"), L(sheet, "dataset"))
  }
  dv("plots", "plot_type", "PP_TYPES")
  dv("plots", "dataset", "PP_DATASETS")
  dv("plots", "theme", "PP_THEMES")
  dv("plots", "palette", "PP_PALETTES")
  dv("plots", "legend_type", "PP_LEGEND_TYPES")
  dv("plots", "legend_pos", "PP_LEGEND_POS")
  dv("plots", "units", "PP_UNITS")
  for (sh in c("roles", "filters")) {
    dv(sh, "plot_id", "plots!$A$2:$A$1000")
    dv(sh, "layer", "PP_LAYERS")
    dv(sh, "dataset", "PP_DATASETS")
    dv(sh, "variable", sprintf('INDIRECT("V_"&%s)', ds_of(sh)))
  }
  dv("roles", "role", "PP_ROLES")
  dv("roles", "shape", "PP_SHAPES")
  dv("filters", "value", sprintf('INDIRECT("L_"&%s&"_"&%s)', ds_of("filters"), L("filters", "variable")))
  dv("levels", "plot_id", "plots!$A$2:$A$1000")
  dv("levels", "variable", "PP_CL_VARS")
  dv("levels", "value", sprintf('INDIRECT("LV_"&%s)', L("levels", "variable")))
  dv("legend", "plot_id", "plots!$A$2:$A$1000")
  dv("legend", "glyph", "PP_GLYPHS")
  dv("legend", "shape", "PP_SHAPES")
  dv("options", "key", "PP_OPTIONS")

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

#' Read an Excel plot spec
#'
#' @param path Path to the spec `.xlsx`.
#' @return A `tfl_fig_spec` object (list of data frames, all columns character).
#' @export
tfl_read_fig_spec <- function(path) {
  sheets <- openxlsx::getSheetNames(path)
  out <- lapply(names(pp_sheet_cols), function(sh) {
    if (!sh %in% sheets) return(NULL)
    openxlsx::read.xlsx(path, sheet = sh, colNames = TRUE, skipEmptyRows = TRUE,
                        na.strings = c("", "NA"))
  })
  names(out) <- names(pp_sheet_cols)
  pp_normalize_spec(out)
}

#' Build a spec from data frames
#'
#' @param plots,roles,filters,levels,legend,options Data frames with the
#'   columns of the corresponding spec sheet (missing columns are added).
#' @return A `tfl_fig_spec` object.
#' @export
tfl_fig_spec <- function(plots, roles = NULL, filters = NULL, levels = NULL,
                      legend = NULL, options = NULL) {
  pp_normalize_spec(list(plots = plots, roles = roles, filters = filters,
                         levels = levels, legend = legend, options = options))
}

pp_normalize_spec <- function(spec) {
  if (inherits(spec, "tfl_fig_spec")) return(spec)
  out <- lapply(names(pp_sheet_cols), function(sh) {
    cols <- pp_sheet_cols[[sh]]
    df <- spec[[sh]]
    if (is.null(df) || !nrow(df)) {
      return(as.data.frame(stats::setNames(replicate(length(cols), character(), simplify = FALSE), cols),
                           stringsAsFactors = FALSE))
    }
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    for (cc in setdiff(cols, names(df))) df[[cc]] <- NA_character_
    df <- df[cols]
    df[] <- lapply(df, function(v) {
      v <- as.character(v)
      v <- trimws(v)
      v[v == ""] <- NA_character_
      v
    })
    # drop fully empty rows
    df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  })
  names(out) <- names(pp_sheet_cols)
  structure(out, class = "tfl_fig_spec")
}

#' @export
print.tfl_fig_spec <- function(x, ...) {
  cat("<tfl_fig_spec>", nrow(x$plots), "plot(s):",
      paste(sprintf("%s [%s]", x$plots$plot_id, x$plots$plot_type), collapse = ", "), "\n")
  invisible(x)
}
