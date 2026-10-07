# `a`, unless it is NULL, empty or a single NA (a blank spec cell); then `b`.
# Not `%or%`: the ARD / plan code needs base R's NULL-only meaning of that.
`%or%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# File name of a plot id: keeps letters, digits, '-', '_' and '.' (F-14.2.1 stays as is).
pp_file_name <- function(x) gsub("[^A-Za-z0-9._-]", "_", x)

# Quote a string as R code.
q <- function(x) encodeString(as.character(x), quote = '"')

# Code for a (named) vector; numbers stay unquoted.
vec_code <- function(x, width = 70) {
  num <- is.numeric(x)
  vals <- if (num) format(x, trim = TRUE) else q(x)
  items <- if (!is.null(names(x))) paste(q(names(x)), "=", vals) else vals
  one <- paste0("c(", paste(items, collapse = ", "), ")")
  if (nchar(one) <= width) return(one)
  paste0("c(\n  ", paste(items, collapse = ",\n  "), "\n)")
}

indent <- function(x, n = 2) {
  x <- unlist(strsplit(paste(x, collapse = "\n"), "\n", fixed = TRUE))
  paste0(strrep(" ", n), x)
}

is_num_string <- function(x) !is.na(suppressWarnings(as.numeric(x)))

split_list <- function(x) {
  if (is.null(x) || is.na(x)) return(character())
  out <- trimws(unlist(strsplit(x, "[,;]")))
  out[out != ""]
}

pp_shape_code <- function(x, default) {
  if (is.null(x) || is.na(x) || x == "") x <- default
  if (is_num_string(x)) return(as.numeric(x))
  if (!x %in% names(pp_shape_names)) stop("Unknown shape: ", x, call. = FALSE)
  unname(pp_shape_names[x])
}

# ---- context ---------------------------------------------------------------

pp_context <- function(spec, pid, adam) {
  prow <- as.list(spec$plots[spec$plots$plot_id %in% pid, , drop = FALSE][1, ])
  type <- prow$plot_type
  if (is.na(type) || !type %in% names(pp_types())) {
    stop("Plot ", pid, ": unknown plot_type '", type, "'.", call. = FALSE)
  }
  if (is.na(prow$dataset)) stop("Plot ", pid, ": `dataset` is required.", call. = FALSE)
  ctx <- new.env(parent = emptyenv())
  ctx$spec <- spec
  ctx$pid <- pid
  ctx$prow <- prow
  ctx$type <- type
  ctx$def <- pp_types()[[type]]
  ctx$adam <- adam
  ctx$ds <- prow$dataset
  ctx$layers <- split_list(prow$layers)
  bad <- setdiff(ctx$layers, names(ctx$def$layers))
  if (length(bad)) stop("Plot ", pid, ": unknown layer(s) for ", type, ": ", paste(bad, collapse = ", "), call. = FALSE)
  ctx$datasets <- character()
  ctx
}

pp_opt <- function(ctx, key) {
  o <- ctx$spec$options
  hit <- o$value[o$plot_id %in% ctx$pid & o$key %in% key]
  if (!length(hit)) hit <- o$value[is.na(o$plot_id) & o$key %in% key]
  if (length(hit) && !is.na(hit[1])) return(hit[1])
  d <- pp_default_options(ctx$type)[[key]]
  if (is.null(d)) stop("Unknown option: ", key, call. = FALSE)
  if (identical(d, "")) NA_character_ else d
}
# Option value only when set in the spec (NA otherwise).
pp_opt_explicit <- function(ctx, key) {
  o <- ctx$spec$options
  hit <- o$value[(o$plot_id %in% ctx$pid | is.na(o$plot_id)) & o$key %in% key]
  if (length(hit)) hit[1] else NA_character_
}
pp_opt_num <- function(ctx, key, default = NA_real_) {
  v <- pp_opt(ctx, key)
  if (is.na(v)) default else as.numeric(v)
}

has_layer <- function(ctx, layer) layer %in% ctx$layers

# Role rows of one layer ("base" = rows with blank layer).
pp_roles <- function(ctx, layer = "base") {
  r <- ctx$spec$roles
  lay <- ifelse(is.na(r$layer), "base", r$layer)
  r <- r[r$plot_id %in% ctx$pid & lay == layer, , drop = FALSE]
  r$dataset[is.na(r$dataset)] <- ctx$ds
  r
}

pp_role <- function(ctx, role, layer = "base", required = FALSE) {
  r <- pp_roles(ctx, layer)
  r <- r[r$role %in% role, , drop = FALSE]
  if (!nrow(r) || is.na(r$variable[1])) {
    if (required) stop("Plot ", ctx$pid, ": role '", role, "' (", layer, ") is required.", call. = FALSE)
    return(NULL)
  }
  as.list(r[1, ])
}

# ---- data access --------------------------------------------------------------

pp_ds_name <- function(ds) tolower(ds)

# Register a dataset; returns the R object name used in the code.
pp_use_ds <- function(ctx, ds) {
  ctx$datasets <- union(ctx$datasets, ds)
  pp_ds_name(ds)
}

pp_load_lines <- function(ctx) {
  expr <- pp_opt(ctx, "data_expr")
  out <- character()
  for (ds in ctx$datasets) {
    rhs <- gsub("{ds}", pp_ds_name(ds), gsub("{DS}", ds, expr, fixed = TRUE), fixed = TRUE)
    if (rhs != pp_ds_name(ds)) out <- c(out, sprintf("%s <- %s", pp_ds_name(ds), rhs))
  }
  if (!length(out)) {
    out <- sprintf("# Input data frames: %s", paste(pp_ds_name(ctx$datasets), collapse = ", "))
  }
  out
}

# Filters for (layer, dataset): list of list(var, values).
pp_filters <- function(ctx, layer, ds) {
  f <- ctx$spec$filters
  lay <- ifelse(is.na(f$layer), "base", f$layer)
  fds <- ifelse(is.na(f$dataset), ctx$ds, f$dataset)
  f <- f[f$plot_id %in% ctx$pid & lay == layer & fds == ds & !is.na(f$variable), , drop = FALSE]
  lapply(split(f$value, factor(f$variable, levels = unique(f$variable))), function(v) v)
}

pp_filter_code <- function(ctx, flist, ds) {
  if (!length(flist)) return(NULL)
  df <- ctx$adam[[ds]]
  conds <- vapply(names(flist), function(v) {
    vals <- flist[[v]]
    numeric_col <- if (!is.null(df) && v %in% names(df)) is.numeric(df[[v]]) else all(is_num_string(vals))
    lit <- if (numeric_col) vals else q(vals)
    if (length(vals) == 1) sprintf("%s == %s", v, lit) else sprintf("%s %%in%% c(%s)", v, paste(lit, collapse = ", "))
  }, character(1))
  if (length(conds) == 1) return(sprintf("filter(%s)", conds))
  paste0("filter(\n", paste(indent(conds), collapse = ",\n"), "\n)")
}

pp_filtered_data <- function(ctx, ds, flist) {
  df <- ctx$adam[[ds]]
  if (is.null(df)) return(NULL)
  for (v in names(flist)) {
    if (!v %in% names(df)) next
    df <- df[as.character(df[[v]]) %in% flist[[v]], , drop = FALSE]
  }
  df
}

# Pipeline: first line is the data object, following lines are chained verbs.
pipe_code <- function(target, steps) {
  steps <- steps[!vapply(steps, is.null, logical(1))]
  body <- vapply(steps, function(s) paste(indent(s), collapse = "\n"), character(1))
  if (length(body) == 1) return(sprintf("%s <- %s", target, trimws(body)))
  paste0(target, " <- ", trimws(body[1]), " |>\n", paste(body[-1], collapse = " |>\n"))
}

# Bring `var` from another dataset by id. Returns list(code, name).
pp_join <- function(ctx, layer, ds_from, var, id, taken) {
  flist <- pp_filters(ctx, layer, ds_from)
  name <- NULL
  if ("PARAMCD" %in% names(flist) && length(flist$PARAMCD) == 1 && make.names(flist$PARAMCD) == flist$PARAMCD) {
    name <- flist$PARAMCD
  }
  if (is.null(name) || name %in% taken) name <- paste0(var, "_", ds_from)
  src <- pp_use_ds(ctx, ds_from)
  inner <- pipe_code("x", list(src, pp_filter_code(ctx, flist, ds_from), sprintf("select(%s, %s = %s)", id, name, var)))
  inner <- sub("^x <- ", "", inner)
  code <- paste0("left_join(\n", paste(indent(inner), collapse = "\n"), ",\n  by = ", q(id), "\n)")
  list(code = code, name = name, values = pp_values_or_null(pp_filtered_data(ctx, ds_from, flist), var))
}

pp_values_or_null <- function(df, var) {
  if (is.null(df) || !var %in% names(df)) return(NULL)
  pp_values(df, var)
}

pp_var_label_of <- function(ctx, ds, var) {
  df <- ctx$adam[[ds]]
  if (is.null(df) || !var %in% names(df)) return(NA_character_)
  lab <- pp_var_label(df[[var]])
  if (lab == "") NA_character_ else lab
}

# ---- colours -------------------------------------------------------------------

# Resolve colours for the values of `var`. Returns list(code, name, values,
# colours, labels); `values` is NULL when only known at run time.
pp_scale_values <- function(ctx, obj, var, data_values, df_name, default_palette, palette = NULL) {
  pal_name <- palette %or% ctx$prow$palette %or% default_palette
  pals <- tfl_fig_palettes()
  if (!pal_name %in% names(pals)) stop("Unknown palette: ", pal_name, call. = FALSE)
  pal <- pals[[pal_name]]
  fallback <- if (is.null(names(pal))) pal else pals$okabe_ito

  lv <- ctx$spec$levels
  lv <- lv[lv$plot_id %in% ctx$pid & lv$variable %in% var, , drop = FALSE]
  if (nrow(lv)) {
    ord <- suppressWarnings(as.numeric(lv$order))
    lv <- lv[order(is.na(ord), ord, seq_len(nrow(lv))), , drop = FALSE]
    values <- lv$value
    given <- lv$colour
    labels <- ifelse(is.na(lv$label), lv$value, lv$label)
  } else if (!is.null(data_values)) {
    values <- if (!is.null(names(pal))) c(intersect(names(pal), data_values), setdiff(data_values, names(pal))) else data_values
    given <- rep(NA_character_, length(values))
    labels <- values
  } else if (!is.null(names(pal))) {
    values <- names(pal)
    given <- rep(NA_character_, length(values))
    labels <- values
  } else {
    code <- sprintf("%s <- %s\n%s <- setNames(%s[seq_along(%s)], %s)",
                    obj, sprintf("unique(na.omit(as.character(%s$%s)))", df_name, var), obj,
                    vec_code(unname(pal)), obj, obj)
    return(list(code = code, name = obj, values = NULL, colours = NULL, labels = NULL))
  }
  k <- 0
  colours <- vapply(seq_along(values), function(i) {
    if (!is.na(given[i])) return(given[i])
    if (!is.null(names(pal)) && values[i] %in% names(pal)) return(unname(pal[values[i]]))
    k <<- k + 1
    fallback[(k - 1) %% length(fallback) + 1]
  }, character(1))
  names(colours) <- values
  code <- sprintf("%s <- %s", obj, vec_code(colours))
  has_lab <- any(labels != values)
  if (has_lab) code <- paste0(code, "\n", sprintf("%s_lab <- %s", obj, vec_code(stats::setNames(labels, values))))
  list(code = code, name = obj, values = values, colours = unname(colours),
       labels = labels, has_labels = has_lab)
}

# Palettes known at generation time go before the data step (factor levels use
# them); run-time palettes are computed from the data, so they go after.
pal_before <- function(...) unlist(lapply(list(...), function(sv) if (!is.null(sv) && !is.null(sv$values)) sv$code))
pal_after <- function(...) unlist(lapply(list(...), function(sv) if (!is.null(sv) && is.null(sv$values)) sv$code))

# scale_*_manual() call for a resolved palette.
pp_scale_manual <- function(aes, sv, extra = NULL) {
  args <- c(sprintf("values = %s", sv$name), sprintf("breaks = names(%s)", sv$name))
  if (isTRUE(sv$has_labels)) args <- c(args, sprintf("labels = %s_lab", sv$name))
  args <- c(args, extra)
  sprintf("scale_%s_manual(%s)", aes, paste(args, collapse = ", "))
}

# ---- theme -----------------------------------------------------------------------

pp_theme_code <- function(ctx) {
  th <- ctx$prow$theme %or% pp_opt(ctx, "theme") %or% "boxed"
  pp_theme_lines(th, pp_opt(ctx, "base_size"), function(k) pp_opt(ctx, k))
}

# Join ggplot terms with " +".
plus_code <- function(target, terms, append = FALSE) {
  terms <- unlist(terms[!vapply(terms, is.null, logical(1))])
  lhs <- if (append) sprintf("%s <- %s +", target, target) else sprintf("%s <-", target)
  body <- vapply(terms, function(t) paste(indent(t), collapse = "\n"), character(1))
  paste0(lhs, "\n", paste(body, collapse = " +\n"))
}

section <- function(title) {
  head <- paste("# ----", title, "")
  paste0(head, strrep("-", max(3L, 78L - nchar(head))))
}
