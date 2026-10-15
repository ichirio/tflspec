# ---- a figure's content: its layers in the schema, its design ----------------
#
# A figure report's `content:` is its layers, one item a layer of the
# figure designer (`km_curve: {linewidth: 0.3}`, `ard_number: {...}`), its
# `data:` the designer's data steps and statistics.  The schema's entries
# for the layers are made from the designer's own catalog
# (tfl_fig_parts()), so the two cannot differ; the figure's code is the
# designer's (tfl_fig_design_code()) of the design the report stands for.

# the designer's kinds of field as the schema's types
.fig_kind_types <- c(variable = "column", variables = "list<column>", flag = "column",
                     dataset = "string", param = "string", text = "string",
                     object = "string", analysis = "string", ard_stat = "string",
                     ard_variable = "string", ard_function = "string",
                     analysis_data = "string", number = "number", logical = "logical",
                     expr = "r", code = "r", raw = "r", shape = "number|string",
                     named = "map<string, column>", values = "list<string|number>|string")

# One schema entry a layer of the designer: its fields, each its type, its
# choices an enum; `package: tflspec`, `figure: layer` (no function of
# that name: the designer writes the layer's code)
.schema_fig_entries <- function() {
  p <- tfl_fig_parts()
  p <- p[p$section == "layers" & !is.na(p$field), , drop = FALSE]
  out <- list()
  for (pc in unique(p$piece)) {
    rows <- p[p$piece == pc, , drop = FALSE]
    args <- list()
    for (i in seq_len(nrow(rows))) {
      k <- rows$kind[i]
      ty <- if (identical(k, "choice") && !is.na(rows$choices[i])) {
        paste0("enum[", gsub("\\s*\\|\\s*", "|", trimws(rows$choices[i])), "]")
      } else .fig_kind_types[[k]] %||% "string"
      a <- list(type = ty)
      if (!is.na(rows$help[i]) && nzchar(rows$help[i])) a$description <- rows$help[i]
      args[[rows$field[i]]] <- a
    }
    out[[pc]] <- list(package = "tflspec", figure = "layer",
                      description = rows$piece_label[1L], args = args)
  }
  out
}

# The figure design (tfl_fig_design()'s list) a figure report stands for:
# its data steps and statistics, its layers (the content's items, each
# `layer:` and its fields), and what it says beside them (template, plot,
# plots, compose)
.fig_design_of_output <- function(output) {
  fd <- list()
  for (k in c("template", "ggplot2_version")) if (!is.null(output[[k]])) fd[[k]] <- output[[k]]
  if (!is.null(output$data$steps)) fd$data <- output$data$steps
  if (!is.null(output$data$stats)) fd$stats <- output$data$stats
  if (!is.null(output$plot)) fd$plot <- output$plot
  fd$layers <- lapply(output$content, function(it) c(list(layer = it$fn), it$args))
  for (k in c("plots", "compose")) if (!is.null(output[[k]])) fd[[k]] <- output[[k]]
  fd
}

# A report's code lists (`codelists:` -- {VARIABLE: [{value, label}]}) as
# the rows the figure's code puts on its data (output_id, variable, value,
# label, order)
.codelists_rows <- function(codelists, id) {
  if (!length(codelists)) return(NULL)
  rows <- lapply(names(codelists), function(v) {
    l <- codelists[[v]]
    data.frame(output_id = id, variable = v,
               value = vapply(l, function(e) as.character(e$value %||% NA), ""),
               label = vapply(l, function(e) as.character(e$label %||% NA), ""),
               order = seq_along(l), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# the figure's code, the figure in `content`, not saved (the report's
# program writes it into its RTF): of its items (R/fig_content.R), or of
# the designer's design the old content stands for
.content_fig_code <- function(output) {
  if (.fig_new_content(output$content)) return(as.character(.fig_content_code(output)))
  # (the design as its list: tfl_fig_design_code() reads it as a design)
  design <- .fig_design_of_output(output)
  as.character(tfl_fig_design_code(design, plot_id = output$id, save = FALSE,
                                   name = "content",
                                   codelists = .codelists_rows(output$codelists, output$id)))
}

# the figure made: its code run on the objects it reads (`data`: a named
# list -- the datasets by their names in lower case, `ard`)
.content_fig_run <- function(output, data) {
  env <- new.env(parent = globalenv())
  if (is.data.frame(data)) data <- list(df = data)
  for (n in names(data)) assign(n, data[[n]], envir = env)
  # a figure of items runs after the study's figure setup (theme_tfl(),
  # scale_colour_tfl(), tfl_colours()), as its program does
  if (.fig_new_content(output$content)) {
    eval(parse(text = tfl_fig_setup_code()), env)
    if (!is.null(data$ard)) eval(parse(text = tfl_helpers_code()), env)
    code <- .fig_content_code(output)
    libs <- attr(code, "libs")
    eval(parse(text = c(sprintf("suppressPackageStartupMessages(library(%s))", libs), code)), env)
    return(get("content", envir = env, inherits = FALSE))
  }
  eval(parse(text = .content_fig_code(output)), env)
  get("content", envir = env, inherits = FALSE)
}

# ---- a ggplot2 layer the figure names itself: its schema from ggplot2 -------

# A layer function of ggplot2 or an extension (`geom_area`, `stat_ecdf`,
# `ggrepel::geom_text_repel`) as a schema entry, from the installed
# package itself: `aes`, the aesthetics its geom and its stat know; `args`,
# what it takes as parameters -- its own arguments, its geom's and stat's
# parameters, and the aesthetics given a fixed value (`size = 2`).  NULL
# when the name is no layer function.
.schema_ggplot_entry <- function(fn) {
  pkg <- if (grepl("::", fn, fixed = TRUE)) sub("::.*$", "", fn) else "ggplot2"
  nm <- sub("^.*::", "", fn)
  if (!requireNamespace(pkg, quietly = TRUE)) return(NULL)
  f <- tryCatch(getExportedValue(pkg, nm), error = function(e) NULL)
  if (!is.function(f)) return(NULL)
  l <- tryCatch(suppressWarnings(suppressMessages(f())), error = function(e) NULL)
  if (!inherits(l, "Layer")) return(NULL)
  aes <- unique(c(l$geom$aesthetics(), l$stat$aesthetics()))
  own <- setdiff(names(formals(f)), c("mapping", "data", "..."))
  params <- unique(c(own, l$geom$parameters(TRUE), l$stat$parameters(TRUE), aes))
  list(package = pkg, figure = "ggplot layer",
       description = sprintf("The ggplot2 layer %s::%s().", pkg, nm),
       aes = sort(setdiff(aes, "group")) |> c("group"),
       args = stats::setNames(lapply(params, function(p) list(type = "r|string|number|logical")), params))
}

# A figure's `geom` layer (`geom: geom_area`, `aes:`, `params:`) checked
# against the layer function's own schema: the function exists, each
# aesthetic and each parameter is one it knows (an unknown one warns, as
# every other item's does)
.schema_check_geom <- function(item, bad) {
  a <- item$args %||% list()
  g <- a$geom
  if (is.null(g) || !is.character(g) || length(g) != 1L) {
    bad("geom", "the geom layer names no layer function (geom: geom_area)")
    return(invisible())
  }
  ent <- .schema_ggplot_entry(g)
  if (is.null(ent)) {
    bad("geom", sprintf("%s is no ggplot2 layer function the installed packages have", g))
    return(invisible())
  }
  for (k in names(a$aes %||% list())) {
    if (!k %in% ent$aes) {
      bad("aes", sprintf("%s has no aesthetic `%s` (it has: %s)", g, k, paste(ent$aes, collapse = ", ")),
          "warning")
    }
  }
  for (k in names(a$params %||% list())) {
    if (!k %in% names(ent$args)) {
      bad("params", sprintf("%s takes no parameter `%s`", g, k), "warning")
    }
  }
  invisible()
}
