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
  if (!is.null(.schema_cache$fig)) return(.schema_cache$fig)
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
  .schema_cache$fig <- out
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

# the figure's code: the designer's, the figure in `content`, not saved
# (the report's program writes it into its RTF)
.content_fig_code <- function(output) {
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
  eval(parse(text = .content_fig_code(output)), env)
  get("content", envir = env, inherits = FALSE)
}
