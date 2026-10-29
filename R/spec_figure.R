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
  # (a template SPEC, {name, args}: the design is its template's)
  if (is.list(fd$template)) fd$template <- fd$template$name
  if (!is.null(output$data$steps)) fd$data <- output$data$steps
  if (!is.null(output$data$stats)) fd$stats <- output$data$stats
  if (!is.null(output$plot)) fd$plot <- output$plot
  fd$layers <- list()
  for (it in output$content) {
    x <- .fig_item_as_piece(it)
    if (identical(x$to, "add")) {
      fd$plot$add <- c(fd$plot$add, list(x$piece))
    } else {
      fd$layers <- c(fd$layers, list(x$piece))
    }
  }
  for (k in c("plots", "compose")) if (!is.null(output[[k]])) fd[[k]] <- output[[k]]
  fd
}

# A content item as the design's piece: a layer of the designer as it is;
# an item of a function call (the items form: a template's report adds
# them after the template's layers) as the design's own pieces for it --
# tfl_ard_number the `ard_number` layer, a figure-wide function (labs,
# theme, scale_*) a `plot.add` call, any other a `call` layer
.fig_item_as_piece <- function(it) {
  fn <- it$fn
  pc <- .fig_pieces()[[fn]]
  if (!is.null(pc) && identical(pc$section, "layers")) {
    return(list(to = "layer", piece = c(list(layer = fn), it$args)))
  }
  if (identical(.schema_fn_name(fn), "tfl_ard_number")) {
    return(list(to = "layer", piece = c(list(layer = "ard_number"), .fig_args_plain(it$args))))
  }
  a <- it$args %||% list()
  call <- list(fn = fn)
  if (!is.null(a$data)) call$data <- as.character(a$data)
  if (!is.null(a$mapping)) call$aes <- a$mapping
  a$data <- NULL
  a$mapping <- NULL
  if (length(a)) call$args <- a
  if (grepl(.fig_plotwide_re, .fig_bare_fn_name(fn))) return(list(to = "add", piece = call))
  list(to = "layer", piece = c(list(layer = "call"), call))
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
               order = seq_along(l),
               under = vapply(l, function(e) as.character(e$under %||% NA), ""),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# the figure's code, the figure in `content`, not saved (the report's
# program writes it into its RTF): of its items (R/fig_content.R), or of
# the designer's design the old content stands for
.content_fig_code <- function(output) as.character(.content_fig_code_out(output))

# (the same, with what it calls: attributes libs, reads ...)
.content_fig_code_out <- function(output) {
  if (.fig_new_content(output$content)) return(.fig_content_code(output))
  # (the design as its list: tfl_fig_design_code() reads it as a design)
  design <- .fig_design_of_output(output)
  tfl_fig_design_code(design, plot_id = output$id, save = FALSE, name = "content",
                      codelists = .codelists_rows(output$codelists, output$id))
}

# the figure made: its code run on the objects it reads (`data`: a named
# list -- the datasets by their names in lower case, `ard`)
.content_fig_run <- function(output, data) {
  if (is.data.frame(data)) data <- list(df = data)
  new <- .fig_new_content(output$content)
  code <- .content_fig_code_out(output)
  # the packages it calls: their exports around it, none attached
  env <- new.env(parent = .fig_pkg_env(attr(code, "libs")))
  for (n in names(data)) assign(n, data[[n]], envir = env)
  # a figure of items runs after the study's figure setup (theme_tfl(),
  # scale_colour_tfl(), tfl_colours()), as its program does
  if (new) eval(parse(text = tfl_fig_setup_code()), env)
  if (!is.null(data$ard)) eval(parse(text = tfl_helpers_code()), env)
  eval(parse(text = .fig_strip_library(code)), env)
  get("content", envir = env, inherits = FALSE)
}

# The packages' exports in one environment, a later package's over an
# earlier's (as library() in that order), its parent `parent`: a figure's
# code runs in a child of it -- the session's search path is not changed
.fig_pkg_env <- function(libs, parent = globalenv()) {
  e <- new.env(parent = parent)
  for (p in unique(libs)) {
    if (!requireNamespace(p, quietly = TRUE)) {
      .ard_stop(sprintf("The figure's code needs the %s package: install it.", p))
    }
    nms <- getNamespaceExports(p)
    nms <- nms[!startsWith(nms, ".__")]
    importIntoEnv(e, nms, asNamespace(p), nms)
  }
  e
}

# a program's library() lines left out (its packages are around it)
.fig_strip_library <- function(code) {
  code <- as.character(code)
  code[!grepl("^\\s*(suppressPackageStartupMessages\\()?library\\(", code)]
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
  # (a layer of a function: stat_function(), geom_function())
  if (is.null(l) && "fun" %in% names(formals(f))) {
    l <- tryCatch(suppressWarnings(suppressMessages(f(fun = identity))), error = function(e) NULL)
  }
  if (!inherits(l, "Layer")) return(NULL)
  aes <- unique(c(l$geom$aesthetics(), l$stat$aesthetics()))
  own <- setdiff(names(formals(f)), "...")
  params <- unique(c(own, l$geom$parameters(TRUE), l$stat$parameters(TRUE), aes))
  list(package = pkg, figure = "ggplot layer",
       description = sprintf("The ggplot2 layer %s::%s().", pkg, nm),
       aes = sort(setdiff(aes, "group")) |> c("group"),
       args = stats::setNames(lapply(params, function(p) list(type = .schema_gg_generic)), params))
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

# ---- ggplot2's functions the catalog has not; the arguments a kind shares ----
#
# A figure may call any function ggplot2 exports for a plot (a geom, stat,
# scale, coord, facet, theme, guide, position or element; labs(), xlab(),
# annotate(); the values expansion(), sec_axis(), margin() ...): the
# catalog (inst/spec/schema.yml) gives a card to those a clinical figure
# uses most, any other is written and checked by its own arguments.  The
# arguments the functions of one kind share -- a layer's stat, position
# and colour, a scale's name, breaks and labels, theme()'s settings -- are
# typed once, in inst/fig/gg_args.csv, and given to each function of the
# kind that takes them.

# the type of an argument nothing types: any value a spec can hold
.schema_gg_generic <- paste0("r|call|string|number|logical|list<string|number|logical>|",
                             "map<string, string|number|logical>")

.fig_gg_args <- function() {
  if (is.null(.fig_registry$gg_args)) {
    .fig_registry$gg_args <- utils::read.csv(
      system.file("fig", "gg_args.csv", package = "tflspec", mustWork = TRUE),
      stringsAsFactors = FALSE, na.strings = "", colClasses = "character",
      encoding = "UTF-8")
  }
  .fig_registry$gg_args
}

# the kind of a ggplot2 function by its name (NA: no function of a plot)
.schema_gg_prefix <- c(geom = "layer", stat = "layer", scale = "scale", coord = "coord",
                       facet = "facet", theme = "theme", guide = "guide",
                       position = "position", element = "element")
.schema_gg_helpers <- c("expansion", "sec_axis", "dup_axis", "margin", "margin_auto",
                        "margin_part", "arrow", "unit", "rel")
.schema_gg_kind <- function(nm) {
  if (identical(nm, "theme")) return("theme")
  if (nm %in% c("labs", "xlab", "ylab", "ggtitle")) return("labs")
  if (identical(nm, "annotate")) return("layer")
  if (nm %in% .schema_gg_helpers) return("helper")
  k <- .schema_gg_prefix[sub("_.*$", "", nm)]
  if (is.na(k)) NA_character_ else unname(k)
}

# A ggplot2 function the catalog has not, as an entry: its kind, no card
# (no rank, no label); its arguments are its formals and, by its kind,
# the shared ones it takes.  NULL for a name ggplot2 does not export, or
# that is no function of a plot (ggsave), or that rtfreporter has too.
.schema_gg_entry <- function(nm) {
  if (!requireNamespace("ggplot2", quietly = TRUE) || !nm %in% getNamespaceExports("ggplot2")) {
    return(NULL)
  }
  if (requireNamespace("rtfreporter", quietly = TRUE) &&
      nm %in% getNamespaceExports("rtfreporter")) {
    return(NULL)
  }
  kind <- .schema_gg_kind(nm)
  if (is.na(kind)) return(NULL)
  if (identical(kind, "layer") && grepl("^(geom|stat)_", nm)) return(.schema_ggplot_entry(nm))
  list(package = "ggplot2", figure = kind,
       description = sprintf("ggplot2's %s(), by its own arguments.", nm), args = list())
}

# The arguments a function takes by name: its formals, and a scale's
# `...` -- the arguments of the constructor it passes them on to
# (discrete_scale(), continuous_scale(), binned_scale())
.schema_gg_takes <- function(nm, pkg = "ggplot2") {
  f <- if (requireNamespace(pkg, quietly = TRUE)) {
    tryCatch(getExportedValue(pkg, nm), error = function(e) NULL)
  }
  if (!is.function(f)) return(character())
  fa <- names(formals(f))
  # a function renamed, now only `...` passed on to its new name
  # (coord_trans() of ggplot2 4.0 is coord_transform()'s)
  if (identical(fa, "...")) {
    cp <- .fig_compat()
    to <- cp$replacement[cp$fn == nm & cp$change == "fn_renamed"]
    if (length(to) == 1L) return(.schema_gg_takes(to, pkg))
  }
  if ("..." %in% fa && startsWith(nm, "scale_")) fa <- c(fa, .schema_scale_dots(nm, f))
  # coord_fixed(ratio, ...): coord_cartesian()'s; labs(...): the labels by
  # aesthetic
  if ("..." %in% fa && startsWith(nm, "coord_")) fa <- c(fa, names(formals(ggplot2::coord_cartesian)))
  if ("..." %in% fa && identical(nm, "labs")) {
    fa <- c(fa, c("x", "y", "colour", "fill", "shape", "linetype"))
  }
  unique(setdiff(fa, "..."))
}

.schema_scale_dots <- function(nm, f) {
  s <- tryCatch(suppressWarnings(suppressMessages(f())), error = function(e) NULL)
  ctor <- if (inherits(s, "ScaleBinned") || grepl("_(stepsn?|binned|fermenter)$", nm)) {
    "binned_scale"
  } else if (inherits(s, "ScaleContinuous") || grepl("_(gradientn|continuous)$", nm)) {
    "continuous_scale"
  } else {
    "discrete_scale"
  }
  setdiff(names(formals(getExportedValue("ggplot2", ctor))), c("scale_name", "super", "call"))
}

.schema_theme_sides <- c("x", "y", "top", "bottom", "left", "right", "x.top", "x.bottom",
                         "y.left", "y.right", "theta", "r", "inside")

# The entry with the shared arguments of its kind the function takes,
# typed (an argument the catalog types itself stays as it says); any
# other argument a scale takes through `...`, untyped.  `extra`: more
# names the function takes (a layer's stat's parameters).
.schema_fig_common <- function(ent, nm, extra = character()) {
  kind <- ent$figure
  if (is.null(kind)) return(ent)
  takes <- unique(c(.schema_gg_takes(nm, ent$package %||% "ggplot2"), names(ent$args), extra))
  g <- .fig_gg_args()
  g <- g[g$kind == kind & g$arg %in% takes, , drop = FALSE]
  for (i in seq_len(nrow(g))) {
    a <- g$arg[i]
    old <- ent$args[[a]]
    if (!is.null(old$type) && !identical(old$type, .schema_gg_generic)) next
    new <- list(type = g$type[i], label = list(en = g$en[i], ja = g$ja[i]))
    if (!is.na(g$help[i])) new$description <- g$help[i]
    ent$args[[a]] <- c(new, old[setdiff(names(old), names(new))])
  }
  # a theme setting of one side or axis (axis.text.x.top, legend.key.spacing.y)
  # is typed as the setting it refines
  if (identical(kind, "theme")) {
    th <- .fig_gg_args()
    th <- th[th$kind == "theme", , drop = FALSE]
    for (a in setdiff(takes, names(ent$args))) {
      p <- a
      while (grepl(".", p, fixed = TRUE) && !p %in% th$arg) p <- sub("\\.[^.]*$", "", p)
      i <- match(p, th$arg)
      suf <- substring(a, nchar(p) + 2L)
      if (is.na(i) || !suf %in% .schema_theme_sides) next
      ent$args[[a]] <- list(type = th$type[i],
                            label = list(en = sprintf("%s (%s)", th$en[i], suf),
                                         ja = sprintf("%s\uff08%s\uff09", th$ja[i], suf)))
    }
  }
  if (identical(kind, "scale")) {
    fa <- names(.schema_formals(nm, ent) %||% list())
    for (a in setdiff(takes, c(fa, names(ent$args)))) ent$args[[a]] <- list(type = .schema_gg_generic)
  }
  ent
}

# The layer a geom or a stat is drawn with when the item names another:
# geom_step(stat = "ecdf") is stat_ecdf()'s too (pad, n), stat_summary(geom
# = "errorbar") geom_errorbar()'s (width) -- its entry, or NULL
.schema_layer_partner <- function(fn, args) {
  nm <- .schema_fn_name(fn)
  pick <- function(v) {
    if (!is.character(v) || length(v) != 1L || !nzchar(v)) return(NULL)
    sub("^(geom|stat)_", "", v)
  }
  other <- if (startsWith(nm, "geom_")) {
    s <- pick(args$stat)
    if (!is.null(s)) paste0("stat_", s)
  } else if (startsWith(nm, "stat_")) {
    g <- pick(args$geom)
    if (!is.null(g)) paste0("geom_", g)
  }
  if (is.null(other)) return(NULL)
  pkg <- if (grepl("::", fn, fixed = TRUE)) sub("::.*$", "", fn)
  ent <- .schema_ggplot_entry(other)
  if (is.null(ent) && !is.null(pkg)) ent <- .schema_ggplot_entry(paste0(pkg, "::", other))
  ent
}

# The entry an item is checked against: its function's, with the
# parameters of the layer it names as its stat or geom
.schema_item_entry <- function(ent, fn, args) {
  part <- .schema_layer_partner(fn, args)
  if (is.null(ent) || is.null(part)) return(ent)
  for (k in setdiff(names(part$args), names(ent$args))) ent$args[[k]] <- part$args[[k]]
  ent$aes <- unique(c(ent$aes, part$aes))
  .schema_fig_common(ent, .schema_fn_name(fn))
}
