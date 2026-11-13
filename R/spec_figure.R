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
  # (a draft that says its layers as a figure answer does, `layers:` beside
  # its content -- the agent's file, as instructions-figure.md shows them --
  # has them first)
  fd$layers <- Filter(function(l) is.list(l) && !is.null(l$layer), output$layers %||% list())
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
  # (the report's program runs after the study's helpers: the arms in their
  # numeric order, as the figure of items has them)
  tfl_fig_design_code(design, plot_id = output$id, save = FALSE, name = "content",
                      codelists = .codelists_rows(output$codelists, output$id),
                      order_arms = TRUE)
}

# the figure made: its code run on the objects it reads (`data`: a named
# list -- the datasets by their names in lower case, `ard`)
# A dataset a figure's steps read and the data given do not hold would be
# found on the search path -- the package's sample adtte, without the
# study's flags, and a puzzling "object 'FASFL' not found": a clear stop
.fig_reads_given <- function(output, data) {
  reads <- unique(tolower(unlist(lapply(output$data$steps, function(st) {
    if (is.list(st) && identical(st$step, "read") && is.null(.fig_read_data(st))) st$dataset
  }))))
  given <- if (is.list(data) && !is.data.frame(data)) tolower(names(data)) else character()
  miss <- setdiff(reads, given)
  if (length(miss)) {
    .ard_stop(sprintf("tfl_content_run(): the figure reads %s, not among the data given (%s).",
                      paste(toupper(miss), collapse = ", "),
                      if (length(given)) paste(toupper(given), collapse = ", ") else "none"))
  }
  invisible(TRUE)
}

.content_fig_run <- function(output, data) {
  if (is.data.frame(data)) data <- list(df = data)
  .fig_reads_given(output, data)
  new <- .fig_new_content(output$content)
  code <- .content_fig_code_out(output)
  # the packages it calls: their exports around it, none attached
  env <- new.env(parent = .fig_pkg_env(attr(code, "libs")))
  for (n in names(data)) assign(n, data[[n]], envir = env)
  # a figure of items runs after the study's figure setup (theme_tfl(),
  # scale_colour_tfl(), tfl_colours()), as its program does
  if (new) eval(parse(text = tfl_fig_setup_code()), env)
  # (the study's helpers: the ARD's readers, and set_levels() of the code
  # lists and order_levels() of the arms)
  if (!is.null(data$ard) || any(grepl("set_levels(", code, fixed = TRUE)) ||
      any(grepl("order_levels(", code, fixed = TRUE))) {
    eval(parse(text = tfl_helpers_code()), env)
  }
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
  # and its position's (ggplot2 4.0: geom_text()'s nudge_x / nudge_y, mapped
  # or given)
  pos_aes <- if (is.function(l$position$aesthetics)) {
    tryCatch(l$position$aesthetics(), error = function(e) character())
  }
  aes <- unique(c(l$geom$aesthetics(), l$stat$aesthetics(), pos_aes))
  own <- setdiff(names(formals(f)), "...")
  params <- unique(c(own, l$geom$parameters(TRUE), l$stat$parameters(TRUE), aes,
                     .schema_layer_dots(f)))
  list(package = pkg, figure = "ggplot layer",
       description = sprintf("The ggplot2 layer %s::%s().", pkg, nm),
       aes = sort(setdiff(aes, "group")) |> c("group"),
       args = stats::setNames(lapply(params, function(p) list(type = .schema_gg_generic)), params))
}

# What any layer function passes on to layer() through its `...`: the
# legend key's look (key_glyph) and, ggplot2 4.0, the panels it is drawn
# in (layout)
.schema_layer_dots <- function(f) {
  if (!"..." %in% names(formals(f))) return(character())
  intersect(c("key_glyph", "layout"), names(formals(ggplot2::layer)))
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

# A layer's `mapping` (geom_line(mapping = {x: AVISITN, colour: TRT01A})):
# each name an aesthetic the layer has (`color` is `colour`, as ggplot2
# reads it); an unknown one warns, as ggplot2's "Ignoring unknown
# aesthetics" does.  Only where the layer's package is installed (its
# aesthetics are the installed layer's).
.schema_check_mapping <- function(fn, ent, mapping, bad) {
  if (is.null(ent$aes) || !is.list(mapping) || is.null(names(mapping))) return(invisible())
  if (!requireNamespace(ent$package %||% "ggplot2", quietly = TRUE)) return(invisible())
  given <- names(mapping)[nzchar(names(mapping))]
  std <- ggplot2::standardise_aes_names(given)
  for (i in which(!std %in% ent$aes)) {
    bad("mapping", sprintf("%s has no aesthetic `%s` (it has: %s)", .schema_fn_name(fn), given[i],
                           paste(ent$aes, collapse = ", ")), "warning")
  }
  invisible()
}

# The aesthetics a layer needs (ggplot2's own `required_aes`: "x|y" is x or
# y), from the installed layer drawn by the stat and geom the item names:
# a layer of a stat that computes them (geom_bar's count) needs the stat's,
# any other its geom's.  NULL when the layer cannot be made.
.schema_layer_required <- function(fn, args) {
  l <- .schema_layer_obj(fn, args)
  if (is.null(l)) return(NULL)
  nm <- sub("^.*::", "", fn)
  if (startsWith(nm, "stat_") || !inherits(l$stat, "StatIdentity")) l$stat$required_aes else
    l$geom$required_aes
}

# The installed layer an item makes (its stat and geom as it names them),
# or NULL
.schema_layer_obj <- function(fn, args) {
  if (!is.character(fn) || length(fn) != 1L) return(NULL)
  pkg <- if (grepl("::", fn, fixed = TRUE)) sub("::.*$", "", fn) else
    tfl_schema(fn)$package %||% "ggplot2"
  nm <- sub("^.*::", "", fn)
  if (!requireNamespace(pkg, quietly = TRUE)) return(NULL)
  f <- tryCatch(getExportedValue(pkg, nm), error = function(e) NULL)
  if (!is.function(f)) return(NULL)
  pass <- args[intersect(c("stat", "geom"), names(args))]
  pass <- pass[vapply(pass, function(v) is.character(v) && length(v) == 1L, NA)]
  if ("fun" %in% names(formals(f))) pass$fun <- identity
  l <- tryCatch(suppressWarnings(suppressMessages(do.call(f, pass))), error = function(e) NULL)
  if (!inherits(l, "Layer")) return(NULL)
  l
}

# A plot's layers each given the aesthetics they need: mapped by the layer,
# given a value (geom_hline(yintercept = 0)) or inherited from the first
# item's ggplot(mapping =) (unless inherit.aes: false).  A missing one
# warns: ggplot2 would stop at the draw ("requires the following missing
# aesthetics").  A plot not begun by ggplot() (ggsurvfit()'s own mapping),
# or a mapping written as R, is not judged.
.schema_check_plot_aes <- function(plot) {
  out <- data.frame(fn = character(), arg = character(), level = character(),
                    message = character(), stringsAsFactors = FALSE)
  items <- plot$items
  if (length(items) < 2L || !identical(.schema_fn_name(items[[1L]]$fn %||% ""), "ggplot")) return(out)
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(out)
  std <- function(x) ggplot2::standardise_aes_names(x %||% character())
  gm <- items[[1L]]$args$mapping
  global <- if (is.null(gm)) character() else if (is.list(gm)) std(names(gm))
  # an aes() added to the plot later: what it maps is not known here
  if (any(vapply(items, function(it) identical(.schema_fn_name(it$fn %||% ""), "aes"), NA))) global <- NULL
  for (it in items[-1L]) {
    fn <- it$fn %||% ""
    a <- it$args %||% list()
    if (identical(fn, "geom")) {
      fn <- a$geom %||% ""
      a <- c(list(mapping = a$aes), a$params %||% list())
    }
    if (!is.character(fn) || length(fn) != 1L || !grepl("^(geom|stat)_", .schema_fn_name(fn))) next
    m <- a$mapping
    if (!is.null(m) && !is.list(m)) next
    inherit <- !isFALSE(a$inherit.aes)
    if (inherit && is.null(global)) next
    req <- .schema_layer_required(fn, a)
    if (!length(req)) next
    # geom_abline() gives the slope (1) and the intercept (0) itself when it
    # has no mapping, and the other when it is given one of them
    if (identical(.schema_fn_name(fn), "geom_abline") &&
        (!length(m) || any(c("slope", "intercept") %in% names(a)))) next
    given <- c(std(names(m)), std(setdiff(names(a), c("mapping", "data"))), if (inherit) global)
    for (g in req) {
      alts <- strsplit(g, "|", fixed = TRUE)[[1L]]
      if (any(alts %in% given)) next
      msg <- sprintf("%s needs the aesthetic `%s`%s: map it in %s, or give it a value",
                     .schema_fn_name(fn), alts[1L],
                     if (length(alts) > 1L) paste0(" (or ", paste0("`", alts[-1L], "`", collapse = ", "), ")") else "",
                     if (inherit) "the layer's mapping or ggplot()'s" else
                       "the layer's mapping (it has inherit.aes: false)")
      out[nrow(out) + 1L, ] <- list(.schema_fn_name(fn), "mapping", "warning", msg)
    }
  }
  out
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
                        "margin_part", "arrow", "unit", "rel", "vars", "labeller", "as_labeller",
                        "label_value", "label_both", "label_wrap_gen", "label_parsed",
                        "label_context", "label_bquote", "derive")
.schema_gg_kind <- function(nm) {
  if (identical(nm, "theme")) return("theme")
  if (nm %in% c("labs", "xlab", "ylab", "ggtitle")) return("labs")
  # a layer without the prefix (annotation_logticks() for a log axis,
  # expand_limits() a blank layer); xlim(), ylim() and lims() a scale's
  # limits
  if (nm %in% c("annotate", "expand_limits") || startsWith(nm, "annotation_")) return("layer")
  if (nm %in% c("xlim", "ylim", "lims")) return("scale")
  if (nm %in% .schema_gg_helpers) return("helper")
  k <- .schema_gg_prefix[sub("_.*$", "", nm)]
  if (is.na(k)) NA_character_ else unname(k)
}

# ggplot2's other spelling of a function (scale_color_manual,
# guide_colorbar, theme_grey, stat_bin2d) as the name the catalog gives it
# a card by; any other name as it is
.schema_gg_alias <- function(nm) {
  to <- sub("^(scale|guide)_color", "\\1_colour", nm)
  to <- sub("^((geom|stat)_(bin|density|summary))2d", "\\1_2d", to)
  to <- sub("^stat_binhex$", "stat_bin_hex", to)
  if (identical(to, "theme_grey")) to <- "theme_gray"
  to
}

# A ggplot2 function the catalog has not, as an entry: its kind, no card
# (no rank, no label); its arguments are its formals and, by its kind,
# the shared ones it takes.  NULL for a name ggplot2 does not export, or
# that is no function of a plot (ggsave), or that rtfreporter has too.
# `pkg`: an extension package the figure may call (ggh4x::facet_grid2,
# patchwork::inset_element), its functions of a plot by the same kinds
# and its own (.schema_ext_kind()).
.schema_gg_entry <- function(nm, pkg = "ggplot2") {
  if (!requireNamespace(pkg, quietly = TRUE) || !nm %in% getNamespaceExports(pkg)) {
    return(NULL)
  }
  if (nm %in% getNamespaceExports("rtfreporter")) {
    return(NULL)
  }
  kind <- if (identical(pkg, "ggplot2")) .schema_gg_kind(nm) else .schema_ext_kind(nm, pkg)
  if (is.na(kind)) return(NULL)
  if (identical(kind, "layer") && grepl("^(geom|stat)_", nm)) {
    return(.schema_ggplot_entry(if (identical(pkg, "ggplot2")) nm else paste0(pkg, "::", nm)))
  }
  ent <- list(package = pkg, figure = kind,
              description = sprintf("%s's %s(), by its own arguments.", pkg, nm), args = list())
  # a scales function's `...` passed on: label_percent()'s to
  # label_number() (taken by name, .schema_gg_takes()); any other's to a
  # function of another package (breaks_pretty()'s to pretty()), any name
  if (identical(kind, "scales") && "..." %in% names(formals(getExportedValue(pkg, nm))) &&
      !.schema_scales_number(nm)) {
    ent$dots <- list(style = "named", type = .schema_gg_generic)
  }
  ent
}

# a scales function of numbers whose `...` are label_number()'s
# (label_percent, label_comma, percent_format ...)
.schema_scales_number <- function(nm) {
  nm %in% c("label_comma", "label_percent", "label_dollar", "label_currency", "label_bytes",
            "label_number_si", "label_ordinal", "comma_format",
            "percent_format", "dollar_format", "unit_format", "number_bytes_format",
            "ordinal_format")
}

# the extension packages a figure may call a function of by pkg::fn
.schema_ext_pkgs <- c("ggsurvfit", "ggrepel", "ggh4x", "ggtext", "ggnewscale", "patchwork")

# scales' functions a scale's value is made by (its breaks, labels,
# transformation, palette, out-of-bounds handling, a colour's places):
# kind `scales`, their arguments typed by inst/fig/gg_args.csv
# (scales::label_percent(accuracy = 1), scales::breaks_width(4),
# scales::trans_breaks("log10", function(x) 10^x))
.schema_scales_re <- paste0("^(label_|breaks_|minor_breaks_|transform_|pal_|oob_|cut_|ordinal_)",
                            "|_format$|_breaks$|_trans$|^(trans_new|new_transform|rescale|",
                            "rescale_mid|rescale_max)$")
.schema_scales_kind <- function(nm) {
  if (grepl(.schema_scales_re, nm)) "scales" else NA_character_
}

# the kind of an extension's function: ggplot2's by its prefix, and the
# package's own (ggsurvfit's add_*() on a ggsurvfit plot, patchwork's
# arrangement, ggnewscale's new scale between layers, ggh4x's strips and
# element lists as elements, its panel sizes as a facet's)
.schema_ext_kind <- function(nm, pkg) {
  if (identical(pkg, "scales")) return(.schema_scales_kind(nm))
  if (!pkg %in% .schema_ext_pkgs) return(NA_character_)
  if (identical(pkg, "ggsurvfit") && startsWith(nm, "add_")) return("ggsurvfit_layer")
  if (identical(pkg, "patchwork") &&
      nm %in% c("wrap_plots", "plot_layout", "plot_annotation", "plot_spacer", "wrap_elements",
                "inset_element", "guide_area", "area", "free", "wrap_table", "wrap_ggplot_grob")) {
    return("compose")
  }
  if (identical(pkg, "ggnewscale") && startsWith(nm, "new_scale")) return("layer")
  if (identical(pkg, "ggh4x")) {
    if (startsWith(nm, "strip_") || startsWith(nm, "elem_list_")) return("element")
    if (nm %in% c("force_panelsizes", "facetted_pos_scales")) return("facet")
  }
  if (identical(pkg, "ggtext") && startsWith(nm, "element_")) return("element")
  if (nm %in% .schema_gg_helpers) return(NA_character_)
  .schema_gg_kind(nm)
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
  if ("..." %in% fa && identical(pkg, "scales") && .schema_scales_number(nm)) {
    fa <- c(fa, names(formals(scales::label_number)))
  }
  # coord_fixed(ratio, ...): coord_cartesian()'s; labs(...): the labels by
  # aesthetic
  if ("..." %in% fa && startsWith(nm, "coord_")) fa <- c(fa, names(formals(ggplot2::coord_cartesian)))
  # theme(...): ggplot2 4.0's default palettes (palette.colour.discrete),
  # settings of its element tree with no formal of their own
  if ("..." %in% fa && identical(nm, "theme")) {
    tree <- tryCatch(names(getExportedValue("ggplot2", "get_element_tree")()), error = function(e) NULL)
    fa <- c(fa, grep("^palette[.]", tree, value = TRUE))
  }
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
  out <- setdiff(names(formals(getExportedValue("ggplot2", ctor))), c("scale_name", "super", "call"))
  # scale_y_log10(...), _sqrt(), _reverse(): scale_y_continuous()'s (its
  # sec.axis, position)
  if (grepl("^scale_[xy]_(log10|sqrt|reverse)$", nm)) {
    out <- c(out, names(formals(getExportedValue("ggplot2", sub("_[a-z0-9]+$", "_continuous", nm)))))
  }
  unique(out)
}

# ggplot2's guide arguments before 3.5.0, taken still (deprecated) and
# passed into the guide's `theme`: the theme() setting each is now
.schema_guide_old_args <- c(
  title.position = "legend.title.position", title.theme = "legend.title",
  title.hjust = "legend.title", title.vjust = "legend.title", label = "legend.text",
  label.position = "legend.text.position", label.theme = "legend.text", label.hjust = "legend.text",
  label.vjust = "legend.text", keywidth = "legend.key.width", keyheight = "legend.key.height",
  barwidth = "legend.key.width", barheight = "legend.key.height", byrow = "legend.byrow",
  frame.colour = "legend.frame", frame.linewidth = "legend.frame", frame.linetype = "legend.frame",
  ticks = "legend.ticks", ticks.colour = "legend.ticks", ticks.linewidth = "legend.ticks",
  axis = "legend.axis.line", axis.colour = "legend.axis.line", axis.linewidth = "legend.axis.line",
  axis.arrow = "legend.axis.line", default.unit = "")

# the warning for a guide's argument of before ggplot2 3.5.0
# (guide_legend(title.position = "top")), or NULL for any other
.schema_guide_old_text <- function(fn, ent, a) {
  if (!identical(ent$figure, "guide") || !a %in% names(.schema_guide_old_args) ||
      !grepl("^guide_(legend|colou?rbar|bins|colou?rsteps)$", .schema_fn_name(fn))) {
    return(NULL)
  }
  to <- .schema_guide_old_args[[a]]
  sprintf("%s(%s = ) is deprecated since ggplot2 3.5.0%s", .schema_fn_name(fn), a,
          if (nzchar(to)) sprintf(": give it as the guide's theme, theme = {fn: theme, args: {%s: ...}}",
                                  to) else "")
}

# The theme() setting a theme_sub_*() argument stands for (ggplot2 4.0):
# theme_sub_axis_x(text =) is axis.text.x, theme_sub_legend(key.size =)
# legend.key.size; theme()'s own argument as it is
.schema_theme_sub_setting <- function(nm, a) {
  if (!startsWith(nm, "theme_sub_")) return(a)
  part <- sub("^theme_sub_", "", nm)
  if (!startsWith(part, "axis")) return(paste(part, a, sep = "."))
  side <- c(axis = "", axis_x = "x", axis_y = "y", axis_bottom = "x.bottom", axis_top = "x.top",
            axis_left = "y.left", axis_right = "y.right")[[part]]
  paste(c("axis", a, if (nzchar(side)) side), collapse = ".")
}

.schema_theme_sides <- c("x", "y", "top", "bottom", "left", "right", "x.top", "x.bottom",
                         "y.left", "y.right", "theta", "r", "inside")

# The rows of inst/fig/gg_args.csv for a function of a kind: first those
# written for it alone (the `only` column: function names, `pkg::fn` for an
# extension's, `*` for any ending: ggh4x::facet_*), then the kind's shared
# ones.  `via`: the layer function that also gives the item its parameters
# (geom_bar(stat = "bin") has stat_bin()'s).
.schema_gg_rows <- function(g, kind, key, via = NULL) {
  g <- g[g$kind == kind, , drop = FALSE]
  for_it <- function(only) {
    pats <- trimws(strsplit(only, ",", fixed = TRUE)[[1L]])
    any(vapply(pats, function(p) {
      rx <- utils::glob2rx(p)
      grepl(rx, key) || (!is.null(via) && grepl(rx, via))
    }, NA))
  }
  own <- !is.na(g$only) & vapply(ifelse(is.na(g$only), "", g$only), for_it, NA)
  # a row for other functions only is not this one's at all
  g <- g[own | is.na(g$only), , drop = FALSE]
  g[order(is.na(g$only)), , drop = FALSE]
}

# The entry with the shared arguments of its kind the function takes,
# typed (an argument the catalog types itself stays as it says); any
# other argument a scale takes through `...`, untyped.  `extra`: more
# names the function takes (a layer's stat's parameters).
.schema_fig_common <- function(ent, nm, extra = character(), via = NULL) {
  kind <- ent$figure
  if (is.null(kind)) return(ent)
  takes <- unique(c(.schema_gg_takes(nm, ent$package %||% "ggplot2"), names(ent$args), extra))
  # what a card shows of its `...` (add_quantile's linetype, passed on to
  # its layer)
  if (!is.null(ent$dots)) takes <- unique(c(takes, unlist(ent$visible)))
  g <- .fig_gg_args()
  # a theme_sub_*() argument is the setting it stands for (below), not
  # theme()'s of its name (theme_sub_axis_x(text =) is axis.text.x)
  key <- if (identical(ent$package %||% "ggplot2", "ggplot2")) nm else paste0(ent$package, "::", nm)
  g <- .schema_gg_rows(g, kind, key, via)
  g <- g[g$arg %in% takes & !startsWith(nm, "theme_sub_"), , drop = FALSE]
  for (i in seq_len(nrow(g))) {
    a <- g$arg[i]
    old <- ent$args[[a]]
    if (!is.null(old$type) && !identical(old$type, .schema_gg_generic)) next
    new <- list(type = g$type[i], label = list(en = g$en[i], ja = g$ja[i]))
    if (!is.na(g$help[i])) new$description <- g$help[i]
    ent$args[[a]] <- c(new, old[setdiff(names(old), names(new))])
  }
  # a theme setting of one side or axis (axis.text.x.top, legend.key.spacing.y)
  # is typed as the setting it refines; a theme_sub_*() argument as the
  # setting it stands for (theme_sub_axis_bottom(text =) is axis.text.x.bottom)
  if (identical(kind, "theme")) {
    th <- .fig_gg_args()
    th <- th[th$kind == "theme", , drop = FALSE]
    own <- .schema_all(figure = FALSE)$theme$args
    for (a in setdiff(takes, names(ent$args))) {
      full <- .schema_theme_sub_setting(nm, a)
      # a setting theme()'s card types itself (legend.position)
      if (!identical(full, a) && !is.null(own[[full]]$type)) {
        ent$args[[a]] <- c(own[[full]], list(description = sprintf("theme(%s = ).", full)))
        next
      }
      p <- full
      while (grepl(".", p, fixed = TRUE) && !p %in% th$arg) p <- sub("\\.[^.]*$", "", p)
      i <- match(p, th$arg)
      suf <- substring(full, nchar(p) + 2L)
      if (is.na(i) || !(suf %in% .schema_theme_sides || !nzchar(suf))) next
      ent$args[[a]] <- if (nzchar(suf)) {
        list(type = th$type[i],
             label = list(en = sprintf("%s (%s)", th$en[i], suf),
                          ja = sprintf("%s\uff08%s\uff09", th$ja[i], suf)))
      } else {
        list(type = th$type[i], label = list(en = th$en[i], ja = th$ja[i]))
      }
      if (!identical(full, a)) ent$args[[a]]$description <- sprintf("theme(%s = ).", full)
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
  if (!is.null(ent)) ent$via <- other
  ent
}

# The entry an item is checked against: its function's, with the
# parameters of the layer it names as its stat or geom
.schema_item_entry <- function(ent, fn, args) {
  part <- .schema_layer_partner(fn, args)
  if (is.null(ent) || is.null(part)) return(ent)
  for (k in setdiff(names(part$args), names(ent$args))) ent$args[[k]] <- part$args[[k]]
  ent$aes <- unique(c(ent$aes, part$aes))
  .schema_fig_common(ent, .schema_fn_name(fn), via = part$via)
}
