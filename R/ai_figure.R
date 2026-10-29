# ============================================================================
#  A study's figures in the AI flow: a figure request's answer (a figure
#  design, tfl_ai_parse(task = "figure")) as the figure's report file, and
#  the figure drawn by tfl_ai_run()
# ----------------------------------------------------------------------------
#  The report file keeps the design's own form -- `data: {source: code,
#  steps, stats}`, the layers as content items (`km_curve: {...}`), `plot:`
#  and `template:` -- as tfl_convert_book(figures = "design") writes it:
#  tfl_sheets_from_yaml() gives it back as the same design (its code
#  identical), the one tflplanner's figure designer opens and draws with
#  tfl_fig_design_code().
# ============================================================================

# The figure design of an answer as its report file (a tfl_output).
.ai_fig_output <- function(design, id, ctx) {
  if (inherits(design, "tfl_ai_tpl_figure")) return(.ai_tpl_fig_output(design, id, ctx))
  y <- unclass(design)
  data <- list(source = "code")
  if (length(y$data)) data$steps <- y$data
  if (length(y$stats)) data$stats <- y$stats
  content <- lapply(y$layers %||% list(), function(l) {
    tfl_item(l$layer %||% "layer", l[setdiff(names(l), "layer")])
  })
  ttl <- ctx$titles %||% character()
  pop <- ctx$population$id
  o <- tfl_output(id, kind = "figure", label = if (length(ttl)) ttl[1L] else NA_character_,
                  population = if (length(pop)) pop else NA_character_,
                  data = data, content = content,
                  page = if (length(ttl)) list(titles = as.list(ttl)) else list())
  for (k in c("template", "ggplot2_version", "plot", "plots", "compose")) {
    if (length(y[[k]])) o[[k]] <- y[[k]]
  }
  o
}

# A figure answer as a table answer's shape: its report file under
# `outputs`, its assumptions -- for the checks per report and the files'
# writing (.ai_unpack_table()).
.ai_fig_as_files <- function(a, ids, ctx) {
  id <- a$output_id %||% ids[1L]
  if (!identical(id, ids[1L])) {
    return(list(error = sprintf("the answer is for %s, not %s: its header's output_id", id, ids[1L])))
  }
  o <- tryCatch(.ai_fig_output(a$design, id, ctx), error = function(e) e)
  if (inherits(o, "error")) {
    return(list(error = paste("its design could not be written as the report file:",
                              .ai_one_line(conditionMessage(o)))))
  }
  rows <- a$assumption_rows
  if (!is.null(rows) && nrow(rows)) rows$output_id[is.na(rows$output_id)] <- id
  structure(list(task = "table", outputs = stats::setNames(list(o), id),
                 assumption_rows = rows), class = "tfl_ai_answer")
}

# A figure of the study drawn as tflplanner draws it: the report file back
# as its design (tfl_sheets_from_yaml()), its program
# (tfl_fig_design_code()), the datasets it reads from spec/data.yml; the
# PNG where the program saves it, under the study's output path.  The plot,
# for the report's RTF.  Run in the study folder.
.ai_fig_draw <- function(st, id, out_dir) {
  sh <- tfl_sheets_from_yaml(".")
  fd <- sh$figures[[id]]
  if (is.null(fd)) .ard_stop(sprintf("%s has no figure design in its report file.", id))
  args <- c(list(data = fd$data %||% list(), stats = fd$stats %||% list(),
                 plot = fd$plot %||% list(), layers = fd$layers %||% list()),
            fd[setdiff(names(fd), c("data", "stats", "plot", "layers"))])
  args <- args[names(args) %in% names(formals(tfl_fig_design))]
  design <- do.call(tfl_fig_design, args)
  code <- tfl_fig_design_code(design, id, setup = TRUE, save = TRUE, name = "fig",
                              codelists = sh$table$codelists)
  reads <- unique(toupper(attr(code, "reads") %||% character()))
  # (its packages around it, none attached)
  env <- new.env(parent = .fig_pkg_env(attr(code, "libs")))
  ds <- st$data$datasets %||% list()
  have <- toupper(vapply(ds, function(z) z$dataset %||% NA_character_, ""))
  for (r in reads) {
    i <- match(r, have)
    if (is.na(i)) .ard_stop(sprintf("%s reads %s, which spec/data.yml does not list.", id, r))
    assign(tolower(r), .ai_read_dataset(ds[[i]]$path), envir = env)
  }
  code <- as.character(code)
  # the PNG under the study's output path, as the RTF's
  code <- sub(sprintf('file.path("output", "%s.png")', id),
              sprintf('file.path("%s", "%s.png")', out_dir, id), code, fixed = TRUE)
  # after the study's figure setup (theme_tfl(), the palettes, the days in
  # a month), as the program runs after programs/tfl/fig_setup.R
  code <- c(tfl_fig_setup_code(), .fig_strip_library(code))
  suppressMessages(suppressWarnings(eval(parse(text = code), env)))
  fig <- get("fig", envir = env, inherits = FALSE)
  .ai_fig_nothing_drawn(fig, id)
  list(plot = fig, png = file.path(out_dir, paste0(id, ".png")))
}

# A figure that draws nothing -- its data empty after its steps (a
# parameter the data does not have, an analysis set that keeps no subject,
# a filter that keeps no record) -- stops with why, rather than an empty
# page no one notices
.ai_fig_nothing_drawn <- function(fig, id) {
  b <- tryCatch(suppressMessages(suppressWarnings(ggplot2::ggplot_build(fig))),
                error = function(e) NULL)
  if (is.null(b) || !length(b$data)) return(invisible(FALSE))
  n <- vapply(b$data, function(d) if (is.data.frame(d)) nrow(d) else 0L, 1L)
  if (all(n == 0L)) {
    .ard_stop(sprintf(paste(
      "%s: the figure draws nothing -- its data has no rows after its steps (the",
      "parameter, the analysis set, a filter keeps no record): check them"), id))
  }
  invisible(TRUE)
}

# A dataset file as a data frame: .xpt / .sas7bdat (haven), .rds.
.ai_read_dataset <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") return(readRDS(path))
  if (!requireNamespace("haven", quietly = TRUE)) {
    .ard_stop(sprintf("reading %s needs the haven package.", basename(path)))
  }
  d <- if (ext == "xpt") haven::read_xpt(path) else haven::read_sas(path)
  as.data.frame(d)
}
