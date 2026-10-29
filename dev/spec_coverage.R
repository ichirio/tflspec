# The SPEC coverage table: every exported rtfreporter function and argument
# against what a tflspec SPEC (the YAML spec's content: and page:, the
# sheets' columns) can write.  Writes inst/dev/spec-coverage-rtfreporter.csv and the
# numbers for inst/dev/spec-coverage.md.
#
# Run from the tflspec folder, with rtfreporter@next and this tflspec
# installed:  Rscript dev/spec_coverage.R
#
# covered:
#   yes      a SPEC writes it: a content item checked by the schema
#            (inst/spec/schema.yml), or a page: argument the report code
#            passes on (R/spec_report.R), or a value tflspec fills itself
#   partial  it can be written, but not as the function's own argument (an
#            equivalent plan verb says it), or a content item the schema
#            does not know (no type check, no rank, no help)
#   no       a SPEC cannot write it
# A note starting "[helper]" is an API that is no SPEC item by design (an
# option, an inspection helper, a file assembler); the summary gives the
# percentage with and without them.

stopifnot(requireNamespace("rtfreporter"), requireNamespace("tflspec"))
`%||%` <- function(a, b) if (is.null(a)) b else a

ns <- getNamespaceExports("rtfreporter")
ns <- sort(ns[vapply(ns, function(f) is.function(getExportedValue("rtfreporter", f)), NA)])
sch <- tflspec::tfl_schema()
sch <- sch[vapply(sch, function(e) identical(e$package %||% "rtfreporter", "rtfreporter"), NA)]

# the page: functions (R/spec_report.R passes on the schema's arguments)
page_fns <- c("rtf_page", "rtf_default_format", "rtf_document", "rtf_tables",
              "rtf_figures", "rtf_titles", "rtf_footnotes", "rtf_header",
              "rtf_footer", "generate_rtfreport")
# arguments tflspec fills itself (the pipe, the page's lines, the study)
implicit <- list(
  rtf_document = c(page = "page.rtf_page", default_format = "page.rtf_default_format",
                   tokens = "page.tokens (+ OUTPUT_ID / OUTPUT_LABEL)",
                   file = "the study's RTF path (toc / settings)"),
  rtf_tables = c(doc = "the pipe", tables = "the content",
                 titles = "page.titles (contents: per object)",
                 footnotes = "page.footnotes (contents: per object)"),
  rtf_figures = c(doc = "the pipe", figures = "the content",
                  titles = "page.titles (contents: per object)",
                  footnotes = "page.footnotes (contents: per object)"),
  rtf_titles = c(doc = "the pipe", titles = "page.titles"),
  rtf_footnotes = c(doc = "the pipe", footnotes = "page.footnotes"),
  rtf_header = c(rows = "page.header lines"),
  rtf_footer = c(rows = "page.footer lines"),
  rtf_section = c(doc = "the pipe", header = "page.header / rtf_header",
                  footer = "page.footer / rtf_footer"),
  rtfplot = c(x = "the figure content", render_width = "rtf_figures / tfl_image",
              render_height = "rtf_figures / tfl_image",
              render_dpi = "rtf_figures / tfl_image (dpi)",
              png_device = "rtf_figures.png_device",
              align = "rtf_figures.align"),
  generate_rtfreport = c(doc = "the pipe", file_path = "the study's RTF path"),
  normalize_ard = c(x = "data: (the ARD)"),
  listing_col = c(),
  widen_ard = c())
# rtf_tables' look: said by the plan verbs (the plan's attributes are read)
via_plan <- c(
  border = "plan_style(border)", font = "plan_style(font)",
  row_height_twips = "plan_style", row_height_exact = "plan_style",
  header_row_height_twips = "plan_style", blank_row_height_twips = "plan_style",
  cell_padding_left_twips = "plan_style", cell_padding_right_twips = "plan_style",
  cell_valign = "plan_style", table_align = "plan_style", markup = "plan_style",
  blank_row_normalize = "plan_style", table_width_twips = "plan_style",
  table_width_pct = "plan_style", table_width_pct_of_writable = "plan_style",
  col_header = "plan_col_header", col_header_align = "plan_col_header",
  spanning_header = "plan_col_header(lines)", col_rel_width = "plan_columns(widths)",
  column_widths_twips = "plan_columns(widths)", row_title = "plan_columns(row_title)",
  blank_rows = "plan_blanks",
  style = "plan_style (rtf_table_style()'s fields: border_header ... underline)")
# deprecated arguments: no SPEC item by design
deprecated <- c(rtf_tables.read_gt = "deprecated: as_rtftables() reads a gt table")
# APIs that are no SPEC item by design
helper <- c(
  rtfreporter_ai_manual = "AI manual", rtfreporter_options = "package options",
  rtfreporter_reset_defaults = "package options", rtf_config = "package options",
  list_ard_keys = "inspection", plan_header_tokens = "inspection",
  plan_n_candidates = "inspection", plan_template = "writes a plan skeleton",
  text_width_in = "measurement", listing_disp_width = "measurement",
  listing_code = "writes code", listing_wrap_code = "writes code",
  assemble_rtf = "file assembly", assemble_files = "file assembly",
  assemble_toc = "file assembly", assemble_spec = "file assembly",
  assemble_from_spec = "file assembly", assemble_folder = "file assembly",
  toc_heading = "file assembly", toc_entry = "file assembly",
  rtf_replace_text = "post-processing of a file", rtf_text_tokens = "inspection",
  rtfreporter_validation_kit = "validation kit")
out_of_scope <- c(plan_layers = "plan_layer / table_plan layers: designed in another session")

rows <- list()
add <- function(fn, arg, cov, where, note = "") {
  rows[[length(rows) + 1L]] <<- data.frame(package = "rtfreporter", `function` = fn,
                                           argument = arg, covered = cov,
                                           where = where, note = note, check.names = FALSE)
}
data_args <- c("x", "plan", ".plan", "data", "doc")
for (fn in ns) {
  f <- formals(getExportedValue("rtfreporter", fn))
  args <- names(f)
  if (!length(args)) args <- "(none)"
  e <- sch[[fn]]
  imp <- implicit[[fn]] %||% character()
  for (a in args) {
    if (fn %in% names(helper)) {
      add(fn, a, "no", "", paste("[helper]", helper[[fn]]))
    } else if (fn %in% names(out_of_scope)) {
      add(fn, a, "no", "", paste("[out of scope]", out_of_scope[[fn]]))
    } else if (a %in% names(imp)) {
      add(fn, a, "yes", imp[[a]], "filled by tflspec")
    } else if (fn %in% page_fns) {
      if (fn == "rtf_document" && a == "watermark" &&
          !grepl("rtf_watermark", e$args$watermark$type %||% "", fixed = TRUE)) {
        add(fn, a, "partial", "page.rtf_document.watermark", "a word only: no rtf_watermark() look")
      } else if (!is.null(e) && a %in% names(e$args)) {
        add(fn, a, "yes", paste0("page.", fn, ".", a), "schema.yml; R/spec_report.R")
      } else if (paste(fn, a, sep = ".") %in% names(deprecated)) {
        add(fn, a, "no", "", paste("[helper]", deprecated[[paste(fn, a, sep = ".")]]))
      } else if (fn == "rtf_tables" && a %in% names(via_plan)) {
        add(fn, a, "partial", paste0("content: ", via_plan[[a]]), "said by the plan verb, not page.rtf_tables")
      } else {
        add(fn, a, "no", "", "page: does not pass it on")
      }
    } else if (!is.null(e)) {
      where <- if (fn %in% c("rtf_border", "rtf_col_header", "listing_col", "cell_rows", "rtf_watermark"))
        paste0("{fn: ", fn, "} value") else paste0("content: ", fn)
      if (a %in% data_args) add(fn, a, "yes", "the pipe", "")
      else if (a == "..." && !is.null(e$dots)) add(fn, a, "yes", where, "schema dots grammar")
      else if (a %in% names(e$args)) add(fn, a, "yes", where, "schema.yml type")
      else add(fn, a, "yes", where, "formals pass through (no schema type)")
    } else if (grepl("^plan_", fn)) {
      add(fn, a, "partial", paste0("content: ", fn),
          "runs as a content item, but schema.yml has no entry (no check, rank or help)")
    } else if (fn %in% names(implicit)) {
      add(fn, a, "no", "", "tflspec calls it, this argument is not said")
    } else {
      add(fn, a, "no", "", "lower-level API: a SPEC cannot call it")
    }
  }
}
cov <- do.call(rbind, rows)
utils::write.csv(cov, "inst/dev/spec-coverage-rtfreporter.csv", row.names = FALSE, na = "")

pct <- function(d) {
  w <- c(yes = 1, partial = 0.5, no = 0)
  sprintf("%.1f%% (yes %d, partial %d, no %d of %d)", 100 * mean(w[d$covered]),
          sum(d$covered == "yes"), sum(d$covered == "partial"), sum(d$covered == "no"), nrow(d))
}
spec <- cov[!startsWith(cov$note, "[helper]"), ]
cat("all arguments:      ", pct(cov), "\n")
cat("SPEC-relevant:      ", pct(spec), "\n")
fns <- tapply(cov$covered, cov$`function`, function(v) if (all(v == "no")) "no" else if (all(v == "yes")) "yes" else "partial")
cat("functions:          ", paste(names(table(fns)), table(fns), collapse = ", "), "\n")
