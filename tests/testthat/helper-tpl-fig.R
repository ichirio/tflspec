# A figure template in a study: the study made from XPT data, the report
# written as a template SPEC and read back, its three uses checked against
# each other (the template SPEC's design run, A: the program with the
# template's call, B: the written-out report read back), and the program
# of the design (no call) checked for tflspec functions.

tplfig_png <- function(p) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f))
  ggplot2::ggsave(f, p, width = 9, height = 5, dpi = 72)
  unname(tools::md5sum(f))
}

# a study of the package's datasets as XPT files (tfl_ai_study_init()); the
# datasets as the study reads them
tplfig_study <- function(d, datasets, id = "FIG-1") {
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in datasets) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = id)
  lapply(stats::setNames(nm = datasets), function(x)
    as.data.frame(haven::read_xpt(file.path(d, "adam", paste0(x, ".xpt")))))
}

# The report `o` (a template SPEC) in the study `d`: the figure of each use,
# expected the same; returns the A program, the resolved report and the PNG
tplfig_check <- function(o, d, data) {
  f <- file.path(d, "spec", "outputs", paste0(o$id, ".yml"))
  tfl_write_output(o, f)
  st <- tfl_read_study_spec(d)
  ro <- tfl_resolve_output(tfl_read_output(f), st)
  png_t <- tplfig_png(suppressMessages(tfl_content_run(ro, data)))
  # A: the data steps written out, the call, the report's own items
  a <- tfl_content_code(ro)
  testthat::expect_true(any(grepl(paste0("<- tpl_", o$template$name, "("), a, fixed = TRUE)))
  env <- new.env(parent = .fig_pkg_env(c(attr(a, "libs"), "tflspec")))
  for (n in names(data)) assign(n, data[[n]], envir = env)
  eval(parse(text = tfl_fig_setup_code()), env)
  eval(parse(text = a), env)
  testthat::expect_identical(tplfig_png(env$content), png_t)
  # B: the written-out report, read back
  b <- tfl_template_to_spec(tfl_read_output(f), st)
  b$id <- paste0(o$id, "-B")
  fb <- file.path(d, "spec", "outputs", paste0(b$id, ".yml"))
  tfl_write_output(b, fb)
  rb <- tfl_resolve_output(tfl_read_output(fb), st)
  testthat::expect_null(attr(rb, "template"))
  testthat::expect_identical(tplfig_png(suppressMessages(tfl_content_run(rb, data))), png_t)
  # the program of the design (no call): no tflspec function in it
  code <- tfl_content_code(ro, template = "items")
  testthat::expect_false(any(grepl("tpl_", code, fixed = TRUE)))
  calls <- unique(all.names(parse(text = code), functions = TRUE))
  testthat::expect_length(intersect(calls, getNamespaceExports("tflspec")), 0L)
  list(a = a, ro = ro, png = png_t, b_file = fb)
}
