km_skip <- function() {
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("dplyr")
}

km_png <- function(p) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f))
  ggplot2::ggsave(f, p, width = 9, height = 5, dpi = 72)
  unname(tools::md5sum(f))
}

test_that("tpl_km_simple(): the curves of the data it is given, a column bare or as text", {
  km_skip()
  saf <- adsl$USUBJID[adsl$SAFFL == "Y"]
  df <- adtte[adtte$PARAMCD == "PFS" & adtte$USUBJID %in% saf, ]
  p <- tpl_km_simple(df, group = TRT01A, time_unit = "days")
  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$x, "Time (Days)")
  expect_identical(km_png(p), km_png(tpl_km_simple(df, group = "TRT01A", time_unit = "days")))
  # the settings over the template's
  q <- tpl_km_simple(df, group = TRT01A, plot = list(x_label = "Days"))
  expect_identical(q$labels$x, "Days")
  expect_error(tpl_km_simple("adtte"), "a data frame")
})

test_that("in a study: A (the data steps, the call) = B (the design) = the template SPEC", {
  skip_on_cran()
  km_skip()
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adtte")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "KM-1")
  o <- tfl_output("F-9-1", "figure", population = "SAF", label = "KM curves",
                  extends = "defaults",
                  template = list(name = "km_simple",
                                  args = list(param = "PFS", group = "TRT01A", time_unit = "days")),
                  content = list(tfl_item("geom_hline", list(yintercept = 0.5, linetype = "dashed")),
                                 tfl_item("labs", list(y = "Probability of no event"))))
  f <- file.path(d, "spec", "outputs", "F-9-1.yml")
  tfl_write_output(o, f)
  st <- tfl_read_study_spec(d)
  ro <- tfl_resolve_output(tfl_read_output(f), st)
  data <- lapply(c(adsl = "adsl", adtte = "adtte"), function(x)
    as.data.frame(haven::read_xpt(file.path(d, "adam", paste0(x, ".xpt")))))
  # the template SPEC: its design run
  p_t <- suppressMessages(tfl_content_run(ro, data))
  # A: the data steps written out, then the call and the report's own items
  a <- tfl_content_code(ro)
  txt <- paste(a, collapse = "\n")
  expect_match(txt, "semi_join(", fixed = TRUE)
  expect_match(txt, "filter(PARAMCD == \"PFS\")", fixed = TRUE)
  expect_match(txt, "content <- tpl_km_simple(adtte_pfs, group = TRT01A, time_unit = \"days\") +",
               fixed = TRUE)
  expect_match(txt, "labs(y = \"Probability of no event\")", fixed = TRUE)
  env <- new.env(parent = globalenv())
  for (n in names(data)) assign(n, data[[n]], envir = env)
  eval(parse(text = tfl_fig_setup_code()), env)
  suppressPackageStartupMessages(for (l in attr(a, "libs")) library(l, character.only = TRUE))
  eval(parse(text = a), env)
  expect_identical(km_png(env$content), km_png(p_t))
  # B: the written-out report, read back
  fb <- file.path(d, "spec", "outputs", "F-9-1-B.yml")
  b <- tfl_template_to_spec(tfl_read_output(f), st)
  b$id <- "F-9-1-B"
  tfl_write_output(b, fb)
  rb <- tfl_resolve_output(tfl_read_output(fb), st)
  expect_null(attr(rb, "template"))
  expect_identical(km_png(suppressMessages(tfl_content_run(rb, data))), km_png(p_t))
  # the program of the design (no call): no tflspec function in it
  code <- tfl_content_code(ro, template = "items")
  calls <- unique(all.names(parse(text = code), functions = TRUE))
  expect_false(any(grepl("tpl_km_simple", code, fixed = TRUE)))
  expect_length(intersect(calls, getNamespaceExports("tflspec")), 0L)
})

test_that("a report that replaces a template's layer: its design, not the call", {
  km_skip()
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(d)
  file.copy(list.files(system.file("extdata", "spec-yaml", package = "tflspec"),
                       full.names = TRUE), d, recursive = TRUE)
  s <- tfl_read_study_spec(d)
  o <- tfl_output("F-9", "figure",
                  template = list(name = "km_simple", args = list(param = "PFS", group = "TRT01A")),
                  content = list(tfl_item("censor_mark", list(shape = "plus", size = 2, stroke = 0.5))))
  ro <- tfl_resolve_output(o, s)
  expect_false(attr(ro, "template")$ok)
  expect_message(code <- tfl_content_code(ro), "its design is written")
  expect_false(any(grepl("tpl_km_simple", code, fixed = TRUE)))
})
