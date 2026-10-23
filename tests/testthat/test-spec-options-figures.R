# rtf_figures()'s drawing arguments (rtfreporter#719) and the study's
# rtfreporter options (defaults.yml's options:) in the SPEC (#452)

test_that("page.rtf_figures: render_width / render_height / render_dpi / png_device", {
  st <- list(render_width = 7, render_height = 5, render_dpi = 200, png_device = "ragg")
  expect_identical(nrow(tfl_schema_check(tfl_item("rtf_figures", st))), 0L)
  expect_false(nrow(tfl_schema_check(tfl_item("rtf_figures", list(png_device = "jpeg")))) == 0L)
  o <- tfl_output("F-1", "figure", page = list(rtf_figures = st))
  code <- paste(tfl_report_code(o, content = "content"), collapse = "\n")
  for (a in c("render_width = 7", "render_height = 5", "render_dpi = 200", "png_device = \"ragg\"")) {
    expect_match(code, a, fixed = TRUE, info = a)
  }
  skip_if_not_installed("rtfreporter")
  skip_if(utils::packageVersion("rtfreporter") < "0.8.2.9096")
  expect_true(all(names(tfl_schema("rtf_figures")$args) %in%
                    names(formals(rtfreporter::rtf_figures))))
})

test_that("the study's options: written by the setup, by rtfreporter's names", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "spec"))
  writeLines(c("options:", "  rtfreporter.png_device: ragg", "  rtfreporter.figure.default_dpi: 300",
               "  rtfreporter.ard_pipe: '|>'", "  rtfreporter.font_lang: {east_asian: ja}"),
             file.path(d, "spec", "defaults.yml"))
  st <- tfl_read_study_spec(d)
  expect_identical(st$options$rtfreporter.png_device, "ragg")
  code <- paste(tflspec:::.study_setup_code(st), collapse = "\n")
  expect_match(code, "rtfreporter.png_device = \"ragg\"", fixed = TRUE)
  expect_match(code, "rtfreporter.figure.default_dpi = 300", fixed = TRUE)
  expect_match(code, "rtfreporter.ard_pipe = \"|>\"", fixed = TRUE)
  expect_match(code, "rtfreporter.font_lang = c(east_asian = \"ja\")", fixed = TRUE)
  # the code runs and sets them
  old <- options(rtfreporter.png_device = NULL, rtfreporter.figure.default_dpi = NULL,
                 rtfreporter.ard_pipe = NULL, rtfreporter.font_lang = NULL)
  on.exit(options(old), add = TRUE)
  eval(parse(text = code), new.env())
  expect_identical(getOption("rtfreporter.png_device"), "ragg")
  expect_identical(getOption("rtfreporter.font_lang"), c(east_asian = "ja"))
})
