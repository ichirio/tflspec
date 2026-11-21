# A figure's render settings are its content's tfl_image, no more its page's
# (decided 2026-11-19): the same values write the same program (so the same
# RTF); the defaults are the study's figure style standard's (defaults.yml
# figure_style:, the figure type's row, else all figures'), else built-in;
# the page keys that said them, and study.yml settings: figure:, stop.

# a study whose figure style settings are `rows` (records type, key, value);
# NULL: tflspec's built-in style
fi_study <- function(rows = NULL) {
  structure(list(study = list(settings = list()),
                 data = list(), defaults = list(content = list(), page = list()),
                 figure_style = if (!is.null(rows)) list(settings = rows) else list(),
                 codelists = list(), patterns = list(), toc = list(), dir = NULL),
            class = "tfl_study_spec")
}
fi_row <- function(key, value, type = NA) list(type = type, key = key, value = value)

fi_items <- function(img = NULL) {
  c(list(tfl_item("ggplot", list(data = "df", mapping = list(x = "AVAL", y = "CHG"))),
         tfl_item("geom_point", list())),
    if (!is.null(img)) list(tfl_item("tfl_image", img)))
}

fi_fig_call <- function(o, study = NULL) {
  code <- tfl_report_code(o, study = study, content = "content")
  # (its lines as one: the call between the pipes)
  code <- gsub("\\s*\n\\s*", " ", paste(code, collapse = "\n"))
  code <- gsub(" [)]", ")", gsub("[(] ", "(", code))
  sub("\\s+$", "", regmatches(code, regexpr("rtf_figures\\([^|]*", code)))
}

test_that("a figure of items: tfl_image writes the call page.rtf_figures wrote before", {
  page <- list(rtf_page = list(orientation = "landscape"))
  o <- tfl_output("F-1", "figure", page = page,
                  content = fi_items(list(width_in = 9.5, height_in = 4, align = "left",
                                          dpi = 150L, png_device = "ragg")))
  # (as the page's rtf_figures: {width_twips: 13680, height_twips: 5760,
  # align: left, png_device: ragg} with tfl_image: {dpi: 150} wrote it)
  expect_identical(fi_fig_call(o), paste0(
    "rtf_figures(rtfplot(content, render_width = 9.5, render_height = 4, render_dpi = 150, ",
    "png_device = \"ragg\"), align = \"left\")"))
})

test_that("a figure of R or a template's call: placed at its size and drawn at it", {
  o <- tfl_output("F-1", "figure", content = list(tfl_item("tfl_image", list(
    width_in = 9.5, height_in = 5.5, align = "right", dpi = 200L, png_device = "png"))))
  expect_identical(fi_fig_call(o), paste0(
    "rtf_figures(content, width_twips = 13680, height_twips = 7920, align = \"right\", ",
    "render_width = 9.5, render_height = 5.5, render_dpi = 200, png_device = \"png\")"))
  # nothing said: the figure style standard's all-figures size (7.3 x 4.2 in, 300 dpi)
  expect_identical(fi_fig_call(tfl_output("F-1", "figure")), paste0(
    "rtf_figures(content, width_twips = 10512, height_twips = 6048, render_width = 7.3, ",
    "render_height = 4.2, render_dpi = 300)"))
})

test_that("the defaults are the figure style standard's by the figure's type", {
  # tflspec's built-in style: km 8.33 x 4.79, swimmer 7.71 x 4.17, the rest 7.3 x 4.2
  km <- tfl_output("F-1", "figure", content = fi_items())
  km$template <- "km_simple"
  expect_identical(fi_fig_call(km),
                   "rtf_figures(rtfplot(content, render_width = 8.33, render_height = 4.79, render_dpi = 300))")
  sw <- tfl_output("F-1", "figure")
  sw$template <- list(name = "swimmer_bar")
  expect_match(fi_fig_call(sw), "render_width = 7.71, render_height = 4.17", fixed = TRUE)
  expect_match(fi_fig_call(tfl_output("F-1", "figure", content = fi_items())),
               "render_width = 7.3, render_height = 4.2, render_dpi = 300", fixed = TRUE)
  # a study's own rows: the type's row, else the all-figures row; units in cm
  st <- fi_study(list(fi_row("width", 20), fi_row("height", 10), fi_row("units", "cm"),
                      fi_row("dpi", 150), fi_row("align", "left"),
                      fi_row("width", 25.4, "km"), fi_row("rtf_format", "emf", "km")))
  d <- tfl_image_defaults(st, "km")
  expect_equal(d$width_in, 10)
  expect_equal(d$height_in, 10 / 2.54)
  expect_identical(d[c("dpi", "align", "rtf_format", "png_device", "format")],
                   list(dpi = 150L, align = "left", rtf_format = "emf", png_device = "auto", format = "png"))
  expect_identical(attr(d, "source")[c("width_in", "height_in", "rtf_format", "png_device")],
                   c(width_in = "figure_style.km", height_in = "figure_style",
                     rtf_format = "figure_style.km", png_device = "builtin"))
  all <- tfl_image_defaults(st)
  expect_equal(all$width_in, 20 / 2.54)
  expect_identical(all$rtf_format, "png")
  expect_identical(unname(attr(all, "source")["width_in"]), "figure_style")
  expect_error(tfl_image_defaults(st, "pie"), "the types are .*km")
  # the resolved report carries them in its tfl_image
  o <- tfl_output("F-1", "figure", extends = "none", content = fi_items())
  last <- utils::tail(tfl_resolve_output(o, st)$content, 1L)[[1L]]
  expect_identical(last$fn, "tfl_image")
  expect_identical(last$args$align, "left")
  # a study folder works too
  dir <- system.file("extdata", "spec-yaml", package = "tflspec")
  expect_equal(tfl_image_defaults(dir, "km")$width_in, 8.33)
})

test_that("a report's own tfl_image wins over the figure style standard's", {
  km <- tfl_output("F-1", "figure", content = fi_items(list(width_in = 5, dpi = 150L)))
  km$template <- "km_simple"
  expect_identical(fi_fig_call(km),
                   "rtf_figures(rtfplot(content, render_width = 5, render_height = 4.79, render_dpi = 150))")
  st <- fi_study(list(fi_row("align", "left"), fi_row("rtf_format", "emf")))
  own <- tfl_output("F-1", "figure", extends = "none",
                    content = fi_items(list(align = "right", rtf_format = "png")))
  call <- fi_fig_call(tfl_resolve_output(own, st), st)
  expect_match(call, "align = \"right\"", fixed = TRUE)
  expect_false(grepl("render_format", call, fixed = TRUE))
})

test_that("study.yml settings: figure: stops, naming the figure_style key", {
  d <- tempfile("fi-settings-")
  dir.create(file.path(d, "spec"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines(c("id: FI-1", "settings:", "  figure: {width_in: 7, dpi: 150, file: x.png}"),
             file.path(d, "study.yml"))
  expect_error(tfl_read_study_spec(d), "width_in -> figure_style key width", fixed = TRUE)
  expect_error(tfl_read_study_spec(d), "file -> the report's tfl_image", fixed = TRUE)
  st <- fi_study()
  st$study$settings$figure <- list(dpi = 150L)
  expect_error(tfl_image_defaults(st), "dpi -> figure_style key dpi", fixed = TRUE)
})

test_that("a figure style with no size, dpi or look: the built-in defaults", {
  st <- fi_study(list(fi_row("theme", "classic")))
  d <- tfl_image_defaults(st, "km")
  expect_identical(unname(attr(d, "source")), rep("builtin", 7L))
  expect_identical(d[c("width_in", "height_in", "dpi", "align", "rtf_format")],
                   list(width_in = 6.5, height_in = 4.5, dpi = 300L, align = "center", rtf_format = "png"))
  # nothing written: rtfplot()'s and rtf_figures()'s own
  expect_identical(fi_fig_call(tfl_output("F-1", "figure"), st), "rtf_figures(content)")
  expect_identical(fi_fig_call(tfl_output("F-1", "figure", extends = "none", content = fi_items()), st),
                   "rtf_figures(rtfplot(content, render_width = 6.5, render_height = 4.5, render_dpi = 300))")
  expect_identical(tflspec:::.fig_image_fill(fi_items(), st), fi_items())
})

test_that("page.rtf_figures's own settings and a layout row's size stop, saying where they went", {
  for (k in c("width_twips", "height_twips", "align", "render_width", "render_height",
              "render_dpi", "png_device")) {
    pg <- list(rtf_figures = stats::setNames(list(if (k %in% c("align", "png_device")) "left" else 100), k))
    o <- tfl_output("F-1", "figure", page = pg, content = fi_items())
    expect_error(tfl_report_code(o, content = "content"), paste0("page.rtf_figures.", k), fixed = TRUE,
                 info = k)
  }
  # what tables and figures share stays
  ok <- tfl_output("F-1", "figure", page = list(rtf_figures = list(page_break = FALSE, space_before = 1L)))
  expect_match(fi_fig_call(ok), "page_break = FALSE, space_before = 1", fixed = TRUE)
  m <- tflspec:::.fig_page_moved_said(list(layout = list(list(object = "km", width_in = 6))))
  expect_match(m, "page.layout (object km): width_in", fixed = TRUE)
})

test_that("tfl_check_spec() flags a page that says a figure's own setting", {
  d <- tempfile("fi-check-")
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines("id: FI-1", file.path(d, "study.yml"))
  o <- tfl_output("F-1", "figure", extends = "none", content = fi_items(list(dpi = 300L)),
                  page = list(rtf_figures = list(width_twips = 9000L)))
  tfl_write_output(o, file.path(d, "spec", "outputs", "F-1.yml"))
  p <- tfl_check_spec(d)
  hit <- p[p$part == "page" & p$item == "page.rtf_figures.width_twips", , drop = FALSE]
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$level, "error")
  expect_match(hit$message, "tfl_image: {width_in", fixed = TRUE)
})

test_that("the report sheet's figure size converts into the content's tfl_image", {
  notes <- character()
  note <- function(...) notes <<- c(notes, paste(..., collapse = " "))
  o <- tfl_output("F-1", "figure", content = fi_items(list(dpi = 300L)),
                  page = list(.image = list(width_in = "fit", height_in = 5)))
  out <- tflspec:::.conv_fig_image(o, note)
  expect_null(out$page$.image)
  expect_identical(out$content[[3L]]$args, list(dpi = 300L, width_in = "fit", height_in = 5))
  # a figure of none: the item made
  o2 <- tfl_output("F-1", "figure", page = list(.image = list(width_in = 6)))
  expect_identical(tflspec:::.conv_fig_image(o2, note)$content[[1L]]$args, list(width_in = 6))
})

test_that("tfl_image: rtf_format emf is render_format; png and none write what they did; another stops", {
  page <- list(rtf_page = list(orientation = "landscape"))
  items <- function(img) tfl_output("F-1", "figure", page = page, content = fi_items(img))
  plain <- fi_fig_call(items(list(width_in = 9.5, dpi = 150L)))
  expect_identical(fi_fig_call(items(list(width_in = 9.5, dpi = 150L, rtf_format = "png"))), plain)
  expect_false(grepl("render_format", plain, fixed = TRUE))
  emf <- fi_fig_call(items(list(width_in = 9.5, dpi = 150L, png_device = "ragg", rtf_format = "emf")))
  expect_identical(emf, paste0("rtf_figures(rtfplot(content, render_width = 9.5, render_height = 4.2, ",
                               "render_format = \"emf\"))"))
  # a figure of R / a template's call: rtf_figures()'s own
  tp <- tfl_output("F-1", "figure", content = list(tfl_item("tfl_image", list(dpi = 200L, rtf_format = "emf"))))
  expect_identical(fi_fig_call(tp), paste0("rtf_figures(content, width_twips = 10512, height_twips = 6048, ",
                                         "render_width = 7.3, render_height = 4.2, render_format = \"emf\")"))
  expect_error(fi_fig_call(items(list(rtf_format = "svg"))), "rtf_format is png or emf, not svg", fixed = TRUE)
  expect_false(nrow(tfl_schema_check(tfl_item("tfl_image", list(rtf_format = "jpeg")))) == 0L)
})
