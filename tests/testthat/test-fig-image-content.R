# A figure's render settings are its content's tfl_image, no more its page's
# (decided 2026-11-19): the same values write the same program (so the same
# RTF), the study's figure settings (study.yml settings: figure:) are the
# defaults, and the page keys that said them stop, named where they went.

fi_study <- function(figure = NULL) {
  structure(list(study = list(settings = if (!is.null(figure)) list(figure = figure)),
                 data = list(), defaults = list(content = list(), page = list()),
                 codelists = list(), patterns = list(), toc = list(), dir = NULL),
            class = "tfl_study_spec")
}

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

test_that("a figure of R or a template's call: placed at its size, as before", {
  o <- tfl_output("F-1", "figure", content = list(tfl_item("tfl_image", list(
    width_in = 9.5, height_in = 5.5, align = "right", dpi = 200L, png_device = "png"))))
  # (as rtf_figures: {width_twips: 13680, height_twips: 7920, align: right,
  # render_dpi: 200, png_device: png} wrote it)
  expect_identical(fi_fig_call(o), paste0(
    "rtf_figures(content, width_twips = 13680, height_twips = 7920, align = \"right\", ",
    "render_dpi = 200, png_device = \"png\")"))
  # nothing said: the figure's own size, as before
  expect_identical(fi_fig_call(tfl_output("F-1", "figure")), "rtf_figures(content)")
})

test_that("the study's figure settings are the defaults; the item's own win", {
  st <- fi_study(list(dpi = 150L, align = "left", width_in = 7, height_in = 4))
  o <- tfl_output("F-1", "figure", extends = "none", content = fi_items())
  r <- tfl_resolve_output(o, st)
  last <- r$content[[length(r$content)]]
  expect_identical(last$fn, "tfl_image")
  expect_identical(last$args[c("dpi", "align")], list(dpi = 150L, align = "left"))
  expect_identical(fi_fig_call(r, st), paste0(
    "rtf_figures(rtfplot(content, render_width = 7, render_height = 4, render_dpi = 150), ",
    "align = \"left\")"))
  own <- tfl_output("F-1", "figure", extends = "none", content = fi_items(list(dpi = 300L)))
  expect_match(fi_fig_call(tfl_resolve_output(own, st), st), "render_dpi = 300", fixed = TRUE)
  # a figure template's call (no items): the study's settings place it too
  tp <- tfl_output("F-1", "figure")
  expect_identical(fi_fig_call(tp, st), paste0(
    "rtf_figures(content, width_twips = 10080, height_twips = 5760, align = \"left\", ",
    "render_dpi = 150)"))
  # no study settings: as before
  expect_identical(tflspec:::.fig_image_fill(fi_items(), fi_study()), fi_items())
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
