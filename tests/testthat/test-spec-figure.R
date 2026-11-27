# A figure's content: its layers have schema entries from the figure
# designer's catalog, and its code is the designer's.

test_that("the figure designer's layers are in the schema", {
  ent <- tfl_schema("km_curve")
  expect_identical(ent$figure, "layer")
  expect_identical(ent$args$linewidth$type, "number")
  expect_identical(nrow(tfl_schema_check(tfl_item("hline", list(yintercept = 0.5, linetype = "dashed")))), 0L)
  p <- tfl_schema_check(tfl_item("point", list(na.rm = "yes")))
  expect_identical(p$arg, "na.rm")
  parts <- tfl_fig_parts()
  layers <- unique(parts$piece[parts$section == "layers" & !is.na(parts$field)])
  expect_true(all(layers %in% names(tfl_schema())))
})

test_that("a figure report's code is its design's", {
  design <- list(
    data = list(list(step = "read", dataset = "ADSL")),
    layers = list(list(layer = "point", x = "AGE", y = "WEIGHTBL")))
  out <- tfl_output("F-1", "figure",
                    data = list(source = "code", steps = design$data),
                    content = list(tfl_item("point", list(x = "AGE", y = "WEIGHTBL"))))
  expect_identical(tfl_content_code(out),
                   as.character(tfl_fig_design_code(design, plot_id = "F-1",
                                                    save = FALSE, name = "content")))
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("cards")
  p <- tfl_content_run(out, list(adsl = cards::ADSL))
  expect_s3_class(p, "ggplot")
})

test_that("a ggplot2 layer the figure names itself checks against ggplot2", {
  skip_if_not_installed("ggplot2")
  ent <- tfl_schema("geom_point")
  expect_identical(ent$package, "ggplot2")
  expect_true(all(c("x", "y", "colour", "size") %in% ent$aes))
  expect_true(all(c("na.rm", "size", "position") %in% names(ent$args)))
  ok <- tfl_item("geom", list(geom = "geom_point", aes = list(x = "AGE", y = "WEIGHTBL"),
                              params = list(size = 2, alpha = 0.5)))
  expect_identical(nrow(tfl_schema_check(ok)), 0L)
  typo <- tfl_schema_check(tfl_item("geom", list(geom = "geom_point", aes = list(colr = "ARM"),
                                                 params = list(sise = 2))))
  expect_identical(sort(typo$arg), c("aes", "params"))
  expect_true(all(typo$level == "warning"))
  none <- tfl_schema_check(tfl_item("geom", list(geom = "geom_nothing")))
  expect_identical(none$level, "error")
  expect_null(tfl_schema("geom_nothing"))
  skip_if_not_installed("ggrepel")
  expect_identical(tfl_schema("ggrepel::geom_text_repel")$package, "ggrepel")
})
