# One rule for every figure-style table and key: the study's over the
# company's over tflspec's built-in one (canon: the standards' layering)

fsl_get <- function(key, style, type = NA) tflspec:::.fs_get(key, type, style)
fsl_company <- function() {
  co <- tflspec:::.fig_style_builtin()
  co$settings$value[co$settings$key == "axis_title_width" & is.na(co$settings$type)] <- "40"
  co$settings$value[co$settings$key == "base_size" & is.na(co$settings$type)] <- "11"
  co
}

test_that("tfl_fig_style(study): the study's rows over the company's over the built-in", {
  co <- fsl_company()
  old <- options(tflspec.fig_style = co)
  on.exit(options(old), add = TRUE)
  # no study: the company's
  expect_identical(fsl_get("axis_title_width", tfl_fig_style()), "40")
  # a study's copy without the key (made before it): the company's; the key
  # the study gives, the study's
  st <- list(figure_style = list(settings = list(list(key = "base_size", value = "12"))))
  s <- tfl_fig_style(st)
  expect_identical(fsl_get("axis_title_width", s), "40")
  expect_identical(fsl_get("base_size", s), "12")
  # every other row stays (fig_setup.R writes the whole table), the study's once
  key_of <- function(d) paste(d$type, d$key)
  expect_setequal(key_of(s$settings), key_of(co$settings))
  expect_identical(sum(s$settings$key == "base_size" & is.na(s$settings$type)), 1L)
  st2 <- list(figure_style = list(settings = list(list(key = "axis_title_width", value = "30"))))
  expect_identical(fsl_get("axis_title_width", tfl_fig_style(st2)), "30")
  # a type's row is its own item: the study's km row leaves the blank row
  st3 <- list(figure_style = list(settings = list(list(type = "km", key = "base_size", value = "9"))))
  s3 <- tfl_fig_style(st3)
  expect_identical(fsl_get("base_size", s3, "km"), "9")
  expect_identical(fsl_get("base_size", s3), "11")
  # a palette is one item: the study's whole, in its order; the others the company's
  pal <- list(list(palette = "treatment", colour = "#111111"), list(palette = "treatment", colour = "#222222"))
  s4 <- tfl_fig_style(list(figure_style = list(colors = pal)))
  expect_identical(s4$colors$colour[s4$colors$palette == "treatment"], c("#111111", "#222222"))
  others <- setdiff(unique(co$colors$palette), "treatment")
  expect_identical(s4$colors[s4$colors$palette %in% others, "colour"],
                   co$colors[co$colors$palette %in% others, "colour"])
  # the company's from a maker's standards, never the session's option
  expect_identical(fsl_get("axis_title_width", tfl_fig_style(st, standards = list())), "50")
  expect_identical(fsl_get("axis_title_width",
                           tfl_fig_style(st, standards = list(figure_settings = co$settings))), "40")
})

test_that("a template's figure_style reference and the style in force read one rule", {
  old <- options(tflspec.fig_style = fsl_company())
  on.exit(options(old), add = TRUE)
  st <- list(figure_style = list(settings = list(list(key = "base_size", value = "12"))))
  # the study's copy lacks axis_title_width: the company's 40, not the built-in 50
  expect_identical(tflspec:::.tpl_fig_style_read("axis_title_width", st), 40)
  expect_identical(tflspec:::.tpl_fig_style_read("base_size", st), 12)
  # the same in a program's making, its style the study's over the company's
  st2 <- list(figure_style = list(settings = list(list(key = "axis_title_width", value = "30"))))
  w <- tflspec:::.with_code_context(tflspec:::.fs_title_width(),
                                    list(fig_style = tfl_fig_style(st2, list())))
  expect_identical(w, 30)
  expect_identical(tflspec:::.tpl_fig_style_read("axis_title_width", st2), 30)
})

test_that("fig_setup.R draws with the study's figure style over the company's", {
  co <- fsl_company()
  st <- list(figure_style = list(settings = list(list(key = "axis_title_width", value = "30"))))
  code <- paste(tfl_setup_program("fig_setup", study = st,
                                  standards = list(figure_settings = co$settings)),
                collapse = "\n")
  e <- new.env()
  eval(parse(text = sub("(?s)^.*?(tfl_settings <- data.frame\\(.*?stringsAsFactors = FALSE\\)).*$",
                        "\\1", code, perl = TRUE)), e)
  expect_identical(e$tfl_settings$value[e$tfl_settings$key == "axis_title_width"], "30")
  expect_identical(e$tfl_settings$value[e$tfl_settings$key == "base_size" & is.na(e$tfl_settings$type)],
                   "11")
  # no study: the company's, as before
  code0 <- paste(tfl_setup_program("fig_setup", standards = list(figure_settings = co$settings)),
                 collapse = "\n")
  e0 <- new.env()
  eval(parse(text = sub("(?s)^.*?(tfl_settings <- data.frame\\(.*?stringsAsFactors = FALSE\\)).*$",
                        "\\1", code0, perl = TRUE)), e0)
  expect_identical(e0$tfl_settings$value[e0$tfl_settings$key == "axis_title_width"], "40")
})
