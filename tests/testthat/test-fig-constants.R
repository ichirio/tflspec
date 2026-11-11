# Round 14 of the figure SPEC coverage: what programmers build of
# constants in a figure's code -- a scale's labels by paste0() / sprintf(),
# xlim(c(0, 52)), a layer's table of constants (data.frame()), a glm's
# family, a palette of RColorBrewer or viridisLite, setNames() colours --
# and the mapping functions of a CI band (pmax(), qt()).

co_item <- function(txt) .fig_lang_item(str2lang(txt))
co_code <- function(it) .fig_deparse(.fig_item_lang(it))
co_problems <- function(fn, args) tfl_schema_check(tfl_item(fn, args))

test_that("texts built of constants are typed where a list of texts goes", {
  expect_identical(.schema_r_texts(tfl_r('paste0("W", seq(0, 24, by = 4))')),
                   c("W0", "W4", "W8", "W12", "W16", "W20", "W24"))
  expect_identical(.schema_r_texts(tfl_r('sprintf("Day %d", 1:3)')), c("Day 1", "Day 2", "Day 3"))
  expect_identical(.schema_r_texts(tfl_r('c("BL", paste("Week", c(4, 8)))')), c("BL", "Week 4", "Week 8"))
  expect_identical(.schema_r_texts(tfl_r("month.abb[1]")), NULL)
  expect_identical(.schema_r_texts(tfl_r("toupper(letters[1:3])")), NULL)
  expect_identical(.schema_r_texts(tfl_r("toupper(month.abb)"))[1:2], c("JAN", "FEB"))
  # numbers alone, a column or an object, another function: not texts
  expect_null(.schema_r_texts(tfl_r("seq(0, 24, by = 4)")))
  expect_null(.schema_r_texts(tfl_r('paste0("W", visits)')))
  expect_null(.schema_r_texts(tfl_r('paste0("W", Sys.time())')))
  expect_null(.schema_r_texts(tfl_r('rep("a", 1e9)')))
  # read as R (kept as written), judged by the check, written back the same
  it <- co_item('scale_x_continuous(breaks = seq(0, 24, by = 4), labels = paste0("W", seq(0, 24, by = 4)))')
  expect_s3_class(it$args$labels, "tfl_r")
  expect_identical(nrow(tfl_schema_check(it)), 0L)
  expect_identical(co_code(it),
                   'scale_x_continuous(breaks = seq(0, 24, by = 4), labels = paste0("W", seq(0, 24, by = 4)))')
  # one of that form that does not evaluate: an error, as the code would stop
  p <- co_problems("scale_x_continuous", list(labels = tfl_r('sprintf("Day %d", 1.5)')))
  expect_identical(p$level, "error")
  expect_match(p$message, "texts built by R that do not evaluate", fixed = TRUE)
  # any other R as before: no word of it
  expect_identical(nrow(co_problems("scale_x_continuous", list(labels = tfl_r("my_labels")))), 0L)
})

test_that("xlim(c(0, 52)) reads as its limits, an open end as NA", {
  it <- co_item("xlim(c(0, 52))")
  expect_identical(it$args$limits, c(0, 52))
  expect_identical(co_code(it), "xlim(0, 52)")
  it <- co_item("ylim(c(-5, NA))")
  expect_identical(it$args$limits, c(-5, NA))
  expect_identical(co_code(it), "ylim(-5, NA)")
  expect_identical(co_code(co_item("xlim(0, NA)")), "xlim(0, NA)")
  expect_identical(co_code(co_item('xlim(c("Placebo", "Drug"))')), 'xlim("Placebo", "Drug")')
  # R in it: the whole call as R, as before
  expect_identical(co_item("xlim(range(x))")$fn, ".r")
})

test_that("a layer's data can be a table of constants, data.frame(), checked by its columns", {
  it <- co_item('geom_hline(aes(yintercept = y, linetype = what), data = data.frame(y = c(20, -30), what = c("PD", "PR")))')
  expect_s3_class(it$args$data, "tfl_call")
  expect_identical(it$args$data$fn, "data.frame")
  expect_identical(it$args$data$args$y, c(20, -30))
  expect_identical(nrow(tfl_schema_check(it)), 0L)
  expect_identical(co_code(it), paste0('geom_hline(mapping = aes(yintercept = y, linetype = what), ',
                                       'data = data.frame(y = c(20, -30), what = c("PD", "PR")))'))
  # from YAML
  y <- .calls_in(yaml::yaml.load("{data: {fn: data.frame, args: {y: [20, -30], what: [PD, PR]}}}",
                                 handlers = .yaml_handlers()))
  g <- tfl_item("geom_hline", c(list(mapping = list(yintercept = "y", linetype = "what")), y))
  expect_identical(nrow(tfl_schema_check(g)), 0L)
  expect_identical(co_code(g), co_code(it))
  # columns of lengths that do not fit; no column
  p <- co_problems("geom_hline", list(data = tfl_call("data.frame", list(y = c(1, 2, 3), g = c("a", "b")))))
  expect_identical(p$level, "error")
  expect_match(p$message, "y 3, g 2", fixed = TRUE)
  expect_match(co_problems("geom_hline", list(data = tfl_call("data.frame", list())))$message,
               "takes its columns by name", fixed = TRUE)
  # a layer's data by a dataset's name: as before
  expect_identical(co_item("geom_point(data = adsl)")$args$data, "adsl")
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    eval(.fig_item_lang(g), asNamespace("ggplot2"))
  expect_identical(sort(ggplot2::layer_data(p, 1L)$yintercept), c(-30, 20))
})

test_that("a glm's family in method.args is its function, written bare", {
  it <- co_item('geom_smooth(method = "glm", method.args = list(family = binomial), formula = y ~ x)')
  expect_identical(it$args$method.args, list(family = "binomial"))
  expect_identical(co_code(it), 'geom_smooth(method = "glm", method.args = list(family = binomial), formula = y ~ x)')
  # from a SPEC's {family: binomial}: bare too (glm() takes both)
  expect_identical(co_code(tfl_item("geom_smooth", list(method = "glm", method.args = list(family = "poisson")))),
                   'geom_smooth(method = "glm", method.args = list(family = poisson))')
  # a link: the family's call
  it <- co_item('geom_smooth(method = "glm", method.args = list(family = binomial(link = "probit")))')
  expect_s3_class(it$args$method.args$family, "tfl_call")
  expect_identical(it$args$method.args$family$args$link, "probit")
  expect_identical(nrow(tfl_schema_check(it)), 0L)
  expect_identical(co_code(it), 'geom_smooth(method = "glm", method.args = list(family = binomial(link = "probit")))')
  # another value of method.args: as before
  expect_identical(co_code(co_item("geom_smooth(method = \"loess\", method.args = list(degree = 1))")),
                   'geom_smooth(method = "loess", method.args = list(degree = 1))')
  expect_identical(co_code(tfl_item("geom_smooth", list(method.args = list(family = "symmetric")))),
                   'geom_smooth(method.args = list(family = "symmetric"))')
})

test_that("colours by RColorBrewer, viridisLite and setNames() are typed", {
  it <- co_item('scale_colour_manual(values = RColorBrewer::brewer.pal(3, "Dark2"))')
  expect_identical(it$args$values$fn, "RColorBrewer::brewer.pal")
  expect_identical(it$args$values$args, list(n = 3, name = "Dark2"))
  expect_identical(nrow(tfl_schema_check(it)), 0L)
  expect_identical(co_code(it), 'scale_colour_manual(values = RColorBrewer::brewer.pal(3, "Dark2"))')
  it <- co_item("scale_fill_gradientn(colours = viridisLite::magma(5))")
  expect_identical(it$args$colours$fn, "viridisLite::magma")
  expect_identical(co_code(it), "scale_fill_gradientn(colours = viridisLite::magma(5))")
  # a function the package does not have: an error
  p <- co_problems("scale_colour_manual", list(values = tfl_call("viridisLite::magenta", list(n = 3))))
  expect_identical(p$level, "error")
  it <- co_item('scale_colour_manual(values = setNames(c("grey40", "red"), c("Placebo", "Drug")))')
  expect_identical(it$args$values, list(Placebo = "grey40", Drug = "red"))
  expect_identical(co_code(it), 'scale_colour_manual(values = c(Placebo = "grey40", Drug = "red"))')
  # names from an object: R, as before
  expect_s3_class(co_item('scale_colour_manual(values = setNames(c("a", "b"), arms))')$args$values, "tfl_r")
})

test_that("a mapping takes pmax(), pmin(), cumsum(), qt() and qnorm() at its top", {
  it <- co_item("geom_ribbon(aes(ymin = pmax(lower, 0), ymax = pmin(upper, 1)))")
  expect_identical(it$args$mapping$ymin, "pmax(lower, 0)")
  expect_identical(it$args$mapping$ymax, "pmin(upper, 1)")
  it <- co_item("geom_errorbar(aes(ymin = mean - qt(0.975, n - 1) * se, ymax = mean + qnorm(0.975) * se))")
  expect_identical(it$args$mapping$ymin, "mean - qt(0.975, n - 1) * se")
  expect_identical(nrow(tfl_schema_check(it)), 0L)
  expect_identical(co_code(it),
                   "geom_errorbar(mapping = aes(ymin = mean - qt(0.975, n - 1) * se, ymax = mean + qnorm(0.975) * se))")
  expect_identical(co_item("geom_step(aes(y = cumsum(n)))")$args$mapping$y, "cumsum(n)")
})

test_that("a text built of constants is a text where one goes", {
  it <- co_item('labs(x = paste("Time", "(weeks)"), caption = sprintf("N = %d", 120L))')
  expect_s3_class(it$args$caption, "tfl_r")
  expect_true(.schema_ok(it$args$caption, .schema_type("string")))
  expect_false(.schema_ok(tfl_r('paste0("W", 1:2)'), .schema_type("string")))
  expect_false(.schema_ok(tfl_r("paste(x)"), .schema_type("string")))
  p <- co_problems("labs", list(caption = tfl_r('sprintf("N = %d", 1.5)')))
  expect_identical(nrow(p), 0L)
  expect_identical(co_code(it), 'labs(x = paste("Time", "(weeks)"), caption = sprintf("N = %d", 120L))')
})
