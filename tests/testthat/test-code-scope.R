# SPEC code in a scope of its own (A1, the user 2026-11-24: SPEC code runs
# only in its own step): one expression that assigns no name but the
# one it makes is written as it is; any other piece in local({ }), so what
# it assigns does not change the program's objects.

test_that("what code assigns: the object of a replacement, not a function's own", {
  ca <- tflspec:::.code_assigns
  expect_identical(ca("data$TRT01A <- stats::relevel(factor(data$TRT01A), ref = \"B\")"), "data")
  expect_identical(ca("x[1] <- 2\nnames(y) <- \"a\"\nlevels(z$A) <- c(\"1\")"), c("x", "y", "z"))
  expect_identical(ca("for (i in 1:2) s <- i"), c("i", "s"))
  expect_identical(ca("lapply(1:2, function(n) m <- n)"), character())
  expect_identical(ca("dplyr::filter(adae, AESER == \"Y\")"), character())
  expect_true(is.na(ca("x <<- 1")))
  expect_true(is.na(ca("f(")))
})

test_that("one expression as it is; several in local(), the name it makes their result", {
  cp <- tflspec:::.code_piece
  b <- c(data = "adtte_saf", population = "pop_saf")
  # one expression that assigns nothing: renamed, the program's own line
  expect_identical(cp("cardx::ard_regression(data)", ".ard_hr", "last", binds = b),
                   ".ard_hr <- cardx::ard_regression(adtte_saf)")
  # several: as written, `data` bound (population is not read)
  expect_identical(
    cp("data$A <- 1\ncox <- f(data)\ng(cox)", ".ard_hr", "last", binds = b),
    ".ard_hr <- local({\n  data <- adtte_saf\n  data$A <- 1\n  cox <- f(data)\n  g(cox)\n})")
  # one expression that assigns another name: in local() too
  expect_identical(cp("x <- f(data)", ".ard_x", "last", binds = c(data = "data")),
                   ".ard_x <- local({\n  x <- f(data)\n})")
  # a piece that leaves `data`: one assignment of it as it is
  expect_identical(cp("data$x <- 1", "data", "name"), "data$x <- 1")
  expect_identical(cp("data <- data[data$A > 1, ]", "data", "name"), "data <- data[data$A > 1, ]")
  expect_identical(cp("n <- 2\ndata <- head(data, n)", "data", "name"),
                   "data <- local({\n  n <- 2\n  data <- head(data, n)\n  data\n})")
  # (it ends in `data`: not said twice; a blank line stays blank)
  expect_identical(cp("n <- 2\n\ndata", "data", "name"), "data <- local({\n  n <- 2\n\n  data\n})")
  # a string over lines keeps its text
  expect_identical(cp("s <- \"a\nb\"\ndata$s <- s", "data", "name"),
                   "data <- local({\n  s <- \"a\nb\"\n  data$s <- s\n  data\n})")
  # nothing kept
  expect_identical(cp("n <- 1\nif (n > 0) warning(\"w\")", NULL, "name"),
                   "local({\n  n <- 1\n  if (n > 0) warning(\"w\")\n})")
  # the steps after it
  expect_identical(cp("x <- 1\ndplyr::tibble(a = x)", "d1", "last", steps = list("f()")),
                   "d1 <- local({\n  x <- 1\n  dplyr::tibble(a = x)\n}) |>\n  f()")
})

test_that("an analysis's code of several statements changes no data of the program's", {
  cp <- tflspec:::.code_piece
  env <- new.env()
  env$adtte_saf <- data.frame(TRT01A = c("B", "A", "B"), AVAL = c(1, 2, 3), stringsAsFactors = FALSE)
  before <- env$adtte_saf
  code <- cp(paste0("data$TRT01A <- stats::relevel(factor(data$TRT01A), ref = \"B\")\n",
                    "fit <- table(data$TRT01A)\n",
                    "names(fit)"), ".ard_x", "last", binds = c(data = "adtte_saf"))
  eval(parse(text = code), env)
  expect_identical(env$.ard_x, c("B", "A"))
  expect_identical(env$adtte_saf, before)
  expect_false(exists("fit", envir = env, inherits = FALSE))
  expect_false(exists("data", envir = env, inherits = FALSE))
})

test_that("a figure's code step keeps its out; a later line reading what it does not keep stops", {
  steps <- list(list(step = "read", dataset = "ADTTE"),
                list(step = "code", code = "n <- nrow(df)\nlab <- paste(\"N =\", n)", out = "lab"))
  d <- tfl_fig_design(data = steps,
                      layers = list(list(layer = "point", x = "AVAL", y = "CNSR"),
                                    list(layer = "layer_code", code = "p <- p + ggplot2::labs(caption = lab)")))
  txt <- paste(tfl_fig_design_code(d), collapse = "\n")
  expect_match(txt, "lab <- local({\n  n <- nrow(adtte_sub)\n  lab <- paste(\"N =\", n)\n  lab\n})",
               fixed = TRUE)
  # (the layer's one expression on p: as it is)
  expect_match(txt, "p <- p + ggplot2::labs(caption = lab)", fixed = TRUE)
  # n is the step's own: a layer reading it stops, saying out:
  d$layers[[2L]]$code <- "p <- p + ggplot2::labs(caption = n)"
  expect_error(tfl_fig_design_code(d), "data[2] code: its code makes `n`", fixed = TRUE)
  expect_error(tfl_fig_design_code(d), "say `out: n`", fixed = TRUE)
  # one assignment and no out: as it is, its name kept
  d$data[[2L]] <- list(step = "code", code = "n <- nrow(df)")
  expect_match(paste(tfl_fig_design_code(d), collapse = "\n"), "\nn <- nrow(adtte_sub)\n", fixed = TRUE)
  # out is read back and written back
  f <- withr_tempdir()
  d2 <- tfl_fig_design(data = steps, layers = list(list(layer = "point", x = "AVAL", y = "CNSR")))
  tfl_write_fig_design(d2, file.path(f, "d.yml"))
  expect_identical(tfl_read_fig_design(file.path(f, "d.yml"))$data[[2L]]$out, "lab")
  # a layer's code of several statements: in local(), `p` its result
  d2$layers <- c(d2$layers, list(list(layer = "layer_code", code = "cap <- \"x\"\np <- p + ggplot2::labs(caption = cap)")))
  expect_match(paste(tfl_fig_design_code(d2), collapse = "\n"),
               "p <- local({\n  cap <- \"x\"\n  p <- p + ggplot2::labs(caption = cap)\n  p\n})", fixed = TRUE)
})

test_that("a listing's rework of several statements runs in a scope that leaves data", {
  sp <- tfl_listing_spec(list(
    listings = data.frame(output_id = "L-1", dataset = "ADAE", stringsAsFactors = FALSE),
    listing_cols = data.frame(output_id = "L-1", vars = "USUBJID", stringsAsFactors = FALSE)))
  cat <- data.frame(dataset = "ADAE", path = "data/adae.rds", derive = NA)
  code <- tfl_listing_code(sp, "L-1", cat, rework = c("k <- 2", "data <- head(data, k)"))
  expect_true(all(c("data <- local({", "  k <- 2", "  data <- head(data, k)", "  data", "})") %in%
                    unlist(strsplit(code, "\n", fixed = TRUE))))
  # one expression: as it is
  code <- tfl_listing_code(sp, "L-1", cat, rework = "data$X <- 1")
  expect_true("data$X <- 1" %in% code)
})
