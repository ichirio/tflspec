# ggplot2 3.5 / 4.0 (issue #37): the compat table, the target version, the
# code written for it, the check's errors and the advice's fixes.

compat_design <- function(add = list(), layers = list(), version = NULL) {
  d <- tfl_fig_template("km_risk_table", param = "OS", group = "TRT01P")
  d$plot$add <- add
  d$layers <- c(d$layers, layers)
  d$ggplot2_version <- version
  d
}

# ---- the table checks itself against the installed ggplot2 ------------------

# the names a ggplot2 function takes: its formals, theme()'s elements, a
# layer's aesthetics and parameters
gg_accepts <- function(fn) {
  f <- getExportedValue("ggplot2", fn)
  out <- names(formals(f))
  if (fn == "theme") out <- c(out, names(ggplot2::get_element_tree()))
  if (grepl("^(geom|stat)_", fn)) {
    l <- tryCatch(f(), error = function(e) NULL)
    if (inherits(l, "Layer")) {
      out <- c(out, l$geom$aesthetics(), l$geom$parameters(TRUE), l$stat$parameters(TRUE))
    }
  }
  unique(out)
}

test_that("the compat table's rows hold for the installed ggplot2", {
  want <- Sys.getenv("TFLSPEC_GGPLOT2_UNDER_TEST")
  inst <- utils::packageVersion("ggplot2")
  if (nzchar(want)) expect_true(startsWith(as.character(inst), want))
  exported <- getNamespaceExports("ggplot2")
  cp <- tfl_fig_compat()
  expect_named(cp, c("package", "fn", "arg", "change", "from", "replacement", "level", "note"))
  expect_true(all(cp$change %in% c("fn_added", "fn_renamed", "fn_deprecated", "arg_added",
                                   "arg_renamed", "arg_removed", "arg_deprecated",
                                   "value_deprecated", "behaviour")))
  expect_true(all(cp$level %in% c("error", "warning", "info")))
  fns <- function(pat) {
    if (grepl("*", pat, fixed = TRUE)) {
      # a glob: a few of the functions it stands for
      list(theme_ = c("theme_grey", "theme_bw", "theme_minimal", "theme_void"),
        scale_ = c("scale_x_continuous", "scale_y_continuous", "scale_x_binned"))[[sub("\\*$", "", pat)]]
    } else pat
  }
  has <- function(fn, arg) {
    acc <- gg_accepts(fn)
    if (grepl("*", arg, fixed = TRUE)) any(grepl(utils::glob2rx(arg), acc)) else arg %in% acc
  }
  for (r in seq_len(nrow(cp))) {
    x <- cp[r, ]
    if (x$change == "behaviour") next
    ge <- inst >= x$from
    for (fn in fns(x$fn)) {
      what <- sprintf("row %d: %s %s(%s) from %s", r, x$change, fn, if (is.na(x$arg)) "" else x$arg, x$from)
      switch(x$change,
        fn_added = expect_identical(fn %in% exported, ge, label = what),
        fn_renamed = {
          expect_true(fn %in% exported, label = what)
          expect_identical(x$replacement %in% exported, ge, label = what)
        },
        fn_deprecated = expect_true(fn %in% exported, label = what),
        arg_added = {
          if (fn %in% exported) expect_identical(has(fn, x$arg), ge, label = what)
          else expect_false(ge, label = what)
        },
        arg_renamed = {
          expect_true(has(fn, x$arg), label = what)
          expect_identical(has(fn, x$replacement), ge, label = what)
        },
        arg_removed = if (ge) expect_false(has(fn, x$arg), label = what),
        expect_true(fn %in% exported, label = what))
    }
  }
})

# ---- the target -------------------------------------------------------------

test_that("the target: argument > design > option > installed", {
  inst <- tflspec:::.fig_installed_gg()
  other <- setdiff(c("3.5", "4.0"), inst)
  d <- compat_design()
  withr::local_options(tflspec.ggplot2_version = NULL)
  t0 <- tflspec:::.fig_target_version(NULL, d)
  expect_equal(t0[c("version", "explicit", "source")], list(version = inst, explicit = FALSE, source = "installed"))
  withr::local_options(tflspec.ggplot2_version = other)
  expect_equal(tflspec:::.fig_target_version(NULL, d)$source, "option")
  d$ggplot2_version <- "3.5"
  expect_equal(tflspec:::.fig_target_version(NULL, d)[c("version", "source")], list(version = "3.5", source = "design"))
  expect_equal(tflspec:::.fig_target_version("4.0", d)[c("version", "source")], list(version = "4.0", source = "argument"))
})

test_that("versions are read as major.minor; only 3.5 and 4.0", {
  expect_equal(tflspec:::.fig_norm_version(4), "4.0")
  expect_equal(tflspec:::.fig_norm_version("4"), "4.0")
  expect_equal(tflspec:::.fig_norm_version(3.5), "3.5")
  expect_equal(tflspec:::.fig_norm_version("4.0.3"), "4.0")
  expect_error(tflspec:::.fig_norm_version("3.4"), "one of")
  expect_error(tfl_fig_design(ggplot2_version = "5.0"), "one of")
  expect_error(tfl_fig_design_code(compat_design(), ggplot2_version = "3.4"), "one of")
})

test_that("tfl_fig_compat() says what each row means for a version", {
  s35 <- tfl_fig_compat("3.5")
  s40 <- tfl_fig_compat("4.0")
  row <- function(x, fn, arg = NA) x[x$fn == fn & (is.na(arg) & is.na(x$arg) | x$arg %in% arg), "status"][1]
  expect_equal(row(s35, "element_geom"), "absent")
  expect_equal(row(s40, "element_geom"), "ok")
  expect_equal(row(s35, "geom_label", "label.size"), "ok")
  expect_equal(row(s40, "geom_label", "label.size"), "renamed")
  expect_equal(row(s40, "coord_trans"), "renamed")
  expect_equal(row(s35, "geom_boxplot", "outliers"), "ok")
})

test_that("the design's ggplot2_version goes through YAML", {
  d <- compat_design(version = "4.0")
  f <- tempfile(fileext = ".yml")
  tfl_write_fig_design(d, f)
  expect_true(any(grepl("^ggplot2_version: '4.0'", readLines(f))))
  back <- tfl_read_fig_design(f)
  expect_equal(back$ggplot2_version, "4.0")
  # a bare 4.0 in YAML is a number
  writeLines(c("ggplot2_version: 4.0", "layers: []"), f)
  expect_equal(tfl_read_fig_design(f)$ggplot2_version, "4.0")
})

# ---- the code ---------------------------------------------------------------

label_layer <- list(layer = "call", fn = "geom_label", data = "df",
                    aes = list(x = "AVAL", y = 0.5, label = "USUBJID"),
                    args = list(label.size = 0.3))

test_that("renamed arguments are written for the target, with a comment", {
  code40 <- tfl_fig_design_code(compat_design(layers = list(label_layer)), ggplot2_version = "4.0")
  line <- grep("<- p + geom_label", code40, value = TRUE, fixed = TRUE)
  expect_match(line, "linewidth = 0.3", fixed = TRUE)
  expect_match(line, "# ggplot2 4.0: label.size -> linewidth", fixed = TRUE)
  # 4.0's name for 3.5
  l35 <- label_layer
  l35$args <- list(linewidth = 0.3)
  code35 <- tfl_fig_design_code(compat_design(layers = list(l35)), ggplot2_version = "3.5")
  line <- grep("<- p + geom_label", code35, value = TRUE, fixed = TRUE)
  expect_match(line, "label.size = 0.3", fixed = TRUE)
  expect_match(line, "# ggplot2 3.5: linewidth -> label.size", fixed = TRUE)
})

test_that("renamed functions, nested calls and irregular names use the table", {
  add <- list(list(fn = "coord_trans", args = list(y = "log10")),
              list(fn = "theme", args = list(legend.title = list(fn = "element_blank"))))
  code <- tfl_fig_design_code(compat_design(add = add), ggplot2_version = "4.0")
  expect_true(any(grepl("p <- p + coord_transform(y = \"log10\")   # ggplot2 4.0: coord_trans -> coord_transform",
                        code, fixed = TRUE)))
  w <- tflspec:::.fig_compat_walk(list(fn = "layer_scales"), "4.0", "x")
  expect_equal(w$spec$fn, "get_panel_scales")
  w <- tflspec:::.fig_compat_walk(list(fn = "ggplot2::coord_transform"), "3.5", "x")
  expect_equal(w$spec$fn, "ggplot2::coord_trans")
  # nested: an element inside theme()
  w <- tflspec:::.fig_compat_walk(
    list(fn = "theme", args = list(geom = list(fn = "element_geom", args = list(ink = "red")))), "3.5", "p")
  expect_true(all(c("args$geom", "fn") %in% w$errors$field))
  expect_true(any(w$errors$part == "p > element_geom"))
})

test_that("a version guard only when a 4.0-only feature is written", {
  d <- compat_design(add = list(list(fn = "labs", args = list(dictionary = list(AVAL = "Value")))))
  code <- tfl_fig_design_code(d, ggplot2_version = "4.0")
  expect_true(any(code == 'stopifnot(utils::packageVersion("ggplot2") >= "4.0.0")'))
  expect_true(any(grepl("# needs ggplot2 >= 4.0.0: labs(dictionary =)", code, fixed = TRUE)))
  expect_lt(grep("stopifnot", code), grep("^# ---- data", code))
  # 3.5.0 features need none (the package needs ggplot2 >= 3.5.0)
  d2 <- compat_design(add = list(list(fn = "coord_radial")))
  expect_false(any(grepl("stopifnot", tfl_fig_design_code(d2, ggplot2_version = "4.0"))))
  expect_false(any(grepl("stopifnot", tfl_fig_design_code(compat_design(), ggplot2_version = "4.0"))))
  # linewidth for geom_label is 4.0's
  code <- tfl_fig_design_code(compat_design(layers = list(label_layer)), ggplot2_version = "4.0")
  expect_true(any(grepl("stopifnot", code)))
})

test_that("the header names the version only when it was set", {
  withr::local_options(tflspec.ggplot2_version = NULL)
  d <- compat_design()
  expect_false(any(grepl("Written for ggplot2", tfl_fig_design_code(d))))
  expect_equal(grep("Written for ggplot2", tfl_fig_design_code(d, ggplot2_version = "3.5"), value = TRUE),
               "# Written for ggplot2 3.5")
  d$ggplot2_version <- "4.0"
  expect_equal(grep("Written for ggplot2", tfl_fig_design_code(d), value = TRUE), "# Written for ggplot2 4.0")
  withr::local_options(tflspec.ggplot2_version = "3.5")
  expect_equal(grep("Written for ggplot2", tfl_fig_design_code(compat_design()), value = TRUE),
               "# Written for ggplot2 3.5")
})

# ---- the check: errors only -------------------------------------------------

test_that("the check: what the target does not have, or drops", {
  d <- compat_design(add = list(list(fn = "coord_cartesian", args = list(reverse = "y"))))
  p35 <- tfl_check_fig_design(d, ggplot2_version = "3.5")
  expect_true(any(p35$field == "args$reverse" & grepl("ggplot2 3.5: coord_cartesian\\(reverse =\\) is not available", p35$problem)))
  expect_false(any(grepl("ggplot2", tfl_check_fig_design(d, ggplot2_version = "4.0")$problem)))
  bar <- list(layer = "call", fn = "geom_col", data = "df", aes = list(x = "USUBJID", y = "AVAL"),
              args = list(size = 0.5))
  p40 <- tfl_check_fig_design(compat_design(layers = list(bar)), ggplot2_version = "4.0")
  expect_true(any(grepl("drops size \\(not translated\\); use linewidth", p40$problem)))
  # deprecated is not an error
  line <- list(layer = "call", fn = "geom_hline", args = list(yintercept = 0.5, size = 1))
  expect_equal(nrow(tfl_check_fig_design(compat_design(layers = list(line)), ggplot2_version = "4.0")), 0L)
  # the other version's name is not reported as unknown by the installed check
  l35 <- label_layer
  l35$args <- list(linewidth = 0.3)
  expect_equal(nrow(tfl_check_fig_design(compat_design(layers = list(l35)), ggplot2_version = "3.5")), 0L)
  expect_equal(nrow(tfl_check_fig_design(compat_design(layers = list(label_layer)), ggplot2_version = "4.0")), 0L)
  # a function of either version only
  for (fn in c("coord_trans", "coord_transform")) {
    p <- tflspec:::.fig_check_call(list(fn = fn, args = list(y = "log10")), "x")
    expect_equal(nrow(p), 0L)
  }
})

# ---- the advice: deprecations, with a fix -----------------------------------

test_that("deprecations are advice with a one-click fix", {
  line <- list(layer = "call", fn = "geom_hline", args = list(yintercept = 0.5, size = 1))
  add <- list(list(fn = "theme", args = list(legend.position = list(0.8, 0.2))),
              list(fn = "geom_errorbarh"))
  d <- compat_design(layers = list(line), add = add)
  a <- tfl_fig_advice(d, ggplot2_version = "4.0")
  a <- a[a$rule == "gg_compat", ]
  expect_equal(nrow(a), 3L)
  expect_true(all(a$level == "warning"))
  for (f in a$fix) d <- tfl_fig_apply_fix(d, f)
  n <- length(d$layers)
  expect_equal(d$layers[[n]]$args, list(yintercept = 0.5, linewidth = 1))
  expect_equal(d$plot$add[[1]]$args, list(legend.position = "inside", legend.position.inside = c(0.8, 0.2)))
  expect_equal(d$plot$add[[2]]$fn, "geom_errorbar")
  expect_equal(d$plot$add[[2]]$args, list(orientation = "y"))
  expect_equal(sum(tfl_fig_advice(d, ggplot2_version = "4.0")$rule == "gg_compat"), 0L)
  # the removed geom_col(size) is fixed to linewidth too
  bar <- list(layer = "call", fn = "geom_col", data = "df", aes = list(x = "USUBJID", y = "AVAL"),
              args = list(size = 0.5))
  d <- compat_design(layers = list(bar))
  a <- tfl_fig_advice(d, ggplot2_version = "4.0")
  f <- a$fix[a$rule == "gg_compat"][[1]]
  d <- tfl_fig_apply_fix(d, f)
  expect_equal(names(d$layers[[length(d$layers)]]$args), "linewidth")
  expect_equal(nrow(tfl_check_fig_design(d, ggplot2_version = "4.0")), 0L)
})

test_that("ggplot2 4.0's label attribute: advice when an axis has no title", {
  adam <- tfl_example_adam()
  attr(adam$ADSL$AGE, "label") <- "Age (years)"
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADSL")),
    layers = list(list(layer = "call", fn = "geom_histogram", data = "df", aes = list(x = "AGE"))))
  a <- tfl_fig_advice(d, adam, ggplot2_version = "4.0")
  expect_true(any(a$rule == "gg_label_attr" & grepl("AGE would be titled 'Age (years)'", a$message, fixed = TRUE)))
  expect_false(any(tfl_fig_advice(d, adam, ggplot2_version = "3.5")$rule == "gg_label_attr"))
  d$plot$x_label <- "Age"
  expect_false(any(tfl_fig_advice(d, adam, ggplot2_version = "4.0")$rule == "gg_label_attr"))
})

test_that("templates' output does not depend on the target", {
  withr::local_options(tflspec.ggplot2_version = NULL)
  for (t in c("km_risk_table", "waterfall_response")) {
    d <- tfl_fig_template(t)
    a <- tfl_fig_design_code(d)
    b <- tfl_fig_design_code(d, ggplot2_version = "3.5")
    expect_identical(unclass(a), unclass(b)[!grepl("^# Written for ggplot2", b)])
  }
})

test_that("the script written for the installed ggplot2 runs without dropped arguments", {
  adam <- tfl_example_adam()
  d <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADSL")),
    layers = list(
      list(layer = "call", fn = "ggplot", base = TRUE, data = "df",
           aes = list(x = "TRTDURD", y = "TRTDURD", label = "SEX")),
      # 4.0's name: written as label.size for 3.5
      list(layer = "call", fn = "geom_label", args = list(linewidth = 0.2))))
  code <- tfl_fig_design_code(d, "t")
  e <- new.env()
  for (n in names(adam)) assign(tolower(n), adam[[n]], envir = e)
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  warns <- character()
  withCallingHandlers(
    suppressMessages(eval(parse(text = code), envir = e)),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_false(any(grepl("Ignoring unknown parameters", warns)))
  expect_true(file.exists(e$fig_path))
})
