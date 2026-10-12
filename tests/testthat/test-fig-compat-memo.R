# The compat walk's versions and patterns worked out once (0.0.24.9092):
# the same answers as worked out each time, for every target, and after
# the compat table changes

test_that("the walk's answers do not depend on what was worked out before", {
  d <- tfl_fig_template("km_risk_table")
  fresh <- function(target) {
    rm(list = grep("^ge:", ls(.gg_memo), value = TRUE), envir = .gg_memo)
    .gg_memo$glob <- list()
    tfl_check_fig_design(d, ggplot2_version = target)
  }
  for (target in c("3.5", "4.0")) {
    a <- fresh(target)
    b <- tfl_check_fig_design(d, ggplot2_version = target)   # from the memo
    expect_identical(a, b)
  }
  # a figure using a function added in a later ggplot2: an error on the
  # older target, none on the newer, whatever the order
  cp <- .fig_compat()
  k <- which(cp$change == "fn_added" & cp$package == "ggplot2")[1L]
  skip_if(is.na(k))
  fn <- gsub("[*?]", "", cp$fn[k])
  w4 <- .fig_compat_walk(list(fn = fn), "4.0", "x")$errors
  w3 <- .fig_compat_walk(list(fn = fn), "3.5", "x")$errors
  w4b <- .fig_compat_walk(list(fn = fn), "4.0", "x")$errors
  expect_identical(w4, w4b)
  if (!.fig_ver_ge("3.5", cp$from[k])) expect_gt(nrow(w3), 0L)
})

test_that("a changed compat table is not answered from the memo", {
  old <- .fig_registry$compat
  on.exit(.fig_registry$compat <- old, add = TRUE)
  cp <- .fig_compat()
  w <- .fig_compat_walk(list(fn = "geom_point"), "3.5", "x")
  # every row said to come from a version beyond any target
  .fig_registry$compat$from <- rep("9.9.0", nrow(cp))
  w2 <- .fig_compat_walk(list(fn = "geom_point"), "3.5", "x")
  expect_true(is.list(w2))
  key_old <- paste0("ge:3.5:", paste(cp$from, collapse = "|"))
  key_new <- paste0("ge:3.5:", paste(.fig_registry$compat$from, collapse = "|"))
  expect_false(identical(.gg_memo[[key_old]], .gg_memo[[key_new]]))
  expect_false(any(.gg_memo[[key_new]]))
})
