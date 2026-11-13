# The arguments of emmeans::emmeans() a regression row writes in
# `emmeans_args = list(...)` (R/ard_emmeans_args.R): tidy_plus_plus() gives
# them to emmeans() for the pairwise contrasts, and emmeans() drops a name
# it does not know without a word.  And mmrm's cov_struct()'s other names
# (R/ard_model_args.R).

reg_row <- function(args, method = "cardx::ard_regression") list(
  method = method, population_id = "SAF",
  args = paste("formula = AGE ~ TRT01A + SEX, method = \"lm\",", args))

test_that("emmeans()'s names are looked for in the installed emmeans", {
  skip_if_not_installed("emmeans")
  has <- .emmeans_arg_names()
  expect_true(all(c("weights", "by", "offset") %in% has$own))
  expect_true(all(c("at", "cov.reduce", "nuisance", "wt.nuis", "regrid", "df") %in%
                    has$ref_grid))
  expect_true(all(c("adjust", "level", "bias.adjust") %in% has$options))
  # a model method's own (an ordinal model's `mode`)
  expect_true("mode" %in% has$methods)
  # update()'s options by their beginning (emmeans' pmatch)
  expect_identical(.emmeans_args_not_matched(
    c("weights", "bias.adj", "mode", "wieghts", "typ", "covreduce"), has),
    c("wieghts", "covreduce"))
  # the catalog's arguments are all emmeans' (tfl_ard_model_args() lists them)
  m <- tfl_ard_model_args("emmeans", "emmeans")
  expect_identical(unique(m$fn), "emmeans::emmeans")
  expect_true(all(c("weights", "type", "at", "regrid") %in% m$arg[m$described]))
  expect_false(any(c("object", "specs", "...") %in% m$arg))
  expect_identical(m$kind[m$arg == "weights"], "choice")
})

test_that("a name emmeans() would drop, or tidy_plus_plus() gives itself, is said", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("emmeans")
  pw <- "add_pairwise_contrasts = TRUE, "
  # a misspelt name: equal weights without a word
  expect_warning(exact_spec(reg_row(paste0(
    pw, "emmeans_args = list(wieghts = \"proportional\")"))),
    paste0("ard_regression\\(emmeans_args = \\) gives `wieghts`, which emmeans .*'s ",
           "emmeans\\(\\) and ref_grid\\(\\) do not take.*drop it without a word"))
  expect_warning(exact_spec(reg_row(paste0(
    pw, "emmeans_args = list(type = \"response\", covreduce = median)"),
    "cardx::ard_regression_basic")),
    "ard_regression_basic\\(emmeans_args = \\) gives `covreduce`")
  # the names tidy_plus_plus() writes itself, or another argument says
  expect_warning(exact_spec(reg_row(paste0(pw, "emmeans_args = list(specs = \"SEX\")"))),
                 "gives `specs`, which tidy_plus_plus\\(\\) gives emmeans\\(\\) itself")
  expect_warning(exact_spec(reg_row(paste0(pw, "emmeans_args = list(adjust = \"none\")"))),
                 "gives `adjust`.*write `contrasts_adjust =` in args instead")
  expect_warning(exact_spec(reg_row(paste0(pw, "emmeans_args = list(level = 0.9)"))),
                 "gives `level`.*write `conf.level =` in args instead")
  # no pairwise contrasts: emmeans() is never called
  expect_warning(exact_spec(reg_row("emmeans_args = list(weights = \"proportional\")")),
                 "not `add_pairwise_contrasts = TRUE`.*it would be ignored")
  # values the catalog describes
  expect_warning(exact_spec(reg_row(paste0(pw, "emmeans_args = list(weights = \"prop\")"))),
                 "emmeans\\(weights = \\) is one of \"equal\", \"proportional\"")
  expect_warning(exact_spec(reg_row(paste0(pw, "emmeans_args = list(type = \"resp\")"))),
                 "emmeans\\(type = \\) is one of \"link\", \"response\"")
  expect_warning(exact_spec(reg_row(paste0(pw, "emmeans_args = list(df = 0)"))),
                 "emmeans\\(df = \\) is a number above 0")
  expect_warning(exact_spec(reg_row(paste0(pw, "emmeans_args = list(at = 50)"))),
                 "emmeans\\(at = \\) is a list")
  # all fine: emmeans' own, ref_grid()'s, update()'s options, a method's,
  # numeric weights, the program's objects
  for (x in c(
    "emmeans_args = list(weights = \"proportional\", type = \"response\")",
    "emmeans_args = list(weights = c(0.5, 0.5), at = list(AGE = 50), df = Inf)",
    "emmeans_args = list(regrid = \"response\", bias.adjust = FALSE, nuisance = \"SEX\")",
    "emmeans_args = list(regrid = TRUE, cov.reduce = median, by = \"SEX\")",
    "emmeans_args = list(weights = my_weights)",
    "emmeans_args = my_emmeans_args")) {
    expect_no_warning(exact_spec(reg_row(paste0(pw, x))), message = x)
  }
})

test_that("emmeans_args reaches emmeans(): proportional weights and differences of proportions", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("emmeans")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  # a logistic model of a response by arm and sex; the arms' differences of
  # the response rates (regrid = "response"), averaged over sex in
  # proportion to the data (SAS's OM): weights change them
  args <- function(ea) paste(
    "formula = RESP ~ TRT01A + SEX, method = \"glm\",",
    "method.args = list(family = binomial), add_pairwise_contrasts = TRUE,",
    "contrasts_adjust = \"none\", emmeans_args =", ea)
  cs <- list(
    row = list(method = "regression", population_id = "SAF",
               args = args("list(regrid = \"response\", weights = \"proportional\")")),
    pop_derive = "RESP = as.integer(AGE >= 75)",
    hand = paste("cardx::ard_regression(data, formula = RESP ~ TRT01A + SEX,",
                 "method = \"glm\", method.args = list(family = binomial),",
                 "add_pairwise_contrasts = TRUE, contrasts_adjust = \"none\",",
                 "emmeans_args = list(regrid = \"response\", weights = \"proportional\"))"))
  r <- exact_run(cs, adam, dir)
  expect_true(isTRUE(r$same), info = r$error)
  # the same differences as emmeans() itself, and not those of equal weights
  # (what the misspelt `wieghts =` gives)
  d <- exact_inputs(cs$row, adam, pop_derive = cs$pop_derive)$data
  fit <- stats::glm(RESP ~ TRT01A + SEX, family = stats::binomial, data = d)
  want <- function(...) {
    e <- emmeans::emmeans(fit, "TRT01A", regrid = "response", ...)
    # (tidy_plus_plus()'s pairwise_reverse = TRUE: the later level first)
    as.data.frame(graphics::pairs(e, adjust = "none", reverse = TRUE))$estimate
  }
  prop <- want(weights = "proportional")
  equal <- suppressMessages(want(wieghts = "proportional"))
  expect_equal(equal, want(), tolerance = 1e-12)
  expect_gt(max(abs(prop - equal)), 1e-6)
  ard <- suppressMessages(suppressWarnings(
    tfl_build_ard(exact_spec(cs$row, pop_derive = cs$pop_derive), dir = dir,
                  save = FALSE)))
  est <- ard[ard$stat_name == "estimate" & ard$variable == "TRT01A", ]
  expect_equal(unname(unlist(est$stat)), prop, tolerance = 1e-8)
})

test_that("mmrm's cov_struct(): group, heterogeneous and a name it does not have", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("mmrm")
  mmrm_row <- function(cs) list(
    method = "lsmeans_diff", dataset = "ADLB", population_id = "SAF",
    args = paste("formula = AVAL ~ TRTA + AVISIT, method = \"mmrm\", package = \"mmrm\",",
                 "method.args = list(covariance = ", cs, ")"))
  expect_no_warning(exact_spec(mmrm_row(paste(
    "cov_struct(\"us\", visits = \"AVISIT\", subject = \"USUBJID\",",
    "group = \"TRTA\", heterogeneous = TRUE)"))))
  expect_warning(exact_spec(mmrm_row(
    "cov_struct(\"us\", visits = \"AVISIT\", subject = \"USUBJID\", group = TRTA)")),
    "`group` is a column in quotes \\(\"TRTA\"\\), not TRTA")
  expect_warning(exact_spec(mmrm_row(
    "cov_struct(\"ar1\", visits = \"AVISIT\", subject = \"USUBJID\", heterogeneous = \"yes\")")),
    "`heterogeneous` is TRUE or FALSE")
  expect_warning(exact_spec(mmrm_row(
    "cov_struct(\"us\", visits = \"AVISIT\", subject = \"USUBJID\", groups = \"TRTA\")")),
    "cov_struct\\(\\) has no argument `groups`.*mmrm would stop on it")
})
