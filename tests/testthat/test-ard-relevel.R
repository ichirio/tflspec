# relevel_ard(): an ARD that is not cards' in the code lists' order (#362):
# cardx's survfit makes the arms' factor again, alphabetical.

# tflspec's adtte: ggsurvfit's, attached by an earlier test, has no TRT01A
# (under devtools::test() the helpers' copy is behind it)
adtte <- tflspec::adtte

lvl <- function(x) vapply(x, function(v) if (length(v)) as.character(v[[1L]]) else NA_character_, "")

test_that("relevel_ard() puts a column's levels in its code list's order, values and rows as they were", {
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  env <- new.env()
  eval(parse(text = tfl_helpers_code()), envir = env)
  adtte <- as.data.frame(cards::ADTTE)
  arms <- c("Xanomeline Low Dose", "Placebo", "Xanomeline High Dose")
  adtte$TRTA <- factor(adtte$TRTA, levels = arms)
  fit <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte)
  ard <- cardx::ard_survival_survfit(fit, times = c(30, 60))
  arm_rows <- which(ard$group1 %in% "TRTA")
  # cardx: the rows in the arms' order, the factor's levels alphabetical
  expect_identical(levels(ard$group1_level[[arm_rows[1L]]]), sort(arms))
  out <- env$relevel_ard(ard, TRTA = arms)
  expect_identical(levels(out$group1_level[[arm_rows[1L]]]), arms)
  expect_true(all(vapply(out$group1_level[arm_rows], function(x) identical(levels(x), arms), NA)))
  expect_identical(lvl(out$group1_level), lvl(ard$group1_level))
  expect_identical(out$stat, ard$stat)
  expect_identical(dim(out), dim(ard))
  expect_s3_class(out, "card")
  # a list of ARDs: each of them; a value the list does not have: as it was
  two <- env$relevel_ard(list(ard, ard), TRTA = arms)
  expect_identical(levels(two[[2L]]$group1_level[[arm_rows[1L]]]), arms)
  part <- env$relevel_ard(ard, TRTA = arms[1:2])
  hi <- arm_rows[lvl(ard$group1_level[arm_rows]) == "Xanomeline High Dose"][1L]
  expect_identical(part$group1_level[[hi]], ard$group1_level[[hi]])
  # a variable's own rows (variable_level) too
  tab <- cards::ard_tabulate(data.frame(SEX = c("M", "F", "F")), variables = SEX)
  expect_identical(levels(env$relevel_ard(tab, SEX = c("M", "F"))$variable_level[[1L]]), c("M", "F"))
})

test_that("the ARD program puts relevel_ard() after an analysis that is not cards', on the code lists' columns it reads", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  skip_if_not_installed("survival")
  adam <- exact_data()
  dir <- exact_dir(adam)
  sp <- exact_spec(list(
    method = "custom", population_id = "SAF", dataset = "ADTTE",
    code = paste("cardx::ard_survival_survfit(survival::survfit(",
                 "survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = data), times = c(30, 60))")))
  arms <- c("Xanomeline Low Dose", "Placebo", "Xanomeline High Dose")
  cl <- data.frame(output_id = "T", variable = "TRTA", value = arms, order = as.character(1:3))
  code <- paste(tfl_ard_code(sp, part = "body", codelists = cl), collapse = "\n")
  expect_match(code, "relevel_ard(TRTA = cl_trta)", fixed = TRUE)
  expect_lt(regexpr("relevel_ard(", code, fixed = TRUE), regexpr("tag_ard(report_id", code, fixed = TRUE))
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE, codelists = cl))
  k <- which(ard$group1 %in% "TRTA")[1L]
  expect_identical(levels(ard$group1_level[[k]]), arms)
  # without the code list: no step
  expect_false(grepl("relevel_ard", paste(tfl_ard_code(sp, part = "body"), collapse = "\n"), fixed = TRUE))
  # a cards analysis keeps the data's order itself: no step
  sp2 <- exact_spec(list(method = "categorical", dataset = "ADSL", population_id = "SAF",
                         by = "ARM", variables = "SEX"))
  cl2 <- data.frame(output_id = "T", variable = "ARM", order = as.character(1:3),
                    value = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
  expect_false(grepl("relevel_ard", paste(tfl_ard_code(sp2, part = "body", codelists = cl2),
                                          collapse = "\n"), fixed = TRUE))
  # the fingerprint says the step (an ARD made before it is made again)
  h1 <- tflspec:::.ard_output_hash(sp, "T", codelists = cl)
  local_mocked_bindings(.ard_keeps_order = function(a) TRUE)
  expect_false(identical(h1, tflspec:::.ard_output_hash(sp, "T", codelists = cl)))
})
