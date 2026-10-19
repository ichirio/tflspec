# A hierarchical analysis with no records (#367): the ARD's overall row 0,
# not a stop; complete_hierarchy() adds nothing under it unless asked

helpers_env <- function() {
  env <- new.env()
  eval(parse(text = tfl_helpers_code()), envir = env)
  env
}

test_that("ard_hierarchical_or_none(): cards' ARD with records; with none, the overall row 0 and the groups' N", {
  skip_if_not_installed("cards")
  env <- helpers_env()
  adsl <- cards::ADSL
  adae <- cards::ADAE[cards::ADAE$AESOC %in% unique(cards::ADAE$AESOC)[1:2], ]
  with_rec <- env$ard_hierarchical_or_none(adae, variables = c(AESOC, AEDECOD), by = TRTA,
                                           denominator = adsl, id = USUBJID, over_variables = TRUE)
  expect_identical(with_rec, cards::ard_stack_hierarchical(adae, variables = c(AESOC, AEDECOD),
                                                           by = TRTA, denominator = adsl,
                                                           id = USUBJID, over_variables = TRUE))
  # cards stops on none
  expect_error(cards::ard_stack_hierarchical(adae[0, ], variables = c(AESOC, AEDECOD), by = TRTA,
                                             denominator = adsl, id = USUBJID))
  none <- env$ard_hierarchical_or_none(adae[0, ], variables = c(AESOC, AEDECOD), by = TRTA,
                                       denominator = adsl, id = USUBJID, over_variables = TRUE)
  expect_s3_class(none, "card")
  expect_setequal(unique(none$variable), c("..ard_hierarchical_overall..", "TRTA"))
  ov <- none[none$variable == "..ard_hierarchical_overall..", ]
  expect_true(all(unlist(ov$stat[ov$stat_name == "n"]) == 0))
  expect_true(all(unlist(ov$stat[ov$stat_name == "p"]) == 0))
  n_arm <- table(adsl$TRTA)
  got <- unlist(ov$stat[ov$stat_name == "N"])
  names(got) <- vapply(ov$group1_level[ov$stat_name == "N"], as.character, "")
  expect_identical(as.numeric(got[names(n_arm)]), as.numeric(n_arm))
  # without over_variables: the groups' counts alone
  none2 <- env$ard_hierarchical_or_none(adae[0, ], variables = c(AESOC, AEDECOD), by = TRTA,
                                        denominator = adsl, id = USUBJID)
  expect_identical(unique(none2$variable), "TRTA")
  expect_error(env$ard_hierarchical_or_none(adae[0, ], variables = c(AESOC, AEDECOD)), "denominator")
})

test_that("the ARD program of a nested count with no records makes its table: the overall row 0 (0.0)", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  adam$ADAE <- adam$ADAE[0, ]
  dir <- exact_dir(adam)
  sp <- exact_spec(list(method = "hierarchical", population_id = "SAF", dataset = "ADAE",
                        by = "TRTA", variables = "AESOC | AEDECOD", args = "over_variables = TRUE"))
  code <- paste(tfl_ard_code(sp, part = "body", save = FALSE), collapse = "\n")
  expect_match(code, "ard_hierarchical_or_none(", fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  t <- suppressMessages(rtfreporter::plan_apply(
    rtfreporter::table_plan(rtfreporter::normalize_ard(ard, hierarchy = c("AESOC", "AEDECOD"),
                                                       overall = "Any TEAE"),
                            cols = "TRTA", rows = c(soc = "AESOC", pt = "AEDECOD")) |>
      rtfreporter::plan_cells(hierarchical = "{n} ({p:.1f%})", notes = FALSE), "table"))
  t <- as.data.frame(t)
  expect_identical(nrow(t), 1L)
  expect_identical(t$soc, "Any TEAE")
  expect_true(all(unlist(t[setdiff(names(t), c("soc", "pt", "label"))]) == "0 (0.0)"))
  # ARS shows cards' function, not the helper
  expect_false(grepl("ard_hierarchical_or_none", paste(tflspec:::.analysis_body(
    sp$analyses[1L, ], tfl_ard_methods(), "USUBJID", function(a) FALSE), collapse = ""), fixed = TRUE))
})

test_that("complete_hierarchy(): nothing under an overall row of 0, unless when_none = TRUE", {
  skip_if_not_installed("cards")
  env <- helpers_env()
  adsl <- data.frame(USUBJID = paste0("S", 1:4), TRT = factor(c("A", "B", "A", "B")))
  ae <- data.frame(USUBJID = character(), TRT = factor(character(), levels = c("A", "B")),
                   SOC = factor(character(), levels = c("Cardiac", "Eye")))
  none <- env$ard_hierarchical_or_none(ae, variables = c(SOC), by = TRT, denominator = adsl,
                                       id = USUBJID, over_variables = TRUE)
  expect_identical(env$complete_hierarchy(none, "SOC"), none)
  # with records: as before
  ae1 <- data.frame(USUBJID = "S1", TRT = factor("A", levels = c("A", "B")),
                    SOC = factor("Cardiac", levels = c("Cardiac", "Eye")))
  r <- cards::ard_stack_hierarchical(ae1, variables = c(SOC), by = TRT, denominator = adsl,
                                     id = USUBJID, over_variables = TRUE)
  out <- env$complete_hierarchy(r, "SOC")
  expect_true("Eye" %in% vapply(out$variable_level, function(v) if (length(v)) as.character(v[[1L]]) else NA_character_, ""))
})
