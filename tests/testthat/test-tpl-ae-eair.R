# ae_eair: SOC / PT by the exposure-adjusted incidence rate

# the ARD as the study's program makes it: each level's time at risk (its
# analysis data's code), the rates by cardx, the term as the variable
eair_ard <- function(x) {
  adsl_saf <- adsl[adsl$SAFFL == "Y", ]
  d <- x$ard$analysis_data
  adae_saf <- adae[adae$USUBJID %in% adsl_saf$USUBJID & adae$TRTEMFL == "Y", ]
  adae_saf$DAY <- eval(parse(text = d[[2L]]$derive), adae_saf)
  e <- environment()
  for (i in 3:5) assign(d[[i]]$id, eval(parse(text = d[[i]]$code), e))
  rate <- function(data, strata, k) {
    a <- cardx::ard_incidence_rate(data, time = YEARS, count = EVENT, id = USUBJID, by = TRT01A,
                                   strata = cards::all_of(strata), n_person_time = 100,
                                   unit_label = "years")
    a$variable <- a[[paste0("group", k)]]
    a$variable_level <- a[[paste0("group", k, "_level")]]
    a[c(paste0("group", k), paste0("group", k, "_level"))] <- NULL
    a
  }
  cards::bind_ard(cards::ard_tabulate(adsl_saf, variables = TRT01A),
                  rate(adae_saf_risk, "AEBODSYS", 2L),
                  rate(adae_saf_risk_soc, "AEBODSYS", 2L),
                  rate(adae_saf_risk_pt, c("AEBODSYS", "AEDECOD"), 3L))
}
eair_tab <- function(p) {
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  t
}
eair_skip <- function() {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("dplyr")
}

test_that("expanded: the event's day, each level's time at risk, the rates by cardx", {
  x <- tfl_template_expand("ae_eair", list(population = "SAF"))
  d <- x$ard$analysis_data
  expect_identical(vapply(d, `[[`, "", "id"), c("adsl_saf", "adae_saf", "adae_saf_risk",
                                                "adae_saf_risk_soc", "adae_saf_risk_pt"))
  expect_identical(as.character(d[[2L]]$where), "TRTEMFL == \"Y\"")
  expect_identical(as.character(d[[2L]]$derive), "DAY = ASTDY")
  expect_match(d[[5L]]$code, ".by = c(USUBJID, AEBODSYS, AEDECOD)", fixed = TRUE)
  expect_match(d[[5L]]$code, "dplyr::if_else(EVENT, DAY, TRTDURD) / 365.25", fixed = TRUE)
  expect_match(d[[3L]]$code, "AEBODSYS = \"Any TEAE\"", fixed = TRUE)
  an <- x$ard$analyses
  expect_identical(vapply(an, `[[`, "", "method")[-1L], rep("cardx::ard_incidence_rate", 3L))
  expect_match(an[[4L]]$post, "variable = group3", fixed = TRUE)
  expect_match(an[[2L]]$args, "n_person_time = 100", fixed = TRUE)
  # the day as R, the rate per 1000 person-years
  y <- tfl_template_expand("ae_eair", list(population = "SAF", start = "ASTDT - TRTSDT + 1",
                                           per = 1000))
  expect_identical(as.character(y$ard$analysis_data[[2L]]$derive), "DAY = ASTDT - TRTSDT + 1")
  expect_match(y$ard$analyses[[2L]]$args, "n_person_time = 1000", fixed = TRUE)
})

test_that("a PT's rate by hand: its subjects over their time at risk, per 100 person-years", {
  eair_skip()
  x <- tfl_template_expand("ae_eair", list(population = "SAF"))
  t <- eair_tab(tpl_ae_eair(eair_ard(x), "SAF"))
  saf <- adsl[adsl$SAFFL == "Y" & adsl$TRT01A == "Drug X", ]
  ae <- adae[adae$TRTEMFL == "Y" & adae$AEDECOD == "Nausea" & adae$USUBJID %in% saf$USUBJID, ]
  first <- tapply(ae$ASTDY, ae$USUBJID, min)
  days <- ifelse(saf$USUBJID %in% names(first), first[saf$USUBJID], saf$TRTDURD)
  n <- length(first)
  rate <- n / (sum(days) / 365.25) * 100
  expect_identical(t[["Drug X"]][t$label %in% "Nausea"], sprintf("%d (%.2f)", n, rate))
  # the overall row first, then the SOCs, each over its PTs
  expect_identical(t$group1[1L], "Any TEAE")
  soc <- t$group1[t$label %in% "Nausea"]
  expect_true(is.na(t$label[match(soc, t$group1)]))
})

test_that("A (the call) = B (the items as the report's content)", {
  eair_skip()
  x <- tfl_template_expand("ae_eair", list(population = "SAF"))
  ard <- eair_ard(x)
  ta <- eair_tab(tpl_ae_eair(ard, "SAF"))
  b <- suppressMessages(tfl_content_run(tfl_output("T-1", "table", content = x$content), ard))
  expect_identical(eair_tab(b), ta)
  hd <- Filter(function(it) it$fn == "plan_col_header", x$content)[[1L]]$args$header
  expect_identical(hd[[4L]]$text, "n (EAIR per 100 PY)")
})
