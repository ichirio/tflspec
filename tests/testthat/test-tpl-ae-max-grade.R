# ae_max_grade: AEs by SOC and PT at the maximum severity or grade (#385)

mg_lv <- c("MILD", "MODERATE", "SEVERE")

# the ARD as the study's program makes it: each subject's highest record
# overall (as one more SOC, the overall row's), in each SOC, in each PT
mg_ard <- function(x, ae = adae[adae$SAFFL == "Y" & adae$TRTEMFL == "Y", ]) {
  saf <- adsl[adsl$SAFFL == "Y", ]
  adae_saf <- ae
  data <- lapply(x$ard$analysis_data[3:5], function(d) eval(parse(text = d$code)))
  h <- function(d, v) cards::ard_stack_hierarchical(d, variables = tidyselect::all_of(v),
                                                    by = TRT01A, denominator = saf, id = USUBJID)
  cards::bind_ard(h(data[[1L]], c("AEBODSYS", "AESEV")), h(data[[2L]], c("AEBODSYS", "AESEV")),
                  h(data[[3L]], c("AEBODSYS", "AEDECOD", "AESEV")), .distinct = TRUE, .quiet = TRUE)
}
mg_tab <- function(p) {
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  t
}
mg_skip <- function() {
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if(utils::packageVersion("rtfreporter") < "0.8.2.9083", "rtfreporter with #695")
}

test_that("expanded: the highest record overall, in each SOC and PT; the overall block pinned", {
  x <- tfl_template_expand("ae_max_grade", list(population = "SAF", grade_levels = mg_lv))
  ids <- vapply(x$ard$analysis_data, `[[`, "", "id")
  expect_identical(ids, c("adsl_saf", "adae_saf", "adae_saf_max", "adae_saf_max_soc", "adae_saf_max_pt"))
  code <- vapply(x$ard$analysis_data[3:5], `[[`, "", "code")
  expect_match(code[1L], "by = c(USUBJID)", fixed = TRUE)
  expect_match(code[1L], "dplyr::mutate(", fixed = TRUE)
  expect_match(code[1L], "AEBODSYS = \"Any TEAE\"", fixed = TRUE)
  expect_match(code[2L], "by = c(USUBJID, AEBODSYS)", fixed = TRUE)
  expect_match(code[3L], "by = c(USUBJID, AEBODSYS, AEDECOD)", fixed = TRUE)
  expect_match(code[3L], "match(AESEV, c(\"MILD\", \"MODERATE\", \"SEVERE\"))", fixed = TRUE)
  fns <- vapply(x$content, `[[`, "", "fn")
  expect_true(all(c("normalize_ard", "plan_levels", "table_plan", "plan_sort") %in% fns))
  keys <- Filter(function(it) it$fn == "plan_sort", x$content)[[1L]]$args$keys
  expect_identical(keys[1:3], c("levels(group1)", "-n@group1", "group1"))
  nm <- tfl_template_expand("ae_max_grade", list(population = "SAF", sort = "name"))
  nk <- Filter(function(it) it$fn == "plan_sort", nm$content)[[1L]]$args$keys
  expect_false(any(grepl("-n@", nk, fixed = TRUE)))
  # no levels: the grade's own order
  y <- tfl_template_expand("ae_max_grade", list(population = "SAF"))
  expect_match(y$ard$analysis_data[[3L]]$code, "xtfrm(AESEV)", fixed = TRUE)
})

test_that("as rows: the overall block and its grades, then each SOC's own rows, then its PTs", {
  mg_skip()
  for (ov in c("Any TEAE", "Subjects with at least one TEAE")) {
    x <- tfl_template_expand("ae_max_grade", list(population = "SAF", grade_levels = mg_lv,
                                                  overall = ov))
    t <- mg_tab(tpl_ae_max_grade(mg_ard(x), "SAF", grade_levels = mg_lv, overall = ov))
    expect_identical(t$group1[1:4], rep(ov, 4L), info = ov)
    expect_identical(as.character(t$label[2:4]), mg_lv, info = ov)
    soc <- t$group1[5L]
    blk <- t[t$group1 %in% soc, , drop = FALSE]
    own <- which(is.na(blk$group2))
    expect_identical(own, seq_along(own), info = ov)
    # the grades of a term add up to its count at any grade
    n <- function(z) as.integer(sub(" .*", "", z))
    expect_identical(n(blk[["Drug X"]][1L]), sum(n(blk[["Drug X"]][own[-1L]]), na.rm = TRUE))
  }
})

test_that("as columns: each arm's Any grade first; the overall row one row", {
  mg_skip()
  x <- tfl_template_expand("ae_max_grade", list(population = "SAF", grade_levels = mg_lv,
                                                grade_in = "cols"))
  t <- mg_tab(tpl_ae_max_grade(mg_ard(x), "SAF", grade_levels = mg_lv, grade_in = "cols"))
  arms <- c("Drug X", "Control")
  expect_identical(setdiff(names(t), c("group1", "label")),
                   unlist(lapply(arms, function(a) paste0(a, "____", c("Any grade", mg_lv)))))
  expect_identical(sum(t$group1 %in% "Any TEAE"), 1L)
  expect_identical(t$group1[1L], "Any TEAE")
  expect_false(is.na(t[["Drug X____MILD"]][1L]))
})

test_that("the overall block first even when a SOC has its count (a tie)", {
  mg_skip()
  ae <- adae[adae$SAFFL == "Y" & adae$TRTEMFL == "Y", ]
  # every subject with an event has one in this SOC
  soc1 <- names(sort(table(ae$AEBODSYS), decreasing = TRUE))[1L]
  ae <- ae[ae$USUBJID %in% ae$USUBJID[ae$AEBODSYS == soc1], ]
  x <- tfl_template_expand("ae_max_grade", list(population = "SAF", grade_levels = mg_lv,
                                                overall = "ZZ last by name"))
  t <- mg_tab(tpl_ae_max_grade(mg_ard(x, ae), "SAF", grade_levels = mg_lv,
                               overall = "ZZ last by name"))
  expect_identical(t$group1[1L], "ZZ last by name")
  expect_identical(t[["Drug X"]][1L], t[["Drug X"]][t$group1 %in% soc1 & is.na(t$group2) &
                                                      is.na(t$label)])
})

test_that("A (the call) = B (the items as the report's content)", {
  mg_skip()
  x <- tfl_template_expand("ae_max_grade", list(population = "SAF", grade_levels = mg_lv))
  ard <- mg_ard(x)
  a <- mg_tab(tpl_ae_max_grade(ard, "SAF", grade_levels = mg_lv))
  b <- suppressMessages(tfl_content_run(tfl_output("T-1", "table", content = x$content), ard))
  expect_identical(mg_tab(b), a)
})
