# ae_rel_soc_pt: treatment-emergent AEs related to the study drug, by SOC and PT (#627)

rel_where <- "TRTEMFL == \"Y\" & AEREL == \"RELATED\""
rel_fn <- function(x) vapply(x$content, `[[`, "", "fn")

test_that("expanded: ae_soc_pt's items, the related filter, its own any-event label, shown to the AI", {
  x <- tfl_template_expand("ae_rel_soc_pt", list(population = "SAF"))
  y <- tfl_template_expand("ae_soc_pt", list(population = "SAF", where = rel_where,
                                             any_row_label = "Any related TEAE"))
  # (the same items and ARD as ae_soc_pt with these arguments; only where they
  # come from, and the ARD's label, the variant's own)
  no_from <- function(items) lapply(items, function(it) it[names(it) != "from"])
  expect_identical(rel_fn(x), rel_fn(y))
  expect_identical(no_from(x$content), no_from(y$content))
  expect_identical(x$content[[1L]]$from, "template:ae_rel_soc_pt")
  expect_identical(x$ard$analyses[[1L]]$label, "Related adverse events by SOC and PT")
  x$ard$analyses[[1L]]$label <- y$ard$analyses[[1L]]$label
  expect_identical(x$ard, y$ard)
  expect_identical(x$ard$analyses[[1L]]$variables, c("AEBODSYS", "AEDECOD"))
  wh <- vapply(x$ard$analysis_data, function(d) paste(as.character(d$where), collapse = ""), "")
  expect_true(rel_where %in% wh)
  # the registry: a variant of ae_soc_pt, its label, ai = TRUE (#993)
  t <- tfl_templates()
  expect_identical(t$from[t$name == "ae_rel_soc_pt"], "ae_soc_pt")
  expect_identical(t$label[t$name == "ae_rel_soc_pt"], "Related adverse events by SOC and PT")
  expect_true(t$ai[t$name == "ae_rel_soc_pt"])
  p <- tfl_template_params("ae_rel_soc_pt")
  expect_identical(p$where$default, rel_where)
  expect_identical(p$any_row_label$default, "Any related TEAE")
})

test_that("the table: the any-event row is the key counted in plain R", {
  skip_if_not_installed("cards")
  # the key, counted from adsl and adae without the template: the safety
  # population, and its treatment-emergent events judged related (AEREL is
  # "RELATED" or "NOT RELATED": the whole value, never a substring)
  saf <- adsl[adsl$SAFFL == "Y", ]
  ae <- adae[adae$SAFFL == "Y" & adae$TRTEMFL == "Y" & adae$AEREL == "RELATED", ]
  arms <- c("Drug X", "Control")
  big_n <- as.integer(table(saf$TRT01A)[arms])                     # subjects in SAF
  subj <- vapply(arms, function(a) length(unique(ae$USUBJID[ae$TRT01A == a])), 0L)
  recs <- vapply(arms, function(a) sum(ae$TRT01A == a), 0L)       # related TEAE records
  pct <- sprintf("%.1f", 100 * subj / big_n)
  expect_identical(big_n, c(80L, 78L))
  expect_identical(unname(subj), c(76L, 65L))
  expect_identical(pct, c("95.0", "83.3"))
  expect_identical(unname(recs), c(256L, 185L))
  expect_identical(length(unique(ae$AEBODSYS)), 12L)
  # (a substring match would also catch "NOT RELATED": more subjects, wrongly)
  sub_ae <- adae[adae$SAFFL == "Y" & adae$TRTEMFL == "Y" & grepl("RELATED", adae$AEREL), ]
  expect_gt(length(unique(sub_ae$USUBJID)), sum(subj))

  ard <- cards::ard_stack_hierarchical(ae, variables = c(AEBODSYS, AEDECOD),
                                       by = TRT01A, denominator = saf,
                                       id = USUBJID, over_variables = TRUE)
  p <- tfl_tpl(ard, "ae_rel_soc_pt", population = "SAF")
  tab <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  expect_identical(tab$group1[1L], "Any related TEAE")
  expect_identical(tab[["Drug X"]][1L], sprintf("%d (%s)", subj[["Drug X"]], pct[1L]))
  expect_identical(tab[["Control"]][1L], sprintf("%d (%s)", subj[["Control"]], pct[2L]))
  # the SOCs: the 12 of the key
  expect_setequal(setdiff(unique(tab$group1), "Any related TEAE"), unique(ae$AEBODSYS))
})
