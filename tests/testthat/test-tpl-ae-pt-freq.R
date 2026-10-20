# ae_pt_freq: AEs by preferred term alone, most frequent first, >= x% (#383)

ptf_ard <- function() {
  saf <- adsl[adsl$SAFFL == "Y", ]
  ae <- adae[adae$SAFFL == "Y" & adae$TRTEMFL == "Y", ]
  cards::ard_stack_hierarchical(ae, variables = AEDECOD, by = TRT01A, denominator = saf,
                                id = USUBJID, over_variables = TRUE)
}
ptf_cl <- list(TRT01A = c("Drug X", "Control"))
ptf_fn <- function(x) vapply(x$content, `[[`, "", "fn")
ptf_arg <- function(x, fn, arg) Filter(function(it) identical(it$fn, fn), x$content)[[1L]]$args[[arg]]

test_that("expanded: one level, sorted by the code list's first group, the filter as asked", {
  x <- tfl_template_expand("ae_pt_freq", list(population = "SAF"), codelists = ptf_cl)
  expect_identical(ptf_fn(x), c("normalize_ard", "table_plan", "plan_sort", "plan_col_header",
                                "plan_paginate_rows"))
  expect_identical(ptf_arg(x, "plan_sort", "keys"), c(".overall", "-n[Drug X]", "label"))
  expect_identical(x$ard$analyses[[1L]]$variables, "AEDECOD")
  expect_identical(as.character(x$ard$analysis_data[[2L]]$where), "TRTEMFL == \"Y\"")
  # no code list: all the columns' counts
  y <- tfl_template_expand("ae_pt_freq", list(population = "SAF"))
  expect_identical(ptf_arg(y, "plan_sort", "keys"), c(".overall", "-n", "label"))
  # min_pct in any / all / the Total column; sort_by
  a <- tfl_template_expand("ae_pt_freq", list(population = "SAF", min_pct = 5), codelists = ptf_cl)
  expect_identical(as.character(ptf_arg(a, "plan_filter", "where")), "p >= 0.05")
  expect_null(ptf_arg(a, "plan_filter", "any"))
  b <- tfl_template_expand("ae_pt_freq", list(population = "SAF", min_pct = 10, min_pct_in = "all"))
  expect_false(ptf_arg(b, "plan_filter", "any"))
  t <- tfl_template_expand("ae_pt_freq", list(population = "SAF", min_pct = 5, min_pct_in = "total"))
  expect_identical(as.character(ptf_arg(t, "plan_filter", "where")), "p >= 0.05 & TRT01A == \"Total\"")
  expect_identical(ptf_arg(t, "plan_sort", "keys")[2L], "-n[Total]")
  s <- tfl_template_expand("ae_pt_freq", list(population = "SAF", sort_by = "Control"),
                           codelists = ptf_cl)
  expect_identical(ptf_arg(s, "plan_sort", "keys")[2L], "-n[Control]")
  expect_error(tfl_template_expand("ae_pt_freq", list(population = "SAF", min_pct_in = "some")))
})

test_that("the table: the overall row first, the PTs by Drug X's subjects, the filter kept", {
  skip_if_not_installed("cards")
  ard <- ptf_ard()
  tab <- function(p) as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  all_pt <- tab(tpl_ae_pt_freq(ard, "SAF", codelists = ptf_cl))
  expect_identical(all_pt$label[1L], "Any TEAE")
  n1 <- as.integer(sub(" .*", "", all_pt[["Drug X"]][-1L]))
  expect_false(is.unsorted(rev(n1)))
  any20 <- tab(tpl_ae_pt_freq(ard, "SAF", min_pct = 20, codelists = ptf_cl))
  all20 <- tab(tpl_ae_pt_freq(ard, "SAF", min_pct = 20, min_pct_in = "all", codelists = ptf_cl))
  expect_lt(nrow(any20), nrow(all_pt))
  expect_lt(nrow(all20), nrow(any20))
  # A (the call) = B (the items as the report's content)
  x <- tfl_template_expand("ae_pt_freq", list(population = "SAF", min_pct = 20), codelists = ptf_cl)
  b <- suppressMessages(tfl_content_run(tfl_output("T-1", "table", content = x$content), ard))
  expect_identical(tab(b), any20)
})
