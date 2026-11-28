# The templates' registration lines changed from 2026-11-24 (#993: `ai =
# TRUE` on the shipped templates; #995: every template has a label).  They
# run while the package loads, before covr counts: the registry keeps the
# functions of that time.  A register that only calls .tfl_template() can be
# run again and gives the same entry; one that adds to the package's
# tables (km's, the figures', pk's .tpl_extend()) cannot after loading (the
# namespace is locked), and is checked by what it registered.

cov_entry <- function(name) {
  e <- get(name, envir = .tpl_registry, inherits = FALSE)
  e[setdiff(names(e), "body")]
}

test_that("a table template's register can run again and gives the same entry, with its label and ai flag", {
  regs <- list(lab_abnormal = .tpl_la_register, ae_eair = .tpl_eair_register,
               ancova = .tpl_ancova_register, param_change = .tpl_pc_register,
               ae_overview = .tpl_ae_overview_register)
  for (nm in names(regs)) {
    # (the entry as it was put back, whatever an earlier test left)
    was <- get(nm, envir = .tpl_registry, inherits = FALSE)
    do.call(on.exit, list(bquote(assign(.(nm), .(was), envir = .tpl_registry)), add = TRUE))
    before <- cov_entry(nm)
    expect_identical(regs[[nm]](), nm)
    after <- cov_entry(nm)
    expect_identical(after[c("name", "kind", "layout", "label", "ai", "extends")],
                     before[c("name", "kind", "layout", "label", "ai", "extends")])
    expect_true(after$ai)
    expect_true(is.character(after$label) && nzchar(after$label))
  }
})

test_that("pk_conc, km and the figure templates are registered with a label and ai = TRUE", {
  t <- tfl_templates()
  want <- c(pk_conc = "PK concentrations by subject with their summary",
            km = "Kaplan-Meier curves",
            forest_hr = "Hazard ratio by subgroup (forest)",
            mean_se = "Mean +/- SE by visit",
            mean_ci = "Mean (95% CI) by visit",
            waterfall = "Waterfall of each subject's change")
  r <- t[match(names(want), t$name), ]
  expect_identical(r$label, unname(want))
  expect_true(all(r$ai))
})

test_that("a figure's group has no default: the report says it", {
  for (ps in list(.tpl_mean_params(), .tpl_forest_params())) {
    g <- Filter(function(p) identical(p$name, "group"), ps)[[1L]]
    expect_true(g$required)
    expect_null(g$default)
  }
})
