# The arguments the stack templates need and have no default for (migration batch 3: the
# variable set -- the hierarchy, the term -- and `by`): what a test gives, then its own.

stack_req <- function(name) {
  switch(name,
    ae_pt_freq = list(by = "TRT01A", term = "AEDECOD"),
    hierarchical = , ae_soc_pt = , ae_ser_soc_pt = , ae_disc_soc_pt = , ae_max_grade = , ae_eair =
      list(by = "TRT01A", hierarchy = c("AEBODSYS", "AEDECOD")),
    list())
}

# A template's example as a report's template args: without `population` when the template takes
# the analysis set from the report's `population:` field.
report_example <- function(name) {
  ex <- tflspec:::.tpl_examples[[name]]
  if (isTRUE(tflspec:::.tpl_get(name, "test")$params$population$hidden)) ex$population <- NULL
  ex
}
