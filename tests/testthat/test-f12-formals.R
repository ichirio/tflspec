# F12 (the foundation decision of 2026-11-22, 1 and 2a): the card fields
# come from tflspec's definitions only, and rtfreporter's formals are the
# reference.  Every formal of every exported plan_*() is a schema row, the
# data the pipe hands in, or `...` -- or listed below as a gap with the day
# it is filled.  A new rtfreporter argument the schema does not say fails here.

f12_not_cards <- c("plan_apply", "plan_header_tokens", "plan_n_candidates", "plan_layers",
                   "plan_template")
# the gaps there are today: one filled must come off this list (none since
# 2026-11-22: plan_total direction, plan_cell_style row, plan_titles and
# plan_footnotes border added)
f12_gaps <- character()
# a formal the SPEC writes another way: plan_append's `part` is a table_plan
# that tflspec builds from the part's own `data` and `content`
f12_written_as <- c(plan_append.part = "data, content")
f12_piped <- c(".plan", "x", "data", "ard")

test_that("every formal of rtfreporter's exported plan_*() is in the schema (or a listed gap)", {
  skip_if_not_installed("rtfreporter")
  fns <- grep("^plan_", getNamespaceExports("rtfreporter"), value = TRUE)
  fns <- setdiff(sort(fns), f12_not_cards)
  miss <- unlist(lapply(fns, function(f) {
    fm <- setdiff(names(formals(getExportedValue("rtfreporter", f))), c(f12_piped, "..."))
    e <- tfl_schema(f)
    if (is.null(e)) return(paste0(f, " (no schema entry)"))
    have <- c(names(e$args), if (is.list(e$dots)) e$dots$key)
    if (length(fm)) paste0(f, ".", setdiff(fm, have)) else character()
  }))
  miss <- miss[!grepl("[.]$", miss)]
  miss <- setdiff(miss, names(f12_written_as))
  expect_true(all(vapply(strsplit(names(f12_written_as), ".", fixed = TRUE), function(p) {
    all(trimws(strsplit(f12_written_as[[paste(p, collapse = ".")]], ",")[[1L]]) %in% names(tfl_schema(p[1L])$args))
  }, NA)))
  expect(length(setdiff(miss, f12_gaps)) == 0L,
         paste0("rtfreporter arguments the schema does not say: ", paste(setdiff(miss, f12_gaps), collapse = ", ")))
  # (a gap whose formal the installed rtfreporter does not have yet is not
  # counted as filled: an older rtfreporter)
  has <- vapply(strsplit(f12_gaps, ".", fixed = TRUE), function(p) {
    p[1L] %in% fns && p[2L] %in% names(formals(getExportedValue("rtfreporter", p[1L])))
  }, NA)
  filled <- setdiff(f12_gaps[has], miss)
  expect(length(filled) == 0L,
         paste0("filled -- take them off f12_gaps: ", paste(filled, collapse = ", ")))
})

test_that("every table input says its columns", {
  ents <- tflspec:::.schema_all(figure = FALSE)
  bad <- unlist(lapply(names(ents), function(f) {
    args <- ents[[f]]$args %||% list()
    n <- names(args)[vapply(args, function(d) is.list(d) && identical(d$input, "table") && is.null(d$columns), NA)]
    if (length(n)) paste0(f, ".", n) else character()
  }))
  # (none without today; a new one is added here only with a reason)
  known <- character()
  # (step<content>: its columns are the items' own cards, not a columns: map)
  bad <- bad[!vapply(strsplit(bad, ".", fixed = TRUE), function(p) {
    identical(ents[[p[1L]]]$args[[p[2L]]]$type, "step<content>")
  }, NA)]
  expect(length(setdiff(bad, known)) == 0L,
         paste0("table inputs with no columns: ", paste(setdiff(bad, known), collapse = ", ")))
  expect(length(setdiff(known, bad)) == 0L,
         paste0("now with columns -- take them off the list: ", paste(setdiff(known, bad), collapse = ", ")))
})
