# Gate 2: the test_that() blocks of tflspec that exist for today's split of
# a report's own ARD (a block whose code names one of the patterns below).
#   Rscript inst/dev/proto-programs/count_tests.R
pats <- c(
  study_ard_helpers = "(^|[^_.a-z])(save_ard|read_ard|ard_fingerprint)\\(",
  own_rows_template = "table_data|\\{ARD_PROGRAM\\}|run programs/ard/",
  reports_only_run = "have_ard|reports-only|reports only|\\.batch_ard_error|failed_ard",
  own_reads = "reads = \"[A-Z]+-[0-9A-Z-]+\", kind = \"own\"|kind == \"own\"|tfl_ard_reads\\(")
rows <- list()
for (f in list.files("tests/testthat", "^test-.*\\.R$", full.names = TRUE)) {
  ex <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
  for (e in ex) {
    if (!is.call(e) || !identical(e[[1L]], as.name("test_that"))) next
    txt <- paste(deparse(e), collapse = "\n")
    hit <- names(pats)[vapply(pats, grepl, NA, x = txt)]
    if (length(hit)) rows[[length(rows) + 1L]] <- data.frame(file = basename(f), test = as.character(e[[2L]]),
                                                             kinds = paste(hit, collapse = ","))
  }
}
d <- do.call(rbind, rows)
print(table(d$file, d$kinds))
cat("blocks:", nrow(d), "\n")
print(table(unlist(strsplit(d$kinds, ","))))
