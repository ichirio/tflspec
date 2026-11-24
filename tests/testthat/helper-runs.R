# Helpers for the run definitions tests (test-runs-file.R, test-runs-run.R):
# a helper file, so that every test file sees them.

# a study folder: reports T-1, T-2, F-1 (reads T-1's ARD), L-1; the report
# list says them in this order, T-2 first
runs_study <- function(toc_sets = NULL) {
  d <- normalizePath(tempfile("runsstudy"), winslash = "/", mustWork = FALSE)
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines("study_id: S-1", file.path(d, "study.yml"))
  an <- list(list(id = "N", method = "cards::ard_tabulate", data = "adsl_sub"))
  out <- function(id, ...) {
    tfl_write_output(tfl_output(id, ...), file.path(d, "spec", "outputs", paste0(id, ".yml")))
  }
  out("T-1", "table", ard = list(analyses = an))
  out("T-2", "table", ard = list(analyses = an))
  out("F-1", "figure", page = list(report = list(ard_source = "table:T-1")))
  out("L-1", "listing")
  ent <- function(id, kind) {
    c(paste0("  - id: ", id), paste0("    kind: ", kind),
      if (!is.null(toc_sets[[id]])) c("    sets:", paste0("      - ", toc_sets[[id]])))
  }
  writeLines(c("outputs:", ent("T-2", "table"), ent("T-1", "table"), ent("F-1", "figure"),
               ent("L-1", "listing")),
             file.path(d, "spec", "toc.yml"))
  d
}

write_runs_text <- function(d, lines) {
  writeLines(lines, file.path(d, "spec", "runs.yml"))
}
