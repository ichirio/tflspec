# Gate 1 (identical results) and Gate 3 (not slower) of the prototype
# (c/proto-programs): each fixture study run today's way and the
# prototype's, in folders of their own, then compared.
#
#   Rscript inst/dev/proto-programs/measure.R <out-dir> [study ...]
#
# Today:     every ARD program, then every report program (an official
#            run's "all": ARD before the reports).
# Prototype: run = "full" (the full programs, then collect_ard()), and
#            run = "ard" (the ARD programs, then collect_ard()) in another
#            folder.  "both" writes both kinds and runs the full ones, so
#            it is "full" as a run.
# Fictional data only (tflspec's simulated oncology study).

args <- commandArgs(TRUE)
out_dir <- normalizePath(args[1L], "/", mustWork = FALSE)
studies <- if (length(args) > 1L) args[-1L] else c("sample", "edited")
pp_root <- normalizePath(Sys.getenv("PP_ROOT", "."), "/")
adam <- Sys.getenv("PP_ADAM")
source(file.path(pp_root, "inst", "dev", "proto-programs", "study.R"))
source(file.path(pp_root, "inst", "dev", "proto-programs", "compare.R"))

run_today <- function(x, dir) {
  p <- pp_write_split(x, dir)
  t0 <- Sys.time()
  rows <- lapply(c(p$ard, p$tfl), function(f) {
    r <- pp_rscript(dir, f)
    data.frame(program = f, status = r$status, seconds = r$seconds, peak_kb = r$kb)
  })
  out <- do.call(rbind, rows)
  attr(out, "wall") <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  out
}

run_proto <- function(x, dir, run) {
  w <- pp_write_onepass(x, dir, ard = TRUE)
  t0 <- Sys.time()
  out <- tflspec::tfl_onepass_run(dir, w$programs, run = run, needs = w$needs)
  attr(out, "wall") <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  out
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
res <- list()
for (s in studies) {
  base <- file.path(out_dir, s)
  unlink(base, recursive = TRUE)
  d <- c(today = file.path(base, "today"), full = file.path(base, "full"),
         ard = file.path(base, "ard"))
  x <- NULL
  for (k in names(d)) x <- pp_study_base(s, d[[k]], adam)
  runs <- list(today = run_today(x, d[["today"]]),
               full = run_proto(x, d[["full"]], "full"),
               ard = run_proto(x, d[["ard"]], "ard"))
  res[[s]] <- list(runs = runs, cmp = pp_compare(d, names(x$reports)))
  saveRDS(res[[s]], file.path(base, "result.rds"))
  cat("\n==", s, "\n")
  print(res[[s]]$cmp$summary)
  for (k in names(runs)) {
    r <- runs[[k]]
    cat(sprintf("%-6s programs %3d  failed %2d  wall %6.1fs  sum %6.1fs  peak %5.0f MB\n",
                k, nrow(r), sum(r$status != 0L, na.rm = TRUE) + sum(is.na(r$status)),
                attr(r, "wall"), sum(r$seconds), max(r$peak_kb, na.rm = TRUE) / 1024))
  }
}
saveRDS(res, file.path(out_dir, "result.rds"))
