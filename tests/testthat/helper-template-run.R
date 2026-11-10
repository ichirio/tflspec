# Shared by test-template-to-spec-run.R and test-golden-rtf.R: a study of the
# bundled data, a catalog template's report, and child Rs that build the
# reports side by side.

# a study of the bundled ADaM data (spec/defaults.yml as tfl_ai_study_init()
# writes it), in `root`/study
tsr_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adlb", "advs", "adtte", "adrs", "adtr", "adex")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  # (pk_conc's concentrations: helper-pk.R)
  haven::write_xpt(tflspec:::.ai_chr(pk_adpc(tflspec::adsl)), file.path(d, "adam", "adpc.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "TSR-1")
  d
}

# The catalog's examples this study cannot build as they are -- the same in
# both forms, so no conversion is at stake: km_table and stat_block take
# the report's own ARD analyses (`ard:`), which an example has not;
# param_change's example keeps ANL01FL == "Y", which the bundled ADLB has
# not (its records are taken as they are here).
tsr_args <- list(param_change = list(where = NULL))
tsr_without <- c("km_table", "stat_block")

# a template report of the catalog's example (a template whose data stage
# is the report's own: its data: steps, as test-template-spec.R gives them);
# `use` names the template the report calls when it is not `nm`'s own (a
# company's copy of it: the report is the same, id and label included)
tsr_output <- function(nm, use = nm) {
  tp <- tfl_templates()
  own <- if (any(vapply(tflspec:::.tpl_get(nm, "test")$params, function(p) isTRUE(p$hidden) &&
                          identical(p$stage, "data"), NA))) {
    list(steps = list(list(step = "read", dataset = "ADTTE"), list(step = "param", value = "PFS")))
  }
  tfl_output(paste0("X-", gsub("_", "-", nm)), tp$kind[tp$name == nm], population = "SAF",
             label = nm, template = list(name = use, args = utils::modifyList(
               tflspec:::.tpl_examples[[nm]], tsr_args[[nm]] %||% list(), keep.null = FALSE)),
             data = own)
}

# Child Rs, each building its share of the study's reports `ids` (they are
# independent: a few children side by side, the slow ones spread over them).
# `script` is the child's program (lines): it gets d, part, sdir and the ids
# as its arguments, builds each report in the copy `sdir` of the study (which
# holds only that share's reports -- a run reads every report of its study),
# and leaves its results in `d` and last a file tsr-<part>.csv there (a
# build's error is recorded, never stops the loop).  `d` is a copy of the
# study; `w` an empty folder for the children's own files.  Returns the
# csvs read together (`r`) and the children's log (`out`).
tsr_children <- function(d, ids, script, w, n_max = 4L, mins = 30) {
  rscript <- file.path(R.home("bin"), "Rscript")
  writeLines(script, file.path(w, "run.R"))
  n_child <- min(n_max, length(ids))
  share <- rep_len(seq_len(n_child), length(ids))
  sds <- file.path(w, paste0("study-", seq_len(n_child)))
  for (k in seq_len(n_child)) {
    file.copy(d, w, recursive = TRUE)
    file.rename(file.path(w, basename(d)), sds[k])
    unlink(file.path(sds[k], "spec", "outputs", paste0(unname(ids)[share != k], ".yml")))
  }
  logs <- file.path(w, paste0("child-", seq_len(n_child), ".log"))
  for (k in seq_len(n_child)) {
    system2(rscript, c(file.path(w, "run.R"), d, k, sds[k], unname(ids)[share == k]),
            stdout = logs[k], stderr = logs[k], wait = FALSE)
  }
  csvs <- file.path(d, paste0("tsr-", seq_len(n_child), ".csv"))
  # (each child writes its csv last, errors of a build recorded: wait for
  # them all)
  t0 <- Sys.time()
  while (!all(file.exists(csvs)) && difftime(Sys.time(), t0, units = "mins") < mins) {
    Sys.sleep(1)
  }
  # (a child may still hold its log open after its csv is written: on Windows
  # reading it then fails, "Permission denied"; the log is only for the message)
  out <- unlist(lapply(logs, function(l) {
    if (file.exists(l)) tryCatch(suppressWarnings(readLines(l, warn = FALSE)),
                                 error = function(e) character())
  }))
  r <- do.call(rbind, lapply(csvs[file.exists(csvs)], utils::read.csv,
                             colClasses = "character", na.strings = character()))
  list(ok = all(file.exists(csvs)), r = r, out = out)
}

# A child R must find tflspec where this R does: skips the test when not.
tsr_skip_without_child <- function(env = parent.frame()) {
  rscript <- file.path(R.home("bin"), "Rscript")
  withr::local_envvar(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep), .local_envir = env)
  has <- suppressWarnings(system2(rscript, c("-e", shQuote("cat(requireNamespace('tflspec', quietly = TRUE))")),
                                  stdout = TRUE, stderr = FALSE))
  skip_if_not(identical(utils::tail(has, 1L), "TRUE"), "tflspec is not installed where a child R looks")
}
