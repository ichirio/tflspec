# The study's sheets, read once while its files stay as they are.
#
# tfl_sheets_from_yaml() (the bridge from the layers to the sheets) is the slow
# part of .ard_spec_of_study(), and a caller that goes report by report
# (tfl_data_code() for each report of a study: the program check of a save)
# used to run it again for each one.  The sheets are kept here for the last
# few studies, under a key of everything the read depends on:
#   - the study folder, its normalised path;
#   - every file the read opens: study.yml and each .yml/.yaml/.R file under
#     spec/ (defaults, data, toc, patterns, styles, templates, the reports),
#     the reports' folder too when `tflspec.outputs_dir` puts it elsewhere --
#     the names and the md5 of each (a file changed to the same size and the
#     same modification time is a miss);
#   - the options that change what is read (`tflspec.*`, but not the ones
#     only the generated code reads or a read sets for a moment);
#   - the templates registered by tfl_template_variant() (the study's own,
#     or the session's).
# Not in the key: a file a template's R code reads by itself, and the
# installed packages (the memo lives in one session).  An error is not kept.
# A hit does not repeat the warnings the read gave.

.sheets_memo <- new.env(parent = emptyenv())
.sheets_memo$entries <- list()
.sheets_memo$runs <- 0L
.sheets_memo_keep <- 3L

# options a read of the sheets does not depend on (the code's, or set for a
# moment while a template file is read)
.sheets_memo_skip <- c("tflspec.paths", "tflspec.attached", "tflspec.plain_ns",
                       "tflspec.user_attached", "tflspec.template_source")

.sheets_memo_key <- function(dir) {
  dir <- normalizePath(dir, "/", mustWork = FALSE)
  spec <- normalizePath(.spec_dir(dir), "/", mustWork = FALSE)
  outs <- normalizePath(.outputs_dir(dir), "/", mustWork = FALSE)
  lf <- function(d) {
    list.files(d, "[.](ya?ml|[Rr])$", recursive = TRUE, full.names = TRUE)
  }
  files <- c(file.path(dir, "study.yml"), lf(spec))
  if (!startsWith(outs, paste0(spec, "/"))) files <- c(files, lf(outs))
  files <- sort(unique(normalizePath(files, "/", mustWork = FALSE)))
  op <- options()
  op <- op[grepl("^tflspec[.]", names(op)) & !names(op) %in% .sheets_memo_skip]
  op <- op[order(names(op))]
  variants <- Filter(function(e) !is.null(e$from), as.list(.tpl_registry, sorted = TRUE))
  list(dir = dir, outs = outs, files = files,
       md5 = unname(tools::md5sum(files)),
       options = rlang::hash(op), variants = rlang::hash(variants))
}

# The study's sheets (tfl_sheets_from_yaml(dir)), from the memo when the key
# is the same.
.sheets_of_study <- function(dir) {
  key <- .sheets_memo_key(dir)
  for (e in .sheets_memo$entries) {
    if (identical(e$key, key)) return(e$sheets)
  }
  sheets <- tfl_sheets_from_yaml(dir)
  .sheets_memo$runs <- .sheets_memo$runs + 1L
  # (the same folder once, the last few folders)
  old <- Filter(function(e) !identical(e$key$dir, key$dir), .sheets_memo$entries)
  .sheets_memo$entries <- utils::head(c(list(list(key = key, sheets = sheets)), old),
                                      .sheets_memo_keep)
  sheets
}

.sheets_memo_clear <- function() {
  .sheets_memo$entries <- list()
  invisible()
}
