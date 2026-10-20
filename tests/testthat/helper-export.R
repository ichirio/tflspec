# A report's program without tflspec (R/export_program.R), run where tflspec
# is not: a child R that sees this session's packages but tflspec, and no
# user library of its own.

# The child's libraries: this session's, behind one whose tflspec (and
# tflplanner) is no package -- a folder with a DESCRIPTION and nothing
# built, so loading it fails and requireNamespace() says FALSE.  (Nothing
# is copied or linked: the session's packages are the child's.)
export_lib <- local({
  lib <- NULL
  function() {
    if (!is.null(lib) && dir.exists(lib)) return(lib)
    lib <<- tempfile("lib-no-tflspec")
    for (p in c("tflspec", "tflplanner")) {
      dir.create(file.path(lib, p), recursive = TRUE)
      writeLines(c(paste("Package:", p), "Version: 0.0.0",
                   "Title: Not installed", "Description: Hides the installed one."),
                 file.path(lib, p, "DESCRIPTION"))
    }
    lib
  }
})
export_libs <- function() {
  paste(c(export_lib(), setdiff(normalizePath(.libPaths(), "/"), normalizePath(.Library, "/"))),
        collapse = .Platform$path.sep)
}

# Run `code` in that child, from the folder `wd`; its output (stdout and
# stderr) is returned, `status` its exit status
export_run <- function(code, wd) {
  f <- tempfile(fileext = ".R")
  on.exit(unlink(f))
  writeLines(c(
    "if (requireNamespace(\"tflspec\", quietly = TRUE)) stop(\"tflspec is installed here\")",
    "cat(\"tflspec: no\\n\")",
    code), f, useBytes = TRUE)
  empty <- tempfile("empty-lib")
  dir.create(empty)
  old <- setwd(wd)
  on.exit(setwd(old), add = TRUE)
  # (system2()'s env does not reach the child on Windows: set them here)
  keep <- Sys.getenv(c("R_LIBS", "R_LIBS_USER", "R_LIBS_SITE"), unset = NA)
  on.exit(for (k in names(keep)) {
    if (is.na(keep[[k]])) Sys.unsetenv(k) else do.call(Sys.setenv, as.list(keep[k]))
  }, add = TRUE)
  Sys.setenv(R_LIBS = export_libs(), R_LIBS_USER = empty, R_LIBS_SITE = empty)
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                  c("--no-init-file", shQuote(f)),
                                  stdout = TRUE, stderr = TRUE))
  structure(out, status = attr(out, "status") %||% 0L)
}

# The functions a program calls (its calls' names), those it defines left out
export_calls <- function(code) {
  pd <- utils::getParseData(parse(text = code, keep.source = TRUE))
  calls <- unique(pd$text[pd$token == "SYMBOL_FUNCTION_CALL"])
  e <- parse(text = code, keep.source = FALSE)
  defined <- unlist(lapply(e, function(x) {
    if (is.call(x) && as.character(x[[1L]]) %in% c("<-", "=") && is.name(x[[2L]]) &&
        is.call(x[[3L]]) && identical(x[[3L]][[1L]], as.name("function"))) {
      as.character(x[[2L]])
    }
  }))
  setdiff(calls, defined)
}
