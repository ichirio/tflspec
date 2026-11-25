# A study's folder layout and the files of its programs (tflspec#910, G5-1a).
#
# The layout is where each kind of file goes in a study folder, relative to
# it.  The generated programs name the folders through the variables
# programs/study_setup.R defines (`path_` and a key: path_adam, path_ard
# ...), so a study can be moved, copied or zipped and still run.  It is a
# default: a caller may give another one (a company's own folders) to the
# functions that take `layout`, which never read it from the session.

#' The folder layout of a study
#'
#' The folders of a study, relative to the study folder: its input data,
#' its SPEC, its programs and what they make.  This is the default layout;
#' a function that takes a `layout` takes another one with the same keys (a
#' company's own folders).  The study's setup program defines a variable
#' for each folder, `path_` and its key (`path_adam`), and the generated
#' programs name the folders through them.
#'
#' @return A named character vector of folders, relative to the study
#'   folder: `adam`, `sdtm` and `other` (the input data), `spec`,
#'   `programs_ard`, `programs_tfl`, `ard` (the study's working ARD), `tfl`
#'   (the working reports), `ard_import` (ARDs made elsewhere), `toc_import`,
#'   `runs` (the official runs), `logs_preview` and `docs`.
#' @seealso [tfl_study_program_files()]
#' @examples
#' tfl_study_layout()["adam"]
#' names(tfl_study_layout())
#' @export
tfl_study_layout <- function() {
  c(adam = "data/adam", sdtm = "data/sdtm", other = "data/other",
    spec = "spec", programs_ard = "programs/ard",
    programs_tfl = "programs/tfl", ard = "output/ard", tfl = "output/tfl",
    ard_import = "input/ard", toc_import = "input/toc", runs = "runs",
    logs_preview = "logs/preview", docs = "docs")
}

# `layout` checked: a named character vector with `keys`
.layout_check <- function(layout, keys, fn) {
  if (!is.character(layout) || is.null(names(layout)) ||
      !all(keys %in% names(layout))) {
    stop(fn, ": `layout` is a named character vector with ",
         paste(keys, collapse = ", "), " (see tfl_study_layout()).", call. = FALSE)
  }
  layout
}

# An output id's ARD program: the id, with any character but a letter, a
# digit, `.`, `_` or `-` as `_`, and `.R`
.ard_program_name <- function(output_id) {
  if (!length(output_id)) return(character())
  paste0(gsub("[^A-Za-z0-9._-]", "_", output_id), ".R")
}

#' The files of a study's programs
#'
#' Where each report's program, its RTF file and its ARD program go, from
#' values alone (no file is read), and what is wrong with their names: a
#' program named like a file the generator writes itself, two reports with
#' one program, an output id whose ARD program is a generated file's, two
#' output ids with one ARD program.  Names are compared in lower case
#' (Windows and macOS keep one file for `T1.R` and `t1.R`).  A report's
#' program is in the layout's `programs_tfl` and an ARD program in its
#' `programs_ard`: each is compared with its own folder's, and both with
#' every name in `reserved` (one rule for every folder of the programs).
#'
#' @param programs A named character vector: a report's output id (the
#'   name) to its program's file name (`"t_14_1_1.R"`).  `NA` for a report
#'   whose program is not known (its name is not checked; its ARD program's
#'   is).  A report without an id (`NA` or `""`) is not checked.
#' @param rtf The reports' RTF files, named by output id the same way
#'   (`NULL`: not given).
#' @param reserved The file names the generator writes itself in the
#'   programs' folders (`study_setup.R`, `batch.R` ...).
#' @param layout The study's folders ([tfl_study_layout()]): `programs_tfl`
#'   and `programs_ard`.
#' @return A data frame, a row a report in the order of `programs`:
#'   `output_id`, `program` (relative to the study folder), `rtf` and
#'   `ard_program`.  Its attribute `problems` is a list, one element a
#'   problem in the order found: `id` (the report), `kind` (`"reserved"`,
#'   `"duplicate"`, `"ard_reserved"` or `"ard_duplicate"`) and `args`, the
#'   values a message names (the id and the name, or the first id, the
#'   second and the name).
#' @examples
#' f <- tfl_study_program_files(c(T1 = "t1.R", T2 = "T1.R", T3 = "batch.R"),
#'                              reserved = "batch.R")
#' f
#' vapply(attr(f, "problems"), function(x) x$kind, "")
#' @keywords internal
#' @export
tfl_study_program_files <- function(programs, rtf = NULL, reserved = character(),
                                    layout = tfl_study_layout()) {
  fn <- "tfl_study_program_files()"
  if (!(is.character(programs) || all(is.na(programs))) ||
      (length(programs) && is.null(names(programs)))) {
    stop(fn, ": `programs` is a character vector named by output id.", call. = FALSE)
  }
  layout <- .layout_check(layout, c("programs_tfl", "programs_ard"), fn)
  ids <- names(programs) %||% character()
  prog <- unname(as.character(programs))
  rtf <- if (is.null(rtf)) rep(NA_character_, length(ids)) else
    unname(as.character(rtf)[match(ids, names(rtf))])
  path <- file.path(layout[["programs_tfl"]], prog)
  path[is.na(prog)] <- NA_character_
  out <- data.frame(
    output_id = ids,
    program = path,
    rtf = rtf,
    ard_program = file.path(layout[["programs_ard"]], .ard_program_name(ids)),
    stringsAsFactors = FALSE)
  problems <- list()
  add <- function(id, kind, args) {
    problems[[length(problems) + 1L]] <<- list(id = id, kind = kind,
                                               args = as.character(args))
  }
  taken <- tolower(as.character(reserved))
  check <- function(ids, names, kind) {
    low <- tolower(names)
    for (i in seq_along(ids)) {
      if (is.na(names[i])) next
      if (low[i] %in% taken) add(ids[i], kind[1L], c(ids[i], names[i]))
      j <- match(low[i], low[seq_len(i - 1L)])
      if (!is.na(j)) add(ids[i], kind[2L], c(ids[j], ids[i], names[i]))
    }
  }
  keep <- !is.na(ids) & nzchar(ids)
  check(ids[keep], prog[keep], c("reserved", "duplicate"))
  ids <- unique(ids[keep])
  check(ids, .ard_program_name(ids), c("ard_reserved", "ard_duplicate"))
  attr(out, "problems") <- problems
  out
}
