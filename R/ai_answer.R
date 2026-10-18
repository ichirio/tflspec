# ============================================================================
#  The AI's answer: a study's spec files in one Markdown text
# ============================================================================
#
#  A chat returns text more reliably than files, and several files least of
#  all.  So the kit asks for ONE answer that holds every file of the study's
#  YAML spec, each as a heading with its path and a ```yaml block:
#
#    ### spec/outputs/T-1.yml
#
#    ```yaml
#    id: T-1
#    ...
#    ```
#
#  .ai_answer_md() writes a folder in that form (the kit's example and blank
#  template); tfl_ai_unpack() writes the files of an answer back into a
#  study folder.

# A text file a person saved (an AI's answer, a report file edited by
# hand) as its lines: UTF-8 with or without the byte-order mark Windows
# Notepad writes, any line ends (CRLF, LF, CR).  Notepad's "Unicode"
# (UTF-16) is read too.  Text in another encoding -- "ANSI", Shift-JIS /
# CP932 on a Japanese PC -- is an error that says how to save it again.
.ai_read_lines <- function(f, fn) {
  raw <- readBin(f, "raw", n = max(file.info(f)$size, 0, na.rm = TRUE))
  bom <- function(...) {
    b <- as.raw(c(...))
    length(raw) >= length(b) && identical(raw[seq_along(b)], b)
  }
  again <- paste0(
    " -- save it again as UTF-8 (Notepad: File > Save As, Encoding: UTF-8; ",
    "\u30e1\u30e2\u5e33: \u540d\u524d\u3092\u4ed8\u3051\u3066\u4fdd\u5b58 ",
    "\u2192 \u6587\u5b57\u30b3\u30fc\u30c9 UTF-8), then run %s again.")
  if (bom(0xff, 0xfe) || bom(0xfe, 0xff)) {
    x <- iconv(list(raw[-(1:2)]), if (bom(0xff, 0xfe)) "UTF-16LE" else "UTF-16BE", "UTF-8")
  } else {
    if (bom(0xef, 0xbb, 0xbf)) raw <- raw[-(1:3)]
    if (any(raw == as.raw(0L))) {
      x <- NA_character_
    } else {
      x <- rawToChar(raw)
      Encoding(x) <- "UTF-8"
      if (!validUTF8(x)) {
        .ard_stop(sprintf(paste0("%s: %s is not UTF-8 (it looks like Shift-JIS / CP932, ",
                                 "Notepad's \"ANSI\")", again),
                          fn, sQuote(f, FALSE), fn))
      }
    }
  }
  if (is.na(x)) {
    .ard_stop(sprintf(paste0("%s: %s is not text in UTF-8", again), fn, sQuote(f, FALSE), fn))
  }
  x <- gsub("\r\n?", "\n", x)
  if (!nzchar(x)) return(character())
  strsplit(x, "\n", fixed = TRUE)[[1L]]
}

# The files an answer may hold: study.yml at the top, the spec's .yml files
# under spec/.  Anything else (another folder, another kind of file, a path
# out of the study) is not a spec file.
.ai_answer_path_ok <- function(path) {
  p <- gsub("\\\\", "/", path)
  if (!nzchar(p) || grepl("^/|^[A-Za-z]:", p)) return(FALSE)
  parts <- strsplit(p, "/", fixed = TRUE)[[1L]]
  if (any(parts %in% c("", ".", ".."))) return(FALSE)
  if (identical(p, "study.yml")) return(TRUE)
  identical(parts[1L], "spec") && length(parts) >= 2L && grepl("[.]ya?ml$", p)
}

# The files of a folder, in the answer's form: the folder's spec files in a
# reading order -- the study, its data, its defaults, the TOC, the patterns,
# the reports, the assumptions -- each a heading and a yaml block.
.ai_answer_md <- function(dir, files = NULL) {
  if (is.null(files)) {
    files <- list.files(dir, pattern = "[.]ya?ml$", recursive = TRUE)
    files <- files[vapply(files, .ai_answer_path_ok, NA)]
    files <- setdiff(files, "spec/_not_converted.yml")
  }
  rank <- function(f) {
    if (identical(f, "study.yml")) return(1L)
    if (identical(f, "spec/data.yml")) return(2L)
    if (identical(f, "spec/defaults.yml")) return(3L)
    if (identical(f, "spec/toc.yml")) return(4L)
    if (startsWith(f, "spec/patterns/")) return(5L)
    if (startsWith(f, "spec/outputs/")) return(6L)
    7L
  }
  files <- files[order(vapply(files, rank, 1L), files)]
  unlist(lapply(files, function(f) {
    txt <- readLines(file.path(dir, f), warn = FALSE, encoding = "UTF-8")
    while (length(txt) && !nzchar(trimws(txt[length(txt)]))) txt <- txt[-length(txt)]
    c(paste("###", f), "", "```yaml", txt, "```", "")
  }), use.names = FALSE)
}

#' Write an AI assistant's answer into a study's spec files
#'
#' The AI kit ([tflspec_ai_manual()]) asks the assistant for one answer
#' holding every file of the study's YAML spec: each file a heading with its
#' path (`### spec/outputs/T-1.yml`) and its content in a ```` ```yaml ````
#' block, the way the kit's example (`06_example_spec.md`) is written.
#' `tfl_ai_unpack()` writes those files into a study folder, ready for
#' [tfl_ai_review()] and the study's other functions
#' ([tfl_read_study_spec()], [tfl_read_outputs()]).
#'
#' The answer to a `table` prompt ([tfl_ai_prompt()]) -- one block, its
#' `outputs:` the report files of a batch -- is written too: give the
#' answer read by [tfl_ai_parse()] (or its text).  Each report becomes
#' `spec/outputs/<id>.yml`, written as [tfl_write_output()] writes one,
#' and its assumptions join `spec/_assumptions.yml` (the items of these
#' reports replaced, the others kept), so the batches of a study add up in
#' one folder.  Check the answer first ([tfl_ai_check()]): a report that
#' could not be read is not written.
#'
#' Only spec files are written: `study.yml`, and `.yml` files under `spec/`
#' (`spec/data.yml`, `spec/toc.yml`, `spec/defaults.yml`,
#' `spec/patterns/<name>.yml`, `spec/outputs/<id>.yml`,
#' `spec/_assumptions.yml`).  A block under any other heading, a path that
#' leaves the folder, or a file twice is an error that names it, and nothing
#' is written.  Each file is checked as YAML before any is written.
#'
#' @param answer The answer: its text (a character vector, one element a
#'   line or the whole text), the path of a `.md` / `.txt` file holding it,
#'   or a `table` answer read by [tfl_ai_parse()].
#' @param dir The study folder to write into.
#' @param overwrite Write over files already there?
#' @return Invisibly, the paths written.
#' @examples
#' answer <- c("### spec/outputs/T-1.yml", "", "```yaml", "id: T-1", "kind: table",
#'             "content:", "  - table_plan: {cols: TRT01A}", "```")
#' d <- tempfile("study-")
#' tfl_ai_unpack(answer, d)
#' tfl_read_output(file.path(d, "spec", "outputs", "T-1.yml"))
#' @seealso [tflspec_ai_manual()], [tfl_ai_review()]
#' @export
tfl_ai_unpack <- function(answer, dir, overwrite = FALSE) {
  if (inherits(answer, "tfl_ai_answer")) {
    if (!identical(answer$task, "table")) {
      .ard_stop(sprintf(paste0("tfl_ai_unpack(): a %s answer is not files; apply it as ",
                               "the task's own functions say (tfl_ai_diff())."), answer$task))
    }
    return(.ai_unpack_table(answer, dir, overwrite))
  }
  if (!is.character(answer) || !length(answer)) {
    .ard_stop("tfl_ai_unpack(): `answer` is the answer's text, or the path of a file holding it.")
  }
  if (length(answer) == 1L && !grepl("\n", answer, fixed = TRUE) && file.exists(answer)) {
    answer <- .ai_read_lines(answer, "tfl_ai_unpack()")
  }
  x <- unlist(strsplit(paste(answer, collapse = "\n"), "\n", fixed = TRUE))
  blocks <- .ai_answer_blocks(x)
  # a table answer (one block, the tflspec_ai header, `outputs:`)
  if (!length(blocks) && any(grepl("^\\s*tflspec_ai:.*\\btable\\b", x))) {
    return(.ai_unpack_table(tfl_ai_parse(x, "table"), dir, overwrite))
  }
  if (!length(blocks)) {
    .ard_stop(paste0("tfl_ai_unpack(): the answer holds no file -- each is a heading with its ",
                     "path (### spec/outputs/T-1.yml) and a ```yaml block under it."))
  }
  paths <- names(blocks)
  bad <- paths[!vapply(paths, .ai_answer_path_ok, NA)]
  if (length(bad)) {
    .ard_stop(sprintf(paste0("tfl_ai_unpack(): %s is not a spec file of the study: the answer ",
                             "writes study.yml and .yml files under spec/."),
                      paste(sQuote(bad, FALSE), collapse = ", ")))
  }
  dup <- unique(paths[duplicated(paths)])
  if (length(dup)) {
    .ard_stop(sprintf("tfl_ai_unpack(): %s is in the answer twice.",
                      paste(sQuote(dup, FALSE), collapse = ", ")))
  }
  for (p in paths) {
    msg <- tryCatch({
      yaml::yaml.load(paste(blocks[[p]], collapse = "\n"), handlers = .yaml_handlers())
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(msg)) .ard_stop(sprintf("tfl_ai_unpack(): %s is not valid YAML: %s", p, msg))
  }
  out <- file.path(dir, paths)
  there <- out[file.exists(out)]
  if (length(there) && !isTRUE(overwrite)) {
    .ard_stop(sprintf("tfl_ai_unpack(): %s already there; pass `overwrite = TRUE`.",
                      paste(sQuote(paths[file.exists(out)], FALSE), collapse = ", ")))
  }
  for (i in seq_along(paths)) {
    dir.create(dirname(out[i]), showWarnings = FALSE, recursive = TRUE)
    .ai_write_text(blocks[[i]], out[i])
  }
  invisible(normalizePath(out, winslash = "/"))
}

# The answer's files: a heading naming a path, then the first fenced block
# under it (```yaml, ```yml or a bare ```), its lines as they are.  A
# heading with no block under it before the next heading is no file.
.ai_answer_blocks <- function(x) {
  out <- list()
  i <- 1L
  n <- length(x)
  while (i <= n) {
    h <- regmatches(x[i], regexec("^#{1,6}\\s+`?([^`]+?)`?\\s*$", x[i]))[[1L]]
    if (length(h) && grepl("[.]ya?ml$", h[2L])) {
      path <- trimws(h[2L])
      j <- i + 1L
      while (j <= n && !grepl("^```", x[j]) && !grepl("^#{1,6}\\s", x[j])) j <- j + 1L
      if (j <= n && grepl("^```", x[j])) {
        k <- j + 1L
        while (k <= n && !grepl("^```\\s*$", x[k])) k <- k + 1L
        # (appended, not by name: a file twice is the caller's to see)
        out[[length(out) + 1L]] <- if (k > j + 1L) x[(j + 1L):(k - 1L)] else character()
        names(out)[length(out)] <- path
        i <- k + 1L
        next
      }
    }
    i <- i + 1L
  }
  out
}
