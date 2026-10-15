# ============================================================================
#  tflspec_ai_manual() -- hand the AI briefing to the user
# ============================================================================
#
#  The manual ships in `inst/ai/` so that it is the same artefact as the
#  code it describes: a user is on whichever release they installed, and a
#  manual read against another version is worse than none -- an assistant
#  quotes a function that will not resolve with exactly the confidence it
#  quotes one that will.  (rtfreporter does the same: rtfreporter_ai_manual().)

#' The AI assistant manual that ships with this package
#'
#' tflspec is too new to be in any chat model's training data: asked for a
#' table specification or the program made from one, an assistant invents
#' sheet columns and function names and flags neither as a guess.  The fix
#' is to give it the facts first, and this file is those facts -- the
#' specification formats (ARD, table, report, listing, figure design), the
#' functions that turn them into R code, what that code looks like, the
#' former names that are refused, and the complete export list -- sized to
#' sit in one chat session.
#'
#' `tflspec_ai_manual()` returns the path to the copy **installed with this
#' package**, so the manual you attach always describes the version you
#' actually have.  For the rendering side attach rtfreporter's own manual
#' instead (`rtfreporter::rtfreporter_ai_manual()`).
#'
#' With `dir`, it writes everything a chat needs to draft a study's specs
#' from the company's TOC and shells into one flat folder -- seven files
#' (eight with the ADaM metadata), under the ten files most chats take:
#'
#' * `00_START_HERE.md` -- the request to paste, and what to attach;
#' * `01_tflspec-ai-user-manual.md` -- this manual;
#' * `02_tflspec-ai-toc-shell-guide.md` -- how to read a TOC and shells
#'   into the spec files, the checklist the assistant runs, the assumptions
#'   it must list;
#' * `03_spec_schema.yml` -- the installed `inst/spec/schema.yml`: every
#'   function a spec item calls, its arguments and their types
#'   ([tfl_schema()]), and the spec sheets' columns ([tfl_spec_columns()]);
#' * `04_blank_spec.md` -- every file of a study's YAML spec, blank, its
#'   keys said in comments, in the form of the answer;
#' * `05_example_toc_shell.xlsx`, `06_example_spec.md` -- a made-up study
#'   end to end: its TOC and four shells (a demographics table, an AE table
#'   by SOC and PT, a listing of serious AEs, a figure of mean blood
#'   pressure), and the spec files written from them, with the
#'   `spec/_assumptions.yml` the guide asks for.  The example is written now
#'   through the one-time conversion ([tfl_yaml_from_sheets()]), so it is
#'   this version's.
#'
#' The assistant answers with ONE text holding every file, each a heading
#' with its path and a `yaml` block; [tfl_ai_unpack()] writes it into a
#' study folder and [tfl_ai_review()] checks the folder.
#'
#' With `define`, `adam_spec` or `datasets` it also writes
#' `07_adam_metadata.csv`, the study's ADaM metadata ([tfl_adam_metadata()]:
#' one row a variable, its label, kind and levels; never the data), and the
#' request tells the assistant to take every dataset, variable and level
#' from it.  Give one; with several, the first of `define`, `adam_spec`,
#' `datasets` is used.
#'
#' @param dir Optional folder.  When given, the kit is written there (the
#'   folder is made if need be) and the paths of its files are returned
#'   invisibly; the files, their sizes and what to attach are printed.
#' @param overwrite Write over a kit already in `dir`.  Default `FALSE`.
#' @param define,adam_spec,datasets With `dir`: the study's ADaM metadata,
#'   from its `define.xml`, its ADaM spec workbook or the datasets'
#'   column information (see [tfl_adam_metadata()]); written as
#'   `07_adam_metadata.csv`.
#'
#' @return The path to the manual when `dir` is `NULL`; otherwise the kit's
#'   files, invisibly.
#'
#' @examples
#' # where the manual lives
#' tflspec_ai_manual()
#' writeLines(head(readLines(tflspec_ai_manual()), 3))
#'
#' # the kit to attach to a chat
#' \donttest{
#' kit <- tflspec_ai_manual(file.path(tempdir(), "ai-kit"))
#' basename(kit)
#' }
#' @seealso [tfl_ai_review()] for the files the assistant returns.
#' @export
tflspec_ai_manual <- function(dir = NULL, overwrite = FALSE,
                              define = NULL, adam_spec = NULL,
                              datasets = NULL) {
  name <- "tflspec-ai-user-manual.md"
  src  <- system.file("ai", name, package = "tflspec")
  if (!nzchar(src) || !file.exists(src)) {
    stop(sprintf(paste0(
      "`tflspec_ai_manual()`: %s is not in this installation.\n",
      "  It ships in inst/ai/ from tflspec 0.0.24 on: reinstall."), name),
      call. = FALSE)
  }
  if (is.null(dir)) return(src)
  if (file.exists(dir) && !dir.exists(dir)) {
    stop(sprintf(paste0(
      "`tflspec_ai_manual()`: '%s' is a file; give a folder (the kit is ",
      "several files)."), dir), call. = FALSE)
  }
  meta <- if (!is.null(define) || !is.null(adam_spec) || !is.null(datasets)) {
    tfl_adam_metadata(define = define, adam_spec = adam_spec,
                      datasets = datasets)
  }
  .ai_write_kit(dir, overwrite = isTRUE(overwrite), meta = meta)
}
