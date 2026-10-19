# The AI table guide's step 3 in one call: the study folder, the ADaM
# summary, the TOC / SHELL workbook read, the requests.

#' Prepare a study's AI table requests in one call
#'
#' The steps before the first request, one after the other:
#' [tfl_ai_study_init()] (only when the study has no `study.yml` yet -- one
#' that is there is kept, so the call can be run again),
#' [tfl_ai_adam_summary()] (to `ai/adam_summary.md`),
#' [tfl_ai_read_shell()] (to `ai/toc_shell.md`; its summary is printed,
#' with what to pass when no TOC or a report's shell was found) and
#' [tfl_ai_write_requests()].  Nothing is sent anywhere.
#'
#' @param dir The study folder.
#' @param toc The TOC / SHELL workbook (`.xlsx` or `.xlsm`).
#' @param adam The ADaM folder, from `dir` (relative) or absolute.
#' @param study_id,title The study's id and title, for a new study folder.
#' @param toc_sheet,sheets As [tfl_ai_read_shell()]: the TOC sheet when the
#'   guess is wrong, the sheets to keep.
#' @param size,language,workbook,instructions,max_chars As
#'   [tfl_ai_write_requests()]: the reports a request holds, the language of
#'   the assumptions, how the workbook reaches the chat (`"text"`,
#'   `"attach"`, `"both"`), the instructions in each request or once, a
#'   request's length at most.
#' @param overwrite `TRUE` to start the study folder again
#'   ([tfl_ai_study_init()] over its `study.yml` and `spec/`).  The requests
#'   are written again on every call (the answers are kept).
#' @return The requests, as [tfl_ai_write_requests()] gives them.
#' @seealso The AI table guide: `system.file("ai", "tflspec-ai-table-guide.md",
#'   package = "tflspec")`.
#' @examples
#' ex <- system.file("ai", "example", package = "tflspec")
#' d <- file.path(tempdir(), "prepare-study")
#' dir.create(file.path(d, "adam"), recursive = TRUE, showWarnings = FALSE)
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   for (x in c("adsl", "adae", "adrs", "adlb")) {
#'     df <- getExportedValue("tflspec", x)
#'     for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
#'     haven::write_xpt(df, file.path(d, "adam", paste0(x, ".xpt")))
#'   }
#'   r <- tfl_ai_prepare(d, file.path(ex, "onco_toc_shell.xlsx"),
#'                       study_id = "XYZ-ONC-01", overwrite = TRUE)
#'   r[c("request", "outputs", "chars")]
#' }
#' @export
tfl_ai_prepare <- function(dir, toc, adam = "adam", study_id = "STUDY-01", title = NULL,
                           toc_sheet = NULL, sheets = NULL, size = 3L, language = NULL,
                           workbook = c("text", "attach", "both"),
                           instructions = c("each", "once"), max_chars = NULL,
                           overwrite = FALSE) {
  fn <- "tfl_ai_prepare()"
  workbook <- match.arg(workbook)
  instructions <- match.arg(instructions)
  if (!is.character(toc) || length(toc) != 1L || !file.exists(toc)) {
    .ard_stop(sprintf("%s: `toc` is the TOC / SHELL workbook's path (.xlsx).", fn))
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(file.path(dir, "study.yml")) || isTRUE(overwrite)) {
    tfl_ai_study_init(dir, adam = adam, study_id = study_id, title = title,
                      overwrite = overwrite)
  }
  ai <- file.path(dir, "ai")
  dir.create(ai, recursive = TRUE, showWarnings = FALSE)
  adam_dir <- if (grepl("^(/|[A-Za-z]:)", adam)) adam else file.path(dir, adam)
  ad <- tfl_ai_adam_summary(adam_dir, file = file.path(ai, "adam_summary.md"))
  shell <- do.call(tfl_ai_read_shell, c(list(toc, file = file.path(ai, "toc_shell.md")),
                                        Filter(Negate(is.null), list(toc_sheet = toc_sheet,
                                                                     sheets = sheets))))
  print(shell)
  tfl_ai_write_requests(dir, shell, ad, size = size, language = language,
                        instructions = instructions, max_chars = max_chars,
                        workbook = workbook, overwrite = TRUE)
}
