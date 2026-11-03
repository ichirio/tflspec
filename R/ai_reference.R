# ============================================================================
#  The company's reference pack for AI agents (tflspec#547, step 3)
# ----------------------------------------------------------------------------
#  The company standard's conventions, by kind of report -- those it had and
#  those adopted from past TFLs (tfl_ai_style_diff(), tfl_ai_style_adopt())
#  -- written for an agent (tfl_ai_reference_build(): <pack>/conventions.md).
#  Every study's agent files carry a copy (ai/reference/, copied again when
#  the pack changed), and AGENTS.md's second step reads it before the usual
#  convention: an open point it settles is `from: "company convention"`.
#  The pack is the app's (tflplanner's home); tflspec's own data folder when
#  no app says where (option tflspec.ai_reference).
# ============================================================================

.ai_reference_dir <- function() {
  getOption("tflspec.ai_reference") %||%
    file.path(tools::R_user_dir("tflspec", "data"), "ai", "reference")
}

#' Adopt conventions learned from past TFLs
#'
#' The company standard's conventions with the learned ones a person chose
#' from [tfl_ai_style_diff()]: each chosen row's learned value takes the
#' standard's place (or is added), the rest stay as they were.  What becomes
#' of the result -- the company standard's store -- is the app's.
#'
#' @param diff What [tfl_ai_style_diff()] returns.
#' @param rows The rows of `diff` to adopt: numbers, or a logical vector;
#'   `NULL` (default) every row whose `adopt` is `"proposed"` and whose
#'   `status` is `"differs"` or `"new"`.
#' @param standard The company standard's conventions (`kind`, `key`,
#'   `value`), as given to [tfl_ai_style_diff()].
#' @return The conventions: a data frame with `kind`, `key`, `value`,
#'   `from` (`"standard"`, or `"learned"` for one adopted now), ordered by
#'   kind and key.
#' @seealso [tfl_ai_reference_build()]
#' @examples
#' f <- tempfile(fileext = ".yml")
#' writeLines(c("tfl_style_learned: {version: 2, files: 3, tables: 9}",
#'              "kinds:",
#'              "  demog:",
#'              "    n_form: {value: \"(N=xx)\", seen: 3, of: 3}"), f)
#' std <- data.frame(kind = "all", key = "n_form", value = "N = xx")
#' tfl_ai_style_adopt(tfl_ai_style_diff(f, std), standard = std)
#' @export
tfl_ai_style_adopt <- function(diff, rows = NULL, standard = NULL) {
  need <- c("kind", "key", "learned", "adopt", "status")
  if (!is.data.frame(diff) || !all(need %in% names(diff))) {
    .ard_stop("tfl_ai_style_adopt(): `diff` is what tfl_ai_style_diff() returns.")
  }
  std <- .ai_conventions_frame(standard, "tfl_ai_style_adopt()")
  std$from <- rep("standard", nrow(std))
  if (is.null(rows)) {
    rows <- which(diff$adopt %in% "proposed" & diff$status %in% c("differs", "new"))
  } else if (is.logical(rows)) {
    rows <- which(rows)
  }
  if (!is.numeric(rows) || anyNA(rows) || any(rows < 1L | rows > nrow(diff))) {
    .ard_stop("tfl_ai_style_adopt(): `rows` are rows of `diff` (numbers or TRUE / FALSE).")
  }
  for (i in rows) {
    if (is.na(diff$learned[i])) next
    at <- which(std$kind == diff$kind[i] & std$key == diff$key[i])
    if (length(at)) {
      std$value[at] <- diff$learned[i]
      std$from[at] <- "learned"
    } else {
      std <- rbind(std, data.frame(kind = diff$kind[i], key = diff$key[i], value = diff$learned[i],
                                   from = "learned", stringsAsFactors = FALSE))
    }
  }
  .ai_conventions_order(std)
}

#' Write the company's reference pack for AI agents
#'
#' Writes the company's conventions, by kind of report, as an AI agent
#' reads them (`conventions.md`, with `README.md`).  Every study prepared
#' for an agent afterwards ([tfl_ai_prepare()]) gets a copy in its
#' `ai/reference/`, and its agent takes a convention from it before the
#' usual one (`from: "company convention"`); a study prepared before gets
#' the new pack the next time its agent files are written.
#'
#' @param conventions The conventions: a data frame with `kind`, `key` and
#'   `value` (kind `all` for every kind), such as [tfl_ai_style_adopt()]
#'   returns.
#' @param dir The pack's folder; `NULL`: the option `tflspec.ai_reference`,
#'   else `ai/reference/` of tflspec's data folder ([tools::R_user_dir()]).
#' @param notes Lines the agent should read first (the company's own words),
#'   or `NULL`.
#' @return The files written, invisibly.
#' @seealso [tfl_ai_style_adopt()]
#' @examples
#' d <- tempfile()
#' conv <- data.frame(kind = c("all", "demog"), key = c("font", "n_form"),
#'                    value = c("Courier New 9 pt", "(N=xx)"))
#' f <- tfl_ai_reference_build(conv, dir = d)
#' cat(readLines(f[["conventions"]]), sep = "\n")
#' @export
tfl_ai_reference_build <- function(conventions, dir = NULL, notes = NULL) {
  conv <- .ai_conventions_order(.ai_conventions_frame(conventions, "tfl_ai_reference_build()"))
  dir <- dir %||% .ai_reference_dir()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  what <- stats::setNames(.ai_style_keys$what, .ai_style_keys$key)
  body <- unlist(lapply(unique(conv$kind), function(k) {
    d <- conv[conv$kind == k, , drop = FALSE]
    c(sprintf("## %s", if (k == "all") "Every kind" else paste0("`", k, "`")), "",
      sprintf("- `%s`: %s%s", d$key, d$value,
              ifelse(is.na(what[d$key]), "", paste0("  _(", what[d$key], ")_"))), "")
  }))
  text <- c(
    "# The company's conventions",
    "",
    "How this company makes each kind of report. Take a point from here when the study's documents do not settle it, before the usual convention, and write it as an assumption with `from: \"company convention\"`. A study's documents (the shell, the SAP, a decision) win over this file.",
    "",
    if (length(notes)) c(as.character(notes), ""),
    "A kind's own line wins over the same line under *Every kind*. The kinds: `demog` (demographics, baseline), `ae_soc_pt` (AEs by SOC and PT), `ae` (other AE tables), `param_change` (labs, vital signs, ECG by visit with change), `shift`, `listing`, `figure`, `other`.",
    "",
    body)
  f_conv <- file.path(dir, "conventions.md")
  .ai_write_text(text, f_conv)
  f_readme <- file.path(dir, "README.md")
  .ai_write_text(c(
    "# The company's reference pack for AI agents",
    "",
    "Written by `tflspec::tfl_ai_reference_build()` from the company standard; do not edit it here. Every study prepared for an agent gets a copy in `ai/reference/`.",
    "",
    "- `conventions.md`: the conventions by kind of report."), f_readme)
  invisible(c(conventions = f_conv, readme = f_readme))
}

# The conventions as a plain data frame of kind, key, value
.ai_conventions_frame <- function(x, fn) {
  if (is.null(x)) return(data.frame(kind = character(), key = character(), value = character(),
                                    stringsAsFactors = FALSE))
  if (!is.data.frame(x) || !all(c("kind", "key", "value") %in% names(x))) {
    .ard_stop(sprintf("%s: the conventions are a data frame with `kind`, `key` and `value`.", fn))
  }
  out <- data.frame(kind = as.character(x$kind), key = as.character(x$key),
                    value = as.character(x$value), stringsAsFactors = FALSE)
  if ("from" %in% names(x)) out$from <- as.character(x$from)
  out[!is.na(out$value) & nzchar(trimws(out$value)), , drop = FALSE]
}

# By kind (as the prompt lists them, others after), then by the keys' order
.ai_conventions_order <- function(x) {
  k <- match(x$kind, .ai_style_kinds)
  k[is.na(k)] <- length(.ai_style_kinds) + 1L
  y <- match(x$key, .ai_style_keys$key)
  y[is.na(y)] <- nrow(.ai_style_keys) + 1L
  x <- x[order(k, x$kind, y, x$key), , drop = FALSE]
  rownames(x) <- NULL
  x
}

# A study's copy of the pack (ai/reference/): copied when the pack has a
# file the copy lacks or that differs; NULL when there is no pack.  The
# files written.
.ai_reference_copy <- function(study_dir) {
  src <- .ai_reference_dir()
  fs <- if (dir.exists(src)) list.files(src, "[.]md$") else character()
  if (!length(fs)) return(NULL)
  to <- file.path(study_dir, "ai", "reference")
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  out <- character()
  for (f in fs) {
    a <- file.path(src, f)
    b <- file.path(to, f)
    if (!file.exists(b) || !identical(unname(tools::md5sum(a)), unname(tools::md5sum(b)))) {
      file.copy(a, b, overwrite = TRUE)
      out <- c(out, b)
    }
  }
  out
}
