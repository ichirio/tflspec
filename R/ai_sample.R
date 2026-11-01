# ============================================================================
#  The sample study for trying the AI flow: tfl_ai_sample_study(dir)
# ============================================================================
#
#  A tester needs a study to hand to the agent: ADaM, a TOC, annotated
#  shells, an ADaM spec and a SAP.  Everything here is invented (Example
#  Pharma, study DMY-001).  The ADaM is made from tflspec's bundled sample
#  datasets (adsl, adae, adlb, advs, adtte): the arms become Placebo /
#  Drug A 10 mg / Drug A 20 mg and FASFL is added.  The documents are the
#  files in inst/ai-sample.  No answer -- no spec and no expected output --
#  is written: the tester's agent writes the specs.

.ai_sample_arms <- c("Placebo", "Drug A 10 mg", "Drug A 20 mg")
.ai_sample_armcd <- c("PBO", "DA10", "DA20")
.ai_sample_labels <- c(adsl = "Subject-Level Analysis Dataset",
                       adae = "Adverse Events Analysis Dataset",
                       adlb = "Laboratory Analysis Dataset",
                       advs = "Vital Signs Analysis Dataset",
                       adtte = "Time-to-Event Analysis Dataset")

# The sample ADaM: a named list of the five data frames
.ai_sample_adam <- function() {
  src <- new.env()
  utils::data(list = names(.ai_sample_labels), package = "tflspec", envir = src)
  arms <- .ai_sample_arms
  adsl0 <- src$adsl
  old_id <- as.character(adsl0$USUBJID)
  # Planned arm: Control -> Placebo; Drug X split 1:1 into 10 mg / 20 mg by
  # subject order.  A subject never dosed keeps no actual arm.
  planned_old <- as.character(adsl0$TRT01P)
  pl <- rep(NA_character_, length(old_id))
  pl[planned_old %in% "Control"] <- arms[1]
  dx <- which(planned_old %in% "Drug X")
  dx <- dx[order(old_id[dx])]
  pl[dx] <- ifelse(seq_along(dx) %% 2 == 1, arms[2], arms[3])
  map <- data.frame(old = old_id, USUBJID = sub("^XYZ-ONC-01-", "DMY-001-", old_id), P = pl,
                    A = ifelse(is.na(as.character(adsl0$TRT01A)), NA_character_, pl),
                    stringsAsFactors = FALSE)

  set_col <- function(df, v, value) {
    lb <- attr(df[[v]], "label")
    df[[v]] <- value
    attr(df[[v]], "label") <- lb
    df
  }
  relabel <- function(df) {
    i <- match(as.character(df$USUBJID), map$old)
    df <- set_col(df, "STUDYID", rep("DMY-001", nrow(df)))
    df <- set_col(df, "USUBJID", map$USUBJID[i])
    for (v in intersect(c("TRT01P", "ARM"), names(df))) df <- set_col(df, v, factor(map$P[i], levels = arms))
    for (v in intersect(c("TRT01A", "ACTARM"), names(df))) df <- set_col(df, v, factor(map$A[i], levels = arms))
    if ("TRT01PN" %in% names(df)) df <- set_col(df, "TRT01PN", match(map$P[i], arms) - 1)
    if ("TRT01AN" %in% names(df)) df <- set_col(df, "TRT01AN", match(map$A[i], arms) - 1)
    if ("ARMCD" %in% names(df)) df <- set_col(df, "ARMCD", .ai_sample_armcd[match(map$P[i], arms)])
    if ("ACTARMCD" %in% names(df)) df <- set_col(df, "ACTARMCD", .ai_sample_armcd[match(map$A[i], arms)])
    df
  }
  # FAS = all randomised subjects (ITTFL = 'Y'); SAF = all treated.  Two
  # randomised subjects were never dosed, so N(FAS) = N(SAF) + 2.
  add_fasfl <- function(df) {
    f <- as.character(df$ITTFL)
    attr(f, "label") <- "Full Analysis Set Population Flag"
    df$FASFL <- f
    df[append(setdiff(names(df), "FASFL"), "FASFL", after = match("ITTFL", names(df)))]
  }

  out <- lapply(names(.ai_sample_labels), function(nm) {
    x <- relabel(src[[nm]])
    if (nm %in% c("adsl", "adtte")) x <- add_fasfl(x)
    attr(x, "label") <- .ai_sample_labels[[nm]]
    x
  })
  stats::setNames(out, names(.ai_sample_labels))
}

#' Write the sample study for trying the AI flow
#'
#' Writes a made-up study, DMY-001 of Example Pharma, for trying the AI flow
#' (the tflplanner AI tab, or [tfl_ai_prepare()]): the ADaM in `adam/` and
#' the documents an agent reads in `inputs/`.  Everything is invented.  No
#' spec and no expected output is written: those are what the agent writes.
#'
#' * `adam/`: `adsl`, `adae`, `adlb`, `advs` and `adtte`, as SAS transport
#'   files (`.xpt`) when the haven package is installed, else as `.rds`.
#'   They are made from tflspec's bundled sample data; the arms are
#'   Placebo, Drug A 10 mg and Drug A 20 mg.  `advs` is used by no shell.
#' * `inputs/toc.xlsx`: the TOC (eight outputs; sheets `TOC`,
#'   `Revision history`, `Notes`).
#' * `inputs/shells.xlsx`: the annotated shells, one per sheet (six
#'   shells), and `General conventions`, which is not a shell.
#' * `inputs/adam_spec.xlsx`: the ADaM spec in the Pinnacle 21 form;
#'   `inputs/define.xml`: the same as define.xml.
#' * `inputs/sap.docx`: the statistical analysis plan, a reference document.
#'
#' Some points are left open in the documents on purpose: an agent should
#' ask about them in the chat rather than decide them silently.
#' `GUIDE-ja.md`, written beside the two folders, is the tester's guide in
#' Japanese: from this call to step 4 of the AI tab.
#'
#' @param dir Folder to write into; made when missing.  Put it where the
#'   agent's editor can open it (e.g. under the tflplanner home).
#' @param overwrite Overwrite files already there?  When `FALSE` (default),
#'   a file already there stops the call before anything is written.
#' @return `dir`, invisibly.  The paths written are in its attribute `files`.
#' @export
#' @examples
#' d <- tfl_ai_sample_study(file.path(tempdir(), "DMY-001-inputs"))
#' list.files(d, recursive = TRUE)
tfl_ai_sample_study <- function(dir, overwrite = FALSE) {
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    stop("`dir` must be one folder path.", call. = FALSE)
  }
  docs_from <- system.file("ai-sample", package = "tflspec")
  docs <- c("toc.xlsx", "shells.xlsx", "adam_spec.xlsx", "define.xml", "sap.docx")
  if (!nzchar(docs_from) || !all(file.exists(file.path(docs_from, c(docs, "GUIDE-ja.md"))))) {
    stop("The sample documents are not installed (inst/ai-sample).", call. = FALSE)
  }
  xpt <- requireNamespace("haven", quietly = TRUE)
  adam_files <- file.path("adam", paste0(names(.ai_sample_labels), if (xpt) ".xpt" else ".rds"))
  files <- c(adam_files, file.path("inputs", docs), "GUIDE-ja.md")
  there <- files[file.exists(file.path(dir, files))]
  if (length(there) && !isTRUE(overwrite)) {
    stop("Already there (use overwrite = TRUE): ", paste(there, collapse = ", "), call. = FALSE)
  }

  dir.create(file.path(dir, "adam"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "inputs"), recursive = TRUE, showWarnings = FALSE)
  ds <- .ai_sample_adam()
  for (i in seq_along(ds)) {
    nm <- names(ds)[i]
    x <- ds[[i]]
    if (xpt) {
      for (v in names(x)) {
        if (is.factor(x[[v]])) {
          lb <- attr(x[[v]], "label")
          x[[v]] <- as.character(x[[v]])
          attr(x[[v]], "label") <- lb
        }
      }
      haven::write_xpt(x, file.path(dir, adam_files[i]), version = 5, name = toupper(nm),
                       label = .ai_sample_labels[[nm]])
    } else {
      saveRDS(x, file.path(dir, adam_files[i]))
    }
  }
  file.copy(file.path(docs_from, docs), file.path(dir, "inputs", docs), overwrite = TRUE)
  file.copy(file.path(docs_from, "GUIDE-ja.md"), dir, overwrite = TRUE)
  invisible(structure(dir, files = file.path(dir, files)))
}
