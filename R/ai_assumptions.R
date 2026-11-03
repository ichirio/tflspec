# ============================================================================
#  The AI's assumptions and their ids: ai/assumption_ids.csv
# ----------------------------------------------------------------------------
#  Each assumption of spec/_assumptions.yml is given an id, `A<n>`, kept
#  across collections: its key is its report, its `where` and its `item`
#  (the item compared without case, spaces or punctuation).  Each is also
#  given a kind, `K<n>`: the assumptions that ask the same question, or,
#  with no question, that say the same item and assumption.
#  ai/assumption_ids.csv keeps the key of each id.  The person reads them
#  (tflplanner, tfl_ai_assumptions()); what to change, they tell the agent
#  in its chat, or change in the builder.
#
#    tfl_ai_assumptions()    one row an assumption, its id and kind
# ============================================================================

.ai_asm_ids_file <- function(d) file.path(d, "ai", "assumption_ids.csv")
.ai_asm_answer_cols <- c("id", "scope", "output_id", "verdict", "correct")

# Text compared without case, spaces or punctuation (as the kinds
# groups its questions); NA as ""
.ai_asm_norm <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  tolower(trimws(gsub("[[:space:][:punct:]]+", " ", x)))
}

.ai_asm_trim <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

# An assumption's key: report | where | item; a second one of the same key
# in the file `#2`
.ai_asm_keys <- function(a) {
  k <- paste(.ai_asm_trim(a$output_id), .ai_asm_trim(a$where), .ai_asm_norm(a$item), sep = "|")
  n <- stats::ave(seq_along(k), k, FUN = seq_along)
  ifelse(n > 1L, paste0(k, "#", n), k)
}

# An assumption's kind's key: its question, else its item and assumption
.ai_asm_kind_keys <- function(a) {
  q <- .ai_asm_norm(a$question)
  ifelse(nzchar(q), paste0("q|", q),
         paste0("a|", .ai_asm_norm(a$item), "|", .ai_asm_norm(a$assumption)))
}

.ai_asm_ids_read <- function(d) {
  none <- data.frame(type = character(), key = character(), id = character(),
                     stringsAsFactors = FALSE)
  f <- .ai_asm_ids_file(d)
  if (!file.exists(f)) return(none)
  r <- tryCatch(utils::read.csv(f, colClasses = "character", na.strings = character(),
                                fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE),
                error = function(e) NULL)
  if (is.null(r) || !all(names(none) %in% names(r))) return(none)
  r[names(none)]
}

# The assumptions given their id (by their key) and kind (by their kind's
# key; a row the agent fixed keeps the kind it had, its text being the
# person's now); the keys new to ai/assumption_ids.csv added to it
.ai_asm_number <- function(a, d) {
  if (!nrow(a)) {
    a$id <- character()
    a$kind <- character()
    return(a)
  }
  reg <- .ai_asm_ids_read(d)
  n0 <- nrow(reg)
  give <- function(type, keys) {
    have <- reg$key[reg$type == type]
    new <- setdiff(unique(keys), have)
    if (length(new)) {
      num <- suppressWarnings(as.integer(sub("^[A-Z]", "", reg$id[reg$type == type])))
      top <- max(c(0L, num), na.rm = TRUE)
      reg <<- rbind(reg, data.frame(type = type, key = new, id = paste0(type, top + seq_along(new)),
                                    stringsAsFactors = FALSE))
    }
    at <- reg$type == type
    reg$id[at][match(keys, reg$key[at])]
  }
  a$id <- give("A", .ai_asm_keys(a))
  had <- if (is.null(a$kind)) rep(NA_character_, nrow(a)) else as.character(a$kind)
  fixed <- if (is.null(a$status)) rep(FALSE, nrow(a)) else a$status %in% "fixed"
  keep <- fixed & !is.na(had) & grepl("^K[0-9]+$", had)
  kind <- had
  if (any(!keep)) kind[!keep] <- give("K", .ai_asm_kind_keys(a[!keep, , drop = FALSE]))
  a$kind <- kind
  if (nrow(reg) > n0) {
    dir.create(dirname(.ai_asm_ids_file(d)), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(reg, .ai_asm_ids_file(d), row.names = FALSE, fileEncoding = "UTF-8")
  }
  a
}

# spec/_assumptions.yml written from its rows: each a mapping, its id and
# kind first, an empty field left out; the path
.ai_asm_write <- function(d, rows) {
  f <- file.path(d, "spec", "_assumptions.yml")
  ord <- c("id", "kind", "output_id", "where", "item", "assumption", "value", "question",
           "status", "note")
  ord <- c(intersect(ord, names(rows)), setdiff(names(rows), ord))
  lst <- lapply(seq_len(nrow(rows)), function(i) {
    r <- as.list(rows[i, ord, drop = FALSE])
    r[!vapply(r, function(z) is.null(z) || is.na(z[1L]), NA)]
  })
  .ai_write_text(yaml::as.yaml(lst, indent.mapping.sequence = TRUE), f)
  f
}

# A report's assumptions from its answer (`new`), collected again: one the
# agent fixed (`old`, `status: fixed`, the same key) stays as the agent
# left it -- the answer may still say the old text -- with a note when the
# answer's text differs.  The rows, with the columns of the file's frame.
.ai_asm_keep_fixed <- function(old, new) {
  # (a column the answers add -- `from`, the decision an assumption came
  # from -- is kept beside the file's)
  cols <- union(names(old), names(new))
  for (k in setdiff(cols, names(new))) new[[k]] <- rep(NA_character_, nrow(new))
  for (k in setdiff(cols, names(old))) old[[k]] <- rep(NA_character_, nrow(old))
  new <- new[cols]
  old <- old[cols]
  fx <- old[old$status %in% "fixed", , drop = FALSE]
  if (!nrow(fx) || !nrow(new)) return(new)
  at <- match(.ai_asm_keys(new), .ai_asm_keys(old))
  for (i in which(!is.na(at) & old$status[at] %in% "fixed")) {
    r <- old[at[i], , drop = FALSE]
    r$note <- if (.ai_asm_norm(new$assumption[i]) == .ai_asm_norm(r$assumption)) NA_character_ else
      sprintf("collected again: the answer still says \"%s\"; kept as the agent fixed it",
              .ai_asm_trim(new$assumption[i]))
    new[i, ] <- r
  }
  new
}

# spec/_assumptions.yml with every row's id and kind (written back when one
# was new or changed): the frame with `where`, `id`, `kind`, `status`, `note`
.ai_asm_stamp <- function(d) {
  a <- .ai_folder_assumptions(d, all = TRUE)
  names(a)[names(a) == "sheet"] <- "where"
  if (!nrow(a)) return(a)
  b <- .ai_asm_number(a, d)
  if (!identical(b$id, a$id) || !identical(b$kind, a$kind)) .ai_asm_write(d, b)
  b
}

# ---- the assumptions, numbered ------------------------------------------------------

# One row an assumption, with the reports of its kind counted
.ai_asm_table <- function(study_dir) {
  a <- .ai_asm_stamp(study_dir)
  a$n_reports <- vapply(a$kind, function(k) {
    length(unique(.ai_asm_trim(a$output_id[a$kind %in% k])))
  }, 1L, USE.NAMES = FALSE)
  a
}

#' The AI's assumptions and their ids
#'
#' What the agent took on a guess or from a decision, from
#' `spec/_assumptions.yml` (written as its drafts are checked,
#' [tfl_ai_collect_answers()]), one row an assumption, for a screen to show
#' and the person to read.  What is wrong, the person tells the agent in
#' its chat, or changes in the builder.
#'
#' Each assumption has an id, `A<n>`, kept across collections: the same
#' report, `where` and `item` (the item compared without case, spaces or
#' punctuation) keep their id, and a new one takes the next number
#' (`ai/assumption_ids.csv` keeps them: columns `type`, `key`, `id`).  Each
#' also has a kind, `K<n>`: the assumptions asking the same question, or,
#' with no question, saying the same item and assumption.
#'
#' @param study_dir The study folder.
#' @return A data frame, one row an assumption: `id`, `kind`, `n_reports`
#'   (the reports of its kind), `output_id`, `where`, `item`,
#'   `assumption` (one sentence: what the AI decided and how), `value`
#'   (the value it used, as text; `NA` when the item has none), `question`,
#'   `from` (the decision it was taken from, `D<n>`; `NA` when its own).
#' @seealso [tfl_ai_confirm()], [tfl_ai_decisions_read()]
#' @export
tfl_ai_assumptions <- function(study_dir) {
  a <- .ai_asm_table(study_dir)
  out <- data.frame(id = a$id, kind = a$kind, n_reports = as.integer(a$n_reports),
                    output_id = a$output_id, where = a$where, item = a$item,
                    assumption = a$assumption, value = a$value, question = a$question,
                    from = a$from %||% rep(NA_character_, nrow(a)),
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

# ---- ai/defaults.md: every default the drafts took, the most important first ----

# How much an assumption changes the output: 1 the numbers (the data and the
# analyses, or an item about the population, the denominator, a filter, the
# arms, the parameter, the visit, the baseline, the worst case, missing
# values, what is counted, a derivation), 2 the rows or the statistics (the
# content, the code lists, a template; or the rows, statistics, sort order,
# levels, categories, a total), 3 the text or the look (titles, footnotes,
# labels, widths -- wherever they are written --, digits, the page)
.ai_impact_class <- function(where, item) {
  where <- trimws(ifelse(is.na(where), "", as.character(where)))
  item <- trimws(ifelse(is.na(item), "", as.character(item)))
  one <- paste0("population|analysis set|denominator|filter|where|arm|treatment|",
                "parameter|visit|baseline|worst|missing|counted|derivation")
  two <- "rows|statistics|sort|order|levels|categories|total"
  # (column widths are the look, wherever they are written)
  ifelse(grepl("width", item, ignore.case = TRUE), 3L,
         ifelse(grepl("^(data|ard)", where, ignore.case = TRUE) | grepl(one, item, ignore.case = TRUE), 1L,
                ifelse(grepl("^(content|codelists|template)", where, ignore.case = TRUE) |
                         grepl(two, item, ignore.case = TRUE), 2L, 3L)))
}

# ai/defaults.md: every report's `ai: assumptions`, read as
# tfl_ai_assumptions() reads them, in three groups by how much they change
# the output; a line a default, the lines of a group by report id; a
# report's width lines in one.  The path.
.ai_defaults_write <- function(study_dir) {
  a <- tfl_ai_assumptions(study_dir)
  a$class <- if (nrow(a)) .ai_impact_class(a$where, a$item) else integer()
  say <- function(x, y) ifelse(!is.na(x) & nzchar(trimws(x)), trimws(x), ifelse(is.na(y), "", trimws(y)))
  a$text <- say(a$assumption, a$value)
  a$src <- ifelse(is.na(a$from) | !nzchar(trimws(a$from)), "not stated", trimws(a$from))
  a$id <- ifelse(is.na(a$output_id), "", trimws(a$output_id))
  wid <- a$class == 3L & grepl("width", ifelse(is.na(a$item), "", a$item), ignore.case = TRUE)
  # (a report's widths: the first line, the others folded into it)
  keep <- !(wid & duplicated(paste(a$id, wid)))
  a <- a[keep, , drop = FALSE]
  a <- a[order(a$class, a$id, method = "radix"), , drop = FALSE]
  line <- function(x) {
    if (!nrow(x)) return("- (none)")
    sprintf("- %s %s/%s: %s (from: %s)", x$id, ifelse(is.na(x$where), "", x$where),
            ifelse(is.na(x$item), "", x$item), x$text, x$src)
  }
  title <- c("Changes the numbers", "Changes the rows or statistics", "Changes the text or the look")
  body <- unlist(lapply(1:3, function(k) {
    x <- a[a$class == k, , drop = FALSE]
    c(sprintf("## %d. %s (%d)", k, title[k], nrow(x)), "", line(x), "")
  }))
  f <- file.path(study_dir, "ai", "defaults.md")
  .ai_write_text(c("# Defaults", "",
                   paste("Every default the drafts took, the most important first.",
                         "The questions (ai/questions-end.md) are the ones the agent was unsure of;",
                         "read these too."),
                   "", body), f)
  f
}
