# ============================================================================
#  AI-assisted drafting: what an answer changes
# ----------------------------------------------------------------------------
#  An answer is always whole (the TOC, the design), so what it changes is
#  found here, item by item, for a person to accept or reject: a TOC's rows
#  by output_id; a figure's pieces by section, then by step / layer name
#  and position.  Text is compared normalized (trimmed, blank = NA), so
#  writing noise does not show as a change.
# ============================================================================

#' What a drafting answer changes
#'
#' Compares the current state with a read answer ([tfl_ai_parse()]), item
#' by item, for a review where each item is accepted or rejected.
#'
#' * `toc`: the rows matched by `output_id`; a changed row has one row a
#'   changed column.
#' * `figure`: the pieces of each section (`data`, `stats`, `layers`)
#'   matched by their `step` / `layer` name in order of appearance (the
#'   second `point` layer with the second); the figure-wide `plot` settings
#'   as one piece, field by field; the design's `template`,
#'   `ggplot2_version` and `compose` as the piece `design`.  A piece that
#'   keeps its fields but changes its order relative to the others is
#'   `moved` (one inserted in the middle moves nothing).  A composed
#'   figure's plots are compared plot by plot (`plots$km/layers` ...).
#'
#' @param current What there is now: `toc`, a data frame of TOC rows
#'   ([tfl_read_toc()]'s fields as columns; `NULL` for none); `figure`, a
#'   [tfl_fig_design()] (`NULL` for none).  A `tfl_ai_answer` is accepted
#'   too.
#' @param answer A `tfl_ai_answer`, or what `current` is.
#' @param task One of [tfl_ai_tasks()]; by default the answer's.
#' @return A data frame, one row an item and, for a changed item, one a
#'   changed field: `part` (`toc`, or the section), `key` (the output id;
#'   the piece's name, with `#2` for a second one of a name), `status`
#'   (`added`, `changed`, `removed`, `moved`, `same`), `field`, `was`,
#'   `now` (for `changed` the field's values; for `moved` the positions).
#' @examples
#' cur <- tfl_fig_template("km_simple")
#' new <- cur
#' new$layers <- c(new$layers, list(list(layer = "censor_mark", shape = "x")))
#' new$plot$x_label <- "Time (Months)"
#' d <- tfl_ai_diff(cur, new, "figure")
#' d[d$status != "same", ]
#' @export
tfl_ai_diff <- function(current, answer, task = NULL) {
  task <- task %||% if (inherits(answer, "tfl_ai_answer")) answer$task
  if (is.null(task)) {
    .ard_stop(paste(
      "tfl_ai_diff(): give the `task` when `answer` is not a tfl_ai_answer."
    ))
  }
  task <- .ai_task(task, "tfl_ai_diff()")
  get <- function(x, what) {
    if (!inherits(x, "tfl_ai_answer")) return(x)
    if (task == "toc") x$sheets$toc else x$design
  }
  cur <- get(current)
  new <- get(answer)
  out <- switch(task,
    toc = .ai_diff_rows(.ai_toc_frame(cur, "current"),
                        .ai_toc_frame(new, "answer"), "output_id", "toc"),
    figure = .ai_diff_design(.ai_design_or_empty(cur, "current"),
                             .ai_design_or_empty(new, "answer"), "")
  )
  rownames(out) <- NULL
  out
}

.ai_diff_frame <- function(part = character(), key = character(),
                           status = character(), field = NA_character_,
                           was = NA_character_, now = NA_character_) {
  n <- length(part)
  data.frame(part = part, key = key, status = status,
             field = rep_len(field, n), was = rep_len(was, n),
             now = rep_len(now, n), stringsAsFactors = FALSE)
}

# normalized text: trimmed, blank NA
.ai_norm <- function(v) {
  v <- trimws(as.character(v))
  v[!is.na(v) & !nzchar(v)] <- NA_character_
  v
}

.ai_toc_frame <- function(x, what) {
  if (is.null(x)) {
    x <- as.data.frame(stats::setNames(
      rep(list(character()), length(.toc_fields)), .toc_fields
    ), stringsAsFactors = FALSE)
  }
  if (!is.data.frame(x) || !"output_id" %in% names(x)) {
    .ard_stop(sprintf(
      "tfl_ai_diff(): `%s` is a data frame of TOC rows with `output_id`.", what
    ))
  }
  x[] <- lapply(x, .ai_norm)
  x[!is.na(x$output_id), , drop = FALSE]
}

# Rows matched by their key columns; columns of either compared.
.ai_diff_rows <- function(cur, new, keys, part) {
  key_of <- function(d) do.call(paste, c(d[keys], sep = " / "))
  kc <- key_of(cur)
  kn <- key_of(new)
  fields <- setdiff(union(names(cur), names(new)), keys)
  val <- function(d, i, f) if (f %in% names(d)) d[[f]][i] else NA_character_
  out <- list()
  for (j in seq_along(kn)) {
    i <- match(kn[j], kc)
    if (is.na(i)) {
      out[[length(out) + 1L]] <- .ai_diff_frame(part, kn[j], "added")
      next
    }
    was <- vapply(fields, function(f) val(cur, i, f), "")
    now <- vapply(fields, function(f) val(new, j, f), "")
    ch <- !(is.na(was) & is.na(now)) & (is.na(was) | is.na(now) | was != now)
    out[[length(out) + 1L]] <- if (any(ch)) {
      .ai_diff_frame(rep(part, sum(ch)), kn[j], "changed", fields[ch],
                     was[ch], now[ch])
    } else {
      .ai_diff_frame(part, kn[j], "same")
    }
  }
  gone <- setdiff(kc, kn)
  if (length(gone)) {
    out[[length(out) + 1L]] <- .ai_diff_frame(rep(part, length(gone)), gone,
                                              "removed")
  }
  if (length(out)) do.call(rbind, out) else .ai_diff_frame()
}

# ---- a figure ---------------------------------------------------------------

.ai_design_or_empty <- function(x, what) {
  if (is.null(x)) return(tfl_fig_design())
  if (!inherits(x, "tfl_fig_design")) {
    .ard_stop(sprintf("tfl_ai_diff(): `%s` is a tfl_fig_design().", what))
  }
  x
}

.ai_diff_design <- function(cur, new, prefix) {
  out <- list()
  top <- c("template", "ggplot2_version", "compose")
  out$design <- .ai_diff_piece(
    paste0(prefix, "design"), "design",
    cur[top], new[top]
  )
  for (s in c("data", "stats")) {
    out[[s]] <- .ai_diff_pieces(cur[[s]], new[[s]], "step", paste0(prefix, s))
  }
  out$plot <- .ai_diff_piece(paste0(prefix, "plot"), "plot",
                             cur$plot, new$plot, .ai_piece_defaults("plot"))
  out$layers <- .ai_diff_pieces(cur$layers, new$layers, "layer",
                                paste0(prefix, "layers"))
  nm <- union(names(cur$plots), names(new$plots))
  for (p in nm) {
    a <- cur$plots[[p]]
    b <- new$plots[[p]]
    pre <- sprintf("%splots$%s/", prefix, p)
    out[[pre]] <- if (is.null(a)) {
      .ai_diff_frame(paste0(prefix, "plots"), p, "added")
    } else if (is.null(b)) {
      .ai_diff_frame(paste0(prefix, "plots"), p, "removed")
    } else {
      .ai_diff_design(a, b, pre)
    }
  }
  out <- do.call(rbind, out)
  # the design's own row only when it says something
  out[!(out$part == paste0(prefix, "design") & out$status == "same" &
          !length(c(cur$template, new$template))), , drop = FALSE]
}

# A piece's fields compared as text; one row "same", or a row a change.  A
# field left out is its default (`defaults`: field -> text).
.ai_diff_piece <- function(part, key, a, b, defaults = character()) {
  a <- .ai_piece_text(a)
  b <- .ai_piece_text(b)
  fields <- union(names(a), names(b))
  was <- unname(a[fields])
  now <- unname(b[fields])
  def <- unname(defaults[fields])
  was[is.na(was)] <- def[is.na(was)]
  now[is.na(now)] <- def[is.na(now)]
  ch <- !(is.na(was) & is.na(now)) & (is.na(was) | is.na(now) | was != now)
  if (!any(ch)) return(.ai_diff_frame(part, key, "same"))
  .ai_diff_frame(rep(part, sum(ch)), key, "changed", fields[ch], was[ch],
                 now[ch])
}

# A piece's defaults (tfl_fig_parts()): field -> text.
.ai_piece_defaults <- function(piece) {
  p <- tfl_fig_parts()
  p <- p[p$piece == piece & !is.na(p$default) & nzchar(p$default), ,
         drop = FALSE]
  stats::setNames(p$default, p$field)
}

# A piece as named text, one value a field (lists as YAML flow).
.ai_piece_text <- function(x) {
  x <- x[!vapply(x, function(v) is.null(v) || !length(v), NA)]
  if (!length(x)) return(stats::setNames(character(), character()))
  v <- vapply(x, function(z) {
    if (is.list(z) || length(z) != 1L || .is_fig_r(z)) .ai_yaml_flow(z)
    else as.character(z)
  }, "")
  stats::setNames(.ai_norm(v), names(x))
}

# A section's pieces matched: identical pieces first, in order (a longest
# common run), then the rest by name in order; moved = out of the longest
# run of matches that keeps its order.
.ai_diff_pieces <- function(cur, new, by, part) {
  name <- function(ps) {
    vapply(ps, function(p) as.character(p[[by]] %||% "?")[1L], "")
  }
  key <- function(nm) {
    occ <- stats::ave(seq_along(nm), nm, FUN = seq_along)
    ifelse(occ > 1L, paste0(nm, "#", occ), nm)
  }
  sig <- function(ps) {
    vapply(ps, function(p) paste(.ai_piece_text(p), collapse = "\r"), "")
  }
  nc <- name(cur)
  nn <- name(new)
  pos <- .ai_lcs_match(sig(cur), sig(new))
  for (j in which(is.na(pos))) {
    free <- setdiff(which(nc == nn[j]), pos)
    if (length(free)) pos[j] <- free[1L]
  }
  kc <- key(nc)
  kn <- key(nn)
  kept <- which(!is.na(pos))
  in_order <- kept[.ai_lis(pos[kept])]
  out <- list()
  for (j in seq_along(nn)) {
    i <- pos[j]
    if (is.na(i)) {
      out[[length(out) + 1L]] <- .ai_diff_frame(part, kn[j], "added",
                                                now = as.character(j))
      next
    }
    d <- .ai_diff_piece(part, kn[j], cur[[i]], new[[j]],
                        .ai_piece_defaults(nn[j]))
    if (!j %in% in_order) {
      mv <- .ai_diff_frame(part, kn[j], "moved", was = as.character(i),
                           now = as.character(j))
      d <- if (all(d$status == "same")) mv else rbind(mv, d)
    }
    out[[length(out) + 1L]] <- d
  }
  gone <- setdiff(seq_along(nc), pos)
  if (length(gone)) {
    out[[length(out) + 1L]] <- .ai_diff_frame(
      rep(part, length(gone)), kc[gone], "removed", was = as.character(gone)
    )
  }
  if (length(out)) do.call(rbind, out) else .ai_diff_frame()
}

# For each element of b, the position of its partner in a in a longest
# common subsequence of a and b (NA: none).
.ai_lcs_match <- function(a, b) {
  n <- length(a)
  m <- length(b)
  len <- matrix(0L, n + 1L, m + 1L)
  for (i in rev(seq_len(n))) {
    for (j in rev(seq_len(m))) {
      len[i, j] <- if (a[i] == b[j]) len[i + 1L, j + 1L] + 1L else
        max(len[i + 1L, j], len[i, j + 1L])
    }
  }
  out <- rep(NA_integer_, m)
  i <- 1L
  j <- 1L
  while (i <= n && j <= m) {
    if (a[i] == b[j]) {
      out[j] <- i
      i <- i + 1L
      j <- j + 1L
    } else if (len[i + 1L, j] >= len[i, j + 1L]) {
      i <- i + 1L
    } else {
      j <- j + 1L
    }
  }
  out
}

# The positions of a longest increasing subsequence of x.
.ai_lis <- function(x) {
  n <- length(x)
  if (!n) return(integer())
  len <- rep(1L, n)
  prev <- rep(0L, n)
  for (i in seq_len(n)) {
    for (j in seq_len(i - 1L)) {
      if (x[j] < x[i] && len[j] + 1L > len[i]) {
        len[i] <- len[j] + 1L
        prev[i] <- j
      }
    }
  }
  i <- which.max(len)
  out <- integer()
  while (i > 0L) {
    out <- c(i, out)
    i <- prev[i]
  }
  out
}
