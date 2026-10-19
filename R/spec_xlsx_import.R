# Taking a workbook of the flat view back into the YAML spec: only the rows
# the workbook holds, each matched by its key to the item it came from.
# What the workbook cannot hold (content, !r structures, calls) is not
# touched.  Design: spec-format-redesign 5 (the round trip's guarantee).

#' Take an edited flat-view workbook back into the YAML spec
#'
#' The workbook [tfl_write_spec_xlsx()] wrote, edited in Excel, read back:
#' each row is matched by its key (`toc`: `id`; `datasets`: `dataset`;
#' `populations`: `id`; `analysis_data` and `analyses`: `output_id` + `id`;
#' `codelists`: `output_id` + `variable` + `value`; `tokens`: `output_id` +
#' `name`; `header` / `footer` / `titles` / `footnotes`: `output_id` +
#' `line`; `page` / `report`: `output_id`; `listing_cols`: `output_id` +
#' `vars`) and the item it names is updated with the row's cells; a row
#' with a new key is added; an item the workbook no longer has is reported
#' (`missing`), not removed.  Cells read as the converter reads sheets: `a
#' | b` a list, `(none)` an `!unset`, the columns that hold R as `!r`.
#' Nothing else of the YAML files changes.
#'
#' @param dir The study folder (or its `spec/`).
#' @param path The workbook.
#' @param write Write the changed YAML files? `FALSE` only reports.
#' @return Invisibly, a list: `changes` (a data frame: `sheet`, `key`,
#'   `column`, `from`, `to`), `added` (keys new to the spec), `missing`
#'   (keys of the spec the workbook lacks), `files` (written).
#' @export
tfl_import_spec_xlsx <- function(dir, path, write = TRUE) {
  .ard_need("readxl", "tfl_import_spec_xlsx()")
  if (!file.exists(path)) stop("tfl_import_spec_xlsx(): no file ", path, call. = FALSE)
  spec <- .spec_dir(dir)
  sheets <- readxl::excel_sheets(path)
  rd <- function(s) if (s %in% sheets) as.data.frame(readxl::read_excel(path, s, col_types = "text"), stringsAsFactors = FALSE) else NULL
  st <- .imp_state()
  touched <- character()

  # the files, read as plain lists (the tags kept), written only when changed
  files <- list()
  get_file <- function(rel) {
    if (is.null(files[[rel]])) {
      f <- file.path(if (identical(rel, "study.yml")) dirname(spec) else spec, rel)
      files[[rel]] <<- list(path = f, y = if (file.exists(f)) yaml::read_yaml(f, handlers = .yaml_handlers()) %||% list() else list())
    }
    files[[rel]]$y
  }
  set_file <- function(rel, y) { files[[rel]]$y <<- y; touched <<- union(touched, rel) }
  out_rel <- function(id) {
    f <- list.files(file.path(spec, "outputs"), paste0("^", id, "\\.ya?ml$"), recursive = TRUE, full.names = TRUE)
    if (length(f)) sub(paste0("^", normalizePath(spec, "/"), "/"), "", normalizePath(f[1L], "/")) else file.path("outputs", paste0(id, ".yml"))
  }
  scope_rel <- function(output_id) {
    if (is.na(output_id) || !nzchar(output_id)) "defaults.yml"
    else if (startsWith(output_id, "@")) file.path("patterns", paste0(substring(output_id, 2L), ".yml"))
    else out_rel(output_id)
  }

  # a list of maps (rows) under `path_in` of the file `rel`, updated by key
  update_rows <- function(sheet, rel, path_in, key_cols, id_col = NULL, r_cols = character(), list_cols = character(), rename = character()) {
    d <- rd(sheet)
    if (is.null(d) || !nrow(d)) return()
    y <- get_file(rel)
    rows <- .get_in(y, path_in) %||% list()
    seen <- character()
    for (i in seq_len(nrow(d))) {
      key <- paste(vapply(key_cols, function(k) .na_scalar(d[[k]][i]), ""), collapse = " / ")
      seen <- c(seen, key)
      m <- list()
      for (cn in setdiff(names(d), c("output_id", key_cols[key_cols == "output_id"]))) {
        v <- .conv_cell(d[[cn]][i], cn, r_cols, list_cols, function(...) NULL, NA, sheet, i)
        nm <- if (cn %in% names(rename)) rename[[cn]] else cn
        if (!is.null(v)) m[[nm]] <- v
      }
      kvals <- lapply(key_cols[key_cols != "output_id"], function(k) if (k %in% names(rename)) rename[[k]] else k)
      j <- .find_row(rows, kvals, vapply(key_cols[key_cols != "output_id"], function(k) .na_scalar(d[[k]][i]), ""))
      if (is.na(j)) {
        rows[[length(rows) + 1L]] <- m
        st$added <- c(st$added, paste0(sheet, ": ", key))
      } else {
        for (nm in names(m)) {
          old <- rows[[j]][[nm]]
          if (!identical(.flat_text(old), .flat_text(m[[nm]]))) {
            st$changes[[length(st$changes) + 1L]] <- data.frame(
              sheet = sheet, key = key, column = nm, from = .flat_text(old), to = .flat_text(m[[nm]]), stringsAsFactors = FALSE)
            rows[[j]][[nm]] <- m[[nm]]
          }
        }
      }
    }
    have <- vapply(rows, function(r) paste(vapply(key_cols[key_cols != "output_id"], function(k) {
      nm <- if (k %in% names(rename)) rename[[k]] else k
      .flat_text(r[[nm]]) }, ""), collapse = " / "), "")
    keyed_seen <- vapply(seen, function(k) sub("^[^/]* / ", "", k), "")
    gone <- setdiff(have, c(keyed_seen, seen))
    if (length(gone)) st$missing <- c(st$missing, paste0(sheet, ": ", gone))
    y <- .set_in(y, path_in, rows)
    set_file(rel, y)
  }

  # study-wide rows
  update_rows("toc", "toc.yml", "outputs", "id", list_cols = c("sets", "datasets"))
  update_rows("datasets", "data.yml", "datasets", "dataset", r_cols = "derive")
  update_rows("populations", "data.yml", "populations", "id", r_cols = c("where", "derive"))
  # rows by report (or defaults / pattern)
  by_scope <- function(sheet, fun) {
    d <- rd(sheet)
    if (is.null(d) || !nrow(d)) return()
    oid <- if ("output_id" %in% names(d)) d$output_id else rep(NA_character_, nrow(d))
    for (sc in unique(ifelse(is.na(oid), "", oid))) {
      part <- d[ifelse(is.na(oid), "", oid) == sc, , drop = FALSE]
      fun(if (nzchar(sc)) sc else NA_character_, part)
    }
  }
  by_scope("analysis_data", function(id, part) .imp_rows_of(st, part, "analysis_data", scope_rel(id), c("ard", "analysis_data"), "id",
                                                            r_cols = c("where", "derive", "code"), list_cols = c("add", "keep", "distinct"), get_file = get_file, set_file = set_file))
  by_scope("analyses", function(id, part) .imp_rows_of(st, part, "analyses", scope_rel(id), c("ard", "analyses"), "id",
                                                        r_cols = c("where", "args", "post", "code"), list_cols = c("by", "strata", "variables", "statistics", "formats"), get_file = get_file, set_file = set_file))
  by_scope("listing_cols", function(id, part) {
    # best-effort: the listing_col calls of plan_listing, by vars
    rel <- scope_rel(id)
    y <- get_file(rel)
    k <- which(vapply(y$content %||% list(), function(it) is.list(it) && identical(names(it), "plan_listing"), NA))
    if (!length(k)) { st$missing <- c(st$missing, paste0("listing_cols: ", id, " (no plan_listing)")); return() }
    pl <- y$content[[k[1L]]]$plan_listing
    rows <- lapply(pl$cols %||% list(), function(cl) cl$args %||% list())
    for (i in seq_len(nrow(part))) {
      m <- list()
      for (cn in setdiff(names(part), "output_id")) {
        v <- .conv_cell(part[[cn]][i], cn, character(), "vars", function(...) NULL, NA, "listing_cols", i)
        if (!is.null(v)) m[[cn]] <- v
      }
      kv <- .flat_text(m$vars)
      j <- .find_row(rows, list("vars"), kv)
      key <- paste(id, kv, sep = " / ")
      if (is.na(j)) { rows[[length(rows) + 1L]] <- m; st$added <- c(st$added, paste0("listing_cols: ", key)) }
      else for (nm in names(m)) if (!identical(.flat_text(rows[[j]][[nm]]), .flat_text(m[[nm]]))) {
        st$changes[[length(st$changes) + 1L]] <- data.frame(sheet = "listing_cols", key = key, column = nm,
                                                          from = .flat_text(rows[[j]][[nm]]), to = .flat_text(m[[nm]]), stringsAsFactors = FALSE)
        rows[[j]][[nm]] <- m[[nm]]
      }
    }
    pl$cols <- lapply(rows, function(r) list(fn = "listing_col", args = r))
    y$content[[k[1L]]]$plan_listing <- pl
    set_file(rel, y)
  })
  by_scope("codelists", function(id, part) {
    rel <- if (is.na(id)) "codelists.yml" else scope_rel(id)
    y <- get_file(rel)
    for (v in unique(part$variable)) {
      pv <- part[part$variable == v, , drop = FALSE]
      base <- if (is.na(id)) y else y$codelists
      rows <- base[[v]] %||% list()
      for (i in seq_len(nrow(pv))) {
        j <- .find_row(rows, list("value"), pv$value[i])
        m <- list(value = pv$value[i])
        if (!is.na(pv$label[i])) m$label <- pv$label[i]
        un <- if (is.null(pv$under) || is.na(pv$under[i]) || !nzchar(pv$under[i])) NULL else pv$under[i]
        if (!is.null(un)) m$under <- un
        key <- paste(if (is.na(id)) "" else id, v, pv$value[i], sep = " / ")
        if (is.na(j)) { rows[[length(rows) + 1L]] <- m; st$added <- c(st$added, paste0("codelists: ", key)) }
        else if (!identical(.flat_text(rows[[j]]$label), .flat_text(m$label))) {
          st$changes[[length(st$changes) + 1L]] <- data.frame(sheet = "codelists", key = key, column = "label",
                                                            from = .flat_text(rows[[j]]$label), to = .flat_text(m$label), stringsAsFactors = FALSE)
          rows[[j]]$label <- m$label
        }
        if (!is.na(j) && !identical(.flat_text(rows[[j]]$under), .flat_text(un))) {
          st$changes[[length(st$changes) + 1L]] <- data.frame(sheet = "codelists", key = key, column = "under",
                                                            from = .flat_text(rows[[j]]$under), to = .flat_text(un), stringsAsFactors = FALSE)
          rows[[j]]$under <- un
        }
      }
      if (is.na(id)) y[[v]] <- rows else y$codelists[[v]] <- rows
    }
    set_file(rel, y)
  })
  for (band in c("header", "footer", "titles", "footnotes")) {
    by_scope(band, function(id, part) {
      rel <- scope_rel(id)
      y <- get_file(rel)
      m <- y$page[[band]] %||% list()
      for (i in seq_len(nrow(part))) {
        line <- as.character(part$line[i])
        cells <- list()
        for (cn in intersect(c("left", "center", "right"), names(part))) {
          v <- .conv_cell(part[[cn]][i], cn, character(), character(), function(...) NULL, NA, band, i)
          if (!is.null(v)) cells[[cn]] <- v
        }
        if (length(cells) && all(vapply(cells, inherits, NA, "tfl_unset"))) cells <- tfl_unset()
        key <- paste(if (is.na(id)) "" else id, line, sep = " / ")
        if (is.null(m[[line]])) st$added <- c(st$added, paste0(band, ": ", key))
        else if (!identical(.flat_text(m[[line]]), .flat_text(cells))) {
          st$changes[[length(st$changes) + 1L]] <- data.frame(sheet = band, key = key, column = "line",
                                                            from = .flat_text(m[[line]]), to = .flat_text(cells), stringsAsFactors = FALSE)
        }
        m[[line]] <- cells
      }
      y$page[[band]] <- m
      set_file(rel, y)
    })
  }
  by_scope("tokens", function(id, part) {
    rel <- scope_rel(id)
    y <- get_file(rel)
    m <- y$page$tokens %||% list()
    for (i in seq_len(nrow(part))) {
      v <- .conv_cell(part$value[i], "value", character(), character(), function(...) NULL, NA, "tokens", i)
      key <- paste(if (is.na(id)) "" else id, part$name[i], sep = " / ")
      if (is.null(m[[part$name[i]]])) st$added <- c(st$added, paste0("tokens: ", key))
      else if (!identical(.flat_text(m[[part$name[i]]]), .flat_text(v))) {
        st$changes[[length(st$changes) + 1L]] <- data.frame(sheet = "tokens", key = key, column = "value",
                                                          from = .flat_text(m[[part$name[i]]]), to = .flat_text(v), stringsAsFactors = FALSE)
      }
      m[[part$name[i]]] <- v
    }
    y$page$tokens <- m
    set_file(rel, y)
  })
  for (sh in c("page", "report")) {
    by_scope(sh, function(id, part) {
      rel <- scope_rel(id)
      y <- get_file(rel)
      page <- y$page %||% list()
      before <- .page_columns(page)[[sh]]
      for (cn in setdiff(names(part), "output_id")) {
        v <- .conv_cell(part[[cn]][1L], cn, character(), character(), function(...) NULL, NA, sh, 1L)
        if (is.null(v)) next
        if (!identical(.flat_text(before[[cn]]), .flat_text(v))) {
          st$changes[[length(st$changes) + 1L]] <- data.frame(sheet = sh, key = if (is.na(id)) "" else id, column = cn,
                                                            from = .flat_text(before[[cn]]), to = .flat_text(v), stringsAsFactors = FALSE)
          page <- .page_put(page, sh, cn, v, function(...) NULL, id)
        }
      }
      y$page <- page
      set_file(rel, y)
    })
  }


  written <- character()
  if (write) {
    for (rel in touched) {
      f <- files[[rel]]$path
      dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
      writeLines(yaml::as.yaml(files[[rel]]$y, indent.mapping.sequence = TRUE, handlers = .yaml_out_handlers()), f, useBytes = TRUE)
      written <- c(written, f)
    }
  }
  ch <- if (length(st$changes)) do.call(rbind, st$changes) else
    data.frame(sheet = character(), key = character(), column = character(), from = character(), to = character(), stringsAsFactors = FALSE)
  invisible(list(changes = ch, added = st$added, missing = st$missing, files = written))
}

.imp_state <- function() { e <- new.env(); e$changes <- list(); e$added <- character(); e$missing <- character(); e }

# the rows of one report's list (ard.analyses ...) or its content items of
# one function (a listing's columns), updated by key
.imp_rows_of <- function(st, part, sheet, rel, path_in, key_col, r_cols = character(), list_cols = character(),
                         get_file, set_file, content_fn = NULL) {
  y <- get_file(rel)
  if (is.null(content_fn)) rows <- .get_in(y, path_in) %||% list()
  else rows <- lapply(Filter(function(it) is.list(it) && identical(names(it), content_fn), y$content %||% list()), function(it) it[[1L]])
  id <- .na_scalar(part$output_id[1L])
  for (i in seq_len(nrow(part))) {
    m <- list()
    for (cn in setdiff(names(part), "output_id")) {
      v <- .conv_cell(part[[cn]][i], cn, r_cols, list_cols, function(...) NULL, NA, sheet, i)
      if (!is.null(v)) m[[cn]] <- v
    }
    kv <- .flat_text(m[[key_col]])
    j <- .find_row(rows, list(key_col), kv)
    key <- paste(id, kv, sep = " / ")
    if (is.na(j)) { rows[[length(rows) + 1L]] <- m; st$added <- c(st$added, paste0(sheet, ": ", key)) }
    else for (nm in names(m)) if (!identical(.flat_text(rows[[j]][[nm]]), .flat_text(m[[nm]]))) {
      st$changes[[length(st$changes) + 1L]] <- data.frame(sheet = sheet, key = key, column = nm,
                                                        from = .flat_text(rows[[j]][[nm]]), to = .flat_text(m[[nm]]), stringsAsFactors = FALSE)
      rows[[j]][[nm]] <- m[[nm]]
    }
  }
  if (is.null(content_fn)) y <- .set_in(y, path_in, rows)
  else {
    others <- Filter(function(it) !(is.list(it) && identical(names(it), content_fn)), y$content %||% list())
    y$content <- c(lapply(rows, function(r) stats::setNames(list(r), content_fn)), others)
  }
  set_file(rel, y)
}

.get_in <- function(y, path) { for (p in path) y <- y[[p]]; y }
.set_in <- function(y, path, value) {
  if (length(path) == 1L) { y[[path]] <- value; return(y) }
  y[[path[1L]]] <- .set_in(y[[path[1L]]] %||% list(), path[-1L], value)
  y
}
# the index of the row whose key fields equal `vals`; NA when none
.find_row <- function(rows, key_names, vals) {
  for (j in seq_along(rows)) {
    ok <- all(vapply(seq_along(key_names), function(k) identical(.flat_text(rows[[j]][[key_names[[k]]]]), vals[[k]]), NA))
    if (ok) return(j)
  }
  NA_integer_
}
# a value as the text a cell would show (for comparing)
.flat_text <- function(v) {
  if (is.null(v)) return(NA_character_)
  if (inherits(v, "tfl_unset")) return("(none)")
  if (inherits(v, "tfl_r")) return(as.character(v))
  if (is.list(v)) {
    if (!is.null(names(v))) return(paste(names(v), vapply(v, .flat_text, ""), sep = "=", collapse = "; "))
    v <- unlist(v)
  }
  if (is.logical(v)) v <- ifelse(v, "TRUE", "FALSE")
  paste(as.character(v), collapse = " | ")
}
