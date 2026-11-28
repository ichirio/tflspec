# ---- a table's items on the sheets, read back and checked (#689) ------------
#
# tfl_sheets_from_spec() puts a table's items on the sheets through the
# bridge (.plan_sheets_from_content()).  What the sheets cannot say is kept
# as YAML in the tables row's content_more (#448).  Which arguments those
# are is not a list of names: every item is written, read back as the
# converter reads it (.plan_content_from_sheets(), content_more put with
# it, from the sheets as they are returned), and compared with the item as
# written, argument by argument, as the code each makes.
#
# An argument that does not come back the same has one of two outcomes:
# - "same cells": written again from what is read back, the sheets' rows
#   are the same as from the item as written -- the argument is on the
#   sheets in another form (header `lines` read as header rows,
#   plan_labels(AGE = ) as plan_labels(variable = )).  It stays on the
#   sheets, and content_more marks it, `- plan_col_header: {lines: !sheet}`:
#   the reader takes its value from the sheets' item in its own form
#   (.sheet_forms).  The grid stays the one place the value lives.
# - "held": anything else (not written, a text a cell does not keep) is
#   the item's rest in content_more, as YAML; the arguments that come back
#   stay on the sheets, and read back the two are one item again.
# A mark whose argument still does not come back is held.  An item that
# still does not come back (its unnamed arguments, an argument the sheets
# make up) is kept whole.  What cannot be made right is said, by report and
# verb.  Named arguments are compared by name: read back, they may stand in
# the sheets' order, which is the same call.

# The sheets of a table's items and what goes to content_more.  `items` the
# items without normalize_ard() (`nz`, which the bridge reads for its
# stack), `id` the report (NA: the defaults), `bridge` the bridge, `study`
# (the defaults) the study sheet as tfl_sheets_from_spec() returns it,
# made from the bridge's.  Returns list(sp, more, notes, unkept): the
# bridge's sheets, the items for content_more, the bridge's notes with the
# check's, and the check's alone (what does not come back even kept as
# YAML: the caller says it aloud).
.sheets_said <- function(items, nz, id, bridge, study = NULL) {
  oid <- if (is.na(id)) NULL else id
  n <- length(items)
  # each item's arguments kept apart (its rest), those marked as on the
  # sheets in another form, and the items kept whole
  held <- rep(list(character()), n)
  marked <- rep(list(character()), n)
  whole <- rep(FALSE, n)
  cut_item <- function(i) {
    it <- items[[i]]
    if (length(held[[i]])) it$args <- it$args[!names(it$args) %in% held[[i]]]
    it
  }
  # an item's part in content_more: its rest as written, its marks
  more_of <- function(i, rest) {
    a <- items[[i]]$args
    a <- a[names(a) %in% c(rest, marked[[i]])]
    for (k in setdiff(intersect(names(a), marked[[i]]), rest)) {
      a[k] <- list(.sheet_mark(.sheet_shape(items[[i]]$fn, k, a[[k]])))
    }
    if (length(a)) tfl_item(items[[i]]$fn, a)
  }
  # the bridge on these items: refused (a stop) or not said
  # (not_converted_at), an item goes whole and the bridge runs on the rest
  run <- function(keep) {
    while (length(keep)) {
      sp <- tryCatch(bridge(c(nz, lapply(keep, cut_item)), oid), error = function(e) e)
      if (inherits(sp, "error")) {
        refused <- vapply(keep, function(k) inherits(tryCatch(
          bridge(c(nz, list(cut_item(k))), oid), error = function(e) e), "error"), NA)
        if (!any(refused)) stop(sp)
        keep <- keep[!refused]
        next
      }
      out <- attr(sp, "not_converted_at") %||% integer()
      if (!length(out)) return(list(sp = sp, keep = keep))
      keep <- keep[-out]
    }
    list(sp = list(), keep = keep)
  }
  # (written again, does an item give the same rows? an argument that the
  # rows say, read back in another form)
  # (with no report: the same rows for every report, one bridge run)
  rows_of <- function(fn, args) {
    sp <- tryCatch(bridge(c(nz, list(tfl_item(fn, args))), NULL), error = function(e) NULL)
    if (is.null(sp) || length(attr(sp, "not_converted"))) NULL else .sheets_rows(sp)
  }
  cells_same <- function(fn, a, b, args) {
    x <- rows_of(fn, a)
    !is.null(x) && identical(x, rows_of(fn, b)) && !identical(x, rows_of(fn, a[!names(a) %in% args]))
  }
  # (what the study sheet gives alone -- the defaults' rounding from
  # study.yml's settings -- is the study's, not a difference)
  base <- if (is.na(id) && !is.null(study)) .sheets_read_back(list(), id, list(), study) else list()
  if (inherits(base, "error")) base <- list()
  said <- character()
  repeat {
    r <- run(which(!whole))
    sp <- r$sp
    gone <- !seq_len(n) %in% r$keep
    # (the rest the bridge keeps apart itself joins the item's own: one item
    # of the verb in content_more, its arguments in the item's order)
    rest <- held
    other <- list()
    fns <- vapply(items, function(it) it$fn, "")
    for (x in attr(sp, "not_converted_rest")) {
      i <- which(fns == x$fn & !gone)
      if (length(i) == 1L) rest[[i]] <- union(rest[[i]], names(x$args)) else other <- c(other, list(x))
    }
    more <- list()
    for (i in seq_len(n)) {
      it <- if (gone[i]) items[[i]] else more_of(i, rest[[i]])
      if (!is.null(it)) more[[length(more) + 1L]] <- it
    }
    more <- c(more, other)
    back <- .sheets_read_back(sp, id, more, study)
    # sheets that cannot be read back: said, with the reader's words; the
    # items that cannot be read back alone are kept whole, else all of them
    if (inherits(back, "error")) {
      said <- union(said, sprintf(
        "its items could not be read back from the sheets (%s): kept whole, as YAML",
        conditionMessage(back)))
      if (all(whole | gone)) break
      alone <- vapply(seq_len(n), function(i) {
        if (whole[i] || gone[i]) return(FALSE)
        s1 <- tryCatch(bridge(c(nz, list(cut_item(i))), oid), error = function(e) NULL)
        inherits(.sheets_read_back(s1, id, Filter(Negate(is.null), list(more_of(i, held[[i]]))), study),
                 "error")
      }, NA)
      whole <- whole | gone | if (any(alone)) alone else TRUE
      next
    }
    chk <- .sheets_holds(items, back, held, marked, gone, cells_same, base, bridge)
    notes <- chk$notes
    if (identical(chk$held, held) && identical(chk$marked, marked) &&
        identical(chk$whole, whole | gone)) break
    held <- chk$held
    marked <- chk$marked
    whole <- chk$whole
  }
  unkept <- if (inherits(back, "error")) said else notes
  # (a reader error the items were kept whole for, when they then came back)
  read_notes <- if (inherits(back, "error")) character() else said
  list(sp = sp, more = more, notes = c(attr(sp, "not_converted") %||% character(), read_notes, unkept),
       unkept = unkept)
}

# The items the converter reads from these sheets and content_more
# (tfl_yaml_from_sheets(): a report's own rows, or the defaults' with the
# study sheet as it is returned, `study`), the normalize_ard() left out;
# the reader's error when they cannot be read
.sheets_read_back <- function(sp, id, more, study = NULL) {
  if (!inherits(sp, "tfl_table_spec")) sp <- tfl_table_spec()
  # (a cell as a workbook gives it back: trimmed, a blank one empty)
  for (nm in names(sp)) {
    d <- sp[[nm]]
    if (!is.data.frame(d) || !nrow(d)) next
    for (j in seq_along(d)) {
      if (!is.character(d[[j]])) next
      v <- trimws(d[[j]])
      v[!is.na(v) & !nzchar(v)] <- NA_character_
      d[[j]] <- v
    }
    if ("output_id" %in% names(d) && !is.na(id)) d$output_id[is.na(d$output_id)] <- id
    sp[[nm]] <- d
  }
  if (is.na(id) && !is.null(study)) sp$study <- study(sp$study)
  if (length(more)) {
    tb <- sp$tables
    if (!nrow(tb)) {
      tb[1L, ] <- NA
      tb$output_id <- id
    }
    if (!"content_more" %in% names(tb)) tb$content_more <- NA_character_
    tb$content_more[1L] <- .sheet_more_out(more, items = TRUE)
    sp$tables <- tb
  }
  back <- tryCatch(suppressMessages(suppressWarnings(
    .plan_content_from_sheets(sp, id, own = !is.na(id)))), error = function(e) e)
  if (inherits(back, "error")) return(back)
  Filter(function(it) !identical(it$fn, "normalize_ard"), back)
}

# A bridge's sheets as rows alone (no attributes, no row names): what two
# runs are compared by
.sheets_rows <- function(sp) {
  x <- unclass(sp)
  x <- lapply(x[sort(names(x))], function(d) {
    if (is.data.frame(d)) rownames(d) <- NULL
    d
  })
  x[!vapply(x, function(d) is.null(d) || (is.data.frame(d) && !nrow(d)), NA)]
}

# An item's code, the way the program writes it: the one form two items are
# compared in (a list or a vector, a guard's quotes, are all the same code).
# An item that cannot be written is its value: two of them are the same
# only when their values are
.sheets_code <- function(fn, args) {
  # (the same arguments are compared again each round, and in like reports:
  # kept for the session, a few thousand at most)
  key <- rlang::hash(list(fn, args))
  got <- .sheets_code_memo[[key]]
  if (!is.null(got)) return(got)
  if (length(.sheets_code_memo) > 5000L) rm(list = ls(.sheets_code_memo), envir = .sheets_code_memo)
  got <- .sheets_code_of(fn, args)
  assign(key, got, envir = .sheets_code_memo)
  got
}
.sheets_code_memo <- new.env(parent = emptyenv())
.sheets_code_of <- function(fn, args) {
  code <- tryCatch(paste(tfl_item_code(tfl_item(fn, args)), collapse = "\n"),
                   error = function(e) paste("<unwritable>", paste(deparse(args), collapse = "")))
  # (and the kinds of its values: a header row's `line` 1 and "1" write the
  # same code, but the file says a number or a text)
  paste(code, paste(.sheets_kinds(args), collapse = ""))
}
.sheets_kinds <- function(x) {
  if (is.list(x)) return(unlist(lapply(unname(unclass(x)), .sheets_kinds)))
  if (is.numeric(x)) "n" else if (is.character(x)) "c" else if (is.logical(x)) "l" else ""
}

# The items read back against the items as written, verb by verb: the
# arguments that do not come back marked (`marked`, the same cells) or
# kept apart (`held`), an item that cannot be put right by that `whole`,
# and what cannot be made right said.  `gone`: the items already kept
# whole.  A verb several items of which are written, or read back, is
# compared whole: any difference keeps them all whole.  `base`: the items
# the study sheet gives alone.
.sheets_holds <- function(items, back, held, marked, gone, cells_same, base = list(),
                          bridge = .plan_sheets_from_content) {
  notes <- character()
  whole <- gone
  fn_of <- function(x) vapply(x, function(it) it$fn, "")
  code_of <- function(x) vapply(x, function(it) .sheets_code(it$fn, it$args %||% list()), "")
  part <- function(a, named) {
    nm <- names(a) %||% rep("", length(a))
    if (named) a[nzchar(nm)] else unname(a[!nzchar(nm)])
  }
  # (each side as the program writes it: plan_cells() in the variables'
  # order, whatever the item's, .content_table_pipeline())
  prog <- function(x) {
    if (!length(x)) return(x)
    tryCatch(.content_cells_order(x, vapply(x, function(it) .schema_fn_name(it$fn), ""),
                                  function(items) bridge(items, NULL)),
             error = function(e) x)
  }
  items <- prog(items)
  back <- prog(back)
  fi <- fn_of(items)
  fb <- fn_of(back)
  # (a table_plan() the defaults do not write, read back with nothing in
  # it; an item the study sheet gives alone that the items do not have)
  drop <- fb == "table_plan" & !"table_plan" %in% fi &
    vapply(back, function(it) !length(it$args), NA)
  if (length(base) && length(back)) drop <- drop | (!fb %in% fi & code_of(back) %in% code_of(base))
  back <- back[!drop]
  fb <- fb[!drop]
  for (fn in unique(fi)) {
    io <- which(fi == fn)
    ib <- which(fb == fn)
    if (length(io) == 1L && length(ib) <= 1L) {
      a <- items[[io]]$args %||% list()
      b <- if (length(ib)) back[[ib]]$args %||% list() else list()
      # (the item as a whole first: what is the same in all is the most)
      if (length(ib) && identical(.sheets_code(fn, a), .sheets_code(fn, b))) next
      na <- part(a, TRUE)
      nb <- part(b, TRUE)
      # (named arguments by name, whatever their order; unnamed ones by place)
      bad <- names(na)[!vapply(names(na), function(k) identical(
        .sheets_code(fn, na[k]), if (k %in% names(nb)) .sheets_code(fn, nb[k]) else NA_character_), NA)]
      extra <- setdiff(names(nb), names(na))
      pos_same <- identical(.sheets_code(fn, part(a, FALSE)), .sheets_code(fn, part(b, FALSE)))
      # (all of them back, in another order, is the same: named arguments in
      # the sheets' order are the same call; a builder's edit adds its
      # argument last, and that is no reason to take the item off the sheets)
      if (!length(bad) && !length(extra) && pos_same) next
      if (gone[io]) {
        notes <- c(notes, .sheets_note(fn, names(na)))
        next
      }
      # (a mark that did not bring its argument back: the argument is held)
      undone <- intersect(bad, marked[[io]])
      new <- setdiff(bad, c(held[[io]], marked[[io]]))
      # (the arguments missing from what is read back first: they may be
      # what made the others differ, plan_labels(AGE = ) read into `variable`)
      missing <- intersect(new, setdiff(names(na), names(nb)))
      if (length(missing)) new <- missing
      # (nothing more to mark or keep apart, and still not the same: whole)
      if (!pos_same || (!length(new) && !length(undone))) {
        whole[io] <- TRUE
        next
      }
      if (length(undone)) {
        marked[[io]] <- setdiff(marked[[io]], undone)
        held[[io]] <- c(held[[io]], undone)
      }
      if (length(new)) {
        kept <- function(x) x[!names(x) %in% held[[io]]]
        if (length(ib) && cells_same(items[[io]]$fn, kept(a), kept(b), new)) {
          marked[[io]] <- c(marked[[io]], new)
        } else {
          held[[io]] <- c(held[[io]], new)
        }
      }
      next
    }
    same <- length(io) == length(ib) && all(vapply(seq_along(io), function(k) identical(
      .sheets_code(fn, items[[io[k]]]$args), .sheets_code(fn, back[[ib[k]]]$args)), NA))
    if (same) next
    if (!all(gone[io])) {
      whole[io] <- TRUE
      next
    }
    notes <- c(notes, .sheets_note(fn, unlist(lapply(items[io], function(it) names(it$args)))))
  }
  for (fn in setdiff(unique(fb), fi)) {
    notes <- c(notes, sprintf("%s(): the sheets give it, the report does not have it", fn))
  }
  list(held = held, marked = marked, whole = whole, notes = notes)
}

.sheets_note <- function(fn, args) {
  args <- unique(args[nzchar(args)])
  sprintf("%s(%s): does not come back from the sheets as written", fn, paste(args, collapse = ", "))
}

# ---- an argument on the sheets in another form: `!sheet` ---------------------

# content_more's mark of an argument whose value is on the sheets in
# another form (`lines: !sheet`): the reader takes it from the sheets' item.
# `shape`: what of the form the cells do not say -- a span's cells named
# line by line (`span: !sheet [.values, .values]`) -- never a value.
.sheet_mark <- function(shape = NULL) structure(list(shape = shape), class = "tfl_sheet")
.is_sheet_mark <- function(x) inherits(x, "tfl_sheet")
# (in YAML: `!sheet ''`, or the shape as a sequence)
.sheet_mark_out <- function(x) {
  if (is.null(x$shape)) structure("", tag = "!sheet") else structure(as.list(x$shape), tag = "!sheet")
}
.sheet_mark_in <- function(x) {
  v <- as.character(unlist(x))
  .sheet_mark(if (length(v) && !identical(v, "")) v)
}

# An argument's shape, by verb and argument (.sheet_shapes; NULL: none)
.sheet_shape <- function(fn, name, value) {
  f <- .sheet_shapes[[.schema_fn_name(fn)]][[name]]
  if (is.null(f)) NULL else f(value)
}
# (a span given line by line: each line's cells named "a | b", or "" for a
# line's one value)
.sheet_shapes <- list(
  plan_col_header = list(span = function(value) {
    if (!is.list(value) || inherits(value, c("tfl_r", "tfl_call"))) return(NULL)
    vapply(value, function(x) if (is.null(names(x))) "" else paste(names(x), collapse = " | "), "")
  }))

# The forms the bridge writes in another argument, read back as written,
# by verb: function(args, marked, shapes) gives the sheets' item's
# arguments with the marked ones in their own form (the inverse of the
# bridge's branch); `shapes` the marks' shapes, by argument
.sheet_forms <- list(
  # header `lines` (and `span`) are written as header rows (.lines_span())
  plan_col_header = function(args, marked, shapes) {
    # (header rows with a number for `line`, as a file writes it: the
    # sheets hold it as text)
    if ("header" %in% marked && is.list(args[["header"]])) {
      h <- args[["header"]]
      num <- function(v) if (length(v) == 1L && grepl("^[0-9]+$", v)) as.integer(v) else v
      if (is.data.frame(h)) {
        if (all(grepl("^[0-9]+$", h$line))) h$line <- as.integer(h$line)
      } else h <- lapply(h, function(r) { if (!is.null(r[["line"]])) r[["line"]] <- num(r[["line"]]); r })
      args[["header"]] <- h
    }
    if (!"lines" %in% marked) return(args)
    ln <- .header_as_lines(args[["header"]], shapes[["span"]])
    if (is.null(ln)) return(args)
    args[["header"]] <- NULL
    c(list(lines = ln$lines), if (!is.null(ln$span)) list(span = ln$span), args)
  },
  # a heading by its variable's name, plan_labels(AGE = ), is written as
  # the variables sheet's label, read as plan_labels(variable = )
  plan_labels = function(args, marked, shapes) {
    v <- args[["variable"]]
    for (k in setdiff(marked, "variable")) {
      if (!k %in% names(v)) next
      args[[k]] <- unname(unlist(v[[k]]))
      v <- v[names(v) != k]
    }
    if ("variable" %in% names(args)) {
      if (length(v)) args[["variable"]] <- v else args[["variable"]] <- NULL
    }
    args
  })

# The col_header sheet's rows (plan_col_header(header = )) as header lines:
# a line each row number, its cells by column (a blank text ""), and the
# span the `span` cells say (a blank cell "one"; all "each", no span: the
# default; with `shape`, line by line, each line's cells named as the shape
# names them, or its one value).  NULL when the rows say more than lines can.
.header_as_lines <- function(h, shape = NULL) {
  # (the rows as the reader gives them: a list of rows, or a data frame)
  if (is.list(h) && !is.data.frame(h) && length(h) &&
      all(vapply(h, function(r) is.list(r) && !is.null(names(r)), NA))) {
    keys <- unique(unlist(lapply(h, names)))
    h <- as.data.frame(lapply(stats::setNames(keys, keys), function(k) vapply(h, function(r) {
      v <- r[[k]]
      if (is.null(v) || length(v) != 1L) NA_character_ else as.character(v)
    }, "")), stringsAsFactors = FALSE)
  }
  if (!is.data.frame(h) || !nrow(h) || !all(c("line", "cols") %in% names(h)) ||
      !all(names(h) %in% c("line", "cols", "text", "span"))) return(NULL)
  ln <- as.character(h$line)
  u <- unique(ln)
  txt <- if ("text" %in% names(h)) as.character(h$text) else rep(NA_character_, nrow(h))
  txt[is.na(txt)] <- ""
  sp <- if ("span" %in% names(h)) as.character(h$span) else rep(NA_character_, nrow(h))
  sp[is.na(sp)] <- "one"
  cols <- as.character(h$cols)
  lines <- lapply(u, function(l) stats::setNames(as.list(txt[ln == l]), cols[ln == l]))
  # (a line's span: its one value, or its cells by name)
  per_line <- function(l, named = NULL) {
    s <- stats::setNames(sp[ln == l], cols[ln == l])
    if (is.null(named) && length(unique(s)) == 1L) return(s[[1L]])
    if (is.null(named)) return(as.list(s))
    # (the cells the shape names, and any other cell an edit made not "each")
    as.list(s[names(s) %in% named | s != "each"])
  }
  if (length(shape) && length(shape) == length(u)) {
    span <- lapply(seq_along(u), function(i) {
      per_line(u[i], if (nzchar(shape[i])) strsplit(shape[i], " | ", fixed = TRUE)[[1L]])
    })
    return(list(lines = lines, span = span))
  }
  span <- if (all(sp == "each")) NULL else if (length(unique(sp)) == 1L) sp[[1L]] else {
    lapply(u, function(l) {
      s <- sp[ln == l]
      if (length(unique(s)) == 1L) s[[1L]] else stats::setNames(as.list(s), cols[ln == l])
    })
  }
  list(lines = lines, span = span)
}

# content_more's marks put on the sheets' items: each marked argument read
# from the sheets' item of its verb in its own form, and the mark taken out
# of content_more (with no sheets' item -- its rows deleted -- the
# argument is gone, as an emptied grid says)
.sheet_marks_take <- function(items, more) {
  fn_of <- function(x) vapply(x, function(it) it$fn, "")
  keep <- rep(TRUE, length(more))
  for (m in seq_along(more)) {
    a <- more[[m]]$args
    if (!length(a)) next
    mk <- names(a)[vapply(a, .is_sheet_mark, NA)]
    if (!length(mk)) next
    shapes <- lapply(a[mk], function(x) x$shape)
    fn <- more[[m]]$fn
    ix <- which(fn_of(items) == fn)
    form <- .sheet_forms[[.schema_fn_name(fn)]]
    if (length(ix) == 1L && !is.null(form)) items[[ix]]$args <- form(items[[ix]]$args %||% list(), mk, shapes)
    more[[m]]$args <- a[!names(a) %in% mk]
    keep[m] <- length(more[[m]]$args) > 0L
  }
  list(items = items, more = more[keep])
}

# The bridge with its results kept for one tfl_sheets_from_spec() call: the
# check runs it on the same items again (each round, each report's like
# items, .content_cells_order()), and a run is the same for the same items
# and report.  An error is kept too, and given again.
.bridge_cached <- function(bridge) {
  force(bridge)
  memo <- new.env(parent = emptyenv())
  function(items, output_id = NULL) {
    key <- rlang::hash(list(items, output_id))
    got <- memo[[key]]
    if (is.null(got)) {
      got <- tryCatch(list(value = bridge(items, output_id)), error = function(e) list(error = e))
      memo[[key]] <- got
    }
    if (!is.null(got$error)) stop(got$error)
    got$value
  }
}
