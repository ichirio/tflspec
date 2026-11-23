# ---- a table's items on the sheets, read back and checked (#689) ------------
#
# tfl_sheets_from_spec() puts a table's items on the sheets through the
# bridge (.plan_sheets_from_content()).  What the sheets cannot say is kept
# as YAML in the tables row's content_more (#448).  Which arguments those
# are is not a list of names: every item is written, read back as the
# converter reads it (.plan_content_from_sheets(), content_more put with
# it), and compared with the item as written, argument by argument, as the
# code each makes.  An argument that does not come back the same -- not
# written, written in another form (plan_labels(AGE = ) read back as
# plan_labels(variable = ), header `lines` as header rows), a text a cell
# does not keep -- is the item's rest in content_more, and the arguments
# that come back stay on the sheets; read back, the two are one item again.
# An item that still does not come back (its unnamed arguments, an argument
# the sheets make up) is kept whole.  What cannot be made right is said, by
# report and verb.

# The sheets of a table's items and what goes to content_more.  `items` the
# items without normalize_ard() (`nz`, which the bridge reads for its
# stack), `id` the report (NA: the defaults), `bridge` the bridge.  Returns
# list(sp, more, notes): the bridge's sheets, the items for content_more,
# and the bridge's notes with the check's.
.sheets_said <- function(items, nz, id, bridge) {
  oid <- if (is.na(id)) NULL else id
  n <- length(items)
  # each item's arguments kept apart (its rest), and the items kept whole
  held <- rep(list(character()), n)
  whole <- rep(FALSE, n)
  cut_item <- function(i) {
    it <- items[[i]]
    if (length(held[[i]])) it$args <- it$args[!names(it$args) %in% held[[i]]]
    it
  }
  repeat {
    keep <- which(!whole)
    sp <- list()
    # the bridge refuses an item (a stop), or cannot say it (not_converted_at):
    # that item goes whole, and the bridge is run again on the rest
    while (length(keep)) {
      sp <- tryCatch(bridge(c(nz, lapply(keep, cut_item)), oid), error = function(e) e)
      if (inherits(sp, "error")) {
        refused <- vapply(keep, function(k) inherits(tryCatch(
          bridge(c(nz, list(cut_item(k))), oid), error = function(e) e), "error"), NA)
        if (!any(refused)) stop(sp)
        keep <- keep[!refused]
        sp <- list()
        next
      }
      out <- attr(sp, "not_converted_at") %||% integer()
      if (!length(out)) break
      keep <- keep[-out]
      sp <- list()
    }
    gone <- !seq_len(n) %in% keep
    # (the rest the bridge keeps apart itself joins the item's own: one item
    # of the verb in content_more, its arguments in the item's order)
    rest <- held
    other <- list()
    fns <- vapply(items, function(it) it$fn, "")
    for (r in attr(sp, "not_converted_rest")) {
      i <- which(fns == r$fn & !gone)
      if (length(i) == 1L) rest[[i]] <- union(rest[[i]], names(r$args)) else other <- c(other, list(r))
    }
    more <- list()
    for (i in seq_len(n)) {
      if (gone[i]) {
        more[[length(more) + 1L]] <- items[[i]]
      } else if (length(rest[[i]])) {
        a <- items[[i]]$args
        more[[length(more) + 1L]] <- tfl_item(items[[i]]$fn, a[names(a) %in% rest[[i]]])
      }
    }
    more <- c(more, other)
    back <- .sheets_read_back(sp, id, more)
    # (sheets that cannot be read back: every item kept whole, said)
    if (is.null(back)) {
      if (all(whole)) break
      whole[] <- TRUE
      next
    }
    chk <- .sheets_holds(items, back, held, gone)
    notes <- chk$notes
    if (identical(chk$held, held) && identical(chk$whole, whole | gone)) break
    held <- chk$held
    whole <- chk$whole
  }
  notes <- c(attr(sp, "not_converted") %||% character(),
             if (is.null(back)) "its items could not be read back from the sheets: kept whole, as YAML"
             else notes)
  list(sp = sp, more = more, notes = notes)
}

# The items the converter reads from these sheets and content_more
# (tfl_yaml_from_sheets(): a report's own rows, or the defaults'), the
# normalize_ard() left out; NULL when they cannot be read
.sheets_read_back <- function(sp, id, more) {
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
    .plan_content_from_sheets(sp, id, own = !is.na(id)))), error = function(e) NULL)
  if (is.null(back)) return(NULL)
  Filter(function(it) !identical(it$fn, "normalize_ard"), back)
}

# An item's code, the way the program writes it: the one form two items are
# compared in (a list or a vector, a guard's quotes, are all the same code)
.sheets_code <- function(fn, args) {
  tryCatch(paste(tfl_item_code(tfl_item(fn, args)), collapse = "\n"),
           error = function(e) paste("<unwritable>", conditionMessage(e)))
}

# The items read back against the items as written, verb by verb: the
# arguments that do not come back added to `held`, an item that cannot be
# put right by that `whole`, and what cannot be made right said.  `gone`:
# the items already kept whole.  A verb several items of which are written,
# or read back, is compared whole: any difference keeps them all whole.
.sheets_holds <- function(items, back, held, gone) {
  notes <- character()
  whole <- gone
  fn_of <- function(x) vapply(x, function(it) it$fn, "")
  part <- function(a, named) {
    nm <- names(a) %||% rep("", length(a))
    if (named) a[nzchar(nm)] else unname(a[!nzchar(nm)])
  }
  fi <- fn_of(items)
  fb <- fn_of(back)
  # (a table_plan() the defaults do not write, read back with nothing in it)
  drop <- fb == "table_plan" & !"table_plan" %in% fi &
    vapply(back, function(it) !length(it$args), NA)
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
      # (all of them back, in another order: the program writes them in the
      # item's order, so from the first out of place they are the rest, put
      # after the others when read back)
      if (!length(bad) && !length(extra) && pos_same && !identical(names(na), names(nb))) {
        at <- which(names(na) != names(nb))[1L]
        bad <- names(na)[seq.int(at, length(na))]
      }
      if (!length(bad) && !length(extra) && pos_same) next
      if (gone[io]) {
        notes <- c(notes, .sheets_note(fn, names(na)))
        next
      }
      new <- setdiff(bad, held[[io]])
      # (nothing more to keep apart, and still not the same: whole)
      if (length(new) && pos_same) held[[io]] <- c(held[[io]], new) else whole[io] <- TRUE
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
  list(held = held, whole = whole, notes = notes)
}

.sheets_note <- function(fn, args) {
  args <- unique(args[nzchar(args)])
  sprintf("%s(%s): does not come back from the sheets as written", fn, paste(args, collapse = ", "))
}
