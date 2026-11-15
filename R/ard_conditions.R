# ---- an analysis item: its params, conditions and calls ---------------------
#
# An item of the ARD runs one process -- a filter (`where`), new columns
# (`derive`), the values' levels (`levels`) and one or more calls (`ards`:
# the item's own fields, or several rows of the analyses sheet with the same
# id, one after another) -- once for each row of its conditions table.  The
# process declares the parameters it takes (`params`); the table
# (the `conditions` sheet, one cell a row and param; YAML `conditions:`, a
# map a row) gives their values, and the process writes `{name}` where it
# uses one.  The program writes the table (`ard_<id>_conditions`), the
# process as a function of one row (`ard_<id>_one`, the params its
# arguments), and runs it on every row:
#   ard_<id> <- purrr::pmap(ard_<id>_conditions, ard_<id>_one) |> dplyr::bind_rows()
# A `{name}` becomes, by the field:
#   by, variables, strata (columns)       dplyr::all_of(name)
#   where, derive, post (data masking)    !!name
#   levels, args, denominator, code       name
# and a text "{name}" in R the value itself; "a {name} b" paste0().

# the fields of one call (an `ards:` element's), the rest the item's
.ard_call_fields <- c("method", "by", "overall", "strata", "variables", "optional",
                      "statistics", "denominator", "formats", "args", "code")

# the analyses sheet's fields an item writes on its first row only
.ard_item_fields <- c("label", "parent", "data", "dataset", "records", "population_id",
                      "where", "derive", "levels", "params", "groups", "post",
                      "purpose", "reason")

.cond_brace_rx <- "\\{\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*\\}"

# the rows of each item: consecutive rows of one output_id / analysis_id
.ard_runs <- function(a) {
  n <- nrow(a)
  if (!n) return(integer())
  key <- paste(a$output_id, a$analysis_id, sep = "\r")
  cumsum(c(TRUE, key[-1L] != key[-n]))
}

.cond_params <- function(r) .split_bar(r$params %||% NA)

# Is this run of rows an item the program writes as one process?
.ard_is_item <- function(ra) {
  nrow(ra) > 1L || length(.cond_params(ra[1L, ])) > 0L ||
    !is.na(ra$derive[1L] %||% NA) || !is.na(ra$levels[1L] %||% NA)
}

# ---- the values -------------------------------------------------------------

# A cell of the conditions sheet: `!r <R>` R (a constant, written as it is,
# or an expression, written quote()), a | b a vector, else a text
.cond_value <- function(cell) {
  if (is.null(cell) || is.na(cell)) return(list(kind = "na"))
  cell <- trimws(cell)
  if (grepl("^!r(\\s|$)", cell)) {
    txt <- trimws(sub("^!r", "", cell))
    e <- tryCatch(str2lang(txt), error = function(err) err)
    if (inherits(e, "error") || !nzchar(txt)) return(list(kind = "bad", text = txt))
    const <- !length(all.vars(e)) &&
      all(all.names(e) %in% c("c", "-", ":", "TRUE", "FALSE", "NA", "NULL"))
    return(list(kind = if (const) "const" else "expr", text = .expr_text(e), expr = e))
  }
  v <- .split_bar(cell)
  if (length(v) > 1L) return(list(kind = "vec", value = trimws(v)))
  list(kind = "str", value = cell)
}

# a value as a cell of the tribble
.cond_r <- function(v) {
  switch(v$kind,
         str = .q(v$value),
         vec = .lay(.cl("c", as.list(.q(v$value))), width = Inf),
         const = v$text,
         expr = paste0("quote(", v$text, ")"),
         "NA")
}

# the text values of a param over the rows (what a column field names)
.cond_union <- function(conds, p) {
  unique(unlist(lapply(conds, function(r) {
    v <- .cond_value(r[[p]] %||% NA)
    switch(v$kind, str = v$value, vec = v$value, NULL)
  })))
}

# A YAML value as a cell: R `!r ...`, a number or TRUE/FALSE `!r 0.9`, a
# sequence a | b
.cond_cell <- function(v) {
  if (is.null(v)) return(NA_character_)
  if (inherits(v, "tfl_r")) return(paste("!r", as.character(v)))
  if (is.list(v)) v <- unlist(v)
  if (!length(v)) return(NA_character_)
  if (is.numeric(v) || is.logical(v)) {
    txt <- if (length(v) == 1L) .expr_text(v[[1L]]) else .expr_text(v)
    return(paste("!r", txt))
  }
  paste(as.character(v), collapse = " | ")
}

# ... and back: a cell as the YAML value
.cond_yaml <- function(cell) {
  v <- .cond_value(cell)
  switch(v$kind,
         na = NULL,
         str = v$value,
         vec = as.list(v$value),
         const = {
           x <- eval(v$expr, baseenv())
           if (length(x) == 1L) x else as.list(x)
         },
         tfl_r(v$text))
}

# The rows of an item's conditions (the sheet's rows of it, by `row`): a
# named list of cells each
.cond_rows <- function(x, output_id, analysis_id) {
  cs <- x$conditions
  if (is.null(cs) || !nrow(cs)) return(list())
  k <- which(cs$output_id %in% output_id & cs$analysis_id %in% analysis_id)
  if (!length(k)) return(list())
  rn <- suppressWarnings(as.integer(cs$row[k]))
  rn[is.na(rn)] <- 0L
  lapply(split(k, factor(rn, sort(unique(rn)))), function(j) {
    stats::setNames(as.list(cs$value[j]), cs$param[j])
  })
}

# ---- the levels cell --------------------------------------------------------

# `levels:` {VAR: [values]} or {VAR: "{param}"} as the cell `VAR = a | b;
# VAR2 = {param}`, and back
.levels_cell <- function(v) {
  if (is.null(v) || inherits(v, "tfl_r") || is.character(v)) return(v)
  if (!is.list(v) || is.null(names(v))) return(NA_character_)
  paste(sprintf("%s = %s", names(v), vapply(v, function(z)
    paste(as.character(unlist(z)), collapse = " | "), "")), collapse = "; ")
}

.levels_parse <- function(cell) {
  if (is.null(cell) || is.na(cell) || !nzchar(trimws(cell))) return(list())
  parts <- trimws(strsplit(cell, ";", fixed = TRUE)[[1L]])
  parts <- parts[nzchar(parts)]
  nm <- trimws(sub("=.*$", "", parts))
  val <- lapply(sub("^[^=]*=", "", parts), function(s) trimws(.split_bar(trimws(s))))
  stats::setNames(val, nm)
}

.levels_yaml <- function(cell) {
  lv <- .levels_parse(cell)
  lapply(lv, function(v) if (length(v) == 1L && grepl(paste0("^", .cond_brace_rx, "$"), v))
    v else as.list(v))
}

# ---- {name} ----------------------------------------------------------------

# R text in its pieces: string literals and the code between them
.r_pieces <- function(txt) {
  m <- gregexpr("\"(?:[^\"\\\\]|\\\\.)*\"|'(?:[^'\\\\]|\\\\.)*'", txt, perl = TRUE)[[1L]]
  if (m[1L] == -1L) return(list(list(lit = FALSE, text = txt)))
  ln <- attr(m, "match.length")
  out <- list()
  at <- 1L
  for (k in seq_along(m)) {
    if (m[k] > at) out[[length(out) + 1L]] <- list(lit = FALSE, text = substr(txt, at, m[k] - 1L))
    out[[length(out) + 1L]] <- list(lit = TRUE, text = substr(txt, m[k], m[k] + ln[k] - 1L))
    at <- m[k] + ln[k]
  }
  if (at <= nchar(txt)) out[[length(out) + 1L]] <- list(lit = FALSE, text = substr(txt, at, nchar(txt)))
  out
}

.brace_names <- function(s) {
  m <- regmatches(s, gregexpr(.cond_brace_rx, s, perl = TRUE))[[1L]]
  unique(gsub("[{}[:space:]]", "", m))
}

# The {name}s R text uses: in its code any (a block of one name is no R a
# person writes), in its strings the params only (a glue text may have its
# own)
.r_used <- function(txt, params) {
  if (is.null(txt) || is.na(txt)) return(character())
  p <- .r_pieces(txt)
  unique(c(unlist(lapply(p, function(z) if (!z$lit) .brace_names(z$text))),
           intersect(unlist(lapply(p, function(z) if (z$lit) .brace_names(z$text))), params)))
}

# R text with its params in: `inject` !!name (data masking), else name
.cond_subst_r <- function(txt, params, inject) {
  if (is.null(txt) || is.na(txt) || !length(params)) return(txt)
  ref <- function(p) if (inject) paste0("!!", p) else p
  rx <- paste0("\\{\\s*(", paste(gsub(".", "\\.", params, fixed = TRUE), collapse = "|"),
               ")\\s*\\}")
  out <- vapply(.r_pieces(txt), function(z) {
    if (!z$lit) return(gsub(rx, if (inject) "!!\\1" else "\\1", z$text, perl = TRUE))
    if (!grepl(rx, z$text, perl = TRUE)) return(z$text)
    q <- substr(z$text, 1L, 1L)
    body <- substr(z$text, 2L, nchar(z$text) - 1L)
    m <- gregexpr(rx, body, perl = TRUE)[[1L]]
    ln <- attr(m, "match.length")
    parts <- character()
    at <- 1L
    for (k in seq_along(m)) {
      if (m[k] > at) parts <- c(parts, paste0(q, substr(body, at, m[k] - 1L), q))
      parts <- c(parts, ref(gsub("[{}[:space:]]", "", substr(body, m[k], m[k] + ln[k] - 1L))))
      at <- m[k] + ln[k]
    }
    if (at <= nchar(body)) parts <- c(parts, paste0(q, substr(body, at, nchar(body)), q))
    if (length(parts) == 1L) parts else sprintf("paste0(%s)", paste(parts, collapse = ", "))
  }, "")
  paste(out, collapse = "")
}

# a column field (by, variables, strata) with its params in: `{name}`
# dplyr::all_of(name)
.cond_subst_cols <- function(cell, params) {
  if (is.null(cell) || is.na(cell) || !length(params)) return(cell)
  v <- .split_bar(cell)
  rx <- paste0("^", .cond_brace_rx, "$")
  v <- vapply(v, function(s) {
    s <- trimws(s)
    if (grepl(rx, s, perl = TRUE) && .brace_names(s) %in% params) {
      sprintf("dplyr::all_of(%s)", .brace_names(s))
    } else s
  }, "", USE.NAMES = FALSE)
  paste(v, collapse = " | ")
}

# ---- the sheets: an item's rows -----------------------------------------------

# One YAML item as the analyses sheet's rows (its `ards:` a row each, the
# item's fields on the first, each row its call's fields with the item's
# defaults) and the conditions sheet's rows
.ard_item_sheet_rows <- function(a, id, cell) {
  one <- function(a) {
    if (!is.null(a$denominator)) a$denominator <- .den_cell(a$denominator)
    # (`by: []`: no columns, said so)
    if ("by" %in% names(a) && !length(a$by)) a$by <- tfl_unset()
    if (!is.null(a$levels)) a$levels <- .levels_cell(a$levels)
    d <- a$derive
    if (is.list(d) && length(d) && !is.null(names(d)) && !inherits(d, "tfl_r")) {
      a$derive <- paste(sprintf("%s = %s", names(d), vapply(d, as.character, "")),
                        collapse = " | ")
    }
    m <- lapply(a, cell)
    m$analysis_id <- m$id
    m$id <- NULL
    if (!is.null(m$population)) {
      m$population_id <- m$population
      m$population <- NULL
    }
    c(list(output_id = id), m)
  }
  conds <- a$conditions
  ards <- a$ards
  a$conditions <- NULL
  a$ards <- NULL
  if (!length(ards)) {
    rows <- list(one(a))
  } else {
    base <- a[intersect(names(a), .ard_call_fields)]
    rows <- lapply(seq_along(ards), function(k) {
      e <- ards[[k]]
      if (!is.list(e)) e <- list()
      bad <- setdiff(names(e), .ard_call_fields)
      if (length(bad)) {
        stop(sprintf(paste("%s / %s: `ards:` element %d has %s: an element is one call's",
                           "fields (%s); `post` and the rest are the item's"),
                     id, a$id %||% "?", k, paste0("`", bad, "`", collapse = ", "),
                     paste(.ard_call_fields, collapse = ", ")), call. = FALSE)
      }
      m <- base
      for (nm in names(e)) m[nm] <- list(e[[nm]])
      own <- if (k == 1L) a[setdiff(names(a), .ard_call_fields)] else list(id = a$id)
      one(c(own, m))
    })
  }
  cr <- list()
  for (k in seq_along(conds)) {
    rk <- conds[[k]]
    for (p in names(rk)) {
      cr[[length(cr) + 1L]] <- list(output_id = id, analysis_id = cell(a$id), row = as.character(k),
                                    param = p, value = .cond_cell(rk[[p]]))
    }
  }
  list(analyses = rows, conditions = cr)
}

# The YAML items of one report from its analyses rows (.conv_rows()'s maps,
# a row each) and the conditions sheet: a run of rows of one id is an item
# with `ards:` (a call's field the same in every row the item's, the rest
# each element's), its conditions a map a row
.ard_items_yaml <- function(items, an, cs) {
  if (!length(items)) return(items)
  run <- .ard_runs(an)
  out <- list()
  x <- list(conditions = cs)
  for (r in unique(run)) {
    k <- which(run == r)
    first <- items[[k[1L]]]
    if (!is.null(first$levels)) first$levels <- .levels_yaml(as.character(first$levels))
    if (!is.null(first$params)) first$params <- as.list(unlist(first$params))
    item <- first
    if (length(k) > 1L) {
      els <- lapply(items[k], function(m) m[intersect(names(m), .ard_call_fields)])
      common <- Filter(function(f) all(vapply(els, function(e)
        !is.null(e[[f]]) && identical(e[[f]], els[[1L]][[f]]), NA)), .ard_call_fields)
      item <- first[setdiff(names(first), setdiff(.ard_call_fields, common))]
      item$ards <- lapply(els, function(e) {
        e <- e[setdiff(names(e), common)]
        if (!length(e)) stats::setNames(list(), character()) else e
      })
    }
    rows <- .cond_rows(x, an$output_id[k[1L]], an$analysis_id[k[1L]])
    if (length(rows)) {
      pp <- unlist(first$params)
      item$conditions <- lapply(rows, function(rw) {
        nm <- c(intersect(pp, names(rw)), setdiff(names(rw), pp))
        m <- lapply(rw[nm], .cond_yaml)
        m[!vapply(m, is.null, NA)]
      })
      names(item$conditions) <- NULL
    }
    out[[length(out) + 1L]] <- item
  }
  out
}

# ---- the analyses as the checks and the data read them ------------------------

# The analyses sheet with each item's fields on all its rows, and a
# {param} of a column field the values the conditions give it (what the
# data must have); an item with conditions filters inside its process, so
# its `where` is not the data's
.ard_params_view <- function(a, x) {
  if (is.null(a) || !nrow(a)) return(a)
  run <- .ard_runs(a)
  first <- match(run, run)
  for (cn in intersect(setdiff(.ard_item_fields, c("post", "parent")), names(a))) {
    a[[cn]] <- a[[cn]][first]
  }
  if (is.null(a$params)) return(a)
  for (i in which(!is.na(a$params))) {
    pp <- .cond_params(a[i, ])
    conds <- .cond_rows(x, a$output_id[i], a$analysis_id[i])
    for (cn in intersect(c("by", "variables", "strata"), names(a))) {
      if (is.na(a[[cn]][i])) next
      v <- unlist(lapply(trimws(.split_bar(a[[cn]][i])), function(s) {
        b <- .brace_names(s)
        if (length(b) == 1L && grepl(paste0("^", .cond_brace_rx, "$"), s) && b %in% pp) {
          .cond_union(conds, b)
        } else s
      }))
      a[[cn]][i] <- if (length(v)) paste(unique(v), collapse = " | ") else NA_character_
    }
    a$where[i] <- NA_character_
  }
  a
}

# ---- the checks ---------------------------------------------------------------

.cond_r_fields <- c(where = "mask", derive = "mask", post = "mask", args = "plain",
                    denominator = "plain", code = "plain")
.cond_col_fields <- c("by", "variables", "strata")

# Every problem of the items' params, conditions and calls (stops)
.ard_conditions_problems <- function(x, a) {
  pr <- list(.spec_problems_empty())
  add <- function(i, field, message) {
    pr[[length(pr) + 1L]] <<- .ard_problem(a, i, field, message)
  }
  if (is.null(a) || !nrow(a)) return(.spec_problems_empty())
  for (cn in setdiff(c(.ard_item_fields, .ard_call_fields), names(a))) a[[cn]] <- NA_character_
  tag <- function(i) paste(a$output_id[i], a$analysis_id[i], sep = " / ")
  run <- .ard_runs(a)
  key <- paste(a$output_id, a$analysis_id, sep = "\r")
  # (one id, two places: not one item)
  twice <- tapply(run, key, function(z) length(unique(z)))
  for (kk in names(twice)[twice > 1L]) {
    i <- which(key == kk)[1L]
    add(i, "analysis_id", sprintf(paste("output_id / analysis_id repeated: %s (the rows of an",
                                        "item's ards are one after another)"),
                                  sub("\r", " ", kk, fixed = TRUE)))
  }
  keys <- tfl_ard_methods()
  cs <- x$conditions
  # conditions of no item
  if (!is.null(cs) && nrow(cs)) {
    lost <- which(!paste(cs$output_id, cs$analysis_id, sep = "\r") %in% key)
    for (j in utils::head(lost, 1L)) {
      pr[[length(pr) + 1L]] <- .spec_problem(sprintf(
        "`conditions`: %s / %s is not an analysis", cs$output_id[j], cs$analysis_id[j]),
        "conditions", cs$output_id[j], cs$analysis_id[j] %||% "", "analysis_id")
    }
    badrow <- which(is.na(suppressWarnings(as.integer(cs$row))) | is.na(cs$param))
    for (j in utils::head(badrow, 1L)) {
      pr[[length(pr) + 1L]] <- .spec_problem(sprintf(
        "`conditions`: %s / %s: each row has its `row` (1, 2, ...) and `param`",
        cs$output_id[j], cs$analysis_id[j]),
        "conditions", cs$output_id[j], cs$analysis_id[j] %||% "", "row")
    }
  }
  for (r in unique(run)) {
    k <- which(run == r)
    i <- k[1L]
    pp <- .cond_params(a[i, ])
    conds <- .cond_rows(x, a$output_id[i], a$analysis_id[i])
    # item fields on the first row only
    if (length(k) > 1L) {
      for (j in k[-1L]) {
        set <- .ard_item_fields[!is.na(unlist(a[j, .ard_item_fields]))]
        if (length(set)) add(j, set[1L], sprintf(paste(
          "%s: %s on a row after an item's first: the item's fields (%s) are on",
          "its first row only"), tag(i), paste0("`", set, "`", collapse = ", "),
          paste(.ard_item_fields, collapse = ", ")))
      }
      if (any(!is.na(a$parent[k])) || any(!is.na(a$parent) & a$parent == a$analysis_id[i] &
                                         a$output_id == a$output_id[i])) {
        add(i, "parent", sprintf("%s: an item of several calls (ards) is not a parent or a child",
                                 tag(i)))
      }
    }
    if (length(pp) && !is.na(a$groups[i])) {
      add(i, "groups", sprintf("%s: `groups` and `params` / `conditions` together: write the groups as conditions",
                               tag(i)))
    }
    for (j in k) {
      m <- .method_key(a$method[j], keys)
      fn <- if (is.na(m)) a$method[j] else keys$call[m]
      if (identical(fn, "(subjects)") && (length(pp) || length(k) > 1L)) {
        add(j, "method", sprintf("%s: a `subjects` analysis is not run in an item's process (params, ards)",
                                 tag(i)))
      }
      if (identical(fn, "(code)") && (length(pp) || length(k) > 1L) && !is.na(a$code[j]) &&
          is.null(.code_plain_single(a$code[j]))) {
        add(j, "code", sprintf("%s: the `code` of an item's call is one R expression (its ARD)",
                               tag(i)))
      }
    }
    if (!length(pp)) {
      if (length(conds)) add(i, "params", sprintf(paste(
        "%s: `conditions` without `params`: declare the parameters the process takes",
        "(`params: [%s]`)"), tag(i), paste(unique(unlist(lapply(conds, names))), collapse = ", ")))
      # {name} in a column field or levels: a param no one declared
      for (j in k) for (cn in c(.cond_col_fields, if (j == i) "levels")) {
        b <- .brace_names(a[[cn]][j] %||% "")
        if (length(b) && !is.na(a[[cn]][j])) add(j, cn, sprintf(
          "%s: `%s` uses {%s}, but the item declares no `params`", tag(i), cn, b[1L]))
      }
      next
    }
    okname <- grepl("^[A-Za-z][A-Za-z0-9_]*$", pp) & !pp %in% c("data", .r_reserved()) &
      make.names(pp) == pp
    if (any(!okname)) add(i, "params", sprintf(paste(
      "%s: params %s: a param is an R name (a letter first, no dot first), not `data`",
      "or an R word"), tag(i), paste(pp[!okname], collapse = ", ")))
    if (anyDuplicated(pp)) add(i, "params", sprintf("%s: params repeated: %s", tag(i),
                                                     paste(unique(pp[duplicated(pp)]), collapse = ", ")))
    if (!length(conds)) {
      add(i, "params", sprintf("%s: `params` without `conditions`: give the rows (one a run)", tag(i)))
      next
    }
    # each row gives each param; a column is R or values, not both
    kinds <- list()
    for (rr in seq_along(conds)) {
      miss <- setdiff(pp, names(conds[[rr]])[!is.na(unlist(conds[[rr]]))])
      if (length(miss)) add(i, "conditions", sprintf("%s: condition %d gives no %s",
                                                     tag(i), rr, paste(miss, collapse = ", ")))
      for (p in intersect(pp, names(conds[[rr]]))) {
        v <- .cond_value(conds[[rr]][[p]])
        if (identical(v$kind, "bad")) add(i, "conditions", sprintf(
          "%s: condition %d, %s: `!r %s` is not R", tag(i), rr, p, v$text))
        kinds[[p]] <- c(kinds[[p]], v$kind)
      }
    }
    expr_p <- names(kinds)[vapply(kinds, function(z) "expr" %in% z, NA)]
    for (p in expr_p) {
      if (!all(kinds[[p]] %in% c("expr", "const", "na"))) add(i, "conditions", sprintf(
        "%s: param %s is R (!r) in some conditions and a value in others", tag(i), p))
    }
    # the {name}s the process uses
    used <- character()
    for (j in k) {
      for (cn in .cond_col_fields) {
        if (is.na(a[[cn]][j])) next
        for (s in trimws(.split_bar(a[[cn]][j]))) {
          b <- .brace_names(s)
          if (!length(b)) next
          used <- c(used, b)
          if (!grepl(paste0("^", .cond_brace_rx, "$"), s)) add(j, cn, sprintf(
            "%s: `%s` has %s: a column is a whole {param} (give the full name as the param's value)",
            tag(i), cn, s))
          if (any(b %in% expr_p)) add(j, cn, sprintf(
            "%s: `%s` uses {%s}, an R expression (!r): a column field takes names", tag(i), cn,
            intersect(b, expr_p)[1L]))
        }
      }
      for (cn in names(.cond_r_fields)) {
        if (j != i && cn %in% .ard_item_fields) next
        u <- .r_used(a[[cn]][j], pp)
        used <- c(used, u)
        if (identical(.cond_r_fields[[cn]], "plain") && any(u %in% expr_p) && cn != "code") {
          add(j, cn, sprintf(paste(
            "%s: `%s` uses {%s}, an R expression (!r): an expression goes in where, derive",
            "or post (data masking)"), tag(i), cn, intersect(u, expr_p)[1L]))
        }
      }
    }
    lv <- .levels_parse(a$levels[i] %||% NA)
    for (v in lv) {
      b <- unlist(lapply(v, .brace_names))
      used <- c(used, b)
      if (any(b %in% expr_p)) add(i, "levels", sprintf(
        "%s: `levels` uses {%s}, an R expression (!r): levels take values", tag(i),
        intersect(b, expr_p)[1L]))
    }
    used <- unique(used)
    unknown <- setdiff(used, pp)
    if (length(unknown)) add(i, "params", sprintf(paste(
      "%s: the process uses %s, not a param of the item (params: %s)"), tag(i),
      paste0("{", unknown, "}", collapse = ", "), paste(pp, collapse = ", ")))
  }
  do.call(rbind, pr)
}

# What the items' params and conditions may mean that the writer did not
# (warnings): a row's key not a param, a `.` key (tflspec's own, later), a
# param the process does not use, a blank value of a by column
.ard_conditions_warnings <- function(x, a) {
  if (is.null(a) || !nrow(a) || is.null(a$params)) return(character())
  out <- character()
  run <- .ard_runs(a)
  for (r in unique(run)) {
    k <- which(run == r)
    i <- k[1L]
    pp <- .cond_params(a[i, ])
    if (!length(pp)) next
    tag <- paste(a$output_id[i], a$analysis_id[i], sep = " / ")
    conds <- .cond_rows(x, a$output_id[i], a$analysis_id[i])
    keys <- unique(unlist(lapply(conds, names)))
    dot <- keys[startsWith(keys, ".")]
    other <- setdiff(keys, c(pp, dot))
    if (length(other)) out <- c(out, sprintf(
      "%s: conditions give %s, not a param: left out of the program's table", tag,
      paste(other, collapse = ", ")))
    if (length(dot)) out <- c(out, sprintf(
      "%s: conditions give %s: a key starting with `.` is tflspec's own (none yet), ignored",
      tag, paste(dot, collapse = ", ")))
    used <- character()
    by_used <- character()
    for (j in k) {
      for (cn in .cond_col_fields) {
        b <- .brace_names(a[[cn]][j] %||% "")
        used <- c(used, b)
        if (cn == "by") by_used <- c(by_used, b)
      }
      for (cn in names(.cond_r_fields)) used <- c(used, .r_used(a[[cn]][j] %||% NA, pp))
    }
    used <- c(used, unlist(lapply(.levels_parse(a$levels[i] %||% NA), function(v)
      unlist(lapply(v, .brace_names)))))
    # (a derived column a by names: its value's blank is NA, which cards drops)
    dv <- .derive_names(a$derive[i] %||% NA)
    for (d in intersect(.split_bar(paste(a$by[k], collapse = " | ")), dv)) {
      src <- .brace_names(a$derive[i])
      by_used <- c(by_used, src)
    }
    unused <- setdiff(pp, used)
    if (length(unused)) out <- c(out, sprintf("%s: param %s is not used by the process",
                                              tag, paste(unused, collapse = ", ")))
    for (p in intersect(unique(by_used), pp)) {
      blank <- vapply(conds, function(rw) {
        v <- .cond_value(rw[[p]] %||% NA)
        identical(v$kind, "str") && !nzchar(trimws(v$value))
      }, NA)
      if (any(blank)) out <- c(out, sprintf(paste(
        "%s: param %s is \"\" in a condition and makes a `by` column: a blank is",
        "missing there, and its rows are dropped (write a value, e.g. ALL)"), tag, p))
    }
  }
  out
}

.r_reserved <- function() {
  c("if", "else", "repeat", "while", "function", "for", "next", "break", "TRUE", "FALSE",
    "NULL", "Inf", "NaN", "NA", "in")
}

# ---- the program ----------------------------------------------------------------

# One element's call (an analyses row with its params in, `rs`) on `data`:
# the call (.cl()), its own steps (keep_stats, the formats), the function;
# `rv` the row as the data reads it (the names), `post` the item's
.ard_item_call <- function(x, rs, rv, keys, subj, data, post, lv) {
  pid <- rv$population_id
  pop <- if (!is.na(pid)) paste0("pop_", .r_name(pid))
  pop_name <- if (is.null(pop)) "NULL" else pop
  given <- c(.args_given(rs$args), if (!is.na(rs$strata)) "strata",
             if (!is.na(rs$denominator)) "denominator")
  has <- function(arg) arg %in% given
  k <- .method_key(rs$method, keys)
  kind <- if (is.na(k)) "" else keys$kind[k]
  call_fn <- if (is.na(k)) rs$method else keys$call[k]
  den <- .den_code(rs$denominator, x, pop, subj, population = pop_name, by = rs$by)
  fmt <- c(if (!is.na(k)) .parse_formats(keys$formats[k]), .parse_formats(rv$formats))
  fmt <- fmt[!duplicated(names(fmt), fromLast = TRUE)]
  rf <- rv
  rf$post <- post
  fmt_fun <- .call_fmt_fun(rf, keys, given, fmt)
  node <- .analysis_node(rs, keys, subj, has, den, data = data, population = pop_name,
                         fmt_fun = fmt_fun, codelists = lv)
  if (isTRUE(.ard_yes(rs$overall %||% NA)) && !is.na(rs$by) &&
      !call_fn %in% c("(code)", "(subjects)")) {
    r0 <- rs
    r0$by <- NA
    node0 <- .analysis_node(r0, keys, subj, has, den, data = data, population = pop_name,
                            fmt_fun = fmt_fun, codelists = lv)
    node <- .cl("dplyr::bind_rows", list(node, node0))
  }
  keep <- if (!kind %in% c("continuous", "categorical", "missing")) .split_bar(rv$statistics)
  list(node = node, fn = call_fn,
       keep = if (length(keep)) list(.cl("keep_stats", list(.cl("c", .q(keep))))),
       fmt = list(.fmt_step(fmt, !is.null(fmt_fun))))
}

# a call's first argument the data's blank step (.blank_as_na()) back to the data
.strip_blank_head <- function(node, data) {
  if (inherits(node, "tfl_cl") && length(node$args) && identical(names(node$args)[1L] %||% "", "") &&
      .is_blank_head(node$args[[1L]], data)) {
    node$args[[1L]] <- data
  }
  if (inherits(node, "tfl_cl") && identical(node$fn, "dplyr::bind_rows")) {
    node$args <- lapply(node$args, .strip_blank_head, data = data)
  }
  node
}

# lines indented by `n` spaces
.indent <- function(lines, n = 2L) {
  lines <- unlist(strsplit(lines, "\n", fixed = TRUE))
  ifelse(nzchar(lines), paste0(strrep(" ", n), lines), lines)
}

# The item's lines: its table, its function of one row and the run over
# the table (with conditions), or its calls on its data (without)
.ard_item_lines <- function(x, ra, rv, keys, subj, src, name) {
  r1 <- ra[1L, ]
  v1 <- rv[1L, ]
  o <- r1$output_id
  pp <- .cond_params(r1)
  conds <- if (length(pp)) .cond_rows(x, o, r1$analysis_id) else list()
  cond <- length(pp) > 0L
  lv <- attr(x, "levels")[[o]]
  item_lv <- .levels_parse(r1$levels %||% NA)
  # the process's steps on its data: where, derive, levels
  prep <- list()
  if (cond && !is.na(r1$where)) {
    prep <- c(prep, list(.step_cl(sprintf("dplyr::filter(%s)", .cond_subst_r(r1$where, pp, TRUE)))))
  }
  if (!is.na(r1$derive %||% NA)) {
    d <- vapply(.split_bar(r1$derive), .cond_subst_r, "", params = pp, inject = TRUE)
    prep <- c(prep, list(.step_cl(sprintf("dplyr::mutate(%s)", paste(trimws(d), collapse = ", ")))))
  }
  if (length(item_lv)) {
    args <- lapply(item_lv, function(v) {
      if (length(v) == 1L && grepl(paste0("^", .cond_brace_rx, "$"), v) && .brace_names(v) %in% pp) {
        .brace_names(v)
      } else .lay(.c_str(v), width = Inf)
    })
    prep <- c(prep, list(.cl("set_levels", args)))
  }
  # each call, its params in
  calls_fn <- vapply(seq_len(nrow(ra)), function(j) {
    m <- .method_key(ra$method[j], keys)
    if (is.na(m)) ra$method[j] else keys$call[m]
  }, "")
  # the blank values missing, the unlisted categories' unused levels
  # dropped: once, on the data (what each tabulation's head would do)
  if (any(calls_fn == "cards::ard_tabulate")) {
    drop <- unique(unlist(lapply(which(calls_fn == "cards::ard_tabulate"), function(j)
      setdiff(.unlisted_vars(rv[j, ], lv), names(item_lv)))))
    prep <- c(prep, list(.blank_head_step(.blank_as_na("data", drop))))
  }
  post <- if (!is.na(r1$post)) .cond_subst_r(r1$post, pp, TRUE) else NA_character_
  data <- if (cond) "data" else paste0(name, "_data")
  pipe1 <- nrow(ra) == 1L && !identical(calls_fn[1L], "(code)")
  on <- if (!length(prep) || pipe1) src else data
  els <- lapply(seq_len(nrow(ra)), function(j) {
    rs <- ra[j, ]
    for (cn in .cond_col_fields) rs[[cn]] <- .cond_subst_cols(rs[[cn]], pp)
    for (cn in c("args", "denominator", "code")) rs[[cn]] <- .cond_subst_r(rs[[cn]], pp, FALSE)
    rs$where <- NA_character_
    rs$post <- NA_character_
    rs$population_id <- rv$population_id[j]
    e <- .ard_item_call(x, rs, rv[j, ], keys, subj, on, post, lv)
    e$node <- .strip_blank_head(e$node, on)
    e
  })
  rp <- v1
  rp$post <- post
  ps <- .post_fmt_split(.post_steps(x, rp))
  # the relevel of the ARD: every call's columns
  rr <- v1
  for (cn in intersect(c("by", "variables", "strata"), names(rv))) {
    u <- unique(unlist(lapply(rv[[cn]], .split_bar)))
    rr[[cn]] <- if (length(u)) paste(u, collapse = " | ") else NA_character_
  }
  tail_steps <- c(.relevel_step(x, rr), list(.tag_step(r1$analysis_id, v1$population_id)))
  # the body: one pipe (one call), or the data then the calls bound
  if (pipe1) {
    e <- els[[1L]]
    node <- e$node
    a1 <- node$args
    if (inherits(node, "tfl_cl") && length(a1) && identical(names(a1)[1L] %||% "", "") &&
        identical(a1[[1L]], on)) {
      node$args <- a1[-1L]
      steps <- c(prep, list(node), ps$before, e$keep, e$fmt, ps$after, tail_steps)
      body <- .pipe_code(if (cond) NULL else name, src, steps)
    } else {
      body <- .pipe_code(if (cond) NULL else name, node,
                         c(ps$before, e$keep, e$fmt, ps$after, tail_steps))
    }
  } else {
    pre <- if (length(prep)) .pipe_code(data, src, prep)
    pieces <- vapply(els, function(e) {
      .pipe_code(NULL, e$node, c(e$keep, e$fmt))
    }, "")
    bound <- .bind_commas(pieces)
    body <- c(pre, .pipe_code(if (cond) NULL else name, bound,
                              c(ps$before, ps$after, tail_steps)))
  }
  lbl <- if (!is.na(r1$label)) sprintf("# %s: %s", r1$analysis_id, r1$label) else
    sprintf("# %s", r1$analysis_id)
  if (!cond) return(c(lbl, body))
  tab <- paste0(name, "_conditions")
  fun <- paste0(name, "_one")
  c(lbl,
    sprintf("# (%s() runs once for each row of %s)", fun, tab),
    .cond_tribble(tab, pp, conds),
    .fun_head(fun, pp),
    .indent(body),
    "}",
    sprintf("%s <- purrr::pmap(%s, %s) |>", name, tab, fun),
    "  dplyr::bind_rows()")
}

# dplyr::bind_rows() of the calls, a call (and its steps) an argument
.bind_commas <- function(pieces) {
  p <- vapply(pieces, function(s) paste(.indent(s), collapse = "\n"), "", USE.NAMES = FALSE)
  paste0("dplyr::bind_rows(\n", paste(p, collapse = ",\n"), "\n)")
}

# `fun <- function(a, b) {`, wrapped at 100 characters
.fun_head <- function(fun, pp) {
  one <- sprintf("%s <- function(%s) {", fun, paste(pp, collapse = ", "))
  if (nchar(one) <= 100L) return(one)
  lead <- strrep(" ", nchar(fun) + 13L)
  rows <- character()
  cur <- ""
  for (k in seq_along(pp)) {
    piece <- paste0(pp[k], if (k < length(pp)) "," else ") {")
    try <- if (nzchar(cur)) paste(cur, piece) else piece
    if (nzchar(cur) && nchar(lead) + nchar(try) > 100L) {
      rows <- c(rows, cur)
      cur <- piece
    } else cur <- try
  }
  rows <- c(rows, cur)
  c(paste0(fun, " <- function(", rows[1L]), if (length(rows) > 1L) paste0(lead, rows[-1L]))
}

# `tab <- dplyr::tribble(~a, ~b, <rows>)`: a row a line when it fits in
# 100 characters, else its cells over lines
.cond_tribble <- function(tab, pp, conds) {
  # (a column of expressions: its constants quoted too, `quote(TRUE)`)
  is_expr <- vapply(pp, function(p) any(vapply(conds, function(rw)
    identical(.cond_value(rw[[p]] %||% NA)$kind, "expr"), NA)), NA)
  rows <- lapply(seq_along(conds), function(k) {
    cells <- vapply(pp, function(p) {
      v <- .cond_value(conds[[k]][[p]] %||% NA)
      if (is_expr[[p]] && identical(v$kind, "const")) v$kind <- "expr"
      .cond_r(v)
    }, "")
    last <- k == length(conds)
    cells[length(cells)] <- paste0(cells[length(cells)], if (last) "" else ",")
    cells[-length(cells)] <- paste0(cells[-length(cells)], ",")
    out <- character()
    cur <- ""
    for (s in cells) {
      if (nchar(s) > 98L && grepl("^c\\(", s)) {
        if (nzchar(cur)) out <- c(out, cur)
        cur <- ""
        e <- str2lang(sub(",$", "", s))
        long <- .lay_fill(.cl("c", lapply(as.list(e)[-1L], .expr_text)), lead = 0L, width = 98L)
        out <- c(out, paste0(long, if (endsWith(s, ",")) "," else ""))
        next
      }
      try <- if (nzchar(cur)) paste(cur, s) else s
      if (nzchar(cur) && 2L + nchar(try) > 100L) {
        out <- c(out, cur)
        cur <- s
      } else cur <- try
    }
    if (nzchar(cur)) out <- c(out, cur)
    .indent(out)
  })
  c(sprintf("%s <- dplyr::tribble(", tab),
    paste0("  ", paste0("~", pp, collapse = ", "), ","),
    unlist(rows),
    ")")
}
