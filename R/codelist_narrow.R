# The code lists a data's filter narrows (canon 1.42): a column made a factor
# from a code list is made AFTER the filter, with the levels the filter's
# simple conditions exclude removed -- a study's PARAMCD list of every test
# gives, behind `PARAMCD %in% c("ALT", "AST")`, the two.  A level with no
# record that the filter did not exclude stays (counted 0).
#
# Read are the conditions joined by a top-level `&`, each of the forms
#   VAR %in% c(...)   VAR == "x"   !VAR %in% c(...)   !(VAR %in% c(...))
#   VAR != "x"        (and the value on the left of == and !=)
# Any other condition that names the column (inside `|`, a function call
# such as grepl(), a comparison with another column) excludes no level.

# A condition as its parts: `A & B & C` split, brackets taken off
.narrow_parts <- function(e) {
  while (is.call(e) && identical(e[[1L]], as.name("("))) e <- e[[2L]]
  if (is.call(e) && identical(e[[1L]], as.name("&")) && length(e) == 3L) {
    return(c(.narrow_parts(e[[2L]]), .narrow_parts(e[[3L]])))
  }
  list(e)
}

# The values a condition compares a column with: a character vector, or NULL
# when they are not plain text (a name, a call, a number)
.narrow_values <- function(e) {
  v <- tryCatch(eval(e, baseenv()), error = function(err) NULL)
  if (is.character(v) && !anyNA(v)) v
}

# One part: list(var =, op = "in" | "out", values =) when it is one of the
# simple forms, else NULL
.narrow_simple <- function(e) {
  neg <- FALSE
  while (is.call(e) && identical(e[[1L]], as.name("("))) e <- e[[2L]]
  if (is.call(e) && identical(e[[1L]], as.name("!")) && length(e) == 2L) {
    neg <- TRUE
    e <- e[[2L]]
    while (is.call(e) && identical(e[[1L]], as.name("("))) e <- e[[2L]]
  }
  if (!is.call(e) || length(e) != 3L || !is.name(e[[1L]])) return(NULL)
  op <- as.character(e[[1L]])
  if (!op %in% c("%in%", "==", "!=")) return(NULL)
  l <- e[[2L]]
  r <- e[[3L]]
  if (is.name(l)) {
    var <- as.character(l)
    vals <- .narrow_values(r)
  } else if (op != "%in%" && is.name(r)) {
    var <- as.character(r)
    vals <- .narrow_values(l)
  } else return(NULL)
  if (is.null(vals)) return(NULL)
  list(var = var, op = if (xor(op == "!=", neg)) "out" else "in", values = vals)
}

# What conditions (expressions, each a filter's) say of the columns `vars`:
# list(include = list(VAR = codes), exclude = list(VAR = codes), other = the
# columns a condition that is not simple names).  Several parts on a column
# combine: the includes meet, the excludes join.
.narrow_conditions <- function(conds, vars) {
  inc <- list()
  exc <- list()
  other <- character()
  for (cd in conds) {
    for (p in .narrow_parts(cd)) {
      s <- .narrow_simple(p)
      if (!is.null(s) && s$var %in% vars) {
        if (s$op == "in") {
          inc[[s$var]] <- if (is.null(inc[[s$var]])) s$values else
            intersect(inc[[s$var]], s$values)
        } else {
          exc[[s$var]] <- union(exc[[s$var]], s$values)
        }
      } else {
        other <- c(other, intersect(all.vars(p), vars))
      }
    }
  }
  list(include = inc, exclude = exc, other = unique(other))
}

# A code list narrowed, as the R a program writes: `cl[c("ALT", "AST")]`
# (a list with names: its codes), `intersect(cl, c(...))` (a list without);
# an exclusion alone `cl[setdiff(names(cl), c("C"))]` /
# `setdiff(cl, c("C"))`.  The codes in the list's order, only those the list
# has.  NULL when the conditions leave every level.  `name`: the list's
# name in the program.
.narrow_expr <- function(cl, name, include = NULL, exclude = NULL) {
  named <- !is.null(names(cl))
  codes <- if (named) names(cl) else unname(cl)
  q <- function(x) paste0("c(", paste(encodeString(x, quote = "\""), collapse = ", "), ")")
  if (!is.null(include)) {
    keep <- setdiff(codes[codes %in% include], exclude)
    if (length(keep) == length(codes)) return(NULL)
    return(if (named) sprintf("%s[%s]", name, q(keep)) else
      sprintf("intersect(%s, %s)", name, q(keep)))
  }
  out <- codes[codes %in% exclude]
  if (!length(out)) return(NULL)
  if (named) sprintf("%s[setdiff(names(%s), %s)]", name, name, q(out)) else
    sprintf("setdiff(%s, %s)", name, q(out))
}

# The levels each listed column of `conds` keeps: a named list, VAR = the
# list's codes in its order (the narrowed list, or the whole one).  A
# condition that names a column without being simple narrows nothing: its
# columns are in the `other` attribute.
.narrow_codes <- function(levels, conds) {
  nc <- .narrow_conditions(conds, names(levels))
  out <- lapply(stats::setNames(names(levels), names(levels)), function(v) {
    cl <- levels[[v]]
    codes <- if (is.null(names(cl))) unname(cl) else names(cl)
    if (!is.null(nc$include[[v]])) codes <- codes[codes %in% nc$include[[v]]]
    codes[!codes %in% nc$exclude[[v]]]
  })
  attr(out, "other") <- nc$other
  out
}

# The levels as the lists narrowed to `codes` (.narrow_codes()): the same
# vectors, fewer entries, in the list's order
.narrow_levels <- function(levels, codes) {
  lapply(stats::setNames(names(levels), names(levels)), function(v) {
    cl <- levels[[v]]
    if (is.null(names(cl))) cl[cl %in% codes[[v]]] else cl[names(cl) %in% codes[[v]]]
  })
}

# The filter conditions behind a data a program defines: its own filter()s
# and those of the data it is made from (named in the code before it, as the
# analysis sets' objects are).  `code`: the program's elements; `k`: the one
# that defines the data.
.def_conditions <- function(code, k, depth = 0L) {
  rx <- "^([A-Za-z.][A-Za-z0-9._]*) <- "
  if (depth > 20L || !grepl(rx, code[k])) return(list())
  e <- tryCatch(parse(text = sub(rx, "", code[k]), keep.source = FALSE)[[1L]],
                error = function(err) NULL)
  conds <- list()
  src <- NULL
  while (!is.null(e)) {
    if (is.name(e)) {
      src <- as.character(e)
      break
    }
    if (!is.call(e) || length(e) < 2L) break
    fn <- paste(deparse(e[[1L]]), collapse = "")
    if (!grepl("^([A-Za-z.][A-Za-z0-9._]*::)?[A-Za-z.][A-Za-z0-9._]*$", fn) ||
        fn == "local") break
    args <- as.list(e)[-1L]
    nm <- names(args) %||% rep("", length(args))
    if (fn %in% c("filter", "dplyr::filter")) {
      conds <- c(conds, args[-1L][!nzchar(nm[-1L])])
    }
    e <- args[[1L]]
  }
  if (!is.null(src) && k > 1L) {
    nm_all <- sub(" <- .*$", "", code[seq_len(k - 1L)])
    j <- which(nm_all == src & grepl(rx, code[seq_len(k - 1L)]))
    if (length(j)) conds <- c(conds, .def_conditions(code, max(j), depth + 1L))
  }
  conds
}

# The code lists as the data defined at code[k] holds them: each variable's
# expression in the program, `cl_<variable>` narrowed (.narrow_expr()) by
# the data's filters.  A condition on a listed column that is not a simple
# one says so (message()) and narrows nothing.
.def_narrowing <- function(code, k, levels) {
  conds <- .def_conditions(code, k)
  if (!length(conds)) return(list())
  nc <- .narrow_conditions(conds, names(levels))
  obj <- sub(" <- .*$", "", code[k])
  for (v in nc$other) {
    message(sprintf(paste0("%s: the filter on %s is not a plain one (%%in%%, ==, != ",
                           "joined by &): its code list keeps all its levels."), obj, v))
  }
  out <- lapply(stats::setNames(names(levels), names(levels)), function(v)
    .narrow_expr(levels[[v]], .cl_name(v), nc$include[[v]], nc$exclude[[v]]))
  out[!vapply(out, is.null, NA)]
}

# ---- the lists on the table side -----------------------------------------

# The filter conditions an analysis row of a definition `x` puts on its data:
# its analysis set's, its analysis data's (and those they are made from),
# its own `where`; a parent row's too (the rows inside take the parent's).
.analysis_conditions <- function(x, r) {
  ad <- .adata_of(.adata_sheet(x), r$output_id)
  pops <- x$populations
  parse1 <- function(w) {
    if (length(w) != 1L || is.na(w) || !nzchar(trimws(w))) return(list())
    e <- tryCatch(str2lang(w), error = function(err) NULL)
    if (is.null(e)) list() else list(e)
  }
  rows <- list(r)
  par <- r$parent %||% NA
  an <- x$analyses
  if (!is.na(par) && !is.null(an)) {
    k <- which(an$analysis_id == par & an$output_id == r$output_id)
    if (length(k)) rows <- c(rows, list(an[k[1L], , drop = FALSE]))
  }
  conds <- list()
  for (row in rows) {
    pid <- row$population_id
    d <- .data_col(row)
    if (!is.na(d)) {
      for (id in .adata_chain(ad, d)) conds <- c(conds, parse1(ad$where[match(id, ad$data_id)]))
      pid <- .adata_pop(ad, d)
    }
    if (!is.na(pid) && !is.null(pops)) {
      conds <- c(conds, parse1(pops$where[match(pid, pops$population_id)]))
    }
    conds <- c(conds, parse1(row$where))
  }
  conds
}

# The code lists of a table definition, a variable each: its values (codes)
# in their order, named by their labels when the list has any (the shape of
# .codelist_levels()), else unnamed.  The columns the plan makes
# (.ard_spec_plan_cols()) are not in it.
.ard_spec_lists <- function(sp) {
  own <- .ard_spec_plan_cols(sp)
  out <- list()
  for (cl in .ard_spec_codelists(sp)) {
    var <- cl$variable[1L]
    if (var %in% own) next
    lab <- if (is.null(cl$label)) rep(NA_character_, nrow(cl)) else as.character(cl$label)
    lab[!is.na(lab) & !nzchar(trimws(lab))] <- NA
    v <- as.character(cl$value)
    out[[var]] <- if (all(is.na(lab))) v else stats::setNames(ifelse(is.na(lab), v, lab), v)
  }
  out
}

# The code lists of the variables the report's analyses group by or
# analyse -- the variables its ARD program makes factors of -- each as the
# analyses' filters leave it (.narrow_codes(): the same levels set_levels()
# gets).  A variable several analyses read keeps what any of them keeps.
# Without the ARD definition (`ard_spec`: a table definition by itself) every
# listed variable, whole.
.ard_spec_read_lists <- function(sp, ard_spec = NULL) {
  lists <- .ard_spec_lists(sp)
  a <- ard_spec$analyses
  id <- attr(sp, "output_id")
  if (is.data.frame(a) && !is.null(id) && "output_id" %in% names(a)) {
    a <- a[!is.na(a$output_id) & a$output_id %in% id, , drop = FALSE]
  }
  if (!length(lists) || !is.data.frame(a) || !nrow(a)) return(lists)
  keep <- list()
  for (i in seq_len(nrow(a))) {
    row <- a[i, , drop = FALSE]
    v <- intersect(names(lists), .ard_read_vars(row))
    if (!length(v)) next
    codes <- .narrow_codes(lists[v], .analysis_conditions(ard_spec, row))
    for (k in v) keep[[k]] <- union(keep[[k]], codes[[k]])
  }
  .narrow_levels(lists[names(lists) %in% names(keep)], keep)
}
