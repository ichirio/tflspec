# ============================================================================
#  The review of a study's definition
# ----------------------------------------------------------------------------
#  Every check, wherever it runs, answers as rows of one data frame: the
#  report, the level (error: cannot be used; check: valid, probably wrong;
#  hand: something the rules require is missing, set it by hand), the area,
#  where it is (sheet, the row's key, the column), the message, a hint and
#  the rule.  The rules are a catalog (inst/review/rules.csv): what the
#  tests, the filters and a translation key on.  What the constructors find
#  is rule S01; the rest are what is valid but probably wrong, judged from
#  the definition, the catalogs and -- when given -- the facts of the data
#  (tfl_data_facts()) and of the ARD (tfl_ard_facts()).  A rule that needs
#  facts it is not given is skipped, not failed; nothing here stops.
# ============================================================================

#' The rules of the review
#'
#' The catalog of what [tfl_review_spec()] checks: each rule's id, its
#' level (`error`: the definition cannot be used; `check`: valid, probably
#' wrong; `hand`: something the rules require is missing, to be set by
#' hand), its area, what it needs (`spec`, `catalog`, `ard`: what the
#' report's ARD holds, `data`: the facts of the data), which package runs
#' it (`tflspec`, or `tflplanner` for what needs a study folder: the report
#' list, the files), the message (a template: `%s` filled in) and a hint.
#' A rule that is not in the catalog does not exist.
#'
#' @return A data frame: `rule`, `level`, `area`, `needs`, `checked_by`,
#'   `message`, `hint`.
#' @seealso [tfl_review_spec()]
#' @examples
#' r <- tfl_review_rules()
#' r[r$checked_by == "tflspec", c("rule", "level", "area", "needs")]
#' @export
tfl_review_rules <- function() {
  p <- system.file("review", "rules.csv", package = "tflspec")
  r <- utils::read.csv(p, stringsAsFactors = FALSE, na.strings = character(),
                       encoding = "UTF-8")
  r$hint[is.na(r$hint)] <- ""
  r
}

.review_levels <- c("error", "check", "hand")

# the sheets in the order a workbook has them, for the sort
.review_sheet_order <- c("_tflplanner", "study", "tables", "variables",
                         "codelists", "cells", "digits", "layout", "columns",
                         "style", "cell_styles", "col_header", "report",
                         "page", "header", "footer", "titles", "footnotes",
                         "tokens", "datasets", "populations", "analysis_data",
                         "analyses", "listings", "listing_cols", "design")

.review_empty <- function() {
  out <- data.frame(output_id = character(), level = character(),
                    area = character(), sheet = character(), row = character(),
                    field = character(), message = character(),
                    hint = character(), rule = character(), draft = logical(),
                    stringsAsFactors = FALSE)
  out$args <- list()
  out$fix <- list()
  out
}

# One row of the review: the rule's level, area, message and hint from
# the catalog unless given; `args` fill the message's template
.rv <- function(rule, output_id = NA_character_, sheet = "", row = "",
                field = "", args = character(), level = NULL, area = NULL,
                message = NULL, hint = NULL, fix = NULL, cat = .review_cat()) {
  k <- match(rule, cat$rule)
  args <- as.character(args)
  if (is.null(message)) {
    message <- tryCatch(do.call(sprintf, c(list(cat$message[k]), as.list(args))),
                        error = function(e) paste(args, collapse = " "))
  }
  out <- data.frame(output_id = as.character(output_id),
                    level = level %||% cat$level[k],
                    area = area %||% cat$area[k],
                    sheet = sheet, row = ifelse(is.na(row), "", row),
                    field = ifelse(is.na(field), "", field),
                    message = message, hint = hint %||% cat$hint[k],
                    rule = rule, draft = FALSE, stringsAsFactors = FALSE)
  out$args <- list(args)
  out$fix <- list(fix)
  out
}

# the catalog, read once a session
.review_cat <- function() {
  if (is.null(.review_env$cat)) .review_env$cat <- tfl_review_rules()
  .review_env$cat
}
.review_env <- new.env(parent = emptyenv())

# The constructors' rows (.spec_problems_keyed()'s) as rule S01
.rv_from_problems <- function(p, area) {
  if (is.null(p) || !nrow(p)) return(list())
  lapply(seq_len(nrow(p)), function(i)
    .rv("S01", p$output_id[i], p$sheet[i], p$row[i], p$field[i],
        args = p$message[i], area = area))
}

#' Review a study's definition
#'
#' Lists, for the whole study, what is wrong with its definition (`error`:
#' it cannot be used -- every problem the constructors [tfl_table_spec()],
#' [tfl_ard_spec()], [tfl_listing_spec()] and [tfl_check_fig_design()]
#' stop or report on), what is valid but probably wrong (`check`), and
#' what is missing and has to be set by hand (`hand`).  Nothing stops: a
#' definition that cannot be built is reviewed as the sheets it is.  With
#' `facts`, the definition is also checked against what the data hold
#' ([tfl_data_facts()]: a column an analysis reads that its data do not
#' have, a condition that keeps nothing, a code list against the values)
#' and against what each report's ARD holds (`facts$ard[[output_id]]`,
#' [tfl_ard_facts()]).  The rules are [tfl_review_rules()].
#'
#' @param spec The table definition: a [tfl_table_spec()] or a list of its
#'   sheets (`tables`, `variables`, `cells` ...).
#' @param ard The ARD definition: a [tfl_ard_spec()] or a list of its five
#'   sheets.
#' @param listings The listing definition: a [tfl_listing_spec()] or a
#'   list of its two sheets.
#' @param figures The figures' designs: a list of [tfl_fig_design()]s (or
#'   their lists) named by their reports.
#' @param output_id Only these reports; `NULL` reviews every report the
#'   sheets name, and the study-wide rows (`output_id` `NA`: the defaults,
#'   the data).
#' @param facts The facts of the data ([tfl_data_facts()]), with the
#'   reports' ARD facts in `facts$ard`; `NULL` runs the rules that need the
#'   definition only.
#' @param rules Only these rules: their ids (`"T03"`) or areas (`"table"`).
#' @return A `tfl_review`: a data frame, one row per item -- `output_id`,
#'   `level` (`error`, `check`, `hand`), `area` (`report`, `codelists`,
#'   `ard`, `ard_run`, `table`, `listing`, `figure`, `data`, `spec`),
#'   `sheet`, `row` (the row's key, as the sheet is keyed: the variable,
#'   the analysis_id, `variable / context / row` on `cells`), `field` (the
#'   column), `message`, `hint`, `rule`, `draft` (`FALSE`: set by an import
#'   for the rows it brings), `args` (the values filled into the rule's
#'   template, for a translation) and `fix` (a one-step fix, where there is
#'   one: [tfl_fig_apply_fix()]).  Errors first, then checks, then what to
#'   set by hand; within a level by report and sheet.
#' @seealso [tfl_review_rules()], [tfl_data_facts()], [tfl_ard_facts()]
#' @examples
#' ard <- list(
#'   datasets = data.frame(dataset = "ADSL", path = "adsl.rds"),
#'   populations = data.frame(population_id = "SAF", dataset = "ADSL",
#'                            where = 'SAFFL == "Y"'),
#'   analyses = data.frame(output_id = "T-1", analysis_id = "AGE",
#'                         method = "continuous", population_id = "SAF",
#'                         dataset = "ADSL", by = "TRT01A",
#'                         variables = "AGE | TRT01A"))
#' r <- tfl_review_spec(ard = ard)
#' r[, c("output_id", "level", "rule", "message")]
#' summary(r)
#' @export
tfl_review_spec <- function(spec = NULL, ard = NULL, listings = NULL,
                            figures = NULL, output_id = NULL, facts = NULL,
                            rules = NULL) {
  out <- list()
  put <- function(x) {
    if (is.data.frame(x)) x <- list(x)
    out <<- c(out, x)
  }
  # a rule that fails on an odd value is one row naming it, never a lost
  # review
  run <- function(rule, expr) {
    tryCatch(put(expr), error = function(e) {
      put(.rv("review", args = c(rule, conditionMessage(e))))
    })
  }
  # ---- the definitions, as their constructors read them (S01)
  sp <- NULL
  if (!is.null(spec)) {
    if (inherits(spec, "tfl_table_spec")) {
      sp <- spec
    } else if (is.list(spec) && !is.data.frame(spec)) {
      res <- tryCatch(suppressMessages(.table_spec_problems(spec)),
                      error = function(e) e)
      if (inherits(res, "error")) {
        put(.rv("S01", area = "spec", args = conditionMessage(res)))
      } else {
        put(.rv_from_problems(res$problems, "table"))
        sp <- res$sp
      }
    } else {
      put(.rv("S01", area = "spec", args = paste(
        "The table definition is not a list of sheets but", .what(spec))))
    }
  }
  x <- NULL
  if (!is.null(ard)) {
    if (is.list(ard) && !is.data.frame(ard)) {
      x <- tryCatch(.review_ard_sheets(ard), error = function(e) e)
      if (inherits(x, "error")) {
        put(.rv("S01", area = "spec", args = conditionMessage(x)))
        x <- NULL
      } else {
        run("S01", .rv_from_problems(.ard_spec_problems(x), "ard"))
      }
    } else {
      put(.rv("S01", area = "spec", args = paste(
        "The ARD definition is not a list of sheets but", .what(ard))))
    }
  }
  lf <- NULL
  if (!is.null(listings)) {
    lf <- tryCatch(if (inherits(listings, "tfl_listing_spec")) listings else
      tfl_listing_spec(listings, check = FALSE), error = function(e) e)
    if (inherits(lf, "error")) {
      put(.rv("S01", area = "spec", args = conditionMessage(lf)))
      lf <- NULL
    } else {
      run("S01", .rv_listing_problems(lf))
    }
  }
  # ---- the rules
  if (!is.null(x)) {
    run("A06", .rule_a06(x))
    run("A08", .rule_a08(x))
    run("A15", .rule_a15(x))
  }
  if (!is.null(sp)) {
    run("T", .rules_table(sp, x, facts))
    if (!is.null(x)) run("C01", .rule_c01(sp, x))
  }
  if (length(figures)) run("F", .rules_figures(figures, facts))
  if (!is.null(facts)) {
    if (!is.null(x)) {
      run("A01", .rules_data_ard(x, facts))
      run("A05", .rule_a05(x, facts))
      if (!is.null(sp)) run("C02", .rules_codelists(sp, x, facts))
    }
    if (!is.null(lf)) run("L02", .rule_l02(lf, x, facts))
    run("A17", .rule_a17(facts))
  }
  r <- do.call(rbind, c(list(.review_empty()), out))
  .review_finish(r, output_id, rules)
}

# Narrow, sort, class
.review_finish <- function(r, output_id = NULL, rules = NULL) {
  if (!is.null(output_id)) r <- r[r$output_id %in% output_id, , drop = FALSE]
  if (!is.null(rules)) {
    r <- r[r$rule %in% rules | r$area %in% rules | r$rule == "review", ,
           drop = FALSE]
  }
  key <- paste(r$output_id, r$level, r$sheet, r$row, r$field, r$message,
               sep = "\r")
  r <- r[!duplicated(key), , drop = FALSE]
  sh <- match(r$sheet, .review_sheet_order)
  sh[is.na(sh)] <- length(.review_sheet_order) + 1L
  o <- order(match(r$level, .review_levels), !is.na(r$output_id), r$output_id,
             sh, method = "radix")
  r <- r[o, , drop = FALSE]
  rownames(r) <- NULL
  class(r) <- c("tfl_review", "data.frame")
  r
}

#' @export
print.tfl_review <- function(x, ...) {
  n <- table(factor(x$level, .review_levels))
  cat(sprintf("<tfl_review> %d error%s, %d to check, %d to set by hand\n",
              n[["error"]], if (n[["error"]] == 1L) "" else "s", n[["check"]],
              n[["hand"]]))
  if (!nrow(x)) return(invisible(x))
  where <- paste(x$sheet, x$row, x$field, sep = " | ")
  where <- gsub("( \\| )+$", "", gsub("^( \\| )+", "", where))
  lines <- sprintf("%-5s %-4s %-12s %s: %s", x$level, x$rule,
                   ifelse(is.na(x$output_id), "(study)", x$output_id),
                   where, gsub("\n\\s*", " ", x$message))
  cat(paste0("  ", lines), sep = "\n")
  invisible(x)
}

#' @export
summary.tfl_review <- function(object, ...) {
  id <- ifelse(is.na(object$output_id), "(study)", object$output_id)
  ids <- unique(id)
  out <- data.frame(output_id = ids, stringsAsFactors = FALSE)
  for (l in .review_levels) {
    out[[l]] <- vapply(ids, function(i) sum(id == i & object$level == l), 1L,
                       USE.NAMES = FALSE)
  }
  out
}

# ---- the definitions as sheets ---------------------------------------------

# An ARD definition as its five sheets in shape, whatever was given
.review_ard_sheets <- function(ard) {
  x <- lapply(names(.ard_spec_sheets), function(s) {
    d <- ard[[s]]
    if (is.null(d)) d <- data.frame()
    .normalize_ard_sheet(d, s)
  })
  names(x) <- names(.ard_spec_sheets)
  structure(x, class = "tfl_ard_spec")
}

# The listing definition's problems: a listing without its columns is
# something to set (L01), the rest cannot be used (S01)
.rv_listing_problems <- function(lf) {
  p <- .listing_problems(lf)
  hand <- grepl(": no columns in `listing_cols`$|: no `vars`$", p$message)
  c(.rv_from_problems(p[!hand, , drop = FALSE], "listing"),
    lapply(which(hand), function(i)
      .rv("L01", p$output_id[i], p$sheet[i], p$row[i], p$field[i],
          args = p$message[i])))
}

# ---- what a report's analyses read -----------------------------------------

# The analyses with those inside a parent given the parent's data, set,
# condition and groups (the parents that only run them dropped)
.review_flat <- function(a) {
  a$data <- .data_col(a)
  par <- a$parent %||% rep(NA_character_, nrow(a))
  for (i in which(!is.na(par))) {
    p <- which(a$output_id == a$output_id[i] & a$analysis_id == par[i])[1L]
    if (!is.na(p) && is.na(a$data[i])) a$data[i] <- a$data[p]
  }
  .ard_spec_flat(a)
}

# the names a `derive` makes (`TRTA = TRT01A | AGEGR = ...`)
.derive_names <- function(x) {
  d <- unlist(lapply(x, .split_bar))
  d <- d[grepl("^[A-Za-z.][A-Za-z0-9._]*\\s*=[^=]", d)]
  trimws(sub("=.*$", "", d))
}

# What analysis `i` of `a` (flat) reads: its dataset, analysis set, every
# condition on the way (one text) and the columns made on the way (derive,
# add); NULL when its data is made by code
.review_source <- function(x, a, i) {
  ad <- .adata_of(x$analysis_data, a$output_id[i])
  pops <- x$populations
  dcol <- .data_col(a)[i]
  pop_derive <- function(p) .derive_names(pops$derive[pops$population_id %in% p])
  if (!is.na(dcol)) {
    ch <- .adata_chain(ad, dcol)
    if (is.null(ch)) return(NULL)
    rows <- ad[match(ch, ad$data_id), , drop = FALSE]
    if (any(!is.na(rows$code %||% NA))) return(NULL)
    ds <- .adata_dataset(ad, dcol)
    pop <- .adata_pop(ad, dcol)
    wh <- c(rows$where, a$where[i])
    extra <- c(.derive_names(rows$derive),
               unlist(lapply(rows$add, .split_bar)), pop_derive(pop))
  } else {
    ds <- a$dataset[i]
    pop <- a$population_id[i]
    if (is.na(ds) && !is.na(pop)) ds <- pops$dataset[match(pop, pops$population_id)]
    wh <- a$where[i]
    extra <- pop_derive(pop)
  }
  wh <- wh[!is.na(wh)]
  ds_derive <- .derive_names(x$datasets$derive[toupper(x$datasets$dataset) %in% toupper(ds)])
  list(dataset = if (is.na(ds)) NA_character_ else toupper(ds),
       population_id = if (is.na(pop)) NA_character_ else pop,
       where = if (!length(wh)) NA_character_ else if (length(wh) == 1L) wh else
         paste0("(", wh, ")", collapse = " & "),
       extra = unique(c(extra, ds_derive)))
}

# the plain names of a `|` list (a name, not a tidyselect call)
.bar_names <- function(x) {
  v <- .split_bar(x)
  v[grepl("^[A-Za-z.][A-Za-z0-9._]*$", v)]
}

# A method's kind in the catalog (NA: a function the catalog does not know)
.method_kind <- function(m, keys = tfl_ard_methods()) {
  vapply(m, function(one) {
    k <- .method_key(one, keys)
    if (is.na(k)) NA_character_ else keys$kind[k]
  }, "", USE.NAMES = FALSE)
}

# ---- rules on the ARD definition -------------------------------------------

# A06: a grouping variable also analysed
.rule_a06 <- function(x) {
  a <- x$analyses
  out <- list()
  for (i in seq_len(nrow(a))) {
    by <- .bar_names(a$by[i])
    st <- .bar_names(a$strata[i] %||% NA)
    v <- .bar_names(a$variables[i])
    for (k in unique(c(intersect(by, v), intersect(st, v), intersect(by, st)))) {
      out[[length(out) + 1L]] <- .rv("A06", a$output_id[i], "analyses",
                                     a$analysis_id[i], if (k %in% by) "by" else "strata",
                                     args = c(a$analysis_id[i], k))
    }
  }
  out
}

# A08: statistics of a kind the method does not give (the continuous case
# is tfl_ard_spec()'s error)
.rule_a08 <- function(x) {
  a <- x$analyses
  st <- tfl_ard_statistics()
  keys <- tfl_ard_methods()
  out <- list()
  for (i in which(!is.na(a$statistics))) {
    kind <- .method_kind(a$method[i], keys)
    if (is.na(kind) || !kind %in% c("categorical", "missing")) next
    bad <- setdiff(.split_bar(a$statistics[i]), st$statistic[st$kind == kind])
    if (length(bad)) {
      out[[length(out) + 1L]] <- .rv("A08", a$output_id[i], "analyses",
                                     a$analysis_id[i], "statistics",
                                     args = c(a$analysis_id[i], kind,
                                              paste(bad, collapse = ", ")))
    }
  }
  out
}

# the names the ARD program and the ARD use themselves
.ard_own_names <- c("variable", "variable_level", "context", "stat_name",
                    "stat", "stat_label", "stat_fmt", "fmt_fun", "fmt_fn",
                    "warning", "error", "output_id", "analysis_id",
                    "population_id")
.ard_own_name <- function(v) v %in% .ard_own_names | grepl("^group[0-9]+(_level)?$", v)

# A15: a column named as one of the ARD's own; a derived name that is an R
# reserved word (a data_id named as another object is tfl_ard_spec()'s)
.rule_a15 <- function(x) {
  out <- list()
  a <- x$analyses
  for (i in seq_len(nrow(a))) {
    for (cn in c("by", "strata", "variables")) {
      for (v in .bar_names(a[[cn]][i] %||% NA)) {
        if (.ard_own_name(v)) {
          out[[length(out) + 1L]] <- .rv("A15", a$output_id[i], "analyses",
                                         a$analysis_id[i], cn,
                                         args = c(sprintf("`%s` of %s", cn, a$analysis_id[i]),
                                                  v))
        }
      }
    }
  }
  reserved <- c("if", "else", "repeat", "while", "function", "for", "next",
                "break", "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "in",
                "NA_integer_", "NA_real_", "NA_character_")
  sheets <- list(datasets = "dataset", populations = "population_id",
                 analysis_data = "data_id")
  for (s in names(sheets)) {
    d <- x[[s]]
    for (i in seq_len(nrow(d))) {
      for (v in .derive_names(d$derive[i])) {
        if (.ard_own_name(v) || v %in% reserved) {
          out[[length(out) + 1L]] <- .rv(
            "A15", if (s == "analysis_data") d$output_id[i] else NA_character_,
            s, .na_or(d[[sheets[[s]]]][i], ""), "derive",
            args = c("a derived column", v))
        }
      }
    }
  }
  out
}

# ---- rules on the table definition -----------------------------------------

# A report's row of a one-row sheet (`tables`): its own, else the default
.review_rows <- function(d, o) {
  if (is.null(d) || !nrow(d)) return(d)
  mine <- d[!is.na(d$output_id) & d$output_id == o, , drop = FALSE]
  if (nrow(mine)) mine else d[is.na(d$output_id), , drop = FALSE]
}

# the keys a `cols` / `rows` role list names (`group1 = AEBODSYS`)
.role_keys <- function(x) {
  k <- trimws(sub("^.*=", "", .split_bar(x)))
  setdiff(k[nzchar(k)], c("variable", "label", "variable_level", "context",
                          "stat_name"))
}

# The table rules, report by report
.rules_table <- function(sp, x, facts) {
  a_all <- if (!is.null(x)) .review_flat(x$analyses) else NULL
  ids <- .ard_first_seen(c(sp$tables$output_id, sp$cells$output_id,
                           sp$variables$output_id, sp$digits$output_id,
                           a_all$output_id))
  st <- tfl_ard_statistics()
  keys <- tfl_ard_methods()
  kind_stats <- function(k) st$statistic[st$kind == k]
  result_stats <- kind_stats("result")
  out <- list()
  add <- function(r) out[[length(out) + 1L]] <<- r
  for (o in ids) {
    a <- if (is.null(a_all)) NULL else a_all[a_all$output_id == o, , drop = FALSE]
    has_a <- !is.null(a) && nrow(a) > 0L
    tb <- .review_rows(sp$tables, o)
    # a table report: its own tables row, or analyses
    is_table <- has_a || any(sp$tables$output_id %in% o)
    if (!is_table) next
    cells_own <- sp$cells[sp$cells$output_id %in% o, , drop = FALSE]
    cells_def <- sp$cells[is.na(sp$cells$output_id), , drop = FALSE]
    # T09: no column key
    if (!nrow(tb) || is.na(tb$cols[1L])) {
      add(.rv("T09", o, "tables", "", "cols", args = o))
    }
    # T10: analyses but no cells row that applies
    if (has_a && !nrow(cells_own) && !nrow(cells_def)) {
      add(.rv("T10", o, "cells", "", "", args = o))
    }
    have_ard <- !is.null(facts$ard[[o]])
    # T06 / T07 / T08: against what the report's ARD holds
    if (have_ard) {
      for (r in .ard_spec_vs_ard(sp, o, facts$ard[[o]])) add(r)
    }
    if (!has_a) next
    kinds <- .method_kind(a$method, keys)
    # what the review cannot see into: code of one's own, a function the
    # catalog does not know
    opaque <- any(is.na(kinds)) || any(a$method %in% "custom")
    groups <- unique(c(unlist(lapply(a$by, .bar_names)),
                       unlist(lapply(a$strata %||% NA, .bar_names))))
    analysed <- unique(unlist(lapply(a$variables, .bar_names)))
    asked <- unique(unlist(lapply(a$statistics, .split_bar)))
    # T01: a column / row key no analysis groups by or analyses
    if (!opaque && nrow(tb)) {
      for (role in c("cols", "rows")) {
        for (k in setdiff(.role_keys(tb[[role]][1L]), c(groups, analysed))) {
          add(.rv("T01", o, "tables", "", role, args = c(role, k, o)))
        }
      }
    }
    if (!opaque) {
      # T02: a variables row for a variable not analysed nor grouped by
      vr <- sp$variables[sp$variables$output_id %in% o, , drop = FALSE]
      for (v in setdiff(vr$variable, c(analysed, groups))) {
        add(.rv("T02", o, "variables", v, "variable", args = c(v, o)))
      }
      # T04: cells rows for a variable the report has no analysis of
      cv <- setdiff(stats::na.omit(unique(cells_own$variable)),
                    c("continuous", "categorical", "missing"))
      for (v in setdiff(cv, c(analysed, groups))) {
        i <- which(cells_own$variable %in% v)[1L]
        add(.rv("T04", o, "cells", .cells_key(cells_own, i), "variable",
                args = c(v, o)))
      }
    }
    # T03: a statistic of the other kind (when the ARD is there, T06 says
    # what it really has: D11)
    if (!have_ard) {
      analyses_of <- function(v) vapply(a$variables, function(z) v %in% .bar_names(z), NA)
      var_kind <- function(v) {
        k <- unique(kinds[analyses_of(v)])
        k[!is.na(k)]
      }
      for (i in seq_len(nrow(cells_own))) {
        v <- cells_own$variable[i]
        vk <- if (is.na(v)) unique(stats::na.omit(kinds[kinds != "none"])) else
          if (v %in% c("continuous", "categorical", "missing")) v else var_kind(v)
        vk <- setdiff(vk, "none")
        if (length(vk) != 1L) next
        own <- kind_stats(vk)
        # what the variable's analyses ask for by name is theirs
        mine <- if (is.na(v) || v %in% c("continuous", "categorical", "missing")) asked else
          unlist(lapply(a$statistics[analyses_of(v)], .split_bar))
        other <- setdiff(unlist(lapply(setdiff(c("continuous", "categorical",
                                                 "missing"), vk), kind_stats)),
                         c(own, result_stats, mine))
        for (s in intersect(.template_stats(cells_own$template[i]), other)) {
          add(.rv("T03", o, "cells", .cells_key(cells_own, i), "template",
                  args = c(if (is.na(v)) o else v, s,
                           st$kind[match(s, st$statistic)], vk)))
        }
      }
    }
    # T05: digits for a statistic no template reads
    read <- unique(.template_stats(c(cells_own$template, cells_def$template)))
    dg <- sp$digits[sp$digits$output_id %in% o, , drop = FALSE]
    rows_table <- nrow(tb) && identical(tb$stats[1L], "rows")
    if (!rows_table && length(read)) {
      for (i in which(!is.na(dg$statistic) & !dg$statistic %in% read)) {
        add(.rv("T05", o, "digits",
                paste(.na_or(dg$variable[i], ""), dg$statistic[i], sep = " / "),
                "statistic", args = c(o, dg$statistic[i], o)))
      }
    }
  }
  out
}

# the statistics a template reads ({mean}, {n:d})
.template_stats <- function(tpl) {
  tpl <- tpl[!is.na(tpl)]
  tok <- unlist(regmatches(tpl, gregexpr("\\{[^{}]+\\}", tpl)))
  unique(sub("[:}].*$", "", sub("^\\{", "", tok)))
}

.cells_key <- function(d, i) {
  paste(vapply(c("variable", "context", "row"), function(k) .na_or(d[[k]][i], ""), ""),
        collapse = " / ")
}

# What a table reads that an ARD does not have: the column and row keys
# that are neither a group nor a variable of it, the variables the cells
# are written for that it does not analyse, the statistics the templates
# read that it does not have (each once, at the first cells row reading
# it).  tfl_check_ard()'s three rules, and the review's T07 / T06: one
# row each, `check` (cols, rows, cells, statistics), `message`, `sheet`,
# `row`, `field`.
.ard_read_problems <- function(tb, ce, have_vars, have_groups, have_stats) {
  out <- data.frame(check = character(), message = character(),
                    sheet = character(), row = character(), field = character(),
                    stringsAsFactors = FALSE)
  add <- function(check, message, sheet, row, field) {
    out[nrow(out) + 1L, ] <<- list(check, message, sheet, row, field)
  }
  if (NROW(tb)) {
    for (role in intersect(c("cols", "rows"), names(tb))) {
      for (k in .role_keys(tb[[role]][1L])) {
        if (!k %in% c(have_groups, have_vars)) {
          add(role, sprintf(
            "the table's %s name %s, which is neither a group nor a variable of the ARD",
            role, k), "tables", "", role)
        }
      }
    }
  }
  if (NROW(ce)) {
    vv <- setdiff(stats::na.omit(unique(ce$variable)),
                  c("continuous", "categorical"))
    for (v in setdiff(vv, have_vars)) {
      add("cells", sprintf(
        "the cells are written for %s, which the ARD does not analyse", v),
        "cells", .cells_key(ce, which(ce$variable %in% v)[1L]), "variable")
    }
    for (s in setdiff(.template_stats(ce$template), have_stats)) {
      i <- which(vapply(ce$template, function(t) s %in% .template_stats(t), NA))[1L]
      add("statistics", sprintf(
        "a template reads {%s}, a statistic the ARD does not have", s),
        "cells", .cells_key(ce, i), "template")
    }
  }
  out
}

# The report's table against what its ARD holds (`f`: tfl_ard_facts()):
# the keys and variables (T07), the statistics (T06), the levels (T08).
# Its own rows: a default serves many reports, each reading part of it.
.ard_spec_vs_ard <- function(sp, o, f) {
  out <- list()
  p <- .ard_read_problems(.review_rows(sp$tables, o),
                          sp$cells[sp$cells$output_id %in% o, , drop = FALSE],
                          names(f$variables), names(f$groups), f$stats)
  for (i in seq_len(nrow(p))) {
    out[[length(out) + 1L]] <- .rv(if (p$check[i] == "statistics") "T06" else "T07",
                                   o, p$sheet[i], p$row[i], p$field[i],
                                   args = p$message[i])
  }
  vr <- sp$variables[sp$variables$output_id %in% o & !is.na(sp$variables$levels), ,
                     drop = FALSE]
  for (i in seq_len(nrow(vr))) {
    v <- vr$variable[i]
    have <- c(f$variables[[v]]$levels, f$groups[[v]])
    if (!length(have)) next
    bad <- setdiff(.ard_spec_split(vr$levels[i]), have)
    if (length(bad)) {
      out[[length(out) + 1L]] <- .rv("T08", o, "variables", v, "levels",
                                     args = c(v, paste(bad, collapse = ", ")))
    }
  }
  out
}

# C01: a code list of a variable the report does not read
.rule_c01 <- function(sp, x) {
  cl <- sp$codelists
  if (!nrow(cl)) return(list())
  a <- x$analyses
  out <- list()
  for (o in unique(stats::na.omit(cl$output_id))) {
    ao <- a[a$output_id %in% o, , drop = FALSE]
    # a report whose ARD is not made here (a figure, a listing, an ARD taken
    # in): its code lists are read by code the review does not see
    if (!nrow(ao) || any(ao$method %in% "custom") ||
        any(is.na(.method_kind(ao$method)))) next
    read <- .ard_read_vars(.review_flat(ao))
    tb <- .review_rows(sp$tables, o)
    keys <- if (NROW(tb)) c(.role_keys(tb$cols[1L]), .role_keys(tb$rows[1L]))
    for (v in setdiff(unique(cl$variable[cl$output_id %in% o]), c(read, keys))) {
      out[[length(out) + 1L]] <- .rv("C01", o, "codelists", v, "variable",
                                     args = c(v, o))
    }
  }
  out
}

# ---- rules on the data facts -----------------------------------------------

.facts_cols <- function(facts, ds) {
  f <- facts$datasets[[toupper(.na_or(ds, ""))]]
  if (is.null(f)) NULL else f$columns
}

# The facts of a condition (NULL when not counted)
.facts_condition <- function(facts, ds, pop, where) {
  cd <- facts$conditions
  if (is.null(cd) || !nrow(cd)) return(NULL)
  eq <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  i <- which(eq(toupper(cd$dataset), toupper(ds)) & eq(cd$population_id, pop) &
               eq(cd$where, where))
  if (!length(i)) return(NULL)
  cd[i[1L], , drop = FALSE]
}

# A01 / A02 / A03 / A04 / A16: the columns, conditions and flags of the ARD
# definition against the data
.rules_data_ard <- function(x, facts) {
  out <- list()
  add <- function(r) out[[length(out) + 1L]] <<- r
  pops <- x$populations
  # the analysis sets
  for (i in seq_len(nrow(pops))) {
    p <- pops$population_id[i]
    cols <- .facts_cols(facts, pops$dataset[i])
    if (is.na(p) || is.null(cols)) next
    avail <- c(cols$name, .derive_names(x$datasets$derive[
      toupper(x$datasets$dataset) %in% toupper(pops$dataset[i])]))
    miss <- .where_missing(pops$where[i], avail)
    if (length(miss)) {
      add(.rv("A02", NA_character_, "populations", p, "where",
              args = c(pops$where[i], pops$dataset[i],
                       paste("no column", paste(miss, collapse = ", ")))))
      next
    }
    f <- facts$populations[[p]]
    if (is.null(f)) next
    if (!is.na(f$error %||% NA) && !length(miss)) {
      add(.rv("A02", NA_character_, "populations", p, "where",
              args = c(pops$where[i], pops$dataset[i], f$error)))
    } else if (identical(as.integer(f$n_subjects), 0L)) {
      add(.rv("A03", NA_character_, "populations", p, "where",
              args = c(.na_or(pops$where[i], "(none)"), pops$dataset[i], "subjects")))
    }
    fv <- f$flag_values
    if (!is.na(f$flag %||% NA) && length(fv)) {
      odd <- setdiff(fv[!is.na(fv)], c("Y", "N", ""))
      if (length(odd)) {
        add(.rv("A04", NA_character_, "populations", p, "where",
                args = c(f$flag, pops$dataset[i], paste(odd, collapse = ", "))))
      }
    }
    for (v in .derive_names(pops$derive[i])) {
      if (v %in% cols$name) {
        add(.rv("A16", NA_character_, "populations", p, "derive",
                args = c(v, pops$dataset[i])))
      }
    }
  }
  # the analysis data: their columns, made on the way
  ad <- x$analysis_data
  for (i in seq_len(nrow(ad))) {
    if (!is.na(ad$code[i] %||% NA) || is.na(ad$data_id[i])) next
    o <- ad$output_id[i]
    ado <- .adata_of(ad, o)
    ch <- .adata_chain(ado, ad$data_id[i])
    if (is.null(ch)) next
    rows <- ado[match(ch, ado$data_id), , drop = FALSE]
    if (any(!is.na(rows$code %||% NA))) next
    ds <- .adata_dataset(ado, ad$data_id[i])
    cols <- .facts_cols(facts, ds)
    if (is.null(cols)) next
    pop <- .adata_pop(ado, ad$data_id[i])
    above <- rows[-nrow(rows), , drop = FALSE]
    avail <- c(cols$name, .derive_names(above$derive),
               unlist(lapply(above$add, .split_bar)),
               .derive_names(pops$derive[pops$population_id %in% pop]),
               .derive_names(x$datasets$derive[toupper(x$datasets$dataset) %in% toupper(ds)]))
    for (cn in c("add", "keep", "distinct")) {
      for (v in setdiff(.bar_names(ad[[cn]][i]), c(avail, .derive_names(ad$derive[i])))) {
        add(.rv("A01", o, "analysis_data", ad$data_id[i], cn,
                args = c(ad$data_id[i], v, ds)))
      }
    }
    miss <- .where_missing(ad$where[i], c(avail, unlist(lapply(ad$add[i], .split_bar))))
    if (length(miss)) {
      add(.rv("A02", o, "analysis_data", ad$data_id[i], "where",
              args = c(ad$where[i], ds, paste("no column", paste(miss, collapse = ", ")))))
    }
    for (v in .derive_names(ad$derive[i])) {
      if (v %in% cols$name) {
        add(.rv("A16", o, "analysis_data", ad$data_id[i], "derive", args = c(v, ds)))
      }
    }
  }
  # the analyses
  a <- .review_flat(x$analyses)
  seen <- character()
  for (i in seq_len(nrow(a))) {
    if (a$method[i] %in% "custom") next
    s <- .review_source(x, a, i)
    if (is.null(s)) next
    cols <- .facts_cols(facts, s$dataset)
    if (is.null(cols)) next
    avail <- c(cols$name, s$extra)
    for (cn in c("by", "strata", "variables")) {
      for (v in setdiff(.bar_names(a[[cn]][i] %||% NA), avail)) {
        add(.rv("A01", a$output_id[i], "analyses", a$analysis_id[i], cn,
                args = c(a$analysis_id[i], v, s$dataset)))
      }
    }
    miss <- .where_missing(a$where[i], avail)
    if (length(miss)) {
      add(.rv("A02", a$output_id[i], "analyses", a$analysis_id[i], "where",
              args = c(a$where[i], s$dataset,
                       paste("no column", paste(miss, collapse = ", ")))))
      next
    }
    f <- .facts_condition(facts, s$dataset, s$population_id, s$where)
    if (is.null(f) || is.na(s$where)) next
    key <- paste(a$output_id[i], s$dataset, s$population_id, s$where)
    if (key %in% seen) next
    seen <- c(seen, key)
    # a condition that fails on columns the data have (not on one made on
    # the way, which the facts cannot see)
    if (!is.na(f$error) && !length(.where_missing(s$where, cols$name))) {
      add(.rv("A02", a$output_id[i], "analyses", a$analysis_id[i],
              if (!is.na(a$where[i])) "where" else "data",
              args = c(s$where, s$dataset, f$error)))
    } else if (identical(as.integer(f$n_rows), 0L)) {
      add(.rv("A03", a$output_id[i], "analyses", a$analysis_id[i],
              if (!is.na(a$where[i])) "where" else "data",
              args = c(s$where, s$dataset, "rows")))
    }
  }
  out
}

# the columns a condition names that are not in `avail` (none when it does
# not read as R: tfl_ard_spec() says that)
.where_missing <- function(where, avail) {
  if (is.null(where) || is.na(where)) return(character())
  e <- tryCatch(str2lang(where), error = function(e) NULL)
  if (is.null(e)) return(character())
  v <- all.vars(e)
  # T / F and the constants R has
  setdiff(v, c(avail, "T", "F", "pi", "LETTERS", "letters", "month.abb",
               "month.name"))
}

# A05: a continuous method on a character column; a categorical one on a
# numeric column of many values
.rule_a05 <- function(x, facts) {
  a <- .review_flat(x$analyses)
  keys <- tfl_ard_methods()
  out <- list()
  for (i in seq_len(nrow(a))) {
    kind <- .method_kind(a$method[i], keys)
    if (is.na(kind) || !kind %in% c("continuous", "categorical")) next
    s <- .review_source(x, a, i)
    if (is.null(s)) next
    cols <- .facts_cols(facts, s$dataset)
    if (is.null(cols)) next
    for (v in .bar_names(a$variables[i])) {
      k <- match(v, cols$name)
      if (is.na(k)) next
      cls <- cols$class[k]
      odd <- (kind == "continuous" && cls %in% c("character", "factor")) ||
        (kind == "categorical" && cls == "numeric" && cols$n_distinct[k] > 20L)
      if (odd) {
        out[[length(out) + 1L]] <- .rv("A05", a$output_id[i], "analyses",
                                       a$analysis_id[i], "method",
                                       args = c(v, kind, a$analysis_id[i], cls))
      }
    }
  }
  out
}

# C02 / C03 / C05: the code lists against the values of the data the
# report's analyses read
.rules_codelists <- function(sp, x, facts) {
  cl <- sp$codelists
  if (!nrow(cl)) return(list())
  a_all <- .review_flat(x$analyses)
  maxl <- facts$max_levels %||% 200L
  out <- list()
  add <- function(r) out[[length(out) + 1L]] <<- r
  for (o in unique(stats::na.omit(cl$output_id))) {
    a <- a_all[a_all$output_id %in% o & !a_all$method %in% "custom", , drop = FALSE]
    if (!nrow(a)) next
    for (v in unique(cl$variable[cl$output_id %in% o & !is.na(cl$variable)])) {
      users <- which(vapply(seq_len(nrow(a)), function(i)
        v %in% unlist(lapply(c(a$by[i], a$strata[i] %||% NA, a$variables[i]),
                             .bar_names)), NA))
      if (!length(users)) next
      seen <- NULL
      capped <- FALSE
      for (i in users) {
        s <- .review_source(x, a, i)
        if (is.null(s)) next
        vals <- .facts_values_for(facts, s, v)
        if (identical(vals, "capped")) {
          capped <- TRUE
          next
        }
        if (is.null(vals)) next
        seen <- union(seen, vals)
      }
      if (capped) {
        add(.rv("C05", o, "codelists", v, "value", args = c(v, maxl)))
        next
      }
      if (is.null(seen)) next
      rows <- cl[cl$output_id %in% o & cl$variable %in% v, , drop = FALSE]
      listed <- rows$value
      for (val in setdiff(listed, seen)) {
        add(.rv("C02", o, "codelists", paste(v, val, sep = " / "), "value",
                args = c(sQuote(val, FALSE), v, o)))
      }
      for (val in setdiff(seen[nzchar(seen)], listed)) {
        add(.rv("C03", o, "codelists", v, "value",
                args = c(sQuote(val, FALSE), v, o)))
      }
    }
  }
  out
}

# The values of `v` in the data an analysis reads (`s`: .review_source()):
# its condition's rows when counted, else its analysis set's subjects,
# else the whole dataset; "capped" when there are too many; NULL unknown
.facts_values_for <- function(facts, s, v) {
  pick <- function(vals) {
    if (!v %in% names(vals)) return(NULL)
    if (is.null(vals[[v]])) "capped" else vals[[v]]
  }
  f <- .facts_condition(facts, s$dataset, s$population_id, s$where)
  if (!is.null(f) && is.na(f$error) && !is.na(f$n_rows)) {
    r <- pick(f$values[[1L]])
    if (!is.null(r)) return(r)
  }
  if (is.na(s$where) && !is.na(s$population_id)) {
    p <- facts$populations[[s$population_id]]
    if (!is.null(p) && identical(toupper(p$dataset), s$dataset)) {
      r <- pick(p$values)
      if (!is.null(r)) return(r)
    }
  }
  if (!is.na(s$where) || !is.na(s$population_id)) return(NULL)
  cols <- .facts_cols(facts, s$dataset)
  k <- match(v, cols$name)
  if (is.na(k)) return(NULL)
  if (is.null(cols$values[[k]])) {
    return(if (cols$class[k] %in% c("character", "factor")) "capped" else NULL)
  }
  cols$values[[k]]
}

# L02: a listing's dataset and columns against the data (A01's and A03's
# rules for its `where`)
.rule_l02 <- function(lf, x, facts) {
  l <- lf$listings
  cl <- lf$listing_cols
  known <- toupper(c(names(facts$datasets), x$datasets$dataset))
  out <- list()
  add <- function(r) out[[length(out) + 1L]] <<- r
  for (i in seq_len(nrow(l))) {
    o <- l$output_id[i]
    ds <- l$dataset[i]
    if (is.na(o) || is.na(ds)) next
    if (!toupper(ds) %in% known) {
      add(.rv("L02", o, "listings", "", "dataset",
              args = sprintf("The listing %s reads the dataset %s, which is not in the catalog.",
                             o, ds)))
      next
    }
    cols <- .facts_cols(facts, ds)
    if (is.null(cols)) next
    miss <- setdiff(sub("^-", "", .split_bar(l$sort[i])), cols$name)
    if (length(miss)) {
      add(.rv("L02", o, "listings", "", "sort", args = sprintf(
        "The listing %s sorts by %s, which %s does not have.", o,
        paste(miss, collapse = ", "), ds)))
    }
    mine <- which(cl$output_id %in% o)
    for (j in seq_along(mine)) {
      miss <- setdiff(.bar_names(cl$vars[mine[j]]), cols$name)
      if (length(miss)) {
        add(.rv("L02", o, "listing_cols", as.character(j), "vars", args = sprintf(
          "Column %d of the listing %s shows %s, which %s does not have.", j, o,
          paste(miss, collapse = ", "), ds)))
      }
    }
    miss <- .where_missing(l$where[i], cols$name)
    if (length(miss)) {
      add(.rv("A02", o, "listings", "", "where", area = "listing",
              args = c(l$where[i], ds, paste("no column", paste(miss, collapse = ", ")))))
      next
    }
    f <- .facts_condition(facts, ds, NA_character_, l$where[i])
    if (!is.null(f) && !is.na(l$where[i])) {
      if (!is.na(f$error)) {
        add(.rv("A02", o, "listings", "", "where", area = "listing",
                args = c(l$where[i], ds, f$error)))
      } else if (identical(as.integer(f$n_rows), 0L)) {
        add(.rv("A03", o, "listings", "", "where", area = "listing",
                args = c(l$where[i], ds, "rows")))
      }
    }
  }
  out
}

# A17: what the last ARD run of each report said went wrong
.rule_a17 <- function(facts) {
  out <- list()
  for (o in names(facts$ard)) {
    cd <- facts$ard[[o]]$conditions
    if (is.null(cd) || !nrow(cd)) next
    for (i in seq_len(nrow(cd))) {
      aid <- if ("analysis_id" %in% names(cd)) cd$analysis_id[i] else NA
      out[[length(out) + 1L]] <- .rv(
        "A17", o, "analyses", .na_or(aid, ""), "",
        level = if (identical(cd$level[i], "error")) "error" else "check",
        args = sprintf("%s%s: %s", .na_or(cd$variable[i], ""),
                       if (nzchar(.na_or(cd$groups[i], ""))) paste0(" (", cd$groups[i], ")") else "",
                       cd$message[i]))
    }
  }
  out
}

# ---- figures ---------------------------------------------------------------

# F01 / F02 / F03 and the design's own problems (S01)
.rules_figures <- function(figures, facts) {
  out <- list()
  add <- function(r) out[[length(out) + 1L]] <<- r
  adam <- if (!is.null(facts)) .facts_frames(facts)
  for (o in names(figures)) {
    d <- figures[[o]]
    d <- tryCatch(if (inherits(d, "tfl_fig_design")) d else .fig_design_from_list(d),
                  error = function(e) e)
    if (inherits(d, "error")) {
      add(.rv("S01", o, "design", "", "", area = "figure",
              args = conditionMessage(d)))
      next
    }
    p <- tfl_check_fig_design(d)
    for (i in seq_len(nrow(p))) {
      if (identical(p$problem[i], "is required")) {
        add(.rv("F01", o, "design", p$part[i], p$field[i],
                args = c(p$part[i], p$field[i])))
      } else {
        add(.rv("S01", o, "design", p$part[i], p$field[i], area = "figure",
                args = paste(p$part[i], p$field[i], p$problem[i])))
      }
    }
    adv <- tryCatch(tfl_fig_advice(d), error = function(e) NULL)
    for (i in seq_len(NROW(adv))) {
      add(.rv("F02", o, "design", adv$part[i], "", args = adv$message[i],
              fix = adv$fix[[i]]))
    }
    if (!is.null(adam)) {
      q <- tryCatch(tfl_check_fig_design(d, adam), error = function(e) NULL)
      if (NROW(q)) {
        new <- !paste(q$part, q$field, q$problem) %in% paste(p$part, p$field, p$problem)
        for (i in which(new)) {
          add(.rv("F03", o, "design", q$part[i], q$field[i],
                  args = paste(q$part[i], q$field[i], q$problem[i])))
        }
      }
    }
  }
  out
}

# Frames that hold what the facts know (each column's class and values),
# for the checks that take data: one row per value, the values recycled
.facts_frames <- function(facts) {
  lapply(facts$datasets, function(f) {
    cols <- f$columns
    n <- max(1L, lengths(cols$values))
    out <- lapply(seq_len(nrow(cols)), function(j) {
      v <- cols$values[[j]]
      switch(cols$class[j],
        numeric = rep(0, n),
        logical = rep(NA, n),
        Date = rep(as.Date(NA), n),
        if (length(v)) rep_len(v, n) else rep(NA_character_, n))
    })
    as.data.frame(stats::setNames(out, cols$name), stringsAsFactors = FALSE,
                  check.names = FALSE)
  })
}
