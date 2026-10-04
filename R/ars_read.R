# ============================================================================
#  CDISC ARS: reading a reporting event back into specs
# ============================================================================
#
#  The other way round from tfl_ars(): an ARS JSON -- tflspec's own or anyone
#  else's -- read as the model (tfl_read_ars_json()), then as tflspec specs
#  (tfl_ars_to_specs()):
#
#    analysisSets           -> populations
#    dataSubsets            -> analyses$where (a WhereClause as R)
#    analyses + methods     -> analyses (method, variables, by)
#    analysisGroupings      -> by; listed groups -> table spec levels
#    outputs, list of contents, displays -> the report spec
#
#  The method of an analysis is found from tflspec's own method ids
#  (Mth_<method>), else from what its operations compute (mean / sd ... is
#  a summary, n with a percentage a count, a p-value a test ...).  The
#  layout is read as tfl_ars() writes it and as CDISC's example does: a
#  count of the subject key grouped by a variable is a categorical analysis
#  of that variable.  What has no place in the specs is listed in
#  attr(, "unmapped"), with the reason, never dropped silently.
# ============================================================================

#' Read a CDISC ARS reporting event from JSON
#'
#' Reads an ARS v1.0 JSON file -- written by [tfl_write_ars_json()] or by
#' any other tool -- as the model [tfl_ars()] makes.  A reference that
#' names nothing (a method, an analysis set, a grouping ...) stops it; a
#' missing purpose or reason is warned about.  [tfl_ars_to_specs()] turns
#' it into specs.
#'
#' @param path An ARS `.json` file.
#' @return A `tfl_ars`.  Its profile is `"siera"` when its methods carry
#'   siera code templates, else `"cdisc"`.
#' @seealso [tfl_ars_to_specs()], [tfl_check_ars()]
#' @export
tfl_read_ars_json <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    .ard_stop("`path` must be an existing ARS .json file.")
  }
  re <- jsonlite::fromJSON(paste(readLines(path, warn = FALSE,
                                           encoding = "UTF-8"),
                                 collapse = "\n"), simplifyVector = FALSE)
  if (!is.list(re) || is.null(re$id) || is.null(re$analyses)) {
    .ard_stop(sprintf("%s is not an ARS reporting event (no id or analyses).",
                      path))
  }
  ctx <- unlist(lapply(re$methods, function(m) m$codeTemplate$context))
  profile <- if (any(ctx %in% .ars_siera_context)) "siera" else "cdisc"
  ars <- structure(re, class = "tfl_ars", profile = profile,
                   unmapped = data.frame(where = character(),
                                         item = character(),
                                         reason = character(),
                                         stringsAsFactors = FALSE))
  ck <- tfl_check_ars(ars, schema = FALSE, profile = "cdisc")
  soft <- ck$field %in% c("purpose", "reason")
  if (any(!soft)) {
    .ard_stop(paste(c(sprintf("%s does not hold together as ARS:", path),
                      sprintf("%s %s: %s", ck$part[!soft], ck$field[!soft],
                              ck$problem[!soft])), collapse = "\n  "))
  }
  if (any(soft)) {
    warning(sprintf("%d analyses without a CDISC purpose or reason.",
                    length(unique(ck$part[soft]))), call. = FALSE)
  }
  ars
}

# -- WhereClauses as R -----------------------------------------------------

.ars_r_value <- function(v) {
  v <- as.character(unlist(v))
  num <- grepl("^-?[0-9]+(\\.[0-9]+)?$", v)
  ifelse(num, v, encodeString(v, quote = "\""))
}

.ars_r_cond <- function(w, top = TRUE) {
  if (!is.null(w$condition)) {
    c <- w$condition
    v <- .ars_r_value(c$value)
    op <- c(EQ = "==", NE = "!=", GT = ">", GE = ">=", LT = "<",
            LE = "<=")[c$comparator]
    if (c$comparator %in% c("IN", "NOTIN") || (!is.na(op) && length(v) > 1L)) {
      s <- sprintf("%s %%in%% c(%s)", c$variable, paste(v, collapse = ", "))
      if (c$comparator %in% c("NOTIN", "NE")) s <- sprintf("!(%s)", s)
      return(s)
    }
    if (is.na(op)) return(NA_character_)
    return(sprintf("%s %s %s", c$variable, op, v))
  }
  ce <- w$compoundExpression
  if (is.null(ce)) return(NA_character_)
  parts <- vapply(ce$whereClauses, .ars_r_cond, "", top = FALSE)
  if (anyNA(parts)) return(NA_character_)
  s <- switch(ce$logicalOperator,
              AND = paste(parts, collapse = " & "),
              OR = paste(parts, collapse = " | "),
              NOT = sprintf("!(%s)", parts[1L]),
              NA_character_)
  if (!top && ce$logicalOperator == "OR") s <- sprintf("(%s)", s)
  s
}

# The datasets a WhereClause names.
.ars_cond_datasets <- function(w) {
  if (!is.null(w$condition)) return(w$condition$dataset)
  unlist(lapply(w$compoundExpression$whereClauses, .ars_cond_datasets))
}

# -- methods: what tflspec method each ARS method is -------------------------

.ars_op_alias <- c(
  n = "n", count = "n", "count of subjects" = "n", N = "N",
  mean = "mean", sd = "sd", "standard deviation" = "sd", median = "median",
  q1 = "p25", p25 = "p25", "first quartile" = "p25", q3 = "p75",
  p75 = "p75", "third quartile" = "p75", min = "min", minimum = "min",
  max = "max", maximum = "max", pct = "p", "%" = "p", p = "p",
  percentage = "p", pval = "p.value", "p-value" = "p.value",
  p.value = "p.value", estimate = "estimate", conf.low = "conf.low",
  conf.high = "conf.high")

# An operation as a cards statistic name (NA when it is none).
.ars_op_stat <- function(o) {
  for (x in c(o$label, sub("^.*_", "", o$id %||% ""), o$name)) {
    if (is.null(x)) next
    k <- .ars_op_alias[x]
    if (is.na(k)) k <- .ars_op_alias[tolower(x)]
    if (!is.na(k)) return(unname(k))
  }
  NA_character_
}

# A method: list(method, stats, opt, code), method NA when tflspec has none.
.ars_method_of <- function(m) {
  keys <- tfl_ard_methods()$method
  ops <- m$operations %||% list()
  stats <- vapply(ops, .ars_op_stat, "")
  id <- m$id %||% ""
  code <- m$codeTemplate$code
  out <- function(method, opt = NULL, code = NULL) {
    list(method = method, stats = stats, opt = opt, code = code)
  }
  # tflspec's own ids: Mth_<method>[_<option>][_<n>]
  if (startsWith(id, "Mth_")) {
    rest <- sub("^Mth_", "", id)
    if (rest == "custom" || startsWith(rest, "custom_")) {
      return(out("custom", code = code))
    }
    siera <- c(total_n = "total_n", categorical = "categorical",
               flat = "subjects", nested2 = "hierarchical",
               nested3 = "hierarchical", continuous = "continuous",
               chisq = "chisq")
    for (k in names(siera)) {
      if (identical(rest, k) || startsWith(rest, paste0(k, "_"))) {
        if (k == "continuous" || k == "categorical" || k == "total_n" ||
            k == "chisq" || !is.null(m$codeTemplate)) {
          return(out(unname(siera[k])))
        }
      }
    }
    hit <- keys[vapply(keys, function(k) identical(rest, k) ||
                         startsWith(rest, paste0(k, "_")), NA)]
    if (length(hit)) {
      k <- hit[which.max(nchar(hit))]
      opt <- NULL
      if (k == "proportion_ci") {
        o <- sub("^proportion_ci_?", "", rest)
        o <- sub("_[0-9]+$", "", o)
        if (nzchar(o)) opt <- o
      }
      return(out(k, opt = opt))
    }
    if (!is.null(code) && grepl("^[A-Za-z.][A-Za-z0-9.]*::[A-Za-z._][A-Za-z0-9._]*\\(",
                                code)) {
      return(out(sub("\\(.*$", "", code)))
    }
  }
  # anyone's: what the operations compute
  txt <- tolower(paste(m$name, m$description, m$label))
  st <- stats[!is.na(stats)]
  if (any(c("mean", "sd", "median") %in% st)) return(out("continuous"))
  if ("p.value" %in% st) {
    if (grepl("fisher|fishex", txt)) return(out("fisher"))
    if (grepl("chi", txt)) return(out("chisq"))
    if (grepl("wilcox|rank", txt)) return(out("wilcox"))
    if (grepl("t-test|t test|ttest", txt)) return(out("ttest"))
    return(out(NA_character_))
  }
  if (all(c("conf.low", "conf.high") %in% st)) return(out("proportion_ci"))
  if ("p" %in% st) return(out("categorical"))
  if (length(st) && all(st %in% c("n", "N"))) return(out("total_n"))
  out(NA_character_)
}

# -- the specs --------------------------------------------------------------

#' A CDISC ARS reporting event as tflspec specs
#'
#' Turns a reporting event ([tfl_read_ars_json()], or a [tfl_ars()]) into
#' the specs it says:
#'
#' * the **ARD spec**: analysis sets as `populations`, analyses as
#'   `analyses` rows (method, dataset, population, `where`, `by`,
#'   `variables`, `statistics`, `purpose`, `reason`), the datasets they
#'   read as `datasets` (without a path: ARS does not say where the ADaM
#'   is);
#' * the **report spec**: one `report` row per output in the list of
#'   contents' order, its file, and its display's titles, footnotes,
#'   header and footer;
#' * with `table = TRUE`, a **table spec** with the `variables` rows the
#'   groupings give: a grouping's listed groups as the variable's `levels`.
#'
#' tfl_ars()'s own layout comes back as it was written (one row with
#' several variables, a hierarchy as one row); anyone else's analysis
#' becomes one row each.  A method is read from tflspec's ids, else from
#' what its operations compute; an analysis whose method tflspec has no
#' keyword for (an ANOVA, SAS code only ...) is not made a row and is
#' listed in `unmapped`.
#'
#' @param ars A [tfl_ars()] or [tfl_read_ars_json()].
#' @param table Also a table spec.
#' @return A list: `ard` (a [tfl_ard_spec()]), `report` (a [tfl_table_spec()]
#'   holding the report sheets) and, with `table = TRUE`, `table`.
#'   Attribute `unmapped`: a data frame `where`, `item`, `reason`.
#' @seealso [tfl_read_ars_json()], [tfl_ars()]
#' @export
tfl_ars_to_specs <- function(ars, table = FALSE) {
  if (!inherits(ars, "tfl_ars")) {
    .ard_stop("`ars` must be a tfl_ars() or tfl_read_ars_json().")
  }
  re <- .ars_plain(ars)
  un <- data.frame(where = character(), item = character(),
                   reason = character(), stringsAsFactors = FALSE)
  miss <- function(where, item, why) {
    un[nrow(un) + 1L, ] <<- list(where, item, why)
  }
  by_id <- function(k) stats::setNames(re[[k]] %||% list(),
                                       vapply(re[[k]] %||% list(), `[[`, "",
                                              "id"))
  sets <- by_id("analysisSets")
  subs <- by_id("dataSubsets")
  grps <- by_id("analysisGroupings")
  meths <- by_id("methods")
  ans <- by_id("analyses")

  # the subject key: the variable of the counts (USUBJID unless said)
  vv <- vapply(ans, function(a) a$variable %||% "", "")
  subj <- if ("USUBJID" %in% vv) "USUBJID" else {
    t <- table(vv[grepl("SUBJ|^ID$", vv)])
    if (length(t)) names(t)[which.max(t)] else "USUBJID"
  }

  # populations
  pops <- data.frame(population_id = character(), dataset = character(),
                     where = character(), stringsAsFactors = FALSE)
  for (s in sets) {
    w <- if (is.null(s$condition) && is.null(s$compoundExpression)) {
      NA_character_
    } else .ars_r_cond(s)
    ds <- unique(.ars_cond_datasets(s))
    if (!is.null(s$condition) || !is.null(s$compoundExpression)) {
      if (is.na(w)) miss(s$id, "analysisSets", "a condition with no R form")
    }
    if (length(ds) > 1L) {
      miss(s$id, "analysisSets", sprintf(
        "conditions on several datasets (%s): a population is of one",
        paste(ds, collapse = ", ")))
    }
    pops[nrow(pops) + 1L, ] <- list(s$id, ds[1L] %||% NA_character_, w)
  }

  # outputs in the list of contents' order, each with its analyses
  items <- re$mainListOfContents$contentsList$listItems %||% list()
  flat_ids <- function(it) {
    c(it$analysisId, unlist(lapply(it$sublist$listItems, flat_ids)))
  }
  out_items <- list()
  walk <- function(its) {
    for (it in its) {
      if (!is.null(it$outputId)) {
        out_items[[length(out_items) + 1L]] <<- list(
          id = it$outputId, analyses = unlist(lapply(it$sublist$listItems,
                                                     flat_ids)))
      } else walk(it$sublist$listItems)
    }
  }
  walk(items)
  listed <- unlist(lapply(out_items, `[[`, "analyses"))
  # analyses no output lists: an output of their own, "(unlisted)"
  rest <- setdiff(names(ans), listed)
  if (length(rest)) {
    out_items[[length(out_items) + 1L]] <- list(id = "UNLISTED",
                                                analyses = rest)
    miss("UNLISTED", "mainListOfContents", sprintf(
      "%d analyses in no output: written under output_id UNLISTED",
      length(rest)))
  }

  # each analysis as a row
  gvar <- function(gid) grps[[gid]]$groupingVariable %||% NA_character_
  rows <- list()
  for (oi in out_items) {
    o <- oi$id
    for (aid in unique(oi$analyses)) {
      a <- ans[[aid]]
      if (is.null(a)) next
      tag <- paste(o, aid, sep = " / ")
      mm <- .ars_method_of(meths[[a$methodId]] %||% list())
      g <- a$orderedGroupings %||% list()
      g <- g[order(vapply(g, function(z) as.integer(z$order %||% 0L), 1L))]
      gv <- vapply(g, function(z) gvar(z$groupingId), "")
      res <- vapply(g, function(z) isTRUE(z$resultsByGroup), NA)
      pre <- paste0("An_", o, "_")
      own <- if (startsWith(aid, pre)) substring(aid, nchar(pre) + 1L) else aid
      tfl_made <- !identical(own, aid)
      # the subject count tfl_ars() added for a percentage's denominator
      if (tfl_made && grepl("^BIGN_", own) &&
          sub("_[0-9]+$", "", sub("^BIGN_", "", own)) %in%
          c(if (length(gv)) paste(gv, collapse = "_") else "ALL")) {
        next
      }
      if (is.na(mm$method)) {
        miss(tag, "method", sprintf(
          "method %s: tflspec has no keyword for what it computes",
          a$methodId %||% "?"))
        next
      }
      m <- mm$method
      var <- a$variable %||% NA_character_
      by <- gv
      vars <- var
      level <- NA_character_
      # a count of subjects in record-level data (the data is not the
      # population's): subjects with a record by the first grouping, or a
      # hierarchy below it
      pds <- if (!is.null(a$analysisSetId) && a$analysisSetId %in%
                 pops$population_id) {
        pops$dataset[match(a$analysisSetId, pops$population_id)]
      } else NA_character_
      rec <- !is.null(a$dataset) && !is.na(pds) && !identical(a$dataset, pds)
      if (!tfl_made && m == "categorical" && identical(var, subj) && rec &&
          length(gv)) {
        m <- if (length(gv) == 1L) "subjects" else "hierarchical"
      }
      if (m == "subjects") {
        vars <- NA_character_
      } else if (m %in% c("categorical", "chisq", "fisher") &&
                 identical(var, subj)) {
        # a count of the subject key grouped by the variable
        if (length(gv)) {
          vars <- gv[length(gv)]
          by <- gv[-length(gv)]
        }
      } else if (m == "proportion_ci" && identical(var, subj)) {
        vars <- gv[length(gv)]
        by <- gv[-length(gv)]
      } else if (m == "total_n") {
        # the subject count by group: a categorical of the group variable
        m <- "categorical"
        vars <- gv[length(gv)]
        by <- gv[-length(gv)]
      } else if (m == "hierarchical") {
        level <- "L"
        vars <- gv[-1L]
        by <- gv[1L]
      }
      # tfl_ars()'s hierarchy: An_<out>_<id>_ANY / _L<k>
      if (tfl_made && grepl("_(ANY|L[0-9]+)$", own) &&
          m %in% c("categorical", "hierarchical", "subjects")) {
        level <- sub("^.*_", "", own)
        own <- sub("_(ANY|L[0-9]+)$", "", own)
        m <- "hierarchical"
        if (level == "ANY") {
          vars <- character()
        } else {
          k <- as.integer(sub("^L", "", level))
          by <- gv[seq_len(length(gv) - k)]
          vars <- gv[seq(length(gv) - k + 1L, length(gv))]
        }
      }
      if (length(by) && any(!res[seq_along(by)]) &&
          !m %in% c("chisq", "fisher", "ttest", "wilcox")) {
        miss(tag, "orderedGroupings", "a grouping without results by group")
      }
      ss <- a$dataSubsetId
      where <- if (is.null(ss)) NA_character_ else {
        w <- .ars_r_cond(subs[[ss]])
        if (is.na(w)) miss(tag, "dataSubsetId",
                           sprintf("data subset %s has no R form", ss))
        w
      }
      st <- mm$stats[!is.na(mm$stats)]
      # a summary's n is the number of values: cards' N
      if (m == "continuous") st[st == "n"] <- "N"
      stats <- NA_character_
      if (m == "continuous" && length(st) &&
          !identical(st, .ars_method_stats("continuous"))) {
        stats <- paste(st, collapse = " | ")
      }
      args <- NA_character_
      if (m == "proportion_ci" && !is.null(mm$opt)) {
        args <- sprintf("method = \"%s\"", mm$opt)
      }
      rows[[length(rows) + 1L]] <- list(
        output_id = o, analysis_id = own, ars_id = aid, method = m,
        dataset = a$dataset %||% NA_character_,
        population_id = a$analysisSetId %||% NA_character_, where = where,
        by = by, variables = vars, level = level, statistics = stats,
        args = args, code = mm$code %||% NA_character_,
        name = a$name %||% NA_character_,
        purpose = a$purpose$controlledTerm %||% NA_character_,
        reason = a$reason$controlledTerm %||% NA_character_,
        tfl_made = tfl_made)
    }
  }

  # rows tfl_ars() split (one per variable, one per depth) back into one
  analyses <- list()
  keyof <- function(r) paste(r$output_id, r$method, r$dataset,
                             r$population_id, r$where, r$statistics,
                             r$args, r$purpose, r$reason, sep = "\r")
  for (r in rows) {
    j <- NA_integer_
    if (r$tfl_made && length(analyses)) {
      last <- analyses[[length(analyses)]]
      split_id <- if (r$method == "hierarchical") r$analysis_id else
        sub(paste0("_", r$variables[1L], "$"), "", r$analysis_id)
      if (identical(keyof(last), keyof(r)) &&
          identical(last$analysis_id, split_id)) {
        if (r$method == "hierarchical") {
          j <- length(analyses)
        } else if (identical(last$by, r$by) &&
                   paste0(split_id, "_", r$variables) == r$analysis_id &&
                   isTRUE(last$several)) {
          j <- length(analyses)
        }
      }
    }
    if (!is.na(j)) {
      l <- analyses[[j]]
      if (r$method == "hierarchical") {
        if (identical(r$level, "ANY")) l$over <- TRUE else {
          if (length(r$variables) > length(l$variables)) {
            l$variables <- r$variables
            l$by <- r$by
          }
        }
      } else {
        l$variables <- c(l$variables, r$variables)
      }
      l$labels <- c(l$labels, r$name)
      analyses[[j]] <- l
      next
    }
    if (r$tfl_made && r$method != "hierarchical" &&
        length(r$variables) == 1L && !is.na(r$variables) &&
        endsWith(r$analysis_id, paste0("_", r$variables))) {
      r$analysis_id <- sub(paste0("_", r$variables, "$"), "", r$analysis_id)
      r$several <- TRUE
    }
    r$over <- identical(r$level, "ANY")
    if (r$method == "hierarchical" && identical(r$level, "ANY")) {
      r$variables <- character()
    }
    r$labels <- r$name
    analyses[[length(analyses) + 1L]] <- r
  }

  # the default names tfl_ars() gives are not labels
  plain <- function(r) {
    nm <- unique(r$labels)
    if (length(nm) != 1L || is.na(nm)) return(NA_character_)
    if (nm %in% c(r$variables, r$analysis_id, "Number of subjects", "Any",
                  paste(r$variables, collapse = " / "))) return(NA_character_)
    if (r$method == "hierarchical") return(sub(":.*$", "", nm))
    nm
  }
  an <- data.frame(
    output_id = vapply(analyses, `[[`, "", "output_id"),
    analysis_id = vapply(analyses, `[[`, "", "analysis_id"),
    label = vapply(analyses, plain, ""),
    method = vapply(analyses, `[[`, "", "method"),
    dataset = vapply(analyses, `[[`, "", "dataset"),
    population_id = vapply(analyses, `[[`, "", "population_id"),
    where = vapply(analyses, `[[`, "", "where"),
    by = vapply(analyses, function(r) if (length(r$by) && !all(is.na(r$by)))
      paste(r$by, collapse = " | ") else NA_character_, ""),
    variables = vapply(analyses, function(r) {
      v <- r$variables[!is.na(r$variables)]
      if (length(v)) paste(v, collapse = " | ") else NA_character_
    }, ""),
    statistics = vapply(analyses, `[[`, "", "statistics"),
    strata = NA_character_,
    denominator = NA_character_,
    formats = NA_character_,
    args = vapply(analyses, function(r) {
      if (r$method == "hierarchical" && isTRUE(r$over)) {
        return("over_variables = TRUE")
      }
      r$args
    }, ""),
    code = vapply(analyses, `[[`, "", "code"),
    purpose = vapply(analyses, `[[`, "", "purpose"),
    reason = vapply(analyses, `[[`, "", "reason"),
    stringsAsFactors = FALSE)
  # the dataset is the population's when it is the same
  pd <- unname(stats::setNames(pops$dataset, pops$population_id)[
    an$population_id])
  an$dataset[which(an$dataset == pd)] <- NA_character_
  dup <- duplicated(paste(an$output_id, an$analysis_id))
  if (any(dup)) {
    an$analysis_id[dup] <- make.unique(paste(an$output_id, an$analysis_id),
                                       sep = "_")[dup]
    an$analysis_id[dup] <- sub("^\\S+ ", "", an$analysis_id[dup])
  }

  datasets <- unique(stats::na.omit(c(pops$dataset, an$dataset,
                                      vapply(rows, `[[`, "", "dataset"))))
  ds <- data.frame(dataset = datasets, level = NA_character_,
                   path = NA_character_, derive = NA_character_,
                   stringsAsFactors = FALSE)
  if (length(datasets)) {
    miss("datasets", "path",
         "ARS does not say where the ADaM is: fill datasets$path")
  }
  study <- data.frame(key = c("id", "study_id"),
                      value = c(subj, re$id %||% NA_character_),
                      stringsAsFactors = FALSE)
  pop <- data.frame(population_id = pops$population_id,
                    dataset = pops$dataset, where = pops$where,
                    derive = NA_character_, stringsAsFactors = FALSE)
  # a column ARS has no place for is blank
  for (cn in setdiff(.ard_spec_sheets$analyses, names(an))) {
    an[[cn]] <- rep(NA_character_, nrow(an))
  }
  ard <- tfl_ard_spec(list(study = study, datasets = ds, populations = pop,
                           analyses = an[.ard_spec_sheets$analyses]))

  # the report spec: outputs, their files and display sections
  outs <- by_id("outputs")
  oids <- vapply(out_items, `[[`, "", "id")
  oids <- oids[oids != "UNLISTED"]
  sec <- c(Title = "titles", Footnote = "footnotes", Abbreviation = "footnotes",
           Header = "header", Footer = "footer")
  glob <- list()
  for (gs in re$globalDisplaySections) {
    for (z in gs$subSections) glob[[z$id]] <- z$text
  }
  sh <- list(titles = list(), footnotes = list(), header = list(),
             footer = list())
  report <- data.frame(output_id = oids, type = "table",
                       file = NA_character_, stringsAsFactors = FALSE)
  for (i in seq_along(oids)) {
    o <- outs[[oids[i]]]
    if (is.null(o)) next
    f <- o$fileSpecifications
    if (length(f)) report$file[i] <- f[[1L]]$location %||% f[[1L]]$name %||% NA
    d <- o$displays
    if (length(d) > 1L) {
      miss(oids[i], "displays", "several displays: the first one is read")
    }
    disp <- if (length(d)) d[[1L]]$display else NULL
    for (s in disp$displaySections) {
      to <- sec[s$sectionType]
      if (is.na(to)) {
        miss(oids[i], "displaySections", sprintf(
          "a %s section has no place in the report spec", s$sectionType))
        next
      }
      line <- 0L
      for (z in s$orderedSubSections) {
        txt <- z[["subSection"]][["text"]] %||%
          glob[[z[["subSectionId"]] %||% ""]]
        if (is.null(txt)) next
        line <- line + 1L
        # (several sections of a kind follow each other)
        n0 <- sum(vapply(sh[[to]], function(r) identical(r$output_id, oids[i]),
                         NA))
        sh[[to]][[length(sh[[to]]) + 1L]] <- list(
          output_id = oids[i], line = as.character(n0 + 1L),
          left = if (to == "titles") NA_character_ else txt,
          center = if (to == "titles") txt else NA_character_)
      }
    }
  }
  as_df <- function(l) {
    if (!length(l)) return(NULL)
    data.frame(output_id = vapply(l, `[[`, "", "output_id"),
               line = vapply(l, `[[`, "", "line"),
               left = vapply(l, `[[`, "", "left"),
               center = vapply(l, `[[`, "", "center"),
               right = NA_character_, stringsAsFactors = FALSE)
  }
  report_spec <- tfl_table_spec(c(list(report = report),
                                  Filter(Negate(is.null),
                                         lapply(sh, as_df))))
  out <- list(ard = ard, report = report_spec)

  if (isTRUE(table)) {
    v <- list()
    for (g in grps) {
      if (isTRUE(g$dataDriven) || !length(g$groups)) next
      lv <- vapply(g$groups, function(z) z$name %||% "", "")
      v[[length(v) + 1L]] <- list(
        variable = g$groupingVariable,
        label = if (!identical(g$name, g$groupingVariable)) g$name else NA,
        levels = paste(lv, collapse = " | "))
    }
    vars <- if (length(v)) {
      d <- data.frame(output_id = NA_character_,
                      variable = vapply(v, `[[`, "", "variable"),
                      label = vapply(v, function(z) as.character(z$label), ""),
                      order = NA_character_,
                      levels = vapply(v, `[[`, "", "levels"),
                      stringsAsFactors = FALSE)
      d[!duplicated(d$variable), , drop = FALSE]
    }
    out$table <- tfl_table_spec(list(variables = vars))
    miss("table spec", "cells", paste(
      "cell templates and the table's look are the display's: the table",
      "spec has the groupings' levels only"))
  }
  attr(out, "unmapped") <- unique(un)
  out
}
