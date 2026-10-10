# ============================================================================
#  CDISC ARS: the specs as an Analysis Results Standard reporting event
# ============================================================================
#
#  tflspec's specs stay the source: the ARD spec says what is analysed, the
#  table / report specs how it is shown.  tfl_ars() writes them as the CDISC
#  ARS v1.0 model -- the classes and names of the CDISC LinkML model as
#  nested R lists, which tfl_write_ars_json() writes as the exchange JSON.
#
#    ReportingEvent
#      analysisSets       <- populations
#      dataSubsets        <- analyses$where
#      analysisGroupings  <- analyses$by, and a categorical variable's levels
#      methods            <- analyses$method (operations = its statistics)
#      analyses           <- analyses, one per variable (and depth)
#      outputs            <- output_id; displays from titles / footnotes /
#                            header / footer
#      mainListOfContents <- output -> its analyses
#
#  The shapes follow CDISC's own example (Common Safety Displays): a count
#  of a categorical variable is an analysis of the subject key (USUBJID)
#  grouped by the variable, its percentage a NUMERATOR / DENOMINATOR pair
#  that names the output's subject count by group.
#
#  What has no place in ARS (a derivation, display formats, the table's
#  look, a condition with no WhereClause) is kept in attr(, "unmapped") with
#  the reason, never dropped silently.
# ============================================================================

.ars_purposes <- c("PRIMARY OUTCOME MEASURE", "SECONDARY OUTCOME MEASURE",
                   "EXPLORATORY OUTCOME MEASURE")
.ars_reasons <- c("SPECIFIED IN PROTOCOL", "SPECIFIED IN SAP", "DATA DRIVEN",
                  "REQUESTED BY REGULATORY AGENCY")

# -- conditions -------------------------------------------------------------
#
# An R condition as an ARS WhereClause: a `condition` (one variable, one
# comparator, its values) or a `compoundExpression` (AND / OR / NOT of
# where clauses).  NULL when it has none (a function call, arithmetic, a
# comparison of two variables).
.ars_where <- function(txt, dataset) {
  if (is.null(txt) || is.na(txt) || !nzchar(trimws(txt))) return(list())
  e <- tryCatch(str2lang(txt), error = function(err) NULL)
  if (is.null(e)) return(NULL)
  ops <- c("==" = "EQ", "!=" = "NE", ">=" = "GE", "<=" = "LE", ">" = "GT",
           "<" = "LT", "%in%" = "IN")
  values <- function(v) {
    if (is.call(v) && identical(v[[1L]], as.name("c"))) {
      v <- as.list(v)[-1L]
    } else {
      v <- list(v)
    }
    ok <- vapply(v, function(x) (is.character(x) || is.numeric(x)) &&
                   length(x) == 1L && !is.na(x), NA)
    if (!all(ok)) return(NULL)
    vapply(v, function(x) if (is.numeric(x)) format(x, scientific = FALSE)
           else x, "")
  }
  one <- function(e, level, order) {
    while (is.call(e) && identical(e[[1L]], as.name("("))) e <- e[[2L]]
    f <- if (is.call(e)) as.character(e[[1L]])[1L] else ""
    if (f %in% c("&", "&&", "|", "||")) {
      lo <- if (f %in% c("&", "&&")) "AND" else "OR"
      # a & b & c is one AND of three
      parts <- list()
      flat <- function(x) {
        while (is.call(x) && identical(x[[1L]], as.name("("))) x <- x[[2L]]
        g <- if (is.call(x)) as.character(x[[1L]])[1L] else ""
        if (identical(if (g %in% c("&", "&&")) "AND" else if
                      (g %in% c("|", "||")) "OR" else "", lo)) {
          flat(x[[2L]])
          flat(x[[3L]])
        } else parts[[length(parts) + 1L]] <<- x
      }
      flat(e)
      wc <- lapply(seq_along(parts), function(k) one(parts[[k]], level + 1L, k))
      if (any(vapply(wc, is.null, NA))) return(NULL)
      return(list(level = level, order = order,
                  compoundExpression = list(logicalOperator = lo,
                                            whereClauses = wc)))
    }
    if (f == "!") {
      inner <- e[[2L]]
      while (is.call(inner) && identical(inner[[1L]], as.name("("))) {
        inner <- inner[[2L]]
      }
      # !(x %in% c(...)) is NOTIN
      if (is.call(inner) && identical(inner[[1L]], as.name("%in%")) &&
          is.name(inner[[2L]])) {
        v <- values(inner[[3L]])
        if (is.null(v)) return(NULL)
        return(list(level = level, order = order,
                    condition = list(dataset = dataset,
                                     variable = as.character(inner[[2L]]),
                                     comparator = "NOTIN",
                                     value = as.list(v))))
      }
      w <- one(inner, level + 1L, 1L)
      if (is.null(w)) return(NULL)
      return(list(level = level, order = order,
                  compoundExpression = list(logicalOperator = "NOT",
                                            whereClauses = list(w))))
    }
    if (!f %in% names(ops) || !is.name(e[[2L]])) return(NULL)
    v <- values(e[[3L]])
    if (is.null(v)) return(NULL)
    op <- ops[[f]]
    if (op == "EQ" && length(v) > 1L) op <- "IN"
    if (op != "IN" && length(v) > 1L) return(NULL)
    list(level = level, order = order,
         condition = list(dataset = dataset, variable = as.character(e[[2L]]),
                          comparator = op, value = as.list(v)))
  }
  one(e, 1L, 1L)
}

# A WhereClause's condition / compoundExpression, for a class that carries
# them at its top (AnalysisSet, DataSubset, Group).
.ars_cond_fields <- function(w) w[intersect(names(w), c("condition",
                                                         "compoundExpression"))]

# -- methods and their operations -------------------------------------------

# The statistics (the operations) a method gives by default.
.ars_method_stats <- function(method) {
  switch(method,
    continuous = c("N", "mean", "sd", "median", "p25", "p75", "min", "max"),
    categorical = , dichotomous = , hierarchical = , max = ,
    subjects = c("n", "p"),
    missing = c("N_obs", "N_miss", "N_nonmiss", "p_miss", "p_nonmiss"),
    total_n = "N",
    proportion_ci = c("N", "n", "estimate", "conf.low", "conf.high"),
    mean_ci = c("estimate", "conf.low", "conf.high"),
    ttest = , wilcox = , chisq = , fisher = "p.value",
    "result")
}

.ars_stat_names <- c(
  N = "Number of non-missing values", n = "Count of subjects",
  p = "Percentage of subjects", mean = "Mean", sd = "Standard deviation",
  median = "Median", p25 = "First quartile", p75 = "Third quartile",
  min = "Minimum", max = "Maximum", N_obs = "Number of observations",
  N_miss = "Number missing", N_nonmiss = "Number not missing",
  p_miss = "Percentage missing", p_nonmiss = "Percentage not missing",
  estimate = "Estimate", conf.low = "Lower confidence limit",
  conf.high = "Upper confidence limit", p.value = "p-value",
  result = "Result")

# A method's kind of analysis: how its analyses are laid out.
#   value  -- the analysis variable is the variable (continuous ...)
#   count  -- a count of subjects grouped by the variable (categorical ...)
#   test   -- a comparison across the groups (no result per group)
.ars_method_shape <- function(method) {
  if (method %in% c("continuous", "missing", "mean_ci")) return("value")
  if (method %in% c("categorical", "dichotomous", "max", "hierarchical",
                    "subjects", "proportion_ci", "total_n")) return("count")
  if (method %in% c("ttest", "wilcox")) return("test_value")
  if (method %in% c("chisq", "fisher")) return("test_count")
  "other"
}

# -- the model --------------------------------------------------------------

#' The specs as a CDISC ARS reporting event
#'
#' Writes what the ARD spec analyses -- and, when given, what the table and
#' report specs say of each output -- as the CDISC Analysis Results
#' Standard (ARS) v1.0 model: a `ReportingEvent` with its analysis sets,
#' data subsets, groupings, methods, analyses, outputs and list of
#' contents.  [tfl_write_ars_json()] writes it as the ARS JSON, the form
#' the standard is exchanged in; [tfl_check_ars()] checks it.  The specs
#' stay the source: the ARS is written from them, not edited.
#'
#' The layout follows CDISC's own example (Common Safety Displays):
#'
#' * a population is an `AnalysisSet`, an analysis's `where` a `DataSubset`
#'   (an R condition on one variable at a time, joined by `&`, `|`, `!`,
#'   becomes a `WhereClause`);
#' * each `by` variable is an `AnalysisGrouping`; its groups are listed
#'   (and `dataDriven = FALSE`) when the table spec gives the variable's
#'   `levels`;
#' * a continuous variable is an analysis of the variable; a categorical
#'   one an analysis of the subject key grouped by the variable, its
#'   percentage pointing at the output's subject count by group as its
#'   denominator (added, and said in `unmapped`, when the output has none);
#' * a hierarchy (SOC / PT) is one analysis per depth;
#' * each method is an `AnalysisMethod` whose operations are its statistics;
#'   a `custom` or `pkg::function` method carries its code as the
#'   method's `codeTemplate`.
#'
#' `purpose` and `reason` are the SAP's decisions, so they come from the
#' analyses' own `purpose` / `reason` columns (or the arguments), never
#' guessed.  ARS requires a purpose: an analysis without one is written
#' without it and [tfl_check_ars()] names it.
#'
#' @param ard_spec An ARD spec ([tfl_read_ard_spec()]).
#' @param table_spec,report_spec Optional table / report specs
#'   ([tfl_read_table_spec()], [tfl_read_report_spec()]; one object may be
#'   given for both): the levels of a grouping, and each output's titles,
#'   footnotes, header, footer and file.
#' @param profile `"cdisc"`: the layout of CDISC's examples.  `"siera"`:
#'   the same, made runnable by siera's `readARS()` (still valid CDISC
#'   ARS): each method carries an R code template, a proportion with its CI
#'   is an analysis of the variable, each output's subject count comes
#'   first, ids keep only letters, digits and `_`, and the list of outputs
#'   is there.  An analysis siera has no template for (a test other than
#'   chi-square, `missing`, `mean_ci`, `custom` ...) is left out and listed
#'   by [tfl_ars_unmapped()].  [tfl_ars_ard()] runs it.
#' @param study_id The reporting event's id; default the ARD spec's `study`
#'   key `study_id`, else `"STUDY"`.
#' @param purpose,reason For analyses whose `purpose` / `reason` is blank:
#'   one of the CDISC terms, or `NULL`.  `reason` defaults to
#'   `"SPECIFIED IN SAP"`.
#' @param dataset_names A named character vector: the ADaM name the ARS
#'   uses for a spec dataset (default: the spec's name in upper case).
#' @param dir The study folder: the files of the study key `source` (its
#'   own ARD functions) are read from it -- not run -- for the statistics a
#'   function says it gives (`cards::as_cards_fn(stat_names = )`).
#' @param references The outputs that print another output's analyses (a
#'   figure printing a table's median and hazard ratio, #293): a data frame
#'   with `output_id` (the figure), `source` (the table) and `analysis_id`
#'   (the table's analysis), a row each.  Such an output is an ARS
#'   `Output` (its displays and file, from the report spec) whose list of
#'   contents names the source's analyses -- they are not written twice;
#'   one whose analyses are not in the ARS is listed by
#'   [tfl_ars_unmapped()].
#' @return A `tfl_ars`: the reporting event as a nested list, with
#'   attributes `profile`, `unmapped` (a data frame `where`, `item`,
#'   `reason`: what the ARS does not say) and `ids` (each spec analysis and
#'   the ARS analyses written for it).
#' @seealso [tfl_write_ars_json()], [tfl_check_ars()], [tfl_ars_unmapped()]
#' @export
tfl_ars <- function(ard_spec, table_spec = NULL, report_spec = NULL,
                    profile = c("cdisc", "siera"), study_id = NULL,
                    purpose = NULL,
                    reason = "SPECIFIED IN SAP", dataset_names = NULL,
                    dir = ".", references = NULL) {
  profile <- match.arg(profile)
  if (!inherits(ard_spec, "tfl_ard_spec")) ard_spec <- tfl_ard_spec(ard_spec)
  if (!is.null(purpose)) purpose <- match.arg(toupper(purpose), .ars_purposes)
  if (!is.null(reason)) reason <- match.arg(toupper(reason), .ars_reasons)
  x <- ard_spec
  un <- data.frame(where = character(), item = character(),
                   reason = character(), stringsAsFactors = FALSE)
  miss <- function(where, item, why) {
    un[nrow(un) + 1L, ] <<- list(where, item, why)
  }
  subj <- .study_value(x, "id", "USUBJID")
  study_id <- study_id %||% .study_value(x, "study_id", "STUDY")
  # the study's own ARD functions: the file each is in, and the statistics
  # it declares
  own <- .ars_own_functions(x, dir)
  own_src <- own$file
  own_stats <- own$stat_names
  ds_name <- function(d) {
    if (is.na(d)) return(NA_character_)
    if (!is.null(dataset_names) && d %in% names(dataset_names)) {
      return(unname(dataset_names[[d]]))
    }
    toupper(d)
  }
  for (i in seq_len(nrow(x$datasets))) {
    if (!is.na(x$datasets$derive[i])) {
      miss(x$datasets$dataset[i], "datasets$derive",
           "ARS reads the ADaM as it is: a derivation belongs in the ADaM")
    }
  }

  # analysis sets
  sets <- list()
  pops <- x$populations
  for (i in seq_len(nrow(pops))) {
    pid <- pops$population_id[i]
    ds <- ds_name(pops$dataset[i])
    s <- list(id = pid, name = pid, level = 1L, order = i)
    w <- .ars_where(pops$where[i], ds)
    if (is.null(w)) {
      miss(pid, "populations$where", sprintf(
        "`%s` has no ARS WhereClause (a comparison of one variable with values, joined by & | !)",
        pops$where[i]))
    } else s <- c(s, .ars_cond_fields(w))
    if (!is.na(pops$derive[i])) {
      miss(pid, "populations$derive",
           "an analysis set has no derivation in ARS")
    }
    sets[[length(sets) + 1L]] <- s
  }
  pop_ds <- stats::setNames(vapply(pops$dataset, ds_name, ""),
                            pops$population_id)

  # table spec levels: a grouping lists its groups when they are given
  # (an output's own row first, then a default row: blank output_id)
  var_row <- function(out, var, col) {
    v <- table_spec$variables
    if (is.null(v) || !NROW(v) || is.null(v[[col]])) return(NA_character_)
    oid <- v$output_id %||% rep(NA_character_, nrow(v))
    hit <- v$variable %in% var & !is.na(v[[col]])
    l <- c(v[[col]][hit & oid %in% out], v[[col]][hit & is.na(oid)])
    if (length(l)) l[1L] else NA_character_
  }
  levels_of <- function(out, var) {
    l <- var_row(out, var, "levels")
    if (is.na(l)) character() else .split_bar(l)
  }
  label_of <- function(out, var) {
    l <- var_row(out, var, "label")
    if (is.na(l)) var else l
  }

  groupings <- list()
  grouping <- function(ds, var, out) {
    lv <- levels_of(out, var)
    key <- paste(ds, var, paste(lv, collapse = "|"), sep = "\r")
    hit <- match(key, vapply(groupings, `[[`, "", "key"))
    if (!is.na(hit)) return(groupings[[hit]]$id)
    base <- paste("AG", ds, var, sep = "_")
    n <- sum(vapply(groupings, function(g) identical(g$base, base), NA))
    id <- if (n) paste0(base, "_", n + 1L) else base
    g <- list(id = id, name = label_of(out, var), groupingDataset = ds,
              groupingVariable = var, dataDriven = !length(lv))
    if (length(lv)) {
      # a group's name as the table shows it, its condition the data's own
      # value (a code list's label back to its value)
      cl <- .codelist_levels(table_spec, out)[[var]]
      val <- if (is.null(names(cl))) lv else
        ifelse(lv %in% cl, names(cl)[match(lv, cl)], lv)
      g$groups <- lapply(seq_along(lv), function(k) list(
        id = paste0(id, "_", k), name = lv[k], level = 1L, order = k,
        condition = list(dataset = ds, variable = var, comparator = "EQ",
                         value = list(val[k]))))
    }
    groupings[[length(groupings) + 1L]] <<- c(list(key = key, base = base),
                                              list(model = g), list(id = id))
    id
  }

  subsets <- list()
  subset <- function(where, ds, tag) {
    if (is.na(where)) return(NULL)
    key <- paste(ds, where, sep = "\r")
    hit <- match(key, vapply(subsets, `[[`, "", "key"))
    if (!is.na(hit)) return(subsets[[hit]]$model$id)
    w <- .ars_where(where, ds)
    if (is.null(w)) {
      miss(tag, "analyses$where", sprintf(
        "`%s` has no ARS WhereClause (a comparison of one variable with values, joined by & | !)",
        where))
      return(NULL)
    }
    id <- paste0("DS_", length(subsets) + 1L)
    m <- c(list(id = id, name = where, level = 1L,
                order = length(subsets) + 1L), .ars_cond_fields(w))
    subsets[[length(subsets) + 1L]] <<- list(key = key, model = m)
    id
  }

  keys <- tfl_ard_methods()
  methods <- list()
  method <- function(m, stats, opt = NULL, code = NULL, tag) {
    default <- .ars_method_stats(m)
    if (!length(stats)) stats <- default
    base <- paste0("Mth_", gsub("[^A-Za-z0-9_]", "_", m),
                   if (!is.null(opt)) paste0("_", gsub("[^A-Za-z0-9]", "", opt)))
    key <- paste(base, paste(stats, collapse = "|"), code %||% "", sep = "\r")
    hit <- match(key, vapply(methods, `[[`, "", "key"))
    if (!is.na(hit)) return(methods[[hit]])
    same <- sum(vapply(methods, function(z) identical(z$base, base), NA))
    id <- if (same) paste0(base, "_", same + 1L) else base
    k <- .method_key(m, keys)
    ops <- lapply(seq_along(stats), function(j) {
      o <- list(id = paste(id, j, stats[j], sep = "_"),
                name = unname(.ars_stat_names[stats[j]] %||% stats[j]),
                label = stats[j], order = j)
      if (is.na(o$name)) o$name <- stats[j]
      o
    })
    mm <- list(id = id,
               name = if (!is.na(k)) keys$label[k] else m,
               description = if (!is.na(k)) paste0(keys$call[k],
                 if (!is.null(opt)) paste0(" (", opt, ")")) else m,
               operations = ops)
    if (!is.null(code)) {
      mm$codeTemplate <- list(context = "R", code = code)
    }
    z <- list(key = key, base = base, id = id, model = mm,
              op = stats::setNames(vapply(ops, `[[`, "", "id"), stats))
    methods[[length(methods) + 1L]] <<- z
    z
  }

  # a percentage's NUMERATOR / DENOMINATOR: the operation relationships of
  # its method, the denominator naming the subject count's operation
  rel <- function(mz, den_op) {
    j <- which(vapply(mz$model$operations, `[[`, "", "label") == "p")
    if (!length(j)) return(mz)
    for (i in seq_along(methods)) {
      if (!identical(methods[[i]]$id, mz$id)) next
      o <- methods[[i]]$model$operations[[j]]
      if (is.null(o$referencedOperationRelationships)) {
        o$referencedOperationRelationships <- list(
          list(id = paste0(o$id, "_NUM"),
               referencedOperationRole = list(controlledTerm = "NUMERATOR"),
               operationId = mz$op[["n"]]),
          list(id = paste0(o$id, "_DEN"),
               referencedOperationRole = list(controlledTerm = "DENOMINATOR"),
               operationId = den_op))
        methods[[i]]$model$operations[[j]] <<- o
      }
    }
    mz
  }

  # an analysis on an analysis data: its analysis set, the dataset it is made
  # from, and its conditions and the analysis's together; what else it does
  # (columns added or derived, one row per subject ...) ARS has no place for
  an <- x$analyses
  ad <- .adata_sheet(x)
  dcol <- .data_col(an)
  for (i in which(!is.na(dcol))) {
    ad_o <- .adata_of(ad, an$output_id[i])
    if (!dcol[i] %in% ad_o$data_id) next
    tag <- paste(an$output_id[i], an$analysis_id[i], sep = " / ")
    ch <- .adata_chain(ad_o, dcol[i])
    rows <- ad_o[match(ch, ad_o$data_id), , drop = FALSE]
    w <- c(stats::na.omit(rows$where), stats::na.omit(an$where[i]))
    an$where[i] <- if (!length(w)) NA_character_ else if (length(w) == 1L) w else
      paste0("(", w, ")", collapse = " & ")
    an$population_id[i] <- .adata_pop(ad_o, dcol[i])
    an$dataset[i] <- .adata_dataset(ad_o, dcol[i])
    for (cn in c("subjects", "add", "derive", "distinct", "code")) {
      for (j in which(!is.na(rows[[cn]]))) {
        miss(tag, paste0("analysis_data$", cn), sprintf(
          "%s: `%s` -- ARS reads the ADaM as it is, by a WhereClause",
          rows$data_id[j], rows[[cn]][j]))
      }
    }
  }
  if (!is.null(an$data)) an$data <- NA_character_
  a <- .ard_spec_flat(an)
  analyses <- list()
  # each ARS analysis, what it was written from: the spec row, its method,
  # its role (groupn: the output's subjects per group; any: subjects with any
  # record; level: a depth of a hierarchy; count / value / test_count /
  # test_value / other: as .ars_method_shape()) and its variable
  ids <- data.frame(output_id = character(), analysis_id = character(),
                    ars_id = character(), method = character(),
                    role = character(), variable = character(),
                    by = character(), stringsAsFactors = FALSE)
  groupn <- list()   # output / population / grouping -> the subject count
  pending_den <- list()

  add_analysis <- function(r, id, name, ds, var, mz, grp, no_res = character(),
                           ss = NULL, pur, rea, role, v = NA_character_,
                           m = r$method) {
    an <- list(id = id, name = name, version = 1L)
    if (!is.na(rea)) an$reason <- list(controlledTerm = rea)
    if (!is.na(pur)) an$purpose <- list(controlledTerm = pur)
    an$methodId <- mz$id
    if (!is.na(r$population_id)) an$analysisSetId <- r$population_id
    an$dataset <- ds
    an$variable <- var
    if (!is.null(ss)) an$dataSubsetId <- ss
    if (length(grp)) {
      an$orderedGroupings <- lapply(seq_along(grp), function(k) list(
        order = k, groupingId = grp[k], resultsByGroup = !grp[k] %in% no_res))
    }
    analyses[[length(analyses) + 1L]] <<- list(output = r$output_id,
                                               model = an, mz = mz,
                                               pop = r$population_id,
                                               grp = grp)
    ids[nrow(ids) + 1L, ] <<- list(r$output_id, r$analysis_id, id, m, role,
                                   v, paste(.split_bar(r$by), collapse = "|"))
    id
  }

  need_den <- function(id, r, by, mz, ds, pur, rea) {
    if (!"p" %in% names(mz$op)) return(invisible())
    pending_den[[length(pending_den) + 1L]] <<- list(
      id = id, out = r$output_id, pop = r$population_id, by = by, mz = mz,
      ds = ds, r = r, pur = pur, rea = rea)
  }

  # an analysis with `overall` is two ARS analyses: by its groups, and
  # over all its subjects without the grouping (_TOTAL) -- no group value
  # the data does not have
  ov <- vapply(a$overall %||% rep(NA_character_, nrow(a)), function(v)
    isTRUE(.ard_yes(v)), NA) & !is.na(a$by)
  todo <- unlist(lapply(seq_len(nrow(a)), function(i) if (ov[i]) c(i, -i) else i))
  for (j in todo) {
    i <- abs(j)
    overall <- j < 0L
    r <- a[i, ]
    if (overall) r$by <- NA_character_
    out <- r$output_id
    tag <- paste(out, r$analysis_id, sep = " / ")
    m <- r$method
    # a keyword's function name is the keyword's method
    km <- .method_key(m, keys)
    if (!is.na(km)) m <- keys$method[km]
    pds <- if (!is.na(r$population_id)) pop_ds[[r$population_id]] else NA
    ds <- if (!is.na(r$dataset)) ds_name(r$dataset) else pds
    if (is.na(ds)) {
      miss(tag, "dataset", "no dataset (and no population to take it from)")
      next
    }
    # strata are groupings too: the analysis is repeated within them
    by <- c(.split_bar(r$by), .split_bar(r$strata))
    vars <- .split_bar(r$variables)
    stats <- .split_bar(r$statistics)
    pur <- if (!is.na(r$purpose)) toupper(trimws(r$purpose)) else
      purpose %||% NA_character_
    rea <- if (!is.na(r$reason)) toupper(trimws(r$reason)) else
      reason %||% NA_character_
    if (is.na(pur)) {
      miss(tag, "purpose", paste(
        "ARS requires Analysis.purpose (PRIMARY / SECONDARY / EXPLORATORY",
        "OUTCOME MEASURE): fill the analyses' `purpose` column"))
    } else if (!pur %in% .ars_purposes) {
      miss(tag, "purpose", sprintf("`%s` is not a CDISC analysis purpose",
                                   r$purpose))
    }
    if (!is.na(rea) && !rea %in% .ars_reasons) {
      miss(tag, "reason", sprintf("`%s` is not a CDISC analysis reason",
                                  r$reason))
    }
    if (!is.na(r$formats)) {
      miss(tag, "formats",
           "display formats are the table's, not ARS analysis metadata")
    }
    if (!is.na(r$post %||% NA)) {
      miss(tag, "post", sprintf(
        "`%s`: ARS has no place for steps on the results after the method",
        r$post))
    }
    opt <- NULL
    code <- NULL
    over <- FALSE
    if (!is.na(r$args)) {
      # args read as R: the CI method of a proportion and the any-event row
      # of a hierarchy are the method's; the rest ARS has no place for
      al <- .args_list(r$args)
      if (m == "proportion_ci" && is.character(al$method) &&
          length(al$method) == 1L) {
        opt <- al$method
        al$method <- NULL
      }
      if (m == "hierarchical" && isTRUE(al$over_variables)) {
        over <- TRUE
        al$over_variables <- NULL
      }
      if (length(al)) miss(tag, "args", sprintf(
        "`%s`: an ARS method states no arguments",
        paste(ifelse(nzchar(names(al) %||% rep("", length(al))),
                     paste(names(al), "= "), ""),
              vapply(al, function(e) paste(deparse(e), collapse = " "), ""),
              collapse = ", ", sep = "")))
    }
    if (!is.na(r$denominator %||% NA) && r$denominator != "population") {
      miss(tag, "denominator", sprintf(paste(
        "`%s`: a percentage's denominator in ARS is the analysis set's",
        "subject count; this one is the ARD program's"), r$denominator))
    }
    if (m == "custom") {
      code <- r$code
    } else if (!m %in% keys$method) {
      # the call the ARD program makes, and where an own function is defined
      given <- c(.args_given(r$args), if (!is.na(r$strata)) "strata",
                 if (!is.na(r$denominator)) "denominator")
      code <- tryCatch(
        .analysis_body(r, keys, subj, function(arg) arg %in% given),
        error = function(e) sprintf("%s(...)", m))
      where <- own_src[[m]]
      if (!is.null(where)) code <- paste0("# ", m, "(): ", where, "\n", code)
      # the statistics: the row's, else the ones the function says it gives
      if (!length(stats)) stats <- own_stats[[m]] %||% character()
    }
    # (the condition runs on the data with the code lists' labels; ARS
    # compares the data's own values)
    ss <- subset(.where_labels_to_values(
      r$where, .codelist_levels(table_spec, out)), ds, tag)
    lbl <- if (!is.na(r$label)) r$label else NULL
    # (overall: the name says so)
    lab_of <- function(o, v) paste(c(label_of(o, v), if (overall) "(overall)"),
                                   collapse = " ")
    if (overall && !is.null(lbl)) lbl <- paste(lbl, "(overall)")
    g_by <- vapply(by, grouping, "", ds = ds, out = out)
    aid <- function(...) paste(c("An", out, r$analysis_id,
                                 if (overall) "TOTAL", ...),
                               collapse = "_")
    shape <- .ars_method_shape(m)
    mz <- method(m, stats, opt, code, tag)
    several <- length(vars) > 1L
    if (m == "hierarchical") {
      if (over) {
        id <- add_analysis(r, aid("ANY"), lbl %||% "Any", ds, subj, mz, g_by,
                           ss = ss, pur = pur, rea = rea, role = "any")
        need_den(id, r, by, mz, pds %|NA|% ds, pur, rea)
      }
      for (k in seq_along(vars)) {
        gv <- vapply(vars[seq_len(k)], grouping, "", ds = ds, out = out)
        id <- add_analysis(r, aid(paste0("L", k)),
                           paste(c(lbl, paste(vars[seq_len(k)],
                                              collapse = " / ")),
                                 collapse = ": "),
                           ds, subj, mz, c(g_by, gv), ss = ss, pur = pur,
                           rea = rea, role = "level", v = vars[k])
        need_den(id, r, by, mz, pds %|NA|% ds, pur, rea)
      }
    } else if (m == "total_n" || (m == "categorical" && !length(by) &&
               length(vars) == 1L &&
               vars %in% unlist(lapply(a$by[a$output_id %in% out],
                                       .split_bar)))) {
      # the output's subject count by group -- the subjects per group, which
      # clinical reporting calls big N -- the denominator of its percentages
      mz <- method("total_n", if (m == "total_n") stats else character(),
                   tag = tag)
      bv <- if (m == "total_n") by else vars
      gv <- vapply(bv, grouping, "", ds = ds, out = out)
      id <- add_analysis(r, aid(), lbl %||% "Number of subjects", ds, subj,
                         mz, gv, ss = ss, pur = pur, rea = rea, role = "groupn",
                         m = "total_n")
      groupn[[paste(out, r$population_id, paste(bv, collapse = ","),
                  sep = "\r")]] <- list(id = id, op = mz$op[["N"]])
    } else if (m == "subjects") {
      # subjects with any record of the data: counted by the grouping alone
      id <- add_analysis(r, aid(), lbl %||% r$analysis_id, ds, subj, mz,
                         g_by, ss = ss, pur = pur, rea = rea, role = "any")
      need_den(id, r, by, mz, pds %|NA|% ds, pur, rea)
    } else if (shape == "count") {
      for (v in vars) {
        gv <- c(g_by, grouping(ds, v, out))
        id <- add_analysis(r, if (several) aid(v) else aid(),
                           lbl %||% lab_of(out, v), ds, subj, mz, gv,
                           ss = ss, pur = pur, rea = rea, role = "count",
                           v = v)
        need_den(id, r, by, mz, pds %|NA|% ds, pur, rea)
      }
    } else if (shape %in% c("value", "test_value")) {
      for (v in vars) {
        add_analysis(r, if (several) aid(v) else aid(),
                     lbl %||% lab_of(out, v), ds, v, mz, g_by,
                     no_res = if (shape == "test_value") g_by else character(),
                     ss = ss, pur = pur, rea = rea, role = shape, v = v)
      }
    } else if (shape == "test_count") {
      for (v in vars) {
        gv <- c(g_by, grouping(ds, v, out))
        add_analysis(r, if (several) aid(v) else aid(),
                     lbl %||% lab_of(out, v), ds, subj, mz, gv,
                     no_res = gv, ss = ss, pur = pur, rea = rea,
                     role = "test_count", v = v)
      }
    } else {
      # custom or pkg::function: what it analyses is in its code
      add_analysis(r, aid(), lbl %||% r$analysis_id, ds,
                   if (length(vars)) vars[1L] else subj, mz, g_by,
                   ss = ss, pur = pur, rea = rea, role = "other")
      if (length(vars) > 1L) miss(tag, "variables", paste(
        "a custom analysis is one ARS analysis: its variables beyond the",
        "first are in its code only"))
    }
  }

  # each percentage names its denominator: the output's subject count with
  # the same population and grouping, added when the output has none
  for (p in pending_den) {
    key <- paste(p$out, p$pop, paste(p$by, collapse = ","), sep = "\r")
    b <- groupn[[key]]
    if (is.null(b)) {
      r <- p$r
      r$analysis_id <- "GROUPN"
      mz <- method("total_n", character(), tag = p$out)
      gv <- vapply(p$by, grouping, "", ds = p$ds, out = p$out)
      # An_<output>_GROUPN_<by> (_ALL without a grouping), never an id the
      # output already has
      id <- paste(c("An", p$out, "GROUPN",
                    if (length(p$by)) p$by else "ALL"), collapse = "_")
      taken <- vapply(analyses, function(z) z$model$id, "")
      if (id %in% taken) {
        k <- 2L
        while (paste0(id, "_", k) %in% taken) k <- k + 1L
        id <- paste0(id, "_", k)
      }
      id <- add_analysis(r, id,
                         "Number of subjects", p$ds, subj, mz, gv,
                         pur = p$pur, rea = p$rea, role = "groupn",
                         m = "total_n")
      # the count comes first in its output
      k <- length(analyses)
      first <- match(p$out, vapply(analyses, `[[`, "", "output"))
      analyses <- append(analyses[-k], analyses[k], after = first - 1L)
      b <- groupn[[key]] <- list(id = id, op = mz$op[["N"]])
      miss(p$out, "denominator", sprintf(
        "added %s, the subject count by group the percentages divide by", id))
    }
    rel(p$mz, b$op)
    for (k in seq_along(analyses)) {
      if (!identical(analyses[[k]]$model$id, p$id)) next
      pid <- p$mz$model$operations[[match("p", names(p$mz$op))]]$id
      analyses[[k]]$model$referencedAnalysisOperations <- list(
        list(referencedOperationRelationshipId = paste0(pid, "_NUM"),
             analysisId = p$id),
        list(referencedOperationRelationshipId = paste0(pid, "_DEN"),
             analysisId = b$id))
    }
  }

  # the table's look has no place in ARS
  if (!is.null(table_spec)) {
    for (sh in c("layout", "columns", "style", "col_header", "cells")) {
      d <- table_spec[[sh]]
      if (!is.null(d) && NROW(d)) {
        miss(paste0("table spec `", sh, "`"), sh, if (sh == "cells")
          "cell templates and digits are the display's, not ARS analysis metadata" else
          "the table's look (pages, stub, widths, borders, header cells) is not ARS metadata")
      }
    }
  }

  # outputs, their displays and the list of contents
  rs <- report_spec %||% table_spec
  outs <- unique(c(intersect(rs$report$output_id, a$output_id), a$output_id))
  outs <- outs[outs %in% vapply(analyses, `[[`, "", "output")]
  # the outputs that print another's analyses: an output each, its list of
  # contents the source's analyses (those the ARS has)
  refs <- list()
  if (!is.null(references) && NROW(references)) {
    rf <- as.data.frame(references, stringsAsFactors = FALSE)
    for (o in setdiff(unique(rf$output_id), outs)) {
      r <- rf[rf$output_id == o, , drop = FALSE]
      hit <- ids$ars_id[paste(ids$output_id, ids$analysis_id) %in%
                          paste(r$source, r$analysis_id)]
      if (length(hit)) {
        refs[[o]] <- unique(hit)
      } else {
        miss(o, "references", sprintf(
          "it prints %s, which the ARS has no analysis of", paste(
            unique(paste0(r$source, " ", r$analysis_id)), collapse = ", ")))
      }
    }
    outs <- c(outs, names(refs))
  }
  # a report with no analyses is not an ARS output: say so, with why
  rep_all <- rs$report
  # (one that prints another's analyses is said above when none is in it)
  rf_out <- if (!is.null(references) && NROW(references)) unique(references$output_id) else character()
  if (!is.null(rep_all) && NROW(rep_all)) {
    for (k in seq_len(nrow(rep_all))) {
      o <- rep_all$output_id[k]
      if (is.na(o) || o %in% outs || o %in% rf_out) next
      type <- tolower(rep_all$type[k] %||% NA_character_)
      miss(o, "output", switch(type %|NA|% "",
        user = paste("a report of user code with no analyses in the ARD",
                     "definition: what it shows is made by its own code,",
                     "which ARS does not hold"),
        listing = paste("a listing: ARS describes analyses and their",
                        "results, and a listing has none"),
        paste("no analyses in the ARD definition, so nothing for ARS to",
              "describe")))
    }
  }
  text_of <- function(d, j) {
    t <- stats::na.omit(c(d$left[j], d$center[j], d$right[j]))
    paste(t, collapse = "  ")
  }
  secs <- c(header = "Header", titles = "Title", footnotes = "Footnote",
            footer = "Footer")
  outputs <- lapply(outs, function(o) {
    did <- paste0(o, "_D1")
    dsec <- list()
    title <- NULL
    for (sh in names(secs)) {
      d <- rs[[sh]]
      if (is.null(d) || !NROW(d)) next
      d <- d[d$output_id %in% o, , drop = FALSE]
      d <- d[order(suppressWarnings(as.numeric(d$line))), , drop = FALSE]
      sub <- list()
      for (j in seq_len(nrow(d))) {
        txt <- text_of(d, j)
        if (!nzchar(txt)) next
        sub[[length(sub) + 1L]] <- list(
          order = length(sub) + 1L,
          subSection = list(id = paste(did, secs[[sh]], length(sub) + 1L,
                                       sep = "_"), text = txt))
      }
      if (length(sub)) {
        dsec[[length(dsec) + 1L]] <- list(sectionType = secs[[sh]],
                                          orderedSubSections = sub)
        if (sh == "titles") {
          title <- paste(vapply(sub, function(s) s$subSection$text, ""),
                         collapse = " ")
        }
      }
    }
    disp <- list(id = did, name = o, version = 1L)
    if (!is.null(title)) disp$displayTitle <- title
    if (length(dsec)) disp$displaySections <- dsec
    m <- list(id = o, name = title %||% o, version = 1L,
              displays = list(list(order = 1L, display = disp)))
    rep <- rs$report
    if (!is.null(rep) && NROW(rep)) {
      k <- match(o, rep$output_id)
      if (!is.na(k) && !is.na(rep$file[k])) {
        f <- rep$file[k]
        ext <- tolower(tools::file_ext(f))
        fs <- list(name = basename(f), location = f)
        if (ext %in% c("rtf", "pdf", "txt")) fs$fileType <-
          list(controlledTerm = ext)
        m$fileSpecifications <- list(fs)
      }
    }
    m
  })
  items <- lapply(seq_along(outs), function(i) {
    o <- outs[i]
    an <- if (o %in% names(refs)) Filter(function(z) z$model$id %in% refs[[o]], analyses) else
      Filter(function(z) identical(z$output, o), analyses)
    list(name = outputs[[i]]$name, level = 1L, order = i, outputId = o,
         sublist = list(listItems = lapply(seq_along(an), function(k) list(
           name = an[[k]]$model$name, level = 2L, order = k,
           analysisId = an[[k]]$model$id))))
  })

  re <- list(id = study_id, name = study_id, version = 1L,
             mainListOfContents = list(
               name = "List of Planned Analyses",
               contentsList = list(listItems = items)))
  if (length(sets)) re$analysisSets <- sets
  if (length(subsets)) re$dataSubsets <- lapply(subsets, `[[`, "model")
  if (length(groupings)) re$analysisGroupings <- lapply(groupings, `[[`,
                                                        "model")
  if (length(methods)) re$methods <- lapply(methods, `[[`, "model")
  if (length(analyses)) re$analyses <- lapply(analyses, `[[`, "model")
  if (length(outputs)) re$outputs <- outputs
  out <- structure(re, class = "tfl_ars", profile = profile,
                   unmapped = unique(un), ids = ids)
  if (profile == "siera") out <- .ars_siera(out)
  out
}

`%|NA|%` <- function(a, b) if (is.null(a) || is.na(a)) b else a  # nolint: object_name_linter.

#' What the ARS does not say
#'
#' @param ars A [tfl_ars()].
#' @return A data frame: `where` (the output / analysis or sheet), `item`
#'   (the spec column) and `reason`.
#' @export
tfl_ars_unmapped <- function(ars) {
  attr(ars, "unmapped") %||% data.frame(where = character(),
                                        item = character(),
                                        reason = character(),
                                        stringsAsFactors = FALSE)
}

#' @export
print.tfl_ars <- function(x, ...) {
  n <- function(k) length(x[[k]])
  cat(sprintf("<tfl_ars> %s (CDISC ARS v1.0, profile %s)\n", x$id,
              attr(x, "profile") %||% "cdisc"))
  cat(sprintf(paste0("  %d output(s), %d analyses, %d analysis set(s), ",
                     "%d data subset(s), %d grouping(s), %d method(s)\n"),
              n("outputs"), n("analyses"), n("analysisSets"),
              n("dataSubsets"), n("analysisGroupings"), n("methods")))
  u <- tfl_ars_unmapped(x)
  if (nrow(u)) {
    cat(sprintf("  %d item(s) not in the ARS: tfl_ars_unmapped()\n", nrow(u)))
  }
  invisible(x)
}

# The study's own ARD functions (the files of the study key `source`), read
# without running them: for each function the file it is defined in, and
# the statistics it declares the way cards' own do,
# `name <- cards::as_cards_fn(function(...) ..., stat_names = c("a", "b"))`.
.ars_own_functions <- function(x, dir = ".") {
  out <- list(file = list(), stat_names = list())
  src <- .split_bar(.study_value(x, "source", NA))
  if (!length(src)) return(out)
  info <- tfl_ard_function_info(file.path(dir, src))
  info <- info[!is.na(info$name), , drop = FALSE]
  for (i in seq_len(nrow(info))) {
    out$file[[info$name[i]]] <- src[match(info$file[i], file.path(dir, src))]
    if (nzchar(info$stat_names[i])) {
      out$stat_names[[info$name[i]]] <- .split_bar(info$stat_names[i])
    }
  }
  out
}
