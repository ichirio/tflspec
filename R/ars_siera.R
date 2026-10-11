# ============================================================================
#  CDISC ARS, profile "siera": a reporting event siera can run
# ============================================================================
#
#  siera (pharmaverse) reads an ARS reporting event and writes, for each
#  output, an R programme that makes its ARD: it loads the ADaM, applies the
#  analysis set and data subset, then runs each analysis method's code
#  template with its parameters filled in.  tfl_ars(profile = "siera")
#  writes the model so that works, and so it is still valid CDISC ARS:
#
#    * each method carries an R code template (`codeTemplate`, context
#      "R (siera)") written here for tflspec, against siera's contract:
#      the analysis data is `df2_<analysis id>`, the result `df3_<analysis
#      id>`, a parameter is replaced by the siera value its `valueSource`
#      names (ana_var, by_vars, strata_vars, by_listc, distinct_list,
#      DEN_analysisid, AG_denom_var1, AG_max_dataDriven, AG_var1, AG_var2);
#    * a proportion with its CI is an analysis of the variable, grouped by
#      the `by` variables only (the template tabulates the variable);
#    * each output's subject count comes first (its df2 is the
#      percentages' denominator data);
#    * ids hold only letters, digits and `_` (siera makes R names of them);
#    * the list of outputs (otherListsOfContents) is there: siera reads it.
#
#  An analysis siera has no template for is left out (the cdisc profile
#  carries it) and said in tfl_ars_unmapped().  What siera needs beyond
#  the model (three analyses an output, one population an output, a
#  population of one condition) is checked by tfl_check_ars().
# ============================================================================

.ars_siera_context <- "R (siera)"

.ars_san <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

# The siera method each tflspec analysis becomes, from its method and role
# (tfl_ars()'s attr(, "ids")); NA when siera has none.
.ars_siera_kind <- function(method, role) {
  ifelse(role == "groupn", "total_n",
  ifelse(role == "any", "flat",
  ifelse(role == "level", "categorical",
  ifelse(role == "count" & method == "categorical", "categorical",
  ifelse(role == "count" & method == "proportion_ci", "proportion_ci",
  ifelse(role == "value" & method == "continuous", "continuous",
  ifelse(role == "test_count" & method == "chisq", "chisq",
         NA_character_)))))))
}

.ars_siera_stats <- list(
  total_n = "n", categorical = c("n", "p"), flat = c("n", "p"),
  nested = c("n", "p"),
  proportion_ci = c("n", "N", "estimate", "conf.low", "conf.high"),
  chisq = "p.value")

.ars_siera_names <- c(
  total_n = "Subject count by group",
  categorical = "Subject count and percentage by category",
  nested = "Subject count and percentage by a level of a hierarchy",
  flat = "Subject count and percentage with any record",
  continuous = "Summary statistics",
  proportion_ci = "Proportion with confidence interval",
  chisq = "Chi-square test")

.ars_siera_params <- list(
  total_n = c(anavarhere = "ana_var", groupvar1here = "AG_var1",
              bystmthere = "by_vars"),
  categorical = c(distinctlisthere = "distinct_list",
                  denomanaidhere = "DEN_analysisid",
                  denomvarhere = "AG_denom_var1",
                  isdatadrivenhere = "AG_max_dataDriven",
                  byvarshere = "by_vars", stratavarshere = "strata_vars"),
  flat = c(distinctlisthere = "distinct_list",
           denomanaidhere = "DEN_analysisid",
           denomvarhere = "AG_denom_var1", bylistchere = "by_listc"),
  nested2 = c(distinctlisthere = "distinct_list",
              denomanaidhere = "DEN_analysisid",
              denomvarhere = "AG_denom_var1", stratavarshere = "strata_vars",
              parentvarhere = "AG_var1", childvarhere = "AG_var2"),
  nested3 = c(distinctlisthere = "distinct_list",
              denomanaidhere = "DEN_analysisid",
              denomvarhere = "AG_denom_var1", stratavarshere = "strata_vars",
              firstvarhere = "AG_var1", parentvarhere = "AG_var2",
              childvarhere = "AG_var3"),
  continuous = c(bylistchere = "by_listc", anavarhere = "ana_var"),
  proportion_ci = c(bylistchere = "by_listc", anavarhere = "ana_var"),
  chisq = c(groupvar1here = "AG_var1", groupvar2here = "AG_var2"))

# `stat_name == 'n' ~ '<op id>', ...` for the method's operations
.ars_siera_opcase <- function(stats, opids) {
  paste0("dplyr::case_when(\n",
         paste0("      stat_name == '", stats, "' ~ '", opids, "'",
                collapse = ",\n"), ")")
}

.ars_siera_code <- function(kind, stats, opids, opt = NULL, depth = NULL) {
  depth <- if (is.null(depth)) NULL else as.integer(depth)
  # a nested level of `depth` groupings: the level above is group<depth - 1>
  nest_group <- paste0("group", (depth %||% 2L) - 1L, "_level")
  keep <- paste0("c(", paste0("'", stats, "'", collapse = ", "), ")")
  tail <- paste0(" |>\n  dplyr::filter(stat_name %in% ", keep, ") |>\n",
                 "  dplyr::mutate(operationid = ",
                 .ars_siera_opcase(stats, opids), ")")
  switch(kind,
    total_n = paste0(
      "in_data <- df2_analysisidhere |>\n",
      "  dplyr::select(anavarhere, groupvar1here) |>\n",
      "  unique()\n",
      "df3_analysisidhere <- cards::ard_tabulate(\n",
      "  data = in_data\n",
      "  bystmthere\n",
      ")", tail),
    categorical = paste0(
      "denom_dataset <- df2_denomanaidhere |>\n",
      "  dplyr::select(denomvarhere)\n",
      "in_data <- df2_analysisidhere |>\n",
      "  dplyr::distinct(distinctlisthere)\n",
      "# strata when the innermost grouping is data-driven: only the\n",
      "# combinations in the data\n",
      "if (isdatadrivenhere) {\n",
      "  df3_analysisidhere <- cards::ard_tabulate(\n",
      "    data = in_data\n",
      "    stratavarshere,\n",
      "    denominator = denom_dataset\n",
      "  )\n",
      "} else {\n",
      "  df3_analysisidhere <- cards::ard_tabulate(\n",
      "    data = in_data\n",
      "    byvarshere,\n",
      "    denominator = denom_dataset\n",
      "  )\n",
      "}\n",
      "df3_analysisidhere <- df3_analysisidhere", tail),
    nested = paste0(
      "denom_dataset <- df2_denomanaidhere |>\n",
      "  dplyr::select(denomvarhere)\n",
      "in_data <- df2_analysisidhere |>\n",
      "  dplyr::distinct(distinctlisthere)\n",
      "# the groupings, as siera reads them: stratavarshere\n",
      "# every group of the first grouping (by), the levels above as they are\n",
      "# in the data (strata)\n",
      "df3_analysisidhere <- cards::ard_tabulate(\n",
      "  data = in_data,\n",
      if (identical(depth, 3L)) "  by = 'firstvarhere',\n",
      "  strata = 'parentvarhere',\n",
      "  variables = 'childvarhere',\n",
      "  denominator = denom_dataset\n",
      ")\n",
      "# a level of a hierarchy: only the pairs of it and the level above it\n",
      "# that are in the data\n",
      "seen <- in_data |>\n",
      "  dplyr::distinct(.p = as.character(parentvarhere),\n",
      "                  .c = as.character(childvarhere))\n",
      ".chr <- function(x) vapply(x, function(v) as.character(unlist(v))[1L], '')\n",
      "df3_analysisidhere <- df3_analysisidhere |>\n",
      "  dplyr::mutate(.p = .chr(", nest_group, "), .c = .chr(variable_level)) |>\n",
      "  dplyr::semi_join(seen, by = c('.p', '.c')) |>\n",
      "  dplyr::select(-'.p', -'.c')", tail),
    flat = paste0(
      "denom_dataset <- df2_denomanaidhere |>\n",
      "  dplyr::select(denomvarhere)\n",
      "in_data <- df2_analysisidhere |>\n",
      "  dplyr::distinct(distinctlisthere) |>\n",
      "  dplyr::mutate(.flag_ = 'Y')\n",
      "df3_analysisidhere <- cards::ard_tabulate(\n",
      "  data = in_data,\n",
      "  by = c(bylistchere),\n",
      "  variables = '.flag_',\n",
      "  denominator = denom_dataset\n",
      ")", tail),
    continuous = paste0(
      "df3_analysisidhere <- cards::ard_summary(\n",
      "  data = df2_analysisidhere,\n",
      "  by = c(bylistchere),\n",
      "  variables = anavarhere\n",
      ")", tail),
    proportion_ci = paste0(
      "df3_analysisidhere <- cardx::ard_categorical_ci(\n",
      "  data = df2_analysisidhere,\n",
      "  by = c(bylistchere),\n",
      "  variables = anavarhere",
      if (!is.null(opt)) paste0(",\n  method = '", opt, "'"), "\n",
      ")", tail),
    chisq = paste0(
      "df3_analysisidhere <- cardx::ard_stats_chisq_test(\n",
      "  data = df2_analysisidhere,\n",
      "  by = groupvar1here,\n",
      "  variables = groupvar2here\n",
      ")", tail))
}

# Rename every id in the model so it holds only [A-Za-z0-9_].
.ars_san_ids <- function(x) {
  keys <- c("id", "analysisId", "outputId", "analysisSetId", "dataSubsetId",
            "groupingId", "methodId", "operationId", "subSectionId",
            "referencedOperationRelationshipId")
  walk <- function(z) {
    if (!is.list(z)) return(z)
    nm <- names(z)
    for (i in seq_along(z)) {
      if (!is.null(nm) && nm[i] %in% keys && is.character(z[[i]])) {
        z[[i]] <- .ars_san(z[[i]])
      } else if (is.list(z[[i]])) {
        z[[i]] <- walk(z[[i]])
      }
    }
    z
  }
  walk(x)
}

# tfl_ars(profile = "siera"): the cdisc model, made runnable by siera.
.ars_siera <- function(ars) {
  info <- attr(ars, "ids")
  un <- attr(ars, "unmapped")
  miss <- function(where, item, why) {
    un[nrow(un) + 1L, ] <<- list(where, item, why)
  }
  info$kind <- .ars_siera_kind(info$method, info$role)
  an_by_id <- stats::setNames(ars$analyses,
                              vapply(ars$analyses, `[[`, "", "id"))
  ng <- vapply(info$ars_id, function(i) length(an_by_id[[i]]$orderedGroupings),
               1L)
  lvl <- info$role == "level"
  first <- lvl & !duplicated(paste(info$output_id, info$analysis_id, lvl))
  nest <- lvl & !first
  info$kind[nest & ng %in% 2:3] <- "nested"
  for (i in which(nest & !ng %in% 2:3)) {
    miss(paste(info$output_id[i], info$analysis_id[i], sep = " / "),
         "variables", paste(
           "siera names three grouping variables at most: this level of",
           "the hierarchy is left out of the siera profile"))
    info$kind[i] <- NA_character_
  }
  old_m <- stats::setNames(ars$methods, vapply(ars$methods, `[[`, "", "id"))
  drop <- info$ars_id[is.na(info$kind)]
  for (i in which(is.na(info$kind) & info$role != "level")) {
    miss(paste(info$output_id[i], info$analysis_id[i], sep = " / "),
         "method", sprintf(paste(
           "siera has no template for `%s` here: left out of the siera",
           "profile (the cdisc profile carries it)"), info$method[i]))
  }

  # the siera methods, one per kind (and statistics / CI method)
  methods <- list()
  groupn_op <- "Mth_total_n_1_n"
  method_for <- function(i) {
    a <- an_by_id[[info$ars_id[i]]]
    kind <- info$kind[i]
    opt <- NULL
    stats <- .ars_siera_stats[[kind]]
    if (kind == "continuous") {
      ops <- old_m[[a$methodId]]$operations
      stats <- vapply(ops, `[[`, "", "label")
      def <- .ars_method_stats("continuous")
      bad <- setdiff(stats, def)
      if (length(bad)) {
        miss(paste(info$output_id[i], info$analysis_id[i], sep = " / "),
             "statistics", sprintf(paste(
               "%s: the siera template summarises %s only"),
               paste(bad, collapse = ", "), paste(def, collapse = ", ")))
        stats <- intersect(stats, def)
      }
    }
    if (kind == "proportion_ci") {
      d <- old_m[[a$methodId]]$description
      if (grepl("\\(([^)]+)\\)$", d)) opt <- sub("^.*\\(([^)]+)\\)$", "\\1", d)
    }
    depth <- if (kind == "nested") length(a$orderedGroupings) else NULL
    id <- paste0("Mth_", kind, depth,
                 if (!is.null(opt)) paste0("_", .ars_san(opt)))
    if (kind == "continuous" &&
        !identical(stats, .ars_method_stats("continuous"))) {
      id <- paste0(id, "_", paste(.ars_san(stats), collapse = "_"))
    }
    if (is.null(methods[[id]])) {
      opids <- paste(id, seq_along(stats), .ars_san(stats), sep = "_")
      ops <- lapply(seq_along(stats), function(j) {
        o <- list(id = opids[j],
                  name = unname(.ars_stat_names[stats[j]] %|NA|% stats[j]),
                  label = stats[j], order = j)
        if (stats[j] == "p") {
          o$referencedOperationRelationships <- list(
            list(id = paste0(opids[j], "_NUM"),
                 referencedOperationRole = list(controlledTerm = "NUMERATOR"),
                 operationId = opids[match("n", stats)],
                 description = "the count of this analysis"),
            list(id = paste0(opids[j], "_DEN"),
                 referencedOperationRole = list(controlledTerm = "DENOMINATOR"),
                 operationId = groupn_op,
                 description = "the output's subject count by group"))
        }
        o
      })
      p <- .ars_siera_params[[paste0(kind, depth)]]
      methods[[id]] <<- list(
        id = id, name = unname(.ars_siera_names[[kind]]), label = kind,
        description = sprintf("tflspec template for siera (%s%s)", kind,
                              if (!is.null(opt)) paste0(", ", opt) else ""),
        operations = ops,
        codeTemplate = list(
          context = .ars_siera_context,
          code = .ars_siera_code(kind, stats, opids, opt, depth),
          parameters = lapply(names(p), function(n) list(
            name = n, valueSource = unname(p[[n]]),
            description = sprintf("siera's %s", p[[n]])))))
    }
    methods[[id]]
  }

  analyses <- list()
  for (i in seq_len(nrow(info))) {
    if (is.na(info$kind[i])) next
    a <- an_by_id[[info$ars_id[i]]]
    m <- method_for(i)
    a$methodId <- m$id
    if (info$kind[i] == "proportion_ci") {
      # the template tabulates the variable itself, by the `by` groupings
      a$variable <- info$variable[i]
      a$orderedGroupings <- utils::head(a$orderedGroupings, -1L)
      if (!length(a$orderedGroupings)) a$orderedGroupings <- NULL
    }
    if (!is.null(a$referencedAnalysisOperations)) {
      p <- m$operations[[match("p", vapply(m$operations, `[[`, "",
                                           "label"))]]$id
      a$referencedAnalysisOperations[[1L]]$referencedOperationRelationshipId <-
        paste0(p, "_NUM")
      a$referencedAnalysisOperations[[2L]]$referencedOperationRelationshipId <-
        paste0(p, "_DEN")
    }
    if (info$kind[i] == "chisq" && length(a$orderedGroupings) != 2L) {
      miss(paste(info$output_id[i], info$analysis_id[i], sep = " / "), "by",
           "the siera chi-square template compares one `by` variable")
    }
    analyses[[length(analyses) + 1L]] <- c(a, list(.out = info$output_id[i],
                                                   .groupn = info$role[i] == "groupn"))
  }
  # each output's subject count first
  outs <- vapply(ars$outputs, `[[`, "", "id")
  ord <- order(match(vapply(analyses, `[[`, "", ".out"), outs),
               !vapply(analyses, `[[`, NA, ".groupn"))
  analyses <- analyses[ord]
  for (k in seq_along(analyses)) analyses[[k]]$.out <- analyses[[k]]$.groupn <- NULL

  # the list of contents: kept analyses, the count first
  kept <- vapply(analyses, `[[`, "", "id")
  items <- ars$mainListOfContents$contentsList$listItems
  for (i in seq_along(items)) {
    sub <- items[[i]]$sublist$listItems
    sub <- Filter(function(z) z$analysisId %in% kept, sub)
    sub <- sub[order(match(vapply(sub, `[[`, "", "analysisId"), kept))]
    for (k in seq_along(sub)) sub[[k]]$order <- k
    items[[i]]$sublist$listItems <- sub
  }
  items <- Filter(function(z) length(z$sublist$listItems) > 0L, items)
  for (i in seq_along(items)) items[[i]]$order <- i
  ars$mainListOfContents$contentsList$listItems <- items
  keep_out <- vapply(items, `[[`, "", "outputId")
  ars$outputs <- Filter(function(o) o$id %in% keep_out, ars$outputs)
  ars$otherListsOfContents <- list(list(
    name = "List of Planned Outputs",
    contentsList = list(listItems = lapply(seq_along(items), function(i) list(
      name = items[[i]]$name, level = 1L, order = i,
      outputId = items[[i]]$outputId)))))

  ars$analyses <- analyses
  ars$methods <- unname(methods)
  # groupings and data subsets still used
  used_g <- unique(unlist(lapply(analyses, function(a)
    vapply(a$orderedGroupings %||% list(), `[[`, "", "groupingId"))))
  ars$analysisGroupings <- Filter(function(g) g$id %in% used_g,
                                  ars$analysisGroupings)
  used_s <- unique(unlist(lapply(analyses, `[[`, "dataSubsetId")))
  ars$dataSubsets <- Filter(function(s) s$id %in% used_s,
                            ars$dataSubsets %||% list())
  # siera reads these sections even when they are empty
  for (k in c("dataSubsets", "analysisSets", "analysisGroupings")) {
    if (is.null(ars[[k]])) ars[[k]] <- list()
  }

  # ids siera can make R names of
  info$siera_id <- .ars_san(info$ars_id)
  info$siera_output_id <- .ars_san(info$output_id)
  for (k in c("analyses", "outputs", "analysisSets", "analysisGroupings")) {
    i <- vapply(ars[[k]], `[[`, "", "id")
    d <- unique(i[duplicated(.ars_san(i)) & !duplicated(i)])
    if (length(d)) {
      .ard_stop(sprintf(paste(
        "These ids become the same id for siera (only letters, digits and",
        "_ are kept): %s.  Rename them in the spec."),
        paste(d, collapse = ", ")))
    }
  }
  ars <- .ars_san_ids(ars)
  attr(ars, "ids") <- info
  attr(ars, "unmapped") <- unique(un)
  ars
}

# What siera needs beyond the CDISC model (tfl_check_ars() on a siera
# profile): rows for tfl_check_ars()'s table.
.ars_siera_checks <- function(re) {
  out <- data.frame(part = character(), field = character(),
                    problem = character(), stringsAsFactors = FALSE)
  add <- function(p, f, x) out[nrow(out) + 1L, ] <<- list(p, f, x)
  for (k in c("otherListsOfContents", "mainListOfContents", "dataSubsets",
              "analysisSets", "analysisGroupings", "analyses", "methods")) {
    if (is.null(re[[k]])) add("ReportingEvent", k, "siera reads this section")
  }
  ids <- c(vapply(re$analyses %||% list(), `[[`, "", "id"),
           vapply(re$outputs %||% list(), `[[`, "", "id"))
  bad <- ids[grepl("[^A-Za-z0-9_]", ids)]
  for (b in bad) add(b, "id", "siera makes R names of ids: letters, digits and _ only")
  for (s in re$analysisSets) {
    if (!is.null(s$compoundExpression)) {
      add(paste("AnalysisSet", s$id), "condition", paste(
        "siera applies an analysis set of one condition only:",
        "the rest is not applied"))
    }
  }
  an <- stats::setNames(re$analyses %||% list(),
                        vapply(re$analyses %||% list(), `[[`, "", "id"))
  for (it in re$mainListOfContents$contentsList$listItems) {
    a <- vapply(it$sublist$listItems, function(z) z$analysisId %||% "", "")
    a <- a[a %in% names(an)]
    part <- paste("Output", it$outputId)
    if (length(a) < 3L) {
      add(part, "analyses", paste(
        "siera applies the analysis set from the output's third analysis:",
        "an output needs three analyses at least"))
    }
    sets <- unique(vapply(an[a], function(z) z$analysisSetId %||% "", ""))
    if (length(sets) > 1L) {
      add(part, "analysisSetId", sprintf(
        "siera applies one analysis set an output (here %s)",
        paste(sets, collapse = ", ")))
    }
    for (z in an[a]) {
      m <- Filter(function(m) identical(m$id, z$methodId), re$methods)
      if (length(m) && is.null(m[[1L]]$codeTemplate)) {
        add(paste("Analysis", z$id), "methodId", sprintf(
          "method %s has no code template: siera skips it", z$methodId))
      }
    }
  }
  out
}
