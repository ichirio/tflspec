# ============================================================================
#  CDISC ARS: writing the JSON and checking it
# ============================================================================

# The reporting event as plain lists: no class, no attributes, no empty
# fields.  A list stays a JSON array even with one element; a length-one
# atomic is a JSON scalar.
.ars_plain <- function(x) {
  if (is.list(x)) {
    nm <- names(x)
    x <- lapply(unclass(x), .ars_plain)
    attributes(x) <- NULL
    if (!is.null(nm)) {
      names(x) <- nm
      keep <- !vapply(x, function(v) is.null(v) ||
                        (is.atomic(v) && length(v) == 1L && is.na(v)), NA)
      x <- x[keep]
    }
    return(x)
  }
  if (is.factor(x)) x <- as.character(x)
  attributes(x) <- NULL
  x
}

.ars_json <- function(ars, pretty = TRUE) {
  jsonlite::toJSON(.ars_plain(ars), auto_unbox = TRUE, pretty = pretty,
                   null = "null", digits = NA)
}

#' Write a CDISC ARS reporting event as JSON
#'
#' The JSON of the CDISC Analysis Results Standard v1.0 model, the form ARS
#' is exchanged in (and what siera's `readARS()` and CDISC's own tools
#' read).  The same specs always give the same file.
#'
#' @param ars A [tfl_ars()].
#' @param path Destination `.json`.
#' @param pretty Indent the JSON.
#' @return `path`, invisibly.
#' @seealso [tfl_check_ars()]
#' @export
tfl_write_ars_json <- function(ars, path, pretty = TRUE) {
  if (!inherits(ars, "tfl_ars")) {
    .ard_stop("`ars` must be a tfl_ars(); write the specs with tfl_ars() first.")
  }
  if (!is.character(path) || length(path) != 1L ||
      !grepl("\\.json$", path, ignore.case = TRUE)) {
    .ard_stop("`path` must be one .json file.")
  }
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(paste0(.ars_json(ars, pretty), "\n"))), con)
  invisible(path)
}

# The CDISC ARS v1.0 JSON Schema shipped with the package.
.ars_schema_path <- function() {
  system.file("ars", "ars_ldm.json", package = "tflspec")
}

#' Check a CDISC ARS reporting event
#'
#' Checks what ARS requires and what the reporting event refers to: each
#' required field is there (an analysis's `purpose` above all: it is the
#' SAP's decision, so [tfl_ars()] leaves it blank when the spec does), each
#' id is used once in its class, and each reference (`methodId`,
#' `analysisSetId`, `dataSubsetId`, `groupingId`, an operation, an output or
#' analysis of the list of contents) names something that is there, and each
#' sub-clause of an analysis set's or data subset's `compoundExpression`
#' carries its `level` and `order` and a `subClauseId` naming a set / subset
#' (or its own `condition` or `compoundExpression`).  With
#' `schema = TRUE` and the jsonvalidate package installed, the JSON is
#' also checked against CDISC's JSON Schema for ARS v1.0
#' (`system.file("ars", "ars_ldm.json", package = "tflspec")`).
#'
#' @param ars A [tfl_ars()], or the path of an ARS `.json` file.
#' @param schema Check against the JSON Schema too (needs jsonvalidate;
#'   skipped with a message when it is not installed).
#' @param profile `"siera"` checks also what siera needs to run it (three
#'   analyses an output, one analysis set an output and of one condition,
#'   a code template for each method, ids of letters, digits and `_`).
#'   Default: the profile the `tfl_ars()` was written with, else `"cdisc"`.
#' @return A data frame, one row per problem (none: zero rows): `part` (the
#'   class and id), `field` and `problem`.
#' @examples
#' f <- system.file("ars", "ars_ldm.json", package = "tflspec")
#' file.exists(f)
#' @export
tfl_check_ars <- function(ars, schema = TRUE, profile = NULL) {
  profile <- profile %||% attr(ars, "profile") %||% "cdisc"
  profile <- match.arg(profile, c("cdisc", "siera"))
  out <- data.frame(part = character(), field = character(),
                    problem = character(), stringsAsFactors = FALSE)
  add <- function(p, f, x) out[nrow(out) + 1L, ] <<- list(p, f, x)
  if (is.character(ars) && length(ars) == 1L) {
    if (!file.exists(ars)) .stop_no_file(ars, "tfl_check_ars")
    txt <- paste(readLines(ars, warn = FALSE, encoding = "UTF-8"),
                 collapse = "\n")
    re <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  } else if (inherits(ars, "tfl_ars")) {
    re <- .ars_plain(ars)
    txt <- NULL
  } else {
    .ard_stop("`ars` must be a tfl_ars() or the path of an ARS .json file.")
  }

  need <- function(obj, part, fields) {
    for (f in fields) {
      v <- obj[[f]]
      if (is.null(v) || (is.atomic(v) && length(v) == 1L &&
                         (is.na(v) || identical(v, "")))) {
        add(part, f, "required, and missing")
      }
    }
  }
  ids_of <- function(k) vapply(re[[k]] %||% list(), function(z)
    as.character(z$id %||% NA), "")
  dup <- function(k, cls) {
    i <- ids_of(k)
    for (d in unique(i[duplicated(i)])) {
      add(sprintf("%s %s", cls, d), "id", "used more than once")
    }
  }
  need(re, "ReportingEvent", c("id", "name", "mainListOfContents"))

  for (k in c("analysisSets", "dataSubsets")) {
    cls <- if (k == "analysisSets") "AnalysisSet" else "DataSubset"
    dup(k, cls)
    for (z in re[[k]]) need(z, paste(cls, z$id), c("id", "name", "level",
                                                    "order"))
  }
  # a compound expression's sub-clauses: each its level and order, and a
  # subClauseId naming a set / subset of the class, or its own condition or
  # compoundExpression (#511: what the JSON Schema says, without jsonvalidate)
  subs_ids <- list(analysisSets = ids_of("analysisSets"),
                   dataSubsets = ids_of("dataSubsets"))
  clauses <- function(ce, part, k) {
    for (j in seq_along(ce$whereClauses)) {
      w <- ce$whereClauses[[j]]
      at <- sprintf("%s whereClauses[%d]", part, j)
      need(w, at, c("level", "order"))
      if (!is.null(w$subClauseId)) {
        if (!w$subClauseId %in% subs_ids[[k]]) {
          add(at, "subClauseId", sprintf("no %s %s",
                                         if (k == "analysisSets") "analysis set" else "data subset",
                                         w$subClauseId))
        }
      } else if (is.null(w$condition) && is.null(w$compoundExpression)) {
        add(at, "subClauseId",
            "a sub-clause holds a subClauseId, a condition or a compoundExpression")
      }
      if (!is.null(w$compoundExpression)) clauses(w$compoundExpression, at, k)
    }
  }
  for (k in names(subs_ids)) {
    cls <- if (k == "analysisSets") "AnalysisSet" else "DataSubset"
    for (z in re[[k]]) {
      if (!is.null(z$compoundExpression)) clauses(z$compoundExpression, paste(cls, z$id), k)
    }
  }
  dup("analysisGroupings", "AnalysisGrouping")
  for (g in re$analysisGroupings) {
    need(g, paste("AnalysisGrouping", g$id), c("id", "name", "dataDriven"))
  }
  dup("methods", "AnalysisMethod")
  ops <- character()
  rels <- character()
  for (m in re$methods) {
    part <- paste("AnalysisMethod", m$id)
    need(m, part, c("id", "name", "operations"))
    for (o in m$operations) {
      need(o, paste("Operation", o$id), c("id", "name", "order"))
      ops <- c(ops, o$id)
      for (r in o$referencedOperationRelationships) {
        rels <- c(rels, r$id)
      }
    }
  }
  for (m in re$methods) for (o in m$operations) {
    for (r in o$referencedOperationRelationships) {
      if (!r$operationId %in% ops) {
        add(paste("Operation", o$id), "referencedOperationRelationships",
            sprintf("names operation %s, which no method has", r$operationId))
      }
    }
  }
  dup("analyses", "Analysis")
  sets <- ids_of("analysisSets")
  subs <- ids_of("dataSubsets")
  grps <- ids_of("analysisGroupings")
  meths <- ids_of("methods")
  ans <- ids_of("analyses")
  for (a in re$analyses) {
    part <- paste("Analysis", a$id)
    need(a, part, c("id", "name", "methodId"))
    # a CDISC term, or a sponsor's (its id in terminologyExtensions)
    term <- function(field, terms, what, hint) {
      v <- a[[field]]
      if (!is.null(v$sponsorTermId)) return(invisible())
      if (is.null(v$controlledTerm)) {
        add(part, field, sprintf("ARS requires a %s (%s)", field, hint))
      } else if (!v$controlledTerm %in% terms) {
        add(part, field, sprintf("`%s` is not a CDISC analysis %s",
                                 v$controlledTerm, what))
      }
    }
    term("purpose", .ars_purposes, "purpose", paste(
      "PRIMARY / SECONDARY / EXPLORATORY OUTCOME MEASURE: fill the",
      "analyses' `purpose` column of the ARD spec"))
    term("reason", .ars_reasons, "reason", "SPECIFIED IN SAP ...")
    if (!is.null(a$methodId) && !a$methodId %in% meths) {
      add(part, "methodId", sprintf("no method %s", a$methodId))
    }
    if (!is.null(a$analysisSetId) && !a$analysisSetId %in% sets) {
      add(part, "analysisSetId", sprintf("no analysis set %s",
                                         a$analysisSetId))
    }
    if (!is.null(a$dataSubsetId) && !a$dataSubsetId %in% subs) {
      add(part, "dataSubsetId", sprintf("no data subset %s", a$dataSubsetId))
    }
    for (g in a$orderedGroupings) {
      if (!g$groupingId %in% grps) {
        add(part, "orderedGroupings", sprintf("no grouping %s", g$groupingId))
      }
    }
    for (r in a$referencedAnalysisOperations) {
      if (!r$referencedOperationRelationshipId %in% rels) {
        add(part, "referencedAnalysisOperations", sprintf(
          "no operation relationship %s", r$referencedOperationRelationshipId))
      }
      if (!r$analysisId %in% ans) {
        add(part, "referencedAnalysisOperations",
            sprintf("no analysis %s", r$analysisId))
      }
    }
  }
  dup("outputs", "Output")
  outs <- ids_of("outputs")
  for (o in re$outputs) {
    need(o, paste("Output", o$id), c("id", "name", "displays"))
  }
  walk <- function(items) {
    for (it in items) {
      if (!is.null(it$outputId) && !it$outputId %in% outs) {
        add("ListOfContents", "outputId", sprintf("no output %s", it$outputId))
      }
      if (!is.null(it$analysisId) && !it$analysisId %in% ans) {
        add("ListOfContents", "analysisId",
            sprintf("no analysis %s", it$analysisId))
      }
      walk(it$sublist$listItems)
    }
  }
  walk(re$mainListOfContents$contentsList$listItems)

  if (isTRUE(schema)) {
    if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
      message("The JSON Schema check needs the jsonvalidate package; ",
              "the structure was checked without it.")
    } else {
      if (is.null(txt)) txt <- .ars_json(ars, pretty = FALSE)
      sch <- paste(readLines(.ars_schema_path(), warn = FALSE,
                             encoding = "UTF-8"), collapse = "\n")
      v <- jsonvalidate::json_schema$new(sch, engine = "ajv")
      ok <- v$validate(txt, verbose = TRUE, greedy = TRUE)
      if (!isTRUE(ok)) {
        e <- attr(ok, "errors")
        for (j in seq_len(NROW(e))) {
          # a missing purpose / reason is named above, analysis by analysis
          if (grepl("required property '(purpose|reason)'", e$message[j])) {
            next
          }
          add(paste("schema", e$instancePath[j] %||% e$dataPath[j]),
              e$keyword[j], e$message[j])
        }
      }
    }
  }
  if (profile == "siera") out <- rbind(out, .ars_siera_checks(re))
  unique(out)
}
