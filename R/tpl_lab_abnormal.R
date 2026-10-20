# ---- lab_abnormal: subjects with laboratory values in abnormal categories ------
# (registered at the end of tpl_registry.R: R reads this file before it)

.tpl_la_params <- function() list(
  tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
  tfl_param("data", "ADLB", doc = "the dataset of records"),
  tfl_param("params", NULL, doc = "the PARAMCD values; NULL: all"),
  tfl_param("where", NULL, doc = "the records kept (R), e.g. ONTRTFL == \"Y\""),
  tfl_param("by", "TRT01A", doc = "the column variable"),
  tfl_param("category", "ANRIND",
            doc = "the category's variable (ANRIND, a criterion's MCRIT1ML)"),
  tfl_param("levels", NULL, type = "levels", multiple = TRUE, of = "category",
            doc = "the values counted, in order (lowest first); NULL: the code list's"),
  tfl_param("cumulative", FALSE,
            doc = "count a subject in every level up to their worst (level 2 or more ...)"),
  tfl_param("baseline_normal", FALSE,
            doc = "only the subjects normal at baseline, in n and in the N"),
  tfl_param("baseline", "BNRIND", doc = "the category at baseline"),
  tfl_param("baseline_value", "NORMAL", doc = "the value of `baseline` that is normal"),
  tfl_param("data_id", NULL, doc = "the analysis data's id; NULL: <data>_<population>"),
  tfl_param("heading", "Parameter / Category", doc = "the label column's heading")
)

.tpl_la_body <- function(a, ctx) {
  pop <- a$population
  subj <- paste0("adsl_", tolower(pop))
  set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(pop))
  ct <- a$category
  lv <- a$levels %||% .tpl_levels(ct, ctx$codelists)[[ct]]
  cum <- isTRUE(a$cumulative)
  if (cum && !length(lv)) {
    .ard_stop(paste("template lab_abnormal: cumulative = TRUE needs `levels`",
                    "(or a code list of the category)."))
  }
  q <- function(x) paste0("c(", paste0("\"", x, "\"", collapse = ", "), ")")
  where <- c(a$where, if (length(a$params)) sprintf("PARAMCD %%in%% %s", q(a$params)),
             if (isTRUE(a$baseline_normal)) sprintf("%s == \"%s\"", a$baseline, a$baseline_value))
  keys <- paste(c("USUBJID", a$by, "PARAM"), collapse = ", ")
  per <- function(suffix, label, ..., from = set) Filter(Negate(is.null), list(
    id = paste0(set, suffix), label = label, from = from, ...))
  # each subject once in a category of a parameter; cumulative: in every
  # level up to their worst
  # (code over two lines: the program lays it out a call a line)
  cats <- if (cum) list(
    per("_worst", "each subject's worst level of each parameter", code = sprintf(
      "dplyr::summarise(dplyr::filter(%s, %s %%in%% %s),\n  worst = max(match(%s, %s)), .by = c(%s))",
      set, ct, q(lv), ct, q(lv), keys)),
    per("_cat", "each subject in every level up to their worst", from = paste0(set, "_worst"),
        code = sprintf(paste0(
          "dplyr::filter(dplyr::cross_join(%s_worst, data.frame(%s = factor(%s, levels = %s))),",
          "\n  as.integer(%s) <= worst)"), set, ct, q(lv), q(lv), ct))
  ) else list(
    per("_cat", "each subject once in each category",
        where = if (length(lv)) tfl_r(sprintf("%s %%in%% %s", ct, q(lv))),
        derive = if (length(lv)) tfl_r(sprintf("%s = factor(%s, levels = %s)", ct, ct, q(lv))),
        distinct = paste(c("USUBJID", a$by, "PARAM", ct), collapse = " | ")))
  sets <- c(list(
    list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
         population = pop),
    Filter(Negate(is.null), list(
      id = set, from = a$data, subjects = subj,
      where = if (length(where)) tfl_r(paste(where, collapse = " & ")),
      # every parameter analysed a level: its rows even with no subject in
      # a category (0/N)
      derive = tfl_r("PARAM = factor(PARAM)"))),
    # the N
    per("_n", "the subjects with a value of each parameter",
        code = sprintf("dplyr::distinct(%s, %s)", set, keys))), cats)
  analyses <- list(
    list(id = "GROUPN", method = "categorical", data = subj, variables = a$by,
         purpose = "PRIMARY OUTCOME MEASURE"),
    list(id = "ABNORMAL", method = "categorical", data = paste0(set, "_cat"),
         by = c(a$by, "PARAM"), variables = ct, denominator = paste0(set, "_n"),
         purpose = "PRIMARY OUTCOME MEASURE"))
  content <- c(
    list(tfl_item("table_plan", list(cols = a$by, rows = list(group = "PARAM"))),
         tfl_item("plan_cells", list(categorical = "{n}/{N} ({p:.1f%})"))),
    if (length(lv)) list(tfl_item("plan_levels", list(.label = lv))),
    list(tfl_item("plan_stub", list(vars = c("group", "label"), name = "row_label")),
         tfl_item("plan_col_header", list(header = list(
           list(line = "1", cols = "row_label", text = a$heading),
           list(line = "1", cols = ".values", span = "each", text = "{col}\n(N={n})")))),
         tfl_item("plan_paginate_rows", list(max_rows = 30L, split = "group_safe"))))
  list(ard = list(analysis_data = sets, analyses = analyses),
       data = list(source = "ard", object = "ard"), content = content)
}

.tpl_la_register <- function() {
  .tfl_template("lab_abnormal", kind = "table", layout = "summary",
                params = .tpl_la_params(), body = .tpl_la_body)
}

#' Laboratory abnormalities: a table plan from the template `lab_abnormal`
#'
#' The subjects with a laboratory value in an abnormal category, a block a
#' parameter (`PARAM`): the normal range indicator (`ANRIND`: `LOW`,
#' `HIGH`), or a criterion's levels (`MCRIT1ML`).  A cell is n/N (%): N is
#' the arm's subjects with a value of the parameter in the records kept, n
#' those with a value in the category; every parameter analysed has its
#' rows, 0/N when no subject is in a category.  A subject is counted once in each
#' category; with `cumulative = TRUE`, in every level up to their worst
#' (`levels` lowest first: "level 2 or more").  With `baseline_normal =
#' TRUE`, n and N are the subjects normal at baseline only (`BNRIND ==
#' "NORMAL"`).  The categories' order is `levels`, or the code list of
#' `category`.
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `data`, `params`, `where`,
#'   `by`, `category`, `levels`, `cumulative`, `baseline_normal`,
#'   `baseline`, `baseline_value`, `heading` ...
#' @return A [rtfreporter::table_plan()].
#' @examples
#' x <- tfl_template_expand("lab_abnormal", list(population = "SAF",
#'                          levels = c("LOW", "HIGH"), where = "ONTRTFL == \"Y\""))
#' vapply(x$ard$analysis_data, `[[`, "", "id")
#' @export
tpl_lab_abnormal <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("lab_abnormal", ard, c(list(population = population), list(...)),
              codelists = codelists)
}
