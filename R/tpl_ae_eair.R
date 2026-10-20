# ---- ae_eair: SOC / PT by the exposure-adjusted incidence rate -----------------
# (registered at the end of tpl_registry.R: R reads this file before it)

.tpl_eair_params <- function() list(
  tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
  tfl_param("data", "ADAE", doc = "the dataset of records"),
  tfl_param("where", "TRTEMFL == \"Y\"", doc = "the records kept (R)"),
  tfl_param("by", "TRT01A", doc = "the column variable"),
  tfl_param("hierarchy", c("AEBODSYS", "AEDECOD"), doc = "the levels, outer first"),
  tfl_param("exposure", "TRTDURD", doc = "the ADSL variable of the days of exposure"),
  tfl_param("start", "ASTDY",
            doc = "the event's day from the start of treatment: a variable or R (ASTDT - TRTSDT + 1)"),
  tfl_param("per", 100, doc = "the rate per this many person-years"),
  tfl_param("overall", "Any TEAE", doc = "the any-record row's label")
)

.tpl_eair_body <- function(a, ctx) {
  pop <- a$population
  subj <- paste0("adsl_", tolower(pop))
  set <- paste0(tolower(a$data), "_", tolower(pop))
  h <- a$hierarchy
  # each subject with each term (a level's): at risk to the day of their
  # first event, else for the whole exposure
  risk <- function(suffix, keys, what) {
    ks <- c("USUBJID", keys)
    one <- function(x, q = "") {
      if (length(x) == 1L) paste0(q, x, q) else sprintf("c(%s)", paste0(q, x, q, collapse = ", "))
    }
    code <- paste0(
      sprintf("%s |>\n  dplyr::select(USUBJID, %s, %s) |>\n", subj, a$by, a$exposure),
      "  # the day of each subject's first event of each term (none: NA)\n",
      if (length(keys)) sprintf("  dplyr::cross_join(dplyr::distinct(%s, %s)) |>\n", set,
                                paste(keys, collapse = ", ")),
      sprintf("  dplyr::left_join(dplyr::summarise(%s, DAY = min(DAY), .by = %s),\n", set, one(ks)),
      sprintf("    by = %s) |>\n", one(ks, "\"")),
      if (!length(keys)) sprintf("  # the overall row's own SOC\n  dplyr::mutate(%s = \"%s\") |>\n",
                                 h[1L], a$overall),
      "  # at risk to the first event, else for the whole exposure (years)\n",
      "  dplyr::mutate(EVENT = !is.na(DAY),\n",
      sprintf("    YEARS = dplyr::if_else(EVENT, DAY, %s) / 365.25)", a$exposure))
    list(id = paste0(set, suffix), label = paste("each subject's time at risk of", what),
         from = set, code = code)
  }
  levels <- list(list("_risk", character(), h[1L], "any event"),
                 list("_risk_soc", h[1L], h[1L], paste("each", h[1L])),
                 list("_risk_pt", h, h, paste("each", h[length(h)])))
  sets <- c(list(
    list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
         population = pop),
    Filter(Negate(is.null), list(
      id = set, from = a$data, subjects = subj,
      where = if (!is.null(a$where)) tfl_r(a$where), derive = tfl_r(paste("DAY =", a$start))))),
    lapply(levels, function(l) risk(l[[1L]], l[[2L]], l[[4L]])))
  # the rate of a level, the innermost term as the variable (the shape of a
  # hierarchical ARD: its own rows of each level)
  rate <- function(id, l) {
    k <- length(l[[3L]]) + 1L
    list(id = id, method = "cardx::ard_incidence_rate", data = paste0(set, l[[1L]]),
         by = a$by, strata = l[[3L]],
         args = tfl_r(sprintf(paste("time = YEARS, count = EVENT, id = USUBJID,",
                                    "n_person_time = %s, unit_label = \"years\""), a$per)),
         post = sprintf(paste("dplyr::mutate(variable = group%d, variable_level = group%d_level,",
                              "group%d = NULL, group%d_level = NULL)"), k, k, k, k),
         purpose = "PRIMARY OUTCOME MEASURE")
  }
  analyses <- list(
    list(id = "GROUPN", method = "categorical", data = subj, variables = a$by,
         purpose = "PRIMARY OUTCOME MEASURE"),
    rate("EAIR", levels[[1L]]), rate("EAIR_SOC", levels[[2L]]),
    rate("EAIR_PT", levels[[3L]]))
  n <- length(h)
  grp <- paste0("group", seq_len(n - 1L))
  content <- list(
    tfl_item("normalize_ard", list(hierarchy = h)),
    # the overall block first, whatever the order of the others
    tfl_item("plan_levels", stats::setNames(list(a$overall), h[1L])),
    tfl_item("table_plan", list(cols = a$by, rows = stats::setNames(as.list(h[-n]), grp),
                                label = list(label = h[n]))),
    tfl_item("plan_cells", list(incidence_rate = "{n_events} ({estimate:.2f})")),
    tfl_item("plan_sort", list(keys = c("levels(group1)", grp, ".depth", "label"))),
    tfl_item("plan_col_header", list(header = list(
      list(line = "1", cols = "row_label"),
      list(line = "1", cols = ".values", span = "each", text = "{col}\n(N={n})"),
      list(line = "2", cols = "row_label",
           text = "System Organ Class\n   Preferred Term"),
      list(line = "2", cols = ".values", span = "each", text = sprintf("n (EAIR per %s PY)", a$per))))),
    tfl_item("plan_paginate_rows", list(max_rows = 22L, split = "group_force")))
  list(ard = list(analysis_data = sets, analyses = analyses),
       data = list(source = "ard", object = "ard"), content = content)
}

.tpl_eair_register <- function() {
  .tfl_template("ae_eair", kind = "table", layout = "hierarchical",
                params = .tpl_eair_params(), body = .tpl_eair_body)
}

#' Exposure-adjusted incidence rates by SOC and PT: the template `ae_eair`
#'
#' A display template on the `hierarchical` layout: each arm's subjects
#' with an event (n) and the exposure-adjusted incidence rate (EAIR) --
#' overall, in each system organ class and in each preferred term.  The
#' EAIR is the subjects with the event over their total time at risk, per
#' `per` person-years (365.25 days a year): a subject with the event is at
#' risk until their first one, a subject without it for their whole
#' exposure (`exposure`, ADSL's `TRTDURD`).  The rates are cardx's
#' `ard_incidence_rate()`.
#'
#' @details The days to a subject's first event are the smallest `start`
#'   of their records of the term: `ASTDY` (the study standard), or R that
#'   makes the day from ADAE's columns, `ASTDT - TRTSDT + 1`.
#'
#' @inheritParams tpl_demog
#' @param ... The template's other parameters: `data`, `where`, `by`,
#'   `hierarchy`, `exposure`, `start`, `per`, `overall`.
#' @return A [rtfreporter::table_plan()].
#' @examples
#' x <- tfl_template_expand("ae_eair", list(population = "SAF"))
#' vapply(x$ard$analyses, `[[`, "", "method")
#' @export
tpl_ae_eair <- function(ard, population, ..., codelists = NULL) {
  .tpl_object("ae_eair", ard, c(list(population = population), list(...)),
              codelists = codelists)
}
