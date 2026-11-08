# ---- pk_conc: concentrations by subject, their summary below (#565) ----------
# (registered at the end of tpl_registry.R: R reads this file before it)
# One table a page per group (an analyte, an arm): the subjects' values
# above, one row a subject, the time points across, and their summary
# statistics below over the same columns -- two ordinary plans, the
# summary laid under the listing by rtfreporter's plan_append() (#737), as
# the SPEC writes a part (spec_parts.R).

.tpl_pk_stats <- list(N = "{N:.0f}", Mean = "{mean:.4s}", SD = "{sd:.5s}", SE = "{se:.5s}",
                      `CV(%)` = "{cv:.1f}", Median = "{median:.4s}", Min = "{min:.3s}",
                      Max = "{max:.3s}")

# The summary's statistics: every one the cells may name (a time point
# with no value -- every sample BLQ after Cmax -- leaves them empty, N 0;
# CV(%) is empty when the mean is 0)
.tpl_pk_statistic <- paste(
  "statistic = ~ list(N = length, sd = sd, median = median,",
  "mean = \\(x) if (length(x)) mean(x) else NA_real_,",
  "min = \\(x) if (length(x)) min(x) else NA_real_,",
  "max = \\(x) if (length(x)) max(x) else NA_real_,",
  "se = \\(x) sd(x) / sqrt(length(x)),",
  "cv = \\(x) if (!length(x) || mean(x) == 0) NA_real_ else 100 * sd(x) / mean(x))")

.tpl_pk_params <- function() list(
  tfl_param("population", required = TRUE, doc = "the analysis set's id (SAF)"),
  .tpl_subset_param(),
  tfl_param("data", "ADPC", doc = "the dataset of concentrations"),
  tfl_param("where", NULL, doc = "the records kept (R), e.g. ANL01FL == \"Y\""),
  tfl_param("by", c("PARAM", "TRT01A"), type = "vars", multiple = TRUE,
            doc = "the groups: a page each (an analyte, an arm), the summary's too"),
  tfl_param("heading", NULL, doc = paste(
    "a page's name in the titles ({<group>}: its value); NULL: the groups'",
    "values, a line each")),
  tfl_param("time", "ATPT", doc = "the time point's variable (the columns)"),
  tfl_param("time_order", "ATPTN", doc = "the time points' order variable; NULL: by name"),
  tfl_param("times", NULL, doc = "the time points, in order; NULL: by time_order"),
  tfl_param("value", "AVAL", doc = "the concentration"),
  tfl_param("blq", NULL, doc = paste(
    "the records below the limit of quantification (R), e.g. BLQFL == \"Y\":",
    "the listing prints blq_label for them; the summary reads the values")),
  tfl_param("blq_label", "BLQ", doc = "what the listing prints for a BLQ record"),
  tfl_param("cell", "{value:.3s}", doc = "a subject's value (3 significant digits)"),
  tfl_param("subject", "{USUBJID}/{AGE}/{SEX}", doc = "a subject's row label ({<variable>})"),
  tfl_param("subject_heading", "Subject ID/Age (years)/Gender",
            doc = "the subjects' column heading"),
  tfl_param("time_heading", "Time Relative to Dose", doc = "the heading over the time points"),
  tfl_param("summary_label", "Summary Statistics", doc = "the summary's heading row"),
  tfl_param("stats", .tpl_pk_stats, doc = "the summary's rows: label = cell"),
  tfl_param("split_at", NULL, doc = paste(
    "the time point that starts a second block of columns (each block its",
    "pages, the group's first); NULL: one")),
  tfl_param("data_id", NULL, doc = "the analysis data's id; NULL: <data>_<population>")
)

.tpl_pk_body <- function(a, ctx) {
  pop <- a$population
  subj <- paste0("adsl_", tolower(pop))
  set <- a$data_id %||% paste0(tolower(a$data), "_", tolower(pop))
  tm <- a$time
  q <- function(x) sprintf("\"%s\"", gsub("\"", "\\\"", x, fixed = TRUE))
  order <- if (length(a$time_order)) sprintf("%s = reorder_by(%s, %s)", tm, tm, a$time_order)
  # a BLQ record is -Inf in the listing's value (its cell prints blq_label);
  # a missing value or an absent time point is empty
  blq_value <- sprintf("%s = ifelse(%s, -Inf, %s)", a$value, a$blq, a$value)
  # the listing's records (a BLQ one -Inf: it prints `blq_label`) and the
  # summary's (the values)
  rec <- function(id, label, where, derive) Filter(Negate(is.null), list(
    id = id, label = label, from = a$data, subjects = subj,
    where = if (length(where)) tfl_r(.tpl_and_all(where)),
    derive = if (length(derive)) tfl_r(paste(derive, collapse = " | "))))
  sets <- list(
    list(id = subj, label = .tpl_set_label(pop, ctx$population), from = "ADSL",
         population = pop),
    rec(set, "the subjects' concentrations", a$where,
        c(order, if (length(a$blq)) blq_value)),
    rec(paste0(set, "_s"), "the concentrations summarised",
        c(a$where, sprintf("!is.na(%s)", a$value)), order))
  analyses <- list(list(id = "CONC", method = "continuous", data = paste0(set, "_s"),
                        by = c(a$by, tm), variables = a$value,
                        args = tfl_r(.tpl_pk_statistic)))
  summary <- list(
    tfl_item("normalize_ard", list()),
    tfl_item("table_plan", list(cols = tm, rows = list(
      block = tfl_r(sprintf("~ %s", q(a$summary_label)))))),
    tfl_item("plan_cells", list(continuous = as.list(a$stats))))
  heading <- a$heading %||% paste0("{", a$by, "}", collapse = "\n")
  # (one group: the page's value is `{value}`; its variable's name says it too)
  if (length(a$by) == 1L) heading <- gsub(paste0("{", a$by, "}"), "{value}", heading, fixed = TRUE)
  content <- c(
    list(tfl_item("table_plan", list(cols = tm, rows = list(
           subject = tfl_r(sprintf("~ %s", q(a$subject)))), value = a$value)),
         tfl_item("plan_cells", if (length(a$blq)) list(
           default = list(tfl_r(sprintf("value == -Inf ~ %s", q(a$blq_label))), a$cell),
           na = "") else list(default = a$cell)),
         tfl_item(.spec_part$fn, list(name = "summary", data = list(source = "ard"),
                                      content = summary))),
    if (length(a$times)) list(tfl_item("plan_levels", stats::setNames(list(a$times), tm))),
    # (the subjects' column row_label, as a study's tables have it)
    list(tfl_item("plan_stub", list(name = "row_label", before = TRUE)),
         tfl_item("plan_blanks", list(first = TRUE, last = TRUE, where = "between_parts")),
         tfl_item("plan_col_header", list(
           lines = list(list(row_label = "", .values = a$time_heading),
                        list(row_label = a$subject_heading, .values = "{col}")),
           span = tfl_r("list(c(.values = \"one\"), c(.values = \"each\"))"))),
         tfl_item("plan_paginate_rows", list(page_by = a$by, heading = heading,
                                             heading_at = "title")),
         tfl_item("plan_columns", list(decimal = ".values"))),
    if (length(a$split_at)) list(tfl_item("plan_paginate_cols", list(
      at = a$split_at, order = "down"))))
  list(ard = list(analysis_data = sets, analyses = analyses),
       data = list(source = "dataset", data = set), content = content)
}

.tpl_pk_register <- function() {
  text <- list(type = "text", multiple = FALSE)
  .tpl_extend(
    param_kinds = c(
      stats::setNames(rep(list(text), 7L), c("blq", "blq_label", "subject", "subject_heading",
                                             "time_heading", "summary_label", "split_at")),
      list(time_order = list(type = "vars", multiple = FALSE),
           times = list(type = "levels", multiple = TRUE, of = "time"),
           stats = list(type = "list", multiple = FALSE, fields = c("label", "template")))),
    examples = list(pk_conc = list(population = "SAF", blq = "BLQFL == \"Y\"")))
  .tfl_template("pk_conc", kind = "table", layout = "listing_summary",
                params = .tpl_pk_params(), body = .tpl_pk_body)
}

#' PK concentrations by subject with their summary: a table plan from the
#' template `pk_conc`
#'
#' One table, a page per group (`by`: the analyte `PARAM`, the arm
#' `TRT01A`): the subjects' concentrations above, one row a subject
#' (`{USUBJID}/{AGE}/{SEX}`), the time points across (`ATPT`, in the order
#' of `ATPTN`), and their summary statistics below over the same columns
#' (N, Mean, SD, SE, CV(%), Median, Min, Max).  A record below the limit of
#' quantification (`blq`, an R condition) prints `BLQ` in the listing; the
#' summary reads the non-missing values.  The summary is laid under the
#' listing by rtfreporter's `plan_append()`; `split_at` starts a second
#' block of columns at a time point.
#'
#' @param data The listing's records: the concentrations, one row a sample.
#' @param ard The summary's ARD: the statistics by the groups and the time
#'   point (the template's analysis, not normalized).
#' @param population The analysis set's id (`"PK"`).
#' @param ... The template's other parameters: `where`, `by`, `heading`,
#'   `time`, `time_order`, `times`, `value`, `blq`, `blq_label`, `cell`,
#'   `subject`, `subject_heading`, `time_heading`, `summary_label`, `stats`,
#'   `split_at` ...
#' @param codelists The report's code lists; `NULL` takes the data's.
#' @return A [rtfreporter::table_plan()].
#' @examples
#' x <- tfl_template_expand("pk_conc", list(population = "PK", blq = "BLQFL == \"Y\""))
#' vapply(x$content, `[[`, "", "fn")
#' @export
tpl_pk_conc <- function(data, ard, population, ..., codelists = NULL) {
  x <- tfl_template_expand("pk_conc", c(list(population = population), list(...)),
                           codelists = codelists)
  o <- tfl_output("tpl-pk_conc", kind = "table", extends = "none", data = x$data,
                  content = x$content)
  tfl_content_run(o, list(data = data, ard = ard))
}
