# The review's rules, one case each: one change to the clean study of
# helper-review.R (`edit`) and the row the review must give for it
# (`rule`, `output_id`, `sheet`, `row`, `field`).  test-review.R fails for a
# rule of tfl_review_rules() run by tflspec that no case here expects.
# `facts`: arguments of tfl_data_facts(); `ard`: the reports' ARD facts.
rc <- function(rule, edit, output_id, sheet, row, field, level = NULL,
               facts = list(), ard = NULL, data = TRUE) {
  list(rule = rule, edit = edit, output_id = output_id, sheet = sheet,
       row = row, field = field, level = level, facts = facts, ard = ard,
       data = data)
}
# ARD facts of T-1 as the clean study's ARD would have them
ard_t1 <- function(stats = c("N", "mean", "sd", "n", "p"), groups = list(TRT01A = c("Drug", "Placebo")),
                   conditions = NULL) {
  list(`T-1` = list(groups = groups,
                    variables = list(AGE = list(levels = character(), stats = c("N", "mean", "sd"), contexts = "continuous"),
                                     SEX = list(levels = c("F", "M"), stats = c("n", "p"), contexts = "categorical")),
                    stats = stats, conditions = conditions))
}

list(
  rc("S01", function(s) { s$spec$tables$stats <- c("bogus", NA); s },
     "T-1", "tables", "", "stats", data = FALSE),
  rc("S01", function(s) { s$ard$analyses$method[2] <- "not a method!"; s },
     "T-1", "analyses", "AGE", "method", data = FALSE),
  rc("S01", function(s) { s$listings$listing_cols$align <- c("middle", NA); s },
     "L-1", "listing_cols", "1", "align", data = FALSE),
  rc("C01", function(s) {
       s$spec$codelists <- rbind(s$spec$codelists,
         data.frame(output_id = "T-1", variable = "RACE", value = "WHITE", order = 1))
       s
     }, "T-1", "codelists", "RACE", "variable", data = FALSE),
  rc("C02", function(s) {
       s$spec$codelists <- rbind(s$spec$codelists,
         data.frame(output_id = "T-1", variable = "SEX", value = "U", order = 3))
       s
     }, "T-1", "codelists", "SEX / U", "value"),
  rc("C03", function(s) { s$data$ADSL$SEX[1] <- "U"; s },
     "T-1", "codelists", "SEX", "value", level = "error"),
  rc("C05", function(s) s, "T-1", "codelists", "SEX", "value",
     facts = list(max_levels = 1L)),
  rc("A01", function(s) { s$ard$analyses$by[2] <- "TRT01X"; s },
     "T-1", "analyses", "AGE", "by", level = "error"),
  rc("A02", function(s) { s$ard$populations$where <- 'SAFFX == "Y"'; s },
     NA, "populations", "SAF", "where", level = "error"),
  rc("A03", function(s) { s$ard$analysis_data$where[3] <- 'TRTEMFL == "Z"'; s },
     "T-2", "analyses", "TEAE", "data"),
  rc("A04", function(s) { s$data$ADSL$SAFFL[7] <- "No"; s },
     NA, "populations", "SAF", "where"),
  rc("A05", function(s) { s$ard$analyses$variables[2] <- "SEX"; s },
     "T-1", "analyses", "AGE", "method"),
  rc("A06", function(s) { s$ard$analyses$variables[3] <- "SEX | TRT01A"; s },
     "T-1", "analyses", "SEX", "by", data = FALSE),
  rc("A08", function(s) { s$ard$analyses$statistics[3] <- "n | mean"; s },
     "T-1", "analyses", "SEX", "statistics", data = FALSE),
  rc("A15", function(s) { s$ard$analyses$by[2] <- "context"; s },
     "T-1", "analyses", "AGE", "by", level = "error", data = FALSE),
  rc("A16", function(s) { s$ard$populations$derive <- "SEX = tolower(SEX)"; s },
     NA, "populations", "SAF", "derive"),
  rc("A17", function(s) s, "T-1", "analyses", "AGE", "", level = "error",
     ard = ard_t1(conditions = data.frame(
       output_id = "T-1", analysis_id = "AGE", variable = "AGE", groups = "",
       level = "error", message = "no data", statistics = "mean")),
     data = FALSE),
  rc("T01", function(s) { s$spec$tables$cols[1] <- "TRT01P"; s },
     "T-1", "tables", "", "cols", data = FALSE),
  rc("T02", function(s) {
       s$spec$variables <- rbind(s$spec$variables,
         data.frame(output_id = "T-1", variable = "BMI", label = "BMI"))
       s
     }, "T-1", "variables", "BMI", "variable", data = FALSE),
  rc("T03", function(s) {
       s$spec$cells <- rbind(s$spec$cells,
         data.frame(output_id = "T-1", variable = "SEX", row = "Mean", template = "{mean}"))
       s
     }, "T-1", "cells", "SEX /  / Mean", "template", data = FALSE),
  rc("T04", function(s) {
       s$spec$cells <- rbind(s$spec$cells,
         data.frame(output_id = "T-1", variable = "WEIGHT", row = "n", template = "{N}"))
       s
     }, "T-1", "cells", "WEIGHT /  / n", "variable", data = FALSE),
  rc("T05", function(s) {
       s$spec$digits <- rbind(s$spec$digits,
         data.frame(output_id = "T-1", variable = NA, statistic = "median", digits = 1))
       s
     }, "T-1", "digits", " / median", "statistic", data = FALSE),
  rc("T06", function(s) s, "T-1", "cells", "continuous /  / Mean (SD)", "template",
     ard = ard_t1(stats = c("N", "mean", "n", "p")), data = FALSE),
  rc("T07", function(s) s, "T-1", "tables", "", "cols",
     ard = ard_t1(groups = list(ARM = c("A", "B"))), data = FALSE),
  rc("T08", function(s) {
       s$spec$variables$levels <- c(NA, "F | M | U")
       s
     }, "T-1", "variables", "SEX", "levels", ard = ard_t1(), data = FALSE),
  rc("T09", function(s) { s$spec$tables$cols[2] <- NA; s },
     "T-2", "tables", "", "cols", level = "hand", data = FALSE),
  rc("T10", function(s) { s$spec$cells <- s$spec$cells[0, ]; s },
     "T-2", "cells", "", "", level = "hand", data = FALSE),
  rc("L01", function(s) { s$listings$listing_cols <- s$listings$listing_cols[0, ]; s },
     "L-1", "listings", "", "", level = "hand", data = FALSE),
  rc("L02", function(s) { s$listings$listing_cols$vars[2] <- "AETERM"; s },
     "L-1", "listing_cols", "2", "vars", level = "error"),
  rc("F01", function(s) {
       s$figures <- list(`F-1` = tfl_fig_design(
         data = list(list(step = "read", dataset = "ADSL"),
                     list(step = "derive", variable = "X"))))
       s
     }, "F-1", "design", "data[2] derive", "expr", level = "hand", data = FALSE),
  rc("F02", function(s) {
       s$figures <- list(`F-1` = tfl_fig_template("km_simple"))
       s
     }, "F-1", "design", "layers", "", data = FALSE),
  rc("F03", function(s) {
       s$figures <- list(`F-1` = tfl_fig_design(data = list(list(step = "read", dataset = "ADXX"))))
       s
     }, "F-1", "design", "data[1] read", "dataset", level = "error")
)
