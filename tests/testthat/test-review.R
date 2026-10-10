# tfl_review_spec(): the rules of the catalog, each with a case that makes
# it fire (fixtures/review-cases.R), on a study it finds nothing wrong with
# (helper-review.R).

.rv_cases <- function() source(test_path("fixtures", "review-cases.R"))$value

.rv_case_review <- function(cs) {
  s <- cs$edit(.rv_study())
  facts <- if (cs$data) do.call(tfl_data_facts, c(list(s$data, populations = s$ard,
    listings = tfl_listing_spec(s$listings, check = FALSE)), cs$facts))
  if (!is.null(cs$ard)) {
    if (is.null(facts)) facts <- list()
    facts$ard <- cs$ard
  }
  tfl_review_spec(s$spec, s$ard, s$listings, s$figures, facts = facts)
}

test_that("the clean study has nothing to review, with and without its data", {
  s <- .rv_study()
  expect_identical(nrow(.rv_review(s, data = FALSE)), 0L)
  expect_identical(nrow(.rv_review(s)), 0L)
})

test_that("every rule tflspec runs has a case, and every case gives its row", {
  cat <- tfl_review_rules()
  mine <- setdiff(cat$rule[cat$checked_by == "tflspec"], "review")
  cases <- .rv_cases()
  expect_identical(setdiff(mine, vapply(cases, `[[`, "", "rule")), character(0),
                   info = "rules of the catalog no case makes fire")
  for (cs in cases) {
    r <- .rv_case_review(cs)
    hit <- r[r$rule == cs$rule & r$sheet == cs$sheet & r$row == cs$row &
               r$field == cs$field &
               ((is.na(cs$output_id) & is.na(r$output_id)) |
                  (!is.na(r$output_id) & r$output_id %in% cs$output_id)), , drop = FALSE]
    expect_true(nrow(hit) >= 1L, info = paste(cs$rule, cs$sheet, cs$row, cs$field,
                                               "--", paste(r$rule, r$sheet, r$row, r$field,
                                                           collapse = "; ")))
    if (!is.null(cs$level) && nrow(hit)) {
      expect_identical(hit$level[1L], cs$level, info = cs$rule)
    }
    expect_true(all(r$rule %in% c(cat$rule)), info = cs$rule)
  }
})

test_that("the catalog is whole: levels, areas, a message for each rule", {
  cat <- tfl_review_rules()
  expect_named(cat, c("rule", "level", "area", "needs", "checked_by", "message", "hint"))
  expect_false(anyDuplicated(cat$rule) > 0L)
  expect_true(all(cat$level %in% c("error", "check", "hand")))
  expect_true(all(cat$needs %in% c("spec", "catalog", "ard", "data")))
  expect_true(all(cat$checked_by %in% c("tflspec", "tflplanner")))
  expect_true(all(nzchar(cat$message)))
  # C03: the programs stop on a data value the code list does not have
  expect_identical(cat$level[cat$rule == "C03"], "error")
})

test_that("no rule that needs the data fires without them", {
  cat <- tfl_review_rules()
  data_rules <- cat$rule[cat$needs %in% c("data", "ard")]
  for (cs in .rv_cases()) {
    cs$data <- FALSE
    cs$ard <- NULL
    r <- .rv_case_review(cs)
    expect_false(any(r$rule %in% data_rules), info = cs$rule)
  }
})

test_that("the rows without facts are among those with them", {
  for (cs in .rv_cases()) {
    if (!cs$data) next
    with <- .rv_case_review(cs)
    cs$data <- FALSE
    without <- .rv_case_review(cs)
    k <- function(r) paste(r$rule, r$output_id, r$sheet, r$row, r$field)
    expect_true(all(k(without) %in% k(with)), info = cs$rule)
  }
})

test_that("each row's message is its template filled with its values", {
  cat <- tfl_review_rules()
  tp <- tfl_review_templates()
  expect_named(tp, c("rule", "name", "template"))
  expect_true(all(tp$rule %in% cat$rule))
  expect_false(anyDuplicated(tp$template) > 0L)
  # the rules whose message is the whole sentence, with words of their own
  own <- c("T06", "T07", "L01", "L02", "F02", "F03")
  for (cs in .rv_cases()) {
    r <- .rv_case_review(cs)
    for (i in seq_len(nrow(r))) {
      expect_identical(r$message[i], do.call(sprintf, c(list(r$template[i]), as.list(r$args[[i]]))),
                       info = paste(r$rule[i], r$message[i]))
      if (r$rule[i] %in% own) {
        expect_false(identical(r$template[i], "%s"), info = paste(r$rule[i], r$message[i]))
        if (r$rule[i] != "F02") {
          expect_true(r$template[i] %in% tp$template[tp$rule == r$rule[i]],
                      info = paste(r$rule[i], r$message[i]))
        }
      } else if (r$rule[i] %in% cat$rule) {
        expect_identical(r$template[i], cat$message[cat$rule == r$rule[i]], info = r$rule[i])
      }
    }
  }
  # the ones the cases do not make: a column without vars, cells for a
  # variable the ARD does not analyse, a variable the design's data lack
  s <- .rv_study()
  s$listings$listing_cols$vars[2] <- NA
  r <- tfl_review_spec(s$spec, s$ard, s$listings)
  expect_identical(r$template[r$rule == "L01"], "listing %s, column %s: no `vars`")
  expect_identical(r$args[r$rule == "L01"], list(c("L-1", "2")))
  s <- .rv_study()
  s$spec$cells <- rbind(s$spec$cells, data.frame(output_id = "T-1", variable = "WEIGHT",
                                                 row = "n", template = "{N}"))
  f <- list(`T-1` = list(groups = list(TRT01A = c("Drug", "Placebo")),
                         variables = list(AGE = list(levels = character(),
                                                     stats = c("N", "mean", "sd"),
                                                     contexts = "continuous")),
                         stats = c("N", "mean", "sd")))
  r <- tfl_review_spec(s$spec, s$ard, facts = list(ard = f))
  t7 <- r[r$rule == "T07", ]
  expect_true("the cells are written for %s, which the ARD does not analyse" %in% t7$template)
  # tfl_check_ard() says the same sentences
  expect_identical(r$message[r$rule == "T07"][1L],
                   sprintf(t7$template[1L], t7$args[[1L]]))
  s$figures <- list(`F-1` = tfl_fig_design(data = list(list(step = "read", dataset = "ADSL"),
                                                       list(step = "flag", variable = "NOPE"))))
  facts <- tfl_data_facts(s$data, populations = s$ard)
  r <- tfl_review_spec(s$spec, s$ard, figures = s$figures, facts = facts)
  f3 <- r[r$rule == "F03", ]
  expect_identical(f3$template, "%s %s no variable %s in %s")
  expect_identical(f3$args[[1L]], c("data[2] flag", "variable", "NOPE", "df"))
})

test_that("the shape: columns, sort, summary, print, narrowing", {
  s <- .rv_study()
  s$spec$tables$cols[2] <- NA                   # T09, hand
  s$spec$tables$stats <- c("bogus", NA)         # S01, error
  s$ard$analyses$variables[3] <- "SEX | TRT01A" # A06, check
  r <- tfl_review_spec(s$spec, s$ard, s$listings)
  expect_s3_class(r, "tfl_review")
  expect_named(r, c("output_id", "level", "area", "sheet", "row", "field",
                    "message", "template", "hint", "rule", "draft", "args", "fix"))
  expect_identical(r$level, c("error", "check", "hand"))
  expect_false(any(r$draft))
  sm <- summary(r)
  expect_identical(sm$output_id, c("T-1", "T-2"))
  expect_identical(sm$error + sm$check + sm$hand, c(2L, 1L))
  expect_output(print(r), "1 error, 1 to check, 1 to set by hand")
  expect_identical(tfl_review_spec(s$spec, s$ard, output_id = "T-2")$rule, "T09")
  expect_identical(tfl_review_spec(s$spec, s$ard, rules = "A06")$rule, "A06")
  expect_identical(tfl_review_spec(s$spec, s$ard, rules = "table")$rule,
                   c("S01", "T09"))
})

test_that("what cannot be read is one row, and a failing rule never loses the review", {
  r <- tfl_review_spec(spec = "not sheets", ard = 1)
  expect_identical(r$rule, c("S01", "S01"))
  expect_identical(r$area, c("spec", "spec"))
  local_mocked_bindings(.rule_a06 = function(x) stop("boom"))
  s <- .rv_study()
  r <- tfl_review_spec(s$spec, s$ard)
  expect_identical(r$rule, "review")
  expect_match(r$message, "A06 could not be run: boom")
})

test_that("an invalid ARD definition is reviewed as its sheets", {
  s <- .rv_study()
  s$ard$analyses$analysis_id[2] <- NA
  s$ard$populations$population_id <- NA
  expect_error(tfl_ard_spec(s$ard), "not valid")
  r <- tfl_review_spec(ard = s$ard)
  expect_true(all(r$rule == "S01"))
  expect_true(nrow(r) >= 2L)
})

test_that("the constructors' conditions carry the problems as rows", {
  s <- .rv_study()
  s$ard$analyses$method[2] <- "not a method!"
  s$ard$analyses$population_id <- c(NA, NA, "FAS", NA, NA)
  s$ard$analyses$data[3] <- NA
  e <- tryCatch(tfl_ard_spec(s$ard), error = function(e) e)
  expect_s3_class(e, "tflspec_spec_error")
  expect_identical(e$problems$row, c("AGE", "SEX"))
  expect_identical(e$problems$field, c("method", "population_id"))
  expect_identical(conditionMessage(e), paste(c("The ARD definition is not valid:",
                                                e$problems$message), collapse = "\n  "))
  # the table definition stops on its first, as it did
  sp <- s$spec
  sp$cells$template[1] <- NA
  sp$digits$digits[2] <- "x"
  e <- tryCatch(tfl_table_spec(sp), error = function(e) e)
  expect_s3_class(e, "tflspec_table_spec_error")
  expect_identical(conditionMessage(e), e$problems$message[1L])
  expect_identical(e$problems$sheet, c("digits", "cells"))
  expect_identical(e$problems$row, c(" / sd", "continuous /  / n"))
  # the listings, all of them
  l <- s$listings
  l$listing_cols$align <- c("middle", "top")
  e <- tryCatch(tfl_listing_spec(l), error = function(e) e)
  expect_s3_class(e, "tflspec_listing_spec_error")
  expect_identical(e$problems$row, c("1", "2"))
})

test_that("a figure's dataset the catalog has but the facts do not is not missing", {
  s <- .rv_study()
  s$figures <- list(`F-1` = tfl_fig_design(data = list(list(step = "read", dataset = "ADVS"))))
  facts <- tfl_data_facts(s$data, populations = s$ard)
  # ADVS is in no catalog: missing
  r <- tfl_review_spec(s$spec, s$ard, figures = s$figures, facts = facts)
  expect_true(any(r$rule == "F03"))
  # in the catalog, its facts not made yet: not said missing
  s$ard$datasets <- rbind(s$ard$datasets,
                          data.frame(dataset = "ADVS", level = "ADaM",
                                     path = "advs.rds", derive = NA))
  r <- tfl_review_spec(s$spec, s$ard, figures = s$figures, facts = facts)
  expect_false(any(r$rule == "F03"))
})

test_that("the memos give what the work gives", {
  expect_identical(.split_bar("AGE | SEX "), c("AGE", "SEX"))
  expect_identical(.split_bar("AGE | SEX "), c("AGE", "SEX"))
  expect_identical(.split_bar(""), character())
  expect_identical(.split_bar("  "), character())
  expect_identical(.split_bar(NA), character())
  expect_identical(.fig_style_builtin(), .fig_style_builtin_make())
  # the user's style still over tflspec's own
  b <- .fig_style_builtin_make()
  own <- b
  own$colors$colour[1] <- "#123456"
  withr::local_options(tflspec.fig_style = own)
  expect_identical(tfl_fig_style()$colors$colour[1], "#123456")
})

test_that("A01: columns an analysis data adds are the population's data's, not its own dataset's (#293 P6)", {
  s <- .rv_study()
  # the AEs with SEX added from ADSL by the subject: no column missing
  s$ard$analysis_data$add[3] <- "SEX"
  r <- .rv_review(s)
  expect_false(any(r$rule == "A01" & grepl("SEX", r$message, fixed = TRUE)))
  # a column the population's data has not got is still found
  s$ard$analysis_data$add[3] <- "NOPE"
  r <- .rv_review(s)
  expect_true(any(r$rule == "A01" & grepl("NOPE", r$message, fixed = TRUE)))
})
