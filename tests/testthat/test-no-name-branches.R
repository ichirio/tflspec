# F12 (the foundation plan): no new branch on a function's or a template's
# name.  The fields come from the definitions (the schema, the arg defs,
# the series' templates); a branch by name is a rule left to move there.
# name-branches-allowed.csv lists the ones there are today, each with when
# it goes: a new one fails here, and one removed must be taken off the list.
# Comment lines are not code (roxygen examples name templates), and an
# escaped quote before a template's name is a message's example, not a
# branch.

nb_patterns <- c('identical(fn, "', 'fn %in% c("', 'startsWith(fn, "', "switch(fn,",
                 "identical(template", '"tflspec_v1/',
                 # (11-22 decision 5) an argument's name held in name / nm / arg
                 'identical(name, "', 'identical(nm, "', 'identical(arg, "',
                 'name %in% c("', 'nm %in% c("', 'arg %in% c("',
                 # a function's name as the literal, whatever the variable
                 # (fn's own forms are counted above)
                 'a "plan_/fig_/tfl_" literal')

# the forms matched as regular expressions (the others are fixed text)
nb_regex <- c(
  # (`identical(templates, ` is not a branch on a template's name)
  'identical(template' = 'identical[(]template[,) ]',
  'name %in% c("' = '(?<![A-Za-z0-9_.$])name %in% c[(]"',
  'nm %in% c("' = '(?<![A-Za-z0-9_.$])nm %in% c[(]"',
  'arg %in% c("' = '(?<![A-Za-z0-9_.$])arg %in% c[(]"',
  'a "plan_/fig_/tfl_" literal' =
    '(identical[(](?!fn,)[^,()]+, |(?<![A-Za-z0-9_.$])(?!fn )[A-Za-z0-9_.$]+ %in% c[(])"(plan|fig|tfl)_'
)

nb_count <- function(t, p) {
  if (p %in% names(nb_regex)) return(sum(gregexpr(nb_regex[[p]], t, perl = TRUE)[[1L]] > 0L))
  k <- sum(gregexpr(p, t, fixed = TRUE)[[1L]] > 0L)
  if (startsWith(p, '"')) k <- k - sum(gregexpr(paste0(intToUtf8(92L), p), t, fixed = TRUE)[[1L]] > 0L)
  k
}

test_that("the branches by name are the listed ones, no more and no fewer", {
  src <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(src), "the package's R/ sources are not here (an installed check)")
  allowed <- utils::read.csv(testthat::test_path("name-branches-allowed.csv"), stringsAsFactors = FALSE)
  expect_true(all(nzchar(allowed$remove_when)))
  expect_true(all(allowed$pattern %in% nb_patterns))
  now <- do.call(rbind, lapply(sort(list.files(src, "[.]R$")), function(f) {
    l <- readLines(file.path(src, f), warn = FALSE, encoding = "UTF-8")
    t <- paste(l[!grepl("^[[:space:]]*#", l)], collapse = "\n")
    n <- vapply(nb_patterns, function(p) nb_count(t, p), 0L)
    if (!any(n > 0L)) return(NULL)
    data.frame(file = f, pattern = nb_patterns[n > 0L], count = unname(n[n > 0L]), stringsAsFactors = FALSE)
  }))
  key <- function(d) paste(d$file, d$pattern, sep = " :: ")
  got <- stats::setNames(now$count, key(now))
  want <- stats::setNames(allowed$count, key(allowed))
  more <- names(got)[is.na(want[names(got)]) | got > want[names(got)]]
  fewer <- names(want)[is.na(got[names(want)]) | want > got[names(want)]]
  expect(length(more) == 0L, paste0("new branches by name (move the rule to the definitions): ",
                                    paste(more, collapse = "; ")))
  expect(length(fewer) == 0L, paste0("branches removed -- take them off name-branches-allowed.csv: ",
                                     paste(fewer, collapse = "; ")))
})

test_that("the analysis set is no study reference by name (A2-T): the template's set parameter is", {
  expect_false("population" %in% names(.tpl_study_by_name))
  # (which parameter it is comes from the template: choices "populations")
  expect_identical(.tpl_set_param("demog"), "population")
})
