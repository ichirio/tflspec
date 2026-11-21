# F12 (the foundation plan): no new branch on a function's or a template's
# name.  The fields come from the definitions (the schema, the arg defs,
# the series' templates); a branch by name is a rule left to move there.
# name-branches-allowed.csv lists the ones there are today, each with when
# it goes: a new one fails here, and one removed must be taken off the list.

nb_patterns <- c('identical(fn, "', 'fn %in% c("', 'startsWith(fn, "', "switch(fn,",
                 "identical(template", "tflspec_v1/")

test_that("the branches by name are the listed ones, no more and no fewer", {
  src <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(src), "the package's R/ sources are not here (an installed check)")
  allowed <- utils::read.csv(testthat::test_path("name-branches-allowed.csv"), stringsAsFactors = FALSE)
  expect_true(all(nzchar(allowed$remove_when)))
  expect_true(all(allowed$pattern %in% nb_patterns))
  now <- do.call(rbind, lapply(sort(list.files(src, "[.]R$")), function(f) {
    t <- paste(readLines(file.path(src, f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    n <- vapply(nb_patterns, function(p) {
      m <- gregexpr(p, t, fixed = TRUE)[[1L]]
      sum(m > 0L)
    }, 0L)
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
