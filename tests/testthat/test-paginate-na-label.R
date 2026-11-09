# plan_paginate_rows(na_label = ) (rtfreporter #937): the text a blank BY value
# prints as in a page's heading and name

test_that("plan_paginate_rows na_label: the schema accepts it, the code writes it", {
  it <- tfl_item("plan_paginate_rows",
                 list(page_by = "AESEV", heading = "Severity: {value}", na_label = "(Missing)"))
  expect_identical(nrow(tfl_schema_check(it)), 0L)
  o <- tfl_output("L-1", "listing", extends = "none",
                  content = list(tfl_item("plan_listing",
                                          list(cols = list(tfl_call("listing_col", list(vars = "USUBJID"))))),
                                 it))
  code <- paste(tfl_content_code(o), collapse = "\n")
  expect_match(code, 'na_label = "(Missing)"', fixed = TRUE)
  # the code runs where rtfreporter has the argument
  skip_if_not("na_label" %in% names(formals(rtfreporter::plan_paginate_rows)),
              "rtfreporter has no plan_paginate_rows(na_label = ) yet (rtfreporter #937)")
  expect_no_error(eval(parse(text = paste0("rtfreporter::table_plan(data.frame(USUBJID = 'a', AESEV = 'x')) |>",
    " rtfreporter::plan_paginate_rows(page_by = 'AESEV', na_label = '(Missing)')"))))
})

test_that("plan_paginate_rows na_label: YAML to sheets to YAML keeps it", {
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  d <- withr::local_tempdir()
  file.copy(list.files(src, full.names = TRUE), d, recursive = TRUE)
  s <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  o <- outs[["T-14-1-1"]]
  i <- which(vapply(o$content, function(it) identical(it$fn, "plan_paginate_rows"), NA))
  o$content[[i]]$args$page_by <- "TRT01A"
  o$content[[i]]$args$na_label <- "(Missing)"
  outs[["T-14-1-1"]] <- o
  sh <- tfl_sheets_from_spec(s, outs)
  expect_identical(sh$table$layout$pages_na_label[sh$table$layout$output_id %in% "T-14-1-1"], "(Missing)")
  to <- withr::local_tempdir()
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
  back <- tfl_read_outputs(to)[["T-14-1-1"]]
  j <- which(vapply(back$content, function(it) identical(it$fn, "plan_paginate_rows"), NA))
  expect_identical(back$content[[j]]$args$na_label, "(Missing)")
})

test_that("listing template by_na_label: passed to plan_paginate_rows, for 'page' only", {
  x <- tfl_template_expand("listing", list(
    data = "ADAE", columns = list(list(vars = "USUBJID", label = "Subject")),
    by = "AESEV", by_na_label = "(Missing)"))
  a <- Filter(function(it) identical(it$fn, "plan_paginate_rows"), x$content)[[1L]]$args
  expect_identical(a$na_label, "(Missing)")
  expect_identical(a$page_by, "AESEV")
  expect_identical(tfl_template_params("listing")$by_na_label$type, "text")
  cols <- list(list(vars = "USUBJID", label = "Subject"))
  expect_error(tfl_template_expand("listing", list(data = "ADAE", columns = cols, by = "AESEV",
                                                   by_break = "block", by_na_label = "x")),
               "by_na_label")
  expect_error(tfl_template_expand("listing", list(data = "ADAE", columns = cols, by_na_label = "x")),
               "about `by`")
  expect_error(tfl_template_expand("listing", list(data = "ADAE", columns = cols, by = "AESEV",
                                                   by_na_label = c("a", "b"))), "one text")
})

test_that("listing template by_na_label: a blank AESEV builds a page named with it", {
  skip_on_cran()
  skip_if_not_installed("rtfreporter")
  skip_if_not("na_label" %in% names(formals(rtfreporter::plan_paginate_rows)),
              "rtfreporter has no plan_paginate_rows(na_label = ) yet (rtfreporter #937)")
  dat <- data.frame(USUBJID = sprintf("S-%02d", 1:6), AEDECOD = sprintf("Event %02d", 1:6),
                    AESEV = c("MILD", "MILD", "", "SEVERE", "", "SEVERE"), stringsAsFactors = FALSE)
  pages <- function(...) {
    p <- rtfreporter::plan_apply(suppressMessages(tpl_listing(
      dat, data = "ADAE", columns = list(list(vars = "USUBJID", label = "Subject"),
                                         list(vars = "AEDECOD", label = "Event")),
      by = "AESEV", ...)))
    names(p)
  }
  expect_true(any(grepl("(Missing)", pages(by_na_label = "(Missing)"), fixed = TRUE)))
  expect_false(any(grepl("(Missing)", pages(), fixed = TRUE)))
})
