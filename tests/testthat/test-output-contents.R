# One report, several objects (contents:, page: layout:): read and written
# back in their form; each object built as a report of its own; one RTF,
# the report's titles above the first object, its footnotes below the last

mo_items <- function() list(tfl_item("table_plan", list(cols = "TRT01A")))

test_that("contents: the short and the long form, read and written back as they were", {
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  o <- tfl_output("M-1", "table", content = list())
  o$contents <- list(
    a = list(content = mo_items(), short = TRUE),
    km = list(kind = "figure", data = list(steps = list(list(step = "read", dataset = "ADTTE"))),
              template = list(name = "km_survival", args = list(by = "TRT01A")), short = FALSE))
  o$page <- list(layout = list(list(object = "a"), list(object = "km", page_break = FALSE)))
  f <- file.path(d, "M-1.yml")
  tfl_write_output(o, f)
  y <- yaml::read_yaml(f)
  expect_null(y[["content"]])
  expect_true(is.null(names(y$contents$a)))
  expect_identical(y$contents$km$template$name, "km_survival")
  back <- tfl_read_output(f)
  expect_identical(names(back$contents), c("a", "km"))
  expect_identical(back$contents$a$content[[1L]]$fn, "table_plan")
  expect_identical(.object_kind(back$contents$a), "table")
  expect_identical(.object_kind(back$contents$km), "figure")
  objs <- .output_objects(back)
  expect_identical(names(objs), c("a", "km"))
  expect_identical(objs$km$template$name, "km_survival")
  expect_null(objs$a$contents)
  # both content: and contents:, or a layout that does not match: said
  writeLines(c(readLines(f), "content:", "  - table_plan: {cols: TRT01A}"), f)
  expect_error(tfl_read_output(f), "both content: and contents:")
  bad <- back
  bad$page$layout <- list(list(object = "a"), list(object = "zz"))
  expect_error(.output_objects(bad), "names zz, not in contents")
  bad$page$layout <- list(list(object = "a"))
  expect_error(.output_objects(bad), "contents km not in the layout")
})

test_that("two tables and a KM plot in one RTF: the titles and footnotes where the layout says", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("ggsurvfit")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "XYZ"))
  ans <- readLines(system.file("ai", "example", "onco-answer.md", package = "tflspec"),
                   encoding = "UTF-8")
  invisible(tfl_ai_unpack(ans, d))
  o <- tfl_read_output(file.path(d, "spec", "outputs", "T-14-1-1.yml"))
  m <- o
  m$id <- "M-1"
  m$content <- list()
  m$contents <- list(
    demog = list(content = o$content, short = TRUE),
    again = list(content = o$content, short = TRUE),
    km = list(kind = "figure",
              data = list(steps = list(list(step = "read", dataset = "ADTTE"),
                                       list(step = "param", value = "OS"))),
              template = list(name = "km_survival", args = list(by = "TRT01A")), short = FALSE))
  m$page$titles <- list(`1` = list(center = "Table M-1"), `2` = list(center = "OUTPUT TITLE"))
  m$page$footnotes <- list(`1` = list(left = "OUTPUT FOOT"))
  m$page$layout <- list(list(object = "demog"),
                        list(object = "again", space_before = 1L,
                             titles = list(`1` = list(left = "SECOND TITLE"))),
                        list(object = "km", page_break = TRUE, height_in = 3,
                             footnotes = list(`1` = list(left = "KM FOOT"))))
  tfl_write_output(m, file.path(d, "spec", "outputs", "M-1.yml"))
  st <- tfl_read_study_spec(d)
  built <- withr::with_dir(d, tfl_output_contents(tfl_read_outputs(d)[["M-1"]], st, "."))
  expect_identical(names(built), c("demog", "again", "km"))
  expect_s3_class(built$km, "ggplot")
  # the program: a call each, the report's titles with the first, its footnotes with the last
  code <- paste(tfl_report_code(tfl_read_outputs(d)[["M-1"]], study = st), collapse = "\n")
  expect_match(code, "rtf_tables[(][[:space:]]*demog")
  expect_match(code, "rtf_figures[(][[:space:]]*km")
  expect_false(grepl("rtf_titles(", code, fixed = TRUE))
  f <- tfl_run_output("M-1", d)
  expect_true(file.exists(f))
  x <- paste(readLines(f, warn = FALSE), collapse = " ")
  at <- vapply(c("OUTPUT TITLE", "SECOND TITLE", "KM FOOT", "OUTPUT FOOT"),
               function(k) regexpr(k, x, fixed = TRUE)[1L], 0L)
  expect_true(all(at > 0L))
  expect_false(is.unsorted(at))
  expect_match(x, "\\pict", fixed = TRUE)
})

test_that("a listing as an object: its records read and laid out, beside a table", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "XYZ"))
  m <- tfl_output("M-9", "table", label = "AEs and their listing", population = "SAF")
  m$ard <- list(analyses = list(list(id = "AE", method = "categorical", dataset = "ADAE",
                                     population = "SAF", by = list("TRT01A"),
                                     variables = list("AESEV"))))
  m$data <- list(source = "ard")
  m$content <- list()
  lst <- list(tfl_item("plan_listing", list(cols = list(
    tfl_call("listing_col", list(vars = list("USUBJID"), label = "Subject", width = 20)),
    tfl_call("listing_col", list(vars = list("AEDECOD"), label = "Preferred term", width = 30))))))
  m$contents <- list(
    summary = list(content = list(tfl_item("table_plan", list(cols = "TRT01A", rows = list(group = "variable")))),
                   short = TRUE),
    records = list(kind = "listing", content = lst, short = FALSE,
                   data = list(source = "dataset", dataset = "ADAE", population = "SAF",
                               where = tfl_r("AESEV == \"SEVERE\""), sort = list("USUBJID"))))
  m$page <- list(titles = list(`1` = list(center = "Table M-9")),
                 layout = list(list(object = "summary"), list(object = "records", space_before = 1L)))
  tfl_write_output(m, file.path(d, "spec", "outputs", "M-9.yml"))
  st <- tfl_read_study_spec(d)
  built <- withr::with_dir(d, tfl_output_contents(tfl_read_outputs(d)[["M-9"]], st, "."))
  expect_identical(names(built), c("summary", "records"))
  # (a listing's content as a table's: its plan, laid out when the RTF is written)
  expect_s3_class(built$records, class(built$summary)[1L])
  f <- tfl_run_output("M-9", d)
  x <- paste(readLines(f, warn = FALSE), collapse = " ")
  expect_match(x, "Preferred term", fixed = TRUE)
})
