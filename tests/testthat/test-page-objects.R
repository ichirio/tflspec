# One report, several objects (canon 11-28, tflspec#1025): the content makes
# named objects, `page: layout:` places them by name; `page_break` not
# written is a new page.  The old `contents:` stops with the new form.  One
# placement rule for the run (tfl_content_run(), tfl_report(),
# tfl_run_output()) and the program (tfl_content_code(), tfl_report_code(),
# tfl_report_program()).

po_cols <- function() list(tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable"))),
                           tfl_item("plan_cells", list(categorical = "{n} ({p:.1f%})")))
po_card <- function(name, from = NULL, items = po_cols()) tfl_plot(name, items, input = from, form = "map")
po_out <- function(..., layout = NULL, kind = "table") {
  o <- tfl_output("M-1", kind, content = list(...))
  if (!is.null(layout)) o$page <- list(layout = layout)
  o
}
po_read <- function(txt) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  writeLines(txt, f)
  tfl_read_output(f)
}
po_ard <- function() {
  d <- data.frame(USUBJID = sprintf("S%02d", 1:12), ARM = rep(c("Placebo", "Drug"), each = 6),
                  SEX = rep(c("F", "M", "F"), 4), stringsAsFactors = FALSE)
  cards::ard_stack(d, .by = ARM, cards::ard_tabulate(variables = SEX))
}
po_pages <- function(p) suppressMessages(rtfreporter::plan_apply(p, "table"))

test_that("contents: is an old form: reading it stops with the form to write instead", {
  e <- expect_error(po_read(c("id: M-1", "kind: table", "contents:",
                              "  demog: [{table_plan: {cols: TRT01A}}]")),
                    class = "tflspec_old_form")
  expect_identical(e$form, "contents")
  expect_match(conditionMessage(e), "page: layout:", fixed = TRUE)
  expect_match(conditionMessage(e), "{object: ae_list, page_break: false}", fixed = TRUE)
  expect_true(any(grepl("contents:", tflspec:::.old_form_lines(), fixed = TRUE)))
})

test_that("a layout is read and written back; its rows are checked before anything runs", {
  o <- po_out(po_card("demog", "ard"), po_card("again", "ard"),
              layout = list(list(object = "demog"), list(object = "again", page_break = FALSE,
                                                        space_before = 1L)))
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  tfl_write_output(o, f)
  back <- tfl_read_output(f)
  expect_identical(back$content, o$content)
  expect_identical(back$page$layout, o$page$layout)
  expect_identical(names(.page_layout_rows(back)), c("demog", "again"))
  expect_identical(.placed_objects(back), c("demog", "again"))
  # no layout: the last object (one object: the fixed name, plan)
  expect_identical(.placed_objects(po_out(po_card("h", "ard"), po_card("plan", "h"))), "plan")
  expect_identical(.placed_objects(tfl_output("T-1", "table", content = po_cols())), "plan")
  # the stops, each of class tfl_page_layout
  lay <- function(...) {
    x <- o
    x$page$layout <- list(...)
    .page_layout_rows(x)
  }
  expect_error(lay(list(object = "demog"), list(object = "zz")),
               "places zz, which the content does not make \\(it makes demog, again\\)",
               class = "tfl_page_layout")
  expect_error(lay(list(object = "demog"), list(object = "demog")), "places demog twice",
               class = "tfl_page_layout")
  expect_error(lay(list(object = "demog"), list(object = "again", first_rows = 5L)),
               "starts a fresh page: write `page_break: false`", class = "tfl_page_layout")
  expect_error(lay(list(object = "demog", page_break = FALSE, first_rows = 5L)),
               "the first object", class = "tfl_page_layout")
  expect_error(lay(list(page_break = FALSE)), "row 1 names no object", class = "tfl_page_layout")
  expect_error(lay(list(object = "demog", pagebreak = FALSE)), "`pagebreak` is not a layout key",
               class = "tfl_page_layout")
  # first_rows on a row that goes on under the one before: fine
  expect_identical(names(lay(list(object = "demog"),
                             list(object = "again", page_break = FALSE, first_rows = 5L))),
                   c("demog", "again"))
  # a figure report's layout: phase 2
  fo <- tfl_output("F-1", "figure", content = list())
  fo$page <- list(layout = list(list(object = "km")))
  expect_error(.page_layout_rows(fo), "phase 2", class = "tfl_page_layout_phase2")
})

test_that("code: each placed object made by its name, placed in order; no page_break is a new page", {
  o <- po_out(po_card("ard2", "ard", list(tfl_item("normalize_ard", list()))),
              po_card("demog", "ard2"), po_card("again", "ard2"), po_card("third", "ard2"),
              layout = list(list(object = "demog"),
                            list(object = "again", page_break = FALSE, space_before = 1L,
                                 titles = list(`1` = list(left = "SECOND"))),
                            list(object = "third")))
  code <- no_bign(suppressMessages(tfl_content_code(o, pipe = "|>")))
  expect_identical(code[1:2], c("ard2 <- ard |>", "  normalize_ard()"))
  expect_true(all(c("demog <- ard2 |>", "again <- ard2 |>", "third <- ard2 |>") %in% code))
  # the document: a call each, by name; the second on under the first, the
  # third (no page_break written) on a new page, as rtfreporter's default
  st <- .page_report_steps(o, NULL, "plan")
  tb <- Filter(function(s) identical(s$fun, "rtf_tables"), st)
  expect_identical(vapply(tb, function(s) s$args[[2L]]$name, ""), c("demog", "again", "third"))
  expect_identical(lapply(tb, function(s) s$args$page_break), list(NULL, FALSE, NULL))
  expect_identical(tb[[2L]]$args$space_before, 1L)
  rc <- paste(tfl_report_code(o, content = "plan"), collapse = "\n")
  expect_match(rc, "rtf_tables(\n    again,", fixed = TRUE)
  expect_false(grepl("rtf_titles(", rc, fixed = TRUE))
  # with a layout the last object needs no fixed name; without one it is
  # the plan the program reads
  nolay <- o
  nolay$page <- list()
  expect_error(tfl_content_code(nolay), "name it `plan`, or place it by its name")
  expect_error(tfl_content_code(po_out(po_card("a"), po_card("a"), layout = list(list(object = "a")))),
               "makes a twice")
})

test_that("run: the objects made as the program makes them; the layout's checks on what they are", {
  skip_if_not_installed("cards")
  ard <- po_ard()
  o <- po_out(po_card("ard2", "ard", list(tfl_item("normalize_ard", list()))),
              po_card("demog", "ard2"), po_card("again", "ard2"),
              layout = list(list(object = "demog"), list(object = "again", page_break = FALSE)))
  objs <- tfl_content_run(o, ard)
  expect_identical(names(objs), c("ard2", "demog", "again"))
  expect_s3_class(objs$demog, "table_plan")
  # the program's statements give the same plans
  env <- new.env()
  assign("ard", ard, envir = env)
  suppressMessages(eval(parse(text = c("library(rtfreporter)", tfl_content_code(o, pipe = "|>"))), env))
  expect_identical(po_pages(env$again), po_pages(objs$again))
  expect_identical(po_pages(env$demog), po_pages(objs$demog))
  # the document of both, one RTF
  doc <- tfl_report(o, content = objs)
  expect_s3_class(doc, "rtf_document")
  # a data frame or rtftable pages are tables too; a plot is phase 2; a
  # number cannot be told; a name not made stops
  ok <- objs
  ok$again <- data.frame(a = 1)
  expect_s3_class(tfl_report(o, content = ok), "rtf_document")
  skip_if_not_installed("ggplot2")
  bad <- objs
  bad$again <- ggplot2::ggplot()
  expect_error(tfl_report(o, content = bad), "again is a figure.*phase 2",
               class = "tfl_page_layout_phase2")
  bad$again <- 42
  expect_error(tfl_report(o, content = bad), "cannot be told", class = "tfl_page_layout_kind")
  expect_error(tfl_report(o, content = objs["demog"]), "places again, which the content does not make",
               class = "tfl_page_layout")
  expect_error(tfl_report(o, content = objs$demog), "not its objects by name", class = "tfl_page_layout")
  # made, not placed, not read by another object: a table is named, a
  # helper data frame is not
  more <- c(objs, list(spare = objs$demog, df = data.frame(x = 1)))
  expect_identical(.layout_unplaced(o, more), "spare")
  # one object, a flat content: placed by its fixed name, `plan`
  flat <- tfl_output("T-1", "table", content = c(list(tfl_item("normalize_ard", list())), po_cols()))
  flat$page <- list(layout = list(list(object = "plan", space_before = 1L)))
  p <- tfl_content_run(flat, ard)
  expect_s3_class(tfl_report(flat, content = p), "rtf_document")
  expect_match(paste(tfl_report_code(flat, content = "plan"), collapse = "\n"), "rtf_tables(plan)",
               fixed = TRUE)
})

test_that("own content code that leaves a named list: the layout places its objects", {
  skip_if_not_installed("cards")
  code <- paste(
    "a <- rtfreporter::normalize_ard(ard)",
    "content <- list(demog = rtfreporter::table_plan(a, cols = \"ARM\"),",
    "                again = rtfreporter::table_plan(a, cols = \"ARM\"))", sep = "\n")
  o <- tfl_output("M-2", "table", content = tfl_r(code))
  o$page <- list(layout = list(list(object = "demog"), list(object = "again", page_break = FALSE)))
  objs <- suppressMessages(tfl_content_run(o, list(ard = po_ard())))
  expect_identical(names(objs), c("demog", "again"))
  expect_s3_class(tfl_report(o, content = objs), "rtf_document")
  # (names the code makes are known when it runs: a name it does not make stops then)
  o$page$layout[[2L]]$object <- "zz"
  expect_error(suppressMessages(tfl_content_run(o, list(ard = po_ard()))),
               "places zz, which the content does not make \\(it makes demog, again\\)",
               class = "tfl_page_layout")
  # with no layout: the content contract, as before (a table_plan)
  o$page <- list()
  expect_error(suppressMessages(tfl_content_run(o, list(ard = po_ard()))),
               class = "tfl_content_contract")
})

test_that("tfl_bign_drop() keeps the rows it takes out aside: a second table from the ARD reads them", {
  x <- data.frame(analysis_id = c("BIGN", "BIGN", "SEX"), stat_name = "n",
                  variable_level = I(list("Drug", "Placebo", "F")), stat = I(list(6, 6, 4)))
  dropped <- tfl_bign_drop(x)
  expect_identical(nrow(dropped), 1L)
  expect_identical(tfl_bign_values(dropped), tfl_bign_values(x))
  expect_identical(tfl_bign_values(tfl_bign_drop(dropped)), tfl_bign_values(x))
  expect_null(tfl_bign_values(x[3L, ]))
  # (an ARD with no BIGN row: as it was)
  expect_identical(tfl_bign_drop(x[3L, ]), x[3L, ])
})

# ---- a study: one RTF from the study's files, as the program makes it -----

po_study <- function(xs = c("adsl", "adae")) {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  d <- tempfile("page-objects-")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in xs) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  invisible(tfl_ai_study_init(d, adam = "adam", study_id = "XYZ"))
  d
}

# the RTF's text, and its page breaks (a new page: a section break)
po_rtf <- function(f) paste(readLines(f, warn = FALSE), collapse = " ")
po_breaks <- function(x) lengths(regmatches(x, gregexpr("\\\\(sect|page)[^a-z]", x)))

test_that("two tables in one RTF: titles and footnotes where the layout says, the pages as before", {
  d <- po_study(c("adsl", "adae", "adrs", "adlb", "adtte"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  ans <- readLines(system.file("ai", "example", "onco-answer.md", package = "tflspec"),
                   encoding = "UTF-8")
  invisible(tfl_ai_unpack(ans, d))
  o <- tfl_read_output(file.path(d, "spec", "outputs", "T-14-1-1.yml"))
  m <- o
  m$id <- "M-1"
  # (was: contents: {demog: [...], again: [...], third: [...]}, the second
  # on under the first -- the old default -- now written `page_break:
  # false`; the third, `page_break: true` then, now the default; the same
  # RTF as next's contents: made, compared when this was written)
  m$content <- list(po_card("demog", items = o$content), po_card("again", items = o$content),
                    po_card("third", items = o$content))
  m$page$titles <- list(`1` = list(center = "Table M-1"), `2` = list(center = "OUTPUT TITLE"))
  m$page$footnotes <- list(`1` = list(left = "OUTPUT FOOT"))
  m$page$layout <- list(list(object = "demog"),
                        list(object = "again", page_break = FALSE, space_before = 1L,
                             titles = list(`1` = list(left = "SECOND TITLE"))),
                        list(object = "third", titles = list(`1` = list(left = "THIRD TITLE"))))
  tfl_write_output(m, file.path(d, "spec", "outputs", "M-1.yml"))
  st <- tfl_read_study_spec(d)
  mm <- tfl_read_outputs(d)[["M-1"]]
  ro <- tfl_resolve_output(mm, st)
  # the study's defaults go into each placed object, as into a flat content
  expect_true("plan_stub" %in% vapply(ro$content[[2L]]$items, function(it) it$fn, ""))
  f <- tfl_run_output("M-1", d)
  x <- po_rtf(f)
  at <- vapply(c("OUTPUT TITLE", "SECOND TITLE", "THIRD TITLE", "OUTPUT FOOT"),
               function(k) regexpr(k, x, fixed = TRUE)[1L], 0L)
  expect_true(all(at > 0L))
  expect_false(is.unsorted(at))
  # the pagination of the old form: the second table on under the first,
  # the third on a page of its own
  expect_identical(po_breaks(x), 1L)
  expect_true(regexpr("\\\\sect[^a-z]", x) < regexpr("THIRD TITLE", x, fixed = TRUE))
  # each table its headings' N (three tables from one ARD)
  expect_identical(lengths(regmatches(x, gregexpr("[(]N=[0-9]+[)]", x))), 9L)
  expect_identical(nrow(tfl_check_build(st, "M-1")), 0L)
  # the program: its content part makes both objects, its report part places them
  r <- list(id = "M-1", type = "table", program = "M-1.R", file = "M-1.rtf", output = mm,
            spec_output = mm)
  code <- tfl_report_program(r, study = st, shared = list(ard = tflspec:::.ard_spec_of_study(st)$spec),
                             date = as.Date("2026-01-01"), generator = "tflspec 0.0.0")
  rep <- code[grep("^# ---- report", code)[1L]:length(code)]
  expect_true(all(c("demog <- ard |>", "again <- ard |>", "third <- ard |>") %in% rep))
  expect_true(any(grepl("^  rtf_tables[(]$", rep)))
  expect_true("    again," %in% rep)
  # ... and run, they make the same RTF as the direct run
  ard <- suppressMessages(tfl_build_ard(st, dir = d, output_id = "M-1", save = FALSE))
  env <- new.env()
  assign("ard", ard, envir = env)
  assign("report_id", "M-1", envir = env)
  # (the study's path variables, as its setup sets them)
  lay <- tfl_study_layout()
  for (v in unique(unlist(regmatches(rep, gregexpr("\\bpath_[a-z_]+", rep))))) {
    assign(v, lay[[sub("^path_", "", v)]] %||% ".", envir = env)
  }
  # (the study's rounding, as its setup sets it)
  old_r <- tflspec:::.study_rounding_option(st)
  on.exit(options(old_r), add = TRUE)
  upto <- grep("^generate_rtfreport", rep)[1L] - 1L
  withr::with_dir(d, suppressMessages(eval(parse(text = c("library(rtfreporter)", rep[seq_len(upto)])),
                                           env)))
  g <- tempfile(fileext = ".rtf")
  on.exit(unlink(g), add = TRUE)
  suppressMessages(rtfreporter::generate_rtfreport(env$doc, g, overwrite = TRUE))
  strip <- function(s) {
    # (the time each was written: {DATETIME})
    gsub("[0-9]{2}[A-Z][a-z]{2}[0-9]{4} +[0-9:]+|[0-9]{4}-[0-9]{2}-[0-9]{2}[T ][0-9:]+", "", s)
  }
  expect_identical(strip(po_rtf(g)), strip(x))
  # own content code that leaves the objects as a named list: the program
  # takes each the layout places from it by name, and places it
  own <- tfl_output("M-2", "table", content = tfl_r(paste(
    "a <- rtfreporter::normalize_ard(ard)",
    "content <- list(one = rtfreporter::table_plan(a, cols = \"TRT01A\"),",
    "                two = rtfreporter::table_plan(a, cols = \"TRT01A\"))", sep = "\n")))
  own$ard <- mm$ard
  own$page <- list(layout = list(list(object = "one"), list(object = "two", page_break = FALSE)))
  tfl_write_output(own, file.path(d, "spec", "outputs", "M-2.yml"))
  o2 <- tfl_read_outputs(d)[["M-2"]]
  r2 <- list(id = "M-2", type = "table", program = "M-2.R", file = "M-2.rtf", output = o2,
             spec_output = o2, steps = list(data = NA_character_, content = as.character(o2$content)))
  code2 <- tfl_report_program(r2, study = st, shared = list(ard = tflspec:::.ard_spec_of_study(st)$spec),
                              date = as.Date("2026-01-01"), generator = "tflspec 0.0.0")
  expect_true(all(c("one <- content[[\"one\"]]", "two <- content[[\"two\"]]") %in% code2))
  expect_false("content <- report_content_check(content, \"table\", report_id)" %in% code2)
  expect_true("  rtf_tables(two, page_break = FALSE)" %in% code2)
  expect_true(file.exists(tfl_run_output("M-2", d)))
})

test_that("a listing beside a table: its records from a study dataset, a check note for what is not placed", {
  d <- po_study()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  m <- tfl_output("M-9", "table", label = "AEs and their listing", common = list(population = "SAF"))
  m$ard <- list(analyses = list(list(id = "AE", method = "categorical", dataset = "ADAE",
                                     population = "SAF", by = list("TRT01A"),
                                     variables = list("AESEV"))))
  m$data <- list(source = "ard")
  m$content <- list(
    po_card("summary", items = list(tfl_item("table_plan", list(cols = "TRT01A",
                                                                rows = list(group = "variable"))))),
    po_card("severe", "adae", list(tfl_item(".r", list(code = tfl_r(
      "subset(AESEV == \"SEVERE\", select = c(USUBJID, AEDECOD))"))))),
    po_card("records", "severe", list(
      tfl_item("plan_listing", list(cols = list(
        tfl_call("listing_col", list(vars = list("USUBJID"), label = "Subject", width = 20)),
        tfl_call("listing_col", list(vars = list("AEDECOD"), label = "Preferred term", width = 30))))))),
    po_card("spare", "summary", list(tfl_item("plan_cells", list(na = "-")))))
  m$page <- list(titles = list(`1` = list(center = "Table M-9")),
                 layout = list(list(object = "summary"),
                               list(object = "records", page_break = FALSE, space_before = 1L)))
  tfl_write_output(m, file.path(d, "spec", "outputs", "M-9.yml"))
  st <- tfl_read_study_spec(d)
  f <- tfl_run_output("M-9", d)
  x <- po_rtf(f)
  expect_match(x, "Preferred term", fixed = TRUE)
  # the listing's first page on under the summary (its later pages, pages
  # of their own: the same RTF as next's contents: made, when this was written)
  expect_true(regexpr("Preferred term", x, fixed = TRUE) < regexpr("\\\\sect[^a-z]", x))
  # `spare` is made, not placed, read by no other object: a note
  cb <- tfl_check_build(st, "M-9")
  expect_identical(cb$level, "note")
  expect_identical(cb$item, "page.layout")
  expect_match(cb$message, "makes spare (a table_plan)", fixed = TRUE)
})
