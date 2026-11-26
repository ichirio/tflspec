# A table's or listing's content object in the map form `{name, from, calls}`
# (OBJECTS.md B-4 / B-5, the table side): reading, the round trip, the
# checks, the code (`plan <- ard |> table_plan(...)`) and that the code runs.
# A content with no such object is today's flat call list, which the other
# tests cover.

to_calls <- function() list(
  tfl_item("table_plan", list(cols = "ARM", rows = list(group = "variable"))),
  tfl_item("plan_cells", list(categorical = "{n} ({p:.1f%})")))

to_out <- function(..., kind = "table", data = NULL) {
  tfl_output("T-1", kind, data = data, content = list(...))
}

to_roundtrip <- function(o) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  tfl_write_output(o, f)
  list(lines = readLines(f), back = tfl_read_output(f))
}

to_read_text <- function(txt) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  writeLines(txt, f)
  tfl_read_output(f)
}

to_code <- function(o) no_bign(suppressMessages(tfl_content_code(o, pipe = "|>")))

# the ARD of a small fictional study, and the pages a program makes
to_ard <- function() {
  d <- data.frame(USUBJID = sprintf("S%02d", 1:12),
                  ARM = rep(c("Placebo", "Drug"), each = 6),
                  SEX = rep(c("F", "M", "F"), 4), stringsAsFactors = FALSE)
  cards::ard_stack(d, .by = ARM, cards::ard_tabulate(variables = SEX))
}
to_pages <- function(p) suppressMessages(rtfreporter::plan_apply(p, "table"))
to_run <- function(code, objects) {
  env <- new.env()
  for (n in names(objects)) assign(n, objects[[n]], envir = env)
  suppressMessages(eval(parse(text = c("library(rtfreporter)", code)), env))
  env
}

test_that("a table's object in the map form is read, and written back as it was", {
  txt <- c("id: T-1", "kind: table", "content:",
           "  - name: ard2", "    from: ard", "    calls:", "      - normalize_ard: {}",
           "  - name: plan", "    from: ard2", "    calls:",
           "      - table_plan: {cols: ARM, rows: {group: variable}}",
           "      - plan_cells: {categorical: '{n} ({p:.1f%})'}")
  o <- to_read_text(txt)
  ct <- o$content
  expect_identical(vapply(ct, function(it) it$fn, ""), c("ard2", "plan"))
  expect_identical(vapply(ct, function(it) it$form, ""), c("map", "map"))
  expect_identical(vapply(ct, function(it) it$input, ""), c("ard", "ard2"))
  expect_identical(ct[[2L]]$items[[1L]]$fn, "table_plan")
  expect_identical(ct[[2L]]$items[[1L]]$args$cols, "ARM")
  r <- to_roundtrip(o)
  expect_true(all(c("  - name: ard2", "    from: ard", "  - name: plan", "    from: ard2",
                    "    calls:") %in% r$lines))
  expect_identical(r$back$content, ct)
  # read -> write -> read: the same text again
  expect_identical(to_roundtrip(r$back)$lines, r$lines)
  # a listing's content too
  lo <- to_read_text(c("id: L-1", "kind: listing", "content:", "  - name: plan", "    from: adae",
                       "    calls:", "      - plan_paginate_rows: {max_rows: 22}"))
  expect_identical(lo$content[[1L]]$input, "adae")
  expect_identical(to_roundtrip(lo)$back$content, lo$content)
})

test_that("a content with no object is read and written as the flat list it is", {
  o <- to_out(tfl_item("table_plan", list(cols = "ARM")), tfl_item("plan_cells", list(na = "-")))
  r <- to_roundtrip(o)
  expect_identical(r$back$content, o$content)
  expect_false(any(grepl("calls:", r$lines, fixed = TRUE)))
  expect_false(.content_has_objects(o$content))
  expect_false(.content_has_objects(list()))
})

test_that("the read stops of a table's object are the figure's", {
  bad <- function(...) to_read_text(c("id: T-1", "kind: table", "content:", ...))
  expect_error(bad("  - {name: plan, calls: [], from_x: a}"), "from_x")
  expect_error(bad("  - {name: plan, from: ard}"), "calls")
  expect_error(bad("  - {name: plan, calls: []}"), "calls")
  expect_error(bad("  - calls:", "      - table_plan: {cols: ARM}"), "comes with B-5")
  expect_error(bad("  - {name: plan, from: [a, b], calls: [{table_plan: {}}]}"), "from")
})

test_that("code: a From-less object is today's code, `from:` pipes the plan", {
  flat <- to_out(to_calls()[[1L]], to_calls()[[2L]])
  m <- to_out(tfl_plot("plan", to_calls(), form = "map"))
  expect_identical(to_code(m), to_code(flat))
  expect_identical(to_code(to_roundtrip(m)$back), to_code(flat))
  f <- to_out(tfl_plot("plan", to_calls(), input = "ard", form = "map"))
  code <- to_code(f)
  expect_identical(code[1L], "plan <- ard |>")
  expect_identical(code[2L], "  table_plan(cols = \"ARM\", rows = c(group = \"variable\")) |>")
  expect_identical(utils::tail(code, 1L), "  plan_cells(categorical = \"{n} ({p:.1f%})\")")
  # the same through a file, and with the other pipe
  expect_identical(to_code(to_roundtrip(f)$back), code)
  expect_identical(no_bign(suppressMessages(tfl_content_code(f, pipe = "%>%")))[1L], "plan <- ard %>%")
})

test_that("code: a helper object above the plan, each statement in order", {
  helper <- tfl_plot("ard2", list(tfl_item("normalize_ard", list())), input = "ard", form = "map")
  o <- to_out(helper, tfl_plot("plan", to_calls(), input = "ard2", form = "map"))
  code <- to_code(o)
  expect_identical(code[1:3], c("ard2 <- ard |>", "  normalize_ard()", ""))
  expect_identical(code[4L], "plan <- ard2 |>")
  # a helper without `from:` starts with its first call as written
  h2 <- tfl_plot("h2", list(tfl_item("normalize_ard", list()), tfl_item("normalize_ard", list())),
                 form = "map")
  c2 <- to_code(to_out(h2, tfl_plot("plan", to_calls(), form = "map")))
  expect_identical(c2[1:3], c("h2 <- normalize_ard() |>", "  normalize_ard()", ""))
  expect_identical(c2[4L], "plan <- table_plan(data, cols = \"ARM\", rows = c(group = \"variable\")) |>")
  # a helper of one call
  c3 <- to_code(to_out(tfl_plot("h", list(tfl_item("normalize_ard", list())), form = "map"),
                       tfl_plot("plan", to_calls(), form = "map")))
  expect_identical(c3[1:2], c("h <- normalize_ard()", ""))
})

test_that("code: the table's own rules apply to the plan object, and from + data is a stop", {
  o <- to_out(tfl_plot("plan", to_calls(), input = "ard", form = "map"))
  o$data <- list(object = "ard")
  expect_error(tfl_content_code(o), "two inputs")
  # the last object is the plan the program reads
  bad <- to_out(tfl_plot("tab", to_calls(), form = "map"))
  expect_error(tfl_content_code(bad), "name it `plan`")
  # objects or a flat list
  mixed <- to_out(tfl_plot("plan", to_calls(), form = "map"), tfl_item("plan_cells", list()))
  expect_error(tfl_content_code(mixed), "not both")
  # a plan named for the argument is the name
  expect_identical(
    no_bign(suppressMessages(tfl_content_code(to_out(tfl_plot("p2", to_calls(), form = "map")), plan = "p2")))[1L],
    "p2 <- table_plan(data, cols = \"ARM\", rows = c(group = \"variable\")) |>")
})

test_that("code: a listing's object", {
  calls <- list(tfl_item("plan_paginate_rows", list(max_rows = 22)))
  flat <- tfl_output("L-1", "listing", content = calls)
  m <- tfl_output("L-1", "listing", content = list(tfl_plot("plan", calls, form = "map")))
  expect_identical(to_code(m), to_code(flat))
  f <- tfl_output("L-1", "listing", content = list(tfl_plot("plan", calls, input = "adae", form = "map")))
  expect_identical(to_code(f), c("plan <- adae |>", "  table_plan() |>", "  plan_paginate_rows(max_rows = 22)"))
})

test_that("the program runs: the objects make the plan the flat content makes", {
  skip_if_not_installed("cards")
  ard <- to_ard()
  flat <- to_out(tfl_item("normalize_ard", list()), to_calls()[[1L]], to_calls()[[2L]])
  want <- to_pages(to_run(to_code(flat), list(ard = ard))$plan)
  o <- to_out(tfl_plot("data", list(tfl_item("normalize_ard", list())), input = "ard", form = "map"),
              tfl_plot("plan", to_calls(), input = "data", form = "map"))
  env <- to_run(to_code(o), list(ard = ard))
  expect_s3_class(env$plan, "table_plan")
  expect_identical(to_pages(env$plan), want)
  # From-less: the report's data is table_plan()'s first argument, as today
  data <- suppressMessages(rtfreporter::normalize_ard(ard))
  m <- to_out(tfl_plot("plan", to_calls(), form = "map"))
  expect_identical(to_pages(to_run(to_code(m), list(data = data))$plan), want)
  # a listing, on a small data frame
  d <- data.frame(USUBJID = sprintf("S%02d", 1:8), ARM = rep(c("A", "B"), 4), stringsAsFactors = FALSE)
  calls <- list(tfl_item("plan_paginate_rows", list(max_rows = 5)))
  lf <- tfl_output("L-1", "listing", content = calls)
  lo <- tfl_output("L-1", "listing", content = list(tfl_plot("plan", calls, input = "adsl", form = "map")))
  expect_identical(to_pages(to_run(to_code(lo), list(adsl = d))$plan),
                   to_pages(to_run(to_code(lf), list(data = d))$plan))
})

test_that("a content that holds objects is not run or measured from its items", {
  o <- to_out(tfl_plot("plan", to_calls(), input = "ard", form = "map"))
  expect_error(tfl_content_run(o, to_ard()), "not run yet")
  expect_error(.content_table_pipeline(o$content), "not run or measured")
})

test_that("tfl_output_objects(): the objects, `from:` as a use, the plan fixed", {
  o <- to_out(tfl_plot("ard2", list(tfl_item("normalize_ard", list())), input = "ard", form = "map"),
              tfl_plot("plan", to_calls(), input = "ard2", form = "map"))
  ob <- tfl_output_objects(o)
  r <- ob[ob$stage == "content" & ob$where %in% c("content[1]", "content[2]"), ]
  expect_identical(r$name, c("ard2", "plan"))
  expect_identical(r$from, c("ard", "ard2"))
  expect_identical(r$changeable, c(TRUE, FALSE))
  expect_true(all(r$used))
  u <- tfl_output_object_uses(o, "ard2")
  expect_identical(u$where[u$arg == "from"], "content[2]")
})

test_that("a rename follows `from:`, and the plan cannot be renamed", {
  o <- to_out(tfl_plot("ard2", list(tfl_item("normalize_ard", list())), input = "ard", form = "map"),
              tfl_plot("plan", to_calls(), input = "ard2", form = "map"))
  n <- tfl_output_rename(o, "ard2", "ard_n")
  expect_identical(vapply(n$content, function(it) it$fn, ""), c("ard_n", "plan"))
  expect_identical(n$content[[2L]]$input, "ard_n")
  expect_identical(n$content[[1L]]$input, "ard")
  expect_error(tfl_output_rename(o, "plan", "plan2"), "cannot change")
})

test_that("checks: `from:`, the last object, two inputs, mixed content, B-7", {
  chk <- function(o, ...) .check_object_rows(o, "T-1", ...)
  errs <- function(r) r$message[r$level == "error"]
  ok <- to_out(tfl_plot("ard2", list(tfl_item("normalize_ard", list())), input = "ard", form = "map"),
               tfl_plot("plan", to_calls(), input = "ard2", form = "map"))
  expect_length(errs(chk(ok)), 0L)
  expect_equal(nrow(chk(ok)), 0L)
  # a `from:` nothing above makes; a study dataset is fine
  nf <- to_out(tfl_plot("plan", to_calls(), input = "nowhere", form = "map"))
  expect_match(errs(chk(nf)), "takes its `from:` `nowhere`", all = FALSE)
  expect_length(errs(chk(to_out(tfl_plot("plan", to_calls(), input = "ADSL", form = "map")),
                         datasets = "ADSL")), 0L)
  # made below it
  below <- to_out(tfl_plot("plan", to_calls(), input = "late", form = "map"),
                  tfl_plot("late", to_calls(), form = "map"))
  expect_true(any(grepl("made below it", errs(chk(below)))))
  # the last object is the plan
  last <- to_out(tfl_plot("tab", to_calls(), form = "map"))
  expect_match(errs(chk(last)), "the last object of a table's content is `tab`", all = FALSE)
  # two inputs
  two <- to_out(tfl_plot("plan", to_calls(), input = "ard", form = "map"), data = list(object = "ard"))
  expect_match(errs(chk(two)), "two inputs", all = FALSE)
  # objects and flat calls
  mixed <- to_out(tfl_plot("plan", to_calls(), form = "map"), tfl_item("plan_cells", list()))
  expect_match(errs(chk(mixed)), "not both", all = FALSE)
  # B-7: a call that writes the argument the pipe fills
  d <- tfl_fn_defs()
  first <- d$first_arg_name[d$fn == "table_plan"]
  expect_false(is.na(first))
  args <- stats::setNames(list("ard", "ARM"), c(first, "cols"))
  b7 <- to_out(tfl_plot("plan", list(tfl_item("table_plan", args)), input = "ard", form = "map"))
  expect_match(errs(chk(b7)), "the value and the pipe disagree", all = FALSE)
  none <- to_out(tfl_plot("plan", list(tfl_item("table_plan", args)), form = "map"))
  expect_length(errs(chk(none)), 0L)
  # a helper nothing reads is a note, as for a figure's
  un <- to_out(tfl_plot("h", to_calls(), form = "map"), tfl_plot("plan", to_calls(), form = "map"))
  expect_identical(chk(un)$level, "note")
  # a listing's last object is the plan too
  lst <- tfl_output("L-1", "listing", content = list(tfl_plot("x", to_calls()[2L], form = "map")))
  expect_match(errs(.check_object_rows(lst, "L-1")), "the last object of a listing's content", all = FALSE)
})
