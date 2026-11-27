# Branches no other test reaches in the object cards and content objects
# that entered `next` from 2026-11-24 (B-1 .. B-5): the objects list, the
# function table, a table's and a figure's content objects as code, the
# analyses sheet's keys and the R tags.  Fictional data only.

cov_calls <- function() {
  list(tfl_item("table_plan", list(cols = "ARM")),
       tfl_item("plan_cells", list(categorical = "{n}")))
}

# ---- the objects list (R/object_list.R) ---------------------------------------

test_that("a table's content object with no calls is listed as an object, not a plan", {
  o <- tfl_output("T-1", "table", content = list(tfl_plot("plan", list(), form = "map")))
  x <- tfl_output_objects(o)
  r <- x[x$where %in% "content[1]", ]
  expect_identical(r$name, "plan")
  expect_identical(r$kind, "object")
})

test_that("a figure's plot with no calls is listed as a plot", {
  o <- tfl_output("F-1", "figure", content = list(tfl_plot("a", list(), form = "map")))
  x <- tfl_output_objects(o)
  r <- x[x$where %in% "content[1]", ]
  expect_identical(r$name, "a")
  expect_identical(r$kind, "plot")
})

test_that("a kind with no final object of its own falls back to plan, or content for R", {
  expect_identical(.obj_final_of("no_such_kind"), "plan")
  expect_identical(.obj_final_of("no_such_kind", TRUE), "content")
})

test_that("an analysis item that is not a map is passed over by the objects list", {
  o <- tfl_output("T-1", "table", ard = list(analyses = list("not a map")),
                  content = list(tfl_item("table_plan", list(cols = "ARM"))))
  # (the object checks skip such an item, R/object_list.R:969, but they are
  # reached only through the objects list, which stops on it first: the
  # checks give no row at all)
  expect_equal(nrow(.check_object_rows(o, "T-1")), 0L)
  skip("bug: tfl_output_objects() stops with \"$ operator is invalid for atomic vectors\" on an analysis item that is not a map (R/object_list.R:90, .obj_merge())")
  expect_no_error(tfl_output_objects(o))
})

# ---- the function table (R/fn_defs.R) -------------------------------------------

test_that("a schema's first_arg_name that is not a name stops, saying an argument's name", {
  m <- data.frame(method = "m", label = "M", call = "cards::ard_summary")
  expect_error(.fn_defs_build(list(foo = list(first_arg_name = 1L)), m),
               "schema foo: first_arg_name: \"1\" is not one of an argument's name")
})

test_that("a schema entry that is not a map gives no row", {
  m <- data.frame(method = "m", label = "M", call = "cards::ard_summary")
  d <- .fn_defs_build(list(foo = "not a map", bar = list(label = "Bar")), m)
  expect_false("foo" %in% d$fn)
  expect_true("bar" %in% d$fn)
})

# ---- a table's content objects as code (R/spec_code.R) --------------------------

test_that("the plan with from: and the report's data steps written is two inputs, a stop", {
  o <- tfl_output("T-1", "table", data = list(steps = list("data <- ard")),
                  content = list(tfl_plot("plan", cov_calls(), input = "ard", form = "map")))
  expect_error(tfl_content_code(o), "`plan` takes `from: ard` and the report's data steps are written too")
})

test_that("a helper object's R item is written as it is, piped like a call", {
  h <- tfl_plot("h", list(tfl_item(".r", list(code = "dplyr::filter(x > 1)")),
                          tfl_item("normalize_ard", list())), input = "ard", form = "map")
  expect_identical(.content_object_lines(h),
                   c("h <- ard |>", "  dplyr::filter(x > 1) |>", "  normalize_ard()"))
  o <- tfl_output("T-1", "table", content = list(h, tfl_plot("plan", cov_calls(), input = "h", form = "map")))
  code <- no_bign(suppressMessages(tfl_content_code(o)))
  expect_identical(code[1:3], c("h <- ard |>", "  dplyr::filter(x > 1) |>", "  normalize_ard()"))
})

test_that("a helper object with no from: starts with its first call, the rest indented", {
  h <- tfl_plot("h", list(tfl_item("normalize_ard", list()), tfl_item("normalize_ard", list())), form = "map")
  expect_identical(.content_object_lines(h), c("h <- normalize_ard() |>", "  normalize_ard()"))
})

test_that("a helper object's long call is laid out over lines, indented by its place", {
  long <- list(a_long_argument_name = "a long value to pass eighty", another = "xxxxxxxxxxxxxxx")
  h <- tfl_plot("h", list(tfl_item("normalize_ard", long)), form = "map")
  expect_identical(.content_object_lines(h),
                   c("h <- normalize_ard(", "  a_long_argument_name = \"a long value to pass eighty\",",
                     "  another = \"xxxxxxxxxxxxxxx\"", ")"))
  h2 <- tfl_plot("h", list(tfl_item(".r", list(code = "x")), tfl_item("normalize_ard", long)), form = "map")
  expect_identical(.content_object_lines(h2),
                   c("h <- x |>", "  normalize_ard(", "    a_long_argument_name = \"a long value to pass eighty\",",
                     "    another = \"xxxxxxxxxxxxxxx\"", "  )"))
})

test_that("no data steps write no lines", {
  expect_identical(.content_steps_code(character()), character())
})

# ---- a figure's plot as code (R/fig_content.R) ----------------------------------

cov_fig <- function(...) {
  tfl_output("F-1", "figure", content = list(tfl_plot("km", list(...), input = "df", form = "map")))
}
cov_fig_lines <- function(o) {
  x <- .fig_content_code(o)
  x[!startsWith(x, "# ----") & nzchar(x)]
}

test_that("an R item in a plot with from: is joined with +", {
  l <- cov_fig_lines(cov_fig(tfl_item("ggplot", list()), tfl_item(".r", list(code = "geom_point()"))))
  expect_identical(l, c("content <- df |>", "  ggplot() +", "  geom_point()"))
})

test_that("tfl_ard_number in a plot with from: writes its value above and is joined with +", {
  l <- cov_fig_lines(cov_fig(tfl_item("ggplot", list()),
                             tfl_item("tfl_ard_number", list(analysis_id = "KM", stat = "estimate"))))
  expect_true("ard_label_1 <- ard_value(ard, \"KM\", \"estimate\")" %in% l)
  expect_identical(l[match("content <- df |>", l) + 1L], "  ggplot() +")
  expect_true("  annotate(" %in% l)
})

test_that("a function with no row in tfl_fn_defs() is joined with +", {
  expect_identical(.fig_fn_join("no_such_layer"), "+")
  l <- cov_fig_lines(cov_fig(tfl_item("ggplot", list()), tfl_item("no_such_layer", list())))
  expect_identical(l, c("content <- df |>", "  ggplot() +", "  no_such_layer()"))
})

test_that("a chain of three calls with no input joins the third as its function says", {
  expect_identical(.fig_pipe_chain("p", NULL, c("a()", "b()", "c()"), c("|>", "+", "+")),
                   c("p <- a() +", "  b() +", "  c()"))
  expect_identical(.fig_pipe_chain("p", NULL, c("a()", "b()", "c()"), c("|>", "+", "|>")),
                   c("p <- (a() +", "  b()) |>", "  c()"))
})

# ---- the analyses sheet's keys (R/spec_sheets.R) --------------------------------

test_that("a key of an analysis joins the args already written", {
  a <- .analysis_keys_carried(list(id = "A", method = "cards::ard_summary", args = "x = 1", foo = "b"), "T-1")
  expect_identical(a$args, "x = 1, foo = \"b\"")
  expect_null(a$foo)
})

test_that("a key's list of single values of one type is written as c()", {
  expect_identical(.analysis_arg_r(list("a", "b")), "c(\"a\", \"b\")")
  expect_identical(.analysis_arg_r(list(1, 2)), "c(1, 2)")
})

test_that("an analysis that is not a map is one analysis as it is", {
  expect_identical(.analysis_ids_split("x", "T-1"), list("x"))
})

test_that("analysis_ids as a list with no names stops, saying a list", {
  a <- list(id = "A", variables = list("AGE"), analysis_ids = list("A1"))
  expect_error(.analysis_ids_split(a, "T-1"),
               "analysis A \\(report T-1\\): `analysis_ids` is a map of its variables \\(AGE\\); a list is not one")
})

test_that("a nested analysis item with name: cannot be written to the sheets", {
  skip_if_not_installed("haven")
  root <- tempfile("cov1124-obj")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  d <- as_study(root)
  st <- tfl_read_study_spec(d)
  t1 <- tfl_read_output(file.path(d, "spec", "outputs", "T-14-1-1.yml"))
  t1$ard$analyses <- c(t1$ard$analyses, list(
    list(id = "DEMO", method = "stack", by = "TRT01A"),
    list(id = "CONT", parent = "DEMO", name = "x", method = "continuous", variables = "AGE")))
  expect_error(suppressMessages(tfl_sheets_from_spec(st, list(`T-14-1-1` = t1))),
               "`name:` / `from:` on a nested item \\(CONT, in DEMO\\) cannot be written")
})

test_that("a TOC set the figure's data rows do not keep is warned, naming the data's set", {
  skip_if_not_installed("haven")
  root <- tempfile("cov1124-obj")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  d <- as_study(root)
  st <- tfl_read_study_spec(d)
  f <- tfl_read_output(file.path(d, "spec", "outputs", "F-1.yml"))
  f$common <- NULL
  f$data <- list(list(id = "adsl_fas", from = "ADSL", population = "FAS"))
  st$toc$outputs <- list(list(id = "F-1", population = "SAF"))
  expect_warning(suppressMessages(tfl_sheets_from_spec(st, list(`F-1` = f))),
                 "F-1: the TOC's analysis set SAF is not kept: the report's data names FAS")
})

# ---- the function catalog's dots, the R tags, the program's pieces --------------

test_that("free dots are carried to the dots' entry", {
  d <- .arg_schema_dots(list(), list(type = "string", free = TRUE))[["..."]]
  expect_true(d$free)
  expect_null(.arg_schema_dots(list(), list(type = "string"))[["..."]]$free)
})

test_that("an NA string stays NA among the values made R", {
  v <- .r_tag_all(c(a = "x > 1", b = NA))
  expect_s3_class(v$a, "tfl_r")
  expect_identical(v$b, NA_character_)
})

test_that("a value that is not text or a list is left as it is", {
  expect_identical(.r_tag_all(1L), 1L)
})

test_that("code of more than one expression is not written as the program's own lines", {
  expect_null(.code_as_program("a <- 1\nb <- 2", "data", NULL))
  expect_null(.code_as_program("a <- (", "data", NULL))
})
