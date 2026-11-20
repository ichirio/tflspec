# tfl_output_objects(), tfl_output_object_uses(), tfl_output_rename(),
# tfl_default_name(): a report's named objects (OBJECTS.md B-1, B-8, B-9),
# and the checks of the names a report's fields give (card rules 5, 7, 11)

oo_read <- function(lines) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(lines, f)
  tfl_read_output(f)
}

oo_table <- function() oo_read(c(
  "id: T-1", "kind: table", "label: AE",
  "data:",
  "  - id: adsl_sub", "    from: ADSL",
  "  - id: adae_sub", "    from: ADAE", "    subjects_in: adsl_sub",
  "    where: TRTEMFL == \"Y\" & USUBJID %in% adsl_sub$USUBJID",
  "ard:", "  analyses:",
  "    - id: BIGN", "      method: categorical", "      data: adsl_sub", "      variables: [TRT01A]",
  "    - id: AE", "      method: hierarchical", "      data: adae_sub", "      denominator: adsl_sub",
  "      variables: [AEBODSYS, AEDECOD]",
  "    - id: KM_FIT", "      type: model", "      method: survfit", "      data: adsl_sub",
  "content:", "  - table_plan: {rows: [AEBODSYS]}"))

oo_figure <- function(plots = "[km, risk]") oo_read(c(
  "id: F-1", "kind: figure", "label: KM",
  "data:", "  steps:", "    - {step: read, dataset: ADTTE}",
  "    - {step: survfit, name: fit, time: AVAL, censor: CNSR}",
  "content:",
  "  - km:", "      - ggsurvfit: {x: fit, linewidth: 0.3}", "      - labs: {x: Time}",
  "  - risk:", "      - tfl_risk_panel: {fit: fit, title: At risk}",
  paste0("  - wrap_plots: {plots: ", plots, ", ncol: 1}"), "  - tfl_image: {dpi: 300}"))

test_that("a table: its data, items and stage results, in order, with the program's R names", {
  x <- tfl_output_objects(oo_table())
  expect_named(x, c("output_id", "object", "name", "id", "stage", "kind", "where",
                    "name_source", "changeable", "used", "from", "used_by"))
  expect_identical(x$name_source, rep(c("written", "default"), c(2L, 6L)))
  expect_identical(x$id, c(NA, NA, "BIGN", "AE", "KM_FIT", NA, NA, NA))
  # (an ARD item's R name is the program's builder's, whatever its prefix)
  an <- tflspec:::.ard_item_names(c("BIGN", "AE", "KM_FIT"))
  expect_identical(x$name, c("adsl_sub", "adae_sub", an[1:2], "km_fit",
                                  "ard", "plan", "doc"))
  expect_identical(x$stage, c("data", "data", "analysis", "analysis", "analysis", "analysis",
                              "content", "page"))
  expect_identical(x$kind, c("data", "data", "ard", "ard", "model", "ard", "plan", "doc"))
  expect_identical(x$where[1:5], c("data[1]", "data[2]", "analysis[1]", "analysis[2]", "analysis[3]"))
  # (a stage's result can be renamed, the page's doc not)
  expect_identical(x$changeable, c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
  # (the model: nothing reads it)
  expect_identical(x$used, c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE))
  expect_identical(x$from[1:5], c("ADSL", "ADAE", "adsl_sub", "adae_sub", "adsl_sub"))
  expect_identical(x$used_by[1L], "adae_sub | BIGN | AE | KM_FIT")
  # the uses: the fields that name it, the R that reads it
  u <- attr(x, "uses")
  expect_named(u, c("name", "stage", "where", "item", "arg", "how"))
  a <- u[u$name == "adsl_sub", ]
  expect_identical(a$arg, c("subjects_in", "data", "denominator", "data", "where"))
  expect_identical(a$how, c("ref", "ref", "ref", "ref", "r"))
  expect_identical(a$where[a$how == "r"], "data[2]")
  # the builders the program is written with
  b <- tflspec:::.ard_item_names("BIGN")
  expect_identical(tflspec:::.ard_item_names(c("BIGN", "a-b", "bign"), b),
                   c(paste0(b, "_1"), sub("bign$", "a_b", b), paste0(b, "_2")))
  expect_identical(tflspec:::.ard_model_name("KM_FIT", "km_fit"), "km_fit_1")
})

test_that("a figure of two named plots and wrap_plots: the plots' R names, the steps' fit, its uses", {
  out <- oo_figure()
  x <- tfl_output_objects(out)
  pn <- tflspec:::.fig_plot_name(c("km", "risk"))
  expect_identical(x$name, c("fit", "km", "risk", "content", "doc"))
  expect_true(all(x$used))
  expect_identical(x$name_source, c("written", "written", "written", "default", "default"))
  expect_identical(x$kind, c("model", "plot", "plot", "content", "doc"))
  expect_identical(x$where[1:3], c("data.steps[2]", "content[1]", "content[2]"))
  expect_identical(x$used_by[1L], "km$ggsurvfit | risk$tfl_risk_panel")
  expect_identical(x$used_by[2L], "wrap_plots")
  # the names the code writes
  code <- tfl_content_code(out)
  expect_true(any(startsWith(code, paste0(pn[1L], " <- "))))
  expect_true(any(grepl(sprintf("wrap_plots(%s, %s", pn[1L], pn[2L]), code, fixed = TRUE)))
  # one named plot: it is the content
  one <- out
  one$content <- one$content[c(1L, 5L)]
  expect_true(tfl_output_objects(one)$used[2L])
  # a plot named in wrap_plots is used; one that is not can be deleted
  expect_identical(tfl_output_object_uses(out, "risk")$arg, "plots")
  expect_identical(tfl_output_object_uses(out, pn[1L])$item, "wrap_plots")
  nouse <- out
  nouse$content[[3L]]$args$plots <- list("km")
  expect_identical(nrow(tfl_output_object_uses(nouse, "risk")), 0L)
  expect_error(tfl_output_object_uses(out, "nothing"), "has no object nothing")
})

test_that("a rename rewrites the fields that name it, lists the R that reads it", {
  r <- tfl_output_rename(oo_table(), "adsl_sub", "adsl_c1")
  expect_identical(r$ard$analysis_data[[1L]]$id, "adsl_c1")
  expect_identical(r$ard$analysis_data[[2L]]$subjects_in, "adsl_c1")
  expect_identical(vapply(r$ard$analyses, function(a) a$data, ""), c("adsl_c1", "adae_sub", "adsl_c1"))
  expect_identical(r$ard$analyses[[2L]]$denominator, "adsl_c1")
  # the R is not changed: listed
  expect_match(r$ard$analysis_data[[2L]]$where, "adsl_sub$USUBJID", fixed = TRUE)
  ru <- attr(r, "r_uses")
  expect_identical(ru$where, "data[2]")
  expect_identical(ru$arg, "where")
  # a model's id, its R name with it
  m <- tfl_output_rename(oo_table(), "KM_FIT", "os_fit")
  expect_identical(m$ard$analyses[[3L]]$id, "os_fit")
  expect_identical(tfl_output_objects(m)$name[5L], "os_fit")
  # an ARD item: its id, its R name follows (the default name from the id)
  a <- tfl_output_rename(oo_table(), "BIGN", "N_ARM")
  expect_identical(a$ard$analyses[[1L]]$id, "N_ARM")
  expect_identical(tfl_output_objects(a)$name[3L], tflspec:::.ard_item_names("N_ARM"))
  expect_error(tfl_output_rename(oo_table(), tflspec:::.ard_item_names("AE"), "x"), "rename the item by its id")
  # stops: a clash, a name the kind does not take, no such object
  expect_error(tfl_output_rename(oo_table(), "adsl_sub", "adae_sub"), "already names an object")
  # (a model's R name)
  expect_error(tfl_output_rename(oo_table(), "adsl_sub", "km_fit"), "already names an object")
  expect_error(tfl_output_rename(oo_table(), "BIGN", "AE"), "already names an object")
  expect_error(tfl_output_rename(oo_table(), "adsl_sub", "ADSL_SUB"), "not a name of a data")
  # (a stage's result: its name can change once the SPEC has its key, B-4)
  expect_error(tfl_output_rename(oo_table(), "plan", "tab"), "no key for it yet")
  expect_error(tfl_output_rename(oo_table(), "doc", "x"), "the page's")
  expect_error(tfl_output_rename(oo_table(), "nothing", "x"), "no object nothing")
  # a figure's plot and its data step
  f <- tfl_output_rename(oo_figure(), "km", "km_os")
  expect_identical(f$content[[1L]]$fn, "km_os")
  expect_identical(unlist(f$content[[3L]]$args$plots), c("km_os", "risk"))
  expect_true(any(grepl(sprintf("wrap_plots(%s, %s", tflspec:::.fig_plot_name("km_os"),
                                tflspec:::.fig_plot_name("risk")), tfl_content_code(f), fixed = TRUE)))
  g <- tfl_output_rename(oo_figure(), "fit", "km_fit")
  expect_identical(g$data$steps[[2L]]$name, "km_fit")
  expect_identical(g$content[[1L]]$items[[1L]]$args$x, "km_fit")
  expect_identical(g$content[[2L]]$items[[1L]]$args$fit, "km_fit")
  expect_error(tfl_output_rename(oo_figure(), "km", "ggplot"), "not a name of a plot")
  expect_error(tfl_output_rename(oo_figure(), "km", "risk"), "already names an object")
})

test_that("a template report: the template's rows are fixed, the report's own are not", {
  out <- oo_read(c(
    "id: T-3", "kind: table", "label: DM",
    "template:", "  name: tflspec_v1/demog", "  args: {continuous: [AGE]}",
    "data:", "  - id: adsl_sub", "    where: AGE > 1",
    "  - id: adsl_old", "    from: adsl_sub", "    where: AGE >= 65"))
  x <- tfl_output_objects(out)
  expect_identical(x$name[1:2], c("adsl_sub", "adsl_old"))
  expect_identical(x$changeable[1:2], c(FALSE, TRUE))
  expect_false(any(x$changeable[x$stage == "analysis" & !is.na(x$id)]))
  expect_true("BIGN" %in% x$id)
  expect_error(tfl_output_rename(out, "adsl_sub", "adsl_x"), "the template's name")
  r <- tfl_output_rename(out, "adsl_old", "adsl_65")
  expect_identical(r$ard$analysis_data[[2L]]$id, "adsl_65")
})

test_that("the default name of a new object: <dataset>_sub, <kind>_<n>, the stages'", {
  t1 <- oo_table()
  expect_identical(tfl_default_name(t1, "data", from = "ADSL"), "adsl_sub2")
  expect_identical(tfl_default_name(t1, "data", from = "ADLB"), "adlb_sub")
  expect_identical(tfl_default_name(t1, "model"), "model_1")
  expect_identical(tfl_default_name(t1, "value"), "value_1")
  expect_identical(tfl_default_name(t1, "something"), "obj_1")
  t1$ard$analysis_data <- c(t1$ard$analysis_data, list(list(id = "adsl_sub2", from = "ADSL")),
                            list(list(id = "model_1", from = "ADSL")))
  expect_identical(tfl_default_name(t1, "data", from = "ADSL"), "adsl_sub3")
  expect_identical(tfl_default_name(t1, "model"), "model_2")
  expect_identical(tfl_default_name(t1, "plot"), "plot_1")
  f <- oo_figure()
  f$content[[1L]]$fn <- "plot_1"
  f$content[[3L]]$args$plots <- list("plot_1", "risk")
  expect_identical(tfl_default_name(f, "plot"), "plot_2")
  expect_error(tfl_default_name(t1, "data"), "needs `from`")
  # an ARD item's, by the program's builder; the stages' results
  expect_identical(tfl_default_name(t1, "ard", id = "AGE"), tflspec:::.ard_item_names("AGE"))
  expect_identical(tfl_default_name(t1, "ard", id = "BIGN"),
                   tflspec:::.ard_item_names(c("BIGN", "BIGN"))[2L])
  expect_identical(vapply(c("plan", "content", "doc"), function(k) tfl_default_name(t1, k), ""),
                   c(plan = "plan", content = "content", doc = "doc"))
})

test_that("check: a figure's name nothing above makes; a name below, twice, a dataset's; unused", {
  expect_identical(nrow(tflspec:::.check_object_rows(oo_figure(), "F-1")), 0L)
  p <- tflspec:::.check_object_rows(oo_figure("[km, riks]"), "F-1")
  # (and risk, which nothing uses now: a note)
  expect_setequal(p$level, c("error", "note"))
  p <- p[p$level == "error", ]
  expect_identical(p$item, "wrap_plots(plots)")
  expect_match(p$message, "`riks`, not a plot of the figure (km, risk)", fixed = TRUE)
  f <- oo_figure()
  f$content[[1L]]$items[[1L]]$args$x <- "fitt"
  p <- tflspec:::.check_object_rows(f, "F-1")
  expect_identical(p$item, "km$ggsurvfit(x)")
  expect_match(p$message, "`fitt`, which nothing above", fixed = TRUE)
  # a table's named block is not stopped (it will be a block of every content)
  t1 <- oo_read(c("id: T-9", "kind: table", "label: x",
                  "content:", "  - table_plan: {rows: [PARAM]}",
                  "  - blk:", "      - plan_cells: {categorical: \"{n}\"}"))
  expect_identical(nrow(tflspec:::.check_object_rows(t1, "T-9")), 0L)
  # rule 5: a field names only an object made above it
  t2 <- oo_table()
  t2$ard$analysis_data[[1L]]$subjects_in <- "adae_sub"
  p <- tflspec:::.check_object_rows(t2, "T-1")
  expect_identical(p$item, "adsl_sub$subjects_in")
  expect_match(p$message, "made below it (data[2])", fixed = TRUE)
  # rule 7: a name twice, a study dataset's, a function's
  f2 <- oo_figure()
  f2$content[[2L]]$fn <- "km"
  p <- tflspec:::.check_object_rows(f2, "F-1")
  expect_true(any(p$part == "names" & p$item == "km" & grepl("names 2 objects", p$message)))
  f3 <- oo_figure()
  f3$data$steps[[2L]]$name <- "adtte"
  f3$content[[1L]]$items[[1L]]$args$x <- "adtte"
  f3$content[[2L]]$items[[1L]]$args$fit <- "adtte"
  p <- tflspec:::.check_object_rows(f3, "F-1", datasets = "ADTTE")
  expect_identical(p$message, "`adtte` (data.steps[2]) is a study dataset's name: name the object otherwise")
  f3$data$steps[[2L]]$name <- "filter"
  f3$content[[1L]]$items[[1L]]$args$x <- "filter"
  f3$content[[2L]]$items[[1L]]$args$fit <- "filter"
  expect_match(tflspec:::.check_object_rows(f3, "F-1")$message, "is a function's name", fixed = TRUE)
  # rule 11: made and not used -- a note
  t3 <- oo_table()
  t3$ard$analyses[[3L]]$data <- "adae_sub"
  t3$ard$analysis_data <- c(t3$ard$analysis_data, list(list(id = "adlb_sub", from = "ADLB")))
  p <- tflspec:::.check_object_rows(t3, "T-1")
  expect_identical(p$level, "note")
  expect_identical(p$item, "adlb_sub")
  expect_match(p$message, "made and not used", fixed = TRUE)
  # the same rows for a screen, before it keeps a reorder
  expect_identical(tfl_output_name_check(t2)$item, "adsl_sub$subjects_in")
  nouse <- oo_figure("[km]")
  p <- tflspec:::.check_object_rows(nouse, "F-1")
  expect_identical(p$item, "risk")
  expect_identical(p$level, "note")
})

test_that("a content written as R: its top-level names are its objects; its result is checked", {
  t1 <- oo_table()
  t1$content <- tfl_r("tab <- table_plan(ard)
content <- tab |> plan_cells()")
  x <- tfl_output_objects(t1)
  expect_identical(x$name[x$stage == "content"], c("tab", "content"))
  expect_identical(x$used_by[x$name == "tab"], "content")
  expect_identical(nrow(tflspec:::.check_object_rows(t1, "T-1")), 0L)
  t1$content <- tfl_r("plan <- table_plan(ard)")
  p <- tflspec:::.check_object_rows(t1, "T-1")
  expect_true(any(grepl("assigns no `content`", p$message, fixed = TRUE)))
  t1$content <- tfl_r("assign('content', table_plan(ard))")
  p <- tflspec:::.check_object_rows(t1, "T-1")
  expect_identical(p$level, "note")
  expect_match(p$message, "cannot be checked", fixed = TRUE)
})

test_that("a study's report: the program's own data, what reads it, the ARD program's names", {
  skip_on_cran()
  skip_if_not_installed("haven")
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
  st <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  n <- tfl_output_objects(outs[["T-14-2-1"]], st)
  expect_true("adrs_bor" %in% n$name)
  expect_identical(n$from[n$name == "adrs_bor"], "ADRS")
  expect_true(!is.na(n$used_by[n$name == "adrs_bor"]))
  expect_true("ard" %in% n$name[n$kind == "ard"])
  # each item's R name is the one the ARD program assigns
  code <- tfl_ard_code(st, output_id = "T-14-2-1", save = FALSE)
  for (nm in n$name[n$stage == "analysis"]) {
    expect_true(any(grepl(paste0("^", nm, " <- "), code)), info = nm)
  }
  # the check: the report's names keep the rules (nothing to say)
  p <- tfl_check_spec(st, output_id = "T-14-2-1")
  expect_false(any(p$part == "names" & p$level == "error"))
})
