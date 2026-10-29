# A table's parts (rtfreporter's plan_append(), #737): one content item with
# the part's own data and items; read, written, its code and its run.

sp_fn <- tflspec:::.spec_part$fn
sp_yaml <- function() {
  f <- tempfile(fileext = ".yml")
  writeLines(c(
    "id: T-PK", "kind: table", "label: Concentrations",
    "data: {source: dataset, dataset: ADPC}",
    "content:",
    "  - table_plan:",
    "      cols: ATPT",
    "      rows: {subject: !r '~ \"{USUBJID}/{AGE}/{SEX}\"'}",
    "      value: AVAL",
    "  - plan_cells: {default: \"{value:.3s}\", na: BLQ}",
    paste0("  - ", sp_fn, ":"),
    "      name: summary",
    "      data: {source: ard}",
    "      content:",
    "        - normalize_ard: {}",
    "        - table_plan:",
    "            cols: ATPT",
    "            rows: {block: !r '~ \"Summary Statistics\"'}",
    "        - plan_cells:",
    "            continuous: {N: \"{N:.0f}\", Mean: \"{mean:.4s}\"}"), f)
  f
}
sp_data <- function() {
  d <- data.frame(USUBJID = rep(c("1001", "1002"), each = 2), AGE = rep(c(37L, 63L), each = 2),
                  SEX = "F", ATPT = rep(c("1 hr", "4 hr"), 2), AVAL = c(3.331, 2.73, 3.11, NA),
                  stringsAsFactors = FALSE)
  list(data = d,
       ard = cards::ard_summary(d[!is.na(d$AVAL), ], by = ATPT, variables = AVAL,
                                statistic = ~ list(N = length, mean = mean)))
}

test_that("a part is read as an item with its own data and items, and written back the same", {
  o <- tfl_read_output(sp_yaml())
  it <- o$content[[3L]]
  expect_identical(it$fn, sp_fn)
  expect_identical(it$args$name, "summary")
  expect_identical(it$args$data$source, "ard")
  expect_true(all(vapply(it$args$content, inherits, NA, "tfl_item")))
  expect_identical(vapply(it$args$content, `[[`, "", "fn"), c("normalize_ard", "table_plan", "plan_cells"))
  f <- tempfile(fileext = ".yml")
  tfl_write_output(o, f)
  expect_identical(lapply(tfl_read_output(f)$content, unclass), lapply(o$content, unclass))
})

test_that("the program writes the part's plan from its own object, then lays it in", {
  o <- tfl_read_output(sp_yaml())
  code <- tfl_content_code(o, data = "data")
  at <- grep("^summary <- ard \\|>", code)
  expect_length(at, 1L)
  expect_true(at < grep("^plan <- ", code))
  expect_true(any(grepl(paste0(sp_fn, "(part = summary, name = \"summary\")"), code, fixed = TRUE)))
})

test_that("the run makes the part's plan from its object: its rows under the table's", {
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
  skip_if_not(exists(sp_fn, envir = asNamespace("rtfreporter")), "rtfreporter without the parts verb")
  o <- tfl_read_output(sp_yaml())
  p <- tfl_content_run(o, sp_data())
  t <- as.data.frame(rtfreporter::plan_apply(p, "table"))
  expect_identical(t$label[1:2], c("1001/37/F", "1002/63/F"))
  expect_true("Summary Statistics" %in% t$label)
  expect_identical(t[["4 hr"]][2], "BLQ")
  # its object missing
  expect_error(tfl_content_run(o, list(data = sp_data()$data)), "no `ard` to make its rows from")
})

test_that("a part's object: data's object, the ARD, the dataset in lower case; two parts of one name stop", {
  expect_identical(tflspec:::.part_object(list(source = "ard")), "ard")
  expect_identical(tflspec:::.part_object(list(source = "dataset", dataset = "ADPC")), "adpc")
  expect_identical(tflspec:::.part_object(list(object = "pk", source = "ard")), "pk")
  part <- function(nm) tfl_item(sp_fn, list(name = nm, data = list(source = "ard"),
                                              content = list(tfl_item("table_plan", list(cols = "A")))))
  expect_error(tflspec:::.content_parts(list(part("s"), part("s"))), "two parts are called")
  expect_error(tflspec:::.content_parts(list(tfl_item(sp_fn, list(name = "s")))), "its `content`")
})

test_that("in a study: tfl_run_output() builds a table of a listing and its summary from the ARD", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
  skip_if_not(exists(sp_fn, envir = asNamespace("rtfreporter")), "rtfreporter without the parts verb")
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adlb")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "SP-1")
  keep <- "PARAMCD == \"ALT\" & AVISIT %in% c(\"BASELINE\", \"CYCLE 2 DAY 1\")"
  o <- tfl_output(
    "T-LB", "table", population = "SAF", label = "ALT by subject",
    data = list(source = "dataset", dataset = "ADLB", population = "SAF", where = tfl_r(keep)),
    ard = list(
      analysis_data = list(
        list(id = "adsl_saf", from = "ADSL", population = "SAF"),
        list(id = "adlb_alt", from = "ADLB", subjects = "adsl_saf", where = tfl_r(keep))),
      analyses = list(list(id = "ALT", method = "continuous", data = "adlb_alt",
                           by = "AVISIT", variables = "AVAL", statistics = "N | mean"))),
    content = list(
      tfl_item("table_plan", list(cols = "AVISIT", rows = list(subject = tfl_r("~ \"{USUBJID}\"")),
                                  value = "AVAL")),
      tfl_item("plan_cells", list(default = "{value:.1f}", na = "-")),
      tfl_item(sp_fn, list(name = "summary", data = list(source = "ard"), content = list(
        tfl_item("normalize_ard", list()),
        tfl_item("table_plan", list(cols = "AVISIT", rows = list(block = tfl_r("~ \"Summary\"")))),
        tfl_item("plan_cells", list(continuous = list(N = "{N:.0f}", Mean = "{mean:.2f}"))))))))
  tfl_write_output(o, file.path(d, "spec", "outputs", "T-LB.yml"))
  f <- tfl_run_output("T-LB", dir = d)
  expect_true(file.exists(f))
  rtf <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(rtf, "Summary", fixed = TRUE)
  expect_match(rtf, "Mean", fixed = TRUE)
})

test_that("the schema checks a part's arguments and each of its own items", {
  o <- tfl_read_output(sp_yaml())
  expect_identical(nrow(tfl_schema_check(o$content[[3L]])), 0L)
  it <- o$content[[3L]]
  it$args$content[[2L]]$args$nope <- 1
  it$args$absent <- "maybe"
  p <- tfl_schema_check(it)
  expect_true(any(p$fn == "table_plan" & p$arg == "nope"))
  expect_true(any(p$fn == sp_fn & p$arg == "absent" & p$level == "error"))
  it$args$content <- NULL
  expect_true(any(tfl_schema_check(it)$arg == "content"))
})
