# A report's own code against the rules (tfl_check_code(), U01-U05)
rules_of <- function(r) sort(unique(r$rule))

test_that("good code passes, and says the datasets it uses", {
  code <- c("df <- advs |>",
            "  dplyr::filter(PARAMCD == 'SYSBP') |>",
            "  dplyr::left_join(adsl, by = 'USUBJID')")
  r <- tfl_check_code(code, "data", "figure", datasets = c("ADSL", "ADVS"),
                      catalog = c("ADSL", "ADVS", "ADAE"), output_id = "F-1")
  expect_s3_class(r, "tfl_review")
  expect_identical(nrow(r), 0L)
  expect_setequal(attr(r, "datasets"), c("ADSL", "ADVS"))
  # blank code: nothing to say
  expect_identical(nrow(tfl_check_code("  ", "content", "table")), 0L)
})

test_that("U01: code that does not read as R, with its line", {
  r <- tfl_check_code(c("content <- 1", "x <- (2"), "content", "table", output_id = "T-1")
  expect_identical(r$rule, "U01")
  expect_identical(r$level, "error")
  expect_identical(r$args[[1L]][1:3], c("content", "T-1", "3"))
  expect_identical(r$field, "content_code")
})

test_that("U02: a dataset of the catalog the report does not register", {
  r <- tfl_check_code("df <- advs", "data", "figure", datasets = "ADSL",
                      catalog = c("ADSL", "ADVS"), output_id = "F-1")
  expect_identical(r$rule, "U02")
  expect_match(r$message, "reads advs, which the report does not register", fixed = TRUE)
  # one it makes itself, a column after $, a function's argument name: not a read
  ok <- c("advs <- adsl[adsl$advs > 0, ]", "df <- subset(advs, select = adsl)")
  expect_identical(nrow(tfl_check_code(ok, "data", "figure", datasets = "ADSL",
                                       catalog = c("ADSL", "ADVS"))), 0L)
  # a name that is not a dataset of the catalog: not checked
  expect_identical(nrow(tfl_check_code("df <- my_data", "data", "figure",
                                       catalog = "ADSL")), 0L)
})

test_that("U03: each step leaves what the next one reads", {
  expect_identical(rules_of(tfl_check_code("x <- 1", "data", "table")), "U03")
  # a table's data step may leave its ARD alone (the program normalizes it)
  expect_identical(nrow(tfl_check_code("ard <- my_ard()", "data", "table")), 0L)
  expect_identical(rules_of(tfl_check_code("ard <- 1", "data", "listing")), "U03")
  expect_identical(nrow(tfl_check_code("data <- 1", "data", "listing")), 0L)
  expect_identical(nrow(tfl_check_code("df = 1", "data", "figure")), 0L)
  expect_identical(rules_of(tfl_check_code("data <- 1", "data", "figure")), "U03")
  expect_identical(nrow(tfl_check_code("1 -> content", "content", "figure")), 0L)
  expect_identical(nrow(tfl_check_code("assign('content', 1)", "content", "table")), 0L)
  expect_identical(nrow(tfl_check_code("content$x <- 1", "content", "table")), 0L)
  r <- tfl_check_code("plot <- 1", "content", "figure", output_id = "F-1")
  expect_match(r$message, "leaves no content", fixed = TRUE)
})

test_that("U04: files read, packages attached, the session changed", {
  code <- c("library(dplyr)", "a <- readRDS('x.rds')", "b <- haven::read_xpt('y')",
            "content <- a")
  r <- tfl_check_code(code, "content", "table")
  expect_identical(r$rule, "U04")
  expect_identical(r$level, "check")
  expect_match(r$message, "library(), readRDS(), read_xpt()", fixed = TRUE)
})

test_that("U05: ard where the report does not read its ARD", {
  code <- "content <- ard |> dplyr::filter(stat_name == 'n')"
  expect_identical(rules_of(tfl_check_code(code, "content", "figure")), "U05")
  expect_identical(nrow(tfl_check_code(code, "content", "figure", ard = TRUE)), 0L)
  # made in the code itself: its own
  expect_identical(nrow(tfl_check_code(c("ard <- 1", "content <- ard"), "content", "figure")), 0L)
})

test_that("the report sheet takes data_code and content_code; type is table, listing or figure", {
  rep <- data.frame(output_id = c("T-1", "F-1"), type = c("table", "figure"),
                    data_code = c(NA, "df <- advs"), content_code = c("content <- 1", NA))
  sp <- tfl_table_spec(report = rep)
  expect_identical(sp$report$content_code[sp$report$output_id == "T-1"], "content <- 1")
  expect_identical(sp$report$data_code[sp$report$output_id == "F-1"], "df <- advs")
  expect_error(tfl_table_spec(report = data.frame(output_id = "U-1", type = "user")),
               "user is no longer a type")
  expect_error(tfl_table_spec(report = data.frame(output_id = "U-1", type = "chart")),
               "must be table, listing or figure")
  expect_true(all(c("data_code", "content_code") %in%
                    tfl_spec_columns()$column[tfl_spec_columns()$sheet == "report"]))
})

test_that("the rules are in the catalog", {
  cat <- tfl_review_rules()
  expect_true(all(sprintf("U0%d", 1:5) %in% cat$rule))
  expect_true(all(cat$area[cat$rule %in% sprintf("U0%d", 1:5)] == "program"))
})
