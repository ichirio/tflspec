# The study ARD as JSON / YAML / XPT, and back.

io_ard <- function() {
  adsl <- as.data.frame(cards::ADSL)
  adsl$AGEGR1 <- factor(adsl$AGEGR1, levels = c("<65", "65-80", ">80", "Unknown"))
  two <- adsl[adsl$ARM != "Xanomeline Low Dose", ]
  a <- dplyr::bind_rows(
    cards::ard_tabulate(adsl, by = ARM, variables = AGEGR1),
    cards::ard_summary(adsl, by = ARM, variables = AGE),
    cardx::ard_stats_t_test(two, by = ARM, variables = AGE))
  dplyr::mutate(a, output_id = "T1", analysis_id = "A1", population_id = "SAF",
                .before = 1L)
}

# what must come back: every column but the formatting functions
io_same <- function(a, b) {
  k <- setdiff(names(a), "fmt_fun")
  expect_identical(names(b)[match(k, names(b))], k)
  for (cn in k) {
    x <- a[[cn]]; y <- b[[cn]]
    if (is.list(x)) {
      for (i in seq_along(x)) {
        expect_equal(unname(y[[i]]), unname(x[[i]]), label = paste(cn, i))
      }
    } else expect_identical(y, x, label = cn)
  }
}

test_that("the rows shape of JSON and YAML gives the ARD back, levels and all", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  a <- io_ard()
  for (ext in c("json", "yaml")) {
    f <- tempfile(fileext = paste0(".", ext))
    expect_warning(tfl_write_ard(a, f), "formatting functions")
    b <- tfl_read_ard(f)
    expect_s3_class(b, "card")
    io_same(as.data.frame(a), as.data.frame(b))
    # the levels in their order, the level no subject has included
    v <- b$variable_level[b$variable == "AGEGR1"][[1L]]
    expect_identical(levels(v), c("<65", "65-80", ">80", "Unknown"))
  }
  f <- tempfile(fileext = ".json")
  suppressWarnings(tfl_write_ard(a, f))
  js <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_identical(js$meta$format, "tflspec ARD")
  expect_identical(unlist(js$levels$AGEGR1), c("<65", "65-80", ">80", "Unknown"))
})

test_that("the nested shape is cards' own, and says what it loses", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  a <- io_ard()
  f <- tempfile(fileext = ".json")
  expect_warning(tfl_write_ard(a, f, shape = "nested"), "levels' order")
  js <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_true("AGEGR1" %in% names(js$variable))
  expect_error(tfl_read_ard(f), "cannot be read back")
})

test_that("XPT holds a flat table and says what it loses", {
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("haven")
  a <- io_ard()
  f <- tempfile(fileext = ".xpt")
  w <- tryCatch(tfl_write_ard(a, f), warning = function(w) conditionMessage(w))
  expect_match(w, "factors and their levels")
  expect_match(w, "the type of stat")             # the t test's method is text
  suppressWarnings(tfl_write_ard(a, f))
  b <- tfl_read_ard(f)
  expect_identical(nrow(b), nrow(a))
  expect_identical(b$stat_name, a$stat_name)
  f5 <- tempfile(fileext = ".xpt")
  suppressWarnings(tfl_write_ard(a, f5, xpt_version = 5))
  x <- haven::read_xpt(f5)
  expect_true(all(nchar(names(x)) <= 8L))
  expect_true(all(c("GRP1LVL", "VARLVL", "STATNAME") %in% names(x)))
  b5 <- tfl_read_ard(f5)
  expect_identical(b5$stat_name, a$stat_name)
  expect_identical(unlist(b5$group1_level), unlist(lapply(a$group1_level, as.character)))
})

test_that("tfl_write_ard() wants an ARD and a known format", {
  expect_error(tfl_write_ard(data.frame(x = 1), tempfile(fileext = ".json")),
               "must be an ARD")
  skip_if_not_installed("cards")
  expect_error(tfl_write_ard(cards::ard_summary(cards::ADSL, variables = AGE),
                             tempfile(fileext = ".txt")), "not known")
})
