# The data steps' objects (0.0.24.9101): every step's `in` and `out`, a
# function step, an ARD analysis step

io_design <- function(data, layers = list(list(layer = "point", x = "AVAL", y = "CHG"))) {
  tfl_fig_design(data = data, layers = layers)
}
code_of <- function(d) paste(tfl_fig_design_code(d), collapse = "\n")

test_that("no in or out: the steps' objects as before", {
  d <- io_design(list(list(step = "read", dataset = "ADTTE"),
                      list(step = "param", value = "OS"),
                      list(step = "survfit", time = "AVAL", censor = "CNSR")))
  o <- tfl_fig_objects(d)
  expect_identical(o$out, c("df", "df", "fit"))
  expect_identical(o[["in"]], c(NA, "df", "df"))
  # (the program names df after its data: adtte_os)
  expect_match(code_of(d), "fit <- survfit2(Surv(AVAL, CNSR == 0) ~ 1, data = adtte_os)", fixed = TRUE)
})

test_that("a step into another object: an assignment of its own; a named step reads it", {
  d <- io_design(list(list(step = "read", dataset = "ADTTE"),
                      list(step = "param", value = "OS"),
                      list(step = "filter", expr = "AVAL > 0", out = "df_ev"),
                      list(step = "survfit", time = "AVAL", censor = "CNSR", `in` = "df_ev")))
  code <- code_of(d)
  expect_match(code, "adtte_os <- adtte |>\n  filter(PARAMCD == \"OS\")", fixed = TRUE)
  expect_match(code, "df_ev <- adtte_os |>\n  filter(AVAL > 0)", fixed = TRUE)
  expect_match(code, "data = df_ev)", fixed = TRUE)
  # the steps after it go on df_ev (the object above them)
  d2 <- io_design(c(d$data, list(list(step = "derive", variable = "M", expr = "AVAL / 30"))))
  expect_identical(tfl_fig_objects(d2)[["in"]][5L], "fit")
  d3 <- io_design(list(list(step = "read", dataset = "ADTTE"),
                       list(step = "filter", expr = "AVAL > 0", out = "df_ev"),
                       list(step = "derive", variable = "M", expr = "AVAL / 30")))
  expect_match(code_of(d3), "df_ev <- df |>\n  filter(AVAL > 0) |>\n  mutate(M = AVAL / 30)", fixed = TRUE)
})

test_that("a second read makes df2; df is kept", {
  d <- io_design(list(list(step = "read", dataset = "ADTTE"),
                      list(step = "read", dataset = "ADSL"),
                      list(step = "flag", variable = "SAFFL")))
  expect_identical(tfl_fig_objects(d)$out, c("df", "df2", "df2"))
  code <- code_of(d)
  expect_match(code, "adtte_saf <- adtte", fixed = TRUE)
  expect_match(code, "df2 <- adsl |>\n  filter(SAFFL == \"Y\")", fixed = TRUE)
})

test_that("any function: its arguments as written, the object it reads as its data", {
  d <- io_design(list(list(step = "read", dataset = "ADTTE"),
                      list(step = "function", fn = "survival::survfit", name = "fit2",
                           args = "formula = Surv(AVAL, CNSR == 0) ~ TRT01A | conf.type = \"log-log\"")))
  expect_match(code_of(d),
               "fit2 <- survival::survfit(formula = Surv(AVAL, CNSR == 0) ~ TRT01A, conf.type = \"log-log\", data = df)",
               fixed = TRUE)
  expect_identical(nrow(tfl_check_fig_design(d)), 0L)
})

test_that("code with an out: the next steps go on it; a dataset by name is read", {
  d <- io_design(list(list(step = "read", dataset = "ADTTE"),
                      list(step = "code", code = "x <- df[1:10, ]", out = "x"),
                      list(step = "filter", expr = "AVAL > 0"),
                      list(step = "flag", variable = "SAFFL", `in` = "adsl", out = "saf")))
  code <- tfl_fig_design_code(d)
  txt <- paste(code, collapse = "\n")
  expect_match(txt, "x <- x |>\n  filter(AVAL > 0)", fixed = TRUE)
  expect_match(txt, "saf <- adsl |>\n  filter(SAFFL == \"Y\")", fixed = TRUE)
  expect_true("ADSL" %in% attr(code, "reads"))
})

test_that("an ARD analysis step: no line of the program; the program reads the ARD", {
  d <- io_design(list(list(step = "read", dataset = "ADTTE"),
                      list(step = "ard", analysis_id = "KM", fn = "cardx::ard_survival_survfit",
                           `in` = "adtte_saf", args = "times = c(90, 180)")))
  code <- tfl_fig_design_code(d)
  expect_false(any(grepl("ard_survival_survfit", code, fixed = TRUE)))
  expect_true(attr(code, "ard"))
  expect_identical(nrow(tfl_check_fig_design(d)), 0L)
})

test_that("the check: an object read that no step made before", {
  d <- io_design(list(list(step = "read", dataset = "ADTTE"),
                      list(step = "filter", expr = "AVAL > 0", `in` = "nowhere")))
  ch <- tfl_check_fig_design(d)
  expect_true(any(ch$field == "in" & grepl("no object named nowhere", ch$problem)))
  # a dataset by its name is not a problem
  d2 <- io_design(list(list(step = "read", dataset = "ADTTE"),
                       list(step = "flag", variable = "SAFFL", `in` = "adsl", out = "saf")))
  expect_false(any(tfl_check_fig_design(d2)$field == "in"))
  # the columns follow the object: df_ev has df's
  adam <- list(ADTTE = data.frame(USUBJID = "1", AVAL = 1, CNSR = 0, PARAMCD = "OS"))
  d3 <- io_design(list(list(step = "read", dataset = "ADTTE"),
                       list(step = "filter", expr = "AVAL > 0", out = "df_ev"),
                       list(step = "survfit", time = "AVAL", censor = "NOPE", `in` = "df_ev")))
  ch3 <- tfl_check_fig_design(d3, adam = adam)
  expect_true(any(grepl("no variable NOPE", ch3$problem)))
})
