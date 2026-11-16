# The study's rounding is one options() call in the study's setup file:
# tfl_study_setup_code() writes it; option tflspec.study_setup leaves it out of
# the ARD and report setups (#782 step 1).

ssc_study <- function(rounding = NULL) {
  st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  if (!is.null(rounding)) {
    it <- tfl_item("plan_digits", list(rounding = rounding))
    it$from <- "defaults"
    st$defaults$content <- c(st$defaults$content, list(it))
  }
  st
}
ssc_ard_spec <- function(rounding = NULL) {
  sp <- exact_spec(list(method = "categorical", population_id = "SAF",
                        by = "TRT01A", variables = "SEX"))
  if (!is.null(rounding)) {
    sp$study[nrow(sp$study) + 1L, "key"] <- "rounding"
    sp$study[nrow(sp$study), "value"] <- rounding
  }
  sp
}
ssc_flag <- function(value) {
  old <- options(tflspec.study_setup = value)
  old
}

test_that("tfl_study_setup_code() gives the rounding as one options() call", {
  sas <- tfl_study_setup_code(ssc_study("sas"))
  expect_length(sas, 2L)
  expect_match(sas[1L], "^# the study's rounding")
  expect_identical(sas[2L], paste0("options(rtfreporter.rounding = \"sas\", ",
                                   "cards.round_type = \"round-half-up\")"))
  r <- tfl_study_setup_code(ssc_study("r"))
  expect_true(grepl("rtfreporter.rounding = \"r\"", r[2L], fixed = TRUE))
  expect_true(grepl("cards.round_type = \"round-to-even\"", r[2L], fixed = TRUE))
  expect_identical(tfl_study_setup_code(ssc_study()), character())
  # the sheets' spec
  wb <- function(v) tfl_table_spec(
    tables = data.frame(output_id = "T1", cols = "TRT01A", stringsAsFactors = FALSE),
    study = data.frame(key = "rounding", value = v, stringsAsFactors = FALSE))
  expect_identical(tfl_study_setup_code(wb("sas")), sas)
  expect_error(tfl_study_setup_code(1), "study spec")
})

test_that("tflspec.study_setup leaves the rounding out of the report setup", {
  old <- ssc_flag(TRUE)
  on.exit(options(old), add = TRUE)
  setup <- tfl_report_setup_code(ssc_study("sas"))
  expect_false(any(grepl("rounding|round_type", setup)))
  # the other options stay
  st <- ssc_study("sas")
  st$options <- list(rtfreporter.font = "Arial")
  setup <- tfl_report_setup_code(st)
  expect_true(any(grepl("rtfreporter.font", setup, fixed = TRUE)))
  expect_false(any(grepl("rounding|round_type", setup)))
  # the workbook's route
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T1", cols = "TRT01A", stringsAsFactors = FALSE),
    study = data.frame(key = "rounding", value = "sas", stringsAsFactors = FALSE))
  expect_false(any(grepl("rounding|round_type", tfl_report_setup_code(sp))))
  # a report's rounding that is the study's stays out of its program
  out <- tfl_output("T-9", "table", common = list(population = "SAF"), content = list(
    tfl_item("table_plan", list(cols = "TRT01A")),
    tfl_item("plan_digits", list(AGE = 0))))
  code <- tfl_content_code(tfl_resolve_output(out, ssc_study("sas")))
  expect_false(any(grepl("rounding", code, fixed = TRUE)))
})

test_that("tflspec.study_setup leaves cards' rounding out of the ARD setup", {
  skip_if_not_installed("cards")
  old <- ssc_flag(TRUE)
  on.exit(options(old), add = TRUE)
  setup <- tfl_ard_code(ssc_ard_spec("sas"), part = "setup")
  expect_false(any(grepl("round_type", setup, fixed = TRUE)))
  options(tflspec.study_setup = NULL)
  expect_true(any(grepl("round_type", tfl_ard_code(ssc_ard_spec("sas"), part = "setup"),
                        fixed = TRUE)))
})

test_that("without the option, or FALSE, the code is as it was", {
  skip_if_not_installed("cards")
  old <- options(tflspec.study_setup = NULL)
  on.exit(options(old), add = TRUE)
  st <- ssc_study("sas")
  a_rep <- tfl_report_setup_code(st)
  a_ard <- tfl_ard_code(ssc_ard_spec("r"), part = "setup")
  a_ard_all <- tfl_ard_code(ssc_ard_spec("r"))
  options(tflspec.study_setup = FALSE)
  expect_identical(tfl_report_setup_code(st), a_rep)
  expect_identical(tfl_ard_code(ssc_ard_spec("r"), part = "setup"), a_ard)
  expect_identical(tfl_ard_code(ssc_ard_spec("r")), a_ard_all)
  expect_true(any(grepl("rtfreporter.rounding = \"sas\"", a_rep, fixed = TRUE)))
  expect_true(any(grepl("round-to-even", a_ard, fixed = TRUE)))
})

# ---- the study's input data (#782 step 2) -----------------------------------

ssc_yaml_study <- function(data_lines) {
  d <- tempfile("ssc")
  dir.create(d)
  file.copy(system.file("extdata", "spec-yaml", package = "tflspec"), d, recursive = TRUE)
  sp <- file.path(d, "spec-yaml")
  writeLines(data_lines, file.path(sp, "data.yml"))
  tfl_read_study_spec(sp)
}
ssc_data_yaml <- c("datasets:",
                   "  - dataset: ADAE", "    path: data/adam/adae.rds",
                   "    derive: AEDUR = AENDY - ASTDY + 1",
                   "  - dataset: ADSL", "    path: data/adam/adsl.xpt")
ssc_sheets <- function(rounding = NULL) {
  tfl_ard_spec(list(
    study = data.frame(key = c("id", if (!is.null(rounding)) "rounding"),
                       value = c("USUBJID", rounding)),
    datasets = data.frame(dataset = c("ADAE", "ADSL"), path = c("adae.rds", "adsl.rds"),
                          derive = c("AEDUR = AENDY - ASTDY + 1", NA)),
    populations = data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analysis_data = data.frame(output_id = "F-1", data_id = "adae_saf", from = "ADAE",
                               population_id = "SAF"),
    analyses = data.frame(output_id = "T-1", analysis_id = "A1", method = "categorical",
                          dataset = "ADSL", population_id = "SAF", by = "TRT01A",
                          variables = "SEX")))
}

# a long delayedAssign() as the setup writes it: an argument a line
ssc_wrapped <- function(name, expr, env) {
  c("delayedAssign(", sprintf("  \"%s\",", name), paste0("  ", expr, ","),
    paste0("  assign.env = ", env), ")")
}
# the lines one after another in the code
ssc_has <- function(code, lines) {
  grepl(paste(lines, collapse = "\n"), paste(code, collapse = "\n"), fixed = TRUE)
}

# the lines of a study with no code lists, the adam_org lines given
ssc_no_lists <- function(...) {
  c("# ADaM levels: codelists", "",
    "# the study's input data, read when first used: study$adam_org (the ADaM as read,",
    "# with the catalog's derive), study$adam (blanks as NA; the study has no code lists)",
    "study <- list(adam_org = new.env(), adam = new.env())", c(...),
    "delayedAssign(\"adsl\", blank_as_na(study$adam_org$adsl), assign.env = study$adam)",
    "delayedAssign(\"adae\", blank_as_na(study$adam_org$adae), assign.env = study$adam)",
    "message(\"The study has no code lists: study$adam is study$adam_org with blanks as NA.\")",
    "",
    "# (until the report programs read study$adam: the objects they use now)",
    "delayedAssign(\"adsl\", study$adam_org$adsl)",
    "delayedAssign(\"adae\", study$adam_org$adae)")
}

test_that("tfl_study_setup_code() reads the study's datasets: the YAML study", {
  st <- ssc_yaml_study(ssc_data_yaml)
  code <- tfl_study_setup_code(st)
  expect_identical(code, ssc_no_lists(
    ssc_wrapped("adsl", "haven::read_xpt(\"data/adam/adsl.xpt\")", "study$adam_org"),
    ssc_wrapped("adae", "readRDS(\"data/adam/adae.rds\") |> dplyr::mutate(AEDUR = AENDY - ASTDY + 1)",
                "study$adam_org")))
  expect_true(all(nchar(code) <= 100L))
  # the rounding first, as it was
  st$study$settings$rounding <- "sas"
  both <- tfl_study_setup_code(st)
  # (the setting's line first, then the rounding)
  expect_identical(both[1:2], code[1:2])
  expect_identical(both[3:4], tfl_study_setup_code(ssc_study("sas"))[1:2])
  expect_identical(both[5L], "")
  expect_identical(both[-(1:5)], code[-(1:2)])
  # the folder variables of tflspec.paths
  old <- options(tflspec.paths = c(path_adam = "data/adam"))
  on.exit(options(old), add = TRUE)
  expect_true(ssc_has(tfl_study_setup_code(st), ssc_wrapped(
    "adsl", "haven::read_xpt(file.path(path_adam, \"adsl.xpt\"))", "study$adam_org")))
  options(old)
  # no dataset, no rounding: nothing
  expect_identical(tfl_study_setup_code(ssc_study()), character())
})

test_that("tfl_study_setup_code() reads the study's datasets: the sheets", {
  code <- tfl_study_setup_code(ssc_sheets())
  expect_identical(code, ssc_no_lists(
    "delayedAssign(\"adsl\", readRDS(\"adsl.rds\"), assign.env = study$adam_org)",
    ssc_wrapped("adae", "readRDS(\"adae.rds\") |> dplyr::mutate(AEDUR = AENDY - ASTDY + 1)",
                "study$adam_org")))
  expect_identical(tfl_study_setup_code(ssc_sheets("r"))[-(3:5)], code)
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T1", cols = "TRT01A", stringsAsFactors = FALSE),
    study = data.frame(key = "id", value = "USUBJID", stringsAsFactors = FALSE))
  expect_identical(tfl_study_setup_code(sp), character())
})

test_that("tflspec.study_setup: ARD, listing and figure programs start from the objects", {
  x <- ssc_sheets()
  cat <- x$datasets
  sp <- tfl_listing_spec(data.frame(output_id = "L-1", dataset = "ADAE", sort = "USUBJID"),
                         data.frame(output_id = "L-1", vars = "USUBJID"))
  unset <- list(ard = tfl_ard_code(x), data = tfl_ard_code(x, "F-1", part = "data"),
                listing = tfl_listing_code(sp, "L-1", cat))
  expect_true(any(grepl("readRDS(", unset$ard, fixed = TRUE)))
  expect_true(any(grepl("readRDS(", unset$listing, fixed = TRUE)))
  old <- ssc_flag(TRUE)
  on.exit(options(old), add = TRUE)
  on <- list(ard = tfl_ard_code(x), data = tfl_ard_code(x, "F-1", part = "data"),
             listing = tfl_listing_code(sp, "L-1", cat))
  for (k in names(on)) {
    expect_false(any(grepl("^[a-z_.]+ <- (readRDS|haven::read_xpt)[(]", on[[k]])), info = k)
  }
  expect_true(any(grepl("^adsl <- order_levels\\(adsl", on$ard)))
  expect_false(any(grepl("^adsl <- adsl$", on$ard)))
  expect_true(any(grepl("^adae <- order_levels\\(adae", on$data)))
  expect_false(any(grepl("mutate(AEDUR", on$ard, fixed = TRUE)))
  expect_true(any(grepl("^adae_saf <- ", on$data)))
  expect_true(any(grepl("^data <- adae", on$listing)))
  expect_false(any(grepl("^adae <- ", on$listing)))
  # off again: as it was
  options(tflspec.study_setup = NULL)
  expect_identical(tfl_ard_code(x), unset$ard)
  expect_identical(tfl_ard_code(x, "F-1", part = "data"), unset$data)
  expect_identical(tfl_listing_code(sp, "L-1", cat), unset$listing)
})

test_that("tflspec's own runs read the files themselves, the option or not", {
  skip_if_not_installed("cards")
  d <- tempfile("ssc")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  adsl <- data.frame(USUBJID = paste0("S", 1:4), SAFFL = "Y", TRT01A = c("A", "A", "B", "B"),
                     SEX = c("F", "M", "F", "M"))
  saveRDS(adsl, file.path(d, "adsl.rds"))
  x <- tfl_ard_spec(list(
    datasets = data.frame(dataset = "ADSL", path = "adsl.rds"),
    populations = data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analyses = data.frame(output_id = "T-1", analysis_id = "A1", method = "categorical",
                          dataset = "ADSL", population_id = "SAF", by = "TRT01A",
                          variables = "SEX")))
  old <- ssc_flag(TRUE)
  on.exit(options(old), add = TRUE)
  ard <- suppressMessages(tfl_build_ard(x, dir = d, save = FALSE))
  expect_true(nrow(ard) > 0L)
  expect_true(isTRUE(getOption("tflspec.study_setup")))
})

# ---- study$adam_org and study$adam ------------------------------------------

ssc_lists_study <- function() {
  st <- ssc_yaml_study(c("datasets:",
                         "  - dataset: ADSL", "    path: adsl.rds",
                         "  - dataset: ADAE", "    path: adae.rds",
                         "    derive: AEDUR = AENDY - ASTDY + 1"))
  st$defaults$codelists <- list(
    SEX = list(list(value = "F", label = "Female"), list(value = "M", label = "Male")),
    RACE = c("WHITE", "ASIAN"))
  st
}

test_that("the setup writes the study's code lists and both environments", {
  code <- tfl_study_setup_code(ssc_lists_study())
  expect_identical(code[1:6], c(
    "# ADaM levels: codelists", "",
    "# the study's code lists (spec/defaults.yml codelists:)",
    "cl_sex <- c(F = \"Female\", M = \"Male\")",
    "cl_race <- c(\"WHITE\", \"ASIAN\")", ""))
  expect_true("study <- list(adam_org = new.env(), adam = new.env())" %in% code)
  expect_true("delayedAssign(\"adsl\", readRDS(\"adsl.rds\"), assign.env = study$adam_org)" %in% code)
  expect_true(ssc_has(code, ssc_wrapped(
    "adae", "readRDS(\"adae.rds\") |> dplyr::mutate(AEDUR = AENDY - ASTDY + 1)", "study$adam_org")))
  # every dataset's adam line passes all the lists
  # (blanks as NA first, then the lists)
  for (ds in c("adsl", "adae")) {
    expect_true(ssc_has(code, c(
      "delayedAssign(", sprintf("  \"%s\",", ds), sprintf("  study$adam_org$%s |>", ds),
      "    blank_as_na() |>", "    set_levels(SEX = cl_sex, RACE = cl_race),",
      "  assign.env = study$adam", ")")), info = ds)
  }
  expect_true(all(nchar(code) <= 100L))
  # the objects the programs use now
  expect_identical(utils::tail(code, 3L), c(
    "# (until the report programs read study$adam: the objects they use now)",
    "delayedAssign(\"adsl\", study$adam_org$adsl)",
    "delayedAssign(\"adae\", study$adam_org$adae)"))
  expect_false(any(grepl("message(", code, fixed = TRUE)))
  expect_no_error(parse(text = code))
})

test_that("the setup of a study with no code lists makes study$adam study$adam_org", {
  code <- tfl_study_setup_code(ssc_yaml_study(ssc_data_yaml))
  expect_false(any(grepl("^cl_", code)))
  expect_false(any(grepl("set_levels", code, fixed = TRUE)))
  expect_true("delayedAssign(\"adsl\", blank_as_na(study$adam_org$adsl), assign.env = study$adam)" %in% code)
  expect_true(paste0("message(\"The study has no code lists: study$adam is study$adam_org ",
                     "with blanks as NA.\")") %in% code)
  expect_false("study$adam <- study$adam_org" %in% code)
})

test_that("the setup's code reads each dataset when first used", {
  d <- tempfile("ssc")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  st <- ssc_yaml_study(c("datasets:",
                         "  - dataset: ADSL", "    path: data/adam/adsl.rds",
                         "  - dataset: ADAE", "    path: data/adam/adae.rds"))
  st$defaults$codelists <- list(SEX = list(list(value = "F", label = "Female"),
                                           list(value = "M", label = "Male")))
  old <- options(tflspec.paths = c(path_adam = "data/adam"))
  on.exit(options(old), add = TRUE)
  code <- tfl_study_setup_code(st)
  options(old)
  env <- new.env(parent = globalenv())
  env$path_adam <- d
  eval(parse(text = tfl_helpers_code()), env)
  # the files are not there when the setup runs: nothing is read yet
  expect_no_error(eval(parse(text = code), envir = env))
  expect_identical(sort(ls(env$study$adam_org)), c("adae", "adsl"))
  expect_identical(sort(ls(env$study$adam)), c("adae", "adsl"))
  saveRDS(data.frame(USUBJID = c("S1", "S2", "S3"), SEX = c("M", "F", "M"),
                     stringsAsFactors = FALSE), file.path(d, "adsl.rds"))
  expect_identical(class(env$study$adam$adsl$SEX), "factor")
  expect_identical(levels(env$study$adam$adsl$SEX), c("F", "M"))
  expect_identical(as.character(env$study$adam$adsl$SEX), c("M", "F", "M"))
  expect_identical(class(env$study$adam_org$adsl$SEX), "character")
  expect_identical(env$adsl, env$study$adam_org$adsl)
  # adae has no file: it stops only when it is used
  expect_error(suppressWarnings(env$study$adam_org$adae))
  expect_error(suppressWarnings(env$adae))
})

test_that("study$adam has the blanks as NA, study$adam_org as read (with and without lists)", {
  d <- tempfile("ssc")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  adsl <- data.frame(USUBJID = c("S1", "S2", "S3"), SEX = c("M", "", "F"),
                     RACE = c("", "ASIAN", "WHITE"), AGE = c(30, NA, 40),
                     GRP = factor(c("a", "", "b"), levels = c("b", "", "a")),
                     stringsAsFactors = FALSE)
  saveRDS(adsl, file.path(d, "adsl.rds"))
  for (lists in c(TRUE, FALSE)) {
    st <- ssc_yaml_study(c("datasets:", "  - dataset: ADSL", "    path: data/adam/adsl.rds"))
    if (lists) st$defaults$codelists <- list(SEX = c("F", "M"))
    old <- options(tflspec.paths = c(path_adam = "data/adam"))
    code <- tfl_study_setup_code(st)
    options(old)
    env <- new.env(parent = globalenv())
    env$path_adam <- d
    eval(parse(text = tfl_helpers_code()), env)
    suppressMessages(eval(parse(text = code), envir = env))
    a <- env$study$adam$adsl
    expect_identical(a$RACE, c(NA, "ASIAN", "WHITE"))
    expect_identical(as.character(a$SEX), c("M", NA, "F"))
    expect_identical(class(a$SEX), if (lists) "factor" else "character")
    expect_identical(a$AGE, adsl$AGE)
    expect_identical(levels(a$GRP), c("b", "a"))
    expect_identical(as.character(a$GRP), c("a", NA, "b"))
    # as read
    expect_identical(env$study$adam_org$adsl, adsl)
    expect_identical(env$adsl, adsl)
  }
})

test_that("blank_as_na() makes a blank of a character column or a factor NA, leaves the rest", {
  env <- new.env(parent = globalenv())
  eval(parse(text = tfl_helpers_code()), env)
  x <- data.frame(a = c("x", "", NA), n = c(1, 2, 3), l = c(TRUE, FALSE, NA),
                  f = factor(c("u", "", "v")), o = factor(c("lo", "", "hi"),
                                                          levels = c("", "lo", "hi"), ordered = TRUE),
                  s = c(" ", "y", ""), stringsAsFactors = FALSE)
  y <- env$blank_as_na(x)
  expect_identical(y$a, c("x", NA, NA))
  expect_identical(y$s, c(" ", "y", NA))
  expect_identical(y[c("n", "l")], x[c("n", "l")])
  expect_identical(levels(y$f), c("u", "v"))
  expect_identical(as.character(y$f), c("u", NA, "v"))
  expect_identical(levels(y$o), c("lo", "hi"))
  expect_true(is.ordered(y$o))
  expect_identical(env$blank_as_na(y), y)
})

# ---- settings: adam_levels (codelists | none) --------------------------------

ssc_levels_study <- function(value = NULL) {
  st <- ssc_lists_study()
  st$study$settings$adam_levels <- value
  st
}

test_that("adam_levels: the setup says it first; codelists is the default", {
  code <- tfl_study_setup_code(ssc_levels_study())
  expect_identical(code[1L], "# ADaM levels: codelists")
  expect_identical(tfl_study_setup_code(ssc_levels_study("codelists")), code)
  expect_true("cl_sex <- c(F = \"Female\", M = \"Male\")" %in% code)
  expect_true(any(grepl("set_levels(SEX = cl_sex, RACE = cl_race)", code, fixed = TRUE)))
})

test_that("adam_levels: none -- no code lists, study$adam is blank_as_na() only", {
  code <- tfl_study_setup_code(ssc_levels_study("none"))
  expect_identical(code[1:2], c("# ADaM levels: none", ""))
  expect_false(any(grepl("^cl_", code)))
  expect_false(any(grepl("set_levels", code, fixed = TRUE)))
  expect_false(any(grepl("message(", code, fixed = TRUE)))
  expect_true("delayedAssign(\"adsl\", blank_as_na(study$adam_org$adsl), assign.env = study$adam)" %in% code)
  expect_true("delayedAssign(\"adsl\", readRDS(\"adsl.rds\"), assign.env = study$adam_org)" %in% code)
  expect_true(any(grepl("adam_levels: none", code[-1L], fixed = TRUE)))
  expect_no_error(parse(text = code))
  # with the rounding: the setting, then the rounding, then the data
  st <- ssc_levels_study("none")
  st$study$settings$rounding <- "sas"
  both <- tfl_study_setup_code(st)
  expect_identical(both[1:2], c("# ADaM levels: none", ""))
  expect_match(both[3L], "^# the study's rounding")
  # the sheets: the study sheet's key
  sp <- ssc_sheets()
  sp$study <- rbind(sp$study, data.frame(key = "adam_levels", value = "none"))
  expect_identical(tfl_study_setup_code(sp)[1L], "# ADaM levels: none")
  # another value stops
  expect_error(tfl_study_setup_code(ssc_levels_study("define")),
               "adam_levels is codelists or none; got \"define\"")
})

test_that("adam_levels: none -- the blanks are NA in study$adam all the same", {
  d <- tempfile("ssc")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  adsl <- data.frame(USUBJID = c("S1", "S2", "S3"), SEX = c("M", "", "F"),
                     RACE = c("", "ASIAN", "WHITE"), stringsAsFactors = FALSE)
  saveRDS(adsl, file.path(d, "adsl.rds"))
  st <- ssc_yaml_study(c("datasets:", "  - dataset: ADSL", "    path: data/adam/adsl.rds"))
  st$defaults$codelists <- list(SEX = c("F", "M"))
  st$study$settings$adam_levels <- "none"
  old <- options(tflspec.paths = c(path_adam = "data/adam"))
  code <- tfl_study_setup_code(st)
  options(old)
  env <- new.env(parent = globalenv())
  env$path_adam <- d
  eval(parse(text = tfl_helpers_code()), env)
  eval(parse(text = code), envir = env)
  a <- env$study$adam$adsl
  expect_identical(a$SEX, c("M", NA, "F"))
  expect_identical(a$RACE, c(NA, "ASIAN", "WHITE"))
  expect_false(exists("cl_sex", envir = env, inherits = FALSE))
  expect_identical(env$study$adam_org$adsl, adsl)
})

test_that("adam_levels is part of the ARD's fingerprint", {
  sp <- ssc_ard_spec()
  with_key <- function(v) {
    x <- sp
    x$study <- rbind(x$study, data.frame(key = "adam_levels", value = v))
    x
  }
  h <- tfl_ard_spec_hash(sp, "T")
  # codelists, the default, leaves it as it was; none changes it
  expect_identical(tfl_ard_spec_hash(with_key("codelists"), "T"), h)
  expect_false(identical(tfl_ard_spec_hash(with_key("none"), "T"), h))
  expect_error(tfl_ard_spec_hash(with_key("x"), "T"), "codelists or none")
  # the ARD's study sheet of a YAML study says none, and only none
  o <- tfl_output("T-9", "table", common = list(population = "SAF"),
                  content = list(tfl_item("table_plan", list(cols = "TRT01A"))))
  outs <- list(`T-9` = o)
  ard_key <- function(st) {
    s <- tfl_sheets_from_spec(st, outs)$ard$study
    s$value[s$key %in% "adam_levels"]
  }
  expect_identical(ard_key(ssc_levels_study()), character())
  expect_identical(ard_key(ssc_levels_study("codelists")), character())
  expect_identical(ard_key(ssc_levels_study("none")), "none")
})

test_that("adam_levels: none -- a report's own levels work, a study code list by name stops", {
  st <- ssc_levels_study("none")
  item <- function(...) tfl_item("table_plan", list(cols = "TRT01A"))
  # levels written as values, and the report's own list: fine
  own <- tfl_output("T-1", "table", common = list(population = "SAF"),
                    data = list(list(id = "adsl_saf", from = "ADSL",
                                     derive = list(SEXF = "factor(SEX, levels = c(\"F\", \"M\"))"))),
                    content = list(item()))
  expect_no_error(tfl_resolve_output(own, st))
  mine <- tfl_output("T-2", "table", common = list(population = "SAF"),
                     codelists = list(SEX = c("F", "M")),
                     data = list(list(id = "adsl_saf", from = "ADSL",
                                      derive = list(SEXF = "factor(SEX, levels = cl_sex)"))),
                     content = list(item()))
  expect_no_error(tfl_resolve_output(mine, st))
  # the study's list by name
  named <- tfl_output("T-3", "table", common = list(population = "SAF"),
                      data = list(list(id = "adsl_saf", from = "ADSL",
                                       derive = list(SEXF = "factor(SEX, levels = cl_sex)"))),
                      content = list(item()))
  msg <- "T-3: adam_levels: none -- the study code list SEX (cl_sex) is not read"
  expect_error(tfl_resolve_output(named, st), msg, fixed = TRUE)
  expect_error(tfl_report_codelists(st, outputs = list(`T-3` = named)), msg, fixed = TRUE)
  # under codelists it is read
  expect_no_error(tfl_resolve_output(named, ssc_levels_study("codelists")))
})
