# A report's code lists applied to the data before its ARD is made.

test_that("code lists make the listed columns factors, their labels the ARD's values", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  sp <- exact_spec(list(method = "categorical", dataset = "ADSL",
                        population_id = "SAF", by = "ARM",
                        variables = "AGEGR1 | SEX"))
  cl <- data.frame(output_id = "T",
                   variable = c("AGEGR1", "AGEGR1", "AGEGR1", "AGEGR1", "SEX", "SEX"),
                   value = c("<65", "65-80", ">80", "Unknown", "M", "F"),
                   label = c("Under 65", NA, NA, NA, "Male", "Female"),
                   order = c("1", "2", "3", "4", "1", "2"))
  code <- paste(tfl_ard_code(sp, part = "body", codelists = cl), collapse = "\n")
  # the lists at the program's head, a value named by... -- what it is in
  # the ARD; put on the data the analysis reads
  expect_match(code, paste0("cl_agegr1 <- c(
  \"<65\" = \"Under 65\",
  \"65-80\" = \"65-80\",
",
                            "  \">80\" = \">80\",
  Unknown = \"Unknown\"
)"), fixed = TRUE)
  expect_match(code, "cl_sex <- c(M = \"Male\", F = \"Female\")", fixed = TRUE)
  expect_match(code, paste0("pop_saf <- adsl |>\n  filter(SAFFL == \"Y\") |>\n",
                            "  set_levels(AGEGR1 = cl_agegr1, SEX = cl_sex)"), fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE, codelists = cl))
  ag <- ard[ard$variable == "AGEGR1", ]
  lv <- unique(vapply(ag$variable_level, as.character, ""))
  expect_identical(lv, c("Under 65", "65-80", ">80", "Unknown"))   # in order, the empty one too
  n0 <- unlist(ag$stat[ag$stat_name == "n" &
                         vapply(ag$variable_level, as.character, "") == "Unknown"])
  expect_true(all(n0 == 0))
  sx <- unique(vapply(ard$variable_level[ard$variable == "SEX"], as.character, ""))
  expect_identical(sx, c("Male", "Female"))
  # a value the list does not have stops the program, and says which
  short <- cl[cl$value != "F", ]
  expect_error(suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE, codelists = short)),
               "SEX has values its code list does not: \"F\"", fixed = TRUE)
  # a value the population or the condition leave out need not be listed
  # (a screen failure's arm in a table of the safety population)
  sp2 <- exact_spec(list(method = "categorical", dataset = "ADSL",
                         population_id = "SAF", by = "ARM", where = "ARM != \"Placebo\"",
                         variables = "SEX"))
  cl2 <- data.frame(output_id = "T", variable = "ARM",
                    value = c("Xanomeline High Dose", "Xanomeline Low Dose"),
                    order = c("1", "2"))
  a2 <- suppressMessages(tfl_build_ard(sp2, dir = dir, save = FALSE, codelists = cl2))
  arms <- unique(vapply(a2$group1_level, function(v) as.character(v[[1L]]), ""))
  expect_identical(arms[!is.na(arms)][1:2], c("Xanomeline High Dose", "Xanomeline Low Dose"))
  expect_false("Placebo" %in% arms)
  # a condition on a listed variable the analysis reads is written in its
  # labels: the data have them by then
  sp3 <- exact_spec(list(method = "categorical", dataset = "ADSL",
                         population_id = "SAF", by = "ARM", where = "SEX == \"F\"",
                         variables = "SEX"))
  expect_error(tfl_ard_code(sp3, codelists = cl), "write \"Female\" for \"F\"", fixed = TRUE)
  # without code lists: as before
  plain <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE))
  expect_false(any(vapply(plain$variable_level[plain$variable == "AGEGR1"],
                          as.character, "") == "Unknown"))
  # the fingerprint changes with code lists, and with a label
  expect_identical(tfl_ard_spec_hash(sp, "T"), tfl_ard_spec_hash(sp, "T", codelists = NULL))
  expect_false(identical(tfl_ard_spec_hash(sp, "T"), tfl_ard_spec_hash(sp, "T", codelists = cl)))
  relabel <- cl
  relabel$label[5L] <- "Men"
  expect_false(identical(tfl_ard_spec_hash(sp, "T", codelists = cl),
                         tfl_ard_spec_hash(sp, "T", codelists = relabel)))
  # another report's rows are not this one's
  cl4 <- cl; cl4$output_id <- "T1"
  expect_null(.codelist_levels(cl4, "T"))
  expect_false(any(grepl("set_levels", tfl_ard_code(sp, part = "body", codelists = cl4),
                         fixed = TRUE)))
  # a code list is a report's: a row without one stops, and so does a
  # table without the column
  cl3 <- cl; cl3$output_id[2L] <- NA
  expect_error(tfl_ard_code(sp, codelists = cl3), "Row 2 of the code lists (AGEGR1 / 65-80)",
               fixed = TRUE)
  expect_error(tfl_ard_code(sp, codelists = cl[-1L]), "no `output_id` column", fixed = TRUE)
})

test_that("only the variables a report's analyses read; the study's program a part a report", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  sp <- exact_spec(list(method = "categorical", dataset = "ADSL",
                        population_id = "SAF", by = "ARM", variables = "AGEGR1"))
  b <- sp$analyses
  b$output_id <- "U"
  b$analysis_id <- "B"
  b$variables <- "SEX"
  sp$analyses <- rbind(sp$analyses, b)
  cl <- data.frame(output_id = c("T", "T", "T", "T", "U", "U"),
                   variable = c("AGEGR1", "AGEGR1", "AGEGR1", "SEX", "SEX", "SEX"),
                   value = c(">80", "<65", "65-80", "F", "M", "F"),
                   order = c("1", "2", "3", "1", "1", "2"))
  # one report's program: only what its analyses read (T reads no SEX)
  t1 <- tfl_ard_code(sp, output_id = "T", part = "body", codelists = cl)
  expect_true(any(grepl("cl_agegr1 <- c(\">80\", \"<65\", \"65-80\")", t1, fixed = TRUE)))
  expect_false(any(grepl("cl_sex", t1, fixed = TRUE)))
  # the fingerprint likewise: T's SEX row is not part of it
  expect_identical(tfl_ard_spec_hash(sp, "T", codelists = cl),
                   tfl_ard_spec_hash(sp, "T", codelists = cl[-4L, ]))
  expect_false(identical(tfl_ard_spec_hash(sp, "T", codelists = cl),
                         tfl_ard_spec_hash(sp, "T", codelists = cl[-1L, ])))
  # the study's: each part its code lists and its data read with them
  all <- tfl_ard_code(sp, save = FALSE, codelists = cl)
  u <- which(all == "# ---- U ----")
  expect_true(any(grepl("cl_agegr1 <-", all[seq_len(u)], fixed = TRUE)))
  expect_true(any(grepl("cl_sex <- c(\"M\", \"F\")", all[u:length(all)], fixed = TRUE)))
  expect_identical(sum(grepl("^adsl <- readRDS", all)), 2L)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE, codelists = cl))
  lv <- function(v) unique(vapply(ard$variable_level[ard$variable == v], as.character, ""))
  expect_identical(lv("AGEGR1"), c(">80", "<65", "65-80"))
  expect_identical(lv("SEX"), c("M", "F"))
  # each the same as its own program's
  for (o in c("T", "U")) {
    one <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE,
                                          output_id = o, codelists = cl))
    expect_equal(one, ard[ard$output_id == o, ], ignore_attr = TRUE)
  }
  # no code lists: the study's program reads the data once, as before
  plain <- tfl_ard_code(sp, save = FALSE)
  expect_identical(sum(grepl("^adsl <- readRDS", plain)), 1L)
})

test_that("variables$empty_levels = hide leaves out the values no record has", {
  skip_if_not_installed("cards")
  skip_if(utils::packageVersion("rtfreporter") < "0.8.2.9015")
  adsl <- data.frame(
    USUBJID = 1:6, TRT = rep(c("A", "B"), 3),
    RACE = factor(c("WHITE", "WHITE", "ASIAN", "WHITE", "ASIAN", "WHITE"),
                  levels = c("WHITE", "ASIAN", "OTHER")))
  d <- suppressMessages(rtfreporter::normalize_ard(
    cards::ard_tabulate(adsl, by = TRT, variables = RACE)))
  tab <- function(v) {
    sp <- tfl_table_spec(tables = data.frame(cols = "TRT"), variables = v)
    p <- tfl_table_plan(d, sp) |> rtfreporter::plan_cells(notes = FALSE)
    as.character(suppressMessages(rtfreporter::plan_apply(p, "table"))$label)
  }
  # the default (and `show`): every value of the code list has its row
  expect_identical(tab(data.frame(variable = "RACE", label = "Race")),
                   c("WHITE", "ASIAN", "OTHER"))
  expect_identical(tab(data.frame(variable = "RACE", empty_levels = "show")),
                   c("WHITE", "ASIAN", "OTHER"))
  hide <- data.frame(variable = "RACE", empty_levels = "hide")
  expect_identical(tab(hide), c("WHITE", "ASIAN"))
  # the code says so, and a plan gives the column back
  sp <- tfl_table_spec(tables = data.frame(cols = "TRT"), variables = hide)
  expect_true(any(grepl(".drop_empty = \"RACE\"", tfl_table_code(sp),
                        fixed = TRUE)))
  back <- suppressMessages(tfl_as_table_spec(tfl_table_plan(d, sp), "T1"))
  expect_identical(back$variables$empty_levels[back$variables$variable == "RACE"],
                   "hide")
  expect_error(tfl_table_spec(tables = data.frame(cols = "TRT"),
    variables = data.frame(variable = "RACE", empty_levels = "no")),
    "empty_levels")
})

test_that("variables$under puts a variable's rows under a level of another (plan_nest())", {
  skip_if_not_installed("cards")
  skip_if(!"plan_nest" %in% getNamespaceExports("rtfreporter"), "rtfreporter has no plan_nest()")
  adsl <- data.frame(
    USUBJID = paste0("S", 1:6), TRT = rep(c("A", "B"), 3),
    RACE = factor(c("WHITE", "WHITE", "ASIAN", "WHITE", "ASIAN", "WHITE"),
                  levels = c("WHITE", "ASIAN", "OTHER")),
    RSUB = factor(c(NA, NA, "CHINESE", NA, "JAPANESE", NA),
                  levels = c("CHINESE", "JAPANESE", "KOREAN")))
  d <- suppressMessages(rtfreporter::normalize_ard(
    cards::ard_tabulate(adsl, by = TRT, variables = c(RACE, RSUB), denominator = adsl)))
  v <- data.frame(variable = c("RACE", "RSUB"), label = c("Race", "Race Sub Asian"),
                  order = 1:2, under = c(NA, "RACE: ASIAN"))
  sp <- tfl_table_spec(tables = data.frame(cols = "TRT", rows = "group = variable"), variables = v)
  code <- tfl_table_code(sp)
  expect_true(any(grepl("plan_nest(RSUB = c(RACE = \"ASIAN\"))", code, fixed = TRUE)))
  p <- tfl_table_plan(d, sp) |> rtfreporter::plan_cells(notes = FALSE)
  tb <- suppressMessages(rtfreporter::plan_apply(p, "table"))
  pad <- strrep(intToUtf8(160L), 4L)
  expect_identical(as.character(tb$label),
                   c("WHITE", "ASIAN", paste0(pad, c("CHINESE", "JAPANESE", "KOREAN")), "OTHER"))
  expect_identical(unique(as.character(tb$group)), "Race")
  # a plan gives the column back
  back <- suppressMessages(tfl_as_table_spec(tfl_table_plan(d, sp), "T1"))
  expect_identical(back$variables$under[back$variables$variable == "RSUB"], "RACE: ASIAN")
  expect_error(tfl_table_spec(tables = data.frame(cols = "TRT"),
    variables = data.frame(variable = "RSUB", under = "RACE")), "variables[$]under")
})
