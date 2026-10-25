# An analysis in each level of condition-defined groups (the ARS's
# grouping factors): `groups` on the analysis, the levels on the `groups`
# sheet (data.yml `groups:`)

grp_sheet <- function() {
  exact_sheet(list(
    list(group_id = "ALL", label = "All", level = "", where = NA),
    list(group_id = "AGE", label = "Age Group", level = "<65 years", where = "AGE < 65"),
    list(group_id = "AGE", label = "Age Group", level = ">=65 years", where = "AGE >= 65"),
    # (levels may overlap)
    list(group_id = "AGE", label = "Age Group", level = ">=75 years", where = "AGE >= 75")),
    .ard_spec_sheets$groups)
}
grp_spec <- function(r, groups = grp_sheet()) {
  r0 <- r
  r0$groups <- NULL
  s <- unclass(exact_spec(r0))
  s$analyses$groups <- r$groups %||% NA_character_
  s$groups <- groups
  tfl_ard_spec(s)
}

test_that("an analysis in each level of its groups: group2 / group2_level, the definition's order", {
  skip_if_not_installed("cards")
  adam <- exact_data()
  dir <- exact_dir(adam)
  on.exit(unlink(dir, recursive = TRUE))
  r <- list(method = "categorical", population_id = "SAF", by = "TRTA", variables = "SEX",
            groups = "ALL | AGE")
  sp <- grp_spec(r)
  code <- tfl_ard_code(sp, output_id = "T", save = FALSE)
  txt <- paste(code, collapse = "\n")
  # the conditions as a list, the loop over them, the analysis's own code inside
  expect_match(txt, "groups_ard_a <- list(", fixed = TRUE)
  expect_match(txt, "\"Age Group\" = list(", fixed = TRUE)
  expect_match(txt, "\"<65 years\" = \"AGE < 65\"", fixed = TRUE)
  # (the whole group's level, no name: "NA" in the list, an empty level in the ARD)
  expect_match(txt, "\"NA\" = \"TRUE\"", fixed = TRUE)
  expect_match(txt, "rlang::parse_expr(groups_ard_a[[group]][[level]])", fixed = TRUE)
  expect_match(txt, "group2 = group,", fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, output_id = "T", save = FALSE))
  g2 <- as.character(unlist(ard$group2))
  expect_identical(unique(g2), c("All", "Age Group"))
  lv <- vapply(ard$group2_level, function(v) as.character(v %||% NA), "")
  expect_identical(unique(lv[g2 == "Age Group"]), c("<65 years", ">=65 years", ">=75 years"))
  # each level's numbers: the analysis on its records alone
  saf <- dplyr::filter(adam$ADSL, SAFFL == "Y")
  hand <- cards::ard_tabulate(dplyr::filter(saf, AGE >= 75), by = TRTA, variables = SEX)
  got <- ard[g2 == "Age Group" & lv == ">=75 years" & ard$stat_name == "n", , drop = FALSE]
  want <- hand[hand$stat_name == "n", , drop = FALSE]
  key <- function(d) paste(unlist(d$group1_level), unlist(d$variable_level))
  expect_identical(unlist(got$stat)[order(key(got))], unlist(want$stat)[order(key(want))])
  # "All": every record
  all_n <- sum(unlist(ard$stat[g2 == "All" & ard$stat_name == "n"]))
  expect_identical(as.integer(all_n), nrow(saf))
})

test_that("groups: an unknown group, a level without its group, and strata stop", {
  r <- list(method = "categorical", population_id = "SAF", by = "TRTA", variables = "SEX",
            groups = "AGE | BMI")
  expect_error(grp_spec(r), "groups BMI not in the `groups` sheet")
  r2 <- list(method = "categorical", population_id = "SAF", by = "TRTA", variables = "SEX",
             strata = "SEX", groups = "AGE")
  expect_error(grp_spec(r2), "`groups` take group2")
  g <- grp_sheet()
  g$group_id[2L] <- NA
  expect_error(grp_spec(list(method = "categorical", population_id = "SAF", variables = "SEX"),
                        groups = g), "`groups\\$group_id` is blank")
  # a definition without groups: an empty sheet, nothing written
  sp <- tfl_ard_spec(exact_spec(list(method = "categorical", population_id = "SAF",
                                     variables = "SEX")))
  expect_identical(nrow(sp$groups), 0L)
  expect_false(any(grepl("groups_", tfl_ard_code(sp, output_id = "T", save = FALSE), fixed = TRUE)))
})

test_that("data.yml `groups:` <-> the groups sheet", {
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(d)
  # (a study of its data catalog alone)
  f <- file.path(d, "data.yml")
  y <- c("groups:", "  - id: AGE", "    label: Age Group", "    levels:",
         "      - {level: \"<65 years\", where: !r AGE < 65}",
         "      - {level: \">=65 years\", where: !r AGE >= 65}",
         "  - id: ALL", "    label: All", "    levels:", "      - {level: \"\"}")
  writeLines(y, f)
  writeLines(c("study_id: GRP-1", "keys:", "  id: USUBJID"), file.path(d, "study.yml"))
  sh <- tfl_sheets_from_yaml(d)
  g <- sh$ard$groups
  expect_identical(g$group_id, c("AGE", "AGE", "ALL"))
  expect_identical(g$where[1:2], c("AGE < 65", "AGE >= 65"))
  expect_true(is.na(g$where[3L]))
  out <- tempfile()
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  suppressMessages(tfl_yaml_from_sheets(sh, out))
  back <- tfl_sheets_from_yaml(out)
  expect_identical(back$ard$groups, g)
})
