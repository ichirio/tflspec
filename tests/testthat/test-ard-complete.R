# complete_hierarchy(): a nested count's code-list values no subject has,
# counted 0 (a `post` step), and the code lists' `under`.

hier_adam <- function() {
  adam <- exact_data()
  ae <- adam$ADAE
  ae <- ae[ae$AEDECOD %in% c("ERYTHEMA", "PRURITUS", "DIARRHOEA"), , drop = FALSE]
  adam$ADAE <- ae
  adam
}

hier_cl <- function(under = TRUE) {
  data.frame(
    output_id = "T",
    variable = c(rep("AESOC", 3L), rep("AEDECOD", 4L)),
    value = c("GASTROINTESTINAL DISORDERS", "EYE DISORDERS",
              "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
              "DIARRHOEA", "VISION BLURRED", "ERYTHEMA", "PRURITUS"),
    label = c(NA, "Eye disorders", NA, NA, "Vision blurred", NA, NA),
    order = as.character(c(1:3, 1:4)),
    under = c(NA, NA, NA, NA, if (under) "EYE DISORDERS" else NA, NA, NA),
    stringsAsFactors = FALSE)
}

lvl <- function(x) vapply(x, function(v) if (length(v)) as.character(v[[1L]]) else NA_character_, "")

test_that("complete_hierarchy() counts a code list's values no subject has as 0, in its order", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adam <- hier_adam()
  dir <- exact_dir(adam)
  sp <- exact_spec(list(method = "hierarchical", population_id = "SAF",
                        dataset = "ADAE", by = "TRTA",
                        variables = "AESOC | AEDECOD", post = "complete_hierarchy()"))
  cl <- hier_cl()
  code <- paste(tfl_ard_code(sp, part = "body", codelists = cl), collapse = "\n")
  # written in full: the row's variables, the code lists' under (as in the ARD)
  expect_match(code, "complete_hierarchy(", fixed = TRUE)
  expect_match(code, "variables = c(\"AESOC\", \"AEDECOD\")", fixed = TRUE)
  expect_match(code, "under = list(AEDECOD = c(\"Vision blurred\" = \"Eye disorders\"))", fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE, codelists = cl))
  soc <- ard[ard$variable == "AESOC" & ard$stat_name == "n", ]
  # every SOC of the list, Eye disorders 0 in each arm
  expect_setequal(unique(lvl(soc$variable_level)),
                  c("GASTROINTESTINAL DISORDERS", "Eye disorders",
                    "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"))
  eye <- soc[lvl(soc$variable_level) == "Eye disorders", ]
  expect_identical(nrow(eye), 3L)
  expect_true(all(unlist(eye$stat) == 0))
  expect_identical(levels(eye$variable_level[[1L]])[2L], "Eye disorders")
  # the PT under it: 0 under Eye disorders, N the arm's
  pt <- ard[ard$variable == "AEDECOD" & lvl(ard$variable_level) == "Vision blurred", ]
  expect_true(all(lvl(pt$group2_level) == "Eye disorders"))
  expect_true(all(unlist(pt$stat[pt$stat_name == "n"]) == 0))
  N <- unlist(ard$stat[ard$variable == "AESOC" & ard$stat_name == "N" &
                         lvl(ard$variable_level) == "GASTROINTESTINAL DISORDERS"])
  expect_identical(sort(unlist(pt$stat[pt$stat_name == "N"])), sort(N))
  # formatted as the rows it copies
  expect_true(all(nzchar(unlist(pt$stat_fmt))))

  # a value with no `under` is not added, and the program says so
  sp_msg <- tryCatch(tfl_build_ard(sp, dir = dir, save = FALSE, codelists = hier_cl(under = FALSE)),
                     message = function(m) conditionMessage(m))
  expect_match(paste(sp_msg, collapse = ""), "VISION BLURRED|Vision blurred")
  a2 <- suppressMessages(tfl_build_ard(sp, dir = dir, save = FALSE, codelists = hier_cl(under = FALSE)))
  expect_false("Vision blurred" %in% lvl(a2$variable_level))
  expect_true("Eye disorders" %in% lvl(a2$variable_level))   # the outer value: no under needed

  # without the step: the values some subject has, as before
  sp0 <- exact_spec(list(method = "hierarchical", population_id = "SAF",
                         dataset = "ADAE", by = "TRTA", variables = "AESOC | AEDECOD"))
  a0 <- suppressMessages(tfl_build_ard(sp0, dir = dir, save = FALSE, codelists = cl))
  expect_false(any(c("Eye disorders", "Vision blurred") %in% lvl(a0$variable_level)))
  # a step that gives its own arguments keeps them
  sp1 <- exact_spec(list(method = "hierarchical", population_id = "SAF",
                         dataset = "ADAE", by = "TRTA", variables = "AESOC | AEDECOD",
                         post = "complete_hierarchy(\"AESOC\") | cards::sort_ard_hierarchical()"))
  c1 <- paste(tfl_ard_code(sp1, part = "body", codelists = cl), collapse = "\n")
  expect_match(c1, "complete_hierarchy[(][[:space:]]*\"AESOC\",[[:space:]]*under = list[(]AEDECOD")
  expect_match(c1, "sort_ard_hierarchical()", fixed = TRUE)
})

test_that("complete_hierarchy() on its own: the Total column too, more than two levels", {
  skip_if_not_installed("cards")
  env <- new.env()
  eval(parse(text = tfl_helpers_code()), envir = env)
  adsl <- data.frame(USUBJID = paste0("S", 1:6), TRT = factor(rep(c("A", "B"), 3)))
  ae <- data.frame(USUBJID = c("S1", "S2"), TRT = factor(c("A", "B")),
                   L1 = factor(c("a", "a"), levels = c("a", "b")),
                   L2 = factor(c("a1", "a1"), levels = c("a1", "b1")),
                   L3 = factor(c("x", "x"), levels = c("x", "y")))
  ard <- dplyr::bind_rows(
    cards::ard_stack_hierarchical(ae, variables = c(L1, L2, L3), by = TRT, denominator = adsl, id = USUBJID),
    cards::ard_stack_hierarchical(ae, variables = c(L1, L2, L3), denominator = adsl, id = USUBJID))
  out <- env$complete_hierarchy(ard, c("L1", "L2", "L3"),
                                under = list(L2 = c(b1 = "b"), L3 = c(y = "b1")))
  expect_s3_class(out, "card")
  new <- out[-seq_len(nrow(ard)), ]
  # b (arms A, B and Total), b1 under b, y under b1: 3 statistics each
  expect_identical(nrow(new), 3L * 3L * 3L)
  expect_identical(unique(lvl(new$variable_level)), c("b", "b1", "y"))
  # y under b1 under b: its parent and its parent's parent, by arm and in Total
  y <- new[lvl(new$variable_level) == "y", ]
  lev_of <- function(d, g) vapply(seq_len(nrow(d)), function(i) {
    k <- which(vapply(c("group1", "group2", "group3"), function(cn) identical(d[[cn]][i], g), NA))
    if (length(k)) lvl(d[[paste0("group", k, "_level")]][i]) else NA_character_
  }, "")
  expect_true(all(lev_of(y, "L1") == "b"))
  expect_true(all(lev_of(y, "L2") == "b1"))
  expect_setequal(unique(lev_of(y, "TRT")), c("A", "B", NA))
  expect_true(all(unlist(new$stat[new$stat_name == "N"]) %in% c(3L, 6L)))
  # nothing to add: the ARD as it was
  expect_identical(env$complete_hierarchy(ard, "TRT"), ard)
})

test_that("the code lists' `under` goes between the YAML and the sheets", {
  cl <- tflspec:::.conv_codelists(hier_cl())
  expect_identical(cl$AEDECOD[[2L]], list(value = "VISION BLURRED", label = "Vision blurred",
                                          under = "EYE DISORDERS"))
  expect_null(cl$AEDECOD[[1L]]$under)
  rows <- tflspec:::.codelists_rows(cl, "T")
  expect_identical(rows$under[rows$value == "VISION BLURRED"], "EYE DISORDERS")
  expect_true(is.na(rows$under[rows$value == "ERYTHEMA"]))
  un <- tflspec:::.codelist_under(rows, "T")
  expect_identical(un, list(AEDECOD = c("VISION BLURRED" = "EYE DISORDERS")))
  # no under anywhere: NULL (the fingerprints stay)
  expect_null(tflspec:::.codelist_under(hier_cl(under = FALSE), "T"))
})

test_that("`under` in the flat-view workbook: written, edited and taken back", {
  skip_if_not_installed("openxlsx")
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  d <- withr::local_tempdir()
  file.copy(list.files(src, full.names = TRUE), d, recursive = TRUE)
  f <- file.path(d, "view.xlsx")
  tfl_write_spec_xlsx(d, f)
  wb <- openxlsx::loadWorkbook(f)
  x <- openxlsx::read.xlsx(wb, "codelists")
  # (the view has the keys its entries have: none has `under` yet)
  expect_false("under" %in% names(x))
  x[] <- lapply(x, as.character)
  x$under <- NA_character_
  x$under[x$value == "Control"] <- "PARENT"
  openxlsx::removeWorksheet(wb, "codelists"); openxlsx::addWorksheet(wb, "codelists")
  openxlsx::writeData(wb, "codelists", x, keepNA = FALSE)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  r <- tfl_import_spec_xlsx(d, f)
  expect_true(any(r$changes$sheet == "codelists" & r$changes$column == "under" &
                    r$changes$to == "PARENT"))
  o <- tfl_read_outputs(d)[["T-14-1-1"]]
  expect_identical(o$codelists$TRT01A[[2L]]$under, "PARENT")
  expect_null(o$codelists$TRT01A[[1L]]$under)
  # and back to the sheets
  sh <- tfl_sheets_from_yaml(d)
  cl <- sh$table$codelists
  expect_identical(cl$under[cl$output_id %in% "T-14-1-1" & cl$value == "Control"], "PARENT")
})
