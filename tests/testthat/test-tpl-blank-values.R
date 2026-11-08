# Blank text values (an .xpt holds "" for a missing value), subjects with no
# record, a missing event day and a stub folded first: the templates on
# invented data (the package's datasets, with the cases put in)

bv_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
}

# a study of these datasets (a list of name = data frame)
bv_study <- function(root, data) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (n in names(data)) {
    haven::write_xpt(tflspec:::.ai_chr(data[[n]]), file.path(d, "adam", paste0(n, ".xpt")))
  }
  suppressMessages(tfl_ai_study_init(d, adam = "adam", study_id = "BV-1"))
  d
}

bv_put <- function(d, id, name, args, population = "SAF") {
  tfl_write_output(tfl_output(id, "table", population = population, extends = "defaults",
                              template = list(name = name, args = args)),
                   file.path(d, "spec", "outputs", paste0(id, ".yml")))
}

# the output's ARD as a data frame, its list columns as text
bv_ard <- function(d, id) {
  a <- suppressMessages(tfl_build_ard(tfl_read_study_spec(d), output_id = id, dir = d, save = FALSE))
  a <- as.data.frame(a)
  for (v in intersect(c("group1_level", "variable_level", "stat"), names(a))) {
    a[[v]] <- vapply(a[[v]], function(x) if (length(x)) as.character(x[[1L]]) else NA_character_, "")
  }
  a[, intersect(c("analysis_id", "group1", "group1_level", "variable", "variable_level", "stat_name",
                  "stat"), names(a))]
}

# one statistic of a variable's level in an arm (group1 = the arm)
bv_stat <- function(a, variable, level, arm, stat, analysis = NULL) {
  k <- a$variable == variable & a$variable_level %in% level & a$group1_level %in% arm &
    a$stat_name == stat
  if (!is.null(analysis)) k <- k & a$analysis_id == analysis
  as.numeric(a$stat[k])
}

test_that("demog, summary and disposition: a blank is missing, not a category", {
  bv_skip()
  adsl <- tflspec::adsl
  saf <- which(adsl$SAFFL == "Y")
  # one blank sex and one blank end-of-study status in each arm
  for (arm in c("Drug X", "Control")) {
    i <- saf[adsl$TRT01A[saf] == arm][1:2]
    adsl$SEX[i[1L]] <- ""
    adsl$EOSSTT[i[2L]] <- ""
  }
  d <- bv_study(withr::local_tempdir(), list(adsl = adsl))
  bv_put(d, "T-D", "demog", list(population = "SAF"))
  bv_put(d, "T-S", "summary", list(population = "SAF", continuous = "AGE",
                                   categorical = c("SEX", "EOSSTT"), total = TRUE))
  bv_put(d, "T-DI", "disposition", list(population = "SAF"))
  s <- adsl[adsl$SAFFL == "Y", ]
  for (id in c("T-D", "T-S", "T-DI")) {
    a <- bv_ard(d, id)
    for (v in intersect(c("SEX", "EOSSTT"), unique(a$variable))) {
      lv <- a$variable_level[a$variable == v & a$stat_name == "n"]
      expect_false(any(lv %in% ""), info = paste(id, v))
      for (arm in c("Drug X", "Control")) {
        x <- s[[v]][s$TRT01A == arm]
        x <- x[x != ""]
        for (l in unique(x)) {
          expect_equal(bv_stat(a, v, l, arm, "n"), sum(x == l), info = paste(id, v, l, arm))
          # the percent is of the subjects with a value
          expect_equal(bv_stat(a, v, l, arm, "p"), sum(x == l) / length(x), info = paste(id, v, l, arm))
        }
      }
    }
  }
  # the Total column too
  a <- bv_ard(d, "T-S")
  sex <- s$SEX[s$SEX != ""]
  tot <- a$variable == "SEX" & is.na(a$group1_level) & a$stat_name == "n"
  expect_setequal(a$variable_level[tot], c("F", "M"))
  expect_equal(sum(as.numeric(a$stat[tot])), length(sex))
  # the program says it: the count reads the data with its blank text missing
  txt <- paste(tfl_ard_code(tfl_read_study_spec(d), output_id = "T-D", save = FALSE, dir = d),
               collapse = "\n")
  # (dplyr and cards attached in the program: no `pkg::`)
  expect_match(txt, "mutate(across(where(is.character), ~ na_if(.x, \"\"))) |>\n  ard_tabulate(",
               fixed = TRUE)
})

test_that("lab_abnormal: a blank category is missing, not a category", {
  bv_skip()
  lb <- tflspec::adlb
  j <- which(lb$PARAMCD == "ALT" & lb$ONTRTFL == "Y" & lb$SAFFL == "Y")
  lb$ANRIND[j[c(1, 5, 9)]] <- ""
  d <- bv_study(withr::local_tempdir(), list(adsl = tflspec::adsl, adlb = lb))
  bv_put(d, "T-L", "lab_abnormal", list(population = "SAF", where = "ONTRTFL == \"Y\"",
                                        params = "ALT"))
  a <- bv_ard(d, "T-L")
  lv <- a$variable_level[a$analysis_id == "ABNORMAL" & a$variable == "ANRIND"]
  expect_false(any(lv %in% ""))
  expect_true(all(c("HIGH", "LOW", "NORMAL") %in% lv))
  # the counts are those of the records with a category
  r <- lb[lb$PARAMCD == "ALT" & lb$ONTRTFL == "Y" & lb$SAFFL == "Y" & lb$ANRIND != "", ]
  for (arm in c("Drug X", "Control")) {
    ra <- r[r$TRT01A == arm, ]
    n <- tapply(ra$USUBJID, ra$ANRIND, function(u) length(unique(u)))
    for (l in names(n)) {
      expect_equal(bv_stat(a, "ANRIND", l, arm, "n"), unname(n[l]), info = paste(arm, l))
    }
  }
})

test_that("orr: a subject with no BOR record is a non-responder in N; a blank response is missing", {
  bv_skip()
  rs <- tflspec::adrs
  itt <- tflspec::adsl[tflspec::adsl$ITTFL == "Y", ]
  b <- rs[rs$PARAMCD == "BOR" & rs$USUBJID %in% itt$USUBJID, ]
  # two subjects of Drug X: one with no BOR record, one with a blank response
  dx <- b$USUBJID[b$TRT01P == "Drug X"]
  none <- dx[1L]
  blank <- dx[2L]
  rs <- rs[!(rs$PARAMCD == "BOR" & rs$USUBJID == none), ]
  rs$AVALC[rs$PARAMCD == "BOR" & rs$USUBJID == blank] <- ""
  d <- bv_study(withr::local_tempdir(), list(adsl = tflspec::adsl, adrs = rs))
  bv_put(d, "T-O", "orr", list(population = "ITT"), population = "ITT")
  a <- bv_ard(d, "T-O")
  b <- rs[rs$PARAMCD == "BOR" & rs$USUBJID %in% itt$USUBJID, ]
  for (arm in c("Drug X", "Control")) {
    n_itt <- sum(itt$TRT01P == arm)
    # the arm's N is its subjects of the analysis set
    expect_equal(bv_stat(a, "RESP", c("TRUE", "FALSE"), arm, "N", "ORR")[1L], n_itt, info = arm)
    resp <- sum(b$AVALC[b$TRT01P == arm] %in% c("CR", "PR"))
    expect_equal(bv_stat(a, "RESP", "TRUE", arm, "n", "ORR"), resp, info = arm)
  }
  expect_equal(bv_stat(a, "AVALC", "CR", "Drug X", "N", "BOR"), sum(itt$TRT01P == "Drug X"))
  lv <- a$variable_level[a$analysis_id == "BOR" & a$variable == "AVALC"]
  expect_false(any(lv %in% ""))
  for (l in c("CR", "PR", "SD", "PD")) {
    expect_equal(bv_stat(a, "AVALC", l, "Drug X", "n", "BOR"),
                 sum(b$AVALC[b$TRT01P == "Drug X"] == l), info = l)
  }
  # the same for the columns layout
  bv_put(d, "T-O2", "orr", list(population = "ITT", stat_layout = "cols"), population = "ITT")
  a2 <- bv_ard(d, "T-O2")
  expect_equal(bv_stat(a2, "RESP", c("TRUE", "FALSE"), "Drug X", "N", "ORR")[1L],
               sum(itt$TRT01P == "Drug X"))
})
