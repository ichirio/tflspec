# R/define.R: the branches no other test reaches -- a define.xml with no
# MetaDataVersion or no def namespace, value-level entries that cannot be
# placed, each RangeCheck comparator, a define spec's own profile, and what
# tfl_define_codelists() sets by hand.  Defines made up, as the fixtures.

cov_def_vl <- function() test_path("fixtures", "define", "define-vl.xml")
cov_def_21 <- function() system.file("extdata", "define", "define.xml", package = "tflspec")

# the fixture `from` with `old` replaced by `new` (fixed), in a temporary file
cov_def_edit <- function(from, old, new, env = parent.frame()) {
  f <- tempfile(fileext = ".xml")
  do.call(on.exit, list(substitute(unlink(f)), add = TRUE), envir = env)
  x <- readLines(from, encoding = "UTF-8")
  for (i in seq_along(old)) {
    hit <- grepl(old[i], x, fixed = TRUE)
    stopifnot(any(hit))
    x <- sub(old[i], new[i], x, fixed = TRUE)
  }
  writeLines(x, f, useBytes = TRUE)
  f
}

test_that("a define.xml with an ODM root but no MetaDataVersion is refused by name", {
  skip_if_not_installed("xml2")
  f <- tempfile(fileext = ".xml")
  on.exit(unlink(f), add = TRUE)
  writeLines(c('<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3"',
               '     xmlns:def="http://www.cdisc.org/ns/def/v2.1">',
               '  <Study OID="ST.X"></Study>', "</ODM>"), f)
  expect_error(tfl_read_define(f), "has no ODM/Study/MetaDataVersion", fixed = TRUE)
})

test_that("a define.xml with no def namespace at all says it is missing", {
  skip_if_not_installed("xml2")
  f <- tempfile(fileext = ".xml")
  on.exit(unlink(f), add = TRUE)
  writeLines(c('<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3">',
               "  <Study/>", "</ODM>"), f)
  expect_error(tfl_read_define(f), "its def namespace is missing", fixed = TRUE)
})

test_that("a RangeCheck on an item the define does not list takes the variable from the OID", {
  skip_if_not_installed("xml2")
  # (the BILI RangeCheck, the line above its value, on an item not defined)
  x <- readLines(cov_def_vl())
  at <- grep("<CheckValue>BILI</CheckValue>", x, fixed = TRUE) - 1L
  x[at] <- sub("IT.ADLB.PARAMCD", "IT.ADLB.LBCAT", x[at], fixed = TRUE)
  f <- tempfile(fileext = ".xml")
  on.exit(unlink(f), add = TRUE)
  writeLines(x, f)
  vl <- tfl_read_define(f)$value_level
  expect_identical(vl$where[2L], "LBCAT == \"BILI\"")
  expect_identical(vl$where[1L], "PARAMCD == \"ALT\"")
})

test_that("a ValueListRef to a value list the define does not have gives no value-level rows", {
  skip_if_not_installed("xml2")
  f <- cov_def_edit(cov_def_vl(), '<def:ValueListRef ValueListOID="VL.ADLB.AVAL"/>',
                    '<def:ValueListRef ValueListOID="VL.ADLB.NONE"/>')
  expect_identical(nrow(tfl_read_define(f)$value_level), 0L)
})

test_that("a value list of a variable no dataset refers to gives no value-level rows", {
  skip_if_not_installed("xml2")
  f <- cov_def_edit(cov_def_vl(), '<ItemRef ItemOID="IT.ADLB.AVAL" OrderNumber="3" Mandatory="No"/>', "")
  def <- tfl_read_define(f)
  expect_false("AVAL" %in% def$variables$variable)
  expect_identical(nrow(def$value_level), 0L)
})

test_that("an empty ValueListDef gives no value-level rows", {
  skip_if_not_installed("xml2")
  x <- readLines(cov_def_vl())
  from <- grep('<def:ValueListDef OID="VL.ADLB.AVAL">', x, fixed = TRUE)
  to <- grep("</def:ValueListDef>", x, fixed = TRUE)
  x <- c(x[seq_len(from)], x[to:length(x)])
  f <- tempfile(fileext = ".xml")
  on.exit(unlink(f), add = TRUE)
  writeLines(x, f)
  expect_identical(nrow(tfl_read_define(f)$value_level), 0L)
})

test_that("a RangeCheck with an empty Comparator is read as EQ", {
  skip_if_not_installed("xml2")
  f <- cov_def_edit(cov_def_vl(), '<RangeCheck Comparator="EQ" SoftHard="Soft" def:ItemOID="IT.ADLB.PARAMCD">',
                    '<RangeCheck Comparator="" SoftHard="Soft" def:ItemOID="IT.ADLB.PARAMCD">')
  expect_identical(tfl_read_define(f)$value_level$where[1L], "PARAMCD == \"ALT\"")
  expect_identical(.define_condition("PARAMCD", NA_character_, "ALT"), "PARAMCD == \"ALT\"")
})

test_that("each RangeCheck comparator is written as R: NE, NOTIN, LT, LE, GT, GE", {
  expect_identical(.define_condition("SEX", "NE", "F"), "SEX != \"F\"")
  expect_identical(.define_condition("SEX", "notin", c("F", "U")), "!SEX %in% c(\"F\", \"U\")")
  expect_identical(.define_condition("AGE", "LT", "65"), "AGE < 65")
  expect_identical(.define_condition("AGE", "LE", "65"), "AGE <= 65")
  expect_identical(.define_condition("AGE", "GT", "18"), "AGE > 18")
  expect_identical(.define_condition("AGE", "GE", "18"), "AGE >= 18")
})

test_that("a comparator the define standard does not have is kept as written", {
  expect_identical(.define_condition("AGE", "BETWEEN", c("18", "65")), "AGE BETWEEN 18, 65")
})

test_that("print() of a define spec read from a workbook shows no version", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("openxlsx")
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  openxlsx::write.xlsx(list(
    Datasets = data.frame(Dataset = "ADSL"),
    Variables = data.frame(Dataset = "ADSL", Variable = "SEX", Label = "Sex", Codelist = "CL.SEX"),
    Codelists = data.frame(ID = "CL.SEX", Term = c("M", "F"))), f)
  def <- tfl_read_define_spec(f)
  expect_true(is.na(attr(def, "source")$version))
  out <- capture.output(print(def))
  expect_identical(out[1L], "<tfl_define> define spec")
})

test_that("a profile that names the Extended column takes extended from it, not from the NCI codes", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("openxlsx")
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  openxlsx::write.xlsx(list(
    Datasets = data.frame(Dataset = "ADAE"),
    Variables = data.frame(Dataset = "ADAE", Variable = "AESEV", Label = "Severity",
                           Codelist = "AESEV"),
    Codelists = data.frame(ID = "AESEV", `NCI Codelist Code` = c("C66769", NA, NA),
                           Term = c("MILD", "MODERATE", "LIFE THREAT"),
                           `NCI Term Code` = c("C70666", NA, NA),
                           Extended = c("No", "No", "Yes"), check.names = FALSE)), f)
  # by the NCI codes, a term with none is extended
  expect_identical(tfl_read_define_spec(f)$codelists$extended, c(FALSE, TRUE, TRUE))
  # by the column the profile names: as the sheet says
  def <- tfl_read_define_spec(f, map = list(codelists = list(extended = "Extended")))
  expect_identical(def$codelists$extended, c(FALSE, FALSE, TRUE))
})

test_that("a profile that is neither a name nor a list is refused", {
  expect_error(tfl_read_define_spec(cov_def_21(), profile = 5),
               "`profile` is a name or a list", fixed = TRUE)
})

test_that("a profile list without a part takes that part from the built-in profile", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("openxlsx")
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  openxlsx::write.xlsx(list(
    Datasets = data.frame(Dataset = "ADSL"),
    Variables = data.frame(Dataset = "ADSL", Variable = "SEX", Label = "Sex", Codelist = "CL.SEX"),
    Codelists = data.frame(ID = "CL.SEX", Term = c("M", "F"))), f)
  prof <- tfl_define_profile()
  prof$external <- NULL
  prof$value_level <- NULL
  expect_identical(tfl_read_define_spec(f, profile = prof), tfl_read_define_spec(f))
})

test_that("a variable labelled differently in two datasets: the first label taken, set by hand", {
  skip_if_not_installed("xml2")
  def <- tfl_read_define(cov_def_21())
  i <- which(def$variables$variable == "SEX")
  skip_if(length(i) < 2L, "the example define has SEX in one dataset only")
  def$variables$label[i[2L]] <- "Sex of the Subject"
  out <- tfl_define_codelists(def, "SEX", output_id = "T-1")
  hand <- attr(out, "set_by_hand")
  expect_identical(hand$variable, "SEX")
  expect_match(hand$reason, "its label differs between datasets", fixed = TRUE)
  expect_identical(attr(out, "variables")$label, def$variables$label[i[1L]])
})

test_that("a variable with no label in the define is set by hand, its values still given", {
  skip_if_not_installed("xml2")
  def <- tfl_read_define(cov_def_21())
  def$variables$label[def$variables$variable == "AGEGR1"] <- NA_character_
  out <- tfl_define_codelists(def, "AGEGR1", output_id = "T-1")
  expect_identical(attr(out, "set_by_hand")$reason, "no label in the define")
  expect_gt(nrow(out), 0L)
  expect_identical(nrow(attr(out, "variables")), 0L)
})

test_that("an external dictionary with no version is named without one", {
  skip_if_not_installed("xml2")
  f <- cov_def_edit(cov_def_21(), '<ExternalCodeList Dictionary="MedDRA" Version="27.0"/>',
                    '<ExternalCodeList Dictionary="MedDRA"/>')
  out <- tfl_define_codelists(tfl_read_define(f), "AEBODSYS", output_id = "T-1")
  expect_identical(attr(out, "set_by_hand")$reason,
                   "its code list CL.MEDDRA is an external dictionary (MedDRA)")
})

test_that("a code list the define names but does not list is set by hand", {
  skip_if_not_installed("xml2")
  def <- tfl_read_define(cov_def_21())
  def$variables$codelist[def$variables$variable == "AGEGR1"] <- "CL.NOSUCH"
  out <- tfl_define_codelists(def, "AGEGR1", output_id = "T-1", labels = FALSE)
  expect_identical(attr(out, "set_by_hand")$reason,
                   "its code list CL.NOSUCH has no values in the define")
  expect_identical(nrow(out), 0L)
})

test_that("a define given as a path that is neither .xml nor .xlsx is refused", {
  expect_error(tfl_define_codelists("define.csv", "SEX", output_id = "T-1"),
               "a define is a .xml or an .xlsx; got 'define.csv'", fixed = TRUE)
  expect_error(tfl_define_codelists("C:\\study\\define", "SEX", output_id = "T-1"),
               "a define is a .xml or an .xlsx", fixed = TRUE)
})
