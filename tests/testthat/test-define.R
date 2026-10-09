# Document import, level 1: a define read by rule.  The Define-XML 2.1 is the
# package's example (inst/extdata/define/define.xml); the 2.0 one is a test
# fixture; the Pinnacle 21 style workbook is written here.

.define_21 <- function() {
  system.file("extdata", "define", "define.xml", package = "tflspec")
}
.define_20 <- function() test_path("fixtures", "define", "define-2-0.xml")

# A small define spec in the Pinnacle 21 layout (made up, as the XML).
.p21_xlsx <- function(dir, codelists_sheet = "Codelists", label = "Label") {
  path <- file.path(dir, "define-spec.xlsx")
  vars <- data.frame(
    Order = c(1, 2, 3, 4, 1, 2),
    Dataset = c("ADSL", "ADSL", "ADSL", "ADSL", "ADAE", "ADAE"),
    Variable = c("USUBJID", "AGE", "SEX", "AGEGR1", "USUBJID", "AESEV"),
    Label = c("Unique Subject Identifier", "Age", "Sex",
              "Pooled Age Group 1", "Unique Subject Identifier",
              "Severity/Intensity"),
    `Data Type` = c("text", "integer", "text", "text", "text", "text"),
    Length = c(20, 3, 1, 5, 20, 8),
    Codelist = c(NA, NA, "SEX", "AGEGR1", NA, "AESEV"),
    Origin = c("Predecessor", "Predecessor", "Predecessor", "Derived",
               "Predecessor", "Predecessor"),
    check.names = FALSE)
  names(vars)[names(vars) == "Label"] <- label
  sheets <- list(
    Datasets = data.frame(
      Dataset = c("ADSL", "ADAE"),
      Description = c("Subject-Level Analysis Dataset",
                      "Adverse Events Analysis Dataset"),
      Class = c("SUBJECT LEVEL ANALYSIS DATASET",
                "OCCURRENCE DATA STRUCTURE"),
      Structure = c("One record per subject",
                    "One record per subject per adverse event"),
      Purpose = "Analysis",
      `Key Variables` = c("USUBJID", "USUBJID, AESEV"),
      check.names = FALSE),
    Variables = vars,
    Codelists = data.frame(
      ID = c("SEX", "SEX", "AGEGR1", "AGEGR1", "AESEV", "AESEV", "AESEV"),
      Name = c("Sex", NA, "Pooled Age Group 1", NA, "Severity", NA, NA),
      `NCI Codelist Code` = c("C66731", NA, NA, NA, "C66769", NA, NA),
      `Data Type` = "text",
      Order = c(1, 2, 1, 2, 1, 2, 3),
      Term = c("M", "F", "<65", ">=65", "MILD", "MODERATE", "LIFE THREAT"),
      `NCI Term Code` = c("C20197", "C16576", NA, NA, "C70666", "C70667",
                          NA),
      `Decoded Value` = c("Male", "Female", NA, NA, "Mild", "Moderate",
                          "Life threatening"),
      check.names = FALSE),
    Dictionaries = data.frame(ID = "MEDDRA", Name = "MedDRA",
                              Dictionary = "MedDRA", Version = "27.0"))
  names(sheets)[names(sheets) == "Codelists"] <- codelists_sheet
  openxlsx::write.xlsx(sheets, path)
  path
}

test_that("tfl_read_define() reads a Define-XML 2.1", {
  skip_if_not_installed("xml2")
  def <- tfl_read_define(.define_21())
  expect_s3_class(def, "tfl_define")
  expect_identical(attr(def, "source")$version, "2.1")
  expect_identical(def$study$study_name, "TFLSPEC-EXAMPLE")
  expect_identical(def$study$define_version, "2.1.0")

  ds <- def$datasets
  expect_identical(ds$dataset, c("ADSL", "ADAE"))
  expect_identical(ds$label[1], "Subject-Level Analysis Dataset")
  expect_identical(ds$class, c("SUBJECT LEVEL ANALYSIS DATASET",
                               "OCCURRENCE DATA STRUCTURE"))
  expect_identical(ds$subclass, c(NA, "ADVERSE EVENT"))
  expect_identical(ds$keys, c("USUBJID", "USUBJID | AEBODSYS"))
  expect_identical(ds$repeating, c("No", "Yes"))

  vs <- def$variables
  adsl <- vs[vs$dataset == "ADSL", ]
  expect_identical(adsl$variable,
                   c("USUBJID", "AGE", "AGEGR1", "SEX", "TRT01A", "SAFFL"))
  expect_identical(adsl$label[adsl$variable == "AGE"], "Age")
  expect_identical(adsl$type[adsl$variable == "AGE"], "integer")
  expect_identical(adsl$length[adsl$variable == "USUBJID"], 20L)
  expect_identical(adsl$codelist[adsl$variable == "SEX"], "CL.SEX")
  expect_identical(adsl$origin[adsl$variable == "AGEGR1"], "Derived")
  expect_identical(adsl$key[adsl$variable == "USUBJID"], 1L)
  expect_true(is.na(adsl$codelist[adsl$variable == "AGE"]))

  cl <- def$codelists
  sex <- cl[cl$codelist == "CL.SEX", ]
  # the file's order is kept; OrderNumber is the order
  expect_identical(sex$value, c("F", "M"))
  expect_identical(sex$decode, c("Female", "Male"))
  expect_identical(sex$order, c(2L, 1L))
  expect_identical(sex$code, c("C16576", "C20197"))
  expect_identical(unique(sex$kind), "item")
  agegr <- cl[cl$codelist == "CL.AGEGR1", ]
  expect_identical(agegr$value, c("<65", "65-80", ">80"))
  expect_true(all(is.na(agegr$decode)))
  expect_identical(unique(agegr$kind), "enumerated")
  sev <- cl[cl$codelist == "CL.AESEV", ]
  expect_identical(sev$extended, c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(sev$rank, c(1, 2, 3, 4))
  expect_identical(def$external$dictionary, "MedDRA")
  expect_identical(def$external$version, "27.0")
  expect_false("CL.MEDDRA" %in% cl$codelist)
  expect_output(print(def), "Define-XML|define.xml 2.1")
})

test_that("the texts are taken in the language asked for", {
  skip_if_not_installed("xml2")
  ja <- tfl_read_define(.define_21(), lang = "ja")
  expect_identical(ja$datasets$label[1], paste0(
    "\u88ab\u9a13\u8005\u30ec\u30d9\u30eb",
    "\u89e3\u6790\u30c7\u30fc\u30bf\u30bb\u30c3\u30c8"))
  expect_identical(ja$variables$label[ja$variables$variable == "AGE"],
                   "\u5e74\u9f62")
  # none in Japanese: the English one (the only one)
  expect_identical(ja$datasets$label[2], "Adverse Events Analysis Dataset")
  sex <- ja$codelists[ja$codelists$codelist == "CL.SEX", ]
  expect_identical(sex$decode, c("\u5973\u6027", "\u7537\u6027"))
})

test_that("tfl_read_define() reads a Define-XML 2.0", {
  skip_if_not_installed("xml2")
  def <- tfl_read_define(.define_20())
  expect_identical(attr(def, "source")$version, "2.0")
  # def:Class as an attribute (2.0); texts without xml:lang
  expect_identical(def$datasets$class,
                   rep("BASIC DATA STRUCTURE", 2L))
  expect_identical(def$datasets$label[1], "Vital Signs Analysis Dataset")
  expect_identical(def$variables$codelist,
                   c("CL.VSPARAMCD", "CL.AVISIT", "CL.LBPARAMCD",
                     "CL.AVISIT"))
  av <- def$codelists[def$codelists$codelist == "CL.AVISIT", ]
  expect_identical(av$extended, c(FALSE, FALSE, TRUE))
})

test_that("namespaces are found by URI, not by prefix", {
  skip_if_not_installed("xml2")
  x <- readLines(.define_21(), encoding = "UTF-8")
  x <- gsub("def:", "d21:", x, fixed = TRUE)
  x <- sub("xmlns:def=", "xmlns:d21=", x, fixed = TRUE)
  x <- sub("<ODM xmlns=\"http://www.cdisc.org/ns/odm/v1.3\"",
           "<odm:ODM xmlns:odm=\"http://www.cdisc.org/ns/odm/v1.3\"", x,
           fixed = TRUE)
  # every ODM element gets the odm: prefix
  els <- c("ODM", "Study", "GlobalVariables", "StudyName",
           "StudyDescription", "ProtocolName", "MetaDataVersion",
           "ItemGroupDef", "ItemRef", "ItemDef", "Description",
           "TranslatedText", "CodeListRef", "CodeList", "CodeListItem",
           "EnumeratedItem", "Decode", "Alias", "ExternalCodeList")
  x <- gsub(sprintf("<(/?)(%s)([ />])", paste(els, collapse = "|")),
            "<\\1odm:\\2\\3", x)
  x <- gsub("<(/?)odm:odm:", "<\\1odm:", x)
  f <- tempfile(fileext = ".xml")
  on.exit(unlink(f), add = TRUE)
  writeLines(x, f, useBytes = TRUE)
  a <- tfl_read_define(.define_21())
  b <- tfl_read_define(f)
  attr(b, "source") <- attr(a, "source")
  expect_identical(b, a)
})

test_that("what is not a Define-XML 2.0 / 2.1 is refused", {
  skip_if_not_installed("xml2")
  expect_error(tfl_read_define("nope.xml"), "no file 'nope.xml'")
  f <- tempfile(fileext = ".xml")
  on.exit(unlink(f), add = TRUE)
  writeLines("<a><b/></a>", f)
  expect_error(tfl_read_define(f), "not a Define-XML")
  writeLines("not xml at all <", f)
  expect_error(tfl_read_define(f), "is not XML")
  x <- readLines(.define_20())
  writeLines(sub("def/v2.0", "def/v1.0", x, fixed = TRUE), f)
  expect_error(tfl_read_define(f), "def/v1.0")
})

test_that("tfl_define_codelists() gives a report's codelists rows", {
  skip_if_not_installed("xml2")
  def <- tfl_read_define(.define_21())
  cl <- tfl_define_codelists(def, c("SEX", "agegr1", "AGE"),
                             output_id = "T-14-1-1", datasets = "ADSL")
  expect_identical(names(cl), c("output_id", "variable", "value", "label",
                                "order", "note"))
  expect_true(all(vapply(cl, is.character, NA)))
  expect_identical(unique(cl$output_id), "T-14-1-1")
  sex <- cl[cl$variable == "SEX", ]
  expect_identical(sex$value, c("F", "M"))
  expect_identical(sex$label, c("Female", "Male"))
  expect_identical(sex$order, c("2", "1"))
  expect_identical(unique(sex$note), "define: CL.SEX")
  ag <- cl[cl$variable == "AGEGR1", ]
  expect_identical(ag$value, c("<65", "65-80", ">80"))
  expect_true(all(is.na(ag$label)))     # prints as the value
  # the labels: the code list of `variable`
  lab <- cl[cl$variable == "variable", ]
  expect_identical(lab$value, c("SEX", "AGEGR1", "AGE"))
  expect_identical(lab$label, c("Sex", "Pooled Age Group 1", "Age"))
  expect_identical(lab$note[1], "define: ADSL.SEX")
  # AGE has no code list: its label only, and listed to set by hand
  sbh <- attr(cl, "set_by_hand")
  expect_identical(sbh$variable, "AGE")
  expect_match(sbh$reason, "no code list")

  # no labels; several reports; an extended value is noted
  two <- tfl_define_codelists(def, "AESEV", output_id = c("T-1", "T-2"),
                              labels = FALSE)
  expect_identical(two$output_id, rep(c("T-1", "T-2"), each = 4L))
  expect_false("variable" %in% two$variable)
  expect_identical(two$note[4], "define: CL.AESEV, extended")
})

test_that("rows the spec takes: tfl_table_spec() accepts them", {
  skip_if_not_installed("xml2")
  def <- tfl_read_define(.define_21())
  cl <- tfl_define_codelists(def, c("SEX", "TRT01A"), output_id = "T-14-1-1")
  sp <- tfl_table_spec(list(codelists = cl))
  expect_identical(nrow(sp$codelists), nrow(cl))
})

test_that("output_id is required", {
  skip_if_not_installed("xml2")
  def <- tfl_read_define(.define_21())
  expect_error(tfl_define_codelists(def, "SEX"), "output_id. is required")
  expect_error(tfl_define_codelists(def, "SEX", output_id = ""),
               "output_id. is required")
  expect_error(tfl_define_codelists(def, "SEX", output_id = NA_character_),
               "output_id. is required")
})

test_that("what the define cannot give is listed, not guessed", {
  skip_if_not_installed("xml2")
  def <- tfl_read_define(.define_21())
  expect_warning(
    cl <- tfl_define_codelists(def, c("SEX", "NOPE", "AEBODSYS"),
                               output_id = "T-1"),
    "not in the define: NOPE")
  sbh <- attr(cl, "set_by_hand")
  expect_identical(sbh$variable, c("NOPE", "AEBODSYS"))
  expect_match(sbh$reason[2], "external dictionary \\(MedDRA 27.0\\)")
  # SEX is in ADSL and ADAE with the same code list: no question
  expect_identical(sum(cl$variable == "SEX"), 2L)

  d20 <- tfl_read_define(.define_20())
  expect_error(tfl_define_codelists(d20, "PARAMCD", output_id = "F-1"),
               "ADVS: CL.VSPARAMCD, ADLB: CL.LBPARAMCD")
  vs <- tfl_define_codelists(d20, "PARAMCD", output_id = "F-1",
                             datasets = "advs")
  expect_identical(vs$value[vs$variable == "PARAMCD"], c("SYSBP", "DIABP"))
  expect_error(tfl_define_codelists(d20, "PARAMCD", output_id = "F-1",
                                    datasets = "ADXX"),
               "no dataset 'ADXX'")
  expect_error(tfl_define_codelists(list(), output_id = "F-1"),
               "`define` is what")
})

test_that("variables = NULL: every variable with a code list", {
  skip_if_not_installed("xml2")
  cl <- tfl_define_codelists(.define_21(), output_id = "T-1",
                             datasets = "ADSL", labels = FALSE)
  expect_identical(unique(cl$variable), c("AGEGR1", "SEX", "TRT01A", "SAFFL"))
})

test_that("tfl_read_define_spec() reads a Pinnacle 21 style workbook", {
  skip_if_not_installed("readxl")
  dir <- withr_tempdir()
  def <- tfl_read_define_spec(.p21_xlsx(dir))
  expect_s3_class(def, "tfl_define")
  expect_identical(attr(def, "source")$kind, "define spec")
  expect_identical(def$datasets$dataset, c("ADSL", "ADAE"))
  expect_identical(def$datasets$keys, c("USUBJID", "USUBJID | AESEV"))
  vs <- def$variables
  expect_identical(vs$variable[vs$dataset == "ADSL"],
                   c("USUBJID", "AGE", "SEX", "AGEGR1"))
  expect_identical(vs$length[vs$variable == "AGE"], 3L)
  expect_identical(vs$key[vs$dataset == "ADAE"], c(1L, 2L))
  cl <- def$codelists
  expect_identical(cl$name[cl$codelist == "SEX"], c("Sex", "Sex"))
  expect_identical(cl$kind[cl$codelist == "AGEGR1"], c("enumerated",
                                                      "enumerated"))
  # a term with no NCI code in a code list that has one: extended
  expect_identical(cl$extended[cl$codelist == "AESEV"],
                   c(FALSE, FALSE, TRUE))
  expect_false(any(cl$extended[cl$codelist == "AGEGR1"]))
  expect_identical(def$external$dictionary, "MedDRA")
  # the profile's columns this workbook lacks are listed
  un <- attr(def, "unread")
  expect_true(all(c("digits", "role") %in% un$field))

  rows <- tfl_define_codelists(def, c("SEX", "AESEV"), output_id = "T-1")
  expect_identical(rows$label[rows$variable == "SEX"], c("Male", "Female"))
  expect_identical(rows$note[rows$value == "LIFE THREAT"],
                   "define: AESEV, extended")
  # from the path, too
  expect_identical(tfl_define_codelists(file.path(dir, "define-spec.xlsx"),
                                        "SEX", output_id = "T-1"),
                   rows[rows$variable %in% "SEX" |
                          rows$value %in% "SEX", ],
                   ignore_attr = TRUE)
})

test_that("a company's layout: the profile changed through `map`", {
  skip_if_not_installed("readxl")
  dir <- withr_tempdir()
  path <- .p21_xlsx(dir, codelists_sheet = "CT", label = "Variable Label")
  expect_error(tfl_read_define_spec(path), "no sheet 'Codelists'")
  map <- list(codelists = list(sheet = "CT"),
              variables = list(label = "variable label"))
  def <- tfl_read_define_spec(path, map = map)
  expect_identical(def$variables$label[2], "Age")
  # a whole profile works the same
  prof <- utils::modifyList(tfl_define_profile(), map)
  expect_identical(tfl_read_define_spec(path, profile = prof), def)

  expect_error(tfl_read_define_spec(path, map = list(codelist = list())),
               "no part 'codelist'")
  expect_error(tfl_read_define_spec(path, map = list(
    codelists = list(sheet = "CT", decoded = "x"))), "no field 'decoded'")
  expect_error(tfl_read_define_spec(path, map = list(
    codelists = list(sheet = "CT", value = "Value"))),
    "no column 'Value'")
  expect_error(tfl_read_define_spec(path, map = list(
    codelists = list(sheet = "CT", value = NA))), "must name its `value`")
  expect_error(tfl_read_define_spec(path, map = "x"), "list of parts")
  expect_error(tfl_read_define_spec("nope.xlsx"), "no file")
})

test_that("a define spec's numbers must be whole; required cells filled", {
  skip_if_not_installed("readxl")
  dir <- withr_tempdir()
  path <- file.path(dir, "bad.xlsx")
  openxlsx::write.xlsx(list(
    Datasets = data.frame(Dataset = "ADSL"),
    Variables = data.frame(Order = c("1", "x"), Dataset = "ADSL",
                           Variable = c("A", "B")),
    Codelists = data.frame(ID = "C", Term = "1")), path)
  expect_error(tfl_read_define_spec(path), "column 'Order', row\\(s\\) 3")
  openxlsx::write.xlsx(list(
    Datasets = data.frame(Dataset = "ADSL"),
    Variables = data.frame(Dataset = "ADSL", Variable = c("A", NA),
                           Label = c("a", "b")),
    Codelists = data.frame(ID = "C", Term = "1")), path, overwrite = TRUE)
  expect_error(tfl_read_define_spec(path), "row\\(s\\) 3: no variable")
})
