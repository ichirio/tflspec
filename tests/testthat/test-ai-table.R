# The `table` drafting task end to end, offline: the sample TOC / SHELL
# workbook read as text, the sample ADaM written as XPT and summarised, the
# prompt built, the stored example answer parsed, checked and unpacked, and
# the unpacked report files run to RTF.

ai_xlsx <- function() system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
ai_answer <- function() readLines(system.file("ai", "example", "onco-answer.md", package = "tflspec"),
                                  encoding = "UTF-8")

# A study folder: the sample ADaM as XPT (factors as character, as a
# company's XPT holds them), and tfl_ai_study_init()'s files.
ai_table_study <- function(env = parent.frame()) {
  testthat::skip_if_not_installed("haven")
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "adam"))
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "XYZ-ONC-01")
  d
}

# ---- the workbook ----------------------------------------------------------

test_that("the workbook is read whatever its layout: the TOC, the shells, merges, indents", {
  x <- tfl_ai_read_shell(ai_xlsx())
  expect_s3_class(x, "tfl_ai_shell")
  expect_identical(x$sheets$sheet, c("TOC", "T14.1.1", "T14.2.1", "F14.2.2", "Safety"))
  expect_identical(x$sheets$role, c("toc", "shell", "shell", "shell", "shell"))
  # the TOC's report numbers, not the version in its title nor its sections
  expect_identical(x$toc$output_id, c("T-14-1-1", "T-14-2-1", "F-14-2-2", "T-14-3-1", "T-14-3-5"))
  expect_identical(x$toc$type, c("table", "table", "figure", "table", "table"))
  expect_identical(x$toc$shell, c("T14.1.1", "T14.2.1", "F14.2.2", "Safety", "Safety"))
  expect_match(x$toc$title[4L], "^Treatment-Emergent Adverse Events")
  txt <- format(x)
  expect_match(txt, "## Sheet \"TOC\" (the TOC)", fixed = TRUE)
  expect_match(txt, "2: A2:C2=Demographic and Baseline Characteristics", fixed = TRUE)
  expect_match(txt, "8: A=  n | B=xx | C=xx | D=xx", fixed = TRUE)   # the alignment's indent
  expect_output(print(x), "5 report(s) in the TOC, 5 with a shell found", fixed = TRUE)
})

test_that("outputs keep the TOC's header and their own rows and shells; numbers match loosely", {
  x <- tfl_ai_read_shell(ai_xlsx(), outputs = c("Table 14.3.5", "T14.2.1"))
  expect_identical(x$outputs, c("T-14-3-5", "T-14-2-1"))
  txt <- format(x)
  expect_match(txt, "A=No. | B=Type", fixed = TRUE)
  expect_match(txt, "T-14-2-1 | B=Table", fixed = TRUE)
  expect_false(grepl("T-14-1-1", txt, fixed = TRUE))
  expect_false(grepl("Sheet \"T14.1.1\"", txt, fixed = TRUE))
  # the Safety sheet holds two shells: only the shift table's rows
  expect_match(txt, "Shift from Baseline", fixed = TRUE)
  expect_false(grepl("SYSTEM ORGAN CLASS 1", txt, fixed = TRUE))
  # sheets whole, a cap, a file
  s <- tfl_ai_read_shell(ai_xlsx(), sheets = "TOC")
  expect_identical(unique(s$cells$sheet[s$cells$sheet %in% "TOC"]), "TOC")
  expect_false(grepl("Sheet \"Safety\"", format(s), fixed = TRUE))
  f <- withr::local_tempfile(fileext = ".md")
  cut <- tfl_ai_read_shell(ai_xlsx(), max_chars = 600, file = f)
  expect_true(cut$truncated)
  expect_lte(nchar(format(cut)), 800)
  expect_match(format(cut), "pick fewer reports", fixed = TRUE)
  expect_identical(readLines(f, encoding = "UTF-8")[1L], strsplit(format(cut), "\n")[[1L]][1L])
  expect_error(tfl_ai_read_shell(ai_xlsx(), outputs = "T-99-1-1"), "no report 'T-99-1-1'")
  expect_error(tfl_ai_read_shell(ai_xlsx(), sheets = "Nope"), "no sheet 'Nope'")
  bad <- withr::local_tempfile(fileext = ".xlsx")
  writeLines("not a workbook", bad)
  expect_error(tfl_ai_read_shell(bad), "not an .xlsx workbook")
})

test_that("report numbers: one key whatever the writing; the phonetic runs left out", {
  k <- tflspec:::.ai_id_key(c("T-14-1-1", "Table 14.1.1", "14.1.1", "\u886814.1.1", "F14.2.1",
                              "Listing 16.2.7"))
  expect_identical(k, c("T14.1.1", "T14.1.1", "14.1.1", "T14.1.1", "F14.2.1", "L16.2.7"))
  expect_true(tflspec:::.ai_id_same("T14.1.1", "14.1.1"))
  expect_false(tflspec:::.ai_id_same("T14.1.1", "F14.1.1"))
  si <- "<si><t>\u5e74\u9f62</t><rPh sb=\"0\" eb=\"2\"><t>\u30cd\u30f3\u30ec\u30a4</t></rPh></si>"
  expect_identical(tflspec:::.xl_si_text(si), "\u5e74\u9f62")
  expect_identical(tflspec:::.xl_unescape("a &amp; b &lt;1&gt; &#65;"), "a & b <1> A")
})

test_that("the tables in batches of one family", {
  b <- tfl_ai_batches(tfl_ai_read_shell(ai_xlsx()))
  expect_identical(b$family, c("demographics", "ae", "labs", "efficacy"))
  expect_identical(b$batch, c("demographics-1", "ae-1", "labs-1", "efficacy-1"))
  toc <- data.frame(output_id = sprintf("T-%d", 1:5),
                    title = c(rep("Adverse events by SOC", 3), "Vital signs", "Anything"))
  b <- tfl_ai_batches(toc, size = 2)
  expect_identical(b$batch, c("ae-1", "ae-1", "ae-2", "vitals-1", "other-1"))
  own <- tfl_ai_batches(toc, families = c(anything = "(?i)anything"))
  expect_identical(own$family[own$output_id == "T-5"], "anything")
  expect_error(tfl_ai_batches(data.frame(x = 1)), "output_id")
})

# ---- the ADaM summary and the study folder ------------------------------------

test_that("the ADaM summary with a define: its code lists in order with decodes, digits, value level", {
  skip_if_not_installed("xml2")
  d <- ai_table_study()
  def <- test_path("fixtures", "define", "define-vl.xml")
  s <- tfl_ai_adam_summary(file.path(d, "adam"), datasets = c("ADSL", "ADLB"), define = def)
  v <- s$variables
  # the define's order and decodes, over the data's values
  expect_identical(v$levels[v$dataset == "ADSL" & v$variable == "SEX"], "M | F")
  expect_identical(v$codes[v$dataset == "ADSL" & v$variable == "SEX"], "M = Male | F = Female")
  expect_identical(v$levels[v$dataset == "ADLB" & v$variable == "PARAMCD"],
                   "ALT | BILI | HGB | PLAT | AST | NEUT")
  expect_match(v$codes[v$dataset == "ADLB" & v$variable == "PARAMCD"], "BILI = Bilirubin (umol/L)", fixed = TRUE)
  expect_identical(v$define[v$dataset == "ADLB" & v$variable == "AVAL"], "digits 2, format 8.2")
  expect_identical(nrow(s$value_level), 3L)
  txt <- format(s)
  expect_match(txt, "AVAL (num) Analysis Value [define.xml: digits 2, format 8.2]", fixed = TRUE)
  expect_match(txt, "## Value level (define.xml)", fixed = TRUE)
  expect_match(txt, "- ADLB AVAL where `PARAMCD == \"BILI\"`: digits 1, format 8.1", fixed = TRUE)
  # a P21 workbook the same way; none: as before
  skip_if_not_installed("openxlsx")
  xl <- file.path(d, "spec.xlsx")
  openxlsx::write.xlsx(list(
    Datasets = data.frame(Dataset = "ADSL", Description = "Subject-Level Analysis Dataset"),
    Variables = data.frame(Order = 1:2, Dataset = "ADSL", Variable = c("SEX", "AGE"),
                           Label = c("Sex", "Age"), Codelist = c("CL.SEX", NA),
                           `Significant Digits` = c(NA, 0), check.names = FALSE),
    Codelists = data.frame(ID = "CL.SEX", Name = "Sex", Order = 1:2, Term = c("M", "F"),
                           `Decoded Value` = c("Male", "Female"), check.names = FALSE)), xl)
  s2 <- tfl_ai_adam_summary(file.path(d, "adam"), datasets = "ADSL", adam_spec = xl)
  expect_identical(s2$variables$codes[s2$variables$variable == "SEX"], "M = Male | F = Female")
  expect_identical(s2$variables$levels[s2$variables$variable == "SEX"], "M | F")
  # a spec whose terms are not the data's: the data's values are listed,
  # the spec's decodes go to the codes only
  xl2 <- file.path(d, "spec2.xlsx")
  openxlsx::write.xlsx(list(
    Datasets = data.frame(Dataset = "ADSL", Description = "Subject-Level Analysis Dataset"),
    Variables = data.frame(Order = 1, Dataset = "ADSL", Variable = "SEX", Label = "Sex",
                           Codelist = "CL.SEX", check.names = FALSE),
    Codelists = data.frame(ID = "CL.SEX", Name = "Sex", Order = 1:2, Term = c("Male", "Female"),
                           `Decoded Value` = c("Male", "Female"), check.names = FALSE)), xl2)
  s3 <- tfl_ai_adam_summary(file.path(d, "adam"), datasets = "ADSL", adam_spec = xl2)
  expect_identical(s3$variables$levels[s3$variables$variable == "SEX"], "F | M")
  expect_match(format(s3), "SEX (char) Sex: F | M [missing: none]", fixed = TRUE)
  expect_match(format(s2), "AGE (num) Age [define spec: digits 0]", fixed = TRUE)
  expect_false(grepl("Value level", format(s2), fixed = TRUE))
  s0 <- tfl_ai_adam_summary(file.path(d, "adam"), datasets = "ADSL")
  expect_identical(nrow(s0$value_level), 0L)
  expect_false(grepl("define", format(s0), fixed = TRUE))
})

test_that("the ADaM summary: variables, labels, types, short value lists, no records", {
  d <- ai_table_study()
  s <- tfl_ai_adam_summary(file.path(d, "adam"))
  expect_s3_class(s, "tfl_ai_adam")
  expect_identical(s$datasets$dataset, c("ADAE", "ADLB", "ADRS", "ADSL", "ADTTE"))
  v <- s$variables
  expect_identical(v$levels[v$dataset == "ADSL" & v$variable == "SEX"], "F | M")
  expect_identical(v$levels[v$dataset == "ADLB" & v$variable == "PARAMCD"],
                   "ALT | AST | BILI | HGB | NEUT | PLAT")
  expect_true(is.na(v$levels[v$dataset == "ADSL" & v$variable == "USUBJID"]))
  txt <- format(s)
  expect_match(txt, "## ADSL (200 records, 200 subjects, 45 variables)", fixed = TRUE)
  expect_match(txt, "SEX (char) Sex: F | M [missing: none]", fixed = TRUE)
  # the subjects beside the records; the missing counts, NA and "" alike
  expect_identical(s$datasets$subjects[s$datasets$dataset == "ADSL"], 200L)
  expect_lt(s$datasets$subjects[s$datasets$dataset == "ADAE"],
            s$datasets$records[s$datasets$dataset == "ADAE"])
  expect_identical(v$missing[v$dataset == "ADSL" & v$variable == "SEX"], 0L)
  m <- tfl_ai_adam_summary(list(adxx = data.frame(USUBJID = c("1", "1", "2", NA),
                                                  SEX = c("F", "", NA, "M"), AGE = c(1, NA, 3, 4))))
  expect_identical(m$datasets$subjects, 2L)
  expect_identical(m$variables$missing, c(1L, 2L, 1L))
  expect_match(format(m), "SEX (char): F | M [missing: 2 of 4]", fixed = TRUE)
  expect_match(format(m), "AGE (num) [missing: 1 of 4]", fixed = TRUE)
  expect_match(format(m), "## ADXX (4 records, 2 subjects, 3 variables)", fixed = TRUE)
  n <- tfl_ai_adam_summary(list(adyy = data.frame(A = 1)))
  expect_true(is.na(n$datasets$subjects))
  expect_match(format(n), "## ADYY (1 records, 1 variables)", fixed = TRUE)
  expect_false(grepl(adsl$USUBJID[1L], txt, fixed = TRUE))
  expect_lt(nchar(txt), 20000)
  expect_identical(tfl_ai_adam_summary(file.path(d, "adam"), datasets = "adsl")$datasets$dataset,
                   "ADSL")
  expect_error(tfl_ai_adam_summary(file.path(d, "adam"), datasets = "ADXX"), "no dataset ADXX")
  expect_output(print(s), "5 dataset(s)", fixed = TRUE)
  # what a code stands for: from the variable it pairs with, or its value labels
  expect_identical(v$codes[v$dataset == "ADSL" & v$variable == "AGEGR1N"], "1 = <65 | 2 = >=65")
  expect_identical(v$codes[v$dataset == "ADSL" & v$variable == "ARMCD"],
                   "CTRL = Control | DRUGX = Drug X")
  expect_true(is.na(v$codes[v$dataset == "ADSL" & v$variable == "SEX"]))
  expect_match(format(s), "AGEGR1N (num) Pooled Age Group 1 (N) [codes: 1 = <65 | 2 = >=65]",
               fixed = TRUE)
  x <- data.frame(SEXN = c(1, 2, 1))
  attr(x$SEXN, "labels") <- c(Male = 1, Female = 2)
  expect_identical(tflspec:::.ai_adam_codes(x, "SEXN", 10L), "1 = Male | 2 = Female")
  # a code with two texts: none shown
  y <- data.frame(RACEN = c(1, 1), RACE = c("A", "B"))
  expect_true(is.na(tflspec:::.ai_adam_codes(y, "RACEN", 10L)))
})

test_that("tfl_ai_study_init(): the datasets, ADSL's analysis sets, the defaults", {
  d <- ai_table_study()
  st <- tfl_read_study_spec(d)
  expect_identical(st$study$study_id, "XYZ-ONC-01")
  pops <- vapply(st$data$populations, `[[`, "", "id")
  expect_true(all(c("SAF", "ITT", "EFF", "ALL") %in% pops))
  all_ <- st$data$populations[[which(pops == "ALL")]]
  expect_identical(all_$dataset, "ADSL")
  expect_null(all_$where)
  saf <- st$data$populations[[which(pops == "SAF")]]
  expect_s3_class(saf$where, "tfl_r")
  expect_identical(as.character(saf$where), "SAFFL == \"Y\"")
  expect_identical(st$data$datasets[[4L]]$path, "adam/adsl.xpt")
  expect_identical(vapply(st$defaults$content, `[[`, "", "fn")[1L], "normalize_ard")
  expect_error(tfl_ai_study_init(d, adam = "adam"), "already there")
})

# ---- the prompt ----------------------------------------------------------------

test_that("the table context and prompt: the reports, the study's files, the data, the shells", {
  d <- ai_table_study()
  sh <- tfl_ai_read_shell(ai_xlsx())
  ctx <- tfl_ai_context("table", outputs = c("T-14-1-1", "T-14-3-1"), shell = sh,
                        datasets = tfl_ai_adam_summary(file.path(d, "adam")), study_dir = d)
  expect_identical(ctx$titles$output_id, c("T-14-1-1", "T-14-3-1"))
  expect_identical(ctx$study$study_id, "XYZ-ONC-01")
  expect_true("SAF" %in% ctx$populations)
  # the whole workbook given: only the two reports' parts go into the prompt
  expect_false(grepl("Best Overall Response", ctx$shell, fixed = TRUE))
  p <- tfl_ai_prompt("table", ctx)
  expect_match(p$system, "tflspec_ai: {task: table, version: 1, outputs: [T-14-1-1, T-14-3-1]}",
               fixed = TRUE)
  expect_match(p$system, "- `plan_cells`: How the statistics become cell text.", fixed = TRUE)
  expect_match(p$system, "- `hierarchical`: Nested counts", fixed = TRUE)
  expect_match(p$system, "### ard: analysis_data", fixed = TRUE)
  expect_match(p$system, "## Example", fixed = TRUE)
  expect_match(p$user, "`spec/defaults.yml`: what every report starts from", fixed = TRUE)
  expect_match(p$user, "SEX (char) Sex: F | M", fixed = TRUE)
  expect_match(p$user, "## The TOC and the shells", fixed = TRUE)
  expect_identical(p$attach, character())
  expect_false(grepl("{{", format(p), fixed = TRUE))
  # no shell given: the workbook is attached
  expect_match(tfl_ai_prompt("table", tfl_ai_context("table", outputs = "T-1"))$attach,
               "TOC / SHELL workbook", fixed = TRUE)
  # a reviewed report of the family as the model
  ex <- withr::local_tempfile(fileext = ".yml")
  writeLines(c("id: T-14-3-1", "kind: table", "content:", "  - table_plan: {cols: TRT01A}"), ex)
  pe <- tfl_ai_prompt("table", tfl_ai_context("table", outputs = "T-14-3-2", examples = ex))
  expect_match(pe$user, "## Reviewed reports of this study", fixed = TRUE)
  expect_match(pe$user, "table_plan: {cols: TRT01A}", fixed = TRUE)
  # the fields of the task, and only those
  expect_error(tfl_ai_context("figure", output_id = "F-1", outputs = "T-1"), "no 'outputs'")
  expect_error(tfl_ai_context("table", study_dir = file.path(d, "nope")), "study folder")
  expect_error(tfl_ai_context("table", examples = "nope.yml"), "reviewed report files")
  expect_match(tfl_ai_schema("table", "plan_sort"), "`plan_sort`: The row order", fixed = TRUE)
})

# ---- the answer -----------------------------------------------------------------

test_that("the stored answer reads, checks clean in its context, and unpacks", {
  d <- ai_table_study()
  toc <- tfl_ai_read_shell(ai_xlsx())$toc
  ctx <- tfl_ai_context("table", outputs = toc$output_id[toc$type == "table"],
                        datasets = tfl_ai_adam_summary(file.path(d, "adam")), study_dir = d)
  a <- tfl_ai_parse(ai_answer(), "table")
  expect_s3_class(a, "tfl_ai_answer")
  expect_identical(names(a$outputs), c("T-14-1-1", "T-14-2-1", "T-14-3-1", "T-14-3-5"))
  expect_true(all(vapply(a$outputs, inherits, NA, "tfl_output")))
  expect_identical(nrow(a$assumption_rows), 5L)
  expect_identical(nrow(a$problems), 0L)
  expect_output(print(a), "outputs: T-14-1-1, T-14-2-1, T-14-3-1, T-14-3-5", fixed = TRUE)
  expect_identical(nrow(tfl_ai_check(a, ctx)), 0L)
  w <- tfl_ai_unpack(a, d)
  expect_identical(basename(w), c(paste0(c("T-14-1-1", "T-14-2-1", "T-14-3-1", "T-14-3-5"), ".yml"),
                                  "_assumptions.yml"))
  outs <- tfl_read_outputs(d)
  expect_identical(outs[["T-14-2-1"]]$ard$analysis_data[[1L]]$id, "adrs_bor")
  expect_error(tfl_ai_unpack(a, d), "already there")
  # a second batch adds to the assumptions; a batch again replaces its own
  a2 <- a
  a2$outputs <- a$outputs["T-14-1-1"]
  a2$assumption_rows <- a$assumption_rows[1L, ]
  a2$assumption_rows$assumption <- "changed"
  tfl_ai_unpack(a2, d, overwrite = TRUE)
  ass <- yaml::read_yaml(file.path(d, "spec", "_assumptions.yml"))
  # (the answer's own; a report whose widths R set has one more, item widths)
  ass <- Filter(function(x) !identical(x$item, "widths"), ass)
  ids <- vapply(ass, `[[`, "", "output_id")
  expect_identical(sum(ids == "T-14-1-1"), 1L)
  expect_identical(sum(ids == "T-14-3-5"), 1L)
  # the text, as pasted, unpacks too
  d2 <- withr::local_tempdir()
  expect_length(tfl_ai_unpack(ai_answer(), d2), 5L)
})

test_that("a flawed answer: each problem names the report, the item and the key", {
  d <- ai_table_study()
  ctx <- tfl_ai_context("table", outputs = c("T-14-1-1", "T-14-2-1"),
                        datasets = tfl_ai_adam_summary(file.path(d, "adam")), study_dir = d)
  x <- ai_answer()
  x <- sub("variables: [SEX, AGEGR1, RACE]", "variables: [SEX, AGEGRP, RACE]", x, fixed = TRUE)
  x <- sub("table_plan: {cols: TRT01A, rows: {group: variable}}",
           "table_plan: {colz: TRT01A, rows: {group: variable}}", x, fixed = TRUE)
  x <- sub("- plan_total: {label: Total}", "- plan_magic: {label: Total}", x, fixed = TRUE)
  x <- sub("population: ITT, where", "population: FULL, where", x, fixed = TRUE)
  x <- sub("method: proportion_ci", "method: proportion", x, fixed = TRUE)
  x <- sub("    data: {source: ard}", "    data: {source: ard}\n    colour: red", x, fixed = TRUE)
  a <- tfl_ai_parse(x, "table")
  p <- tfl_ai_check(a, ctx)
  has <- function(where, what) {
    any(grepl(where, p$where, fixed = TRUE) & grepl(what, p$problem, fixed = TRUE))
  }
  expect_true(has("outputs[1] T-14-1-1 colour", "is not a key of a report file"))
  expect_true(has("ard.analyses[3] CAT variables", "AGEGRP is not a variable of ADSL"))
  expect_true(has("content[1] table_plan field colz", "table_plan has no argument `colz`"))
  expect_true(has("content[3] plan_magic", "not a function of a table's content"))
  expect_true(has("ard.analysis_data[1] population", "'FULL' is not an analysis set"))
  expect_true(has("ard.analyses[2] ORR method", "'proportion' is not a method"))
  expect_true(has("outputs T-14-3-1", "was not asked for"))
  expect_true(all(p$severity[!grepl("not asked", p$problem)] == "error"))
  # the repair prompt: the problems, the schema of the parts they are in
  r <- tfl_ai_repair_prompt(a, p)
  expect_match(r, "AGEGRP is not a variable of ADSL", fixed = TRUE)
  expect_match(r, "- `table_plan`: The roles", fixed = TRUE)
  expect_false(grepl("- `plan_sort`:", r, fixed = TRUE))
  expect_match(r, "Return the whole block again", fixed = TRUE)
  # a report asked for and missing; a block that does not read
  a1 <- tfl_ai_parse(ai_answer(), "table")
  ctx2 <- tfl_ai_context("table", outputs = c("T-14-1-1", "T-14-9-9"))
  expect_true(any(grepl("has no report T-14-9-9", tfl_ai_check(a1, ctx2)$problem, fixed = TRUE)))
  broken <- tfl_ai_parse(c("```yaml", "tflspec_ai: {task: table, version: 1}", "outputs:",
                           "  - id: T-1", "    content: [", "```"), "table")
  expect_identical(broken$problems$where, "block")
  expect_error(tfl_ai_unpack(broken, withr::local_tempdir()), "no report")
  expect_error(tfl_ai_diff(NULL, a1), "report files")
})

# ---- the files run -------------------------------------------------------------

test_that("the unpacked report files run to RTF on the XPT data", {
  skip_on_cran()
  skip_if_not_installed("rtfreporter")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  d <- ai_table_study()
  tfl_ai_unpack(ai_answer(), d)
  st <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  ard <- suppressMessages(tfl_build_ard(st, dir = d, save = FALSE))
  expect_setequal(unique(ard$output_id), names(outs))
  want <- c(`T-14-1-1` = "Age group, n (%)", `T-14-2-1` = "Objective response rate (CR + PR)",
            `T-14-3-1` = "Subjects with at least one TEAE", `T-14-3-5` = "Aspartate Aminotransferase")
  for (id in names(outs)) {
    o <- tfl_resolve_output(outs[[id]], st)
    plan <- suppressMessages(tfl_content_run(o, ard[ard$output_id == id, ]))
    f <- file.path(d, paste0(id, ".rtf"))
    suppressMessages(rtfreporter::generate_rtfreport(tfl_report(o, content = plan, study = st), f,
                                                     overwrite = TRUE))
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_match(txt, want[[id]], fixed = TRUE, info = id)
    n <- c(`T-14-1-1` = "(N=80)", `T-14-2-1` = "(N=82)", `T-14-3-1` = "(N=80)")
    if (id %in% names(n)) expect_match(txt, n[[id]], fixed = TRUE, info = id)
  }
})
