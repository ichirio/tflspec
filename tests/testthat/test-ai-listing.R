# The AI drafts a listing with the tables: the TOC's listing a request
# (family `listing`), its answer a report of `kind: listing` checked (its
# analysis set and parameter required, as a table's) and built to an RTF

lst_book <- function(f) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TOC")
  openxlsx::writeData(wb, "TOC", data.frame(
    `Output No.` = c("Table 14.1.1", "Listing 16.2.7"),
    Title = c("Demographics", "Listing of Serious Adverse Events"),
    check.names = FALSE), startRow = 2)
  openxlsx::addWorksheet(wb, "t.14.1.1")
  lines <- list("Table 14.1.1  Demographics", "Safety Population", t(c("", "Placebo", "Drug X")),
                t(c("Age (years)", "", "")), t(c("  Mean (SD)", "xx.x (xx.xx)", "xx.x (xx.xx)")))
  for (j in seq_along(lines)) openxlsx::writeData(wb, "t.14.1.1", lines[[j]], startRow = j, colNames = FALSE)
  openxlsx::addWorksheet(wb, "l.16.2.7")
  lines <- list("Listing 16.2.7  Listing of Serious Adverse Events", "Safety Population",
                t(c("Treatment", "Subject", "SOC / PT", "Start day")),
                t(c("xxxxxx", "xxx-xxx", "xxxxxxx / xxxxxx", "xx")))
  for (j in seq_along(lines)) openxlsx::writeData(wb, "l.16.2.7", lines[[j]], startRow = j, colNames = FALSE)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  f
}

lst_answer <- function(id, extra = "") {
  c("```yaml",
    sprintf("tflspec_ai: {task: table, version: 1, outputs: [%s]}", id),
    "outputs:",
    sprintf("  - id: %s", id),
    "    kind: listing",
    "    label: Listing of Serious Adverse Events",
    "    extends: [defaults]",
    "    data: {source: dataset, dataset: ADAE, population: SAF, where: !r 'AESER == \"Y\"', sort: [TRT01A, USUBJID, ASTDY]}",
    "    content:",
    "      - plan_listing:",
    "          type: multiline",
    "          cols:",
    "            - {fn: listing_col, args: {vars: [TRT01A], label: Treatment, collapse_repeats: true}}",
    "            - {fn: listing_col, args: {vars: [USUBJID], label: Subject, collapse_repeats: true}}",
    "            - {fn: listing_col, args: {vars: [AEBODSYS, AEDECOD], label: SOC / PT}}",
    "            - {fn: listing_col, args: {vars: [ASTDY], label: Start day}}",
    extra,
    "    page:",
    "      titles:",
    sprintf("        \"1\": {center: %s}", id),
    "        \"2\": {center: Listing of Serious Adverse Events}",
    "assumptions: []",
    "```")
}

test_that("the TOC's listing: a request of its own, its answer checked and built to an RTF", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  skip_if_not_installed("rtfreporter")
  root <- withr::local_tempdir()
  ad <- file.path(root, "adam")
  dir.create(ad)
  for (x in c("adsl", "adae")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(ad, paste0(x, ".xpt")))
  }
  d <- file.path(root, "study")
  book <- lst_book(file.path(root, "toc.xlsx"))
  invisible(tfl_ai_prepare(unit = "toc", d, book, adam = ad, quiet = TRUE,
                           templates = FALSE))
  rq <- tfl_ai_requests(d)
  # (a request a report in an agent's study: named after it)
  req <- rq$request[grepl("^Listing", rq$request)]
  expect_length(req, 1L)
  id <- rq$outputs[rq$request == req]
  expect_match(id, "16")
  # the instructions tell the AI how a listing is written
  ins <- paste(readLines(file.path(d, "ai", "instructions-table.md"), encoding = "UTF-8"), collapse = "\n")
  expect_match(ins, "### A listing", fixed = TRUE)
  expect_match(ins, "listing_col", fixed = TRUE)
  ai_drafts_from_answer(d, lst_answer(id), "table")
  res <- suppressMessages(tfl_ai_run(d, requests = req))
  r <- res$reports[res$reports$request == req, ]
  expect_identical(r$status, "ok")
  expect_true(file.exists(file.path(d, r$rtf)))
  x <- paste(readLines(file.path(d, r$rtf), warn = FALSE), collapse = " ")
  expect_match(x, "Start day", fixed = TRUE)
})

test_that("a listing's answer: no analysis set, a dataset of parameters not kept, a column not in the data -- errors", {
  skip_on_cran()
  skip_if_not_installed("haven")
  vars <- data.frame(dataset = c(rep("ADAE", 6), rep("ADLB", 3)),
                     variable = c("USUBJID", "AESER", "AEBODSYS", "AEDECOD", "TRT01A", "ASTDY",
                                  "USUBJID", "PARAMCD", "AVAL"),
                     stringsAsFactors = FALSE)
  ctx <- structure(list(task = "table", datasets = vars, populations = c("SAF", "ITT")),
                   class = "tfl_ai_context")
  one <- function(lines) {
    a <- tfl_ai_parse(paste(lines, collapse = "\n"), task = "table")
    tfl_ai_check(a, ctx)
  }
  ok <- one(lst_answer("L-1"))
  expect_false(any(ok$severity == "error"))
  # no analysis set
  nopop <- sub("population: SAF, ", "", lst_answer("L-1"), fixed = TRUE)
  p <- one(nopop)
  expect_true(any(p$severity == "error" & grepl("names no analysis set", p$problem)))
  # a column of no variable of the data
  bad <- sub("vars: [ASTDY]", "vars: [NOPE]", lst_answer("L-1"), fixed = TRUE)
  p <- one(bad)
  expect_true(any(p$severity == "error" & grepl("NOPE is not a variable of ADAE", p$problem)))
  # a lab listing keeping no parameter
  lab <- lst_answer("L-2")
  lab <- sub("dataset: ADAE, population: SAF, where: !r 'AESER == \"Y\"', sort: [TRT01A, USUBJID, ASTDY]",
             "dataset: ADLB, population: SAF, sort: [USUBJID]", lab, fixed = TRUE)
  lab <- lab[!grepl("TRT01A|AEBODSYS|ASTDY", lab)]
  lab <- c(lab[seq_len(grep("cols:", lab))],
           "            - {fn: listing_col, args: {vars: [AVAL], label: Value}}",
           lab[-seq_len(grep("cols:", lab))])
  p <- one(lab)
  expect_true(any(p$severity == "error" & grepl("keeps no parameter", p$problem)))
  # kept by its parameter: fine
  p <- one(sub("population: SAF, sort", "population: SAF, where: !r 'PARAMCD == \"ALT\"', sort", lab, fixed = TRUE))
  expect_false(any(grepl("keeps no parameter", p$problem)))
  # analyses in a listing; a table function in its content
  p <- one(c(lst_answer("L-1")[1:7], "    ard: {analyses: [{id: A, method: categorical, dataset: ADAE, variables: [AESER]}]}",
             lst_answer("L-1")[-(1:7)]))
  expect_true(any(grepl("a listing has no analyses", p$problem)))
})
