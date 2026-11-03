# pk_conc: PK concentrations by subject with their summary below (#565),
# from a study's data and ARD (the template-study rule)

pk_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  sl <- tflspec::adsl
  pc <- pk_adpc(sl)
  for (x in list(list("adsl", sl), list("adpc", pc))) {
    haven::write_xpt(tflspec:::.ai_chr(x[[2L]]), file.path(d, "adam", paste0(x[[1L]], ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "PK-1")
  list(dir = d, adpc = pc)
}

pk_report <- function(d, args) {
  f <- file.path(d, "spec", "outputs", "T-PK.yml")
  tfl_write_output(tfl_output("T-PK", "table", population = "SAF", extends = "defaults",
                              template = list(name = "pk_conc", args = args)), f)
  st <- tfl_read_study_spec(d)
  ro <- tfl_resolve_output(tfl_read_output(f), st)
  objs <- tflspec:::.parts_objects(ro, st, dir = d)
  list(ro = ro, objs = objs, plan = suppressMessages(tfl_content_run(ro, objs)))
}

test_that("pk_conc expands to a listing with its summary laid under it", {
  x <- tfl_template_expand("pk_conc", list(population = "PK", blq = "BLQFL == \"Y\"",
                                           split_at = "4 hr"))
  fns <- vapply(x$content, `[[`, "", "fn")
  expect_identical(fns[1:3], c("table_plan", "plan_cells", tflspec:::.spec_part$fn))
  expect_true(all(c("plan_blanks", "plan_col_header", "plan_paginate_rows",
                    "plan_paginate_cols") %in% fns))
  expect_identical(x$data, list(source = "dataset", data = "adpc_pk"))
  part <- x$content[[3L]]$args
  expect_identical(part$data$source, "ard")
  expect_identical(vapply(part$content, `[[`, "", "fn"), c("normalize_ard", "table_plan", "plan_cells"))
  # the listing's records: the time points in order, a BLQ sample missing
  ad <- x$ard$analysis_data
  expect_identical(vapply(ad, `[[`, "", "id"), c("adsl_pk", "adpc_pk", "adpc_pk_s"))
  expect_match(unclass(ad[[2L]]$derive), "ATPT = reorder_by(ATPT, ATPTN)", fixed = TRUE)
  expect_match(unclass(ad[[2L]]$derive), "AVAL = ifelse(BLQFL == \"Y\", NA, AVAL)", fixed = TRUE)
  expect_match(unclass(ad[[3L]]$where), "!is.na(AVAL)", fixed = TRUE)
  expect_identical(x$ard$analyses[[1L]]$by, c("PARAM", "TRT01A", "ATPT"))
  # the page name: the groups' values, a line each
  pr <- Filter(function(it) identical(it$fn, "plan_paginate_rows"), x$content)[[1L]]
  expect_identical(pr$args$heading, "{PARAM}\n{TRT01A}")
})

test_that("pk_conc on a study: each subject's values, BLQ, and the summary's N and mean", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
  skip_if_not(exists(tflspec:::.spec_part$fn, envir = asNamespace("rtfreporter")),
              "rtfreporter without the parts verb")
  s <- pk_study(withr::local_tempdir())
  r <- pk_report(s$dir, list(population = "SAF", blq = "BLQFL == \"Y\""))
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(r$plan, "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  vals <- intersect(pk_times, names(t))
  # the time points in ATPTN's order, not the records' or by name
  expect_identical(vals, pk_times)
  pc <- s$adpc
  one <- pc[pc$USUBJID == pc$USUBJID[1L], ]
  lab <- sprintf("%s/%s/%s", one$USUBJID[1L], one$AGE[1L], one$SEX[1L])
  row <- t[t$row_label == lab, , drop = FALSE]
  expect_identical(nrow(row), 1L)
  # a value to 3 significant digits; Pre-Dose (BLQ) printed BLQ
  v1 <- one$AVAL[one$ATPT == "1 hr"]
  expect_identical(row[["1 hr"]], formatC(signif(v1, 3), digits = 3, format = "fg", flag = "#"))
  expect_identical(row[["Pre-Dose"]], "BLQ")
  # the summary under the subjects: N and the mean of the non-missing values
  # (the statistics' labels indented)
  lbl <- trimws(gsub(" ", " ", t$row_label, fixed = TRUE))
  expect_true("Summary Statistics" %in% lbl)
  arm <- one$TRT01A[1L]
  grp <- t$TRT01A == arm
  at <- pc[pc$TRT01A == arm & pc$ATPT == "2 hr" & !is.na(pc$AVAL), "AVAL"]
  n_row <- which(grp & lbl == "N")
  m_row <- which(grp & lbl == "Mean")
  expect_length(n_row, 1L)
  expect_identical(t[n_row, "2 hr"], as.character(length(at)))
  expect_identical(t[m_row, "2 hr"], formatC(signif(mean(at), 4), digits = 4, format = "fg", flag = "#"))
  # a time point where some samples are BLQ (missing): N counts the others
  at24 <- pc[pc$TRT01A == arm & pc$ATPT == "24 hr" & !is.na(pc$AVAL), "AVAL"]
  expect_identical(t[n_row, "24 hr"], as.character(length(at24)))
})

test_that("pk_conc builds through the study's program to an RTF, a page an arm", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
  skip_if_not(exists(tflspec:::.spec_part$fn, envir = asNamespace("rtfreporter")),
              "rtfreporter without the parts verb")
  s <- pk_study(withr::local_tempdir())
  r <- pk_report(s$dir, list(population = "SAF", blq = "BLQFL == \"Y\"", split_at = "4 hr"))
  pg <- suppressMessages(rtfreporter::plan_apply(r$plan, "pages"))
  # two arms x two column blocks
  expect_length(pg, 4L)
  # the blank row between the listing and the summary: the table's, not
  # the study's plan_blanks() between groups
  bl <- Filter(function(it) identical(it$fn, "plan_blanks"), r$ro$content)
  expect_identical(bl[[length(bl)]]$args$where, "between_parts")
  # the program: the summary's plan from the ARD, laid under the listing's
  # from the records (the items, not one template call)
  code <- tfl_content_code(r$ro)
  expect_true(any(grepl("^summary <- ard \\|>", code)))
  expect_true(any(grepl("plan_append(part = summary, name = \"summary\")", code, fixed = TRUE)))
  expect_false(any(grepl("tfl_tpl(", code, fixed = TRUE)))
  f <- suppressMessages(tfl_run_output("T-PK", dir = s$dir))
  f <- file.path(s$dir, "output", "tfl", "T-PK.rtf")
  expect_true(file.exists(f))
  rtf <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(rtf, "Summary Statistics", fixed = TRUE)
  expect_match(rtf, "Drug X (ng/mL)", fixed = TRUE)
})

test_that("an AI draft with pk_conc builds in tfl_ai_run(): the parts' objects, its widths", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
  skip_if_not(exists(tflspec:::.spec_part$fn, envir = asNamespace("rtfreporter")),
              "rtfreporter without the parts verb")
  root <- withr::local_tempdir()
  ad <- file.path(root, "adam")
  dir.create(ad)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)), file.path(ad, paste0(x, ".xpt")))
  }
  haven::write_xpt(tflspec:::.ai_chr(pk_adpc(tflspec::adsl)), file.path(ad, "adpc.xpt"))
  d <- file.path(root, "study")
  book <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
  tfl_ai_prepare(unit = "toc", d, book, adam = ad, quiet = TRUE, size = 3L)
  # (the labs request answered with the template, as an agent would)
  ai_drafts_from_answer(d, paste(c("```yaml",
    "tflspec_ai: {task: table, version: 1, outputs: [T-14-3-5]}",
    "outputs:",
    "  - id: T-14-3-5",
    "    kind: table",
    "    label: Drug X Plasma Concentrations by Subject",
    "    population: SAF",
    "    extends: [defaults]",
    "    template: {name: pk_conc, args: {blq: BLQFL == \"Y\", split_at: 4 hr}}",
    "    page: {titles: {\"1\": {center: Table 14.3.5}}}",
    "assumptions: []", "```"), collapse = "\n"), "table")
  res <- suppressMessages(tfl_ai_run(d, requests = "labs-1"))
  rp <- res$reports[res$reports$output_id == "T-14-3-5", ]
  expect_identical(rp$status, "ok", info = rp$first)
  expect_true(file.exists(file.path(d, rp$rtf)))
  rtf <- paste(readLines(file.path(d, rp$rtf), warn = FALSE), collapse = "\n")
  expect_match(rtf, "Summary Statistics", fixed = TRUE)
  # its widths written into its file: the subjects' column by name, the
  # value columns (two blocks of them) as .values
  y <- yaml::read_yaml(file.path(d, "ai", "spec", "outputs", "T-14-3-5.yml"))
  pc <- Filter(function(it) identical(names(it), "plan_columns"), y$content)
  w <- pc[[1L]]$plan_columns$widths
  expect_identical(names(w), c("row_label", ".values"))
})
