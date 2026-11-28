# The branches no other test reaches in R/ai_adam.R (the ADaM summary, the
# study folder to start) and R/ai_agent.R (tfl_ai_run() and its files:
# built.csv, status.md, toc.yml, progress.md, agent_inputs.yml).  Error and
# edge paths first.  Fictional data only.

cov_ns <- function() asNamespace("tflspec")

cov_adam_dir <- function(env = parent.frame()) {
  d <- withr_tempdir(env)
  dir.create(file.path(d, "adam"))
  d
}

# ---- R/ai_adam.R: reading the ADaM --------------------------------------------

test_that("tfl_ai_adam_summary(): a path that does not exist stops, naming it and the working folder", {
  d <- cov_adam_dir()
  expect_error(tfl_ai_adam_summary(file.path(d, "no-such-folder")),
               "no folder or file .*no-such-folder.* in .*`path` is the folder of the ADaM files")
})

test_that("tfl_ai_adam_summary(): a path of several strings stops as a path that does not exist", {
  expect_error(tfl_ai_adam_summary(c("a", "b")), "no folder or file 'a b'")
})

test_that("tfl_ai_adam_summary(): a folder with no ADaM file stops, saying to copy them there", {
  d <- cov_adam_dir()
  writeLines("not data", file.path(d, "adam", "readme.txt"))
  expect_error(tfl_ai_adam_summary(file.path(d, "adam")),
               "no .xpt, .sas7bdat, .rds or .parquet file in")
})

test_that("tfl_ai_adam_summary(): one file (not a folder) is read as its dataset", {
  d <- cov_adam_dir()
  f <- file.path(d, "adam", "adxx.rds")
  saveRDS(data.frame(USUBJID = c("S-01", "S-02", "S-02"), AVAL = c(1, 2, 3)), f)
  s <- tfl_ai_adam_summary(f)
  expect_identical(s$datasets$dataset, "ADXX")
  expect_identical(s$datasets$records, 3L)
  expect_identical(s$datasets$subjects, 2L)
})

test_that("tfl_ai_adam_summary(): a dataset with no label: NA in the frame, none in the heading", {
  s <- tfl_ai_adam_summary(list(adxx = data.frame(USUBJID = "S-01", AVAL = 1)))
  expect_identical(s$datasets$label, NA_character_)
  expect_match(format(s), "## ADXX (1 records, 1 subjects, 2 variables)", fixed = TRUE)
})

test_that("tfl_ai_adam_summary(): a dataset's label: in the frame and in its heading", {
  d <- data.frame(USUBJID = "S-01", AVAL = 1)
  attr(d, "label") <- "Fictional Analysis Dataset"
  s <- tfl_ai_adam_summary(list(adxx = d))
  expect_identical(s$datasets$label, "Fictional Analysis Dataset")
  expect_match(format(s), "## ADXX: Fictional Analysis Dataset (1 records", fixed = TRUE)
})

test_that("tfl_ai_adam_summary(): a .sas7bdat file is read with haven", {
  skip_if_not_installed("haven")
  d <- cov_adam_dir()
  f <- file.path(d, "adam", "adxx.sas7bdat")
  ok <- tryCatch({
    suppressWarnings(haven::write_sas(data.frame(USUBJID = c("S-01", "S-02"), AVAL = c(1, 2)), f))
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(ok && file.exists(f), "haven cannot write a .sas7bdat file here")
  s <- tfl_ai_adam_summary(file.path(d, "adam"))
  expect_identical(s$datasets$dataset, "ADXX")
  expect_identical(s$datasets$records, 2L)
})

test_that("tfl_ai_adam_summary(): a .parquet file is read with arrow", {
  skip_if_not_installed("arrow")
  d <- cov_adam_dir()
  arrow::write_parquet(data.frame(USUBJID = "S-01", AVAL = 1), file.path(d, "adam", "adxx.parquet"))
  s <- tfl_ai_adam_summary(file.path(d, "adam"))
  expect_identical(s$datasets$dataset, "ADXX")
})

test_that("tfl_ai_adam_summary(): a code variable whose pair is numeric has no codes", {
  s <- tfl_ai_adam_summary(list(adsl = data.frame(USUBJID = c("S-01", "S-02"),
                                                  AGEGR1N = c(1, 2), AGEGR1 = c(1, 2))))
  expect_identical(s$variables$codes[s$variables$variable == "AGEGR1N"], NA_character_)
})

# ---- R/ai_adam.R: a define merged in ------------------------------------------

cov_define <- function() {
  ns <- cov_ns()
  v <- ns$.define_variables_empty()
  v[1:2, c("dataset", "variable", "label")] <- list(c("ADSL", "ADXX"), c("AGE", "FOO"),
                                                    c("Age in years", "Foo"))
  v$digits <- c(0L, NA)
  v$format <- c(NA_character_, NA_character_)
  v$codelist <- c(NA_character_, NA_character_)
  vl <- ns$.define_vl_empty()
  vl[1:2, c("dataset", "variable", "where", "label")] <- list(c("ADSL", "ADSL"), c("AGE", "AGE"),
                                                              c(NA, ""), c("Age (years)", NA))
  vl$digits <- c(1L, NA)
  vl$format <- c(NA, "8.1")
  ns$.define_new(ns$.define_study_empty(), ns$.define_datasets_empty(), v,
                 ns$.define_codelists_empty(), ns$.define_external_empty(),
                 source = list(file = "define.xml", kind = "define.xml"), value_level = vl)
}

test_that("a define: a variable the data has not is passed over; a label the data lacks is the define's", {
  s <- tfl_ai_adam_summary(list(adsl = data.frame(USUBJID = "S-01", AGE = 30)), define = cov_define())
  v <- s$variables
  expect_false("FOO" %in% v$variable)
  expect_identical(v$label[v$variable == "AGE"], "Age in years")
  expect_identical(v$define[v$variable == "AGE"], "digits 0")
})

test_that("a define's value level with no where clause: its label, else '(a value level entry)'", {
  s <- tfl_ai_adam_summary(list(adsl = data.frame(USUBJID = "S-01", AGE = 30)), define = cov_define())
  txt <- format(s)
  expect_match(txt, "- ADSL AGE (Age (years)): digits 1", fixed = TRUE)
  expect_match(txt, "- ADSL AGE (a value level entry): format 8.1", fixed = TRUE)
})

# ---- R/ai_adam.R: tfl_ai_study_init() -------------------------------------------

test_that("tfl_ai_study_init(): an ADaM folder with no file stops, naming the folder and the study", {
  d <- cov_adam_dir()
  expect_error(tfl_ai_study_init(d), "no .xpt, .sas7bdat, .rds or .parquet file in .*give `adam` as their folder from")
  expect_false(file.exists(file.path(d, "study.yml")))
})

test_that("tfl_ai_study_init(): no ADSL: no analysis set but ALL", {
  d <- cov_adam_dir()
  saveRDS(data.frame(USUBJID = "S-01", AEDECOD = "Headache"), file.path(d, "adam", "adae.rds"))
  tfl_ai_study_init(d)
  y <- yaml::read_yaml(file.path(d, "spec", "data.yml"))
  expect_identical(vapply(y$populations, `[[`, "", "id"), "ALL")
  expect_identical(y$datasets[[1L]]$path, "adam/adae.rds")
})

test_that("tfl_ai_study_init(): an ADSL with no population flag: no analysis set but ALL", {
  d <- cov_adam_dir()
  saveRDS(data.frame(USUBJID = "S-01", SEXFL = "Y"), file.path(d, "adam", "adsl.rds"))
  tfl_ai_study_init(d)
  y <- yaml::read_yaml(file.path(d, "spec", "data.yml"))
  expect_identical(vapply(y$populations, `[[`, "", "id"), "ALL")
})

# ---- R/ai_agent.R: paths ------------------------------------------------------

test_that(".ai_norm_path(): NA stays NA", {
  expect_identical(cov_ns()$.ai_norm_path(NA_character_), NA_character_)
})

test_that(".ai_norm_path(): a file not there yet is put under its nearest folder that is", {
  d <- withr_tempdir()
  p <- cov_ns()$.ai_norm_path(file.path(d, "not", "yet.yml"))
  expect_identical(p, paste0(normalizePath(d, winslash = "/"), "/not/yet.yml"))
})

test_that(".ai_from_study(): no path (NULL, empty, NA) is NA", {
  ns <- cov_ns()
  d <- withr_tempdir()
  expect_identical(ns$.ai_from_study(d, NULL), NA_character_)
  expect_identical(ns$.ai_from_study(d, character()), NA_character_)
  expect_identical(ns$.ai_from_study(d, NA_character_), NA_character_)
})

test_that(".ai_from_study(): a path inside the study, not there yet, is the part below it", {
  d <- withr_tempdir()
  expect_identical(cov_ns()$.ai_from_study(d, file.path(d, "programs", "t1.R")), "programs/t1.R")
})

# ---- R/ai_agent.R: ai/built.csv -------------------------------------------------

test_that(".ai_built_read(): a file of an older tflspec (no problem column): every row built", {
  d <- withr_tempdir()
  dir.create(file.path(d, "ai"))
  utils::write.csv(data.frame(output_id = "T-1", key = "k1", rtf = "t1.rtf", png = ""),
                   file.path(d, "ai", "built.csv"), row.names = FALSE)
  b <- cov_ns()$.ai_built_read(d)
  expect_identical(b$output_id, "T-1")
  expect_identical(b$problem, NA_character_)
})

test_that(".ai_built_read(): a file without the columns reads as none built", {
  d <- withr_tempdir()
  dir.create(file.path(d, "ai"))
  utils::write.csv(data.frame(foo = 1), file.path(d, "ai", "built.csv"), row.names = FALSE)
  b <- cov_ns()$.ai_built_read(d)
  expect_identical(nrow(b), 0L)
  expect_named(b, c("output_id", "key", "rtf", "png", "problem"))
})

test_that(".ai_built_key(): a report file there changes the key", {
  ns <- cov_ns()
  d <- withr_tempdir()
  withr::local_dir(d)
  k0 <- ns$.ai_built_key("T-1", "common")
  dir.create(file.path("spec", "outputs"), recursive = TRUE)
  writeLines("id: T-1", file.path("spec", "outputs", "T-1.yml"))
  k1 <- ns$.ai_built_key("T-1", "common")
  expect_false(identical(k0, k1))
})

# ---- R/ai_agent.R: next steps and status.md -------------------------------------

cov_run_res <- function(problem) {
  list(reports = data.frame(request = c("r1", "r2")[seq_along(problem)],
                            output_id = c("T-1", "T-2")[seq_along(problem)],
                            status = "ok", rtf = NA_character_, rtf_problem = problem,
                            built = NA_character_, first = NA_character_,
                            stringsAsFactors = FALSE),
       next_steps = stats::setNames(rep("x", length(problem)), c("r1", "r2")[seq_along(problem)]))
}

test_that(".ai_run_next(): a package missing: no_package, the package named, the answer not blamed", {
  n <- cov_ns()$.ai_run_next(cov_run_res("Error: there is no package called 'zzpkg'"))
  expect_identical(unname(n$next_kind["r1"]), "no_package")
  expect_identical(n$next_args$r1, list(report = "T-1", package = "zzpkg"))
  expect_match(n$next_steps[["r1"]], "install.packages(\"zzpkg\")", fixed = TRUE)
})

test_that(".ai_status_text(): no next step is '(no request)'; drafts no request asks for are listed", {
  res <- cov_run_res(c(NA, "a | b"))
  res$next_steps <- character()
  res$stray <- c("X-1", "X-2")
  txt <- cov_ns()$.ai_status_text(res)
  expect_true("- (no request)" %in% txt)
  expect_true(any(grepl("Not read (no request asks for them): X-1, X-2", txt, fixed = TRUE)))
  # (a | in a problem would break the table: written as /)
  expect_true(any(grepl("| a / b |", txt, fixed = TRUE)))
})

# ---- R/ai_agent.R: spec/toc.yml -------------------------------------------------

cov_toc_shell <- function() {
  list(toc = data.frame(output_id = c("T-14-1-1", "L-16-2-1"), key = c("T14.1.1", "L16.2.1"),
                        type = c(NA, "listing"), title = c("Demographics", NA),
                        text = c("Safety Population", "x"),
                        parts = c("T-14-1-1a | T-14-1-1b", NA), stringsAsFactors = FALSE),
       cells = NULL)
}

test_that(".ai_toc_yml(): a report in parts is one entry a part; no type is a table", {
  d <- withr_tempdir()
  dir.create(file.path(d, "spec"))
  writeLines(c("ai:", "  shell_code: '^S[0-9]+$'"), file.path(d, "study.yml"))
  f <- cov_ns()$.ai_toc_yml(d, cov_toc_shell())
  y <- yaml::read_yaml(f)$outputs
  expect_identical(vapply(y, `[[`, "", "id"), c("T-14-1-1a", "T-14-1-1b", "L-16-2-1"))
  expect_identical(vapply(y, `[[`, "", "kind"), c("table", "table", "listing"))
  expect_identical(y[[1L]]$section, "14.1")
  expect_null(y[[3L]]$label)
})

test_that(".ai_population_of(): a data.yml with no populations is none", {
  d <- withr_tempdir()
  dir.create(file.path(d, "spec"))
  writeLines("datasets: []", file.path(d, "spec", "data.yml"))
  expect_null(cov_ns()$.ai_population_of(d, "Safety Population"))
})

# ---- R/ai_agent.R: ai/progress.md -----------------------------------------------

cov_progress <- function(lines) {
  d <- withr_tempdir(parent.frame())
  dir.create(file.path(d, "ai"))
  writeLines(lines, file.path(d, "ai", "progress.md"))
  d
}
cov_built <- data.frame(request = c("q1", "q2"), rtf_problem = NA_character_,
                        built = "built", stringsAsFactors = FALSE)

test_that(".ai_progress_built(): no progress file: nothing done", {
  d <- withr_tempdir()
  expect_null(cov_ns()$.ai_progress_built(d, cov_built, 1:2))
  expect_false(file.exists(file.path(d, "ai", "progress.md")))
})

test_that(".ai_progress_built(): a progress file with no table is left as it is", {
  lines <- c("# Progress", "", "no table here")
  d <- cov_progress(lines)
  expect_null(cov_ns()$.ai_progress_built(d, cov_built, 1:2))
  expect_identical(readLines(file.path(d, "ai", "progress.md")), lines)
})

test_that(".ai_progress_built(): a table with no status column is left as it is", {
  lines <- c("| request | reports |", "|---|---|", "| q1 | T-1 |")
  d <- cov_progress(lines)
  expect_null(cov_ns()$.ai_progress_built(d, cov_built, 1:2))
  expect_identical(readLines(file.path(d, "ai", "progress.md")), lines)
})

test_that(".ai_progress_built(): a cell with no spaces gets one each side; a row with no closing bar keeps none", {
  d <- cov_progress(c("|request|reports|status|", "|---|---|---|", "|q1|T-1|todo|", "| q2 | T-2 | todo"))
  cov_ns()$.ai_progress_built(d, cov_built, 1:2)
  x <- readLines(file.path(d, "ai", "progress.md"))
  expect_identical(x[3L], "|q1|T-1| built |")
  expect_identical(x[4L], "| q2 | T-2 | built ")
})

# ---- R/ai_agent.R: ai/agent_inputs.yml --------------------------------------------

test_that(".ai_agent_inputs_write(): fewer docs_from than docs: the rest empty", {
  ns <- cov_ns()
  d <- withr_tempdir()
  ns$.ai_agent_inputs_write(d, TRUE, "adam", "book.xlsx", "r.md", c("a.md", "b.md"), "x", 3L)
  x <- ns$.ai_agent_inputs_read(d)
  expect_identical(x$docs, c("a.md", "b.md"))
  expect_identical(x$docs_from, c("x", ""))
})

test_that(".ai_agent_inputs_write(add = TRUE): the docs there kept first, the new ones after", {
  ns <- cov_ns()
  d <- withr_tempdir()
  ns$.ai_agent_inputs_write(d, TRUE, "adam", "book.xlsx", "r.md", c("a.md", "b.md"), c("x", "y"), 3L)
  ns$.ai_agent_inputs_write(d, TRUE, "adam", "book.xlsx", "r.md", c("b.md", "c.md"), c("y2", "z"), 3L,
                            add = TRUE)
  x <- ns$.ai_agent_inputs_read(d)
  expect_identical(x$docs, c("a.md", "b.md", "c.md"))
  expect_identical(x$docs_from, c("x", "y2", "z"))
})

test_that(".ai_agent_inputs_read(): a file with fewer docs_from than docs: the rest empty; defaults", {
  d <- withr_tempdir()
  dir.create(file.path(d, "ai"))
  yaml::write_yaml(list(docs = list("a.md", "b.md"), docs_from = list("x")),
                   file.path(d, "ai", "agent_inputs.yml"))
  x <- cov_ns()$.ai_agent_inputs_read(d)
  expect_identical(x$docs_from, c("x", ""))
  expect_false(x$templates)
  expect_identical(x$max_reads, 3L)
})

# ---- R/ai_agent.R: tfl_ai_run() ---------------------------------------------------

cov_run_study <- function(env = parent.frame()) {
  skip_on_cran()
  skip_if_not_installed("haven")
  root <- withr_tempdir(env)
  ad <- file.path(root, "adam")
  dir.create(ad)
  for (x in c("adsl", "adae", "adrs", "adlb", "adtte")) {
    df <- getExportedValue("tflspec", x)
    for (v in names(df)) if (is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
    haven::write_xpt(df, file.path(ad, paste0(x, ".xpt")))
  }
  d <- file.path(root, "study")
  tfl_ai_prepare(unit = "toc", d, system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec"),
                 adam = normalizePath(ad, winslash = "/"), quiet = TRUE, size = 3L)
  ai_drafts_from_fixtures(d, only = "demographics-1")
  d
}

test_that("tfl_ai_run(): the study's reports that cannot be read: each built report says so", {
  d <- cov_run_study()
  real <- tfl_read_outputs
  res <- with_mocked_bindings(
    suppressMessages(tfl_ai_run(d, rtf = FALSE)),
    tfl_read_outputs = function(dir, ...) {
      if (identical(dir, ".")) stop("the outputs folder is unreadable") else real(dir, ...)
    },
    .package = "tflspec")
  r <- res$reports[res$reports$output_id == "T-14-1-1", ]
  expect_identical(r$status, "ok")
  expect_identical(r$rtf_problem, "the study could not be read: the outputs folder is unreadable")
})

test_that("tfl_ai_run(): the study that cannot be read: each built report says so", {
  d <- cov_run_study()
  real <- tfl_read_study_spec
  res <- with_mocked_bindings(
    suppressMessages(tfl_ai_run(d, rtf = FALSE)),
    tfl_read_study_spec = function(dir, ...) {
      if (identical(dir, ".")) stop("study.yml is unreadable") else real(dir, ...)
    },
    .package = "tflspec")
  r <- res$reports[res$reports$output_id == "T-14-1-1", ]
  expect_identical(r$rtf_problem, "the study could not be read: study.yml is unreadable")
})

test_that("tfl_ai_run(): an ARD with no output_id column is taken whole", {
  d <- cov_run_study()
  res <- with_mocked_bindings(
    suppressMessages(tfl_ai_run(d, rtf = FALSE)),
    tfl_build_ard = function(...) data.frame(stat = 1),
    .package = "tflspec")
  r <- res$reports[res$reports$output_id == "T-14-1-1", ]
  expect_identical(r$built, "built")
  expect_identical(r$rtf_problem, NA_character_)
})
