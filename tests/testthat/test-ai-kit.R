# The AI kit (tflspec_ai_manual(dir)), the answer it asks for (one Markdown
# text of the study's spec files: tfl_ai_unpack()) and the review of it
# (tfl_ai_review() of the study folder).

kit_dir <- function() {
  d <- file.path(tempdir(), "ai-kit-test")
  if (!dir.exists(d) || length(list.files(d)) < 7L) {
    unlink(d, recursive = TRUE)
    suppressMessages(tflspec_ai_manual(d))
  }
  d
}
kit_file <- function(name) file.path(kit_dir(), name)

kit_toc <- function() {
  tfl_read_toc(kit_file("05_example_toc_shell.xlsx"), sheet = "TOC", skip = 1L,
               map = list(output_id = "Shell Number", title = c("Title1", "Title2"),
                          population = "Population",
                          footnote = c("Footnote1", "Footnote2")))
}

# the example's answer, unpacked into a study folder of its own
kit_study <- function(name = "ai-kit-study") {
  d <- file.path(tempdir(), name)
  unlink(d, recursive = TRUE)
  tfl_ai_unpack(kit_file("06_example_spec.md"), d)
  d
}

test_that("the kit is seven flat files, small enough to attach", {
  d <- file.path(tempdir(), "ai-kit-files")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_message(kit <- tflspec_ai_manual(d), "Attach all files in")
  files <- unname(tflspec:::.ai_kit_files[names(tflspec:::.ai_kit_files) != "metadata"])
  expect_identical(basename(kit), files)       # 07_adam_metadata.csv only when given
  expect_identical(list.files(d), sort(files))
  expect_false(any(dir.exists(file.path(d, list.files(d)))))
  expect_lt(sum(file.size(kit)), 1024^2)
  # the schema is the installed one
  expect_identical(readLines(file.path(d, "03_spec_schema.yml")),
                   readLines(system.file("spec", "schema.yml", package = "tflspec")))
})

test_that("the blank study unpacks, reads, and its items are what the schema takes", {
  d <- withr::local_tempdir()
  files <- tfl_ai_unpack(kit_file("04_blank_spec.md"), d)
  expect_true(all(c("study.yml", "spec/data.yml", "spec/toc.yml", "spec/defaults.yml",
                    "spec/_assumptions.yml") %in%
                    sub(paste0("^", normalizePath(d, "/"), "/"), "", files)))
  st <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  expect_setequal(names(outs), c("T-1", "L-1", "F-1"))
  for (o in outs) {
    if (inherits(o$content, "tfl_r")) next
    for (it in o$content) {
      p <- tfl_schema_check(it)
      expect_identical(nrow(p[p$level == "error", ]), 0L, info = paste(o$id, it$fn))
    }
  }
})

test_that("the example's answer reads, reviews clean, and is the example TOC's", {
  d <- kit_study()
  outs <- tfl_read_outputs(d)
  expect_setequal(names(outs), c("T-1", "T-2", "L-1", "F-1"))
  r <- tfl_ai_review(d, toc = kit_toc(), write = FALSE)
  expect_s3_class(r, "tfl_ai_review")
  expect_identical(sum(r$who == "ai"), 0L)
  expect_setequal(unique(r$rule), c("K09", "K10", "K11", "K13"))
  expect_identical(sum(r$rule == "K09"), nrow(attr(r, "assumptions")))
  # a finding names the file and the key it is in
  expect_true(all(grepl("[.]yml", r$sheet[r$rule != "K09"])))
  # the titles are the TOC's
  st <- tfl_read_study_spec(d)
  t2 <- tfl_resolve_output(outs[["T-2"]], st)
  toc <- kit_toc()
  expect_identical(t2$page$titles[["2"]]$center,
                   toc$titles$center[toc$titles$output_id == "T-2" & toc$titles$line == 2])
})

test_that("the example runs on the pilot data: the ARD, the tables, the listing, the figure", {
  skip_on_cran()
  skip_if_not_installed("pharmaverseadam")
  skip_if_not_installed("rtfreporter")
  d <- kit_study("ai-kit-run")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "data", "adam"), recursive = TRUE, showWarnings = FALSE)
  for (x in c("adsl", "adae", "advs")) {
    saveRDS(getExportedValue("pharmaverseadam", x), file.path(d, "data", "adam", paste0(x, ".rds")))
  }
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, dir = d, save = FALSE))
  # the code list's labels and order are the ARD's
  sex <- ard[ard$output_id == "T-1" & ard$variable == "SEX", ]
  expect_identical(levels(unlist(sex$variable_level)), c("Male", "Female"))
  outs <- tfl_read_outputs(d)
  for (id in c("T-1", "T-2")) {
    o <- tfl_resolve_output(outs[[id]], st)
    plan <- suppressMessages(tfl_content_run(o, ard[ard$output_id == id, ]))
    out <- file.path(d, paste0(id, ".rtf"))
    expect_no_warning(suppressMessages(
      rtfreporter::generate_rtfreport(tfl_report(o, content = plan, study = st), out, overwrite = TRUE)))
    txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
    expect_match(txt, "(N=86)", fixed = TRUE)
    if (id == "T-2") expect_match(txt, "Subjects with at least one TEAE", fixed = TRUE)
  }
  skip_if_not_installed("ggplot2")
  # (the figure as written: the defaults' table items are no figure's --
  # S2's layering keeps them to tables)
  p <- suppressMessages(tfl_content_run(outs[["F-1"]],
                                        list(advs = readRDS(file.path(d, "data", "adam", "advs.rds")))))
  expect_s3_class(p, "ggplot")
})

test_that("tfl_ai_review(): a flawed answer gives the AI its fixes and writes the two prompts", {
  d <- kit_study("ai-kit-flawed")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  t1 <- file.path(d, "spec", "outputs", "T-1.yml")
  x <- readLines(t1)
  x <- sub("center: Demographic Characteristics", "center: Demographics", x, fixed = TRUE)
  x <- sub("      name: row_label", "      nmae: row_label", x, fixed = TRUE)
  writeLines(x, t1)
  t9 <- file.path(d, "spec", "outputs", "T-9.yml")
  writeLines(c("id: T-9", "kind: table", "extends: none", "content:", "  - table_plan: {cols: TRT01A}"), t9)
  unlink(file.path(d, "spec", "_assumptions.yml"))
  r <- NULL
  out <- utils::capture.output(r <- tfl_ai_review(d, toc = kit_toc()))
  ai <- r[r$who == "ai", ]
  expect_true(all(c("K03", "K05", "K08", "K17") %in% ai$rule))
  expect_true("T-9" %in% ai$output_id[ai$rule == "K03"])
  expect_match(ai$message[ai$rule == "K05"], "the TOC has \"Demographic Characteristics\"", fixed = TRUE)
  expect_match(ai$message[ai$rule == "K17"], "nmae", fixed = TRUE)
  p <- attr(r, "paths")
  expect_true(all(file.exists(p)))
  expect_identical(dirname(normalizePath(p[["repair"]], "/")), normalizePath(d, "/"))
  rep_txt <- readLines(p[["repair"]])
  expect_true(any(grepl("spec/outputs/T-1.yml", rep_txt, fixed = TRUE)))
  expect_true(any(grepl("For the AI", out)))
  expect_error(tfl_ai_review(d, toc = kit_toc()), "overwrite")
})
