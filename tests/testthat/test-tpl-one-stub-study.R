# Templates of one row-heading column in a study whose standard folds a
# stub (tfl_ai_study_init()'s: plan_stub(name = "row_label", before = TRUE)):
# the column is the stub as it is (rtfreporter #701)

test_that("ae_pt_freq and ae_overview draw in a study of the default standard", {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "ST-1")
  st0 <- tfl_read_study_spec(d)
  stub <- Filter(function(it) identical(it$fn, "plan_stub"), st0$defaults$content)
  expect_identical(stub[[1L]]$args$name, "row_label")
  ids <- c(ae_pt_freq = "T-9-4", ae_overview = "T-9-5")
  for (nm in names(ids)) {
    tfl_write_output(tfl_output(ids[[nm]], "table", population = "SAF", label = nm,
                                extends = "defaults",
                                template = list(name = nm, args = list(population = "SAF"))),
                     file.path(d, "spec", "outputs", paste0(ids[[nm]], ".yml")))
  }
  st <- tfl_read_study_spec(d)
  for (nm in names(ids)) {
    id <- ids[[nm]]
    ard <- suppressMessages(tfl_build_ard(st, output_id = id, dir = d, save = FALSE))
    ro <- tfl_resolve_output(tfl_read_output(file.path(d, "spec", "outputs",
                                                       paste0(id, ".yml"))), st)
    p <- suppressMessages(tfl_content_run(ro, ard))
    f <- tempfile(fileext = ".rtf")
    suppressMessages(rtfreporter::generate_rtfreport(tfl_report(ro, content = p, study = st), f,
                                                     overwrite = TRUE, program = "p.R"))
    rtf <- paste(readLines(f, warn = FALSE), collapse = "
")
    unlink(f)
    # the row headings and the stub's heading are printed
    expect_match(rtf, c(ae_pt_freq = "Any TEAE", ae_overview = "Subjects with")[[nm]],
                 fixed = TRUE, info = nm)
  }
})
