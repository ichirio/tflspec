# Every help page as R CMD check reads it: checkRd() finds nothing (a lost
# brace, text outside a section, a misplaced \item).  The template pages' own
# text: test-tpl-docs.R

test_that("every Rd page passes checkRd()", {
  db <- tryCatch(tools::Rd_db("tflspec"), error = function(e) list())
  if (!length(db)) {
    # (a source tree, devtools::load_all(): its man/ files)
    man <- system.file("man", package = "tflspec")
    f <- if (nzchar(man)) list.files(man, "[.]Rd$", full.names = TRUE) else character()
    db <- stats::setNames(lapply(f, tools::parse_Rd), basename(f))
  }
  skip_if(!length(db), "no Rd pages")
  for (nm in names(db)) {
    expect_identical(as.character(tools::checkRd(db[[nm]])), character(), label = nm)
  }
})
