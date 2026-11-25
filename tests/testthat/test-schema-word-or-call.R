# A type of a word or a call: plan_style(border = ) takes "tfl", "none" or a
# call of rtf_border() -- rtfreporter reads no other word -- and
# rtf_document(watermark = ) a text or a call of rtf_watermark().  A call
# written as R names its function with or without its package.

tsw_check <- function(fn, a, v) tfl_schema_check(tfl_item(fn, stats::setNames(list(v), a)))

test_that("plan_style(border = ): tfl, none or an rtf_border() call; another word is refused", {
  ok <- function(v) nrow(tsw_check("plan_style", "border", v))
  expect_identical(ok("tfl"), 0L)
  expect_identical(ok("none"), 0L)
  expect_identical(ok(tfl_call("rtf_border", list(top = "single"))), 0L)
  expect_identical(ok(tfl_r("rtf_border(top = \"single\")")), 0L)
  expect_identical(ok(tfl_r("rtfreporter::rtf_border(top = \"single\", inside_v = \"single\")")), 0L)
  p <- tsw_check("plan_style", "border", "single")
  expect_identical(p$level, "error")
  expect_match(p$message, "one of tfl, none", fixed = TRUE)
  # a call of another function, with or without its package: refused
  expect_gt(ok(tfl_r("rtfreporter::rtf_watermark(\"DRAFT\")")), 0L)
  expect_gt(ok(tfl_r("paste(\"a\")")), 0L)
})

test_that("rtf_document(watermark = ): a text stays a text, a call of rtf_watermark() a call", {
  ok <- function(v) nrow(tsw_check("rtf_document", "watermark", v))
  expect_identical(ok("DRAFT"), 0L)
  expect_identical(ok(tfl_call("rtf_watermark", list(text = "DRAFT"))), 0L)
  expect_identical(ok(tfl_r("rtfreporter::rtf_watermark(\"DRAFT\")")), 0L)
})
