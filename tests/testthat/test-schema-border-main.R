# A border's, a border line's and a watermark's own arguments are a small form
# (main, with a label in en and ja), not a bare R box.

test_that("rtf_border, rtf_border_line and rtf_watermark args are main with en/ja labels", {
  s <- tfl_schema()
  chk <- function(fn, args) {
    d <- tfl_arg_defs(fn = fn)
    expect_true(all(args %in% d$name), label = fn)
    for (a in args) {
      x <- s[[fn]]$args[[a]]
      expect_true(isTRUE(x$main), label = paste0(fn, "$", a, " main"))
      expect_true(is.character(x$label$en) && nzchar(x$label$en), label = paste0(fn, "$", a, " en"))
      expect_true(is.character(x$label$ja) && nzchar(x$label$ja), label = paste0(fn, "$", a, " ja"))
    }
  }
  chk("rtf_border", c("all", "top", "bottom", "left", "right", "inside_h", "inside_v"))
  chk("rtf_border_line", c("style", "width", "color"))
  chk("rtf_watermark", names(s$rtf_watermark$args))
})
