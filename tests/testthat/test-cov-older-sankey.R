# R/sankey.R and R/sankey_batch.R: the argument checks and the branches no
# other test reaches -- a zero-size layout, an empty palette, a named palette
# by line, link colors by target, the batch's filters, reference subgroup,
# cap and PNG files.  Fictional treatment lines only.

cov_sk_nodes <- function() {
  data.frame(id = c("A", "B", "C"), stage = c("L1", "L2", "L2"),
             trt = c("Drug X", "Drug X", "Control"), line = c("1", "2", "2"),
             n = c(15, 10, 5), stringsAsFactors = FALSE)
}

cov_sk_links <- function() {
  data.frame(source = c("A", "A"), target = c("B", "C"), value = c(10, 5),
             arm = c("X", "Y"), stringsAsFactors = FALSE)
}

cov_sk_plot <- function(...) tfl_plot_sankey(cov_sk_nodes(), cov_sk_links(), ...)

# the fills of the node rectangles, in node order (the second last layer)
cov_sk_fills <- function(p) ggplot2::layer_data(p, length(p$layers) - 1L)$fill

# ---- tfl_plot_sankey(): the argument checks --------------------------------

test_that("tfl_plot_sankey(): nodes that are no rows, or links that are not a data frame, are refused", {
  expect_error(tfl_plot_sankey(cov_sk_nodes()[0L, ], cov_sk_links()), "`nodes` must be a non-empty data.frame")
  expect_error(tfl_plot_sankey(list(id = "A"), cov_sk_links()), "`nodes` must be a non-empty data.frame")
  expect_error(tfl_plot_sankey(cov_sk_nodes(), NULL), "`links` must be a data.frame")
})

test_that("tfl_plot_sankey(): a node or link column not there is named", {
  expect_error(tfl_plot_sankey(cov_sk_nodes(), cov_sk_links(), node_stage = "STAGE"),
               "Missing node columns: STAGE")
  expect_error(tfl_plot_sankey(cov_sk_nodes(), cov_sk_links(), link_value = "n"),
               "Missing link columns: n")
})

test_that("tfl_plot_sankey(): the label, value, treatment and line columns must be in nodes", {
  expect_error(cov_sk_plot(node_label = "lbl"), "`node_label` column not found")
  expect_error(cov_sk_plot(node_value = "N"), "`node_value` column not found")
  expect_error(cov_sk_plot(node_treatment = "TRT"), "`node_treatment` column not found")
  expect_error(cov_sk_plot(node_line = "LINE"), "`node_line` column not found")
})

test_that("tfl_plot_sankey(): sizes that are not positive numbers are refused", {
  expect_error(cov_sk_plot(adaptive_max_multiplier = 0), "`adaptive_max_multiplier` must be a positive number")
  expect_error(cov_sk_plot(adaptive_max_multiplier = "3"), "`adaptive_max_multiplier` must be a positive number")
  expect_error(cov_sk_plot(node_width = 0), "`node_width` must be > 0")
  expect_error(cov_sk_plot(node_gap = -1), "`node_gap` must be NULL")
  expect_error(cov_sk_plot(node_gap = c(0.1, 0.2)), "`node_gap` must be NULL")
  expect_error(cov_sk_plot(node_gap = NA_real_), "`node_gap` must be NULL")
  expect_error(cov_sk_plot(label_gap_scale = NA_real_), "`label_gap_scale` must be a single number >= 0")
  expect_error(cov_sk_plot(bezier_n = 4), "`bezier_n` must be >= 8")
})

# ---- tfl_plot_sankey(): the layout and colours -----------------------------

test_that("link_fill as one colour a link keeps each link's colour; a wrong length takes the default", {
  p <- cov_sk_plot(link_fill = c("#111111", "#222222"))
  poly <- ggplot2::layer_data(p, 1L)
  expect_setequal(unique(poly$fill), c("#111111", "#222222"))
  p2 <- cov_sk_plot(link_fill = c("#111111", "#222222", "#333333"))
  expect_identical(unique(ggplot2::layer_data(p2, 1L)$fill), "#CCCCCC")
})

test_that("nodes with no size and no links: the automatic gap and the scale fall back to 1", {
  nodes <- data.frame(id = c("A", "B"), stage = c("L1", "L2"), stringsAsFactors = FALSE)
  links <- data.frame(source = "A", target = "B", value = 0)
  p <- tfl_plot_sankey(nodes, links, node_min_size = 0)
  expect_s3_class(p, "ggplot")
  r <- ggplot2::layer_data(p, length(p$layers) - 1L)
  expect_identical(nrow(r), 2L)
  expect_false(anyNA(r$ymin))
})

test_that("an empty treatment palette gives every treatment the one default colour", {
  p <- cov_sk_plot(node_treatment = "trt", treatment_palette = character())
  expect_identical(unique(cov_sk_fills(p)), "#4F6D7A")
})

test_that("by line, the palette starts afresh for each line (rotated)", {
  p <- cov_sk_plot(node_treatment = "trt", node_line = "line", treatment_color_mode = "by_line",
                   treatment_palette = c("#AA0000", "#00AA00", "#0000AA"))
  # line 1 takes the palette as it is, line 2 rotated by one
  expect_identical(cov_sk_fills(p), c("#AA0000", "#00AA00", "#0000AA"))
})

test_that("by line, a palette of one colour is that colour on every line", {
  p <- cov_sk_plot(node_treatment = "trt", node_line = "line", treatment_color_mode = "by_line",
                   treatment_palette = "#AA0000")
  expect_identical(unique(cov_sk_fills(p)), "#AA0000")
})

test_that("by line, a named palette fixes a treatment's colour on every line", {
  p <- cov_sk_plot(node_treatment = "trt", node_line = "line", treatment_color_mode = "by_line",
                   treatment_palette = c(`Drug X` = "#AA0000", Control = "#0000AA"))
  expect_identical(cov_sk_fills(p), c("#AA0000", "#AA0000", "#0000AA"))
})

test_that("adaptive scale with no shared maximum: the plot's own scale", {
  a <- cov_sk_plot(scale_mode = "adaptive")
  b <- cov_sk_plot(scale_mode = "auto")
  expect_identical(ggplot2::layer_data(a, length(a$layers) - 1L)$ymax,
                   ggplot2::layer_data(b, length(b$layers) - 1L)$ymax)
})

test_that("links coloured by their target node", {
  p <- cov_sk_plot(node_fill = c("#111111", "#222222", "#333333"), use_link_color_by_target = TRUE)
  poly <- ggplot2::layer_data(p, 1L)
  expect_setequal(unique(poly$fill), c("#222222", "#333333"))
})

# ---- tfl_plot_sankey_batch() -------------------------------------------------

cov_sk_specs <- function() {
  data.frame(subgroup = c("x", "y"), link_filter = c("arm == 'X'", "arm == 'Y'"),
             stringsAsFactors = FALSE)
}

cov_sk_batch <- function(specs = cov_sk_specs(), ...) {
  tfl_plot_sankey_batch(cov_sk_nodes(), cov_sk_links(), specs, save_png = FALSE, ...)
}

test_that("tfl_plot_sankey_batch(): nodes, links and subgroup_specs must be data frames with rows", {
  expect_error(tfl_plot_sankey_batch(cov_sk_nodes()[0L, ], cov_sk_links(), cov_sk_specs()),
               "`nodes` must be a non-empty data.frame")
  expect_error(tfl_plot_sankey_batch(cov_sk_nodes(), "links", cov_sk_specs()),
               "`links` must be a data.frame")
  expect_error(tfl_plot_sankey_batch(cov_sk_nodes(), cov_sk_links(), cov_sk_specs()[0L, ]),
               "`subgroup_specs` must be a non-empty data.frame")
})

test_that("subgroup_specs without a subgroup column: subgroup_1, subgroup_2 ...", {
  res <- cov_sk_batch(data.frame(link_filter = c("arm == 'X'", "arm == 'Y'")))
  expect_identical(names(res$plots), c("subgroup_1", "subgroup_2"))
  expect_identical(res$metadata$file_name, c("sankey_subgroup_1.png", "sankey_subgroup_2.png"))
})

test_that("scale_reference_subgroup must be one text; one not there warns and takes the first", {
  expect_error(cov_sk_batch(scale_reference_subgroup = c("x", "y")),
               "`scale_reference_subgroup` must be NULL or a single character string")
  expect_error(cov_sk_batch(scale_reference_subgroup = 1),
               "`scale_reference_subgroup` must be NULL or a single character string")
  expect_warning(res <- cov_sk_batch(scale_strategy = "shared_first_stage", scale_reference_subgroup = "z"),
                 "not found in `subgroup_specs`")
  ref <- cov_sk_batch(scale_strategy = "shared_first_stage", scale_reference_subgroup = "x")
  expect_identical(res$metadata$shared_scale_max, ref$metadata$shared_scale_max)
})

test_that("first_stage_max_multiplier: NULL is no cap; zero, NA or two numbers are refused", {
  res <- cov_sk_batch(scale_strategy = "first_stage_normalized", first_stage_max_multiplier = NULL)
  expect_identical(res$metadata$multiplier, c(1, 2))
  expect_error(cov_sk_batch(first_stage_max_multiplier = 0), "must be a positive number, `Inf`, or `NULL`")
  expect_error(cov_sk_batch(first_stage_max_multiplier = NA_real_), "must be a positive number")
  expect_error(cov_sk_batch(first_stage_max_multiplier = c(1, 2)), "must be a positive number")
})

test_that("a filter that is not TRUE/FALSE, or of the wrong length, is refused", {
  expect_error(cov_sk_batch(data.frame(subgroup = "x", link_filter = "value * 2")),
               "Filter expression must return logical values: value \\* 2")
  expect_error(cov_sk_batch(data.frame(subgroup = "x", link_filter = "c(TRUE, FALSE, TRUE)")),
               "Filter expression length mismatch")
})

test_that("a filter of one TRUE keeps every row", {
  res <- cov_sk_batch(data.frame(subgroup = "all", link_filter = "TRUE"))
  expect_identical(res$metadata$local_scale_max,
                   cov_sk_batch(data.frame(subgroup = "all", link_filter = ""))$metadata$local_scale_max)
})

test_that("the batch's own column names: nodes or links without them are refused", {
  expect_error(tfl_plot_sankey_batch(cov_sk_nodes(), cov_sk_links(), cov_sk_specs(),
                                     node_id = "ID", save_png = FALSE),
               "Subgroup nodes are missing required columns")
  expect_error(tfl_plot_sankey_batch(cov_sk_nodes(), cov_sk_links(), cov_sk_specs(),
                                     link_value = "N", save_png = FALSE),
               "Subgroup links are missing required columns")
})

test_that("subgroups with no flow: the scales fall back to 1 and to the smallest node", {
  nodes <- data.frame(id = c("A", "B"), stage = c("L1", "L2"), stringsAsFactors = FALSE)
  links <- data.frame(source = "A", target = "B", value = 0)
  res <- tfl_plot_sankey_batch(nodes, links, data.frame(subgroup = "none"), save_png = FALSE,
                               node_min_size = 0, node_gap = 0,
                               scale_strategy = "first_stage_normalized")
  expect_identical(res$metadata$local_scale_max, 1)
  expect_identical(res$metadata$shared_scale_max, 1)
  expect_identical(res$metadata$first_stage_span, 0)
})

test_that("shared first stage with no flow and no minimum size: the shared scale falls back to 1", {
  nodes <- data.frame(id = c("A", "B"), stage = c("L1", "L2"), stringsAsFactors = FALSE)
  links <- data.frame(source = "A", target = "B", value = 0)
  res <- tfl_plot_sankey_batch(nodes, links, data.frame(subgroup = "none"), save_png = FALSE,
                               node_min_size = 0, node_gap = 0,
                               scale_strategy = "shared_first_stage")
  expect_identical(res$metadata$shared_scale_max, 1)
})

test_that("first-stage normalization scales node_value with the links", {
  res <- cov_sk_batch(scale_strategy = "first_stage_normalized", node_value = "n",
                      node_gap = 0, node_min_size = 0)
  # (node sizes are given: each subgroup's first stage is node A's 15, the same)
  expect_identical(res$metadata$multiplier, c(1, 1))
  # a subgroup of node C alone (size 5): magnified x3, its node with it
  sub <- data.frame(subgroup = c("x", "c"), node_filter = c("", "id == 'C'"),
                    link_filter = c("", "arm == 'none'"))
  res2 <- cov_sk_batch(sub, scale_strategy = "first_stage_normalized", node_value = "n",
                       node_gap = 0, node_min_size = 0)
  expect_identical(res2$metadata$multiplier, c(1, 3))
  r <- ggplot2::layer_data(res2$plots$c, length(res2$plots$c$layers) - 1L)
  expect_equal(r$ymax - r$ymin, 15)
})

test_that("an empty subgroup name or file name: subgroup_<i> and sankey_<name>.png", {
  specs <- data.frame(subgroup = c("", NA), file_name = c(NA, ""), title = c(NA, "Second"),
                      link_filter = c("arm == 'X'", "arm == 'Y'"), stringsAsFactors = FALSE)
  res <- cov_sk_batch(specs)
  expect_identical(names(res$plots), c("subgroup_1", "subgroup_2"))
  expect_identical(res$metadata$file_name, c("sankey_subgroup_1.png", "sankey_subgroup_2.png"))
  expect_identical(res$metadata$title, c("", "Second"))
})

test_that("save_png writes one PNG a subgroup into output_dir, made when not there", {
  skip_on_cran()
  d <- file.path(withr_tempdir(), "figs", "sankey")
  res <- tfl_plot_sankey_batch(cov_sk_nodes(), cov_sk_links(), cov_sk_specs(), output_dir = d,
                               width = 3, height = 2, dpi = 30)
  expect_true(dir.exists(d))
  expect_setequal(list.files(d), c("sankey_x.png", "sankey_y.png"))
})

test_that("save_png with no output_dir writes into the working directory", {
  skip_on_cran()
  d <- withr_tempdir()
  withr::local_dir(d)
  tfl_plot_sankey_batch(cov_sk_nodes(), cov_sk_links(),
                        data.frame(subgroup = "x", file_name = "one.png", link_filter = "arm == 'X'"),
                        width = 3, height = 2, dpi = 30)
  expect_true(file.exists(file.path(d, "one.png")))
})
