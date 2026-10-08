# tfl_ai_diff(): what an answer changes, item by item.

ai_diff_toc <- function(...) {
  tfl_ai_parse(c("```yaml", "tflspec_ai: {task: toc, version: 1}", "toc:", ...,
                 "```"), "toc")
}

test_that("toc rows: added, changed field by field, removed, same", {
  cur <- data.frame(output_id = c("T-1", "T-2", "T-3"),
                    title = c("Demographics", "Disposition", "AEs"),
                    population = c("Safety", "Safety ", NA))
  new <- ai_diff_toc(
    "- {output_id: T-1, title: Demographics, population: Safety}",
    "- {output_id: T-2, title: Subject Disposition, population: All Subjects}",
    "- {output_id: T-4, title: Labs}"
  )
  d <- tfl_ai_diff(cur, new)
  expect_identical(d$status, c("same", "changed", "changed", "added", "removed"))
  expect_identical(d$key, c("T-1", "T-2", "T-2", "T-4", "T-3"))
  expect_identical(d$field[2:3], c("title", "population"))
  expect_identical(d$was[2:3], c("Disposition", "Safety"))
  expect_identical(d$now[2:3], c("Subject Disposition", "All Subjects"))
  expect_identical(unique(d$part), "toc")
  # an answer against an answer, and against nothing
  expect_identical(tfl_ai_diff(new, new)$status, rep("same", 3L))
  expect_identical(tfl_ai_diff(NULL, new)$status, rep("added", 3L))
})

test_that("normalized text: blanks and spaces are not changes", {
  cur <- data.frame(output_id = "T-1", title = " A ", footnote = "")
  new <- data.frame(output_id = "T-1", title = "A", footnote = NA)
  expect_identical(tfl_ai_diff(cur, new, "toc")$status, "same")
})

test_that("a figure: one piece changed, one inserted, one removed, one moved", {
  cur <- tfl_fig_design(
    data = list(list(step = "read", dataset = "ADTTE"),
                list(step = "param", value = "OS"),
                list(step = "flag", variable = "FASFL")),
    plot = list(x_label = "Days", legend = "inside"),
    layers = list(list(layer = "km_curve"),
                  list(layer = "censor_mark", shape = "x"),
                  list(layer = "risk_table")),
    template = "km_risk_table"
  )
  new <- cur
  new$data <- cur$data[c(1, 3, 2)]                   # param moved after flag
  new$plot$x_label <- "Months"                       # changed
  new$plot$legend <- NULL                            # back to its default
  new$layers <- list(cur$layers[[1]],
                     list(layer = "km_ci", alpha = 0.3),  # inserted
                     list(layer = "censor_mark", shape = "plus"))  # changed
  # risk_table removed
  d <- tfl_ai_diff(cur, new, "figure")
  st <- function(part, key) d$status[d$part == part & d$key == key]
  expect_identical(st("data", "read"), "same")
  expect_identical(st("data", "flag"), "same")
  expect_identical(st("data", "param"), "moved")
  expect_identical(unlist(d[d$status == "moved", c("was", "now")]),
                   c(was = "2", now = "3"))
  p <- d[d$part == "plot", ]
  expect_identical(p$field, c("x_label", "legend"))
  expect_identical(p$was, c("Days", "inside"))
  expect_identical(p$now, c("Months", "bottom"))
  expect_identical(st("layers", "km_curve"), "same")
  expect_identical(st("layers", "km_ci"), "added")
  expect_identical(st("layers", "censor_mark"), "changed")
  expect_identical(d$was[d$key == "censor_mark"], "x")
  expect_identical(st("layers", "risk_table"), "removed")
  expect_identical(st("design", "design"), "same")
})

test_that("a field left out is its default, not a change", {
  cur <- tfl_fig_design(plot = list(legend = "bottom", dpi = 300),
                        layers = list(list(layer = "censor_mark", shape = "x",
                                           size = 3)))
  new <- tfl_fig_design(plot = list(dpi = 300L),
                        layers = list(list(layer = "censor_mark")))
  d <- tfl_ai_diff(cur, new, "figure")
  expect_true(all(d$status == "same"))
  new$plot$legend <- "none"
  d <- tfl_ai_diff(cur, new, "figure")
  expect_identical(unlist(d[d$part == "plot", c("field", "was", "now")]),
                   c(field = "legend", was = "bottom", now = "none"))
})

test_that("an inserted piece of a name already there leaves the old one same", {
  cur <- tfl_fig_design(layers = list(list(layer = "km_curve"),
                                      list(layer = "hline", yintercept = 0.5)))
  new <- cur
  new$layers <- list(cur$layers[[1]], list(layer = "hline", yintercept = 0.25),
                     cur$layers[[2]])
  d <- tfl_ai_diff(cur, new, "figure")
  expect_identical(d$status[d$part == "layers"], c("same", "added", "same"))
  expect_identical(d$key[d$part == "layers"], c("km_curve", "hline", "hline#2"))
})

test_that("a design from nothing is all added; a parsed answer works", {
  a <- tfl_ai_parse(readLines(test_path("fixtures", "ai", "figure-yaml.md")),
                    "figure")
  d <- tfl_ai_diff(NULL, a)
  expect_true(all(d$status[d$part %in% c("data", "stats", "layers")] == "added"))
  expect_identical(d$now[d$part == "design"], "km_risk_table")
  expect_true(all(tfl_ai_diff(a$design, a)$status == "same"))
})

test_that("a composed figure is compared plot by plot", {
  km <- tfl_fig_template("km_simple")
  box <- tfl_fig_template("box_by_group")
  cur <- tfl_fig_design(plots = list(km = km, box = box))
  km2 <- km
  km2$plot$title <- "OS"
  new <- tfl_fig_design(plots = list(km = km2))
  d <- tfl_ai_diff(cur, new, "figure")
  expect_identical(d$field[d$part == "plots$km/plot"], "title")
  expect_identical(d$status[d$part == "plots" & d$key == "box"], "removed")
})

test_that("tfl_ai_diff() checks its arguments", {
  expect_error(tfl_ai_diff(NULL, list()), "give the `task`")
  expect_error(tfl_ai_diff(list(), tfl_fig_design(), "figure"), "tfl_fig_design")
  expect_error(tfl_ai_diff(data.frame(x = 1), NULL, "toc"), "output_id")
})
