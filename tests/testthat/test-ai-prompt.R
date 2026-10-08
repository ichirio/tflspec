# The drafting prompts (tfl_ai_prompt()) are text a model reads: what they
# say is pinned by golden snapshots (a change to the prompt files, to
# tfl_fig_parts() or to the TOC fields shows as a snapshot change), and
# their schema is checked against what tflspec reads.

ai_toc_context <- function() {
  specs <- list.files(system.file("extdata", "ard-spec", package = "tflspec"),
                      pattern = "\\.xlsx$")
  ids <- setdiff(sub("\\.xlsx$", "", specs), c("study", "report"))
  tfl_ai_context(
    "toc",
    study = list(study_id = "CDISCPILOT01",
                 title = "Safety and Efficacy of the Xanomeline Transdermal Therapeutic System"),
    reports = data.frame(output_id = ids, title = paste("Report", ids))
  )
}

ai_figure_context <- function(design = NULL) {
  adam <- tfl_example_adam()
  tfl_ai_context(
    "figure", output_id = "F-14-2-1",
    study = list(study_id = "CDISCPILOT01"),
    titles = c("Figure 14.2.1", "Kaplan-Meier Plot of Overall Survival"),
    population = list(id = "FAS", label = "Full Analysis Set",
                      where = "FASFL == \"Y\""),
    datasets = adam[c("ADSL", "ADTTE")], design = design
  )
}

ai_words <- function(x) length(strsplit(trimws(x), "\\s+")[[1L]])

test_that("golden prompts: toc and figure, chat and api", {
  expect_snapshot(cat(format(tfl_ai_prompt("toc", ai_toc_context()))))
  expect_snapshot(cat(format(tfl_ai_prompt(
    "toc", ai_toc_context(), mode = "api", format = "json",
    documents = list(list(
      name = "SAP", kind = "sap", section = "14",
      text = "14.1.1 Demographics (Safety Population)\n14.2.1 Overall survival (FAS)"
    ))
  ))))
  expect_snapshot(cat(format(tfl_ai_prompt("figure", ai_figure_context()))))
  design <- tfl_fig_template("km_simple", data = "ADTTE", param = "OS",
                             pop = "FASFL", group = "TRT01P")
  expect_snapshot(cat(format(tfl_ai_prompt(
    "figure", ai_figure_context(design), mode = "api", example = FALSE,
    documents = list(name = "SAP", section = "9.4",
                     text = "Overall survival is shown by Kaplan-Meier curves by arm.")
  ))))
})

test_that("the prompts stay within their size budget", {
  p <- tfl_ai_prompt("figure", ai_figure_context())
  expect_lt(ai_words(p$system), 6000)
  expect_lt(ai_words(tfl_ai_prompt("toc", ai_toc_context())$system), 1500)
  expect_identical(unname(p$nchar), c(nchar(p$system), nchar(p$user)))
})

test_that("a prompt has its parts, every placeholder filled", {
  for (task in tfl_ai_tasks()$task) {
    ctx <- if (task == "toc") ai_toc_context() else ai_figure_context()
    for (mode in c("chat", "api")) {
      for (fmt in c("yaml", "json")) {
        p <- tfl_ai_prompt(task, ctx, mode = mode, format = fmt)
        expect_s3_class(p, "tfl_ai_prompt")
        txt <- format(p)
        expect_false(grepl("{{", txt, fixed = TRUE), label = paste(task, mode, fmt))
        expect_true(grepl(paste0("```", fmt), p$system, fixed = TRUE))
        expect_identical(p$messages[[1L]], list(role = "system", content = p$system))
        expect_identical(p$messages[[2L]], list(role = "user", content = p$user))
        expect_identical(length(p$attach) > 0L, mode == "chat")
        expect_match(p$system, "language of the user's message", fixed = TRUE)
      }
    }
  }
  expect_output(print(tfl_ai_prompt("toc")), "tflspec_ai: {task: toc, version: 1}",
                fixed = TRUE)
})

test_that("the language line follows the context's language", {
  ctx <- tfl_ai_context("toc", language = "Japanese")
  p <- tfl_ai_prompt("toc", ctx)
  expect_match(p$system, "Write the assumptions in Japanese.", fixed = TRUE)
  expect_false(grepl("language of the user's message", p$system, fixed = TRUE))
})

test_that("each prompt's example answer reads and checks clean", {
  for (task in tfl_ai_tasks()$task) {
    for (fmt in c("yaml", "json")) {
      txt <- tflspec:::.ai_example_text(task, fmt)
      a <- tfl_ai_parse(txt, task)
      expect_identical(a$format, fmt)
      expect_identical(nrow(tfl_ai_check(a)), 0L, label = paste(task, fmt))
    }
  }
  # the figure's example is the template's design, read back whole
  a <- tfl_ai_parse(tflspec:::.ai_example_text("figure", "yaml"), "figure")
  ex <- tflspec:::.ai_example("figure")
  expect_equal(a$design$layers, ex$layers)
  expect_equal(a$design$plot, ex$plot)
})

test_that("the toc schema names exactly tfl_read_toc()'s fields", {
  s <- tfl_ai_schema("toc")
  expect_s3_class(s, "tfl_ai_text")
  named <- sub("^- `([a-z_]+)`.*$", "\\1",
               grep("^- `", strsplit(s, "\n")[[1L]], value = TRUE))
  expect_setequal(named, tflspec:::.toc_fields)
  expect_setequal(tflspec:::.ai_toc_columns()$column, tflspec:::.toc_fields)
  # the default columns are fields the reader maps
  expect_true(all(tflspec:::.ai_toc_default_fields %in% tflspec:::.toc_fields))
})

test_that("the figure schema names every piece and field of tfl_fig_parts()", {
  s <- strsplit(tfl_ai_schema("figure"), "\n")[[1L]]
  parts <- tfl_fig_parts()
  pieces <- setdiff(unique(parts$piece), c("plot", "plot_add"))
  listed <- sub("^- `([A-Za-z0-9_.]+)`:.*$", "\\1",
                grep("^- `[A-Za-z0-9_.]+`: .* Fields:$", s, value = TRUE))
  expect_setequal(listed, pieces)
  fields <- sub("^  - `([A-Za-z0-9_.]+)`.*$", "\\1", grep("^  - `", s, value = TRUE))
  expect_setequal(unique(fields), unique(parts$field))
  # each piece's own fields under it
  at <- grep("^- `km_curve`:", s)
  expect_identical(
    sub("^  - `([a-z_]+)`.*$", "\\1", s[at + seq_len(2L)]),
    parts$field[parts$piece == "km_curve"]
  )
  # the templates offered are the templates made of pieces
  tp <- tfl_fig_templates()
  tl <- sub("^- `([a-z0-9_]+)`:.*$", "\\1",
            grep("^- `[a-z0-9_]+`: .*\\(reads ", s, value = TRUE))
  expect_setequal(tl, tp$template[tp$parts])
  # every kind used has its meaning
  expect_true(all(unique(parts$kind) %in% names(tflspec:::.ai_fig_kinds())))
})

test_that("tfl_ai_schema() describes only the parts asked for", {
  s <- tfl_ai_schema("figure", c("stats", "censor_mark"))
  expect_match(s, "### stats", fixed = TRUE)
  expect_match(s, "`censor_mark`", fixed = TRUE)
  expect_false(grepl("### data", s, fixed = TRUE))
  expect_false(grepl("`km_curve`", s, fixed = TRUE))
  expect_error(tfl_ai_schema("figure", "colours"), "no part 'colours'")
  expect_error(tfl_ai_schema("toc", "tables"), "no part")
})

test_that("tfl_ai_tasks() lists the tasks; the later ones are refused", {
  t <- tfl_ai_tasks()
  expect_identical(t$task, c("toc", "figure"))
  expect_error(tfl_ai_prompt("table"), "not available yet")
  expect_error(tfl_ai_prompt("tables"), "no task 'tables'")
})

test_that("tfl_ai_context() checks its fields", {
  expect_error(tfl_ai_context("toc", output_id = "T-1"), "toc context has no 'output_id'")
  expect_error(tfl_ai_context("figure"), "needs the `output_id`")
  expect_error(tfl_ai_context("figure", output_id = c("A", "B")), "one non-blank string")
  expect_error(tfl_ai_context("toc", study = list(id = "X")), "has no 'id'")
  expect_error(tfl_ai_context("toc", toc_fields = "colour"), "toc_fields")
  expect_error(tfl_ai_context("figure", output_id = "F", templates = "pie"), "templates")
  expect_error(tfl_ai_context("figure", output_id = "F", design = list()), "tfl_fig_design")
  expect_error(tfl_ai_context("figure", output_id = "F", datasets = data.frame(x = 1)),
               "`dataset` and `variable`")
  ctx <- tfl_ai_context("toc", toc_fields = c("title", "type"))
  expect_identical(ctx$toc_fields, c("output_id", "type", "title"))
  expect_identical(tfl_ai_context("toc")$toc_fields,
                   tflspec:::.ai_toc_default_fields)
  expect_output(print(ctx), "<tfl_ai_context: toc>", fixed = TRUE)
})

test_that("a context carries the datasets' names and labels, never their values", {
  adam <- tfl_example_adam()
  ctx <- tfl_ai_context("figure", output_id = "F", datasets = adam["ADTTE"])
  d <- ctx$datasets
  expect_identical(unique(d$dataset), "ADTTE")
  expect_setequal(d$variable, names(adam$ADTTE))
  expect_true(all(is.na(d$levels)))
  user <- tfl_ai_prompt("figure", ctx)$user
  expect_match(user, "| AVAL | Analysis Value | numeric |", fixed = TRUE)
  expect_false(grepl("PFS", user, fixed = TRUE))   # a PARAMCD value
  # given as a catalog, the values come only when the caller gives them
  cat <- data.frame(dataset = "adtte", variable = c("PARAMCD", "AVAL"),
                    levels = c("OS | PFS", NA))
  ctx <- tfl_ai_context("figure", output_id = "F", datasets = cat)
  expect_identical(ctx$datasets$dataset, c("ADTTE", "ADTTE"))
  expect_match(tfl_ai_prompt("figure", ctx)$user, "OS \\| PFS", fixed = TRUE)
})

test_that("documents are for the api mode, fenced", {
  ctx <- tfl_ai_context("toc")
  doc <- list(name = "SAP", text = "See </document> here")
  expect_error(tfl_ai_prompt("toc", ctx, documents = doc), "mode \"api\"")
  expect_error(tfl_ai_prompt("toc", ctx, mode = "api", documents = list(list(x = 1))),
               "`name`")
  p <- tfl_ai_prompt("toc", ctx, mode = "api", documents = doc)
  expect_match(p$user, "<document name=\"SAP\">\nSee <\\/document> here\n</document>",
               fixed = TRUE)
  p <- tfl_ai_prompt("toc", ctx, mode = "api")
  expect_match(p$user, "No document text is included", fixed = TRUE)
})

test_that("the prompt's context and task must agree", {
  expect_error(tfl_ai_prompt("figure"), "needs a context")
  expect_error(tfl_ai_prompt("figure", tfl_ai_context("toc")), "for the toc task")
  expect_error(tfl_ai_prompt("toc", list(task = "toc")), "tfl_ai_context")
})

test_that("the YAML written follows the prompt's quoting rule and reads back", {
  w <- tflspec:::.ai_yaml_flow
  expect_identical(w("Time (Months)"), "Time (Months)")
  expect_identical(w("{n} ({p}%)"), "\"{n} ({p}%)\"")
  expect_identical(w("ADSL | ADAE"), "\"ADSL | ADAE\"")
  expect_identical(w("yes"), "\"yes\"")
  expect_identical(w("1"), "\"1\"")
  expect_identical(w(1.5), "1.5")
  expect_identical(w(TRUE), "true")
  expect_identical(w(list(a = "x", b = c(1, 2))), "{a: x, b: [1, 2]}")
  expect_identical(w(tfl_fig_r("scales::percent")), "!r \"scales::percent\"")
  vals <- list("a: b", "say \"hi\"", "line\nbreak", " pad", "%d", "-30")
  for (v in vals) {
    expect_identical(yaml::yaml.load(paste0("x: ", w(v)))$x, v)
  }
})
