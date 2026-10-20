# Every template's report as a program without tflspec (the items, not the
# tpl_*() call; the ARD helpers and the figure setup written in it), run
# where tflspec is not installed: the same RTF (a table, a listing) or PNG
# (a figure) as the template run here.  A template joins by its example
# (.tpl_examples).

export_png <- function(p, f) {
  ggplot2::ggsave(f, p, width = 9, height = 5, dpi = 72)
  unname(tools::md5sum(f))
}

# One template's example as a report of a study made from XPT data: the
# RTF or PNG here (the template run) and from its program there
export_check <- function(name, args) {
  t <- tflspec:::.tpl_get(name, "test")
  x <- tfl_template_expand(name, args)
  ds <- tolower(unique(c("ADSL", tflspec:::.tpl_data_read(x))))
  d <- tempfile("exp-")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  for (n in ds) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", n)),
                     file.path(d, "adam", paste0(n, ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "EXP")
  id <- paste0("X-", gsub("_", "-", name))
  o <- tfl_output(id, t$kind, population = "SAF", label = name, extends = "defaults",
                  template = list(name = name, args = args))
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  tfl_write_output(o, f)
  st <- tfl_read_study_spec(d)
  o <- tfl_read_output(f)
  ro <- tfl_resolve_output(o, st)
  data <- lapply(stats::setNames(nm = ds), function(n)
    as.data.frame(haven::read_xpt(file.path(d, "adam", paste0(n, ".xpt")))))
  p <- tflspec:::.export_program(o, st)
  code <- p$code
  out <- file.path(d, if (p$kind == "figure") "out.png" else "out.rtf")
  here <- tempfile(fileext = if (p$kind == "figure") ".png" else ".rtf")
  old <- setwd(d)
  on.exit(setwd(old), add = TRUE)
  if (p$kind == "figure") {
    if (length(ro$ard$analyses %||% list())) {
      data$ard <- suppressMessages(tfl_build_ard(st, output_id = id, dir = d, save = FALSE))
    }
    md5_here <- export_png(suppressMessages(tfl_content_run(ro, data)), here)
    run <- c(code, "",
             sprintf("ggplot2::ggsave(%s, content, width = 9, height = 5, dpi = 72)",
                     encodeString(out, quote = "\"")))
  } else {
    if (p$kind == "listing") {
      # its data as the listing's program makes it
      e <- new.env(parent = .fig_pkg_env("dplyr"))
      dl <- code[seq_len(match("# ---- the content ----", code) - 1L)]
      eval(parse(text = dl[!grepl("^library\\(", dl)]), e)
      input <- e$data
    } else {
      input <- suppressMessages(tfl_build_ard(st, output_id = id, dir = d, save = FALSE))
    }
    plan <- suppressMessages(tfl_content_run(ro, input))
    suppressMessages(rtfreporter::generate_rtfreport(tfl_report(ro, content = plan, study = st),
                                                    here, overwrite = TRUE, program = "p.R"))
    md5_here <- unname(tools::md5sum(here))
    run <- c("options(rtfreporter.render_time = as.POSIXct(\"2000-01-01 00:00:00\", tz = \"UTC\"))",
             code, "",
             sprintf("rtfreporter::generate_rtfreport(doc, %s, overwrite = TRUE, program = \"p.R\")",
                     encodeString(out, quote = "\"")))
  }
  res <- export_run(run, d)
  list(name = name, kind = p$kind, code = code, log = res, status = attr(res, "status"),
       here = md5_here, there = if (file.exists(out)) unname(tools::md5sum(out)) else NA)
}

test_that("every template's report: its program runs without tflspec, the same file", {
  skip_on_cran()
  for (p in c("cards", "cardx", "dplyr", "haven", "rtfreporter", "ggplot2", "ggsurvfit",
              "patchwork", "survival", "broom")) {
    skip_if_not_installed(p)
  }
  op <- options(rtfreporter.render_time = as.POSIXct("2000-01-01 00:00:00", tz = "UTC"))
  on.exit(options(op))
  ex <- tflspec:::.tpl_examples
  exports <- getNamespaceExports("tflspec")
  for (nm in names(ex)) {
    # a template with no data of its own (km_table: the report's own
    # analyses) has no example report to run
    if (!length(tflspec:::.tpl_data_read(tfl_template_expand(nm, ex[[nm]])))) next
    r <- export_check(nm, ex[[nm]])
    expect_identical(r$status, 0L, info = paste(nm, paste(utils::tail(r$log, 15), collapse = "\n")))
    expect_true("tflspec: no" %in% r$log, info = nm)
    expect_false(any(grepl("tflspec::", r$code, fixed = TRUE)), info = nm)
    expect_identical(intersect(export_calls(r$code), exports), character(), info = nm)
    expect_identical(r$there, r$here, info = nm)
  }
})
