# A page configuration is a style set: complete for the page step (the
# settings and the fixed lines of the four bands in two blocks, `first` and
# `last`); the page step reads it, the template, the report and what is
# forced over it, and nothing from the study's files.

pc_dir <- function(defaults = NULL, study = NULL, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  file.copy(list.files(src, full.names = TRUE), d, recursive = TRUE)
  if (!is.null(defaults)) writeLines(defaults, file.path(d, "defaults.yml"))
  if (!is.null(study)) writeLines(study, file.path(d, "study.yml"))
  d
}
pc_defaults <- c("content:", "  - plan_cells: {categorical: \"{n} ({p:.1f%})\"}")
pc_acme <- c(
  "styles:",
  "  acme:",
  "    page:",
  "      rtf_page: {orientation: portrait, margin_left_in: 0.75}",
  "      rtf_style: {font_size_half_points: 16}",
  "      header:",
  "        first:",
  "          - {left: \"ACME\", right: \"Page {PAGE} of {TOTAL_PAGES}\"}",
  "        last:",
  "          - {center: \"{OUTPUT_LABEL}\"}",
  "      footer:",
  "        last:",
  "          - {left: \"{PROGRAM}\", right: \"{DATETIME}\"}",
  "          - {left: \"Confidential\"}",
  "    content:",
  "      - plan_paginate_rows: {max_rows: 20}")
pc_code <- function(o, s) paste(tfl_report_code(tfl_resolve_output(o, s), content = "plan", study = s),
                                collapse = "\n")
pc_report <- function(d) tfl_read_outputs(d)[["T-14-1-1"]]

test_that("a defaults.yml with page: stops, naming the page configuration", {
  d <- pc_dir(c(pc_defaults, "page:", "  header:", "    1: {left: X}"))
  expect_error(tfl_read_study_spec(d), "`page:` moved to a page configuration \\(styles:\\)")
})

test_that("extends: {page: ...} stops: the page comes from the page configuration", {
  expect_error(tfl_output("T-1", "table", extends = list(page = "defaults")),
               "the page comes from the page configuration \\(styles:\\)")
  d <- pc_dir()
  f <- file.path(d, "outputs", "T-14-1-1.yml")
  writeLines(sub("extends: [defaults]", "extends: {page: none}", readLines(f), fixed = TRUE), f)
  expect_error(tfl_read_outputs(d), "the page comes from the page configuration")
})

test_that("a study set named as a built-in one stops", {
  for (nm in c("tfl_standard", "tfl_small")) {
    d <- pc_dir(c(pc_defaults, "styles:", sprintf("  %s:", nm), "    page: {rtf_page: {orientation: portrait}}"))
    expect_error(tfl_read_study_spec(d), sprintf("%s is a built-in: copy it and save it under your own name", nm))
  }
})

test_that("a configuration's keys are a closed list", {
  env <- environment()
  bad <- function(lines) pc_dir(c(pc_defaults, "styles:", "  acme:", lines), env = env)
  expect_error(tfl_read_study_spec(bad(c("    page: {rtf_bogus: {a: 1}}"))), "no key 'rtf_bogus'")
  expect_error(tfl_read_study_spec(bad(c("    data: {}"))), "unknown key 'data'")
  expect_error(tfl_read_study_spec(bad(c("    page: {tokens: {A: b}}"))), "tokens moved to study.yml")
  expect_error(tfl_read_study_spec(bad(c("    page: {header: {first: [{left: a}], 1: {left: b}}}"))),
               "mixes numbered lines and the blocks")
  # titles are a band of a configuration too
  s <- tfl_read_study_spec(bad(c("    page: {titles: {first: [{center: T}]}}")))
  expect_identical(names(s$styles$acme$page$titles), "first")
})

test_that("a band prints the configuration's first lines, the body, then the last lines", {
  d <- pc_dir(c(pc_defaults, pc_acme))
  s <- tfl_read_study_spec(d)
  o <- pc_report(d)
  o$style <- "acme"
  o$page$header <- list(`1` = list(left = "BODY"))
  ro <- tfl_resolve_output(o, s)
  hl <- vapply(ro$page$header, function(l) paste(unlist(l), collapse = "|"), "")
  expect_identical(unname(hl), c("ACME|Page {PAGE} of {TOTAL_PAGES}", "BODY", "{OUTPUT_LABEL}"))
  # the configuration's settings; the report's own change one
  expect_identical(ro$page$rtf_page$margin_left_in, 0.75)
  o$page$rtf_page <- list(orientation = "landscape")
  ro2 <- tfl_resolve_output(o, s)
  expect_identical(ro2$page$rtf_page$orientation, "landscape")
  expect_identical(ro2$page$rtf_page$margin_left_in, 0.75)
  # the configuration's content half is a layer of the content step
  fns <- vapply(ro$content, `[[`, "", "fn")
  expect_identical(ro$content[[match("plan_paginate_rows", fns)]]$args$max_rows, 30L)
  o$content <- Filter(function(it) it$fn != "plan_paginate_rows", o$content)
  fns <- vapply(tfl_resolve_output(o, s)$content, `[[`, "", "fn")
  expect_identical(tfl_resolve_output(o, s)$content[[match("plan_paginate_rows", fns)]]$args$max_rows, 20L)
})

test_that("footer: {last: {1: !unset}} drops the configuration's program line, the others stay", {
  d <- pc_dir(c(pc_defaults, pc_acme))
  s <- tfl_read_study_spec(d)
  o <- pc_report(d)
  o$style <- "acme"
  expect_match(pc_code(o, s), "{PROGRAM}", fixed = TRUE)
  o$page$footer <- list(last = list(`1` = tfl_unset()))
  code <- pc_code(o, s)
  expect_false(grepl("{PROGRAM}", code, fixed = TRUE))
  expect_match(code, "Confidential", fixed = TRUE)
  # a whole band off
  o$page$footer <- tfl_unset()
  expect_false(grepl("Confidential", pc_code(o, s), fixed = TRUE))
})

test_that("the default configuration: study.yml settings.style, else tfl_standard; study tokens from study.yml", {
  d <- pc_dir(c(pc_defaults, pc_acme), "settings: {style: acme}\ntokens: {COMPANY: Acme Pharma}")
  s <- tfl_read_study_spec(d)
  o <- pc_report(d)
  expect_identical(tfl_resolve_output(o, s)$page$rtf_page$orientation, "portrait")
  expect_identical(tfl_resolve_output(o, s)$page$tokens$COMPANY, "Acme Pharma")
  o$style <- "tfl_standard"
  ro <- tfl_resolve_output(o, s)
  expect_null(ro$page$rtf_page$orientation)
  expect_identical(ro$page$tokens$COMPANY, "Acme Pharma")
  # a report's own token wins
  o$page$tokens <- list(COMPANY = "Own")
  expect_identical(tfl_resolve_output(o, s)$page$tokens$COMPANY, "Own")
  # no settings.style: tfl_standard
  s2 <- tfl_read_study_spec(pc_dir())
  expect_identical(tfl_styles(s2)$name[tfl_styles(s2)$default], "tfl_standard")
  expect_error(tfl_resolve_output({o2 <- pc_report(d); o2$style <- "nope"; o2}, s), "no style 'nope'")
})

test_that("the setup file writes each configuration's header and footer as header_<name> / footer_<name>", {
  d <- pc_dir(c(pc_defaults, pc_acme), "settings: {style: acme}\ntokens: {COMPANY: Acme Pharma}")
  s <- tfl_read_study_spec(d)
  code <- tfl_report_setup_code(s)
  # (the study's own sets; a built-in one when it is the default)
  for (x in c("header_acme", "footer_acme")) expect_true(any(startsWith(code, paste(x, "<-"))), info = x)
  expect_false(any(grepl("tfl_standard|tfl_small", code)))
  # the study's tokens and the default configuration's font size
  expect_match(paste(code, collapse = "\n"), "rtfreporter.tokens = list(COMPANY = \"Acme Pharma\")", fixed = TRUE)
  expect_match(paste(code, collapse = "\n"), "rtfreporter.font_size_half_points = 16", fixed = TRUE)
  # acme's header: its first line, then its last line
  i <- grep("^header_acme <-", code)
  hdr <- paste(code[i:(grep("^footer_acme <-", code) - 1L)], collapse = "\n")
  expect_lt(regexpr("ACME", hdr, fixed = TRUE), regexpr("OUTPUT_LABEL", hdr, fixed = TRUE))
  # a report of the setup file names its configuration's band, and leaves the
  # study's font size and tokens to the options
  o <- pc_report(d)
  rc <- paste(tfl_report_code(o, setup = TRUE, study = s), collapse = "\n")
  expect_match(rc, "header = header_acme", fixed = TRUE)
  expect_match(rc, "footer = footer_acme", fixed = TRUE)
  expect_false(grepl("COMPANY", rc, fixed = TRUE))
  o$style <- "tfl_small"
  rc <- paste(tfl_report_code(o, setup = TRUE, study = s), collapse = "\n")
  # (a built-in set that is not the default has no object: its band is written out)
  expect_false(grepl("header_tfl_small", rc, fixed = TRUE))
  expect_match(rc, "Protocol: {STUDY_ID}", fixed = TRUE)
  expect_match(rc, "font_size_half_points = 14", fixed = TRUE)
  # a report with a body line writes its band out
  o <- pc_report(d)
  o$page$header <- list(`1` = list(left = "BODY"))
  rc <- paste(tfl_report_code(o, setup = TRUE, study = s), collapse = "\n")
  expect_false(grepl("header = header_acme", rc, fixed = TRUE))
  expect_match(rc, "BODY", fixed = TRUE)
})

test_that("YAML -> sheets -> YAML keeps the configurations' lines, a report's per-line overrides, its style and a default not named standard", {
  skip_if_not_installed("openxlsx")
  env <- environment()
  rt_styles <- c(
    "styles:",
    "  acme:",
    "    page:",
    "      rtf_page: {orientation: portrait}",
    "      header:",
    "        first:",
    "          '1': {left: ACME, right: \"Page {PAGE} of {TOTAL_PAGES}\"}",
    "          '2': {left: \"Protocol: {STUDY_ID}\"}",
    "      footer:",
    "        last:",
    "          '99': {left: \"{PROGRAM}\"}",
    "  wide:",
    "    page:",
    "      rtf_page: {orientation: landscape}",
    "      header:",
    "        first:",
    "          '1': {left: WIDE}")
  d <- pc_dir(c(pc_defaults, rt_styles), "settings: {style: acme}
tokens: {COMPANY: Acme Pharma}", env = env)
  o <- pc_report(d)
  # first / last per-line overrides: an !unset, a new text; a body line
  o$page$header <- list(first = list(`2` = tfl_unset()))
  o$page$footer <- list(`50` = list(left = "Note"), last = list(`99` = list(left = "Own")))
  tfl_write_output(o, file.path(d, "outputs", "T-14-1-1.yml"))
  o2 <- o
  o2$id <- "T-2"
  o2$style <- "wide"
  o2$page$header <- NULL
  o2$page$footer <- NULL
  tfl_write_output(o2, file.path(d, "outputs", "T-2.yml"))
  s <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  sh <- tfl_sheets_from_yaml(d)
  # (the sheets: the lines keep their numbers; an override is the same number)
  hd <- sh$table$header
  expect_identical(hd$line[is.na(hd$output_id)], c("1", "2"))
  expect_identical(hd$left[!is.na(hd$output_id) & hd$output_id == "T-14-1-1"], "(none)")
  expect_identical(hd$line[!is.na(hd$output_id) & hd$output_id == "T-14-1-1"], "2")
  ft <- sh$table$footer
  expect_identical(sort(ft$line[ft$output_id %in% "T-14-1-1"]), c("50", "99"))
  expect_false(any(c("first", "last") %in% c(hd$line, ft$line)))
  expect_identical(sh$table$report$pattern[sh$table$report$output_id %in% "T-2"], "@wide")
  to <- withr::local_tempdir()
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
  s2 <- tfl_read_study_spec(to)
  outs2 <- tfl_read_outputs(to)
  expect_identical(s2$study$settings$style, "acme")
  for (nm in c("acme", "wide")) {
    expect_equal(s2$styles[[nm]]$page$header, s$styles[[nm]]$page$header, info = nm)
    expect_equal(s2$styles[[nm]]$page$footer, s$styles[[nm]]$page$footer, info = nm)
    expect_equal(s2$styles[[nm]]$page$rtf_page, s$styles[[nm]]$page$rtf_page, info = nm)
  }
  expect_identical(outs2[["T-2"]]$style, "wide")
  expect_null(outs2[["T-14-1-1"]]$style)
  h2 <- outs2[["T-14-1-1"]]$page$header
  expect_s3_class(h2$first[["2"]], "tfl_unset")
  f2 <- outs2[["T-14-1-1"]]$page$footer
  expect_identical(f2$last[["99"]]$left, "Own")
  expect_identical(f2[["50"]]$left, "Note")
  # the reports resolve to the same pages
  lines <- function(p) lapply(p, function(l) paste(unlist(l), collapse = "|"))
  for (id in c("T-14-1-1", "T-2")) {
    r1 <- tfl_resolve_output(outs[[id]], s)$page
    r2 <- tfl_resolve_output(outs2[[id]], s2)$page
    for (b in c("header", "footer")) expect_identical(lines(r2[[b]]), lines(r1[[b]]), info = paste(id, b))
    expect_identical(r2$rtf_page$orientation, r1$rtf_page$orientation, info = id)
  }
  # and the sheets of the result are the sheets again
  sh2 <- tfl_sheets_from_yaml(to)
  for (k in c("header", "footer")) {
    a <- sh$table[[k]]; b <- sh2$table[[k]]
    key <- function(x) paste(x$output_id, x$line, x$left, x$center, x$right)
    expect_setequal(key(b), key(a))
  }
  # both routes name the setup's objects after the default configuration
  expect_identical(sh$table$study$value[sh$table$study$key == "style"], "acme")
  sp <- tfl_table_spec(sh$table[intersect(names(sh$table), c("study", names(tflspec:::.ard_spec_schema())))])
  for (code in list(tfl_report_setup_code(s), tfl_report_setup_code(sp))) {
    expect_true(any(startsWith(code, "header_acme <-")))
    expect_true(any(startsWith(code, "footer_acme <-")))
  }
})

# A study with three configurations: acme (the default), compact (acme's
# header and footer, another page) and wide (its own header); a report each
pc_three <- function(env) {
  hd <- c("      header:",
          "        first:",
          "          '1': {left: ACME, right: \"Page {PAGE} of {TOTAL_PAGES}\"}",
          "          '2': {left: \"Protocol: {STUDY_ID}\"}",
          "      footer:",
          "        last:",
          "          '99': {left: \"{PROGRAM}\"}")
  styles <- c("styles:", "  acme:", "    page:", "      rtf_page: {orientation: portrait}", hd,
              "  compact:", "    page:", "      rtf_page: {orientation: landscape, margin_left_in: 0.5}", hd,
              "  wide:", "    page:", "      rtf_page: {orientation: landscape}",
              "      header:", "        first:", "          '1': {left: WIDE}",
              "      footer:", "        last:", "          '99': {left: \"{PROGRAM}\"}")
  d <- pc_dir(c(pc_defaults, styles), "settings: {style: acme}\ntokens: {COMPANY: Acme Pharma}", env = env)
  o <- pc_report(d)
  for (k in list(c("T-2", "compact"), c("T-3", "wide"))) {
    o2 <- o
    o2$id <- k[1L]
    o2$style <- k[2L]
    tfl_write_output(o2, file.path(d, "outputs", paste0(k[1L], ".yml")))
  }
  d
}
pc_sheets_spec <- function(sh) {
  tfl_table_spec(sh$table[intersect(names(sh$table), c("study", names(tflspec:::.ard_spec_schema())))])
}

test_that("the sheets route names a report's page configuration like the YAML route: header_<style>, with an object for each", {
  env <- environment()
  d <- pc_three(env)
  s <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  sh <- tfl_sheets_from_yaml(d)
  sp <- pc_sheets_spec(sh)
  # (the configuration whose header is the default's: its own object, as the YAML route has it)
  for (id in c("T-14-1-1", "T-2", "T-3")) {
    want <- c("T-14-1-1" = "acme", "T-2" = "compact", "T-3" = "wide")[[id]]
    a <- paste(tfl_report_code(outs[[id]], setup = TRUE, study = s), collapse = "\n")
    b <- paste(tfl_report_code(sp, id, setup = TRUE), collapse = "\n")
    for (code in list(a, b)) {
      expect_match(code, sprintf("header = header_%s", want), fixed = TRUE, info = id)
      expect_match(code, sprintf("footer = footer_%s", want), fixed = TRUE, info = id)
    }
  }
  # the setups say the same objects, in the same order
  objects <- function(code) code[seq(min(grep("^header_", code)), length(code))]
  expect_identical(objects(tfl_report_setup_code(sp)), objects(tfl_report_setup_code(s)))
  expect_true(all(c("header_compact", "footer_compact", "header_wide", "footer_wide") %in%
                    sub(" <-.*", "", grep("^(header|footer)_", tfl_report_setup_code(sp), value = TRUE))))
  # a report's own header line is written out, not named
  sp$header <- rbind(sp$header, data.frame(output_id = "T-2", line = "7", left = "BODY",
                                           center = NA, right = NA, row_height_twips = NA))
  code <- paste(tfl_report_code(sp, "T-2", setup = TRUE), collapse = "\n")
  expect_false(grepl("header = header_compact", code, fixed = TRUE))
  expect_match(code, "BODY", fixed = TRUE)
})

test_that("the sheets bridge writes the new form: styles with first / last blocks, a report's style:, no patterns file", {
  env <- environment()
  d <- pc_three(env)
  sh <- tfl_sheets_from_yaml(d)
  to <- withr::local_tempdir()
  suppressMessages(tfl_yaml_from_sheets(sh, to, overwrite = TRUE))
  expect_false(dir.exists(file.path(to, "spec", "patterns")))
  y <- yaml::read_yaml(file.path(to, "spec", "defaults.yml"))
  # (a band's lines are in its blocks, the default one's too, a plain line `first`)
  expect_identical(names(y$styles$acme$page$header), "first")
  expect_identical(names(y$styles$compact$page$header), "first")
  # (no report says a footer line: the default one's lines are `first`, as the split goes by the reports' numbers)
  expect_true(all(names(y$styles$acme$page$footer) %in% c("first", "last")))
  expect_identical(names(y$styles$wide$page$header), "first")
  outs <- tfl_read_outputs(to)
  expect_identical(outs[["T-2"]]$style, "compact")
  expect_identical(as.character(unlist(outs[["T-2"]]$extends)), "defaults")
  expect_identical(outs[["T-3"]]$style, "wide")
  # the same as the study written the YAML way
  s2 <- tfl_read_study_spec(to)
  s1 <- tfl_read_study_spec(d)
  for (nm in c("acme", "compact", "wide")) {
    expect_equal(s2$styles[[nm]]$page$header, s1$styles[[nm]]$page$header, info = nm)
  }
})

test_that("a configuration with nothing that differs from the default is still a known name on the sheets", {
  env <- environment()
  hd <- c("      header:", "        first:", "          '1': {left: ACME}")
  d <- pc_dir(c(pc_defaults, "styles:", "  acme:", "    page:", "      rtf_page: {orientation: portrait}", hd,
                "  plain:", "    page:", "      rtf_page: {orientation: portrait}", hd,
                "  wide:", "    page:", "      rtf_page: {orientation: landscape}", hd),
              "settings: {style: acme}", env = env)
  o <- pc_report(d)
  o$id <- "T-2"
  o$style <- "plain"
  tfl_write_output(o, file.path(d, "outputs", "T-2.yml"))
  sh <- tfl_sheets_from_yaml(d)
  rp <- sh$table$report
  # (plain: one empty row of the report sheet; wide has rows of its own and no marker)
  expect_identical(sum(rp$output_id %in% "@plain"), 1L)
  expect_identical(sum(sh$table$page$output_id %in% "@plain"), 0L)
  expect_identical(sum(rp$output_id %in% "@wide"), 0L)
  expect_identical(rp$pattern[rp$output_id %in% "T-2"], "@plain")
  # (the sheets take the report's pattern cell: no "no page pattern of that name")
  expect_no_error(pc_sheets_spec(sh))
  sp <- pc_sheets_spec(sh)
  expect_identical(attr(tflspec:::.ard_spec_scope(sp, "T-2"), "pattern"), "plain")
  # and a configuration with a line of its own has no marker row
  sh2 <- tfl_sheets_from_yaml(pc_three(env))
  expect_identical(sum(sh2$table$report$output_id %in% c("@compact", "@wide")), 0L)
})
