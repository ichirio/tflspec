# The series figure templates' arguments (PS-4 C4, C5): forest_hr declares
# forest_hr()'s look params with the function's defaults, each reaching the
# call; km_simple and forest_hr select the parameter by their data row
# adtte_sub's `where` (the template's row default, PARAMCD == "OS"; a
# report's row of that id gives its own, canon 11-14 A); km_cards' `param`
# is OS by default, not required.  Fictional data only.

sp_study <- list(settings = list(rounding = "sas"))

sp_report <- function(text) {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(text, f)
  .series_read_yaml(f)
}

sp_forest_lines <- c(
  "id: F-2", "kind: figure", "label: Forest",
  "template:", "  name: tflspec_v1/forest_hr", "  data:", "    subgroup_vars: [SEX]",
  "  analysis:", "    by: TRT01A", "    comparison: Drug X",
  "    conditions:", "      - {subgroup: SEX, subgroup_lvl: F, where: !r SEX == \"F\"}")

sp_km_lines <- c(
  "id: F-1", "kind: figure", "label: Kaplan-Meier",
  "template:", "  name: tflspec_v1/km_simple", "  analysis:", "    by: TRT01A")

sp_row <- function(o, id) {
  rows <- o$ard$analysis_data %||% o$data
  Filter(function(r) identical(r$id, id), rows)[[1L]]
}

test_that("every param a figure content function takes from its template is the template's param", {
  dir <- dirname(.template_file("tflspec_v1/forest_hr"))
  tb <- .fig_content_table()
  for (f in list.files(dir, pattern = "[.]yml$", full.names = TRUE)) {
    t <- .series_read_yaml(f)
    if (is.null(t$figure)) next
    expect_identical(setdiff(tb[[t$figure]]$params, names(t$params)), character(), info = basename(f))
  }
})

test_that("forest_hr's look params: forest_hr()'s defaults, folded, on the content step", {
  d <- tfl_arg_defs(template = "tflspec_v1/forest_hr")
  look <- c("subgroup_labels", "favours", "x_breaks", "size_by_n", "bold_headings", "n_by_arm")
  expect_true(all(look %in% d$name))
  d <- d[match(look, d$name), , drop = FALSE]
  expect_identical(d$step, rep("content", length(look)))
  expect_identical(d$tier, rep("other", length(look)))
  expect_false(any(d$required))
  # (forest_hr()'s own: NULL, but the two flags FALSE)
  got <- stats::setNames(d$default, d$name)
  for (k in c("subgroup_labels", "favours", "x_breaks", "n_by_arm")) expect_null(got[[k]], info = k)
  for (k in c("size_by_n", "bold_headings")) {
    expect_true(is.logical(got[[k]]) && length(got[[k]]) == 1L && !got[[k]], info = k)
  }
})

test_that("a forest_hr report sets each look param and the call carries it", {
  o <- tfl_template_resolve(sp_report(c(
    sp_forest_lines,
    "  content:",
    "    subgroup_labels: {SEX: Sex at birth}",
    "    favours: [Favours Drug X, Favours Control]",
    "    x_breaks: [0.25, 0.5, 1, 2, 4]",
    "    size_by_n: true",
    "    bold_headings: true",
    "    n_by_arm: [Drug X, Control]")), study = sp_study)
  code <- tfl_content_code(o)
  expect_identical(code[-1L], c(
    "content <- tflspec::forest_hr(", "  ard = ard,", "  params = list(",
    "    comparison = \"Drug X\",",
    "    subgroup_labels = c(SEX = \"Sex at birth\"),",
    "    favours = c(\"Favours Drug X\", \"Favours Control\"),",
    "    x_breaks = c(0.25, 0.5, 1, 2, 4),",
    "    size_by_n = TRUE,",
    "    bold_headings = TRUE,",
    "    n_by_arm = c(\"Drug X\", \"Control\")",
    "  )", ")"))
  expect_no_error(parse(text = code, keep.source = FALSE))
  # each one alone, written under `template:` (a flat argument) as well
  for (one in list(c("subgroup_labels: {SEX: Sex}", "subgroup_labels = c(SEX = \"Sex\")"),
                   c("favours: [Left, Right]", "favours = c(\"Left\", \"Right\")"),
                   c("x_breaks: [0.5, 2]", "x_breaks = c(0.5, 2)"),
                   c("size_by_n: true", "size_by_n = TRUE"),
                   c("bold_headings: true", "bold_headings = TRUE"),
                   c("n_by_arm: [Control, Drug X]", "n_by_arm = c(\"Control\", \"Drug X\")"))) {
    lines <- append(sp_forest_lines, paste0("  ", one[[1L]]), after = 5L)
    code <- tfl_content_code(tfl_template_resolve(sp_report(lines), study = sp_study))
    expect_true(any(startsWith(trimws(code), one[[2L]])), info = one[[1L]])
  }
  # !unset: the function's own default (the argument not written)
  code <- tfl_content_code(tfl_template_resolve(sp_report(c(
    sp_forest_lines, "  content:", "    size_by_n: !unset")), study = sp_study))
  expect_false(any(grepl("size_by_n", code, fixed = TRUE)))
  expect_true(any(grepl("bold_headings = FALSE", code, fixed = TRUE)))
})

test_that("forest_hr's look params reach the plot", {
  skip_if_not_installed("ggplot2", "3.5.2")
  skip_if_not_installed("survival")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom.helpers")
  set.seed(20261127)
  n <- 60L
  d <- data.frame(TRT01A = rep(c("Control", "Drug X"), each = n / 2L), SEX = rep(c("F", "M"), n / 2L),
                  AVAL = round(stats::rexp(n, 1 / 250) + 1), CNSR = stats::rbinom(n, 1L, 0.3),
                  stringsAsFactors = FALSE)
  rows <- lapply(list(c("ALL", "ALL"), c("SEX", "F"), c("SEX", "M")), function(cnd) {
    sub <- if (cnd[[1L]] == "ALL") d else d[d$SEX == cnd[[2L]], , drop = FALSE]
    a <- cardx::ard_regression(survival::coxph(survival::Surv(AVAL, CNSR == 0) ~ TRT01A, data = sub),
                               exponentiate = TRUE)
    a$group1 <- "subgroup"
    a$group1_level <- list(cnd[[1L]])
    a$group2 <- "subgroup_lvl"
    a$group2_level <- list(cnd[[2L]])
    as.data.frame(a)
  })
  hr <- do.call(rbind, rows)
  hr$analysis_id <- "HR"
  o <- tfl_template_resolve(sp_report(c(
    sp_forest_lines, "  content:", "    subgroup_labels: {SEX: Sex at birth}",
    "    bold_headings: true", "    favours: [Favours Drug X, Favours Control]")), study = sp_study)
  env <- new.env(parent = globalenv())
  env$ard <- hr
  eval(parse(text = tfl_content_code(o), keep.source = FALSE), envir = env)
  p <- env$content
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  # the heading row's label: the report's, bold (plotmath, the y scale's labels)
  lab <- p$scales$get_scales("y")$labels
  txt <- vapply(seq_along(lab), function(i) paste(deparse(lab[[i]]), collapse = ""), "")
  expect_true(any(txt == "bold(\"Sex at birth\")"))
  # the favours' labels: a text layer
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), NA)))
})

test_that("km_simple and forest_hr: the parameter is the data row's where, OS by default", {
  for (lines in list(sp_km_lines, sp_forest_lines)) {
    o <- tfl_template_resolve(sp_report(lines), study = sp_study)
    r <- sp_row(o, "adtte_sub")
    expect_identical(as.character(r$where), "PARAMCD == \"OS\"")
    expect_false("param" %in% names(attr(o, "args")))
  }
})

test_that("km_simple and forest_hr: a report's row of adtte_sub gives its own where, the rest kept", {
  for (lines in list(sp_km_lines, sp_forest_lines)) {
    o0 <- tfl_template_resolve(sp_report(lines), study = sp_study)
    o <- tfl_template_resolve(sp_report(c(lines, "data:", "  - id: adtte_sub",
                                          "    where: !r PARAMCD == \"PFS\"")), study = sp_study)
    r <- sp_row(o, "adtte_sub")
    expect_identical(as.character(r$where), "PARAMCD == \"PFS\"")
    r0 <- sp_row(o0, "adtte_sub")
    # (only the where changes: its source, subjects, added variables, steps kept)
    expect_identical(r[setdiff(names(r0), "where")], r0[setdiff(names(r0), "where")])
  }
})

test_that("km_simple and forest_hr: the report's where reaches the program", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- file.path(tempdir(), "tpl-fig-series-params")
  unlink(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  suppressMessages(tplfig_study(d, c("adsl", "adtte"), id = "SP-1"))
  invisible(suppressMessages(tfl_copy_templates(d)))
  for (lines in list(sp_km_lines, sp_forest_lines)) {
    id <- sub("^id: ", "", lines[[1L]])
    f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
    for (pc in c("OS", "PFS")) {
      # (the study's data has ITT, no FAS: the subjects' row says it)
      txt <- c(lines, "data:", "  - id: adsl_sub", "    population: ITT",
               if (pc != "OS") c("  - id: adtte_sub", paste0("    where: !r PARAMCD == \"", pc, "\"")))
      writeLines(txt, f)
      st <- tfl_read_study_spec(d)
      code <- suppressMessages(suppressWarnings(c(
        tfl_ard_code(st, output_id = id, save = FALSE),
        tfl_ard_code(st, output_id = id, save = FALSE, part = "data"))))
      other <- setdiff(c("OS", "PFS"), pc)
      expect_true(any(grepl(paste0("PARAMCD == \"", pc, "\""), code, fixed = TRUE)), info = paste(id, pc))
      expect_false(any(grepl(paste0("PARAMCD == \"", other, "\""), code, fixed = TRUE)), info = paste(id, pc))
    }
    unlink(f)
  }
})

test_that("km_cards: no param needed (OS); a report's param or !unset", {
  rep <- function(...) {
    sp_report(c("id: F-9", "kind: figure", "template:", "  name: tflspec_v1/km_cards",
                "  data:", "    by: TRT01P", ...))
  }
  o <- tfl_template_resolve(rep(), study = sp_study)
  expect_identical(o$data$steps[[2L]]$value, "OS")
  expect_identical(tfl_arg_defs(template = "tflspec_v1/km_cards")$required[
    tfl_arg_defs(template = "tflspec_v1/km_cards")$name == "param"], FALSE)
  o <- tfl_template_resolve(rep("    param: PFS"), study = sp_study)
  expect_identical(o$data$steps[[2L]]$value, "PFS")
  # none is no KM (unset: stop): it stops at the resolve, naming it
  expect_error(tfl_template_resolve(rep("    param: !unset"), study = sp_study),
               "km_cards's param cannot be none (!unset)", fixed = TRUE)
  # a report's !study K that finds no value (no fallback): its own words,
  # no !unset the report did not write
  e <- tryCatch(tfl_template_resolve(rep("    param: !study settings.km_param"), study = sp_study),
                error = function(e) conditionMessage(e))
  expect_identical(e, paste0(
    "tfl_template_resolve(): tflspec_v1/km_cards's param needs a value (unset: stop); the report's ",
    "!study settings.km_param finds none in the study (missing, ~ or !unset there) and gives no ",
    "fallback. Give the study a value at settings.km_param, a fallback (!study {key: ",
    "settings.km_param, fallback: ...}), or leave param out for the template's default."))
  # the study's value, or the fallback: no stop
  o <- tfl_template_resolve(rep("    param: !study settings.km_param"),
                            study = list(settings = list(rounding = "sas", km_param = "PFS")))
  expect_identical(o$data$steps[[2L]]$value, "PFS")
  o <- tfl_template_resolve(rep("    param: !study {key: settings.km_param, fallback: EFS}"),
                            study = sp_study)
  expect_identical(o$data$steps[[2L]]$value, "EFS")
})

test_that("a required argument's !unset keeps the required message (not the unset: stop one)", {
  # (a required argument's `unset` is "stop" by default, tfl_arg_defs(); it
  # has no default to leave it out for)
  d <- tfl_arg_defs(template = "tflspec_v1/km_simple")
  expect_true(d$required[d$name == "by"])
  expect_identical(d$unset[d$name == "by"], "stop")
  for (lines in list(c(sp_km_lines[1:6], "    by: !unset"),
                     c("id: F-9", "kind: figure", "template:", "  name: tflspec_v1/km_cards",
                       "  data:", "    by: !unset"))) {
    tpl <- sub("^  name: ", "", lines[[grep("^  name: ", lines)]])
    e <- tryCatch(tfl_template_resolve(sp_report(lines), study = sp_study),
                  error = function(e) conditionMessage(e))
    expect_identical(e, paste0("tfl_template_resolve(): ", tpl, " needs by (required: the report gives ",
                               "no value and the template no default)."), info = tpl)
    expect_false(grepl("cannot be none", e, fixed = TRUE), info = tpl)
  }
})

test_that("forest_hr's favours: exactly two labels, checked at the resolve as forest_hr() checks it", {
  for (fav in list("[Left]", "[Left, Middle, Right]")) {
    n <- length(strsplit(fav, ",", fixed = TRUE)[[1L]])
    e <- tryCatch(tfl_template_resolve(sp_report(c(sp_forest_lines, "  content:", paste0("    favours: ", fav))),
                                       study = sp_study),
                  error = function(e) conditionMessage(e))
    expect_identical(e, sprintf(paste0("tfl_template_resolve(): tflspec_v1/forest_hr's favours is two labels, ",
                                       "left and right of 1; %d given."), n), info = fav)
  }
  expect_no_error(tfl_template_resolve(sp_report(c(sp_forest_lines, "  content:", "    favours: [L, R]")),
                                       study = sp_study))
  # (one rule: the content table's check, the one forest_hr() reads)
  expect_identical(.fig_content_table()$forest_hr$check(list(favours = c("a", "b", "c"))),
                   "favours is two labels, left and right of 1; 3 given.")
  expect_null(.fig_content_table()$forest_hr$check(list(favours = NULL)))
})

test_that("forest_hr's subgroup_labels: its variables are the report's subgroup_vars", {
  d <- tfl_arg_defs(template = "tflspec_v1/forest_hr")
  expect_identical(unlist(d$choices[[which(d$name == "subgroup_labels")]]), "from_args(subgroup_vars)")
})
