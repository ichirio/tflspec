# Named continuous format sets (tflspec#903): study.yml's
# settings.format_sets, a set's name as a value of `formats` ("*" and a
# variable) and of plan_digits(), the layers, the stops, fmtset_<name> /
# digset_<name> in the generated R, and settings.params' parameters' sets on
# the plan_digits route.

fs_sets <- list(
  continuous_0 = list(N = 0L, mean = 1L, sd = 2L, se = 2L, median = 1L, p25 = 1L, p75 = 1L,
                      min = 0L, max = 0L),
  continuous_1 = list(N = 0L, mean = 2L, sd = 3L, se = 3L, median = 2L, p25 = 2L, p75 = 2L,
                      min = 1L, max = 1L))

# A study of the bundled ADSL and ADLB (fictional data), its reports `outs`
# (each lines of YAML, by id), its study.yml's settings changed by `settings`
fs_study <- function(outs = list(), settings = list(format_sets = fs_sets),
                     env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "adam"))
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adlb), file.path(d, "adam", "adlb.xpt"))
  utils::capture.output(tfl_ai_study_init(d, adam = "adam", study_id = "FS-1"))
  sy <- yaml::read_yaml(file.path(d, "study.yml"))
  for (k in names(settings)) sy$settings[k] <- list(settings[[k]])
  yaml::write_yaml(sy, file.path(d, "study.yml"))
  dir.create(file.path(d, "spec", "outputs"), showWarnings = FALSE)
  for (id in names(outs)) writeLines(outs[[id]], file.path(d, "spec", "outputs", paste0(id, ".yml")))
  d
}

# a code SPEC report: BIGN and a continuous analysis of AGE and BSLD, its
# `formats` (a line of YAML) and its content's extra items
fs_code_report <- function(id, formats = NULL, content = character(), cells = NULL,
                           variables = "[AGE, BSLD]") {
  c(paste0("id: ", id), "kind: table", "label: Demographics", "extends: [defaults]",
    "data:", "  - id: adsl_sub", "    from: ADSL", "    population: SAF",
    "ard:", "  analyses:",
    "  - id: BIGN", "    method: categorical", "    data: adsl_sub", "    variables: [TRT01A]",
    "  - id: CONT", "    method: continuous", "    data: adsl_sub", "    by: [TRT01A]",
    paste0("    variables: ", variables), "    statistics: [N, mean, sd, median, min, max]",
    if (!is.null(formats)) paste0("    formats: ", formats),
    "content:", "  - table_plan: {cols: TRT01A, rows: {group: variable}}",
    "  - plan_cells:", "      continuous:", "        \"n\": \"{N}\"",
    paste0("        Mean (SD): \"", cells %||% "{mean} ({sd})", "\""),
    "        Median: \"{median}\"", "        Min, Max: \"{min}, {max}\"",
    content,
    "page:", "  titles:", "    1: {center: Table}")
}

fs_tpl_report <- function(id, name, formats, extra = character()) {
  c(paste0("id: ", id), "kind: table", "label: Demographics", "template:", paste0("  name: ", name),
    "  args:", "    by: TRT01A", "    continuous: [AGE, BSLD]", "    categorical: [SEX]",
    paste0("    formats: ", formats), extra,
    "page:", "  titles:", "    1: {center: Table}")
}

# each report's continuous statistics' stat_fmt (Drug X), by variable and statistic
fs_digits <- function(d, ids) {
  st <- tfl_read_study_spec(d)
  a <- suppressWarnings(suppressMessages(tfl_build_ard(st, dir = d, save = FALSE)))
  g <- vapply(a$group1_level, function(z) paste(unlist(z), collapse = ""), "")
  a <- a[a$output_id %in% ids & a$variable %in% c("AGE", "BSLD") & g == "Drug X" &
           a$stat_name %in% c("N", "mean", "sd", "median", "min", "max"), ]
  out <- lapply(ids, function(id) {
    b <- a[a$output_id == id, ]
    stats::setNames(vapply(b$stat_fmt, function(z) paste(unlist(z)), ""),
                    paste(b$variable, b$stat_name, sep = ":"))
  })
  lapply(stats::setNames(out, ids), function(v) v[order(names(v))])
}

fs_skip <- function() {
  skip_on_cran()
  for (p in c("cards", "dplyr", "haven")) skip_if_not_installed(p)
}

test_that("settings.format_sets is read and checked", {
  st <- structure(list(study = list(settings = list(format_sets = fs_sets))), class = "tfl_study_spec")
  fs <- .study_format_sets(st)
  expect_identical(names(fs), c("continuous_0", "continuous_1"))
  expect_identical(fs$continuous_1[["mean"]], "2")
  # (none: NULL)
  expect_null(.study_format_sets(structure(list(study = list(settings = list())),
                                           class = "tfl_study_spec")))
  bad <- function(x) structure(list(study = list(settings = list(format_sets = x))),
                               class = "tfl_study_spec")
  expect_error(.study_format_sets(bad(list(Continuous = list(mean = 1)))), "lower case")
  expect_error(.study_format_sets(bad(list(xx = list(mean = 1)))), "not a format")
  expect_error(.study_format_sets(bad(list(c1 = list(n = 1)))), "not a continuous statistic")
  expect_error(.study_format_sets(bad(list(c1 = list(mean = "two")))), "number of decimals")
  expect_error(.study_format_sets(bad(list(c1 = 2))), "map of statistic")
  # settings.params: each parameter's set, one the study has
  sp <- structure(list(study = list(settings = list(
    format_sets = fs_sets,
    params = list(ALT = list(label = "ALT (U/L)", format = "continuous_0"), BILI = list(label = "B"))))),
    class = "tfl_study_spec")
  expect_identical(.study_param_formats(sp), c(ALT = "continuous_0"))
  sp$study$settings$params$ALT$format <- "continuous_9"
  expect_error(.study_param_formats(sp), "continuous_9, which the study has not got; its sets: continuous_0, continuous_1")
  sp$study$settings$format_sets <- NULL
  expect_error(.study_param_formats(sp), "the study has no format sets")
})

test_that("the layers: a report's set over the standard, scope first (design 2.3)", {
  sets <- .fmtsets_check(fs_sets, "t")
  std <- list(N = 0L, mean = 2L, sd = 3L, median = 1L, min = 0L, max = 0L, p = "xx.x%",
              "AGE:mean" = 1L, "AGE:sd" = 2L)
  # the report's "*" takes the place of the standard's statistics (3 < 4), not
  # of its AGE:stat (7, a narrower scope) nor of p (no continuous statistic)
  m <- .tr_merge_map(.fmtset_over(std, list("*" = "continuous_1", sd = 3L)),
                     list("*" = "continuous_1", sd = 3L))
  expect_setequal(names(m), c("p", "AGE:mean", "AGE:sd", "*", "sd"))
  m$p <- NULL
  f <- .parse_formats(paste(names(m), unlist(m), sep = "=", collapse = " | "))
  r <- .fmtset_resolve(f, c("AGE", "BSLD"), "continuous", c("N", "mean", "sd"), sets, "T / CONT")
  expect_identical(r[["mean"]], "2")
  expect_identical(r[["sd"]], "3")
  expect_identical(r[["AGE:mean"]], "1")
  expect_identical(r[["AGE:sd"]], "2")
  # the R: one line an entry (design 2.4)
  code <- .lay(.fmtset_fun_arg(attr(r, "fmtsets"), c("AGE", "BSLD")))
  expect_match(code, "everything() ~ utils::modifyList(fmtset_continuous_1, list(sd = 3L))", fixed = TRUE)
  expect_match(code, "AGE ~ utils::modifyList(fmtset_continuous_1, list(sd = 2L, mean = 1L))", fixed = TRUE)
  # a report's variable set takes the place of the standard's VAR:stat (7 < 8)
  m <- .tr_merge_map(.fmtset_over(std, list(AGE = "continuous_0")), list(AGE = "continuous_0"))
  expect_false(any(c("AGE:mean", "AGE:sd") %in% names(m)))
  expect_identical(m$mean, 2L)
  # the standard's variable set wins over the report's statistic (6 over 5);
  # the report's VAR:stat over the variable set (9 over 6)
  f <- c("*" = "continuous_1", AGE = "continuous_0", mean = "3", "AGE:sd" = "4")
  r <- .fmtset_resolve(f, c("AGE", "BSLD"), "continuous", c("N", "mean", "sd"), sets, "T / CONT")
  expect_identical(r[["AGE:mean"]], "1")
  expect_identical(r[["AGE:sd"]], "4")
  expect_identical(r[["mean"]], "3")
  # with no set named, the formats as they were; a categorical analysis does
  # not read a set
  f0 <- c(mean = "2", "AGE:sd" = "3")
  expect_identical(.fmtset_resolve(f0, "AGE", "continuous", "mean", sets, "x"), f0)
  expect_identical(.fmtset_resolve(c("*" = "continuous_1", p = "xx.x%"), "SEX", "categorical",
                                   "p", sets, "x"), c(p = "xx.x%"))
  # (a name no set of the study has stops there too)
  expect_error(.fmtset_resolve(c(SEX = "abc", p = "xx.x%"), "SEX", "categorical", "p", sets, "x"),
               "the format set abc, which the study has not got")
})

test_that("the stop rules: a set unknown, a set without a statistic, a set on the wrong key", {
  sets <- .fmtsets_check(fs_sets, "t")
  res <- function(f, s = sets, stats = c("N", "mean", "sd", "median", "min", "max")) {
    .fmtset_resolve(f, c("AGE", "BSLD"), "continuous", stats, s, "T-1 / CONT")
  }
  expect_error(res(c("*" = "continuous_9")),
               "continuous_9, which the study has not got; its sets: continuous_0, continuous_1")
  expect_error(res(c(AGE = "continuous_0"), s = NULL), "the study has no format sets")
  # (a set without a statistic the analysis computes, no narrower key: stop,
  # not a lower layer)
  s2 <- sets
  s2$continuous_1 <- s2$continuous_1[names(s2$continuous_1) != "median"]
  expect_error(res(c("*" = "continuous_1"), s2),
               "the format set continuous_1 has no median \\(variable AGE\\)")
  # (a statistic over it, or VAR:stat for every variable: no stop)
  expect_silent(res(c("*" = "continuous_1", median = "1"), s2))
  expect_silent(res(c("*" = "continuous_1", "AGE:median" = "1", "BSLD:median" = "2"), s2))
  expect_error(res(c(AGE = "continuous_1"), s2), "has no median \\(variable AGE\\)")
  expect_error(res(c("*" = "2")), "\"\\*\" takes the name of one of the study's format sets")
  expect_error(res(c(mean = "continuous_1")), "formats mean is a statistic")
})

test_that("a code SPEC report: fmtset_<name> in the ARD program, the digits of its sets", {
  fs_skip()
  d <- fs_study(list(`T-1` = fs_code_report("T-1", "{\"*\": continuous_1, AGE: continuous_0, \"BSLD:sd\": 4}")))
  st <- tfl_read_study_spec(d)
  code <- tfl_ard_code(st, save = FALSE)
  expect_silent(parse(text = code))
  txt <- paste(code, collapse = "\n")
  expect_match(txt, "fmtset_continuous_0 <- modifyList(", fixed = TRUE)
  expect_match(txt, "fmtset_continuous_1 <- modifyList(", fixed = TRUE)
  expect_match(txt, "everything() ~ fmtset_continuous_1,", fixed = TRUE)
  expect_match(txt, "AGE ~ fmtset_continuous_0,", fixed = TRUE)
  expect_match(txt, "BSLD ~ modifyList(fmtset_continuous_1, list(sd = 4L))", fixed = TRUE)
  dg <- fs_digits(d, "T-1")[["T-1"]]
  expect_identical(dg[["AGE:mean"]], "60.1")
  expect_identical(dg[["AGE:min"]], "39")
  expect_identical(dg[["BSLD:mean"]], "69.19")
  expect_identical(dg[["BSLD:sd"]], "33.5665")
  expect_identical(dg[["BSLD:min"]], "25.0")
  # the fingerprint has the sets' values: a set changed, the ARD is out of date
  sp <- tflspec:::.ard_spec_of_study(st)$spec
  h1 <- tfl_ard_spec_hash(sp, "T-1", d)
  sp$format_sets$continuous_0[["mean"]] <- "2"
  expect_false(identical(tfl_ard_spec_hash(sp, "T-1", d), h1))
  # (a set no analysis names does not count)
  sp2 <- tflspec:::.ard_spec_of_study(st)$spec
  sp2$format_sets$continuous_2 <- sp2$format_sets$continuous_1
  expect_identical(tfl_ard_spec_hash(sp2, "T-1", d), h1)
})

test_that("a set in a template report and in a code SPEC report gives the same digits", {
  fs_skip()
  fm <- "{\"*\": continuous_1, AGE: continuous_0, \"BSLD:sd\": 4}"
  d <- fs_study(list(
    `T-1` = fs_code_report("T-1", fm),
    `T-2` = fs_tpl_report("T-2", "tflspec_v1/tflspec_summary", fm),
    `T-3` = fs_tpl_report("T-3", "demog", "{\"*\": continuous_1, AGE: continuous_0, BSLD: {sd: 4}}")))
  utils::capture.output(tfl_copy_templates(d, "tflspec_v1/tflspec_summary"))
  dg <- fs_digits(d, c("T-1", "T-2", "T-3"))
  expect_length(dg[["T-1"]], 12L)
  expect_identical(dg[["T-2"]], dg[["T-1"]])
  expect_identical(dg[["T-3"]], dg[["T-1"]])
})

test_that("plan_digits(): a set's name is digset_<name>; the session's run has its digits", {
  fs_skip()
  d <- fs_study(list(`T-4` = fs_code_report(
    "T-4", content = "  - plan_digits: {continuous: continuous_1, AGE: continuous_0}")))
  st <- tfl_read_study_spec(d)
  ro <- tfl_resolve_output(tfl_read_outputs(d)[["T-4"]], st)
  dg <- Filter(function(it) identical(it$fn, "plan_digits"), ro$content)[[1L]]$args
  expect_identical(attr(dg$continuous, "tfl_digset"), "continuous_1")
  expect_identical(unclass(dg$AGE)[["mean"]], 1)
  code <- tfl_content_code(ro)
  expect_silent(parse(text = code))
  txt <- paste(code, collapse = "\n")
  expect_match(txt, "digset_continuous_1 <- c(", fixed = TRUE)
  expect_match(txt, "plan_digits(continuous = digset_continuous_1, AGE = digset_continuous_0)", fixed = TRUE)
  skip_if_not_installed("rtfreporter")
  f <- suppressWarnings(tfl_run_output("T-4", dir = d))
  rtf <- paste(readLines(f), collapse = "\n")
  # AGE continuous_0 (mean 1, sd 2), BSLD continuous_1 (mean 2, sd 3)
  expect_match(rtf, "60.1 (9.59)", fixed = TRUE)
  expect_match(rtf, "69.19 (33.567)", fixed = TRUE)
})

test_that("the cells' own digits on a statistic a set gives stop", {
  fs_skip()
  d <- fs_study(list(`T-1` = fs_code_report("T-1", "{\"*\": continuous_1}", cells = "{mean:.2f} ({sd})")))
  st <- tfl_read_study_spec(d)
  expect_error(tfl_resolve_output(tfl_read_outputs(d)[["T-1"]], st),
               "the cell \\{mean:.2f\\} gives mean its digits, and the format set continuous_1 does too")
  d2 <- fs_study(list(`T-4` = fs_code_report("T-4", cells = "{mean} ({sd:.3f})",
                                            content = "  - plan_digits: {continuous: continuous_1}")))
  expect_error(tfl_resolve_output(tfl_read_outputs(d2)[["T-4"]], tfl_read_study_spec(d2)),
               "the format set continuous_1 does too")
  # (a statistic the set has not got, plan_digits(): stop)
  s <- fs_sets
  s$continuous_1$median <- NULL
  d3 <- fs_study(list(`T-4` = fs_code_report("T-4", content = "  - plan_digits: {continuous: continuous_1}")),
                 settings = list(format_sets = s))
  expect_error(tfl_resolve_output(tfl_read_outputs(d3)[["T-4"]], tfl_read_study_spec(d3)),
               "the format set continuous_1 has no median \\(plan_digits continuous\\)")
})

test_that("a variable's set a report writes for a variable it does not analyse stops; a standard's is not read", {
  fs_skip()
  d <- fs_study(list(`T-1` = fs_code_report("T-1", "{\"*\": continuous_1, WEIGHTBL: continuous_0}")))
  expect_error(tfl_ard_code(tfl_read_study_spec(d), save = FALSE),
               "formats names a set for WEIGHTBL, which no continuous analysis of the report analyses")
  # a template's standard (its default) with a variable's set the report
  # does not analyse: left out, no stop
  fm <- list(N = 0L, mean = 1L, "*" = "continuous_1", WEIGHTBL = "continuous_0", AGE = "continuous_0")
  out <- .fmtset_std_prune(fm, given = character(), args = list(continuous = list("AGE", "BSLD")))
  expect_identical(names(out), c("N", "mean", "*", "AGE"))
  # (the report's own stays, for the stop)
  expect_identical(names(.fmtset_std_prune(fm, given = "WEIGHTBL", args = list(continuous = "AGE"))),
                   c("N", "mean", "*", "WEIGHTBL", "AGE"))
})

test_that("settings.params: a parameter's set through plan_digits(PARAMCD = ) (param_change)", {
  fs_skip()
  pc <- function(id, params, formats) {
    c(paste0("id: ", id), "kind: table", "label: Lab", "template:", "  name: param_change",
      "  args:", "    population: SAF", "    by: TRT01A", "    data: ADLB",
      paste0("    params: ", params), paste0("    formats: ", formats),
      "page:", "  titles:", "    1: {center: Table}")
  }
  d <- fs_study(list(`L-1` = pc("L-1", "[ALT, AST]", "{\"*\": continuous_1, AST: {mean: 3}}"),
                     `L-2` = pc("L-2", "[AST]", "{mean: 2}"),
                     `L-3` = pc("L-3", "[ALT]", "{ALT: continuous_1}")),
                settings = list(format_sets = fs_sets, params = list(
                  ALT = list(label = "Alanine Aminotransferase (U/L)", format = "continuous_0"),
                  AST = list(format = "continuous_0"),
                  BILI = list(format = "continuous_1"))))
  st <- tfl_read_study_spec(d)
  outs <- tfl_read_outputs(d)
  dg <- function(id) {
    ro <- tfl_resolve_output(outs[[id]], st)
    list(args = Filter(function(it) identical(it$fn, "plan_digits"), ro$content)[[1L]]$args,
         code = paste(suppressMessages(tfl_content_code(ro)), collapse = "\n"))
  }
  l1 <- dg("L-1")
  # the parameters it shows, each its set (BILI, not shown, is not written);
  # the report's map over the parameter's set (9 over 6)
  expect_identical(names(l1$args$PARAMCD), c("ALT", "AST"))
  expect_match(l1$code, "continuous = digset_continuous_1", fixed = TRUE)
  expect_match(l1$code, "ALT = digset_continuous_0", fixed = TRUE)
  expect_match(l1$code, "AST = replace(digset_continuous_0, \"mean\", 3)", fixed = TRUE)
  expect_silent(parse(text = l1$code))
  l2 <- dg("L-2")
  expect_identical(names(l2$args$PARAMCD), "AST")
  expect_identical(unclass(l2$args$continuous)[["mean"]], 2L)
  # the report's set for a parameter over the study's (8 over 6)
  expect_match(dg("L-3")$code, "ALT = digset_continuous_1", fixed = TRUE)
  skip_if_not_installed("rtfreporter")
  rtf <- paste(readLines(suppressWarnings(tfl_run_output("L-1", dir = d))), collapse = "\n")
  # ALT's Mean (SD) by continuous_0: one decimal, two for the SD
  alt <- sub("Parameter: AST.*$", "", rtf)
  expect_match(alt, " [0-9]+[.][0-9] [(][0-9]+[.][0-9]{2}[)]")
  # AST's mean by the report's map over its set: three decimals
  expect_match(sub("^.*Parameter: AST", "", rtf), " [0-9]+[.][0-9]{3} [(][0-9]+[.][0-9]{2}[)]")
})

test_that("a study without sets writes the programs it wrote; sets nobody names change nothing", {
  fs_skip()
  d0 <- fs_study(list(`T-1` = fs_code_report("T-1", "{mean: 2, \"AGE:sd\": 3}")), settings = list())
  d1 <- fs_study(list(`T-1` = fs_code_report("T-1", "{mean: 2, \"AGE:sd\": 3}")))
  c0 <- tfl_ard_code(tfl_read_study_spec(d0), save = TRUE, dir = d0)
  c1 <- tfl_ard_code(tfl_read_study_spec(d1), save = TRUE, dir = d1)
  strip <- function(x) x[!grepl("^# Generated by", x)]
  expect_identical(strip(c1), strip(c0))
  expect_false(any(grepl("fmtset_", c1)))
})
