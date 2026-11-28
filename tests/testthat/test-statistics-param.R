# A template's `statistics` param and a study's own statistics
# (owner 2026-11-28): tflspec_summary's continuous analysis computes the
# statistics its cells name and those its `statistics` adds -- no fixed
# list; a name the catalog has not got stops, naming the catalog; a study
# adds a statistic of its own, by its name and function, in
# spec/defaults.yml `ard_statistics:` (built-in -> company -> study).

sp_report <- function(id, analysis = list(), content = list()) {
  tpl <- list(name = "tflspec_v1/tflspec_summary",
              analysis = utils::modifyList(list(by = "TRT01A", continuous = "AGE",
                                                categorical = "SEX"), analysis))
  if (length(content)) tpl$content <- content
  list(id = id, kind = "table", label = "Summary", template = tpl)
}

sp_cont <- function(out) {
  a <- out$ard$analyses
  a[[match("AGE", vapply(a, function(r) r$id %||% "", ""))]]
}

test_that("tflspec_summary: the ARD has the statistics the cells name, then `statistics`", {
  st <- list(settings = list(rounding = "sas"))
  # the default cells: the six, in the cells' order
  b <- tfl_template_resolve(sp_report("T-1"), study = st)
  expect_identical(sp_cont(b)$statistics, c("N", "mean", "sd", "median", "min", "max"))
  expect_null(attr(b, "args")$statistics)
  # Q1 / Q3 in the cells: p25 and p75 in the ARD
  q <- sp_report("T-2", content = list(cells = list(n = "{N}", Median = "{median}",
                                                    `Q1, Q3` = "{p25:.1f}, {p75:.1f}")))
  expect_identical(sp_cont(tfl_template_resolve(q, study = st))$statistics,
                   c("N", "median", "p25", "p75"))
  # only n and mean: only those, then what `statistics` adds (once each)
  nm <- sp_report("T-3", analysis = list(statistics = c("p5", "mean", "p95")),
                  content = list(cells = list(n = "{N}", Mean = "{mean}")))
  expect_identical(sp_cont(tfl_template_resolve(nm, study = st))$statistics,
                   c("N", "mean", "p5", "p95"))
  # the param: a pick of the statistics, as an analysis's statistics
  d <- tfl_arg_defs(template = "tflspec_v1/tflspec_summary")
  p <- d[d$name == "statistics", ]
  expect_identical(p$type, "list<string>")
  expect_identical(unlist(p$choices), "stats")
  expect_true(p$ordered)
})

test_that("derive: tokens gives the cells' statistics, unique the elements once each", {
  env <- list(cells = list(a = "{N}", b = "{mean:.1f} ({sd:.2f})", c = "{{x}} {mean}"),
              more = c("sd", "p5"), none = NULL)
  one <- function(d) tflspec:::.tr_derive_one(d, env, "derive: x")
  expect_identical(one(list(tokens = "{cells}")), c("N", "mean", "sd"))
  expect_null(one(list(tokens = "{none}")))
  expect_identical(one(list(unique = c("{more}", "{more}", "{none}"))), c("sd", "p5"))
  expect_null(one(list(unique = "{none}")))
})

# an ARD definition of one continuous analysis asking for `statistics`
sp_min_spec <- function(statistics) {
  x <- tfl_ard_spec_template()
  x$datasets <- data.frame(dataset = "ADSL", path = "adsl.rds")
  x$populations <- data.frame(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"")
  x$analyses <- data.frame(output_id = "T1", analysis_id = "AGE", method = "continuous",
                           population_id = "SAF", by = "TRT01A", variables = "AGE",
                           statistics = statistics)
  x
}

test_that("a name not in the catalog stops, naming the catalog and how to add one", {
  x <- sp_min_spec("N|gmean_pos")
  expect_error(tfl_ard_spec(x), paste0(
    "no continuous statistic gmean_pos in the catalog, tfl_ard_statistics\\(\\).*",
    "chosen by its name.*company standard's ard_statistics.*spec/defaults.yml ard_statistics:"))
  # the study's row adds it
  own <- tflspec:::.ard_statistics_rows(list(list(
    statistic = "gmean_pos", kind = "continuous", label = "Geometric mean (values > 0)",
    fmt = "xx.xx", fun = "function(x) exp(mean(log(x[x > 0])))")), "ard_statistics:")
  expect_s3_class(tfl_ard_spec(x, statistics = tflspec:::.ard_statistics_merge(
    tfl_ard_statistics(), own)), "tfl_ard_spec")
})

test_that("the study's rows go over the catalog: the same statistic and kind replaced", {
  own <- tflspec:::.ard_statistics_rows(list(
    list(statistic = "cv", kind = "continuous", label = "CV (%), n - 1", fmt = 2,
         fun = "function(x) 100 * stats::sd(x) / mean(x)"),
    list(statistic = "mad", kind = "continuous", label = "MAD", fmt = "xx.x",
         fun = "stats::mad", group = "spread")), "ard_statistics:")
  expect_identical(own$group, c("study", "spread"))
  b <- tfl_ard_statistics()
  m <- tflspec:::.ard_statistics_merge(b, own)
  expect_identical(names(m), names(b))
  expect_identical(nrow(m), nrow(b) + 1L)
  # (in its place)
  k <- which(b$statistic == "cv")
  expect_identical(m$label[k], "CV (%), n - 1")
  expect_identical(m$fmt[k], "2")
  expect_identical(m$statistic[nrow(m)], "mad")
  # another kind of the same name is another row
  ncat <- sum(m$statistic == "N")
  expect_identical(ncat, sum(b$statistic == "N"))
  # no study rows: the catalog as given (NULL too)
  expect_null(tflspec:::.ard_statistics_of_study(tempfile()))
})

test_that("a study's row that is not one R function, or not a row, stops naming it", {
  rows <- function(...) tflspec:::.ard_statistics_rows(list(...), "spec/defaults.yml ard_statistics:")
  ok <- list(statistic = "s1", kind = "continuous", label = "S1", fmt = "xx.x",
             fun = "function(x) mean(x)")
  expect_error(rows(ok, utils::modifyList(ok, list(statistic = "s2", fun = "function(x) mean(x"))),
               "ard_statistics: row 2 \\(s2\\): fun is not one R function")
  expect_error(rows(utils::modifyList(ok, list(fun = "mean(x); sd(x)"))),
               "row 1 \\(s1\\): fun is not one R function")
  expect_error(rows(utils::modifyList(ok, list(fun = "1 + 2"))), "row 1 \\(s1\\): fun is not")
  expect_error(rows(utils::modifyList(ok, list(fun = NULL))), "row 1 \\(s1\\): .*needs its fun")
  expect_error(rows(utils::modifyList(ok, list(kind = "other"))), "kind is one of")
  expect_error(rows(utils::modifyList(ok, list(fmt = "abc"))), "fmt is")
  expect_error(rows(utils::modifyList(ok, list(where = "x"))), "where is not a column")
  expect_error(rows(utils::modifyList(ok, list(label = NULL))), "needs its label")
  # cards' own needs no fun; \(x) and a name are functions
  expect_identical(nrow(rows(list(statistic = "mean", kind = "continuous", label = "Mean",
                                  fmt = "xx.xx"),
                             utils::modifyList(ok, list(fun = "\\(x) sum(x)")),
                             utils::modifyList(ok, list(statistic = "s3", fun = "stats::mad")))), 3L)
  expect_error(tflspec:::.ard_statistics_rows(list(a = ok), "ard_statistics:"), "is a list of rows")
})

# ---- a study: the ARD built and the programs written ----------------------

sp_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
}

# A study of the package's data (fictional), its defaults.yml the sample's
# with `extra` lines after it
sp_study <- function(d, extra = character()) {
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "STAT-1")
  dl <- readLines(system.file("extdata", "spec-yaml", "defaults.yml", package = "tflspec"))
  dl <- append(dl, "  - normalize_ard: {}", after = match("content:", dl))
  writeLines(c(dl, extra), file.path(d, "spec", "defaults.yml"))
  d
}

# the report `r` in the study `d` as its code SPEC (the planner's path),
# and the study read again
sp_put <- function(d, r) {
  f <- file.path(d, "spec", "outputs", paste0(r$id, ".yml"))
  yaml::write_yaml(r, f)
  invisible(tfl_copy_templates(d))
  tfl_write_output(tfl_template_to_spec(tfl_read_output(f), tfl_read_study_spec(d)), f)
  tfl_read_study_spec(d)
}

sp_gmean <- c(
  "# the study's own statistics (fictional)",
  "ard_statistics:",
  "  - statistic: gmean_pos",
  "    kind: continuous",
  "    label: Geometric mean (values > 0)",
  "    fmt: xx.xx",
  "    fun: \"function(x) exp(mean(log(x[x > 0])))\"")

test_that("a study: the ARD has the cells' statistics only, and a study's own one, computed", {
  sp_skip()
  d <- sp_study(withr::local_tempdir("stat-study"), sp_gmean)
  st <- sp_put(d, sp_report("T-9-1", analysis = list(statistics = "gmean_pos"),
                            content = list(cells = list(n = "{N}", Mean = "{mean}",
                                                        `Q1, Q3` = "{p25}, {p75}"))))
  ard <- as.data.frame(suppressMessages(
    tfl_build_ard(st, dir = d, output_id = "T-9-1", save = FALSE)))
  age <- ard[ard$analysis_id == "AGE", ]
  expect_setequal(unique(age$stat_name), c("N", "mean", "p25", "p75", "gmean_pos"))
  # the study's function, computed by arm
  a <- tflspec::adsl
  a <- a[a$SAFFL %in% "Y", ]
  want <- tapply(a$AGE, as.character(a$TRT01A), function(x) exp(mean(log(x[x > 0]))))
  g <- age[age$stat_name == "gmean_pos" & !is.na(age$group1), ]
  arm <- vapply(g$group1_level, function(e) as.character(unlist(e))[1L], "")
  expect_equal(stats::setNames(vapply(g$stat, function(e) as.numeric(unlist(e)), 0), arm)[names(want)],
               c(want))
  # written into the program: the setup's tfl_stats, the analysis's call
  sp <- tflspec:::.ard_spec_of_study(st, output_id = "T-9-1")$spec
  setup <- paste(tfl_ard_program(sp, dir = d, date = as.Date("2026-11-28")), collapse = "\n")
  expect_match(setup, "tfl_stats <- list(gmean_pos = function(x) exp(mean(log(x[x > 0]))))",
               fixed = TRUE)
  expect_match(setup, "gmean_pos = 2L", fixed = TRUE)
  body <- paste(tfl_ard_program(sp, "T-9-1", dir = d, date = as.Date("2026-11-28")),
                collapse = "\n")
  expect_match(body, "tfl_stats[\"gmean_pos\"]", fixed = TRUE)
  # the same from tfl_ard_code() of the study
  expect_match(paste(tfl_ard_code(st, output_id = "T-9-1", part = "setup"), collapse = "\n"),
               "gmean_pos = function(x)", fixed = TRUE)
  # a study without the block: the statistic is unknown, and stops
  writeLines(readLines(file.path(d, "spec", "defaults.yml"))[
    seq_len(length(readLines(file.path(d, "spec", "defaults.yml"))) - length(sp_gmean))],
    file.path(d, "spec", "defaults.yml"))
  st <- tfl_read_study_spec(d)
  expect_error(suppressMessages(tfl_build_ard(st, dir = d, output_id = "T-9-1", save = FALSE)),
               "no continuous statistic gmean_pos in the catalog, tfl_ard_statistics")
})

test_that("a study: a broken fun of its own stops, naming the row", {
  sp_skip()
  bad <- sub("x[x > 0]))", "x[x > 0])", sp_gmean, fixed = TRUE)
  d <- sp_study(withr::local_tempdir("stat-bad"), bad)
  st <- sp_put(d, sp_report("T-9-2"))
  expect_error(suppressMessages(tfl_build_ard(st, dir = d, output_id = "T-9-2", save = FALSE)),
               "spec/defaults.yml ard_statistics: row 1 \\(gmean_pos\\): fun is not one R function")
  # (the program's maker reads it too)
  expect_error(tfl_ard_program(tfl_ard_spec(sp_min_spec("N|mean")), "T1", dir = d),
               "row 1 \\(gmean_pos\\): fun is not one R function")
})
