# Every param of every template in the package's series (tfl_series_list()'s
# package folders: tflspec_v1) through the YAML files and into the code:
# the report of the template, with its required args and the rest at their
# defaults, gets each value arg_samples() makes for one param; it is
# written (tfl_write_output()), read back (tfl_read_output()) and must give
# the param identical (after the reader's own normalisation only); resolved
# for the study (tfl_resolve_output()), its code (tfl_content_code(),
# tfl_report_code(), tfl_ard_code()) must differ from the code with the
# param absent -- or the param is display-only by the template's
# definition.  dev-rep RULES section 4.  The study is the fictional sample
# study's data (as_study()).  Fictional data only.

# each template's report: what a report of it needs (its required args, by
# step, and its own data rows); the other params at their defaults
ep_reports <- list(
  ae_soc_pt = c("kind: table", "template:", "  name: tflspec_v1/ae_soc_pt",
                "  analysis:", "    by: [TRT01A]"),
  demog = c("kind: table", "template:", "  name: tflspec_v1/demog",
            "  analysis:", "    by: [TRT01A]", "    continuous: [AGE]", "    categorical: [AGEGR1, SEX]"),
  forest_hr = c("kind: figure", "template:", "  name: tflspec_v1/forest_hr",
                "  data:", "    subgroup_vars: [SEX]",
                "  analysis:", "    by: TRT01A",
                "    conditions:", "      - {subgroup: SEX, subgroup_lvl: F, where: !r SEX == \"F\"}"),
  km_cards = c("kind: figure", "template:", "  name: tflspec_v1/km_cards",
               "  data:", "    by: TRT01A", "    param: PFS"),
  km_simple = c("kind: figure", "template:", "  name: tflspec_v1/km_simple",
                "  analysis:", "    by: TRT01A"),
  lab_tox_shift = c("kind: table", "template:", "  name: tflspec_v1/lab_tox_shift",
                    "  analysis:", "    by: [TRT01A]",
                    "    conditions:", "      - {tox_dir: High, param: Alanine Aminotransferase (U/L)}",
                    "      - {tox_dir: Low, param: Hemoglobin (g/L)}",
                    "content:", "  - table_plan: {cols: TRT01A, rows: {param: PARAM, base: base}}"),
  listing = c("kind: listing",
              "data:", "  - id: lst_sub", "    where: !r AESEV == \"SEVERE\"",
              "template:", "  name: tflspec_v1/listing", "  data:", "    from: ADAE"),
  orr_subgroup = c("kind: table", "template:", "  name: tflspec_v1/orr_subgroup",
                   "  data:", "    subgroup_vars: [SEX]",
                   "  analysis:", "    by: [TRT01A]",
                   "    conditions:", "      - {subgroup: SEX, subgroup_lvl: F, where: !r SEX == \"F\"}",
                   "content:", "  - table_plan: {cols: TRT01A, rows: {group: variable}}"),
  tflspec_hierarchy = c("kind: table", "template:", "  name: tflspec_v1/tflspec_hierarchy",
                        "  data:", "    from: ADAE",
                        "  analysis:", "    by: [TRT01A]", "    hierarchy: [AEBODSYS, AEDECOD]"),
  tflspec_listing = c("kind: listing", "template:", "  name: tflspec_v1/tflspec_listing",
                      "  data:", "    from: ADAE",
                      "  content:", "    columns:", "      - {vars: [USUBJID], label: Subject}",
                      "      - {vars: [AEDECOD], label: Preferred term}"),
  tflspec_shift = c("kind: table", "template:", "  name: tflspec_v1/tflspec_shift",
                    "  analysis:", "    by: [TRT01A]"),
  tflspec_summary = c("kind: table", "template:", "  name: tflspec_v1/tflspec_summary",
                      "  analysis:", "    by: [TRT01A]", "    continuous: [AGE]", "    categorical: [SEX]")
)

# params a case needs beside the one walked, by `template.param`: (step,
# name, value) each, for it and for the case with it absent
ep_with <- list(
  tflspec_listing.by_break = list(list("content", "by", "AESEV")),
  tflspec_listing.by_heading = list(list("content", "by", "AESEV"))
)

# A param whose value changes nothing in the code, and why (one a line;
# `template.param`, or `template.param=value` for one value).
ep_allow <- c(
  tflspec_listing.formats = "a listing counts nothing: formats is kept as every template has it"
)

# Cases a GitHub issue holds (each skipped with its number), by
# `template.param`, or `template.param=!unset` for its !unset only
ep_issues <- c(
  # an !unset record_where: a blank `where`
  `lab_tox_shift.record_where=!unset` = "tflspec#926",
  # params the template's body never uses
  listing.columns = "tflspec#928", listing.by = "tflspec#928", listing.formats = "tflspec#928"
)

# the issue a walk's row is held by, or NA
ep_issue_of <- function(r) {
  k2 <- paste(sub("^.*/", "", r$fn), r$arg, sep = ".")
  k1 <- paste0(k2, "=", ifelse(r$unset, "!unset", r$value))
  unname(ifelse(k1 %in% names(ep_issues), ep_issues[k1], ep_issues[k2]))
}

# the package's templates, `<series>/<template>`
ep_templates <- function() {
  sl <- tfl_series_list()
  sl <- sl[sl$source == "package", , drop = FALSE]
  unlist(lapply(seq_len(nrow(sl)), function(i) {
    f <- list.files(file.path(sl$path[i], "templates"), "[.]yml$")
    paste0(sl$name[i], "/", sub("[.]yml$", "", f))
  }))
}

# the study with every template's report in it (its id X-<template>)
ep_study <- function(root) {
  d <- as_study(root)
  unlink(list.files(file.path(d, "spec", "outputs"), full.names = TRUE))
  for (nm in names(ep_reports)) {
    txt <- ep_reports[[nm]]
    # (a template with a denominator: a second data row it may name)
    if (any(tfl_arg_defs(template = paste0("tflspec_v1/", nm))$name == "denominator")) {
      at <- grep("^data:$", txt)
      txt <- if (length(at)) append(txt, c("  - id: adsl_all", "    from: ADSL"), at) else
        c("data:", "  - id: adsl_all", "    from: ADSL", txt)
    }
    writeLines(c(paste0("id: X-", nm), paste0("label: ", nm), txt),
               file.path(d, "spec", "outputs", paste0("X-", nm, ".yml")))
  }
  invisible(suppressMessages(tfl_copy_templates(d, paste0("tflspec_v1/", names(ep_reports)))))
  # (the study's efficacy set: its data has ITT, no FAS)
  invisible(suppressMessages(tfl_series_values_set(d, "tflspec_v1", list(population_efficacy = "ITT"))))
  d
}

# a param's value in a report's template, by its step
ep_put <- function(o, step, name, v) {
  if (is.null(v)) {
    o$template[[step]][[name]] <- NULL
    if (!length(o$template[[step]])) o$template[[step]] <- NULL
  } else {
    o$template[[step]][name] <- list(v)
  }
  o
}

ep_code <- function(st, o) {
  one <- function(expr) tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))
  r <- one(suppressMessages(tfl_resolve_output(o, st)))
  c(if (inherits(r, "tfl_output")) one(suppressMessages(tfl_content_code(r, study = st))) else r,
    one(suppressMessages(tfl_report_code(o, study = st))),
    # (the ARD program, and the report's own data: a figure's records)
    one(suppressMessages(tfl_ard_code(st, output_id = o$id, save = FALSE))),
    one(suppressMessages(tfl_ard_code(st, output_id = o$id, save = FALSE, part = "data"))))
}

# one template's walk: a row a value (template, arg, value, read, changed,
# error)
ep_walk_tpl <- function(d, tpl, first_only = FALSE) {
  nm <- sub("^.*/", "", tpl)
  id <- paste0("X-", nm)
  f <- file.path(d, "spec", "outputs", paste0(id, ".yml"))
  o0 <- tfl_read_output(f)
  on.exit(tfl_write_output(o0, f), add = TRUE)
  # (the study's other reports aside while this one is walked: the ARD's
  # code reads every report of the folder)
  held <- file.path(d, "held")
  dir.create(held, showWarnings = FALSE)
  others <- setdiff(list.files(dirname(f), "[.]yml$", full.names = TRUE), f)
  file.rename(others, file.path(held, basename(others)))
  on.exit(file.rename(file.path(held, basename(others)), others), add = TRUE)
  st <- tfl_read_study_spec(d)
  defs <- tfl_arg_defs(template = tpl, study_dir = d)
  # (the resolve's and the checks' warnings are not what is walked)
  run <- function(o) suppressWarnings({
    tfl_write_output(o, f)
    back <- tfl_read_output(f)
    list(back = back, code = ep_code(st, back))
  })
  # (the series' values: a param's `!series` default is the value it has)
  sv <- tfl_series_read(sub("/.*$", "", tpl), dir = file.path(d, "spec", "series"))$values
  # (the report as it is makes its code)
  base_err <- grep("^ERROR:", run(o0)$code, value = TRUE)
  rows <- if (length(base_err)) {
    list(data.frame(fn = tpl, arg = "(report)", value = "", read = TRUE, read_as = "",
                    changed = TRUE, layered = FALSE, error = paste(base_err, collapse = " | "),
                    unset = FALSE, stringsAsFactors = FALSE))
  } else list()
  absent_cache <- list()
  for (i in seq_len(nrow(defs))) {
    def <- defs[i, ]
    dflt <- def$default[[1L]]
    if (identical(def$default_kind, "series")) dflt <- sv[[dflt$value]]
    vals <- arg_samples(def, tpl, default = dflt)
    if (first_only) vals <- vals[1L]
    o1 <- o0
    for (w in ep_with[[paste(sub("^.*/", "", tpl), def$name, sep = ".")]]) {
      o1 <- ep_put(o1, w[[1L]], w[[2L]], w[[3L]])
    }
    absent <- ep_put(o1, def$step, def$name, NULL)
    ak <- as_key(absent$template)
    if (is.null(absent_cache[[ak]])) absent_cache[[ak]] <- run(absent)
    a0 <- absent_cache[[ak]]
    for (v in vals) {
      got <- run(ep_put(o1, def$step, def$name, v))
      rb <- got$back$template[[def$step]][[def$name]]
      err <- grep("^ERROR:", got$code, value = TRUE)
      rows[[length(rows) + 1L]] <- data.frame(
        fn = tpl, arg = def$name, value = as_key(v),
        read = identical(rb, as_read(v)), read_as = as_key(rb),
        changed = !identical(got$code, a0$code), layered = FALSE,
        error = paste(setdiff(err, grep("^ERROR:", a0$code, value = TRUE)), collapse = " | "),
        unset = inherits(v, "tfl_unset"), stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

# what a walk's rows say is wrong: one line a case
ep_problems <- function(r) {
  if (is.null(r) || !nrow(r)) return(character())
  key <- paste(sub("^.*/", "", r$fn), r$arg, sep = ".")
  allowed <- key %in% names(ep_allow) | paste0(key, "=", r$value) %in% names(ep_allow)
  bad <- !r$read | nzchar(r$error) | (!r$changed & !r$unset & !allowed)
  r <- r[bad, , drop = FALSE]
  if (!nrow(r)) return(character())
  sprintf("%s(%s = %s): %s", r$fn, r$arg, substr(r$value, 1L, 80L),
          ifelse(!r$read, paste("read back as", substr(r$read_as, 1L, 80L)),
                 ifelse(nzchar(r$error), substr(r$error, 1L, 200L), "the code does not change")))
}

test_that("every template in the package has a report here", {
  expect_setequal(paste0("tflspec_v1/", names(ep_reports)), ep_templates())
})

test_that("every template param survives the YAML file and reaches the code", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- ep_study(withr_tempdir())
  first_only <- identical(Sys.getenv("CI"), "true")
  for (tpl in ep_templates()) {
    r <- ep_walk_tpl(d, tpl, first_only = first_only)
    p <- ep_problems(r[is.na(ep_issue_of(r)), , drop = FALSE])
    expect(length(p) == 0L, paste(c(paste0(tpl, ":"), p), collapse = "\n  "))
  }
})

# the cases an issue holds: they pass once it is fixed (then take them off
# ep_issues and this skip)
for (iss in unique(ep_issues)) {
  local({
    n <- iss
    test_that(paste0("the template cases of ", n, " survive the YAML file"), {
      skip(n)
      d <- ep_study(withr_tempdir())
      tpls <- paste0("tflspec_v1/", unique(sub("[.].*$", "", names(ep_issues)[ep_issues == n])))
      for (tpl in tpls) {
        r <- ep_walk_tpl(d, tpl)
        p <- ep_problems(r[ep_issue_of(r) %in% n, , drop = FALSE])
        expect(length(p) == 0L, paste(c(paste0(tpl, ":"), p), collapse = "\n  "))
      }
    })
  })
}
