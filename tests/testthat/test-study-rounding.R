# The study's rounding has one place, spec/defaults.yml `force:` (canon
# 1.33; PRINCIPLES.md 11-10 :363-:369, :378, 11-11 :400).  It is written once,
# in the setup every report and ARD program sources (options(
# rtfreporter.rounding = , cards.round_type = )); a table's own
# plan_digits(rounding = ) of another value is warned of and takes the forced
# one; the old places of the rounding stop.

sr_study <- function(rounding = "sas") {
  st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  st$force <- tflspec:::.force_of(rounding)
  st
}
sr_output <- function(content = list(), kind = "table") {
  tfl_output("T-9", kind, common = list(population = "SAF"), content = c(list(
    tfl_item("table_plan", list(cols = "TRT01A")),
    tfl_item("plan_digits", list(AGE = 0))), content))
}
# a copy of the spec-yaml study (its files beside outputs/), defaults.yml
# with `dflt` lines over its own first lines and study.yml with `study` lines
sr_dir <- function(dflt = character(), study = character()) {
  d <- tempfile("sr-study")
  dir.create(file.path(d, "outputs"), recursive = TRUE)
  src <- system.file("extdata", "spec-yaml", package = "tflspec")
  file.copy(file.path(src, "outputs", "T-14-1-1.yml"), file.path(d, "outputs"))
  writeLines(c(dflt, readLines(file.path(src, "defaults.yml"))), file.path(d, "defaults.yml"))
  writeLines(c(readLines(file.path(src, "study.yml")), study), file.path(d, "study.yml"))
  d
}

test_that("defaults.yml `force:` holds the study's rounding; the setup writes it", {
  d <- sr_dir(c("force:", "  content:", "    - plan_digits: {rounding: r}"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  st <- tfl_read_study_spec(d)
  expect_identical(tflspec:::.study_rounding(st), "r")
  expect_identical(st$force$content[[1L]]$fn, "plan_digits")
  # (no layer's card: the defaults' content does not have it)
  expect_false(any(vapply(st$defaults$content, function(it) identical(it$fn, "plan_digits"), NA)))
  setup <- tfl_report_setup_code(st)
  expect_true(any(grepl("rtfreporter.rounding = \"r\"", setup, fixed = TRUE)))
  expect_true(any(grepl("cards.round_type = \"round-to-even\"", setup, fixed = TRUE)))
  expect_identical(tfl_study_setup_code(st)[1:2], c(
    "# the study's rounding (every ARD and report program)",
    "options(rtfreporter.rounding = \"r\", cards.round_type = \"round-to-even\")"))
  # a study that forces none: no options()
  st0 <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  expect_null(tflspec:::.study_rounding(st0))
  expect_false(any(grepl("rounding|round_type", tfl_report_setup_code(st0))))
})

test_that("only the rounding is forced: another step or card under `force:` stops", {
  bad <- list(
    c("force:", "  page:", "    orientation: landscape"),
    c("force:", "  content:", "    - plan_stub: {name: row_label}"),
    c("force:", "  content:", "    - plan_digits: {rounding: sas, AGE: 0}"),
    c("force:", "  content:", "    - plan_digits: {rounding: sas}", "    - plan_digits: {rounding: r}"),
    c("force:", "  content: {plan_digits: {rounding: sas}}"),
    c("force: sas"))
  for (b in bad) {
    d <- sr_dir(b)
    expect_error(tfl_read_study_spec(d), "only the rounding is forced (canon 1.33)", fixed = TRUE)
    unlink(d, recursive = TRUE)
  }
  # the value is sas or r, spelled so
  d <- sr_dir(c("force:", "  content:", "    - plan_digits: {rounding: iec60559}"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  e <- tryCatch(tfl_read_study_spec(d), tflspec_spec_keys = function(e) e)
  expect_identical(e$keys, "force: content: plan_digits: rounding")
  expect_match(conditionMessage(e), "takes \"sas\" or \"r\", not \"iec60559\"", fixed = TRUE)
})

test_that("the old places of the rounding stop, naming the file and `force:`", {
  # study.yml `settings: rounding`
  d <- sr_dir(study = "  rounding: r")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines(sub("^settings: \\{adam_levels: define_xml\\}$", "settings:\n  adam_levels: define_xml",
                 readLines(file.path(d, "study.yml"))), file.path(d, "study.yml"))
  e <- tryCatch(tfl_read_study_spec(d), error = function(e) e)
  expect_s3_class(e, "tflspec_spec_keys")
  expect_identical(e$problems$form, "settings_rounding")
  expect_match(conditionMessage(e), "study.yml: a key it does not take:", fixed = TRUE)
  expect_match(conditionMessage(e), "`settings: rounding`: an old form, no longer read", fixed = TRUE)
  expect_match(conditionMessage(e), "write it in spec/defaults.yml `force:`", fixed = TRUE)
  expect_match(conditionMessage(e), paste(
    "      force:", "        content:", "          - plan_digits:", "              rounding: r", sep = "\n"),
    fixed = TRUE)
  # a plan_digits(rounding = ) card in defaults.yml's content
  d2 <- sr_dir(c("content:", "  - plan_digits: {rounding: sas}"))
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  x <- readLines(file.path(d2, "defaults.yml"))
  x <- x[-(grep("^content:", x)[2L])]
  writeLines(x, file.path(d2, "defaults.yml"))
  e <- tryCatch(tfl_read_study_spec(d2), error = function(e) e)
  expect_identical(e$form, "content_rounding")
  expect_match(conditionMessage(e), "spec/defaults.yml `content:`", fixed = TRUE)
  expect_match(conditionMessage(e), "force:\n    content:\n      - plan_digits:\n          rounding: sas",
               fixed = TRUE)
  # ... and in a style set's content
  d3 <- sr_dir(c("styles:", "  house:", "    content:", "      - plan_digits: {rounding: r, AGE: 1}"))
  on.exit(unlink(d3, recursive = TRUE), add = TRUE)
  e <- tryCatch(tfl_read_study_spec(d3), error = function(e) e)
  expect_identical(e$form, "content_rounding")
  expect_match(conditionMessage(e), "`styles: house: content:`", fixed = TRUE)
  # both are in the old forms' list the spec_version stop gives
  expect_true(all(c("settings_rounding", "content_rounding") %in% tflspec:::.old_forms()$id))
})

test_that("the study's rounding is the setup's option, not the report's code", {
  st <- sr_study("sas")
  setup <- tfl_report_setup_code(st)
  expect_true(any(grepl("rtfreporter.rounding = \"sas\"", setup, fixed = TRUE)))
  code <- tfl_content_code(tfl_resolve_output(sr_output(), st))
  expect_false(any(grepl("rounding", code, fixed = TRUE)))
  expect_true(any(grepl("plan_digits(AGE = 0)", code, fixed = TRUE)))
  # the resolved report has no card of it: an in-process run sets the
  # option as the setup does (.study_rounding_option())
  ro <- tfl_resolve_output(sr_output(), st)
  r <- unlist(lapply(Filter(function(it) it$fn == "plan_digits", ro$content), function(it) it$args$rounding))
  expect_null(r)
})

test_that("a table's own rounding: the forced one, warned of when it differs; a listing is left alone", {
  a <- sr_study("sas")
  b <- sr_study("r")
  expect_false(identical(tfl_report_setup_code(a), tfl_report_setup_code(b)))
  expect_identical(tfl_content_code(tfl_resolve_output(sr_output(), a)),
                   tfl_content_code(tfl_resolve_output(sr_output(), b)))
  # another value: a warning, and the forced value (the setup's: not in the code)
  own <- sr_output(list(tfl_item("plan_digits", list(rounding = "r"))))
  expect_warning(ro <- tfl_resolve_output(own, a), class = "tflspec_force_over")
  expect_warning(tfl_resolve_output(own, a), "the study forces the rounding \"sas\"", fixed = TRUE)
  r <- unlist(lapply(Filter(function(it) it$fn == "plan_digits", ro$content), function(it) it$args$rounding))
  expect_identical(r, "sas")
  code <- suppressWarnings(tfl_content_code(tfl_resolve_output(own, a)))
  expect_false(any(grepl("rounding", code, fixed = TRUE)))
  # the forced value itself: nothing to say
  expect_no_warning(ro <- tfl_resolve_output(own, b))
  expect_false(any(grepl("rounding", tfl_content_code(ro), fixed = TRUE)))
  # an !unset of it cannot take it off: a warning, the forced value
  un <- sr_output(list(tfl_item("plan_digits", list(rounding = tfl_unset()))))
  expect_warning(tfl_resolve_output(un, a), "plan_digits(rounding = !unset) is not used", fixed = TRUE)
  # (the provenance says where the value came from)
  ro <- suppressWarnings(tfl_resolve_output(own, a, provenance = TRUE))
  pd <- Filter(function(it) it$fn == "plan_digits" && !is.null(it$args$rounding), ro$content)
  expect_identical(pd[[1L]]$arg_from[["rounding"]], "force")
  # a listing has no table content: skipped, in silence
  li <- sr_output(list(tfl_item("plan_digits", list(rounding = "r"))), kind = "listing")
  expect_no_warning(tfl_resolve_output(li, a))
  # a study that forces none: a report's own rounding stays, in its code
  st0 <- sr_study(NULL)
  expect_no_warning(ro <- tfl_resolve_output(own, st0))
  expect_true(any(grepl("rounding = \"r\"", tfl_content_code(ro), fixed = TRUE)))
  expect_false(any(grepl("rtfreporter.rounding", tfl_report_setup_code(st0), fixed = TRUE)))
})

test_that("tfl_check_spec() lists a rounding the force replaces", {
  skip_on_cran()
  d <- sr_dir(c("force:", "  content:", "    - plan_digits: {rounding: sas}"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "outputs", "T-14-1-1.yml")
  x <- readLines(f)
  at <- grep("^  - plan_digits: \\{continuous:", x)
  expect_length(at, 1L)
  x[at] <- sub("{continuous:", "{rounding: r, continuous:", x[at], fixed = TRUE)
  writeLines(x, f)
  p <- tfl_check_spec(d, adam = FALSE)
  hit <- p[p$output_id %in% "T-14-1-1" & p$item %in% "plan_digits$rounding", , drop = FALSE]
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$level, "warning")
  expect_match(hit$message, "the study forces the rounding \"sas\"", fixed = TRUE)
})

test_that("the writers write `force:`: tfl_write_study_part(), the converter, the sheets", {
  d <- sr_dir(c("force:", "  content:", "    - plan_digits: {rounding: r}"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  st <- tfl_read_study_spec(d)
  # the builder's path: study$force, written by tfl_write_study_part()
  st$force <- tflspec:::.force_of("sas")
  tfl_write_study_part(st, "defaults")
  y <- yaml::read_yaml(file.path(d, "defaults.yml"))
  expect_identical(y$force, list(content = list(list(plan_digits = list(rounding = "sas")))))
  expect_identical(tflspec:::.study_rounding(tfl_read_study_spec(d)), "sas")
  # (none: the block is gone, not kept from the file written over)
  st$force <- list()
  tfl_write_study_part(st, "defaults")
  expect_null(yaml::read_yaml(file.path(d, "defaults.yml"))$force)
  expect_null(tflspec:::.study_rounding(tfl_read_study_spec(d)))
  # the sheets: the study sheet's key `rounding` is the force, both ways
  st$force <- tflspec:::.force_of("r")
  sh <- tfl_sheets_from_spec(st, tfl_read_outputs(d))
  expect_identical(sh$table$study$value[sh$table$study$key == "rounding"], "r")
  expect_null(sh$study$settings$rounding)
  to <- tempfile("sr-yaml")
  on.exit(unlink(to, recursive = TRUE), add = TRUE)
  suppressWarnings(suppressMessages(tfl_yaml_from_sheets(sh, to)))
  expect_null(yaml::read_yaml(file.path(to, "study.yml"))$settings$rounding)
  y <- yaml::read_yaml(file.path(to, "spec", "defaults.yml"))
  expect_identical(y$force, list(content = list(list(plan_digits = list(rounding = "r")))))
  expect_false(any(grepl("rounding", unlist(y$content), fixed = TRUE)))
  expect_false(any(vapply(y$content, function(it) identical(names(it), "plan_digits") &&
                            !is.null(it$plan_digits$rounding), NA)))
  # tfl_ai_study_init(): the rounding in defaults.yml's force:, not in study.yml
  ai <- tempfile("sr-ai")
  on.exit(unlink(ai, recursive = TRUE), add = TRUE)
  dir.create(file.path(ai, "adam"), recursive = TRUE)
  saveRDS(tflspec::adsl, file.path(ai, "adam", "adsl.rds"))
  tfl_ai_study_init(ai, adam = "adam", study_id = "SR-AI")
  expect_null(yaml::read_yaml(file.path(ai, "study.yml"))$settings$rounding)
  expect_identical(tflspec:::.study_rounding(tfl_read_study_spec(ai)), "sas")
})

test_that("the workbook's study rounding: in the setup, not in each table's code", {
  sp <- tfl_table_spec(
    tables = data.frame(output_id = "T1", cols = "TRT01A", stringsAsFactors = FALSE),
    study = data.frame(key = "rounding", value = "sas", stringsAsFactors = FALSE))
  expect_identical(tflspec:::.study_rounding(sp), "sas")
  expect_true(any(grepl("rtfreporter.rounding = \"sas\"", tfl_report_setup_code(sp), fixed = TRUE)))
  expect_false(any(grepl("rounding", tfl_table_code(sp, "T1", normalize = FALSE), fixed = TRUE)))
  # the defaults' content the converter writes has no card of it
  items <- tflspec:::.plan_content_from_sheets(sp, NA)
  expect_false(any(vapply(items, function(it) identical(it$fn, "plan_digits"), NA)))
})

# ---- cards formats the ARD's numbers: the study's rounding reaches it ----

sr_ard_spec <- function(rounding = NULL) {
  sp <- exact_spec(list(method = "categorical", population_id = "SAF",
                        by = "TRT01A", variables = "SEX"))
  if (!is.null(rounding)) {
    sp$study[nrow(sp$study) + 1L, "key"] <- "rounding"
    sp$study[nrow(sp$study), "value"] <- rounding
  }
  sp
}
# the numbers cards formats under the options a setup's lines set
sr_cards_under <- function(setup, x, digits = 1) {
  # (a setup line may set rtfreporter's option too: both put back)
  old <- options(cards.round_type = NULL, rtfreporter.rounding = NULL)
  on.exit(options(old), add = TRUE)
  for (l in grep("cards.round_type", setup, value = TRUE, fixed = TRUE)) eval(str2lang(l))
  cards::label_round(digits)(x)
}

test_that("the setup a program writes sets cards' rounding with the study's", {
  st_sas <- tfl_report_setup_code(sr_study("sas"))
  st_r <- tfl_report_setup_code(sr_study("r"))
  expect_true(any(grepl("cards.round_type = \"round-half-up\"", st_sas, fixed = TRUE)))
  expect_true(any(grepl("cards.round_type = \"round-to-even\"", st_r, fixed = TRUE)))
  expect_false(any(grepl("cards.round_type", tfl_report_setup_code(
    tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))), fixed = TRUE)))
  # the workbook's study rounding
  wb <- function(v) tfl_table_spec(
    tables = data.frame(output_id = "T1", cols = "TRT01A", stringsAsFactors = FALSE),
    study = data.frame(key = "rounding", value = v, stringsAsFactors = FALSE))
  expect_true(any(grepl("round-to-even", tfl_report_setup_code(wb("r")), fixed = TRUE)))
  expect_true(any(grepl("round-half-up", tfl_report_setup_code(wb("sas")), fixed = TRUE)))
  # and the numbers that setup gives: a tie at 1 decimal, and at none
  expect_identical(sr_cards_under(st_r, c(0.25, 2.5), 0), c("0", "2"))
  expect_identical(sr_cards_under(st_r, 0.25), "0.2")
  expect_identical(sr_cards_under(st_sas, c(0.25, 2.45)), c("0.3", "2.5"))
  expect_identical(sr_cards_under(st_sas, 2.5, 0), "3")
})

test_that("the ARD program's setup sets cards' rounding with the study's", {
  none <- tfl_ard_code(sr_ard_spec(), part = "setup")
  expect_false(any(grepl("cards.round_type", none, fixed = TRUE)))
  r <- tfl_ard_code(sr_ard_spec("r"), part = "setup")
  s <- tfl_ard_code(sr_ard_spec("sas"), part = "setup")
  expect_true(any(grepl("cards.round_type = \"round-to-even\"", r, fixed = TRUE)))
  expect_true(any(grepl("cards.round_type = \"round-half-up\"", s, fixed = TRUE)))
  # (the whole program carries it too)
  expect_true(any(grepl("round-to-even", tfl_ard_code(sr_ard_spec("r"), save = FALSE), fixed = TRUE)))
  expect_identical(sr_cards_under(r, c(0.25, 2.5), 0), c("0", "2"))
  expect_identical(sr_cards_under(s, c(0.25, 2.5), 0), c("0", "3"))
  expect_identical(sr_cards_under(r, 0.25), "0.2")
  expect_identical(sr_cards_under(s, 0.25), "0.3")
})

test_that("a YAML study's ARD setup carries its forced rounding", {
  skip_on_cran()
  for (v in c("sas", "r")) {
    dir <- tempfile("sr-ard")
    dir.create(file.path(dir, "spec", "outputs"), recursive = TRUE)
    write_study_yml(dir, c("study_id: SR-1", "keys:", "  id: USUBJID"))
    writeLines(c("force:", "  content:", paste0("    - plan_digits: {rounding: ", v, "}")),
               file.path(dir, "spec", "defaults.yml"))
    writeLines(c("datasets:", "  - dataset: ADSL", "    level: ADaM", "    path: data/adam/adsl.rds",
                 "populations:", "  - id: SAF", "    dataset: ADSL", "    where: !r SAFFL == \"Y\""),
               file.path(dir, "spec", "data.yml"))
    writeLines(c("outputs:", "  - id: T-1", "    kind: table"), file.path(dir, "spec", "toc.yml"))
    writeLines(c("id: T-1", "kind: table", "extends: none", "analysis:", "  analyses:",
                 "    - id: AGE", "      method: continuous", "      population: SAF",
                 "      by: [ARM]", "      variables: [AGE]", "content:", "  - table_plan: {cols: ARM}"),
               file.path(dir, "spec", "outputs", "T-1.yml"))
    st <- tfl_read_study_spec(dir)
    expect_identical(tflspec:::.study_rounding(st), v)
    setup <- tfl_ard_code(st, part = "setup")
    expect_true(any(grepl(if (v == "r") "cards.round_type = \"round-to-even\"" else
                            "cards.round_type = \"round-half-up\"", setup, fixed = TRUE)))
    unlink(dir, recursive = TRUE)
  }
})

test_that("an in-process run sets cards' rounding and puts the options back", {
  old <- options(rtfreporter.rounding = NULL, cards.round_type = NULL)
  on.exit(options(old), add = TRUE)
  op <- tflspec:::.study_rounding_option(sr_study("r"))
  expect_identical(getOption("rtfreporter.rounding"), "r")
  expect_identical(getOption("cards.round_type"), "round-to-even")
  expect_identical(cards::label_round(1)(0.25), "0.2")
  options(op)
  expect_null(getOption("rtfreporter.rounding"))
  expect_null(getOption("cards.round_type"))
  op <- tflspec:::.study_rounding_option(sr_study("sas"))
  expect_identical(getOption("cards.round_type"), "round-half-up")
  expect_identical(cards::label_round(1)(0.25), "0.3")
  options(op)
  expect_null(getOption("cards.round_type"))
  # a study that forces none leaves both
  options(cards.round_type = "round-to-even")
  st <- sr_study(NULL)
  expect_identical(tflspec:::.study_rounding_option(st), list())
  expect_identical(getOption("cards.round_type"), "round-to-even")
  # tfl_build_ard() leaves cards' option as it found it
  options(cards.round_type = NULL)
  sp <- sr_ard_spec("r")
  d <- exact_dir(exact_data())
  suppressMessages(tfl_build_ard(sp, dir = d, save = FALSE))
  expect_null(getOption("cards.round_type"))
})

test_that("the sheets' setup writes the rounding and cards' in one options() call, as the YAML route", {
  for (v in c("sas", "r")) {
    setup <- tfl_report_setup_code(tfl_table_spec(study = c(rounding = v)))
    hit <- grep("rtfreporter.rounding", setup, value = TRUE, fixed = TRUE)
    expect_length(hit, 1L)
    expect_match(hit, "cards.round_type", fixed = TRUE)
    expect_length(grep("cards.round_type", setup, fixed = TRUE), 1L)
  }
})
