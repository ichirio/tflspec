# The keys each SPEC file takes (R/spec_keys.R): one closed list, every key
# a file does not take in one classed stop, and tfl_check_spec()'s rows
# (canon 11-28 :465, spec-keys-v2.md 2-4).

keys_err <- function(expr) tryCatch(expr, tflspec_spec_keys = function(e) e)

test_that("one list: study.yml, its settings, ai and keys, a report and its ai", {
  k <- tflspec:::.spec_keys
  expect_setequal(unique(k$part), c("study", "settings", "ai", "keys", "report", "report_ai"))
  expect_false(anyDuplicated(paste(k$part, k$key)) > 0L)
  expect_setequal(k$key[k$part == "ai"], c("run", "review", "language", "requests_per_chat", "carry",
                                           "max_rounds", "shell_footnotes", "shell_code"))
  expect_identical(k$key[k$part == "keys"], c("id", "output", "source"))
  expect_false(any(c("series", "rounding", "shell_code", "layers") %in% k$key[k$part %in% c("study", "settings", "report")]))
  # (the report's in the writer's order: codelists before data, last_step last)
  r <- k$key[k$part == "report"]
  expect_lt(match("codelists", r), match("data", r))
  expect_lt(match("template", r), match("data", r))
  expect_identical(r[length(r)], "last_step")
  expect_identical(tflspec:::.output_keys$key, r)
  # (the old keys each have their old form)
  expect_true(all(tflspec:::.spec_keys_old$form %in% tflspec:::.old_forms()$id))
})

test_that("study.yml: every key it does not take in one classed stop, each with what to do", {
  d <- withr::local_tempdir()
  write_study_yml(d, c("study_id: S-1", "titel: x", "my_memo: 1", "series: {a: 1}",
                       "settings:", "  output_pth: out", "  shell_code: '^t_'", "  rounding: r",
                       "ai:", "  footnotes_at: table", "  ask_when: always", "  titles_in_header: [A]",
                       "keys:", "  id: USUBJID", "  rounding: sas"))
  e <- keys_err(tfl_read_study_spec(d))
  expect_s3_class(e, c("tflspec_spec_keys", "tflspec_spec_form"))
  expect_setequal(e$keys, c("titel", "my_memo", "series", "settings: output_pth", "settings: shell_code",
                            "settings: rounding", "ai: footnotes_at", "ai: ask_when",
                            "ai: titles_in_header", "keys: rounding"))
  p <- e$problems
  expect_identical(p$kind[p$key == "titel"], "typo")
  expect_identical(p$near[p$key == "titel"], "title")
  expect_identical(p$kind[p$key == "my_memo"], "unknown")
  expect_identical(p$form[p$key == "settings: rounding"], "settings_rounding")
  expect_identical(p$form[p$key == "ai: footnotes_at"], "ai_footnotes_at")
  m <- conditionMessage(e)
  expect_match(m, "study.yml: 10 keys it does not take:", fixed = TRUE)
  expect_match(m, "`titel`: not a key: a typo? did you mean `title:`?", fixed = TRUE)
  expect_match(m, "`my_memo`: not a key. Your own memo: write it in a `#` comment. A new need", fixed = TRUE)
  expect_match(m, "`ai: footnotes_at`: an old form, no longer read: write `ai: shell_footnotes:`", fixed = TRUE)
  expect_match(m, "`settings: shell_code`: an old form, no longer read: .* `ai: shell_code:`")
  expect_match(m, "        rounding: r", fixed = TRUE)
  expect_match(m, "study.yml takes `spec_version`", fixed = TRUE)
  expect_match(m, "study.yml `settings:` takes `output_path`", fixed = TRUE)
  # (the study as read without them, for a GUI that still opens it)
  expect_s3_class(e$spec, "tfl_study_spec")
  expect_identical(e$spec$study$study_id, "S-1")
  # tfl_check_spec(): a row a key, the study checked all the same
  r <- tfl_check_spec(d)
  kr <- r[r$part == "file", , drop = FALSE]
  expect_identical(nrow(kr), 10L)
  expect_true(all(kr$level == "error" & is.na(kr$output_id)))
  expect_true("study.yml: settings: output_pth" %in% kr$item)
})

test_that("study.yml settings: the keys a study's templates declare they read are taken", {
  d <- withr::local_tempdir()
  write_study_yml(d, c("study_id: S-1", "settings:", "  house_rule: x", "  sd_missing: '-'"))
  expect_identical(keys_err(tfl_read_study_spec(d))$keys, "settings: house_rule")
  dir.create(file.path(d, "spec", "series", "acme_v1", "templates"), recursive = TRUE)
  writeLines(c("name: t", "args:", "  rule: !study settings.house_rule"),
             file.path(d, "spec", "series", "acme_v1", "templates", "t.yml"))
  expect_s3_class(tfl_read_study_spec(d), "tfl_study_spec")
  expect_true("house_rule" %in% tflspec:::.settings_declared(d))
  expect_true(all(c("sd_missing", "style") %in% tflspec:::.settings_declared()))
})

test_that("ai: the keys it takes; shell_footnotes read, the old names stop", {
  d <- withr::local_tempdir()
  write_study_yml(d, c("study_id: X", "ai:", "  shell_footnotes: table", "  max_rounds: 2"))
  s <- tflspec:::.ai_spec_settings(d)
  expect_identical(s$shell_footnotes, "table")
  expect_identical(s$max_rounds, 2L)
  write_study_yml(d, c("study_id: X", "ai:", "  mystery: 1"))
  e <- keys_err(tflspec:::.ai_spec_settings(d))
  expect_s3_class(e, "tflspec_spec_keys")
  expect_identical(e$keys, "ai: mystery")
})

test_that("ai: shell_code is read from study.yml's ai:", {
  d <- withr::local_tempdir()
  write_study_yml(d, c("study_id: X", "ai:", "  shell_code: '^S[0-9]+$'"))
  expect_s3_class(tfl_read_study_spec(d), "tfl_study_spec")
  expect_identical(yaml::read_yaml(file.path(d, "study.yml"))$ai$shell_code, "^S[0-9]+$")
})

test_that("a report: every key it does not take, its maps' too, in one stop naming the file", {
  d <- withr::local_tempdir()
  f <- file.path(d, "T-1.yml")
  writeLines(c("id: T-1", "kind: table", "contnet: []", "layers: [{layer: point}]",
               "ggplot2_version: '4.0'", "last_step: content", "outputs: []",
               "ai: {assumptions: [a], mood: ok}", "common: {population: SAF, popn: x}",
               "data:", "  - {id: ADSL, from: ADSL, wherre: x}",
               "analysis:", "  analyses:", "    - {id: AGE, methd: continuous}"), f)
  e <- keys_err(tfl_read_output(f))
  expect_s3_class(e, "tflspec_spec_keys")
  expect_identical(e$file, "T-1.yml")
  expect_setequal(e$keys, c("contnet", "layers", "ggplot2_version", "last_step", "outputs", "ai: mood",
                            "common: popn", "data: ADSL: wherre", "analysis: analyses: AGE: methd"))
  p <- e$problems
  expect_identical(p$near[p$key == "contnet"], "content")
  expect_identical(p$kind[p$key == "ggplot2_version"], "kind")
  expect_identical(p$kind[p$key == "last_step"], "later")
  expect_identical(p$form[p$key == "layers"], "report_layers")
  expect_identical(p$form[p$key == "outputs"], "report_outputs")
  m <- conditionMessage(e)
  expect_match(m, "^T-1.yml: 9 keys it does not take:")
  expect_match(m, "`ggplot2_version`: a key of a figure report, not of a table one", fixed = TRUE)
  expect_match(m, "Your own memo: write it under `note:` or in a `#` comment", fixed = TRUE)
  expect_match(m, "a table report takes `id`, `kind`, `label`", fixed = TRUE)
  # last_step: page is read
  writeLines(c("id: T-1", "kind: table", "last_step: page"), f)
  expect_identical(tfl_read_output(f)$last_step, "page")
})

test_that("plot / plots and content / contents: the value's shape names the other key", {
  d <- withr::local_tempdir()
  f <- file.path(d, "F-1.yml")
  writeLines(c("id: F-1", "kind: figure", "plot: {km: {theme: bw}}", "contents: [{plan_x: {}}]"), f)
  e <- keys_err(tfl_read_output(f))
  expect_setequal(e$keys, c("plot", "contents"))
  expect_match(conditionMessage(e), "`plot`: holds plots by name, which is `plots:`", fixed = TRUE)
  expect_match(conditionMessage(e), "one list of items is `content:`", fixed = TRUE)
  writeLines(c("id: F-1", "kind: figure", "plots: {theme: bw}", "content: {plan_x: {}}"), f)
  expect_setequal(keys_err(tfl_read_output(f))$keys, c("plots", "content"))
})

test_that("tfl_check_spec(): a report's keys are rows, not one unreadable file", {
  d <- withr::local_tempdir()
  write_study_yml(d, "study_id: S-1")
  dir.create(file.path(d, "spec", "outputs"), recursive = TRUE)
  writeLines(c("id: T-1", "kind: table", "contnet: []", "mood: ok"), file.path(d, "spec", "outputs", "T-1.yml"))
  r <- tfl_check_spec(d)
  kr <- r[r$part == "file", , drop = FALSE]
  expect_identical(kr$item, c("T-1.yml: contnet", "T-1.yml: mood"))
  expect_true(all(kr$output_id == "T-1" & kr$level == "error"))
})

test_that("the writer writes the list's order: the layer pickers, codelists before data", {
  o <- tfl_output("T-1", "table", label = "L", data = list(source = "ard"),
                  codelists = list(SEX = c(M = "Male")),
                  content = list(tfl_item("plan_cells", list(categorical = "{n}"))))
  o$template <- list(name = "x")
  o$style <- "small"
  o$note <- "n"
  y <- tflspec:::.output_to_list(o)
  expect_identical(names(y), intersect(tflspec:::.output_keys$key, names(y)))
  expect_lt(match("codelists", names(y)), match("content", names(y)))
  expect_lt(match("template", names(y)), match("codelists", names(y)))
  o$mystery <- 1
  e <- keys_err(tfl_write_output(o, tempfile(fileext = ".yml")))
  expect_identical(e$keys, "mystery")
})

test_that("force: one form; a force that holds anything else is the classed stop", {
  d <- withr::local_tempdir()
  write_study_yml(d, "study_id: S-1")
  dir.create(file.path(d, "spec"))
  writeLines(c("force:", "  content:", "    - plan_digits: {rounding: half}"), file.path(d, "spec", "defaults.yml"))
  e <- keys_err(tfl_read_study_spec(d))
  expect_s3_class(e, "tflspec_spec_keys")
  expect_identical(e$keys, "force: content: plan_digits: rounding")
  expect_match(conditionMessage(e), "takes \"sas\" or \"r\", not \"half\"", fixed = TRUE)
  writeLines(c("force:", "  page: {}"), file.path(d, "spec", "defaults.yml"))
  expect_identical(keys_err(tfl_read_study_spec(d))$keys, "force: page")
  # tfl_study_rounding(): the one reader and writer
  unlink(file.path(d, "spec", "defaults.yml"))
  s <- tfl_read_study_spec(d)
  expect_null(tfl_study_rounding(s))
  s <- tfl_study_rounding(s, "r")
  expect_identical(tfl_study_rounding(s), "r")
  f <- tfl_write_study_part(s, "defaults")
  expect_identical(readLines(f)[1:4], tflspec:::.force_yaml("r"))
  expect_identical(tfl_study_rounding(tfl_read_study_spec(d)), "r")
  expect_null(tfl_study_rounding(tfl_study_rounding(s, NULL)))
  expect_error(tfl_study_rounding(list()), "tfl_read_study_spec")
})

test_that("the ARD sheets' study key rounding is an old form: the study's force", {
  p <- tflspec:::.spec_keys_find(list(id = "USUBJID", rounding = "sas"), "keys", "the ARD sheets' study sheet",
                                 "keys", ok = c(tflspec:::.spec_keys_of("keys"), "adam_levels"))
  expect_identical(p$form, "ard_study_rounding")
  expect_match(p$text, "spec/defaults.yml `force:`", fixed = TRUE)
})

test_that("CG-3: the tables sheet's header_n is col_header_values; header_n stops naming it", {
  sp <- tfl_table_spec(tables = data.frame(cols = "BGRADE", col_header_values = "n = page | N = table"))
  expect_identical(tflspec:::.ard_spec_table_args(sp)$col_header_values, list(n = "page", N = "table"))
  expect_error(tfl_table_spec(tables = data.frame(cols = "BGRADE", header_n = "table")),
               "it is named `col_header_values`", fixed = TRUE)
})
