# tfl_check_spec(): every `!r` value of a report (its `ard:` aside, which
# the ARD check reads by its own fields) is read as R once; one that does
# not read is an error row with its place in the SPEC and the parser's
# message

test_that("an `!r` value that does not read as R: an error row, its place and the parser's message", {
  skip_if_not_installed("haven")
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "RP-1")
  one <- function(id, item) {
    tfl_write_output(tfl_output(id, "table", common = list(population = "SAF"),
      template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")),
      content = list(item)), file.path(d, "spec", "outputs", paste0(id, ".yml")))
  }
  # a slot of R (plan_filter's condition), and a guard before a cell template
  one("T-F", tfl_item("plan_filter", list(where = tfl_r("p >="))))
  one("T-G", tfl_item("plan_cells", list(categorical = list(tfl_r("n == 0 ~"), "{n} ({p:.1f%})"))))
  p <- tfl_check_spec(d)
  shown <- paste(utils::capture.output(print(p[, c("output_id", "part", "item", "level", "message")])),
                 collapse = "\n")
  r <- p[grepl("does not read as R", p$message, fixed = TRUE), , drop = FALSE]
  expect_identical(sort(r$output_id), c("T-F", "T-G"), info = shown)
  expect_true(all(r$level == "error"), info = shown)
  expect_identical(r$item[r$output_id == "T-F"], "content$plan_filter$args$where", info = shown)
  expect_identical(r$item[r$output_id == "T-G"], "content$plan_cells$args$categorical$1", info = shown)
  expect_true(all(grepl("unexpected end of input", r$message, fixed = TRUE)), info = shown)
  expect_match(r$message[r$output_id == "T-F"], "p >=", fixed = TRUE)

  # R that reads: no such row
  one("T-F", tfl_item("plan_filter", list(where = tfl_r("p >= 0.05"))))
  one("T-G", tfl_item("plan_cells", list(categorical = list(tfl_r("n == 0 ~ '0'"), "{n} ({p:.1f%})"))))
  p <- tfl_check_spec(d)
  expect_false(any(grepl("does not read as R", p$message, fixed = TRUE)))
})

# The walker itself (.r_walk(), tflspec#986): in-memory reports, the parser
# by the kind of slot the schema says, each `!r` once

test_that(".r_walk() reads each `!r` by its slot: expression, guard, formula, call", {
  walk <- function(...) {
    outs <- list(`T-1` = tfl_output("T-1", "table", content = list(...)))
    .r_walk(NULL, outs, "T-1")
  }
  # a plain expression (plan_filter's condition)
  r <- walk(tfl_item("plan_filter", list(where = tfl_r("p >="))))
  expect_identical(r$item, "content$plan_filter$args$where")
  expect_identical(r$part, "content")
  expect_identical(r$level, "error")
  expect_identical(r$output_id, "T-1")
  expect_match(r$message, "unexpected end of input", fixed = TRUE)
  # a guard before a cell template: its place is the list's index
  r <- walk(tfl_item("plan_cells", list(categorical = list(tfl_r("n == 0 ~"), "{n}"))))
  expect_identical(r$item, "content$plan_cells$args$categorical$1")
  # (a one-sided `~ text` is no guard; the same text is a formula in a formula's slot)
  r <- walk(tfl_item("plan_cells", list(categorical = list(tfl_r("~ 'x'"), "{n}"))))
  expect_identical(r$item, "content$plan_cells$args$categorical$1")
  expect_match(r$message, "both sides", fixed = TRUE)
  expect_identical(nrow(walk(tfl_item("plan_cell_style", list(where = tfl_r("~ p > 0.5"))))), 0L)
  # a formula (plan_cell_style's rows)
  r <- walk(tfl_item("plan_cell_style", list(where = tfl_r("p >"))))
  expect_identical(r$item, "content$plan_cell_style$args$where")
  # a call (plan_style's border)
  r <- walk(tfl_item("plan_style", list(border_body = tfl_r("rtf_border(inside_h = TRUE"))))
  expect_identical(r$item, "content$plan_style$args$border_body")
})

test_that(".r_walk(): valid R, blank text and plain strings give no row; a template's defaults are not walked", {
  walk <- function(o) .r_walk(NULL, list(`T-1` = o), "T-1")
  ok <- tfl_output("T-1", "table", content = list(
    tfl_item("plan_filter", list(where = tfl_r("p >= 0.05"))),
    tfl_item("plan_cells", list(categorical = list(tfl_r("n == 0 ~ '0'"), "{n} ({p:.1f%})"))),
    tfl_item("plan_cell_style", list(where = tfl_r("~ p > 0.5"))),
    tfl_item("plan_style", list(border_body = tfl_r("rtf_border(inside_h = TRUE)"))),
    tfl_item("plan_filter", list(where = tfl_r("")))))
  expect_identical(nrow(walk(ok)), 0L)
  # an `!r` that is not in the report as written (a template's own default,
  # which the report does not hold) is not read: walk `o`, not the resolved one
  o <- tfl_output("T-1", "table", template = list(name = "demog", args = list(by = "TRT01A")))
  expect_identical(nrow(walk(o)), 0L)
  # in a template's arguments as written it is read, with its place
  o <- tfl_output("T-1", "table", template = list(name = "demog", args = list(by = tfl_r("TRT01A +"))))
  expect_identical(walk(o)$item, "template$args$by")
})

test_that("a broken `!r` is one row, and the `program` warning that said the same is gone", {
  skip_if_not_installed("haven")
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "RP-2")
  broken <- tfl_item("plan_cells", list(categorical = list(tfl_r("n == 0 ~"), "{n} ({p:.1f%})")))
  # (without the walker's row the writer's stop is what said it)
  expect_error(suppressMessages(tfl_content_code(tfl_output("T-G", "table", content = list(broken)))))
  tfl_write_output(tfl_output("T-G", "table", common = list(population = "SAF"),
    template = list(name = "demog", args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")),
    content = list(broken)), file.path(d, "spec", "outputs", "T-G.yml"))
  p <- tfl_check_spec(d)
  shown <- paste(utils::capture.output(print(p[, c("output_id", "part", "item", "level", "message")])),
                 collapse = "\n")
  g <- p[p$output_id %in% "T-G", , drop = FALSE]
  expect_identical(sum(g$item == "content$plan_cells$args$categorical$1"), 1L, info = shown)
  expect_identical(sum(grepl("does not read as R", g$message, fixed = TRUE)), 1L, info = shown)
  expect_identical(sum(g$part == "program"), 0L, info = shown)
  # no row twice: the same (output, part, item, message)
  expect_false(anyDuplicated(g[, c("part", "item", "message")]) > 0L, info = shown)
})

# Slice 2 (tflspec#986): the report's `ard:` fields of R, by a fixed kind
# each (.r_ard_kinds); slice 3: the study's own files

test_that(".r_walk(): the ARD's fields by their kind: where, derive, then / code, args, post", {
  walk <- function(ard) .r_walk(NULL, list(`T-1` = tfl_output("T-1", "table", ard = ard)), "T-1")
  ad <- function(...) list(analysis_data = list(c(list(id = "adae_sub", from = "ADAE"), list(...))))
  an <- function(...) list(analyses = list(c(list(id = "AE", method = "custom"), list(...))))
  # where: one condition
  r <- walk(ad(where = tfl_r("TRTEMFL ==")))
  expect_identical(r$item, "ard$analysis_data$adae_sub$where")
  expect_identical(r$part, "ard")
  expect_identical(r$level, "error")
  expect_match(r$message, "`!r TRTEMFL ==` does not read as R (unexpected end of input)", fixed = TRUE)
  expect_match(walk(ad(where = tfl_r("A == 1; B == 2")))$message, "one expression", fixed = TRUE)
  # derive: pieces `name = expression` between `|`
  r <- walk(ad(derive = tfl_r("FLAG = 1 | X = (")))
  expect_identical(r$item, "ard$analysis_data$adae_sub$derive")
  expect_match(r$message, "piece `X = (`", fixed = TRUE)
  r <- walk(ad(derive = tfl_r("FLAG = ifelse(is.na(AESER) | AESER != \"Y\", 0, 1) | paste(AEDECOD)")))
  expect_match(r$message, "piece `paste(AEDECOD)` is not `name = expression`", fixed = TRUE)
  # then and code: code blocks, several expressions; the line when it has lines
  r <- walk(ad(then = tfl_r("dplyr::filter(")))
  expect_identical(r$item, "ard$analysis_data$adae_sub$then")
  r <- walk(an(code = tfl_r("fit <- lm(AVAL ~ TRT01A, data = data)\ncards::ard_summary(data,")))
  expect_identical(r$item, "ard$analyses$AE$code")
  expect_match(r$message, "(line 2: unexpected end of input)", fixed = TRUE)
  # args: a call's arguments, which plain R is not
  expect_identical(nrow(walk(an(args = tfl_r("method = \"x\", conf.level = 0.9")))), 0L)
  r <- walk(an(args = tfl_r("method = ")))
  expect_identical(r$item, "ard$analyses$AE$args")
  expect_match(r$message, "`!r method =` does not read as R (argument `method` has no value)", fixed = TRUE)
  expect_match(walk(an(args = tfl_r("method = \"x\",, conf.level = 0.9")))$message, "argument 2 has no value",
               fixed = TRUE)
  expect_match(walk(an(args = tfl_r("method = (")))$message, "does not read as R (unexpected", fixed = TRUE)
  # post: steps between `|`
  r <- walk(an(post = tfl_r("cards::add_calculated_row(expr = sd / sqrt(N), stat_name = \"se\") | f(")))
  expect_identical(r$item, "ard$analyses$AE$post")
  expect_match(r$message, "step `f(`", fixed = TRUE)
  # a field of R written without `!r` is R all the same (the ARD program
  # pastes it), and is shown without `!r`
  r <- walk(an(where = "AESEV =="))
  expect_identical(r$item, "ard$analyses$AE$where")
  expect_match(r$message, "^`AESEV ==` does not read as R")
  # an analysis without an id: its index
  expect_identical(walk(list(analyses = list(list(method = "custom", where = tfl_r("x >")))))$item,
                   "ard$analyses$1$where")
  # valid R, a blank text: nothing
  ok <- c(ad(where = tfl_r("TRTEMFL == \"Y\""), derive = tfl_r("FLAG = 1 | X = ifelse(A | B, 1, 0)"),
             then = tfl_r("dplyr::filter(AESER == \"Y\") |> dplyr::distinct(USUBJID, .keep_all = TRUE)")),
          an(code = tfl_r("x <- 1\ncards::ard_summary(data, variables = AGE)"), where = tfl_r(""),
             args = "denominator = population", post = "cards::add_calculated_row(expr = sd / sqrt(N), stat_name = \"se\")"))
  expect_identical(nrow(walk(ok)), 0L)
})

test_that("tfl_ard_spec() says a field of R that does not read with the walker's words", {
  df <- function(...) as.data.frame(list(...), stringsAsFactors = FALSE)
  spec <- function(ad) list(
    study = df(key = "id", value = "USUBJID"),
    datasets = df(dataset = c("ADSL", "ADAE"), path = c("adsl.rds", "adae.rds")),
    populations = df(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\""),
    analysis_data = ad,
    analyses = df(output_id = "T1", analysis_id = "AE", method = "cards::ard_tabulate",
                  data = "adae_sub", variables = "AEDECOD"))
  ad <- function(...) c(list(output_id = "T1", data_id = "adae_sub", from = "ADAE", population_id = "SAF"),
                        list(...))
  msg <- function(...) conditionMessage(tryCatch(tfl_ard_spec(spec(df(...))), error = function(e) e))
  expect_match(msg(output_id = "T1", data_id = "adae_sub", from = "ADAE", population_id = "SAF",
                   where = "TRTEMFL =="),
               "analysis_data T1 / adae_sub: `where` `TRTEMFL ==` does not read as R (unexpected end of input)",
               fixed = TRUE)
  expect_match(msg(output_id = "T1", data_id = "adae_sub", from = "ADAE", population_id = "SAF",
                   then = "dplyr::filter("),
               "`then` `dplyr::filter(` does not read as R", fixed = TRUE)
  expect_s3_class(tfl_ard_spec(spec(df(output_id = "T1", data_id = "adae_sub", from = "ADAE",
                                       population_id = "SAF", where = "TRTEMFL == \"Y\""))),
                  "tfl_ard_spec")
  invisible(ad)
})

test_that(".r_walk_study(): study.yml setup, defaults.yml, data.yml populations / records / datasets", {
  skip_if_not_installed("haven")
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "RP-3")
  st <- tfl_read_study_spec(d)
  # the study as made: nothing
  expect_identical(nrow(.r_walk_study(st)), 0L)
  expect_identical(nrow(.r_walk(st, list(), character())), 0L)
  bad <- st
  bad$study$setup <- tfl_r("library(dplyr)\noptions(digits = 3")
  bad$defaults$content <- c(bad$defaults$content,
                            list(tfl_item("plan_filter", list(where = tfl_r("p >=")))))
  pops <- bad$data$populations
  pops[[1L]]$where <- tfl_r("SAFFL ==")
  pops[[1L]]$derive <- tfl_r("AGEGR = (")
  bad$data$populations <- pops
  bad$data$records <- list(list(id = "TEAE", dataset = "ADSL", where = tfl_r("TRTEMFL %in%")))
  bad$data$datasets[[1L]]$derive <- "AGE2 = AGE *"
  r <- .r_walk_study(bad)
  shown <- paste(utils::capture.output(print(r)), collapse = "\n")
  pop <- as.character(pops[[1L]]$id)
  ds <- as.character(bad$data$datasets[[1L]]$dataset)
  expect_setequal(r$item, c("study.yml: setup", "defaults.yml: content$plan_filter$args$where",
                            paste0("data.yml: populations$", pop, "$where"),
                            paste0("data.yml: populations$", pop, "$derive"),
                            "data.yml: records$TEAE$where",
                            paste0("data.yml: datasets$", ds, "$derive")))
  expect_true(all(is.na(r$output_id)), info = shown)
  expect_true(all(r$part == "study"), info = shown)
  expect_true(all(r$level == "error"), info = shown)
  expect_match(r$message[r$item == "study.yml: setup"], "(line 2: unexpected end of input)", fixed = TRUE)
  # on a full check only, once each; a check of one report has none
  p <- tfl_check_spec(bad)
  shown <- paste(utils::capture.output(print(p[, c("output_id", "part", "item", "level", "message")])),
                 collapse = "\n")
  s <- p[p$part %in% "study" & grepl("does not read as R", p$message, fixed = TRUE), , drop = FALSE]
  expect_setequal(s$item, r$item)
  expect_false(anyDuplicated(s$item) > 0L, info = shown)
})

test_that("no row twice: a bad value in every slot is one row each, and no `program` warning", {
  skip_if_not_installed("haven")
  d <- file.path(withr::local_tempdir(), "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  haven::write_xpt(tflspec:::.ai_chr(tflspec::adsl), file.path(d, "adam", "adsl.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "RP-4")
  tfl_write_output(tfl_output("T-B", "table", common = list(population = "SAF"),
    data = list(list(id = "adsl_sub", from = "ADSL", where = tfl_r("AGE >"),
                     derive = tfl_r("AGE2 = ("), then = tfl_r("dplyr::filter("))),
    ard = list(analyses = list(
      list(id = "N", method = "categorical", data = "adsl_sub", by = "TRT01A", variables = "SEX",
           where = tfl_r("SEX =="), args = tfl_r("denominator = ")),
      list(id = "C", method = "custom", data = "adsl_sub", code = tfl_r("cards::ard_summary(data,")))),
    content = list(tfl_item("plan_filter", list(where = tfl_r("p >="))),
                   tfl_item("plan_cells", list(categorical = list(tfl_r("n == 0 ~"), "{n}"))))),
    file.path(d, "spec", "outputs", "T-B.yml"))
  # (a set of records the ARD check reads too, from the file: "the condition
  # is not R")
  cat("records:\n  - id: OLD\n    dataset: ADSL\n    where: !r AGE >\n",
      file = file.path(d, "spec", "data.yml"), append = TRUE)
  st <- tfl_read_study_spec(d)
  st$data$populations[[1L]]$where <- tfl_r("SAFFL ==")
  p <- tfl_check_spec(st)
  shown <- paste(utils::capture.output(print(p[, c("output_id", "part", "item", "level", "message")])),
                 collapse = "\n")
  g <- p[p$output_id %in% "T-B", , drop = FALSE]
  want <- c("ard$analysis_data$adsl_sub$where", "ard$analysis_data$adsl_sub$derive",
            "ard$analysis_data$adsl_sub$then", "ard$analyses$N$where", "ard$analyses$N$args",
            "ard$analyses$C$code", "content$plan_filter$args$where",
            "content$plan_cells$args$categorical$1")
  r <- g[grepl("does not read as R", g$message, fixed = TRUE), , drop = FALSE]
  expect_setequal(r$item, want)
  # each (output_id, item) once; the ARD check's own rows for the same
  # fields are not given again
  expect_false(anyDuplicated(g[, c("output_id", "item")]) > 0L, info = shown)
  expect_false(any(grepl("does not read as R arguments|does not read as R$", g$message)), info = shown)
  expect_false(any(g$item %in% c("adsl_sub$where", "adsl_sub$derive", "adsl_sub$then", "N$args", "OLD$where")),
               info = shown)
  # the program's warnings that said the same are not given
  expect_identical(sum(g$part == "program"), 0L, info = shown)
  # the study's rows once, with no report
  s <- p[p$part %in% "study" & grepl("does not read as R", p$message, fixed = TRUE), , drop = FALSE]
  expect_identical(nrow(s), 2L, info = shown)
  expect_true(all(is.na(s$output_id)))
  expect_true("data.yml: records$OLD$where" %in% s$item, info = shown)
  # a check of the report alone: no study row, and the ARD check's own row
  # for the record (the walker did not read data.yml) stays
  p1 <- tfl_check_spec(st, output_id = "T-B")
  expect_false(any(p1$part %in% "study" & grepl("does not read as R", p1$message, fixed = TRUE)))
  expect_true(any(p1$item %in% "OLD$where"))
})

test_that("a neutral condition (a plain string) in a slot of R is no `!r`: not read", {
  walk <- function(...) .r_walk(NULL, list(`T-1` = tfl_output("T-1", "table", content = list(...))), "T-1")
  expect_identical(nrow(walk(tfl_item("plan_filter", list(where = "p >=")))), 0L)
  expect_identical(nrow(walk(tfl_item("plan_filter", list(where = tfl_r(""))))), 0L)
})
