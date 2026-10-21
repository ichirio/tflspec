# The standard filter pair of every template: which subjects (`subset`, on
# the analysis set's ADSL) and which records (`where`)

subset_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("haven")
}

# the package's ADSL with a PART (every other subject in Part 1); ADAE has none
subset_adsl <- function() {
  sl <- adsl
  sl$PART <- ifelse(seq_len(nrow(sl)) %% 2L == 1L, "Part 1", "Part 2")
  sl
}

test_that("every table and figure template takes subset; a records template where", {
  t <- tfl_templates()
  has <- function(n, p) p %in% names(tflspec:::.tpl_get(n, "test")$params)
  # (km_table and stat_block read the report's own ARD: no data of their own)
  own <- setdiff(t$name[t$package == "tflspec"], c("km_table", "stat_block"))
  expect_true(all(vapply(own, has, NA, p = "subset")))
  expect_true(all(vapply(setdiff(own, c("demog", "disposition", "summary")), has, NA,
                         p = "where")))
  p <- tflspec:::.tpl_get("ae_soc_pt", "test")$params
  expect_identical(vapply(p[c("population", "subset", "where")], `[[`, "", "group"),
                   c(population = "target", subset = "target", where = "target"))
  expect_identical(p$subset$type, "text")
  # tfl_test_templates(): one without them is said
  tfl_template("tsub_ae", kind = "table", layout = "hierarchical",
               params = list(tfl_param("population", "SAF", study = "output.population")),
               body = function(a, ctx) list(
                 ard = list(analysis_data = list(list(id = "adae_x", from = "ADAE",
                                                      population = a$population)),
                            analyses = list()),
                 data = list(source = "ard"), content = list()))
  on.exit(rm(list = "tsub_ae", envir = tflspec:::.tpl_registry))
  r <- tfl_test_templates("tsub_ae")
  expect_setequal(r$status, c("no subset", "no where (a records template)"))
  expect_identical(nrow(tfl_test_templates(own)), 0L)
})

test_that("expanded: subset on the set's ADSL; a records data takes its subjects", {
  x <- tfl_template_expand("ae_soc_pt", list(population = "SAF", subset = "PART == \"Part 1\""))
  d <- x$ard$analysis_data
  expect_identical(as.character(d[[1L]]$where), "PART == \"Part 1\"")
  expect_identical(d[[2L]]$subjects, "adsl_saf")
  # a records data of the set itself: an ADSL data of the set first
  y <- tfl_template_expand("lab_shift", list(population = "SAF", visit = "WEEK 2",
                                             params = "ALT", subset = "PART == \"Part 1\"",
                                             where = "ANL01FL == \"Y\""))
  d <- y$ard$analysis_data
  expect_identical(vapply(d, `[[`, "", "id"), c("adsl_saf", "adlb_saf"))
  expect_null(d[[2L]]$population)
  expect_identical(d[[2L]]$subjects, "adsl_saf")
  expect_match(as.character(d[[2L]]$where), "& ANL01FL == \"Y\"$")
  # an "or" in brackets
  z <- tfl_template_expand("demog", list(population = "SAF", subset = "PART == \"A\" | AGE > 70"))
  expect_identical(as.character(z$ard$analysis_data[[1L]]$where), "PART == \"A\" | AGE > 70")
  # a figure: the set's semi join keeps them too; none given: one of its own
  f <- tfl_template_expand("km_simple", list(param = "PFS", subset = "PART == \"Part 1\""))
  j <- f$data$steps[[2L]]
  expect_identical(c(j$step, j$dataset, j$where, j$how), c("join", "ADSL", "PART == \"Part 1\"", "semi"))
  # a listing has no analysis set: the records of ADSL's subjects it keeps
  l <- tfl_template_expand("listing", list(data = "ADAE", subset = "PART == \"Part 1\"",
                                           columns = list(list(vars = "USUBJID"))))
  expect_null(l$data$where)
  expect_match(as.character(l$data$steps[[1L]]),
               "adsl |> dplyr::filter(PART == \"Part 1\") |> dplyr::select(USUBJID)", fixed = TRUE)
})

test_that("in a study: a listing of ADAE (no PART) with subset lists Part 1's subjects only", {
  subset_skip()
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  sl <- subset_adsl()
  haven::write_xpt(tflspec:::.ai_chr(sl), file.path(d, "adam", "adsl.xpt"))
  haven::write_xpt(tflspec:::.ai_chr(adae), file.path(d, "adam", "adae.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "SUB-L")
  o <- tfl_output("L-9-1", "listing", label = "AEs, Part 1", extends = "defaults",
                  template = list(name = "listing", args = list(
                    data = "ADAE", where = "TRTEMFL == \"Y\"", subset = "PART == \"Part 1\"",
                    columns = list(list(vars = "USUBJID", label = "Subject"),
                                   list(vars = "AEDECOD", label = "PT")))))
  f <- file.path(d, "spec", "outputs", "L-9-1.yml")
  tfl_write_output(o, f)
  st <- tfl_read_study_spec(d)
  ro <- tfl_resolve_output(tfl_read_output(f), st)
  # the program as the sheets write it: the listing's rows, its rework
  sh <- suppressMessages(tfl_sheets_from_spec(st, list(`L-9-1` = ro)))
  rework <- stats::na.omit(sh$planner$process_code[sh$planner$output_id == "L-9-1"])
  code <- tfl_listing_code(tfl_listing_spec(sh$listing, check = FALSE), "L-9-1",
                           sh$ard$datasets, rework = rework, part = "data")
  expect_true(any(grepl("adsl <- ", code, fixed = TRUE)))
  e <- new.env(parent = .fig_pkg_env("dplyr"))
  old <- setwd(d)
  on.exit(setwd(old), add = TRUE)
  eval(parse(text = code), envir = e)
  p1 <- sl$USUBJID[sl$PART == "Part 1"]
  te <- adae[adae$TRTEMFL == "Y", ]
  expect_true(nrow(e$data) > 0L)
  expect_true(all(e$data$USUBJID %in% p1))
  expect_identical(nrow(e$data), sum(te$USUBJID %in% p1))
})

test_that("in a study: demog and ae_soc_pt of Part 1's subjects; N and counts theirs; A = B", {
  subset_skip()
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  dir.create(file.path(d, "adam"), recursive = TRUE)
  sl <- subset_adsl()
  haven::write_xpt(tflspec:::.ai_chr(sl), file.path(d, "adam", "adsl.xpt"))
  haven::write_xpt(tflspec:::.ai_chr(adae), file.path(d, "adam", "adae.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "SUB-1")
  sub <- list(subset = "PART == \"Part 1\"")
  outs <- list(
    tfl_output("T-9-1", "table", population = "SAF", label = "Demographics, Part 1",
               extends = "defaults", template = list(name = "demog", args = sub)),
    tfl_output("T-9-2", "table", population = "SAF", label = "TEAEs, Part 1",
               extends = "defaults", template = list(name = "ae_soc_pt", args = sub)),
    # the same set's name in another report, every subject: its own
    tfl_output("T-9-3", "table", population = "SAF", label = "Demographics",
               extends = "defaults", template = list(name = "demog")))
  for (o in outs) tfl_write_output(o, file.path(d, "spec", "outputs", paste0(o$id, ".yml")))
  st <- tfl_read_study_spec(d)
  code <- paste(tfl_ard_code(st, output_id = "T-9-2", save = FALSE, part = "body"),
                collapse = "\n")
  # the denominator: the analysis set and the subset in one filter(); the
  # target subjects, then the target records
  expect_match(code, "adsl_saf <- filter(adsl, SAFFL == \"Y\", PART == \"Part 1\")", fixed = TRUE)
  expect_match(code, paste0("  inner_join(select(adsl_saf, USUBJID, TRT01A), by = \"USUBJID\") |>\n",
                            "  filter(TRTEMFL == \"Y\")"), fixed = TRUE)
  ard <- suppressMessages(tfl_build_ard(st, dir = d, save = FALSE))
  saf <- sl[sl$SAFFL == "Y", ]
  p1 <- saf[saf$PART == "Part 1", ]
  n_of <- function(a, o, id) {
    r <- a[a$output_id == o & a$analysis_id == id & a$variable == "TRT01A" &
             a$stat_name == "n", ]
    stats::setNames(unlist(r$stat), as.character(unlist(r$variable_level)))
  }
  expect_identical(as.integer(n_of(ard, "T-9-1", "GROUPN")[c("Drug X", "Control")]),
                   as.integer(table(p1$TRT01A)[c("Drug X", "Control")]))
  expect_identical(as.integer(n_of(ard, "T-9-3", "GROUPN")[c("Drug X", "Control")]),
                   as.integer(table(saf$TRT01A)[c("Drug X", "Control")]))
  # the AE table: Part 1's subjects with a TEAE, over Part 1's N
  te <- adae[adae$TRTEMFL == "Y" & adae$USUBJID %in% p1$USUBJID, ]
  any <- ard[ard$output_id == "T-9-2" & ard$stat_name %in% c("n", "N") &
               vapply(ard$variable_level, function(v) identical(unlist(v), TRUE) ||
                        is.null(unlist(v)), NA), ]
  arm <- vapply(any$group1_level, function(v) as.character(unlist(v))[1L], "")
  over <- any[any$variable == "..ard_hierarchical_overall.." & arm == "Drug X", ]
  expect_identical(as.integer(unlist(over$stat[over$stat_name == "n"])),
                   length(unique(te$USUBJID[te$TRT01A == "Drug X"])))
  expect_identical(as.integer(unlist(over$stat[over$stat_name == "N"])),
                   sum(p1$TRT01A == "Drug X"))
  # A (the call) = B (the items)
  tab <- function(p) {
    t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
    t[] <- lapply(t, as.character)
    t
  }
  for (id in c("T-9-1", "T-9-2")) {
    ro <- tfl_resolve_output(tfl_read_output(file.path(d, "spec", "outputs",
                                                       paste0(id, ".yml"))), st)
    a <- ard[ard$output_id == id, ]
    b <- tab(suppressMessages(tfl_content_run(ro, a)))
    e <- new.env()
    e$ard <- a
    suppressMessages(eval(parse(text = tfl_content_code(ro, pipe = "|>")), envir = e))
    expect_identical(tab(e$plan), b)
  }
})

test_that("a figure (km_simple) of Part 1's subjects: A = B = the template SPEC", {
  skip_on_cran()
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("haven")
  d <- tempfile()
  on.exit(unlink(d, recursive = TRUE))
  data <- tplfig_study(d, c("adsl", "adtte"), "KM-SUB")
  sl <- subset_adsl()
  haven::write_xpt(tflspec:::.ai_chr(sl), file.path(d, "adam", "adsl.xpt"))
  data$adsl <- as.data.frame(haven::read_xpt(file.path(d, "adam", "adsl.xpt")))
  o <- tfl_output("F-9-2", "figure", population = "SAF", label = "KM curves, Part 1",
                  extends = "defaults",
                  template = list(name = "km_simple",
                                  args = list(param = "PFS", group = "TRT01A",
                                              subset = "PART == \"Part 1\"")))
  x <- tplfig_check(o, d, data)
  txt <- paste(x$a, collapse = "\n")
  expect_match(txt, "PART == \"Part 1\"", fixed = TRUE)
  env <- new.env(parent = .fig_pkg_env(c(attr(x$a, "libs"), "tflspec")))
  for (n in names(data)) assign(n, data[[n]], envir = env)
  eval(parse(text = tfl_fig_setup_code()), env)
  eval(parse(text = x$a), env)
  p1 <- sl$USUBJID[sl$SAFFL == "Y" & sl$PART == "Part 1"]
  expect_setequal(unique(env$adtte_pfs$USUBJID),
                  intersect(p1, adtte$USUBJID[adtte$PARAMCD == "PFS"]))
})
