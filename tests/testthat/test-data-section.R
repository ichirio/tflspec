# The data section of a generated program (DATA-PART 3, step DP-1): one
# `# ---- data ----`, the analysis set built once as <dataset>_sub, each
# records dataset one pipe (drop the same-named columns, one inner_join,
# the condition), <dataset>_den for a denominator; names never read from a
# value.

ds_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adlb", "advs", "adtte", "adrs", "adtr", "adex")) {
    haven::write_xpt(tflspec:::.ai_chr(getExportedValue("tflspec", x)),
                     file.path(d, "adam", paste0(x, ".xpt")))
  }
  haven::write_xpt(tflspec:::.ai_chr(pk_adpc(tflspec::adsl)), file.path(d, "adam", "adpc.xpt"))
  tfl_ai_study_init(d, adam = "adam", study_id = "DS-1")
  d
}

# a template report of the catalog's example, its analysis set `population`
ds_output <- function(nm, id = paste0("X-", gsub("_", "-", nm)), population = "SAF", args = list()) {
  tp <- tfl_templates()
  a <- utils::modifyList(tflspec:::.tpl_examples[[nm]], args, keep.null = FALSE)
  if (nm %in% c("demog", "stf_t02_demog", "summary", "disposition")) a$population <- NULL
  if ("population" %in% names(tflspec:::.tpl_get(nm, "test")$params) && !is.null(a$population)) {
    a$population <- population
  }
  tfl_output(id, tp$kind[tp$name == nm], common = list(population = population), label = nm,
             template = list(name = nm, args = a))
}

ds_write <- function(d, ...) {
  for (o in list(...)) tfl_write_output(o, file.path(d, "spec", "outputs", paste0(o$id, ".yml")))
  tfl_read_study_spec(d)
}

# the program of a report: a table's ARD program, a listing's content code
ds_code <- function(d, st, id, kind = "table") {
  if (identical(kind, "listing")) {
    ro <- tfl_resolve_output(tfl_read_output(file.path(d, "spec", "outputs", paste0(id, ".yml"))), st)
    return(tfl_content_code(ro, study = st))
  }
  suppressMessages(tfl_ard_code(st, id))
}

# the lines from `# ---- data ----` to the analyses (or the listing's own code)
ds_section <- function(code) {
  code <- unlist(strsplit(code, "\n", fixed = TRUE))
  from <- match("# ---- data ----", code)
  stopifnot(!is.na(from))
  rest <- code[-seq_len(from)]
  stop_at <- grep("^# ---- |^lst <- ", rest)
  c(code[from], if (length(stop_at)) rest[seq_len(stop_at[1L] - 1L)] else rest)
}

ds_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("rtfreporter")
}

test_that("the data section of demog, ae_soc_pt, lab_shift and a listing", {
  ds_skip()
  d <- ds_study(withr::local_tempdir())
  st <- ds_write(d, ds_output("demog"), ds_output("ae_soc_pt"), ds_output("lab_shift"),
                 ds_output("listing"))
  expect_snapshot(cat(ds_section(ds_code(d, st, "X-demog")), sep = "\n"))
  expect_snapshot(cat(ds_section(ds_code(d, st, "X-ae-soc-pt")), sep = "\n"))
  expect_snapshot(cat(ds_section(ds_code(d, st, "X-lab-shift")), sep = "\n"))
  expect_snapshot(cat(ds_section(ds_code(d, st, "X-listing", "listing")), sep = "\n"))
})

test_that("no generated program has pop_ objects, intersect() or a semi_join(); local() only around SPEC code of several statements", {
  ds_skip()
  d <- ds_study(withr::local_tempdir())
  tp <- tfl_templates()
  nms <- c("demog", "stf_t02_demog", "summary", "disposition", "ae_soc_pt", "ae_pt_freq",
           "ae_max_grade", "ae_overview", "ae_eair", "lab_shift", "shift", "lab_abnormal",
           "param_change", "ancova", "orr", "listing")
  nms <- intersect(nms, tp$name)
  os <- lapply(nms, ds_output)
  st <- do.call(ds_write, c(list(d), os))
  for (i in seq_along(nms)) {
    code <- ds_code(d, st, os[[i]]$id, tp$kind[tp$name == nms[i]])
    # (the program's own part: the helper block holds set_levels()'s intersect())
    code <- unlist(strsplit(code, "\n", fixed = TRUE))
    code <- code[seq_along(code) >= match("# ---- data ----", code)]
    expect_false(any(grepl("pop_[a-z]", code)), info = nms[i])
    # (A1, the user 2026-11-24: SPEC code runs in its own step; one
    # expression is written as it is, so a local() holds several)
    locals <- list()
    find <- function(e) {
      if (!is.call(e)) return(invisible())
      if (identical(e[[1L]], as.name("local"))) locals[[length(locals) + 1L]] <<- e[[2L]]
      a <- as.list(e)[-1L]
      for (k in seq_along(a)) if (is.call(a[[k]])) find(a[[k]])
    }
    ex <- parse(text = code, keep.source = FALSE)
    for (k in seq_along(ex)) find(ex[[k]])
    for (b in locals) {
      expect_true(is.call(b) && identical(b[[1L]], as.name("{")) && length(b) > 2L, info = nms[i])
    }
    expect_false(any(grepl("intersect(", code, fixed = TRUE)), info = nms[i])
    expect_false(any(grepl("semi_join", code, fixed = TRUE)), info = nms[i])
    expect_equal(sum(code == "# ---- data ----"), 1L, info = nms[i])
  }
})

test_that("one template in two analysis sets prints the same object names", {
  ds_skip()
  d <- ds_study(withr::local_tempdir())
  nms <- c("demog", "ae_soc_pt", "lab_shift")
  a <- lapply(nms, ds_output, population = "SAF")
  b <- lapply(nms, function(nm) ds_output(nm, id = paste0("Y-", gsub("_", "-", nm)),
                                          population = "ITT"))
  st <- do.call(ds_write, c(list(d), a, b))
  objs <- function(id) {
    sec <- ds_section(ds_code(d, st, id))
    sub(" <-.*$", "", grep("^[A-Za-z_.0-9]+ <- ", sec, value = TRUE))
  }
  for (i in seq_along(nms)) expect_identical(objs(a[[i]]$id), objs(b[[i]]$id), info = nms[i])
})
