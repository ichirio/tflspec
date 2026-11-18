# The shipped series folder inst/series/tflspec_v1: every file parses, and
# every template has the keys, params and "{name}" spots a loader needs

series_dir <- function() {
  system.file("series", "tflspec_v1", package = "tflspec")
}

series_tag_handlers <- function() {
  id <- function(x) x
  list(series = id, study = id, fixed = id, unset = id, r = id)
}

series_read <- function(f) {
  yaml::read_yaml(f, handlers = series_tag_handlers())
}

series_templates <- function() {
  fs <- list.files(file.path(series_dir(), "templates"), "[.]yml$",
                   full.names = TRUE)
  stats::setNames(lapply(fs, series_read), basename(fs))
}

# Every "{x}" name in a value: the name before a ":" or the closing brace
series_spots <- function(x) {
  s <- unlist(x, use.names = FALSE)
  s <- as.character(s[vapply(s, is.character, logical(1))])
  m <- regmatches(s, gregexpr("\\{[A-Za-z_][A-Za-z0-9_.]*(:[^}]*)?\\}", s))
  m <- unlist(m, use.names = FALSE)
  unique(sub("^\\{([A-Za-z_][A-Za-z0-9_.]*).*$", "\\1", m))
}

test_that("every file in the tflspec_v1 series folder parses", {
  d <- series_dir()
  skip_if(!nzchar(d), "the series folder is not installed")
  fs <- list.files(d, "[.]yml$", recursive = TRUE, full.names = TRUE)
  expect_gte(length(fs), 8L)
  for (f in fs) {
    expect_no_error(series_read(f))
  }
  s <- series_read(file.path(d, "series.yml"))
  expect_identical(s$name, "tflspec_v1")
  expect_true(is.list(s$values))
  expect_null(s$values$header_n)
  expect_null(s$values$overall_row)
})

test_that("every template has its keys, a BIGN item and ids on its items", {
  skip_if(!nzchar(series_dir()), "the series folder is not installed")
  ts <- series_templates()
  expect_length(ts, 12L)
  # (a listing counts nothing: tflspec_listing has no analysis step;
  # tflspec_listing_summary's column headings have no N: no BIGN)
  no_analysis <- "tflspec_listing.yml"
  no_bign <- "tflspec_listing_summary.yml"
  for (nm in names(ts)) {
    t <- ts[[nm]]
    keys <- c("template", "label", "params", "data", if (!nm %in% no_analysis) "analysis")
    for (k in keys) {
      expect_false(is.null(t[[k]]), info = paste(nm, k))
    }
    if (nm %in% no_analysis) {
      expect_null(t$analysis, info = nm)
      next
    }
    ids <- vapply(t$analysis, function(a) {
      if (is.null(a$id)) NA_character_ else a$id
    }, character(1))
    expect_false(anyNA(ids), info = nm)
    expect_identical("BIGN" %in% ids, !nm %in% no_bign, info = nm)
  }
})

test_that("the percentages' denominator is BIGN and the items say how analysis ids are made", {
  skip_if(!nzchar(series_dir()), "the series folder is not installed")
  ts <- series_templates()
  for (nm in c("demog", "ae_soc_pt", "lab_tox_shift", "tflspec_shift")) {
    a <- ts[[paste0(nm, ".yml")]]$analysis
    ids <- vapply(a, function(x) x$id, character(1))
    den <- unlist(lapply(a, function(x) x$denominator))
    expect_identical(unique(den), "BIGN", info = nm)
    expect_identical(sum(ids == "BIGN"), 1L, info = nm)
  }
  lab <- ts[["lab_tox_shift.yml"]]
  expect_identical(lab$params$conditions$type,
                   "rows<tox_dir,param>")
  expect_null(lab$params$graded_params)
  shift <- lab$analysis[[2]]
  for (f in c("orr_subgroup.yml", "forest_hr.yml")) {
    t <- ts[[f]]
    expect_null(t$params$overall_row, info = f)
    expect_identical(t$params$subgroup_vars$type, "list<column>", info = f)
  }
  expect_identical(shift$analyses$per, c("PARAMCD", "tox_dir"))
  tsh <- ts[["tflspec_shift.yml"]]
  expect_true(isTRUE(tsh$params$by$required))
  expect_identical(tsh$params$from$default, "ADLB")
  expect_null(tsh$params$count_cell$default)
  expect_identical(tsh$analysis[[2]]$analyses$per, "{param_var}")
  expect_null(shift$analysis_id)
  ts_ae <- ts[["ae_soc_pt.yml"]]$analysis[[2]]
  expect_identical(ts_ae$analyses$per, "level")
  expect_null(ts[["orr_subgroup.yml"]]$analysis[[2]]$analyses)
  ls <- ts[["listing.yml"]]$params
  expect_null(ls$from$required)
  expect_null(ls$columns$required)
  expect_identical(ls$columns$tier, "other")
  tl <- ts[["tflspec_listing.yml"]]
  expect_true(isTRUE(tl$params$from$required))
  expect_true(isTRUE(tl$params$columns$required))
  expect_identical(tl$params$columns$type, "rows<vars,label,width,collapse_repeats>")
  expect_identical(tl$params$by_break$default, "page")
  expect_identical(tl$style, "style_listing")
  cmp <- ts[["forest_hr.yml"]]$params$comparison
  expect_identical(cmp$type, "string")
  expect_identical(cmp$input, "pick")
  expect_identical(cmp$step, "analysis")
  expect_identical(cmp$tier, "usual")
  expect_identical(cmp$description, "The arm compared against the reference")
  expect_match(ts[["km_simple.yml"]]$data[[2]]$derive,
               "[[\"{time_unit}\"]]", fixed = TRUE)
  expect_match(ts[["km_simple.yml"]]$data[[2]]$derive, "months = {days_per_month}", fixed = TRUE)
  expect_equal(ts[["ae_soc_pt.yml"]]$params$min_pct$default, 0)
  expect_identical(ts[["orr_subgroup.yml"]]$data[[1]]$population,
                   "population_efficacy")
  expect_null(ts[["demog.yml"]]$params$continuous$required)
  expect_null(ts[["demog.yml"]]$params$categorical$required)
  dm <- ts[["demog.yml"]]$analysis
  expect_identical(dm[[2]]$analyses$per, "variable")
  expect_null(dm[[2]]$analysis_ids)
})

test_that("every param is an argument definition with closed words", {
  skip_if(!nzchar(series_dir()), "the series folder is not installed")
  inputs <- c("pick", "text", "condition", "table")
  steps <- c("data", "analysis", "content", "page")
  sources <- c("vars", "levels", "codelist", "populations", "report_data",
               "study_data", "ard", "columns", "study_info", "tokens",
               "stats", "fixed", "styles", "from_args")
  for (nm in names(series_templates())) {
    ps <- series_templates()[[nm]]$params
    expect_true(length(ps) > 0L && !is.null(names(ps)), info = nm)
    for (p in names(ps)) {
      def <- ps[[p]]
      where <- paste(nm, p)
      expect_true(is.list(def), info = where)
      expect_true(all(c("type", "input", "step") %in% names(def)),
                  info = where)
      expect_true(def$input %in% inputs, info = where)
      expect_true(def$step %in% steps, info = where)
      expect_true(is.character(def$description) && nzchar(def$description),
                  info = where)
      if (!is.null(def$choices)) {
        words <- trimws(strsplit(def$choices, ",\\s*(?![^()]*\\))",
                                 perl = TRUE)[[1]])
        words <- sub("\\(.*$", "", words)
        expect_true(all(words %in% sources), info = where)
      }
    }
  }
})

test_that("every \"{x}\" in a derive: entry names a param or an earlier derived name", {
  skip_if(!nzchar(series_dir()), "the series folder is not installed")
  for (nm in names(series_templates())) {
    t <- series_templates()[[nm]]
    known <- names(t$params)
    for (k in names(t$derive)) {
      # (an each:'s `to` reads its rows' columns and {item}; "{{" is a brace)
      e <- t$derive[[k]]
      if (!is.null(e$each)) e$to <- NULL
      e <- rapply(e, function(s) gsub("\\{\\{[^{}]*\\}\\}", "", s), classes = "character",
                  how = "replace")
      bad <- setdiff(series_spots(e), known)
      expect_true(length(bad) == 0L, info = paste(nm, k, paste(bad, collapse = " ")))
      expect_false(k %in% names(t$params), info = paste(nm, k))
      known <- c(known, k)
    }
  }
})

test_that("every \"{x}\" in data and analysis names a param or a condition column", {
  skip_if(!nzchar(series_dir()), "the series folder is not installed")
  known <- c("conditions", "where", "subgroup", "subgroup_lvl", "tox_dir",
             "graded_params", "worst", "base")
  for (nm in names(series_templates())) {
    t <- series_templates()[[nm]]
    ok <- c(names(t$params), names(t$derive), known)
    expect_true(all(series_spots(t$data) %in% ok), info = paste(nm, "data"))
    for (a in t$analysis) {
      ok_a <- c(ok, unlist(a$params))
      bad <- setdiff(series_spots(a), ok_a)
      expect_true(length(bad) == 0L,
                  info = paste(nm, a$id, paste(bad, collapse = " ")))
    }
  }
})
