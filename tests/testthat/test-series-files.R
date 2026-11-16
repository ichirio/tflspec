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
})

test_that("every template has its keys, a BIGN item and ids on its items", {
  skip_if(!nzchar(series_dir()), "the series folder is not installed")
  ts <- series_templates()
  expect_length(ts, 7L)
  for (nm in names(ts)) {
    t <- ts[[nm]]
    for (k in c("template", "label", "params", "data", "analysis")) {
      expect_false(is.null(t[[k]]), info = paste(nm, k))
    }
    ids <- vapply(t$analysis, function(a) {
      if (is.null(a$id)) NA_character_ else a$id
    }, character(1))
    expect_false(anyNA(ids), info = nm)
    expect_true("BIGN" %in% ids, info = nm)
  }
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

test_that("every \"{x}\" in data and analysis names a param or a condition column", {
  skip_if(!nzchar(series_dir()), "the series folder is not installed")
  known <- c("conditions", "where", "subgroup", "subgroup_lvl", "tox_dir",
             "graded_params", "worst", "base")
  for (nm in names(series_templates())) {
    t <- series_templates()[[nm]]
    ok <- c(names(t$params), known)
    expect_true(all(series_spots(t$data) %in% ok), info = paste(nm, "data"))
    for (a in t$analysis) {
      ok_a <- c(ok, unlist(a$params))
      bad <- setdiff(series_spots(a), ok_a)
      expect_true(length(bad) == 0L,
                  info = paste(nm, a$id, paste(bad, collapse = " ")))
    }
  }
})
