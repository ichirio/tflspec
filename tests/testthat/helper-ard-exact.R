# The ARD spec checked against hand-written cards / cardx calls: one
# analysis row (fixtures/ard-cases.R) run by tfl_build_ard(), the same
# analysis written by hand on the same data, and the two ARDs compared
# number by number.  Used by test-ard-exact.R and
# data-raw/brushup/ard_coverage.R.

exact_data <- function() {
  adsl <- as.data.frame(cards::ADSL)
  adsl$TRTA <- adsl$TRT01A
  list(ADSL = adsl, ADAE = as.data.frame(cards::ADAE),
       ADTTE = as.data.frame(cards::ADTTE), ADLB = as.data.frame(cards::ADLB))
}

exact_dir <- function(adam) {
  dir <- tempfile("exact")
  dir.create(file.path(dir, "adam"), recursive = TRUE)
  for (nm in names(adam)) {
    saveRDS(adam[[nm]], file.path(dir, "adam", paste0(nm, ".rds")))
  }
  dir
}

exact_sheet <- function(rows, cols) {
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) as.character(r[[cn]] %||% NA), "")),
    stringsAsFactors = FALSE)
}

exact_spec <- function(r, pop_derive = NULL, ds_derive = NULL,
                       source = NULL) {
  S <- .ard_spec_sheets
  r$output_id <- r$output_id %||% "T"
  r$analysis_id <- r$analysis_id %||% "A"
  tfl_ard_spec(list(
    study = exact_sheet(c(list(list(key = "id", value = "USUBJID")),
                          if (!is.null(source))
                            list(list(key = "source",
                                      value = paste0("R/", source)))),
                        S$study),
    datasets = exact_sheet(lapply(c("ADSL", "ADAE", "ADTTE", "ADLB"),
                                  function(d) list(dataset = d,
                                                   path = paste0("adam/", d, ".rds"),
                                                   derive = if (identical(d, r$dataset)) ds_derive)),
                           S$datasets),
    populations = exact_sheet(list(list(
      population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"",
      derive = pop_derive)), S$populations),
    analyses = exact_sheet(list(r), S$analyses)))
}

# the analysis data as the generated program makes it (dplyr: a column keeps
# its label)
exact_inputs <- function(r, adam, pop_derive = NULL, ds_derive = NULL) {
  pop <- dplyr::filter(adam$ADSL, SAFFL == "Y")
  if (!is.null(pop_derive)) {
    pop <- eval(str2lang(sprintf("dplyr::mutate(pop, %s)", pop_derive)))
  }
  ds <- r$dataset %||% "ADSL"
  src <- adam[[ds]]
  if (!is.null(ds_derive) && ds != "ADSL") {
    src <- eval(str2lang(sprintf("dplyr::mutate(src, %s)",
                                 gsub("|", ",", ds_derive, fixed = TRUE))))
  }
  d <- if (ds == "ADSL") pop else dplyr::filter(src, USUBJID %in% pop$USUBJID)
  if (!is.null(r$where)) d <- dplyr::filter(d, !!str2lang(r$where))
  list(data = d, population = pop)
}

# every value of an ARD by its keys
exact_numbers <- function(a) {
  if (is.list(a) && !is.data.frame(a)) {
    a <- dplyr::bind_rows(a, .id = "pairwise")
  }
  a <- as.data.frame(a)
  if (!nrow(a) || is.null(a$stat)) return(character())
  g <- sort(grep("^group[0-9]+$", names(a), value = TRUE))
  one <- function(v) vapply(v, function(z) {
    z <- unlist(z)
    if (!length(z)) NA_character_ else paste(as.character(z), collapse = "|")
  }, "")
  gl <- if (length(g)) do.call(paste, c(lapply(g, function(k)
    paste0(one(a[[k]]), "=", one(a[[paste0(k, "_level")]]))), sep = ";")) else ""
  key <- paste(gl, one(a$variable), one(a$variable_level), a$stat_name,
               sep = "|")
  if (!is.null(a$pairwise)) key <- paste(a$pairwise, key, sep = "|")
  val <- vapply(a$stat, function(z) {
    z <- unlist(z)
    if (is.numeric(z)) paste(format(signif(z, 12)), collapse = "|") else
      paste(as.character(z), collapse = "|")
  }, "")
  stats::setNames(val, key)
}

# A package a cards / cardx function needs only when it runs (cardx's own
# Suggests: broom, car, emmeans ...): not installed here, the case cannot
# run -- it is skipped, not failed.
exact_missing_pkg <- function(msg) {
  grepl("package.* (is|are) required", msg)
}

# one case: list(same, n, error, skipped).  A case's own functions (`source`, a
# file of `fixtures`) go to the study folder's R/, where its spec loads
# them, and are what the hand-written call runs too.
exact_run <- function(cs, adam, dir,
                      fixtures = testthat::test_path("fixtures")) {
  out <- list(same = NA, n = NA_integer_, error = "", skipped = FALSE)
  r <- cs$row
  step <- function(what, expr) tryCatch(expr, error = function(e) {
    out$error <<- paste0(what, ": ", conditionMessage(e))
    NULL
  })
  hand_env <- globalenv()
  if (!is.null(cs$source)) {
    dir.create(file.path(dir, "R"), showWarnings = FALSE)
    file.copy(file.path(fixtures, cs$source), file.path(dir, "R"),
              overwrite = TRUE)
    hand_env <- new.env(parent = globalenv())
    sys.source(file.path(fixtures, cs$source), hand_env)
  }
  sp <- step("spec", exact_spec(r, cs$pop_derive, cs$ds_derive, cs$source))
  if (is.null(sp)) return(out)
  got <- step("build", suppressMessages(suppressWarnings(
    tfl_build_ard(sp, dir = dir, save = FALSE))))
  if (is.null(got)) {
    out$skipped <- exact_missing_pkg(out$error)
    return(out)
  }
  env <- list2env(exact_inputs(r, adam, cs$pop_derive, cs$ds_derive),
                  parent = hand_env)
  want <- step("hand", suppressMessages(suppressWarnings(
    eval(str2lang(cs$hand), env))))
  if (is.null(want)) return(out)
  a <- exact_numbers(got)
  b <- exact_numbers(want)
  if (!is.null(cs$keep)) b <- b[sub("^.*\\|", "", names(b)) %in% cs$keep]
  miss <- setdiff(names(b), names(a))
  diff <- names(b)[names(b) %in% names(a) & a[names(b)] != b]
  out$n <- length(b)
  out$same <- length(b) > 0L && !length(miss) && !length(diff)
  if (!isTRUE(out$same)) {
    out$error <- sprintf("missing %d, differ %d (e.g. %s)", length(miss),
                         length(diff), utils::head(c(miss, diff), 1L))
  }
  out
}

# the packages a case needs (its function's and the hand call's)
exact_needs <- function(cs) {
  p <- unique(c(if (grepl("::", cs$fun, fixed = TRUE))
                  sub("::.*$", "", cs$fun),
                regmatches(cs$hand, gregexpr("[A-Za-z][A-Za-z0-9.]*(?=::)",
                                             cs$hand, perl = TRUE))[[1L]]))
  p[grepl("^[A-Za-z]", p)]
}
