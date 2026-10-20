# The company layer of the template SPEC (_archive/template-preset-spec-
# design-2026-10-17.md, §8 and §8.1): a company's templates are an R
# package of its own.  It registers them when it is loaded (tfl_template(),
# usually a thin wrapper of a tflspec template with the company's
# defaults), exports the function its reports' programs call (the A use,
# which calls tfl_template_run()), and tests them (tfl_test_templates()).
# tfl_use_company_templates() writes such a package to start from.

#' Register a template
#'
#' A template's definition: its kind, the layout it is built on, its few
#' parameters ([tfl_param()]) and its body, a function of the parameters'
#' values and the report's context that returns the report's items.  A
#' company's package calls it when it is loaded (`.onLoad()`), and so does
#' a study's `spec/templates/<name>.R`.  The usual body is a tflspec
#' template's with the company's defaults ([tfl_template_params()] to
#' start from, [tfl_template_expand()] in the body); see
#' [tfl_use_company_templates()].
#'
#' A name already registered is replaced, with a message (a company's
#' `demog` over tflspec's: the package loaded later wins).  A parameter
#' that ends in the look -- the rows a page, column widths, the digits, the
#' blank rows, the style -- is warned about: the study's styles and
#' defaults say those, and a template parameter would beat them
#' ([tfl_test_templates()] finds them).
#'
#' @param name The template's name (`"acme_demog"`).
#' @param kind `"table"`, `"figure"` or `"listing"`.
#' @param layout The layout it is built on (`"summary"`, `"hierarchical"`,
#'   ...), or its own name for a layout of its own.
#' @param params Its parameters, a list of [tfl_param()].
#' @param body `function(a, ctx)`: `a` the parameters' values by name (the
#'   report's, else their defaults), `ctx` the report's `codelists` and the
#'   study's `population` (its analysis sets); returns `list(ard = , data =
#'   , content = )`, `content` a list of [tfl_item()].  A figure's body
#'   returns its design (`data` its steps, `content` its layers, `plot` its
#'   settings) and `call_steps`: how many of the data's last steps (the
#'   statistics, the fit) its call makes from the data the steps before
#'   them make.
#' @param label What it makes, in a few words; `NULL` none.
#' @param call The function a report's program calls for it (the A use);
#'   `NULL`: `tpl_<name>`.  A company's package exports it, calling
#'   [tfl_template_run()].
#' @param example Its parameters for a try (a named list): what
#'   [tfl_test_templates()] runs it with, beside the defaults; `NULL` none.
#' @return `name`, invisibly.
#' @examples
#' tfl_template("my_ages", kind = "table", layout = "summary",
#'   params = list(tfl_param("population", "SAF", doc = "the analysis set's id")),
#'   body = function(a, ctx) tfl_template_expand("summary",
#'     list(population = a$population, variables = "AGE"),
#'     codelists = ctx$codelists, population = ctx$population))
#' tfl_templates()[, c("name", "package")]
#' @export
tfl_template <- function(name, kind = c("table", "figure", "listing"), layout, params,
                         body, label = NULL, call = NULL, example = NULL) {
  fn <- "tfl_template()"
  if (!is.character(name) || length(name) != 1L || !grepl("^[A-Za-z][A-Za-z0-9_.]*$", name)) {
    .ard_stop(sprintf("%s: `name` is one name of letters, digits, '_' and '.'.", fn))
  }
  kind <- match.arg(kind)
  if (!is.character(layout) || length(layout) != 1L || !nzchar(layout)) {
    .ard_stop(sprintf("%s: `layout` is the layout's name (\"summary\").", fn))
  }
  if (!is.list(params) || !all(vapply(params, inherits, NA, "tfl_param"))) {
    .ard_stop(sprintf("%s: `params` is a list of tfl_param().", fn))
  }
  if (!is.function(body) || length(formals(body)) < 2L) {
    .ard_stop(sprintf("%s: `body` is function(a, ctx).", fn))
  }
  pkg <- environmentName(topenv(parent.frame()))
  if (identical(pkg, "R_GlobalEnv")) pkg <- "(session)"
  old <- get0(name, envir = .tpl_registry, inherits = FALSE)
  if (!is.null(old)) {
    message(sprintf("%s: %s's template %s replaces %s's.", fn, pkg, name, old$package %||% "tflspec"))
  }
  .tfl_template(name, kind, layout, params, body, label = label, call = call, package = pkg,
                example = example)
  look <- .tpl_look_params(.tpl_get(name, fn))
  look <- look[look$status == "look", , drop = FALSE]
  if (nrow(look)) {
    warning(sprintf(paste0(
      "%s: template %s takes the look as a parameter: %s.\n",
      "  The study's styles and defaults say the look; a template parameter beats them."),
      fn, name, paste(sprintf("`%s` (%s(%s))", look$param, look$fn, look$arg), collapse = ", ")),
      call. = FALSE)
  }
  invisible(name)
}

#' A template with other defaults
#'
#' A company's or a study's template is mostly its defaults: the variables
#' of its demographics table, the severity's levels, the records kept.
#' This registers one by them alone, no R body: `from`'s parameters, types
#' and body, with `defaults` in place of `from`'s.  It is looked up as
#' `from` is when it runs, so a fix to `from` reaches it.
#'
#' A report's YAML names it (`template: {name: demog_jp}`); its program
#' (the A use, [tfl_content_code()]) calls `from`'s function with the
#' variant's defaults written out, so the package that registers it
#' exports nothing.  A value comes from, first to last: the report's
#' arguments, the study standard (`study.yml`, `defaults.yml`: `tfl_param(study
#' = )`), the variant's `defaults`, `from`'s.
#'
#' Register it where it is used: a company's package's `.onLoad()`
#' ([tfl_use_company_templates()]), or a study's `spec/templates/*.R`.
#'
#' @param name The variant's name.  `from`'s own name replaces `from`
#'   (with a message).
#' @param from The template it is a variant of ([tfl_templates()]).
#' @param defaults Its parameters' defaults, by name: `list(variables =
#'   c("AGE", "SEX"))`.  Each is one of `from`'s parameters.
#' @param label Its label; `NULL`: `from`'s.
#' @return `name`, invisibly.
#' @examples
#' tfl_template_variant("demog_age_sex", from = "demog",
#'                      defaults = list(continuous = "AGE", categorical = "SEX"))
#' tfl_template_params("demog_age_sex")$categorical$default
#' @export
tfl_template_variant <- function(name, from, defaults = list(), label = NULL) {
  fn <- "tfl_template_variant()"
  if (!is.character(name) || length(name) != 1L || !grepl("^[A-Za-z][A-Za-z0-9_.]*$", name)) {
    .ard_stop(sprintf("%s: `name` is one name of letters, digits, '_' and '.'.", fn))
  }
  src <- .tpl_get(from, fn)
  if (!is.list(defaults) || (length(defaults) && is.null(names(defaults)))) {
    .ard_stop(sprintf("%s: `defaults` is a named list, list(variables = ...).", fn))
  }
  unknown <- setdiff(names(defaults), names(src$params))
  if (length(unknown)) {
    .ard_stop(sprintf("%s: template %s has no parameter %s; its parameters: %s.", fn, from,
                      paste(sQuote(unknown, FALSE), collapse = ", "),
                      paste(names(src$params), collapse = ", ")))
  }
  # where it is registered: a package, or a study's spec/templates/ file
  pkg <- getOption("tflspec.template_source") %||% environmentName(topenv(parent.frame()))
  if (identical(pkg, "R_GlobalEnv")) pkg <- "(session)"
  old <- get0(name, envir = .tpl_registry, inherits = FALSE)
  if (!is.null(old)) {
    message(sprintf("%s: %s's template %s replaces %s's.", fn, pkg, name,
                    old$package %||% "tflspec"))
  }
  # replacing `from` itself: the variant reads the template it replaces
  if (identical(name, from)) {
    keep <- paste0(".base.", from)
    assign(keep, old, envir = .tpl_registry)
    from <- keep
  }
  assign(name, list(name = name, from = from, defaults = defaults, label = label,
                    package = pkg), envir = .tpl_registry)
  invisible(name)
}

# A variant as a template: `from`'s, as it is now, with the variant's
# defaults (a default given makes a required parameter optional)
.tpl_variant <- function(v) {
  src <- .tpl_get(v$from, "tfl_template_variant()")
  params <- src$params
  for (k in names(v$defaults)) {
    params[[k]]$default <- v$defaults[[k]]
    if (!is.null(v$defaults[[k]])) params[[k]]$required <- FALSE
  }
  root <- src$root %||% src$name
  list(name = v$name, kind = src$kind, layout = src$layout, params = params,
       body = src$body, label = v$label %||% src$label, call = src$call,
       package = v$package, example = src$example %||% .tpl_examples[[root]],
       from = sub("^[.]base[.]", "", v$from), root = root,
       # every default it changes, the ones of a variant it is made from too
       defaults = utils::modifyList(src$defaults %||% list(), v$defaults, keep.null = TRUE))
}

#' A template's parameters
#'
#' The parameters of a registered template, to start a template of one's
#' own from (a company's `demog` with its own default cells).
#'
#' @param name The template's name.
#' @return A named list of [tfl_param()]; change one's `default` and pass
#'   them to [tfl_template()].
#' @examples
#' p <- tfl_template_params("demog")
#' names(p)
#' p$cells$default
#' @export
tfl_template_params <- function(name) {
  .tpl_get(name, "tfl_template_params()")$params
}

#' Run a template: its table plan
#'
#' The A use of any registered template: its items run on the data, as a
#' report's content.  tflspec's `tpl_<name>()` and a company's exported
#' functions call this.  A study's defaults are not in it; the program adds
#' them after the call.
#'
#' @param name The template's name.
#' @param x The data its items run on: the ARD (a table), the dataset (a
#'   listing).
#' @param ... Its parameters, by name.
#' @param codelists The report's code lists: as [tfl_read_output()] reads
#'   them, or the values in order, `list(TRT01A = c("Drug X", "Control"))`.
#' @param sets The study's analysis sets (`data.yml`'s populations), for
#'   their labels; `NULL` none.
#' @return What the template's items make: a [rtfreporter::table_plan()]
#'   for a table or a listing.
#' @examples
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   saf <- adsl[adsl$SAFFL == "Y", ]
#'   ard <- cards::bind_ard(cards::ard_tabulate(saf, variables = TRT01A),
#'                          cards::ard_summary(saf, by = TRT01A, variables = AGE))
#'   plan <- tfl_template_run("summary", ard, population = "SAF", variables = "AGE")
#' }
#' @export
tfl_template_run <- function(name, x, ..., codelists = NULL, sets = NULL) {
  .tpl_object(name, x, list(...), codelists = codelists, population = sets)
}

# ---- the look ------------------------------------------------------------------

# The look: the verbs (and of a verb, its arguments) a template parameter
# should not end in -- the study's styles and defaults say them.  NULL: any
# of the verb's arguments.  (The stub's `vars` and `name` are the table's
# keys and the column the header names: its shape.  A listing's column
# widths are its columns' definition, decided 10/18: plan_listing()'s
# `cols` is not the look.)
.tpl_look <- list(
  plan_style = NULL, plan_blanks = NULL, plan_columns = NULL, plan_digits = NULL,
  plan_cell_style = NULL, plan_stub = c("indent", "before", "group_summary"),
  plan_paginate_rows = "max_rows", plan_listing = "type")

# The arguments of a look verb that say the shape: an item with one of them
# is the shape, over the study's layers.  plan_stub(vars =) names the stub's
# columns -- the study's plan_stub(name, before) must not take them away.
.tpl_shape_keys <- list(plan_stub = "vars")

# An item of the look: a look verb with none of its shape keys
.tpl_is_look <- function(it) {
  it$fn %in% names(.tpl_look) && !any(names(it$args) %in% .tpl_shape_keys[[it$fn]])
}

# A value of the same shape, changed: each string with a mark after it (its
# names kept), each number one more, each logical turned
.tpl_perturb <- function(x) {
  if (inherits(x, "tfl_r")) return(tfl_r(paste0(x, " ")))
  if (inherits(x, "tfl_unset")) return(x)
  if (is.character(x)) {
    y <- paste0(x, "~")
    attributes(y) <- attributes(x)
    return(y)
  }
  if (is.integer(x)) return(x + 1L)
  if (is.numeric(x)) return(x + 1)
  if (is.logical(x)) return(!x)
  if (is.list(x)) {
    x[] <- lapply(x, .tpl_perturb)
    return(x)
  }
  x
}

# Where each of a template's parameters ends: its body run with the
# template's example (its parameters' defaults, `example =` of
# tfl_template(), tflspec's own examples), and again with one parameter
# changed (the same shape, another value); a look verb's argument that
# changed with it is where it ends.  One row a parameter that ends in the
# look (`status` "look"), or a template whose run stopped ("not checked:
# <why>").  A parameter whose value is NULL is not changed.
.tpl_look_params <- function(t) {
  rows <- list()
  row <- function(p, fn, arg, status) rows[[length(rows) + 1L]] <<- data.frame(
    template = t$name, param = p, fn = fn, arg = arg, status = status, stringsAsFactors = FALSE)
  ctx <- list(codelists = NULL, population = NULL)
  run <- function(a) tryCatch(suppressMessages(suppressWarnings(t$body(a, ctx))),
                              error = function(e) e)
  ex <- t$example %||% .tpl_examples[[t$name]] %||% list()
  base <- tryCatch(.tpl_args(t, ex, "tfl_test_templates()"), error = function(e) e)
  x0 <- if (inherits(base, "error")) base else run(base)
  # a parameter with no type of its own: its input field is a guess
  for (p in t$params) if (isTRUE(p$guessed)) {
    row(p$name, NA_character_, NA_character_, sprintf("no type (guessed: %s)", p$type))
  }
  # the analysis set or the treatment, not read from the study standard
  for (p in t$params) if (.tpl_study_kind(p) && is.null(p$study)) {
    row(p$name, NA_character_, NA_character_, "no study source (population / treatment)")
  }
  if (inherits(x0, "error")) {
    row(NA_character_, NA_character_, NA_character_, paste("not checked:", conditionMessage(x0)))
  } else {
    look <- function(items) Filter(function(it) it$fn %in% names(.tpl_look), items %||% list())
    l0 <- look(x0$content)
    for (p in names(t$params)) {
      if (is.null(base[[p]])) next
      a <- base
      a[p] <- list(.tpl_perturb(base[[p]]))
      x1 <- run(a)
      if (inherits(x1, "error")) next
      l1 <- look(x1$content)
      seen <- list()
      for (it in l1) {
        k <- (seen[[it$fn]] %||% 0L) + 1L
        seen[[it$fn]] <- k
        same <- Filter(function(b) identical(b$fn, it$fn), l0)
        b <- if (length(same) >= k) same[[k]]$args else list()
        keys <- union(names(it$args), names(b))
        if (!is.null(.tpl_look[[it$fn]])) keys <- intersect(keys, .tpl_look[[it$fn]])
        for (key in keys) if (!identical(it$args[[key]], b[[key]])) row(p, it$fn, key, "look")
      }
    }
  }
  if (!length(rows)) {
    return(data.frame(template = character(), param = character(), fn = character(),
                      arg = character(), status = character(), stringsAsFactors = FALSE))
  }
  unique(do.call(rbind, rows))
}

# A parameter that is the analysis set or the treatment: by its name, or a
# treatment variable as its default
.tpl_study_kind <- function(p) {
  p$name %in% c(names(.tpl_param_study), "pop", "arm", "trt", "treatment") ||
    any(as.character(unlist(p$default)) %in% c("TRT01A", "TRT01P", "TRTA", "TRTP", "ARM",
                                               "ACTARM"))
}

#' Test templates: no parameter is the look, each has its type
#'
#' Runs each template's body with its example, and again with one
#' parameter at a time changed, and looks which items change with it: a
#' parameter that changes the look (the rows a page, column widths, the
#' digits, the blank rows, the style, a listing's type) is listed.  So is
#' a parameter with no type ([tfl_param()]'s `type`, nor tflspec's for its
#' name): an input form guesses its field from the default; and the analysis
#' set or the treatment not read from the study standard (no `study =`).
#' A study's value of them is the study's, not each report's.
#' tflspec's own templates have none; a company's package tests its own
#' with it (one line in its tests, [tfl_use_company_templates()]).
#'
#' @param names The templates; `NULL`: every one registered.
#' @return A data frame, one row a parameter that ends in the look
#'   (`status` `"look"`: `param`, and the verb `fn` and argument `arg` it
#'   ends in), a parameter with no type (`"no type (guessed: <type>)"`),
#'   one with no study source (`"no study source (population / treatment)"`)
#'   or a template whose run stopped (`"not checked: ..."`):
#'   `template`, `param`, `fn`, `arg`, `status`.  No rows: none.
#' @examples
#' tfl_test_templates("demog")
#' @export
tfl_test_templates <- function(names = NULL) {
  nm <- names %||% sort(ls(.tpl_registry))
  out <- lapply(nm, function(n) .tpl_look_params(.tpl_get(n, "tfl_test_templates()")))
  do.call(rbind, out)
}

# ---- a company's package to start from ---------------------------------------

# An example of each tflspec template's parameters: what a company's
# package tests its wrapped templates with
.tpl_examples <- list(
  demog = list(population = "SAF"),
  summary = list(population = "SAF", variables = "AGE"),
  disposition = list(population = "SAF"),
  hierarchical = list(population = "SAF", hierarchy = c("AEBODSYS", "AEDECOD")),
  ae_soc_pt = list(population = "SAF"),
  ae_pt_freq = list(population = "SAF", min_pct = 5),
  ae_max_grade = list(population = "SAF", grade_levels = c("MILD", "MODERATE", "SEVERE")),
  ae_overview = list(population = "SAF"),
  shift = list(population = "SAF", data_id = "adlb_shift", by_levels = c("Drug X", "Control")),
  lab_shift = list(population = "SAF", visit = "CYCLE 2 DAY 1", params = c("ALT", "AST")),
  listing = list(data = "ADAE", columns = list(list(vars = "USUBJID", label = "Subject"))),
  km_table = list(),
  km_simple = list(param = "PFS", group = "TRT01P"),
  km_risk_table = list(param = "PFS", group = "TRT01P"),
  km_ci = list(param = "PFS", group = "TRT01P"),
  mean_se = list(param = "SYSBP", data = "ADVS"),
  mean_ci = list(param = "SYSBP", data = "ADVS"),
  waterfall = list(param = "SUMDIAM", where = "AVISIT == \"WEEK 12\""),
  orr = list(),
  param_change = list(population = "SAF", data = "ADLB", params = c("ALT", "AST"),
                      where = "ANL01FL == \"Y\""),
  ae_eair = list(population = "SAF"),
  ancova = list(population = "SAF", data = "ADVS", param = "SYSBP", visit = "CYCLE 2 DAY 1"),
  lab_abnormal = list(population = "SAF", levels = c("LOW", "HIGH"), where = "ONTRTFL == \"Y\""),
  vs_change = list(population = "SAF", param = "SYSBP", visit = "WEEK 24",
                   param_label = "Systolic blood pressure", when = "week 24",
                   visit_label = "Week 24", unit = "mmHg"))

#' Write a company's template package to start from
#'
#' A company's templates live in an R package of its own (decided
#' 2026-10-17).  This writes one to start from, every file meant to be
#' read and changed: for each template chosen, one
#' [tfl_template_variant()] call -- tflspec's template with the company's
#' defaults, so tflspec's fixes reach it -- registered when the package is
#' loaded.  It exports nothing: a report's program calls tflspec's
#' `tpl_<template>()` with the company's defaults written out.
#'
#' The files: `DESCRIPTION`, `NAMESPACE`, `LICENSE`, `R/templates.R` (the
#' templates), `R/zzz.R` (their registration), `tests/` (that each is
#' registered, takes no look as a parameter and expands),
#' `inst/standards/defaults.yml` (the company standard a new study's
#' standard is copied from, the form of a study's `spec/defaults.yml`) and
#' `README.md`.
#'
#' @param path The package's folder (made; it must be empty or new).
#' @param package The package's name; the folder's name by default.
#' @param templates tflspec templates to wrap ([tfl_templates()]).
#' @param prefix The wrapped templates' names are `<prefix>_<template>`
#'   (`acme_demog`); `NULL`: `package`.  `""`: the same names, replacing
#'   tflspec's when the package is loaded.
#' @return The files written, invisibly.
#' @examples
#' d <- file.path(tempdir(), "acmetpl")
#' f <- tfl_use_company_templates(d, templates = c("demog", "ae_soc_pt"))
#' basename(f)
#' unlink(d, recursive = TRUE)
#' @export
tfl_use_company_templates <- function(path, package = basename(path),
                                      templates = c("demog", "ae_soc_pt"), prefix = NULL) {
  fn <- "tfl_use_company_templates()"
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$", package)) {
    .ard_stop(sprintf("%s: %s is not a package name (letters, digits and '.').", fn, sQuote(package, FALSE)))
  }
  for (t in templates) .tpl_get(t, fn)
  if (dir.exists(path) && length(list.files(path, all.files = TRUE, no.. = TRUE))) {
    .ard_stop(sprintf("%s: %s is not empty.", fn, path))
  }
  prefix <- prefix %||% package
  nm <- if (nzchar(prefix)) paste0(gsub("[.]", "_", prefix), "_", templates) else templates
  dir.create(file.path(path, "R"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "tests", "testthat"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "inst", "standards"), recursive = TRUE, showWarnings = FALSE)
  files <- character()
  put <- function(rel, lines) {
    f <- file.path(path, rel)
    writeLines(enc2utf8(lines), f, useBytes = TRUE)
    files <<- c(files, f)
  }
  ver <- as.character(utils::packageVersion("tflspec"))
  put("DESCRIPTION", c(
    paste("Package:", package),
    paste0("Title: ", package, "'s Report Templates"),
    "Version: 0.0.0.9000",
    "Authors@R: person(\"First\", \"Last\", email = \"first.last@example.com\", role = c(\"aut\", \"cre\"))",
    paste0("Description: The report templates of ", package, ", on 'tflspec''s: each a"),
    "    template of 'tflspec' with the company's defaults, registered when the",
    "    package is loaded.  A report's YAML names one (template: {name, args});",
    "    its program calls the 'tflspec' template's function with them.",
    "License: file LICENSE",
    "Encoding: UTF-8",
    "Depends: R (>= 4.1.0)",
    paste0("Imports: tflspec (>= ", ver, ")"),
    "Suggests: testthat (>= 3.0.0)",
    "Config/testthat/edition: 3"))
  put("LICENSE", "Proprietary: for the company's own use.")
  put("NAMESPACE", c("# nothing exported: a report's program calls tflspec's tpl_*() with",
                     "# the company's defaults written out"))
  tpl <- lapply(templates, .tpl_get, fn = fn)
  ex <- vapply(templates, function(t) {
    e <- .tpl_examples[[t]] %||% list()
    paste(deparse(e, width.cutoff = 70L), collapse = " ")
  }, "")
  body <- c(
    paste0("# ", package, "'s report templates: tflspec's, with ", package, "'s defaults."),
    "#",
    "# Each is a variant of a tflspec template (tflspec::tfl_template_variant()):",
    "# its parameters, types and body, with the company's defaults -- so",
    "# tflspec's fixes reach it.  Registered when the package is loaded",
    "# (R/zzz.R).  A report's YAML names it:",
    "#   template: {name: <name>, args: {population: SAF}}",
    "# and its program calls tflspec's function with these defaults written out.",
    "# The look (widths, the rows a page, digits) is no parameter:",
    "# inst/standards/defaults.yml says it.",
    "",
    "register_templates <- function() {")
  for (i in seq_along(templates)) {
    t <- tpl[[i]]
    # a default to change, as an example: the continuous rows when it has them
    hint <- if ("cells" %in% names(t$params)) {
      "      # cells = list(n = \"{N:.0f}\", `Mean (SD)` = \"{mean:.1f} ({sd:.1f})\")"
    } else {
      sprintf("      # %s = ...", setdiff(names(t$params), "population")[1L])
    }
    body <- c(body,
      sprintf("  # %s: tflspec's %s (the %s layout; tflspec::tfl_template_params(\"%s\"))",
              nm[i], t$name, t$layout, t$name),
      sprintf("  tflspec::tfl_template_variant(\"%s\", from = \"%s\",", nm[i], t$name),
      "    defaults = list(",
      "      # the company's defaults, e.g.:",
      hint,
      "    ))")
  }
  body <- c(body, "  invisible()", "}")
  put(file.path("R", "templates.R"), body)
  put(file.path("R", "zzz.R"), c(
    "# the templates, registered when the package is loaded (a name of",
    "# tflspec's is replaced, with a message)",
    ".onLoad <- function(libname, pkgname) {",
    "  register_templates()",
    "}"))
  put(file.path("tests", "testthat.R"), c(
    "library(testthat)", paste0("library(", package, ")"), "", paste0("test_check(\"", package, "\")")))
  put(file.path("tests", "testthat", "test-templates.R"), c(
    "test_that(\"the templates are registered, and none takes the look as a parameter\", {",
    sprintf("  tpl <- c(%s)", paste(sprintf("\"%s\"", nm), collapse = ", ")),
    "  expect_true(all(tpl %in% tflspec::tfl_templates()$name))",
    "  look <- tflspec::tfl_test_templates(tpl)",
    "  expect_identical(look[look$status == \"look\", \"param\"], character(0))",
    "})",
    "",
    "test_that(\"each template expands into a report's items\", {",
    unlist(lapply(seq_along(nm), function(i) c(
      sprintf("  x <- tflspec::tfl_template_expand(\"%s\", %s)", nm[i], ex[[i]]),
      "  expect_gt(length(x$content), 0L)"))),
    "})"))
  put(file.path("inst", "standards", "defaults.yml"), c(
    paste0("# ", package, "'s company standard: what a new study's standard is copied"),
    "# from, in the form of a study's spec/defaults.yml (the look of every table: the digits, the",
    "# header, the label column, the blank rows, the widths, the style).",
    "content:",
    "  - normalize_ard: {}",
    "  - plan_cells: {default: \"{n:.0f} ({p:.1f%})\"}",
    "  - plan_digits: {rounding: sas}",
    "  - plan_stub: {name: row_label, before: true}",
    "  - plan_blanks: {where: between_groups, first: true, last: true}"))
  put("README.md", c(
    paste0("# ", package),
    "",
    paste0(package, "'s report templates, on [tflspec](https://ichirio.github.io/tflspec/)'s."),
    "",
    "| Template | A variant of tflspec's | Kind |",
    "|---|---|---|",
    sprintf("| `%s` | `%s` | %s |", nm, templates, vapply(tpl, `[[`, "", "kind")),
    "",
    "- `R/templates.R`: the templates. Change a default in its `defaults = list()`;",
    "  the look (widths, the rows a page, digits) is not a parameter -- it is",
    "  `inst/standards/defaults.yml`, the defaults a new study starts from.",
    "- A report's YAML names a template: `template: {name: <name>, args: {...}}`;",
    paste0("  its program calls tflspec's function with the defaults written out; the"),
    paste0("  templates are registered by `library(", package, ")`."),
    paste0("- `devtools::test()`: each template is registered, takes no look as a"),
    "  parameter (`tflspec::tfl_test_templates()`) and expands.",
    "- Written by `tflspec::tfl_use_company_templates()`."))
  invisible(files)
}
