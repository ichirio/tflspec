# ---- a study's setup of its report programs, from its YAML spec ---------------
#
# tfl_report_setup_code(study) and tfl_report_tokens(output, study): what
# the sheets' versions write from the default rows of the page, tokens,
# header and footer sheets, written from the study's defaults
# (spec/defaults.yml, `page:`) and from a report of the YAML spec.  The
# lines are the ones a report's program refers to after the setup
# (tfl_report_code(output, setup = TRUE, study = ): header_<name> ...),
# made the same way (.page_band()).

#' The lines of the study's setup file that tflspec owns
#'
#' A study's setup file (tflplanner's `programs/study_setup.R`) is sourced by
#' every ARD and report program.  This gives the lines tflspec writes in it,
#' two parts:
#'
#' * the study's rounding as one `options()` call, `rtfreporter.rounding` and
#'   (for "sas" and "r") `cards.round_type`;
#' * the study's input data, in two versions, each read when first used
#'   (`delayedAssign()`): `study$adam_org`, an environment with one object per
#'   dataset of its data catalog (`spec/data.yml`, or the `datasets` sheet),
#'   ADSL first, the others in the catalog's order, as the file has it
#'   (`adsl`: `readRDS(file.path(path_adam, "adsl.rds"))`, with
#'   `|> dplyr::mutate(...)` for a dataset that has a `derive`); and
#'   `study$adam`, the same datasets with blanks as `NA` (`blank_as_na()`:
#'   a `""` of a character column or of a factor) and then the study's code
#'   lists' levels (`set_levels()`: each listed column a factor of its list's
#'   codes in its order; the values as read).  The study's code lists are those of
#'   `spec/defaults.yml` (`codelists:`), written first as `cl_<variable>`
#'   objects, as the ARD program writes a report's.  A study with no code
#'   list has no `cl_*` objects, and `study$adam` is `study$adam_org` with
#'   blanks as `NA` (a message says so when the file is sourced).  A spec
#'   of the sheets has no study-wide code list (a row of its `codelists`
#'   sheet is a report's), so it is written this way.  The reader follows
#'   the file's extension and the folder variables of option
#'   `tflspec.paths`, as in the ARD program.  The last lines make the
#'   objects the ARD, listing and figure programs read (`adsl`) the
#'   datasets of `study$adam`: the generated programs always read
#'   `study$adam`, and `study$adam_org` is there for a program a person
#'   writes on the data as the file has it.
#'
#' study.yml's `settings: adam_levels` says whether `study$adam` gets the
#' study's code lists' levels: `codelists` (the default, also when the key
#' is not there) as above, or `none`: no `cl_*` objects and `study$adam` is
#' `study$adam_org` with blanks as `NA` only.  `study$adam` always has its
#' blanks as `NA`, and `study$adam_org` is always the dataset as the file
#' has it.  The setup's first line says the setting (`# ADaM levels:
#' codelists`), and it is part of each ARD's fingerprint
#' ([tfl_ard_spec_hash()]), so changing it marks the ARD outdated.  Under
#' `none` a report's own levels and labels written as values still work; a
#' report that names a study code list (its object, `cl_sex`) stops, with
#' "adam_levels: none -- the study code list SEX (cl_sex) is not read".  A
#' spec of the sheets says it on its `study` sheet (key `adam_levels`).
#'
#' With option `tflspec.study_setup` set to `TRUE` ([tflspec_code_options])
#' the ARD, listing and figure programs do not write them again: they start
#' from the objects (`adsl <- adsl |> order_levels(...)`), and the ARD and
#' report setups leave the rounding out.
#'
#' @param study The study: what [tfl_read_study_spec()] gives, or a
#'   table / report spec of the sheets (its `study` sheet's `rounding`, its
#'   `datasets` sheet).
#' @return A character vector, one element per line; `character()` when the
#'   study says no rounding and has no dataset.  With a dataset, the first
#'   line says the study's `adam_levels`.
#' @seealso [tflspec_code_options]
#' @examples
#' st <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
#' tfl_study_setup_code(st)
#' @export
tfl_study_setup_code <- function(study) {
  rnd <- if (inherits(study, "tfl_study_spec")) .study_rounding(study) else {
    v <- if (is.list(study) && is.data.frame(study$study)) .ard_spec_study_value(study, "rounding") else
      stop("tfl_study_setup_code(): `study` must be a study spec (tfl_read_study_spec()) ",
           "or a table / report spec of the sheets.", call. = FALSE)
    if (is.na(v)) NULL else v
  }
  out <- character()
  if (!is.null(rnd)) {
    ct <- .cards_round_type(rnd)
    ro <- c(list("options", rtfreporter.rounding = rnd), if (!is.null(ct)) list(cards.round_type = ct))
    out <- c("# the study's rounding (every ARD and report program)",
             .spec_call_code(do.call(.spec_call, ro), 0L))
  }
  al <- .study_adam_levels(study)
  dl <- .study_data_lines(.study_catalog(study),
                          if (identical(al, "none")) list() else .study_codelists(study),
                          adam_levels = al)
  if (length(dl)) {
    # (the setting first: the setup's own record of it)
    out <- c(sprintf("# ADaM levels: %s", al), "", out, if (length(out)) "", dl)
  }
  out
}

# study.yml's `settings: adam_levels`: does study$adam get the study's code
# lists' levels -- "codelists" (the default; the key not there) or "none".
# A spec of the sheets: its study sheet's key `adam_levels`.
.adam_levels_values <- c("codelists", "none")
.study_adam_levels <- function(study) {
  v <- if (inherits(study, "tfl_study_spec")) study$study$settings$adam_levels else
    if (is.list(study) && is.data.frame(study$study)) .ard_spec_study_value(study, "adam_levels")
  .adam_levels_check(v, "study.yml: settings: adam_levels")
}

# A value of adam_levels, checked: NULL, NA or "" is the default "codelists"
.adam_levels_check <- function(v, where) {
  if (is.null(v) || !length(v) || is.na(v[1L]) || !nzchar(trimws(as.character(v[1L])))) {
    return("codelists")
  }
  v <- trimws(as.character(v[1L]))
  if (!v %in% .adam_levels_values) {
    stop(sprintf("%s is %s; got %s.", where,
                 paste(.adam_levels_values, collapse = " or "), encodeString(v, quote = "\"")),
         call. = FALSE)
  }
  v
}

# Under adam_levels: none the setup reads no study code list (no cl_*
# objects): a report that names one (cl_sex, in its code, derive, levels or
# labels) stops.  Its own lists (its `codelists:`) are its program's: they
# stay; so do levels and labels written as values.
.adam_levels_name_check <- function(output, study) {
  if (!inherits(study, "tfl_study_spec") || !identical(.study_adam_levels(study), "none")) {
    return(invisible(NULL))
  }
  lists <- names(.study_codelists(study))
  if (!length(lists)) return(invisible(NULL))
  objs <- stats::setNames(.cl_name(lists), lists)
  objs <- objs[!objs %in% .cl_name(names(output$codelists %||% list()))]
  parts <- unclass(output)[setdiff(names(output), c("codelists", "id", "title", "titles", "footnotes"))]
  txt <- tryCatch(as.character(unlist(parts, use.names = FALSE)), error = function(e) character())
  txt <- txt[!is.na(txt)]
  if (!length(objs) || !length(txt)) return(invisible(NULL))
  m <- unlist(regmatches(txt, gregexpr("(?<![A-Za-z0-9_.])cl_[a-z0-9_]+(?![A-Za-z0-9_.])", txt,
                                       perl = TRUE)))
  hit <- objs[objs %in% m]
  if (length(hit)) {
    stop(sprintf("%s: adam_levels: none -- the study code list %s (%s) is not read.",
                 output$id %||% "the report", names(hit)[1L], hit[[1L]]), call. = FALSE)
  }
  invisible(NULL)
}

# The study's code lists as the programs' lists, each variable's codes in
# their order, named by the code when a label is given (c(F = "Female")),
# else the codes alone: study.yml's / defaults.yml's `codelists:` (the
# study's own lists, those of no report).  The sheets have no study-wide
# code list (a row of `codelists` is a report's), so a spec of the sheets
# has none.  Empty list: none.
.study_codelists <- function(study) {
  if (inherits(study, "tfl_study_spec")) study$codelists %||% list() else list()
}

# The study's data catalog as a data frame (dataset, path, derive), ADSL
# first and the others as the catalog has them: from the YAML study's
# data.yml, or the `datasets` sheet of a spec of the sheets; none: no rows
.study_catalog <- function(study) {
  ds <- if (inherits(study, "tfl_study_spec")) {
    d <- lapply(study$data$datasets %||% list(), function(z)
      data.frame(dataset = as.character(z$dataset %||% NA_character_)[1L],
                 path = as.character(z$path %||% NA_character_)[1L],
                 derive = as.character(z$derive %||% NA_character_)[1L],
                 stringsAsFactors = FALSE))
    if (length(d)) do.call(rbind, d)
  } else if (is.data.frame(study$datasets)) {
    d <- study$datasets
    data.frame(dataset = as.character(d$dataset), path = as.character(d$path),
               derive = as.character(if (is.null(d$derive)) rep(NA_character_, nrow(d)) else d$derive),
               stringsAsFactors = FALSE)
  }
  if (is.null(ds) || !nrow(ds)) return(data.frame(dataset = character(), path = character(),
                                                 derive = character(), stringsAsFactors = FALSE))
  ds <- ds[!is.na(ds$dataset) & nzchar(ds$dataset) & !is.na(ds$path) & nzchar(ds$path), , drop = FALSE]
  ds[order(toupper(ds$dataset) != "ADSL"), , drop = FALSE]
}

# The study's input data, read when first used: two environments,
# study$adam_org (each dataset as the file has it, with the catalog's
# derive) and study$adam (blank_as_na() on it, then the study's code lists'
# levels; the values as they were read otherwise).  The study's code lists come first (`cl_<variable>`,
# as the ARD program writes them).  A study with no code lists has no
# code lists, study$adam is the same with blanks as NA only; so is it under
# `adam_levels` "none" (`levels` empty then).  The last lines make the
# objects the ARD, listing and figure programs read (`adsl`) the datasets
# of study$adam: the generated programs always read study$adam.
.study_data_lines <- function(ds, levels = list(), adam_levels = "codelists") {
  if (!nrow(ds)) return(character())
  nm <- .r_name(ds$dataset)
  delayed <- function(name, expr, env) {
    # (an argument a line when the call is long: readable code)
    .lay(.cl("delayedAssign", list(.q(name), expr, assign.env = env)))
  }
  org <- vapply(seq_len(nrow(ds)), function(i) {
    d <- .split_bar(ds$derive[i])
    code <- .make_code(".x", .reader(ds$path[i]),
                       if (length(d)) sprintf("dplyr::mutate(%s)", paste(d, collapse = ", ")))
    delayed(nm[i], sub("^[.]x <- ", "", code), "study$adam_org")
  }, "")
  # study$adam: the same with blanks as NA (blank_as_na()), then the code lists
  # (set_levels()) when the study has them
  adam <- vapply(seq_len(nrow(ds)), function(i) {
    src <- paste0("study$adam_org$", nm[i])
    expr <- if (length(levels)) {
      args <- stats::setNames(as.list(.cl_name(names(levels))), names(levels))
      step <- gsub("\n", "\n  ", .lay(.cl("set_levels", args), lead = 4L), fixed = TRUE)
      paste0(src, " |>\n  blank_as_na() |>\n  ", step)
    } else {
      .cl("blank_as_na", list(src))
    }
    delayed(nm[i], expr, "study$adam")
  }, "")
  head <- if (identical(adam_levels, "none")) {
    c("# the study's input data, read when first used: study$adam_org (the ADaM as read,",
      "# with the catalog's derive), study$adam (blanks as NA; adam_levels: none, so not the",
      "# study's code lists' levels)",
      "study <- list(adam_org = new.env(), adam = new.env())", org, adam)
  } else if (length(levels)) {
    c("# the study's code lists (spec/defaults.yml codelists:)", .codelists_lines(levels), "",
      "# the study's input data, read when first used: study$adam_org (the ADaM as read,",
      "# with the catalog's derive), study$adam (blanks as NA, then the study's code lists'",
      "# levels)",
      "study <- list(adam_org = new.env(), adam = new.env())", org, adam)
  } else {
    c("# the study's input data, read when first used: study$adam_org (the ADaM as read,",
      "# with the catalog's derive), study$adam (blanks as NA; the study has no code lists)",
      "study <- list(adam_org = new.env(), adam = new.env())", org, adam,
      "message(\"The study has no code lists: study$adam is study$adam_org with blanks as NA.\")")
  }
  code <- c(head, "",
            "# (the objects the ARD, listing and figure programs read: study$adam's)",
            sprintf("delayedAssign(%s, study$adam$%s)", .q(nm), nm))
  code <- .drop_attached_ns(code)
  unlist(lapply(code, function(l) if (nzchar(l)) strsplit(l, "
", fixed = TRUE)[[1L]] else ""))
}

# The setup code of a study's spec, one element a line (none: the study's
# defaults say no font, size, tokens, header or footer).  The font and size
# are its default configuration's; the tokens study.yml's `tokens:`; each
# configuration's header and footer are written once, as objects
# header_<name> and footer_<name>.
.study_setup_code <- function(study) {
  pg <- .page_config_flat(list(page = .study_default_page(study)))
  out <- character()
  # the company's font and size, for every report
  opts <- list()
  ft <- .page_args(pg$rtf_document)$font_table
  if (length(ft)) {
    first <- if (is.list(ft) && length(ft) && is.list(ft[[1L]])) ft[[1L]]$name else ft[[1L]]
    # (one family, or a family per script slot: a named vector)
    first <- .font_slots_value(first)
    if (!is.null(first) && !is.na(first[1L]) && nzchar(first[1L])) {
      opts[[.page_options[["font"]]]] <- if (is.null(names(first))) as.character(first[1L]) else first
    }
  }
  # the language of each font slot (rtfreporter's font_lang)
  fl <- .font_slots_value(.page_args(pg$rtf_document)$font_lang)
  if (length(fl)) opts[["rtfreporter.font_lang"]] <- fl
  # rtfreporter's options the study says by name (defaults.yml's options:);
  # they win over the page's font and size
  for (k in names(study$options)) {
    v <- study$options[[k]]
    opts[[k]] <- if (is.list(v) && !is.null(names(v))) unlist(v) else v
  }
  # the study's rounding, once for every report (a report's own plan_digits(
  # rounding = ) wins where it differs)
  # (the study's setup file sets it when option tflspec.study_setup is TRUE)
  if (!isTRUE(getOption("tflspec.study_setup"))) {
    rnd <- .study_rounding(study)
    if (!is.null(rnd) && is.null(opts[["rtfreporter.rounding"]])) opts[["rtfreporter.rounding"]] <- rnd
    # (the same value for cards, which formats the ARD's numbers)
    ct <- .cards_round_type(opts[["rtfreporter.rounding"]])
    if (!is.null(ct) && is.null(opts[["cards.round_type"]])) opts[["cards.round_type"]] <- ct
  }
  sz <- .page_style(pg)$font_size_half_points
  if (!is.null(sz)) {
    n <- suppressWarnings(as.integer(sz))
    opts[[.page_options[["font_size_half_points"]]]] <- if (is.na(n)) sz else n
  }
  if (length(opts)) {
    out <- c(out, .spec_call_code(do.call(.spec_call, c(list("options"), opts)), 0L))
  }
  # the study's tokens (study.yml)
  tk <- .study_tokens(study)
  if (length(tk)) {
    v <- vapply(tk, function(x) if (length(x) != 1L || is.na(x)) "" else as.character(x), "")
    keep <- !trimws(v) %in% .ard_spec_omit
    if (any(keep)) {
      tok <- stats::setNames(as.list(unname(v[keep])), trimws(names(v)[keep]))
      out <- c(out, .spec_call_code(.spec_call("options", rtfreporter.tokens = tok), 0L))
    }
  }
  # the running header and footer of each configuration: the lines and the band
  for (nm in .setup_style_names(study)) {
    cpg <- .page_config_flat(.tpl_style_set(nm, study))
    for (band in names(.setup_band_names)) {
      b <- .page_band(cpg, band, NULL)
      if (!length(b)) next
      # a line of tokens of one's own goes when a report's are empty
      own <- setdiff(.page_used_tokens(cpg[band]), .ard_spec_rtf_tokens)
      obj <- .setup_band_object(band, nm)
      # (its look as the configuration says it: page.rtf_header / rtf_footer)
      style <- .band_style(cpg, band)
      if (is.null(style$drop_empty_rows) && length(own)) style$drop_empty_rows <- TRUE
      call <- do.call(.spec_call, c(list(paste0("rtf_", band), b), style))
      out <- c(out, paste(obj, "<-", .spec_call_code(call, 0L, lead = nchar(obj) + 4L)))
    }
  }
  if (!length(out)) return(character())
  .drop_attached_ns(unlist(strsplit(out, "\n", fixed = TRUE)))
}

# The {NAME} tokens the lines of page: bands say (header, footer, titles,
# footnotes; as many as are given)
.page_used_tokens <- function(pg) {
  txt <- unlist(lapply(pg, function(m) unlist(lapply(.page_args(m), .page_args))))
  txt <- as.character(txt[!is.na(txt)])
  m <- unlist(regmatches(txt, gregexpr("\\{[A-Z][A-Z0-9_]*\\}", txt)))
  unique(substr(m, 2L, nchar(m) - 1L))
}

# A report's tokens (YAML): as its document takes them, rtf_document(tokens
# = ) of tfl_report_code(output, study = ) -- the study's, its pattern's
# and its own, and OUTPUT_ID / OUTPUT_LABEL when its lines say them
.output_tokens <- function(output, study = NULL) {
  if (!isTRUE(attr(output, "resolved")) && !is.null(study)) {
    output <- tfl_resolve_output(output, study)
  }
  tk <- .page_tokens(output$page %||% list(), output$id %||% NA_character_,
                     output$kind %||% "table", .page_own_tokens(output, study))
  if (is.null(tk)) return(stats::setNames(character(), character()))
  vapply(tk, as.character, "")
}

# The study's rounding (rtfreporter's half to even "r", or "sas"): the last
# plan_digits(rounding = ) of the study's defaults.yml; NULL when it says none
# The study's rounding set as rtfreporter's option for an in-process build
# (tfl_run_output(), tfl_ai_run()), as the programs' setup sets it; the
# options as they were, to put back.  A study that says none leaves the
# option as it is.
.study_rounding_option <- function(study) {
  rnd <- .study_rounding(study)
  if (is.null(rnd)) return(list())
  ct <- .cards_round_type(rnd)
  if (is.null(ct)) return(options(rtfreporter.rounding = rnd))
  options(rtfreporter.rounding = rnd, cards.round_type = ct)
}

# The study's rounding as cards' option says it (cards formats the ARD's
# numbers, rtfreporter's option does not reach it): "sas", a half away from
# zero, is cards' "round-half-up" (round5(), by magnitude); "r", a half to
# even, is "round-to-even" (base round()).  NULL for any other value.
.cards_round_type <- function(rnd) {
  if (is.null(rnd) || length(rnd) != 1L || is.na(rnd)) return(NULL)
  switch(tolower(as.character(rnd)), sas = "round-half-up", r = "round-to-even", NULL)
}

# The values a rounding is spelled as, from the schema ("sas", "r", ...)
.rounding_values <- function() {
  ty <- .schema_arg_type(tfl_schema("plan_digits"), "rounding")
  if (identical(ty$kind, "enum")) ty$args else c("sas", "r")
}

# A rounding must be spelled exactly as one of the values ("SAS" is not
# "sas"); else it stops, naming them.  `where`: the field, with its file.
.check_rounding <- function(x, where) {
  ok <- .rounding_values()
  if (!(is.character(x) && length(x) == 1L && !is.na(x) && x %in% ok)) {
    stop(where, " takes one of ", paste0("\"", ok, "\"", collapse = ", "),
         ", not \"", paste(as.character(x), collapse = ", "), "\"", call. = FALSE)
  }
  x
}

.study_rounding <- function(study) {
  # study.yml's `settings: rounding` (tfl_ai_study_init() writes it); the
  # defaults' plan_digits(rounding = ) over it
  r <- study$study$settings$rounding
  if (!is.null(r)) r <- .check_rounding(as.character(r)[1L], "study.yml: settings: rounding")
  for (it in study$defaults$content %||% list()) {
    if (identical(.schema_fn_name(it$fn %||% ""), "plan_digits") && !is.null(it$args$rounding)) {
      r <- .check_rounding(as.character(it$args$rounding), "plan_digits rounding")
    }
  }
  r
}

# The items as a report's program writes them: the study's rounding is the
# setup's (options(rtfreporter.rounding = ), report_setup.R, every report
# sources it), so a plan_digits() rounding that is the study's is left out
# (the item out when nothing else is left); a report's own rounding that
# differs stays.  `rounding`: the study's (NULL: every rounding is the
# study's -- a workbook's, one per study).  The run in hand keeps the items
# as they are (the same numbers, with no option set).
.items_for_program <- function(content, rounding = NULL) {
  if (inherits(content, "tfl_r") || !length(content)) return(content)
  out <- lapply(content, function(it) {
    if (!inherits(it, "tfl_item") || !identical(.schema_fn_name(it$fn %||% ""), "plan_digits") ||
        is.null(it$args$rounding)) return(it)
    if (!is.null(rounding) && !identical(tolower(as.character(it$args$rounding)), rounding)) return(it)
    it$args$rounding <- NULL
    if (!length(it$args)) NULL else it
  })
  Filter(Negate(is.null), out)
}
