# ---- the study's ADaM metadata (study$adam_meta) ----------------------------
#
# A study keeps two things apart (canon, 2026-11-16):
#
#   study$codelists  its code lists for display, by NAME (spec/defaults.yml
#                    `codelists:`), referred to explicitly;
#   study$adam_meta  the ADaM metadata, in the shape tfl_read_define() gives
#                    (a `tfl_define`: variables with the OID of their code
#                    list, the code lists one row a value).  Until a define is
#                    read it is written by hand in spec/defaults.yml
#                    `adam_meta:`.
#
# Under `settings: adam_levels: codelists` study$adam gets the levels of the
# code lists adam_meta maps to each dataset's variables (and only those: no
# list is put on a column by its name); value-level lists are never put on.

# The parts of `adam_meta:` and each one's empty table (the define's shape)
.adam_meta_parts <- function() {
  list(variables = .define_variables_empty(), codelists = .define_codelists_empty(),
       datasets = .define_datasets_empty(), external = .define_external_empty(),
       value_level = .define_vl_empty())
}

# The columns a row must give, by part
.adam_meta_required <- list(variables = c("dataset", "variable"),
                            codelists = c("codelist", "value"),
                            datasets = "dataset", external = "codelist",
                            value_level = c("dataset", "variable"))

# A study with no adam_meta: the define's shape, no rows
.adam_meta_empty <- function(source = list(file = NA_character_, kind = "defaults.yml",
                                           version = NA_character_)) {
  p <- .adam_meta_parts()
  .define_new(.define_study_empty(), p$datasets, p$variables, p$codelists, p$external,
              source = source, value_level = p$value_level)
}

# defaults.yml's `adam_meta:` as a `tfl_define`: each part a list of rows
# (`variables: [{dataset: ADSL, variable: SEX, codelist: CL.SEX}, ...]`), a
# column a row does not give NA, the columns in the define's classes.  A
# part, or a column, the define does not have stops; so does a row without
# the columns its part needs, two rows of one variable or of one code, and a
# variable's code list that `codelists:` (or `external:`) does not have.
.adam_meta_read <- function(x, where = "defaults.yml") {
  src <- list(file = where, kind = "defaults.yml", version = NA_character_)
  if (is.null(x) || !length(x)) return(.adam_meta_empty(src))
  bad <- function(...) stop(where, ": adam_meta: ", ..., call. = FALSE)
  if (!.is_map(x)) bad("is a map of tables (variables:, codelists:), each a list of rows.")
  parts <- .adam_meta_parts()
  unknown <- setdiff(names(x), names(parts))
  if (length(unknown)) {
    bad("no part ", unknown[1L], "; the parts are ", paste(names(parts), collapse = ", "), ".")
  }
  for (p in names(x)) parts[[p]] <- .adam_meta_rows(x[[p]], parts[[p]], p, bad)
  v <- parts$variables
  cl <- parts$codelists
  dup <- duplicated(data.frame(toupper(v$dataset), toupper(v$variable)))
  if (any(dup)) bad("variables: ", v$dataset[dup][1L], ".", v$variable[dup][1L], " is there twice.")
  dup <- duplicated(cl[c("codelist", "value")])
  if (any(dup)) {
    bad("codelists: ", cl$codelist[dup][1L], " has the value \"", cl$value[dup][1L], "\" twice.")
  }
  miss <- !is.na(v$codelist) & !v$codelist %in% c(cl$codelist, parts$external$codelist)
  if (any(miss)) {
    bad("variables: ", v$dataset[miss][1L], ".", v$variable[miss][1L], " names the code list ",
        v$codelist[miss][1L], ", which codelists: does not have.")
  }
  .define_new(.define_study_empty(), parts$datasets, v, cl, parts$external,
              source = src, value_level = parts$value_level)
}

# One part's rows as its table: `empty` gives the columns and their classes
.adam_meta_rows <- function(rows, empty, part, bad) {
  if (is.null(rows) || !length(rows)) return(empty)
  if (.is_map(rows) || !is.list(rows) || !all(vapply(rows, .is_map, NA))) {
    bad(part, ": is a list of rows, each a map ({", paste(names(empty)[1:2], collapse = ", "), ", ...}).")
  }
  cols <- unique(unlist(lapply(rows, names)))
  unknown <- setdiff(cols, names(empty))
  if (length(unknown)) {
    bad(part, ": no column ", unknown[1L], "; the columns are ", paste(names(empty), collapse = ", "), ".")
  }
  out <- empty[rep(NA_integer_, length(rows)), , drop = FALSE]
  rownames(out) <- NULL
  for (cn in cols) {
    val <- lapply(rows, function(r) {
      e <- r[[cn]]
      if (is.null(e) || !length(e)) NA else e
    })
    if (any(lengths(val) != 1L)) bad(part, ": ", cn, " is one value a row.")
    val <- unlist(val, use.names = FALSE)
    cls <- class(empty[[cn]])[1L]
    got <- switch(cls,
                  integer = suppressWarnings(as.integer(val)),
                  numeric = suppressWarnings(as.numeric(val)),
                  logical = .adam_meta_logical(val),
                  as.character(val))
    if (cls == "integer" && any(!is.na(val) & (is.na(got) | got != suppressWarnings(as.numeric(val))))) {
      bad(part, ": ", cn, " is a whole number.")
    }
    if (cls %in% c("numeric", "logical") && any(!is.na(val) & is.na(got))) {
      bad(part, ": ", cn, " is a ", if (cls == "logical") "yes or no." else "number.")
    }
    out[[cn]] <- got
  }
  for (cn in .adam_meta_required[[part]]) {
    no <- is.na(out[[cn]]) | !nzchar(trimws(out[[cn]]))
    if (any(no)) bad(part, ": row ", which(no)[1L], " has no ", cn, ".")
  }
  out
}

.adam_meta_logical <- function(val) {
  if (is.logical(val)) return(val)
  v <- toupper(trimws(as.character(val)))
  ifelse(v %in% c("YES", "Y", "TRUE"), TRUE, ifelse(v %in% c("NO", "N", "FALSE"), FALSE, NA))
}

# The study's code lists as the programs have them, by name: each list's
# codes in their order, named by the code when a label is given
# (c(F = "Female")), else the codes alone (defaults.yml's `codelists:`).
.study_codelists_read <- function(cl) {
  out <- lapply(.codelists_norm(cl), function(rows) {
    codes <- vapply(rows, function(r) as.character(r$value), "")
    lab <- .tpl_cl_dict(rows)
    # (a list whose labels are its codes: the codes alone)
    if (is.null(lab) || all(names(lab) == unname(lab))) codes else lab
  })
  out[lengths(out) > 0L]
}

# ---- adam_meta in the programs ------------------------------------------------

# The variables adam_meta maps to a code list of values in `dataset` (its
# name in any case), in adam_meta's order: `dataset`, `variable`, `codelist`.  A
# dictionary's list (external:) holds no values, so it puts no levels.
.adam_meta_vars <- function(meta, dataset) {
  v <- meta$variables
  none <- data.frame(dataset = character(), variable = character(), codelist = character(),
                     stringsAsFactors = FALSE)
  if (is.null(v) || !nrow(v) || !length(dataset)) return(none)
  keep <- toupper(v$dataset) %in% toupper(dataset) & !is.na(v$codelist) &
    v$codelist %in% meta$codelists$codelist
  if (!any(keep)) return(none)
  data.frame(dataset = v$dataset[keep], variable = v$variable[keep], codelist = v$codelist[keep],
             stringsAsFactors = FALSE)
}

# The step that puts adam_meta's levels on `dataset`:
# set_levels(SEX = adam_codes("CL.SEX"), ...) (a call to lay out); NULL when
# adam_meta maps none of its variables
.adam_meta_step <- function(meta, dataset) {
  v <- .adam_meta_vars(meta, dataset)
  if (!nrow(v)) return(NULL)
  .cl("set_levels", stats::setNames(as.list(sprintf("adam_codes(%s)", .q(v$codelist))), v$variable))
}

# adam_meta narrowed to `datasets`: their variables with a code list of
# values, and those code lists' rows (a define's tables, as they are)
.adam_meta_narrow <- function(meta, datasets) {
  v <- meta$variables
  keep <- toupper(v$dataset) %in% toupper(datasets) & !is.na(v$codelist) &
    v$codelist %in% meta$codelists$codelist
  cl <- meta$codelists
  list(variables = v[keep, , drop = FALSE],
       codelists = cl[cl$codelist %in% v$codelist[keep], , drop = FALSE])
}

# The ADaM metadata a program writes, `study$adam_meta <- list(...)`: the
# variables of `datasets` with a code list (dataset, variable, codelist) and
# those code lists (codelist, value, and name, decode, order and rank when
# some row gives them), each a tibble::tribble(), a row a line
.adam_meta_lines <- function(meta, datasets) {
  m <- .adam_meta_narrow(meta, datasets)
  cl_cols <- c("codelist", "name", "value", "decode", "order", "rank")
  cl_cols <- cl_cols[cl_cols %in% c("codelist", "value") |
                       vapply(cl_cols, function(cn) any(!is.na(m$codelists[[cn]])), NA)]
  tv <- .tribble_lines(m$variables[c("dataset", "variable", "codelist")])
  tc <- .tribble_lines(m$codelists[cl_cols])
  ind <- function(x) paste0("  ", x)
  tv[1L] <- paste("variables =", tv[1L])
  tc[1L] <- paste("codelists =", tc[1L])
  tv[length(tv)] <- paste0(tv[length(tv)], ",")
  c("study$adam_meta <- list(", ind(tv), ind(tc), ")")
}

# The set_levels() step of a program's own read of `dataset` (no study
# setup): adam_meta's levels, as study$adam has them (a step as text; NULL
# when it maps none of its variables)
.adam_meta_read_step <- function(meta, dataset) {
  st <- .adam_meta_step(meta, dataset)
  if (is.null(st)) NULL else .lay(st, width = Inf)
}

# The head of a program that reads `datasets` itself (no study setup) and
# puts adam_meta's levels on them: `study$adam_meta`, the rows its reads
# use, which adam_codes() reads; none when adam_meta maps none of them
.adam_meta_block <- function(meta, datasets) {
  if (is.null(meta) || !nrow(.adam_meta_vars(meta, datasets))) return(character())
  c("# ---- the study's ADaM metadata: the levels the reads put on, as study$adam has them ----",
    "study <- list()", .adam_meta_lines(meta, datasets), "")
}

# A dataset read in tflspec's own runs (no program: an AI draw, a figure's
# content) with adam_meta's levels on it, as study$adam has them (the study
# helpers' set_levels() and the codes adam_codes() gives); under
# adam_levels: none, or for a variable adam_meta does not map, as it is
.adam_meta_levels <- function(d, dataset, study) {
  if (is.null(dataset) || !inherits(study, "tfl_study_spec") ||
      identical(.study_adam_levels(study), "none")) {
    return(d)
  }
  v <- .adam_meta_vars(study$adam_meta, dataset)
  if (!nrow(v)) return(d)
  lists <- stats::setNames(lapply(v$codelist, function(cl) .adam_codes(study$adam_meta, cl)), v$variable)
  do.call(.helpers_env()$set_levels, c(list(d), lists))
}

# The study helpers (tfl_helpers_code()), once a session
.helpers_env <- function() {
  if (is.null(.helpers_memo$env)) {
    env <- new.env(parent = globalenv())
    eval(parse(text = tfl_helpers_code()), env)
    .helpers_memo$env <- env
  }
  .helpers_memo$env
}
.helpers_memo <- new.env(parent = emptyenv())

# A report's code lists with the columns adam_meta maps (in any dataset)
# as listed too: their levels are study$adam's, kept (a zero row counts),
# not dropped as an unlisted category's are (.unlisted_vars())
.adam_listed <- function(levels, meta, datasets = NULL) {
  v <- meta$variables
  if (is.null(v) || !nrow(v)) return(levels)
  # (the analysis's dataset, and ADSL, whose columns a data adds)
  if (length(datasets)) v <- v[toupper(v$dataset) %in% toupper(c(datasets, "ADSL")), , drop = FALSE]
  extra <- setdiff(unique(v$variable[!is.na(v$codelist)]), names(levels))
  if (!length(extra)) return(levels)
  c(levels, stats::setNames(rep(list(character()), length(extra)), extra))
}

# The dataset an analysis row reads: its `dataset`, else the one its
# analysis data is first made from; NULL when neither says
.analysis_ds <- function(x, r) {
  ds <- r$dataset %||% NA_character_
  if (!is.na(ds)) return(ds)
  d <- .data_col(r)
  if (is.na(d)) return(NULL)
  ds <- .adata_dataset(.adata_of(.adata_sheet(x), r$output_id), d)
  if (is.na(ds)) NULL else ds
}

# The data a program defines (before its analyses, `first_an`), each with
# the levels of an adam_meta column its own filter narrows put on again,
# as a report's code list is (Q3: a subgroup's data has no zero row for the
# levels its filter leaves out): set_levels(SEX = intersect(
# adam_codes("CL.SEX"), c("F"))).  `listed`: the report's own lists'
# variables, which their step narrows.  A variable mapped to two code lists
# (two datasets) is narrowed by the first.
.adam_narrow_put <- function(code, meta, listed = character(), first_an = NA_integer_) {
  v <- meta$variables
  if (is.null(v) || !nrow(v) || !length(code)) return(code)
  v <- v[!is.na(v$codelist) & v$codelist %in% meta$codelists$codelist &
           !v$variable %in% listed, , drop = FALSE]
  v <- v[!duplicated(v$variable), , drop = FALSE]
  if (!nrow(v)) return(code)
  end <- if (is.na(first_an)) length(code) else first_an - 1L
  def <- grepl("^[A-Za-z.][A-Za-z0-9._]* <- ", code) & seq_along(code) <= end
  for (k in which(def)) {
    # (its own filters: the data it is made from has its levels already)
    conds <- .def_conditions(code[k], 1L)
    if (!length(conds)) next
    nc <- .narrow_conditions(conds, v$variable)
    args <- list()
    for (i in seq_len(nrow(v))) {
      var <- v$variable[i]
      e <- .narrow_expr(.adam_codes(meta, v$codelist[i]), sprintf("adam_codes(%s)", .q(v$codelist[i])),
                        nc$include[[var]], nc$exclude[[var]])
      if (!is.null(e)) args[[var]] <- e
    }
    if (!length(args)) next
    step <- gsub("\n", "\n  ", .lay(.cl("set_levels", args), lead = 2L), fixed = TRUE)
    code[k] <- paste0(code[k], " |>\n  ", step)
  }
  code
}

# A data frame as tibble::tribble(), a row a line, the columns lined up:
# text in quotes, a whole number with its L, a missing value NA
.tribble_lines <- function(d) {
  head <- paste0("~", names(d))
  if (!nrow(d)) return(sprintf("tibble::tribble(%s)", paste(head, collapse = ", ")))
  cells <- lapply(d, function(v) {
    out <- if (is.character(v)) .q(v) else if (is.integer(v)) paste0(v, "L") else as.character(v)
    out[is.na(v)] <- "NA"
    out
  })
  m <- rbind(head, do.call(cbind, cells))
  w <- apply(m, 2L, function(x) max(nchar(x)))
  k <- ncol(m)
  rows <- vapply(seq_len(nrow(m)), function(i) {
    cell <- m[i, ]
    cell[-k] <- sprintf("%-*s", w[-k] + 1L, paste0(cell[-k], ","))
    sub(" +$", "", paste(cell, collapse = " "))
  }, "")
  last <- length(rows)
  rows[-last] <- paste0(rows[-last], ",")
  c("tibble::tribble(", paste0("  ", rows), ")")
}

# The study's code lists as the setup writes them, `study$codelists <-
# list(SEX = c(F = "Female", M = "Male"), ...)`, by name, a list a line
.study_codelists_lines <- function(codelists) {
  if (!length(codelists)) return("study$codelists <- list()")
  nm <- .r_arg_name(names(codelists))
  body <- vapply(seq_along(codelists), function(i) {
    cl <- codelists[[i]]
    vals <- as.list(.q(unname(cl)))
    if (!is.null(names(cl))) names(vals) <- names(cl)
    gsub("\n", "\n  ", paste(nm[i], "=", .lay(.cl("c", vals), lead = nchar(nm[i]) + 5L)), fixed = TRUE)
  }, "")
  strsplit(paste0("study$codelists <- list(\n  ", paste(body, collapse = ",\n  "), "\n)"),
           "\n", fixed = TRUE)[[1L]]
}

# The study's adam_meta for its ARD definition (.ard_spec_of_study()): the
# variables of its datasets (data.yml) with a code list of values, and those
# lists; NULL under adam_levels: none, or when it maps none
.adam_meta_used <- function(study) {
  if (!inherits(study, "tfl_study_spec") || identical(.study_adam_levels(study), "none")) return(NULL)
  meta <- study$adam_meta
  if (is.null(meta)) return(NULL)
  m <- .adam_meta_narrow(meta, .study_catalog(study)$dataset)
  if (!nrow(m$variables)) return(NULL)
  m
}

# What adam_meta gives an ARD's fingerprint: each variable of `datasets` it
# maps and the codes it puts on it, in order (none: nothing, so a study
# without adam_meta keeps its fingerprints)
.adam_meta_hash_lines <- function(meta, datasets) {
  if (is.null(meta)) return(character())
  v <- .adam_meta_vars(meta, datasets)
  if (!nrow(v)) return(character())
  c("adam_meta:", vapply(seq_len(nrow(v)), function(i) {
    paste0(toupper(v$dataset[i]), ".", v$variable[i], " ", v$codelist[i], ": ",
           paste(.adam_codes(meta, v$codelist[i]), collapse = " | "))
  }, ""))
}

# The codes of one code list of adam_meta in order: adam_codes(), the study
# helper, done here (by `order`, then `rank`, then as the table has them)
.adam_codes <- function(meta, codelist) {
  cl <- meta$codelists
  rows <- cl[!is.na(cl$codelist) & cl$codelist == codelist, , drop = FALSE]
  n <- nrow(rows)
  col <- function(cn) if (cn %in% names(rows)) rows[[cn]] else rep(NA, n)
  as.character(rows$value[order(col("order"), col("rank"), seq_len(n))])
}

# The study's code lists named like an ADaM column that adam_meta does not
# map, in any dataset (tfl_check_spec()'s study rows): a list's name is not
# a mapping, so study$adam has no levels of that column.  The columns are
# those of the datasets of data.yml whose files can be read (the header
# only for .xpt), and adam_meta's variables.  None under adam_levels: none.
# A list each: list(list = <name>, message = <text>).
.study_list_unmapped <- function(study) {
  lists <- names(study$codelists %||% list())
  if (!length(lists) || identical(.study_adam_levels(study), "none")) return(list())
  meta <- study$adam_meta
  mapped <- toupper(meta$variables$variable[!is.na(meta$variables$codelist)])
  cols <- list()
  dir <- study$dir %||% "."
  for (e in study$data$datasets %||% list()) {
    ds <- as.character(e$dataset %||% NA)[1L]
    p <- as.character(e$path %||% NA)[1L]
    if (is.na(ds) || is.na(p)) next
    if (!grepl("^([A-Za-z]:)?[/\\\\]", p)) p <- file.path(dir, p)
    nm <- tryCatch({
      if (!file.exists(p)) {
        NULL
      } else if (grepl("[.]xpt$", p, ignore.case = TRUE) && requireNamespace("haven", quietly = TRUE)) {
        names(haven::read_xpt(p, n_max = 0L))
      } else if (grepl("[.]rds$", p, ignore.case = TRUE)) {
        names(readRDS(p))
      }
    }, error = function(err) NULL)
    for (v in nm) cols[[toupper(v)]] <- c(cols[[toupper(v)]], toupper(ds))
  }
  for (i in seq_len(nrow(meta$variables %||% data.frame()))) {
    v <- toupper(meta$variables$variable[i])
    cols[[v]] <- unique(c(cols[[v]], toupper(meta$variables$dataset[i])))
  }
  out <- list()
  for (l in lists) {
    u <- toupper(l)
    if (is.null(cols[[u]]) || u %in% mapped) next
    where <- paste0(unique(cols[[u]]), ".", l, collapse = ", ")
    out[[length(out) + 1L]] <- list(list = l, message = sprintf(paste0(
      "code list %s is named like the column %s, which adam_meta does not map: study$adam ",
      "has no levels of it from the list (its name is not a mapping). Map it in ",
      "spec/defaults.yml adam_meta: variables: (a code list of its values), or name the list ",
      "otherwise."), l, where))
  }
  out
}
