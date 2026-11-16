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
