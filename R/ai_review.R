# ============================================================================
#  tfl_ai_review() -- a spec file an AI wrote, checked for the AI and for
#  the person
# ============================================================================
#
#  One file at a time: a workbook (any of the ARD, table, report and
#  listing specs) or a figure design .yml.  The checks are the review's
#  (tfl_review_spec(), tfl_check_fig_design(), the readers' own errors)
#  and a few of the kit's (inst/review/ai_rules.csv): sheets and columns
#  that are not the spec's, outputs and titles that are not the TOC's,
#  numbers written where a token belongs, the assumptions the guide asks
#  for.  Each item says who acts on it: the AI (it has everything it needs
#  to fix it) or the person (only they know: a flag, a denominator, a
#  method, an order).  The two halves are written as two prompts: one to
#  paste back into the chat, one to answer and paste back.

.ai_rules <- function() {
  p <- system.file("review", "ai_rules.csv", package = "tflspec")
  r <- utils::read.csv(p, stringsAsFactors = FALSE, na.strings = character(),
                       encoding = "UTF-8")
  r$hint[is.na(r$hint)] <- ""
  r
}

# The catalog the kit's rows are built from: the review's rules and the
# kit's own, as .rv() reads them.
.ai_cat <- function() {
  main <- tfl_review_rules()
  kit <- .ai_rules()
  kit <- kit[!kit$rule %in% main$rule, , drop = FALSE]
  rbind(main, data.frame(rule = kit$rule, level = kit$level, area = "kit",
                         needs = "spec", checked_by = "tflspec",
                         message = kit$message, hint = kit$hint,
                         stringsAsFactors = FALSE))
}

# A kit row: its hint filled in with `hint_args` when it has a %s.
.ai_rv <- function(rule, ..., hint_args = character(), cat = .ai_cat()) {
  r <- .rv(rule, ..., cat = cat)
  if (length(hint_args) && grepl("%s", r$hint, fixed = TRUE)) {
    r$hint <- do.call(sprintf, c(list(r$hint), as.list(hint_args)))
  }
  r
}

# The metadata as tfl_adam_metadata() gives it, from the frame or the
# kit's CSV read back (where an empty level is "", not NA).
.ai_review_md <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    x <- utils::read.csv(x, colClasses = "character", na.strings = "")
  }
  if (!is.data.frame(x) || !all(c("dataset", "variable") %in% names(x))) {
    .ard_stop(paste0("tfl_ai_review(): `metadata` is tfl_adam_metadata()'s ",
                     "result, or the kit's 08_adam_metadata.csv."))
  }
  x$dataset <- toupper(trimws(as.character(x$dataset)))
  x$variable <- trimws(as.character(x$variable))
  if (!"levels" %in% names(x)) x$levels <- NA_character_
  x
}

# The names a workbook writes, checked against the metadata: datasets
# (datasets, populations, analyses, listings), variables (an analysis's
# variables, by and strata in its dataset; the variables and codelists
# sheets' own) and code list values (among the variable's levels).
.ai_review_metadata <- function(sheets, md, add, cat) {
  sh <- stats::setNames(sheets, tolower(names(sheets)))
  known_ds <- unique(md$dataset)
  near <- function(x, pool) {
    d <- utils::adist(toupper(x), toupper(pool))
    paste(utils::head(pool[order(d)], 3L), collapse = ", ")
  }
  split_names <- function(v) {
    v <- unlist(strsplit(as.character(v[!is.na(v)]), "[|,]"))
    v <- trimws(v)
    unique(v[nzchar(v)])
  }
  for (nm in intersect(c("datasets", "populations", "analyses", "listings"), names(sh))) {
    d <- sh[[nm]]
    if (!"dataset" %in% names(d)) next
    for (i in which(!is.na(d$dataset) & nzchar(trimws(d$dataset)))) {
      ds <- toupper(trimws(d$dataset[i]))
      if (!ds %in% known_ds) {
        add(.ai_rv("K14", output_id = d$output_id[i] %||% NA_character_,
                   sheet = nm, row = as.character(i), field = "dataset",
                   args = c(nm, "the dataset", ds), hint_args = near(ds, known_ds),
                   cat = cat))
      }
    }
  }
  an <- sh[["analyses"]]
  if (!is.null(an)) {
    for (i in seq_len(nrow(an))) {
      ds <- toupper(trimws(an$dataset[i] %||% NA_character_))
      pool <- if (!is.na(ds) && ds %in% known_ds) md$variable[md$dataset == ds]
              else unique(md$variable)
      for (col in intersect(c("variables", "by", "strata"), names(an))) {
        for (v in setdiff(split_names(an[[col]][i]), pool)) {
          add(.ai_rv("K14", output_id = an$output_id[i], sheet = "analyses",
                     row = an$analysis_id[i] %||% as.character(i), field = col,
                     args = c("analyses", "the variable", v),
                     hint_args = near(v, pool), cat = cat))
        }
      }
    }
  }
  vs <- sh[["variables"]]
  if (!is.null(vs) && "variable" %in% names(vs)) {
    pool <- unique(md$variable)
    for (i in which(!is.na(vs$variable) & !vs$variable %in% pool)) {
      add(.ai_rv("K14", output_id = vs$output_id[i], sheet = "variables",
                 row = as.character(i), field = "variable",
                 args = c("variables", "the variable", vs$variable[i]),
                 hint_args = near(vs$variable[i], pool), cat = cat))
    }
  }
  cl <- sh[["codelists"]]
  if (!is.null(cl) && all(c("variable", "value") %in% names(cl))) {
    for (i in seq_len(nrow(cl))) {
      lv <- md$levels[md$variable %in% cl$variable[i] & !is.na(md$levels)]
      if (!length(lv)) next
      lv <- unique(trimws(unlist(strsplit(lv, "|", fixed = TRUE))))
      if (!is.na(cl$value[i]) && !trimws(cl$value[i]) %in% lv) {
        add(.ai_rv("K15", output_id = cl$output_id[i], sheet = "codelists",
                   row = as.character(i), field = "value",
                   args = c(cl$variable[i], cl$value[i]),
                   hint_args = paste(lv, collapse = " | "), cat = cat))
      }
    }
  }
  invisible(NULL)
}

# ---- reading the file --------------------------------------------------------------

# The columns of each sheet the specs read: the blank specs' own.
.ai_sheet_columns <- function() {
  b <- .ai_blank_specs()
  out <- list(study = c("key", "value", "note"), about = c("key", "value", "note"))
  for (sp in list(b$ard, unclass(b$table), unclass(b$listing))) {
    for (nm in names(sp)) {
      if (is.data.frame(sp[[nm]]) && !nm %in% names(out)) {
        out[[nm]] <- unique(c(names(sp[[nm]]), "note"))
      }
    }
  }
  out
}

.ai_read_sheets <- function(path) {
  .ard_need("readxl", "tfl_ai_review()")
  sh <- readxl::excel_sheets(path)
  stats::setNames(lapply(sh, function(s) {
    d <- as.data.frame(readxl::read_excel(path, s, col_types = "text"),
                       stringsAsFactors = FALSE)
    d[rowSums(!is.na(d)) > 0, , drop = FALSE]
  }), sh)
}

# Sheets as a workbook, every cell text (the readers read text).
.ai_write_sheets <- function(sheets, path) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    d <- sheets[[nm]]
    d[] <- lapply(d, as.character)
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, d, keepNA = FALSE)
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

# The kinds a workbook holds, from its sheets.
.ai_kinds <- function(sheets) {
  ks <- .spec_kind_sheets()
  names(ks)[vapply(ks, function(s) any(tolower(sheets) %in% s), NA)]
}

# The assumptions the AI wrote: a workbook's `_assumptions` sheet, a
# design's `assumptions:` list.  A data frame output_id / sheet / item /
# assumption / question.
.ai_review_assumptions <- function(sheets = NULL, yml = NULL) {
  cols <- c("output_id", "sheet", "item", "assumption", "question")
  empty <- stats::setNames(as.data.frame(matrix(character(), 0, 5),
                                         stringsAsFactors = FALSE), cols)
  if (!is.null(yml)) {
    a <- unlist(yml$assumptions %||% list())
    if (!length(a)) return(empty)
    return(data.frame(output_id = NA_character_, sheet = "design",
                      item = as.character(seq_along(a)), assumption = as.character(a),
                      question = "", stringsAsFactors = FALSE))
  }
  k <- which(tolower(names(sheets)) == "_assumptions")
  if (!length(k)) return(NULL)
  d <- sheets[[k[1L]]]
  for (c in cols) if (is.null(d[[c]])) d[[c]] <- NA_character_
  d <- d[cols]
  d[] <- lapply(d, function(x) ifelse(is.na(x), "", as.character(x)))
  d$output_id[!nzchar(d$output_id)] <- NA_character_
  d
}

# ---- the TOC -----------------------------------------------------------------------

.ai_toc <- function(toc) {
  if (is.null(toc)) return(NULL)
  if (inherits(toc, "tfl_table_spec")) return(toc)
  if (is.list(toc) && !is.null(toc$path)) {
    return(do.call(tfl_read_toc, toc))
  }
  .ard_stop(paste0(
    "tfl_ai_review(): `toc` is a tfl_read_toc() result, or a list of its ",
    "arguments (path, map, sheet, skip)."))
}

.ai_lines <- function(d, id) {
  if (is.null(d) || !nrow(d)) return(character())
  d <- d[!is.na(d$output_id) & d$output_id == id, , drop = FALSE]
  d <- d[order(suppressWarnings(as.numeric(d$line))), , drop = FALSE]
  txt <- apply(d[intersect(c("left", "center", "right"), names(d))], 1L, function(x) {
    x <- x[!is.na(x) & nzchar(x)]
    paste(x, collapse = " ")
  })
  trimws(unname(txt))
}

# ---- the checks --------------------------------------------------------------------

.ai_review_book <- function(path, toc, adam, others, cat, metadata = NULL) {
  sheets <- .ai_read_sheets(path)
  known <- .ai_sheet_columns()
  rows <- list()
  add <- function(r) rows[[length(rows) + 1L]] <<- r
  base <- basename(path)
  # K01 / K02: the sheets and columns are the spec's
  for (nm in names(sheets)) {
    if (startsWith(nm, "_")) next
    if (!tolower(nm) %in% names(known)) {
      add(.ai_rv("K01", sheet = nm, args = c(base, nm),
                 hint_args = paste(setdiff(names(known), "about"), collapse = ", "), cat = cat))
      next
    }
    bad <- setdiff(names(sheets[[nm]]), known[[tolower(nm)]])
    for (b in bad) {
      add(.ai_rv("K02", sheet = nm, field = b, args = c(nm, b),
                 hint_args = paste(known[[tolower(nm)]], collapse = ", "), cat = cat))
    }
  }
  kinds <- .ai_kinds(names(sheets))
  # the rest reads a copy without what K01 / K02 named, so one renamed
  # sheet does not hide every other check (and is not said twice)
  clean <- tempfile(fileext = ".xlsx")
  on.exit(unlink(Sys.glob(sub("[.]xlsx$", "*", clean))), add = TRUE)
  keep <- sheets[tolower(names(sheets)) %in% names(known)]
  keep <- lapply(stats::setNames(names(keep), tolower(names(keep))), function(nm) {
    d <- keep[[nm]]
    d[intersect(names(d), known[[tolower(nm)]])]
  })
  # each reader its own sheets: a table's error does not stop the report's
  ks <- .spec_kind_sheets()
  own <- function(k) {
    f <- sub("[.]xlsx$", paste0("_", k, ".xlsx"), clean)
    .ai_write_sheets(keep[intersect(names(keep), c("study", "about", ks[[k]]))], f)
    f
  }
  # the readers' own errors, then the review of what reads
  read <- function(k) tryCatch(switch(k,
    ard = tfl_read_ard_spec(own("ard")),
    table = tfl_read_table_spec(own("table")),
    report = tfl_read_report_spec(own("report")),
    listing = tfl_read_listing_spec(own("listing"))),
    error = function(e) {
      add(.ai_rv("S01", args = paste0("The ", k, " spec: ", conditionMessage(e)), cat = cat))
      NULL
    })
  got <- stats::setNames(lapply(kinds, read), kinds)
  ard <- got$ard %||% others$ard
  spec <- got$table %||% others$table
  listings <- got$listing %||% others$listing
  if (!is.null(ard) || !is.null(spec) || !is.null(listings)) {
    facts <- if (!is.null(adam) && !is.null(ard)) {
      tryCatch(tfl_data_facts(adam, populations = ard$populations), error = function(e) NULL)
    }
    rv <- tryCatch(tfl_review_spec(spec = spec, ard = ard, listings = listings,
                                   facts = facts),
                   error = function(e) {
                     add(.ai_rv("S01", args = conditionMessage(e), cat = cat))
                     NULL
                   })
    if (!is.null(rv) && nrow(rv)) add(as.data.frame(rv))
  }
  # the TOC
  tc <- .ai_toc(toc)
  if (!is.null(tc)) {
    ids <- tc$report$output_id
    mine <- unique(c(if ("report" %in% kinds) got$report$report$output_id,
                     if ("table" %in% kinds) got$table$tables$output_id,
                     if ("listing" %in% kinds) got$listing$listings$output_id,
                     if ("ard" %in% kinds) got$ard$analyses$output_id))
    mine <- mine[!is.na(mine) & nzchar(mine)]
    for (id in setdiff(mine, ids)) add(.ai_rv("K03", output_id = id, args = id, cat = cat))
    if ("report" %in% kinds && !is.null(got$report)) {
      for (id in setdiff(ids, got$report$report$output_id)) {
        add(.ai_rv("K04", output_id = id, sheet = "report", args = c(id, "report"), cat = cat))
      }
      for (part in c("titles", "footnotes")) {
        for (id in intersect(ids, got$report$report$output_id)) {
          want <- .ai_lines(tc[[part]], id)
          have <- .ai_lines(got$report[[part]], id)
          n <- max(length(want), length(have))
          for (i in seq_len(n)) {
            w <- if (i <= length(want)) want[i] else ""
            h <- if (i <= length(have)) have[i] else ""
            if (!identical(w, h)) {
              add(.ai_rv("K05", output_id = id, sheet = part, row = as.character(i),
                         args = c(id, sub("s$", "", part), i, h, w), cat = cat))
            }
          }
        }
      }
    }
  }
  # K06: numbers where a token belongs
  num <- "[(]N *= *[0-9]+[)]|Page [0-9]+ of [0-9]+"
  for (nm in intersect(names(sheets), c("col_header", "titles", "footnotes", "header", "footer"))) {
    d <- sheets[[nm]]
    for (col in intersect(c("text", "left", "center", "right"), names(d))) {
      hit <- which(!is.na(d[[col]]) & grepl(num, d[[col]]))
      for (i in hit) {
        add(.ai_rv("K06", output_id = d$output_id[i] %||% NA_character_, sheet = nm,
                   row = as.character(i), field = col,
                   args = c(d$output_id[i] %||% "", nm, regmatches(d[[col]][i], regexpr(num, d[[col]][i]))),
                   cat = cat))
      }
    }
  }
  # K07: analyses no table of the workbook uses
  if (!is.null(got$ard) && "table" %in% kinds && !is.null(got$table)) {
    tabs <- got$table$tables$output_id
    rep_types <- if (!is.null(got$report)) got$report$report else NULL
    for (id in setdiff(unique(got$ard$analyses$output_id), tabs)) {
      ty <- if (!is.null(rep_types)) rep_types$type[match(id, rep_types$output_id)] else NA
      if (!is.na(ty) && ty %in% c("figure", "user")) next
      add(.ai_rv("K07", output_id = id, sheet = "analyses", args = id, cat = cat))
    }
  }
  # K14 / K15: the names and levels the ADaM metadata does not have
  if (!is.null(metadata)) .ai_review_metadata(sheets, metadata, add, cat)
  # K08 / K09: the AI's own assumptions
  a <- .ai_review_assumptions(sheets)
  if (is.null(a) || !nrow(a)) {
    add(.ai_rv("K08", args = if (is.null(a)) "no sheet _assumptions" else "the sheet _assumptions is empty",
               cat = cat))
  } else {
    for (i in seq_len(nrow(a))) {
      txt <- paste0(a$item[i], ": ", a$assumption[i],
                    if (nzchar(a$question[i])) paste0(" -- ", a$question[i]))
      add(.ai_rv("K09", output_id = a$output_id[i], sheet = a$sheet[i], row = a$item[i],
                 args = txt, cat = cat))
    }
  }
  # K10-K13: what only the person can sign off
  if (!is.null(got$ard)) {
    an <- got$ard$analyses
    pct <- c("categorical", "dichotomous", "hierarchical", "max", "subjects")
    for (i in which(an$method %in% pct)) {
      den <- an$denominator[i]
      what <- if (is.na(den) || !nzchar(den)) {
        if (an$method[i] %in% c("hierarchical", "max")) "the analysis set" else "the analysed rows"
      } else den
      add(.ai_rv("K10", output_id = an$output_id[i], sheet = "analyses",
                 row = an$analysis_id[i], field = "denominator",
                 args = c(an$output_id[i], an$analysis_id[i], what), cat = cat))
    }
    po <- got$ard$populations
    for (i in seq_len(nrow(po))) {
      add(.ai_rv("K11", sheet = "populations", row = po$population_id[i], field = "where",
                 args = c(po$population_id[i], po$dataset[i],
                          if (is.na(po$where[i])) "no condition" else po$where[i]), cat = cat))
    }
    tests <- c("ttest", "wilcox", "chisq", "fisher", "proportion_ci", "mean_ci", "custom")
    for (i in which(an$method %in% tests | grepl("::", an$method))) {
      add(.ai_rv("K12", output_id = an$output_id[i], sheet = "analyses",
                 row = an$analysis_id[i], field = "method",
                 args = c(an$output_id[i], an$analysis_id[i], an$method[i]), cat = cat))
    }
  }
  if (!is.null(got$table)) {
    v <- got$table$variables
    for (i in which(!is.na(v$levels) & nzchar(v$levels))) {
      add(.ai_rv("K13", output_id = v$output_id[i], sheet = "variables", row = v$variable[i],
                 field = "levels", args = c(v$output_id[i], v$variable[i], v$levels[i]), cat = cat))
    }
    cl <- got$table$codelists
    if (!is.null(cl) && nrow(cl)) {
      for (key in unique(paste(cl$output_id, cl$variable))) {
        d <- cl[paste(cl$output_id, cl$variable) == key, , drop = FALSE]
        d <- d[order(suppressWarnings(as.numeric(d$order))), , drop = FALSE]
        lab <- ifelse(is.na(d$label) | !nzchar(d$label), d$value, paste0(d$value, " = ", d$label))
        add(.ai_rv("K13", output_id = d$output_id[1L], sheet = "codelists", row = d$variable[1L],
                   args = c(d$output_id[1L], d$variable[1L], paste(lab, collapse = " | ")), cat = cat))
      }
    }
  }
  list(rows = rows, kinds = kinds, assumptions = a)
}

.ai_review_yml <- function(path, adam, cat) {
  rows <- list()
  add <- function(r) rows[[length(rows) + 1L]] <<- r
  yml <- tryCatch(yaml::read_yaml(path), error = function(e) {
    add(.ai_rv("S01", sheet = "design", args = conditionMessage(e), cat = cat))
    NULL
  })
  # a report of the YAML spec (spec/outputs/<id>.yml), not a figure design
  if (is.list(yml) && !is.null(yml$id) && !is.null(yml$kind)) {
    return(.ai_review_report(path, cat))
  }
  a <- NULL
  if (!is.null(yml)) {
    d <- tryCatch(tfl_read_fig_design(path), error = function(e) {
      add(.ai_rv("S01", sheet = "design", args = conditionMessage(e), cat = cat))
      NULL
    })
    if (!is.null(d)) {
      pr <- tryCatch(tfl_check_fig_design(d, adam = adam), error = function(e) {
        add(.ai_rv("S01", sheet = "design", args = conditionMessage(e), cat = cat))
        NULL
      })
      if (!is.null(pr) && nrow(pr)) {
        for (i in seq_len(nrow(pr))) {
          add(.ai_rv("S01", sheet = "design", row = pr$part[i], field = pr$field[i],
                     args = pr$problem[i], cat = cat))
        }
      }
    }
    a <- .ai_review_assumptions(yml = yml)
    if (!nrow(a)) {
      add(.ai_rv("K08", args = "no assumptions: list in the design", cat = cat))
    } else {
      for (i in seq_len(nrow(a))) {
        add(.ai_rv("K09", sheet = "design", row = a$item[i], args = a$assumption[i], cat = cat))
      }
    }
  }
  list(rows = rows, kinds = "figure", assumptions = a)
}

# A report of the YAML spec: each content item against the schema (K16 an
# error, K17 a warning: an unknown argument, a version), and the R code it
# holds where the spec has a form of its own for the value (K18: decision 7
# of the YAML spec -- !r only where R is needed)
.ai_review_report <- function(path, cat) {
  rows <- list()
  add <- function(r) rows[[length(rows) + 1L]] <<- r
  o <- tryCatch(tfl_read_output(path), error = function(e) {
    add(.ai_rv("S01", sheet = "report", args = conditionMessage(e), cat = cat))
    NULL
  })
  if (is.null(o)) return(list(rows = rows, kinds = "report", assumptions = NULL))
  id <- o$id
  items <- if (inherits(o$content, "tfl_r")) list() else o$content
  # a figure's named plots: their items, each
  items <- unlist(lapply(items, function(it) if (inherits(it, "tfl_plot")) it$items else list(it)),
                  recursive = FALSE)
  for (i in seq_along(items)) {
    it <- items[[i]]
    p <- tfl_schema_check(it)
    for (j in seq_len(nrow(p))) {
      rule <- if (identical(p$level[j], "error")) "K16" else "K17"
      add(.ai_rv(rule, output_id = id, sheet = "content", row = as.character(i),
                 field = p$arg[j], args = c(id, it$fn, p$message[j]), hint_args = it$fn,
                 cat = cat))
    }
    ent <- tfl_schema(it$fn)
    for (a in names(it$args)) {
      if (!inherits(it$args[[a]], "tfl_r")) next
      # (an argument of `...`: the type the schema gives its values)
      ty <- ent$args[[a]]$type %||%
        if (identical(ent$dots$style, "named")) ent$dots$type
      if (is.null(ty)) next
      own <- .ai_r_alternatives(ty)
      if (length(own)) {
        add(.ai_rv("K18", output_id = id, sheet = "content", row = as.character(i), field = a,
                   args = c(id, paste0(it$fn, "(", a, " = )"), paste(own, collapse = " or ")),
                   cat = cat))
      }
    }
  }
  list(rows = rows, kinds = "report", assumptions = NULL)
}

# the forms other than R code a schema type takes (`map<column, ...>|r` ->
# "map<column, ...>"); none when R is all it takes
.ai_r_alternatives <- function(type) {
  alt <- trimws(.schema_split(type, "|"))
  alt[!alt %in% c("r", "formula", "guard", "call")]
}

# A study folder of the YAML spec (an answer, through tfl_ai_unpack()):
# each report file against the schema (K16-K18); the definition as the
# sheets it stands for (tfl_sheets_from_yaml()) through the workbook's
# review -- the checks against the TOC, the data and the metadata, and the
# person's decisions (K10-K13) -- each finding then named by the file and
# key it is in; the assumptions from spec/_assumptions.yml (K08, K09); the
# figures through the figure designer's check.
.ai_review_folder <- function(dir, toc, adam, others, cat, metadata = NULL) {
  rows <- list()
  add <- function(r) rows[[length(rows) + 1L]] <<- r
  outs <- list.files(file.path(dir, "spec", "outputs"), pattern = "[.]ya?ml$", full.names = TRUE)
  for (f in outs) {
    rr <- .ai_review_report(f, cat)$rows
    for (r in rr) {
      r$sheet <- file.path("spec", "outputs", basename(f))
      add(r)
    }
  }
  b <- tryCatch(suppressMessages(tfl_sheets_from_yaml(dir)), error = function(e) {
    add(.ai_rv("S01", sheet = "spec", args = conditionMessage(e), cat = cat))
    NULL
  })
  kinds <- "study"
  if (!is.null(b)) {
    kinds <- c(if (length(b$ard$analyses) && nrow(b$ard$analyses)) "ard",
               if (nrow(b$table$tables %||% data.frame())) "table",
               if (nrow(b$table$report %||% data.frame())) "report",
               if (nrow(b$listing$listings %||% data.frame())) "listing",
               if (length(b$figures)) "figure")
    f <- tempfile(fileext = ".xlsx")
    on.exit(unlink(f), add = TRUE)
    ok <- tryCatch({
      tfl_write_specs(f, ard = tfl_ard_spec(b$ard), table = tfl_table_spec(b$table),
                      report = tfl_table_spec(b$table), listing = tfl_listing_spec(b$listing))
      TRUE
    }, error = function(e) {
      add(.ai_rv("S01", sheet = "spec", args = conditionMessage(e), cat = cat))
      FALSE
    })
    if (ok) {
      br <- .ai_review_book(f, toc, adam, others, cat, metadata = metadata)$rows
      for (r in br) {
        # the assumptions are the folder's own file
        r <- r[!r$rule %in% c("K01", "K02", "K08", "K09"), , drop = FALSE]
        if (!nrow(r)) next
        r$sheet <- vapply(seq_len(nrow(r)), function(i)
          .ai_yaml_place(r$sheet[i], r$output_id[i]), "")
        add(r)
      }
    }
    for (id in names(b$figures)) {
      # a figure of items is checked item by item against the schema (K16-K18)
      if (!is.null(b$figures[[id]]$content)) next
      pr <- tryCatch(tfl_check_fig_design(.fig_design_from_list(b$figures[[id]]), adam = adam),
                     error = function(e) NULL)
      for (i in seq_len(NROW(pr))) {
        add(.ai_rv("S01", output_id = id, sheet = file.path("spec", "outputs", paste0(id, ".yml")),
                   row = pr$part[i], field = pr$field[i], args = pr$problem[i], cat = cat))
      }
    }
  }
  a <- .ai_folder_assumptions(dir)
  if (!nrow(a)) {
    add(.ai_rv("K08", args = "no spec/_assumptions.yml, or it lists nothing", cat = cat))
  } else {
    for (i in seq_len(nrow(a))) {
      add(.ai_rv("K09", output_id = a$output_id[i], sheet = "spec/_assumptions.yml",
                 row = a$item[i], args = a$assumption[i], cat = cat))
    }
  }
  list(rows = rows, kinds = kinds, assumptions = a)
}

# The file (and key) of a study folder a sheet's row is in: a report's row
# is in its own file, the study's in data.yml (the data), defaults.yml (the
# rest) or study.yml
.ai_yaml_place <- function(sheet, output_id) {
  key <- c(datasets = "datasets", populations = "populations",
           analysis_data = "ard.analysis_data", analyses = "ard.analyses",
           tables = "content", variables = "content", codelists = "codelists",
           cells = "content: plan_cells", digits = "content: plan_digits",
           layout = "content", columns = "content: plan_columns", style = "content: plan_style",
           cell_styles = "content: plan_cell_style", col_header = "content: plan_col_header",
           report = "page", page = "page", header = "page.header", footer = "page.footer",
           titles = "page.titles", footnotes = "page.footnotes", tokens = "page.tokens",
           listings = "data", listing_cols = "content: plan_listing", study = "")
  k <- if (!is.na(sheet) && sheet %in% names(key)) key[[sheet]] else sheet %||% ""
  own <- !is.na(output_id) && nzchar(output_id)
  file <- if (own) file.path("spec", "outputs", paste0(output_id, ".yml"))
          else if (sheet %in% c("datasets", "populations", "records", "objects", "analysis_data")) "spec/data.yml"
          else if (sheet %in% c("study", "")) "study.yml"
          else "spec/defaults.yml"
  if (nzchar(k)) paste0(file, ": ", k) else file
}

# spec/_assumptions.yml as the review's frame: output_id, sheet (where),
# item, assumption, question
.ai_folder_assumptions <- function(dir) {
  empty <- data.frame(output_id = character(), sheet = character(), item = character(),
                      assumption = character(), question = character(),
                      stringsAsFactors = FALSE)
  f <- file.path(dir, "spec", "_assumptions.yml")
  if (!file.exists(f)) return(empty)
  y <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
  if (!is.list(y) || !length(y)) return(empty)
  if (!is.null(y$assumptions)) y <- y$assumptions
  cell <- function(e, k) {
    v <- e[[k]]
    if (is.null(v) || !length(v)) NA_character_ else as.character(v[[1L]])
  }
  rows <- lapply(y, function(e) {
    if (is.character(e)) e <- list(assumption = e)
    data.frame(output_id = cell(e, "output_id"), sheet = cell(e, "where"), item = cell(e, "item"),
               assumption = cell(e, "assumption"), question = cell(e, "question"),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# ---- the two prompts -----------------------------------------------------------------

.ai_where <- function(r) {
  # (a study folder's finding names its file and key, not a sheet)
  yml <- grepl("[.]ya?ml", r$sheet)
  x <- c(if (!is.na(r$output_id) && nzchar(r$output_id) && !yml) r$output_id,
         if (nzchar(r$sheet)) if (yml) r$sheet else paste("sheet", r$sheet),
         if (nzchar(r$row)) paste(if (yml) "item" else "row", r$row),
         if (nzchar(r$field)) paste(if (yml) "key" else "column", r$field))
  if (length(x)) paste(x, collapse = ", ") else "the file"
}

.ai_md_cell <- function(x) gsub("|", "\\|", gsub("[\r\n]+", " ", x), fixed = TRUE)

.ai_repair_text <- function(r, file) {
  r <- r[r$who == "ai", , drop = FALSE]
  if (!nrow(r)) {
    return(c(sprintf("# %s: nothing for the AI to correct", file), "",
             "The review found nothing the assistant can fix from what it has."))
  }
  touched <- unique(r$sheet[nzchar(r$sheet) & r$sheet %in% names(.ai_sheet_columns())])
  sch <- if (length(touched)) tfl_spec_columns(touched) else NULL
  c(sprintf("# Corrections for %s", file), "",
    sprintf(paste0("tflspec's review found %d problem(s) in `%s` that you can fix ",
                   "from the manual, the guide and the files you have."), nrow(r), file),
    "Correct each, then return the whole file again -- every sheet, in the same",
    "form -- for download.  Keep `_assumptions` and add a row for anything you",
    "decide while correcting.", "",
    "| # | Where | Problem | How |", "|---|---|---|---|",
    sprintf("| %d | %s | %s | %s |", seq_len(nrow(r)),
            vapply(seq_len(nrow(r)), function(i) .ai_md_cell(.ai_where(r[i, ])), ""),
            .ai_md_cell(r$message), .ai_md_cell(r$hint)),
    if (!is.null(sch) && nrow(sch)) c(
      "", "## The columns of the sheets above", "",
      "| Sheet | Column | Meaning | Example |", "|---|---|---|---|",
      sprintf("| %s | %s | %s | %s |", sch$sheet, .ai_md_cell(ifelse(is.na(sch$column), "", sch$column)),
              .ai_md_cell(sch$description), .ai_md_cell(ifelse(is.na(sch$example), "", sch$example)))))
}

.ai_questions_text <- function(r, file) {
  r <- r[r$who == "human", , drop = FALSE]
  if (!nrow(r)) {
    return(c(sprintf("# %s: nothing to decide", file), "",
             "The review found nothing only you can decide."))
  }
  blocks <- lapply(seq_len(nrow(r)), function(i) c(
    sprintf("%d. **%s** -- %s", i, .ai_where(r[i, ]), r$message[i]),
    sprintf("   %s", r$hint[i]),
    "   Answer: ",
    ""))
  c(sprintf("# Decisions for %s", file), "",
    "Answer each item after `Answer:` (\"yes\" confirms it), then paste this whole",
    "text into the chat as your next message.", "",
    "> To the assistant: apply these answers to the file, update its",
    "> `_assumptions` (remove what is answered, keep what is not), and return",
    "> the whole file again for download.", "",
    unlist(blocks))
}

# ---- the function ----------------------------------------------------------------------

#' Review the spec an AI assistant wrote
#'
#' Checks what the assistant returned -- a study folder its answer was
#' unpacked into ([tfl_ai_unpack()]), or one file: a workbook holding any of
#' the ARD, table, report and listing specs, a figure design `.yml`, a
#' report file of the YAML spec -- and sorts what it finds by **who acts on
#' it**:
#'
#' * **the AI**: what it can correct from the kit ([tflspec_ai_manual()])
#'   and the TOC: a sheet or column that is not the spec's, an output that
#'   is not the TOC's, a title or footnote that differs from it, `(N=86)`
#'   where `(N={n})` belongs, analyses no table uses, a missing
#'   `_assumptions` sheet, and every error and check of the review
#'   ([tfl_review_spec()], [tfl_check_fig_design()]);
#' * **the person**: what only they can decide -- each assumption the
#'   assistant listed, each analysis set's flag, each percentage's
#'   denominator, each test or model, each fixed order of levels -- and
#'   the review's items that are set by hand.
#'
#' The first half is written as a prompt to paste back into the same chat
#' (`<file>_repair.md`), the second as a checklist the person answers and
#' pastes back (`<file>_questions.md`), next to the file -- a study
#' folder's in the folder, named after it.  The kit's rules
#' and who acts on each review rule are `inst/review/ai_rules.csv`.
#'
#' @param file A study folder of the YAML spec (each report file against
#'   the schema; the definition through the workbook's checks, each finding
#'   named by its file and key; `spec/_assumptions.yml`), a spec workbook
#'   (`.xlsx`), a figure design (`.yml`), or one report of the YAML spec
#'   (`spec/outputs/<id>.yml`: its items against the schema, and its `!r`
#'   where the schema has a form of its own).
#' @param toc The TOC the specs were written from: a [tfl_read_toc()]
#'   result, or a list of its arguments (`path`, `map`, `sheet`, `skip`).
#'   With it, the outputs, titles and footnotes are checked against it.
#' @param adam The study's data ([tfl_read_adam()]), for the checks against
#'   the data; `NULL` for none.
#' @param metadata The study's ADaM metadata ([tfl_adam_metadata()], or
#'   the kit's `08_adam_metadata.csv` read back), for the checks of the
#'   names a workbook writes: a dataset or variable it does not have is the
#'   AI's to fix (K14, with the nearest names), and so is a code list value
#'   that is not among a variable's levels (K15).  `NULL` for none.
#' @param others Specs of the same study already read, for the checks
#'   across specs when the file holds only some: a list with any of `ard`
#'   ([tfl_read_ard_spec()]), `table` ([tfl_read_report_spec()]) and
#'   `listing` ([tfl_read_listing_spec()]).
#' @param write Write `<file>_repair.md` and `<file>_questions.md` next to
#'   the file.
#' @param overwrite Write over them if they are there.
#' @return A `tfl_ai_review`: the rows of [tfl_review_spec()] plus `who`
#'   (`ai` or `human`), with attributes `file`, `kinds`, `assumptions` (the
#'   assistant's own list), `repair` and `questions` (the two texts) and
#'   `paths` (the files written).  Invisibly when `write = TRUE`.
#' @examples
#' \donttest{
#' kit <- tflspec_ai_manual(file.path(tempdir(), "kit-review"), overwrite = TRUE)
#' # the example's answer, unpacked into a study folder
#' st <- file.path(tempdir(), "XYZ-001")
#' tfl_ai_unpack(kit[basename(kit) == "06_example_spec.md"], st, overwrite = TRUE)
#' r <- tfl_ai_review(st, write = FALSE)
#' summary(r)
#' head(as.data.frame(r)[r$who == "human", c("output_id", "rule", "message")])
#' }
#' @seealso [tflspec_ai_manual()] for the kit the assistant works from.
#' @export
tfl_ai_review <- function(file, toc = NULL, adam = NULL, others = NULL,
                          metadata = NULL, write = TRUE, overwrite = FALSE) {
  if (!is.character(file) || length(file) != 1L || !file.exists(file)) {
    .ard_stop("tfl_ai_review(): `file` is one spec file that exists (.xlsx or .yml), or a study folder.")
  }
  cat <- .ai_cat()
  ext <- tolower(tools::file_ext(file))
  if (dir.exists(file)) ext <- "folder"
  res <- switch(ext,
    folder = .ai_review_folder(file, toc, adam, others, cat,
                               metadata = .ai_review_md(metadata)),
    xlsx = .ai_review_book(file, toc, adam, others, cat,
                           metadata = .ai_review_md(metadata)),
    yml = , yaml = .ai_review_yml(file, adam, cat),
    .ard_stop("tfl_ai_review(): `file` is a workbook (.xlsx), or a figure design or a report (.yml)."))
  rows <- res$rows
  r <- if (length(rows)) do.call(rbind, rows) else .review_empty()
  r <- r[!duplicated(paste(r$rule, r$output_id, r$message)), , drop = FALSE]
  who <- .ai_rules()
  r$who <- who$who[match(r$rule, who$rule)]
  r$who[is.na(r$who)] <- ifelse(r$level[is.na(r$who)] == "hand", "human", "ai")
  r <- r[order(match(r$who, c("ai", "human")), match(r$level, .review_levels)), , drop = FALSE]
  rownames(r) <- NULL
  base <- basename(file)
  rep_txt <- .ai_repair_text(r, base)
  q_txt <- .ai_questions_text(r, base)
  paths <- character()
  if (isTRUE(write)) {
    # a study folder's prompts in the folder, named after it
    stem <- if (identical(ext, "folder")) file.path(file, basename(normalizePath(file)))
            else sub("[.][^.]+$", "", file)
    paths <- c(repair = paste0(stem, "_repair.md"), questions = paste0(stem, "_questions.md"))
    there <- paths[file.exists(paths)]
    if (length(there) && !isTRUE(overwrite)) {
      .ard_stop(sprintf("tfl_ai_review(): %s already there; pass `overwrite = TRUE`.",
                        paste(basename(there), collapse = ", ")))
    }
    .ai_write_text(rep_txt, paths[["repair"]])
    .ai_write_text(q_txt, paths[["questions"]])
  }
  out <- structure(r, class = c("tfl_ai_review", "data.frame"), file = base,
                   kinds = res$kinds, assumptions = res$assumptions,
                   repair = rep_txt, questions = q_txt, paths = paths)
  if (isTRUE(write)) {
    print(out)
    return(invisible(out))
  }
  out
}

#' @export
print.tfl_ai_review <- function(x, ...) {
  k <- attr(x, "kinds")
  cat(sprintf("tfl_ai_review: %s (%s)\n", attr(x, "file"), paste(k, collapse = ", ")))
  part <- function(who, title, file) {
    d <- x[x$who == who, , drop = FALSE]
    cat(sprintf("\n%s (%d item%s)%s\n", title, nrow(d), if (nrow(d) == 1L) "" else "s",
                if (nrow(d) && nzchar(file)) paste0(" -- ", file) else ""))
    for (i in seq_len(min(nrow(d), 20L))) {
      cat(sprintf("  %-5s %s: %s\n", d$level[i], .ai_where(d[i, ]),
                  substr(d$message[i], 1L, 110L)))
    }
    if (nrow(d) > 20L) cat(sprintf("  ... and %d more\n", nrow(d) - 20L))
  }
  p <- attr(x, "paths")
  part("ai", "For the AI: paste back into the chat",
       if (length(p)) basename(p[["repair"]]) else "")
  part("human", "For you: answer, then paste back",
       if (length(p)) basename(p[["questions"]]) else "")
  invisible(x)
}

#' @export
summary.tfl_ai_review <- function(object, ...) {
  tab <- table(factor(object$who, c("ai", "human")),
               factor(object$level, .review_levels))
  names(dimnames(tab)) <- c("who", "level")
  tab
}
