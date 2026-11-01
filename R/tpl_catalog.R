# ============================================================================
#  The templates' catalog: other collections' items, each an alias
# ----------------------------------------------------------------------------
#  A template is one program; the tables and figures of a published
#  collection -- FDA's Standard Safety Tables and Figures IG v2, PhUSE CSS's
#  white papers, CDISC ARS's examples -- are items of a catalog
#  (inst/templates/catalog.csv), each with its collection (`source`), its
#  number there, a short title of our own, an alias (stf_t14_ae_soc_pt) and
#  the template that writes it (none yet: `status` missing).  An alias runs
#  its template wherever a template's name is taken -- tfl_tpl(), a report's
#  `template: {name: }`, the template sheet -- as itself; a report keeps
#  the name it was written with.  tfl_templates(source = ) lists a
#  collection's every item.  One template, any number of aliases: a
#  variant (its own defaults) is made only where the defaults must differ.
# ============================================================================

# The collections a catalog item may come from
.tpl_sources <- c("tflspec", "FDA STF IG v2", "PhUSE CSS", "CDISC ARS")

.tpl_catalog_cache <- new.env(parent = emptyenv())

# The catalog: one row an item (source, item, title, alias, template,
# status, note), read once
.tpl_catalog <- function() {
  if (!is.null(.tpl_catalog_cache$x)) return(.tpl_catalog_cache$x)
  cols <- c("source", "item", "title", "alias", "template", "status", "note")
  f <- system.file("templates", "catalog.csv", package = "tflspec")
  x <- if (nzchar(f)) {
    utils::read.csv(f, colClasses = "character", na.strings = "", encoding = "UTF-8",
                    stringsAsFactors = FALSE)
  } else NULL
  if (is.null(x) || !all(cols %in% names(x))) {
    x <- stats::setNames(as.data.frame(replicate(length(cols), character()),
                                       stringsAsFactors = FALSE), cols)
  }
  x <- x[cols]
  .tpl_catalog_cache$x <- x
  x
}

# The catalog item an alias names, or NULL
.tpl_alias_item <- function(name) {
  cat <- .tpl_catalog()
  at <- which(!is.na(cat$alias) & cat$alias == name)
  if (length(at)) cat[at[1L], , drop = FALSE] else NULL
}

# The aliases of a template: a data frame of its catalog items
.tpl_aliases_of <- function(name) {
  cat <- .tpl_catalog()
  cat[!is.na(cat$template) & cat$template == name & !is.na(cat$alias) & cat$alias != name, ,
      drop = FALSE]
}

# A template's aliases on one line, "stf_t14_ae_soc_pt (FDA STF IG v2 T14)",
# or NULL
.tpl_aliases_line <- function(name) {
  a <- .tpl_aliases_of(name)
  if (!nrow(a)) return(NULL)
  paste(sprintf("`%s` (%s %s)", a$alias, a$source, a$item), collapse = ", ")
}

# The registry's template an alias runs; an error for an item no template
# writes yet; NULL for a name that is no alias
.tpl_alias_target <- function(name, fn) {
  a <- .tpl_alias_item(name)
  if (is.null(a)) return(NULL)
  if (is.na(a$template)) {
    .ard_stop(sprintf(paste0("%s: %s is %s %s (%s) in the templates' catalog; no template ",
                             "writes it yet (tfl_templates(source = \"%s\"))."),
                      fn, sQuote(name, FALSE), a$source, a$item, a$title, a$source))
  }
  a$template
}

# A collection's items as tfl_templates(source = ) lists them
.tpl_catalog_rows <- function(source) {
  if (identical(source, "tflspec")) {
    tt <- .tpl_registry_table()
    return(data.frame(source = "tflspec", item = NA_character_, title = tt$label,
                      alias = NA_character_, template = tt$name, status = "same",
                      kind = tt$kind, note = NA_character_, stringsAsFactors = FALSE))
  }
  cat <- .tpl_catalog()
  x <- cat[cat$source == source, , drop = FALSE]
  kind <- vapply(seq_len(nrow(x)), function(i) {
    if (!is.na(x$template[i]) && exists(x$template[i], envir = .tpl_registry, inherits = FALSE)) {
      .tpl_get(x$template[i], "tfl_templates()")$kind %||% NA_character_
    } else if (grepl("^F", x$item[i]) && identical(source, "FDA STF IG v2")) "figure" else
      NA_character_
  }, "")
  data.frame(source = x$source, item = x$item, title = x$title, alias = x$alias,
             template = x$template, status = x$status, kind = kind, note = x$note,
             stringsAsFactors = FALSE)
}

# ---- the collections' sources -------------------------------------------------

# The publications the catalog's collections come from (one row a
# collection: its document, version, date, URL, terms, the date we read it),
# from inst/templates/sources.csv
.tpl_sources_table <- function() {
  if (!is.null(.tpl_catalog_cache$src)) return(.tpl_catalog_cache$src)
  cols <- c("source", "document", "version", "date", "url", "terms", "accessed")
  f <- system.file("templates", "sources.csv", package = "tflspec")
  x <- if (nzchar(f)) {
    utils::read.csv(f, colClasses = "character", na.strings = "", encoding = "UTF-8",
                    stringsAsFactors = FALSE)
  } else NULL
  if (is.null(x) || !all(cols %in% names(x))) {
    x <- stats::setNames(as.data.frame(replicate(length(cols), character()),
                                       stringsAsFactors = FALSE), cols)
  }
  x <- x[cols]
  .tpl_catalog_cache$src <- x
  x
}

# A collection's publications cited, one line each (Markdown); NULL for none
.tpl_cite <- function(source) {
  x <- .tpl_sources_table()
  x <- x[x$source %in% source, , drop = FALSE]
  if (!nrow(x)) return(NULL)
  na0 <- function(v) ifelse(is.na(v), "", v)
  sprintf("%s: %s%s%s. <%s> (accessed %s).%s", x$source, x$document,
          ifelse(is.na(x$version), "", paste0(", ", x$version)),
          ifelse(is.na(x$date), "", paste0(", ", x$date)), na0(x$url), na0(x$accessed),
          ifelse(is.na(x$terms), "", paste0(" ", x$terms)))
}

# The organisations behind collections, for the sentence that none of them
# endorses tflspec's templates
.tpl_source_orgs <- c("FDA STF IG v2" = "FDA", "PhUSE CSS" = "PhUSE", "CDISC ARS" = "CDISC")

# That sentence, for the collections named (NULL for tflspec's own alone)
.tpl_not_endorsed <- function(source) {
  orgs <- unique(stats::na.omit(.tpl_source_orgs[intersect(source, names(.tpl_source_orgs))]))
  if (!length(orgs)) return(NULL)
  who <- if (length(orgs) == 1L) orgs else
    paste(paste(orgs[-length(orgs)], collapse = ", "), "or", orgs[length(orgs)])
  sprintf(paste("The aliases follow the numbering of these publications, and the titles are",
                "short descriptions of our own, not their text. The templates are tflspec's,",
                "not endorsed or recommended by %s."), who)
}

# tfl_templates()'s section of the collections and their sources (roxygen
# @eval)
.tpl_rd_sources <- function() {
  x <- .tpl_sources_table()
  if (!nrow(x)) return(character())
  paste0("@section Sources:\nThe catalog's collections, as published:\n\n",
         paste(paste0("* ", .tpl_cite(x$source)), collapse = "\n"), "\n\n",
         .tpl_not_endorsed(x$source))
}
