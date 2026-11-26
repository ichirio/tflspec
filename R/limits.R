# ============================================================================
#  What rtfreporter cannot do yet: one table, read at the content step
# ----------------------------------------------------------------------------
#  inst/spec/limits.yml holds one row per rtfreporter issue.  A table's
#  content that meets a row's `when` stops, with the row's message, its
#  issue and how to write it today (`instead`): never a broken RTF.  The
#  run and the written program both make their pipeline through
#  .content_table_pipeline(), which calls .content_limits(), so neither
#  route can miss it.  (decided 2026-11-21: stop, not warn)
# ============================================================================

.limits_words <- c("item", "arg", "is", "count", "all", "any")

# the table, read once
.limits_table <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      f <- system.file("spec", "limits.yml", package = "tflspec")
      cache <<- if (nzchar(f)) yaml::read_yaml(f)$limits %||% list() else list()
    }
    cache
  }
})

# an argument's values, a "|" in a string separating them (as cols are written)
.limits_values <- function(v) {
  v <- unlist(v, use.names = FALSE)
  if (is.character(v)) v <- trimws(unlist(strsplit(v, "|", fixed = TRUE)))
  v[!is.na(v) & nzchar(as.character(v))]
}

# does the content meet a `when` (one condition, or all: / any: of them)?
.limits_meets <- function(when, content, id) {
  bad <- setdiff(names(when), .limits_words)
  if (length(bad)) {
    stop(sprintf("inst/spec/limits.yml: %s: unknown word %s; the words are %s.", id,
                 paste(bad, collapse = ", "), paste(.limits_words, collapse = ", ")),
         call. = FALSE)
  }
  if (!is.null(when$all)) return(all(vapply(when$all, .limits_meets, NA, content = content, id = id)))
  if (!is.null(when$any)) return(any(vapply(when$any, .limits_meets, NA, content = content, id = id)))
  its <- Filter(function(it) identical(.schema_fn_name(it$fn), when$item), content)
  if (!length(its)) return(FALSE)
  if (is.null(when$arg)) return(TRUE)
  any(vapply(its, function(it) {
    v <- .limits_values(it$args[[when$arg]])
    if (!is.null(when$is)) return(any(as.character(v) %in% as.character(unlist(when$is))))
    if (!is.null(when$count)) {
      m <- regmatches(when$count, regexec("^[[:space:]]*(>=|<=|>|<|==)?[[:space:]]*([0-9]+)[[:space:]]*$", when$count))[[1L]]
      op <- if (nzchar(m[2L])) m[2L] else "=="
      return(do.call(op, list(length(v), as.integer(m[3L]))))
    }
    length(v) > 0L
  }, NA))
}

# The stop message, one form for every row (the row's ja text is for
# tflplanner's screen, which reads the table too)
.limits_message <- function(row, report = NULL) {
  sprintf("%s%s (%s). Instead: %s", if (!is.null(report)) paste0(report, ": ") else "",
          row$message$en, row$issue, row$instead$en)
}

# Stop when a table's content meets a row the installed rtfreporter has not
# fixed.  `version`: the rtfreporter version (the installed one).
.content_limits <- function(content, report = NULL, rows = .limits_table(),
                            version = utils::packageVersion("rtfreporter")) {
  for (row in rows) {
    if (!is.null(row$fixed_in) && version >= package_version(as.character(row$fixed_in))) next
    if (.limits_meets(row$when, content, row$id)) {
      stop(.limits_message(row, report), call. = FALSE)
    }
  }
  invisible(content)
}
