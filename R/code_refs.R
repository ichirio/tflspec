#' The names a SPEC's R code uses
#'
#' Which of `names` each piece of R code reads -- a `where`, a `derive`, an
#' analysis's `args` or `code`, an `!r` value.  For a screen that renames a
#' data or an object: the code that still says the old name is found, to
#' warn of it (the code is not changed).  The code is parsed and its symbols
#' read, so a name inside a string or a comment is not one; code that does
#' not parse is searched for the names as whole words.
#'
#' @param code A character vector of R code (or a list of [tfl_r()] values).
#' @param names The names to look for (data, objects).
#' @return A data frame, one row a use: `code` (the element's position in
#'   `code`, or its name when `code` is named) and `name`.  No rows when
#'   none is used.
#' @examples
#' tfl_code_refs(c(where = "AGE >= 65 & USUBJID %in% adsl_sub$USUBJID",
#'                 derive = "n_jp / 2"),
#'               c("adsl_sub", "n_jp", "adae_sub"))
#' @export
tfl_code_refs <- function(code, names) {
  if (is.list(code)) code <- vapply(code, function(x) paste(as.character(x), collapse = "\n"), "")
  if (!is.character(code) || !is.character(names)) {
    .ard_stop("tfl_code_refs(): `code` and `names` are character vectors.")
  }
  at <- if (!is.null(base::names(code)) && all(nzchar(base::names(code)))) base::names(code)
        else as.character(seq_along(code))
  rows <- lapply(seq_along(code), function(i) {
    x <- code[[i]]
    if (is.na(x) || !nzchar(trimws(x))) return(NULL)
    e <- tryCatch(parse(text = x, keep.source = FALSE), error = function(e) NULL)
    used <- if (!is.null(e)) {
      intersect(names, all.names(e))
    } else {
      # (not R yet: the names as whole words)
      names[vapply(names, function(n) grepl(paste0("(^|[^A-Za-z0-9._])", gsub(".", "\\.", n, fixed = TRUE),
                                                  "($|[^A-Za-z0-9._])"), x), NA)]
    }
    if (length(used)) data.frame(code = at[i], name = used, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(code = character(), name = character(),
                                          stringsAsFactors = FALSE)), rows))
  rownames(out) <- NULL
  out
}
