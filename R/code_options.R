#' How the generated programs name folders and packages
#'
#' Two options make the code tflspec writes fit a study's setup file (such
#' as tflplanner's `programs/study_setup.R`), which defines the study's
#' folders as variables and attaches the packages every program uses.
#'
#' * `tflspec.paths`: a named character vector, a variable for a folder,
#'   relative to the study folder -- `c(path_adam = "data/adam", path_ard =
#'   "output/ard")`.  A file under one of them is written through the
#'   variable: `readRDS(file.path(path_adam, "adsl.rds"))` instead of
#'   `readRDS("data/adam/adsl.rds")`.  The folder that holds it most
#'   closely is the one used.
#' * `tflspec.attached`: the packages the setup attaches with `library()`
#'   -- `c("cards", "dplyr")`.  Their functions are called without `pkg::`
#'   in the program, the code of the spec it carries included (only the
#'   code: a string or a comment keeps what it says), and an ARD program
#'   does not write `library(cards)` itself.  A package not listed keeps
#'   its prefix (`cardx::`).
#'
#' Unset (the default), the code is as it has always been: literal paths
#' and `pkg::` on every call.  The functions that write code read them:
#' [tfl_ard_code()], [tfl_build_ard()], [tfl_table_code()],
#' [tfl_report_code()], [tfl_report_setup_code()], [tfl_listing_code()],
#' [tfl_read_data_code()], [tfl_fig_design_code()] and
#' [tfl_fig_setup_code()].
#'
#' @examples
#' sp <- tfl_ard_spec(list(
#'   study = data.frame(key = "id", value = "USUBJID"),
#'   datasets = data.frame(dataset = "ADSL", path = "data/adam/adsl.rds"),
#'   populations = data.frame(population_id = "SAF", dataset = "ADSL",
#'                            where = "SAFFL == \"Y\""),
#'   analyses = data.frame(output_id = "T1", analysis_id = "AGE",
#'                         method = "continuous", population_id = "SAF",
#'                         by = "TRT01A", variables = "AGE")))
#' old <- options(tflspec.paths = c(path_adam = "data/adam"),
#'                tflspec.attached = "cards")
#' cat(tfl_ard_code(sp, part = "body"), sep = "\n")
#' options(old)
#' @name tflspec_code_options
NULL

# A path as the program writes it: through the variable of the folder that
# holds it (option tflspec.paths), else as it is
.path_code <- function(path) {
  q <- encodeString(path, quote = "\"")
  pv <- getOption("tflspec.paths")
  if (!length(pv) || is.null(names(pv)) || is.na(path)) return(q)
  dirs <- sub("/+$", "", as.character(pv))
  hit <- which(nzchar(dirs) & (startsWith(path, paste0(dirs, "/")) | path == dirs))
  if (!length(hit)) return(q)
  k <- hit[which.max(nchar(dirs[hit]))]
  rest <- substring(path, nchar(dirs[k]) + 2L)
  if (!nzchar(rest)) return(names(pv)[k])
  sprintf("file.path(%s, %s)", names(pv)[k], encodeString(rest, quote = "\""))
}

# the packages the study's setup attaches (option tflspec.attached)
.attached <- function() {
  a <- getOption("tflspec.attached")
  if (is.null(a)) character() else as.character(a)
}

# The code without `pkg::` for the packages attached: the `pkg::` of each
# call in the parsed code (a string or a comment keeps its text).  The
# lines keep their elements (an element may hold several lines); code that
# does not parse is left as it is.
.drop_attached_ns <- function(code) {
  pk <- .attached()
  if (!length(pk) || !length(code)) return(code)
  parts <- lapply(code, function(e) {
    if (is.na(e)) return(NA_character_)
    l <- strsplit(e, "\n", fixed = TRUE)[[1L]]
    if (!length(l)) l <- ""
    if (endsWith(e, "\n")) l <- c(l, "")
    l
  })
  lines <- unlist(parts)
  if (anyNA(lines)) return(code)
  pd <- tryCatch(utils::getParseData(parse(text = lines, keep.source = TRUE)),
                 error = function(e) NULL)
  if (is.null(pd) || !nrow(pd)) return(code)
  pd <- pd[pd$terminal, , drop = FALSE]
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  pkg <- which(pd$token == "SYMBOL_PACKAGE" & pd$text %in% pk)
  # `pkg::name` (not `pkg:::name`): the token after it is NS_GET
  pkg <- pkg[pkg < nrow(pd) & pd$token[pkg + 1L] == "NS_GET"]
  if (!length(pkg)) return(code)
  cut <- data.frame(line = pd$line1[pkg], from = pd$col1[pkg],
                    to = pd$col2[pkg + 1L])
  cut <- cut[order(cut$line, -cut$from), , drop = FALSE]
  for (i in seq_len(nrow(cut))) {
    s <- lines[cut$line[i]]
    lines[cut$line[i]] <- paste0(substr(s, 1L, cut$from[i] - 1L),
                                 substr(s, cut$to[i] + 1L, nchar(s)))
  }
  n <- lengths(parts)
  ends <- cumsum(n)
  starts <- ends - n + 1L
  out <- vapply(seq_along(parts), function(i)
    paste(lines[starts[i]:ends[i]], collapse = "\n"), "")
  attributes(out) <- attributes(code)
  out
}
