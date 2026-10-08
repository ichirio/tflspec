# The ARD program as a person writes it: the layout of a call, the helpers
# of the setup, and the program's lines (data, analysis data, analyses)

# A call as the tidyverse style writes it: on one line when it fits in 80
# characters, else an argument a line, two spaces in, the closing bracket
# on a line of its own.  An argument is a string (written as it is) or a
# call of its own, laid out the same way; a named one is `name = value`.
.cl <- function(fn, args = list()) {
  structure(list(fn = fn, args = as.list(args)), class = "tfl_cl")
}

# how wide a line will be once the attached packages' `pkg::` go
# (.drop_attached_ns())
.vis_width <- function(s) {
  pk <- .attached()
  if (length(pk)) {
    s <- gsub(sprintf("\\b(%s)::(?!:)", paste(pk, collapse = "|")), "", s,
              perl = TRUE)
  }
  max(c(0L, nchar(strsplit(s, "\n", fixed = TRUE)[[1L]])))
}

# A call (.cl()) as code; `lead`: what comes before it on its first line
.lay <- function(x, lead = 0L, width = 80L) {
  if (!inherits(x, "tfl_cl")) return(x)
  a <- x$args
  nm <- names(a) %||% rep("", length(a))
  pre <- ifelse(nzchar(nm), paste0(.r_arg_name(nm), " = "), "")
  flat <- vapply(seq_along(a), function(i)
    paste0(pre[i], .lay(a[[i]], width = Inf)), "")
  one <- paste0(x$fn, "(", paste(flat, collapse = ", "), ")")
  if (!length(a) || (!grepl("\n", one, fixed = TRUE) &&
                     lead + .vis_width(one) <= width)) {
    return(one)
  }
  parts <- vapply(seq_along(a), function(i)
    gsub("\n", "\n  ", paste0(pre[i], .lay(a[[i]], 2L + nchar(pre[i]), width)),
         fixed = TRUE), "")
  paste0(x$fn, "(\n  ", paste(parts, collapse = ",\n  "), "\n)")
}

# `name <- head |> step |> ...`: a step a line, two spaces in
.pipe_code <- function(name, head, steps = list()) {
  steps <- Filter(Negate(is.null), steps)
  first <- .lay(head, lead = nchar(name) + 4L)
  if (!length(steps)) return(paste(name, "<-", first))
  s <- vapply(steps, function(st)
    gsub("\n", "\n  ", .lay(st, lead = 2L), fixed = TRUE), "")
  paste0(name, " <- ", first, " |>\n  ", paste(s, collapse = " |>\n  "))
}

# strings as R: "a", "b"
.q <- function(v) encodeString(v, quote = "\"")

# c("a", "b") (one: "a")
.c_str <- function(v) if (length(v) == 1L) .q(v) else .cl("c", .q(v))

# The arguments a person wrote (`args`), one string each
.split_args <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(character())
  ch <- strsplit(x, "", fixed = TRUE)[[1L]]
  depth <- 0L
  quote <- character()
  cut <- integer()
  for (i in seq_along(ch)) {
    if (length(quote)) {
      if (ch[i] == quote && (i == 1L || ch[i - 1L] != "\\")) quote <- character()
      next
    }
    if (ch[i] %in% c("\"", "'", "`")) quote <- ch[i]
    else if (ch[i] %in% c("(", "[", "{")) depth <- depth + 1L
    else if (ch[i] %in% c(")", "]", "}")) depth <- depth - 1L
    else if (ch[i] == "," && depth == 0L) cut <- c(cut, i)
  }
  parts <- trimws(substring(x, c(1L, cut + 1L), c(cut - 1L, nchar(x))))
  parts[nzchar(parts)]
}

# The terminal tokens of R code, in their order (NULL when it does not
# parse)
.code_tokens <- function(code) {
  pd <- tryCatch(utils::getParseData(parse(text = code, keep.source = TRUE)),
                 error = function(e) NULL)
  if (is.null(pd) || !nrow(pd)) return(NULL)
  pd <- pd[pd$terminal, , drop = FALSE]
  pd[order(pd$line1, pd$col1), , drop = FALSE]
}

# R code with the names `names(map)` written as `map`: the symbols of the
# parsed code only (an argument's name, `x$name`, a string or a comment
# keep theirs)
.rename_symbols <- function(code, map) {
  if (is.na(code) || !nzchar(code)) return(code)
  tk <- .code_tokens(code)
  if (is.null(tk)) return(code)
  prev <- c("", tk$token[-nrow(tk)])
  hit <- which(tk$token == "SYMBOL" & tk$text %in% names(map) &
                 !prev %in% c("'$'", "'@'"))
  if (!length(hit)) return(code)
  lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
  hit <- hit[order(tk$line1[hit], -tk$col1[hit])]
  for (i in hit) {
    s <- lines[tk$line1[i]]
    lines[tk$line1[i]] <- paste0(substr(s, 1L, tk$col1[i] - 1L),
                                 map[[tk$text[i]]],
                                 substr(s, tk$col2[i] + 1L, nchar(s)))
  }
  paste(lines, collapse = "\n")
}

# The names R code assigns, or NA when it defines a function or assigns
# with <<- (code that is best run in a scope of its own)
.code_assigns <- function(code) {
  tk <- .code_tokens(code)
  if (is.null(tk)) return(NA)
  if (any(tk$token == "FUNCTION") || any(tk$text %in% c("<<-", "->>"))) {
    return(NA)
  }
  n <- nrow(tk)
  left <- which(tk$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))
  right <- which(tk$token == "RIGHT_ASSIGN")
  unique(c(tk$text[left[left > 1L] - 1L], tk$text[right[right < n] + 1L]))
}

# The code of a `custom` analysis (or an analysis data's `code`) as the
# program's own lines: everything before its last expression as it is, the
# last made `name` (through `steps`).  NULL when that does not read well:
# the code assigns a name the program has (`avoid`), defines a function,
# or ends in an assignment -- then it runs in local() as before.
.code_as_program <- function(code, name, steps, avoid) {
  as <- .code_assigns(code)
  if (anyNA(as) || any(as %in% avoid)) return(NULL)
  ex <- tryCatch(parse(text = code, keep.source = TRUE), error = function(e) NULL)
  if (!length(ex)) return(NULL)
  last <- ex[[length(ex)]]
  if (is.call(last) && as.character(last[[1L]])[1L] %in% c("<-", "=", "<<-")) {
    return(NULL)
  }
  sr <- attr(ex, "srcref")[[length(ex)]]
  lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
  l1 <- sr[1L]
  l2 <- sr[3L]
  # another expression before it on its line: as it is
  if (nzchar(trimws(substr(lines[l1], 1L, sr[5L] - 1L)))) return(NULL)
  seg <- lines[l1:l2]
  after <- trimws(substring(seg[length(seg)], sr[6L] + 1L))
  seg[length(seg)] <- substr(seg[length(seg)], 1L, sr[6L])
  seg[1L] <- substring(seg[1L], sr[5L])
  head <- paste(seg, collapse = "\n")
  fn <- if (is.call(last)) last[[1L]]
  plain <- is.name(last) || (is.call(last) && (is.call(fn) ||
    (is.name(fn) && grepl("^[A-Za-z.]", as.character(fn)))))
  if (!plain) head <- paste0("(", head, ")")
  # what follows it (a comment) goes above
  c(if (l1 > 1L) lines[seq_len(l1 - 1L)],
    if (nzchar(after)) after,
    if (l2 < length(lines)) lines[(l2 + 1L):length(lines)],
    .pipe_code(name, head, steps))
}

# ---- the setup's helpers -------------------------------------------------

# the names the setup gives (a study's code does not assign them)
.ard_helper_names <- c("tag_ard", "set_levels", "fmt_default", "fmt_with",
                       "fmt_pvalue", "fmt_ard", "keep_stats", "tfl_stats")

# set_levels(): a data's columns of the code lists as factors
.set_levels_lines <- function() {
  c("# the code lists: each listed column a factor in their order (a value",
    "# the list does not have after them), so the ARD keeps the order and",
    "# counts a level no record has (0)",
    "set_levels <- function(data, codelists) {",
    "  for (v in intersect(names(codelists), names(data))) {",
    "    x <- data[[v]]",
    "    if (!is.character(x) && !is.factor(x)) next",
    "    seen <- sort(unique(as.character(x[!is.na(x)])))",
    "    data[[v]] <- factor(as.character(x),",
    "                        levels = unique(c(codelists[[v]], seen)))",
    "  }",
    "  data",
    "}")
}

# tag_ard(): the ids in front
.tag_ard_lines <- function() {
  c("# the ids in front: the report, the analysis and its analysis set.  A",
    "# cards ARD stays one (class card), so the study ARD is one too and",
    "# cards' own tools (as_nested_list(), compare_ard()) take it",
    "tag_ard <- function(ard, output_id, analysis_id,",
    "                    population = NA_character_, analyses = NULL) {",
    "  # analyses run together (cards::ard_stack()): the rows of each one's",
    "  # variables are its own, the rest (the by counts, the total N)",
    "  # `analysis_id`'s",
    "  if (length(analyses)) {",
    "    own <- rep(names(analyses), lengths(analyses))",
    "    id <- own[match(as.character(ard$variable), unlist(analyses))]",
    "    analysis_id <- ifelse(is.na(id), analysis_id, id)",
    "  }",
    "  if (inherits(ard, \"card\")) {",
    "    return(dplyr::mutate(ard, output_id = output_id,",
    "                         analysis_id = analysis_id,",
    "                         population_id = population, .before = 1L))",
    "  }",
    "  ard <- as.data.frame(ard)",
    "  cbind(output_id = output_id, analysis_id = analysis_id,",
    "        population_id = population, ard, stringsAsFactors = FALSE)",
    "}")
}

# library(cards), the study's own functions (its key `source`), tag_ard(),
# set_levels() (`levels`: the program has code lists) and the helpers (the
# computed statistics `used`, stat_fmt)
.ard_common_lines <- function(used, x = NULL, levels = TRUE) {
  src <- .split_bar(.study_value(x, "source", NA))
  c(if (!"cards" %in% .attached()) "library(cards)",
    if (length(src)) c(
      "# the study's own analysis functions (study key `source`)",
      sprintf("source(%s)", vapply(src, .path_code, ""))),
    "",
    .tag_ard_lines(),
    "",
    if (levels) c(.set_levels_lines(), ""),
    .ard_helpers(used))
}

# the helpers every ARD program starts with: the computed statistics it
# uses, and stat_fmt
.ard_helpers <- function(used) {
  st <- tfl_ard_statistics()
  cst <- st[st$kind == "continuous" & !is.na(st$fun) & st$statistic %in% used, ]
  dflt <- st[!duplicated(st$statistic) & !is.na(st$fmt), ]
  dflt <- stats::setNames(dflt$fmt, dflt$statistic)
  c(if (nrow(cst)) c(
      "# statistics cards does not compute itself (company standards)",
      paste("tfl_stats <-", .lay(.cl("list", stats::setNames(as.list(cst$fun),
                                                             cst$statistic)),
                                 lead = 13L)),
      ""),
    "# stat_fmt: each statistic's format, as cards' fmt_fun takes it -- an",
    "# integer is that many decimals, label_round(1, scale = 100) a proportion",
    "# as a percent, fmt_pvalue() <0.001 or 3 decimals",
    "fmt_pvalue <- function(x) ifelse(x < 0.001, \"<0.001\", sprintf(\"%.3f\", x))",
    paste("fmt_default <-", .lay(.cl("list", stats::setNames(
      as.list(vapply(dflt, .fmt_r, "")), names(dflt))), lead = 15L)),
    "# the defaults with an analysis's own: fmt_fun = everything() ~ fmt_with(mean = 2L)",
    "fmt_with <- function(...) utils::modifyList(fmt_default, list(...))",
    "# the formats after the call, for what takes no fmt_fun (cardx, a study's",
    "# own functions, code, ard_stack()'s own rows): `fmt` by statistic or",
    "# variable:statistic over the defaults; the rows of the variables `skip`",
    "# have theirs already (fmt_fun inside ard_stack())",
    "fmt_ard <- function(ard, fmt = list(), skip = character()) {",
    "  # a method that gives several ARDs (cards::ard_pairwise(): one per",
    "  # pair of groups): one, each row keeping its ARD's name as `pairwise`",
    "  if (is.list(ard) && !is.data.frame(ard)) {",
    "    ard <- dplyr::bind_rows(ard, .id = \"pairwise\")",
    "  }",
    "  if (!inherits(ard, \"card\")) return(ard)",
    "  f <- utils::modifyList(fmt_default, fmt)",
    "  f <- f[order(grepl(\":\", names(f), fixed = TRUE))]",
    "  for (k in names(f)) {",
    "    s <- sub(\"^.*:\", \"\", k)",
    "    v <- if (grepl(\":\", k, fixed = TRUE)) sub(\":.*$\", \"\", k)",
    "    rows <- ard$stat_name == s & (is.null(v) | ard$variable %in% v) &",
    "      !ard$variable %in% skip",
    "    if (!any(rows)) next",
    "    ard <- cards::update_ard_fmt_fun(",
    "      ard, variables = dplyr::all_of(unique(ard$variable[rows])),",
    "      stat_names = s, fmt_fun = f[[k]])",
    "  }",
    "  cards::apply_fmt_fun(ard)",
    "}",
    "# only the statistics asked for, of a method that gives more",
    "keep_stats <- function(ard, keep) {",
    "  # several ARDs (cards::ard_pairwise(): one per pair): each of them",
    "  if (is.list(ard) && !is.data.frame(ard)) return(lapply(ard, keep_stats, keep))",
    "  ard[ard$stat_name %in% keep, , drop = FALSE]",
    "}",
    "")
}

# ---- the formats ---------------------------------------------------------

# A format of the spec as R that cards' fmt_fun takes: an integer is that
# many decimals (cards::label_round(d)), xx.x% a proportion as a percent,
# pvalue the program's fmt_pvalue()
.fmt_r <- function(f) {
  if (identical(f, "pvalue")) return("fmt_pvalue")
  d <- if (grepl("^[0-9]+$", f)) as.integer(f) else
    nchar(sub("^[^.]*[.]?", "", sub("%$", "", f)))
  if (endsWith(f, "%")) sprintf("cards::label_round(%d, scale = 100)", d) else
    sprintf("%dL", d)
}

# a name as R writes it: `AGE:sd` and the like in backquotes
.r_arg_name <- function(x) {
  ifelse(make.names(x) == x, x, encodeString(x, quote = "`"))
}

# list(mean = 2L, sd = 3L) as the arguments of a call
.fmt_list <- function(f) {
  stats::setNames(as.list(vapply(f, .fmt_r, "")), names(f))
}

# The formats an analysis's call gives cards itself, fmt_fun: each
# variable's statistics with the defaults (fmt_default) and the analysis's
# own formats over them, a variable's own (`BMIBL:sd`) over those for every
# variable -- what fmt_ard() gives the same ARD after the call.  cards
# takes a variable's list whole (a later formula does not add to an
# earlier one), so a variable with formats of its own gets every one
# again.  NULL when the call cannot say them: a format of a variable the
# call's `variables` do not name (or name as R, not by name).
.fmt_fun_arg <- function(fmt, vars) {
  if (anyNA(fmt) || !all(.fmt_ok(fmt))) return(NULL)
  own <- grepl(":", names(fmt), fixed = TRUE)
  plain <- fmt[!own]
  spec <- fmt[own]
  if (!all(sub(":.*$", "", names(spec)) %in% vars)) return(NULL)
  # a format every variable has, the same (an analysis's own in a stack,
  # given to each of its variables): one for every variable
  for (st in unique(sub("^.*:", "", names(spec)))) {
    k <- paste0(vars, ":", st)
    if (all(k %in% names(spec)) && length(unique(spec[k])) == 1L) {
      plain[st] <- spec[[k[1L]]]
      spec <- spec[!names(spec) %in% k]
    }
  }
  sv <- sub(":.*$", "", names(spec))
  one <- function(lhs, m) {
    if (length(m)) .cl(paste(lhs, "~ fmt_with"), .fmt_list(m)) else
      paste(lhs, "~ fmt_default")
  }
  # each variable's whole list (variables with the same: c(AGE, BMIBL))
  lists <- lapply(intersect(vars, sv), function(v) {
    m <- plain
    o <- spec[sv == v]
    m[sub("^.*:", "", names(o))] <- o
    m
  })
  key <- vapply(lists, function(m) paste(names(m), m, sep = "=", collapse = "|"), "")
  vs <- intersect(vars, sv)
  parts <- c(list(one("everything()", plain)),
             lapply(unique(key), function(k) {
               v <- .r_arg_name(vs[key == k])
               one(if (length(v) > 1L) sprintf("c(%s)", paste(v, collapse = ", ")) else v,
                   lists[[match(k, key)]])
             }))
  if (length(parts) == 1L) parts[[1L]] else .cl("list", parts)
}

# An analysis's fmt_fun, or NULL when its formats go after the call: its
# function takes no fmt_fun (cardx's tests and models, a study's own,
# code), its `args` gives one itself, or `post` changes the ARD after it.
.call_fmt_fun <- function(r, keys, given, fmt) {
  k <- .method_key(r$method, keys)
  fn <- if (is.na(k)) r$method else keys$call[k]
  if (!.takes_fmt_fun(fn) || any(c("fmt_fun", "fmt_fn") %in% given) ||
      !is.na(r$post)) return(NULL)
  vars <- if (identical(fn, "(subjects)")) {
    if (length(.split_bar(r$variables))) .split_bar(r$variables)[1L] else
      make.names(r$analysis_id)
  } else if (fn %in% .fmt_fun_every) character() else .split_bar(r$variables)
  .fmt_fun_arg(fmt, vars)
}

# Functions whose fmt_fun is for every variable only: ard_hierarchical()
# formats a column of its own (a variable's formula finds no AEDECOD), and
# ard_tabulate_rows() has no variables.  A variable's own format: after.
.fmt_fun_every <- c("cards::ard_hierarchical", "cards::ard_hierarchical_count",
                    "cards::ard_tabulate_rows")

# The step that formats the ARD: formatted in the call, its stat_fmt filled
# (cards::apply_fmt_fun()); or formatted after it (fmt_ard(), the defaults
# and `fmt`)
.fmt_step <- function(fmt, in_call, skip = NULL) {
  if (in_call && is.null(skip)) return("cards::apply_fmt_fun()")
  .cl("fmt_ard", c(if (length(fmt)) list(.cl("list", .fmt_list(fmt))),
                   if (length(skip)) list(skip = .cl("c", .q(skip)))))
}

# The step that tags the ARD: tag_ard(output_id, "ID", population = "SAF"),
# with the analyses a stack runs and their variables
.tag_step <- function(analysis_id, population_id, analyses = NULL) {
  .cl("tag_ard", c(list("output_id", .q(analysis_id)),
                   if (!is.na(population_id)) list(population = .q(population_id)),
                   if (length(analyses)) list(analyses = .cl("list", lapply(analyses, .c_str)))))
}

# ---- the analyses --------------------------------------------------------

.vars <- function(x) {
  v <- .split_bar(x)
  if (!length(v)) return(NULL)
  if (length(v) == 1L) v else sprintf("c(%s)", paste(v, collapse = ", "))
}

# The denominator column's words: the analysis set, or cards' own
.den_words <- c("population", "row", "column", "cell")

# The denominator column as R: `population` (the analysis set), "row" /
# "column" / "cell" (cards' percentages within a row, a column, of the
# whole), a population (its analysis set), or a dataset (its records of the
# analysis set's subjects)
.den_code <- function(den, x, pop, subj, population = "population") {
  if (is.null(den) || is.na(den)) return(NULL)
  if (den == "population") return(population)
  if (den %in% .den_words) return(encodeString(den, quote = "\""))
  if (den %in% x$populations$population_id) {
    return(paste0("pop_", .r_name(den)))
  }
  if (den %in% x$analysis_data$data_id) return(den)
  obj <- .r_name(den)
  if (is.null(pop)) obj else
    sprintf("dplyr::filter(%s, %s %%in%% %s$%s)", obj, subj, population, subj)
}

.stat_arg <- function(method, stats) {
  s <- .split_bar(stats)
  if (!length(s)) return(NULL)
  switch(method,
    continuous = {
      # cards' own statistics and those computed by tfl_stats, in the
      # order asked
      own <- !s %in% .computed_stats()
      run <- cumsum(c(TRUE, own[-1L] != own[-length(own)]))
      parts <- lapply(split(seq_along(s), run), function(i) {
        if (own[i[1L]]) .cl("cards::continuous_summary_fns", list(.cl("c", .q(s[i]))))
        else sprintf("tfl_stats[%s]", .lay(.cl("c", .q(s[i])), width = Inf))
      })
      if (length(parts) == 1L) {
        p <- parts[[1L]]
        if (is.character(p)) paste("~", p) else .cl(paste("~", p$fn), p$args)
      } else .cl("~ c", unname(parts))
    },
    categorical = ,
    missing = .cl("~ c", .q(s)),
    NULL)
}

# The call an analysis row stands for (.cl()), on `data`, of `population`.
# What the row writes itself (its args, its code) names them `data` and
# `population`: written as the program's names.  A `custom` row: its code.
.analysis_node <- function(r, keys, subj, has, den = NULL, data = "data",
                           population = "population", fmt_fun = NULL) {
  m <- r$method
  k <- .method_key(m, keys)
  fn <- if (is.na(k)) m else keys$call[k]
  kind <- if (is.na(k)) "" else keys$kind[k]
  stats <- if (!is.na(r$statistics)) r$statistics else if (!is.na(k) &&
    nzchar(keys$statistics[k])) keys$statistics[k] else NA
  map <- c(data = data %||% "data", population = population)
  own_r <- function(txt) .rename_symbols(txt, map)
  if (identical(fn, "(code)")) return(own_r(r$code))
  by <- .vars(r$by)
  vars <- .vars(r$variables)
  strata <- .vars(r$strata)
  user <- as.list(vapply(.split_args(r$args), own_r, ""))
  own <- c(if (!is.null(strata)) list(strata = strata),
           if (!is.null(den)) list(denominator = den))
  st <- if (!has("statistic")) .stat_arg(if (identical(fn, "(subjects)"))
    "categorical" else kind, stats)
  if (identical(fn, "(subjects)")) {
    flag <- if (length(.split_bar(r$variables))) .split_bar(r$variables)[1L] else
      make.names(r$analysis_id)
    return(.cl("cards::ard_tabulate_value", c(
      if (!is.null(by)) list(by = by),
      list(variables = flag, value = sprintf("list(%s = TRUE)", flag)),
      if (!is.null(st)) list(statistic = st), own, unname(user),
      if (!is.null(fmt_fun)) list(fmt_fun = fmt_fun))))
  }
  # the keyword's own arguments, each unless the row's args gives it
  dflt <- if (!is.na(k) && nzchar(keys$defaults[k])) {
    d <- trimws(strsplit(gsub("<id>", subj, keys$defaults[k], fixed = TRUE),
                         ",")[[1L]])
    d <- d[!vapply(sub("\\s*=.*$", "", d), has, NA)]
    gsub("\\bpopulation\\b", population, d, perl = TRUE)
  }
  first <- .data_arg(fn, has)
  if (!is.null(first)) first <- data
  .cl(fn, c(
    as.list(first),
    if (!is.null(by)) list(by = by),
    if (!identical(fn, "cards::ard_total_n") && !is.null(vars))
      list(variables = vars),
    own,
    if (!is.null(st)) list(statistic = st),
    as.list(dflt),
    unname(user),
    if (!is.null(fmt_fun)) list(fmt_fun = fmt_fun)))
}

# The call as code (the ARS model shows it)
.analysis_body <- function(r, keys, subj, has, den = NULL, data = "data",
                           population = "population", fmt_fun = NULL) {
  .lay(.analysis_node(r, keys, subj, has, den, data, population, fmt_fun))
}

# ---- the data ------------------------------------------------------------

# `obj` made from `src` by `steps` (calls without their data:
# "dplyr::filter(X)", "dplyr::mutate(A = B)", "set_levels(codelists)") in
# one statement, the way a person writes it: no step, `obj <- src`; one
# step on a name, `obj <- dplyr::filter(src, X)`; else a pipe, a step a line
.make_code <- function(obj, src, steps) {
  steps <- steps[!is.na(steps) & nzchar(steps)]
  if (!length(steps)) return(sprintf("%s <- %s", obj, src))
  if (length(steps) == 1L && grepl("^[A-Za-z.][A-Za-z0-9._]*$", src)) {
    s <- steps[[1L]]
    inner <- sub("^[^(]*\\((.*)\\)$", "\\1", s)
    fn <- sub("\\(.*$", "", s)
    return(sprintf("%s <- %s(%s%s)", obj, fn, src, if (nzchar(inner)) paste0(", ", inner) else ""))
  }
  if (length(steps) == 1L) return(sprintf("%s <- %s |> %s", obj, src, steps[[1L]]))
  paste0(obj, " <- ", src, " |>\n  ", paste(steps, collapse = " |>\n  "))
}

# the code lists' step
.levels_step <- "set_levels(codelists)"

# the steps of a derive and of the code lists: dplyr::mutate(...),
# set_levels().  With code lists, a data made from another is made factors
# again: the columns it derives, and the values its rows no longer have that
# the lists do not list (a population's: not the screen failures' arm)
.derive_steps <- function(derive, levels = NULL) {
  d <- .split_bar(derive)
  c(if (length(d)) sprintf("dplyr::mutate(%s)", paste(d, collapse = ", ")),
    if (length(levels)) .levels_step)
}

# `nm <- src |> dplyr::filter(cond)`: a data made for an analysis (with code
# lists made factors again, as .derive_steps())
.made_line <- function(nm, src, cond, levels = NULL) {
  .make_code(nm, src, c(sprintf("dplyr::filter(%s)", cond),
                        if (length(levels)) .levels_step))
}

# `codelists <- list(...)`: the code lists' values of each variable
.codelists_line <- function(levels) {
  paste("codelists <-", .lay(.cl("list", lapply(levels, function(v)
    .cl("c", .q(v)))), lead = 13L))
}

# The datasets and populations analyses `a` (and analysis data `nad`)
# read: their lines, with set_levels() when there are code lists
.ard_data_lines <- function(x, a, nad, levels = NULL) {
  pops <- unique(stats::na.omit(c(a$population_id,
                                  a$denominator[a$denominator %in%
                                                  x$populations$population_id],
                                  nad$population_id)))
  den_ds <- a$denominator[a$denominator %in% x$datasets$dataset &
                            !a$denominator %in% .den_words]
  used_ds <- unique(stats::na.omit(c(a$dataset, den_ds,
                                     nad$from[nad$from %in% x$datasets$dataset],
                                     x$populations$dataset[
    x$populations$population_id %in% pops])))
  code <- character()
  for (ds in used_ds) {
    r <- x$datasets[x$datasets$dataset == ds, ]
    obj <- .r_name(ds)
    d <- .split_bar(r$derive[1L])
    code <- c(code, .make_code(obj, .reader(r$path[1L]), c(
      if (length(d)) sprintf("dplyr::mutate(%s)", paste(d, collapse = ", ")),
      if (length(levels)) .levels_step)))
  }
  pop_code <- character()
  for (pid in pops) {
    r <- x$populations[x$populations$population_id == pid, ]
    obj <- paste0("pop_", .r_name(pid))
    src <- .r_name(r$dataset[1L])
    pop_code <- c(pop_code, .make_code(obj, src, c(
      if (!is.na(r$where[1L])) sprintf("dplyr::filter(%s)", r$where[1L]),
      # an analysis set with every record of its dataset: its factors as they are
      .derive_steps(r$derive[1L], if (!is.na(r$where[1L]) ||
                                     length(.split_bar(r$derive[1L]))) levels))))
  }
  list(data = code, pops = pop_code, used_ds = used_ds, pops_id = pops)
}

# ---- the program ---------------------------------------------------------

# The analyses `a`: their data, their analysis sets, one statement each
# (`ard_<id> <- call |> format |> tag_ard()`), bound into `ard`.  `levels`:
# each report's code lists (.codelist_levels_by()).  One report's program
# starts with `output_id <- "..."`; the study's has a part a report, each
# report's ARD into `ards`.
.ard_body_lines <- function(x, a, levels = NULL) {
  subj <- .study_value(x, "id", "USUBJID")
  # an analysis on an analysis data is of its population: the data is its
  # own report's (analysis_data is by report); the named data each report
  # reads (and its denominator), with the rows they are made from
  ad <- .adata_sheet(x)
  x$analysis_data <- ad
  dcol <- .data_col(a)
  outs <- unique(a$output_id)
  for (i in which(!is.na(dcol))) {
    a$population_id[i] <- .adata_pop(.adata_of(ad, a$output_id[i]), dcol[i])
  }
  named <- lapply(stats::setNames(outs, outs), function(o) {
    ad_o <- .adata_of(ad, o)
    .adata_used(ad_o, a[a$output_id == o, , drop = FALSE])
  })
  nad_of <- function(o) {
    ad_o <- .adata_of(ad, o)
    ad_o[ad_o$data_id %in% named[[o]], , drop = FALSE]
  }
  nad <- do.call(rbind, c(list(ad[0L, , drop = FALSE]), lapply(outs, nad_of)))
  several <- length(outs) > 1L
  # the study's program with code lists: each report's part reads its data
  # with its own (a report's factors are not the next report's)
  own_data <- several && length(levels) > 0L
  lv_of <- function(o) if (is.null(levels)) NULL else levels[[o]]
  lv1 <- if (!several) lv_of(outs[1L])
  code <- if (!several) c(sprintf("output_id <- %s", .q(outs[1L])), "")
  if (own_data) {
    used_ds <- character()
    pops <- character()
  } else {
    dl <- .ard_data_lines(x, a, nad, lv1)
    used_ds <- dl$used_ds
    pops <- dl$pops_id
    code <- c(code, "# ---- data ----", if (length(lv1)) .codelists_line(lv1),
              dl$data,
              if (length(dl$pops)) c("", "# ---- populations ----", dl$pops))
  }
  keys <- tfl_ard_methods()
  par <- a$parent %||% rep(NA_character_, nrow(a))
  if (several) code <- c(code, if (length(code)) "", "ards <- list()")
  for (o in outs) {
    xo <- x
    ad_o <- .adata_of(ad, o)
    xo$analysis_data <- ad_o
    rows <- which(a$output_id == o)
    lv <- if (several) lv_of(o) else lv1
    part <- NULL
    if (own_data) {
      dl <- .ard_data_lines(x, a[rows, , drop = FALSE], nad_of(o), lv)
      used_ds <- dl$used_ds
      pops <- dl$pops_id
      part <- c(if (length(lv)) .codelists_line(lv), dl$data, dl$pops)
    }
    # each analysis's data: the dataset, restricted to the population's
    # subjects (or the population itself when it is that dataset), and to
    # the analysis's own subset -- made once, under a name, for every
    # analysis that reads it
    taken <- c(.r_name(used_ds), paste0("pop_", .r_name(pops)), ad_o$data_id)
    data_of <- character()
    made <- .adata_lines(xo, named[[o]], subj, lv, avoid = taken)
    data_name <- vapply(rows, function(i) {
      r <- a[i, ]
      # on an analysis data: it, or its records the analysis's condition keeps
      if (!is.na(dcol[i])) {
        if (is.na(r$where)) return(dcol[i])
        key <- paste(dcol[i], r$where, sep = "\r")
        if (!is.na(data_of[key])) return(data_of[[key]])
        nm <- make.unique(c(taken, dcol[i]), sep = "_")[length(taken) + 1L]
        taken <<- c(taken, nm)
        data_of[[key]] <<- nm
        made <<- c(made, .made_line(nm, dcol[i], r$where, lv))
        return(nm)
      }
      pid <- r$population_id
      pop <- if (!is.na(pid)) paste0("pop_", .r_name(pid))
      ds <- if (!is.na(r$dataset)) .r_name(r$dataset) else pop
      pop_ds <- if (!is.na(pid)) x$populations$dataset[
        x$populations$population_id == pid][1L]
      # one filter() for the population's subjects and the analysis's own
      # condition (filter() drops the rows a condition leaves NA)
      whr <- if (!is.na(r$where)) r$where
      if (is.null(pop) || identical(r$dataset, pop_ds) || is.na(r$dataset)) {
        src <- if (is.null(pop)) ds else pop
        cond <- whr
      } else {
        src <- ds
        cond <- sprintf("%s %%in%% %s$%s", subj, pop, subj)
        if (!is.null(whr)) cond <- .cond_and(cond, whr)
      }
      # no dataset and no population: no data (as before, `data` is NULL)
      if (is.null(src)) return("NULL")
      if (is.null(cond) && src %in% taken) return(src)  # a dataset or a population
      key <- paste(src, cond, sep = "\r")
      if (!is.na(data_of[key])) return(data_of[[key]])
      base <- paste(c(if (!is.na(r$dataset)) .r_name(r$dataset) else
        .r_name(pop_ds), if (!is.na(pid)) .r_name(pid)), collapse = "_")
      nm <- make.unique(c(taken, base), sep = "_")[length(taken) + 1L]
      taken <<- c(taken, nm)
      data_of[[key]] <<- nm
      made <<- c(made, .made_line(nm, src, cond, lv))
      nm
    }, "")
    names(data_name) <- as.character(rows)
    if (several) {
      code <- c(code, "", sprintf("# ---- %s ----", o),
                sprintf("output_id <- %s", .q(o)), part, made)
    } else if (length(made)) {
      code <- c(code, "", "# ---- analysis data ----", made)
    }
    if (!several) code <- c(code, "", "# ---- analyses ----")
    # each analysis's ARD under a name of its own: ard_<id>
    top <- rows[is.na(par[rows])]
    ard_names <- make.unique(c(taken, paste0("ard_", gsub("[^a-z0-9]+", "_",
                                                         tolower(a$analysis_id[top])))),
                             sep = "_")[-seq_along(taken)]
    avoid <- c(taken, ard_names, "ard", "ards", "output_id", "codelists",
               .ard_helper_names)
    for (j in seq_along(top)) {
      i <- top[j]
      r <- a[i, ]
      kids <- which(!is.na(par) & par == r$analysis_id &
                      a$output_id == r$output_id)
      lines <- if (length(kids)) {
        .ard_wrapper_lines(xo, r, a[kids, , drop = FALSE], keys, subj,
                           data_name[[as.character(i)]], ard_names[j])
      } else {
        .ard_analysis_lines(xo, r, keys, subj, data_name[[as.character(i)]],
                            ard_names[j], avoid)
      }
      code <- c(code, if (j > 1L || several) "", lines)
    }
    bound <- .lay(.cl("dplyr::bind_rows", as.list(ard_names)),
                  lead = if (several) 21L else 7L)
    code <- c(code, "",
              if (several) paste("ards[[output_id]] <-", bound) else
                paste("ard <-", bound))
  }
  # dplyr::bind_rows(), not cards::bind_ard(): bind_ard() does not count
  # output_id / analysis_id as part of a row's key, so the same statistic in
  # two outputs (AGE's mean in two analysis sets) would stop it, or, with the
  # same value, lose one output's rows.  The class card is kept by tag_ard().
  if (several) code <- c(code, "", "ard <- dplyr::bind_rows(ards)")
  # (one report's program: an analysis set that is only its adsl_<set> is
  # made once, under that name; the study's keeps pop_* for every report)
  if (several) code else .pop_as_adata(code, ad$data_id)
}

# One analysis: `ard_<id> <- call |> ... |> tag_ard(...)`, its label a
# comment above it
.ard_analysis_lines <- function(x, r, keys, subj, data, name, avoid) {
  pid <- r$population_id
  pop <- if (!is.na(pid)) paste0("pop_", .r_name(pid))
  pop_name <- if (is.null(pop)) "NULL" else pop
  given <- c(.args_given(r$args), if (!is.na(r$strata)) "strata",
             if (!is.na(r$denominator)) "denominator")
  has <- function(arg) arg %in% given
  k <- .method_key(r$method, keys)
  kind <- if (is.na(k)) "" else keys$kind[k]
  call_fn <- if (is.na(k)) r$method else keys$call[k]
  den <- .den_code(r$denominator, x, pop, subj, population = pop_name)
  fmt <- c(if (!is.na(k)) .parse_formats(keys$formats[k]),
           .parse_formats(r$formats))
  fmt <- fmt[!duplicated(names(fmt), fromLast = TRUE)]
  # the formats in the call (fmt_fun), when the function takes them and
  # nothing changes the ARD after it; else after it (fmt_ard())
  fmt_fun <- .call_fmt_fun(r, keys, given, fmt)
  node <- .analysis_node(r, keys, subj, has, den, data = data,
                         population = pop_name, fmt_fun = fmt_fun)
  keep <- if (!kind %in% c("continuous", "categorical", "missing") &&
              !identical(call_fn, "(subjects)"))
    .split_bar(r$statistics)
  steps <- c(as.list(.split_post(r$post)),
             if (length(keep)) list(.cl("keep_stats", list(.cl("c", .q(keep))))),
             list(.fmt_step(fmt, !is.null(fmt_fun)),
                  .tag_step(r$analysis_id, pid)))
  comment <- if (!is.na(r$label)) sprintf("# %s: %s", r$analysis_id, r$label)
  if (identical(call_fn, "(subjects)")) {
    # a subject-level flag: has the subject any record of the data?
    flag <- if (length(.split_bar(r$variables))) .split_bar(r$variables)[1L] else
      make.names(r$analysis_id)
    return(c(comment, .pipe_code(name, pop_name, c(
      list(sprintf("dplyr::mutate(%s = %s %%in%% %s$%s)", flag, subj, data, subj)),
      list(node), steps))))
  }
  if (identical(call_fn, "(code)")) {
    out <- .code_as_program(node, name, steps, avoid)
    if (is.null(out)) {
      # code that is best in a scope of its own: `data` and `population`
      # bound there
      inner <- strsplit(r$code, "\n", fixed = TRUE)[[1L]]
      out <- .pipe_code(name, paste(c("local({",
                                      paste0("  data <- ", data),
                                      paste0("  population <- ", pop_name),
                                      paste0("  ", inner),
                                      "})"), collapse = "\n"), steps)
    }
    return(c(comment, out))
  }
  c(comment, .pipe_code(name, node, steps))
}

# An analysis data that is an analysis set and nothing else (`adsl_saf <-
# pop_saf`), when the set is used for nothing else in the program: made
# once, under the data's name, where the analysis data are
# (`adsl_saf <- adsl |> dplyr::filter(SAFFL == "Y") |> ...`), and no pop_saf.
.pop_as_adata <- function(code, data_ids) {
  rx <- "^([A-Za-z.][A-Za-z0-9._]*) <- (pop_[A-Za-z0-9._]+)$"
  for (k in rev(grep(rx, code))) {
    d <- sub(rx, "\\1", code[k])
    p <- sub(rx, "\\2", code[k])
    if (!d %in% data_ids) next
    def <- which(startsWith(code, paste0(p, " <- ")))
    if (length(def) != 1L) next
    word <- paste0("(^|[^A-Za-z0-9._])", gsub(".", "\\.", p, fixed = TRUE), "($|[^A-Za-z0-9._])")
    if (any(grepl(word, code[-c(k, def)]))) next
    code[k] <- paste0(d, substring(code[def], nchar(p) + 1L))
    code <- code[-def]
  }
  # a populations section with nothing left in it goes
  h <- which(code == "# ---- populations ----")
  if (length(h) && (h == length(code) || !nzchar(code[h + 1L]))) {
    code <- code[-c(if (h > 1L && !nzchar(code[h - 1L])) h - 1L, h)]
  }
  code
}

# The functions that run other analyses: an analysis row whose `parent`
# names a row with one of these as its method is run inside it.
.ard_wrappers <- c("cards::ard_stack", "cards::ard_strata",
                   "cards::ard_pairwise")

# One analysis that runs others (its `parent` rows), as the call a person
# writes: cards::ard_stack(data, .by = ARM, ard_summary(...), ...).
# The rows inside take the parent's data, analysis set and condition; in a
# stack, its by too.  Each variable's rows are tagged with the analysis that
# computed them, the stack's own (the by counts, the total N) with the
# parent's id; inside ard_strata() / ard_pairwise() the one analysis's id.
.ard_wrapper_lines <- function(x, r, kids, keys, subj, data, name) {
  pid <- r$population_id
  pop <- if (!is.na(pid)) paste0("pop_", .r_name(pid))
  pop_name <- if (is.null(pop)) "NULL" else pop
  stack <- identical(r$method, "cards::ard_stack")
  pairwise <- identical(r$method, "cards::ard_pairwise")
  # formats: a row's own, for its own variables
  fmt <- character()
  for (j in seq_len(nrow(kids))) {
    kr <- kids[j, ]
    k <- .method_key(kr$method, keys)
    f <- c(if (!is.na(k)) .parse_formats(keys$formats[k]),
           .parse_formats(kr$formats))
    if (stack && any(!grepl(":", names(f), fixed = TRUE))) {
      plain <- !grepl(":", names(f), fixed = TRUE)
      vv <- .split_bar(kr$variables)
      f <- c(f[!plain], unlist(lapply(vv, function(v)
        stats::setNames(f[plain], paste0(v, ":", names(f)[plain])))))
    }
    fmt <- c(fmt, f)
  }
  fmt <- c(.parse_formats(r$formats), fmt)
  fmt <- fmt[!duplicated(names(fmt), fromLast = TRUE)]
  given_of <- function(kr) c(.args_given(kr$args), if (!is.na(kr$strata)) "strata",
                             if (!is.na(kr$denominator)) "denominator")
  # each analysis's formats in its call (fmt_fun), when every one takes
  # them; not inside ard_pairwise() (a list of ARDs), nor when `post`
  # changes the ARD after
  fmt_funs <- if (!pairwise && is.na(r$post)) lapply(seq_len(nrow(kids)), function(j) {
    kr <- kids[j, ]
    kr$post <- NA
    f <- fmt
    # in a stack, a variable's own formats go to its analysis's call (the
    # rest, the stack's own rows, after it)
    if (stack) f <- f[!grepl(":", names(f), fixed = TRUE) |
                        sub(":.*$", "", names(f)) %in% .split_bar(kr$variables)]
    .call_fmt_fun(kr, keys, given_of(kr), f)
  })
  in_call <- length(fmt_funs) > 0L && !any(vapply(fmt_funs, is.null, NA))
  child <- function(kr, fmt_fun) {
    given <- given_of(kr)
    has <- function(arg) arg %in% given
    den <- .den_code(kr$denominator, x, pop, subj, population = pop_name)
    if (stack) kr$by <- NA
    .analysis_node(kr, keys, subj, has, den,
                   data = if (stack) NULL else ".x",
                   population = pop_name,
                   fmt_fun = if (in_call) fmt_fun)
  }
  bodies <- lapply(seq_len(nrow(kids)), function(j)
    child(kids[j, ], fmt_funs[[j]]))
  tilde <- function(b) if (inherits(b, "tfl_cl")) .cl(paste("~", b$fn), b$args) else
    paste("~", b)
  node <- .cl(r$method, c(
    list(data),
    if (stack && !is.na(r$by)) list(.by = .vars(r$by)),
    if (!stack && identical(r$method, "cards::ard_strata")) c(
      if (!is.na(r$by)) list(.by = .vars(r$by)),
      if (!is.na(r$strata)) list(.strata = .vars(r$strata))),
    if (pairwise) list(variable = .vars(r$variables)),
    if (stack) bodies else list(.f = tilde(bodies[[1L]])),
    as.list(.split_args(r$args))))
  # ard_stack()'s own rows (the by counts, the total N) take no fmt_fun:
  # theirs after it, the analyses' variables skipped (formatted in their
  # calls).  With .missing, the stack's missing rows are of those variables
  # too: all after it, as in the calls.
  skip <- if (in_call && stack) {
    if (".missing" %in% .args_given(r$args)) character() else
      unique(unlist(lapply(kids$variables, .split_bar)))
  }
  post_fmt <- if (length(skip)) fmt[!grepl(":", names(fmt), fixed = TRUE) |
                                      !sub(":.*$", "", names(fmt)) %in% skip] else fmt
  keep <- if (!stack) {
    k <- .method_key(kids$method[1L], keys)
    kind <- if (is.na(k)) "" else keys$kind[k]
    if (!kind %in% c("continuous", "categorical", "missing"))
      .split_bar(kids$statistics[1L])
  }
  tag <- if (stack) {
    # each analysis's variables (an analysis's id once)
    an <- lapply(split(seq_len(nrow(kids)), factor(kids$analysis_id,
                                                   unique(kids$analysis_id))),
                 function(j) unlist(lapply(kids$variables[j], .split_bar)))
    .tag_step(r$analysis_id, pid, an)
  } else .tag_step(kids$analysis_id[1L], pid)
  steps <- c(as.list(.split_post(r$post)),
             if (length(keep)) list(.cl("keep_stats", list(.cl("c", .q(keep))))),
             list(.fmt_step(post_fmt, in_call && !stack,
                            skip = if (stack && in_call) skip),
                  tag))
  lbl <- if (!is.na(r$label)) paste0(": ", r$label) else ""
  c(sprintf("# %s%s (runs %s)", r$analysis_id, lbl,
            paste(kids$analysis_id, collapse = ", ")),
    .pipe_code(name, node, steps))
}
