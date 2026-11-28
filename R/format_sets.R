# ---- Named continuous format sets (tflspec#903) ------------------------------
#
# A study keeps its sets of continuous formats in one place, study.yml's
# `settings.format_sets` (copied from the company standard):
#
#   settings:
#     format_sets:
#       continuous_0: {N: 0, mean: 1, sd: 2, median: 1, min: 0, max: 0}
#       continuous_1: {N: 0, mean: 2, sd: 3, median: 2, min: 1, max: 1}
#     params:
#       ALT: {label: "...", format: continuous_0}
#
# A report's `formats` names a set as a value: `"*": continuous_1` (every
# variable of the analysis), `AGE: continuous_0` (one variable).  The
# statistic keys (`mean: 2`) and `VAR:stat` stay as they were.  The layers, the
# lower one losing (design continuous-format-sets-2026-11-23 2.3): the
# catalog (fmt_default); the standard's "*" set; the standard's statistics;
# the report's "*" set; the report's statistics; a variable's set (the
# standard's, a parameter's of settings.params); the standard's VAR:stat; the
# report's variable set; the report's VAR:stat.  Scope first (every variable
# < one variable), then the report over the standard, then a statistic over a
# set.  .fmtset_over() puts a report's map over the standard's so that one map
# says the same (a report's "*" takes the place of the standard's statistics
# of a set; a report's variable set that of the standard's VAR:stat); in one
# map a statistic wins over the set of its scope.
#
# A set reaches the continuous analyses only (a method of kind continuous);
# elsewhere its keys are not read.  Generated R names a set fmtset_<name>
# (the ARD's fmt_fun) or digset_<name> (plan_digits()).

# A set's name: a lower-case R name, not a format (`x`, `xx` are formats)
.fmtset_name_ok <- function(x) {
  is.character(x) & grepl("^[a-z][a-z0-9_]*$", x) & !.fmt_ok(x)
}

# Is a formats value a set's name?  (a number or a format is not)
.fmtset_is_ref <- function(v) {
  length(v) == 1L && is.character(v) && !is.na(v) && .fmtset_name_ok(v)
}

# A statistic's key: one of the catalog's (tfl_ard_statistics()), so that a
# key that is not one is a variable's (AGE) or a parameter's (ALT)
.fmtset_is_stat_key <- function(k) {
  k %in% tfl_ard_statistics()$statistic
}

# study.yml's settings.format_sets, checked: a named list of named character
# vectors (statistic = format, as the formats column writes them), NULL when
# the study has none.  `where`: the file, for the messages.
.study_format_sets <- function(study, where = "study.yml: settings: format_sets") {
  fs <- if (inherits(study, "tfl_study_spec")) study$study$settings$format_sets else
    if (is.list(study) && !is.data.frame(study) && !is.null(study$format_sets)) study$format_sets
  .fmtsets_check(fs, where)
}

.fmtsets_check <- function(fs, where) {
  if (is.null(fs) || !length(fs)) return(NULL)
  if (!is.list(fs) || is.null(names(fs)) || !all(nzchar(names(fs)))) {
    .ard_stop(where, " is a map of named sets: {continuous_1: {N: 0, mean: 2, sd: 3}}.")
  }
  bad <- names(fs)[!.fmtset_name_ok(names(fs))]
  if (length(bad)) {
    .ard_stop(sprintf(paste0("%s: a set's name is lower case, a letter first, then letters, digits ",
                             "or _, and not a format (x, xx): %s."), where, paste(bad, collapse = ", ")))
  }
  if (anyDuplicated(names(fs))) {
    .ard_stop(sprintf("%s: the set %s is given twice.", where, names(fs)[duplicated(names(fs))][1L]))
  }
  st <- tfl_ard_statistics("continuous")$statistic
  out <- lapply(names(fs), function(n) {
    s <- fs[[n]]
    if (is.atomic(s) && !is.null(names(s))) s <- as.list(s)
    if (!is.list(s) || !length(s) || is.null(names(s)) || !all(nzchar(names(s)))) {
      .ard_stop(sprintf("%s: set %s is a map of statistic to format: {mean: 2, sd: 3}.", where, n))
    }
    unk <- setdiff(names(s), st)
    if (length(unk)) {
      .ard_stop(sprintf("%s: set %s names %s, not a continuous statistic (tfl_ard_statistics(\"continuous\")).",
                        where, n, paste(unk, collapse = ", ")))
    }
    v <- vapply(s, function(x) if (length(x) == 1L && !is.na(x)) as.character(x) else NA_character_, "")
    if (anyNA(v) || !all(.fmt_ok(v))) {
      .ard_stop(sprintf(paste0("%s: set %s: each statistic's value is a number of decimals or a ",
                               "format (xx.x, xx.x%%, pvalue): %s."), where, n,
                        paste(names(v)[is.na(v) | !.fmt_ok(v)], collapse = ", ")))
    }
    v
  })
  stats::setNames(out, names(fs))
}

# study.yml's settings.params: each parameter's (PARAMCD) set, `format:`, as
# a named character vector (PARAMCD = set); the parameters with no format
# left out.  A format that is not a set of the study stops.
.study_param_formats <- function(study, sets = .study_format_sets(study)) {
  pr <- if (inherits(study, "tfl_study_spec")) study$study$settings$params
  if (is.null(pr) || !length(pr)) return(NULL)
  where <- "study.yml: settings: params"
  if (!is.list(pr) || is.null(names(pr))) {
    .ard_stop(where, " is a map by PARAMCD: {ALT: {label: \"...\", format: continuous_0}}.")
  }
  f <- vapply(names(pr), function(p) {
    v <- if (is.list(pr[[p]])) pr[[p]]$format
    if (is.null(v)) return(NA_character_)
    if (!.fmtset_is_ref(v)) {
      .ard_stop(sprintf("%s: %s: format is the name of one of the study's format sets (settings.format_sets).",
                        where, p))
    }
    v
  }, "")
  f <- f[!is.na(f)]
  if (!length(f)) return(NULL)
  for (p in names(f)) .fmtset_get(sets, f[[p]], sprintf("%s: %s", where, p))
  f
}

# One set's values, or a stop naming the study's sets
.fmtset_get <- function(sets, name, where) {
  if (!length(sets)) {
    .ard_stop(sprintf(paste0("%s names the format set %s, but the study has no format sets: ",
                             "write them in study.yml's settings: format_sets."), where, name))
  }
  if (!name %in% names(sets)) {
    .ard_stop(sprintf("%s names the format set %s, which the study has not got; its sets: %s.",
                      where, name, paste(names(sets), collapse = ", ")))
  }
  sets[[name]]
}

# ---- the report over the standard (one map) ----------------------------------

# A report's formats map over the standard's (a template's default, the
# series' or the study's), name by name as .tr_merge_map() and
# .tpl_merge_named() do, after the standard's keys a set of the report takes
# the place of are taken out: under the report's "*", the standard's "*" and
# its statistics of a set (the continuous statistics); under a report's
# variable set (AGE: continuous_0), the standard's AGE:<stat> and AGE map.
# With no set in the report, the standard is as it was.
.fmtset_over <- function(std, rep) {
  if (!is.list(std) && !(is.atomic(std) && !is.null(names(std)))) return(std)
  if (is.null(names(rep)) || !length(std)) return(std)
  is_ref <- function(v) !inherits(v, "tfl_unset") && .fmtset_is_ref(v)
  refs <- names(rep)[vapply(rep, is_ref, NA)]
  if (!length(refs)) return(std)
  drop <- character()
  if ("*" %in% refs) {
    cont <- tfl_ard_statistics("continuous")$statistic
    drop <- c(drop, "*", intersect(names(std), cont))
  }
  for (v in setdiff(refs, "*")) {
    drop <- c(drop, v, names(std)[startsWith(names(std), paste0(v, ":"))])
  }
  out <- std[!names(std) %in% drop]
  if (!length(out)) NULL else out
}

# ---- an analysis's formats with its sets -------------------------------------

# The statistics an analysis computes: its own, else the method's defaults
# (cards::ard_summary()'s)
.fmtset_stats <- function(r) {
  s <- .split_bar(r$statistics %||% NA)
  if (length(s)) s else c("N", "mean", "sd", "median", "p25", "p75", "min", "max")
}

# An analysis's formats (`fmt`: statistic = format, VAR:stat = format, and a
# set's name under "*" or a variable) with its sets opened: the formats as
# they were (statistic = format and VAR:stat = format, what fmt_ard() and
# the fingerprint read), the sets in attribute "fmtsets" for the code that
# names them (list(star = the "*" set or NA, vars = a variable's set by
# variable, plain = the statistics written, own = the VAR:stat written)).
# With no set named, `fmt` as it was.  `vars`: the analysis's variables;
# `kind`: its method's kind (a set reaches a continuous analysis only);
# `stats`: the statistics it computes; `sets`: the study's; `where`: the
# analysis, for the messages.
.fmtset_resolve <- function(fmt, vars, kind, stats, sets, where) {
  if (!length(fmt)) return(fmt)
  k <- names(fmt)
  own_k <- grepl(":", k, fixed = TRUE)
  ref <- !is.na(fmt) & .fmtset_name_ok(fmt) & !own_k
  star_bad <- k == "*" & !ref
  if (any(star_bad)) {
    .ard_stop(sprintf(paste0("%s: formats \"*\" takes the name of one of the study's format sets ",
                             "(settings.format_sets), not %s; a statistic's format is written by ",
                             "its name (mean: 2)."), where, fmt[star_bad][1L]))
  }
  stat_ref <- ref & .fmtset_is_stat_key(k)
  if (any(stat_ref)) {
    .ard_stop(sprintf(paste0("%s: formats %s is a statistic, whose value is a number of decimals or ",
                             "a format, not the set %s; a set goes under \"*\" or a variable (AGE: %s)."),
                      where, k[stat_ref][1L], fmt[stat_ref][1L], fmt[stat_ref][1L]))
  }
  if (!any(ref)) return(fmt)
  # (a name that is no set of the study stops, whatever the analysis: a
  # misspelled format is not left out in silence)
  for (i in which(ref)) .fmtset_get(sets, fmt[[i]], sprintf("%s: formats %s", where, k[i]))
  rest <- fmt[!ref]
  if (!identical(kind, "continuous")) return(rest)
  star <- if ("*" %in% k[ref]) fmt[ref & k == "*"][[1L]] else NA_character_
  vs <- fmt[ref & k != "*"]
  vs <- vs[names(vs) %in% vars]
  plain <- rest[!grepl(":", names(rest), fixed = TRUE)]
  own <- rest[grepl(":", names(rest), fixed = TRUE)]
  # each computed statistic of each variable has a format of its layer: a
  # set without it stops (it does not fall to a lower layer)
  for (v in vars) {
    for (s in stats) {
      if (paste0(v, ":", s) %in% names(own)) next
      if (v %in% names(vs)) {
        if (!s %in% names(sets[[vs[[v]]]])) {
          .fmtset_lack_stop(vs[[v]], s, where, v)
        }
        next
      }
      if (s %in% names(plain)) next
      if (!is.na(star) && !s %in% names(sets[[star]])) .fmtset_lack_stop(star, s, where, v)
    }
  }
  flat <- plain
  if (!is.na(star)) {
    sv <- sets[[star]]
    flat <- c(sv[!names(sv) %in% names(plain)], plain)
  }
  for (v in names(vs)) {
    sv <- sets[[vs[[v]]]]
    flat <- c(flat, stats::setNames(sv, paste0(v, ":", names(sv))))
  }
  flat <- flat[!names(flat) %in% names(own)]
  flat <- c(flat, own)
  attr(flat, "fmtsets") <- list(star = star, vars = vs, plain = plain, own = own)
  flat
}

.fmtset_lack_stop <- function(set, stat, where, var) {
  .ard_stop(sprintf(paste0("%s: the format set %s has no %s (variable %s). Add %s to the set ",
                           "(settings.format_sets), or write %s or %s:%s in formats."),
                    where, set, stat, var, stat, stat, var, stat))
}

# An analysis row's formats: its method's, its own over them, its sets
# opened (.fmtset_resolve()).  `x`: the ARD definition, whose format_sets
# are the study's.
.analysis_fmt <- function(r, keys, x, k = .method_key(r$method, keys)) {
  fmt <- c(if (!is.na(k)) .parse_formats(keys$formats[k]), .parse_formats(r$formats))
  fmt <- fmt[!duplicated(names(fmt), fromLast = TRUE)]
  kind <- if (is.na(k)) "" else keys$kind[k]
  .fmtset_resolve(fmt, .split_bar(r$variables), kind, .fmtset_stats(r), x$format_sets,
                  sprintf("%s / %s", r$output_id %||% "", r$analysis_id %||% ""))
}

# The sets the ARD program's analyses name, in the order of the study's
# sets: those its setup writes (fmtset_<name>)
.fmtsets_used <- function(x, keys = tfl_ard_methods()) {
  a <- x$analyses
  sets <- x$format_sets
  if (!length(sets) || is.null(a) || !nrow(a) || is.null(a$formats)) return(character())
  used <- character()
  for (i in seq_len(nrow(a))) {
    f <- .parse_formats(a$formats[i])
    if (!any(.fmtset_name_ok(f[!is.na(f)]))) next
    r <- .fmtset_safe(a[i, , drop = FALSE], keys, x)
    fs <- attr(r, "fmtsets")
    if (!is.null(fs)) used <- c(used, if (!is.na(fs$star)) fs$star, unname(fs$vars))
  }
  intersect(names(sets), used)
}

# (a wrong row stops where its analysis is written, not here)
.fmtset_safe <- function(r, keys, x) {
  tryCatch(.analysis_fmt(r, keys, x), error = function(e) NULL)
}

# The setup's lines of the sets: each a list over fmt_default, as cards'
# fmt_fun takes it
.fmtset_lines <- function(used, sets) {
  if (!length(used)) return(character())
  c("# the format sets this program uses (the study standard, settings.format_sets)",
    vapply(used, function(n) {
      paste(paste0("fmtset_", n), "<-",
            .lay(.cl("utils::modifyList", list("fmt_default", .cl("list", .fmt_list(sets[[n]])))),
                 lead = nchar(n) + 11L))
    }, ""),
    "")
}

# The fmt_fun of an analysis whose formats name a set: everything() its
# "*" set (or fmt_default) with the statistics written over it; a variable
# of a set of its own that set with its VAR:stat; a variable of VAR:stat
# only the "*" layer with them.  NULL when the call cannot say it (a
# VAR:stat of a variable it does not analyse).
.fmtset_fun_arg <- function(fs, vars) {
  own_v <- sub(":.*$", "", names(fs$own))
  if (!all(own_v %in% vars)) return(NULL)
  base <- if (is.na(fs$star)) "fmt_default" else paste0("fmtset_", fs$star)
  one <- function(lhs, b, m) {
    if (length(m)) .cl(paste(lhs, "~ utils::modifyList"), list(b, .cl("list", .fmt_list(m)))) else
      paste(lhs, "~", b)
  }
  lists <- list()
  for (v in vars) {
    o <- fs$own[own_v == v]
    names(o) <- sub("^.*:", "", names(o))
    if (v %in% names(fs$vars)) {
      lists[[v]] <- list(b = paste0("fmtset_", fs$vars[[v]]), m = o)
    } else if (length(o)) {
      m <- fs$plain
      m[names(o)] <- o
      lists[[v]] <- list(b = base, m = m)
    }
  }
  key <- vapply(lists, function(l) paste(l$b, paste(names(l$m), l$m, sep = "=", collapse = "|")), "")
  vs <- names(lists)
  parts <- c(list(one("everything()", base, fs$plain)),
             lapply(unique(key), function(kk) {
               v <- .r_arg_name(vs[key == kk])
               l <- lists[[match(kk, key)]]
               one(if (length(v) > 1L) sprintf("c(%s)", paste(v, collapse = ", ")) else v, l$b, l$m)
             }))
  if (length(parts) == 1L) parts[[1L]] else .cl("list", parts)
}

# The fingerprint's lines of the sets an output's analyses name: a study
# that changes a set's values makes the ARD out of date
.fmtset_hash_lines <- function(spec, a) {
  sets <- spec$format_sets
  if (!length(sets) || !nrow(a) || is.null(a$formats)) return(character())
  used <- .fmtsets_used(list(analyses = a, format_sets = sets))
  if (!length(used)) return(character())
  vapply(used, function(n) {
    paste0("format set ", n, ": ", paste(names(sets[[n]]), sets[[n]], sep = "=", collapse = " | "))
  }, "")
}

# The problems of a variable's set a report names (AGE: continuous_0) for a
# variable none of the report's continuous analyses analyses: a key the
# report wrote stops (a standard's that does not apply is not read: a
# template takes it out, .fmtset_std_prune())
.fmtset_var_problems <- function(a, x, keys) {
  pr <- list(.spec_problems_empty())
  if (is.null(a$formats) || !nrow(a)) return(pr[[1L]])
  cont <- vapply(seq_len(nrow(a)), function(i) {
    k <- .method_key(a$method[i], keys)
    !is.na(k) && identical(keys$kind[k], "continuous")
  }, NA)
  for (i in which(cont)) {
    f <- .parse_formats(a$formats[i])
    vk <- names(f)[!is.na(f) & .fmtset_name_ok(f) & names(f) != "*" &
                     !grepl(":", names(f), fixed = TRUE) & !.fmtset_is_stat_key(names(f))]
    if (!length(vk)) next
    same <- which(cont & a$output_id %in% a$output_id[i])
    vars <- unique(unlist(lapply(a$variables[same], .split_bar)))
    bad <- setdiff(vk, vars)
    if (length(bad)) {
      pr[[length(pr) + 1L]] <- .ard_problem(a, i, "formats", sprintf(paste0(
        "%s / %s: formats names a set for %s, which no continuous analysis of the report ",
        "analyses (its variables: %s). Write a variable it analyses, or take the key out."),
        a$output_id[i], a$analysis_id[i], paste(bad, collapse = ", "),
        if (length(vars)) paste(vars, collapse = ", ") else "none"))
    }
  }
  do.call(rbind, pr)
}

# A template's formats with the standard's variable sets the report does
# not use taken out (canon 649: a standard's key that does not apply is not
# read).  `fm`: the formats as resolved; `given`: the names the report
# wrote; `args`: the template's other arguments, whose values name the
# variables (continuous: [AGE, BMIBL], params: [ALT]).
.fmtset_std_prune <- function(fm, given, args) {
  if (!is.list(fm) && !(is.atomic(fm) && !is.null(names(fm)))) return(fm)
  if (is.null(names(fm)) || !length(fm)) return(fm)
  is_vset <- vapply(names(fm), function(k) {
    k != "*" && !grepl(":", k, fixed = TRUE) && !k %in% given && .fmtset_is_ref(fm[[k]]) &&
      !.fmtset_is_stat_key(k)
  }, NA)
  if (!any(is_vset)) return(fm)
  vals <- unique(as.character(unlist(lapply(args, function(v) {
    if (is.character(v) || is.list(v)) unlist(v, use.names = FALSE)
  }), use.names = FALSE)))
  keep <- !is_vset | names(fm) %in% vals
  out <- fm[keep]
  if (!length(out)) NULL else out
}

# ---- the plan_digits route (digset_<name>) -----------------------------------

# A set as plan_digits() takes it: a vector of decimals named by statistic
# (xx.x is 1).  A format that is no count of decimals (xx.x%, pvalue) stops:
# plan_digits() gives decimals only.
.digset_num <- function(set, name) {
  d <- vapply(names(set), function(s) {
    f <- set[[s]]
    if (grepl("^[0-9]+$", f)) return(as.integer(f))
    if (grepl("^x+([.]x+)?$", f)) return(nchar(sub("^x+[.]?", "", f)))
    .ard_stop(sprintf(paste0("format set %s: %s is %s, which plan_digits() cannot give (it takes ",
                             "decimals); write a number of decimals."), name, s, f))
  }, 0L)
  stats::setNames(as.numeric(d), names(set))
}

# A plan_digits() value that names a set: the set's name (`continuous_1`),
# or a map with the set under "*" and statistics over it
# (`{"*": continuous_1, sd: 3}`), as the set's digits (attribute
# "tfl_digset", its name; "tfl_digset_over", the statistics over it).  Any
# other value as it is.
.digset_value <- function(v, sets, where) {
  if (inherits(v, c("tfl_r", "tfl_unset"))) return(v)
  if (.fmtset_is_ref(v)) {
    out <- .digset_num(.fmtset_get(sets, v, where), v)
    attr(out, "tfl_digset") <- v
    return(out)
  }
  if ((is.list(v) || is.atomic(v)) && "*" %in% names(v)) {
    s <- v[["*"]]
    if (!.fmtset_is_ref(s)) {
      .ard_stop(sprintf("%s: \"*\" takes the name of one of the study's format sets, not %s.",
                        where, paste(format(s), collapse = ", ")))
    }
    out <- .digset_num(.fmtset_get(sets, s, where), s)
    over <- v[names(v) != "*"]
    if (length(over)) {
      ov <- vapply(over, function(z) as.numeric(z)[1L], 0)
      out[names(ov)] <- ov
    }
    attr(out, "tfl_digset") <- s
    attr(out, "tfl_digset_over") <- names(over)
    return(out)
  }
  v
}

# A set's digits with statistics written over them (a parameter's map,
# layer 9, over its set of settings.params, layer 6)
.digset_over <- function(base, over) {
  if (!length(over)) return(base)
  ov <- vapply(as.list(over), function(z) as.numeric(z)[1L], 0)
  out <- base
  out[names(ov)] <- ov
  attr(out, "tfl_digset") <- attr(base, "tfl_digset")
  attr(out, "tfl_digset_over") <- union(attr(base, "tfl_digset_over"), names(ov))
  out
}

# The plan_digits() items of a resolved report with their sets opened, the
# parameters' sets of settings.params (`pfmt`, PARAMCD = set) under the
# report's own PARAMCD entries (when its analyses are by PARAMCD: only then
# is a key a parameter's), and the PARAMCD entries of every item put
# together on the last (rtfreporter's later call replaces a key whole).
# `stats`: the continuous statistics the report's ARD computes, each of
# which a set must give; `shown`: the parameters the report shows (NULL:
# every one).
.digsets_content <- function(content, sets, pfmt = NULL, stats = character(), shown = NULL,
                             where = "plan_digits") {
  if (inherits(content, "tfl_r") || !length(content)) return(content)
  is_dg <- vapply(content, function(it) {
    inherits(it, "tfl_item") && identical(.schema_fn_name(it$fn %||% ""), "plan_digits")
  }, NA)
  if (!any(is_dg) && !length(pfmt)) return(content)
  check <- function(v, key) {
    if (is.null(attr(v, "tfl_digset"))) return(invisible())
    miss <- setdiff(stats, names(v))
    if (length(miss)) {
      .ard_stop(sprintf(paste0("%s: the format set %s has no %s (plan_digits %s). Add it to the set ",
                               "(settings.format_sets), or write it over the set ({\"*\": %s, %s: 1})."),
                        where, attr(v, "tfl_digset"), paste(miss, collapse = ", "), key,
                        attr(v, "tfl_digset"), miss[1L]))
    }
  }
  pmap <- list()
  if (length(pfmt)) {
    p <- if (is.null(shown)) pfmt else pfmt[names(pfmt) %in% shown]
    for (k in names(p)) {
      v <- .digset_value(p[[k]], sets, sprintf("study.yml: settings: params: %s", k))
      pmap[[k]] <- v
    }
  }
  last <- 0L
  for (i in which(is_dg)) {
    a <- content[[i]]$args %||% list()
    for (k in setdiff(names(a), c("", "rounding"))) {
      if (identical(k, "PARAMCD") && is.list(a[[k]]) && !is.null(names(a[[k]]))) {
        for (pc in names(a[[k]])) {
          v <- .digset_value(a[[k]][[pc]], sets, sprintf("%s: PARAMCD %s", where, pc))
          # (a parameter's map over its set of the study: statistics over it)
          if (is.null(attr(v, "tfl_digset")) && !is.null(attr(pmap[[pc]], "tfl_digset")) &&
              !is.null(names(v))) {
            v <- .digset_over(pmap[[pc]], v)
          }
          pmap[pc] <- list(v)
        }
        last <- i
        next
      }
      a[k] <- list(.digset_value(a[[k]], sets, sprintf("%s: %s", where, k)))
      check(a[[k]], k)
    }
    content[[i]]$args <- a
  }
  for (k in names(pmap)) check(pmap[[k]], paste("PARAMCD", k))
  if (length(pmap)) {
    if (last == 0L && any(is_dg)) last <- max(which(is_dg))
    if (last > 0L) {
      content[[last]]$args$PARAMCD <- pmap
      # (the others' PARAMCD: the last one holds them all)
      for (i in setdiff(which(is_dg), last)) content[[i]]$args$PARAMCD <- NULL
      content <- content[!vapply(seq_along(content), function(i) {
        is_dg[i] && i != last && !length(content[[i]]$args)
      }, NA)]
    } else {
      content <- c(content, list(tfl_item("plan_digits", list(PARAMCD = pmap))))
    }
  }
  content
}

# The values a set gave, as the program writes them: digset_<name> (with
# the statistics over it, replace()); the setup's lines of the sets used
.digsets_for_program <- function(content) {
  used <- character()
  one <- function(v) {
    n <- attr(v, "tfl_digset")
    if (is.null(n)) return(v)
    used <<- c(used, n)
    over <- attr(v, "tfl_digset_over")
    code <- paste0("digset_", n)
    if (length(over)) {
      code <- sprintf("replace(%s, %s, %s)", code,
                      if (length(over) > 1L) sprintf("c(%s)", paste0("\"", over, "\"", collapse = ", ")) else
                        paste0("\"", over, "\""),
                      if (length(over) > 1L) sprintf("c(%s)", paste(format(v[over]), collapse = ", ")) else
                        format(v[[over]]))
    }
    tfl_r(code)
  }
  if (inherits(content, "tfl_r")) return(content)
  out <- lapply(content, function(it) {
    if (!inherits(it, "tfl_item") || !identical(.schema_fn_name(it$fn %||% ""), "plan_digits")) return(it)
    for (k in names(it$args)) {
      if (identical(k, "PARAMCD") && is.list(it$args[[k]]) && !inherits(it$args[[k]], "tfl_r")) {
        it$args[[k]] <- lapply(it$args[[k]], one)
      } else {
        it$args[k] <- list(one(it$args[[k]]))
      }
    }
    it
  })
  attr(out, "digsets") <- unique(used)
  out
}

# The setup lines of the plan_digits sets a report's program uses
.digset_lines <- function(used, sets) {
  if (!length(used)) return(character())
  c("# the format sets this report uses (the study standard, settings.format_sets)",
    vapply(used, function(n) {
      d <- .digset_num(sets[[n]], n)
      paste(paste0("digset_", n), "<-",
            .lay(.cl("c", stats::setNames(as.list(format(d, trim = TRUE)), names(d))),
                 lead = nchar(n) + 11L))
    }, ""),
    "")
}

# The cells' own digits (`{mean:.2f}`) on a statistic a set gives too (the
# ARD route's formats, or plan_digits()'s): stops, naming the set (canon 404
# (1), after the set is opened).  `set_stats`: each set the report uses, by
# name, its statistics.
.fmtset_cells_clash <- function(content, set_stats, where) {
  if (!length(set_stats) || inherits(content, "tfl_r")) return(invisible())
  for (it in content) {
    if (!inherits(it, "tfl_item") || !identical(.schema_fn_name(it$fn %||% ""), "plan_cells")) next
    txt <- .cell_templates(it$args)
    for (t in txt) {
      for (tk in .cell_tokens(t)) {
        if (is.null(tk$spec) || !grepl("^[.][0-9]+[fs]", tk$spec)) next
        nm <- tk$name
        for (s in names(set_stats)) {
          if (nm %in% set_stats[[s]]) {
            .ard_stop(sprintf(paste0("%s: the cell %s gives %s its digits, and the format set %s does too. ",
                                     "Write the token without digits ({%s}), or take %s out of the set's place."),
                              where, tk$token, nm, s, nm, s))
          }
        }
      }
    }
  }
  invisible()
}

# A resolved report's sets: its plan_digits() values that name a set opened
# (with settings.params' parameters' sets, when its analyses are by
# PARAMCD), and the cells' own digits checked against every set it uses
.fmtsets_output <- function(output, study) {
  sets <- .study_format_sets(study)
  an <- (if (is.list(output$ard) && !inherits(output$ard, "tfl_r")) output$ard$analyses) %||% list()
  if (is.data.frame(an)) an <- lapply(seq_len(nrow(an)), function(i) as.list(an[i, , drop = FALSE]))
  keys <- tfl_ard_methods()
  kind <- function(r) {
    k <- .method_key(as.character(r$method %||% NA)[1L], keys)
    if (is.na(k)) "" else keys$kind[k]
  }
  flat <- function(v) as.character(unlist(v, use.names = FALSE))
  cont <- Filter(function(r) is.list(r) && identical(kind(r), "continuous"), an)
  stats <- unique(unlist(lapply(cont, function(r) {
    s <- flat(r$statistics)
    if (length(s)) s else .fmtset_stats(list(statistics = NA))
  })))
  by_pc <- any(vapply(an, function(r) is.list(r) && "PARAMCD" %in% flat(r$by), NA))
  pfmt <- if (by_pc) .study_param_formats(study, sets)
  tp <- attr(output, "template")
  shown <- if (is.list(tp)) flat(tp$args$params)
  if (!length(shown)) shown <- NULL
  where <- sprintf("%s: plan_digits", output$id %||% "report")
  output$content <- .digsets_content(output$content, sets, pfmt, stats, shown, where)
  # the sets it uses, each with its statistics: the cells' own digits on one
  # of them stop
  used <- list()
  for (r in cont) {
    f <- r$formats
    if (is.list(f) || (is.atomic(f) && !is.null(names(f)))) {
      f <- unlist(f)
    } else {
      f <- .parse_formats(paste(flat(f), collapse = " | "))
    }
    for (k in names(f)) {
      v <- f[[k]]
      if (!grepl(":", k, fixed = TRUE) && .fmtset_is_ref(v) && v %in% names(sets)) {
        used[[v]] <- names(sets[[v]])
      }
    }
  }
  if (!inherits(output$content, "tfl_r")) {
    for (it in output$content) {
      if (!inherits(it, "tfl_item") || !identical(.schema_fn_name(it$fn %||% ""), "plan_digits")) next
      vals <- c(it$args[setdiff(names(it$args), "PARAMCD")], it$args$PARAMCD)
      for (v in vals) {
        n <- attr(v, "tfl_digset")
        if (!is.null(n)) used[[n]] <- setdiff(names(v), attr(v, "tfl_digset_over"))
      }
    }
  }
  .fmtset_cells_clash(output$content, used, output$id %||% "report")
  attr(output, "study_format_sets") <- sets
  # (a template's call cannot say the study's sets: its items are written)
  if (is.list(tp) && any(vapply(output$content, function(it) {
    inherits(it, "tfl_item") && any(vapply(c(it$args, it$args$PARAMCD), function(v) {
      !is.null(attr(v, "tfl_digset"))
    }, NA))
  }, NA))) {
    tp$ok <- FALSE
    attr(output, "template") <- tp
  }
  output
}
