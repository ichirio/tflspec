# ggplot2's own examples, written by a SPEC: each call of a function in the
# scope (dev/spec-coverage-ggplot2.R's) in the examples of ggplot2's help
# pages is read as a SPEC item (.fig_lang_item(): what tflspec reads from R
# code), checked (tfl_schema_check()), written back as code
# (.fig_item_lang()), and the example drawn with that code in its place:
# the same data, layout, theme, labels and scales' labels as the original.
#
#   Rscript dev/spec-coverage-ggplot2-examples.R [out.csv] [figures.R]
#
# With figures.R the calls are that file's instead of the help pages'
# (dev/spec-coverage-ggplot2-clinical.R: the clinical figures), its
# figures `fig_<name> <- ...` the pages.
#
# Run with the tflspec to measure installed; a few minutes (every example
# drawn twice, then once a call).  The examples are read from the
# installed ggplot2's help (tools::Rd_db()), not copied.
#
# One row a function's argument as the examples write it (and "" for a
# call with none).  A use of it is
#   yes      read as a typed value (not !r), no problem of the check on it,
#            and the example draws the same
#   partial  read as R (!r), or with R in it ({fn: dup_axis, args: {labels:
#            !r scales::label_time()}}), or a warning of the check on it,
#            and it draws the same (!r NULL and a number written as
#            arithmetic, !r 1/5 or !r 2^(-2:2), and texts built of
#            constants, !r paste0("W", 1:4), are typed: the check judges
#            them)
#   no       an error of the check on it, the call not read as an item, or
#            the example drawn otherwise (or not at all) with the code
# An argument's coverage is its uses' mean (yes 1, partial 0.5); a
# function's its arguments' mean; the percentage the functions' mean by
# the measure's weights.  An example that does not draw by itself (a
# package not installed, its data drawn at random twice otherwise, a
# global theme set) is not judged on its drawing; a call inside a
# function's definition (aes({{ x }})) is no SPEC's and left out.

suppressPackageStartupMessages({
  library(tflspec)
  library(ggplot2)
})
ns <- asNamespace("tflspec")
`%||%` <- function(a, b) if (is.null(a)) b else a
lang_item <- get(".fig_lang_item", ns)
item_lang <- get(".fig_item_lang", ns)
dep <- get(".fig_deparse", ns)
alias_of <- get(".schema_gg_alias", ns)
# (a tflspec before the check took a number written as arithmetic: none)
r_number <- if (exists(".schema_r_number", ns)) get(".schema_r_number", ns) else function(v) NULL
# (numbers written as arithmetic, !r 2^(-2:2): typed where a list<number> goes)
r_numbers <- if (exists(".schema_r_numbers", ns)) get(".schema_r_numbers", ns) else function(v) NULL
# (texts built of constants, !r paste0("W", seq(0, 24, by = 4)): typed where a list<string> goes)
r_texts <- if (exists(".schema_r_texts", ns)) get(".schema_r_texts", ns) else function(v) NULL
# the R in a value (any !r but NULL and a number), the first, or NULL
r_in <- function(v) {
  if (inherits(v, "tfl_r")) {
    txt <- as.character(unclass(v))
    return(if (identical(txt, "NULL") || !is.null(r_number(v)) || !is.null(r_numbers(v)) ||
                 !is.null(r_texts(v))) NULL else txt)
  }
  if (inherits(v, "tfl_call")) v <- v$args
  if (is.list(v)) for (e in v) if (!is.null(r <- r_in(e))) return(r)
  NULL
}

# the scope and its weights: dev/spec-coverage-ggplot2.R's `scope`
for (e in parse(file.path("dev", "spec-coverage-ggplot2.R"))) {
  if (is.call(e) && identical(e[[1L]], as.name("<-")) && identical(e[[2L]], as.name("scope"))) eval(e)
}
scope <- scope[names(scope) != "geom_stat"]
weight_of <- unlist(unname(scope))
group_of <- stats::setNames(rep(names(scope), lengths(scope)), unlist(lapply(scope, names)))
scope_fn <- function(f) {
  if (f %in% names(weight_of)) return(f)
  a <- alias_of(f)
  if (a %in% names(weight_of)) a else NA_character_
}

is_scoped <- function(e) is.call(e) && is.name(e[[1L]]) && !is.na(scope_fn(as.character(e[[1L]])))
is_missing <- function(x) identical(x, quote(expr = ))
# the places of the scope's calls in an expression (not inside one: a
# call's arguments are read with it), none inside a function's definition
paths_of <- function(e, p = integer()) {
  if (!is.call(e)) return(list())
  if (identical(e[[1L]], as.name("function"))) return(list())
  if (is_scoped(e)) return(list(p))
  out <- list()
  for (i in seq_along(e)[-1L]) {
    if (!is.null(e[[i]]) && !is_missing(e[[i]])) out <- c(out, paths_of(e[[i]], c(p, i)))
  }
  out
}
get_at <- function(e, p) {
  for (i in p) e <- e[[i]]
  e
}
set_at <- function(e, p, v) {
  if (!length(p)) return(v)
  e[[p[1L]]] <- set_at(e[[p[1L]]], p[-1L], v)
  e
}
quiet <- function(expr) {
  withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"),
                      message = function(m) invokeRestart("muffleMessage"))
}
failed <- function(err) structure(list(msg = conditionMessage(err)), class = "failed")
run <- function(e, env) {
  set.seed(1L)
  tryCatch(quiet(eval(e, env)), error = failed)
}
# what a plot draws: its built data and layout, theme, labels, scales' labels
drawn <- function(v) {
  if (inherits(v, "failed") || !inherits(v, "ggplot")) return(v)
  tryCatch({
    set.seed(1L)
    b <- quiet(ggplot_build(v))
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    quiet(ggplotGrob(v))
    list(data = b$data, layout = b$layout$layout, theme = v$theme, labels = v$labels,
         scales = lapply(b$plot$scales$scales, function(s) tryCatch(s$get_labels(), error = function(e) NULL)))
  }, error = failed)
}
same <- function(a, b) isTRUE(all.equal(a, b, check.environment = FALSE))

old_theme <- theme_get()
figures <- commandArgs(TRUE)[2L]
# the pages: the help pages' examples, or a file's figures (one page)
pages <- if (is.na(figures)) {
  rd <- tools::Rd_db("ggplot2")
  lapply(stats::setNames(nm = names(rd)), function(page) {
    f <- tempfile(fileext = ".R")
    tools::Rd2ex(rd[[page]], f, commentDontrun = TRUE, commentDonttest = TRUE)
    if (file.exists(f)) tryCatch(parse(f), error = function(e) NULL)
  })
} else {
  stats::setNames(list(parse(figures)), basename(figures))
}
# the page of a row: a figure's name when the file names them
page_of <- function(page, e) {
  if (!is.na(figures) && is.call(e) && identical(e[[1L]], as.name("<-")) && is.name(e[[2L]])) {
    return(sub("^fig_", "", as.character(e[[2L]])))
  }
  sub("[.]Rd$", "", page)
}
uses <- list()
use <- function(...) uses[[length(uses) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE)
for (page in names(pages)) {
  exprs <- pages[[page]]
  if (is.null(exprs)) next
  env <- new.env(parent = globalenv())
  for (e in exprs) {
    ps <- paths_of(e)
    snap <- as.list(env, all.names = TRUE)
    fresh <- function() list2env(snap, parent = globalenv())
    d0 <- NULL
    judged <- FALSE
    if (length(ps)) {
      d0 <- drawn(run(e, fresh()))
      theme_set(old_theme)
      d00 <- drawn(run(e, fresh()))
      theme_set(old_theme)
      judged <- is.list(d0) && !inherits(d0, "failed") && !is.null(d0$data) && same(d0, d00)
    }
    for (p in ps) {
      t <- get_at(e, p)
      fn <- scope_fn(as.character(t[[1L]]))
      it <- tryCatch(lang_item(t), error = failed)
      base <- list(fn = fn, page = page_of(page, e), code = dep(t))
      if (inherits(it, "failed") || identical(it$fn, ".r")) {
        use(base, arg = "", status = "no", note = if (inherits(it, "failed")) it$msg else "read as R, the whole call")
        next
      }
      pr <- tryCatch(tfl_schema_check(it), error = function(err) {
        data.frame(arg = "", level = "error", message = conditionMessage(err))
      })
      back <- tryCatch(item_lang(it), error = failed)
      d1 <- if (judged && !inherits(back, "failed")) {
        r <- drawn(run(set_at(e, p, back), fresh()))
        theme_set(old_theme)
        r
      }
      draw_ok <- !judged || (!inherits(back, "failed") && same(d0, d1))
      draw_note <- if (inherits(back, "failed")) paste("not written:", back$msg) else
        if (inherits(d1, "failed")) paste("the code does not draw:", d1$msg) else
          if (!draw_ok) paste("drawn otherwise:", dep(back)) else ""
      args <- names(it$args) %||% character()
      if (!length(args)) args <- ""
      for (a in args) {
        v <- if (nzchar(a)) it$args[[a]]
        pa <- pr[pr$arg %in% c(a, "") | (identical(a, "mapping") & pr$arg == "mapping"), , drop = FALSE]
        r <- r_in(v)
        status <- if (!draw_ok || any(pa$level == "error")) "no" else
          if (!is.null(r) || any(pa$level == "warning")) "partial" else "yes"
        note <- c(draw_note, if (nrow(pa)) paste(pa$message, collapse = "; "),
                  if (!is.null(r)) paste("as R:", r))
        use(base, arg = a, status = status, note = paste(note[nzchar(note)], collapse = "; "))
      }
    }
    run(e, env)
    theme_set(old_theme)
  }
}
u <- do.call(rbind, uses)
u$score <- c(yes = 1, partial = 0.5, no = 0)[u$status]

# one row a function's argument
key <- paste(u$fn, u$arg, sep = "\r")
rows <- lapply(split(seq_len(nrow(u)), key), function(i) {
  s <- mean(u$score[i])
  bad <- i[u$score[i] < 1]
  data.frame(package = "ggplot2", `function` = u$fn[i[1L]], argument = u$arg[i[1L]],
             covered = if (s == 1) "yes" else if (s == 0) "no" else "partial",
             where = paste(unique(u$page[i]), collapse = "; "),
             note = if (length(bad)) sprintf("%d of %d uses: %s (%s)", length(bad), length(i),
                                             u$note[bad[1L]], u$code[bad[1L]]) else "",
             group = group_of[[u$fn[i[1L]]]], weight = weight_of[[u$fn[i[1L]]]],
             uses = length(i), score = s, check.names = FALSE, stringsAsFactors = FALSE)
})
cov <- do.call(rbind, rows)
cov <- cov[order(cov$group, cov$`function`, cov$argument), , drop = FALSE]

pct <- function(d) {
  if (!nrow(d)) return(NA_real_)
  f <- stats::aggregate(score ~ `function` + weight, data = d, FUN = mean)
  round(100 * sum(f$score * f$weight) / sum(f$weight), 1)
}
main <- cov[cov$group != "stat", , drop = FALSE]
cat(sprintf("ggplot2's examples written by a SPEC (geoms, scales, coords, themes, facets, guides, labs, positions): %.1f%%\n",
            pct(main)))
cat(sprintf("stat_*(): %.1f%%\n", pct(cov[cov$group == "stat", , drop = FALSE])))
for (g in names(scope)) cat(sprintf("  %-9s %5.1f%%\n", g, pct(cov[cov$group == g, , drop = FALSE])))
cat("functions:", length(unique(cov$`function`)), " arguments:", nrow(cov), " yes:", sum(cov$covered == "yes"),
    " partial:", sum(cov$covered == "partial"), " no:", sum(cov$covered == "no"), "\n")
cat("calls:", nrow(unique(u[, c("page", "code")])), " uses:", nrow(u), " yes:", sum(u$status == "yes"),
    " partial:", sum(u$status == "partial"), " no:", sum(u$status == "no"), "\n")

out <- commandArgs(TRUE)[1L]
if (!is.na(out)) {
  cov <- cov[, c("package", "function", "argument", "covered", "where", "note", "group", "weight", "uses")]
  utils::write.csv(cov, out, row.names = FALSE, fileEncoding = "UTF-8")
}
