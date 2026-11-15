# The clinical figures as whole programs, through a SPEC and back: each
# program of dev/spec-coverage-ggplot2-programs.R is read by
# tfl_fig_content_from_r(), its content written back as the figure's code
# (what a figure report generates) and that code run on the same data; the
# figure is judged against the program's own, drawn as it is.
#
#   Rscript dev/spec-coverage-ggplot2-program-measure.R [out.csv] [programs.R]
#
# Run with the tflspec to measure installed.  One row a program:
#   read     tfl_fig_content_from_r() reads it (no error)
#   runs     the code written back runs and makes the figure
#   same     it draws the same: the layers' data, the axes' labels, the
#            plot's labels and its theme as the program's
#   values   the items' argument values, typed / R (!r, an item of R)
#   problems the check's errors / warnings on the items
# A program counts 1 when it reads, runs and draws the same with no value
# of R and no problem, 0.5 when it draws the same with values of R or
# warnings, 0 otherwise; the percentage is the programs' mean.

suppressPackageStartupMessages({
  library(tflspec)
  library(ggplot2)
  library(dplyr)
  library(ggsurvfit)
  library(patchwork)
})
`%||%` <- function(a, b) if (is.null(a)) b else a
ns <- asNamespace("tflspec")
# (the extensions' namespaces loaded first: reading a program looks at
# their exports, and ggh4x adds a theme element when loaded)
for (pkg in get(".fig_prog_pkgs", ns)) suppressWarnings(requireNamespace(pkg, quietly = TRUE))
content_code <- get(".fig_content_code", ns)
r_number <- get(".schema_r_number", ns)
r_numbers <- get(".schema_r_numbers", ns)
r_texts <- get(".schema_r_texts", ns)

argv <- commandArgs(trailingOnly = TRUE)
out_csv <- if (length(argv)) argv[1L] else file.path("inst", "dev", "spec-coverage-ggplot2-programs.csv")
out_csv <- file.path(normalizePath(dirname(out_csv)), basename(out_csv))
src <- if (length(argv) > 1L) argv[2L] else file.path("dev", "spec-coverage-ggplot2-programs.R")
src <- normalizePath(src)

lines <- readLines(src, warn = FALSE)
marks <- grep("^#### program: ", lines)
progs <- lapply(seq_along(marks), function(i) {
  to <- if (i < length(marks)) marks[i + 1L] - 1L else length(lines)
  list(name = sub("^#### program: *", "", lines[marks[i]]), code = lines[(marks[i] + 1L):to])
})
data_env <- new.env(parent = globalenv())
eval(parse(text = lines[seq_len(marks[1L] - 1L)]), data_env)

# the values of the items: typed (a value of the SPEC's) or R
count_values <- function(items) {
  n <- c(typed = 0L, r = 0L)
  walk <- function(v) {
    if (inherits(v, "tfl_r")) {
      txt <- as.character(unclass(v))
      ok <- identical(txt, "NULL") || !is.null(r_number(v)) || !is.null(r_numbers(v)) ||
        !is.null(r_texts(v))
      n[[if (ok) "typed" else "r"]] <<- n[[if (ok) "typed" else "r"]] + 1L
      return(invisible())
    }
    if (inherits(v, "tfl_call")) v <- v$args
    if (is.list(v) && length(v)) {
      for (e in v) walk(e)
      return(invisible())
    }
    n[["typed"]] <<- n[["typed"]] + 1L
  }
  for (it in items) {
    if (inherits(it, "tfl_plot")) {
      sub <- count_values(it$items)
      n <- n + sub
      next
    }
    if (identical(it$fn, ".r")) {
      n[["r"]] <- n[["r"]] + 1L
      next
    }
    for (a in it$args) walk(a)
  }
  n
}

# the program run as it is: the figure it prints or saves last
run_program <- function(code, env) {
  shown <- NULL
  env$ggsave <- function(filename, plot = ggplot2::last_plot(), ...) shown <<- plot
  env$print <- function(x, ...) if (inherits(x, "ggplot")) shown <<- x else base::print(x, ...)
  for (e in as.list(parse(text = code, keep.source = FALSE))) {
    v <- withVisible(eval(e, env))
    if (v$visible && (inherits(v$value, "ggplot") || inherits(v$value, "patchwork"))) shown <- v$value
  }
  shown
}

plot_theme <- utils::getFromNamespace("plot_theme", "ggplot2")
fingerprint <- function(p) {
  b <- ggplot_build(p)
  pp <- b$layout$panel_params[[1L]]
  labs_of <- function(s) tryCatch(s$get_labels(), error = function(e) NULL)
  th <- plot_theme(p)
  list(data = lapply(b$data, function(d) d[order(names(d))]),
       x = labs_of(pp$x), y = labs_of(pp$y),
       labels = ggplot2::get_labs(p)[sort(names(ggplot2::get_labs(p)))],
       theme = th[sort(names(th))])
}

old_wd <- setwd(tempdir())
default_theme <- theme_get()
rows <- lapply(progs, function(pg) {
  theme_set(default_theme)
  env <- new.env(parent = data_env)
  orig <- tryCatch(suppressWarnings(run_program(pg$code, env)), error = function(e) e)
  if (inherits(orig, "error") || is.null(orig)) {
    return(data.frame(program = pg$name, read = NA, runs = NA, same = NA, typed = NA, r = NA,
                      errors = NA, warnings = NA, score = NA,
                      note = paste("the program itself does not draw:",
                                   if (inherits(orig, "error")) conditionMessage(orig) else "no plot")))
  }
  fp0 <- suppressWarnings(fingerprint(orig))
  theme_set(default_theme)
  x <- tryCatch(tfl_fig_content_from_r(pg$code), error = function(e) e)
  if (inherits(x, "error")) {
    return(data.frame(program = pg$name, read = FALSE, runs = FALSE, same = FALSE, typed = 0L,
                      r = 0L, errors = NA, warnings = NA, score = 0,
                      note = conditionMessage(x)))
  }
  n <- count_values(x$content)
  pr <- attr(x, "problems")
  n_err <- sum(pr$level == "error")
  n_warn <- sum(pr$level != "error")
  code <- tryCatch(content_code(list(id = "f", data = x$data, content = x$content)),
                   error = function(e) e)
  note <- character()
  runs <- FALSE
  same <- FALSE
  if (inherits(code, "error")) {
    note <- c(note, paste("not written:", conditionMessage(code)))
  } else {
    env2 <- new.env(parent = data_env)
    env2$ggsave <- function(...) invisible()
    got <- tryCatch(suppressWarnings({
      eval(parse(text = code, keep.source = FALSE), env2)
      env2$content
    }), error = function(e) e)
    if (inherits(got, "error") || is.null(got)) {
      note <- c(note, paste("the code written does not run:",
                            if (inherits(got, "error")) conditionMessage(got) else "no `content`"))
    } else {
      runs <- TRUE
      fp1 <- tryCatch(suppressWarnings(fingerprint(got)), error = function(e) e)
      if (inherits(fp1, "error")) {
        note <- c(note, paste("does not draw:", conditionMessage(fp1)))
      } else {
        diff <- names(fp0)[!vapply(names(fp0), function(k) isTRUE(all.equal(fp0[[k]], fp1[[k]])), NA)]
        same <- !length(diff)
        if (!same) note <- c(note, paste("draws otherwise:", paste(diff, collapse = ", ")))
      }
    }
  }
  if (n[["r"]] > 0L) note <- c(note, sprintf("%d value(s) of R", n[["r"]]))
  if (nrow(pr)) note <- c(note, paste(unique(paste0(pr$fn, ": ", pr$message)), collapse = "; "))
  score <- if (!same || n_err > 0L) 0 else if (n[["r"]] > 0L || n_warn > 0L) 0.5 else 1
  data.frame(program = pg$name, read = TRUE, runs = runs, same = same, typed = n[["typed"]],
             r = n[["r"]], errors = n_err, warnings = n_warn, score = score,
             note = paste(note, collapse = " | "))
})
setwd(old_wd)
theme_set(default_theme)
res <- do.call(rbind, rows)
utils::write.csv(res, out_csv, row.names = FALSE)
ok <- !is.na(res$score)
cat(sprintf("programs: %d (%d judged); read %d, run %d, draw the same %d; values typed %d / R %d\n",
            nrow(res), sum(ok), sum(res$read[ok]), sum(res$runs[ok]), sum(res$same[ok]),
            sum(res$typed[ok]), sum(res$r[ok])))
cat(sprintf("score: %.1f%%\n", 100 * mean(res$score[ok])))
print(res[c("program", "read", "runs", "same", "typed", "r", "score")], row.names = FALSE)
