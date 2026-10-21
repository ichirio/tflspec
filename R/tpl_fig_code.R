# ---- figure templates: a report's program (moved from tpl_spec.R) ---------------

# A figure template report's program (the A use): the data steps written
# out (dplyr), the numbers its own items print from the ARD, then the
# template's call on the data -- the fit, the layers and the settings are
# the call's -- with the report's own items `+`-ed after it, each the
# design's piece for it (a layer, a figure-wide call), and its own settings
# (`plot:`) the call's `plot =`
.content_fig_tpl_code <- function(output, name = "content") {
  tp <- attr(output, "template")
  t <- .tpl_get(tp$name, "tfl_content_code()")
  steps <- output$data$steps %||% list()
  n_call <- tp$call_steps
  data_steps <- steps[seq_len(length(steps) - n_call)]
  lv <- .codelist_levels(.codelists_rows(output$codelists, output$id), output$id)
  d <- .fig_steps_code(data_steps, lv)
  # the call: the parameters it takes that the SPEC gives (or the study's
  # defaults), a column bare
  fun <- get(t$call, envir = asNamespace(t$package %||% "tflspec"), mode = "function")
  keep <- intersect(setdiff(names(formals(fun)), c("data", "plot")), names(tp$args))
  arg_code <- vapply(keep, function(k) {
    v <- tp$args[[k]]
    col <- identical(t$params[[k]]$type, "vars") && is.character(v) && length(v) >= 1L
    paste(k, "=", if (!col) .fig_arg_code(v) else if (length(v) == 1L) v
                  else sprintf("c(%s)", paste(v, collapse = ", ")))
  }, "")
  own_plot <- Filter(Negate(is.null), lapply(stats::setNames(nm = names(output$plot)), function(k) {
    if (!identical(output$plot[[k]], tp$plot[[k]])) output$plot[[k]]
  }))
  if (length(own_plot)) arg_code <- c(arg_code, paste("plot =", .fig_arg_code(own_plot)))
  # (a template drawn from the ARD (forest_hr) takes `ard`; one of panels
  # side by side is assembled after the report's own items, on `fig`)
  form <- .tpl_fig_forms[[t$from %||% tp$name]] %||% list()
  call <- sprintf("%s(%s)", t$call, paste(c(form$input %||% "df", arg_code), collapse = ", "))
  # the report's own items after it
  pre <- character()
  terms <- character()
  libs <- c("dplyr", "ggplot2", d$libs)
  needs <- character()
  n_lab <- 0L
  for (i in seq_along(tp$rest)) {
    x <- .fig_item_as_piece(tp$rest[[i]])
    if (identical(x$to, "add")) {
      res <- .fig_call_code(x$piece)
      terms <- c(terms, res$call)
      needs <- c(needs, res$pkgs)
      next
    }
    lc <- .fig_layer_code(x$piece, i, output$plot)
    code <- lc$code
    # a number of the ARD: its text in the data part, ard_label_1 ...
    if (!is.null(lc$pre)) {
      n_lab <- n_lab + 1L
      mark <- sprintf(".ARDLABEL%d.", i)
      pre <- c(pre, gsub(mark, paste0("ard_label_", n_lab), lc$pre, fixed = TRUE))
      code <- gsub(mark, paste0("ard_label_", n_lab), code, fixed = TRUE)
    }
    terms <- c(terms, unlist(.fig_terms(code)))
    needs <- c(needs, lc$package)
    libs <- c(libs, lc$libs)
  }
  code <- c(
    sprintf("# template %s%s", tp$name,
            if (length(tp$study)) paste0("; study defaults: ", paste(tp$study, collapse = ", ")) else ""),
    # (no data part: a template drawn from the ARD)
    if (length(d$code) || length(pre)) section("data"),
    d$code,
    if (length(pre)) c("# the numbers it prints, from the ARD", pre, ""),
    section("plot"),
    if (is.null(form$assemble)) .fig_plus_chain(name, c(call, terms))
    else c(.fig_plus_chain("fig", c(call, terms)), sprintf("%s <- %s(fig)", name, form$assemble)))
  code <- .code_lines(code)
  code <- .fig_rename_df(code, tfl_fig_data_name(list(data = steps)))
  code <- .fig_tidy_names(code)
  .fig_code_out(c(.fig_units_used(code), code), d$reads, unique(libs),
                setdiff(unique(needs), c("ggplot2", "ggsurvfit", "patchwork")),
                ard = length(pre) > 0L)
}
