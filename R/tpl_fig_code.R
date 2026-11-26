# ---- figure templates: the call (tfl_tpl()) and a report's program ---------------
#
# A figure template is called as a table's is: tfl_tpl(<data>, "<name>", ...)
# on the data the program made (its data steps: the dataset read, the
# analysis set's subjects, the parameter), a ggplot back.  The arguments
# of the data steps (tfl_param(step = "data")) are the program's, not the
# call's.  (The figures' former functions, tpl_mean_se() ..., stay.)

# Parameters the data step takes (step "data"); the rest the call's
.tpl_fig_data_stage <- function(params, names = c("population", "subset", "data", "where")) {
  lapply(params, function(p) {
    if (p$name %in% names) p$step <- "data"
    p
  })
}

# The design the call draws, from the template's arguments: the layout's
# (its root's) function, by name
.tpl_fig_draws <- list(
  km = function(a, name = "km") .tpl_km_layout(a, name),
  mean_se = function(a) {
    .tpl_mean_design("se", a$group, a$value, a$visit_var, a$visit_order, a$param, a$data)
  },
  mean_ci = function(a) {
    .tpl_mean_design("ci", a$group, a$value, a$visit_var, a$visit_order, a$param, a$data)
  },
  waterfall = function(a) .tpl_wf_design(a$value, a$by_response, a$param, a$data, a$where),
  forest_hr = function(a) {
    f <- .tpl_forest_design(a$group, a$subgroups, a$comparison, .tpl_forest_look_args(a),
                            ref = a$ref)
    # (the text column beside it: tpl_forest_layout(), after the report's items)
    f$content <- Filter(function(it) !identical(it$fn, "text_column"), f$content)
    f
  })

# The arguments of a call: a column bare or as text (a parameter of type
# vars), any other value as it is
.tpl_fig_dots <- function(t, dots, fn) {
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    .ard_stop(sprintf("%s: %s's parameters are given by name.", fn, t$name))
  }
  lapply(stats::setNames(nm = names(dots)), function(k) {
    p <- t$params[[k]]
    if (!is.null(p) && identical(p$type, "vars")) {
      if (isTRUE(p$multiple)) .tpl_cols(dots[[k]]) else .tpl_col(dots[[k]])
    } else rlang::eval_tidy(dots[[k]])
  })
}

# tfl_tpl()'s figure: the template's design drawn on `x`
.tpl_fig_call <- function(t, x, dots) {
  fn <- "tfl_tpl()"
  args <- .tpl_fig_dots(t, dots, fn)
  # `plot`: the figure's other settings (a template without the parameter
  # takes it too, as its former function did)
  plot <- NULL
  if (!"plot" %in% names(t$params)) {
    plot <- args$plot
    args$plot <- NULL
  }
  data_stage <- names(args)[vapply(names(args), function(k) {
    identical(t$params[[k]]$step, "data")
  }, NA)]
  if (length(data_stage)) {
    .ard_stop(sprintf(paste0("%s: %s's %s: the program's data step's (the data `x` is made ",
                             "with it), not the call's."),
                      fn, t$name, paste(sQuote(data_stage, FALSE), collapse = ", ")))
  }
  # (the data step's parameters: not needed by the call)
  for (k in names(t$params)) if (identical(t$params[[k]]$step, "data")) t$params[[k]]$required <- FALSE
  a <- .tpl_args(t, args, fn)
  root <- t$root %||% t$name
  draw <- .tpl_fig_draws[[root]]
  if (is.null(draw)) {
    .ard_stop(sprintf("%s: %s is a figure template of %s; tfl_tpl() draws tflspec's.", fn, t$name,
                      t$package %||% "tflspec"))
  }
  form <- .tpl_fig_forms[[root]] %||% list()
  # (the template's name: what its warnings say)
  f <- if ("name" %in% names(formals(draw))) draw(a, t$name) else draw(a)
  .tpl_fig_draw(t$name, f, x, plot, as = if (identical(form$input, "ard")) "ard" else "df")
}

# A figure template report's program (the A use): the data steps written
# out (dplyr), the numbers its own items print from the ARD, then the
# template's call on the data -- tfl_tpl(<data>, "<name>", ...): the fit,
# the layers and the settings are the call's -- with the report's own
# items `+`-ed after it, each the design's piece for it (a layer, a
# figure-wide call).  The call's arguments: the call's parameters (step
# "call") the report or the study standard gives; the report's own
# settings (`plot:`) the arguments of the same name, the rest `plot =`.
.content_fig_tpl_code <- function(output, name = "content") {
  tp <- attr(output, "template")
  t <- .tpl_get(tp$name, "tfl_content_code()")
  steps <- output$data$steps %||% list()
  n_call <- tp$call_steps
  data_steps <- steps[seq_len(length(steps) - n_call)]
  lv <- .codelist_levels(.codelists_rows(output$codelists, output$id), output$id)
  # (the report's subjects, when made: its reads joined to them)
  d <- .fig_steps_code(data_steps, lv, order = TRUE, subjects = attr(output, "fig_subjects"))
  generic <- identical(t$call, "tfl_tpl")
  args <- tp$args
  called <- tp$name
  # a variant of a company's or a study's: its root, its defaults written
  # out (the program does not library() the company's package)
  if (generic && !is.null(t$from) && !identical(t$package, "tflspec")) {
    args <- c(t$defaults[setdiff(names(t$defaults), names(args))], args)
    called <- t$root %||% t$from
  }
  keep <- if (generic) {
    names(args)[vapply(names(args), function(k) {
      !is.null(t$params[[k]]) && !identical(t$params[[k]]$step, "data") && k != "plot"
    }, NA)]
  } else {
    fun <- get(t$call, envir = asNamespace(t$package %||% "tflspec"), mode = "function")
    intersect(setdiff(names(formals(fun)), c("data", "plot")), names(args))
  }
  keep <- intersect(names(t$params), keep)
  # the report's own settings: an argument of the same name, else `plot =`
  own_plot <- Filter(Negate(is.null), lapply(stats::setNames(nm = names(output$plot)), function(k) {
    if (!identical(output$plot[[k]], tp$plot[[k]])) output$plot[[k]]
  }))
  # (its `add` follows the template's own, as the call joins them: the
  # draft's pieces alone are written)
  nb <- length(tp$plot$add)
  if (nb && length(own_plot$add) > nb && identical(own_plot$add[seq_len(nb)], tp$plot$add)) {
    own_plot$add <- own_plot$add[-seq_len(nb)]
  }
  as_arg <- if (generic) intersect(names(own_plot), intersect(.tpl_km_plot_args,
                                                                 names(.tpl_shown(t$params))))
  for (k in as_arg) args[[k]] <- own_plot[[k]]
  keep <- c(keep, setdiff(as_arg, keep))
  own_plot <- .merge_map(if (generic) args$plot, own_plot[setdiff(names(own_plot), as_arg)])
  # (the draft's `add` pieces are layer specs, not values `plot =` takes:
  # they follow the call as `+` terms, after the template's own, as it joins them)
  plot_add <- own_plot$add
  own_plot$add <- NULL
  arg_code <- vapply(keep, function(k) {
    v <- args[[k]]
    col <- identical(t$params[[k]]$type, "vars") && is.character(v) && length(v) >= 1L
    paste(k, "=", if (!col) .fig_arg_code(v) else if (length(v) == 1L) v
                  else sprintf("c(%s)", paste(v, collapse = ", ")))
  }, "")
  # (a list: the settings keep their types -- a number, a flag, a text)
  if (length(own_plot)) arg_code <- c(arg_code, paste("plot =", sub("^c[(]", "list(", .fig_arg_code(own_plot))))
  # (a template drawn from the ARD (forest_hr) takes `ard`; one of panels
  # side by side is assembled after the report's own items, on `fig`)
  form <- .tpl_fig_forms[[t$root %||% t$from %||% tp$name]] %||% list()
  input <- form$input %||% "df"
  call <- if (generic) sprintf("%s(%s)", .tpl_call_name(t, TRUE),
                               paste(c(input, q(called), arg_code), collapse = ", "))
          else sprintf("%s(%s)", .tpl_call_name(t, FALSE), paste(c(input, arg_code), collapse = ", "))
  # the report's own items after it
  pre <- character()
  terms <- character()
  libs <- c("dplyr", "ggplot2", d$libs)
  needs <- character()
  n_lab <- 0L
  own <- list()
  for (pa in plot_add) {
    res <- .fig_call_code(pa)
    terms <- c(terms, res$call)
    needs <- c(needs, res$pkgs)
  }
  for (i in seq_along(tp$rest)) {
    x <- .fig_item_as_piece(tp$rest[[i]])
    # the report's own R code: statements on `p`, after the other items
    if (identical(x$piece$layer, "layer_code")) {
      own <- c(own, list(list(stmt = .fig_own_layer_code(x$piece$code), label = x$piece$label)))
      next
    }
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
  variant <- if (!is.null(t$from)) {
    sprintf(" (a variant of %s%s)", t$from,
            if (!identical(t$package, "tflspec")) paste(",", .tpl_pkg_version(t$package)) else "")
  } else ""
  chain <- c(
    .tpl_fig_chain(if (is.null(form$assemble)) name else "fig", call, terms, own),
    if (!is.null(form$assemble)) {
      # (the call's arguments the assembly needs too: the same values)
      aa <- intersect(form$assemble_args, keep)
      sprintf("%s <- %s(%s)", name, form$assemble,
              paste(c("fig", arg_code[match(aa, keep)]), collapse = ", "))
    })
  # (what a code step does not keep, read after it: stopped)
  .fig_kept_check(d, c(pre, chain))
  code <- c(
    sprintf("# template %s%s%s", tp$name, variant,
            if (length(tp$study)) paste0("; study defaults: ", paste(tp$study, collapse = ", ")) else ""),
    # (no data part: a template drawn from the ARD)
    if (length(d$code) || length(pre)) section("data"),
    d$code,
    if (length(pre)) c("# the numbers it prints, from the ARD", pre, ""),
    section("plot"),
    chain)
  code <- .code_lines(code)
  code <- .fig_rename_df(code, tfl_fig_data_name(list(data = steps)))
  code <- .fig_tidy_names(code)
  # no head (.fig_script_head() is a script of its own's): the template's
  # call is a part of the report's program, run after the study's setup,
  # whose programs/study_helpers.R and figure setup define the helpers it
  # calls (tte_time(), fmt_num(), order_levels() ...) and the days_per_*
  # constants; it calls tflspec::tfl_tpl() and order_levels() too, which no
  # head could carry
  .fig_code_out(code, d$reads, unique(libs),
                setdiff(unique(needs), c("ggplot2", "ggsurvfit", "patchwork")),
                ard = length(pre) > 0L)
}

# The call and the items after it into `name`: one `+` chain; with the
# report's own code (layer_code), the chain into `p`, the statements, then
# `name <- p`
.tpl_fig_chain <- function(name, call, terms, own = list()) {
  if (!length(own)) return(.fig_plus_chain(name, c(call, terms)))
  c(.fig_plus_chain("p", c(call, terms)),
    unlist(lapply(own, function(o) c(paste("#", o$label %||% "the report's own code"), o$stmt))),
    sprintf("%s <- p", name))
}

# ---- a series template's figure: the content function's call ----------------------

# A figure report resolved from a series template (tfl_template_resolve():
# its `template` attribute is the template's name, `<series>/<template>`)
# whose template names its content in `figure:` (km_curves, forest_hr): the
# program calls that function (.fig_content_table(), tfl_figure_fn()) with
# the report's resolved arguments -- the model object when the function
# takes one (km_fit, made from the data the program's data part made), the
# ARD (`ard`) and the appearance `params` -- into `content`.  NULL when the
# report is not one.
#
# The report's own cards (its `content:`, after the template's: canon 1.4,
# the second way to change a report) follow the call, as a table's follow
# its template's plan: a card of the content function itself (`km_curves:
# {params: {legend: inside}}`) is folded onto the call argument by argument,
# the later card winning (.merge_by_arg(), the merge of a table's appended
# plan card; a map argument -- `params` -- by name; `!unset` takes one
# away); a tfl_image card says the image's settings (merged the same way);
# any other card is a piece the call has not, `+`-ed after it in order
# (.fig_item_term(), the writer of a figure's own content items).
.content_fig_series_code <- function(output, name = "content") {
  if (is.null(output$figure)) return(NULL)
  tp <- attr(output, "template")
  key <- tryCatch(.fig_content_key(output), error = function(e) .ard_stop(conditionMessage(e)))
  fig <- key$name
  e <- .fig_content_table()[[fig]]
  cards <- .fig_series_cards(output, e)
  args <- key$args
  # the model: its call with the template's arguments (a required one the
  # report did not give is the resolve's stop, not here)
  model <- NULL
  if (!is.null(e$model)) {
    call <- e$model$call
    for (k in names(args)) {
      v <- args[[k]]
      if (is.character(v) && length(v) == 1L) call <- gsub(paste0("{", k, "}"), v, call, fixed = TRUE)
    }
    left <- regmatches(call, gregexpr("[{][A-Za-z_.]+[}]", call))[[1L]]
    if (length(left)) {
      .ard_stop(sprintf("%s: figure %s needs %s in its `args`.", output$id %||% "report", fig,
                        paste(gsub("[{}]", "", unique(left)), collapse = ", ")))
    }
    model <- .fig_series_model_lines(e$model$arg, call)
  }
  given <- Filter(Negate(is.null), args[intersect(e$params, names(args))])
  # the call's arguments: the model, the ARD, the params; the report's cards
  # of the function over them, in order
  fa <- c(if (!is.null(e$model)) stats::setNames(list(tfl_r(e$model$arg)), e$model$arg),
          list(ard = tfl_r("ard")), if (length(given)) list(params = given))
  for (card in cards$call) fa <- .merge_by_arg(fa, card$args)
  fa <- .drop_unset(fa)
  bad <- setdiff(names(fa$params), e$params)
  if (length(bad)) {
    .ard_stop(sprintf("%s: %s() has no params %s; it takes %s.", output$id %||% "report", e$fn,
                      paste(bad, collapse = ", "), paste(e$params, collapse = ", ")))
  }
  if (!length(fa$params)) fa$params <- NULL
  # one argument a line (the params' list one a line inside it); a comma
  # after each argument but the last
  call_args <- lapply(names(fa), function(k) {
    v <- fa[[k]]
    if (identical(k, "params") && .is_map(v)) {
      return(c("  params = list(",
               paste0("    ", names(v), " = ", vapply(v, .fig_arg_code, ""),
                      c(rep(",", length(v) - 1L), "")),
               "  )"))
    }
    paste0("  ", k, " = ", .fig_arg_code(v))
  })
  for (i in seq_len(length(call_args) - 1L)) {
    n <- length(call_args[[i]])
    call_args[[i]][n] <- paste0(call_args[[i]][n], ",")
  }
  call_args <- unlist(call_args)
  fn <- paste0("tflspec::", e$fn)
  # the cards the call has not: `+`-ed after it, each on its line
  pre <- character()
  terms <- character()
  libs <- character()
  needs <- character()
  k_ard <- 0L
  for (it in cards$add) {
    x <- .fig_item_term(it, k_ard)
    if (isTRUE(x$ard)) k_ard <- k_ard + 1L
    terms <- c(terms, x$term)
    pre <- c(pre, x$pre)
    libs <- c(libs, x$libs)
    needs <- c(needs, x$needs)
  }
  close <- if (length(terms)) {
    # (the cards are ggplot2's calls as the program attaches it)
    libs <- c("ggplot2", libs)
    t <- gsub("\n", "\n  ", terms, fixed = TRUE)
    c(") +", paste0("  ", t, c(rep(" +", length(t) - 1L), "")))
  } else ")"
  code <- c(if (is.character(tp)) sprintf("# template %s: the figure content %s()", tp, e$fn)
            else sprintf("# the figure content %s()", e$fn),
            if (length(pre)) c(pre, ""),
            model,
            sprintf("%s <- %s(", name, fn),
            call_args,
            close,
            .fig_image_save(output, cards$image, name))
  .fig_code_out(code, character(), unique(libs),
                setdiff(unique(needs), c("ggplot2", "ggsurvfit", "patchwork")), ard = TRUE)
}

# A series figure report's own cards (`content:`), as .content_fig_series_code()
# writes them: `call`, the cards of the template's content function (the
# item the template has); `image`, the tfl_image cards' settings merged in
# order; `add`, every other card (a piece the template has not, added
# after the call).  A card of another figure content function, a named
# plot or a content of R code (`content: !r`) is not a card after the call:
# it stops, saying so.
.fig_series_cards <- function(output, e) {
  content <- output$content
  who <- output$id %||% "report"
  if (inherits(content, "tfl_r")) {
    .ard_stop(sprintf(paste0("%s: a report with `figure:` changes its figure with cards after the ",
                             "call (`content:` items), not `content: !r`."), who))
  }
  out <- list(call = list(), image = list(), add = list())
  others <- setdiff(vapply(.fig_content_table(), `[[`, "", "fn"), e$fn)
  takes <- names(formals(get(e$fn, envir = asNamespace("tflspec"), mode = "function")))
  for (it in content %||% list()) {
    if (inherits(it, "tfl_plot")) {
      .ard_stop(sprintf(paste0("%s: a report with `figure:` adds cards after %s(), not a named plot ",
                               "(`%s`)."), who, e$fn, it$fn))
    }
    fn <- .schema_fn_name(it$fn %||% "")
    if (identical(fn, e$fn)) {
      bad <- setdiff(names(it$args), takes)
      if (length(bad)) {
        .ard_stop(sprintf("%s: the %s card's %s: %s() takes %s.", who, e$fn, paste(bad, collapse = ", "),
                          e$fn, paste(takes, collapse = ", ")))
      }
      out$call <- c(out$call, list(it))
    } else if (fn %in% others) {
      .ard_stop(sprintf("%s: the figure is %s()'s (its template's `figure:`); a %s card does not change it.",
                        who, e$fn, fn))
    } else if (identical(fn, "tfl_image")) {
      out$image <- .merge_by_arg(out$image, it$args)
    } else {
      out$add <- c(out$add, list(it))
    }
  }
  out$image <- .drop_unset(out$image)
  # (a scale, coord, facet or guides card given twice: one, by argument)
  out$add <- .fig_fold_once(out$add)
  out
}

# A series figure report's image (its tfl_image cards merged), as the report
# part sizes it on the page; NULL when it has none (the figure's own size)
.fig_series_image <- function(output) {
  has <- is.list(output$content) && !inherits(output$content, "tfl_r") &&
    any(vapply(output$content, function(it) identical(.schema_fn_name(it$fn %||% ""), "tfl_image"), NA))
  if (!has) return(NULL)
  e <- .fig_content_table()[[.fig_content_key(output)$name]]
  .fig_series_cards(output, e)$image
}

# `km_fit <- ggsurvfit::survfit2(a, b)` as `km_fit <- f(` one argument a
# line `)`: the call's top-level arguments split
.fig_series_model_lines <- function(name, call) {
  ex <- str2lang(call)
  fn <- paste(deparse(ex[[1L]]), collapse = "")
  a <- as.list(ex)[-1L]
  txt <- vapply(seq_along(a), function(i) {
    v <- paste(deparse(a[[i]], width.cutoff = 500L), collapse = " ")
    if (nzchar(names(a)[i] %||% "")) paste(names(a)[i], "=", v) else v
  }, "")
  c(sprintf("%s <- %s(", name, fn),
    paste0("  ", txt, c(rep(",", length(txt) - 1L), "")),
    ")")
}
