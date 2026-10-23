# A template's help: its parameters as a table (tfl_tpl_args()), on the
# screen (tfl_tpl_help()) and as its Rd page (.tpl_rd(), roxygen @eval in
# R/tpl_docs.R).  Read from the registry (R/tpl_registry.R), so a new
# parameter is in all three.

# The chain of a template: its own entry, then each variant's `from`, up
# to the template with the body (a list of raw registry entries)
.tpl_chain <- function(name) {
  out <- list()
  nm <- name
  while (!is.null(nm)) {
    e <- get0(nm, envir = .tpl_registry, inherits = FALSE)
    if (is.null(e)) break
    out[[length(out) + 1L]] <- e
    nm <- e$from
  }
  out
}

# A value in a few characters: NULL is NA; a named vector or list
# "name = value; ..."; long ones cut
.tpl_short <- function(x, width = 60L) {
  if (is.null(x)) return(NA_character_)
  one <- function(v) {
    if (is.null(v)) return("NULL")
    if (is.list(v)) return(paste0("(", .tpl_short(v, width = 1000L), ")"))
    paste(as.character(v), collapse = ", ")
  }
  s <- if (is.list(x) || (!is.null(names(x)) && length(x) > 1L)) {
    v <- vapply(seq_along(x), function(i) one(x[[i]]), "")
    nm <- names(x)
    if (!is.null(nm)) v <- ifelse(nzchar(nm), paste(nm, "=", v), v)
    paste(v, collapse = "; ")
  } else {
    one(x)
  }
  if (nchar(s) > width) s <- paste0(substr(s, 1L, width - 3L), "...")
  s
}

# Where a parameter ends: its study source ("plan_stub.indent"), else the
# verb (and argument) its doc names, else NA
.tpl_maps_to <- function(p) {
  if (!is.null(p$study)) return(p$study)
  d <- p$doc %||% ""
  m <- regmatches(d, regexpr("plan_[a-z_]+\\(([a-z_]+) = ", d))
  if (length(m)) return(sub("\\(([a-z_]+) = $", ".\\1", m))
  m <- regmatches(d, regexpr("plan_[a-z_]+", d))
  if (length(m)) return(m)
  NA_character_
}

# The study standard: a tfl_study_spec, or a study's folder read
.tpl_help_study <- function(study, fn) {
  if (is.null(study) || inherits(study, "tfl_study_spec")) return(study)
  if (is.character(study) && length(study) == 1L && dir.exists(study)) {
    return(tfl_read_study_spec(study))
  }
  .ard_stop(sprintf("%s: `study` is a tfl_study_spec or a study's spec folder.", fn))
}

#' A template's parameters, as a table
#'
#' Each parameter of a registered template ([tfl_templates()]): its tier,
#' its type, the layout's default, the default a variant changes, the
#' study standard's value and the value that takes effect.  The order of
#' precedence is a report's argument, then the study standard, then the
#' variant's default, then the layout's default; this table has no
#' report, so its `effective` value is the one a report that does not
#' give the parameter gets.
#'
#' @param name The template's name (`"demog"`).
#' @param study The study standard: a [tfl_read_study_spec()] object or a
#'   study's spec folder; `NULL` for none (the `study` column is then
#'   `NA`).  Its `defaults.yml` content is where a parameter's
#'   `"<verb>.<argument>"` source is read, `study.yml`'s `settings:
#'   treatment:` the `by` (`"settings.treatment"`).
#' @param population The report's analysis set: the value of the
#'   parameters taken from the report's `population:`
#'   (`"output.population"`); `NULL` for none.
#' @return A data frame, one row a parameter in the template's order:
#'   `param`, `tier` (`"required"`, `"usual"`, `"other"`), `stage`
#'   (`"call"`, an argument of the template's call, or `"data"`, of a step
#'   preparing the analysis data: [tfl_param()]`(stage = )`), `type`,
#'   `required`, `default` (the layout's, a short text), `variant` (the
#'   default a variant changes it to, else `NA`), `study` (the study
#'   standard's value, else `NA`), `effective` (the value that takes
#'   effect), `layer` (where it is from: `"study"`, `"variant:<name>"` or
#'   `"template"`), `maps_to` (its study source, `"plan_stub.indent"`, or
#'   the verb its doc names) and `doc`.
#' @seealso [tfl_tpl_help()] to print it; [tfl_tpl()] to run a template.
#' @examples
#' a <- tfl_tpl_args("demog")
#' a[a$tier != "other", c("param", "effective", "layer")]
#' s <- tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
#' a <- tfl_tpl_args("demog", study = s, population = "SAF")
#' a[a$layer == "study", c("param", "effective", "maps_to")]
#' @export
tfl_tpl_args <- function(name, study = NULL, population = NULL) {
  fn <- "tfl_tpl_args()"
  t <- .tpl_get(name, fn)
  study <- .tpl_help_study(study, fn)
  chain <- .tpl_chain(name)
  # the layout's own parameters (the template with the body)
  root <- chain[[length(chain)]]
  root_p <- lapply(root$params, .tpl_param_typed)
  names(root_p) <- vapply(root_p, `[[`, "", "name")
  base <- if (!is.null(study)) .layer_of("defaults", study)$content else list()
  output <- list(population = population)
  rows <- lapply(.tpl_shown(t$params), function(p) {
    rp <- root_p[[p$name]] %||% p
    # the nearest variant that sets it
    vname <- NA_character_
    for (e in chain) {
      if (!is.null(e$from) && p$name %in% names(e$defaults)) {
        vname <- e$name
        break
      }
    }
    sv <- NULL
    if (!is.null(p$study) && (!is.null(study) || identical(p$study, "output.population"))) {
      sv <- .tpl_study_value(p$study, base, output, study)
    }
    layer <- if (!is.null(sv)) "study" else if (!is.na(vname)) paste0("variant:", vname) else
      "template"
    eff <- if (!is.null(sv)) sv else p$default
    data.frame(param = p$name, tier = p$tier %||% "other", stage = p$stage %||% "call",
               type = p$type %||% NA_character_,
               required = isTRUE(p$required),
               default = .tpl_short(rp$default),
               variant = if (is.na(vname)) NA_character_ else
                 if (is.null(p$default)) "NULL" else .tpl_short(p$default),
               study = .tpl_short(sv), effective = .tpl_short(eff), layer = layer,
               maps_to = .tpl_maps_to(p), doc = p$doc %||% NA_character_,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, unname(rows))
  rownames(out) <- NULL
  out
}

# The first argument of a template's call, by its kind: a table's ARD
# (`ard`); a listing's or a figure's data, named after its dataset (the
# `data` parameter's default, else its example's: `adae`), else `data`
.tpl_first_arg <- function(t) {
  if (identical(t$kind, "table")) return("ard")
  ex <- t$example %||% .tpl_examples[[t$root %||% t$name]]
  d <- t$params$data$default %||% ex$data
  if (is.character(d) && length(d) == 1L && grepl("^[A-Za-z][A-Za-z0-9_]*$", d)) tolower(d) else
    "data"
}

# The first line of a template's help: its name, kind, layout, its root
# when a variant, its package
.tpl_help_head <- function(t) {
  what <- c(t$kind, paste("layout", t$layout))
  if (!is.null(t$from)) what <- c(what, paste("variant of", t$from))
  sprintf("%s (%s; %s)", t$name, paste(what[1:2], collapse = ", "),
          paste(c(what[-(1:2)], t$package %||% "tflspec"), collapse = "; "))
}

#' A template's help
#'
#' Prints a registered template's parameters by tier: each one's value
#' that takes effect, where it is from (the study standard, a variant,
#' the template) and what it is.  A report's argument wins over all of
#' them.
#'
#' @inheritParams tfl_tpl_args
#' @param tier The tiers to print: `"required"`, `"usual"`, `"other"`
#'   (all by default; `c("required", "usual")` folds the others away).
#' @return [tfl_tpl_args()]`(name, study, population)`, invisibly.
#' @seealso [tfl_tpl()]; the template's page, `?tfl_tpl_<name>`
#'   (`?tfl_tpl_demog`).
#' @examples
#' tfl_tpl_help("demog", tier = c("required", "usual"))
#' @export
tfl_tpl_help <- function(name, study = NULL, population = NULL,
                         tier = c("required", "usual", "other")) {
  tier <- match.arg(tier, several.ok = TRUE)
  t <- .tpl_get(name, "tfl_tpl_help()")
  a <- tfl_tpl_args(name, study = study, population = population)
  lines <- .tpl_help_head(t)
  if (!is.null(t$label)) lines <- c(lines, t$label)
  if (!is.null(.tpl_stf_note(t$name))) lines <- c(lines, .tpl_stf_note(t$name))
  lines <- c(lines, sprintf("Call: tfl_tpl(%s, \"%s\", ...)", .tpl_first_arg(t), t$name))
  for (k in tier) {
    r <- a[a$tier == k, , drop = FALSE]
    if (!nrow(r)) next
    val <- ifelse(is.na(r$effective), "-", r$effective)
    val <- paste0(val, " (", r$layer, ")")
    what <- ifelse(is.na(r$doc), "", r$doc)
    # (a data step's argument marked: not one of the call's)
    nm <- ifelse(r$stage == "data", paste(r$param, "[data]"), r$param)
    w1 <- max(nchar(c("param", nm)))
    w2 <- max(nchar(c("effective (layer)", val)))
    lines <- c(lines, "", sprintf("%s (%d):", k, nrow(r)),
               sprintf("  %-*s | %-*s | %s", w1, "param", w2, "effective (layer)", "what"),
               sprintf("  %-*s | %-*s | %s", w1, nm, w2, val, what))
  }
  cat(lines, sep = "\n")
  invisible(a)
}

# ---- the Rd page -----------------------------------------------------------------

# Rd text: markdown's and Rd's specials escaped in a table cell
.tpl_rd_cell <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("|", "\\|", x, fixed = TRUE)
  x <- gsub("{", "\\{", x, fixed = TRUE)
  x <- gsub("}", "\\}", x, fixed = TRUE)
  x <- gsub("`", "'", x, fixed = TRUE)
  x <- gsub("[", "\\[", x, fixed = TRUE)
  x <- gsub("]", "\\]", x, fixed = TRUE)
  gsub("\n", " ", x, fixed = TRUE)
}

# An example call's arguments as R text
.tpl_rd_args <- function(args) {
  if (!length(args)) return("")
  v <- vapply(args, function(x) paste(deparse(x, width.cutoff = 500L), collapse = ""), "")
  paste0(", ", paste(names(args), "=", v, collapse = ", "))
}

# The roxygen tags of a template's Rd page, topic tfl_tpl_<name> (the
# table and listing templates' tpl_<name>() functions keep their own
# pages): roxygen @eval in R/tpl_docs.R
# A template after a table of FDA's Standard Safety Tables and Figures
# Integrated Guide (`stf_*`): what it is, and what it is not
.tpl_stf_note <- function(name) {
  if (!startsWith(name, "stf_")) return(NULL)
  paste("It follows the shape of a table in FDA's Standard Safety Tables and Figures",
        "Integrated Guide (version 2); it is not endorsed or recommended by FDA.")
}

.tpl_rd <- function(name) {
  t <- .tpl_get(name, ".tpl_rd()")
  a <- tfl_tpl_args(name)
  topic <- paste0("tfl_tpl_", name)
  x <- .tpl_first_arg(t)
  ex <- t$example %||% .tpl_examples[[t$root %||% name]] %||% list()
  # (a figure's call: the data step's arguments are the program's)
  fig <- identical(t$kind, "figure")
  if (fig) ex <- ex[!vapply(names(ex), function(k) identical(t$params[[k]]$stage, "data"), NA)]
  desc <- sprintf("The %s template `%s`, layout `%s`", t$kind, name, t$layout)
  if (!is.null(t$from)) desc <- sprintf("%s: a variant of `%s` (its defaults changed)", desc, t$from)
  desc <- paste0(desc, sprintf(". Its program is one call, `tfl_tpl(%s, \"%s\", ...)`; ", x, name),
                 "a parameter a report leaves out takes the study standard's value, ",
                 "else the variant's default, else the layout's.",
                 if (!is.null(.tpl_stf_note(name))) paste0(" ", .tpl_stf_note(name)))
  tbl <- c("| Parameter | Tier | Stage | Default | Maps to | What |",
           "|---|---|---|---|---|---|",
           sprintf("| `%s` | %s | %s | %s | %s | %s |", a$param, a$tier, a$stage,
                   .tpl_rd_cell(ifelse(is.na(a$variant), a$default, a$variant)),
                   .tpl_rd_cell(a$maps_to), .tpl_rd_cell(a$doc)))
  c(sprintf("@title %s: the template %s", t$label %||% name, name),
    sprintf("@name %s", topic),
    paste("@description", desc),
    paste0("@section Parameters:\n", paste(tbl, collapse = "\n")),
    paste0("@section Example:\n```\n", if (fig) "fig" else "plan", " <- tfl_tpl(", x,", \"", name, "\"",
           .tpl_rd_args(ex), ")\n```"),
    sprintf("@examples\ntfl_tpl_help(\"%s\", tier = c(\"required\", \"usual\"))", name),
    "@seealso [tfl_tpl()], [tfl_tpl_help()], [tfl_tpl_args()], [tfl_templates()]",
    "@concept template pages")
}
