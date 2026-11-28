# Gate 2 of the prototype (c/proto-programs): the code that exists only for
# today's split of a report's own ARD from its report (it goes if the
# prototype is adopted), and the code the prototype adds.  Executable
# lines: lines with a code token (not blank, not only a comment), counted
# from the parse data.
#
#   Rscript inst/dev/proto-programs/count.R            (from the repository)

# the lines with code of a file, as a logical vector by line
pp_code_lines <- function(file) {
  pd <- utils::getParseData(parse(file, keep.source = TRUE))
  pd <- pd[pd$terminal & pd$token != "COMMENT", ]
  n <- length(readLines(file, warn = FALSE))
  hit <- logical(n)
  for (i in seq_len(nrow(pd))) hit[pd$line1[i]:pd$line2[i]] <- TRUE
  hit
}

# a top-level function's lines (`name <- function`)
pp_fun_range <- function(file, name) {
  ex <- parse(file, keep.source = TRUE)
  sr <- attr(ex, "srcref")
  for (i in seq_along(ex)) {
    e <- ex[[i]]
    if (is.call(e) && identical(e[[1L]], as.name("<-")) && identical(e[[2L]], as.name(name))) {
      return(c(sr[[i]][1L], sr[[i]][3L]))
    }
  }
  stop("no function ", name, " in ", file)
}

# a block's lines: from the first line matching `from` to the first after
# it matching `to`
pp_block_range <- function(file, from, to) {
  l <- readLines(file, warn = FALSE)
  a <- grep(from, l, fixed = TRUE)[1L]
  b <- a - 1L + grep(to, l[a:length(l)], fixed = TRUE)[1L]
  if (is.na(a) || is.na(b)) stop("no block ", from, " in ", file)
  c(a, b)
}

pp_item <- function(file, what, side, role, fun = NULL, from = NULL, to = NULL) {
  r <- if (!is.null(fun)) pp_fun_range(file, fun) else pp_block_range(file, from, to)
  code <- pp_code_lines(file)
  data.frame(side = side, file = file, what = what, lines = paste0(r[1L], "-", r[2L]),
             exec = sum(code[r[1L]:r[2L]]), role = role, stringsAsFactors = FALSE)
}

H <- "inst/helpers/study_helpers.R"
RP <- "R/report_programs.R"
B <- "inst/batch/runner.R"
items <- rbind(
  # ---- today only: the study ARD as the hand-over of a report's own ARD
  pp_item(H, "save_ard()", "today", "rewrites the study ARD per output (read, replace rows, write) and ard_status.csv", fun = "save_ard"),
  pp_item(H, "read_ard()", "today", "a report's rows of the study ARD, 'run programs/ard/<id>.R first'", fun = "read_ard"),
  pp_item(H, "ard_fingerprint()", "today", "the definition a report's rows were built from (ard_status.csv)", fun = "ard_fingerprint"),
  pp_item("R/setup_programs.R", "code template table_data", "today", "a table's own rows picked from the study ARD (company template)",
          from = "code = c(paste(", to = "sep = \"\\n\"),"),
  pp_item(RP, ".rp_fill_template(): {ARD}, {ARD_PROGRAM}", "today", "the study ARD's path and the ARD program in a template",
          from = "`{ARD}` = ctx$shared$study_ard,", to = "code <- gsub(\"\\\"{ARD}\\\"\""),
  pp_item(RP, ".rp_fig_ard_lines(): own / table", "today", "a figure's rows picked from the study ARD, 'run ... first'",
          from = "    own = , table = {", to = "    },"),
  pp_item(RP, ".rp_ard_one()", "today", "the ARD program: a second writer of the same analysis lines", fun = ".rp_ard_one"),
  pp_item("R/ard_spec.R", "tfl_ard_code(part = \"body\"): save_ard()", "today", "the body's save into the study ARD",
          from = "    sv <- if (save && length(unique(a$output_id)) == 1L) {", to = "    }"),
  pp_item("R/ard_code.R", ".ard_body_lines(): save_ard()", "today", "the pipe into save_ard()",
          from = "} else if (length(save)) {", to = "} else .pipe_code(\"ard\", bound))"),
  pp_item(B, ".batch_ard_error()", "today", "patches ard_status.csv for a failed ARD program", fun = ".batch_ard_error"),
  pp_item(B, ".batch_plan(): ARD before the reports", "today", "a selected report whose own ARD the run does not make (chosen, met, have_ard)",
          from = "  chosen <- if (\"ard\" %in% parts) {", to = "  if (length(lacking)) {"),
  pp_item(B, ".batch_clear(): a reports-only run", "today", "a reports-only re-run on the ARD an earlier run left",
          from = "  if (identical(parts, \"tfl\")) {", to = "  } else {"),
  pp_item(B, ".batch_execute(): reports-only without an ARD", "today", "stops a reports-only re-run with no study ARD",
          from = "  if (rerun && identical(intersect(c(\"ard\", \"tfl\"), parts), \"tfl\") &&", to = "  }"),
  pp_item("R/ard_reads.R", "tfl_ard_reads(): a report reads its own ARD", "today", "the `own` rows: a report needs its own ARD program first",
          from = "    got <- if (id %in% has) .reads_row(id, id, \"own\")", to = "    got <- if (id %in% has) .reads_row(id, id, \"own\")"),
  # ---- the prototype's
  pp_item("R/proto_programs.R", ".op_* (the analysis step, saves, reads)", "prototype", "one emitter's analysis step in both programs, its save, a cross read",
          from = ".op_begin <- ", to = "# The ARD program: the same data"),
  pp_item("R/proto_programs.R", ".op_ard_program()", "prototype", "the ARD program from the same lines", fun = ".op_ard_program"),
  pp_item("R/proto_programs.R", "tfl_onepass_program()", "prototype", "the maker (full / ard)", fun = "tfl_onepass_program"),
  pp_item("R/proto_programs.R", "tfl_onepass_run()", "prototype", "the run choice (full / ard / both), then collect_ard()", fun = "tfl_onepass_run"),
  pp_item("R/proto_programs.R", ".op_run_order()", "prototype", "a report after the reports whose ARD it reads", fun = ".op_run_order"),
  pp_item("R/proto_programs.R", ".op_rscript()", "prototype", "a program in a child R (the runner has its own)", fun = ".op_rscript"),
  pp_item("inst/helpers/onepass_helpers.R", "save_report_ard()", "prototype", "a report's ARD into its own file, what made it", fun = "save_report_ard"),
  pp_item("inst/helpers/onepass_helpers.R", "read_report_ard()", "prototype", "another report's ARD (a cross read)", fun = "read_report_ard"),
  pp_item("inst/helpers/onepass_helpers.R", "report_ard_file()", "prototype", "where a report's ARD file is", fun = "report_ard_file"),
  pp_item("inst/helpers/onepass_helpers.R", "report_ard_definition()", "prototype", "the definition of a read report's ARD", fun = "report_ard_definition"),
  pp_item("inst/helpers/onepass_helpers.R", "collect_ard()", "prototype", "the study ARD: the reports' files bound", fun = "collect_ard"),
  pp_item(RP, "the switch's branches", "prototype", "if (!is.null(ctx$onepass)) ... in today's maker",
          from = "  lines <- .op_outside_analysis(lines)", to = "  lines <- .op_outside_analysis(lines)")
)
# the switch's other branches (one line each)
sw <- grep("ctx\\$onepass|\\.op_table_ard_code\\(ctx\\)", readLines(RP))
items$exec[items$what == "the switch's branches"] <- length(sw) + 1L
items$lines[items$what == "the switch's branches"] <- paste(c(sw, grep(".op_outside_analysis(lines)", readLines(RP), fixed = TRUE)), collapse = ",")
print(items[c("side", "file", "what", "lines", "exec")], right = FALSE, row.names = FALSE)
cat("\nexecutable lines: today only", sum(items$exec[items$side == "today"]),
    "| the prototype adds", sum(items$exec[items$side == "prototype"]), "\n")
saveRDS(items, Sys.getenv("PP_COUNT_OUT", tempfile(fileext = ".rds")))
