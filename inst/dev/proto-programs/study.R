# A runnable study folder from a programs-family fixture (fictional data):
# the fixture's study.yml and spec/, the sample's ADaM files, the setup
# programs, and the programs written by today's makers ("split": the ARD
# programs and the report programs) or by the prototype's ("onepass": a
# full program and an ARD program per report).

pp_fx <- function(...) file.path(pp_root, "tests", "testthat", "fixtures", "programs-family", ...)

pp_inputs <- function(study) {
  if (identical(study, "extra")) return(pp_extra(pp_inputs("sample")))
  x <- readRDS(pp_fx("in", paste0(study, ".rds")))
  if (!is.null(x$study) && isTRUE(x$study_dir)) x$study$dir <- pp_fx("study", study)
  if (isTRUE(x$ard$dir)) x$ard$dir <- pp_fx("study", study)
  x
}

pp_write <- function(code, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(unlist(strsplit(paste(code, collapse = "\n"), "\n", fixed = TRUE))),
             file, useBytes = TRUE)
}

# the study folder with its setup programs and data; no report or ARD program
pp_study_base <- function(study, dir, adam) {
  x <- pp_inputs(study)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  src <- pp_fx("study", if (identical(study, "extra")) "sample" else study)
  file.copy(list.files(src, full.names = TRUE, all.files = FALSE), dir, recursive = TRUE)
  # (a study of the sheets, no YAML spec: SAMPLE-01's study.yml and spec/
  # for its setup programs, the same data)
  setup_study <- x$study
  if (!file.exists(file.path(dir, "study.yml"))) {
    smp <- pp_fx("study", "sample")
    file.copy(file.path(smp, c("study.yml", "spec")), dir, recursive = TRUE)
    setup_study <- pp_inputs("sample")$study
  }
  for (o in c("data/adam", "output/ard", "output/tfl")) dir.create(file.path(dir, o), recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(adam, full.names = TRUE), file.path(dir, "data", "adam"))
  # (the extra study's lab data: tflspec's simulated ADLB)
  if (identical(study, "extra")) {
    saveRDS(as.data.frame(tflspec::adlb), file.path(dir, "data", "adam", "adlb.rds"))
    dy <- file.path(dir, "spec", "data.yml")
    txt <- readLines(dy)
    at <- max(grep("^    path: data/adam/", txt))
    writeLines(append(txt, c("  - dataset: ADLB", "    level: ADaM", "    path: data/adam/adlb.rds"),
                      after = at), dy)
    setup_study <- tflspec::tfl_read_study_spec(dir)
  }
  # the ARDs a study takes in (input/ard/): fictional, made from the ADSL
  for (f in pp_imported(x)) {
    dir.create(file.path(dir, "input", "ard"), recursive = TRUE, showWarnings = FALSE)
    adsl <- readRDS(file.path(adam, "adsl.rds"))
    a <- cards::bind_ard(cards::ard_summary(adsl, variables = "AGE"), cards::ard_total_n(adsl))
    saveRDS(a, file.path(dir, "input", "ard", f))
  }
  meta <- yaml::read_yaml(file.path(dir, "study.yml"))[c("study_id", "title", "compound",
                                                         "phase", "description")]
  for (f in c("study_setup", "company_setup", "study_user", "study_helpers",
              "report_setup", "fig_setup")) {
    p <- tflspec:::.setup_paths(x$layout)[[f]]
    pp_write(tflspec::tfl_setup_program(f, meta = meta, study = setup_study,
                                        attached = x$attached, study_code = x$study_code,
                                        fig_libs = c("ggsurvfit", "survival"),
                                        layout = x$layout, date = x$date),
             file.path(dir, p))
  }
  x
}

# today's programs: programs/ard/<id>.R and programs/tfl/<program>.R
pp_write_split <- function(x, dir) {
  ard_ids <- unique(x$ard$spec$analyses$output_id)
  std <- x$standards[intersect(names(x$standards), c("ard_statistics", "ard_methods"))]
  # (a study with no ARD definition: no ARD program)
  if (!inherits(x$ard$spec, "tfl_ard_spec")) ard_ids <- character()
  if (length(ard_ids)) pp_write(tflspec::tfl_ard_program(x$ard$spec, NULL, standards = std, attached = x$attached,
                                    layout = x$layout, date = x$date),
           file.path(dir, x$layout[["programs_ard"]], "ard_setup.R"))
  for (id in ard_ids) {
    pp_write(tflspec::tfl_ard_program(x$ard$spec, id, standards = std, dir = dir,
                                      codelists = x$ard$codelists, attached = x$attached,
                                      study_code = x$study_code, layout = x$layout,
                                      date = x$date),
             file.path(dir, x$layout[["programs_ard"]], tflspec:::.ard_program_name(id)))
  }
  for (id in names(x$reports)) {
    r <- x$reports[[id]]
    pp_write(tflspec::tfl_report_program(r, study = x$study, shared = x$shared,
                                         standards = x$standards, attached = x$attached,
                                         study_code = x$study_code, layout = x$layout,
                                         date = x$date),
             file.path(dir, x$layout[["programs_tfl"]], r$program))
  }
  list(ard = file.path(x$layout[["programs_ard"]], tflspec:::.ard_program_name(ard_ids)),
       tfl = file.path(x$layout[["programs_tfl"]],
                       vapply(x$reports, function(r) r$program, "")))
}

# Rscript <file> from the study folder: status, seconds, peak memory (KB,
# /usr/bin/time's maximum resident set size when there is one)
pp_rscript <- function(dir, file) {
  old <- setwd(dir)
  on.exit(setwd(old))
  tf <- tempfile()
  timer <- if (file.exists("/usr/bin/time")) c("-f", "%M", "-o", tf) else NULL
  t0 <- Sys.time()
  out <- suppressWarnings(if (!is.null(timer)) {
    system2("/usr/bin/time", c(timer, file.path(R.home("bin"), "Rscript"), file),
            stdout = TRUE, stderr = TRUE)
  } else system2(file.path(R.home("bin"), "Rscript"), file, stdout = TRUE, stderr = TRUE))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  st <- attr(out, "status")
  kb <- if (file.exists(tf)) suppressWarnings(as.numeric(utils::tail(readLines(tf), 1L))) else NA
  list(status = if (is.null(st)) 0L else st, seconds = secs, kb = kb, out = out)
}

# the prototype's programs: a full program per report (programs/tfl), and
# its ARD program (programs/ard) when `ard` is TRUE (run choices "ard" and
# "both"); the helpers next to study_helpers.R.  The programs, and what
# each report reads (needs).
pp_write_onepass <- function(x, dir, ard = TRUE) {
  std <- x$standards
  file.copy(system.file("helpers", "onepass_helpers.R", package = "tflspec"),
            file.path(dir, "programs", "onepass_helpers.R"), overwrite = TRUE)
  ard_std <- std[intersect(names(std), c("ard_statistics", "ard_methods"))]
  if (!inherits(x$ard$spec, "tfl_ard_spec")) x$ard$spec <- NULL
  if (!is.null(x$ard$spec)) pp_write(tflspec::tfl_ard_program(x$ard$spec, NULL, standards = ard_std, attached = x$attached,
                                    layout = x$layout, date = x$date),
           file.path(dir, x$layout[["programs_ard"]], "ard_setup.R"))
  rows <- list()
  needs <- list()
  for (id in names(x$reports)) {
    r <- x$reports[[id]]
    args <- list(report = r, spec = x$ard$spec, codelists = x$ard$codelists, dir = dir,
                 study = x$study, shared = x$shared, standards = std, attached = x$attached,
                 study_code = x$study_code, layout = x$layout, date = x$date)
    full <- file.path(x$layout[["programs_tfl"]], r$program)
    pp_write(do.call(tflspec::tfl_onepass_program, c(args, kind = "full")), file.path(dir, full))
    ap <- NA_character_
    if (ard) {
      code <- do.call(tflspec::tfl_onepass_program, c(args, kind = "ard"))
      if (!is.null(code)) {
        ap <- file.path(x$layout[["programs_ard"]], tflspec:::.ard_program_name(id))
        pp_write(code, file.path(dir, ap))
      }
    }
    rows[[id]] <- data.frame(output_id = id, full = full, ard = ap, stringsAsFactors = FALSE)
    src <- r$ard_source
    if (identical(src$kind, "table")) needs[[id]] <- src$id
  }
  list(programs = do.call(rbind, unname(rows)), needs = needs)
}

# the files of input/ard/ a study's reports take in
pp_imported <- function(x) {
  unique(unlist(lapply(x$reports, function(r) c(r$ard_import,
    if (identical(r$ard_source$kind, "import")) r$ard_source$file))))
}

# SAMPLE-01 and two reports the fixtures have not got (fictional data):
#   T-LB-SHIFT  a lab shift (ALT, baseline -> worst post-baseline) whose
#               percentages are over the subjects with that record, not
#               over the BIGN (another denominator), its content its own R;
#   F-KM-FIT    a KM figure whose one survfit gives both its `ard` (its data
#               step, its own R) and its plot (its content step).
pp_extra <- function(x) {
  sp <- x$ard$spec
  sp$datasets <- rbind(sp$datasets, transform(sp$datasets[1L, ], dataset = "ADLB",
                                              path = "data/adam/adlb.rds"))
  ad <- sp$analysis_data
  a1 <- ad[ad$output_id == "T-14-3-1", ]
  a1$output_id <- "T-LB-SHIFT"
  a1$data_id[2L] <- "adlb_alt"
  a1$from[2L] <- "ADLB"
  a1$where[2L] <- "PARAMCD == \"ALT\" & AVISIT == \"WORST POST-BASELINE\""
  sp$analysis_data <- rbind(ad, a1)
  an <- sp$analyses
  b <- an[an$output_id == "T-14-3-1", ]
  b$output_id <- "T-LB-SHIFT"
  b$analysis_id[2L] <- "SHIFT"
  b$label[2L] <- "ALT: baseline by worst post-baseline"
  b$method[2L] <- "categorical"
  b$data[2L] <- "adlb_alt"
  b$by[2L] <- "TRT01A | BNRIND"
  b$variables[2L] <- "ANRIND"
  b$denominator[2L] <- "adlb_alt"
  b$args[2L] <- NA
  sp$analyses <- rbind(an, b)
  x$ard$spec <- sp
  x$shared$ard <- sp
  t <- x$reports[["T-14-1-1"]]
  t$id <- "T-LB-SHIFT"
  t$program <- "T-LB-SHIFT.R"
  t$file <- "output/tfl/T-LB-SHIFT.rtf"
  t$description <- "ALT shift from baseline to the worst post-baseline value"
  t$output$id <- t$spec_output$id <- "T-LB-SHIFT"
  t$output$label <- t$spec_output$label <- t$description
  t$steps <- list(data = NA_character_, content = paste(
    "# n (%) over the subjects with a worst post-baseline ALT (the SHIFT's denominator)",
    "content <- tflspec::tfl_bign_drop(ard) |>",
    "  normalize_ard() |>",
    "  table_plan(cols = \"TRT01A\", rows = c(group = \"BNRIND\")) |>",
    "  plan_cells(default = list(\"{n:.0f} ({p:.1f%})\")) |>",
    "  as_rtftables()",
    sep = "\n"))
  f <- x$reports[["F-14-2-1"]]
  f$id <- "F-KM-FIT"
  f$program <- "F-KM-FIT.R"
  f$file <- "output/tfl/F-KM-FIT.rtf"
  f$description <- "KM of the time to first skin disorder: the fit gives the ARD and the plot"
  f$output$id <- f$spec_output$id <- "F-KM-FIT"
  f$output$label <- f$spec_output$label <- f$description
  f$datasets <- c("ADSL", "ADTTE")
  f$steps <- list(data = paste(
    "# one fit: the ARD (the KM estimates at 30, 60 and 90 days) and the plot",
    "adtte_saf <- adtte |>",
    "  dplyr::filter(PARAMCD == \"TTSKIN\") |>",
    "  dplyr::inner_join(dplyr::select(dplyr::filter(adsl, SAFFL == \"Y\"), USUBJID), by = \"USUBJID\")",
    "fit <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRT01A, data = adtte_saf)",
    "ard <- cardx::ard_survival_survfit(fit, times = c(30, 60, 90))",
    "ard$output_id <- report_id",
    "df <- adtte_saf",
    sep = "\n"), content = paste(
    "# the plot of the same fit",
    "content <- ggsurvfit::ggsurvfit(fit) + theme_tfl()",
    sep = "\n"))
  x$reports[["T-LB-SHIFT"]] <- t
  x$reports[["F-KM-FIT"]] <- f
  x
}
