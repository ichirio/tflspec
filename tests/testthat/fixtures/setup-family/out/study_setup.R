# ============================================================================
#  Program    : programs/study_setup.R
#  What every program of the study starts with (ard_setup.R, report_setup.R
#  and fig_setup.R source it).  tflplanner writes it on every save: do not
#  edit it.  The company's code is in company_setup.R and yours in
#  study_user.R: this file runs them, the company's first and yours last.
#  Generated  : <masked>
# ============================================================================

# This file runs once per program run: a program sets the option
# tflplanner.run when it starts (and clears it at its end), and this
# file sourced again in that run (a figure report: report_setup.R, then
# fig_setup.R) is skipped, so the company's code and yours run once and
# what yours sets stays.  Sourced without a program's run (by hand, the
# Plot Designer), it runs each time.  To run it again by hand:
# rm(.tfl_free_ran)
.tfl_free <- c(company = "programs/company_setup.R", user = "programs/study_user.R")
.tfl_free_key <- paste(getwd(), getOption("tflplanner.run", NA))
.tfl_free_run <- is.null(getOption("tflplanner.run")) ||
  !identical(get0(".tfl_free_ran", envir = environment(), inherits = FALSE),
             .tfl_free_key)
if (.tfl_free_run) {

# ---- the company's code (company_setup.R)
if (file.exists(.tfl_free[["company"]])) source(.tfl_free[["company"]], local = environment())

# ---- the study: its folders, relative to the study folder (the programs
#      run from it), and what it is (study.yml)
# the data root: an official run copies the programs into its batch folder
# and reads the data where run_paths.R says (the study's data folders);
# anywhere else it is the study folder.  company_setup.R runs before these
# lines: company code cannot use path_adam, path_sdtm or path_other.
.tfl_data_root <- if (file.exists("run_paths.R")) { source("run_paths.R"); .tfl_data_root } else "."
path_adam         <- file.path(.tfl_data_root, "data/adam")
path_sdtm         <- file.path(.tfl_data_root, "data/sdtm")
path_other        <- file.path(.tfl_data_root, "data/other")
path_spec         <- "spec"
path_programs_ard <- "programs/ard"
path_programs_tfl <- "programs/tfl"
path_ard          <- "output/ard"
path_tfl          <- "output/tfl"
path_ard_import   <- "input/ard"
path_toc_import   <- "input/toc"
path_runs         <- "runs"
path_logs_preview <- "logs/preview"
path_docs         <- "docs"

study_id          <- "XMP-204"
study_title       <- "A fictional study of Examplumab in moderate plaque"
study_short_title <- "XMP-204 plaque study"
study_compound    <- "Examplumab"
study_phase       <- "3"
study_description <- "A fictional two-arm study (Examplumab vs Placebo) for the golden test of the setup programs; no real data."

# ADaM levels: define_xml

# the study's rounding (every ARD and report program)
options(rtfreporter.rounding = "r", cards.round_type = "round-to-even")

study <- list(adam_org = new.env(), adam = new.env())

# the study's code lists, by name (spec/defaults.yml codelists:)
study$codelists <- list(
  SEX = c(F = "Female", M = "Male"),
  ARM = c(EXM = "Examplumab 150 mg", PBO = "Placebo")
)

# the study's ADaM metadata: each variable's code list, and the code lists
# (spec/defaults.yml adam_meta:)
study$adam_meta <- list(
  variables = tibble::tribble(
    ~dataset, ~variable, ~codelist,
    "ADSL",   "SEX",     "CL.SEX",
    "ADAE",   "AESEV",   "CL.AESEV"
  ),
  codelists = tibble::tribble(
    ~codelist,  ~name,      ~value,     ~decode,  ~order,
    "CL.SEX",   "Sex",      "F",        "Female", 1L,
    "CL.SEX",   "Sex",      "M",        "Male",   2L,
    "CL.AESEV", "Severity", "MILD",     NA,       1L,
    "CL.AESEV", "Severity", "MODERATE", NA,       2L,
    "CL.AESEV", "Severity", "SEVERE",   NA,       3L
  )
)

# the study's input data, read when first used: study$adam_org (the ADaM as read,
# with the catalog's derive), study$adam (blanks as NA, then the levels of the code
# lists adam_meta maps to its variables; adam_levels: define_xml)
delayedAssign(
  "adsl",
  readRDS(file.path(path_adam, "adsl.rds")),
  assign.env = study$adam_org
)
delayedAssign(
  "adae",
  haven::read_xpt(file.path(path_adam, "adae.xpt")) |> dplyr::mutate(AEDUR = AENDY - ASTDY + 1),
  assign.env = study$adam_org
)
delayedAssign(
  "adlb",
  haven::read_sas(file.path(path_adam, "adlb.sas7bdat")),
  assign.env = study$adam_org
)
delayedAssign(
  "adsl",
  study$adam_org$adsl |>
    blank_as_na() |>
    set_levels(SEX = adam_codes("CL.SEX")),
  assign.env = study$adam
)
delayedAssign(
  "adae",
  study$adam_org$adae |>
    blank_as_na() |>
    set_levels(AESEV = adam_codes("CL.AESEV")),
  assign.env = study$adam
)
delayedAssign("adlb", blank_as_na(study$adam_org$adlb), assign.env = study$adam)

# (the objects the ARD, listing and figure programs read: study$adam's)
delayedAssign("adsl", study$adam$adsl)
delayedAssign("adae", study$adam$adae)
delayedAssign("adlb", study$adam$adlb)

# the functions the programs call (tflplanner writes them)
source("programs/study_helpers.R")

# ---- your code (study_user.R)
if (file.exists(.tfl_free[["user"]])) source(.tfl_free[["user"]], local = environment())
.tfl_free_ran <- .tfl_free_key
}
