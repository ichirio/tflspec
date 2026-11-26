# ============================================================================
#  Program    : programs/tfl/report_setup.R
#  The study's font, header, footer and tokens: every report program sources it.
#  Generated  : <masked>
#
#  Written from spec/defaults.yml: change them there (the study tab),
#  then generate the programs again.
# ============================================================================

# the study's setup: the company's, the study's folders and id, your own
source("programs/study_setup.R")

options(rtfreporter.font = "Arial", rtfreporter.font_size_half_points = 18)
options(
  rtfreporter.tokens = list(
    COMPANY = "Example Pharma",
    STATUS = "FINAL",
    ANALYSIS_TYPE = "Week 16 Analysis"
  )
)
header_house <- rtf_header(
  list(
    c(l = "{COMPANY}", r = "{STATUS}"),
    c(l = "Protocol: {STUDY_ID}", r = "Page {PAGE} of {TOTAL_PAGES}"),
    c(c = "{OUTPUT_LABEL}"),
    c(c = "{OUTPUT_TITLE}")
  ),
  drop_empty_rows = TRUE
)
footer_house <- rtf_footer(list(c(l = "{PROGRAM}       Run on: {DATETIME}")))
header_wide <- rtf_header(
  list(
    c(l = "{COMPANY} -- {ANALYSIS_TYPE}", r = "Page {PAGE} of {TOTAL_PAGES}")
  ),
  drop_empty_rows = TRUE
)

# what was made, with which study setup, and -- a figure printing an
# ARD's numbers -- from which definition of that ARD (tflplanner reads it);
# `empty`: TRUE when the report has no rows (the official run notes it)
record_report <- function(output_id, ard = "", empty = NA) {
  sf <- "output/tfl/report_status.csv"
  setup <- paste(paste(unname(tools::md5sum(Filter(file.exists, c("programs/company_setup.R", "programs/study_setup.R", "programs/study_user.R")))),
                collapse = "+"),
          if (file.exists("programs/tfl/report_setup.R")) unname(tools::md5sum("programs/tfl/report_setup.R")) else "",
          sep = "|")
  row <- data.frame(output_id = output_id,
                    built = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    setup = setup,
                    ard = ard,
                    empty = if (isTRUE(empty)) "TRUE" else if (isFALSE(empty)) "FALSE" else "",
                    stringsAsFactors = FALSE)
  dir.create(dirname(sf), recursive = TRUE, showWarnings = FALSE)
  st <- if (file.exists(sf)) utils::read.csv(sf, colClasses = "character")
  if (!is.null(st) && nrow(st)) {
    for (k in setdiff(names(row), names(st))) st[[k]] <- ""
    row <- rbind(st[st$output_id != output_id, names(row), drop = FALSE], row)
  }
  utils::write.csv(row, sf, row.names = FALSE)
  invisible(output_id)
}

