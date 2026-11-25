# this report's rows of the study ARD (made by programs/ard/T-3.R)
ard <- readRDS("output/ard/ard.rds") |>
  dplyr::filter(output_id == report_id) |>
  dplyr::select(-c(output_id, analysis_id, population_id))
if (!nrow(ard)) {
  stop("The study ARD has no rows for ", report_id, ": run programs/ard/T-3.R first.")
}

# normalize and rework
data <- ard |> normalize_ard()
data <- data[1:3, ]
