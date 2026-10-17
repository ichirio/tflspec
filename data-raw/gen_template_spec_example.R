# The Excel example of template SPECs (DM and AE) and the same as YAML:
# inst/extdata/template-spec/.  Run from the package root:
#   Rscript data-raw/gen_template_spec_example.R
pkgload::load_all(".", quiet = TRUE)
out <- file.path("inst", "extdata", "template-spec")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
templates <- data.frame(
  output_id  = c("T-14-1-1", "T-14-3-1"),
  kind       = "table",
  label      = c("Demographic characteristics", "TEAEs by SOC / PT"),
  population = "SAF",
  template   = c("demog", "ae_soc_pt"),
  by         = "TRT01A",
  where      = c(NA, 'TRTEMFL == "Y"'),
  args       = c(NA, "overall = Any TEAE"),
  stringsAsFactors = FALSE)
template_rows <- data.frame(
  output_id = c(rep("T-14-1-1", 5), "T-14-3-1", "T-14-3-1"),
  param     = c("continuous", rep("categorical", 4), "hierarchy", "hierarchy"),
  value     = c("AGE", "AGEGR1", "SEX", "RACE", "ETHNIC", "AEBODSYS", "AEDECOD"),
  label     = c("Age (years)", "Age group, n (%)", "Sex, n (%)", "Race, n (%)",
                "Ethnicity, n (%)", NA, NA),
  stringsAsFactors = FALSE)
writexl::write_xlsx(list(templates = templates, template_rows = template_rows),
                    file.path(out, "templates.xlsx"))
r <- tfl_read_template_sheets(file.path(out, "templates.xlsx"))
for (id in names(r)) tfl_write_template_yaml(r[[id]], file.path(out, paste0(id, ".yml")))
print(list.files(out))
