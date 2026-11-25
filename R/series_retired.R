# The purpose templates that left the tflspec_v1 series (canon 2026-11-24:
# layout templates replace them; they are used in tests only, no alias
# before CRAN), and the basic template a report uses instead.  A study that
# names one and has no copy of it is told so, with the replacement, where
# it would otherwise be told to copy a template that is not there.  A
# study with its copy keeps working (it reads only its copies).
.series_retired <- c(
  "tflspec_v1/demog" = "tflspec_v1/tflspec_summary",
  "tflspec_v1/ae_soc_pt" = "tflspec_v1/tflspec_hierarchy",
  "tflspec_v1/lab_tox_shift" = "tflspec_v1/tflspec_shift",
  "tflspec_v1/listing" = "tflspec_v1/tflspec_listing",
  # (no basic response template yet: tflspec_response is planned)
  "tflspec_v1/orr_subgroup" = NA_character_
)

# The sentence for a retired template ("<series>/<template>"), no final
# full stop; "" for any other
.series_retired_note <- function(template) {
  if (!is.character(template) || length(template) != 1L || is.na(template) ||
      !template %in% names(.series_retired)) {
    return("")
  }
  by <- .series_retired[[template]]
  sprintf("%s was retired from the series (2026-11-24); %s", template,
          if (is.na(by)) "it has no replacement yet; use tflspec_v1/tflspec_summary" else
            paste("use", by, "instead"))
}
