# the data section of demog, ae_soc_pt, lab_shift and a listing

    Code
      cat(ds_section(ds_code(d, st, "X-demog")), sep = "\n")
    Output
      # ---- data ----
      # adsl_sub: the subjects. population SAF (Safety Population)
      adsl_sub <- filter(adsl, SAFFL == "Y")
      # adsl_miss: With a Missing level for a variable that has missing values.
      adsl_miss <- mutate(adsl_sub, across(any_of(c("AGEGR1", "SEX", "RACE", "ETHNIC")), function(x) {
        chr <- trimws(as.character(x))
        miss <- is.na(chr) | !nzchar(chr)
        if (!any(miss)) return(x)
        chr[miss] <- "Missing"
        values <- if (is.factor(x)) levels(x) else sort(unique(chr[!miss]))
        factor(chr, levels = unique(c(values, "Missing")))
      }))

---

    Code
      cat(ds_section(ds_code(d, st, "X-ae-soc-pt")), sep = "\n")
    Output
      # ---- data ----
      # adsl_sub: the subjects. population SAF (Safety Population)
      adsl_sub <- filter(adsl, SAFFL == "Y")
      # adae_sub: the records. ADAE of the subjects of adsl_sub, TRT01A from ADSL;
      # where
      adae_sub <- adae |>
        select(-any_of("TRT01A")) |>
        inner_join(
          select(adsl_sub, USUBJID, TRT01A),
          by = "USUBJID",
          relationship = "many-to-one"
        ) |>
        filter(TRTEMFL == "Y")

---

    Code
      cat(ds_section(ds_code(d, st, "X-lab-shift")), sep = "\n")
    Output
      # ---- data ----
      # adsl_sub: the subjects. population SAF (Safety Population)
      adsl_sub <- filter(adsl, SAFFL == "Y")
      # adlb_sub: the records. ADLB of the subjects of adsl_sub, TRT01A from ADSL;
      # after baseline with a baseline and a post-baseline category; one record a
      # subject and parameter, the worst category.
      adlb_sub <- adlb |>
        select(-any_of("TRT01A")) |>
        inner_join(
          select(adsl_sub, USUBJID, TRT01A),
          by = "USUBJID",
          relationship = "many-to-one"
        ) |>
        filter(!ABLFL %in% "Y", !ANRIND %in% c(NA, ""), !BNRIND %in% c(NA, ""), AVISIT == "CYCLE 2 DAY 1" & PARAMCD %in% c("ALT", "AST")) |>
        arrange(match(ANRIND, c("HIGH", "LOW", "NORMAL"))) |>
        distinct(USUBJID, PARAMCD, .keep_all = TRUE)
      # adlb_total: With the Total row: every subject once more under the baseline's
      # Total.
      adlb_total <- bind_rows(adlb_sub, mutate(adlb_sub, BNRIND = "Total"))
      # adlb_den: The subjects with a category after baseline, a parameter each.
      adlb_den <- distinct(adlb_sub, USUBJID, TRT01A, PARAMCD)

---

    Code
      cat(ds_section(ds_code(d, st, "X-listing", "listing")), sep = "\n")
    Output
      # ---- data ----
      # adsl_sub: the subjects. population SAF (Safety Population)
      adsl_sub <- dplyr::filter(adsl, SAFFL == "Y")
      # its records, in its order; dates as they print
      data <- adae |>
        dplyr::inner_join(
          dplyr::select(adsl_sub, USUBJID),
          by = "USUBJID",
          relationship = "many-to-one"
        ) |>
        dplyr::mutate(
          dplyr::across(dplyr::where(~ inherits(.x, c("Date", "POSIXt"))), format)
        )
      plan <- table_plan(data) |>
        # (template listing)
        plan_listing(
          listing_col(vars = "USUBJID", label = "Subject"),
          type = "multiline"
        ) |>
        # (template listing)
        plan_paginate_rows(max_rows = 22)

