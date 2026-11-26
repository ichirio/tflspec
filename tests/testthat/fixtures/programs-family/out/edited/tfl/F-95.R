# ============================================================================
#  Program    : programs/tfl/F-95.R
#  Output     : F-95 (figure) -> output/tfl/F-95.rtf
#  Generated  : <masked>
#  The data and plot parts make `plot`; the report part is written from spec/.
#  Edit the design in the Plot Designer (spec/outputs/F-95.yml), not this code.
# ============================================================================

stopifnot("run it from the study folder" = file.exists("study.yml"))
# this run of the program: the study's setup runs once in it
options(tflplanner.run = basename(tempfile("run")))
# the study's setup, and the header, footer and tokens of every report
source("programs/tfl/report_setup.R")
# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

report_id <- "F-95"

# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# ---- data ------------------------------------------------------------------
# the design reads an ARD (`ard`), but the figure has none
stop("F-95 reads an ARD: choose it in the figure's step 2 (analysis).")

# the data: ADTTE (data catalog)

adtte_sub <- adtte |>
  order_levels() |>
  filter(PARAMCD == "OS")

est <- ard_stats(
  ard,
  c("HR", "HR_SEX", "HR_AGEGR1"),
  variable = "TRT01P",
  stats = c("n_obs", "estimate", "conf.low", "conf.high"),
  by = c("SEX", "AGEGR1")
)

# the rows to draw: the comparison arm, a heading row per subgroup
est <- local({
  est <- est |>
    # the reference arm has no interval; a subgroup whose model did not
    # converge (no events in an arm) has no point: NE
    filter(!is.na(conf.low)) |>
    mutate(
      subgroup = case_when(
        !is.na(SEX) ~ "SEX",
        !is.na(AGEGR1) ~ "AGEGR1",
        TRUE ~ "All subjects"
      ),
      level = paste(coalesce(SEX, AGEGR1, "All subjects"), TRT01P, sep = ": "),
      ok = is.finite(estimate) & is.finite(conf.low) & is.finite(conf.high) &
        conf.high < 1000,
      txt = ifelse(ok, sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high),
                   "NE"),
      across(c(estimate, conf.low, conf.high), ~ ifelse(ok, .x, NA))
    )
  # a heading row above each subgroup's levels, the levels indented
  # (the headings read the subgroups' labels, a variable's name when it has none)
  sg_label <- c(SEX = "Sex", AGEGR1 = "Age group")
  heads <- est |>
    filter(subgroup != "All subjects") |>
    distinct(subgroup) |>
    mutate(level = coalesce(unname(sg_label[subgroup]), subgroup), head = TRUE)
  est <- est |>
    mutate(head = FALSE) |>
    bind_rows(heads) |>
    arrange(factor(subgroup, levels = unique(est$subgroup)), desc(head)) |>
    mutate(
      label = ifelse(head | subgroup == "All subjects", level, paste0("    ", level)),
      n = ifelse(head, "", as.character(n_obs)),
      txt = ifelse(head, "", txt),
      y = rev(row_number())
    )
  est
})

# ---- plot ------------------------------------------------------------------

plot <- ggplot() +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.3) +
  geom_errorbar(
    data = est,
    aes(y = y, xmin = conf.low, xmax = conf.high),
    width = 0.25,
    na.rm = TRUE
  ) +
  geom_point(
    data = est,
    aes(x = estimate, y = y),
    shape = 15,
    size = 2.5,
    na.rm = TRUE
  ) +
  scale_x_log10() +
  labs(x = "Hazard Ratio (95% CI)") +
  theme_tfl(theme = "boxed") +
  theme(legend.position = "none") +
  scale_y_continuous(
    breaks = est$y,
    labels = est$label,
    expand = expansion(add = 0.6)
  ) +
  labs(y = NULL) +
  theme(axis.text.y = element_text(hjust = 0))

p_text <- ggplot(est, aes(y = y)) +
  geom_text(aes(x = 0, label = n), size = 3) +
  geom_text(aes(x = 1, label = txt), size = 3) +
  scale_x_continuous(
    limits = c(-0.4, 1.6),
    breaks = c(0, 1),
    labels = c("N", "Hazard ratio (95% CI)"),
    position = "top"
  ) +
  scale_y_continuous(breaks = est$y, expand = expansion(add = 0.6)) +
  theme_void(base_size = 10) +
  theme(axis.text.x.top = element_text(face = "bold"))

plot <- plot + p_text + plot_layout(widths = c(0.6, 0.4))

# the figure checks: dropped rows, colours against the standard
tfl_check(plot)

# the figure drawn at its size on the page: 7.3 x 4.2 in (its own size), 300 dpi
plot <- fig_sized(plot, width_in = 7.3, height_in = 4.2, dpi = 300)

# ---- report ----------------------------------------------------------------
# the report, as spec/outputs/F-95.yml defines it
doc <- rtf_document(
  program_fallback = file.path(path_programs_tfl, report_id),
  tokens = list(OUTPUT_LABEL = "Figure 95", OUTPUT_TITLE = "", OUTPUT_POPULATION = ""),
  file = "output/tfl/F-95.rtf"
) |>
  rtf_section(header = header_standard, footer = footer_standard) |>
  rtf_figures(plot)
generate_rtfreport(doc, file.path(path_tfl, paste0(report_id, ".rtf")), overwrite = TRUE)
record_report(report_id)

# the end of this run
options(tflplanner.run = NULL)
