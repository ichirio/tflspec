# Setup code: runs first in every ARD and report program of this study.
# What every program needs: the packages, and the options the study shares.
library(cards)
library(dplyr)
library(rtfreporter)

# Warnings are printed as they happen, so the run's log shows where
options(warn = 1)

# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- data ------------------------------------------------------------------
# ---- code lists: each value in its order, as it is in the data ----
# (a value a list does not have stops the program: add it to the list)
cl_trt01a <- c("Control", "Drug X")
cl_sex <- c(F = "Female", M = "Male")
cl_agegr1 <- c("<65" = "<65 years", ">=65" = ">=65 years")

# this figure's ARD (its rows of the study ARD)
ard <- readRDS("output/ard/ard.rds") |>
  filter(output_id == "F-14-2-4")
if (!nrow(ard)) stop("The study ARD has no rows for F-14-2-4: run programs/ard/F-14-2-4.R first.")
# (the definition it was made from: the report records it, #293)
ard_built <- ard_fingerprint("F-14-2-4")

# the data: ADTTE (data catalog)
adtte <- readRDS("data/adam/adtte.rds")
adtte <- blank_as_na(adtte)

adtte_sub <- adtte |>
  order_levels() |>
  filter(PARAMCD == "TTSKIN", SAFFL == "Y") |>
  set_levels(TRT01A = cl_trt01a, SEX = cl_sex, AGEGR1 = cl_agegr1)

est <- ard_stats(
  ard,
  c("HR", "HR_SEX", "HR_AGEGR1"),
  variable = "TRT01A",
  stats = c("n_obs", "estimate", "conf.low", "conf.high"),
  by = c("SEX", "AGEGR1")
)

# the rows to draw: the comparison arm, a heading row per subgroup
est <- local({
  est <- est |>
    # the reference arm has no interval; a subgroup whose model did not
    # converge (no events in an arm) has no point: NE
    filter(!is.na(conf.low)) |>
    filter(TRT01A == "Drug X") |>
    mutate(
      subgroup = case_when(
        !is.na(SEX) ~ "SEX",
        !is.na(AGEGR1) ~ "AGEGR1",
        TRUE ~ "All subjects"
      ),
      # (the ARD holds the codes: the code lists give the labels)
      level = coalesce(unname(cl_sex[as.character(SEX)]),
                   unname(cl_agegr1[as.character(AGEGR1)]), "All subjects"),
      ok = is.finite(estimate) & is.finite(conf.low) & is.finite(conf.high) &
        conf.high < 1000,
      txt = ifelse(ok, sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high),
                   "NE"),
      across(c(estimate, conf.low, conf.high), ~ ifelse(ok, .x, NA))
    )
  # a heading row above each subgroup's levels, the levels indented
  heads <- est |>
    filter(subgroup != "All subjects") |>
    distinct(subgroup) |>
    mutate(level = subgroup, head = TRUE)
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
  labs(x = "Hazard Ratio (95% CI), Drug X vs Control") +
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
