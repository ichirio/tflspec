# the figure style of the company standards, the figures' packages, and the figure checks
source("programs/tfl/fig_setup.R")

# ---- data: the figure's data -----------------------------------------------
# the report's data: ADSL, ADVS (data catalog)
adsl <- readRDS("data/adam/adsl.rds")
adsl <- blank_as_na(adsl)
advs <- readRDS("data/adam/advs.rds")
advs <- blank_as_na(advs)

# ---- content: your code ----------------------------------------------------
# your code: leaves `content`
content <- ggplot2::ggplot(advs)

# the figure drawn at its size on the page: 6.5 x 4.5 in (its own size), 300 dpi
content <- fig_sized(content, width_in = 6.5, height_in = 4.5, dpi = 300)
content <- report_content(content)
