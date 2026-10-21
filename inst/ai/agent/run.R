# Run by the agent after each answer it saves (AGENTS.md): reads every
# answer in ai/answers/, checks it, writes spec/outputs/<id>.yml, builds
# each report's ARD and RTF -- only those that changed since the last run
# (ai/built.csv) -- and writes ai/status.md and ai/questions.md.
# It uses the R and the packages installed; it reads the ADaM, the agent
# does not.  Run it from anywhere: `Rscript <this folder>/run.R`; with
# request names, only their reports are built: `Rscript run.R ae-1 labs-1`.
args <- commandArgs(trailingOnly = FALSE)
me <- sub("^--file=", "", args[grepl("^--file=", args)])
here <- if (length(me)) dirname(normalizePath(me[1L])) else "."
asked <- commandArgs(trailingOnly = TRUE)
tflspec::tfl_ai_run(here, requests = if (length(asked)) asked)
