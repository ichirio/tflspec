# Run by the agent after each answer it saves (AGENTS.md): reads every
# answer in ai/answers/, checks it, writes spec/outputs/<id>.yml, builds
# each report's ARD and RTF, and writes ai/status.md and ai/questions.md.
# It uses the R and the packages installed; it reads the ADaM, the agent
# does not.  Run it from anywhere: `Rscript <this folder>/run.R`.
args <- commandArgs(trailingOnly = FALSE)
me <- sub("^--file=", "", args[grepl("^--file=", args)])
here <- if (length(me)) dirname(normalizePath(me[1L])) else "."
tflspec::tfl_ai_run(here)
