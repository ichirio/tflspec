# The packages code needs beyond the ones it names with library() or pkg::
# (moved here from tflplanner's R/need_pkg.R, so a run started from tflspec
# alone and one started from tflplanner write the same renv.lock).
# tflplanner asks .pkgs_in_code() for its "install this package" messages;
# tfl_runner_code() writes the tables and the function into programs/batch.R,
# where the runner adds them to the packages of renv.lock.

# The packages code calls by name (`survival::coxph`, a model's
# `package = "mmrm"`), and the packages cardx's ARD functions call at run
# time though they are only in cardx's Suggests: what each function's
# check_pkg_installed() asks for (cardx::ard_survival_survfit() needs
# survival and broom; cardx::ard_regression() needs broom.helpers).  The
# names are prefixes (ard_survival_ is every ard_survival_*()); a name
# repeats for each package.  A test reads cardx's own calls and fails when
# a function's package is missing here.  (The survey.design methods are not
# listed: a survey design's code names survey:: itself.)
.code_pkgs <- c("survival", "ggsurvfit", "emmeans", "mmrm")
.ard_fn_pkgs <- c(
  ard_aod_ = "aod", ard_aod_ = "broom.helpers",
  ard_car_ = "car", ard_car_ = "broom.helpers", ard_car_anova = "parameters",
  ard_categorical_ci = "broom",
  ard_effectsize_ = "effectsize", ard_effectsize_ = "parameters",
  ard_emmeans_ = "emmeans",
  ard_regression = "broom.helpers",
  ard_smd_ = "smd",
  ard_stats_ = "broom", ard_stats_aov = "broom.helpers", ard_stats_aov = "parameters",
  ard_stats_anova = "withr",
  ard_survey_ = "survey", ard_survey_ = "broom",
  ard_survival_ = "survival", ard_survival_ = "broom"
)

# The packages a piece of code (or a method name) calls
.pkgs_in_code <- function(code, code_pkgs = .code_pkgs, ard_fn_pkgs = .ard_fn_pkgs) {
  code <- paste(as.character(unlist(code)), collapse = "\n")
  if (!nzchar(code)) return(character())
  hit <- code_pkgs[vapply(code_pkgs, function(p) grepl(
    paste0("\\b", p, "::|package\\s*=\\s*[\"']", p, "[\"']"), code, perl = TRUE), NA)]
  fam <- ard_fn_pkgs[vapply(names(ard_fn_pkgs), function(f)
    grepl(paste0("\\bcardx::", f), code, perl = TRUE), NA)]
  unique(c(hit, unname(fam)))
}

# The lines tfl_runner_code() writes into programs/batch.R: the two tables
# and .pkgs_in_code(), as the runner's .batch_pkgs_in_code()
.runner_pkg_lines <- function() {
  q <- function(x) encodeString(x, quote = "\"")
  f <- .pkgs_in_code
  formals(f)$code_pkgs <- quote(.batch_code_pkgs)
  formals(f)$ard_fn_pkgs <- quote(.batch_ard_fn_pkgs)
  fn <- deparse(f, width.cutoff = 80L)
  fn[[1L]] <- paste(".batch_pkgs_in_code <-", fn[[1L]])
  c("# the packages code needs beyond those it names (tflspec's table): renv.lock lists them",
    paste0(".batch_code_pkgs <- c(", paste(q(.code_pkgs), collapse = ", "), ")"),
    paste0(".batch_ard_fn_pkgs <- c(",
           paste(paste(names(.ard_fn_pkgs), "=", q(.ard_fn_pkgs)), collapse = ", "), ")"),
    fn)
}
