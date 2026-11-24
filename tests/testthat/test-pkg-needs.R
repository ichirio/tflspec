# The table of the packages code needs beyond those it names (R/pkg_needs.R)

test_that(".pkgs_in_code(): a package named in code, a model's package, a cardx function's", {
  expect_identical(tflspec:::.pkgs_in_code("fit <- mmrm::mmrm(AVAL ~ ARM, data = d)"), "mmrm")
  expect_identical(tflspec:::.pkgs_in_code("cardx::ard_regression(cox, exponentiate = TRUE)"),
                   "broom.helpers")
  expect_identical(tflspec:::.pkgs_in_code("cardx::ard_survival_survfit(x)"), c("survival", "broom"))
  expect_identical(tflspec:::.pkgs_in_code("cardx::ard_emmeans_contrast(d, package = \"mmrm\")"),
                   c("mmrm", "emmeans"))
  expect_identical(tflspec:::.pkgs_in_code("my_ard_survival_x(d)"), character())
  expect_identical(tflspec:::.pkgs_in_code(character()), character())
})

# Every package a cardx ARD function asks for with check_pkg_installed() is
# either one cardx imports or one the table gives for that function: a new
# cardx function, or a new need of one, fails here (not in a study's run).
test_that("the table covers every package cardx's ARD functions check for at run time", {
  skip_if_not_installed("cardx")
  ns <- asNamespace("cardx")
  exported <- grep("^ard_", getNamespaceExports("cardx"), value = TRUE)
  fns <- ls(ns, pattern = "^ard_")
  # (a survey design's methods: its code names survey:: itself)
  fns <- fns[!grepl("[.]survey[.]design$", fns)]
  d <- utils::packageDescription("cardx")
  imports <- trimws(sub("[(].*$", "", unlist(strsplit(paste(d$Depends, d$Imports, sep = ","), ","))))
  lits <- function(e) {
    if (is.character(e)) return(e)
    if (is.call(e)) return(unlist(lapply(as.list(e)[-1L], lits)))
    character()
  }
  checks <- function(e) {
    if (!is.call(e)) return(character())
    if (identical(e[[1L]], quote(check_pkg_installed))) return(lits(e))
    unlist(lapply(as.list(e), checks))
  }
  gaps <- character()
  for (f in fns) {
    fn <- get(f, ns)
    if (!is.function(fn)) next
    need <- unique(checks(body(fn)))
    if (!length(need)) next
    # the exported function this one is (a method's generic)
    gen <- exported[f == exported | startsWith(f, paste0(exported, "."))]
    if (!length(gen)) next
    have <- tflspec:::.pkgs_in_code(paste0("cardx::", gen[[1L]], "("))
    miss <- setdiff(need, c(have, imports, "cardx"))
    if (length(miss)) gaps <- c(gaps, paste0(f, ": ", paste(miss, collapse = ", ")))
  }
  expect_identical(gaps, character(),
                   info = "add these to .ard_fn_pkgs in R/pkg_needs.R")
})
