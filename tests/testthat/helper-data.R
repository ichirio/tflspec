# The package's own example datasets, ahead of the search path: a test
# that attaches ggsurvfit (which has an `adtte` of its own, without TRT01A)
# must not change what a later file's `adtte` is (R CMD check runs every
# file in one session)
for (.d in c("adsl", "adae", "adex", "adlb", "adrs", "adtr", "adtte", "advs")) {
  assign(.d, getExportedValue("tflspec", .d))
}
rm(.d)
