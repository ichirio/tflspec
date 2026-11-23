# A table's program without its BIGN lines (the column headings' N apart
# from the table's rows, R/spec_code.R .bign_code(): they stand just before
# the table, after the data's steps): the tests of a program's other lines
# read it without them.  test-bign-split.R tests the BIGN lines themselves.
no_bign <- function(code) {
  i <- which(startsWith(code, "# BIGN"))
  if (!length(i)) return(code)
  i <- i[[1L]]
  j <- i + 1L
  while (j <= length(code) && nzchar(code[j])) j <- j + 1L
  code[-seq(i, min(j, length(code)))]
}
