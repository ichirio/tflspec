# A table's program without its BIGN lines at the head (the column
# headings' N apart from the table's rows, R/spec_code.R .bign_code()):
# the tests of a program's other lines read it from its first line after
# them.  test-bign-split.R tests the BIGN lines themselves.
no_bign <- function(code) {
  if (!length(code) || !startsWith(code[1L], "# BIGN")) return(code)
  i <- 2L
  while (i <= length(code) && nzchar(code[i])) i <- i + 1L
  code[-seq_len(i)]
}
