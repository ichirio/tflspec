# The file writer of generated programs (G5-3), moved from tflplanner
# (R/study_state.R, R/ard_programs.R) as it was: the checksum in a program's
# banner, a program edited by hand copied to programs/.edited/ before it is
# written again, and a program's state (missing / current / generated /
# edited).  tflplanner and tflspec write the same bytes.

# A generated program carries a checksum of its own body in its banner.  A
# program whose body still matches it has not been touched since tflplanner
# wrote it; one that does not match was edited by hand.  Either is written
# again from the definition (a program is never edited: design 13-00), the
# edited one first copied to programs/.edited/.
.gen_line <- "^#  (Generated|Checksum)  *:"

# (md5sum() reads a file: one file of the session's, written over each
# time -- a new file made and deleted for each program made a save slow)
.hash_file <- new.env()
.body_hash <- function(lines) {
  f <- .hash_file$path
  if (is.null(f)) f <- .hash_file$path <- tempfile("tflplanner-hash-")
  writeLines(enc2utf8(lines[!grepl(.gen_line, lines)]), f, useBytes = TRUE)
  unname(tools::md5sum(f))
}

# A program edited by hand, copied to programs/.edited/ before it is
# written again: named after its place under programs/ and the time
# (tfl_T-14-1-1_20261006-153000.R)
.edited_dir <- file.path("programs", ".edited")
.back_up_edited <- function(f, root) {
  dir <- file.path(root, .edited_dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  rel <- if (startsWith(f, root)) substring(f, nchar(root) + 2L) else basename(f)
  rel <- sub("^programs/", "", rel)
  dest <- file.path(dir, paste0(sub("[.][Rr]$", "", gsub("/", "_", rel, fixed = TRUE)),
                                "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".R"))
  file.copy(f, dest, overwrite = TRUE)
  invisible(dest)
}

.write_program <- function(code, f) {
  at <- grep("^#  Generated  :", code)[1L]
  code <- append(code, paste("#  Checksum   :", .body_hash(code)), at)
  writeLines(enc2utf8(code), f, useBytes = TRUE)
}

# missing / current / generated / edited, as a report program's
.ard_program_state <- function(code, f) {
  if (!file.exists(f)) return("missing")
  have <- readLines(f, warn = FALSE, encoding = "UTF-8")
  chk <- sub("^#  Checksum   : *", "",
             grep("^#  Checksum   :", have, value = TRUE))
  # an earlier tflplanner wrote some programs without a checksum
  if (!length(chk) && any(grepl("^#  Generated  : tflplanner", have))) {
    return("generated")
  }
  h <- .body_hash(have)
  if (!length(chk) || !identical(chk[1L], h)) return("edited")
  if (identical(h, .body_hash(code))) "current" else "generated"
}

# write a generated program (one edited by hand copied to
# programs/.edited/ first); the file's row of what the save did
.put_program <- function(code, f, root) {
  # (the same code over the file it wrote, untouched since: unchanged --
  # without reading the file again; #417, a save after a card's edit)
  md5 <- if (file.exists(f)) unname(tools::md5sum(f))
  hit <- .put_cache[[f]]
  if (!is.null(md5) && !is.null(hit) && identical(hit$md5, md5) && identical(hit$code, code)) {
    return("unchanged")
  }
  state <- .ard_program_state(code, f)
  if (state != "current") {
    if (state == "edited") .back_up_edited(f, root)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    .write_program(code, f)
  }
  .put_cache[[f]] <- list(md5 = unname(tools::md5sum(f)), code = code)
  switch(state, current = "unchanged", edited = "rewritten", "written")
}
.put_cache <- new.env()
