# The ARD programs' writer (moved from tflplanner's R/ard_programs.R as it
# was): the setup and one program per output under programs/ard/, written
# with .put_program() (R/program_writer.R).  The text is tfl_ard_program()'s.

.ard_prog_cache <- new.env(parent = emptyenv())

# the ARD programs, written with the study: the setup, one per output; a
# program (an earlier tflplanner's autoexec_ard.R too) of an output no longer defined goes (one edited by
# hand copied to programs/.edited/ first)
.save_ard_programs <- function(spec, root, codelists = NULL, standards = list(),
                               attached = character(), study_code = TRUE,
                               layout = tfl_study_layout(), date = Sys.Date(),
                               generator = paste("tflspec", utils::packageVersion("tflspec"))) {
  dir <- file.path(root, layout[["programs_ard"]])
  ids <- unique(stats::na.omit(spec$analyses$output_id))
  out <- data.frame(file = character(), status = character(),
                    stringsAsFactors = FALSE)
  put <- function(code, name) {
    f <- file.path(dir, name)
    out[nrow(out) + 1L, ] <<- list(f, .put_program(code, f, root))
  }
  put(tfl_ard_program(spec, NULL, standards = standards, attached = attached,
                      study_code = study_code, layout = layout, date = date,
                      generator = generator), .rp_ard_setup_file)
  # (an output's ARD program kept while the ARD definition, the code lists
  # and the day are the same: a save after an edit of a table writes the
  # same programs, #417)
  key <- list(spec, root, codelists, date, standards, attached, study_code,
              layout, generator)
  if (!identical(.ard_prog_cache$key, key)) {
    .ard_prog_cache$key <- key
    .ard_prog_cache$code <- list()
  }
  for (id in ids) {
    if (is.null(.ard_prog_cache$code[[id]])) {
      .ard_prog_cache$code[[id]] <- tfl_ard_program(
        spec, id, standards = standards, dir = root, codelists = codelists,
        attached = attached, study_code = study_code, layout = layout,
        date = date, generator = generator)
    }
    put(.ard_prog_cache$code[[id]], .ard_program_name(id))
  }
  keep <- c(.rp_ard_setup_file, vapply(ids, .ard_program_name, ""))
  for (f in setdiff(list.files(dir, "\.[Rr]$"), keep)) {
    p <- file.path(dir, f)
    have <- readLines(p, warn = FALSE, encoding = "UTF-8")
    chk <- sub("^#  Checksum   : *", "",
               grep("^#  Checksum   :", have, value = TRUE))
    # a program tflplanner wrote (with a checksum); a file of one's own,
    # never written by it, is not its to remove
    if (!length(chk)) next
    if (!identical(chk[1L], .body_hash(have))) .back_up_edited(p, root)
    file.remove(p)
    out[nrow(out) + 1L, ] <- list(p, "removed")
  }
  # the single make_ard.R of earlier versions
  old <- file.path(root, "programs", "make_ard.R")
  if (file.exists(old)) {
    file.remove(old)
    out[nrow(out) + 1L, ] <- list(old, "removed")
  }
  out
}
