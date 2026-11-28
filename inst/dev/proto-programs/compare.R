# The comparisons of Gate 1: the study ARD by its keyed rows, each RTF with
# its timestamps masked, report_status.csv's ARD fingerprint per report.

# a list column (or a column) as text, a value a row
pp_chr <- function(v) {
  if (!is.list(v)) return(as.character(v))
  vapply(v, function(e) if (!length(e)) NA_character_ else
    paste(format(unlist(e), digits = 15), collapse = "|"), "")
}

# an ARD's rows keyed: key (the row's meaning) and value (its statistic)
pp_keyed <- function(ard) {
  if (is.null(ard) || !nrow(ard)) return(data.frame(output_id = character(), key = character(),
                                                    value = character()))
  kc <- intersect(c("output_id", "analysis_id", "population_id", "variable", "variable_level",
                    grep("^group[0-9]+(_level)?$", names(ard), value = TRUE),
                    "context", "stat_name"), names(ard))
  vc <- intersect(c("stat", "stat_fmt", "stat_label", "fmt_fun", "warning", "error"), names(ard))
  vc <- setdiff(vc, "fmt_fun")
  k <- do.call(paste, c(lapply(ard[kc], pp_chr), sep = "\r"))
  v <- do.call(paste, c(lapply(ard[vc], pp_chr), sep = "\r"))
  data.frame(output_id = pp_chr(ard$output_id), key = k, value = v, stringsAsFactors = FALSE)
}

# two keyed ARDs by output: rows, keys in one only, values that differ
pp_ard_diff <- function(a, b) {
  ids <- sort(unique(c(a$output_id, b$output_id)))
  do.call(rbind, lapply(ids, function(id) {
    x <- a[a$output_id == id, ]
    y <- b[b$output_id == id, ]
    m <- match(x$key, y$key)
    data.frame(output_id = id, rows_a = nrow(x), rows_b = nrow(y),
               only_a = sum(is.na(m)), only_b = sum(!y$key %in% x$key),
               dup_keys = sum(duplicated(x$key)) + sum(duplicated(y$key)),
               values_differ = sum(!is.na(m) & x$value != y$value[m]),
               stringsAsFactors = FALSE)
  }))
}

# an RTF's text, what changes from run to run masked: dates and times, the
# \creatim / \revtim groups, a run's temporary names
pp_rtf_text <- function(f) {
  if (!file.exists(f)) return(NA_character_)
  t <- rawToChar(readBin(f, "raw", file.size(f)))
  t <- gsub("\\\\(creatim|revtim|printim)(\\\\[a-z]+[0-9]+)+", "\\\\\\1<t>", t)
  t <- gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}([ T][0-9]{2}:[0-9]{2}(:[0-9]{2})?)?", "<date>", t)
  t <- gsub("[0-9]{2}(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)[0-9]{4}([ :][0-9]{2}:[0-9]{2}(:[0-9]{2})?)?", "<date>", t)
  t <- gsub("[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?", "<time>", t)
  t
}

pp_read_ard <- function(dir) {
  f <- file.path(dir, "output", "ard", "ard.rds")
  if (file.exists(f)) readRDS(f)
}

pp_status <- function(dir) {
  f <- file.path(dir, "output", "tfl", "report_status.csv")
  if (!file.exists(f)) return(data.frame(output_id = character(), ard = character()))
  utils::read.csv(f, colClasses = "character")
}

# d: the folders c(today =, full =, ard =); ids: the reports
pp_compare <- function(d, ids) {
  ka <- lapply(d, function(x) pp_keyed(pp_read_ard(x)))
  ard_today_full <- pp_ard_diff(ka$today, ka$full)
  ard_full_ard <- pp_ard_diff(ka$full, ka$ard)
  rtf <- do.call(rbind, lapply(ids, function(id) {
    a <- pp_rtf_text(file.path(d[["today"]], "output", "tfl", paste0(id, ".rtf")))
    b <- pp_rtf_text(file.path(d[["full"]], "output", "tfl", paste0(id, ".rtf")))
    data.frame(output_id = id, today = !is.na(a), full = !is.na(b),
               same = if (is.na(a) || is.na(b)) is.na(a) && is.na(b) else identical(a, b),
               stringsAsFactors = FALSE)
  }))
  st <- lapply(d[c("today", "full")], pp_status)
  sm <- merge(st$today[c("output_id", "ard", "empty")], st$full[c("output_id", "ard", "empty")],
              by = "output_id", all = TRUE, suffixes = c("_today", "_full"))
  same_ard <- function(t) {
    sum(t$only_a + t$only_b + t$values_differ + t$dup_keys) == 0L
  }
  summary <- data.frame(
    check = c("study ARD: today vs full (keyed rows)",
              "study ARD: full vs ard run (keyed rows)",
              "RTF: today vs full (timestamps masked)",
              "report_status.csv: today vs full (ard fingerprint, empty)"),
    reports = c(nrow(ard_today_full), nrow(ard_full_ard), nrow(rtf), nrow(sm)),
    rows = c(sum(ard_today_full$rows_a), sum(ard_full_ard$rows_a),
             sum(rtf$today & rtf$full), NA),
    differ = c(sum(ard_today_full$only_a + ard_today_full$only_b + ard_today_full$values_differ),
               sum(ard_full_ard$only_a + ard_full_ard$only_b + ard_full_ard$values_differ),
               sum(!rtf$same),
               sum(!(sm$ard_today %in% sm$ard_full & sm$empty_today %in% sm$empty_full) |
                     xor(is.na(sm$ard_today), is.na(sm$ard_full)))),
    same = c(same_ard(ard_today_full), same_ard(ard_full_ard), all(rtf$same),
             identical(sm$ard_today, sm$ard_full) && identical(sm$empty_today, sm$empty_full)),
    stringsAsFactors = FALSE)
  list(summary = summary, ard_today_full = ard_today_full, ard_full_ard = ard_full_ard,
       rtf = rtf, status = sm)
}
