# shift: percents of the Total row (#542), a parameter's own worst order,
# from a study's ARD (the template-study rule)

sh_study <- function(root) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  lb <- tflspec::adlb
  # one subject with no baseline category for ALT: the Missing row
  one <- lb$USUBJID[lb$PARAMCD == "ALT"][1L]
  lb$BNRIND[lb$PARAMCD == "ALT" & lb$USUBJID == one] <- NA
  for (x in list(list("adsl", tflspec::adsl), list("adlb", lb))) {
    haven::write_xpt(tflspec:::.ai_chr(x[[2L]]), file.path(d, "adam", paste0(x[[1L]], ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "SH-1")
  d
}

sh_report <- function(d, args, codelists = list()) {
  f <- file.path(d, "spec", "outputs", "T-SH.yml")
  tfl_write_output(tfl_output("T-SH", "table", population = "SAF", extends = "defaults",
                              codelists = codelists, template = list(name = "shift", args = args)), f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-SH", dir = d, save = FALSE))
  ro <- tfl_resolve_output(tfl_read_output(f), st)
  list(ard = ard, ro = ro, plan = suppressMessages(tfl_content_run(ro, ard)))
}

sh_pct <- function(x) {
  p <- suppressWarnings(as.numeric(sub("^.*\\(([0-9.]+)%\\)$", "\\1", x)))
  p[is.na(p)] <- 0
  p
}

test_that("shift: percent_of total -- each arm's body rows add to 100% of its Total row", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtfreporter")
  d <- sh_study(withr::local_tempdir())
  args <- list(population = "SAF", where = 'PARAMCD %in% c("ALT", "HGB")',
               percent_of = "total", cell = "{n} ({p:.1f%}%)", param_page = FALSE)
  r <- sh_report(d, args)
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(r$plan, "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  vals <- grep("____", names(t), value = TRUE)
  arms <- unique(sub("____.*$", "", vals))
  for (p in unique(t$group)) {
    rows <- t$group == p
    body <- rows & t$BNRIND != "Total"
    tot <- rows & t$BNRIND == "Total"
    for (a in arms) {
      cols <- vals[startsWith(vals, paste0(a, "____"))]
      # the body (the Missing row in it) is 100% of the arm's subjects
      expect_equal(sum(sh_pct(unlist(t[body, cols]))), 100, tolerance = 0.6, info = paste(p, a))
      # and so is the Total row
      expect_equal(sum(sh_pct(unlist(t[tot, cols]))), 100, tolerance = 0.6, info = paste(p, a))
    }
  }
  expect_true(any(t$BNRIND == "Missing" & t$group == "ALT" &
                    apply(t[vals], 1L, function(v) any(sh_pct(v) > 0))))
  # by row (the default): the Total row's percents are each of 100%, as before
  r0 <- sh_report(d, utils::modifyList(args, list(percent_of = NULL)))
  t0 <- as.data.frame(suppressMessages(rtfreporter::plan_apply(r0$plan, "table")))
  t0[] <- lapply(t0, function(v) if (is.factor(v)) as.character(v) else v)
  low <- t0$BNRIND == "LOW" & t0$group == "ALT"
  if (any(low)) {
    a <- arms[1L]
    cols <- vals[startsWith(vals, paste0(a, "____"))]
    s <- sum(sh_pct(unlist(t0[low, cols])))
    expect_true(s == 0 || abs(s - 100) < 0.6)
  }
})

test_that("shift: worst_by gives a parameter its own order from the worst", {
  x <- tfl_template_expand("shift", list(population = "SAF",
                                         worst_by = list(HGB = c("LOW", "HIGH", "NORMAL"))))
  code <- as.character(x$ard$analysis_data[[1L]]$code)
  expect_match(code, 'PARAMCD == "HGB" ~ match(ANRIND, c("LOW", "HIGH", "NORMAL"))', fixed = TRUE)
  expect_match(code, 'TRUE ~ match(ANRIND, c("HIGH", "LOW", "NORMAL"))', fixed = TRUE)
  # a text of the order, as a sheet holds it
  y <- tfl_template_expand("shift", list(population = "SAF", worst_by = list(HGB = "LOW, HIGH, NORMAL")))
  expect_identical(as.character(y$ard$analysis_data[[1L]]$code), code)
  # run on the records: a subject both LOW and HIGH for HGB is LOW, for ALT HIGH
  skip_if_not_installed("dplyr")
  lb <- data.frame(USUBJID = c("S1", "S1", "S1", "S1"), PARAMCD = c("HGB", "HGB", "ALT", "ALT"),
                   ABLFL = "", ANRIND = c("HIGH", "LOW", "LOW", "HIGH"), BNRIND = "NORMAL",
                   stringsAsFactors = FALSE)
  d <- local({
    adlb <- lb
    adsl_sub <- data.frame(USUBJID = "S1", TRT01A = "A")
    eval(parse(text = code))
  })
  w <- d[d$BNRIND != "Total", ]
  expect_identical(w$ANRIND[w$PARAMCD == "HGB"], "LOW")
  expect_identical(w$ANRIND[w$PARAMCD == "ALT"], "HIGH")
  # percent_of total needs a subject once a parameter
  expect_error(tfl_template_expand("shift", list(population = "SAF", worst = NULL,
                                                 percent_of = "total")), "give `worst`")
})

test_that("shift: the rows are in the code list's labels, Missing and Total last", {
  rows <- function(...) lapply(list(...), function(v) list(value = v[1L], label = v[2L]))
  lv <- function(cl) {
    x <- tfl_template_expand("shift", list(population = "SAF"), codelists = cl)
    Filter(function(it) it$fn == "plan_levels", x$content)[[1L]]$args
  }
  cl <- rows(c("LOW", "Low"), c("NORMAL", "Normal"), c("HIGH", "High"))
  # codes in `levels`, labels in the ARD: the table's values are the labels
  a <- lv(list(BNRIND = cl, ANRIND = cl))
  expect_identical(a$BNRIND, c("Low", "Normal", "High", "Missing", "Total"))
  expect_identical(a$.label, c("Low", "Normal", "High"))
  # named codes (a program's `c(LOW = "Low")`) the same
  nm <- c(LOW = "Low", NORMAL = "Normal", HIGH = "High")
  expect_identical(lv(list(BNRIND = nm, ANRIND = nm))$BNRIND, a$BNRIND)
  # no code list, or `levels` given as the labels: as they are
  expect_identical(lv(NULL)$BNRIND, c("LOW", "NORMAL", "HIGH", "Missing", "Total"))
  y <- tfl_template_expand("shift", list(population = "SAF", levels = c("Low", "Normal", "High")),
                           codelists = list(BNRIND = cl, ANRIND = cl))
  expect_identical(Filter(function(it) it$fn == "plan_levels", y$content)[[1L]]$args$BNRIND,
                   a$BNRIND)
})

test_that("shift: built with a code list on the categories, the rows run Low ... Total", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtfreporter")
  d <- sh_study(withr::local_tempdir())
  rows <- function(...) lapply(list(...), function(v) list(value = v[1L], label = v[2L]))
  cl <- rows(c("LOW", "Low"), c("NORMAL", "Normal"), c("HIGH", "High"))
  # (Missing and Total are still listed: the analysis data holds them)
  r <- sh_report(d, list(population = "SAF", where = 'PARAMCD %in% c("ALT", "HGB")',
                         param_page = FALSE),
                 codelists = list(BNRIND = c(cl, rows(c("Missing", "Missing"), c("Total", "Total"))),
                                  ANRIND = cl))
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(r$plan, "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  expect_identical(unique(t$BNRIND[t$group == "ALT"]),
                   c("Low", "Normal", "High", "Missing", "Total"))
})

# ---- each page's N (#630) ----

# the header of a page, line 1 (the arm and its N), from the plan
sn_hdr1 <- function(pg) {
  r <- pg$col_header[[1L]]
  unique(vapply(r, function(z) as.character(z$label), ""))
}

# a study with a copy of ADLB: ALT dropped for 12 Drug X and 5 Control subjects
sn_study <- function(root, drop = TRUE) {
  d <- file.path(root, "study")
  dir.create(file.path(d, "adam"), recursive = TRUE)
  lb <- tflspec::adlb
  sl <- tflspec::adsl
  saf <- sl[sl$SAFFL == "Y", ]
  gone <- c(saf$USUBJID[saf$TRT01A == "Drug X"][1:12], saf$USUBJID[saf$TRT01A == "Control"][1:5])
  if (drop) lb <- lb[!(lb$PARAMCD == "ALT" & lb$USUBJID %in% gone), ]
  for (x in list(list("adsl", sl), list("adlb", lb))) {
    haven::write_xpt(tflspec:::.ai_chr(x[[2L]]), file.path(d, "adam", paste0(x[[1L]], ".xpt")))
  }
  tfl_ai_study_init(d, adam = "adam", study_id = "SN-1")
  d
}

sn_report <- function(d, args) {
  f <- file.path(d, "spec", "outputs", "T-SN.yml")
  tfl_write_output(tfl_output("T-SN", "table", population = "SAF", extends = "defaults",
                              template = list(name = "lab_shift",
                                              args = c(list(population = "SAF",
                                                            visit = "CYCLE 2 DAY 1",
                                                            params = c("ALT", "AST"),
                                                            cell = "{n} ({p:.1f%})"), args))),
                   f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-SN", dir = d, save = FALSE))
  ro <- tfl_resolve_output(tfl_read_output(f), st)
  suppressMessages(tfl_content_run(ro, ard))
}

# the key, counted in plain R: the SAF subjects of an arm with a record after
# baseline for the parameter
sn_key <- function(lb, param) {
  saf <- tflspec::adsl[tflspec::adsl$SAFFL == "Y", ]
  r <- lb[lb$AVISIT == "CYCLE 2 DAY 1" & lb$PARAMCD == param & !(lb$ABLFL %in% "Y") &
            !is.na(lb$ANRIND) & lb$USUBJID %in% saf$USUBJID, ]
  arm <- saf$TRT01A[match(unique(r$USUBJID), saf$USUBJID)]
  c(`Drug X` = sum(arm == "Drug X"), Control = sum(arm == "Control"))
}

sn_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtfreporter")
  # the N of a page needs the shift templates' .label column key in
  # plan_col_header() (rtfreporter#898)
  skip_if_not(any(grepl("\".label\"", deparse(rtfreporter:::.plan_n_read), fixed = TRUE)))
}

test_that("shift: each page's header shows its parameter's N, counted per page (#630)", {
  sn_skip()
  # the bundled data: ALT 75 / 75, AST 75 / 75
  d <- sn_study(withr::local_tempdir(), drop = FALSE)
  expect_identical(sn_key(tflspec::adlb, "ALT"), c(`Drug X` = 75L, Control = 75L))
  expect_identical(sn_key(tflspec::adlb, "AST"), c(`Drug X` = 75L, Control = 75L))
  p <- sn_report(d, list())
  pg <- suppressMessages(rtfreporter::plan_apply(p, "pages"))
  expect_identical(names(pg), c("Parameter: ALT", "Parameter: AST"))
  expect_identical(sn_hdr1(pg[[1L]]), c("", "Drug X (N=75)", "Control (N=75)"))
  expect_identical(sn_hdr1(pg[[2L]]), c("", "Drug X (N=75)", "Control (N=75)"))
})

test_that("shift: a parameter with fewer subjects shows its own N on its own page (#630)", {
  sn_skip()
  d <- sn_study(withr::local_tempdir())
  lb2 <- haven::read_xpt(file.path(d, "adam", "adlb.xpt"))
  # the key is not the template's: 65 / 70 for ALT, 75 / 75 for AST
  p <- sn_report(d, list(percent_of = "total"))
  pg <- suppressMessages(rtfreporter::plan_apply(p, "pages"))
  alt <- sn_key(lb2, "ALT")
  ast <- sn_key(lb2, "AST")
  expect_identical(sn_hdr1(pg[[1L]])[-1L],
                   sprintf("%s (N=%d)", c("Drug X", "Control"), alt))
  expect_identical(sn_hdr1(pg[[2L]])[-1L],
                   sprintf("%s (N=%d)", c("Drug X", "Control"), ast))
  expect_identical(unname(alt), c(65L, 70L))
  expect_identical(unname(ast), c(75L, 75L))
  # the printed N is the Total row's count, and percent_of = "total"'s denominator
  tt <- as.data.frame(suppressMessages(rtfreporter::plan_apply(p, "table")))
  tt[] <- lapply(tt, function(v) if (is.factor(v)) as.character(v) else v)
  vals <- grep("____", names(tt), value = TRUE)
  n_of <- function(x) as.integer(sub("^([0-9]+) .*$", "\\1", x))
  for (par in c("ALT", "AST")) {
    for (arm in c("Drug X", "Control")) {
      cols <- vals[startsWith(vals, paste0(arm, "____"))]
      tot <- tt[tt$group == par & tt$BNRIND == "Total", cols]
      expect_equal(sum(n_of(unlist(tot))), get(tolower(par))[[arm]], info = paste(par, arm))
    }
  }
  # and the RTF text says it too
  f <- withr::local_tempfile(fileext = ".rtf")
  suppressMessages(rtfreporter::generate_rtfreport(
    rtfreporter::rtf_tables(rtfreporter::rtf_document(), p), f, overwrite = TRUE))
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_true(grepl("Drug X (N=65)", txt, fixed = TRUE))
  expect_true(grepl("Control (N=70)", txt, fixed = TRUE))
  expect_true(grepl("Drug X (N=75)", txt, fixed = TRUE))
  expect_true(grepl("Control (N=75)", txt, fixed = TRUE))
})

test_that("shift: header_n = FALSE and param_page = FALSE show no N (#630)", {
  sn_skip()
  d <- sn_study(withr::local_tempdir())
  for (args in list(list(header_n = FALSE), list(param_page = FALSE))) {
    p <- sn_report(d, args)
    pg <- suppressMessages(rtfreporter::plan_apply(p, "pages"))
    for (g in pg) expect_false(any(grepl("N=", sn_hdr1(g), fixed = TRUE)))
  }
  pg <- suppressMessages(rtfreporter::plan_apply(sn_report(d, list(header_n = FALSE)), "pages"))
  expect_identical(names(pg), c("Parameter: ALT", "Parameter: AST"))
  expect_identical(sn_hdr1(pg[[1L]]), c("", "Drug X", "Control"))
  # the N's analysis is there only when it is shown
  x <- tfl_template_expand("lab_shift", list(population = "SAF", visit = "V", params = "ALT"))
  expect_length(x$ard$analyses, 2L)
  y <- tfl_template_expand("lab_shift", list(population = "SAF", visit = "V", params = "ALT",
                                             header_n = FALSE))
  expect_length(y$ard$analyses, 1L)
  z <- tfl_template_expand("lab_shift", list(population = "SAF", visit = "V", params = "ALT",
                                             param_page = FALSE))
  expect_length(z$ard$analyses, 1L)
})

# the report with code lists whose codes differ from the labels, and the
# program it writes run by a child R
sh_codelist_program <- function(d, cl, extra, rscript) {
  paged <- is.null(extra$param_page)
  info <- if (paged) "default" else "param_page = FALSE"
  args <- c(list(population = "SAF", where = 'PARAMCD %in% c("ALT", "HGB")',
                 levels = c("LOW", "NORMAL", "HIGH")), extra)
  r <- sh_report(d, args, codelists = list(BNRIND = cl, ANRIND = cl))
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(r$plan, "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  rws <- c("Low", "Normal", "High", "Missing", "Total")
  if (paged) {
    pg <- suppressMessages(rtfreporter::plan_apply(r$plan, "pages"))
    expect_length(pg, 2L)
    for (g in pg) {
      expect_identical(unique(as.character(g$data$row_label)), rws, info = info)
      h <- unique(vapply(g$col_header[[1L]], function(z) as.character(z$label), ""))
      expect_true(all(grepl("\\(N=[0-9]+\\)", h[-1L])), info = info)
    }
  } else {
    expect_identical(unique(t$BNRIND[t$group == "ALT"]), rws, info = info)
  }
  # the report's own list keeps its values; the template added the two
  expect_identical(vapply(r$ro$codelists$BNRIND, function(x) x$value, ""),
                   c("LOW", "NORMAL", "HIGH", "Missing", "Total"))
  # the program writes the codes with their labels
  code <- tfl_content_code(r$ro, pipe = "|>")
  expect_true(any(grepl("LOW = \"Low\"", code, fixed = TRUE)))
  # run by a child R: the same rows and labels (and, paged, the same pages)
  w <- withr::local_tempdir()
  withr::local_dir(w)
  saveRDS(r$ard, "ard.rds")
  writeLines(c("library(rtfreporter)", "ard <- readRDS('ard.rds')", code,
               "saveRDS(plan_apply(plan, 'table'), 'tab.rds')",
               "saveRDS(plan_apply(plan, 'pages'), 'pages.rds')"), "prog.R")
  out <- suppressWarnings(system2(rscript, "prog.R", stdout = TRUE, stderr = TRUE))
  expect_true(file.exists("tab.rds"), info = paste(out, collapse = "
"))
  expect_identical(readRDS("tab.rds"), rtfreporter::plan_apply(r$plan, "table"))
  expect_identical(readRDS("pages.rds"), suppressMessages(rtfreporter::plan_apply(r$plan, "pages")))
}

test_that("shift: Missing and Total need not be in the code list; the program maps the codes as the build does", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtfreporter")
  d <- sh_study(withr::local_tempdir())
  rows <- function(...) lapply(list(...), function(v) list(value = v[1L], label = v[2L]))
  cl <- rows(c("LOW", "Low"), c("NORMAL", "Normal"), c("HIGH", "High"))
  rscript <- file.path(R.home("bin"), "Rscript")
  withr::local_envvar(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  has <- suppressWarnings(system2(rscript, c("-e", shQuote("cat(requireNamespace('tflspec', quietly = TRUE))")),
                                  stdout = TRUE, stderr = FALSE))
  skip_if_not(identical(utils::tail(has, 1L), "TRUE"), "tflspec is not installed where a child R looks")
  # param_page = FALSE, and the default (a page a parameter, each with its N)
  for (pp in list(list(param_page = FALSE), list())) {
    # (the page N needs rtfreporter#898, as the #630 tests do)
    if (is.null(pp$param_page)) sn_skip()
    sh_codelist_program(d, cl, pp, rscript)
  }
})

test_that("shift: param_labels as a list of {name, text} builds, the page's Parameter line the label (#637)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtfreporter")
  d <- sh_study(withr::local_tempdir())
  lab <- "Alanine Aminotransferase (U/L)"
  args <- list(population = "SAF", where = 'PARAMCD == "ALT"')
  r <- sh_report(d, c(args, list(param_labels = list(list(name = "ALT", text = lab)))))
  t <- as.data.frame(suppressMessages(rtfreporter::plan_apply(r$plan, "table")))
  expect_true(any(as.character(t$group) == lab))
  m <- sh_report(d, c(args, list(param_labels = list(ALT = lab))))
  tm <- as.data.frame(suppressMessages(rtfreporter::plan_apply(m$plan, "table")))
  expect_identical(t, tm)
  pg <- suppressMessages(rtfreporter::plan_apply(r$plan, "pages"))
  expect_identical(names(pg), paste("Parameter:", lab))
})

# ---- post_header: the header over each post-baseline category (#642) ----

# a report of `shift` or `lab_shift` (the sample study of sh_study()), the
# plan, the ARD and the resolved output
ph_report <- function(d, name, args, codelists = list()) {
  args <- c(list(population = "SAF"),
            if (name == "lab_shift") list(visit = "CYCLE 2 DAY 1", params = "HGB")
            else list(where = 'PARAMCD == "HGB" & AVISIT == "CYCLE 2 DAY 1"'),
            args)
  f <- file.path(d, "spec", "outputs", "T-PH.yml")
  tfl_write_output(tfl_output("T-PH", "table", population = "SAF", extends = "defaults",
                              codelists = codelists, template = list(name = name, args = args)), f)
  st <- tfl_read_study_spec(d)
  ard <- suppressMessages(tfl_build_ard(st, output_id = "T-PH", dir = d, save = FALSE))
  ro <- tfl_resolve_output(tfl_read_output(f), st)
  list(ard = ard, ro = ro, st = st, plan = suppressMessages(tfl_content_run(ro, ard)))
}

# line 2 of a page's header, the values' cells
ph_line2 <- function(pg) {
  as.character(pg$col_header[[2L]])
}

ph_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("haven")
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rtfreporter")
}

test_that("shift, lab_shift: post_header prints its text over each category, the label or the code (#642)", {
  ph_skip()
  d <- sh_study(withr::local_tempdir())
  txt <- "Worst post-baseline [a]: {col}"
  rows <- function(...) lapply(list(...), function(v) list(value = v[1L], label = v[2L]))
  cl <- rows(c("LOW", "Low"), c("NORMAL", "Normal"), c("HIGH", "High"))
  for (nm in c("shift", "lab_shift")) {
    for (with_cl in c(FALSE, TRUE)) {
      info <- paste(nm, if (with_cl) "with codelists" else "without")
      ccl <- if (with_cl) list(BNRIND = cl, ANRIND = cl) else list()
      cats <- if (with_cl) c("Low", "Normal", "High") else c("LOW", "NORMAL", "HIGH")
      want <- paste0("Worst post-baseline [a]: ", cats)
      r <- ph_report(d, nm, list(post_header = txt), ccl)
      pg <- suppressMessages(rtfreporter::plan_apply(r$plan, "pages"))
      expect_length(pg, 1L)
      for (g in pg) {
        l2 <- ph_line2(g)
        # the label column's heading, then the categories under each arm
        expect_identical(l2[1L], "Parameter / Baseline", info = info)
        expect_identical(unique(l2[-1L]), want, info = info)
        expect_length(l2, 1L + 2L * 3L)
      }
      f <- withr::local_tempfile(fileext = ".rtf")
      suppressMessages(rtfreporter::generate_rtfreport(
        rtfreporter::rtf_tables(rtfreporter::rtf_document(), r$plan), f, overwrite = TRUE))
      rtf <- paste(readLines(f, warn = FALSE), collapse = "\n")
      for (w in want) expect_true(grepl(w, rtf, fixed = TRUE), info = paste(info, w))
      # without the argument: the categories alone, as before
      r0 <- ph_report(d, nm, list(), ccl)
      pg0 <- suppressMessages(rtfreporter::plan_apply(r0$plan, "pages"))
      expect_identical(unique(ph_line2(pg0[[1L]])[-1L]), cats, info = info)
    }
  }
})

test_that("shift, lab_shift: post_header with no {col}, or header_n = FALSE, prints as written (#642)", {
  ph_skip()
  d <- sh_study(withr::local_tempdir())
  for (nm in c("shift", "lab_shift")) {
    r <- ph_report(d, nm, list(post_header = "Worst [a]", header_n = FALSE))
    pg <- suppressMessages(rtfreporter::plan_apply(r$plan, "pages"))
    expect_identical(unique(ph_line2(pg[[1L]])[-1L]), "Worst [a]", info = nm)
    # header_n = FALSE: line 1 is the arms only, line 2 as above
    expect_identical(unique(vapply(pg[[1L]]$col_header[[1L]],
                                   function(z) as.character(z$label), "")),
                     c("", "Drug X", "Control"), info = nm)
    r2 <- ph_report(d, nm, list(post_header = "{col} [a]", header_n = FALSE))
    pg2 <- suppressMessages(rtfreporter::plan_apply(r2$plan, "pages"))
    expect_identical(unique(ph_line2(pg2[[1L]])[-1L]),
                     c("LOW [a]", "NORMAL [a]", "HIGH [a]"), info = nm)
  }
})

test_that("shift, lab_shift: the default post_header is {col}, the expansion as before; the ARD is the same (#642)", {
  base <- list(population = "SAF", visit = "V", params = "ALT")
  for (nm in c("shift", "lab_shift")) {
    a <- if (nm == "shift") list(population = "SAF") else base
    x <- tfl_template_expand(nm, a)
    y <- tfl_template_expand(nm, c(a, list(post_header = "{col}")))
    z <- tfl_template_expand(nm, c(a, list(post_header = "Worst post-baseline [a]: {col}")))
    expect_identical(y, x, info = nm)
    h <- function(e) Filter(function(it) it$fn == "plan_col_header", e$content)[[1L]]$args$header
    expect_identical(h(x)[[3L]]$text, "{col}", info = nm)
    expect_identical(h(z)[[3L]]$text, "Worst post-baseline [a]: {col}", info = nm)
    # only that text differs
    expect_identical(z$ard, x$ard, info = nm)
    expect_identical(z$codelists, x$codelists, info = nm)
    h2 <- h(z)
    h2[[3L]]$text <- "{col}"
    expect_identical(h2, h(x), info = nm)
  }
})

test_that("shift, lab_shift: post_header leaves the ARD and its program unchanged (#642)", {
  ph_skip()
  d <- sh_study(withr::local_tempdir())
  rows <- function(...) lapply(list(...), function(v) list(value = v[1L], label = v[2L]))
  cl <- rows(c("LOW", "Low"), c("NORMAL", "Normal"), c("HIGH", "High"))
  for (nm in c("shift", "lab_shift")) {
    r0 <- ph_report(d, nm, list(), list(ANRIND = cl, BNRIND = cl))
    r1 <- ph_report(d, nm, list(post_header = "Worst post-baseline [a]: {col}"),
                    list(ANRIND = cl, BNRIND = cl))
    expect_identical(r1$ard, r0$ard, info = nm)
    # the code list's labels stay the categories' names
    expect_identical(r1$ro$codelists$ANRIND, r0$ro$codelists$ANRIND, info = nm)
    expect_identical(vapply(r1$ro$codelists$ANRIND, function(x) x$label, ""),
                     c("Low", "Normal", "High"), info = nm)
    # the program that builds the ARD
    expect_identical(tfl_ard_code(r1$st, output_id = "T-PH", save = FALSE),
                     tfl_ard_code(r0$st, output_id = "T-PH", save = FALSE), info = nm)
  }
})
