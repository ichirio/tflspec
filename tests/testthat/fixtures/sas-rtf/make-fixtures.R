# Fictional SAS ODS RTF files (DMY-901 / DMY-902, Example Pharma), written by
# hand in the form SAS's ODS RTF destination writes: titles and footnotes in
# \header / \footer, one \trowd ... \row group a row, the column header rows
# marked \trhdr and repeated on each page, a page a \sect.  No real file is
# behind them.  Run from this folder to rewrite them:
#   Rscript make-fixtures.R
head <- function(landscape = TRUE) paste0(
  "{\\rtf1\\ansi\\ansicpg1252\\uc1\\deff0\\deflang1033\\deflangfe1033\n",
  "{\\fonttbl{\\f1\\froman\\fprq2\\fcharset0 Times New Roman;}{\\f2\\fmodern\\fprq1\\fcharset0 Courier New;}}\n",
  "{\\colortbl;\\red0\\green0\\blue0;\\red255\\green255\\blue255;}\n",
  if (landscape) "\\paperw15840\\paperh12240\\margl1440\\margr1440\\margt1440\\margb1440\\landscape\n"
  else "\\paperw12240\\paperh15840\\margl1440\\margr1440\\margt1440\\margb1440\n",
  "\\widowctrl\\ftnbj\\fet0\\sectd\\linex0\\endnhere\\sbknone\n")
# a page's titles and footnotes (ODS: in the section's header and footer)
hf <- function(titles, foots) paste0(
  "{\\header\\pard\\plain\\qr{\\f1\\fs18 Example Pharma Page {\\field{\\*\\fldinst PAGE}} of {\\field{\\*\\fldinst NUMPAGES}}}\\par\n",
  paste0("\\pard\\plain\\qc{\\f1\\fs20 ", titles, "}\\par\n", collapse = ""), "}\n",
  "{\\footer", paste0("\\pard\\plain\\ql{\\f1\\fs16 ", foots, "}\\par\n", collapse = ""), "}\n")
# a row: its cells' right edges, top / bottom borders (one 0/1 a cell),
# alignments, texts; `hdr`: a column header row (\trhdr)
row <- function(cx, txt, al, top = 0, bot = 0, hdr = FALSE, fs = 18) {
  n <- length(cx)
  top <- rep_len(top, n); bot <- rep_len(bot, n); al <- rep_len(al, n)
  defs <- paste0(ifelse(top == 1, "\\clbrdrt\\brdrs\\brdrw15", ""),
                 ifelse(bot == 1, "\\clbrdrb\\brdrs\\brdrw15", ""),
                 "\\clvertalb\\cellx", cx, collapse = "")
  cells <- paste0("\\pard\\plain\\intbl\\sb20\\sa20\\q", al, "{\\f1\\fs", fs, " ", txt, "}\\cell\n", collapse = "")
  paste0("\\trowd\\trkeep", if (hdr) "\\trhdr", "\\trqc\\trgaph58", defs, "\n", cells,
         "{\\row}\n")
}
page_break <- "\\sect\\sectd\\linex0\\endnhere\\sbkpage\n"
end <- "}\n"

# ---- DMY-901 t_dm.rtf: demographics, one page -------------------------------
cx <- c(4320, 7200, 10080, 12960)
dm <- paste0(head(), hf(c("Table 14.1.1", "Demographic and Baseline Characteristics", "(Safety Population)"),
                      c("n (%) = number (percentage) of subjects. SD = standard deviation.",
                        "Program: t_dm.sas")),
  row(cx, c("", "Drug X\\line (N=20)", "Placebo\\line (N=21)", "Total\\line (N=41)"), c("l", "c", "c", "c"),
      top = 1, bot = 1, hdr = TRUE),
  row(cx, c("Age (years)", "", "", ""), "l"),
  row(cx, c("\\~\\~n", "20", "21", "41"), c("l", "c", "c", "c")),
  row(cx, c("\\~\\~Mean (SD)", "54.2 (8.31)", "55.0 (7.92)", "54.6 (8.04)"), c("l", "c", "c", "c")),
  row(cx, c("\\~\\~Median", "53.5", "56.0", "55.0"), c("l", "c", "c", "c")),
  row(cx, c("\\~\\~Min, Max", "38, 71", "40, 69", "38, 71"), c("l", "c", "c", "c")),
  row(cx, c("Sex, n (%)", "", "", ""), "l"),
  row(cx, c("\\~\\~Female", "11 (55.0)", "10 (47.6)", "21 (51.2)"), c("l", "c", "c", "c")),
  row(cx, c("\\~\\~Male", "9 (45.0)", "11 (52.4)", "20 (48.8)"), c("l", "c", "c", "c"), bot = 1),
  end)

# ---- DMY-901 t_ae_soc.rtf: AE by SOC / PT over two pages ---------------------
cx2 <- c(5760, 8640, 11520)
hdr2 <- paste0(
  row(cx2, c("", "Drug X", "Placebo"), c("l", "c", "c"), top = 1, hdr = TRUE),
  row(cx2, c("System Organ Class\\line \\~\\~Preferred Term", "(N=20)\\line n (%)", "(N=21)\\line n (%)"),
      c("l", "c", "c"), bot = 1, hdr = TRUE))
t2 <- c("Table 14.3.1.2", "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
        "(Safety Population)")
f2 <- c("Subjects are counted once per system organ class and preferred term.", "Program: t_ae_soc.sas")
ae <- paste0(head(), hf(t2, f2), hdr2,
  row(cx2, c("Any TEAE", "15 (75.0)", "12 (57.1)"), c("l", "c", "c")),
  row(cx2, c("Gastrointestinal disorders", "9 (45.0)", "5 (23.8)"), c("l", "c", "c")),
  row(cx2, c("\\~\\~Diarrhoea", "6 (30.0)", "3 (14.3)"), c("l", "c", "c")),
  row(cx2, c("\\~\\~Nausea", "4 (20.0)", "2 (9.5)"), c("l", "c", "c")),
  page_break, hf(t2, f2), hdr2,
  row(cx2, c("General disorders", "7 (35.0)", "6 (28.6)"), c("l", "c", "c")),
  row(cx2, c("\\~\\~Fatigue", "7 (35.0)", "6 (28.6)"), c("l", "c", "c"), bot = 1),
  end)

# ---- DMY-902 t_lb_chg.rtf: lab values and change, portrait -------------------
cx3 <- c(2880, 4320, 5760, 7200, 8640, 10800)
lb <- paste0(head(FALSE), hf(c("Table 14.3.4.1", "Laboratory Values and Change from Baseline: Chemistry",
                                "(Safety Population)"), "Program: t_lb_chg.sas"),
  row(cx3, c("", "Drug X (N=30)", "", "Placebo (N=30)", "", ""), c("l", "c", "c", "c", "c", "c"),
      top = 1, bot = c(0, 1, 1, 1, 1, 0), hdr = TRUE),
  row(cx3, c("Parameter\\line \\~\\~Visit", "Value", "Change", "Value", "Change", ""),
      c("l", "c", "c", "c", "c", "c"), bot = 1, hdr = TRUE),
  row(cx3, c("ALT (U/L)", "", "", "", "", ""), "l"),
  row(cx3, c("\\~\\~Baseline", "24.1 (8.20)", "", "23.8 (7.91)", "", ""), c("l", "c", "c", "c", "c", "c")),
  row(cx3, c("\\~\\~Week 4", "26.3 (9.02)", "2.2 (5.11)", "24.0 (8.03)", "0.2 (4.87)", ""),
      c("l", "c", "c", "c", "c", "c"), bot = 1),
  end)

# ---- DMY-902 l_ae.rtf: a listing ---------------------------------------------
cx4 <- c(1800, 3600, 7200, 9000, 10800, 12960)
li <- paste0(head(), hf(c("Listing 16.2.7.1", "Listing of Adverse Events", "(Safety Population)"),
                        "Program: l_ae.sas"),
  row(cx4, c("Subject", "Treatment", "Preferred Term", "Start Date", "Grade", "Serious"), "l",
      top = 1, bot = 1, hdr = TRUE),
  row(cx4, c("DMY-902-001", "Drug X", "Headache", "2024-03-02", "1", "N"), "l"),
  row(cx4, c("DMY-902-004", "Placebo", "Nausea", "2024-03-15", "2", "N"), "l", bot = 1),
  end)

for (f in list(c("DMY-901/t_dm.rtf", dm), c("DMY-901/t_ae_soc.rtf", ae),
               c("DMY-902/t_lb_chg.rtf", lb), c("DMY-902/l_ae.rtf", li))) {
  dir.create(dirname(f[1L]), showWarnings = FALSE)
  writeLines(f[2L], f[1L], useBytes = TRUE)
}
