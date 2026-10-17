# A TOC / SHELL workbook as text for an AI chat

Reads a company's TOC and shells from one Excel workbook without knowing
its layout and writes them as compact text an AI chat reads: every sheet
(its name kept), its non-empty cells in reading order – row by row, left
to right – each with its column letter, a merged cell once with its
range (`A1:E1=...`), a cell's indent (the alignment's indent level) as
two spaces a level, a line break inside a cell as `\n`. Nothing is sent
anywhere: paste the text into the chat, or attach the workbook itself
when the chat reads Excel.

## Usage

``` r
tfl_ai_read_shell(
  path,
  outputs = NULL,
  sheets = NULL,
  toc_sheet = NULL,
  max_chars = .ai_shell_max_chars,
  file = NULL
)
```

## Arguments

- path:

  The workbook (`.xlsx` or `.xlsm`).

- outputs:

  Report numbers to keep (as the workbook or your TOC writes them);
  `NULL` for all.

- sheets:

  Sheet names (or positions) to keep whole; `NULL` for all. With
  `outputs` too, the sheets are added to the reports' parts.

- toc_sheet:

  The TOC sheet's name, when the guess is wrong; `NA` for none.

- max_chars:

  The text's length at most; the rest is cut and the text says so (pick
  fewer `outputs`).

- file:

  A file to write the text to (UTF-8), or `NULL`.

## Value

A `tfl_ai_shell`: a list with `text` (one string), `toc` (a data frame,
one row a report the TOC lists: `output_id` as written, `key`, `type`,
`title`, `text` – the row's text – `sheet`, `row`, `shell` – the sheet
of its shell, `NA` for none found), `sheets` (`sheet`, `role` – `toc`,
`shell` or `other` – `rows`, `cells`, `hidden`), `cells` (every
non-empty cell: `sheet`, `row`, `col`, `ref`, `text`), `outputs` (the
reports kept), `truncated` and `path`.
[`format()`](https://rdrr.io/r/base/format.html) gives the text;
[`print()`](https://rdrr.io/r/base/print.html) a summary.

## Details

The report numbers are found as they are written – `T-14-1-1`,
`Table 14.1.1`, `14.1.1`, `F14.2.1` – and matched whatever the
punctuation (`T-14-1-1` is `Table 14.1.1`). The TOC is the sheet named
like one (TOC, contents, index, list ...) or else the sheet listing the
most report numbers. A shell is a sheet named by its report's number, or
the rows of a sheet from the row that names its number to the row before
the next number.

- `outputs`: only these reports – the TOC's header rows and the reports'
  rows, and each report's shell.

- `sheets`: only these sheets, whole.

## See also

[`tfl_ai_batches()`](https://ichirio.github.io/tflspec/reference/tfl_ai_batches.md)
to split the TOC into requests,
[`tfl_ai_adam_summary()`](https://ichirio.github.io/tflspec/reference/tfl_ai_adam_summary.md),
[`tfl_ai_context()`](https://ichirio.github.io/tflspec/reference/tfl_ai_context.md)
(`shell =`).

## Examples

``` r
f <- system.file("ai", "example", "onco_toc_shell.xlsx", package = "tflspec")
x <- tfl_ai_read_shell(f)
x
#> <tfl_ai_shell: onco_toc_shell.xlsx>
#> 4 sheet(s): TOC (toc), T14.1.1 (shell), T14.2.1 (shell), Safety (shell)
#> 4 report(s) in the TOC, 4 with a shell found
#> text: 3932 characters; format(x) gives it
x$toc[c("output_id", "title", "shell")]
#>   output_id
#> 1  T-14-1-1
#> 2  T-14-2-1
#> 3  T-14-3-1
#> 4  T-14-3-5
#>                                                                        title
#> 1                                   Demographic and Baseline Characteristics
#> 2                          Best Overall Response and Objective Response Rate
#> 3 Treatment-Emergent Adverse Events by System Organ Class and Preferred Term
#> 4  Shift from Baseline in Laboratory Normal Range Indicator at Cycle 2 Day 1
#>     shell
#> 1 T14.1.1
#> 2 T14.2.1
#> 3  Safety
#> 4  Safety
cat(substr(format(tfl_ai_read_shell(f, outputs = "T-14-1-1")), 1, 900))
#> # Workbook onco_toc_shell.xlsx: 2 sheet(s), the parts of T-14-1-1
#> 
#> Each line is one row of a sheet: `<row>: <column>=<text> | ...`, the non-empty cells left to right. A merged cell is written once with its range (`A1:E1=`). Leading spaces are the cell's indent; `\n` is a line break inside a cell.
#> 
#> ## Sheet "TOC" (the TOC)
#> 1: A1:E1=XYZ-ONC-01 Table of Contents (draft 0.2)
#> 3: A=No. | B=Type | C=Title | D=Population | E=Program
#> 4: C=14.1 Demographic Data
#> 5: A=T-14-1-1 | B=Table | C=Demographic and Baseline Characteristics | D=Safety Population | E=t_dm.sas
#> 
#> ## Sheet "T14.1.1"
#> 1: A=Table 14.1.1
#> 2: A2:C2=Demographic and Baseline Characteristics
#> 3: A3:C3=(Safety Population)
#> 5: B=Drug X | C=Control | D=Total
#> 6: A=Characteristic | B=(N=xx) | C=(N=xx) | D=(N=xx)
#> 7: A=Age (years)
#> 8: A=  n | B=xx | C=xx | D=xx
#> 9: A=  Mean (SD) | B=xx.x (xx.xx) | C=xx.x (xx.xx) | D=xx.x (xx.xx)
#> 10: A=  Median | B=xx
```
