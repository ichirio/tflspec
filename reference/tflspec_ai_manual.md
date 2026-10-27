# The AI assistant manual that ships with this package

tflspec is too new to be in any chat model's training data: asked for a
table specification or the program made from one, an assistant invents
sheet columns and function names and flags neither as a guess. The fix
is to give it the facts first, and this file is those facts – the
specification formats (ARD, table, report, listing, figure design), the
functions that turn them into R code, what that code looks like, the
former names that are refused, and the complete export list – sized to
sit in one chat session.

## Usage

``` r
tflspec_ai_manual(
  dir = NULL,
  overwrite = FALSE,
  define = NULL,
  adam_spec = NULL,
  datasets = NULL
)
```

## Arguments

- dir:

  Optional folder. When given, the kit is written there (the folder is
  made if need be) and the paths of its files are returned invisibly;
  the files, their sizes and what to attach are printed.

- overwrite:

  Write over a kit already in `dir`. Default `FALSE`.

- define, adam_spec, datasets:

  With `dir`: the study's ADaM metadata, from its `define.xml`, its ADaM
  spec workbook or the datasets' column information (see
  [`tfl_adam_metadata()`](https://ichirio.github.io/tflspec/reference/tfl_adam_metadata.md));
  written as `07_adam_metadata.csv`.

## Value

The path to the manual when `dir` is `NULL`; otherwise the kit's files,
invisibly.

## Details

`tflspec_ai_manual()` returns the path to the copy **installed with this
package**, so the manual you attach always describes the version you
actually have. For the rendering side attach rtfreporter's own manual
instead
([`rtfreporter::rtfreporter_ai_manual()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_ai_manual.html)).

With `dir`, it writes everything a chat needs to draft a study's specs
from the company's TOC and shells into one flat folder – seven files
(eight with the ADaM metadata), under the ten files most chats take:

- `00_START_HERE.md` – the request to paste, and what to attach;

- `01_tflspec-ai-user-manual.md` – this manual;

- `02_tflspec-ai-toc-shell-guide.md` – how to read a TOC and shells into
  the spec files, the checklist the assistant runs, the assumptions it
  must list;

- `03_spec_schema.yml` – the installed `inst/spec/schema.yml`: every
  function a spec item calls, its arguments and their types
  ([`tfl_schema()`](https://ichirio.github.io/tflspec/reference/tfl_schema.md)),
  and the spec sheets' columns
  ([`tfl_spec_columns()`](https://ichirio.github.io/tflspec/reference/tfl_spec_columns.md));

- `04_blank_spec.md` – every file of a study's YAML spec, blank, its
  keys said in comments, in the form of the answer;

- `05_example_toc_shell.xlsx`, `06_example_spec.md` – a made-up study
  end to end: its TOC and four shells (a demographics table, an AE table
  by SOC and PT, a listing of serious AEs, a figure of mean blood
  pressure), and the spec files written from them, with the
  `spec/_assumptions.yml` the guide asks for. The example is written now
  through the one-time conversion
  ([`tfl_yaml_from_sheets()`](https://ichirio.github.io/tflspec/reference/tfl_convert_sheets_to_yaml.md)),
  so it is this version's.

The assistant answers with ONE text holding every file, each a heading
with its path and a `yaml` block;
[`tfl_ai_unpack()`](https://ichirio.github.io/tflspec/reference/tfl_ai_unpack.md)
writes it into a study folder and
[`tfl_ai_review()`](https://ichirio.github.io/tflspec/reference/tfl_ai_review.md)
checks the folder.

With `define`, `adam_spec` or `datasets` it also writes
`07_adam_metadata.csv`, the study's ADaM metadata
([`tfl_adam_metadata()`](https://ichirio.github.io/tflspec/reference/tfl_adam_metadata.md):
one row a variable, its label, kind and levels; never the data), and the
request tells the assistant to take every dataset, variable and level
from it. Give one; with several, the first of `define`, `adam_spec`,
`datasets` is used.

## See also

[`tfl_ai_review()`](https://ichirio.github.io/tflspec/reference/tfl_ai_review.md)
for the files the assistant returns.

## Examples

``` r
# where the manual lives
tflspec_ai_manual()
#> [1] "/home/runner/work/_temp/Library/tflspec/ai/tflspec-ai-user-manual.md"
writeLines(head(readLines(tflspec_ai_manual()), 3))
#> # tflspec — AI user manual
#> 
#> **This manual documents tflspec 0.0.24.9235** (the development version,

# the kit to attach to a chat
# \donttest{
kit <- tflspec_ai_manual(file.path(tempdir(), "ai-kit"))
#> tflspec AI kit: 7 files, 278 KB, in /tmp/RtmprDOQfE/ai-kit
#>   00_START_HERE.md  (  5 KB)
#>   01_tflspec-ai-user-manual.md  ( 88 KB)
#>   02_tflspec-ai-toc-shell-guide.md  ( 39 KB)
#>   03_spec_schema.yml  (117 KB)
#>   04_blank_spec.md  (  5 KB)
#>   05_example_toc_shell.xlsx  ( 14 KB)
#>   06_example_spec.md  ( 10 KB)
#> Attach all files in /tmp/RtmprDOQfE/ai-kit, plus your TOC and shells, and paste the request in 00_START_HERE.md.
basename(kit)
#> [1] "00_START_HERE.md"                 "01_tflspec-ai-user-manual.md"    
#> [3] "02_tflspec-ai-toc-shell-guide.md" "03_spec_schema.yml"              
#> [5] "04_blank_spec.md"                 "05_example_toc_shell.xlsx"       
#> [7] "06_example_spec.md"              
# }
```
