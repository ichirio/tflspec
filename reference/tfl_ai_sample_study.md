# Write the sample study for trying the AI flow

Writes a made-up study, DMY-001 of Example Pharma, for trying the AI
flow (the tflplanner AI tab, or
[`tfl_ai_prepare()`](https://ichirio.github.io/tflspec/reference/tfl_ai_prepare.md)):
the ADaM in `adam/` and the documents an agent reads in `inputs/`.
Everything is invented. No spec and no expected output is written: those
are what the agent writes.

## Usage

``` r
tfl_ai_sample_study(dir, overwrite = FALSE)
```

## Arguments

- dir:

  Folder to write into; made when missing. Put it where the agent's
  editor can open it (e.g. under the tflplanner home).

- overwrite:

  Overwrite files already there? When `FALSE` (default), a file already
  there stops the call before anything is written.

## Value

`dir`, invisibly. The paths written are in its attribute `files`.

## Details

- `adam/`: `adsl`, `adae`, `adlb`, `advs` and `adtte`, as SAS transport
  files (`.xpt`) when the haven package is installed, else as `.rds`.
  They are made from tflspec's bundled sample data; the arms are
  Placebo, Drug A 10 mg and Drug A 20 mg. `advs` is used by no shell.

- `inputs/toc.xlsx`: the TOC (eight outputs; sheets `TOC`,
  `Revision history`, `Notes`).

- `inputs/shells.xlsx`: the annotated shells, one per sheet (six
  shells), and `General conventions`, which is not a shell.

- `inputs/adam_spec.xlsx`: the ADaM spec in the Pinnacle 21 form;
  `inputs/define.xml`: the same as define.xml.

- `inputs/sap.docx`: the statistical analysis plan, a reference
  document.

Some points are left open in the documents on purpose: an agent should
ask about them in the chat rather than decide them silently.
`GUIDE-ja.md`, written beside the two folders, is the tester's guide in
Japanese: from this call to step 4 of the AI tab.

## Examples

``` r
d <- tfl_ai_sample_study(file.path(tempdir(), "DMY-001-inputs"))
list.files(d, recursive = TRUE)
#>  [1] "GUIDE-ja.md"           "adam/adae.xpt"         "adam/adlb.xpt"        
#>  [4] "adam/adsl.xpt"         "adam/adtte.xpt"        "adam/advs.xpt"        
#>  [7] "inputs/adam_spec.xlsx" "inputs/define.xml"     "inputs/sap.docx"      
#> [10] "inputs/shells.xlsx"    "inputs/toc.xlsx"      
```
