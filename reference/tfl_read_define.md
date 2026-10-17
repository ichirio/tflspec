# Read a Define-XML (2.0 or 2.1)

Reads a study's `define.xml` – the CDISC standard description of its
datasets – by rule (xml2), and gives its datasets, variables and code
lists as tidy data frames. The result is what
[`tfl_define_codelists()`](https://ichirio.github.io/tflspec/reference/tfl_define_codelists.md)
turns into a report's `codelists` rows.

## Usage

``` r
tfl_read_define(path, lang = "en")
```

## Arguments

- path:

  A `define.xml`.

- lang:

  The language of the texts to take (`xml:lang`), e.g. `"en"` or `"ja"`.

## Value

A `tfl_define`: a list of the data frames above, with `attr(, "source")`
(the file, `"define.xml"`, the Define-XML version).

## Details

- `datasets`: one row an `ItemGroupDef`: `dataset`, `label`, `class`,
  `subclass`, `structure`, `purpose`, `repeating`, `reference`, `keys`
  (the key variables in key order, `" | "` between them), `order`,
  `oid`.

- `variables`: one row a variable of a dataset (an `ItemRef` and its
  `ItemDef`), in the dataset's order: `dataset`, `variable`, `label`,
  `type`, `length`, `digits` (significant digits), `format`, `codelist`
  (the code list's OID), `mandatory`, `order`, `key`, `role`, `origin`,
  `oid`.

- `codelists`: one row a value (`CodeListItem` or `EnumeratedItem`), in
  the file's order: `codelist` (OID), `name`, `type`, `value` (the coded
  value), `decode` (`NA` for an enumerated item), `order`
  (`OrderNumber`), `rank`, `extended` (`def:ExtendedValue="Yes"`),
  `code` (the NCI code, `Alias` of context `nci:ExtCodeID`), `kind`
  (`"item"` or `"enumerated"`).

- `external`: code lists that point to a dictionary (`ExternalCodeList`,
  e.g. MedDRA): `codelist`, `name`, `dictionary`, `version`. They hold
  no values.

- `study`: `study_name`, `description`, `protocol`, `define_version`,
  `file_oid`.

Elements are found by their namespace URIs, not by prefix, so a file
that binds the namespaces to other prefixes reads the same. Texts
(`TranslatedText`) are taken in `lang`; else the one without `xml:lang`;
else the first. Value-level metadata, methods, comments and documents
are not read.

## See also

[`tfl_read_define_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_define_spec.md)
for a define spec workbook;
[`tfl_define_codelists()`](https://ichirio.github.io/tflspec/reference/tfl_define_codelists.md).

## Examples

``` r
if (requireNamespace("xml2", quietly = TRUE)) {
  def <- tfl_read_define(system.file("extdata", "define", "define.xml",
                                     package = "tflspec"))
  def
  def$variables[def$variables$dataset == "ADSL", c("variable", "label")]
  def$codelists[def$codelists$codelist == "CL.SEX", ]
}
#>   codelist name type value decode order rank extended   code kind
#> 3   CL.SEX  Sex text     F Female     2   NA    FALSE C16576 item
#> 4   CL.SEX  Sex text     M   Male     1   NA    FALSE C20197 item
```
