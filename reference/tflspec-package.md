# tflspec: Excel specifications for tables, listings and figures

Reads study specifications written in Excel and turns them into what a
clinical TFL report is made of: ARD-based tables (`ard_*()`,
`table_plan()` / `plan_*()`,
[`tfl_table_spec()`](https://ichirio.github.io/tflspec/reference/tfl_table_spec.md)
/
[`tfl_read_report_spec()`](https://ichirio.github.io/tflspec/reference/tfl_read_report_spec.md))
and figure code
([`tfl_fig_spec()`](https://ichirio.github.io/tflspec/reference/tfl_fig_spec.md)
/
[`tfl_fig_code()`](https://ichirio.github.io/tflspec/reference/tfl_fig_code.md)).
Rendering to RTF is rtfreporter's job; this package decides *what* goes
on the page.

## See also

Useful links:

- <https://github.com/ichirio/tflspec>

- <https://ichirio.github.io/tflspec/>

- Report bugs at <https://github.com/ichirio/tflspec/issues>

## Author

**Maintainer**: Yoichi Masui <ichi.r.pro@gmail.com> \[copyright holder\]

Other contributors:

- CDISC (JSON Schema of the ARS model, inst/ars/ars_ldm.json (MIT))
  \[copyright holder\]
