# tflspec_v1: tflspec's own series of templates

A series is one folder of template SPECs, the version in its name
(`tflspec_v1`); a company series is a copy of it under a new name, then changed.

- `series.yml`: the common file: `name`, `label`, `released`, `needs`, `fixes` and `values:`.
- `templates/<name>.yml`: one template each; a report names it `tflspec_v1/<name>`.

`!series <key>` in a template reads `values: <key>` of the `series.yml` read.

A study does not read this folder. `tfl_copy_templates()` copies `series.yml` and
the templates its reports use, whole, into `<study>/spec/series/tflspec_v1/`
(`copied.yml` there records where each came from, when, and its md5), and the
study reads only its copy. The study changes a template by editing its copy, and
a value in its copy's `values:` (`tfl_series_values_set()`). When this folder
changes (a new tflspec), `tfl_template_status()` lists what changed,
`tfl_template_diff()` shows a copy against it, and `tfl_recopy_templates()`
copies the chosen templates again.

A template is a code SPEC (`data:` rows, `analysis:` items with `method:`, `content:`, `page:`)
plus a `params:` block: argument definitions in the schema's shape (`type`, `input`,
`choices`, `default`, `step`, `tier`, `description`). Each `"{name}"` is filled from a param.
