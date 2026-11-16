# tflspec_v1: tflspec's own series of templates

A series is one folder of template SPECs, the version in its name
(`tflspec_v1`); a company series is a copy of it under a new name, then changed.

- `series.yml`: the common file: `name`, `label`, `released`, `needs`, `fixes` and `values:`.
- `templates/<name>.yml`: one template each; a report names it `tflspec_v1/<name>`.

`!series <key>` in a template reads `values: <key>`: the study's
`series_settings.tflspec_v1.<key>` (study.yml) first, then this folder's `series.yml`.

A template is a code SPEC (`data:` rows, `analysis:` items with `method:`, `content:`, `page:`)
plus a `params:` block: argument definitions in the schema's shape (`type`, `input`,
`choices`, `default`, `step`, `tier`, `description`). Each `"{name}"` is filled from a param.
