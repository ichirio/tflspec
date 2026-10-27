## Templates first (templates: TRUE)

This study's requests were written with templates (each starts with
`<!-- templates: TRUE -->`). Read `ai/templates.md` once: the templates
there are -- tflspec's, the company's, the study's variants -- with their
arguments. Each request names the templates likely for its reports.
When one fits a report, answer that report as the template:
`template: {name: <template>, args: {...}}`, only the arguments that differ
from the default, and in `content` only the report's own items for what the
template does not do. When none fits, write the report as a code SPEC, as
the schema says (`ai/instructions-table.md`). A template or an argument `ai/templates.md` does
not list is an error: use only those it lists.
