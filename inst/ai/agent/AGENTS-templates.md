## Templates first (templates: TRUE)

This study's requests were written with templates (each has the line
`<!-- templates: TRUE -->` near its top). Read `ai/templates.md` once: the templates
there are -- tflspec's and the company's -- with their arguments. Each request
names the templates likely for its reports. A template has no study variant.
To change one for a report, set an argument, or add override cards after the
template in `content` (the last one wins).
When one fits a report, answer that report as the template:
`template: {name: <template>, args: {...}}`, only the arguments that differ
from the default, and in `content` only the report's own items for what the
template does not do. When none fits, write the report as a code SPEC, as
the schema says (`ai/instructions-table.md`). A template or an argument `ai/templates.md` does
not list is an error: use only those it lists.
