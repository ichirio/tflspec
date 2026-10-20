## Templates first (templates: TRUE)

This study's requests were written with templates (each starts with
`<!-- templates: TRUE -->`). Each request lists the templates there are --
tflspec's, the company's, the study's variants -- with their arguments.
When one fits a report, answer that report as the template:
`template: {name: <template>, args: {...}}`, only the arguments that differ
from the default, and in `content` only the report's own items for what the
template does not do. When none fits, write the report as a code SPEC, as
the request's schema says. A template or an argument the request does not
list is an error that `ai/status.md` names; repair it as for any other.
