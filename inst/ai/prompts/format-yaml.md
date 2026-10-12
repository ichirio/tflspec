Write the block in YAML:

- Write a list of rows or pieces one item a line, as a flow mapping: `- {name: value, name: value}`.
- Put every text in double quotes when it contains any of `{ } [ ] : , # & * ! | > ' " %` or starts or ends with a space. A template such as `"{n} ({p}%)"` must be quoted, or YAML reads it as a mapping.
- Inside double quotes, write a line break as `\n` and a double quote as `\"`.
- Use spaces, never tabs.
