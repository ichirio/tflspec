Write the block in YAML:

- Indent with two spaces, never tabs. Write the structure in block style; a short mapping or list may be one line in flow style (`- {id: AGE, method: continuous, dataset: ADSL, variables: [AGE]}`).
- Put every text in double quotes when it contains any of `{ } [ ] : , # & * ! | > ' " %` or starts or ends with a space. A cell template such as `"{n} ({p:.1f%})"` must be quoted, or YAML reads it as a mapping. Inside double quotes, write a double quote as `\"`.
- Quote the line numbers of `titles` and `footnotes`: `"1": {center: Table 14.1.1}`.
- Write R code after the tag `!r`, in single quotes when it holds double quotes or a colon: `where: !r 'TRTEMFL == "Y"'`.
