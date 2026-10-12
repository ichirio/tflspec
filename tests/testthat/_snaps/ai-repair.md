# an unreadable block gets the format's rules and no schema

    Code
      cat(r)
    Output
      Your answer had these problems. An error must be fixed; a warning is a name or value worth checking again.
      
      | severity | where | problem |
      |---|---|---|
      | error | block | the block could not be read: YAML: Parser error: while parsing a flow mapping at line 3, column 3 did not find expected ',' or '}' at line 3, column 34 |
      
      Return the whole block again, corrected, with the same header. Keep everything that was right and change only what the problems name. When a problem asks for a name the context does not list, leave the item out and say so under `assumptions`.
      
      Write the block in YAML:
      
      - Write a list of rows or pieces one item a line, as a flow mapping: `- {name: value, name: value}`.
      - Put every text in double quotes when it contains any of `{ } [ ] : , # & * ! | > ' " %` or starts or ends with a space. A template such as `"{n} ({p}%)"` must be quoted, or YAML reads it as a mapping.
      - Inside double quotes, write a line break as `\n` and a double quote as `\"`.
      - Use spaces, never tabs.

